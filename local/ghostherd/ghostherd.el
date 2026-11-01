;;; ghostherd.el --- Manage AI agent CLIs in ghostel terminals -*- lexical-binding: t; -*-

;; Author: Jing
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ghostel "0.32") (xwapp "0.1.0"))
;; Keywords: tools, terminals, ai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ghostherd is a herdr-inspired control plane for coding agents that
;; run inside ghostel terminals.  Each agent is a named session bound
;; to a real PTY buffer.  Sessions are scoped to Emacs (no background
;; daemon).
;;
;; Features:
;; - Spawn claude / grok / agy (or a plain shell) per project
;; - Multiple concurrent agents (e.g. implementer + reviewer)
;; - Session list (posframe overlay, side window fallback) + consult switcher
;; - Inter-agent messaging (prompt, send, read, wait, message, sidecar herd_*)
;; - Notifications on blocked / process exit (via alert when available)
;; - Shared transcript memory (Python sidecar: import + search)
;;
;; Main entry points:
;;   M-x ghostherd-new
;;   M-x ghostherd-switch
;;   M-x ghostherd-sidebar
;;   M-x ghostherd-next-blocked
;;   M-x ghostherd-message

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'project)
(require 'tabulated-list)
;; The host slot, the session struct and the ghostel implementor.  Loaded
;; first because everything below is written against the slot rather than
;; against ghostel directly.
(require 'ghostherd-backend)
(require 'ghostherd-tmux)
(require 'ghostherd-memory)
(require 'ghostherd-usage)

(declare-function magit-status-setup-buffer "magit-status" (&optional directory))
(declare-function alert "alert" (message &rest kwargs))
(declare-function posframe-workable-p "posframe")
(declare-function posframe-show "posframe")
(declare-function posframe-hide "posframe")
(declare-function posframe-refresh "posframe")
(declare-function posframe-poshandler-frame-center "posframe")

;; The ghostel I/O shims, the `ghostherd' group and the session struct
;; now live in ghostherd-backend.el.  These few stay because `defvar'
;; without a value declares a variable special only within one file, and
;; the hook plumbing below is still ghostherd's own.
(defvar ghostel-progress-function)
(defvar ghostel-query-before-killing)
(defvar ghostel-eval-cmds)
(defvar ghostel-exit-functions)
(defvar ghostel-command-start-functions)
(defvar ghostel-command-finish-functions)
(defvar ghostel--input-mode)
(declare-function ghostel-copy-mode "ghostel")
(declare-function ghostel-readonly-exit "ghostel")
(declare-function evil-define-key* "evil")
(declare-function evil-make-intercept-map "evil")

;; The parts, in the order their code once stood in this one file.
;; Each requires only parts above it, so loading this file runs every
;; top-level form in that order -- the order of the functions on a
;; hook among them.
(require 'ghostherd-core)
(require 'ghostherd-worktree)
(require 'ghostherd-log)
(require 'ghostherd-screen)
(require 'ghostherd-spawn)
(require 'ghostherd-comm)

;;; Ghostel integration hooks

(defun ghostherd--on-ghostel-exit (buffer event)
  "React to the ghostel process in BUFFER exiting.  EVENT is the sentinel.

Whether that is a death depends on what the buffer was running.  On the
ghostel backend the buffer *is* the agent, so it is.  On a backend where
the buffer holds a client -- `tmux attach' -- the exit only means the
view went away, and marking the session dead would turn every closed
window into a crash: a notification would fire, a handoff would give up,
and the next poll would flip the state straight back."
  (when-let* ((session (ghostherd-get buffer)))
    (if (ghostherd-backend-view-is-host-p (ghostherd-session-backend session))
        (progn
          (ghostherd--set-state session 'dead (string-trim (or event "exited")))
          (ghostherd--stop-poll-timer))
      (setf (ghostherd-session-buffer session) nil)
      (ghostherd--sidebar-refresh))))

(defun ghostherd--on-command-start (buffer)
  "OSC 133 command-start hook: mark SESSION on BUFFER as working."
  (when-let* ((session (ghostherd-get buffer)))
    (unless (ghostherd-session-manual-state session)
      (ghostherd--set-state session 'working "command start"))))

(defun ghostherd--on-command-finish (buffer status)
  "OSC 133 command-finish hook for BUFFER with exit STATUS."
  (when-let* ((session (ghostherd-get buffer)))
    (unless (ghostherd-session-manual-state session)
      ;; Don't force idle for long-running TUI agents; let screen rules decide.
      (when (eq (ghostherd-session-kind session) 'shell)
        (ghostherd--set-state
         session 'idle
         (format "command finish (%s)" (or status "?")))))))

(defun ghostherd--on-progress (state percent)
  "Record a ConEmu OSC 9;4 progress report for the current buffer's session.
STATE and PERCENT are as documented for `ghostel-progress-function'.

This runs synchronously on ghostel's VT-parser callpath, where anything
slow stalls terminal output, so it only stores the report -- deciding
what it means is left to the poll path.  The previous
`ghostel-progress-function' is always called, since it is a single
global setting rather than a hook and is very likely someone's spinner."
  (when-let* ((session (ghostherd-get (current-buffer))))
    (setf (ghostherd-session-progress-state session) state
          (ghostherd-session-progress-percent session) percent
          (ghostherd-session-progress-at session) (current-time)))
  (when (functionp ghostherd--saved-progress-function)
    (funcall ghostherd--saved-progress-function state percent)))

(defun ghostherd--install-hooks ()
  "Install ghostel hooks used by ghostherd."
  (add-hook 'ghostel-exit-functions #'ghostherd--on-ghostel-exit)
  (add-hook 'ghostel-command-start-functions #'ghostherd--on-command-start)
  (add-hook 'ghostel-command-finish-functions #'ghostherd--on-command-finish)
  (advice-add 'ghostel-copy-mode :after #'ghostherd--copy-mode-sync-evil)
  (advice-add 'ghostel-readonly-exit :after #'ghostherd--copy-mode-sync-evil)
  (when (and ghostherd-use-osc-progress
             (boundp 'ghostel-progress-function)
             (not (eq ghostel-progress-function #'ghostherd--on-progress)))
    (setq ghostherd--saved-progress-function ghostel-progress-function
          ghostel-progress-function #'ghostherd--on-progress)))

(defun ghostherd--remove-hooks ()
  "Undo `ghostherd--install-hooks'.
Restoring `ghostel-progress-function' matters more than the rest: it is
a single global function, not a hook, so leaving ours in place would
keep someone else's spinner permanently displaced."
  (remove-hook 'ghostel-exit-functions #'ghostherd--on-ghostel-exit)
  (remove-hook 'ghostel-command-start-functions #'ghostherd--on-command-start)
  (remove-hook 'ghostel-command-finish-functions #'ghostherd--on-command-finish)
  (advice-remove 'ghostel-copy-mode #'ghostherd--copy-mode-sync-evil)
  (advice-remove 'ghostel-readonly-exit #'ghostherd--copy-mode-sync-evil)
  (when (and (boundp 'ghostel-progress-function)
             (eq ghostel-progress-function #'ghostherd--on-progress))
    (setq ghostel-progress-function ghostherd--saved-progress-function
          ghostherd--saved-progress-function nil)))

(defun ghostherd--register-eval-cmds ()
  "Whitelist ghostherd commands for `ghostel_cmd' from agent shells."
  (when (boundp 'ghostel-eval-cmds)
    (dolist (entry '(("ghostherd-list" ghostherd-cmd-list)
                     ("ghostherd-send" ghostherd-cmd-send)
                     ("ghostherd-prompt" ghostherd-cmd-prompt)
                     ("ghostherd-message" ghostherd-cmd-message)
                     ("ghostherd-read" ghostherd-cmd-read)
                     ("ghostherd-state" ghostherd-cmd-state)
                     ("ghostherd-report" ghostherd-cmd-report)
                     ("ghostherd-memory-search" ghostherd-cmd-memory-search)
                     ("ghostherd-memory-import" ghostherd-cmd-memory-import)
                     ("ghostherd-memory-list" ghostherd-cmd-memory-list)
                     ("ghostherd-memory-chunks" ghostherd-cmd-memory-chunks)))
      (unless (assoc (car entry) ghostel-eval-cmds)
        (add-to-list 'ghostel-eval-cmds entry)))))


;;; Display helpers

(defun ghostherd--state-glyph (state)
  "Return a short glyph for STATE."
  (pcase state
    ('blocked "⚠")
    ('working "⟳")
    ('waiting "◷")
    ('done    "✓")
    ('idle    "○")
    ('starting "…")
    ('dead    "✖")
    (_        "?")))

(defun ghostherd--state-face (state)
  "Return a face symbol for STATE."
  (pcase state
    ('blocked 'error)
    ('working 'warning)
    ('waiting 'font-lock-constant-face)
    ('done    'success)
    ('dead    'shadow)
    ('starting 'font-lock-comment-face)
    (_        'default)))

(defcustom ghostherd-annotation-note-width 32
  "Characters of a session's note shown in the switcher before eliding."
  :type 'integer
  :group 'ghostherd)

(defun ghostherd--format-candidate (session)
  "Return the completing-read candidate string for SESSION.
Just the name: detail belongs in the annotation, where it is visually
distinct and does not silently widen what your input matches against."
  (ghostherd-session-name session))

(defun ghostherd--session-annotation (session)
  "Return the annotation suffix describing SESSION."
  (let* ((note (ghostherd-session-notes session))
         (note (and note (string-trim note)))
         (note (and note (not (string-empty-p note))
                    (truncate-string-to-width
                     note ghostherd-annotation-note-width nil nil t)))
         (parts (delq nil
                      (list (symbol-name (ghostherd-session-kind session))
                            (symbol-name (ghostherd-session-state session))
                            ;; A view, not a state: the agent is running
                            ;; and nobody is attached.  Worth saying here
                            ;; because this is where you pick one, and
                            ;; choosing it is what attaches.
                            (unless (buffer-live-p
                                     (ghostherd-session-buffer session))
                              "detached")
                            (ghostherd--age-string
                             (or (ghostherd-session-last-active session)
                                 (ghostherd-session-started-at session)))
                            (ghostherd--abbreviate
                             (ghostherd-session-project session))
                            note))))
    (concat "  " (propertize (string-join parts "  ")
                             'face 'completions-annotations))))

(defun ghostherd--session-affixation (candidates)
  "Affix CANDIDATES with a state glyph and `ghostherd--session-annotation'."
  (mapcar
   (lambda (name)
     (if-let* ((session (ghostherd-get name)))
         (let ((state (ghostherd-session-state session)))
           (list name
                 (propertize (concat (ghostherd--state-glyph state) " ")
                             'face (ghostherd--state-face state))
                 (ghostherd--session-annotation session)))
       (list name "" "")))
   candidates))

(defun ghostherd--session-completion-table (candidates)
  "Return a completion table over CANDIDATES carrying ghostherd metadata.
A table rather than a plain list so the category is advertised, which is
what lets consult and marginalia treat these as sessions."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        `(metadata (category . ghostherd-session)
                   (affixation-function . ghostherd--session-affixation))
      (complete-with-action action candidates string predicate))))

(defun ghostherd--age-string (time)
  "Human-readable age for TIME."
  (if (null time)
      "-"
    (let* ((secs (max 0 (floor (float-time (time-subtract (current-time) time)))))
           (m (floor secs 60))
           (h (floor m 60)))
      (cond
       ((>= h 1) (format "%dh%02dm" h (mod m 60)))
       ((>= m 1) (format "%dm" m))
       (t (format "%ds" secs))))))


(defcustom ghostherd-scrollback-lines 3000
  "Lines of history `ghostherd-scrollback' asks the host for."
  :type 'integer
  :group 'ghostherd)

;;;###autoload
(defun ghostherd-scrollback (session)
  "Show SESSION's history in an ordinary Emacs buffer.

The tmux backend took something away without meaning to.  A ghostel
buffer accumulates everything the agent ever printed, so scrolling back
is just moving point; behind tmux the buffer holds only the visible
pane, and the history is in tmux -- where `prefix None' leaves no way in
from the keyboard.

Putting it back as a tmux key binding would make tmux a third
navigation layer, which is the one thing this backend refuses to be.
Pulling the history into a buffer instead costs nothing and lands it
somewhere every Emacs motion already works.

A full-screen TUI keeps no history at all -- the alternate screen is not
saved -- so for those this shows the current screen and the agent's own
keys are what scroll it."
  (interactive (list (ghostherd--read-session "Scrollback of agent: ")))
  (setq session (ghostherd-get session))
  (let ((text (ghostherd--host-scrollback session ghostherd-scrollback-lines))
        (name (ghostherd-session-name session)))
    (unless (and text (not (string-empty-p (string-trim text))))
      (user-error "No history kept for %s" name))
    (ghostherd--sidebar-leave-overlay)
    (with-current-buffer (get-buffer-create (format "*ghostherd history: %s*" name))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)
        (goto-char (point-max)))
      (special-mode)
      (pop-to-buffer (current-buffer)))))


(defcustom ghostherd-scroll-lines 3
  "Lines moved per scroll step in an attached agent view."
  :type 'integer
  :group 'ghostherd)

(defun ghostherd--scroll (lines)
  "Scroll the agent viewed in the current buffer back by LINES."
  (if (eq ghostel--input-mode 'copy)
      ;; The client is frozen; driving tmux would move a host this
      ;; buffer no longer shows.
      (if (> lines 0) (scroll-down lines) (scroll-up (- lines)))
    (let ((session (ghostherd-get (current-buffer))))
      (unless session
        (user-error "Not a ghostherd agent buffer"))
      (unless (ghostherd--host-scroll session lines)
        ;; A host that does not scroll its own view keeps its output in the
        ;; buffer, where Emacs has always been able to scroll it.
        (if (> lines 0) (scroll-down lines) (scroll-up (- lines)))))))

;;;###autoload
(defun ghostherd-scroll-up (&optional lines)
  "Scroll the agent view back into its history by LINES."
  (interactive "p")
  (ghostherd--scroll (* (or lines 1) ghostherd-scroll-lines)))

;;;###autoload
(defun ghostherd-scroll-down (&optional lines)
  "Scroll the agent view forward, back towards the live screen."
  (interactive "p")
  (ghostherd--scroll (- (* (or lines 1) ghostherd-scroll-lines))))

;;;###autoload
(defun ghostherd-scroll-page-up ()
  "Scroll the agent view back by roughly a screen."
  (interactive)
  (ghostherd--scroll (max 1 (- (window-body-height) 2))))

;;;###autoload
(defun ghostherd-scroll-page-down ()
  "Scroll the agent view forward by roughly a screen."
  (interactive)
  (ghostherd--scroll (- (max 1 (- (window-body-height) 2)))))

;;;###autoload
(defun ghostherd-scroll-bottom ()
  "Return the agent view to the live screen.

Scrolls forward far enough to hit the bottom, which is also what makes
tmux leave copy mode -- so this is \"put me back where I can type\"
rather than a mode command."
  (interactive)
  (let ((session (ghostherd-get (current-buffer))))
    (unless session
      (user-error "Not a ghostherd agent buffer"))
    (unless (ghostherd--host-scroll session (- (* 1000 ghostherd-scroll-lines)))
      (goto-char (point-max)))))

(defun ghostherd--copy-mode-sync-evil (&rest _)
  "Use Evil normal state in copy mode so search keys do not exit it.

`ghostel-readonly-fast-exit' leaves copy mode on any self-insert.
Evil emacs state makes `?' and `/' self-insert, so the first key
meant to search would drop copy mode -- which is how it looked
impossible to enter."
  (when (and (derived-mode-p 'ghostel-mode)
             (bound-and-true-p ghostherd-session-id)
             (bound-and-true-p evil-local-mode)
             (fboundp 'evil-normal-state)
             (fboundp 'evil-emacs-state))
    (if (eq ghostel--input-mode 'copy)
        (evil-normal-state)
      (evil-emacs-state))))

;;;###autoload
(defun ghostherd-copy-mode ()
  "Toggle ghostel copy mode in this agent view.

Bound to `C-z' so it wins against Evil's emacs-state map, which
would otherwise take `C-z' for `evil-exit-emacs-state' and leave
the terminal live."
  (interactive)
  (unless (derived-mode-p 'ghostel-mode)
    (user-error "Not a ghostel buffer"))
  (call-interactively #'ghostel-copy-mode))

(defvar-keymap ghostherd-terminal-mode-map
  :doc "Scrolling for an attached agent view, in the vocabulary Emacs uses."
  "<wheel-up>"     #'ghostherd-scroll-up
  "<wheel-down>"   #'ghostherd-scroll-down
  "<mouse-4>"      #'ghostherd-scroll-up
  "<mouse-5>"      #'ghostherd-scroll-down
  "<prior>"        #'ghostherd-scroll-page-up
  "<next>"         #'ghostherd-scroll-page-down
  "M-v"            #'ghostherd-scroll-page-up
  "C-M-v"          #'ghostherd-scroll-page-down
  "M->"            #'ghostherd-scroll-bottom
  "C-z"            #'ghostherd-copy-mode)

(defun ghostherd--bind-evil-copy-mode ()
  "Bind `C-z' in Evil emacs/normal so it is not `evil-exit-emacs-state'.

Must call `evil-define-key*', the function.  `evil-define-key' is
a macro; if this file is compiled without evil loaded the call is
left as a function and startup dies with `Invalid function'."
  (when (fboundp 'evil-define-key*)
    (evil-define-key* 'emacs ghostherd-terminal-mode-map
      (kbd "C-z") #'ghostherd-copy-mode)
    (evil-define-key* 'normal ghostherd-terminal-mode-map
      (kbd "C-z") #'ghostherd-copy-mode)))

(with-eval-after-load 'evil
  (ghostherd--bind-evil-copy-mode))

;; So reloading this file is enough; `ghostherd-mode' still adds/removes
;; the same pair around its own lifetime.
(advice-add 'ghostel-copy-mode :after #'ghostherd--copy-mode-sync-evil)
(advice-add 'ghostel-readonly-exit :after #'ghostherd--copy-mode-sync-evil)

;;;###autoload
(define-minor-mode ghostherd-terminal-mode
  "Make an attached agent view scroll like an ordinary buffer.

Only needed where the buffer is a *client*.  On the ghostel backend the
buffer holds everything the agent printed and Emacs has always scrolled
it; behind tmux it holds one screen, and the history is in the host --
so the wheel, PageUp/PageDown and \[ghostherd-scroll-page-up] drive the
host's own view instead, and the same buffer shows older output.

The mechanism is tmux copy mode, driven by command rather than by a
prefix key, so it never becomes something to learn or get stuck in:
scrolling back to the bottom leaves it automatically.  Ghostel copy
mode is separate: `C-z' (and ghostel's own `C-c C-t') freeze the
current screen to select text.  That is not history -- overlay `H'
is.

Evil users in normal state will want the commands bound there too --
`ghostherd-scroll-up\=', `ghostherd-scroll-down\=' and the two page
commands are the whole surface.

Bind them out of the terminal's own vocabulary, though.  This map is on
the agent view, so anything bound here is a key the agent never sees,
and only behind tmux -- the ghostel backend has no such mode, so the
same key keeps working there and the loss looks like a backend bug.
`C-M-v\=' pages down above for that reason: `C-v\=' is how Claude Code
pastes an image."
  :lighter " ⇅"
  :keymap ghostherd-terminal-mode-map
  (when (and ghostherd-terminal-mode
             (bound-and-true-p evil-local-mode)
             (fboundp 'evil-emacs-state)
             (not (eq evil-state 'emacs)))
    ;; The buffer is a PTY.  Normal state would swallow the agent's own
    ;; keys; emacs state is the one this map was written for.
    (evil-emacs-state)))


;;; Sidebar

(defvar ghostherd--sidebar-filter-project nil
  "When non-nil, sidebar only shows this project root.")

(defvar ghostherd--sidebar-query ""
  "Live-narrow string for the session list.  Empty means everything.")

(defvar ghostherd--sidebar-filtering nil
  "Non-nil while `/` has the session list consuming keys as a query.")

(defvar ghostherd--sidebar-match-count 0
  "Rows visible after the live query.")

(defvar ghostherd--sidebar-total-count 0
  "Rows after the project filter, before the live query.")

(defvar-local ghostherd--sidebar-preview-start nil
  "Marker where the preview pane begins in the session-list buffer.")

(defvar ghostherd--sidebar-preview-id nil
  "Session id currently shown in the preview pane.")

(defvar ghostherd--sidebar-preview-timer nil
  "Debounce timer for a fresh capture when the selected row changes.")

(defvar ghostherd--sidebar-help-visible nil
  "Non-nil when the overlay preview pane is showing the key legend.")

(defvar ghostherd--sidebar-picking nil
  "Frozen (DISPLAY ROOT COUNT) candidates while the project picker is open.
Frozen because a poll landing mid-pick would otherwise renumber
the rows under the key you are about to press.  Nil when closed.")

(defvar ghostherd--sidebar-pick-index 0
  "Selected row in the project picker.")

(defvar ghostherd--sidebar-target-width nil
  "Column budget that wins over the live window's width.

Bound around a posframe open or refit so columns are laid out for
the size the overlay *will* be, not the size the child frame still
is.  Without it a resize would paint into the old width and then
stretch the frame, truncating Project one step behind.")

(defvar ghostherd--sidebar-posframe-parent nil
  "Parent frame to restore when the posframe overlay is dismissed.")

(defvar ghostherd--sidebar-posframe-fitted-width nil
  "Last overlay width fitted to the parent frame.")

(defvar ghostherd--sidebar-posframe-refitting nil
  "Non-nil while the overlay is being resized, to ignore nested hooks.")

(defconst ghostherd-sidebar-help-commands
  '((ghostherd-sidebar-visit                 . "Visit agent buffer")
    (next-line                               . "Next row")
    (previous-line                           . "Previous row")
    (ghostherd-new                           . "New agent")
    (ghostherd-new-pair                      . "New pair (split windows)")
    (ghostherd-sidebar-kill                  . "Kill session")
    (ghostherd-sidebar-rename                . "Rename session")
    (ghostherd-respawn                       . "Respawn from recipe")
    (ghostherd-relay                         . "Relay: an heir on another CLI")
    (ghostherd-sidebar-changes               . "What its checkout has not committed")
    (ghostherd-sidebar-notes                 . "Edit note / role")
    (ghostherd-sidebar-interrupt             . "Interrupt (Escape)")
    (ghostherd-sidebar-abort                 . "Abort (C-c)")
    (ghostherd-sidebar-answer                . "Answer menu prompt")
    (ghostherd-sidebar-message               . "Message agent")
    (ghostherd-sidebar-prompt                . "Prompt agent")
    (ghostherd-sidebar-project-filter        . "Filter by project")
    (ghostherd-sidebar-filter                . "Live-narrow (flex)")
    (ghostherd-sidebar-toggle-preview        . "Toggle screen preview")
    (ghostherd-sidebar-refresh               . "Refresh")
    (ghostherd-usage-refresh                 . "Refresh account usage")
    (ghostherd-next-blocked                  . "Next blocked / done")
    (ghostherd-sidebar-mark-state            . "Mark state (manual / auto)")
    (ghostherd-explain                       . "Explain how state was decided")
    (ghostherd-log                           . "Herd log (what happened while you were away)")
    (ghostherd-scrollback                    . "Scrollback (history the pane no longer shows)")
    (ghostherd-sidebar-help                  . "This help")
    (ghostherd-sidebar-quit                  . "Quit"))
  "Commands listed by `ghostherd-sidebar-help', in display order.")

(defvar ghostherd-sidebar-mode-map)

(defun ghostherd--sidebar-help-maps ()
  "Keymaps whose bindings count as overlay keys.
Global leader maps are excluded: `?' is a legend for this panel, not
for `SPC a h'."
  (let ((maps (list ghostherd-sidebar-mode-map)))
    (when (fboundp 'evil-get-auxiliary-keymap)
      (dolist (state '(normal emacs motion))
        (when-let* ((aux (ignore-errors
                           (evil-get-auxiliary-keymap
                            ghostherd-sidebar-mode-map state t t))))
          (push aux maps))))
    maps))

(defun ghostherd--sidebar-help-keys (command)
  "Return up to two overlay key descriptions for COMMAND.
Looks only in `ghostherd-sidebar-mode-map' (and Evil's auxiliary
maps on it).  `where-is-internal' with a nil map would also report
`SPC a h n', which is how the old `*ghostherd help*' buffer listed
leader keys and then dismissed the overlay to show them."
  (seq-take
   (delete-dups
    (delq nil
          (mapcan
           (lambda (map)
             (mapcar
              (lambda (key)
                (unless (and (> (length key) 0)
                             (symbolp (aref key 0))
                             (string-suffix-p "-state"
                                              (symbol-name (aref key 0))))
                  (key-description key)))
              (where-is-internal command map nil nil t)))
           (ghostherd--sidebar-help-maps))))
   2))

(defun ghostherd--sidebar-help-text ()
  "Return the overlay key legend as a string."
  (let* ((rows (delq nil
                     (mapcar
                      (lambda (entry)
                        (when-let* ((keys (ghostherd--sidebar-help-keys
                                           (car entry))))
                          (cons (string-join keys ", ") (cdr entry))))
                      ghostherd-sidebar-help-commands)))
         (width (apply #'max 3 (mapcar (lambda (row) (length (car row))) rows)))
         (line-format (format "%%-%ds  %%s" width)))
    (concat
     (mapconcat (lambda (row)
                  (format line-format (car row) (cdr row)))
                rows "\n")
     "\n\n"
     (mapconcat (lambda (state)
                  (format "%s %s"
                          (ghostherd--state-glyph state) state))
                '(blocked working waiting done idle starting dead)
                "  ")
     (format "\n%s detached (nobody attached; RET attaches)"
             ghostherd-detached-glyph))))

(defun ghostherd-sidebar-help ()
  "Toggle the key legend in the overlay preview pane.
Does not dismiss the overlay -- that was the old `*ghostherd help*'
window, which had nowhere to go except by killing the child frame."
  (interactive)
  (setq ghostherd--sidebar-help-visible
        (not ghostherd--sidebar-help-visible))
  (ghostherd--sidebar-draw-preview)
  (when (ghostherd--sidebar-posframe-showing-p)
    (ghostherd--sidebar-show-posframe (current-buffer)))
  (force-mode-line-update t))

(defvar-keymap ghostherd-sidebar-mode-map
  :doc "Keymap for `ghostherd-sidebar-mode'."
  "?" #'ghostherd-sidebar-help
  "e" #'ghostherd-explain
  "R" #'ghostherd-respawn
  "T" #'ghostherd-relay
  "d" #'ghostherd-sidebar-changes
  "c" #'ghostherd-sidebar-notes
  "z" #'ghostherd-sidebar-interrupt
  "Z" #'ghostherd-sidebar-abort
  "a" #'ghostherd-sidebar-answer
  "n" #'next-line
  "p" #'previous-line
  "RET" #'ghostherd-sidebar-visit
  "o" #'ghostherd-sidebar-visit
  "N" #'ghostherd-new
  "P" #'ghostherd-new-pair
  "k" #'ghostherd-sidebar-kill
  "r" #'ghostherd-sidebar-rename
  "m" #'ghostherd-sidebar-message
  "i" #'ghostherd-sidebar-prompt
  "s" #'ghostherd-sidebar-project-filter
  "/" #'ghostherd-sidebar-filter
  "v" #'ghostherd-sidebar-toggle-preview
  "g" #'ghostherd-sidebar-refresh
  "U" #'ghostherd-usage-refresh
  "q" #'ghostherd-sidebar-quit
  "C-g" #'ghostherd-sidebar-quit
  "<escape>" #'ghostherd-sidebar-quit
  "." #'ghostherd-next-blocked
  "H" #'ghostherd-scrollback
  "L" #'ghostherd-log
  "M" #'ghostherd-sidebar-mark-state)

(define-derived-mode ghostherd-sidebar-mode tabulated-list-mode "GhostHerd"
  "Listing of ghostherd agent sessions."
  ;; Padding first: `ghostherd--sidebar-format' budgets against it.
  (setq tabulated-list-padding 1)
  (setq tabulated-list-format (ghostherd--sidebar-format))
  (setq tabulated-list-sort-key (cons "Name" nil))
  (setq truncate-lines t)
  (setq-local mode-line-format '(" " (:eval (ghostherd--sidebar-footer))))
  (hl-line-mode 1)
  (setq-local ghostherd--sidebar-preview-start nil)
  (add-hook 'tabulated-list-revert-hook #'ghostherd--sidebar-entries nil t)
  (add-hook 'post-command-hook #'ghostherd--sidebar-preview-on-command nil t)
  (tabulated-list-init-header)
  (when (fboundp 'evil-normalize-keymaps)
    (evil-normalize-keymaps)))

(defun ghostherd--sidebar-footer ()
  "Mode-line hint row for the session list.
Remaining account quota (claude / grok / agy) sits in front of the
hints when `ghostherd-usage' has a cache."
  (let* ((query ghostherd--sidebar-query)
         (querying (or ghostherd--sidebar-filtering
                       (and query (not (string-empty-p query)))))
         (counts (when querying
                   (format " %d/%d"
                           ghostherd--sidebar-match-count
                           ghostherd--sidebar-total-count)))
         (usage (and (fboundp 'ghostherd-usage-line)
                     (ghostherd-usage-line
                      (ghostherd--sidebar-available-width))))
         (usage (and usage (not (string-empty-p usage)) usage)))
    (concat
     (or usage "")
     (if usage "  ·  " "")
     "GhostHerd"
     (when ghostherd--sidebar-filter-project
       (format " [%s]"
               (file-name-nondirectory
                (directory-file-name ghostherd--sidebar-filter-project))))
     (when querying (concat "  /" query))
     (or counts "")
     (cond
      (ghostherd--sidebar-picking
       "   RET choose  n/p move  Esc cancel")
      (ghostherd--sidebar-filtering
       "   RET apply  n/p move  Esc clear")
      (ghostherd--sidebar-help-visible
       "   ? close help  Esc close")
      (t
       "   RET visit  / filter  v preview  ? keys  Esc close")))))

(defconst ghostherd--sidebar-column-specs
  '((glyph   "S"        2 mandatory)
    (name    "Name"    14 mandatory)
    (state   "State"    8)
    (age     "Age"      5)
    (kind    "Kind"     6)
    (title   "Title"   20)
    (project "Project" 20))
  "Candidate sidebar columns as (KEY HEADER WIDTH [MANDATORY]).

Listed most useful first: `ghostherd--sidebar-visible-columns' takes them
in this order for as long as they fit.  The glyph and the name are
mandatory -- without them there is no list.  `project' comes last because
it is the widest and says least: sessions are usually all in one project,
and the project filter exists for when they are not.")

(defun ghostherd--sidebar-available-width ()
  "Columns the session list actually has: a bound budget, else the window.

`ghostherd--sidebar-target-width' wins when set: the overlay is sized
from the parent frame *before* the child frame exists (or while it is
still the old size during a refit).  Otherwise the live window, so a
side window follows a manual drag.  Last, the configured width."
  (or ghostherd--sidebar-target-width
      (when-let* ((buf (get-buffer "*ghostherd*"))
                  (win (get-buffer-window buf t)))
        (window-body-width win))
      ghostherd-sidebar-width))

(defun ghostherd--sidebar-visible-columns ()
  "Return the column specs that fit the sidebar's available width.

The widths used to be a fixed vector totalling ~70 columns against a
default sidebar width of 36, so every row wrapped.  Fit them to the
window instead, dropping the least useful columns rather than silently
overflowing."
  ;; No floor on the budget: one used to be here, but glyph + name always
  ;; cost less than it, so the mandatory flag below could never fire and a
  ;; deliberately tiny sidebar overflowed anyway.  Let the budget go small
  ;; and let `mandatory' be what guarantees a usable list.
  (let ((budget (1- (ghostherd--sidebar-available-width)))
        (used 0)
        (kept nil))
    (dolist (spec ghostherd--sidebar-column-specs)
      (when (or (not (eq (car spec) 'title)) ghostherd-sidebar-show-title)
        ;; tabulated-list draws `tabulated-list-padding' leading columns and
        ;; a separating space after every column but the last.
        (let ((cost (+ (nth 2 spec) (if kept 1 tabulated-list-padding))))
          (when (or (nth 3 spec) (<= (+ used cost) budget))
            (setq used (+ used cost))
            (push spec kept)))))
    (ghostherd--sidebar-grow-columns (nreverse kept) budget used)))

(defun ghostherd--sidebar-grow-columns (columns budget used)
  "Stretch project/name/title so leftover width is not empty padding.

A wide overlay that still clips Project at 20 characters wasted the
space that made it wide.  Weights prefer the path, then the name."
  (let ((extra (max 0 (- budget used)))
        (weights '((project . 3) (name . 2) (title . 2))))
    (if (zerop extra)
        columns
      (let* ((keys (seq-filter (lambda (k) (assq k columns))
                               (mapcar #'car weights)))
             (total (apply #'+ (mapcar (lambda (k) (alist-get k weights))
                                       keys))))
        (if (zerop total)
            columns
          (let* ((shares
                  (mapcar (lambda (k)
                            (cons k (floor (* extra (/ (float (alist-get k weights))
                                                       total)))))
                          keys))
                 (rest (- extra (apply #'+ (mapcar #'cdr shares)))))
            (when (and (> rest 0) shares)
              (setcdr (car shares) (+ (cdr (car shares)) rest)))
            (mapcar (lambda (spec)
                      (let ((add (or (alist-get (car spec) shares) 0)))
                        (if (zerop add)
                            spec
                          (append (list (nth 0 spec)
                                        (nth 1 spec)
                                        (+ (nth 2 spec) add))
                                  (nthcdr 3 spec)))))
                    columns)))))))

(defun ghostherd--sidebar-format ()
  "Return `tabulated-list-format' for the columns that fit."
  (vconcat (mapcar (lambda (spec)
                     (list (nth 1 spec) (nth 2 spec) t))
                   (ghostherd--sidebar-visible-columns))))

(defun ghostherd--sidebar-sync-format ()
  "Re-fit `tabulated-list-format' when the available width changed.
Called before every rebuild so widening or narrowing the sidebar brings
columns back rather than needing the buffer recreated."
  (when (derived-mode-p 'ghostherd-sidebar-mode)
    (let ((fitted (ghostherd--sidebar-format)))
      (unless (equal fitted tabulated-list-format)
        (setq tabulated-list-format fitted)
        (tabulated-list-init-header)))))

(defun ghostherd--gone-label (gone)
  "The sidebar's word for GONE, a `ghostherd--dir-gone' entry."
  (if (plist-get gone :worktree) "wt gone" "dir gone"))

(defun ghostherd--sidebar-cell (session key state face)
  "Return SESSION's cell for column KEY, given its STATE and FACE."
  (pcase key
    ('glyph   (concat
               (propertize (ghostherd--state-glyph state) 'face face)
               ;; Its directory gone outranks how it is viewed: the mark
               ;; that says so has the spare character, as the state
               ;; column, when it fits, has the words.
               (if-let* ((gone (ghostherd-session-directory-gone session)))
                   (propertize ghostherd-gone-glyph 'face 'error
                               'help-echo (format "its %s %s is gone"
                                                  (if (plist-get gone :worktree) "worktree" "directory")
                                                  (abbreviate-file-name (plist-get gone :path))))
                 ;; Whether anyone is looking is a fact about the *view*,
                 ;; so it is a separate mark rather than a state glyph of
                 ;; its own -- an agent is working or blocked regardless.
                 ;; It belongs in the overview all the same: without it a
                 ;; herd running headless looks exactly like one you are
                 ;; watching.
                 (unless (buffer-live-p (ghostherd-session-buffer session))
                   (propertize ghostherd-detached-glyph 'face face)))))
    ('name    (let ((name (propertize (ghostherd-session-name session) 'face face))
                    (generation (ghostherd-session-generation session)))
                ;; An heir's place in the line of its name, as the herd
                ;; page shows it: rd II took rd's name and work.
                (if (< generation 2)
                    name
                  (propertize (concat name (propertize (concat " " (ghostherd--roman generation))
                                                       'face (list 'bold face)))
                              'help-echo (ghostherd--line-text session)))))
    ('kind    (symbol-name (ghostherd-session-kind session)))
    ('state   (if-let* ((gone (ghostherd-session-directory-gone session)))
                  (propertize (ghostherd--gone-label gone) 'face 'error
                              'help-echo (format "%s; its %s %s is gone" state
                                                 (if (plist-get gone :worktree) "worktree" "directory")
                                                 (abbreviate-file-name (plist-get gone :path))))
                (propertize (symbol-name state) 'face face)))
    ('project (ghostherd--abbreviate (ghostherd-session-project session)))
    ('title   (or (ghostherd--session-title session) ""))
    ('age     (ghostherd--age-string
               (or (ghostherd-session-last-active session)
                   (ghostherd-session-started-at session))))
    (_        "")))

(defun ghostherd--flex-regexp (query)
  "Return a subsequence regexp for QUERY.
Each character is quoted, so `emacs.d` does not become `emacs<any>d`."
  (mapconcat (lambda (ch) (regexp-quote (char-to-string ch)))
             (string-to-list query)
             ".*"))

(defun ghostherd--flex-match-p (query string)
  "Return non-nil if QUERY is a subsequence of STRING, case-insensitively."
  (let ((case-fold-search t))
    (and query string
         (not (string-empty-p query))
         (string-match-p (ghostherd--flex-regexp query) string))))

(defun ghostherd--sidebar-session-haystack (session)
  "Searchable text for SESSION: name, kind, state, project, notes, title."
  (mapconcat #'identity
             (delq nil
                   (list (ghostherd-session-name session)
                         (when-let* ((k (ghostherd-session-kind session)))
                           (symbol-name k))
                         (when-let* ((st (ghostherd-session-state session)))
                           (symbol-name st))
                         (ghostherd-session-project session)
                         (ghostherd--abbreviate
                          (ghostherd-session-project session))
                         (ghostherd-session-notes session)
                         (ignore-errors (ghostherd--session-title session))
                         ;; So `/gone' finds the agents whose directory is.
                         (and (ghostherd-session-directory-gone session)
                              "gone")))
             " "))

(defun ghostherd--sidebar-query-matches-p (session query)
  "Return non-nil if SESSION matches QUERY.

Empty QUERY matches everything.  Whitespace splits into tokens, each
of which must flex-match the haystack -- so `agy idle` is an AND, not
one long subsequence."
  (or (not query)
      (string-empty-p (string-trim query))
      (let ((hay (ghostherd--sidebar-session-haystack session))
            (case-fold-search t))
        (cl-every (lambda (token)
                    (string-match-p (ghostherd--flex-regexp token) hay))
                  (split-string query)))))

(defun ghostherd--sidebar-build-entries ()
  "Rebuild `tabulated-list-entries' from the sessions as they stand.
Pure rendering -- it does not poll, so unlike `ghostherd--sidebar-entries'
it is safe to call from inside the poll path without recursing."
  (ghostherd--sidebar-sync-format)
  (let* ((columns (ghostherd--sidebar-visible-columns))
         (sessions (if ghostherd--sidebar-filter-project
                       (ghostherd-sessions ghostherd--sidebar-filter-project)
                     (ghostherd-sessions)))
         (query ghostherd--sidebar-query)
         (matched (if (and query (not (string-empty-p (string-trim query))))
                      (cl-remove-if-not
                       (lambda (s) (ghostherd--sidebar-query-matches-p s query))
                       sessions)
                    sessions)))
    (setq ghostherd--sidebar-total-count (length sessions)
          ghostherd--sidebar-match-count (length matched)
          tabulated-list-entries
          (mapcar
           (lambda (s)
             (let* ((state (ghostherd-session-state s))
                    (face (ghostherd--state-face state)))
               ;; Cells are generated from the same column list as the
               ;; header, so the two cannot drift apart.
               (list (ghostherd-session-id s)
                     (vconcat
                      (mapcar (lambda (spec)
                                (ghostherd--sidebar-cell
                                 s (car spec) state face))
                              columns)))))
           matched))))

(defun ghostherd--sidebar-entries ()
  "Poll every session, then rebuild `tabulated-list-entries'.
Used by the interactive refresh and `tabulated-list-revert-hook'."
  (ghostherd-poll-all)
  (ghostherd--sidebar-build-entries))

(defun ghostherd--sidebar-erase-preview ()
  "Remove the preview pane, leaving only the tabulated-list rows."
  (when (and ghostherd--sidebar-preview-start
             (marker-position ghostherd--sidebar-preview-start))
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (delete-region ghostherd--sidebar-preview-start (point-max))))
  (setq ghostherd--sidebar-preview-start nil))

(defun ghostherd--sidebar-preview-body (session)
  "Return the snapshot text for SESSION, truncated to the pane."
  (let* ((raw (or (gethash (ghostherd-session-id session) ghostherd--screens)
                  (ignore-errors (ghostherd--host-capture session))
                  ""))
         (raw (if (and raw (not (string-empty-p (string-trim raw))))
                  raw
                "(no screen yet)"))
         (tail (ghostherd--string-tail raw ghostherd-sidebar-preview-lines))
         (width (max 20 (1- (ghostherd--sidebar-available-width)))))
    (when (and raw (not (string-empty-p raw))
               (not (equal raw "(no screen yet)")))
      (puthash (ghostherd-session-id session) raw ghostherd--screens))
    (concat
     ;; Above its screen, in words: what the row's mark means.
     (when-let* ((gone (ghostherd-session-directory-gone session)))
       (concat (propertize
                (truncate-string-to-width
                 (format "%s %s is gone: a respawn is refused; stop it instead"
                         (if (plist-get gone :worktree) "worktree" "directory")
                         (abbreviate-file-name (plist-get gone :path)))
                 width nil nil t)
                'face 'error)
               "\n"))
     (mapconcat (lambda (line)
                  (truncate-string-to-width line width nil nil t))
                (split-string tail "\n")
                "\n"))))

(defun ghostherd--sidebar-draw-preview ()
  "Rebuild the preview pane under the current row."
  (when (derived-mode-p 'ghostherd-sidebar-mode)
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t)
          (session (and (not ghostherd--sidebar-help-visible)
                        (not ghostherd--sidebar-picking)
                        ghostherd-sidebar-show-preview
                        (ghostherd--sidebar-session-at-point)))
          (width (max 10 (or (ignore-errors (window-body-width))
                             (ghostherd--sidebar-available-width)))))
      (save-excursion
        (ghostherd--sidebar-erase-preview)
        (when (or ghostherd-sidebar-show-preview
                  ghostherd--sidebar-help-visible
                  ghostherd--sidebar-picking)
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (setq ghostherd--sidebar-preview-start (point-marker))
          (insert (propertize (make-string width ?─) 'face 'shadow) "\n")
          (insert (cond
                   (ghostherd--sidebar-picking
                    (ghostherd--sidebar-pick-text))
                   (ghostherd--sidebar-help-visible
                    (ghostherd--sidebar-help-text))
                   (session
                    (ghostherd--sidebar-preview-body session))
                   (t (propertize "(no session)" 'face 'shadow))))
          (unless (bolp) (insert "\n"))))
      (restore-buffer-modified-p nil))))

(defun ghostherd--sidebar-print (&optional update)
  "Print the session table and, if enabled, the preview under it."
  (let ((id (tabulated-list-get-id)))
    (ghostherd--sidebar-erase-preview)
    (tabulated-list-print update)
    (when id
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (equal (tabulated-list-get-id) id)))
        (forward-line 1))
      (when (eobp) (goto-char (point-min))))
    (ghostherd--sidebar-draw-preview)
    (setq ghostherd--sidebar-preview-id (tabulated-list-get-id))))

(defun ghostherd--sidebar-confine-point ()
  "Keep point on a table row, not in the preview pane."
  (when (and ghostherd--sidebar-preview-start
             (marker-position ghostherd--sidebar-preview-start)
             (>= (point) ghostherd--sidebar-preview-start)
             (> ghostherd--sidebar-preview-start (point-min)))
    (goto-char (1- ghostherd--sidebar-preview-start))
    (beginning-of-line)))

(defun ghostherd--sidebar-fresh-capture (id)
  "Recapture ID's screen and redraw the preview if still selected."
  (when-let* ((session (ghostherd-get id))
              (buf (get-buffer "*ghostherd*")))
    (when (and (ghostherd--sidebar-on-screen-p)
               (equal id (with-current-buffer buf
                           (ghostherd--sidebar-confine-point)
                           (tabulated-list-get-id))))
      (when-let* ((screen (ignore-errors (ghostherd--host-capture session))))
        (puthash id screen ghostherd--screens)
        (when (equal id ghostherd--sidebar-preview-id)
          (with-current-buffer buf
            (ghostherd--sidebar-draw-preview)
            (when (and (ghostherd--sidebar-posframe-showing-p)
                       (fboundp 'posframe-refresh))
              (posframe-refresh buf))))))))

(defun ghostherd--sidebar-schedule-fresh-capture (id)
  "Debounce a recapture of ID so holding j/k does not fork per row."
  (when (timerp ghostherd--sidebar-preview-timer)
    (cancel-timer ghostherd--sidebar-preview-timer))
  (setq ghostherd--sidebar-preview-timer
        (run-with-timer 0.2 nil #'ghostherd--sidebar-fresh-capture id)))

(defun ghostherd--sidebar-preview-on-command ()
  "Follow point: draw the cached snapshot, then recapture shortly."
  (when (derived-mode-p 'ghostherd-sidebar-mode)
    (ghostherd--sidebar-confine-point)
    (when (and ghostherd-sidebar-show-preview
               (not ghostherd--sidebar-help-visible))
      (let ((id (tabulated-list-get-id)))
        (unless (equal id ghostherd--sidebar-preview-id)
          (setq ghostherd--sidebar-preview-id id)
          (ghostherd--sidebar-draw-preview)
          (when id
            (ghostherd--sidebar-schedule-fresh-capture id)))))))

(defun ghostherd-sidebar-toggle-preview ()
  "Toggle the snapshot pane under the session list."
  (interactive)
  (setq ghostherd-sidebar-show-preview
        (not ghostherd-sidebar-show-preview))
  (setq ghostherd--sidebar-preview-id nil)
  (ghostherd--sidebar-print t)
  (when (ghostherd--sidebar-posframe-showing-p)
    (ghostherd--sidebar-show-posframe (current-buffer)))
  (message "Preview %s" (if ghostherd-sidebar-show-preview "on" "off")))

(defun ghostherd--sidebar-refresh ()
  "Re-render the sidebar from current session state, without polling.
Rebuilding the entries is the point: `tabulated-list-print' alone
re-prints whatever `tabulated-list-entries' already held, so a state
change would redraw the same stale row it drew last time."
  (when-let* ((buf (get-buffer "*ghostherd*")))
    (with-current-buffer buf
      (when (derived-mode-p 'ghostherd-sidebar-mode)
        (ghostherd--sidebar-build-entries)
        (ghostherd--sidebar-print t)
        (when (and (ghostherd--sidebar-posframe-showing-p)
                   (fboundp 'posframe-refresh))
          (posframe-refresh buf))))))

(defun ghostherd--sidebar-on-screen-p ()
  "Return non-nil when the session list is actually visible.

An invisible posframe still has a window, so `get-buffer-window'
is not enough: Age would keep being rewritten after Esc."
  (when-let* ((buf (get-buffer "*ghostherd*")))
    (cl-some (lambda (win)
               (frame-visible-p (window-frame win)))
             (get-buffer-window-list buf nil t))))

(defun ghostherd--poll-tick ()
  "Timer callback: poll every session, then keep a visible sidebar current.
A state change already redraws via `ghostherd--set-state', but the Age
column advances with no state change at all, so refresh on each tick
while the sidebar is actually on screen.

Sidecar mail/Telegram is not on this timer.  `url-retrieve' on every
poll hitchs typing; that path is `ghostherd--herd-tick-timer'."
  (ghostherd-poll-all)
  ;; Housekeeping on a cheap cadence: drop the turn-end cache's dead
  ;; transcripts even when no agent was removed (a live one may have moved
  ;; to a new conversation).
  (when (zerop (% (cl-incf ghostherd--poll-count) ghostherd--turn-ends-prune-every))
    (ghostherd--prune-turn-ends))
  ;; Whose directory is gone: a stat each, once a minute.
  (with-demoted-errors "ghostherd: looking at the agents' directories: %S"
    (ghostherd--check-directories))
  (when (ghostherd--sidebar-on-screen-p)
    (ghostherd--sidebar-refresh)))


;;; Sidecar herd mail (agents POST /jsonrpc; Emacs delivers to PTYs)

(defvar ghostherd--herd-ack-ids nil
  "Mail ids delivered this cycle, acked on the next `herd_tick'.")

(defvar ghostherd--herd-replies nil
  "Command replies to send on the next `herd_tick'.")

(defvar ghostherd--herd-tick-inflight nil
  "Non-nil while an async `herd_tick' is in flight.")

(defcustom ghostherd-herd-tick-idle 0.8
  "Idle seconds before a sidecar `herd_tick', and seconds between them.

The poll timer runs while you type, so putting HTTP on it made
keystrokes compete with `url-retrieve'.  A tick waits until Emacs has
been idle this long, and keeps coming while it stays idle.  An idle
timer does not: it fires once per spell of idleness, so an Emacs left
alone stopped delivering the herd's mail until someone touched it ---
and two agents asking each other in an empty room stopped with it."
  :type 'number
  :group 'ghostherd)

(defvar ghostherd--herd-tick-timer nil
  "Timer that drives `ghostherd--herd-tick-async' while Emacs is idle.")

(defun ghostherd--herd-tick-when-idle ()
  "Tick the sidecar, unless you are typing."
  (let ((idle (current-idle-time)))
    (when (and idle (>= (float-time idle) ghostherd-herd-tick-idle))
      (ghostherd--herd-tick-async))))

(defun ghostherd--ensure-herd-tick-timer ()
  "Start the idle sidecar tick if `ghostherd-mode' is on."
  (unless (timerp ghostherd--herd-tick-timer)
    (setq ghostherd--herd-tick-timer
          (run-at-time ghostherd-herd-tick-idle ghostherd-herd-tick-idle
                       #'ghostherd--herd-tick-when-idle))))

(defun ghostherd--stop-herd-tick-timer ()
  (when (timerp ghostherd--herd-tick-timer)
    (cancel-timer ghostherd--herd-tick-timer)
    (setq ghostherd--herd-tick-timer nil)))

(defconst ghostherd--herd-protocol "\
Inter-agent mail on this Emacs's ghostherd sidecar.

Environment:
  GHOSTHERD_SESSION  this agent's name
  GHOSTHERD_RPC      POST JSON-RPC 2.0 here; the URL carries a token,
                     so use the variable and do not print its value
  rpc.url            same URL, in this directory, if the env is stale
                     (a 401 means it is: the sidecar now wants a token)

curl -sS \"$GHOSTHERD_RPC\" -H 'Content-Type: application/json' \\
  -d '{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"herd_list\",\"params\":{}}'

{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"herd_message\",\"params\":{
   \"from\":\"<GHOSTHERD_SESSION>\",
   \"to\":\"grok-dev\",
   \"body\":\"Findings:\\n- ...\",
   \"handoff\":true}}

Emacs pastes into the target PTY.  If that agent is working, the
sidecar holds the message until it is idle.  Overlay m does not
use this queue.  Long bodies: method herd_inbox, params session=<name>.

Asks want an answer back.  $GHOSTHERD_HERD is a client for them:

  \"$GHOSTHERD_HERD\" ask agy 'Fetch X and sum it up' --wait
  \"$GHOSTHERD_HERD\" reply <id> 'the answer'

Text given as `-', or left out, is read from stdin.

`to' is a name, or a kind (claude, grok, agy) for that kind's agent
in your project.  --wait prints the answer when it comes; without it
the answer comes back to you as a message once you are idle.  An
agent answering an ask may hand parts of it on, three asks deep at
most, but never to one waiting on it; a kind never picks a project's
lead.  And it may ask any one agent only so many times for it: what
the asker gave as --rounds, 5 when it gave none -- one nobody asked,
until it finishes a turn with no ask open.  Methods: herd_ask {to,
body, from, wait, rounds}, herd_reply {id, body, from}, herd_await {id,
timeout}, herd_cancel {id}, herd_asks.

  \"$GHOSTHERD_HERD\" handoff unnoiac 'what unnoiac must change, and why' --wait

A handoff is an ask of another project's lead, PROJECT by path, name or
store slug; the herd frames it, and starts a lead there when it has
none.  A lead hands off, or an agent in a project with no lead; one
under a lead is refused and tells its lead.  Method: herd_handoff
{project, body, from, wait, rounds}.

`primary' is the herd-wide lead: the user's entry point for what crosses
projects.  It hands work off as above; no agent asks it -- the herd
refuses -- and it asks no project's agents.

Hooks: `\"$GHOSTHERD_HERD\" hook [EVENT]' reads a CLI hook's JSON on
stdin and posts herd_report {session, state, reason, cli,
conversation, transcript, last}.  `herd hooks install' wires it in.
")

;;;###autoload
(defun ghostherd-install-hooks (&optional uninstall)
  "Wire `herd hook' into claude, grok and agy, after showing the change.
claude and grok share ~/.claude/settings.json; agy reads
~/.gemini/config/hooks.json.  The commands do nothing in a CLI the
herd did not start, so every session of each CLI can carry them.
With prefix UNINSTALL, take them out again."
  (interactive "P")
  (let* ((client (or (and (fboundp 'ghostherd-herd-client) (ghostherd-herd-client))
                     (user-error "No bin/herd next to ghostherd")))
         (action (if uninstall "uninstall" "install"))
         (run (lambda (&rest args)
                (with-temp-buffer
                  (cons (apply #'call-process client nil t nil "hooks" action args)
                        (string-trim (buffer-string)))))))
    (pcase-let ((`(,status . ,plan) (funcall run "--dry-run")))
      (unless (eq status 0)
        (user-error "herd hooks %s: %s" action plan))
      (with-help-window "*ghostherd hooks*"
        (princ plan))
      (when (yes-or-no-p (format "%s ghostherd's hooks as shown? " (capitalize action)))
        (pcase-let ((`(,status . ,out) (funcall run)))
          (message "%s%s" out (if (eq status 0) "" "  (failed)")))))))

(defun ghostherd--herd-write-protocol ()
  "Refresh PROTOCOL.md next to rpc.url."
  (let ((dir (expand-file-name "ghostherd-mail" user-emacs-directory)))
    (make-directory dir t)
    (let ((coding-system-for-write 'utf-8))
      (with-temp-file (expand-file-name "PROTOCOL.md" dir)
        (insert ghostherd--herd-protocol)))))

(defun ghostherd--herd-snapshot ()
  "Session list pushed to the sidecar each poll.
Includes the last captured pane so Telegram `screen' can reply
from sqlite instead of waiting for another Emacs round trip, and the
address of each one's room from your phone, for the bot to link to."
  (mapcar
   (lambda (s)
     (let* ((blocked (eq (ghostherd-session-state s) 'blocked))
            (err (and blocked (ghostherd--cli-error s)))
            (url (and blocked (ghostherd--login-url s)))
            (signed-in (and blocked (ghostherd--login-done s)))
            (continue-at (and blocked (ghostherd--sleep-continue-at s)))
            (words (and blocked (ghostherd--continue-words s)))
            (wstat (and (ghostherd-session-worktree s) (ghostherd--worktree-stat s))))
      (list :name (ghostherd-session-name s)
           :kind (symbol-name (or (ghostherd-session-kind s) 'shell))
           :state (symbol-name (or (ghostherd-session-state s) 'idle))
           :notes (or (ghostherd-session-notes s) "")
           :project (or (ghostherd--project-name (ghostherd-session-project s)) "")
           :room (or (when-let* ((p (ghostherd--project-name (ghostherd-session-project s))))
                       (ghostherd--room-url p))
                     "")
           :role (or (ghostherd-session-role s) "")
           :reason (or (ghostherd-session-state-reason s) "")
           :since (let ((t0 (or (ghostherd-session-last-active s)
                                (ghostherd-session-started-at s))))
                    (if t0 (* 1000 (float-time t0)) 0))
           :prompt (ghostherd--prompt-json (ghostherd-session-prompt s))
           ;; A turn killed by an expired login: the card offers a one-click
           ;; /login.  Only while blocked on it, so the button is there
           ;; exactly when it would work -- not once /login is under way,
           ;; where it would be typed as the code or eat the last Return.
           :login (if (and (eq (cdr err) 'login) (not url) (not signed-in)) t :json-false)
           ;; /login's last step, signed in: the card offers the Return it
           ;; waits on (`ghostherd-login-continue').
           :loginContinue (if signed-in t :json-false)
           ;; Signed in again, its turn still stopped at the expired login:
           ;; the card offers Continue (`ghostherd-carry-on') -- once the
           ;; sign-in's last Return is pressed.
           :carryOn (if (and (eq (cdr err) 'carry-on) (not signed-in)) t :json-false)
           ;; A turn killed by the Mac sleeping or a dropped connection: the
           ;; herd continues it itself.  `continueAt' is when (float ms), for
           ;; the card's "continuing after sleep in Ns" countdown and its
           ;; Cancel; `continueStopped' is t once it has stopped retrying.
           ;; `continueAfter' / `continueAgain' are the cause's words for
           ;; each (`ghostherd--continue-causes').
           :continueAt (if continue-at (* 1000 continue-at) 0)
           :continueStopped (if (ghostherd--sleep-continue-stopped-p s) t :json-false)
           :continueAfter (or (car words) "")
           :continueAgain (or (cdr words) "")
           ;; The /login URL to open/copy from the card, while it waits on
           ;; the code.
           :url (or url "")
           ;; Where it works and the branch it is on, so the lead can find
           ;; and read its work.  A worktree agent is in its own directory
           ;; on its own branch (from the recipe, no per-poll git); the
           ;; others share the project's, so their branch is left blank.
           :directory (let ((d (or (plist-get (ghostherd-session-worktree s) :path)
                                   (ghostherd-session-project s))))
                        (if d (ghostherd--abbreviate d) ""))
           :branch (or (plist-get (ghostherd-session-worktree s) :branch) "")
           ;; That directory is gone -- a worktree removed outside the herd:
           ;; `herd list' says so, for its lead to stop it.
           :directoryMissing (if (ghostherd-session-directory-gone s) t :json-false)
           ;; Who started it through `herd start', and where its worktree
           ;; branch stands -- for `herd list' and the card's "started by".
           :startedBy (or (ghostherd-session-started-by s) "")
           :ahead (if wstat (plist-get wstat :ahead) 0)
           :merged (if (and wstat (plist-get wstat :merged)) t :json-false)
           ;; The process it lives under -- its tmux pane's, its ghostel
           ;; shell -- so the herd takes a hook's report as its own only
           ;; from inside it, not from a CLI a server it started runs.
           :hostPid (or (ghostherd--host-pid s) 0)
           ;; Behind the herd's guard: what it asks of others is marked so.
           :guarded (if (ghostherd--session-guarded-p s) t :json-false)
           :screen (or (gethash (ghostherd-session-id s) ghostherd--screens)
                       ""))))
   (hash-table-values ghostherd--sessions)))

(defun ghostherd--herd-deliver-one (msg)
  "Paste one sidecar mail MSG through `ghostherd-message' / `handoff'.
Mail is not the user's prompt -- an agent's message, the herd's note --
so the turn it starts is marked `mail' (`ghostherd--turn-from-ask'),
and its end is no report to the user.  Save what the sidecar says
otherwise (its `turn'): \"user\", the user's own words (a prompt from
Telegram), is the user's turn as any prompt is; \"carry\", the answer to
an ask the taker made or the user's verdict on a call its guard
refused, carries on the work it was on, and its turn is whoever's the
last one was."
  (let* ((from (or (plist-get msg :from) "unknown"))
         (to (plist-get msg :to))
         (body (or (plist-get msg :body) ""))
         (handoff (plist-get msg :handoff))
         (ask (plist-get msg :ask))
         (turn (plist-get msg :turn))
         (submit (if (plist-member msg :submit)
                     (plist-get msg :submit)
                   t)))
    ;; Before the paste, which moves the taker to `working' at once (see
    ;; `ghostherd--ask-deliver'); none when it is not submitted -- the
    ;; Return pressed on it later is someone's own -- nor from a sidecar
    ;; that does not say yet (no `turn'; until it is restarted).
    (when (and (not ask) (or handoff submit) (plist-member msg :turn)
               (not (equal turn "user")))
      (ghostherd--turn-from-ask to (if (equal turn "carry")
                                       (or (ghostherd--turn-origin-of to) 'mail)
                                     'mail)))
    (cond
     (ask (ghostherd--ask-deliver ask from to body (plist-get msg :rounds)
                                  (eq (plist-get msg :guarded) t)))
     (handoff (ghostherd-handoff to body from))
     (t (ghostherd-message from to body :submit submit)))))

(defun ghostherd--herd-held-p (msg)
  "Non-nil when MSG's taker cannot take a paste now: starting, on a menu,
or with no input box at its foot.  A menu -- a trust prompt, a
permission prompt -- takes no paste, and reads the Return pressed after
it as its answer.  The sidecar holds mail by the state Emacs last
ticked; this is for a state that changed since, and for a dialog no
rule reads: the box missing is enough (`ghostherd--off-prompt-p')."
  (when-let* (((not (plist-get msg :handoff)))
              (to (ghostherd-get (plist-get msg :to))))
    (or (memq (ghostherd-session-state to) '(starting blocked))
        (ghostherd--off-prompt-p to))))

(defun ghostherd--herd-deliver (result)
  "Deliver pending mail from a `herd_tick' RESULT plist.
Mail `ghostherd--herd-held-p' holds is not acknowledged: it comes again
on a later tick."
  (dolist (msg (plist-get result :pending))
    (let ((id (plist-get msg :id)))
      (unless (ghostherd--herd-held-p msg)
        (condition-case err
            (ghostherd--herd-deliver-one msg)
          (error
           (message "ghostherd herd mail %s: %s"
                    id (error-message-string err))))
        (when id
          (push id ghostherd--herd-ack-ids))))))


;;; Asks: mail that wants an answer
;;
;; The sidecar keeps the ask; Emacs pastes it, with the line that
;; answers it, and closes it when the taker stops without answering.
;; A taker's own `herd reply' happens before it goes idle -- it is a
;; command it runs during its turn -- so by the time a settled state
;; reaches Emacs a real answer is already in, and the sidecar keeps it.

(defcustom ghostherd-ask-screen-lines 60
  "Screen lines sent back as the answer when a taker forgets to answer."
  :type 'integer
  :group 'ghostherd)

(defvar ghostherd--asks-open (make-hash-table :test 'equal)
  "Asks delivered and not yet closed: ask id -> taker's session id.")

(defvar ghostherd--asks-settled (make-hash-table :test 'equal)
  "Ask ids this Emacs has closed, so a stale tick does not reopen them.")

(defvar ghostherd--relay-held (make-hash-table :test 'equal)
  "Ask id -> the name whose heir answers it, until the heir is told:
see `ghostherd-relay'.  Not open meanwhile: an heir sitting at its first
prompt has not stopped answering it.")

(defvar ghostherd--herd-asks nil
  "Open and recent asks from the last `herd_tick', newest first.")

(defvar ghostherd--herd-asks-taken nil
  "Non-nil once a `herd_tick' has given this Emacs the herd's asks: an
ask new after that is new to the herd, not to this Emacs.")

(defun ghostherd--ask-head (text)
  "First non-blank line of TEXT, shortened for the log."
  (let ((line (or (seq-find (lambda (l) (not (string-blank-p l)))
                            (split-string (or text "") "\n"))
                  "")))
    (truncate-string-to-width (string-trim line) 80 nil nil "…")))

(defun ghostherd--ask-text (id body &optional rounds lead)
  "BODY as the taker sees ask ID: the work, then how to answer it.
The answer line is spelled out in full, the client by its absolute
name, so a taker needs no skill installed and no PATH of ours.
ROUNDS, when the sidecar says, is how many times the taker may ask
any one agent for it.  Last, what to do with work another project
needs: a LEAD hands it to that project's lead; any other taker puts it
in its answer, for whoever asked to take on -- neither edits that
project's checkout."
  (format "[ask %s -- whoever asked is waiting for your answer]\n%s\n\n\
When you are done, send your answer with:\n\
%s reply %s <<'HERD'\n\
<your answer>\n\
HERD\n\
Answer even if you could not do it, and say why.  You may ask other\n\
agents for parts of it, but not whoever is waiting on you%s.\n\
%s"
          id body
          (shell-quote-argument (or (and (fboundp 'ghostherd-herd-client)
                                         (ghostherd-herd-client))
                                    "herd"))
          id
          (if (natnump rounds)
              (format ",\nand any one of them %d time%s at most" rounds (if (= rounds 1) "" "s"))
            "")
          (if lead
              "A change another project needs is not yours to make in its checkout:\n\
hand it to that project's lead (\"$GHOSTHERD_HERD\" handoff PROJECT)."
            "A change another project needs is not yours to make in its checkout:\n\
say it in your answer -- what, where, why -- and whoever asked takes it on.")))

;;; How a turn began: the user's own prompt, or another agent's ask.
;;
;; A worker's turn that the user started -- their prompt from the card,
;; the room or chat composer, Telegram, or typed straight into the
;; terminal -- ends as an Inbox item: its report to the user (a Question
;; when it ends by asking, else a Report).  A turn that an ask from
;; another agent started stays out: whoever asked reports.  So does one
;; the herd's other mail started -- an agent's message, the herd's note
;; that an ask was cancelled -- save the user's own words it carries
;; (`ghostherd--herd-deliver-one').  The herd knows a turn is from its
;; own delivery (`ghostherd--ask-deliver', `ghostherd--herd-deliver-one');
;; every other turn start is the user's.  Leads and primary are
;; unaffected -- their turn end is surfaced by role, as before.

(defvar ghostherd--turn-origin (make-hash-table :test 'equal)
  "Session id -> how its current or last turn began: `ask' (the herd
delivered it another agent's ask), `mail' (the herd's other mail) or
`user' (the user's own prompt).  Read to decide whether a worker's turn
end is its report to the user.")

(defvar ghostherd--turn-ask-pending (make-hash-table :test 'equal)
  "Session id -> the origin the turn the herd's delivery starts takes
(`ask', `mail', or a turn's origin carried on), until that turn is
settled; (submitted . ORIGIN) once its CLI has the prompt.  The next
turn start is marked so.")

(defun ghostherd--turn-from-ask (session &optional origin)
  "Mark that SESSION's next turn begins from an ask the herd delivered --
or, with ORIGIN, from other mail it delivered, marked ORIGIN."
  (when-let* ((s (ghostherd-get session)))
    (puthash (ghostherd-session-id s) (or origin 'ask) ghostherd--turn-ask-pending)))

(defun ghostherd--turn-pending-origin (pending)
  "The origin PENDING, a `ghostherd--turn-ask-pending' value, gives its
turn (t, from before the table held one, is an ask's)."
  (cond ((consp pending) (cdr pending))
        ((memq pending '(t submitted)) 'ask)
        (t pending)))

(defun ghostherd--turn-origin-of (session)
  "How SESSION's current or last turn began: `ask', `user', or nil."
  (when-let* ((s (ghostherd-get session)))
    (gethash (ghostherd-session-id s) ghostherd--turn-origin)))

(defun ghostherd--turn-origin-on-state (session old new)
  "Settle SESSION's turn origin as a turn starts (OLD resting, NEW busy).
A turn the herd started by delivering an ask (`ghostherd--turn-ask-pending')
is `ask', one its other mail started `mail' (or the origin it carries
on); any other turn start -- a user prompt through the herd, or one
typed straight into the terminal -- is `user'.  A mid-turn move into a
busy state (OLD already busy) is not a turn start and is left alone, so
an ask turn that blocks to ask is not re-marked.

The ask's flag is spent only once its prompt is known to have reached
the CLI (`ghostherd--turn-submitted').  The herd moves a taker to
`working' the moment it sends; when the Return was lost, that was no
turn, and the real one starts later -- once Return is pressed again,
by the herd or by hand -- and is still the ask's."
  (when (and (memq new '(working blocked waiting))
             (memq old '(idle done dead nil)))
    (let* ((id (ghostherd-session-id session))
           (pending (gethash id ghostherd--turn-ask-pending)))
      (if pending
          (progn (puthash id (ghostherd--turn-pending-origin pending) ghostherd--turn-origin)
                 (when (or (consp pending) (eq pending 'submitted)
                           (not (ghostherd--submit-pending-p session)))
                   (remhash id ghostherd--turn-ask-pending)))
        (puthash id 'user ghostherd--turn-origin)))))

(defun ghostherd--turn-submitted (session)
  "SESSION's CLI took the prompt the herd sent it: an ask's flag is spent
on the turn it starts -- the one under way, else the next to start."
  (let* ((id (ghostherd-session-id session))
         (pending (gethash id ghostherd--turn-ask-pending))
         (origin (and pending (ghostherd--turn-pending-origin pending))))
    (when pending
      (if (memq (ghostherd-session-state session) '(working blocked waiting))
          (progn (remhash id ghostherd--turn-ask-pending)
                 (puthash id origin ghostherd--turn-origin))
        (puthash id (cons 'submitted origin) ghostherd--turn-ask-pending)))))

(add-hook 'ghostherd-state-change-hook #'ghostherd--turn-origin-on-state)

(defun ghostherd--turn-origin-forget (session)
  "Drop SESSION's turn-origin record when it is gone."
  (let ((id (ghostherd-session-id session)))
    (remhash id ghostherd--turn-origin)
    (remhash id ghostherd--turn-ask-pending)))

(add-hook 'ghostherd-session-removed-hook #'ghostherd--turn-origin-forget)

(defun ghostherd--ask-deliver (id from to body &optional rounds guarded)
  "Paste ask ID from FROM into TO, and watch for TO to stop.
ROUNDS is how many times TO may ask any one agent for it.  GUARDED: FROM
runs behind the herd's guard -- BODY says so to TO, and the log does.  A lead owed
its role -- started for a handoff on a CLI that takes none as it starts
-- reads it after the ask: before it, the ask would not read as one in
the room's chat (server/conversation.py), which stops at its answer
line."
  (let* ((session (or (ghostherd-get to)
                      (user-error "Unknown target session")))
         (role (ghostherd--role-take session)))
    ;; BEFORE the message: sending it moves the taker to `working'
    ;; synchronously (`ghostherd-send'), which runs the turn-origin hook --
    ;; set after, the hook would see no pending ask and mark the turn the
    ;; user's.  The turn this ask starts is from an ask, not the user: its
    ;; end is not the taker's report to the user (whoever asked reports).
    (ghostherd--turn-from-ask session)
    (ghostherd-message from session
                       (concat (ghostherd--ask-text id body rounds
                                                    (equal (ghostherd-session-role session) "lead"))
                               (and role (concat "\n\n---\n\nYour role in this herd, given with \
your first message as your CLI takes none as it starts:\n\n" role)))
                       :submit t)
    (puthash id (ghostherd-session-id session) ghostherd--asks-open)
    (ghostherd--log-add session 'ask
                        (format "ask %s from %s%s: %s" id from
                                (if guarded " (a guarded agent)" "")
                                (ghostherd--ask-head
                                 (if guarded
                                     (replace-regexp-in-string "\\`\\[From a guarded agent:[^]]*\\]\\s-*" "" body)
                                   body))))))

(defun ghostherd--ask-settle (id session dead &optional error)
  "Tell the sidecar the taker of ask ID stopped, or died when DEAD.
With ERROR, it never got the ask, and that says why.  The sidecar
ignores this for an ask already answered, and leaves it open while the
taker waits on asks of its own: it ended its turn to wait, not to stop.
Such an ask is watched again, for the next time it stops."
  (remhash id ghostherd--asks-open)
  (puthash id t ghostherd--asks-settled)
  (let ((screen (and (not dead) (not error)
                     (ignore-errors
                       (ghostherd--host-capture session ghostherd-ask-screen-lines)))))
    (when (fboundp 'ghostherd-memory-request-async)
      (ghostherd-memory-request-async
       "herd_settle"
       (lambda (r)
         (when (equal (plist-get r :status) "delivered")
           (remhash id ghostherd--asks-settled)
           (puthash id (ghostherd-session-id session) ghostherd--asks-open)
           ;; Watched again: `too_soon' is the sidecar holding it open until
           ;; the taker has really worked on it after delivery -- the next
           ;; stop, once it has, closes it.
           (ghostherd--log-add session 'ask
                               (cond
                                ((plist-get r :too_soon)
                                 (format "ask %s left open: %s was only just handed it"
                                         id (ghostherd-session-name session)))
                                ;; Its screen read idle, but its hook has not
                                ;; said the turn ended (nor its transcript):
                                ;; a view scrolled back, say.  Its turn's end
                                ;; closes it.
                                ((plist-get r :turn_on)
                                 (format "ask %s left open: %s's turn has not ended by its hook or transcript"
                                         id (ghostherd-session-name session)))
                                ;; The sidecar holds a failure until this Emacs
                                ;; has picked its herd up, then is asked again.
                                ((plist-get r :unrestored)
                                 (format "ask %s left open: the herd is not restored yet" id))
                                (t
                                 (format "ask %s left open: %s waits on %s"
                                         id (ghostherd-session-name session)
                                         (mapconcat #'identity (plist-get r :waiting_on) ", ")))))))
       (append (list :id id
                     :screen (string-trim-right (or screen ""))
                     :dead (if dead t :json-false))
               (and error (list :error error)))
       (lambda (err) (message "ghostherd: closing ask %s: %s" id err))))))

(defun ghostherd--ask-in (session text)
  "The open ask SESSION was handed in TEXT, or nil."
  (seq-find (lambda (id) (string-search (format "[ask %s " id) text))
            (ghostherd--asks-of session)))

(defun ghostherd--asks-of (session)
  "Ids of the open asks SESSION is answering."
  (let ((sid (ghostherd-session-id session))
        ids)
    (maphash (lambda (id taker) (when (equal taker sid) (push id ids)))
             ghostherd--asks-open)
    ids))

(defun ghostherd--ask-unsent-p (session)
  "Non-nil when an ask still sits unsent in SESSION's input box."
  (when-let* ((screen (ignore-errors (ghostherd--host-capture session 20))))
    (ghostherd--unsent-p screen (substring ghostherd-message-template 0
                                          (string-search "%s" ghostherd-message-template)))))

(defun ghostherd--ask-not-taken (session)
  "The open ask in the text the herd last sent SESSION, while that text
is not known taken: no turn begun on it yet, of an agent whose hooks
say when one does -- the submit check waits for it, and ends by
confirming the text or failing the ask (`ghostherd--confirm-submit')."
  (when-let* ((sub (ghostherd--submit-of session))
              ((eq (plist-get sub :state) 'pending))
              ((ghostherd--hooked-p session)))
    (ghostherd--ask-in session (plist-get sub :text))))

(defun ghostherd--ask-on-state (session _old new)
  "A taker that stopped closes its open ask -- unless it never started.
Delivery marks the taker `working' itself; when the Return was lost the
ask is still in its input box, and the taker goes idle without having
read it.  Its screen then answers nothing, so the ask stays open.  So
too while the text that carried it is not known taken: the submit check
settles that, failing the ask if no turn ever began on it."
  (when (memq new '(idle done dead))
    (let ((ids (ghostherd--asks-of session)))
      (when-let* (((not (eq new 'dead)))
                  (id (ghostherd--ask-not-taken session))
                  ((member id ids)))
        (ghostherd--log-add session 'ask
                            (format "ask %s left open: %s has begun no turn on it yet"
                                    id (ghostherd-session-name session)))
        (setq ids (delete id ids)))
      (when (and ids (not (eq new 'dead)) (ghostherd--ask-unsent-p session))
        (ghostherd--log-add session 'ask
                            (format "ask %s is still in its input box: left open"
                                    (string-join ids ", ")))
        (setq ids nil))
      (dolist (id ids)
        (ghostherd--ask-settle id session (eq new 'dead))))))

(defun ghostherd--ask-on-removed (session)
  "A taker killed outright fails its open ask."
  (dolist (id (ghostherd--asks-of session))
    (ghostherd--ask-settle id session t)))

(add-hook 'ghostherd-state-change-hook #'ghostherd--ask-on-state)
(add-hook 'ghostherd-session-removed-hook #'ghostherd--ask-on-removed)

(defun ghostherd--ask-outcome (ask)
  "Log text for ASK, a `herd_tick' plist that has just closed."
  (let ((id (plist-get ask :id))
        (from (plist-get ask :from)))
    (pcase (plist-get ask :status)
      ("answered" (if (plist-get ask :auto)
                      (format "stopped without answering ask %s; %s got %s"
                              id from
                              (if (equal (plist-get ask :auto_source) "message")
                                  "its last message"
                                "the screen"))
                    (format "answered ask %s for %s: %s" id from
                            (or (plist-get ask :reply_head) ""))))
      ("failed" (format "ask %s from %s failed: %s" id from
                        (or (plist-get ask :error) "")))
      (status (format "ask %s from %s %s" id from status)))))

(defun ghostherd--herd-take-asks (asks)
  "Keep ASKS from a `herd_tick': log what closed, adopt what is ours.
Adopting is for asks delivered before this Emacs started: watched
from here on, and closed now if the taker is already sitting idle."
  (let ((before (make-hash-table :test 'equal)))
    (dolist (a ghostherd--herd-asks)
      (puthash (plist-get a :id) (plist-get a :status) before))
    (dolist (a asks)
      (let* ((id (plist-get a :id))
             (status (plist-get a :status))
             (was (gethash id before)))
        (when (and was (not (equal was status))
                   (member status '("answered" "failed" "cancelled")))
          (remhash id ghostherd--asks-open)
          ;; A handoff that failed before it had a taker: its asker's.
          (ghostherd--log-add (if (equal (plist-get a :to) "") (plist-get a :from) (plist-get a :to))
                              'ask (ghostherd--ask-outcome a)))
        ;; Failed since the last tick -- or before it, when the ask is new
        ;; to a herd that has seen asks already, not at Emacs's start.
        (when-let* (((equal status "failed"))
                    ((not (equal was "failed")))
                    ((or was ghostherd--herd-asks-taken))
                    (asker (ghostherd-get (plist-get a :from))))
          (ghostherd--push 'ask-failed asker :ask id
                           :to (let ((to (or (plist-get a :to) "")))
                                 (if (and (equal to "") (plist-get a :project))
                                       (format "%s's lead" (file-name-nondirectory
                                                            (directory-file-name (plist-get a :project))))
                                     to))
                           :error (or (plist-get a :error) "") :head (or (plist-get a :head) "")))
        (when-let* (((equal status "delivered"))
                    ((not (gethash id ghostherd--asks-open)))
                    ((not (gethash id ghostherd--asks-settled)))
                    ;; Its taker's heir is not told it yet: at its first
                    ;; prompt, it has not stopped answering it.
                    ((not (gethash id ghostherd--relay-held)))
                    (session (ghostherd-get (plist-get a :to))))
          (puthash id (ghostherd-session-id session) ghostherd--asks-open)
          (when (memq (ghostherd-session-state session) '(idle done dead))
            (ghostherd--ask-settle id session
                                   (eq (ghostherd-session-state session) 'dead))))))
    (setq ghostherd--herd-asks asks
          ghostherd--herd-asks-taken t)))

;;; A project's lead
;;
;; One agent per project plans the work and hands it out: you talk to
;; it in the project's chat, it asks the others, and what it says at
;; the end of a turn is its report.  It is an ordinary agent with a
;; role -- its own CLI, transcript and column -- not a bot of the
;; sidecar's.  It starts the first time you write to it.

(defcustom ghostherd-lead-kind 'claude
  "The CLI a project's lead runs on.
Its model is the CLI's to change: tell a claude lead `/model sonnet'."
  :type 'symbol
  :group 'ghostherd)

(defconst ghostherd--memory-rules "\
The herd keeps shared memory for this project -- curated facts every
agent of the project sees, whatever its CLI, and the user's own
preferences across projects.  It is the source of truth: use it, not
your own memory.  Read it with \"$GHOSTHERD_HERD\" recall (no argument
prints the index; a query prints the matching facts in full).  Add a
fact with \"$GHOSTHERD_HERD\" remember [--type project|feedback|reference],
update one with remember --name NAME (when remember says your text
restates a fact, it tells you what to add), and drop one with
\"$GHOSTHERD_HERD\" forget NAME.  What this project's past sessions
said: \"$GHOSTHERD_HERD\" search WORDS."
  "Shared-memory rules, given to a grok or agy agent as it starts.")

(defun ghostherd--memory-link-claude (session)
  "Point a claude SESSION's memory dir at its project's shared store.
Asks the sidecar (memory_link_claude), which resolves a worktree to its
main checkout so worktrees share the main store, and never clobbers a real
memory dir with facts.  A no-op for other CLIs, or when the sidecar is not
up yet (the migration covers the existing dirs; a later spawn covers this)."
  (when (and (eq (ghostherd-session-kind session) 'claude)
             (fboundp 'ghostherd-memory-rpc-url) (ghostherd-memory-rpc-url)
             (fboundp 'ghostherd-memory-request-async))
    (let* ((project (ghostherd-session-project session))
           (wt (ghostherd-session-worktree session))
           (dir (or (and wt (plist-get wt :path)) project)))
      (when (and project dir)
        (ghostherd-memory-request-async
         "memory_link_claude" #'ignore
         (list :cwd (expand-file-name dir) :project (expand-file-name project))
         #'ignore)))))

(add-hook 'ghostherd-session-created-hook #'ghostherd--memory-link-claude)

(defcustom ghostherd-memory-opening-max 4000
  "Most characters of the memory index in a grok/agy agent's opening.
The index is every fact that is not a rule, one line each, then a line
naming the facts that did not fit -- whole lines only, so none is cut
and none dropped silently.  The rules (the user's facts and the feedback
ones) come before it in full, outside this cap: they say how to work.
See the sidecar's `shared_memory.opening'."
  :type 'integer
  :group 'ghostherd)

(defcustom ghostherd-memory-opening-timeout 3
  "Seconds to wait for the sidecar to compose a grok/agy opening.
Past it, or with no sidecar, the opening is the index lines read from
MEMORY.md, capped the same way."
  :type 'number
  :group 'ghostherd)

(defcustom ghostherd-memory-dir "~/.ghostherd/memory"
  "The shared-memory store root -- the same as the sidecar's
GHOSTHERD_SHARED_MEMORY_DIR.  One directory per project (its main
checkout's slug) plus `user'; each holds the fact files and MEMORY.md."
  :type 'directory
  :group 'ghostherd)

(defconst ghostherd--memory-mark "### ghostherd shared memory — do not edit ###"
  "Marks the memory block inside grok's --rules, so a re-launch replaces it.")

(defun ghostherd--memory-slug (path)
  "PATH as a store key -- the same scheme as server/shared_memory.py:
`/' and `.' become `-' in the absolute, un-slashed path."
  (replace-regexp-in-string "[/.]" "-" (directory-file-name (expand-file-name path))))

(defun ghostherd--memory-index (project)
  "PROJECT store's MEMORY.md as the agent should see it: the section and
fact lines (one per fact, the user facts and the project's), or nil when
there is no store yet.  Read straight from the file -- the herd keeps it
current -- so it needs no sidecar."
  (let ((f (expand-file-name (concat (ghostherd--memory-slug project) "/MEMORY.md")
                             ghostherd-memory-dir)))
    (when (file-readable-p f)
      (with-temp-buffer
        (insert-file-contents f)
        (let (lines)
          (goto-char (point-min))
          (while (not (eobp))
            (let ((l (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))))
              (when (string-match-p "\\`\\(- \\|## \\)" l)
                (push l lines)))
            (forward-line 1))
          (and lines (string-join (nreverse lines) "\n")))))))

(defun ghostherd--memory-opening-composed (project &optional agent)
  "PROJECT's opening as the sidecar composes it (`memory_opening'): the
rules in full, then the other facts within `ghostherd-memory-opening-max',
then the names of what did not fit.  The text, \"\" for an empty store,
or nil when the sidecar is not there to ask or does not answer in
`ghostherd-memory-opening-timeout'.  AGENT, the one starting, is who the
sidecar counts the rules as reaching."
  (when (and (fboundp 'ghostherd-memory-rpc-url) (ghostherd-memory-rpc-url)
             (fboundp 'ghostherd-memory-request))
    (condition-case nil
        (let ((r (ghostherd-memory-request
                  "memory_opening"
                  (append (list :project (expand-file-name project)
                                :max ghostherd-memory-opening-max)
                          (and agent (list :agent agent)))
                  ghostherd-memory-opening-timeout)))
          (and (stringp (plist-get r :text)) (plist-get r :text)))
      (error nil))))

(defun ghostherd--memory-opening-lines (index max)
  "INDEX's lines that fit in MAX characters, whole, and a note of how many
did not -- the opening without the sidecar, which alone can read the
rules out of the fact files."
  (let* ((lines (split-string index "\n"))
         (facts (seq-count (lambda (l) (string-prefix-p "- " l)) lines))
         ;; Room for the note, when not every line fits.
         (room (if (<= (length index) max) max (- max 80)))
         (kept nil) (used 0))
    (catch 'full
      (dolist (l lines)
        (when (> (+ used (length l) 1) room)
          (throw 'full nil))
        (push l kept)
        (setq used (+ used (length l) 1))))
    (let ((left (- facts (seq-count (lambda (l) (string-prefix-p "- " l)) kept))))
      (concat (string-join (nreverse kept) "\n")
              (if (> left 0)
                  (format "\n%d more, not listed — \"$GHOSTHERD_HERD\" recall <topic> for any" left)
                "")))))

(defun ghostherd--memory-opening (project &optional agent)
  "The shared-memory opening for a grok or agy agent in PROJECT -- AGENT,
when it is named.
The sidecar composes it (`ghostherd--memory-opening-composed'): the
project's rules in full, then every other fact one line each within
`ghostherd-memory-opening-max', then the names of what did not fit.
Without the sidecar, the index lines from MEMORY.md, whole, within the
same cap, and how many were left out.  Before either, the rule to recall
for the full text and anything newer; just that rule when the store is
empty or absent."
  (let ((composed (ghostherd--memory-opening-composed project agent))
        (head "\n\nThe project's memory as you start (\"$GHOSTHERD_HERD\" recall \
<topic> gives the full text of any fact, and anything newer).  "))
    (cond
     ((and composed (not (string-empty-p (string-trim composed))))
      (concat ghostherd--memory-rules head (string-trim-right composed)))
     (composed ghostherd--memory-rules)
     (t
      (let ((index (ghostherd--memory-index project)))
        (if (not (and index (not (string-empty-p index))))
            ghostherd--memory-rules
          (concat ghostherd--memory-rules head "One line per fact:\n\n"
                  (ghostherd--memory-opening-lines index ghostherd-memory-opening-max))))))))

(defun ghostherd--args-with-memory (project args &optional agent)
  "ARGS with the current memory opening folded into grok's --rules --
AGENT's, when it is named.
A previous memory block (from a re-launch, marked by `ghostherd--memory-mark')
is replaced, so there is always exactly one, with the current index; any role
text already in --rules is kept before it."
  (let ((opening (ghostherd--memory-opening project agent))
        (rules nil) (out nil))
    (while args
      (let ((a (pop args)))
        (if (and (equal a "--rules") args)
            (setq rules (pop args))
          (push a out))))
    (when (and rules (string-match (regexp-quote ghostherd--memory-mark) rules))
      (setq rules (string-trim-right (substring rules 0 (match-beginning 0)))))
    (let ((block (concat ghostherd--memory-mark "\n" opening)))
      (append (nreverse out)
              (list "--rules" (if (and rules (not (string-empty-p rules)))
                                  (concat rules "\n\n" block)
                                block))))))

(defconst ghostherd--lead-role "\
You are the lead of this project's herd of agents: you plan the work,
hand it out to the other agents, and report to the user.

The user talks to you in the project's chat.  What you say at the end of
a turn is what they read there: say what you decided, who is doing what,
what came back, and what you need from them -- briefly, and in the
language they write to you in.  When you need the user to decide
something, end your turn with the question itself, so it reaches their
Inbox as a question to answer.

Your team: \"$GHOSTHERD_HERD\" list shows every agent with its kind,
state, notes and project, and the directory and branch it works in;
yours are the ones in this project.  You see what an agent does only
through what it answers you.

Some agents work in their own git worktree -- their own directory on
their own branch off the project, so two can write code at once without
colliding.  Name an agent's path and branch in the brief you give it, so
it knows where to work; and read another agent's work by its branch
rather than its files -- \"git diff BASE...BRANCH\", where BASE is the
branch it came off (usually the project's main) and BRANCH is the one
\"$GHOSTHERD_HERD\" list shows for it.  Do not edit in another agent's
worktree.  A worktree's dependencies are already there -- node_modules
a copy-on-write clone of the main checkout's, a uv project's venv its
own -- so tell its agent to use \"npm install\", not \"npm ci\", which
deletes the clone and installs a full copy.

Hand work out as an ask, run in the background so you can go on:

  \"$GHOSTHERD_HERD\" ask NAME --wait <<'HERD'
  the work, and what you want back
  HERD

You are told when it finishes; its output is the answer.  Ask several
agents at once when the work allows.

Hand out outcomes, not steps: \"build it and get it through qa, four
rounds at most\" to rd lets rd and qa go back and forth without you in
the middle, and you get the result.  Give the rounds to the herd as
well -- ask rd with --rounds 4 -- and it holds rd to them: past four
asks of qa it refuses the fifth, and rd answers you with where it
stands.  Without --rounds it allows five.  An agent you asked may ask
others; nobody may ask you back while you wait on them -- the herd
refuses it.

Do small things yourself: read a file to plan, answer from what you
know.  Do not do an agent's work for it.

When the team has nobody for a job, start one yourself:

  \"$GHOSTHERD_HERD\" start KIND [--worktree [BRANCH] | --no-worktree] [--name NAME] [--notes TEXT]

KIND is claude, grok or agy.  It prints the agent's name (and its
worktree), which you then ask.  It reuses an idle agent you started whose
branch is merged and worktree clean -- the same agent, with its context,
fresh off the main branch -- before starting a new one; pass --new to
force a fresh one.  A worktree is the default once another agent works in
the checkout, so two do not collide; --no-worktree runs it in the main
checkout.  The herd caps how many you may have up (it lists them when you
hit the cap) and refuses a used-up kind, naming one with quota.

Queue rather than start when the work touches the same files as a busy
agent's (its files are in \"$GHOSTHERD_HERD\" list), is small, waits on
that agent's result, or needs its context: send it as an ask that waits
its turn behind the one it has.  Start or reuse an agent when the area is
independent and the work is substantial enough to run on its own.  Several
agents of one kind draw on one quota, so do not start more than the work
needs.

Stop an agent you started when the user says so or its work is done:

  \"$GHOSTHERD_HERD\" stop NAME

It refuses one still owing an ask; a merged, clean worktree is removed, an
unmerged one kept.  Keep an agent while more of its work is likely rather
than stopping and restarting it from scratch.  Either way -- started,
reused or stopped -- tell the user, briefly, in the chat.

When an ask fails, or comes back as the herd's answer rather than the
agent's, say what happened and decide: ask again, ask someone else, or
ask the user.

When the work needs a change in another project -- an API it serves, a
config it owns, a bug on its side -- do not edit its checkout, and do
not have your agents do it: you do not know its rules, and its own
agents may be at work there.  Hand it to that project's lead:

  \"$GHOSTHERD_HERD\" handoff PROJECT --wait <<'HERD'
  the brief
  HERD

PROJECT is its path or its name (unnoiac).  Brief it for a lead who
knows nothing of your work: what you found; the evidence -- files and
lines, logs, tickets, commits; what is needed of that project; how it
will know it is done; and who waits on it.  It is an ask: run it in the
background, and its answer comes back to you.  A project with no lead
gets one, started for it.  Your agents do not hand off -- the herd
refuses them -- they tell you in their answers, and you decide.

A handoff may come to you from another project: the herd says so where
it begins.  Take it in as your own work -- plan it here, by this
project's rules, with your agents -- and answer with the result: what
was done, where (branch, commit, pull request), and what is left for
the sender.  It cannot be asked while it waits on you: put a question
in your answer, and it can hand off again.

When an agent is stuck for good -- its CLI out of credits or rate
limited, or wedged with no way on -- hand its work to a fresh agent on
another CLI:

  \"$GHOSTHERD_HERD\" relay NAME [KIND] --reason \"why\"

It reads where the work stood and starts a new agent of KIND that takes
the name and carries on.  Leave KIND off to let the herd pick a CLI with
usage left; name one (claude, grok, agy) to choose.  You may relay only
your own project's agents, not yourself.

The herd keeps shared memory for this project -- curated facts every
agent sees, whatever its CLI, and the user's preferences across
projects.  Read it with \"$GHOSTHERD_HERD\" recall (no argument prints
the index; a query prints the matching facts in full), add a fact with
\"$GHOSTHERD_HERD\" remember, drop one with \"$GHOSTHERD_HERD\" forget.
It is the source of truth: use it, not notes kept elsewhere.  Record what
the team learns -- a decision, a gotcha, how a thing is done -- so the
next agent starts with it.  If you are claude this is your own memory
dir: keep writing it as you do, and the herd shares it with the project.

What the project works on -- its branch, its pull request, the ClickUp
tickets they and the team's conversations name -- opens the user's
first message to you, and their next one whenever it changes.  Read the
tickets and the pull request for the work, with whatever tools you have
for them; change none of them -- no ticket's status, fields or
comments, nothing on the pull request -- unless the user asks you to.

The user may also write to an agent directly.  When they have since you
last heard from them, their next message to you starts with a note
saying so: take it into account rather than undoing it.
"
  "The lead's role: given to it as it starts, or with its first message.")

(defun ghostherd--lead-args (kind)
  "Arguments that give a KIND agent the lead's role as it starts.
nil for a CLI that takes none: its first message carries the role.
The text itself rather than a file: claude records the system prompt
with the conversation's first request and resumes with that record,
so a file read afresh would buy nothing, and its -file flag is not one
it lists."
  (pcase kind
    ('claude (list "--append-system-prompt" ghostherd--lead-role))
    ('grok (list "--rules" ghostherd--lead-role))))

(defun ghostherd--with-lead-role (kind args)
  "ARGS with the lead's role as it is now, for a KIND agent.
Whatever role ARGS gave it -- the text after the flag that takes one
-- is dropped, and the current one appended."
  (if-let* ((role (ghostherd--lead-args kind)))
      (let (out)
        (while args
          (let ((a (pop args)))
            (if (equal a (car role)) (pop args) (push a out))))
        (append (nreverse out) role))
    args))

(defun ghostherd-project-lead (project)
  "PROJECT's lead, or nil."
  (let ((key (ghostherd--project-name project)))
    (seq-find (lambda (s)
                (and (equal (ghostherd-session-role s) "lead")
                     (equal (ghostherd--project-name (ghostherd-session-project s)) key)))
              (ghostherd-sessions))))

(defvar ghostherd--role-owed (make-hash-table :test 'equal)
  "Session id -> the role text owed to an agent -- a lead, primary --
started on a CLI that takes none as it starts, until its first message:
that carries the role, whether it is the user's (`ghostherd-tell') or
an ask (`ghostherd--ask-deliver').")

(defvar ghostherd--role-spent (make-hash-table :test 'equal)
  "Session id -> the owed role its last message took, given back if its
CLI never had that message (`ghostherd--role-give-back').")

(defun ghostherd--role-take (session)
  "The role SESSION is owed, taken; or nil."
  (let ((id (ghostherd-session-id session)))
    (prog1 (when-let* ((role (gethash id ghostherd--role-owed)))
             (puthash id role ghostherd--role-spent))
      (remhash id ghostherd--role-owed))))

(defun ghostherd--role-give-back (session text)
  "SESSION is owed again the role TEXT carried: its CLI never had TEXT
(`ghostherd--submit-lost'), so the message sent instead still owes it."
  (let ((id (ghostherd-session-id session)))
    (when-let* ((role (gethash id ghostherd--role-spent))
                ((string-search role text))
                ((not (gethash id ghostherd--role-owed))))
      (puthash id role ghostherd--role-owed)
      (remhash id ghostherd--role-spent))))

(defun ghostherd-start-lead (project &optional kind notes)
  "Start PROJECT's lead, a KIND agent (`ghostherd-lead-kind' by default).
NOTES, when given, say more than that it plans the work.  A CLI that
takes no role as it starts is owed it, with its first message."
  (let* ((kind (or kind ghostherd-lead-kind))
         (dir (file-name-as-directory (expand-file-name project)))
         (lead (ghostherd-spawn
                kind
                :name (ghostherd--unique-name
                       (format "lead-%s" (file-name-nondirectory (directory-file-name dir))))
                :project project
                :directory dir
                :args (append (plist-get (ghostherd--spec kind) :args)
                              (ghostherd--lead-args kind))
                :notes (or notes "lead: plans the work, hands it out")
                :role "lead"
                :display nil)))
    (unless (ghostherd--lead-args kind)
      (puthash (ghostherd-session-id lead) ghostherd--lead-role ghostherd--role-owed))
    lead))

(defvar ghostherd--lead-unheard (make-hash-table :test 'equal)
  "Project -> what the user said to its agents directly since its lead
last heard from them, newest first, as (NAME . TEXT).")

(defun ghostherd--lead-unheard-note (project)
  "What PROJECT's lead has not heard, as the note its next message opens
with; or the empty string."
  (if-let* ((said (reverse (gethash project ghostherd--lead-unheard))))
      (concat "[Since you last heard from me, I wrote to the team directly:\n"
              (mapconcat (lambda (c)
                           (format "- to %s: %s" (car c)
                                   (truncate-string-to-width
                                    (replace-regexp-in-string "[\n\t ]+" " " (string-trim (cdr c)))
                                    300 nil nil "…")))
                         said "\n")
              "]\n\n")
    ""))

(defvar ghostherd-project-work-functions nil
  "Functions called with a project, each saying what it works on -- its
branch, its pull request, the tickets they name -- as lines, or nil.
Its lead is told them.  The herd page's look at the herd's checkouts
answers it.")

(defvar ghostherd--lead-work-told (make-hash-table :test 'equal)
  "Lead session id -> the lines it was last told its project works on.")

(defun ghostherd--project-work (project)
  "What PROJECT works on, as lines; nil when nothing says."
  (seq-mapcat (lambda (f) (with-demoted-errors "ghostherd: %S" (funcall f project)))
              ghostherd-project-work-functions))

(defun ghostherd--lead-work-note (lead project)
  "What LEAD has not been told its PROJECT works on, as the note its next
message opens with; or the empty string.  Told, it is not told again
until it changes."
  (let ((lines (ghostherd--project-work project))
        (had (gethash (ghostherd-session-id lead) ghostherd--lead-work-told 'none)))
    (if (or (null lines) (equal lines had))
        ""
      (puthash (ghostherd-session-id lead) lines ghostherd--lead-work-told)
      (concat (if (eq had 'none)
                  "[What this project works on, as the herd sees it.  Read them for the work; \
change none of them -- no ticket's status, fields or comments, nothing on the pull request -- \
unless I ask you to:\n"
                "[What this project works on has changed since you were told.  Now:\n")
              (mapconcat (lambda (l) (concat "- " l)) lines "\n")
              "]\n\n"))))

(defun ghostherd-tell (session text &optional role by-herd)
  "Prompt SESSION with TEXT from the user -- or, BY-HERD, the herd's own.
The herd log records TEXT, not the notes put before it.
A project's lead hears first what the user said to its agents directly
since it last heard from them, and what the project works on when that
is news to it -- after ROLE, the role of a lead that took none as it
started: the one it is owed (`ghostherd--role-owed') when ROLE is nil.
Any other agent owed its role -- primary, on such a CLI -- reads it
first too.  Said to another agent of a project that has a lead, TEXT is
kept for that."
  (let* ((project (ghostherd--project-name (ghostherd-session-project session)))
         (lead (and project (ghostherd-project-lead project)))
         (ghostherd--input-said (if by-herd (concat "[from the herd] " text) text)))
    ;; A herd-made prompt (a relay, say) starts a turn that is not the user's,
    ;; so its end is not an Inbox item.  Marked BEFORE the prompt, which moves
    ;; the session to `working' synchronously and runs the origin hook.  The
    ;; user's own prompt (BY-HERD nil) is left to the hook, which marks it
    ;; `user'.
    (when by-herd (ghostherd--turn-from-ask session))
    (if (eq lead session)
        (progn
          (ghostherd-prompt session (concat (or role
                                                (when-let* ((owed (ghostherd--role-take session)))
                                                  (concat owed "\n---\n\n")))
                                            (ghostherd--lead-unheard-note project)
                                            (ghostherd--lead-work-note session project)
                                            text))
          (remhash project ghostherd--lead-unheard))
      (ghostherd-prompt session (concat (when-let* ((owed (ghostherd--role-take session)))
                                          (concat owed "\n---\n\n"))
                                        text))
      (when lead
        (push (cons (ghostherd-session-name session) text)
              (gethash project ghostherd--lead-unheard))))))

(defun ghostherd--when-ready (session fn &optional tries fail)
  "Call FN once SESSION is at its prompt, looking every second.
At its prompt: at rest, its input box drawn at its foot -- not a trust
prompt reading idle (`ghostherd--off-prompt-p').  Two minutes at most,
TRIES counting down; a session that never gets there is logged, and one
gone is let be.  Either way FAIL, when given, is called instead."
  (let ((tries (or tries 120)))
    (cond
     ((not (eq (gethash (ghostherd-session-id session) ghostherd--sessions) session))
      (when fail (funcall fail)))
     ((and (memq (ghostherd-session-state session) '(idle done waiting))
           (not (ghostherd--off-prompt-p session)))
      (funcall fn))
     ((<= tries 0)
      (ghostherd--log-add session 'life "never came to its prompt: the message for it was not sent")
      (when fail (funcall fail)))
     (t (run-at-time 1 nil #'ghostherd--when-ready session fn (1- tries) fail)))))

(defun ghostherd--lead-project (project)
  "The project whose lead PROJECT reaches: the one it is in, by
`ghostherd--project-root' -- a subdirectory's or a worktree's -- or
PROJECT itself, in none or primary's."
  (if (ghostherd--primary-dir-p project)
      project
    (or (ghostherd--project-root (expand-file-name project)) project)))

(defun ghostherd-lead-say (project text)
  "Say TEXT to PROJECT's lead, starting one when it has none.
A lead just started hears TEXT once it is ready -- after its role, on a
CLI that took none as it started.  Return the lead.  Primary's directory
has no lead but primary, who is told instead (`ghostherd-primary-say'):
a reply to its report on the phone, its room's chat.
PROJECT is taken as the project it is in (`ghostherd--project-root'): a
subdirectory or a worktree of one reaches that project's lead, and never
starts a second lead on part of it."
  (setq project (ghostherd--lead-project project))
  (cond
   ((ghostherd--primary-dir-p project) (ghostherd-primary-say text))
   ((ghostherd-project-lead project)
    (let ((lead (ghostherd-project-lead project)))
      (ghostherd-tell lead text)
      lead))
   (t
    (let ((lead (ghostherd-start-lead project)))
      ;; Asked now, so that whatever looks has looked by the time the
      ;; lead is ready to be told.
      (ghostherd--project-work project)
      (ghostherd--when-ready lead (lambda () (ghostherd-tell lead text)))
      lead))))

;;; What agents say about themselves, through their CLI's hooks
;;
;; `herd hook' runs in claude's, grok's and agy's lifecycle hooks and
;; posts to the sidecar: the state the agent is in, the conversation it
;; is on, and the answer that ended its turn.  States come here on the
;; tick and go through `ghostherd-report', so they keep its TTL and its
;; place in the authority order.  The rest stays in the sidecar, and a
;; copy of it rides each tick.

(defvar ghostherd--herd-links (make-hash-table :test 'equal)
  "Session name -> what its hooks last said, from the last `herd_tick'.
A plist of :cli, :conversation, :transcript, :last_head (first line of
its last answer) and :last_ms.  Keyed by name because that is what the
agent's environment carries.")

(defun ghostherd--herd-take-reports (reports)
  "Apply REPORTS from a `herd_tick': each agent's own word on its state."
  (dolist (r reports)
    (let ((session (ghostherd-get (plist-get r :session)))
          (state (intern-soft (or (plist-get r :state) "")))
          (reason (plist-get r :reason)))
      (when (and session (or (eq state 'auto) (memq state ghostherd-report-states)))
        (ghostherd-report session state
                          (and reason (not (string-empty-p reason)) reason))
        ;; Its turn's end, by its hook, while its screen already reads it
        ;; stopped: no change of state follows to settle its ask on, and
        ;; the sidecar left that ask open for exactly this.
        (when (and (memq state '(done idle auto))
                   (memq (ghostherd-session-state session) '(idle done)))
          (ghostherd--ask-on-state session nil (ghostherd-session-state session)))))))

(defun ghostherd--herd-take-links (links)
  "Keep LINKS from a `herd_tick', replacing the last set."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (l links)
      (puthash (plist-get l :session) l table))
    (setq ghostherd--herd-links table))
  (ignore-errors (ghostherd--primary-remember)))

(defun ghostherd--link (session)
  "What SESSION's hooks last said, or nil.
Not what another CLI's said under its name: the agent it took over
from (`ghostherd-relay'), whose conversation is not its own."
  (when-let* ((s (ghostherd-get session))
              (link (gethash (ghostherd-session-name s) ghostherd--herd-links)))
    (let ((cli (plist-get link :cli)))
      (and (or (not (stringp cli)) (string-empty-p cli)
               (equal cli (symbol-name (or (ghostherd-session-kind s) 'shell))))
           link))))

(defun ghostherd--turn-began-since (session at)
  "Non-nil when SESSION's hooks said a turn began after AT, a float time:
the sidecar keeps when the last one did (its link's `:turn_ms'), which
no later report folds away."
  (let ((ms (plist-get (ghostherd--link session) :turn_ms)))
    (and (numberp ms) (numberp at) (> ms (* 1000 at)))))

(defun ghostherd--hooked-p (session)
  "Non-nil when SESSION's own CLI's hooks report to the herd: they named
it.  A turn it begins is then said, not only drawn."
  (let ((cli (plist-get (ghostherd--link session) :cli)))
    (and (stringp cli) (not (string-empty-p cli)))))

(defun ghostherd-session-conversation (session)
  "The conversation SESSION is on, as its own hooks named it, or nil."
  (let ((id (plist-get (ghostherd--link session) :conversation)))
    (and (stringp id) (not (string-empty-p id)) id)))

(defun ghostherd--args-conversation (kind args)
  "The conversation ARGS start KIND on, by its resume flag, or nil.
Nil for a fork: that writes a conversation of its own, whose id only
its hooks can name."
  (let* ((spec (ignore-errors (ghostherd--spec kind)))
         (flag (car (plist-get spec :resume-args)))
         (id (and flag (cadr (member flag args)))))
    (and (stringp id)
         (string-match-p ghostherd--session-id-re id)
         (not (seq-some (lambda (f) (member f args)) (plist-get spec :fork-args)))
         id)))

(defconst ghostherd--transcript-retry 30
  "Seconds before looking again for a transcript that was not there.")

(defvar ghostherd--transcripts (make-hash-table :test 'equal)
  "(KIND . ID) -> (PATH-or-nil . WHEN): where a conversation was found.")

(defun ghostherd-session-transcript (session)
  "The transcript file SESSION writes, or nil when it is not known.
Its hooks name it.  Failing that, the conversation it was started on
does, for a kind whose spec says where (`:transcript').  A fresh agent
without hooks has no name for its conversation yet, so none."
  (when-let* ((s (ghostherd-get session)))
    (let ((linked (plist-get (ghostherd--link s) :transcript)))
      (if (and (stringp linked) (not (string-empty-p linked)) (file-readable-p linked))
          linked
        (let* ((kind (ghostherd-session-kind s))
               (id (or (ghostherd-session-conversation s)
                       (ghostherd--args-conversation kind (ghostherd-session-args s))))
               (place (plist-get (ignore-errors (ghostherd--spec kind)) :transcript))
               (key (cons kind id))
               (seen (gethash key ghostherd--transcripts)))
          (when (and id place (string-match-p ghostherd--session-id-re id))
            (if (and seen (if (car seen)
                              (file-exists-p (car seen))
                            (< (- (float-time) (cdr seen)) ghostherd--transcript-retry)))
                (car seen)
              (let ((found (car (sort (file-expand-wildcards
                                       (expand-file-name (format place id)) t)
                                      #'file-newer-than-file-p))))
                (puthash key (cons found (float-time)) ghostherd--transcripts)
                found))))))))

(defun ghostherd--without-resume (kind args)
  "ARGS less any flag that picks a conversation for KIND.
A respawn onto a known conversation adds its own; the recipe's
`--continue', or the `-r' of the conversation it was first started
on, would contradict it."
  (let* ((spec (ignore-errors (ghostherd--spec kind)))
         (resume (car (plist-get spec :resume-args)))
         (drop (append (plist-get spec :continue-args) (plist-get spec :fork-args)))
         out)
    (while args
      (let ((a (pop args)))
        (cond ((and resume (equal a resume)) (pop args))
              ((member a drop))
              (t (push a out)))))
    (nreverse out)))

(defun ghostherd--explain-text (session)
  "One-screen explanation of SESSION state, for Telegram."
  (setq session (ghostherd-get session))
  (format "%s %s\n%s"
          (ghostherd--state-glyph (ghostherd-session-state session))
          (ghostherd-session-state session)
          (or (ghostherd-session-state-reason session) "—")))

(defun ghostherd--relay-request (by target kind reason)
  "Relay agent TARGET to KIND at the request of agent BY, with REASON, as
`herd relay' asks over the wire.  The guardrails a page button or Telegram
tap do not need: BY must be the lead of TARGET's own project, may not relay
itself, and neither it nor TARGET may already be in a relay -- which also
stops a relay from looping back on the one that asked for it.  KIND empty
takes the herd's default for TARGET.  Returns what to tell BY: the relay
started, or, clearly, why it was refused.

A finer \"BY is waiting on an ask from TARGET\" guard is left to the ask
machinery: a relay hands TARGET's open asks to its heir, so BY still gets
its answer rather than losing it."
  (let* ((by-s (ghostherd-get by))
         (tgt (ghostherd-get target)))
    (cond
     ((not by-s) (format "no such agent: %s" by))
     ((not tgt) (format "no such agent: %s" target))
     ((eq by-s tgt) "an agent cannot relay itself")
     ((not (equal (ghostherd-session-role by-s) "lead"))
      (format "only a project's lead may relay; %s is not a lead" by))
     ((let ((p (ghostherd--project-name (ghostherd-session-project by-s))))
        ;; A real, shared project -- not two project-less sessions reading
        ;; equal because both are nil.
        (not (and p (equal p (ghostherd--project-name (ghostherd-session-project tgt))))))
      (format "%s may only relay agents of its own project, not %s" by target))
     ((gethash (ghostherd-session-id by-s) ghostherd--relaying)
      (format "%s is being relayed itself; wait for that to finish" by))
     ((gethash (ghostherd-session-id tgt) ghostherd--relaying)
      (format "%s is already being relayed" target))
     (t
      (let ((k (if (and (stringp kind) (not (string-empty-p kind)))
                   (intern kind)
                 (ghostherd-relay-default-kind tgt))))
        (if (not (memq k (ghostherd--relay-kinds)))
            (format "relay to what: %s is not a CLI the herd relays to"
                    (if (and (stringp kind) (not (string-empty-p kind))) kind k))
          (condition-case err
              (progn
                (ghostherd-relay tgt k (and (stringp reason) (not (string-empty-p reason)) reason))
                (format "relaying %s to %s" target k))
            (error (error-message-string err)))))))))


;;; A lead starts and stops agents through the herd
;;
;; `herd start' / `herd stop' let a lead run agents without reaching into
;; Emacs internals: the sidecar queues the request, Emacs acts here, guarded
;; by who asks.  Only a lead, for its own project, may start; only the agents
;; a lead started may be stopped or reused by it.  Reuse before spawning: an
;; idle agent the lead started, same kind, whose branch is merged and
;; worktree clean, is fast-forwarded and handed back with its context rather
;; than started anew.  The herd caps how many a lead has up and refuses a
;; used-up kind.  `started-by' is kept on the host with the recipe, so a
;; lead still owns its agents after a restart.

(defcustom ghostherd-lead-may-start t
  "Whether a lead may start and stop agents through `herd start' / `herd stop'.
Nil refuses every such request, and the lead is told to ask the user."
  :type 'boolean
  :group 'ghostherd)

(defcustom ghostherd-lead-start-max 2
  "How many agents one lead may have alive at once in a project, through
`herd start'.  Past it the herd refuses and lists the ones already up, so a
lead reuses or stops one instead of piling them on.  Reuse and stop are not
capped; neither are agents the user started by hand."
  :type 'integer
  :group 'ghostherd)

(defun ghostherd--session-project-name (session)
  "The project name SESSION belongs to, or nil."
  (ghostherd--project-name (ghostherd-session-project session)))

(defun ghostherd--lead-started-alive (lead project)
  "The live sessions LEAD started in PROJECT (by project name)."
  (seq-filter (lambda (s)
                (and (equal (ghostherd-session-started-by s) lead)
                     (equal (ghostherd--session-project-name s) project)
                     (ghostherd--session-live-p s)))
              (ghostherd-sessions)))

(defun ghostherd--worktree-ff (path base)
  "Fast-forward the git worktree at PATH onto BASE; non-nil on success."
  (and (file-directory-p (expand-file-name path))
       (let ((default-directory (file-name-as-directory (expand-file-name path))))
         (eq 0 (ignore-errors
                 (call-process "git" nil nil nil "merge" "--ff-only" base))))))

(defun ghostherd--lead-reuse-candidate (lead kind project)
  "An idle agent LEAD started in PROJECT, same KIND, its branch merged into
its base and its worktree clean -- to refresh rather than spawn anew; or nil."
  (seq-find
   (lambda (s)
     (let ((wt (ghostherd-session-worktree s)))
       (and (eq (ghostherd-session-kind s) kind)
            (eq (ghostherd-session-state s) 'idle)
            wt (plist-get wt :path) (plist-get wt :branch) (plist-get wt :base)
            (ghostherd--worktree-clean-p (plist-get wt :path))
            (ghostherd--worktree-merged-p project (plist-get wt :branch)
                                           (plist-get wt :base)))))
   (ghostherd--lead-started-alive lead project)))

(defun ghostherd--project-main-busy-p (project)
  "Non-nil when some live agent already works in PROJECT's main checkout --
no worktree of its own -- so a second agent wants its own worktree."
  (seq-some (lambda (s)
              (and (equal (ghostherd--session-project-name s) project)
                   (ghostherd--session-live-p s)
                   (not (ghostherd-session-worktree s))))
            (ghostherd-sessions)))

(defun ghostherd--agent-owes-ask-p (name)
  "Non-nil when agent NAME is the taker of an ask still open or queued,
by the asks the last herd tick gave this Emacs."
  (seq-some (lambda (a)
              (and (equal (plist-get a :to) name)
                   (member (plist-get a :status) '("queued" "delivered"))))
            ghostherd--herd-asks))

(defun ghostherd-lead-start (by kind-str &rest opts)
  "Start, or reuse, an agent for lead BY, as `herd start' asks over the wire.
KIND-STR is the CLI kind.  OPTS: :worktree (\"yes\"/\"no\"/\"auto\"), :branch,
:name, :notes, :new.  Returns the line to tell BY -- the agent started or was
reused (with its name and worktree), or, plainly, why it was refused."
  (let* ((by-s (ghostherd-get by))
         (kind (and (stringp kind-str) (not (string-empty-p kind-str))
                    (intern kind-str)))
         (wmode (or (plist-get opts :worktree) "auto"))
         (branch (let ((b (plist-get opts :branch)))
                   (and (stringp b) (not (string-empty-p b)) b)))
         (want-name (let ((n (plist-get opts :name)))
                      (and (stringp n) (not (string-empty-p n)) n)))
         (notes (plist-get opts :notes))
         (new (eq (plist-get opts :new) t)))
    (cond
     ((not ghostherd-lead-may-start)
      "the herd is set not to start agents (ghostherd-lead-may-start is off); ask the user")
     ((not by-s) (format "no such agent: %s" by))
     ((not (equal (ghostherd-session-role by-s) "lead"))
      (format "only a project's lead may start agents; %s is not a lead" by))
     ((not (and kind (memq kind (ghostherd--agent-kinds))
                (plist-get (ghostherd--spec kind) :command)))
      (format "no agent kind %s" kind-str))
     (t
      (let ((project (ghostherd--session-project-name by-s))
            (dir (ghostherd-session-project by-s)))
        (cond
         ((not (and project dir (file-directory-p (expand-file-name dir))))
          (format "%s has no project to start an agent in" by))
         ;; Reuse before spawning.
         ((and (not new)
               (when-let* ((r (ghostherd--lead-reuse-candidate by kind project))
                           (wt (ghostherd-session-worktree r)))
                 (ghostherd--worktree-ff (plist-get wt :path) (plist-get wt :base))
                 (ghostherd--log-add r 'life
                                     (format "reused by %s: worktree fast-forwarded to %s"
                                             by (plist-get wt :base)))
                 (format "reused %s (%s on %s, fresh off %s); it keeps its context"
                         (ghostherd-session-name r)
                         (ghostherd--abbreviate (plist-get wt :path))
                         (plist-get wt :branch) (plist-get wt :base)))))
         ;; A used-up kind: name one that is not.
         ((ghostherd-usage-used-up kind)
          (let ((alt (seq-find (lambda (k) (and (not (eq k kind))
                                                (not (ghostherd-usage-used-up k))
                                                (plist-get (ghostherd--spec k) :command)))
                               (ghostherd--agent-kinds))))
            (format "%s is used up or out of quota%s" kind-str
                    (if alt (format "; start a %s instead" alt)
                      ", and so is every kind the herd knows"))))
         ;; The per-lead cap.
         ((let ((alive (ghostherd--lead-started-alive by project)))
            (when (>= (length alive) (max 0 ghostherd-lead-start-max))
              (format "%s already has %d agent%s up in %s (max %d): %s -- reuse or stop one"
                      by (length alive) (if (= (length alive) 1) "" "s") project
                      ghostherd-lead-start-max
                      (mapconcat #'ghostherd-session-name alive ", ")))))
         (t
          (condition-case err
              (let* ((want-wt (pcase wmode
                                ("yes" t) ("no" nil)
                                (_ (or branch (ghostherd--project-main-busy-p project)))))
                     (final (ghostherd--unique-name
                             (or want-name
                                 (format "%s-%s" kind
                                         (file-name-nondirectory (directory-file-name dir))))))
                     (defaults (ghostherd-project-defaults dir))
                     (args (or (and (eq (car defaults) kind) (cdr defaults))
                               (plist-get (ghostherd--spec kind) :args)))
                     ;; The shared-memory opening (rules + index) is added for
                     ;; every grok/agy by `ghostherd-spawn', so it rides every
                     ;; path, not only this one.
                     (spawn (lambda (directory wt)
                              (ghostherd-spawn kind :name final :project dir
                                               :directory directory :args args :notes notes
                                               :started-by by :display nil
                                               :worktree (and wt (plist-get wt :path))
                                               :branch (and wt (plist-get wt :branch))
                                               :base (and wt (plist-get wt :base))))))
                (if (not want-wt)
                    (progn
                      (funcall spawn dir nil)
                      (format "started %s (%s, in the main checkout)" final kind))
                  (let ((wt (ghostherd--worktree-create dir final branch kind)))
                    (ghostherd--worktree-deps-setup dir (plist-get wt :path) final)
                    (funcall spawn (plist-get wt :path) wt)
                    (format "started %s (%s, worktree %s on %s off %s)"
                            final kind (ghostherd--abbreviate (plist-get wt :path))
                            (plist-get wt :branch) (plist-get wt :base)))))
            (error (format "could not start a %s: %s" kind-str (error-message-string err)))))))))))

(defun ghostherd-lead-stop (by name)
  "Stop agent NAME for lead BY, as `herd stop' asks over the wire.  Only an
agent BY started, and not one holding an open or queued ask.  Removes a
merged, clean worktree; keeps an unmerged one.  Returns the line to tell BY."
  (let ((s (ghostherd-get name)))
    (cond
     ((not ghostherd-lead-may-start)
      "the herd is set not to stop agents (ghostherd-lead-may-start is off); ask the user")
     ((not s) (format "no such agent: %s" name))
     ((not (equal (ghostherd-session-started-by s) by))
      (format "%s did not start %s through the herd, so it may not stop it" by name))
     ((ghostherd--agent-owes-ask-p name)
      (format "%s holds an open or queued ask; let it answer, or cancel it, first" name))
     (t
      (let* ((wt (ghostherd-session-worktree s))
             ;; Looked at now, not as the last poll saw it.
             (gone (ghostherd--mark-gone s t))
             (branch (and gone (ghostherd--gone-branch-now s)))
             (will-remove (and wt (plist-get wt :path) (not gone)
                               (ghostherd--worktree-clean-p (plist-get wt :path))
                               (ghostherd--worktree-merged-p (ghostherd-session-project s)
                                                             (plist-get wt :branch)
                                                             (plist-get wt :base)))))
        (ghostherd-kill s t)
        (cond
         ;; Removed outside the herd: nothing of it to keep or remove, and
         ;; the branch, whatever it holds, is left as it is.
         (gone
          (format "stopped %s; its %s %s was already gone%s"
                  name (if (plist-get gone :worktree) "worktree" "directory")
                  (abbreviate-file-name (plist-get gone :path))
                  (if branch
                      (concat " (" (ghostherd--gone-branch-words
                                    (plist-get wt :branch) (plist-get wt :base)
                                    (car branch) (cdr branch))
                              (if (eq (car branch) 'ahead) "; the branch is kept" "")
                              ")")
                    "")))
         ((not wt) (format "stopped %s" name))
         (will-remove
          (format "stopped %s; its worktree was merged and clean, so it was removed" name))
         (t (format "stopped %s; its worktree was not merged or not clean, so it was kept (M-x ghostherd-worktree-forget)"
                    name))))))))


;;; Handoffs: work for another project's lead
;;
;; `herd handoff PROJECT' is an ask of another project's lead: what an
;; agent found needs a change there, and that project's own lead plans
;; it, by that project's rules, rather than the finder editing a checkout
;; it does not know.  The sidecar keeps the ask, judges who may send it
;; and frames the brief; when PROJECT has no lead it asks Emacs to start
;; one (a `handoff' command), and the ask waits for its name.  Starting a
;; lead nobody asked for spends quota in a project the user may not be
;; looking at, so it is a setting, capped, only in a git checkout, and
;; pushed to the user's phone.

(defcustom ghostherd-handoff-may-start-lead t
  "Whether a handoff to a project with no lead starts one there.
`herd handoff PROJECT' goes to PROJECT's lead; with none, the herd starts
one -- a `ghostherd-lead-kind' agent, or the project's own default kind
from its .dir-locals.el -- and hands it the work.  Nil refuses such a
handoff, and the sender is told to ask the user."
  :type 'boolean
  :group 'ghostherd)

(defcustom ghostherd-handoff-lead-max 3
  "How many leads the herd may have up at once that it started for
handoffs.  Past it a handoff to a project with no lead is refused, naming
the ones up, so the user decides.  Leads the user started do not count,
nor ones started before Emacs restarted."
  :type 'integer
  :group 'ghostherd)

(defvar ghostherd--handoff-leads (make-hash-table :test 'equal)
  "Session id -> the agent it was started for: the leads the herd started
for a handoff, counted against `ghostherd-handoff-lead-max'.  Not kept
over a restart: after one, they are leads like any other.")

(defun ghostherd--handoff-leads-up ()
  "The leads the herd started for handoffs that are still up: not dead."
  (seq-filter (lambda (s)
                (and (gethash (ghostherd-session-id s) ghostherd--handoff-leads)
                     (not (eq (ghostherd-session-state s) 'dead))
                     (ghostherd--session-live-p s)))
              (ghostherd-sessions)))

(defun ghostherd--handoff-lead-for (session)
  "The agent SESSION's lead was started for, when the herd started it for
a handoff and it is still up; else nil.  The Inbox shows an Update for
it, kept until read."
  (when-let* ((s (ghostherd-get session))
              (by (gethash (ghostherd-session-id s) ghostherd--handoff-leads)))
    (and (not (eq (ghostherd-session-state s) 'dead))
         (ghostherd--session-live-p s)
         by)))

(defun ghostherd-handoff-lead (by project &optional from-project head)
  "PROJECT's lead, for a handoff from BY, as the sidecar asks over the
wire; one started when it has none.  FROM-PROJECT is BY's project and HEAD
the brief's first line, for the push.  Returns (NAME . TEXT): the lead's
name and what was done, or nil and why it was refused."
  (let* ((dir (and (stringp project) (not (string-empty-p project))
                   (file-name-as-directory (expand-file-name project))))
         (root (and dir (file-directory-p dir)
                    (or (ghostherd--project-root dir) dir)))
         (name (and root (ghostherd--project-name root)))
         (lead (and name (ghostherd-project-lead name)))
         (from (and (stringp from-project) (not (string-empty-p from-project))
                    (ghostherd--project-name from-project)))
         (leaf (lambda (p) (file-name-nondirectory (directory-file-name p))))
         (by-s (and (stringp by) (ghostherd-get by))))
    (cond
     ((not root) (cons nil (format "no directory %s: no project to hand off to"
                                   (if dir (ghostherd--abbreviate dir) project))))
     ((and from (equal from name))
      (cons nil (format "%s is %s's own project" (funcall leaf name) by)))
     ((and lead (eq (ghostherd-session-state lead) 'dead))
      (cons nil (format "%s, %s's lead, is dead; ask the user to start it again"
                        (ghostherd-session-name lead) (funcall leaf name))))
     ((and lead by-s (eq lead by-s))
      (cons nil (format "%s is %s's lead itself" by (funcall leaf name))))
     (lead (cons (ghostherd-session-name lead)
                 (format "handed to %s, %s's lead" (ghostherd-session-name lead)
                         (funcall leaf name))))
     ((not ghostherd-handoff-may-start-lead)
      (cons nil (format "%s has no lead, and the herd is set not to start one for a \
handoff (ghostherd-handoff-may-start-lead is off); ask the user" (funcall leaf name))))
     ((not (ghostherd--git-lines root "rev-parse" "--show-toplevel"))
      (cons nil (format "%s is not a git checkout, and has no lead: the herd starts a \
lead only in a git checkout; ask the user" (ghostherd--abbreviate root))))
     ((let ((up (ghostherd--handoff-leads-up)))
        (when (>= (length up) (max 0 ghostherd-handoff-lead-max))
          (cons nil (format "%s has no lead, and the herd has %d up that it started for \
handoffs (max %d): %s -- ask the user to start one there, or to stop one of those"
                            (funcall leaf name) (length up) ghostherd-handoff-lead-max
                            (mapconcat #'ghostherd-session-name up ", "))))))
     (t
      (let* ((kind (or (car (ghostherd-project-defaults root)) ghostherd-lead-kind))
             ;; Primary hands off the user's requests, from a place of its own.
             (who (cond ((member by '(nil "" "user")) "the user")
                        ((and by-s (equal (ghostherd-session-role by-s) "primary"))
                         (format "%s, for the user" by))
                        (t by)))
             (from (and from (not (ghostherd--primary-dir-p from)) from))
             (for (if from (format "%s in %s" who (funcall leaf from)) who)))
        (if (not (and (memq kind (ghostherd--agent-kinds))
                      (plist-get (ghostherd--spec kind) :command)))
            (cons nil (format "no agent kind %s to start %s's lead on" kind (funcall leaf name)))
          (condition-case err
              (let ((new (ghostherd-start-lead
                          root kind
                          (format "lead: plans the work, hands it out; started for a handoff from %s"
                                  for))))
                (puthash (ghostherd-session-id new) (or by "user") ghostherd--handoff-leads)
                ;; Asked now, as `ghostherd-lead-say' does, so that what the
                ;; project works on has been looked at by the time it is told.
                (ghostherd--project-work name)
                (ghostherd--log-add new 'life (format "started for a handoff from %s" for))
                (message "ghostherd: started %s for a handoff from %s"
                         (ghostherd-session-name new) for)
                (ghostherd--push 'lead-started new :by (or by "user") :from (or from "")
                                 :head (or head ""))
                (cons (ghostherd-session-name new)
                      (format "started %s (%s) in %s for a handoff from %s"
                              (ghostherd-session-name new) kind
                              (ghostherd--abbreviate root) for)))
            (error (cons nil (format "could not start %s's lead: %s" (funcall leaf name)
                                     (error-message-string err)))))))))))

;;; The guard of an agy agent
;;
;; A herd agy agent runs with --dangerously-skip-permissions, and its guard
;; -- `herd guard', bin/herd_guard.py -- is the only gate: a PreToolUse hook
;; (from the hooks.json in `--add-dir <its guard dir>') that answers allow
;; or deny.  agy 1.4.5's own `allow' does not skip its prompt without that
;; flag, and with it `ask' does not prompt (ask 134ac578), so what wants the
;; user is a denial they approve once, for that exact call, on the agent's
;; card (ask 75362f01).  The agent also starts with secret-looking variables
;; gone from its environment (`herd scrub-exec'): what is not there cannot
;; leak.  The guard directory -- its policy, its approvals -- is one per
;; agent, under `ghostherd-guard-root', and the guard refuses the agent
;; every look at it.  agy runs its tools in its first workspace, which is
;; that --add-dir, not the agent's own: the guard sends each command it
;; allows to where it checked it (`cd WORKSPACE && ...'), refuses relative
;; file paths, and the agent's first message names its workspace.

(defcustom ghostherd-agy-guard 'auto
  "How a herd agy agent's tool calls are checked.
`auto': its workspace's reads and writes, read-only commands, the herd's
and the project's own commands run; secrets, the world outside the
workspace and what the guard does not know are denied with a reason --
those the user may approve once on the agent's card.  `read-only':
reads in the workspace and read-only commands only (jnotes' rules).
nil: no guard, agy prompting for each command as it does.  A project
sets its own in .dir-locals.el.  Takes effect when an agy agent starts
or is respawned."
  :type '(choice (const :tag "Auto (guarded)" auto)
                 (const :tag "Read-only (guarded)" read-only)
                 (const :tag "Off: agy prompts" nil))
  :group 'ghostherd)
(put 'ghostherd-agy-guard 'safe-local-variable (lambda (v) (memq v '(nil read-only auto))))

(defcustom ghostherd-guard-allow-commands nil
  "A project's own commands its guarded agents run without asking, as
prefixes: (\"npm test\" \"pytest\").  Set it per project in .dir-locals.el.
Their files are checked all the same; not in `read-only'.  They run code
the agent can change (its tests, package scripts), which the guard never
sees: allow only what you would let it run unwatched."
  :type '(repeat string)
  :group 'ghostherd)
(put 'ghostherd-guard-allow-commands 'safe-local-variable
     (lambda (v) (and (listp v) (seq-every-p #'stringp v))))

(defcustom ghostherd-guard-request-ttl 3600
  "Seconds a call a guarded agent was refused waits for the user before it
expires: by then the turn that wanted it is long over.  An agent's open
requests also clear when it is respawned or relaunched."
  :type 'natnum
  :group 'ghostherd)

(defcustom ghostherd-guard-root "~/.ghostherd/guard/"
  "Where each guarded agent's guard directory goes: <root>/<agent>/.
The sidecar writes the user's approvals there, and only there
(GHOSTHERD_GUARD_ROOT)."
  :type 'directory
  :group 'ghostherd)

(defcustom ghostherd-agent-env-allow
  '(;; Where it is, who it is, its terminal and language.
    "PATH" "HOME" "USER" "LOGNAME" "SHELL" "PWD" "TMPDIR" "TZ"
    "TERM" "TERM_*" "TERMINFO" "TERMINFO_DIRS" "COLORTERM" "CLICOLOR"
    "LANG" "LANGUAGE" "LC_*" "__CF_USER_TEXT_ENCODING"
    "EDITOR" "VISUAL" "PAGER" "XDG_*"
    ;; Who git says made a commit.
    "GIT_AUTHOR_NAME" "GIT_AUTHOR_EMAIL" "GIT_COMMITTER_NAME" "GIT_COMMITTER_EMAIL"
    ;; The way out: a proxy (one with a password in it is dropped all the
    ;; same), and ssh-agent, which signs and gives no key away -- and every
    ;; use of it (git push, ssh) asks the user first.
    "HTTP_PROXY" "HTTPS_PROXY" "NO_PROXY" "ALL_PROXY" "http_proxy" "https_proxy" "no_proxy" "all_proxy"
    "SSH_AUTH_SOCK"
    ;; Switches that keep what it runs from phoning home: onnxruntime 1.29
    ;; posts telemetry to Microsoft unless this is set when it loads.
    "ORT_DISABLE_TELEMETRY"
    ;; The herd's own: GHOSTHERD_RPC holds the agents' token, not the herd's.
    "GHOSTHERD_RPC" "GHOSTHERD_RPC_FILE" "GHOSTHERD_SESSION" "GHOSTHERD_HERD"
    "GHOSTHERD_SOCKET" "GHOSTHERD_BACKEND")
  "The variables a guarded agent starts with -- every other one is left
behind: an allowlist, so DATABASE_URL, SENTRY_DSN, a *_PAT or a webhook
URL never reach it, whatever their names.  Names or globs (LC_*).  agy
itself needs none of the rest: it logs in from ~/.gemini (measured on
1.4.5).  A value with a credential in a URL (http://user:pass@proxy) is
dropped even when its name is allowed.  Add names with
`ghostherd-agent-env-allow-extra'."
  :type '(repeat string)
  :group 'ghostherd)

(defcustom ghostherd-agent-env-allow-extra nil
  "More variables a guarded agent starts with, on top of
`ghostherd-agent-env-allow': yours, or a project's in its .dir-locals.el
-- which Emacs asks you about first, since a project could name a secret
here."
  :type '(repeat string)
  :group 'ghostherd)

(make-obsolete-variable 'ghostherd-agent-env-scrub 'ghostherd-agent-env-allow "2026-10-22")
(make-obsolete-variable 'ghostherd-agent-env-keep 'ghostherd-agent-env-allow-extra "2026-10-22")

(defconst ghostherd--agy-guard-note "\
A herd guard checks each of your tool calls.  Your workspace is %s and your
commands run there.  %s is the herd's guard, listed among your workspaces:
it is not yours -- leave it alone.  Reading and changing files in your
workspace and read-only commands run; anything else is denied, with a
reason saying what to do instead.  A denial that needs the user says so:
they can approve that exact call once on your card, and the herd tells you
when -- then run exactly that call again.  Do not look for another way
around a denial, and never read secrets (.env, keys, tokens, the
environment) without that approval.

"
  "What a guarded agy agent is told first, before its memory opening: a
format string, its workspace and its guard directory.")

(defun ghostherd--dir-local (directory var default)
  "VAR as DIRECTORY's .dir-locals.el sets it, or DEFAULT when it does not --
nil set there counts.  Read as `ghostherd-project-defaults' reads them."
  (let ((directory (file-name-as-directory (expand-file-name directory))))
    (or (ignore-errors
          (with-temp-buffer
            (setq default-directory directory)
            (setq-local buffer-file-name (expand-file-name ".ghostherd" directory))
            (hack-dir-local-variables)
            (when-let* ((cell (assq var file-local-variables-alist)))
              (list (cdr cell)))))
        (list default))))

(defun ghostherd--agy-guard-mode (directory)
  "The guard mode an agy agent in DIRECTORY runs under: auto, read-only or nil."
  (let ((mode (car (ghostherd--dir-local directory 'ghostherd-agy-guard ghostherd-agy-guard))))
    (and (memq mode '(auto read-only)) mode)))

(defun ghostherd--guard-dir (name)
  "Agent NAME's guard directory."
  (file-name-as-directory (expand-file-name name (expand-file-name ghostherd-guard-root))))

(defun ghostherd--agy-guard-setup (name mode directory project)
  "Write agent NAME's guard: its policy -- MODE, its workspace DIRECTORY,
PROJECT to read too, the project's own commands, the herd's secrets -- and
the hooks.json agy loads from it.  Return the guard directory."
  (let* ((dir (ghostherd--guard-dir name))
         (herd (and (fboundp 'ghostherd-herd-client) (ghostherd-herd-client)))
         (cmd (concat (shell-quote-argument (or herd "herd")) " guard " (shell-quote-argument dir)
                      " || echo '{\"decision\":\"deny\",\"reason\":\"The herd guard did not answer; tell the user.\"}'")))
    (make-directory (expand-file-name ".agents" dir) t)
    ;; Rebuilt for a new process: the old one's allowances go with it.
    (ignore-errors (delete-file (expand-file-name "approved.json" dir)))
    (set-file-modes dir #o700)
    (let ((coding-system-for-write 'utf-8)
          (json-encoding-pretty-print t))
      (with-temp-file (expand-file-name "policy.json" dir)
        (insert (json-encode
                 (list :agent name
                       :mode (symbol-name mode)
                       :workspace (expand-file-name directory)
                       :read_roots (vconcat (and project (list (expand-file-name project))))
                       :allow_commands (vconcat (car (ghostherd--dir-local
                                                      directory 'ghostherd-guard-allow-commands
                                                      ghostherd-guard-allow-commands)))
                       :herd (vconcat (delq nil (list herd (and herd (expand-file-name
                                                                      "ghostherd" (file-name-directory herd))))))
                       :guard_root (expand-file-name ghostherd-guard-root)
                       :secret_dirs (vconcat (and (fboundp 'ghostherd-memory--data-directory)
                                                  (list (ghostherd-memory--data-directory))))
                       :secret_files (vconcat (and (fboundp 'ghostherd-memory--mail-directory)
                                                   (list (expand-file-name "rpc.url" (ghostherd-memory--mail-directory)))))
                       ;; The herd's own token: never an agent's, even approved.
                       :never_files (vconcat (and (fboundp 'ghostherd-memory--token-file)
                                                  (list (ghostherd-memory--token-file))))))))
      (with-temp-file (expand-file-name ".agents/hooks.json" dir)
        (insert (json-encode
                 `((ghostherd-guard
                    . ((PreToolUse
                        . [((matcher . "*")
                            (hooks . [((type . "command") (command . ,cmd) (timeout . 15))]))]))))))))
    dir))

(defun ghostherd--with-agy-guard (args dir)
  "ARGS with the guard's flags for DIR -- --dangerously-skip-permissions
and --add-dir DIR -- or, DIR nil, without them.  Any earlier start's pair
(an --add-dir under `ghostherd-guard-root', the flag before it) goes
first, so a respawn takes the guard as it is now."
  (let ((root (file-name-as-directory (expand-file-name ghostherd-guard-root)))
        (in (copy-sequence args)) out)
    (while in
      (let ((a (pop in)))
        (if (and (equal a "--dangerously-skip-permissions")
                 (equal (car in) "--add-dir")
                 (stringp (cadr in))
                 (string-prefix-p root (file-name-as-directory (expand-file-name (cadr in)))))
            (setq in (cddr in))
          (push a out))))
    (append (nreverse out)
            (and dir (list "--dangerously-skip-permissions" "--add-dir"
                           (directory-file-name dir))))))

(defun ghostherd--session-guarded-p (session)
  "Non-nil when SESSION runs behind the herd's guard: an agy started with
its guard's --dangerously-skip-permissions --add-dir pair."
  (let ((root (file-name-as-directory (expand-file-name ghostherd-guard-root))))
    (and (eq (ghostherd-session-kind session) 'agy)
         (cl-loop for (flag add dir) on (ghostherd-session-args session)
                  thereis (and (equal flag "--dangerously-skip-permissions")
                               (equal add "--add-dir")
                               (stringp dir)
                               (string-prefix-p root (file-name-as-directory (expand-file-name dir))))))))

(defun ghostherd--scrub-wrapper (&optional directory)
  "The words run in front of a guarded agent's CLI: `herd scrub-exec',
keeping only `ghostherd-agent-env-allow' and the extra names -- yours, or
DIRECTORY's project's -- in its environment."
  (when-let* (((fboundp 'ghostherd-herd-client))
              (herd (ghostherd-herd-client)))
    (append (list herd "scrub-exec")
            (mapcan (lambda (name) (list "--allow" name))
                    (append ghostherd-agent-env-allow
                            (if directory
                                (car (ghostherd--dir-local directory 'ghostherd-agent-env-allow-extra
                                                           ghostherd-agent-env-allow-extra))
                              ghostherd-agent-env-allow-extra)))
            (list "--"))))

(defvar ghostherd--guard-requests nil
  "The guard's requests from the last `herd_tick', newest first.")

(defvar ghostherd--guard-seen (make-hash-table :test 'equal)
  "Guard request id -> the status the log last told of it.")

;; A request waiting for the user stays `working' to the agent's state --
;; the guard refused the call and the agent goes on -- so no `blocked'
;; tells you of it.  It is told on its own, once: a desktop notification,
;; and a push while you are away.  The sidecar keeps that it was told
;; (`guard_told' in the next tick), so a restarted Emacs, or sidecar, does
;; not tell of it again.  An Emacs gone before that tick tells again
;; after its restart: twice, rather than never.

(defvar ghostherd--guard-told (make-hash-table :test 'equal)
  "Guard request id -> t: told of by this Emacs.")

(defvar ghostherd--guard-told-ids nil
  "Guard requests told of, for the sidecar in the next tick.")

(defun ghostherd--guard-tell (r)
  "Tell the user guard request R waits for them: a desktop notification,
and a push while away -- each when it is on."
  (let* ((name (or (plist-get r :session) "?"))
         (tool (or (plist-get r :tool) ""))
         (summary (or (plist-get r :summary) ""))
         (call (truncate-string-to-width
                (replace-regexp-in-string
                 "[\n\t ]+" " " (if (or (equal tool "") (string-prefix-p (concat tool ":") summary))
                                     summary
                                   (concat tool ": " summary)))
                120 nil nil "…")))
    (when ghostherd-notify-on-guard
      (ghostherd--notify (format "ghostherd · %s needs your approval" name)
                         (format "%s -- approve it or keep it denied on the herd page" call)))
    (when-let* ((session (ghostherd-get name)))
      (ghostherd--push 'guard session :request (plist-get r :id) :tool tool :summary summary))))

(defun ghostherd--herd-take-guard (requests)
  "Keep REQUESTS from a `herd_tick', and log each refusal, approval and
use once.  A new one waiting for the user is told of, once."
  (dolist (r requests)
    (let ((id (plist-get r :id))
          (status (plist-get r :status)))
      (when (and (equal status "pending")
                 (not (eq (plist-get r :told) t))
                 (not (gethash id ghostherd--guard-told)))
        (puthash id t ghostherd--guard-told)
        (push id ghostherd--guard-told-ids)
        (with-demoted-errors "ghostherd: telling of a guard request: %S"
          (ghostherd--guard-tell r)))
      (unless (equal (gethash id ghostherd--guard-seen) status)
        (puthash id status ghostherd--guard-seen)
        (ghostherd--log-add
         (or (plist-get r :session) "?") 'guard
         (pcase status
           ((or "pending" "denied")
            (format "guard %s %s: %s -- %s" (if (equal status "pending") "needs approval" "denied")
                    id (plist-get r :summary) (plist-get r :rule)))
           ("approved" (format "guard %s approved once: %s" id (plist-get r :summary)))
           ("used" (format "guard %s ran, approved: %s" id (plist-get r :summary)))
           ("declined" (format "guard %s kept denied by the user: %s" id (plist-get r :summary)))
           ("expired" (format "guard %s expired, never answered: %s" id (plist-get r :summary)))
           (_ (format "guard %s %s: %s" id status (plist-get r :summary))))))))
  (setq ghostherd--guard-requests requests))

(defvar ghostherd--lint-seen (make-hash-table :test 'equal)
  "The checks' denials the log has told of, by id.")

(defun ghostherd--herd-take-lint (denials)
  "Log each of DENIALS from a `herd_tick' once: a call one of the herd's
checks denied (bin/herd_lint.py) -- the fact, and the command, so a check
that denies what it should not shows.  The page counts them per check."
  (dolist (d (reverse denials))          ; the tick's are newest first
    (let ((id (plist-get d :id)))
      (unless (gethash id ghostherd--lint-seen)
        (puthash id t ghostherd--lint-seen)
        (ghostherd--log-add
         (or (plist-get d :session) "?") 'lint
         (format "check %s denied (%s): %s" (plist-get d :fact) (or (plist-get d :cli) "?")
                 (truncate-string-to-width
                  (replace-regexp-in-string "[\n\t ]+" " " (or (plist-get d :command) ""))
                  160 nil nil "…")))))))

(defun ghostherd--guard-clear (name)
  "Tell the sidecar agent NAME is a new process: its open guard requests expire."
  (when (and (fboundp 'ghostherd-memory-rpc-url) (ghostherd-memory-rpc-url)
             (fboundp 'ghostherd-memory-request-async))
    (ghostherd-memory-request-async "herd_guard_clear" #'ignore (list :session name) #'ignore)))

(defun ghostherd-guard-reject (id)
  "Keep the guard's request ID denied: it closes, and the agent is told not
to try that call again."
  (interactive (list (completing-read "Keep denied: "
                                      (mapcar (lambda (r) (plist-get r :id))
                                              (seq-filter (lambda (r) (equal (plist-get r :status) "pending"))
                                                          ghostherd--guard-requests)))))
  (ghostherd-memory-request-async
   "herd_guard_reject" (lambda (_r) (message "ghostherd: kept %s denied" id))
   (list :id id)
   (lambda (err) (message "ghostherd: keeping %s denied: %s" id err))))

(defun ghostherd-guard-approve (id)
  "Approve the guard's request ID once: the agent may run that exact call
again, this once (the sidecar writes it into the agent's guard, and tells
it)."
  (interactive (list (completing-read "Approve guard request: "
                                      (mapcar (lambda (r) (plist-get r :id))
                                              (seq-filter (lambda (r) (equal (plist-get r :status) "pending"))
                                                          ghostherd--guard-requests)))))
  (ghostherd-memory-request-async
   "herd_guard_approve" (lambda (_r) (message "ghostherd: approved %s once" id))
   (list :id id)
   (lambda (err) (message "ghostherd: approving %s: %s" id err))))

;;; Primary: the herd-wide lead
;;
;; The user's one entry point for what crosses projects, or where they do
;; not know which project it belongs to: one agent, `primary', in the
;; herd's own directory outside every repo.  It routes work to the
;; projects' leads by handoff and keeps track of it; it never writes code
;; and decides nothing that is the user's.  Its directory is its project:
;; so it has a memory store of its own (with the user's, as every project
;; has), a room on the page, and the herd's rules hold for it as for any
;; agent -- it may hand off, as a project with no lead's agent may, and
;; may not ask another project's agents.  Started when first written to
;; -- the page's Primary room, Telegram, M-x ghostherd-primary -- and,
;; once stopped or gone with the host, resumed on its last conversation,
;; so it keeps its context.  Nothing of its own runs on a timer: it wakes
;; when spoken to, or when an answer comes back.

(defcustom ghostherd-primary-kind 'claude
  "The CLI primary, the herd-wide lead, runs on.
Its conversation is resumed on restart only on the same CLI: changing
this starts the next primary afresh."
  :type 'symbol
  :group 'ghostherd)

(defcustom ghostherd-primary-directory "~/.ghostherd/primary/"
  "Where primary works: the herd's own directory, outside every repo.
It is primary's project -- its memory store, its room on the page --
and holds no checkout: primary edits none."
  :type 'directory
  :group 'ghostherd)

(defconst ghostherd--primary-name "primary"
  "Primary's name in the herd, which agents and the herd's words use.")

(defconst ghostherd--primary-role "\
You are primary: the herd-wide lead.  The user brings you what crosses
projects, or what they do not know where it belongs, and asks you what
everyone is doing.  You route the work and keep track of it; you do not
do it.  You never write code and never edit a checkout -- not a line --
and you do not run any project's agents: each project has a lead, or
gets one, and its work goes to it.

What you see, across every project:

  \"$GHOSTHERD_HERD\" list                 every agent: kind, state, notes,
                                         project, branch, the ask it is on
  \"$GHOSTHERD_HERD\" asks                 asks open and lately closed
  \"$GHOSTHERD_HERD\" recall --project P [QUERY]
                                         what project P's memory says

What you do: hand the work to the project it belongs to.

  \"$GHOSTHERD_HERD\" handoff PROJECT --wait <<'HERD'
  the brief
  HERD

PROJECT is its path or its name -- the name of a project with agents, a
memory store, or that claude has run in; one the herd does not know is
refused, saying which it does: give the path then, or ask the user.  Run
it in the background: you are told when the answer comes, and you tell
the user.  A project with no lead gets one started for it.  Brief it for a lead who knows nothing of your
conversation: what the user wants and why, what is known (files,
tickets, links, commits), what done looks like, and that the user waits.
Work that spans projects is a handoff to each, with its part, and says
what each depends on.  Not sure where something belongs: look first
(list, recall); still unsure, ask the user rather than guess.

You do not ask any project's agents yourself -- the herd refuses it:
their lead plans their work.  And agents do not ask you; the user does.

What is the user's to decide, you recommend and leave to them: stopping
or pausing agents, approving a memory promotion, merging, anything that
spends quota for long.  Say what you would do and why.

\"What is everyone doing?\", \"what is stuck?\": answer from herd list and
herd asks, by project -- who is on what, what is blocked or waiting and
on whom, what failed, what came back.  Briefly.

At the end of each turn, report what went where, who is on it and what
to wait for -- or what came back, and what it means for the user.
Briefly, and in the language they write to you in.  When they are away
it reaches them on their phone.

Your memory is your own store: \"$GHOSTHERD_HERD\" remember what you learn
about routing -- which project owns what, which lead handles what.  A
project's memory is its lead's, the user's preferences the user's: read
them with recall, change none.

You wake only when the user writes to you or an answer comes back.  No
polling, no loops, nothing on a timer.
"
  "Primary's role: given to it as it starts, or with its first message.")

(defun ghostherd--primary-args (kind)
  "Arguments that give a KIND primary its role as it starts; nil for a
CLI that takes none, which is owed it with its first message."
  (pcase kind
    ('claude (list "--append-system-prompt" ghostherd--primary-role))
    ('grok (list "--rules" ghostherd--primary-role))))

(defun ghostherd--primary-dir ()
  "Primary's directory, absolute, with a trailing slash."
  (file-name-as-directory (expand-file-name ghostherd-primary-directory)))

(defun ghostherd--primary-dir-p (dir)
  "Non-nil when DIR is primary's directory, however written."
  (and (stringp dir) (not (string-empty-p dir))
       (equal (ghostherd--project-name dir) (ghostherd--project-name (ghostherd--primary-dir)))))

(defun ghostherd--primary-session ()
  "Primary, the herd-wide lead, as the herd has it -- up or not -- or nil."
  (seq-find (lambda (s) (equal (ghostherd-session-role s) "primary")) (ghostherd-sessions)))

(defun ghostherd--primary-up (&optional session)
  "SESSION -- primary by default -- when it is up: its host there, not dead."
  (let ((s (or session (ghostherd--primary-session))))
    (and s (ghostherd--session-live-p s) (not (eq (ghostherd-session-state s) 'dead)) s)))

(defun ghostherd--primary-state-file ()
  "Where primary's last conversation is kept: beside its directory, not in
it, so it is no file of primary's."
  (concat (directory-file-name (ghostherd--primary-dir)) ".eld"))

(defun ghostherd--primary-remembered ()
  "Primary's last conversation as (:kind KIND :conversation ID), or nil."
  (let ((file (ghostherd--primary-state-file)))
    (when (file-readable-p file)
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents file)
          (let ((v (read (current-buffer))))
            (and (plistp v) (symbolp (plist-get v :kind))
                 (stringp (plist-get v :conversation)) v)))))))

(defun ghostherd--primary-remember ()
  "Keep the conversation primary is on, as its hooks name it, for the
next start to resume: a restart of Emacs, or of the host, loses the
agent but not this."
  (when-let* ((s (ghostherd--primary-session))
              (id (ghostherd-session-conversation s)))
    (let ((now (list :kind (ghostherd-session-kind s) :conversation id)))
      (unless (equal now (ghostherd--primary-remembered))
        (make-directory (file-name-directory (ghostherd--primary-state-file)) t)
        (with-temp-file (ghostherd--primary-state-file)
          (prin1 now (current-buffer)))))))

(defun ghostherd--primary-forget ()
  "Drop the kept conversation, so the next start is fresh."
  (let ((file (ghostherd--primary-state-file)))
    (when (file-exists-p file)
      (ignore-errors (delete-file file)))))

(defun ghostherd-primary-start (&optional display)
  "Primary, up: the one already up, or one started.  A primary stopped or
gone with its host is resumed on its last conversation -- the one its
hooks named, else the one kept for it (`ghostherd--primary-remember') --
when that was on the same CLI as `ghostherd-primary-kind'; otherwise it
starts afresh.  With DISPLAY, show it."
  (if-let* ((up (ghostherd--primary-up)))
      (progn (when display (ghostherd-visit up)) up)
    (let* ((old (ghostherd--primary-session))
           (kind ghostherd-primary-kind)
           (kept (ghostherd--primary-remembered))
           (id (or (and old (eq (ghostherd-session-kind old) kind)
                        (ghostherd-session-conversation old))
                   (and (eq (plist-get kept :kind) kind) (plist-get kept :conversation))))
           ;; A kept conversation that is gone (claude/agy, verifiable) would
           ;; make the CLI exit on resume: drop it so the next start is fresh,
           ;; not wedged.  grok's remote ones fall to the backstop.
           (_ (when (and id (ghostherd--conversation-gone-p kind id))
                (ghostherd--log-add
                 ghostherd--primary-name 'life
                 (format "kept conversation %s is gone; starting fresh" id))
                (ghostherd--primary-forget)
                (setq id nil)))
           (resume (and id (ghostherd--resume-args kind id)))
           (dir (ghostherd--primary-dir)))
      (when old (ghostherd-kill old t))
      (when-let* ((other (ghostherd-get ghostherd--primary-name)))
        (user-error "An agent named %s is not the herd's primary; rename it first"
                    (ghostherd-session-name other)))
      (unless (and (memq kind (ghostherd--agent-kinds)) (plist-get (ghostherd--spec kind) :command))
        (user-error "No agent kind %s for primary (ghostherd-primary-kind)" kind))
      (make-directory dir t)
      (let ((new (ghostherd-spawn
                  kind
                  :name ghostherd--primary-name
                  :project dir
                  :directory dir
                  :args (append (plist-get (ghostherd--spec kind) :args)
                                (ghostherd--primary-args kind) resume)
                  :notes "primary: the herd-wide lead -- routes work to projects, tracks it, reports"
                  :role "primary"
                  :display display)))
        ;; Resumed, its conversation has the role; a fresh one on a CLI that
        ;; takes none reads it with its first message.
        (unless (or resume (ghostherd--primary-args kind))
          (puthash (ghostherd-session-id new) ghostherd--primary-role ghostherd--role-owed))
        (ghostherd--log-add new 'life (if resume
                                          (format "resumed on its conversation %s" id)
                                        "started, a fresh conversation"))
        new))))

(defun ghostherd-primary-say (text)
  "Say TEXT to primary, starting or resuming it when it is not up -- then
once it is ready.  Return primary."
  (if-let* ((up (ghostherd--primary-up)))
      (progn (ghostherd-tell up text) up)
    (let ((new (ghostherd-primary-start)))
      (ghostherd--when-ready new (lambda () (ghostherd-tell new text)))
      new)))

;;;###autoload
(defun ghostherd-primary (&optional text)
  "Show primary, the herd-wide lead, starting or resuming it if need be.
With a prefix argument, write TEXT to it as well."
  (interactive (list (and current-prefix-arg (read-string "To primary: "))))
  (let ((s (if (and (stringp text) (not (string-empty-p (string-trim text))))
               (ghostherd-primary-say text)
             (ghostherd-primary-start))))
    (ghostherd-visit s)
    s))

(defun ghostherd--herd-run-command (cmd)
  "Run one sidecar Telegram CMD plist.  Return (:id ID :text TEXT), and
for a handoff the lead it went to as :lead."
  (let* ((id (plist-get cmd :id))
         (op (format "%s" (or (plist-get cmd :op) "")))
         (name (plist-get cmd :session))
         (args (plist-get cmd :args))
         (text "ok")
         lead)
    (condition-case err
        (pcase op
          ("prompt"
           (ghostherd-prompt name (or (plist-get args :body) "") nil)
           (setq text "prompted"))
          ("answer"
           (let ((n (plist-get args :n)))
             (ghostherd-answer name (truncate (or n 1))))
           (setq text "answering"))
          ("interrupt"
           (ghostherd-interrupt name)
           (setq text "interrupted"))
          ("abort"
           (ghostherd-abort name)
           (setq text "aborted"))
          ("kill"
           (ghostherd-kill name t)
           (setq text "killed"))
          ("respawn"
           (ghostherd-respawn name)
           (setq text "respawned"))
          ("relay"
           (if-let* ((by (plist-get args :by)))
               ;; Asked over the wire (`herd relay'): guarded by who asks.
               (setq text (ghostherd--relay-request by name (plist-get args :kind)
                                                    (plist-get args :reason)))
             ;; A page button or Telegram tap: the user, already trusted.
             (let ((kind (plist-get args :kind)))
               (unless (and (stringp kind) (memq (intern kind) (ghostherd--relay-kinds)))
                 (error "Relay to what: %s" kind))
               (ghostherd-relay name (intern kind))
               (setq text (format "relaying %s to %s" name kind)))))
          ("screen"
           (setq text (or (ghostherd-read name 40) "(empty)"))
           (when (> (length text) 3500)
             (setq text (concat (substring text 0 3500) "\n…"))))
          ("screens"
           ;; `herd screens': the saved screens from the log, read-only.
           (setq text (ghostherd--log-screens-text
                       (plist-get args :agent) (plist-get args :since)
                       (plist-get args :why))))
          ("start"
           ;; `herd start': a lead starts or reuses an agent in its project.
           (setq text (apply #'ghostherd-lead-start
                             (plist-get args :by) (plist-get args :kind)
                             (list :worktree (plist-get args :worktree)
                                   :branch (plist-get args :branch)
                                   :name (plist-get args :name)
                                   :notes (plist-get args :notes)
                                   :new (plist-get args :new)))))
          ("stop"
           ;; `herd stop': a lead stops an agent it started.
           (setq text (ghostherd-lead-stop (plist-get args :by) name)))
          ("primary-say"
           ;; Telegram, free-form: for primary, started or resumed if need be.
           (let ((body (string-trim (or (plist-get args :body) "")))
                 (up (ghostherd--primary-up)))
             (when (string-empty-p body)
               (error "Nothing to send"))
             (ghostherd-primary-say body)
             (setq text (if up "sent to primary"
                          "started primary; it reads this once it is up"))))
          ("handoff"
           ;; `herd handoff' to a project the sidecar found no lead in.
           (pcase-let ((`(,name . ,said)
                        (ghostherd-handoff-lead (plist-get args :by) (plist-get args :project)
                                                (plist-get args :from_project)
                                                (plist-get args :head))))
             (setq lead name text said)))
          ("explain"
           (setq text (ghostherd--explain-text name)))
          ("lead-say"
           (let ((project (plist-get args :project))
                 (body (string-trim (or (plist-get args :body) ""))))
             (unless (and (stringp project) (file-directory-p (expand-file-name project)))
               (error "No project %s" project))
             (when (string-empty-p body)
               (error "Nothing to send"))
             (setq text (format "sent to %s" (ghostherd-session-name (ghostherd-lead-say project body))))))
          (_ (setq text (format "unknown op %s" op))))
      (error (setq text (error-message-string err))))
    (append (list :id id :text text) (and lead (list :lead lead)))))

(defun ghostherd--herd-run-commands (result)
  "Execute Telegram commands from RESULT and queue replies."
  (dolist (cmd (plist-get result :commands))
    (push (ghostherd--herd-run-command cmd) ghostherd--herd-replies)))

;;; Relay: an heir takes an agent's name and its work
;;
;; An agent stops -- its CLI's usage used up, wedged, or you would
;; rather another CLI went on -- and a new agent takes its name, its
;; role, its notes and its project: the second of the name, the third.
;; Its first message is where the work stood, which the sidecar reads
;; from the old one's transcript, the herd's asks and the checkout
;; (server/relay.py): the old one may not be able to say anything any
;; more.  The name is what the rest of the herd knows it by, so asks of
;; it, the asks it waits on and mail for it go on as they were; the
;; asks it was answering are its heir's to answer, not failed with it.

(defcustom ghostherd-relay-kinds '(claude grok agy)
  "CLIs an agent's work is relayed to, the herd's first choice first.
`ghostherd-relay' offers the first that is not the agent's own and has
usage left; any other kind with a command can still be picked."
  :type '(repeat symbol)
  :group 'ghostherd)

(defvar ghostherd--relaying (make-hash-table :test 'equal)
  "Session id -> t while its relay waits for the sidecar's brief.")

(defvar ghostherd--relay-offered (make-hash-table :test 'equal)
  "Session id -> the used-up window it was last offered a relay for.")

(defun ghostherd--relay-kinds ()
  "The kinds an agent's work can go to: `ghostherd-relay-kinds' first,
then any other that runs a command."
  (seq-filter (lambda (k) (plist-get (alist-get k ghostherd-agent-specs) :command))
              (seq-uniq (append ghostherd-relay-kinds (ghostherd--agent-kinds)))))

(defun ghostherd-relay-default-kind (session)
  "The CLI SESSION's work goes to unless said otherwise: the first of
`ghostherd-relay-kinds' other than its own with usage left, its own
when none has."
  (let ((own (ghostherd-session-kind session)))
    (or (seq-find (lambda (k) (not (or (eq k own) (ghostherd-usage-used-up k))))
                  (ghostherd--relay-kinds))
        own)))

(defun ghostherd--relay-args (kind recipe lead)
  "Arguments for an heir on KIND to the agent of RECIPE, its :kind
added; LEAD when it leads its project.  On the same CLI, the old one's
own, less what picked its conversation; on another, what the project
or KIND gives an agent, since one CLI's flags mean nothing to another."
  (let* ((dir (or (plist-get recipe :directory) (plist-get recipe :project) default-directory))
         (defaults (ghostherd-project-defaults dir))
         (args (if (eq kind (plist-get recipe :kind))
                   (ghostherd--without-resume kind (plist-get recipe :args))
                 (if (eq (car defaults) kind)
                     (cdr defaults)
                   (plist-get (ghostherd--spec kind) :args)))))
    (if lead (ghostherd--with-lead-role kind args) args)))

(defun ghostherd--relay-ancestor (session reason)
  "SESSION as the line of its name remembers it, gone for REASON."
  `((kind . ,(symbol-name (ghostherd-session-kind session)))
    (conversation . ,(or (ghostherd-session-conversation session) ""))
    (ended . ,(format-time-string "%FT%T%z"))
    (reason . ,(or reason ""))))

(defcustom ghostherd-worktree-brief-commits 20
  "Most of a worktree branch's own commits listed in a relay brief (git
log base..branch) and files in its diffstat; the rest are counted, not
listed.  So the branch state stays within the brief's budget."
  :type 'integer
  :group 'ghostherd)

(defun ghostherd--worktree-brief (session)
  "Where SESSION's worktree branch stands against its base, to add to a
relay brief: the base, commits ahead and behind, the branch's own commits
and a diffstat against the base (each capped by
`ghostherd-worktree-brief-commits'), and what is uncommitted.  Nil for an
agent not in a herd worktree.  No full diff -- the heir reads the branch
itself; this orients it to a branch that has diverged from base."
  (let* ((wt (ghostherd-session-worktree session))
         (dir (and wt (plist-get wt :path)))
         (branch (and wt (plist-get wt :branch)))
         (base (and wt (plist-get wt :base))))
    (when (and dir branch base (file-directory-p (expand-file-name dir)))
      (let* ((cap (max 1 ghostherd-worktree-brief-commits))
             (counts (ghostherd--git-lines dir "rev-list" "--left-right" "--count"
                                           (concat base "..." branch)))
             (ba (and counts (split-string counts)))
             (behind (or (nth 0 ba) "0"))
             (ahead (or (nth 1 ba) "0"))
             (log (ghostherd--git-lines dir "log" "--oneline" "--no-decorate"
                                        (format "-n%d" cap) (concat base ".." branch)))
             (more (let ((n (string-to-number ahead))) (and (> n cap) (- n cap))))
             (stat (ghostherd--git-lines dir "diff" (format "--stat-count=%d" cap) "--stat"
                                         (concat base "..." branch)))
             (dirty (ghostherd--git-lines dir "status" "--porcelain=v1")))
        (when counts
          (string-join
           (delq nil
                 (list
                  (format "\n\n--- Your worktree: branch %s, forked from %s ---" branch base)
                  (format "%s commit(s) ahead of %s, %s behind it." ahead base behind)
                  (and log (concat "Its own commits (git log " base ".." branch "):\n" log
                                   (and more (format "\n  … and %d more" more))))
                  (and stat (concat "Changed against " base " (git diff --stat):\n" stat))
                  (if dirty
                      (concat "Uncommitted in the worktree:\n"
                              (if (> (length dirty) 800)
                                  (concat (substring dirty 0 800) "\n  …")
                                dirty))
                    "Nothing uncommitted in the worktree.")))
           "\n"))))))

;;;###autoload
(defun ghostherd-relay (session kind &optional reason)
  "Hand SESSION's work to a new agent on KIND, which takes its name.
The heir has SESSION's role, notes and project, and is the next of the
name; its first message is where SESSION's work stood -- what it was
asked, its last steps and answer, the asks it owes and waits on, the
checkout -- which the sidecar reads from SESSION's transcript.  The
asks SESSION was answering are the heir's to answer.  SESSION is
killed; its conversation stays where its CLI keeps it.

REASON says why SESSION stops; by default its CLI's usage used up,
when the usage says so.  The heir goes on with the work when SESSION
was cut short -- working or blocked, or its CLI's usage used up -- and
otherwise says where the work stands and waits to be told.

Interactively, acts on the sidebar row at point when there is one, and
offers `ghostherd-relay-default-kind'.
Returns at once: the heir starts when the sidecar has the brief."
  (interactive
   (let* ((s (or (and (derived-mode-p 'ghostherd-sidebar-mode)
                      (ghostherd--sidebar-session-at-point))
                 (ghostherd--read-session "Relay agent: ")))
          (default (ghostherd-relay-default-kind s)))
     (list s (intern (completing-read
                      (format "Relay %s (%s) to: " (ghostherd-session-name s)
                              (ghostherd-session-kind s))
                      (mapcar #'symbol-name (ghostherd--relay-kinds))
                      nil t nil nil (symbol-name default))))))
  (setq session (or (ghostherd-get session) (user-error "No such session")))
  (let* ((id (ghostherd-session-id session))
         (name (ghostherd-session-name session))
         (old (ghostherd-session-kind session))
         (reason (or reason (ghostherd-usage-session-used-up-text session))))
    (unless (plist-get (alist-get kind ghostherd-agent-specs) :command)
      (user-error "%s runs no agent" kind))
    (unless (plist-get (alist-get old ghostherd-agent-specs) :command)
      (user-error "%s is a shell, not an agent" name))
    (unless (and (fboundp 'ghostherd-memory-rpc-url) (ghostherd-memory-rpc-url))
      (user-error "A relay needs the herd's sidecar, to read %s's conversation" name))
    (when (gethash id ghostherd--relaying)
      (user-error "%s is already being relayed" name))
    (ghostherd--refuse-gone session "relaying")
    (puthash id t ghostherd--relaying)
    (ghostherd--log-add session 'life (format "relaying to %s: reading where its work stands" kind))
    (ghostherd-memory-request-async
     "herd_relay_brief"
     (lambda (brief)
       (remhash id ghostherd--relaying)
       ;; Add where the worktree branch stands, so an heir to a worktree
       ;; agent is not left with only the transcript when the branch has
       ;; diverged from its base.
       (ghostherd--relay-hand session kind
                              (concat (plist-get brief :text)
                                      (or (ghostherd--worktree-brief session) ""))
                              reason))
     (list :name name
           :path (or (ghostherd-session-transcript session) "")
           :project (or (ghostherd-session-project session) "")
           :from (symbol-name old)
           :to (symbol-name kind)
           :generation (1+ (ghostherd-session-generation session))
           :reason (or reason "")
           :cut (if (or (memq (ghostherd-session-state session) '(working blocked))
                        (ghostherd-usage-session-used-up session))
                    t :json-false)
           :notes (or (ghostherd-session-notes session) "")
           :client (or (and (fboundp 'ghostherd-herd-client) (ghostherd-herd-client)) "herd"))
     (lambda (err)
       (remhash id ghostherd--relaying)
       (ghostherd--log-add session 'life (format "relay to %s failed: %s" kind err))
       (message "ghostherd: relaying %s: %s" name err)))
    nil))

(defun ghostherd--relay-hand (session kind text reason)
  "Kill SESSION and start its heir on KIND, told TEXT once it is ready.
REASON is why SESSION stopped.  The asks SESSION was answering are held
for the heir meanwhile.  Return the heir, or nil when SESSION left the
herd, or its directory went, while the brief was written."
  (let ((id (ghostherd-session-id session))
        (name (ghostherd-session-name session)))
    (cond
     ((not (eq (gethash id ghostherd--sessions) session))
      (message "ghostherd: %s left the herd before its heir could take over" name)
      nil)
     ;; Its directory went while the brief was written: it stays as it is.
     ((ghostherd--mark-gone session)
      (ghostherd--log-add session 'life (format "not relayed to %s: its directory is gone" kind))
      (message "ghostherd: %s's directory is gone: not relaying it; stop it instead" name)
      nil)
     (t
      (let* ((old (ghostherd-session-kind session))
             (recipe (plist-put (ghostherd-session-recipe session) :kind old))
             (lead (equal (plist-get recipe :role) "lead"))
             (held (ghostherd--asks-of session))
             (lineage (append (ghostherd-session-lineage session)
                              (list (ghostherd--relay-ancestor session reason))))
             (buffer (ghostherd-session-buffer session))
             (shown (and (buffer-live-p buffer) (get-buffer-window buffer t) t))
             (args (ghostherd--relay-args kind recipe lead))
             heir)
        ;; Its asks are its heir's, not failed with it.
        (dolist (ask held)
          (remhash ask ghostherd--asks-open)
          (puthash ask name ghostherd--relay-held))
        (ghostherd--handoff-cancel id)
        ;; What its hooks said, and what its lead was told, were its own.
        (remhash name ghostherd--herd-links)
        (remhash id ghostherd--lead-work-told)
        (ghostherd-memory-request-async "herd_unlink" #'ignore (list :session name) #'ignore)
        (ghostherd-kill session t)
        (setq heir (ghostherd-spawn kind
                                    :name name
                                    :project (plist-get recipe :project)
                                    :directory (or (plist-get recipe :directory)
                                                   (plist-get recipe :project))
                                    :backend (plist-get recipe :backend)
                                    :args args
                                    :notes (plist-get recipe :notes)
                                    :role (plist-get recipe :role)
                                    :lineage lineage
                                    :display shown))
        (ghostherd--log-add heir 'life
                            (format "%s %s, on %s: took over from %s%s"
                                    name (ghostherd--roman (ghostherd-session-generation heir))
                                    kind old (if reason (concat " -- " reason) "")))
        (ghostherd--when-ready heir
                               (lambda () (ghostherd--relay-tell heir text held lead))
                               nil
                               (lambda () (ghostherd--relay-release heir held)))
        heir)))))

(defun ghostherd--relay-tell (heir text held lead)
  "Tell HEIR its work, TEXT; then the asks HELD for it are its to answer.
LEAD when it leads its project: it hears what a lead hears first."
  (if lead
      (ghostherd-tell heir text (unless (ghostherd--lead-args (ghostherd-session-kind heir))
                                  (concat ghostherd--lead-role "\n---\n\n"))
                      t)
    (let ((ghostherd--input-said (concat "[from the herd] " text)))
      (ghostherd-prompt heir text)))
  (dolist (ask held)
    (remhash ask ghostherd--relay-held)
    (puthash ask (ghostherd-session-id heir) ghostherd--asks-open))
  (ghostherd--log-add heir 'life "told where the work stood"))

(defun ghostherd--relay-release (heir held)
  "The asks HELD for HEIR fail: it never came to be told them."
  (dolist (ask held)
    (remhash ask ghostherd--relay-held)
    (ghostherd--ask-settle ask heir nil
                           (format "%s's heir never came to its prompt" (ghostherd-session-name heir)))))

(defun ghostherd--relay-offer (session old new)
  "SESSION stopped working, from OLD to NEW, with its CLI's usage used
up: say so, and that a relay would carry its work on -- once for each
time its usage runs out.

When it settles and is no longer used up, the offer is forgotten.  The
card's relay button reads the live used-up each poll, so it clears on its
own; this is for the push memo, so a later, real used-up offers again
rather than being taken for the one already sent."
  (when (memq new '(idle blocked done))
    (let* ((id (ghostherd-session-id session))
           (kind (ghostherd-session-kind session))
           (window (ghostherd-usage-session-used-up session)))
      (if (not window)
          (remhash id ghostherd--relay-offered)
        (when-let* (((eq old 'working))
                    (to (ghostherd-relay-default-kind session))
                    ((not (eq to kind)))
                    (key (list kind (plist-get window :label) (plist-get window :resets-at)))
                    ((not (equal (gethash id ghostherd--relay-offered) key))))
          (puthash id key ghostherd--relay-offered)
          (let ((why (ghostherd-usage-session-used-up-text session)))
            (ghostherd--log-add session 'life (format "stopped: %s.  A relay hands its work to %s" why to))
            (message "ghostherd: %s stopped: %s -- M-x ghostherd-relay hands its work to %s"
                     (ghostherd-session-name session) why to)
            (ghostherd--push 'used-up session :from (symbol-name kind) :to (symbol-name to) :reason why)))))))

(add-hook 'ghostherd-state-change-hook #'ghostherd--relay-offer)

;;; Pushes to your phone
;;
;; The herd page shows everything -- while you look at it.  Telegram is
;; for when you do not: the sidecar's bot pushes what wants you, an agent
;; blocked, a lead's report at the end of its turn, an ask that failed.
;; Emacs decides when one is worth it -- you are looking at neither the
;; agent nor the herd page, and have left this machine -- and the bot
;; words it: it has the lead's last message, and the buttons to answer.

(defcustom ghostherd-push '(blocked report ask-failed used-up lead-started guard)
  "What the herd pushes to Telegram while you are away: an agent
`blocked', a lead's `report' at the end of its turn, an `ask-failed',
an agent stopped with its CLI's usage `used-up' -- with a button that
relays it -- a lead the herd started in a project for a handoff
from another, `lead-started', and a call a guarded agent's `guard'
refused, waiting for your approval -- told only: it is approved on the
herd page.  Nil pushes nothing.  The sidecar's bot sends them to the
chats of GHOSTHERD_TELEGRAM_CHAT_IDS; a reply to one goes to that
project's lead."
  :type '(set (const blocked) (const report) (const ask-failed) (const used-up)
              (const lead-started) (const guard))
  :group 'ghostherd)

(defcustom ghostherd-push-images t
  "Non-nil: a lead's report pushed to Telegram brings the images its words
name, after its text -- one as a photo, more as an album, one too big for
a photo as a file; four at most.  They are the copies the herd keeps of
what an agent's reply names (`ghostherd-reply-images-directory'), and go to
Telegram's servers as the text does: nil keeps them to the herd page."
  :type 'boolean
  :group 'ghostherd)

(defcustom ghostherd-push-reports t
  "Non-nil: a lead's report pushed to Telegram brings the HTML pages its
words name -- a report an agent wrote as one -- as files after its text,
two at most.  They are the copies the herd keeps (beside the images, in
`ghostherd-reply-images-directory'), and go to Telegram's servers as the
text does -- the file itself, never a link: the herd page's address
holds its secret.  Nil keeps them to the herd page, where a message that
names one has Open under it."
  :type 'boolean
  :group 'ghostherd)

(defcustom ghostherd-push-away-after 120
  "Seconds without a key or the mouse on this Mac after which you count
as away from it, which is when a push goes out.  Nil: whenever you look
at neither the agent nor the herd page, at the desk too.  Where the idle
time cannot be read (no ioreg), you count as away."
  :type '(choice (const :tag "Whenever not looking" nil) natnum)
  :group 'ghostherd)

(defvar ghostherd-present-functions nil
  "Functions of no arguments saying whether the herd page is in front of
you now.  The first non-nil answer holds pushes back.")

(defvar ghostherd-room-url-function nil
  "Function of a project giving the address of its room from your phone
-- of the herd page itself, for nil -- for a push or the bot to link
to; nil, or a function giving nil, links none.")

(defvar ghostherd--room-urls (make-hash-table :test 'equal)
  "(FUNCTION PROJECT HOSTS SCHEME) -> what FUNCTION said is PROJECT's
address from your phone.  Asked once: asking keeps xwapp's address
over restarts, which writes a file, and the herd's tick asks every
second.")

(defun ghostherd--room-url (&optional project)
  "The address of PROJECT's room from your phone -- of the herd page, for
nil -- as `ghostherd-room-url-function' gives it; nil when it gives none."
  (when ghostherd-room-url-function
    (let ((key (list ghostherd-room-url-function project
                     (bound-and-true-p xwapp-remote-hosts)
                     (bound-and-true-p xwapp-remote-scheme))))
      (or (gethash key ghostherd--room-urls)
          (puthash key (ignore-errors (funcall ghostherd-room-url-function project))
                   ghostherd--room-urls)))))

(defvar ghostherd--herd-pushes nil
  "Pushes not yet handed to the sidecar, newest first.")

(defvar ghostherd--working-since (make-hash-table :test 'equal)
  "Session id -> when it last began working: a lead's report is what it
said after that.")

(defun ghostherd--idle-seconds ()
  "Seconds since a key or the mouse was last touched on this Mac, or nil."
  (when (executable-find "ioreg")
    (with-temp-buffer
      (when (and (eq 0 (ignore-errors
                         (call-process "ioreg" nil t nil "-c" "IOHIDSystem" "-d" "4" "-k" "HIDIdleTime")))
                 (re-search-backward "\"HIDIdleTime\" = \\([0-9]+\\)" nil t))
        (/ (string-to-number (match-string 1)) 1e9)))))

(defun ghostherd--away-p ()
  "Non-nil when you count as away from this machine; see
`ghostherd-push-away-after'."
  (or (null ghostherd-push-away-after)
      (let ((idle (ghostherd--idle-seconds)))
        (or (null idle) (>= idle ghostherd-push-away-after)))))

(defun ghostherd--push (kind session &rest fields)
  "Hand the sidecar a push of KIND about SESSION, with FIELDS (a plist),
when it is worth one: see `ghostherd-push'."
  (when (and (memq kind ghostherd-push)
             (getenv "GHOSTHERD_TELEGRAM_TOKEN")
             (getenv "GHOSTHERD_TELEGRAM_CHAT_IDS")
             (not (ghostherd--watched-p session))
             (not (run-hook-with-args-until-success 'ghostherd-present-functions))
             (ghostherd--away-p))
    (let ((project (ghostherd--project-name (ghostherd-session-project session))))
      (push (append (list :kind (symbol-name kind)
                          :session (ghostherd-session-name session)
                          :project (or project "")
                          :url (or (and project (ghostherd--room-url project)) "")
                          :images (if ghostherd-push-images t :json-false)
                          :reports (if ghostherd-push-reports t :json-false))
                    fields)
            ghostherd--herd-pushes))))

(defun ghostherd--push-state (session _old new)
  "Push SESSION's turning NEW: blocked, or a lead done with its turn."
  (pcase new
    ('working (puthash (ghostherd-session-id session) (float-time) ghostherd--working-since))
    ('blocked (ghostherd--push 'blocked session :reason (or (ghostherd-session-state-reason session) "")))
    ('done (when (member (ghostherd-session-role session) '("lead" "primary"))
             (ghostherd--push 'report session
                              :since (gethash (ghostherd-session-id session) ghostherd--working-since 0))))))

(add-hook 'ghostherd-state-change-hook #'ghostherd--push-state)

;;;###autoload
(defun ghostherd-push-test ()
  "Push a message to your phone now, away or not: to see that the herd
reaches you there."
  (interactive)
  (unless (and (getenv "GHOSTHERD_TELEGRAM_TOKEN") (getenv "GHOSTHERD_TELEGRAM_CHAT_IDS"))
    (user-error "No Telegram bot: GHOSTHERD_TELEGRAM_TOKEN and GHOSTHERD_TELEGRAM_CHAT_IDS"))
  (push (list :kind "test" :session "ghostherd" :project "" :url "") ghostherd--herd-pushes)
  (message "A test push goes to Telegram with the herd's next tick"))

(defun ghostherd--herd-tick-async ()
  "Push the snapshot and pull mail plus Telegram commands.  Never blocks."
  (when (and (not ghostherd--herd-tick-inflight)
             (fboundp 'ghostherd-memory-rpc-url)
             (ghostherd-memory-rpc-url)
             (fboundp 'ghostherd-memory-request-async))
    (let ((acks (or ghostherd--herd-ack-ids []))
          (replies ghostherd--herd-replies)
          (pushes ghostherd--herd-pushes)
          (told ghostherd--guard-told-ids))
      (setq ghostherd--herd-tick-inflight t
            ghostherd--herd-replies nil
            ghostherd--herd-pushes nil
            ghostherd--guard-told-ids nil)
      (ghostherd-memory-request-async
       "herd_tick"
       (lambda (result)
         (setq ghostherd--herd-tick-inflight nil
               ghostherd--herd-ack-ids
               (seq-difference ghostherd--herd-ack-ids
                               (append acks nil)))
         (ghostherd--herd-deliver result)
         (ghostherd--herd-take-asks (plist-get result :asks))
         (ghostherd--herd-take-links (plist-get result :links))
         (ghostherd--herd-take-reports (plist-get result :reports))
         (ghostherd--herd-take-guard (plist-get result :guard))
         (ghostherd--herd-take-lint (plist-get result :lint))
         (ghostherd--herd-run-commands result))
       (list :sessions (vconcat (ghostherd--herd-snapshot))
             :page_url (or (ghostherd--room-url) "")
             ;; Whether this Emacs has restored its herd yet: before it, an
             ;; empty snapshot is a herd not yet picked up, and the sidecar
             ;; must not fail an open ask for a taker missing from it.
             :restored (if ghostherd--restored t :json-false)
             :ack_ids (vconcat (append acks nil))
             :replies (vconcat (or replies []))
             :pushes (vconcat (reverse pushes))
             ;; How long a guarded agent's refused call waits for the user.
             :guard_ttl ghostherd-guard-request-ttl
             ;; The guard's requests told of since: not again.
             :guard_told (vconcat (reverse told)))
       (lambda (_err)
         (setq ghostherd--herd-tick-inflight nil
               ghostherd--herd-replies
               (append replies ghostherd--herd-replies)
               ghostherd--herd-pushes
               (append ghostherd--herd-pushes pushes)
               ghostherd--guard-told-ids
               (append ghostherd--guard-told-ids told)))))))

(defun ghostherd-sidebar-refresh ()
  "Interactive sidebar refresh."
  (interactive)
  (ghostherd--sidebar-entries)
  (ghostherd--sidebar-print t)
  (when (fboundp 'ghostherd-usage-refresh)
    (ghostherd-usage-refresh))
  (when (and (ghostherd--sidebar-posframe-showing-p)
             (fboundp 'posframe-refresh))
    (posframe-refresh (current-buffer)))
  (message "ghostherd refreshed"))

(defun ghostherd--sidebar-session-at-point ()
  "Return the session for the sidebar row at point."
  (ghostherd-get (tabulated-list-get-id)))

(defun ghostherd-sidebar-visit ()
  "Visit the session at point, attaching a view when it is detached."
  (interactive)
  (when-let* ((s (ghostherd--sidebar-session-at-point)))
    (ghostherd-visit s)))

(defun ghostherd-sidebar-changes ()
  "Show what the checkout of the session at point has not committed: its
status in magit, or `vc-dir' without magit."
  (interactive)
  (when-let* ((s (ghostherd--sidebar-session-at-point)))
    (let ((dir (ghostherd-session-directory s)))
      (unless (and dir (file-directory-p (expand-file-name dir)))
        (user-error "%s has no directory here" (ghostherd-session-name s)))
      ;; The overlay's window is dedicated: magit has nowhere to go from it.
      (ghostherd--sidebar-leave-overlay)
      (if (require 'magit nil t)
          (magit-status-setup-buffer (expand-file-name dir))
        (vc-dir (expand-file-name dir))))))

(defun ghostherd-sidebar-kill ()
  "Kill the session at point."
  (interactive)
  (when-let* ((s (ghostherd--sidebar-session-at-point)))
    (when (y-or-n-p (format "Kill session %s? " (ghostherd-session-name s)))
      (ghostherd-kill s t))))

(defun ghostherd-sidebar-rename ()
  "Rename the session at point."
  (interactive)
  (when-let* ((s (ghostherd--sidebar-session-at-point)))
    (call-interactively
     (lambda ()
       (interactive)
       (ghostherd-rename
        s
        (read-string "New name: " (ghostherd-session-name s)))))))

(defun ghostherd-sidebar-notes ()
  "Edit the note on the session at point."
  (interactive)
  (when-let* ((s (ghostherd--sidebar-session-at-point)))
    (ghostherd-set-notes
     s (read-string "Note: " (ghostherd-session-notes s)))))

(defun ghostherd-sidebar-interrupt ()
  "Send Escape to the session at point."
  (interactive)
  (when-let* ((s (ghostherd--sidebar-session-at-point)))
    (ghostherd-interrupt s)))

(defun ghostherd-sidebar-abort ()
  "Send C-c to the session at point."
  (interactive)
  (when-let* ((s (ghostherd--sidebar-session-at-point)))
    (ghostherd-abort s)))

(defun ghostherd-sidebar-answer ()
  "Answer the menu prompt of the session at point."
  (interactive)
  (when-let* ((s (ghostherd--sidebar-session-at-point)))
    (ghostherd-answer s (read-number "Choice (1-based): " 1))))

(defun ghostherd--sidebar-caller-session ()
  "Session that opened the list, if the parent window is an agent view.

The overlay buffer is never a session -- `ghostherd-get' of it is nil,
and treating that as FROM used to be harmless only because
`ghostherd-message' falls back to \"user\".  The parent frame's window
is the one that had focus before the overlay, so a message sent from
an agent view is still attributed to that agent."
  (when (and ghostherd--sidebar-posframe-parent
             (frame-live-p ghostherd--sidebar-posframe-parent))
    (ghostherd-get (window-buffer
                    (frame-selected-window
                     ghostherd--sidebar-posframe-parent)))))

(defun ghostherd-sidebar-message ()
  "Message the session at point."
  (interactive)
  (when-let* ((s (ghostherd--sidebar-session-at-point))
              (body (read-string
                     (format "Message → %s: " (ghostherd-session-name s)))))
    ;; SUBMIT is a keyword.  A bare `t' is `Keyword argument t not one
    ;; of (:submit)' -- which is how overlay `m' failed while
    ;; `SPC a h m' worked, the interactive command having used :submit.
    (ghostherd-message (or (ghostherd--sidebar-caller-session) "user")
                       s body :submit t)))

(defun ghostherd-sidebar-prompt ()
  "Prompt the session at point."
  (interactive)
  (when-let* ((s (ghostherd--sidebar-session-at-point))
              (body (read-string
                     (format "Prompt %s: " (ghostherd-session-name s)))))
    (ghostherd-prompt s body)
    (message "Prompted %s" (ghostherd-session-name s))))

(defun ghostherd-sidebar-mark-state ()
  "Manually mark state for session at point."
  (interactive)
  (when-let* ((s (ghostherd--sidebar-session-at-point)))
    (ghostherd-mark-state
     s
     (intern (completing-read
              "State: "
              '("auto" "working" "blocked" "idle" "done" "dead")
              nil t)))))

(defconst ghostherd--sidebar-all-projects "All projects"
  "Candidate that clears the project filter.")

(defun ghostherd--sidebar-project-table ()
  "List of (DISPLAY ROOT COUNT) for the projects the herd has sessions in."
  (let ((seen nil))
    (dolist (s (ghostherd-sessions))
      (when-let* ((p (ghostherd-session-project s)))
        (let* ((root (file-name-as-directory (expand-file-name p)))
               (cell (assoc root seen)))
          (if cell
              (cl-incf (nth 1 cell))
            (push (list root 1) seen)))))
    (mapcar (lambda (c) (list (ghostherd--abbreviate (car c)) (car c) (nth 1 c)))
            (cl-sort seen #'string< :key #'car))))

(defun ghostherd--sidebar-caller-project ()
  "Project root of the buffer the list was opened over, or nil.

The overlay buffer's own `default-directory' is wherever the
posframe happened to be created, so `project-current' asked from
inside it answers for ghostherd's own directory no matter which
project you were actually looking at."
  (let ((buf (and ghostherd--sidebar-posframe-parent
                  (frame-live-p ghostherd--sidebar-posframe-parent)
                  (window-buffer (frame-selected-window
                                  ghostherd--sidebar-posframe-parent)))))
    (if (buffer-live-p buf)
        (with-current-buffer buf (ghostherd--project-root))
      (ghostherd--project-root))))

(defun ghostherd--sidebar-pick-candidates ()
  "Picker candidates: `All projects' first, then one row per project."
  (cons (list ghostherd--sidebar-all-projects nil
              (length (ghostherd-sessions)))
        (ghostherd--sidebar-project-table)))

(defun ghostherd--sidebar-pick-initial (candidates)
  "Index in CANDIDATES the picker should open on.
The row under point when nothing is filtered -- it is the one
thing on screen that says which project you mean -- and `All
projects' when something is, so `s RET' still just clears."
  (if ghostherd--sidebar-filter-project
      0
    (let* ((s (ghostherd--sidebar-session-at-point))
           (root (or (and s (ghostherd-session-project s))
                     (ghostherd--sidebar-caller-project))))
      (or (and root
               (seq-position candidates root
                             (lambda (c r)
                               (and (nth 1 c)
                                    (ignore-errors (file-equal-p (nth 1 c) r))))))
          0))))

(defun ghostherd--sidebar-pick-text ()
  "Render the project picker for the overlay pane."
  (let* ((cands ghostherd--sidebar-picking)
         (width (max 24 (1- (ghostherd--sidebar-available-width))))
         (namew (max 12 (min (apply #'max 0 (mapcar (lambda (c) (length (car c)))
                                                    cands))
                             (- width 20)))))
    (concat
     (propertize "Show project   RET choose · n/p move · 1-9 jump · Esc cancel"
                 'face 'shadow)
     "\n"
     (string-join
      (cl-loop
       for c in cands
       for i from 0
       collect
       (let* ((selected (= i ghostherd--sidebar-pick-index))
              (n (nth 2 c))
              (line (format "%s%2d  %s  %s"
                            (if selected "›" " ")
                            (1+ i)
                            (string-pad
                             (truncate-string-to-width (car c) namew nil nil t)
                             namew)
                            (if (nth 1 c)
                                (format "%d session%s" n (if (= n 1) "" "s"))
                              (format "%d in all" n)))))
         (if selected
             (propertize line 'face 'highlight)
           line)))
      "\n"))))

(defun ghostherd--sidebar-pick-redraw ()
  "Redraw the pane and refit the overlay around it."
  (when-let* ((buf (get-buffer "*ghostherd*")))
    (with-current-buffer buf
      (ghostherd--sidebar-draw-preview)
      (when (ghostherd--sidebar-posframe-showing-p)
        (ghostherd--sidebar-show-posframe buf))))
  (force-mode-line-update t))

(defun ghostherd--set-picking (candidates)
  "Open the picker on CANDIDATES, or close it when nil.
State and keymap only -- the caller redraws.  Refreshes Evil's map
alist for the same reason live-narrow does: the intercept map has
to outrank the overlay's normal-state letters."
  (setq ghostherd--sidebar-picking candidates)
  (when-let* ((buf (get-buffer "*ghostherd*")))
    (with-current-buffer buf
      (ghostherd-pick-mode (if candidates 1 -1))
      (when (fboundp 'evil-normalize-keymaps)
        (evil-normalize-keymaps)))))

(defun ghostherd--sidebar-apply-filter (root)
  "Show only sessions under ROOT, or all of them when ROOT is nil."
  (setq ghostherd--sidebar-filter-project root)
  (when-let* ((buf (get-buffer "*ghostherd*")))
    (with-current-buffer buf
      (when (derived-mode-p 'ghostherd-sidebar-mode)
        (ghostherd--sidebar-build-entries)
        (ghostherd--sidebar-print t)
        (when (ghostherd--sidebar-posframe-showing-p)
          (ghostherd--sidebar-show-posframe buf)))))
  (force-mode-line-update t)
  (message "Project filter: %s" (if root (ghostherd--abbreviate root) "off")))

(defun ghostherd-sidebar-project-filter ()
  "Choose which project the session list shows.

The picker draws in the pane `?' and `v' already use and takes its
keys there, like live-narrow.  It used to be a `completing-read':
in the posframe overlay that put the prompt at the bottom of the
parent frame, nowhere near the floating list you were reading, and
left the order to whatever completion UI happened to be installed.

`s' again closes it; RET applies the highlighted row."
  (interactive)
  (if ghostherd--sidebar-picking
      (ghostherd-sidebar-pick-cancel)
    (let ((cands (ghostherd--sidebar-pick-candidates)))
      (when (and (null (cdr cands)) (not ghostherd--sidebar-filter-project))
        (user-error "No sessions with a project"))
      (setq ghostherd--sidebar-pick-index (ghostherd--sidebar-pick-initial cands))
      (ghostherd--set-picking cands)
      (ghostherd--sidebar-pick-redraw))))

(defun ghostherd-sidebar-pick-cancel ()
  "Close the project picker, leaving the filter as it was."
  (interactive)
  (ghostherd--set-picking nil)
  (ghostherd--sidebar-pick-redraw))

(defun ghostherd-sidebar-pick-confirm ()
  "Apply the selected project and close the picker."
  (interactive)
  (let ((root (nth 1 (nth ghostherd--sidebar-pick-index
                          ghostherd--sidebar-picking))))
    (ghostherd--set-picking nil)
    (ghostherd--sidebar-apply-filter root)))

(defun ghostherd--sidebar-pick-move (delta)
  "Move the picker selection by DELTA, wrapping at both ends."
  (when ghostherd--sidebar-picking
    (setq ghostherd--sidebar-pick-index
          (mod (+ ghostherd--sidebar-pick-index delta)
               (length ghostherd--sidebar-picking)))
    (ghostherd--sidebar-pick-redraw)))

(defun ghostherd-sidebar-pick-next ()
  "Select the next project."
  (interactive)
  (ghostherd--sidebar-pick-move 1))

(defun ghostherd-sidebar-pick-previous ()
  "Select the previous project."
  (interactive)
  (ghostherd--sidebar-pick-move -1))

(defun ghostherd-sidebar-pick-digit ()
  "Apply the numbered project straight away."
  (interactive)
  (let ((n (- (event-basic-type last-command-event) ?0)))
    (if (and (>= n 1) (<= n (length ghostherd--sidebar-picking)))
        (progn (setq ghostherd--sidebar-pick-index (1- n))
               (ghostherd-sidebar-pick-confirm))
      (message "No project %d" n))))

(defun ghostherd-sidebar-pick-ignore ()
  "Swallow a key that means nothing to the picker.
Without this, `x' while picking would reach the overlay map and
kill the session under point."
  (interactive)
  nil)

(defvar-keymap ghostherd-sidebar-pick-map
  :doc "Keymap while the project picker owns the overlay pane.
Modal on purpose: every other key is swallowed rather than run as
an overlay command.  Esc, C-g, q and `s' all close it."
  "RET"         #'ghostherd-sidebar-pick-confirm
  "C-m"         #'ghostherd-sidebar-pick-confirm
  "<return>"    #'ghostherd-sidebar-pick-confirm
  "SPC"         #'ghostherd-sidebar-pick-confirm
  "n"           #'ghostherd-sidebar-pick-next
  "p"           #'ghostherd-sidebar-pick-previous
  "j"           #'ghostherd-sidebar-pick-next
  "k"           #'ghostherd-sidebar-pick-previous
  "C-n"         #'ghostherd-sidebar-pick-next
  "C-p"         #'ghostherd-sidebar-pick-previous
  "<down>"      #'ghostherd-sidebar-pick-next
  "<up>"        #'ghostherd-sidebar-pick-previous
  "s"           #'ghostherd-sidebar-pick-cancel
  "q"           #'ghostherd-sidebar-pick-cancel
  "C-g"         #'ghostherd-sidebar-pick-cancel
  "<escape>"    #'ghostherd-sidebar-pick-cancel)

(define-key ghostherd-sidebar-pick-map [t] #'ghostherd-sidebar-pick-ignore)
;; After `[t]': events the catch-all would otherwise swallow before
;; `function-key-map' translates them (see the filter map).
(define-key ghostherd-sidebar-pick-map [return] #'ghostherd-sidebar-pick-confirm)
(define-key ghostherd-sidebar-pick-map [kp-enter] #'ghostherd-sidebar-pick-confirm)
(define-key ghostherd-sidebar-pick-map [escape] #'ghostherd-sidebar-pick-cancel)
(dotimes (i 9)
  (define-key ghostherd-sidebar-pick-map (vector (+ ?1 i))
              #'ghostherd-sidebar-pick-digit))

(define-minor-mode ghostherd-pick-mode
  "Pick a project filter inside the ghostherd overlay.
A minor mode for the same reason `ghostherd-filter-mode' is one:
Evil's state maps sit in `emulation-mode-map-alists' and would
swallow RET before a transient map saw it."
  :lighter nil
  :keymap ghostherd-sidebar-pick-map)

(define-obsolete-function-alias 'ghostherd-sidebar-toggle-project-filter
  'ghostherd-sidebar-project-filter "2026-09-18")

(defun ghostherd--sidebar-set-query (query)
  "Set the live-narrow QUERY and redraw, keeping point on the same row."
  (setq ghostherd--sidebar-query (or query ""))
  (when-let* ((buf (get-buffer "*ghostherd*")))
    (with-current-buffer buf
      (when (derived-mode-p 'ghostherd-sidebar-mode)
        (ghostherd--sidebar-build-entries)
        (ghostherd--sidebar-print t)
        (when (and (ghostherd--sidebar-posframe-showing-p)
                   (fboundp 'posframe-refresh))
          (posframe-refresh buf)))))
  (force-mode-line-update t))

(defun ghostherd--return-event-p (ev)
  "Return non-nil if EV is Enter in any GUI/tty encoding.

A catch-all `[t]' on the filter map is looked up *before*
`function-key-map' translates `<return>' to RET.  The default
binding then runs instead of the RET command, and a printable-only
self-insert silently drops it -- which is why Enter appeared to
do nothing, or jumped straight to visit."
  (or (memq ev '(return kp-enter newline S-return))
      (and (characterp ev) (memq ev '(?\r ?\n)))
      (memq (event-basic-type ev) '(return kp-enter newline))))

(defun ghostherd-sidebar-filter-self-insert ()
  "Append `last-command-event' to the live query, or apply on Enter."
  (interactive)
  (let ((ev last-command-event))
    (cond
     ((ghostherd--return-event-p ev)
      (ghostherd-sidebar-filter-confirm))
     ((and (characterp ev) (>= ev 32) (not (eq ev 127)))
      (ghostherd--sidebar-set-query
       (concat ghostherd--sidebar-query (char-to-string ev)))))))

(defun ghostherd-sidebar-filter-backspace ()
  "Drop the last character of the live query."
  (interactive)
  (when (> (length ghostherd--sidebar-query) 0)
    (ghostherd--sidebar-set-query
     (substring ghostherd--sidebar-query 0 -1))))

(defun ghostherd-sidebar-filter-clear ()
  "Empty the live query without leaving filter-mode."
  (interactive)
  (ghostherd--sidebar-set-query ""))

(defun ghostherd--set-filtering (on)
  "Turn live-narrow on or off, including `ghostherd-filter-mode'.
Refresh Evil's map alist so the filter intercept map actually
outranks the overlay's normal-state letters."
  (setq ghostherd--sidebar-filtering (and on t))
  (when-let* ((buf (get-buffer "*ghostherd*")))
    (with-current-buffer buf
      (ghostherd-filter-mode (if ghostherd--sidebar-filtering 1 -1))
      (when (fboundp 'evil-normalize-keymaps)
        (evil-normalize-keymaps))))
  (force-mode-line-update t))

(defun ghostherd-sidebar-filter-confirm ()
  "Keep the current query and leave live-narrow.
Returns `j'/`k' and RET to the overlay: move, then visit."
  (interactive)
  (ghostherd--set-filtering nil))

(defvar-keymap ghostherd-sidebar-filter-map
  :doc "Keymap while the session list is live-narrowing.
Printable keys append to the query; `n'/`p' still move.  RET applies
the query and returns to overlay keys so `j'/`k' move and RET visits.
`/` itself is how you enter this map, so it is not a query character."
  "RET"         #'ghostherd-sidebar-filter-confirm
  "C-m"         #'ghostherd-sidebar-filter-confirm
  "<return>"    #'ghostherd-sidebar-filter-confirm
  "n"           #'next-line
  "p"           #'previous-line
  "C-n"         #'next-line
  "C-p"         #'previous-line
  "<down>"      #'next-line
  "<up>"        #'previous-line
  "DEL"         #'ghostherd-sidebar-filter-backspace
  "<backspace>" #'ghostherd-sidebar-filter-backspace
  "<delete>"    #'ghostherd-sidebar-filter-backspace
  "C-h"         #'ghostherd-sidebar-filter-backspace
  "C-u"         #'ghostherd-sidebar-filter-clear
  "C-g"         #'ghostherd-sidebar-quit
  "<escape>"    #'ghostherd-sidebar-quit)

(define-key ghostherd-sidebar-filter-map [t]
            #'ghostherd-sidebar-filter-self-insert)
;; After `[t]': specific events the catch-all would otherwise swallow
;; before function-key-map translation.
(define-key ghostherd-sidebar-filter-map [return]
            #'ghostherd-sidebar-filter-confirm)
(define-key ghostherd-sidebar-filter-map [kp-enter]
            #'ghostherd-sidebar-filter-confirm)
(define-key ghostherd-sidebar-filter-map [S-return]
            #'ghostherd-sidebar-filter-confirm)
(define-key ghostherd-sidebar-filter-map (kbd "C-j")
            #'ghostherd-sidebar-filter-confirm)

(defun ghostherd--bind-filter-query-keys ()
  "Bind printable keys on the filter map to self-insert.

Evil looks up a specific binding in the overlay's normal-state
map (`a' answer, `e' explain, …) before a `[t]' default on a
lower map is considered.  Explicit letters on the intercept map
make `/` then `a` search rather than run the overlay command.
`n'/`p' stay as motion so a match can be highlighted while typing."
  (let ((c 32))
    (while (< c 127)
      (unless (memq c '(?n ?p 127))
        (define-key ghostherd-sidebar-filter-map (vector c)
                    #'ghostherd-sidebar-filter-self-insert))
      (setq c (1+ c)))))

(ghostherd--bind-filter-query-keys)

(define-minor-mode ghostherd-filter-mode
  "Live-narrow the ghostherd overlay.

A minor mode rather than `set-transient-map': evil's state maps
sit in `emulation-mode-map-alists' and can swallow RET before a
terminal-local transient map sees it.  An intercept keymap on this
mode outranks evil, so Enter applies the filter."
  :lighter nil
  :keymap ghostherd-sidebar-filter-map)

(defun ghostherd-sidebar-filter ()
  "Start live-narrowing the session list.

Printable keys append to the query; `n'/`p' still move.  RET keeps
the query and returns to overlay keys (`j'/`k' move, RET visits).
Esc clears a non-empty query, and dismisses the list when the
query is already empty.  Matches name, kind, state, project,
notes and title as a flex subsequence; whitespace is AND."
  (interactive)
  (ghostherd--set-filtering t))

(defun ghostherd--posframe-available-p ()
  "Return non-nil when posframe can actually display a child frame.

`require' succeeding is not enough: batch, a tty, and a frame
without its own minibuffer all fail `posframe-workable-p', and
those are exactly the cases that must fall back."
  (and (require 'posframe nil t)
       (fboundp 'posframe-workable-p)
       (posframe-workable-p)))

(defun ghostherd--use-posframe-p ()
  "Return non-nil when the session list should open as a posframe."
  (and ghostherd-sidebar-use-posframe
       (ghostherd--posframe-available-p)))

(defun ghostherd--sidebar-posframe-frame ()
  "Return the session list's posframe, or nil."
  (when-let* ((buf (get-buffer "*ghostherd*")))
    (and (boundp 'posframe--frame)
         (buffer-local-value 'posframe--frame buf))))

(defun ghostherd--sidebar-posframe-showing-p ()
  "Return non-nil when the session list is a visible posframe."
  (when-let* ((frame (ghostherd--sidebar-posframe-frame)))
    (and (frame-live-p frame)
         (frame-visible-p frame))))

(defun ghostherd--sidebar-hide-posframe ()
  "Hide the overlay and restore input focus to its parent frame."
  (let ((parent ghostherd--sidebar-posframe-parent)
        (buf (get-buffer "*ghostherd*")))
    (setq ghostherd--sidebar-posframe-parent nil
          ghostherd--sidebar-posframe-fitted-width nil
          ghostherd--sidebar-query ""
          ghostherd--sidebar-help-visible nil
          ghostherd--sidebar-preview-id nil)
    (ghostherd--set-filtering nil)
    (ghostherd--set-picking nil)
    (when (timerp ghostherd--sidebar-preview-timer)
      (cancel-timer ghostherd--sidebar-preview-timer)
      (setq ghostherd--sidebar-preview-timer nil))
    (when (and buf (fboundp 'posframe-hide))
      (posframe-hide buf))
    (when (and parent (frame-live-p parent))
      (select-frame-set-input-focus parent))))

(defun ghostherd--sidebar-leave-overlay ()
  "Dismiss the posframe overlay so a subsequent display uses a real window.

The overlay's window is dedicated and its frame is unsplittable.
`pop-to-buffer' from there has nowhere to put an agent or a log
-- or would put it *in* the child frame.  `?' no longer goes
through this: the legend draws in the overlay's own preview pane.
The side-window display is left alone: it is a dashboard, and
visit has always kept it open."
  (when (ghostherd--sidebar-posframe-showing-p)
    (ghostherd--sidebar-hide-posframe)))

(defun ghostherd-sidebar-quit ()
  "Dismiss the session list, or the live query first.

A non-empty `/` query is cleared and filter-mode ends, so the next
key is a command again (`z', `k').  An empty query dismisses: the
posframe overlay hides, or the side window quits."
  (interactive)
  (cond
   (ghostherd--sidebar-picking
    (ghostherd-sidebar-pick-cancel))
   ((and ghostherd--sidebar-query
         (not (string-empty-p ghostherd--sidebar-query)))
    (ghostherd--sidebar-set-query "")
    (ghostherd--set-filtering nil))
   (ghostherd--sidebar-help-visible
    (setq ghostherd--sidebar-help-visible nil)
    (ghostherd--sidebar-draw-preview)
    (when (ghostherd--sidebar-posframe-showing-p)
      (ghostherd--sidebar-show-posframe (get-buffer "*ghostherd*")))
    (force-mode-line-update t))
   (t
    (setq ghostherd--sidebar-query "")
    (ghostherd--set-filtering nil)
    (if (ghostherd--sidebar-posframe-showing-p)
        (ghostherd--sidebar-hide-posframe)
      (quit-window)))))

(defun ghostherd--sidebar-close-side-windows (buf)
  "Delete any side window showing BUF, so the overlay does not share a frame."
  (dolist (win (get-buffer-window-list buf nil t))
    (when (and (window-parameter win 'window-side)
               (not (frame-parent (window-frame win))))
      (ignore-errors (delete-window win)))))

(defun ghostherd--sidebar-posframe-border-color ()
  "Border colour for the overlay, taken from the current theme."
  (or (face-foreground 'vertical-border nil t)
      (face-foreground 'mode-line-inactive nil t)
      "gray50"))

(defun ghostherd--sidebar-posframe-parent-frame ()
  "Frame the overlay should be sized against and return focus to."
  (or (and (frame-live-p ghostherd--sidebar-posframe-parent)
           ghostherd--sidebar-posframe-parent)
      (frame-parent (selected-frame))
      (selected-frame)))

(defun ghostherd--sidebar-posframe-char-width (&optional frame)
  "Character width of the overlay on FRAME.

Scales with `ghostherd-sidebar-posframe-width-ratio' so a full-screen
Emacs does not get a 72-column stamp in the middle.  The configured
width is the floor; the parent minus a margin is the ceiling."
  (let* ((frame (or frame (ghostherd--sidebar-posframe-parent-frame)))
         (avail (max 1 (frame-width frame)))
         (margin (min 12 (max 4 (/ avail 16))))
         (ceil (max 48 (- avail margin)))
         (floor (min ghostherd-sidebar-posframe-width ceil)))
    (if (null ghostherd-sidebar-posframe-width-ratio)
        floor
      (let ((wanted (round (* ghostherd-sidebar-posframe-width-ratio avail))))
        (max floor (min wanted ceil))))))

(defun ghostherd--sidebar-posframe-max-height (&optional frame)
  "Max overlay height in lines, as a fraction of FRAME."
  (let* ((frame (or frame (ghostherd--sidebar-posframe-parent-frame)))
         (avail (max 8 (frame-height frame))))
    (max 8 (min (- avail 4) (round (* 0.72 avail))))))

(defun ghostherd--sidebar-refit-posframe (width)
  "Rebuild the overlay's columns for WIDTH and show it again."
  (when-let* ((buf (get-buffer "*ghostherd*")))
    (let ((ghostherd--sidebar-posframe-refitting t)
          (ghostherd--sidebar-target-width width))
      (setq ghostherd--sidebar-posframe-fitted-width width)
      (with-current-buffer buf
        (when (derived-mode-p 'ghostherd-sidebar-mode)
          (ghostherd--sidebar-build-entries)
          (ghostherd--sidebar-print t)))
      (ghostherd--sidebar-show-posframe buf))))

(defun ghostherd--sidebar-posframe-on-parent-resize (frame)
  "Refit the overlay when its parent FRAME changes size.
Ignores the child frame itself, which also resizes as we show it."
  (when (and (not ghostherd--sidebar-posframe-refitting)
             ghostherd--sidebar-posframe-parent
             (eq frame ghostherd--sidebar-posframe-parent)
             (ghostherd--sidebar-posframe-showing-p))
    (let ((width (ghostherd--sidebar-posframe-char-width frame)))
      (unless (eql width ghostherd--sidebar-posframe-fitted-width)
        (ghostherd--sidebar-refit-posframe width)))))

(defun ghostherd--sidebar-show-posframe (buf)
  "Show BUF as a centred, focusable posframe overlay."
  (ghostherd--sidebar-close-side-windows buf)
  (let* ((parent (ghostherd--sidebar-posframe-parent-frame))
         (width (or ghostherd--sidebar-target-width
                    (ghostherd--sidebar-posframe-char-width parent)))
         (rows (with-current-buffer buf
                 (length tabulated-list-entries)))
         (preview (cond
                   (ghostherd--sidebar-help-visible
                    (max 8 (length (split-string
                                    (ghostherd--sidebar-help-text) "\n"))))
                   (ghostherd-sidebar-show-preview
                    (+ 2 ghostherd-sidebar-preview-lines))
                   (t 0))))
    (setq ghostherd--sidebar-posframe-parent parent
          ghostherd--sidebar-posframe-fitted-width width)
    (add-hook 'window-size-change-functions
              #'ghostherd--sidebar-posframe-on-parent-resize)
    ;; `posframe-show' takes the selected frame as parent.  Calling it
    ;; from inside the overlay would nest a child frame in itself.
    (with-selected-frame parent
      (posframe-show
       buf
       :poshandler #'posframe-poshandler-frame-center
       :position (point)
       :width width
       :min-width (min 48 width)
       :min-height (max 4 (+ 1 rows preview))
       :max-height (ghostherd--sidebar-posframe-max-height parent)
       :left-fringe 12
       :right-fringe 12
       :border-width 2
       :border-color (ghostherd--sidebar-posframe-border-color)
       :respect-header-line t
       :respect-mode-line t
       :lines-truncate t
       :cursor 'box
       :accept-focus t
       :window-point (with-current-buffer buf (point))))
    (when-let* ((frame (ghostherd--sidebar-posframe-frame)))
      (select-frame-set-input-focus frame)
      (select-window (frame-root-window frame)))))

(defun ghostherd--sidebar-show-side-window (buf)
  "Show BUF in a side window -- the display used when posframe cannot."
  (when (fboundp 'posframe-hide)
    (posframe-hide buf))
  (setq ghostherd--sidebar-posframe-parent nil)
  (pop-to-buffer
   buf
   `((display-buffer-in-side-window)
     (side . ,ghostherd-sidebar-side)
     (slot . 0)
     (window-width . ,ghostherd-sidebar-width)
     (preserve-size . (t . nil)))))

(defun ghostherd--sidebar-prepare-buffer ()
  "Return the session-list buffer, filled from the current registry."
  (let ((buf (get-buffer-create "*ghostherd*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'ghostherd-sidebar-mode)
        (ghostherd-sidebar-mode))
      (ghostherd--sidebar-entries)
      (ghostherd--sidebar-print t))
    buf))

;;;###autoload
(defun ghostherd-sidebar ()
  "Open the ghostherd session list.

Uses a posframe overlay when `ghostherd-sidebar-use-posframe' is
non-nil and posframe can display a child frame.  Otherwise opens
the original side window.  The buffer and keys are the same
either way; Esc or `q' dismisses."
  (interactive)
  (ghostherd--maybe-restore)
  (ghostherd--ensure-sessions)
  (setq ghostherd--sidebar-query ""
        ghostherd--sidebar-help-visible nil
        ghostherd--sidebar-preview-id nil)
  (ghostherd--set-filtering nil)
  (let* ((overlay (ghostherd--use-posframe-p))
         (ghostherd--sidebar-target-width
          (and overlay (ghostherd--sidebar-posframe-char-width)))
         (buf (ghostherd--sidebar-prepare-buffer)))
    (if overlay
        (ghostherd--sidebar-show-posframe buf)
      (ghostherd--sidebar-show-side-window buf))
    (when (fboundp 'ghostherd-usage-refresh)
      (ghostherd-usage-refresh)
      (ghostherd-usage--ensure-timer))
    buf))


;;; Global minor mode / modeline

(defvar ghostherd--mode-line-kept nil
  "(COUNTS . SEGMENT): the mode-line segment last made, and the counts it
was made of.")

(defun ghostherd--mode-line-segment ()
  "Mode-line segment summarizing herd attention.

Reads the registry and nothing else.  It used to sweep it first, which
was free while a host check was `buffer-live-p' and is not once a host
check can run a subprocess -- redisplay is no place for I/O, and no
place to discover that the I/O failed either.  The poll timer already
sweeps every `ghostherd-poll-interval'.

Asked at every redisplay of every mode line -- a terminal streaming an
agent's work redraws its own many times a second -- so it counts in one
pass and makes the segment again only when a count changed."
  (let ((blocked 0) (working 0) (waiting 0) (done 0) (total 0))
    (maphash (lambda (_id s)
               (cl-incf total)
               (pcase (ghostherd-session-state s)
                 ('blocked (cl-incf blocked))
                 ('working (cl-incf working))
                 ('waiting (cl-incf waiting))
                 ('done (cl-incf done))))
             ghostherd--sessions)
    (let ((counts (list total blocked working waiting done)))
      (unless (equal counts (car ghostherd--mode-line-kept))
        (setq ghostherd--mode-line-kept
              (cons counts (ghostherd--mode-line-make total blocked working waiting done))))
      ;; A copy, as it always was a fresh string: a mode line may add to it.
      (copy-sequence (cdr ghostherd--mode-line-kept)))))

(defun ghostherd--mode-line-make (total blocked working waiting done)
  "The mode-line segment for these counts of the herd's agents, or nil."
  (when (> total 0)
    (propertize
     (format " 🐄%d%s%s%s%s"
             total
             (if (> blocked 0) (format " ⚠%d" blocked) "")
             (if (> working 0) (format " ⟳%d" working) "")
             (if (> waiting 0) (format " ◷%d" waiting) "")
             (if (> done 0) (format " ✓%d" done) ""))
     'face (cond ((> blocked 0) 'error)
                 ((> done 0) 'success)
                 ((> working 0) 'warning)
                 (t 'shadow))
     'help-echo "ghostherd sessions (click for sidebar)"
     'mouse-face 'mode-line-highlight
     'local-map (define-keymap
                  "<mode-line> <mouse-1>" #'ghostherd-sidebar))))

;;;###autoload
(defun ghostherd--herd-page-for-phone ()
  "Load the herd page now, when your phone reaches it (`xwapp-remote-hosts'):
its address without the secret, a push's room and the bot's links all
want it loaded, and after a restart nothing here has opened it yet."
  (when (bound-and-true-p xwapp-remote-hosts)
    (with-demoted-errors "ghostherd: herd page: %S"
      (require 'ghostherd-memory-page))))

(define-minor-mode ghostherd-mode
  "Global minor mode for ghostherd modeline indicator and hooks."
  :global t
  :group 'ghostherd
  (if ghostherd-mode
      (progn
        (ghostherd--install-hooks)
        (ghostherd--register-eval-cmds)
        ;; The log first, so what the previous Emacs saw is above what this
        ;; one is about to do: restore writes its own lines.
        (unless ghostherd--log-loaded
          (ignore-errors (ghostherd-log-load)))
        ;; Enabling the mode is the restore hook: a herd left running by
        ;; a previous Emacs is picked up here.  Deferred to a zero-delay
        ;; timer so the whole file and its state are loaded first (a
        ;; synchronous restore here could race `ghostherd--worktrees' void),
        ;; and retried past a transient; idempotent.
        (ghostherd--restore-deferred)
        ;; Demoted, not ignored: a sidecar on an old token makes start
        ;; refuse, and without a word agents would just spawn with no
        ;; GHOSTHERD_RPC.
        (with-demoted-errors "ghostherd: memory sidecar: %S"
          (ghostherd-memory-start))
        (ghostherd--herd-page-for-phone)
        (ghostherd-memory-auto-import-start)
        (ignore-errors (ghostherd--herd-write-protocol))
        (ghostherd--ensure-poll-timer)
        (ghostherd--ensure-herd-tick-timer)
        (unless (member '(:eval (ghostherd--mode-line-segment)) mode-line-misc-info)
          (setq mode-line-misc-info
                (append mode-line-misc-info
                        '((:eval (ghostherd--mode-line-segment)))))))
    (ghostherd--remove-hooks)
    (when (timerp ghostherd--poll-timer)
      (cancel-timer ghostherd--poll-timer)
      (setq ghostherd--poll-timer nil))
    (ghostherd--stop-herd-tick-timer)
    (ghostherd-memory-auto-import-stop)
    (setq mode-line-misc-info
          (cl-remove '(:eval (ghostherd--mode-line-segment))
                     mode-line-misc-info
                     :test #'equal))))

(when (bound-and-true-p ghostherd-mode)
  (ghostherd--ensure-herd-tick-timer))


;;; Transient-ish quick menu (no transient dependency)

(defconst ghostherd-menu-choices
  '(("n" "new agent" ghostherd-new)
    ("2" "new pair (split)" ghostherd-new-pair)
    ("s" "switch" ghostherd-switch)
    ("b" "sidebar" ghostherd-sidebar)
    ("." "next blocked" ghostherd-next-blocked)
    ("m" "message agent" ghostherd-message-interactive)
    ("i" "prompt agent" ghostherd-prompt-interactive)
    ("k" "kill" ghostherd-kill)
    ("r" "rename" ghostherd-rename)
    ("M" "mark state" ghostherd-mark-state)
    ("l" "herd log" ghostherd-log)
    ("h" "scrollback" ghostherd-scrollback)
    ("/" "search memory" ghostherd-memory-search)
    ("v" "view memory" ghostherd-memory-view)
    ("I" "import memory" ghostherd-memory-import)
    ("g" "poll now" ghostherd-poll-all))
  "Key / label / command rows for `ghostherd-menu'.")

;;;###autoload
(defun ghostherd-menu ()
  "Simple command dispatcher for ghostherd."
  (interactive)
  (let* ((choices ghostherd-menu-choices)
         (key (char-to-string
               (read-char
                (concat "ghostherd: "
                        (mapconcat (lambda (c)
                                     (format "%s=%s" (nth 0 c) (nth 1 c)))
                                   choices "  ")))))
         (cmd (nth 2 (assoc key choices))))
    (if cmd
        (call-interactively cmd)
      (user-error "Unknown choice %s" key))))

(defun ghostherd--maybe-setup-evil-sidebar ()
  "Bind overlay keys in evil normal state so they are not shadowed.

Must call `evil-define-key*', the function.  Filter-mode uses an
intercept map so RET applies the query instead of visiting.  `j'/`k'
stay as motion.  `?' is evil-search unless it is on this map; the
full overlay alphabet has to live here, not only `/'.

`x' is kill and `gr' is refresh: `k' and `g' are evil motion/prefix."
  (when (fboundp 'evil-make-intercept-map)
    (evil-make-intercept-map ghostherd-sidebar-filter-map 'normal)
    (evil-make-intercept-map ghostherd-sidebar-pick-map 'normal))
  (when (fboundp 'evil-define-key*)
    (evil-define-key* 'normal ghostherd-sidebar-pick-map
      (kbd "RET") #'ghostherd-sidebar-pick-confirm
      (kbd "<return>") #'ghostherd-sidebar-pick-confirm
      (kbd "C-m") #'ghostherd-sidebar-pick-confirm
      (kbd "j") #'ghostherd-sidebar-pick-next
      (kbd "k") #'ghostherd-sidebar-pick-previous
      (kbd "n") #'ghostherd-sidebar-pick-next
      (kbd "p") #'ghostherd-sidebar-pick-previous
      (kbd "s") #'ghostherd-sidebar-pick-cancel
      (kbd "q") #'ghostherd-sidebar-pick-cancel
      (kbd "<escape>") #'ghostherd-sidebar-pick-cancel)
    (dotimes (i 9)
      (evil-define-key* 'normal ghostherd-sidebar-pick-map
        (kbd (number-to-string (1+ i))) #'ghostherd-sidebar-pick-digit))
    (evil-define-key* 'normal ghostherd-sidebar-filter-map
      (kbd "RET") #'ghostherd-sidebar-filter-confirm
      (kbd "<return>") #'ghostherd-sidebar-filter-confirm
      (kbd "C-m") #'ghostherd-sidebar-filter-confirm
      (kbd "<escape>") #'ghostherd-sidebar-quit)
    (evil-define-key* 'normal ghostherd-sidebar-mode-map
      (kbd "?") #'ghostherd-sidebar-help
      (kbd "e") #'ghostherd-explain
      (kbd "R") #'ghostherd-respawn
      (kbd "c") #'ghostherd-sidebar-notes
      (kbd "z") #'ghostherd-sidebar-interrupt
      (kbd "Z") #'ghostherd-sidebar-abort
      (kbd "a") #'ghostherd-sidebar-answer
      (kbd "RET") #'ghostherd-sidebar-visit
      (kbd "<return>") #'ghostherd-sidebar-visit
      (kbd "o") #'ghostherd-sidebar-visit
      (kbd "N") #'ghostherd-new
      (kbd "P") #'ghostherd-new-pair
      (kbd "x") #'ghostherd-sidebar-kill
      (kbd "r") #'ghostherd-sidebar-rename
      (kbd "m") #'ghostherd-sidebar-message
      (kbd "i") #'ghostherd-sidebar-prompt
      (kbd "s") #'ghostherd-sidebar-project-filter
      (kbd "/") #'ghostherd-sidebar-filter
      (kbd "v") #'ghostherd-sidebar-toggle-preview
      (kbd "gr") #'ghostherd-sidebar-refresh
      (kbd "U") #'ghostherd-usage-refresh
      (kbd ".") #'ghostherd-next-blocked
      (kbd "H") #'ghostherd-scrollback
      (kbd "L") #'ghostherd-log
      (kbd "M") #'ghostherd-sidebar-mark-state
      (kbd "q") #'ghostherd-sidebar-quit
      (kbd "<escape>") #'ghostherd-sidebar-quit)))

(with-eval-after-load 'evil
  (ghostherd--maybe-setup-evil-sidebar))

(provide 'ghostherd)
;;; ghostherd.el ends here

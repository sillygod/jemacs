;;; ghostherd-sidebar.el --- Display helpers and the sidebar -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of ghostherd (see ghostherd.el).  Display helpers -- state
;; glyphs, the session completion table, scrollback, the terminal
;; minor mode -- and the sidebar: its columns, preview, filter and
;; project picker, as an overlay or a side window.

;;; Code:

(require 'ghostherd-core)
(require 'ghostherd-worktree)
(require 'ghostherd-log)
(require 'ghostherd-screen)
(require 'ghostherd-spawn)
(require 'ghostherd-comm)

(declare-function magit-status-setup-buffer "magit-status" (&optional directory))
(declare-function posframe-workable-p "posframe")
(declare-function posframe-show "posframe")
(declare-function posframe-hide "posframe")
(declare-function posframe-refresh "posframe")
(declare-function posframe-poshandler-frame-center "posframe")
(defvar ghostel--input-mode)
(declare-function ghostel-copy-mode "ghostel")
(declare-function ghostel-readonly-exit "ghostel")
(declare-function evil-define-key* "evil")
(declare-function ghostherd-relay "ghostherd-primary" (session kind &optional reason))

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

(provide 'ghostherd-sidebar)

;; Local Variables:
;; generated-autoload-load-name: "ghostherd"
;; End:

;;; ghostherd-sidebar.el ends here

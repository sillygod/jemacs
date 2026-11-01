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
(require 'ghostherd-sidebar)
(require 'ghostherd-mail)
(require 'ghostherd-lead)
(require 'ghostherd-hooks)
(require 'ghostherd-primary)

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

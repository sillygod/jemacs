;;; ghostherd.el --- Manage AI agent CLIs in ghostel terminals -*- lexical-binding: t; -*-

;; Author: Jing
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ghostel "0.32"))
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
;; - Sidebar + consult switcher with working/blocked/idle/dead state
;; - Inter-agent messaging (prompt, send, read, wait, message)
;; - Notifications on blocked / process exit (via alert when available)
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
(require 'project)
(require 'tabulated-list)
;; The host slot, the session struct and the ghostel implementor.  Loaded
;; first because everything below is written against the slot rather than
;; against ghostel directly.
(require 'ghostherd-backend)
(require 'ghostherd-tmux)

(declare-function alert "alert" (message &rest kwargs))

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


;;; Customization

(defcustom ghostherd-agent-specs
  '((claude
     :command "claude"
     :args nil
     :description "Claude Code"
     :process-names ("claude")
     :continue-args ("--continue")
     :screen-rules
     ((blocked . ("Do you want to proceed"
                  "Do you want to make this edit"
                  "Allow this action"
                  "Bash command"
                  "permission"
                  "Yes, and don't ask again"
                  "❯ 1\\. Yes"))
      (working . ("esc to interrupt"
                  "Baking…"
                  "Baking..."
                  "Thinking"
                  "⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"))
      (idle . ("────────────────────────────────"
               "^❯ "
               "^> "))))
    (grok
     :command "grok"
     :args nil
     :description "Grok Build TUI"
     :process-names ("grok")
     :continue-args ("--continue")
     :screen-rules
     ((blocked . ("Do you want to proceed"
                  "Allow this"
                  "Approve"
                  "permission"
                  "always-approve"
                  "\\[y/N\\]"
                  "\\(y/n\\)"))
      (working . ("Working"
                  "Thinking"
                  "Running"
                  "esc to interrupt"
                  "ctrl\\+c to interrupt"
                  "⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"))
      (idle . ("^› "
               "^❯ "
               "^> "
               "Grok Build"))))
    (agy
     :command "agy"
     :args nil
     :description "agy CLI"
     :process-names ("agy")
     :continue-args ("--continue")
     :screen-rules
     ((blocked . ("Do you want to proceed"
                  "Allow this"
                  "permission"
                  "Approve"
                  "\\[y/N\\]"
                  "\\(y/n\\)"))
      (working . ("Working"
                  "Thinking"
                  "Running"
                  "esc to interrupt"
                  "⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"))
      (idle . ("^❯ "
               "^> "
               "^› "))))
    (shell
     :command nil
     :args nil
     :description "Plain ghostel shell"
     :process-names nil
     :screen-rules nil))
  "Registry of known agent kinds.

Each entry is (KIND . PLIST) with keys:
:command        executable name or path (nil = shell only)
:args           default argument list (strings)
:continue-args  flag that resumes the CLI's previous conversation, used by
                `ghostherd-respawn'.  nil when the CLI has no such flag.
:description    human label
:process-names  process names used for detection (future)
:screen-rules   alist of (STATE . REGEXP-LIST) for tail matching

Common optional CLI flags (not enabled by default — set via :args
or the ARGUMENTS prompt in `ghostherd-new'):

claude:
  --permission-mode plan|acceptEdits|bypassPermissions|default|dontAsk
  --dangerously-skip-permissions
  --model sonnet|opus|<full-name>
  --continue / --resume
  --system-prompt PROMPT
  -p / --print  (headless; usually not for interactive herd)

grok:
  --permission-mode default|acceptEdits|auto|dontAsk|bypassPermissions|plan
  --always-approve
  -m / --model MODEL
  --continue / --resume
  --cwd DIR
  --system-prompt-override PROMPT
  --worktree [NAME]

agy:
  --mode accept-edits|plan
  --dangerously-skip-permissions
  --model MODEL
  --continue
  --prompt-interactive / -i PROMPT
  --effort low|medium|high
  --sandbox"
  :type '(alist :key-type symbol :value-type plist)
  :group 'ghostherd)

(defcustom ghostherd-default-kind 'claude
  "Default agent kind for `ghostherd-new'."
  :type 'symbol
  :group 'ghostherd)

(defcustom ghostherd-poll-interval 1.5
  "Seconds between automatic state polls for live sessions."
  :type 'number
  :group 'ghostherd)

(defcustom ghostherd-use-osc-progress t
  "Treat ConEmu OSC 9;4 progress reports as evidence that an agent is working.
Screen scraping cannot tell a silent agent from a finished one; a CLI
that emits progress tells us directly.  Takes effect when
`ghostherd-mode' is next enabled."
  :type 'boolean
  :group 'ghostherd)

(defcustom ghostherd-progress-ttl 5
  "Seconds an OSC 9;4 progress report counts as evidence of `working'.
Reports stop arriving the moment an agent blocks on a prompt, so this
only needs to outlast the gap between two updates of a live task."
  :type 'number
  :group 'ghostherd)

(defvar ghostherd--saved-progress-function nil
  "Value of `ghostel-progress-function' displaced by `ghostherd--on-progress'.")

(defcustom ghostherd-notify-on-blocked t
  "Notify when a session transitions into `blocked'."
  :type 'boolean
  :group 'ghostherd)

(defcustom ghostherd-notify-on-exit t
  "Notify when an agent process exits (via `ghostel-exit-functions')."
  :type 'boolean
  :group 'ghostherd)

(defcustom ghostherd-notify-on-done t
  "Notify when a session transitions into `done' (idle after work, unseen)."
  :type 'boolean
  :group 'ghostherd)

(defcustom ghostherd-sidebar-side 'left
  "Side window side for `ghostherd-sidebar'."
  :type '(choice (const left) (const right))
  :group 'ghostherd)

(defcustom ghostherd-sidebar-width 36
  "Width of the ghostherd sidebar window."
  :type 'integer
  :group 'ghostherd)

(defcustom ghostherd-sidebar-show-title nil
  "Offer the agent's OSC 2 terminal title as a sidebar column.

The column still has to earn its place: columns are fitted to the
sidebar's width (see `ghostherd--sidebar-column-specs'), and Title ranks
above Project, so in a narrow sidebar enabling this trades the project
path for the title.  Nothing overflows either way."
  :type 'boolean
  :group 'ghostherd)

(defcustom ghostherd-detached-glyph "·"
  "Marker appended to the sidebar's state glyph when nothing is attached.

Rides in the spare character of the two-wide glyph column rather than
taking a column of its own: at the default sidebar width there is no
room for another column, so a `View' one would be fitted away exactly
where it is needed.

Marks the *detached* rows rather than the attached ones, which keeps it
silent on the ghostel backend -- there a registered session always has
its buffer, so nothing is ever marked."
  :type 'string
  :group 'ghostherd)

(defcustom ghostherd-message-template
  "[ghostherd message from %s → %s]\n%s\n"
  "Template for inter-agent messages.
Filled with FROM name, TO name, and BODY."
  :type 'string
  :group 'ghostherd)

(defcustom ghostherd-submit-on-message t
  "When non-nil, inter-agent messages are submitted with RET after paste."
  :type 'boolean
  :group 'ghostherd)


;;; Session model

;; `ghostherd-session', the registry and `ghostherd-get' live in
;; ghostherd-backend.el: the implementors need them, and they are the one
;; thing both layers share.

(defvar ghostherd--counter 0
  "Monotonic counter for generated session names.")

(defvar ghostherd--poll-timer nil
  "Idle timer that polls session states.")

(defvar ghostherd-state-change-hook nil
  "Hook run after a session state changes.
Each function is called with (SESSION OLD-STATE NEW-STATE).")

(defvar ghostherd-session-created-hook nil
  "Hook run after a session is registered.  Arg: SESSION.")

(defvar ghostherd-session-removed-hook nil
  "Hook run after a session is removed.  Arg: SESSION.")


;;; Helpers

(defun ghostherd--agent-kinds ()
  "Return the list of known agent kind symbols."
  (mapcar #'car ghostherd-agent-specs))

(defvar ghostherd-project-kind nil
  "Default agent kind for a project, set from its =.dir-locals.el=.

  ((nil . ((ghostherd-project-kind . claude))))")
(put 'ghostherd-project-kind 'safe-local-variable #'symbolp)

(defvar ghostherd-project-args nil
  "Default extra CLI arguments for a project, from its =.dir-locals.el=.

  ((nil . ((ghostherd-project-args . (\"--permission-mode\" \"acceptEdits\")))))")
(put 'ghostherd-project-args 'safe-local-variable
     (lambda (value) (and (listp value) (seq-every-p #'stringp value))))

(defun ghostherd-project-defaults (&optional directory)
  "Return (KIND . ARGS) from DIRECTORY's directory-local variables.

Read from the directory rather than from the current buffer on purpose:
`ghostherd-new' is often invoked from the sidebar or another non-file
buffer, which never picked up the project's dir-locals at all.

A malformed .dir-locals.el costs you the defaults, not the ability to
start an agent: `hack-dir-local-variables' warns and yields nothing
rather than signalling.  The `ignore-errors' is for the filesystem
around it, not for that case."
  (let ((directory (file-name-as-directory
                    (expand-file-name (or directory default-directory)))))
    (or (ignore-errors
          (with-temp-buffer
            (setq default-directory directory)
            ;; The lookup keys off `buffer-file-name', not
            ;; `default-directory'; the latter just keeps this temp buffer
            ;; coherent.  `hack-dir-local-variables' collects into
            ;; `file-local-variables-alist' without applying anything.
            (setq-local buffer-file-name
                        (expand-file-name ".ghostherd" directory))
            (hack-dir-local-variables)
            (cons (alist-get 'ghostherd-project-kind file-local-variables-alist)
                  (alist-get 'ghostherd-project-args file-local-variables-alist))))
        (cons nil nil))))

(defun ghostherd--spec (kind)
  "Return the plist for KIND from `ghostherd-agent-specs'."
  (or (alist-get kind ghostherd-agent-specs)
      (user-error "Unknown agent kind: %s" kind)))

(defun ghostherd--project-root (&optional dir)
  "Return project root for DIR, or nil."
  (when-let* ((proj (project-current nil dir)))
    (project-root proj)))

(defun ghostherd--abbreviate (path)
  "Abbreviate PATH for display."
  (if path (abbreviate-file-name path) "~"))

(defun ghostherd--session-title (session)
  "Return SESSION's terminal title, or nil when it says nothing new.
Which OSC 2 stream that comes from is the backend's business: a ghostel
session has it as a buffer-local, a detached tmux session has it as
`#{pane_title}'."
  (ghostherd--host-title session))

(defun ghostherd--unique-name (base)
  "Return a unique session name derived from BASE."
  (let ((name base)
        (n 1))
    (while (ghostherd-get name)
      (setq n (1+ n)
            name (format "%s-%d" base n)))
    name))

(defun ghostherd--session-live-p (session)
  "Return non-nil if SESSION's host is still there.

This used to mean \"the buffer exists\", which was the same statement
while every agent was an Emacs child.  It is not the same on a backend
where the buffer is a client: a tmux session nobody is attached to has
no buffer at all and is very much alive."
  (ghostherd--host-live-p session))

(defun ghostherd-sessions (&optional project)
  "Return all sessions, optionally filtered to PROJECT root."
  (let ((all (hash-table-values ghostherd--sessions)))
    (if (not project)
        (cl-sort all #'string< :key #'ghostherd-session-name)
      (let ((root (expand-file-name project)))
        (cl-sort
         (cl-remove-if-not
          (lambda (s)
            (when-let* ((p (ghostherd-session-project s)))
              (file-equal-p p root)))
          all)
         #'string< :key #'ghostherd-session-name)))))

(defun ghostherd--ensure-sessions ()
  "Drop sessions whose host is gone.

An agent that exited is not gone: the ghostel backend keeps its buffer
(`ghostel-kill-buffer-on-exit' nil) and the tmux backend keeps its pane
(`remain-on-exit on') precisely so the row can sit there saying `dead'
with its last screen intact.  What this prunes is the session whose
host no longer exists to ask."
  (maphash
   (lambda (id session)
     ;; One session whose host cannot be asked must not take the sweep
     ;; down with it, and must not be read as gone either: pruning on a
     ;; failed question would delete the herd rather than report it.
     ;; Keep it, and let the poll surface the error.
     (unless (condition-case nil
                 (ghostherd--session-live-p session)
               (error t))
       (setf (ghostherd-session-state session) 'dead
             (ghostherd-session-state-reason session) "host gone")
       (remhash id ghostherd--sessions)
       (run-hook-with-args 'ghostherd-session-removed-hook session)))
   ghostherd--sessions))


;;; Notifications

(defun ghostherd--notify (title body)
  "Show TITLE/BODY via alert when available, else `message'."
  (cond
   ((and (fboundp 'alert) (require 'alert nil t))
    (alert body :title title :category 'ghostherd))
   (t
    (message "%s: %s" title body))))

(defun ghostherd--maybe-notify-state (session old new)
  "Notify about SESSION state transition OLD → NEW when configured."
  (let ((name (ghostherd-session-name session))
        (kind (symbol-name (ghostherd-session-kind session))))
    (pcase new
      ('blocked
       (when (and ghostherd-notify-on-blocked (not (eq old 'blocked)))
         (ghostherd--notify
          (format "ghostherd · %s blocked" name)
          (format "%s needs attention%s"
                  kind
                  (if-let* ((r (ghostherd-session-state-reason session)))
                      (format " (%s)" r)
                    "")))))
      ('done
       (when (and ghostherd-notify-on-done (not (eq old 'done)))
         (ghostherd--notify
          (format "ghostherd · %s done" name)
          (format "%s finished and is ready for review" kind))))
      ('dead
       (when (and ghostherd-notify-on-exit (not (eq old 'dead)))
         (ghostherd--notify
          (format "ghostherd · %s exited" name)
          (or (ghostherd-session-state-reason session)
              "process exited")))))))


;;; State detection

(defun ghostherd--match-rules (text rules)
  "Return (STATE . REASON) for first matching rule in RULES against TEXT.
RULES is an alist of (STATE . REGEXP-LIST).  Prefer blocked over
working over idle (strict blocked detection, herdr-style)."
  (cl-labels ((try (state)
                (when-let* ((patterns (alist-get state rules)))
                  (cl-loop for pat in patterns
                           when (string-match-p pat text)
                           return (cons state pat)))))
    (or (try 'blocked)
        (try 'working)
        (try 'idle))))

(defun ghostherd--progress-fresh-p (session)
  "Return non-nil if SESSION reported OSC progress recently enough to trust.
`remove' and `pause' are excluded: they say the agent stopped reporting,
which is exactly when the screen rules should take back over."
  (and ghostherd-use-osc-progress
       (memq (ghostherd-session-progress-state session) '(set indeterminate))
       (when-let* ((at (ghostherd-session-progress-at session)))
         (< (float-time (time-subtract (current-time) at))
            ghostherd-progress-ttl))))

(defun ghostherd--progress-reason (session)
  "Describe SESSION's last progress report, for the state reason."
  (let ((percent (ghostherd-session-progress-percent session)))
    (if percent
        (format "osc progress %d%%" percent)
      "osc progress")))

(defun ghostherd--match-all-rules (text rules)
  "Return every (STATE . PATTERN) in RULES matching TEXT.
`ghostherd--match-rules' stops at the first winner, which is what the
state machine wants but hides why a rule lost.  This reports the lot, in
precedence order, for `ghostherd-explain'."
  (cl-loop for state in '(blocked working idle)
           append (cl-loop for pattern in (alist-get state rules)
                           when (string-match-p pattern text)
                           collect (cons state pattern))))

(defun ghostherd--detect-state (session)
  "Return (STATE . REASON) for SESSION from screen rules / buffer liveness."
  (cond
   ((ghostherd-session-manual-state session)
    (cons (ghostherd-session-manual-state session) "manual"))
   ((not (ghostherd--session-live-p session))
    (cons 'dead "host gone"))
   (t
    (let* ((kind (ghostherd-session-kind session))
           (spec (ignore-errors (ghostherd--spec kind)))
           (rules (plist-get spec :screen-rules))
           (exited (ghostherd--host-exited-p session)))
      (cond
       (exited
        (cons 'dead exited))
       ((eq kind 'shell)
        (cons 'idle "shell"))
       ((null rules)
        (cons 'working "no rules"))
       (t
        (let* ((tail (ghostherd--host-capture session))
               (hit (ghostherd--match-rules tail rules)))
          (cond
           ;; Screen rules keep priority for `blocked': a stale progress
           ;; report must never mask an agent sitting on a prompt.
           ((eq (car-safe hit) 'blocked) hit)
           ;; Otherwise a live progress report beats scraping, which cannot
           ;; tell a working agent from a quiet one.
           ((ghostherd--progress-fresh-p session)
            (cons 'working (ghostherd--progress-reason session)))
           (hit hit)
           ;; Known agent, no match → idle fallback (herdr-style)
           (t (cons 'idle "default_known_agent_idle_fallback"))))))))))

(defun ghostherd--set-state (session new &optional reason)
  "Set SESSION state to NEW with optional REASON, run hooks/notify."
  (let ((old (ghostherd-session-state session)))
    (unless (eq old new)
      (setf (ghostherd-session-state session) new
            (ghostherd-session-state-reason session) reason
            (ghostherd-session-last-active session) (current-time))
      ;; Unseen while working/blocked/done so finish can promote to `done'
      ;; until the user visits the session.
      (when (memq new '(working blocked done))
        (setf (ghostherd-session-seen session) nil))
      (ghostherd--maybe-notify-state session old new)
      (run-hook-with-args 'ghostherd-state-change-hook session old new)
      (when (get-buffer "*ghostherd*")
        (ghostherd--sidebar-refresh)))
    new))

(defun ghostherd-poll-session (session)
  "Recompute and store state for SESSION.  Return new state."
  (setq session (ghostherd-get session))
  (when session
    (pcase-let ((`(,state . ,reason) (ghostherd--detect-state session)))
      ;; Promote idle → done when work finishes (herdr-style: stays
      ;; visible until the user views the session).
      (when (and (eq state 'idle)
                 (memq (ghostherd-session-state session)
                       '(working blocked starting))
                 (not (ghostherd-session-seen session)))
        (setq state 'done
              reason (or reason "idle after work")))
      ;; Once viewed, demote done back to plain idle.
      (when (and (eq state 'idle)
                 (eq (ghostherd-session-state session) 'done)
                 (ghostherd-session-seen session))
        (setq reason (or reason "seen")))
      (ghostherd--set-state session state reason))))

(defun ghostherd-poll-all ()
  "Poll every registered session."
  (ghostherd--ensure-sessions)
  (dolist (session (hash-table-values ghostherd--sessions))
    (ignore-errors (ghostherd-poll-session session))))

(defun ghostherd--ensure-poll-timer ()
  "Start the background poll timer if needed."
  (unless (timerp ghostherd--poll-timer)
    (setq ghostherd--poll-timer
          (run-with-timer ghostherd-poll-interval
                          ghostherd-poll-interval
                          #'ghostherd--poll-tick))))

(defun ghostherd--stop-poll-timer ()
  "Stop the background poll timer when no sessions remain."
  (when (and (timerp ghostherd--poll-timer)
             (= (hash-table-count ghostherd--sessions) 0))
    (cancel-timer ghostherd--poll-timer)
    (setq ghostherd--poll-timer nil)))


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
                     ("ghostherd-state" ghostherd-cmd-state)))
      (unless (assoc (car entry) ghostel-eval-cmds)
        (add-to-list 'ghostel-eval-cmds entry)))))


;;; Spawn / kill / rename

(defun ghostherd-spawn (kind &rest plist)
  "Spawn an agent of KIND and return the `ghostherd-session'.

PLIST keys:
  :name      session name (unique)
  :project   project root directory
  :command   override executable
  :args      override argument list
  :notes     free-form note / role description
  :directory working directory (defaults to project or `default-directory')
  :backend   host to start it on (defaults to `ghostherd-backend')
  :display   when non-nil (default t), pop to the buffer"
  (ghostherd--install-hooks)
  (ghostherd--register-eval-cmds)
  (let* ((spec (ghostherd--spec kind))
         (name (or (plist-get plist :name)
                   (ghostherd--unique-name (symbol-name kind))))
         (project (or (plist-get plist :project)
                      (ghostherd--project-root)))
         (directory (expand-file-name
                     (or (plist-get plist :directory)
                         project
                         default-directory)))
         (command (or (plist-get plist :command)
                      (plist-get spec :command)))
         (args (if (plist-member plist :args)
                   (plist-get plist :args)
                 (plist-get spec :args)))
         (notes (plist-get plist :notes))
         (backend (or (plist-get plist :backend) ghostherd-backend))
         (display (if (plist-member plist :display)
                      (plist-get plist :display)
                    t))
         (started (ghostherd-backend-spawn
                   backend
                   (list :name name :kind kind :command command :args args
                         :directory directory :project project :notes notes)))
         session)
    (setq session
          (ghostherd-session--create
           :id name
           :name name
           :kind kind
           :backend backend
           :host-id (plist-get started :host-id)
           :buffer (plist-get started :buffer)
           :project (and project (expand-file-name project))
           :command command
           :args args
           :state (if (plist-get started :started) 'starting 'idle)
           :state-reason (if (plist-get started :started) "spawned" "shell")
           :notes notes
           :last-active (current-time)))
    (puthash name session ghostherd--sessions)
    (ghostherd--ensure-poll-timer)
    (run-hook-with-args 'ghostherd-session-created-hook session)
    (when display
      (ghostherd-visit session))
    (ghostherd--sidebar-refresh)
    session))

(defun ghostherd-visit (session)
  "Display SESSION, attaching a view to its host when there is none.

On the ghostel backend there is always exactly one buffer and this just
pops to it.  On tmux the buffer is a client that may not exist yet, so
visiting is what runs `tmux attach' -- the only moment in the whole
design where a second terminal emulator is in the picture."
  (setq session (ghostherd-get session))
  (unless session
    (user-error "No such session"))
  (let ((buffer (ghostherd--host-view session)))
    (setf (ghostherd-session-buffer session) buffer
          (ghostherd-session-seen session) t)
    (pop-to-buffer buffer)
    buffer))

;;;###autoload
(defun ghostherd-restore ()
  "Register agents the host still has but this Emacs does not.

Only reachable on a backend whose agents outlive Emacs, so this is
where a herd comes back after a restart.  Sessions come back detached
-- alive, with no buffer -- and the first poll fills in their state
from the screen.

Emacs-side bookkeeping does not survive and is not meant to: a manual
state override, whether a session has been seen, the last OSC progress
report.  Notes do, because they were written onto the host.

Every known backend is asked, not just `ghostherd-backend'.  Changing
which host new agents start on should not hide the ones already
running, and a session keeps the backend it was born with anyway."
  (interactive)
  (let ((restored 0))
    (dolist (entry (mapcan (lambda (backend)
                             (mapcar (lambda (recipe) (cons backend recipe))
                                     (ghostherd-backend-list backend)))
                           ghostherd-known-backends))
      (let* ((backend (car entry))
             (recipe (cdr entry))
             (name (plist-get recipe :name)))
        (unless (ghostherd-get name)
          (puthash name
                   (ghostherd-session--create
                    :id name
                    :name name
                    :kind (or (plist-get recipe :kind) 'shell)
                    :backend backend
                    :host-id (plist-get recipe :host-id)
                    :buffer nil
                    :project (plist-get recipe :project)
                    :command (plist-get recipe :command)
                    :args (plist-get recipe :args)
                    :notes (plist-get recipe :notes)
                    :state 'idle
                    :state-reason "restored"
                    ;; Seen: a herd that was already there when Emacs
                    ;; started has not just finished anything.
                    :seen t
                    :last-active (current-time))
                   ghostherd--sessions)
          (setq restored (1+ restored)))))
    (when (> restored 0)
      (ghostherd--ensure-poll-timer)
      (ghostherd--sidebar-refresh))
    restored))

(defun ghostherd-session-recipe (session)
  "Return the plist `ghostherd-spawn' needs to recreate SESSION.

This is the whole of what a respawn can carry over.  The conversation
is not in it and never can be: it lives in the CLI's own store, which
is what `:continue-args' reaches.

Whether the *agent* survives depends on the backend.  On ghostel it
cannot -- the agent is an Emacs child.  On tmux it does, and this same
recipe is written onto the host so `ghostherd-restore' can rebuild the
session around a process that never stopped."
  (list :name (ghostherd-session-name session)
        :project (ghostherd-session-project session)
        :directory (ghostherd-session-project session)
        :backend (ghostherd-session-backend session)
        :command (ghostherd-session-command session)
        :args (ghostherd-session-args session)
        :notes (ghostherd-session-notes session)))

;;;###autoload
(defun ghostherd-respawn (session &optional continue)
  "Relaunch SESSION from its recipe, replacing any existing buffer.

With CONTINUE non-nil, append the CLI's `:continue-args' so it resumes
its previous conversation rather than starting cold.  Interactively that
is the default when the agent kind has such a flag, since the usual
reason to respawn is that the agent died or wedged and you want it back
where it was.

Called interactively, acts on the sidebar row at point when there is
one, and otherwise prompts."
  (interactive
   (let* ((session (or (and (derived-mode-p 'ghostherd-sidebar-mode)
                            (ghostherd--sidebar-session-at-point))
                       (ghostherd--read-session "Respawn agent: ")))
          (spec (ignore-errors
                  (ghostherd--spec (ghostherd-session-kind session))))
          (continuable (plist-get spec :continue-args)))
     (list session
           (and continuable
                (y-or-n-p (format "Resume the previous conversation (%s)? "
                                  (string-join continuable " ")))))))
  (setq session (ghostherd-get session))
  (unless session
    (user-error "No such session"))
  (let* ((kind (ghostherd-session-kind session))
         (spec (ghostherd--spec kind))
         (recipe (ghostherd-session-recipe session))
         (args (append (plist-get recipe :args)
                       (and continue (plist-get spec :continue-args)))))
    (when (and continue (null (plist-get spec :continue-args)))
      (user-error "%s has no resume flag" kind))
    ;; Free both the buffer name and the registry id before respawning:
    ;; `ghostherd-spawn' refuses to clobber an existing buffer.
    (ghostherd-kill session t)
    (apply #'ghostherd-spawn kind (plist-put (copy-sequence recipe) :args args))))

;;;###autoload
(defun ghostherd-new (&optional kind)
  "Interactively create a new ghostherd agent session.
With prefix or KIND, skip the kind prompt."
  (interactive)
  (let* ((project (ghostherd--project-root))
         ;; Resolved before the kind prompt so a project can preselect it.
         ;; A later change of directory does not re-read: re-prompting for
         ;; kind after you already answered would be worse than stale.
         (defaults (ghostherd-project-defaults (or project default-directory)))
         (kind (or kind
                   (intern
                    (completing-read
                     "Agent kind: "
                     (mapcar #'symbol-name (ghostherd--agent-kinds))
                     nil t nil nil
                     (symbol-name (or (car defaults) ghostherd-default-kind))))))
         (spec (ghostherd--spec kind))
         (default-name (ghostherd--unique-name (symbol-name kind)))
         (name (read-string "Session name: " default-name))
         (directory (if project
                        (read-directory-name "Directory: " project nil t)
                      (read-directory-name "Directory: " default-directory nil t)))
         (arg-string (read-string
                      (format "Extra args for %s (optional): "
                              (or (plist-get spec :command) "shell"))
                      (when-let* ((a (or (cdr defaults)
                                         (plist-get spec :args))))
                        (string-join a " "))))
         (args (when (and arg-string (not (string-empty-p arg-string)))
                 (split-string-and-unquote arg-string)))
         (notes (read-string "Role / notes (optional): ")))
    (ghostherd-spawn
     kind
     :name (if (string-empty-p name) default-name name)
     :project (ghostherd--project-root directory)
     :directory directory
     :args args
     :notes (and (not (string-empty-p notes)) notes))))

;;;###autoload
(defun ghostherd-new-pair ()
  "Spawn two agents side by side for the current project.
Prompts for left/right kinds and names (defaults: implementer + reviewer)."
  (interactive)
  (let* ((project (or (ghostherd--project-root)
                      (user-error "Not in a project")))
         (left-kind (intern
                     (completing-read
                      "Left agent kind: "
                      (mapcar #'symbol-name (ghostherd--agent-kinds))
                      nil t nil nil "claude")))
         (right-kind (intern
                      (completing-read
                       "Right agent kind: "
                       (mapcar #'symbol-name (ghostherd--agent-kinds))
                       nil t nil nil "claude")))
         (left-name (read-string "Left name: " "implementer"))
         (right-name (read-string "Right name: " "reviewer"))
         (left (ghostherd-spawn left-kind
                                :name left-name
                                :project project
                                :notes "implementer"
                                :display nil))
         (right (ghostherd-spawn right-kind
                                 :name right-name
                                 :project project
                                 :notes "reviewer"
                                 :display nil)))
    ;; Two Emacs windows, each showing one agent.  On tmux that is two
    ;; attach clients; it is emphatically not a tmux split -- pane layout
    ;; is something Emacs already does better than a herd manager should
    ;; reimplement.
    (delete-other-windows)
    (switch-to-buffer (ghostherd--host-view left))
    (split-window-right)
    (other-window 1)
    (switch-to-buffer (ghostherd--host-view right))
    (message "Spawned %s (%s) | %s (%s)"
             left-name left-kind right-name right-kind)
    (list left right)))

(defun ghostherd--maybe-restore ()
  "Re-adopt hosted agents when the registry has nothing in it.

Cheap insurance against a transient blind spot.  `ghostherd--ensure-sessions'
runs from the mode-line and prunes anything whose host it cannot reach,
so one moment of a host being unreachable would otherwise drop the herd
from the UI until the mode was toggled -- even though every agent is
still running."
  (when (zerop (hash-table-count ghostherd--sessions))
    (ignore-errors (ghostherd-restore))))

(defun ghostherd--read-session (&optional prompt predicate)
  "Read a session via completing-read using PROMPT and optional PREDICATE."
  (ghostherd--maybe-restore)
  (ghostherd--ensure-sessions)
  (let* ((sessions (cl-remove-if-not
                    (or predicate #'identity)
                    (ghostherd-sessions)))
         (candidates (mapcar #'ghostherd--format-candidate sessions)))
    (unless candidates
      (user-error "No ghostherd sessions"))
    (let ((choice (completing-read
                   (or prompt "Session: ")
                   (ghostherd--session-completion-table candidates)
                   nil t)))
      (or (ghostherd-get choice)
          (user-error "Unknown session")))))

;;;###autoload
;;;###autoload
(defun ghostherd-explain (&optional session)
  "Explain how SESSION's state was decided.
Shows the scraped tail, every `:screen-rules' pattern that matched it,
and which one won.  Rule tuning is guesswork without this: a false
`blocked' looks identical to a real one until you can see that some
pattern matched text the agent merely printed.

Interactively, uses the sidebar row at point when there is one, and
otherwise prompts."
  (interactive)
  (let* ((session (or session
                      (and (derived-mode-p 'ghostherd-sidebar-mode)
                           (ghostherd--sidebar-session-at-point))
                      (ghostherd--read-session "Explain agent: ")))
         (kind (ghostherd-session-kind session))
         (spec (ignore-errors (ghostherd--spec kind)))
         (rules (plist-get spec :screen-rules))
         (live (ghostherd--session-live-p session))
         (tail (and live (ghostherd--host-capture session)))
         (hits (and tail rules (ghostherd--match-all-rules tail rules)))
         (winner (and tail rules (ghostherd--match-rules tail rules))))
    (with-help-window "*ghostherd explain*"
      (with-current-buffer standard-output
        (insert (format "%s  (%s)\n\n" (ghostherd-session-name session) kind))
        (insert (format "  state    %s %s\n"
                        (ghostherd--state-glyph (ghostherd-session-state session))
                        (ghostherd-session-state session)))
        (insert (format "  reason   %s\n"
                        (or (ghostherd-session-state-reason session) "—")))
        (when (ghostherd-session-manual-state session)
          (insert (format "  manual   %s  (overrides detection until cleared)\n"
                          (ghostherd-session-manual-state session))))
        (insert (format "  host     %s  (%s)\n"
                        (or (ghostherd-session-host-id session) "—")
                        (ghostherd-session-backend session)))
        (insert (format "  view     %s\n\n"
                        (let ((buffer (ghostherd-session-buffer session)))
                          (cond ((buffer-live-p buffer) (buffer-name buffer))
                                (live "detached — nobody is attached")
                                (t "—")))))

        (cond
         ((not live)
          (insert "Host is gone; no detection runs.\n"))
         ((null rules)
          (insert "No :screen-rules for this kind — state is forced to `working'.\n"))
         ((null hits)
          (insert "No rule matched. Falling back to `idle'.\n"))
         (t
          (insert "Matches, in precedence order (blocked > working > idle):\n\n")
          (pcase-dolist (`(,state . ,pattern) hits)
            (insert (format "  %-8s %-3s %s\n"
                            state
                            (if (equal (cons state pattern) winner) "->" "")
                            pattern)))
          (insert "\nOnly the first line wins.  A rule listed under a\n")
          (insert "higher-precedence state shadows every rule below it.\n")))

        (when tail
          (insert (format "\nScraped tail (last %d lines):\n"
                          ghostherd-screen-tail-lines))
          (insert (make-string 60 ?-) "\n")
          (insert tail)
          (unless (bolp) (insert "\n"))
          (insert (make-string 60 ?-) "\n"))))))

(defun ghostherd-switch ()
  "Switch to a ghostherd session (consult/completing-read)."
  (interactive)
  (ghostherd-visit (ghostherd--read-session "Switch to agent: ")))

;;;###autoload
(defun ghostherd-next-blocked ()
  "Jump to the next blocked (or done/unseen) session."
  (interactive)
  (ghostherd-poll-all)
  (let* ((priority '(blocked done))
         (sessions (ghostherd-sessions))
         (targets (cl-remove-if-not
                   (lambda (s)
                     (or (memq (ghostherd-session-state s) priority)
                         (not (ghostherd-session-seen s))))
                   sessions)))
    (unless targets
      (user-error "No blocked/done agents"))
    (let ((session (car targets)))
      (ghostherd-visit session)
      (message "Jumped to %s (%s)"
               (ghostherd-session-name session)
               (ghostherd-session-state session)))))

;;;###autoload
(defun ghostherd-kill (session &optional kill-buffer)
  "Remove SESSION from the herd.
When KILL-BUFFER is non-nil (the interactive default), also kill its buffer."
  (interactive
   (list (ghostherd--read-session "Kill session: ") t))
  (setq session (ghostherd-get session))
  (unless session
    (user-error "No such session"))
  (let ((buf (ghostherd-session-buffer session))
        (id (ghostherd-session-id session)))
    (remhash id ghostherd--sessions)
    (run-hook-with-args 'ghostherd-session-removed-hook session)
    (when kill-buffer
      ;; The host first: on tmux the buffer is only a client, so killing
      ;; it would detach and leave the agent running headless with
      ;; nothing in the herd pointing at it.
      (ghostherd--host-kill session)
      (when (buffer-live-p buf)
        (let ((kill-buffer-query-functions nil)
              (ghostel-query-before-killing nil))
          (ignore-errors (kill-buffer buf)))))
    (ghostherd--stop-poll-timer)
    (ghostherd--sidebar-refresh)
    (message "Killed session %s" (ghostherd-session-name session))))

;;;###autoload
;;;###autoload
(defun ghostherd-set-notes (session notes)
  "Set SESSION's NOTES -- the free-form role description.

Kept out of the session name on purpose: the name is an identifier that
buffer names and `ghostherd-get' depend on, so it cannot carry \"reviews
the auth branch, do not let it push\".  Notes show up in the switcher
annotation, which is where you are choosing between agents."
  (interactive
   (let ((s (ghostherd--read-session "Note for agent: ")))
     (list s (read-string "Note: " (ghostherd-session-notes s)))))
  (setq session (ghostherd-get session))
  (unless session
    (user-error "No such session"))
  (let ((notes (and notes (string-trim notes))))
    (setf (ghostherd-session-notes session)
          (and notes (not (string-empty-p notes)) notes)))
  (ghostherd--sidebar-refresh)
  (ghostherd-session-notes session))

(defun ghostherd-rename (session new-name)
  "Rename SESSION to NEW-NAME."
  (interactive
   (let* ((s (ghostherd--read-session "Rename session: "))
          (n (read-string "New name: " (ghostherd-session-name s))))
     (list s n)))
  (setq session (ghostherd-get session))
  (when (ghostherd-get new-name)
    (user-error "Name already in use: %s" new-name))
  (let ((old-id (ghostherd-session-id session)))
    (remhash old-id ghostherd--sessions)
    (setf (ghostherd-session-id session) new-name
          (ghostherd-session-name session) new-name)
    (puthash new-name session ghostherd--sessions)
    ;; The host has its own name for the session -- a tmux session name,
    ;; the buffer name -- and it is the backend's job to keep it in step.
    (ghostherd--host-rename session new-name)
    (ghostherd--sidebar-refresh)
    (message "Renamed to %s" new-name)))

;;;###autoload
(defun ghostherd-mark-state (session state)
  "Manually set SESSION state (sticky until cleared).
STATE is one of working, blocked, idle, done, dead, or auto (clear)."
  (interactive
   (list (ghostherd--read-session "Session: ")
         (intern (completing-read
                  "State: "
                  '("auto" "working" "blocked" "idle" "done" "dead")
                  nil t))))
  (setq session (ghostherd-get session))
  (if (eq state 'auto)
      (progn
        (setf (ghostherd-session-manual-state session) nil)
        (ghostherd-poll-session session)
        (message "Auto state for %s" (ghostherd-session-name session)))
    (setf (ghostherd-session-manual-state session) state)
    (ghostherd--set-state session state "manual")
    (message "%s → %s (manual)" (ghostherd-session-name session) state)))


;;; Inter-agent communication

(defun ghostherd-send (session text &optional submit)
  "Send TEXT to SESSION's terminal.
When SUBMIT is non-nil, also send RET (Enter)."
  (setq session (ghostherd-get session))
  (unless (ghostherd--session-live-p session)
    (user-error "Session host is not live"))
  (ghostherd--host-send-text session text submit)
  (unless (ghostherd-session-manual-state session)
    (ghostherd--set-state session 'working "input sent"))
  text)

;;;###autoload
(defun ghostherd-send-keys (session &rest keys)
  "Send control KEYS to SESSION -- Escape, C-c, arrows, Tab and so on.

`ghostherd-send' can only paste text and optionally press Return, which
covers talking *to* an agent but not operating its interface.  Escaping
a runaway generation, answering an arrow-driven permission menu or
sending EOF are keystrokes, not text: pasting the characters \"esc\"
just feeds the agent a word.

Unlike `ghostherd-send', this does not mark the session `working'.  The
usual reason to send a key is to make the agent *stop*, so claiming it
just started would be backwards; the next poll reports what actually
happened."
  (setq session (ghostherd-get session))
  (unless (ghostherd--session-live-p session)
    (user-error "Session host is not live"))
  (ghostherd--host-send-keys session keys)
  keys)

;;;###autoload
(defun ghostherd-interrupt (session)
  "Send Escape to SESSION -- what agent CLIs bind to \"esc to interrupt\"."
  (interactive (list (ghostherd--read-session "Interrupt agent: ")))
  (ghostherd-send-keys session "esc"))

;;;###autoload
(defun ghostherd-abort (session)
  "Send C-c to SESSION, for when Escape is not enough."
  (interactive (list (ghostherd--read-session "Abort agent: ")))
  (ghostherd-send-keys session "C-c"))

;;;###autoload
(defun ghostherd-answer (session choice)
  "Answer SESSION's menu prompt by moving to CHOICE and pressing Return.

CHOICE is 1-based.  Some agent CLIs accept the digit as text; others
only respond to the arrow keys, and there is no way to tell from the
outside which you are looking at.  Arrows work for both."
  (interactive
   (list (ghostherd--read-session "Answer agent: ")
         (read-number "Choice (1-based): " 1)))
  (when (< choice 1)
    (user-error "Choice is 1-based"))
  ;; Home first: the highlighted option is not necessarily the first.
  (apply #'ghostherd-send-keys session
         (append (make-list 9 "up")
                 (make-list (1- choice) "down")
                 (list "return"))))

(defun ghostherd-prompt (session prompt &optional wait timeout)
  "Submit PROMPT to SESSION (text + Enter).
When WAIT is non-nil, poll until state is idle/done/blocked or TIMEOUT
seconds elapse (default 120)."
  (ghostherd-send session prompt t)
  (when wait
    (ghostherd-wait session '(idle done blocked) (or timeout 120)))
  session)

(defun ghostherd-read (session &optional n-lines)
  "Return the last N-LINES of SESSION's screen."
  (setq session (ghostherd-get session))
  (unless (ghostherd--session-live-p session)
    (user-error "Session host is not live"))
  (ghostherd--host-capture session (or n-lines ghostherd-screen-tail-lines)))

(defcustom ghostherd-wait-poll-interval 0.4
  "Seconds between checks in `ghostherd-wait' and `ghostherd-wait-output'."
  :type 'number
  :group 'ghostherd)

(defun ghostherd-wait (session states &optional timeout)
  "Block until SESSION state is a member of STATES or TIMEOUT seconds.
STATES is a list of symbols.  Returns the state or signals on timeout.
TIMEOUT nil means wait indefinitely (not recommended interactively)."
  (setq session (ghostherd-get session))
  (let ((deadline (and timeout (+ (float-time) timeout)))
        state)
    (while (progn
             (setq state (ghostherd-poll-session session))
             (and (not (memq state states))
                  (or (null deadline)
                      (< (float-time) deadline))))
      (sit-for ghostherd-wait-poll-interval))
    (unless (memq state states)
      (user-error "Timeout waiting for %s to become %s (was %s)"
                  (ghostherd-session-name session) states state))
    state))

(defun ghostherd-output-matches (session regexp &optional lines)
  "Return the text in SESSION's last LINES matching REGEXP, or nil.

Matching is case-sensitive: a regexp written to catch a prompt should
mean what it says.  Note this differs from `:screen-rules', which match
under whatever `case-fold-search' happens to be -- see the known issue
in readme.org."
  (setq session (ghostherd-get session))
  (when (ghostherd--session-live-p session)
    (let ((case-fold-search nil)
          (tail (ghostherd--host-capture session lines)))
      (when (string-match regexp tail)
        (match-string 0 tail)))))

;;;###autoload
(defun ghostherd-wait-output (session regexp &optional timeout noerror lines)
  "Block until REGEXP appears in SESSION's recent output, or TIMEOUT.

`ghostherd-wait' watches the state machine, which only knows the five
states the screen rules can produce.  Plenty of things worth waiting for
have no state of their own -- a test summary line, a specific error, the
agent naming the file it just wrote.

Returns the matched text.  On timeout, signals unless NOERROR, in which
case it returns nil; scripts branching on \"did it happen\" should not
have to wrap every call in `condition-case'.

LINES overrides how much of the tail is searched, defaulting to
`ghostherd-screen-tail-lines'.  TIMEOUT nil waits indefinitely."
  (setq session (ghostherd-get session))
  (let ((deadline (and timeout (+ (float-time) timeout)))
        found)
    ;; Check before sleeping: output can arrive between whatever triggered
    ;; the wait and the wait itself, and a first poll one interval late
    ;; would miss it.
    (while (and (not (setq found (ghostherd-output-matches
                                  session regexp lines)))
                (ghostherd--session-live-p session)
                (or (null deadline) (< (float-time) deadline)))
      (sit-for ghostherd-wait-poll-interval))
    (cond
     (found)
     (noerror nil)
     (t (user-error "Timeout waiting for %s to output %s"
                    (ghostherd-session-name session) regexp)))))

(cl-defun ghostherd-message (from to body &key (submit ghostherd-submit-on-message))
  "Send an attributed message from session FROM to session TO.
BODY is the payload.  SUBMIT defaults to `ghostherd-submit-on-message'.
FROM may be a session, session name, or free-form label such as \"user\"."
  (setq to (ghostherd-get to))
  (unless to
    (user-error "Unknown target session"))
  (let* ((from-session (and from (ghostherd-get from)))
         (from-name (cond
                     (from-session (ghostherd-session-name from-session))
                     ((and from (stringp from)) from)
                     (t "user")))
         (to-name (ghostherd-session-name to))
         (payload (format ghostherd-message-template from-name to-name body)))
    (ghostherd-send to payload submit)
    (message "Message %s → %s" from-name to-name)
    payload))

;;;###autoload
(defcustom ghostherd-handoff-settled-states '(blocked idle done dead)
  "States that end a `ghostherd-handoff' watch."
  :type '(repeat symbol)
  :group 'ghostherd)

(defcustom ghostherd-handoff-grace 4
  "Seconds after a handoff during which a settled state is not believed.

An agent does not start working the instant it is handed something: the
CLI has to wake up and print, and until it does the screen rules see the
same idle prompt they saw before.  Without this the watch would report
completion immediately, every time."
  :type 'number
  :group 'ghostherd)

(defvar ghostherd--handoff-watches (make-hash-table :test 'equal)
  "Active handoff watch timers, keyed by session id.")

(defun ghostherd--handoff-cancel (id)
  "Cancel any handoff watch on session ID."
  (when-let* ((timer (gethash id ghostherd--handoff-watches)))
    (cancel-timer timer)
    (remhash id ghostherd--handoff-watches)))

(defun ghostherd--handoff-tick (id deadline grace callback)
  "One poll of the handoff watch on session ID."
  (let ((session (ghostherd-get id))
        (now (float-time)))
    (condition-case error
        (cond
         ((null session) (ghostherd--handoff-cancel id))
         ((< now grace) nil)
         ((memq (ghostherd-poll-session session)
                ghostherd-handoff-settled-states)
          (ghostherd--handoff-cancel id)
          (funcall callback session (ghostherd-session-state session)))
         ((and deadline (>= now deadline))
          (ghostherd--handoff-cancel id)
          (funcall callback session 'timeout)))
      ;; A broken callback must not leave the timer running forever.
      (error
       (ghostherd--handoff-cancel id)
       (message "ghostherd handoff watch failed: %s"
                (error-message-string error))))))

;;;###autoload
(defun ghostherd-handoff (to text &optional from timeout callback)
  "Hand TEXT to agent TO, then watch it settle -- without blocking Emacs.

This is the pipeline primitive: message an agent, wait, report.  It is
deliberately asynchronous.  `ghostherd-wait' blocks in a `sit-for' loop,
which would freeze you out of the very editor you are meant to keep
working in while the other agent runs -- and orchestrating without
having to sit and watch a pane is the whole point.

FROM attributes the message (a session, a name, or a label like
\"user\").  CALLBACK receives (SESSION STATE), where STATE is the
settled state or the symbol `timeout'; it defaults to a notification.
TIMEOUT defaults to 600 seconds, nil means no deadline.

Handing off to a session that is already being watched replaces the
watch rather than stacking a second timer on it."
  (setq to (ghostherd-get to))
  (unless to
    (user-error "Unknown target session"))
  (let* ((id (ghostherd-session-id to))
         (timeout (if (eq timeout 'none) nil (or timeout 600)))
         (callback (or callback #'ghostherd--handoff-notify))
         (now (float-time)))
    (if from
        (ghostherd-message from to text :submit t)
      (ghostherd-send to text t))
    (ghostherd--handoff-cancel id)
    (puthash id
             (run-with-timer
              ghostherd-wait-poll-interval ghostherd-wait-poll-interval
              #'ghostherd--handoff-tick
              id
              (and timeout (+ now timeout))
              (+ now ghostherd-handoff-grace)
              callback)
             ghostherd--handoff-watches)
    to))

(defun ghostherd--handoff-notify (session state)
  "Default `ghostherd-handoff' callback: notify about SESSION and STATE."
  (ghostherd--notify
   (format "ghostherd · %s %s" (ghostherd-session-name session) state)
   (if (eq state 'timeout)
       "handoff timed out"
     (or (ghostherd-session-state-reason session) "handoff settled"))))

;;;###autoload
(defun ghostherd-handoff-interactive (to text)
  "Hand TEXT to agent TO from the current session, and watch it settle."
  (interactive
   (let ((to (ghostherd--read-session "Hand off to agent: ")))
     (list to (read-string (format "Message for %s: "
                                   (ghostherd-session-name to))))))
  (ghostherd-handoff to text (or (ghostherd-get (current-buffer)) "user")))

(defun ghostherd-message-interactive (to body)
  "Interactively message agent TO with BODY (from user or current session)."
  (interactive
   (list (ghostherd--read-session "Message to: ")
         (read-string "Message: ")))
  (let ((from (ghostherd-get (current-buffer))))
    (ghostherd-message from to body :submit t)))

;;;###autoload
(defun ghostherd-prompt-interactive (session prompt)
  "Interactively prompt SESSION with PROMPT."
  (interactive
   (list (ghostherd--read-session "Prompt agent: ")
         (read-string "Prompt: ")))
  (ghostherd-prompt session prompt)
  (message "Prompted %s" (ghostherd-session-name (ghostherd-get session))))


;;; Commands callable from agent shells via ghostel_cmd

(defun ghostherd--json (object)
  "Encode OBJECT as JSON.
`json-serialize' is built in since Emacs 27 and this package requires
28.1, so there is no json.el fallback to keep."
  (json-serialize object))

(defun ghostherd-session-as-alist (session)
  "Return SESSION as an alist ready for JSON encoding."
  (list (cons 'name (ghostherd-session-name session))
        (cons 'kind (symbol-name (ghostherd-session-kind session)))
        (cons 'state (symbol-name (ghostherd-session-state session)))
        (cons 'reason (or (ghostherd-session-state-reason session) ""))
        (cons 'project (or (ghostherd-session-project session) ""))
        (cons 'notes (or (ghostherd-session-notes session) ""))
        (cons 'age (ghostherd--age-string
                    (or (ghostherd-session-last-active session)
                        (ghostherd-session-started-at session))))))

(defun ghostherd-cmd-list (&rest _)
  "Return herd sessions as JSON, for agent shells.

JSON rather than the tab/equals text this used to emit, because two of
the fields are free-form and the old format had no escaping: a note
containing a tab or a newline silently turned one record into several,
and `reason' holds a raw `:screen-rules' regexp, which can contain
anything at all."
  (ghostherd--ensure-sessions)
  (ghostherd--json
   (vconcat (mapcar #'ghostherd-session-as-alist (ghostherd-sessions)))))

(defun ghostherd-cmd-state (name &rest _)
  "Return the state of session NAME as JSON."
  (ghostherd--json
   (if-let* ((s (ghostherd-get name)))
       (ghostherd-session-as-alist s)
     (list (cons 'error "unknown session")
           (cons 'name name)))))

(defun ghostherd-cmd-send (name text &rest _)
  "Send TEXT to session NAME without submitting."
  (ghostherd-send name text nil)
  (format "sent to %s" name))

(defun ghostherd-cmd-prompt (name text &rest _)
  "Prompt session NAME with TEXT (submitted)."
  (ghostherd-prompt name text)
  (format "prompted %s" name))

(defun ghostherd-cmd-message (to text &optional from &rest _)
  "Message TO with TEXT, optional FROM name (default: user/caller)."
  (ghostherd-message (or from "user") to text :submit t)
  (format "message → %s" to))

(defun ghostherd-cmd-read (name &optional n &rest _)
  "Read last N lines from session NAME."
  (ghostherd-read name (and n (string-to-number n))))


;;; Display helpers

(defun ghostherd--state-glyph (state)
  "Return a short glyph for STATE."
  (pcase state
    ('blocked "⚠")
    ('working "⟳")
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


;;; Sidebar

(defvar ghostherd--sidebar-filter-project nil
  "When non-nil, sidebar only shows this project root.")

(defconst ghostherd-sidebar-help-commands
  '((ghostherd-sidebar-visit                 . "Visit agent buffer")
    (next-line                               . "Next row")
    (previous-line                           . "Previous row")
    (ghostherd-new                           . "New agent")
    (ghostherd-new-pair                      . "New pair (split windows)")
    (ghostherd-sidebar-kill                  . "Kill session")
    (ghostherd-sidebar-rename                . "Rename session")
    (ghostherd-respawn                       . "Respawn from recipe")
    (ghostherd-sidebar-notes                 . "Edit note / role")
    (ghostherd-sidebar-interrupt             . "Interrupt (Escape)")
    (ghostherd-sidebar-abort                 . "Abort (C-c)")
    (ghostherd-sidebar-answer                . "Answer menu prompt")
    (ghostherd-sidebar-message               . "Message agent")
    (ghostherd-sidebar-prompt                . "Prompt agent")
    (ghostherd-sidebar-toggle-project-filter . "Toggle project filter")
    (ghostherd-sidebar-refresh               . "Refresh")
    (ghostherd-next-blocked                  . "Next blocked / done")
    (ghostherd-sidebar-mark-state            . "Mark state (manual / auto)")
    (ghostherd-explain                       . "Explain how state was decided")
    (ghostherd-sidebar-help                  . "This help")
    (quit-window                             . "Quit"))
  "Commands listed by `ghostherd-sidebar-help', in display order.")

(defun ghostherd--sidebar-help-keys (command)
  "Return up to two readable key descriptions for COMMAND in this buffer."
  (seq-take
   (delete-dups
    (delq nil
          (mapcar
           (lambda (key)
             ;; Evil also exposes its state bindings under a `<normal-state>'
             ;; pseudo-prefix, which just duplicates the real key.
             (unless (and (> (length key) 0)
                          (symbolp (aref key 0))
                          (string-suffix-p "-state" (symbol-name (aref key 0))))
               (key-description key)))
           (where-is-internal command nil nil nil t))))
   2))

(defun ghostherd-sidebar-help ()
  "Show the sidebar key bindings.
Keys are looked up from the buffer's live keymaps rather than hardcoded,
so the listing stays correct when Evil or a user keymap rebinds them --
e.g. Evil users typically move kill off `k' so it can still move point.
Commands with no binding in the current state are omitted."
  (interactive)
  (let* ((origin (current-buffer))
         (rows (delq nil
                     (mapcar
                      (lambda (entry)
                        (when-let* ((keys (with-current-buffer origin
                                            (ghostherd--sidebar-help-keys
                                             (car entry)))))
                          (cons (string-join keys ", ") (cdr entry))))
                      ghostherd-sidebar-help-commands)))
         (width (apply #'max 3 (mapcar (lambda (row) (length (car row))) rows)))
         ;; Emacs `format' has no `*' field width; build the format string.
         (line-format (format "  %%-%ds  %%s\n" width)))
    (with-help-window "*ghostherd help*"
      (with-current-buffer standard-output
        (insert "GhostHerd sidebar\n\n")
        (pcase-dolist (`(,keys . ,description) rows)
          (insert (format line-format keys description)))
        (insert "\nState glyphs\n\n")
        (dolist (state '(blocked working done idle starting dead))
          (insert (format "  %-3s %s\n"
                          (ghostherd--state-glyph state) state)))
        (insert (format "\n  %-3s no view attached -- the agent is running\n"
                        ghostherd-detached-glyph))
        (insert "      and nobody is looking.  RET attaches one.\n")))))

(defvar-keymap ghostherd-sidebar-mode-map
  :doc "Keymap for `ghostherd-sidebar-mode'."
  "?" #'ghostherd-sidebar-help
  "e" #'ghostherd-explain
  "R" #'ghostherd-respawn
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
  "s" #'ghostherd-sidebar-toggle-project-filter
  "g" #'ghostherd-sidebar-refresh
  "q" #'quit-window
  "." #'ghostherd-next-blocked
  "M" #'ghostherd-sidebar-mark-state)

(define-derived-mode ghostherd-sidebar-mode tabulated-list-mode "GhostHerd"
  "Sidebar listing ghostherd agent sessions."
  ;; Padding first: `ghostherd--sidebar-format' budgets against it.
  (setq tabulated-list-padding 1)
  (setq tabulated-list-format (ghostherd--sidebar-format))
  (setq tabulated-list-sort-key (cons "Name" nil))
  (add-hook 'tabulated-list-revert-hook #'ghostherd--sidebar-entries nil t)
  (tabulated-list-init-header))

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
  "Columns the sidebar actually has: the live window, else the configured width.
Reading the window means the layout follows a manual resize, not just
the value of `ghostherd-sidebar-width'."
  (or (when-let* ((buf (get-buffer "*ghostherd*"))
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
    (dolist (spec ghostherd--sidebar-column-specs (nreverse kept))
      (when (or (not (eq (car spec) 'title)) ghostherd-sidebar-show-title)
        ;; tabulated-list draws `tabulated-list-padding' leading columns and
        ;; a separating space after every column but the last.
        (let ((cost (+ (nth 2 spec) (if kept 1 tabulated-list-padding))))
          (when (or (nth 3 spec) (<= (+ used cost) budget))
            (setq used (+ used cost))
            (push spec kept)))))))

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

(defun ghostherd--sidebar-cell (session key state face)
  "Return SESSION's cell for column KEY, given its STATE and FACE."
  (pcase key
    ('glyph   (concat
               (propertize (ghostherd--state-glyph state) 'face face)
               ;; Whether anyone is looking is a fact about the *view*,
               ;; so it is a separate mark rather than a state glyph of
               ;; its own -- an agent is working or blocked regardless.
               ;; It belongs in the overview all the same: without it a
               ;; herd running headless looks exactly like one you are
               ;; watching.
               (unless (buffer-live-p (ghostherd-session-buffer session))
                 (propertize ghostherd-detached-glyph 'face 'shadow))))
    ('name    (propertize (ghostherd-session-name session) 'face face))
    ('kind    (symbol-name (ghostherd-session-kind session)))
    ('state   (propertize (symbol-name state) 'face face))
    ('project (ghostherd--abbreviate (ghostherd-session-project session)))
    ('title   (or (ghostherd--session-title session) ""))
    ('age     (ghostherd--age-string
               (or (ghostherd-session-last-active session)
                   (ghostherd-session-started-at session))))
    (_        "")))

(defun ghostherd--sidebar-build-entries ()
  "Rebuild `tabulated-list-entries' from the sessions as they stand.
Pure rendering -- it does not poll, so unlike `ghostherd--sidebar-entries'
it is safe to call from inside the poll path without recursing."
  (ghostherd--sidebar-sync-format)
  (let ((columns (ghostherd--sidebar-visible-columns))
        (sessions (if ghostherd--sidebar-filter-project
                      (ghostherd-sessions ghostherd--sidebar-filter-project)
                    (ghostherd-sessions))))
    (setq tabulated-list-entries
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
           sessions))))

(defun ghostherd--sidebar-entries ()
  "Poll every session, then rebuild `tabulated-list-entries'.
Used by the interactive refresh and `tabulated-list-revert-hook'."
  (ghostherd-poll-all)
  (ghostherd--sidebar-build-entries))

(defun ghostherd--sidebar-refresh ()
  "Re-render the sidebar from current session state, without polling.
Rebuilding the entries is the point: `tabulated-list-print' alone
re-prints whatever `tabulated-list-entries' already held, so a state
change would redraw the same stale row it drew last time."
  (when-let* ((buf (get-buffer "*ghostherd*")))
    (with-current-buffer buf
      (when (derived-mode-p 'ghostherd-sidebar-mode)
        (ghostherd--sidebar-build-entries)
        (tabulated-list-print t)))))

(defun ghostherd--poll-tick ()
  "Timer callback: poll every session, then keep a visible sidebar current.
A state change already redraws via `ghostherd--set-state', but the Age
column advances with no state change at all, so refresh on each tick
while the sidebar is actually on screen."
  (ghostherd-poll-all)
  (when-let* ((buf (get-buffer "*ghostherd*")))
    (when (get-buffer-window buf t)
      (ghostherd--sidebar-refresh))))

(defun ghostherd-sidebar-refresh ()
  "Interactive sidebar refresh."
  (interactive)
  (ghostherd--sidebar-entries)
  (tabulated-list-print t)
  (message "ghostherd refreshed"))

(defun ghostherd--sidebar-session-at-point ()
  "Return the session for the sidebar row at point."
  (ghostherd-get (tabulated-list-get-id)))

(defun ghostherd-sidebar-visit ()
  "Visit the session at point, attaching a view when it is detached."
  (interactive)
  (when-let* ((s (ghostherd--sidebar-session-at-point)))
    (ghostherd-visit s)))

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

(defun ghostherd-sidebar-message ()
  "Message the session at point."
  (interactive)
  (when-let* ((s (ghostherd--sidebar-session-at-point))
              (body (read-string
                     (format "Message → %s: " (ghostherd-session-name s)))))
    (ghostherd-message (ghostherd-get (current-buffer)) s body t)))

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

(defun ghostherd-sidebar-toggle-project-filter ()
  "Toggle filtering the sidebar to the current project."
  (interactive)
  (setq ghostherd--sidebar-filter-project
        (if ghostherd--sidebar-filter-project
            nil
          (or (ghostherd--project-root)
              (user-error "Not in a project"))))
  (ghostherd-sidebar-refresh)
  (message "Project filter: %s"
           (or ghostherd--sidebar-filter-project "off")))

;;;###autoload
(defun ghostherd-sidebar ()
  "Open the ghostherd sidebar in a side window."
  (interactive)
  (ghostherd--maybe-restore)
  (ghostherd--ensure-sessions)
  (let ((buf (get-buffer-create "*ghostherd*")))
    (with-current-buffer buf
      (ghostherd-sidebar-mode)
      (ghostherd--sidebar-entries)
      (tabulated-list-print t))
    (pop-to-buffer
     buf
     `((display-buffer-in-side-window)
       (side . ,ghostherd-sidebar-side)
       (slot . 0)
       (window-width . ,ghostherd-sidebar-width)
       (preserve-size . (t . nil))))
    buf))


;;; Global minor mode / modeline

(defun ghostherd--mode-line-segment ()
  "Mode-line segment summarizing herd attention.

Reads the registry and nothing else.  It used to sweep it first, which
was free while a host check was `buffer-live-p' and is not once a host
check can run a subprocess -- redisplay is no place for I/O, and no
place to discover that the I/O failed either.  The poll timer already
sweeps every `ghostherd-poll-interval'."
  (let* ((sessions (hash-table-values ghostherd--sessions))
         (blocked (cl-count-if (lambda (s) (eq (ghostherd-session-state s) 'blocked))
                               sessions))
         (working (cl-count-if (lambda (s) (eq (ghostherd-session-state s) 'working))
                               sessions))
         (done (cl-count-if (lambda (s) (eq (ghostherd-session-state s) 'done))
                            sessions))
         (total (length sessions)))
    (when (> total 0)
      (propertize
       (format " 🐄%d%s%s%s"
               total
               (if (> blocked 0) (format " ⚠%d" blocked) "")
               (if (> working 0) (format " ⟳%d" working) "")
               (if (> done 0) (format " ✓%d" done) ""))
       'face (cond ((> blocked 0) 'error)
                   ((> done 0) 'success)
                   ((> working 0) 'warning)
                   (t 'shadow))
       'help-echo "ghostherd sessions (click for sidebar)"
       'mouse-face 'mode-line-highlight
       'local-map (define-keymap
                    "<mode-line> <mouse-1>" #'ghostherd-sidebar)))))

;;;###autoload
(define-minor-mode ghostherd-mode
  "Global minor mode for ghostherd modeline indicator and hooks."
  :global t
  :group 'ghostherd
  (if ghostherd-mode
      (progn
        (ghostherd--install-hooks)
        (ghostherd--register-eval-cmds)
        ;; Enabling the mode is the restore hook: a herd left running by
        ;; a previous Emacs is picked up here, before anything asks for
        ;; a session list.
        (ignore-errors (ghostherd-restore))
        (ghostherd--ensure-poll-timer)
        (unless (member '(:eval (ghostherd--mode-line-segment)) mode-line-misc-info)
          (setq mode-line-misc-info
                (append mode-line-misc-info
                        '((:eval (ghostherd--mode-line-segment)))))))
    (ghostherd--remove-hooks)
    (when (timerp ghostherd--poll-timer)
      (cancel-timer ghostherd--poll-timer)
      (setq ghostherd--poll-timer nil))
    (setq mode-line-misc-info
          (cl-remove '(:eval (ghostherd--mode-line-segment))
                     mode-line-misc-info
                     :test #'equal))))


;;; Transient-ish quick menu (no transient dependency)

;;;###autoload
(defun ghostherd-menu ()
  "Simple command dispatcher for ghostherd."
  (interactive)
  (let* ((choices
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
            ("g" "poll now" ghostherd-poll-all)))
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

(provide 'ghostherd)
;;; ghostherd.el ends here

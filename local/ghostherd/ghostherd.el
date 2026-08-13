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

(declare-function ghostel "ghostel" (&optional arg))
(declare-function ghostel-send-string "ghostel" (string))
(declare-function ghostel-paste-string "ghostel" (string))
(declare-function ghostel-send-key "ghostel" (key-name &optional mods))
(declare-function alert "alert" (message &rest kwargs))

;; Internal ghostel buffer-locals (not public API, but stable enough).
(defvar ghostel--process)
(defvar ghostel--buffer-identity)
(defvar ghostel--title)
(defvar ghostel-progress-function)
(defvar ghostel-kill-buffer-on-exit)
(defvar ghostel-query-before-killing)
(defvar ghostel-buffer-name)
(defvar ghostel-eval-cmds)
(defvar ghostel-exit-functions)
(defvar ghostel-command-start-functions)
(defvar ghostel-command-finish-functions)


;;; Customization

(defgroup ghostherd nil
  "Manage AI agent CLIs running in ghostel terminals."
  :group 'tools
  :prefix "ghostherd-")

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

(defcustom ghostherd-spawn-delay 0.8
  "Seconds to wait for the shell prompt before launching the agent command.
Only used when the session is started via shell + command (not direct exec)."
  :type 'number
  :group 'ghostherd)

(defcustom ghostherd-poll-interval 1.5
  "Seconds between automatic state polls for live sessions."
  :type 'number
  :group 'ghostherd)

(defcustom ghostherd-screen-tail-lines 40
  "Number of buffer lines from the bottom used for screen-state detection."
  :type 'integer
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

(defcustom ghostherd-buffer-name-format "*ghostherd:%s*"
  "Format string for session buffer names.  %s is the session name."
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

(cl-defstruct (ghostherd-session
               (:constructor ghostherd-session--create)
               (:copier nil))
  id
  name
  kind
  buffer
  project
  command
  args
  (state 'starting)
  state-reason
  (started-at (current-time))
  last-active
  (seen t)
  (manual-state nil)
  notes
  ;; Last ConEmu OSC 9;4 report: the STATE symbol, its percentage (or nil),
  ;; and when it arrived.  Written on ghostel's VT-parser callpath, so
  ;; nothing here may be expensive to set; it is *read* by the poll path.
  progress-state
  progress-percent
  progress-at)

(defvar ghostherd--sessions (make-hash-table :test 'equal)
  "Map of session id string → `ghostherd-session'.")

(defvar ghostherd--counter 0
  "Monotonic counter for generated session names.")

(defvar ghostherd--poll-timer nil
  "Idle timer that polls session states.")

(defvar-local ghostherd-session-id nil
  "Session id of the ghostherd session hosted in this buffer, if any.")

(defvar ghostherd-state-change-hook nil
  "Hook run after a session state changes.
Each function is called with (SESSION OLD-STATE NEW-STATE).")

(defvar ghostherd-session-created-hook nil
  "Hook run after a session is registered.  Arg: SESSION.")

(defvar ghostherd-session-removed-hook nil
  "Hook run after a session is removed.  Arg: SESSION.")


;;; Helpers

(defun ghostherd--require-ghostel ()
  "Load ghostel or signal an error."
  (unless (require 'ghostel nil t)
    (user-error "ghostherd requires the ghostel package")))

(defun ghostherd--agent-kinds ()
  "Return the list of known agent kind symbols."
  (mapcar #'car ghostherd-agent-specs))

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
  "Return SESSION's OSC 2 terminal title, or nil when it says nothing new.
Read straight off ghostel's buffer-local `ghostel--title' rather than
through `ghostel-buffer-name-function', which is how ghostel turns a
title into a buffer name -- taking that over would fight ghostherd's own
naming.  A title equal to the buffer name is the shell echoing us back."
  (when-let* ((buf (ghostherd-session-buffer session)))
    (when (and (buffer-live-p buf) (boundp 'ghostel--title))
      (let ((title (buffer-local-value 'ghostel--title buf)))
        (when (stringp title)
          (let ((title (string-trim title)))
            (unless (or (string-empty-p title)
                        (equal title (buffer-name buf)))
              title)))))))

(defun ghostherd--unique-name (base)
  "Return a unique session name derived from BASE."
  (let ((name base)
        (n 1))
    (while (ghostherd-get name)
      (setq n (1+ n)
            name (format "%s-%d" base n)))
    name))

(defun ghostherd--buffer-name (name)
  "Return buffer name for session NAME."
  (format ghostherd-buffer-name-format name))

(defun ghostherd--session-live-p (session)
  "Return non-nil if SESSION's buffer is live."
  (and session (buffer-live-p (ghostherd-session-buffer session))))

(defun ghostherd-get (id-or-name)
  "Return session for ID-OR-NAME, or nil.
Accepts session id, name, or a live buffer that hosts a session."
  (cond
   ((ghostherd-session-p id-or-name) id-or-name)
   ((bufferp id-or-name)
    (when-let* ((id (buffer-local-value 'ghostherd-session-id id-or-name)))
      (gethash id ghostherd--sessions)))
   ((stringp id-or-name)
    (or (gethash id-or-name ghostherd--sessions)
        (cl-find id-or-name (hash-table-values ghostherd--sessions)
                 :key #'ghostherd-session-name
                 :test #'equal)))
   (t nil)))

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
  "Drop sessions whose buffers are gone."
  (maphash
   (lambda (id session)
     (unless (ghostherd--session-live-p session)
       (when (not (eq (ghostherd-session-state session) 'dead))
         (setf (ghostherd-session-state session) 'dead
               (ghostherd-session-state-reason session) "buffer gone"))
       ;; Keep dead sessions briefly so the sidebar can show them; prune
       ;; only when buffer object is fully gone and already dead.
       (unless (buffer-live-p (ghostherd-session-buffer session))
         (remhash id ghostherd--sessions)
         (run-hook-with-args 'ghostherd-session-removed-hook session))))
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

(defun ghostherd--buffer-tail (buffer &optional n)
  "Return the last N lines of BUFFER as a single string."
  (with-current-buffer buffer
    (let* ((n (or n ghostherd-screen-tail-lines))
           (end (point-max))
           (start (save-excursion
                    (goto-char end)
                    (forward-line (- n))
                    (point))))
      (buffer-substring-no-properties start end))))

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
    (cons 'dead "buffer dead"))
   (t
    (let* ((buf (ghostherd-session-buffer session))
           (proc (buffer-local-value 'ghostel--process buf))
           (kind (ghostherd-session-kind session))
           (spec (ignore-errors (ghostherd--spec kind)))
           (rules (plist-get spec :screen-rules)))
      (cond
       ((and proc (not (process-live-p proc)))
        (cons 'dead "process exited"))
       ((eq kind 'shell)
        (cons 'idle "shell"))
       ((null rules)
        (cons 'working "no rules"))
       (t
        (let* ((tail (ghostherd--buffer-tail buf))
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
  "Mark herd session dead when ghostel process in BUFFER exits.
EVENT is the sentinel event string from ghostel."
  (when-let* ((session (ghostherd-get buffer)))
    (ghostherd--set-state session 'dead (string-trim (or event "exited")))
    (ghostherd--stop-poll-timer)))

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

(defun ghostherd--shell-quote-args (args)
  "Shell-quote ARGS list into a single string."
  (mapconcat #'shell-quote-argument args " "))

(defun ghostherd--build-launch-string (_kind command args)
  "Build the shell command line for COMMAND and ARGS.
_KIND is reserved for kind-specific quoting later."
  (when command
    (string-trim
     (concat command
             (when args
               (concat " " (ghostherd--shell-quote-args args)))))))

(defun ghostherd--launch-in-buffer (buffer launch)
  "Send LAUNCH command + RET in ghostel BUFFER after a short delay."
  (when (and launch (not (string-empty-p launch)))
    (run-at-time
     ghostherd-spawn-delay nil
     (lambda ()
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (derived-mode-p 'ghostel-mode)
             (ghostel-send-string (concat launch "\n")))))))))

(defun ghostherd-spawn (kind &rest plist)
  "Spawn an agent of KIND and return the `ghostherd-session'.

PLIST keys:
  :name      session name (unique)
  :project   project root directory
  :command   override executable
  :args      override argument list
  :notes     free-form note / role description
  :directory working directory (defaults to project or `default-directory')
  :display   when non-nil (default t), pop to the buffer"
  (ghostherd--require-ghostel)
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
         (display (if (plist-member plist :display)
                      (plist-get plist :display)
                    t))
         (bufname (ghostherd--buffer-name name))
         (launch (ghostherd--build-launch-string kind command args))
         buffer
         session)
    (when (get-buffer bufname)
      (user-error "Buffer already exists: %s" bufname))
    (let ((default-directory directory)
          (ghostel-buffer-name bufname)
          ;; Keep exited agent buffers so the herd can mark them dead.
          (ghostel-kill-buffer-on-exit nil))
      (setq buffer (ghostel t))
      ;; `ghostel' may reuse/rename; force our identity/name.
      (with-current-buffer buffer
        (rename-buffer bufname t)
        (setq-local ghostel-kill-buffer-on-exit nil)
        (setq-local ghostherd-session-id name)
        (setq-local ghostel--buffer-identity bufname)
        (ghostherd--launch-in-buffer buffer launch)))
    (setq session
          (ghostherd-session--create
           :id name
           :name name
           :kind kind
           :buffer buffer
           :project (and project (expand-file-name project))
           :command command
           :args args
           :state (if launch 'starting 'idle)
           :state-reason (if launch "spawned" "shell")
           :notes notes
           :last-active (current-time)))
    (puthash name session ghostherd--sessions)
    (ghostherd--ensure-poll-timer)
    (run-hook-with-args 'ghostherd-session-created-hook session)
    (when display
      (pop-to-buffer buffer))
    (ghostherd--sidebar-refresh)
    session))

(defun ghostherd-session-recipe (session)
  "Return the plist `ghostherd-spawn' needs to recreate SESSION.

This is the whole of what can be persisted.  Agents are Emacs child
processes, so nothing survives Emacs itself -- the conversation lives in
the CLI's own store, which is what `:continue-args' reaches."
  (list :name (ghostherd-session-name session)
        :project (ghostherd-session-project session)
        :directory (ghostherd-session-project session)
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
  (let* ((kind (or kind
                   (intern
                    (completing-read
                     "Agent kind: "
                     (mapcar #'symbol-name (ghostherd--agent-kinds))
                     nil t nil nil
                     (symbol-name ghostherd-default-kind)))))
         (spec (ghostherd--spec kind))
         (default-name (ghostherd--unique-name (symbol-name kind)))
         (name (read-string "Session name: " default-name))
         (project (ghostherd--project-root))
         (directory (if project
                        (read-directory-name "Directory: " project nil t)
                      (read-directory-name "Directory: " default-directory nil t)))
         (arg-string (read-string
                      (format "Extra args for %s (optional): "
                              (or (plist-get spec :command) "shell"))
                      (when-let* ((a (plist-get spec :args)))
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
    (delete-other-windows)
    (switch-to-buffer (ghostherd-session-buffer left))
    (split-window-right)
    (other-window 1)
    (switch-to-buffer (ghostherd-session-buffer right))
    (message "Spawned %s (%s) | %s (%s)"
             left-name left-kind right-name right-kind)
    (list left right)))

(defun ghostherd--read-session (&optional prompt predicate)
  "Read a session via completing-read using PROMPT and optional PREDICATE."
  (ghostherd--ensure-sessions)
  (let* ((sessions (cl-remove-if-not
                    (or predicate #'identity)
                    (ghostherd-sessions)))
         (candidates
          (mapcar (lambda (s)
                    (cons (ghostherd--format-candidate s) s))
                  sessions)))
    (unless candidates
      (user-error "No ghostherd sessions"))
    (let ((choice (completing-read (or prompt "Session: ")
                                   candidates nil t)))
      (or (alist-get choice candidates nil nil #'equal)
          (ghostherd-get choice)
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
         (tail (and live (ghostherd--buffer-tail
                          (ghostherd-session-buffer session))))
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
        (insert (format "  buffer   %s%s\n\n"
                        (buffer-name (ghostherd-session-buffer session))
                        (if live "" "  [dead]")))

        (cond
         ((not live)
          (insert "Buffer is dead; no detection runs.\n"))
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
  (let ((session (ghostherd--read-session "Switch to agent: ")))
    (setf (ghostherd-session-seen session) t)
    (pop-to-buffer (ghostherd-session-buffer session))))

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
      (setf (ghostherd-session-seen session) t)
      (pop-to-buffer (ghostherd-session-buffer session))
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
    (when (and kill-buffer (buffer-live-p buf))
      (let ((kill-buffer-query-functions nil)
            (ghostel-query-before-killing nil))
        (ignore-errors (kill-buffer buf))))
    (ghostherd--stop-poll-timer)
    (ghostherd--sidebar-refresh)
    (message "Killed session %s" (ghostherd-session-name session))))

;;;###autoload
(defun ghostherd-rename (session new-name)
  "Rename SESSION to NEW-NAME."
  (interactive
   (let* ((s (ghostherd--read-session "Rename session: "))
          (n (read-string "New name: " (ghostherd-session-name s))))
     (list s n)))
  (setq session (ghostherd-get session))
  (when (ghostherd-get new-name)
    (user-error "Name already in use: %s" new-name))
  (let ((old-id (ghostherd-session-id session))
        (buf (ghostherd-session-buffer session)))
    (remhash old-id ghostherd--sessions)
    (setf (ghostherd-session-id session) new-name
          (ghostherd-session-name session) new-name)
    (puthash new-name session ghostherd--sessions)
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (rename-buffer (ghostherd--buffer-name new-name) t)
        (setq-local ghostherd-session-id new-name)
        (setq-local ghostel--buffer-identity (buffer-name))))
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
    (user-error "Session buffer is not live"))
  (with-current-buffer (ghostherd-session-buffer session)
    (unless (derived-mode-p 'ghostel-mode)
      (user-error "Session buffer is not a ghostel terminal"))
    ;; Prefer bracketed paste for multi-line / long prompts.
    (if (fboundp 'ghostel-paste-string)
        (ghostel-paste-string text)
      (ghostel-send-string text))
    (when submit
      (ghostel-send-key "return")))
  (unless (ghostherd-session-manual-state session)
    (ghostherd--set-state session 'working "input sent"))
  text)

(defun ghostherd-prompt (session prompt &optional wait timeout)
  "Submit PROMPT to SESSION (text + Enter).
When WAIT is non-nil, poll until state is idle/done/blocked or TIMEOUT
seconds elapse (default 120)."
  (ghostherd-send session prompt t)
  (when wait
    (ghostherd-wait session '(idle done blocked) (or timeout 120)))
  session)

(defun ghostherd-read (session &optional n-lines)
  "Return the last N-LINES of SESSION's terminal buffer."
  (setq session (ghostherd-get session))
  (unless (ghostherd--session-live-p session)
    (user-error "Session buffer is not live"))
  (ghostherd--buffer-tail (ghostherd-session-buffer session)
                          (or n-lines ghostherd-screen-tail-lines)))

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
      (sit-for 0.4))
    (unless (memq state states)
      (user-error "Timeout waiting for %s to become %s (was %s)"
                  (ghostherd-session-name session) states state))
    state))

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

(defun ghostherd-cmd-list (&rest _)
  "Return a text listing of herd sessions for agent shells."
  (ghostherd--ensure-sessions)
  (if (= (hash-table-count ghostherd--sessions) 0)
      "(no sessions)"
    (mapconcat
     (lambda (s)
       (format "%s\tkind=%s\tstate=%s\tproject=%s\tnotes=%s"
               (ghostherd-session-name s)
               (ghostherd-session-kind s)
               (ghostherd-session-state s)
               (or (ghostherd-session-project s) "-")
               (or (ghostherd-session-notes s) "")))
     (ghostherd-sessions)
     "\n")))

(defun ghostherd-cmd-state (name &rest _)
  "Return state string for session NAME."
  (if-let* ((s (ghostherd-get name)))
      (format "%s %s %s"
              (ghostherd-session-name s)
              (ghostherd-session-state s)
              (or (ghostherd-session-state-reason s) ""))
    (format "unknown session: %s" name)))

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

(defun ghostherd--format-candidate (session)
  "Format SESSION as a completing-read candidate string."
  (format "%s  %s  %s  %s"
          (propertize (ghostherd--state-glyph (ghostherd-session-state session))
                      'face (ghostherd--state-face
                             (ghostherd-session-state session)))
          (ghostherd-session-name session)
          (ghostherd-session-kind session)
          (ghostherd--abbreviate (ghostherd-session-project session))))

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
          (insert (format line-format keys description)))))))

(defvar-keymap ghostherd-sidebar-mode-map
  :doc "Keymap for `ghostherd-sidebar-mode'."
  "?" #'ghostherd-sidebar-help
  "e" #'ghostherd-explain
  "R" #'ghostherd-respawn
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
    ('glyph   (propertize (ghostherd--state-glyph state) 'face face))
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
  "Visit the session at point."
  (interactive)
  (when-let* ((s (ghostherd--sidebar-session-at-point)))
    (setf (ghostherd-session-seen s) t)
    (pop-to-buffer (ghostherd-session-buffer s))))

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
  "Mode-line segment summarizing herd attention."
  (ghostherd--ensure-sessions)
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

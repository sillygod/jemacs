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


;;; Customization

(defconst ghostherd--rule-permission-prompt
  "\\([^[:alnum:]_]\\|^\\)permission\\([^[:alnum:]_-]\\|$\\)"
  "A `blocked' pattern for a permission *prompt*.

The old substring `permission' matched `permissions' in a JSON allow
list and `permission-mode' in grok's idle banner -- both of which sit
on screen while the agent is parked at a prompt.  Blocked outranks
idle, so the row stayed ⚠.  Character classes rather than `\\\\<' `\\\\>'
because those consult the current buffer's syntax table, the same
class of ambient that `case-fold-search' used to be.")

(defcustom ghostherd-agent-specs
  `((claude
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
                  ,ghostherd--rule-permission-prompt
                  "Yes, and don't ask again"
                  "❯ 1\\. Yes"))
      (working . ("esc to interrupt"
                  "Baking…"
                  "Baking..."
                  "Thinking"
                  "⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"))
      ;; An *empty* prompt.  A scrolling agent keeps every line you ever
      ;; typed, so "^> " matches the transcript rather than the cursor --
      ;; see the note under :screen-rules below.
      (idle . ("^❯ *$"
               "^> *$"))))
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
                  ,ghostherd--rule-permission-prompt
                  "\\[y/N\\]"
                  "\\(y/n\\)"))
      (working . ("Working"
                  "Thinking"
                  "Running"
                  "esc to interrupt"
                  "ctrl\\+c to interrupt"
                  "⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"))
      (idle . ("^› *$"
               "^❯ *$"
               "^> *$"))))
    (agy
     :command "agy"
     :args nil
     :description "agy CLI"
     :process-names ("agy")
     :continue-args ("--continue")
     :screen-rules
     ((blocked . ("Do you want to proceed"
                  "Allow this"
                  ,ghostherd--rule-permission-prompt
                  "Approve"
                  "\\[y/N\\]"
                  "\\(y/n\\)"))
      (working . ("Working"
                  "Thinking"
                  "Running"
                  "esc to interrupt"
                  "⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"))
      (idle . ("^❯ *$"
               "^> *$"
               "^› *$"))))
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

An `idle' rule must match an *empty* prompt: ^> *$ rather than ^> with
anything allowed after it.  These agents scroll rather than repaint, so
every line you have ever typed stays on screen behind its prompt glyph,
and a rule that matches those reports `idle' from the moment you first
press Return, forever.  Tightening costs nothing: a prompt the rules
fail to recognise falls through to the idle default anyway.

Three rules were dropped for the same reason -- claude's separator line,
grok's startup banner and grok's permission-mode banner are part of the
frame, not of the state.  The first two are on screen while the agent
works, so they said `idle' throughout.  The third was worse: grok prints
its mode in the border of the input box, so `always-approve' -- the mode
in which it will *not* ask -- read as `blocked', which outranks
`working', for as long as that mode was on.

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
  "Side window side for `ghostherd-sidebar' when not using posframe."
  :type '(choice (const left) (const right))
  :group 'ghostherd)

(defcustom ghostherd-sidebar-width 36
  "Width of the ghostherd side window.

Used only when the session list is shown as a side window.  The
posframe overlay sizes itself from the parent frame (see
`ghostherd-sidebar-posframe-width-ratio'), so a 36-column dashboard
does not constrain a centred panel."
  :type 'integer
  :group 'ghostherd)

(defcustom ghostherd-sidebar-use-posframe t
  "Show the session list in a posframe overlay when possible.

When non-nil, `ghostherd-sidebar' uses posframe if the library can
be loaded and the display can host a child frame.  Otherwise it
falls back to a side window -- the original display, and the one
that still works on a tty or in batch.

Nil forces the side window even when posframe is available."
  :type 'boolean
  :group 'ghostherd)

(defcustom ghostherd-sidebar-show-preview t
  "Show a snapshot of the session at point under the session list.

The snapshot is `capture-pane' / buffer text, not an attached view.
`v' in the list toggles this."
  :type 'boolean
  :group 'ghostherd)

(defcustom ghostherd-sidebar-preview-lines 16
  "Lines of captured screen shown in the overlay preview."
  :type 'integer
  :group 'ghostherd)

(defcustom ghostherd-sidebar-posframe-width 72
  "Minimum character width of the session-list posframe overlay.

The overlay grows with the parent frame (see
`ghostherd-sidebar-posframe-width-ratio'); this is the floor, so a
narrow Emacs still gets Kind and Project.  Nil for the ratio
makes this a fixed width instead."
  :type 'integer
  :group 'ghostherd)

(defcustom ghostherd-sidebar-posframe-width-ratio 0.6
  "Fraction of the parent frame's width used by the overlay.

Nil means a fixed `ghostherd-sidebar-posframe-width' instead of
scaling.  The result is clamped between that width and the parent
minus a small margin."
  :type '(choice (const :tag "Fixed width" nil)
                 (number :tag "Fraction of frame"))
  :group 'ghostherd)

(defcustom ghostherd-sidebar-show-title nil
  "Offer the agent's OSC 2 terminal title as a sidebar column.

The column still has to earn its place: columns are fitted to the
sidebar's width (see `ghostherd--sidebar-column-specs'), and Title ranks
above Project, so in a narrow sidebar enabling this trades the project
path for the title.  Nothing overflows either way."
  :type 'boolean
  :group 'ghostherd)

(defcustom ghostherd-detached-glyph "▪"
  "Marker appended to the sidebar's state glyph when nothing is attached.

Rides in the spare character of the two-wide glyph column rather than
taking a column of its own: at the default sidebar width there is no
room for another column, so a `View' one would be fitted away exactly
where it is needed.

Marks the *detached* rows rather than the attached ones, which keeps it
silent on the ghostel backend -- there a registered session always has
its buffer, so nothing is ever marked.

Was a shadowed middle dot, on the theory that a detached agent is the
normal case on the tmux backend and should not shout.  It undershot:
one dim pixel next to a glyph is not a signal, and information nobody
notices is information that is not there.  Something with ink, in the
row's own colour."
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


;;; Herd log
;;
;; Notifications are the wrong medium for a herd that runs while you are
;; elsewhere: they arrive once, in the order they happened, and are gone.
;; The log is what you read when you come back -- and, because it keeps
;; the screen behind each `blocked', it is also how `:screen-rules' get
;; tuned without anyone having to be present at the moment a rule fired.

(defcustom ghostherd-log-max 500
  "Entries kept in the herd log.  Oldest are dropped first."
  :type 'integer
  :group 'ghostherd)

(defcustom ghostherd-log-screens 20
  "How many captured screens the log keeps alongside `blocked' entries.

The point of keeping them: rule tuning needs the screen a rule fired
against, and =ghostherd-explain= only offers that while the agent is
still sitting on the prompt.  Storing it means the data accumulates
whether or not you were watching.

Only transitions into `blocked' are captured -- they are rare, and they
are the ones worth arguing about.  0 disables it."
  :type 'integer
  :group 'ghostherd)

(cl-defstruct (ghostherd-log-entry (:constructor ghostherd-log-entry--create)
                                   (:copier nil))
  (time (current-time))
  session
  kind
  text
  screen)

(defcustom ghostherd-log-file (locate-user-emacs-file "ghostherd-log.eld")
  "File the herd log is appended to, or nil to keep it in memory only.

On by default, because the restart this is for is the one nobody
planned.  Phase 3 made agents outlive Emacs -- that is what the tmux
backend is *for* -- and the log describes itself as \"what you read when
you get back\", where the most ordinary way of getting back is starting
Emacs again.  A log that only survives while Emacs does answers every
absence except that one.

The screens matter more than the lines here.  `ghostherd-explain' can
only say why an agent is in a state while it is still in it, so a rule
that fired at 02:00 is unarguable by morning unless the screen behind it
was kept -- and \"by morning\" is exactly the span an Emacs restart
tends to fall inside."
  :type '(choice (const :tag "Memory only" nil) file)
  :group 'ghostherd)

(defvar ghostherd--log nil
  "Herd log entries, newest first.")

(defvar ghostherd--log-loaded nil
  "Non-nil once `ghostherd-log-file' has been read this session.")

(defvar ghostherd--log-writable t
  "Non-nil while appending to `ghostherd-log-file' is still worth trying.

Cleared after one failure, because the write happens per state
transition: a read-only directory would otherwise produce an error
message every time an agent changed state.")

(defun ghostherd--log-form (entry)
  "Return ENTRY as a plist ready to be printed on one line.

A plist rather than the struct itself, deliberately.  Printing
`#s(ghostherd-log-entry ...)' reads back only into the struct as it was
*then*: add a slot and every line written before today comes back with
its fields shifted along, which is the same trap as \"Reloading over a
live herd\" in readme.org and just as remote from its cause.  A plist
tolerates a key that did not exist yet and one that no longer does."
  (append (list :time (ghostherd-log-entry-time entry)
                :session (ghostherd-log-entry-session entry)
                :kind (ghostherd-log-entry-kind entry)
                :text (ghostherd-log-entry-text entry))
          (when-let* ((screen (ghostherd-log-entry-screen entry)))
            (list :screen screen))))

(defun ghostherd--log-entry-from-form (form)
  "Rebuild a log entry from FORM, or nil when FORM is not one."
  (when (and (consp form) (plist-member form :time))
    (ghostherd-log-entry--create
     :time (plist-get form :time)
     :session (plist-get form :session)
     :kind (plist-get form :kind)
     :text (plist-get form :text)
     :screen (plist-get form :screen))))

(defun ghostherd--log-print (entries)
  "Return ENTRIES, oldest first, as one printed form per line.

One line per entry is what makes appending cheap and a crash survivable:
a half-written last line is the only damage possible, and the loader
stops there.  Three print settings are load-bearing --
`print-escape-newlines' because a captured screen is full of them and
would otherwise span lines, and `print-length' / `print-level' because a
user who set them would get \"...\" written into their own log."
  (let ((print-escape-newlines t)
        (print-length nil)
        (print-level nil))
    (mapconcat (lambda (entry)
                 (concat (prin1-to-string (ghostherd--log-form entry)) "\n"))
               entries "")))

(defun ghostherd--log-write (text append)
  "Write TEXT to `ghostherd-log-file', appending when APPEND is non-nil."
  (when (and ghostherd-log-file ghostherd--log-writable)
    (condition-case error
        (let ((coding-system-for-write 'utf-8))
          ;; Explicit UTF-8, and not decorative: a screen is box drawing and
          ;; a reason can be a prompt in any language, and `write-region'
          ;; without one calls `select-safe-coding-system', which *prompts*
          ;; -- an error in batch, a wedged daemon otherwise.
          (write-region text nil ghostherd-log-file append 'silent))
      (error
       (setq ghostherd--log-writable nil)
       (display-warning
        'ghostherd
        (format "cannot write %s, keeping the herd log in memory only: %s"
                ghostherd-log-file (error-message-string error))
        :warning)))))

(defun ghostherd-log-load ()
  "Read `ghostherd-log-file' into the herd log and compact the file.

Compaction happens here rather than on the way in: appending is what
keeps logging cheap, so the file grows with whatever the session did, and
the caps (`ghostherd-log-max', `ghostherd-log-screens') are applied once,
when something is reading anyway.  A line that will not parse ends the
read -- the expected corruption is a truncated tail from a crash, and
what follows one is not worth guessing at."
  (interactive)
  (setq ghostherd--log-loaded t)
  (when (and ghostherd-log-file (file-readable-p ghostherd-log-file))
    (let ((entries nil))
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8))
          (insert-file-contents ghostherd-log-file))
        (goto-char (point-min))
        (condition-case nil
            (while t
              (if-let* ((entry (ghostherd--log-entry-from-form (read (current-buffer)))))
                  (push entry entries)
                ;; A form that is not an entry: skip it rather than stop,
                ;; since two Emacsen appending to one file can interleave.
                nil))
          (error nil)))
      ;; `entries' is newest first already, which is how the log is kept.
      (setq ghostherd--log (ghostherd--log-trim entries))
      (ghostherd--log-write (ghostherd--log-print (reverse ghostherd--log)) nil)
      (let ((read (length ghostherd--log)))
        (when (> read 0)
          ;; Through `--log-add', so the seam is a line of the log like any
          ;; other: trimmed with the rest, and there again next time.
          (ghostherd--log-add "herd" 'life
                              (format "resumed %d entries from an earlier Emacs"
                                      read)))
        read))))

(defun ghostherd--log-trim (entries)
  "Return ENTRIES (newest first) within both log caps.

Entries and screens are capped separately because they cost differently:
500 one-line entries are nothing, and twenty 40x120 screens are the
actual memory -- and now the actual disk."
  (let ((kept (if (> (length entries) ghostherd-log-max)
                  (seq-take entries ghostherd-log-max)
                entries))
        (screens 0))
    (dolist (entry kept)
      (when (ghostherd-log-entry-screen entry)
        (setq screens (1+ screens))
        (when (> screens ghostherd-log-screens)
          (setf (ghostherd-log-entry-screen entry) nil))))
    kept))

;;;###autoload
(defun ghostherd-log-clear ()
  "Forget the herd log, on disk as well as in memory.

Quitting Emacs used to do this by accident; now that it does not,
something has to do it on purpose."
  (interactive)
  (when (or (not (called-interactively-p 'interactive))
            (yes-or-no-p "Forget the herd log, including the kept screens? "))
    (setq ghostherd--log nil)
    (when (and ghostherd-log-file (file-exists-p ghostherd-log-file))
      (ignore-errors (delete-file ghostherd-log-file)))
    (when-let* ((buf (get-buffer "*ghostherd-log*")))
      (ghostherd--log-render buf))
    (message "Herd log cleared")))

(defun ghostherd--log-add (session kind text &optional screen)
  "Record TEXT about SESSION under KIND, optionally with a SCREEN."
  (push (ghostherd-log-entry--create
         :session (if (ghostherd-session-p session)
                      (ghostherd-session-name session)
                    (format "%s" session))
         :kind kind :text text :screen screen)
        ghostherd--log)
  ;; Appended as it happens rather than saved on the way out: a
  ;; `kill-emacs-hook' does not run for the crash this is meant to
  ;; survive, and one line is cheap enough to write per transition.
  (ghostherd--log-write (ghostherd--log-print (list (car ghostherd--log))) t)
  (setq ghostherd--log (ghostherd--log-trim ghostherd--log))
  (when-let* ((buf (get-buffer "*ghostherd-log*")))
    (when (get-buffer-window buf t)
      (ghostherd--log-render buf)))
  nil)

(defun ghostherd--log-transition (session old new reason)
  "Log SESSION moving from OLD to NEW because of REASON."
  (ghostherd--log-add
   session 'state
   (format "%s → %s%s" (or old "?") new (if reason (format "  %s" reason) ""))
   ;; One extra capture, only on the transition worth arguing about.  It
   ;; is not free -- on the tmux backend it is a subprocess -- but an
   ;; agent blocks once per prompt, not once per poll.
   (when (and (eq new 'blocked) (> ghostherd-log-screens 0))
     (ignore-errors (ghostherd--host-capture session)))))


;;; What the agent says about itself
;;
;; The third state authority, after screen rules and the ghostel/OSC
;; hooks.  Rules scrape a TUI that is redrawn differently every release,
;; which is why tuning them is a permanent job; but the CLIs already know
;; when they stop for a question and when they finish, and most of them
;; can run a command at that moment.  A report is that command arriving:
;;
;;   ghostherd report self blocked "Bash(rm -rf build/) — proceed?"
;;
;; It is deliberately *not* `ghostherd-mark-state'.  A manual mark is
;; sticky, which is right for a human overriding detection and wrong for
;; a hook: one missed edge and the row is frozen at `blocked' forever.  A
;; report expires instead, the way an OSC progress report does -- the
;; pattern this is modelled on, down to the freshness check.

(defcustom ghostherd-report-ttl 120
  "Seconds an agent's own report about itself stays trusted.

Short, because expiry is nearly free when the report was *true*: a real
prompt is still on the screen, the rules find it, and the herd falls
back to exactly the behaviour it had before reports existed.  Expiry is
also the only cure when the report was *stale* -- a hook chain that
broke halfway through a session, or a CLI that fires nothing on the way
out of a prompt -- so a long TTL buys a lie and sells nothing.

Long enough, meanwhile, that a prompt nobody answers for a minute is
still described by the agent's own words rather than by whichever
regexp happened to match the box it was drawn in."
  :type 'number
  :group 'ghostherd)

(defconst ghostherd-report-states '(working blocked idle done)
  "States an agent may claim for itself.

`dead' is not among them: liveness is the host's answer, and an agent
that could still report is by definition not dead.")

(defvar ghostherd--reports (make-hash-table :test 'equal)
  "Session id → (STATE REASON TIME), the last thing an agent said about itself.

A hash table rather than two more `ghostherd-session' slots, for the
reason under \"Reloading over a live herd\" in readme.org: struct slots
cannot be added under a running herd without shifting every field after
them.  This is Emacs-side, transient and not worth that risk -- and it
is the same shape as `ghostherd--input-at' and `ghostherd--idle-since',
which are cleared in the same places.")

(defvar ghostherd--screens (make-hash-table :test 'equal)
  "Session id → last captured screen.

Filled by the poll path (and any other capture) so the overlay can
preview without a second round trip, and without a new struct slot.")

;;;###autoload
(defun ghostherd-report (session state &optional reason)
  "Record that SESSION says it is in STATE, because REASON.

STATE is one of `ghostherd-report-states', or `auto' to withdraw the
report and go back to detection alone.

This only records; the poll path decides what it means, the same
division of labour as `ghostherd--on-progress'.  The poll it schedules
is a timer rather than a direct call because the shortest way here is
`ghostel_cmd', which is dispatched inside ghostel's VT parser -- where a
capture is a subprocess in the middle of drawing a terminal."
  (interactive
   (list (ghostherd--read-session "Session: ")
         (intern (completing-read "Reports itself: "
                                  (cons "auto" (mapcar #'symbol-name
                                                       ghostherd-report-states))
                                  nil t))))
  (setq session (ghostherd-get session))
  (unless session
    (user-error "No such session"))
  (unless (or (eq state 'auto) (memq state ghostherd-report-states))
    (user-error "Cannot report state %s (one of %s, or auto)"
                state ghostherd-report-states))
  (let ((id (ghostherd-session-id session)))
    (if (eq state 'auto)
        (progn (remhash id ghostherd--reports)
               (ghostherd--log-add session 'report "withdrew its own report"))
      (puthash id (list state reason (current-time)) ghostherd--reports)
      (ghostherd--log-add session 'report
                          (format "reports %s%s" state
                                  (if reason (format "  %s" reason) ""))))
    (run-with-timer 0 nil #'ghostherd-poll-session id)
    state))

(defun ghostherd--fresh-report (session)
  "Return SESSION's own last report as (STATE . REASON) while it is trusted."
  (when-let* ((entry (gethash (ghostherd-session-id session) ghostherd--reports)))
    (pcase-let ((`(,state ,reason ,at) entry))
      (when (< (float-time (time-subtract (current-time) at))
               ghostherd-report-ttl)
        (cons state (or reason (format "reported %s" state)))))))


;;; State detection

(defun ghostherd--rule-matches-p (pattern text)
  "Return non-nil when screen-rule PATTERN matches TEXT.

Case-sensitively, and that is the whole reason this exists rather than
a bare `string-match-p'.  Rules used to match under whatever
`case-fold-search' happened to be when the poll timer fired -- the
buffer that was current, in other words, which no rule author chooses
or can even see.  The same pattern could therefore mean two different
things on two ticks.

nil rather than t because the patterns were written by reading an
agent's screen and copying what was on it: case-sensitive *is* the
authored intent, and it matches `ghostherd-output-matches', which has
always bound it this way.  It is also the safer direction under the
project's standing policy of preferring a missed `blocked' to a false
one -- folding made `Running' match the word \"running\" in ordinary
agent prose, and `permission' match an agent merely talking about
permissions.

A rule that genuinely wants both cases says so: \\=[Pp]ermission."
  (let ((case-fold-search nil))
    (string-match-p pattern text)))

(defun ghostherd--match-rules (text rules)
  "Return (STATE . REASON) for first matching rule in RULES against TEXT.
RULES is an alist of (STATE . REGEXP-LIST).  Prefer blocked over
working over idle (strict blocked detection, herdr-style)."
  (cl-labels ((try (state)
                (when-let* ((patterns (alist-get state rules)))
                  (cl-loop for pat in patterns
                           when (ghostherd--rule-matches-p pat text)
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
                           when (ghostherd--rule-matches-p pattern text)
                           collect (cons state pattern))))

(defcustom ghostherd-reason-width 90
  "Characters of screen text kept as a session's state reason."
  :type 'integer
  :group 'ghostherd)

(defconst ghostherd--screen-furniture
  "[│─╭╮╰╯┌┐└┘├┤┬┴┼┃━┏┓┗┛▌▐█▏▕]"
  "Box-drawing and gutter characters agent TUIs pad their frames with.")

(defun ghostherd--clean-screen-line (line)
  "Return LINE as one readable sentence, or nil if nothing is left.

A captured line arrives as the agent drew it: inside a box, padded to
the pane width, often behind a prompt glyph.  None of that survives a
trip through a desktop notification, so strip it here rather than at
each of the four places the reason is displayed."
  (let* ((s (replace-regexp-in-string ghostherd--screen-furniture " " line))
         (s (replace-regexp-in-string "\\`[ \t❯›»▸▪●○*•>-]+" "" s))
         (s (replace-regexp-in-string "[ \t]+" " " s))
         (s (string-trim s)))
    (unless (string-empty-p s)
      (truncate-string-to-width s ghostherd-reason-width nil nil t))))

(defcustom ghostherd-reason-context 3
  "Lines to look back for the subject of a prompt the rules matched.

The matched line is usually the *generic* half.  A claude permission
prompt reads

  Bash(rm -rf build/ && npm publish)

  Do you want to proceed?

and the pattern matches the second one, so \"needs attention (Do you
want to proceed?)\" says no more than \"needs attention\" did.  The line
that tells you what you are approving is above it, past however many
blank or box-only rows the agent drew.  Set to 0 to use the matched line
alone."
  :type 'integer
  :group 'ghostherd)

(defun ghostherd--saying-something-p (line)
  "Return non-nil when LINE carries words rather than decoration.

Agent CLIs draw logos and spinners out of block glyphs, and the first
run of the herd log caught the subject lookup picking one up: a state
reason came out as a row of half-blocks followed by the command that had
actually been typed.  Enumerating the glyphs is a losing game -- there
are hundreds and every CLI picks different ones -- so require the line
to contain some letters or digits instead."
  (>= (cl-count-if (lambda (c) (or (<= ?a c ?z) (<= ?A c ?Z) (<= ?0 c ?9)))
                   line)
      3))

(defun ghostherd--screen-lines (text pos)
  "Return the line of TEXT containing POS, and the lines before it."
  (let* ((start (1+ (or (cl-position ?\n text :end pos :from-end t) -1)))
         (end (or (cl-position ?\n text :start pos) (length text))))
    (cons (substring text start end)
          (nreverse (split-string (substring text 0 (max 0 (1- start))) "\n")))))

(defun ghostherd--matched-line (text pattern)
  "Return the cleaned prompt of TEXT that PATTERN matched, or nil.

The reason field used to hold the pattern itself, which is a debugging
artifact: it leaked into the notification, the switcher annotation and
the JSON listing, so \"agy needs attention\" was followed by a regexp
rather than by the question the agent had asked.

Returns the matched line, and where one is found within
`ghostherd-reason-context' rows above it, the nearest line that survives
cleaning -- joined subject-first, so truncation eats the generic half
rather than the specific one.

This is a display heuristic and is allowed to be one: at worst a
notification gains an irrelevant line.  It cannot affect *state*, which
is decided by the pattern alone."
  (let ((case-fold-search nil))
    (when-let* ((pos (string-match pattern text)))
      (pcase-let* ((`(,line . ,before) (ghostherd--screen-lines text pos))
                   (matched (ghostherd--clean-screen-line line)))
        ;; Context only augments a line that said something itself.  An
        ;; anchored prompt like "^> " cleans away to nothing, and dressing
        ;; it with whatever preceded it would attribute an unrelated line
        ;; to the reason.
        (when matched
          (let ((subject (cl-loop for candidate in (seq-take
                                                    before
                                                    ghostherd-reason-context)
                                  for clean = (ghostherd--clean-screen-line
                                               candidate)
                                  when (and clean
                                            (not (equal clean matched))
                                            (ghostherd--saying-something-p clean))
                                  return clean)))
            (if subject
                (ghostherd--clean-screen-line (concat subject " — " matched))
              matched)))))))

(defun ghostherd--rule-reason (text hit)
  "Return the human-readable reason for rule HIT against TEXT.
Falls back to the pattern when the matched line cleans up to nothing --
an anchored prompt like \"^> \" is all furniture."
  (or (ghostherd--matched-line text (cdr hit)) (cdr hit)))

(defun ghostherd--detect-state (session &optional screen)
  "Return (STATE . REASON) for SESSION from screen rules / buffer liveness.

SCREEN is that session's screen when the caller already has it -- the
poll path fetches the whole herd's screens in one go, and reading each
one again individually is the cost that made it worth batching.  Without
it, this captures for itself, which is what every interactive caller
does."
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
       (t
        (let* ((tail (and rules (or screen (ghostherd--host-capture session))))
               (hit (and tail (ghostherd--match-rules tail rules)))
               (report (ghostherd--fresh-report session)))
          (when tail
            (puthash (ghostherd-session-id session) tail ghostherd--screens))
          (cond
           ;; Screen rules keep priority for `blocked': a stale progress
           ;; report must never mask an agent sitting on a prompt, and
           ;; neither may a stale report from the agent itself.
           ((eq (car-safe hit) 'blocked)
            (cons 'blocked (ghostherd--rule-reason tail hit)))
           ;; What the agent says about itself beats reading its screen,
           ;; which is a rendering of a drawing of the same fact -- with
           ;; one exception.  A claimed `blocked' loses to a screen that
           ;; positively says `working', because those patterns match a
           ;; spinner: an agent generating tokens is not sitting on a
           ;; prompt, whatever its last hook said.  `idle' is not
           ;; positive evidence -- it is this function's fallback -- so it
           ;; does not get the same veto.
           ((and report
                 (not (and (eq (car report) 'blocked)
                           (eq (car-safe hit) 'working))))
            report)
           ;; A shell is not an agent, so nothing about it is detectable
           ;; -- but if something reported for it, that was believed above.
           ((eq kind 'shell)
            (cons 'idle "shell"))
           ;; Otherwise a live progress report beats scraping, which cannot
           ;; tell a working agent from a quiet one.
           ((ghostherd--progress-fresh-p session)
            (cons 'working (ghostherd--progress-reason session)))
           (hit (cons (car hit) (ghostherd--rule-reason tail hit)))
           ((null rules)
            (cons 'working "no rules"))
           ;; Known agent, no match → idle fallback (herdr-style)
           (t (cons 'idle "no rule matched"))))))))))

(defcustom ghostherd-input-grace 6
  "Seconds after input during which a session is not believed to be idle.

The same reasoning as `ghostherd-handoff-grace', which had it first and
had it alone.  An agent does not start the instant you press Return: the
CLI has to wake up and print, and until it does the screen still shows
what it showed before -- so the poll one second later reads `idle',
promotes it to `done' because work was in flight, and notifies you that
an agent which has not yet read your prompt is ready for review.

`ghostherd-handoff' guarded its own watch against exactly this and the
poll path did not, which is why it only showed up when someone sent a
prompt by hand and then watched the log."
  :type 'number
  :group 'ghostherd)

(defvar ghostherd--input-at (make-hash-table :test 'equal)
  "Session id → when input was last sent, as a float time.

A hash table rather than a slot on `ghostherd-session': adding a slot
means every session object created before the next reload is read
through shifted accessors, and this package is developed by reloading
into a live herd.  The readme's troubleshooting section has the story.")

(defcustom ghostherd-idle-settle 4
  "Seconds a busy session must look idle before it is believed.

Agent CLIs draw their input prompt whether or not they are working, so
an empty prompt is not evidence of idleness -- what makes a screen read
as busy is a spinner or a status line, and those blink out between two
tool calls.  A poll landing in that gap sees a screen with nothing
working on it, calls it idle, promotes it to `done', and notifies.  The
next poll sees the spinner again.  Three seconds of flapping produced
four notifications in the log this was written from.

Only entering idle waits.  Leaving it is immediate, because that is a
positive signal: something appeared on the screen."
  :type 'number
  :group 'ghostherd)

(defvar ghostherd--idle-since (make-hash-table :test 'equal)
  "Session id → when it first looked idle, as a float time.")

(defun ghostherd--settle-idle (session state)
  "Return STATE, or SESSION's current state while idle has not stuck yet."
  (let ((id (ghostherd-session-id session))
        (was (ghostherd-session-state session)))
    (cond
     ((or (not (eq state 'idle))
          (not (memq was '(working blocked starting))))
      (remhash id ghostherd--idle-since)
      state)
     (t
      (let ((since (or (gethash id ghostherd--idle-since)
                       (puthash id (float-time) ghostherd--idle-since))))
        (if (>= (- (float-time) since) ghostherd-idle-settle)
            (progn (remhash id ghostherd--idle-since) state)
          was))))))

(defun ghostherd--watched-p (session)
  "Return non-nil when you could actually be looking at SESSION now.

`done' means finished while you were not looking, so this decides
whether a notification is worth firing.  Being displayed is not enough:
`get-buffer-window\=' with ALL-FRAMES t counts frames that are
iconified or on another desktop, which are precisely the moments a
banner is for.  So: a window on a *visible* frame, and that frame
holding input focus.

`frame-focus-state\=' answers `unknown\=' where the window system cannot
say.  Unknown is treated as focused, on the grounds that a missing
notification is a smaller wrong than one you did not need."
  (when-let* ((buffer (ghostherd-session-buffer session)))
    (when (buffer-live-p buffer)
      (when-let* ((window (get-buffer-window buffer 'visible)))
        (not (null (frame-focus-state (window-frame window))))))))

(defun ghostherd--waking-up-p (session)
  "Return non-nil while SESSION is too freshly prompted to be believed idle."
  (when-let* ((at (gethash (ghostherd-session-id session)
                           ghostherd--input-at)))
    (< (- (float-time) at) ghostherd-input-grace)))

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
      (ghostherd--log-transition session old new reason)
      (ghostherd--maybe-notify-state session old new)
      (run-hook-with-args 'ghostherd-state-change-hook session old new)
      (when (get-buffer "*ghostherd*")
        (ghostherd--sidebar-refresh)))
    new))

(defun ghostherd-poll-session (session &optional screen)
  "Recompute and store state for SESSION.  Return new state.
SCREEN is SESSION's screen if the caller already fetched it; see
`ghostherd--detect-state'."
  (setq session (ghostherd-get session))
  (when session
    (let ((watched (ghostherd--watched-p session)))
      ;; A session you are looking at has been seen, by definition.  `done'
      ;; means "finished while you were not looking"; announcing it about an
      ;; agent on your screen is just noise, and it was most of the noise.
      (when watched
        (setf (ghostherd-session-seen session) t))
      (pcase-let ((`(,state . ,reason) (ghostherd--detect-state session screen)))
        ;; Freshly prompted agents are not idle, they are slow.  Before the
        ;; grace elapses the screen still shows whatever it showed when you
        ;; pressed Return, and believing it turns into a `done' notification
        ;; for work that has not started.
        ;;
        ;; Both this and the settle below only ever hold back an `idle',
        ;; and they exist because a *screen* is weak evidence.  The states
        ;; an agent actually reports about itself -- `working', `blocked',
        ;; `done' -- pass through untouched, which is the point: a hook
        ;; firing at the moment the CLI stops needs no hysteresis.
        (when (and (eq state 'idle)
                   (not (ghostherd-session-manual-state session))
                   (ghostherd--waking-up-p session))
          (setq state 'working
                reason "input sent, not awake yet"))
        ;; ...and a busy agent between two tool calls is not idle either.
        (setq state (ghostherd--settle-idle session state))
        ;; Promote idle → done when work finishes.
        ;;
        ;; Whether you saw it finish is asked below, of `watched\=', which
        ;; is about this instant.  It used to be asked here of `seen\=',
        ;; which is not: `seen\=' latches the moment the session is on a
        ;; focused frame and only clears on the way back into `working\='.
        ;; So watching an agent start -- which is what sending it a prompt
        ;; looks like -- suppressed the banner for the whole run, however
        ;; long you were away by the time it ended.  No `done\=' was ever
        ;; logged on the herd this was written from.
        (when (and (eq state 'idle)
                   (memq (ghostherd-session-state session)
                         '(working blocked starting)))
          (setq state 'done
                reason (or reason "idle after work")))
        ;; Herdr-style: `done' stays visible until the user views the
        ;; session.  It needs saying, because nothing re-derives it: the
        ;; screen cannot show `done\=' -- `ghostherd--detect-state' has no
        ;; branch that returns it -- and the promotion above only fires
        ;; out of `working'.  Without this the ✓ lasted a single tick.
        (when (and (eq state 'idle)
                   (eq (ghostherd-session-state session) 'done)
                   (not (ghostherd-session-seen session)))
          (setq state 'done
                reason (or (ghostherd-session-state-reason session) reason)))
        ;; `done' means finished while you were not looking, whoever said
        ;; so -- the promotion, the hold above, or an agent reporting it
        ;; outright.  All three answer to this.
        ;;
        ;; Both of these overwrite REASON rather than defaulting it.
        ;; `ghostherd--detect-state' answers with a reason in every
        ;; branch it has, so an `or\=' here could never reach its own
        ;; string, and the log said "no rule matched" about a decision
        ;; the rules did not make -- which is the one line the manual
        ;; tells you to read when a banner does not arrive.
        (when (and (eq state 'done) watched)
          (setq state 'idle
                reason "done, but you are watching"))
        ;; Once viewed, demote done back to plain idle.
        (when (and (eq state 'idle)
                   (eq (ghostherd-session-state session) 'done)
                   (ghostherd-session-seen session))
          (setq reason "seen"))
        (ghostherd--set-state session state reason)))))

(defun ghostherd-poll-all ()
  "Poll every registered session, one round trip per host where possible.

A host that can dump the whole herd's screens in one go says so from
`ghostherd-backend-screens' and is then responsible for the tick; one
that cannot -- ghostel, where the screen is already an Emacs buffer --
gets read session by session, which costs nothing there.

Sessions are grouped by backend rather than swept in one list, because
\"one round trip\" is a claim only a single host can make, and a herd may
straddle two."
  (let ((groups (seq-group-by #'ghostherd-session-backend
                              (hash-table-values ghostherd--sessions))))
    (if (null groups)
        (ghostherd--ensure-sessions)
      (pcase-dolist (`(,backend . ,members) groups)
        ;; The callback closes over MEMBERS and may arrive long after this
        ;; loop has moved on -- safe, because `dolist' binds afresh per
        ;; iteration under lexical binding, and each group's callback
        ;; therefore keeps its own sessions.
        (unless (ignore-errors
                  (ghostherd-backend-screens
                   backend members ghostherd-screen-tail-lines
                   (lambda (screens)
                     (ghostherd--poll-with-screens members screens))))
          (ghostherd--poll-with-screens members nil))))))

(defun ghostherd--poll-with-screens (sessions screens)
  "Recompute state for SESSIONS, reading SCREENS instead of the hosts.

SCREENS is an alist of session id → screen, and it is allowed to be
partial: a host may abandon a batch halfway, and a session it did not
answer for is read individually rather than skipped.

The sweep runs here rather than in the caller so that a batched host has
already stamped whatever it learned in the same round trip -- otherwise
asking whether each session is still alive would be the second
synchronous call this exists to remove."
  (ghostherd--ensure-sessions)
  (dolist (session sessions)
    (ignore-errors
      ;; The sweep may have deregistered it in between.
      (when (ghostherd-get (ghostherd-session-id session))
        (ghostherd-poll-session
         session (cdr (assoc (ghostherd-session-id session) screens)))))))

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
    (ghostherd--log-add session 'life
                        (format "spawned %s on %s" kind backend))
    (ghostherd--ensure-poll-timer)
    (run-hook-with-args 'ghostherd-session-created-hook session)
    (when display
      (ghostherd-visit session))
    (ghostherd--sidebar-refresh)
    session))

(declare-function ghostel--adjust-size "ghostel" (window &optional force))

(defun ghostherd--sync-view-size (buffer)
  "Resize BUFFER's terminal to the window now showing it.

ghostel sizes a terminal when it is created and thereafter from
`window-size-change-functions', which fire when a window is *resized*.
Displaying a buffer in an existing window is not a resize, so a view
created before it was displayed -- which is every tmux attach, since the
client has to exist before there is anything to show -- keeps whatever
size it was born with.  The agent then draws into a screen a fraction of
the window you are looking at, which reads as not being able to scroll
rather than as a pane that is 63x23 inside a window twice that.

Forced, because the terminal's own idea of its size is exactly what is
stale here."
  (when-let* ((window (get-buffer-window buffer t)))
    (when (fboundp 'ghostel--adjust-size)
      (ignore-errors (ghostel--adjust-size window t)))))

(defun ghostherd-visit (session)
  "Display SESSION, attaching a view to its host when there is none.

On the ghostel backend there is always exactly one buffer and this just
pops to it.  On tmux the buffer is a client that may not exist yet, so
visiting is what runs `tmux attach' -- the only moment in the whole
design where a second terminal emulator is in the picture."
  (setq session (ghostherd-get session))
  (unless session
    (user-error "No such session"))
  ;; The overlay's window is dedicated and its frame unsplittable;
  ;; popping the agent from there has nowhere to put it.
  (ghostherd--sidebar-leave-overlay)
  (let ((buffer (ghostherd--host-view session)))
    (setf (ghostherd-session-buffer session) buffer
          (ghostherd-session-seen session) t)
    (pop-to-buffer buffer)
    (ghostherd--sync-view-size buffer)
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
          (ghostherd--log-add name 'life
                              (format "restored from %s, detached" backend))
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
    (ghostherd--sidebar-leave-overlay)
    (delete-other-windows)
    (switch-to-buffer (ghostherd--host-view left))
    (split-window-right)
    (other-window 1)
    (switch-to-buffer (ghostherd--host-view right))
    (ghostherd--sync-view-size (ghostherd-session-buffer left))
    (ghostherd--sync-view-size (ghostherd-session-buffer right))
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
    (ghostherd--sidebar-leave-overlay)
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
        ;; Which authority decided this is the first thing to know, and a
        ;; report is the one that leaves no trace on the screen below.
        (when-let* ((entry (gethash (ghostherd-session-id session)
                                    ghostherd--reports)))
          (pcase-let ((`(,reported ,why ,at) entry))
            (insert (format "  reports  %s%s  (%s ago, trusted for %ss)\n"
                            reported
                            (if why (format "  %s" why) "")
                            (ghostherd--age-string at)
                            ghostherd-report-ttl))
            (unless (ghostherd--fresh-report session)
              (insert "           expired — detection has it back\n"))))
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
    (remhash id ghostherd--input-at)
    (remhash id ghostherd--idle-since)
    (remhash id ghostherd--reports)
    (remhash id ghostherd--screens)
    (ghostherd--log-add session 'life
                        (if kill-buffer "killed" "released from the herd"))
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
    ;; Everything else keyed by id has to move with it.  A rename used to
    ;; drop the input grace and the idle-settle timestamp on the floor,
    ;; which was invisible; dropping a report is not, since it would put a
    ;; blocked row back under the rules mid-prompt.
    (dolist (table (list ghostherd--input-at ghostherd--idle-since
                         ghostherd--reports ghostherd--screens))
      (when-let* ((value (gethash old-id table)))
        (remhash old-id table)
        (puthash new-name value table)))
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
  (puthash (ghostherd-session-id session) (float-time) ghostherd--input-at)
  ;; Logged before the state change, so the log reads in causal order:
  ;; what you sent, then what it did.
  (ghostherd--log-add session 'input
                      (format "← %s%s"
                              (ghostherd--clean-screen-line
                               (car (split-string text "\n" t)))
                              (if submit "" "  (not submitted)")))
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
mean what it says.  `:screen-rules' now agree -- see
`ghostherd--rule-matches-p'."
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

(defun ghostherd--cmd-caller ()
  "Return the session on whose behalf a `ghostel_cmd' is running, or nil.

`ghostel_cmd' is dispatched from the VT parser of the terminal that
asked, so the caller is identifiable with no environment at all: its
buffer is the current one.  `bin/ghostherd' cannot use this --
emacsclient does not carry the caller's environment into the form it
evaluates, and the buffer it evaluates in is nobody's terminal -- so
there the shell substitutes `GHOSTHERD_SESSION' itself.  Two paths, one
answer."
  (ghostherd-get (current-buffer)))

(defun ghostherd--cmd-target (name)
  "Return NAME, or the calling session when NAME is \"self\"."
  (if (equal name "self")
      (or (ghostherd--cmd-caller)
          (user-error "self: this is not an agent's own terminal"))
    name))

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
  "Return the state of session NAME as JSON.  NAME may be \"self\"."
  (ghostherd--json
   (if-let* ((s (ghostherd-get (ghostherd--cmd-target name))))
       (ghostherd-session-as-alist s)
     (list (cons 'error "unknown session")
           (cons 'name name)))))

(defun ghostherd-cmd-send (name text &rest _)
  "Send TEXT to session NAME without submitting."
  (ghostherd-send (ghostherd--cmd-target name) text nil)
  (format "sent to %s" name))

(defun ghostherd-cmd-prompt (name text &rest _)
  "Prompt session NAME with TEXT (submitted)."
  (ghostherd-prompt (ghostherd--cmd-target name) text)
  (format "prompted %s" name))

(defun ghostherd-cmd-message (to text &optional from &rest _)
  "Message TO with TEXT, optional FROM name.

FROM defaults to the calling agent rather than to \"user\": an agent's
message attributed to the human is what identity exists to stop."
  (ghostherd-message (or (and from (ghostherd--cmd-target from))
                         (ghostherd--cmd-caller)
                         "user")
                     (ghostherd--cmd-target to) text :submit t)
  (format "message → %s" to))

(defun ghostherd-cmd-report (name state &optional reason &rest _)
  "Record that session NAME reports itself in STATE, because REASON.
NAME may be \"self\", which is how a CLI hook is expected to call this."
  (ghostherd-report (ghostherd--cmd-target name) (intern state) reason)
  (format "%s reports %s" name state))

(defun ghostherd-cmd-read (name &optional n &rest _)
  "Read last N lines from session NAME.  NAME may be \"self\"."
  (ghostherd-read (ghostherd--cmd-target name)
                  (and n (string-to-number n))))


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


;;; Herd log buffer

(defun ghostherd--log-kind-face (kind)
  "Face for a log entry of KIND."
  (pcase kind
    ('state  'default)
    ('input  'font-lock-string-face)
    ('report 'font-lock-keyword-face)
    ('life   'shadow)
    (_       'default)))

(defvar-keymap ghostherd-log-mode-map
  :doc "Keymap for `ghostherd-log-mode'."
  "RET" #'ghostherd-log-show-screen
  "g"   #'ghostherd-log-refresh
  "C"   #'ghostherd-log-clear
  "q"   #'quit-window)

(define-derived-mode ghostherd-log-mode special-mode "GhostHerd-Log"
  "Chronological log of what the herd did."
  (setq truncate-lines t))

(defun ghostherd--log-render (buffer)
  "Draw the herd log into BUFFER."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (at-end (eobp))
          (point-was (point)))
      (erase-buffer)
      ;; Oldest first: a log is read downwards, and the interesting end is
      ;; the one you have not seen.
      (dolist (entry (reverse ghostherd--log))
        (let ((start (point)))
          (insert (format "%s  %-14s %s\n"
                          (format-time-string "%H:%M:%S"
                                              (ghostherd-log-entry-time entry))
                          (ghostherd-log-entry-session entry)
                          (propertize (or (ghostherd-log-entry-text entry) "")
                                      'face (ghostherd--log-kind-face
                                             (ghostherd-log-entry-kind entry)))))
          (when (ghostherd-log-entry-screen entry)
            (put-text-property start (point) 'ghostherd-log-entry entry)
            (save-excursion
              (goto-char (1- (point)))
              (insert (propertize "  ⏎" 'face 'shadow))))))
      (when (= (point-min) (point-max))
        (insert "Nothing logged yet.\n"))
      (goto-char (if at-end (point-max) (min point-was (point-max)))))))

(defun ghostherd-log-refresh ()
  "Redraw the herd log."
  (interactive)
  (ghostherd--log-render (current-buffer)))

(defun ghostherd-log-show-screen ()
  "Show the screen captured with the log entry at point.

This is the reason the log keeps them.  `ghostherd-explain' answers \"why
is it in that state\" only while the agent is still in it; a rule that
fired at 02:00 is otherwise unarguable by morning."
  (interactive)
  (let ((entry (get-text-property (point) 'ghostherd-log-entry)))
    (unless entry
      (user-error "No screen kept for this entry"))
    (with-help-window "*ghostherd screen*"
      (with-current-buffer standard-output
        (insert (format "%s  %s  %s\n"
                        (format-time-string "%F %T"
                                            (ghostherd-log-entry-time entry))
                        (ghostherd-log-entry-session entry)
                        (ghostherd-log-entry-text entry)))
        (insert (make-string 60 ?-) "\n")
        (insert (ghostherd-log-entry-screen entry))
        (unless (bolp) (insert "\n"))
        (insert (make-string 60 ?-) "\n")))))

;;;###autoload
(defun ghostherd-log ()
  "Show what the herd has been doing.

Every state transition and every prompt sent, with a timestamp.  Lines
marked =⏎= kept the screen that produced them; =RET= shows it.  The log
spans Emacs restarts, so entries from an earlier session are above the
line that says so; `C' forgets the lot."
  (interactive)
  ;; Reachable without `ghostherd-mode' having been enabled -- so this is
  ;; the second place the file gets read, and the flag is why it is not
  ;; read twice.
  (unless ghostherd--log-loaded
    (ghostherd-log-load))
  (let ((buf (get-buffer-create "*ghostherd-log*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'ghostherd-log-mode)
        (ghostherd-log-mode))
      (ghostherd--log-render buf))
    (ghostherd--sidebar-leave-overlay)
    (pop-to-buffer buf)))


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
    (ghostherd-sidebar-notes                 . "Edit note / role")
    (ghostherd-sidebar-interrupt             . "Interrupt (Escape)")
    (ghostherd-sidebar-abort                 . "Abort (C-c)")
    (ghostherd-sidebar-answer                . "Answer menu prompt")
    (ghostherd-sidebar-message               . "Message agent")
    (ghostherd-sidebar-prompt                . "Prompt agent")
    (ghostherd-sidebar-toggle-project-filter . "Toggle project filter")
    (ghostherd-sidebar-filter                . "Live-narrow (flex)")
    (ghostherd-sidebar-toggle-preview        . "Toggle screen preview")
    (ghostherd-sidebar-refresh               . "Refresh")
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
                '(blocked working done idle starting dead)
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
  "/" #'ghostherd-sidebar-filter
  "v" #'ghostherd-sidebar-toggle-preview
  "g" #'ghostherd-sidebar-refresh
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
  (tabulated-list-init-header))

(defun ghostherd--sidebar-footer ()
  "Mode-line hint row for the session list."
  (let* ((query ghostherd--sidebar-query)
         (querying (or ghostherd--sidebar-filtering
                       (and query (not (string-empty-p query)))))
         (counts (when querying
                   (format " %d/%d"
                           ghostherd--sidebar-match-count
                           ghostherd--sidebar-total-count))))
    (concat
     "GhostHerd"
     (when ghostherd--sidebar-filter-project
       (format " [%s]"
               (file-name-nondirectory
                (directory-file-name ghostherd--sidebar-filter-project))))
     (when querying (concat "  /" query))
     (or counts "")
     (cond
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
                 (propertize ghostherd-detached-glyph 'face face))))
    ('name    (propertize (ghostherd-session-name session) 'face face))
    ('kind    (symbol-name (ghostherd-session-kind session)))
    ('state   (propertize (symbol-name state) 'face face))
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
                         (ignore-errors (ghostherd--session-title session))))
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
    (mapconcat (lambda (line)
                 (truncate-string-to-width line width nil nil t))
               (split-string tail "\n")
               "\n")))

(defun ghostherd--sidebar-draw-preview ()
  "Rebuild the preview pane under the current row."
  (when (derived-mode-p 'ghostherd-sidebar-mode)
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t)
          (session (and (not ghostherd--sidebar-help-visible)
                        ghostherd-sidebar-show-preview
                        (ghostherd--sidebar-session-at-point)))
          (width (max 10 (or (ignore-errors (window-body-width))
                             (ghostherd--sidebar-available-width)))))
      (save-excursion
        (ghostherd--sidebar-erase-preview)
        (when (or ghostherd-sidebar-show-preview
                  ghostherd--sidebar-help-visible)
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (setq ghostherd--sidebar-preview-start (point-marker))
          (insert (propertize (make-string width ?─) 'face 'shadow) "\n")
          (insert (cond
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
  "Idle seconds before a sidecar `herd_tick'.

The poll timer runs while you type, so putting HTTP on it made
keystrokes compete with `url-retrieve'.  This fires only after Emacs
has been idle, then repeats while it stays idle."
  :type 'number
  :group 'ghostherd)

(defvar ghostherd--herd-tick-timer nil
  "Idle timer that drives `ghostherd--herd-tick-async'.")

(defun ghostherd--ensure-herd-tick-timer ()
  "Start the idle sidecar tick if `ghostherd-mode' is on."
  (unless (timerp ghostherd--herd-tick-timer)
    (setq ghostherd--herd-tick-timer
          (run-with-idle-timer ghostherd-herd-tick-idle t
                               #'ghostherd--herd-tick-async))))

(defun ghostherd--stop-herd-tick-timer ()
  (when (timerp ghostherd--herd-tick-timer)
    (cancel-timer ghostherd--herd-tick-timer)
    (setq ghostherd--herd-tick-timer nil)))

(defconst ghostherd--herd-protocol "\
Inter-agent mail on this Emacs's ghostherd sidecar.

Environment:
  GHOSTHERD_SESSION  this agent's name
  GHOSTHERD_RPC      POST JSON-RPC 2.0 here
  rpc.url            same URL, in this directory, if the env is stale

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
")

(defun ghostherd--herd-write-protocol ()
  "Refresh PROTOCOL.md next to rpc.url."
  (let ((dir (expand-file-name "ghostherd-mail" user-emacs-directory)))
    (make-directory dir t)
    (let ((coding-system-for-write 'utf-8))
      (with-temp-file (expand-file-name "PROTOCOL.md" dir)
        (insert ghostherd--herd-protocol)))))

(defun ghostherd--herd-snapshot ()
  "Session list pushed to the sidecar each poll."
  (mapcar
   (lambda (s)
     (list :name (ghostherd-session-name s)
           :kind (symbol-name (or (ghostherd-session-kind s) 'shell))
           :state (symbol-name (or (ghostherd-session-state s) 'idle))
           :notes (or (ghostherd-session-notes s) "")
           :project (or (ghostherd-session-project s) "")
           :reason (or (ghostherd-session-state-reason s) "")))
   (hash-table-values ghostherd--sessions)))

(defun ghostherd--herd-deliver-one (msg)
  "Paste one sidecar mail MSG through `ghostherd-message' / `handoff'."
  (let* ((from (or (plist-get msg :from) "unknown"))
         (to (plist-get msg :to))
         (body (or (plist-get msg :body) ""))
         (handoff (plist-get msg :handoff))
         (submit (if (plist-member msg :submit)
                     (plist-get msg :submit)
                   t)))
    (if handoff
        (ghostherd-handoff to body from)
      (ghostherd-message from to body :submit submit))))

(defun ghostherd--herd-deliver (result)
  "Deliver pending mail from a `herd_tick' RESULT plist."
  (dolist (msg (plist-get result :pending))
    (let ((id (plist-get msg :id)))
      (condition-case err
          (ghostherd--herd-deliver-one msg)
        (error
         (message "ghostherd herd mail %s: %s"
                  id (error-message-string err))))
      (when id
        (push id ghostherd--herd-ack-ids)))))

(defun ghostherd--explain-text (session)
  "One-screen explanation of SESSION state, for Telegram."
  (setq session (ghostherd-get session))
  (format "%s %s\n%s"
          (ghostherd--state-glyph (ghostherd-session-state session))
          (ghostherd-session-state session)
          (or (ghostherd-session-state-reason session) "—")))

(defun ghostherd--herd-run-command (cmd)
  "Run one sidecar Telegram CMD plist.  Return (:id ID :text TEXT)."
  (let* ((id (plist-get cmd :id))
         (op (format "%s" (or (plist-get cmd :op) "")))
         (name (plist-get cmd :session))
         (args (plist-get cmd :args))
         (text "ok"))
    (condition-case err
        (pcase op
          ("prompt"
           (ghostherd-prompt name (or (plist-get args :body) "") nil)
           (setq text "prompted"))
          ("answer"
           (let ((n (plist-get args :n)))
             (ghostherd-answer name (truncate (or n 1))))
           (setq text "answered"))
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
          ("screen"
           (setq text (or (ghostherd-read name 40) "(empty)"))
           (when (> (length text) 3500)
             (setq text (concat (substring text 0 3500) "\n…"))))
          ("explain"
           (setq text (ghostherd--explain-text name)))
          (_ (setq text (format "unknown op %s" op))))
      (error (setq text (error-message-string err))))
    (list :id id :text text)))

(defun ghostherd--herd-run-commands (result)
  "Execute Telegram commands from RESULT and queue replies."
  (dolist (cmd (plist-get result :commands))
    (push (ghostherd--herd-run-command cmd) ghostherd--herd-replies)))

(defun ghostherd--herd-tick-async ()
  "Push the snapshot and pull mail plus Telegram commands.  Never blocks."
  (when (and (not ghostherd--herd-tick-inflight)
             (fboundp 'ghostherd-memory-rpc-url)
             (ghostherd-memory-rpc-url)
             (fboundp 'ghostherd-memory-request-async))
    (let ((acks (or ghostherd--herd-ack-ids []))
          (replies ghostherd--herd-replies))
      (setq ghostherd--herd-tick-inflight t
            ghostherd--herd-replies nil)
      (ghostherd-memory-request-async
       "herd_tick"
       (lambda (result)
         (setq ghostherd--herd-tick-inflight nil
               ghostherd--herd-ack-ids
               (seq-difference ghostherd--herd-ack-ids
                               (append acks nil)))
         (ghostherd--herd-deliver result)
         (ghostherd--herd-run-commands result))
       (list :sessions (vconcat (ghostherd--herd-snapshot))
             :ack_ids (vconcat (append acks nil))
             :replies (vconcat (or replies [])))
       (lambda (_err)
         (setq ghostherd--herd-tick-inflight nil
               ghostherd--herd-replies
               (append replies ghostherd--herd-replies)))))))

(defun ghostherd-sidebar-refresh ()
  "Interactive sidebar refresh."
  (interactive)
  (ghostherd--sidebar-entries)
  (ghostherd--sidebar-print t)
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
  "Turn live-narrow on or off, including `ghostherd-filter-mode'."
  (setq ghostherd--sidebar-filtering (and on t))
  (when-let* ((buf (get-buffer "*ghostherd*")))
    (with-current-buffer buf
      (ghostherd-filter-mode (if ghostherd--sidebar-filtering 1 -1))))
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
        ;; The log first, so what the previous Emacs saw is above what this
        ;; one is about to do: restore writes its own lines.
        (unless ghostherd--log-loaded
          (ignore-errors (ghostherd-log-load)))
        ;; Enabling the mode is the restore hook: a herd left running by
        ;; a previous Emacs is picked up here, before anything asks for
        ;; a session list.
        (ignore-errors (ghostherd-restore))
        (ignore-errors (ghostherd-memory-start))
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
stay as motion; after RET applies, they move and RET visits."
  (when (fboundp 'evil-make-intercept-map)
    (evil-make-intercept-map ghostherd-sidebar-filter-map 'normal))
  (when (fboundp 'evil-define-key*)
    (evil-define-key* 'normal ghostherd-sidebar-filter-map
      (kbd "RET") #'ghostherd-sidebar-filter-confirm
      (kbd "<return>") #'ghostherd-sidebar-filter-confirm
      (kbd "C-m") #'ghostherd-sidebar-filter-confirm
      (kbd "<escape>") #'ghostherd-sidebar-quit)
    (evil-define-key* 'normal ghostherd-sidebar-mode-map
      (kbd "RET") #'ghostherd-sidebar-visit
      (kbd "<return>") #'ghostherd-sidebar-visit
      (kbd "/") #'ghostherd-sidebar-filter
      (kbd "q") #'ghostherd-sidebar-quit
      (kbd "<escape>") #'ghostherd-sidebar-quit)))

(with-eval-after-load 'evil
  (ghostherd--maybe-setup-evil-sidebar))

(provide 'ghostherd)
;;; ghostherd.el ends here

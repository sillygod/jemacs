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

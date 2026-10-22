;;; ghostherd-memory-page.el --- The herd's page: memory and log in an xwidget -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jing

;; This file is part of ghostherd.

;;; Commentary:

;; The herd's page.  Agents: account usage per CLI, the herd grouped by
;; project, and the operations the overlay has.  Memory: live search with agent and project
;; filters, the sessions the index holds, and the transcript a hit came
;; from, opened at the hit.  Log: what the herd did, as one state lane
;; per agent above the entries.  Built on xwapp like pr-view and
;; clickup-view: Elisp asks the sidecar and reads the log, ui/ only
;; renders.  The page holds no token and makes no request of its own,
;; so nothing it renders -- and transcripts carry whatever an agent
;; ever read -- can reach the sidecar.
;;
;;   M-x ghostherd-herd-page         the agents, by project, with usage
;;   M-x ghostherd-memory-page       search (C-u: this project only)
;;   M-x ghostherd-memory-sessions   the sessions, newest first
;;   M-x ghostherd-log-page          the herd log
;;
;; The text commands `ghostherd-memory-search', `ghostherd-memory-view'
;; and `ghostherd-log' stay: they need no xwidget.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'project)
(require 'xwapp)
(require 'ghostherd-memory)
(require 'ghostherd)
(require 'ghostherd-usage)

(declare-function pr-view-branch-pr "pr-view" (dir callback))
(declare-function pr-view-open-pr "pr-view" (dir id))
(declare-function pr-view-repo-pr-brief "pr-view" (remote id callback))
(declare-function pr-view-open-remote-pr "pr-view" (remote id))
(declare-function pr-view-local-stat "pr-view" (dir))
(declare-function pr-view-changes "pr-view" (&optional dir))
(declare-function pr-view-remote "pr-view" (dir))
(declare-function clickup-view-task-brief "clickup-view" (id callback))
(declare-function clickup-view-task "clickup-view" (id))

(defgroup ghostherd-memory-page nil
  "The memory page."
  :group 'ghostherd-memory
  :prefix "ghostherd-memory-page-")

(defcustom ghostherd-memory-page-poll-interval 0.2
  "Seconds between title-intent polls."
  :type 'number
  :group 'ghostherd-memory-page)

(defconst ghostherd-memory-page--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this file and ui/.")

(defconst ghostherd-memory-page--max-chunks 400
  "Most chunks one request may ask for; the page asks for windows.")

(defvar ghostherd-memory-page--pending nil
  "What to show once the page says it is ready: plist :view :query :project.")

(defvar ghostherd-memory-page--project nil
  "The project the page was opened from, offered as a filter.")

(defconst ghostherd-memory-page--progress-every 5
  "Seconds between reads of the sidecar's import progress.")

(defvar ghostherd-memory-page--progress-timer nil)
(defvar ghostherd-memory-page--progress-inflight nil)
(defvar ghostherd-memory-page--progress-shown nil
  "Non-nil while the page shows progress it read from the sidecar.")
(defvar ghostherd-memory-page--importing nil
  "Non-nil while an import the page asked for has not answered.")

(defvar ghostherd-memory-page--app
  (xwapp-create :buffer-name "*herd*"
                :index (expand-file-name "ui/index.html" ghostherd-memory-page--dir)
                :prefixes '("ghmem:")
                :namespace "GM"
                :idle-title "Herd"
                :handler #'ghostherd-memory-page--handle
                :on-kill (lambda ()
                           (setq ghostherd-memory-page--pending nil
                                 ghostherd-memory-page--importing nil)
                           (ghostherd-memory-page--unwatch)
                           (ghostherd-memory-page--herd-unwatch)
                           (ghostherd-memory-page--memory-unwatch))
                :reopen #'ghostherd-memory-page-reopen)
  "The page, buffer and intent channel; see `xwapp'.")


;;; Helpers

(defun ghostherd-memory-page--js (fn obj)
  (xwapp-js ghostherd-memory-page--app fn obj))

(defun ghostherd-memory-page--truthy (v)
  "Non-nil when V is true: intents parse JSON false as `:false'."
  (and v (not (memq v '(:false :null)))))

(defun ghostherd-memory-page--int (v default)
  (if (numberp v) (truncate v) default))

(defun ghostherd-memory-page--str (v)
  "V as a non-empty string, or nil."
  (and (stringp v) (not (string-empty-p (string-trim v))) (string-trim v)))

(defun ghostherd-memory-page--vec (xs)
  "XS as a vector.  json.el reads a list of plists as one alist, so
every list going to the page goes as a vector."
  (vconcat xs))

(defun ghostherd-memory-page--current-project ()
  "The current project's root as the importer records it, or nil."
  (when-let* ((p (project-current)))
    (directory-file-name (expand-file-name (project-root p)))))

(defun ghostherd-memory-page--fail (what)
  "An error callback reporting WHAT went wrong in the page."
  (lambda (err)
    (ghostherd-memory-page--js "showError" (format "%s: %s" what err))))


;;; Resuming a session

(defconst ghostherd-memory-page--recent 600
  "Seconds within which a transcript written to may still be open in an agent.")

(defvar ghostherd-memory-page--resumable (make-hash-table :test #'equal)
  "Source path -> (KIND ID DIRECTORY TITLE), as Emacs worked it out.
The page only names a source; what a resume runs comes from here,
never from the page.")

(defun ghostherd-memory-page--conversation-id (agent path chunk)
  "The id AGENT's CLI resumes the session at PATH by."
  (if (equal agent "claude")
      ;; `claude -r ID' opens ID.jsonl, and a transcript that was itself
      ;; resumed can carry another sessionId inside it.
      (file-name-base path)
    (plist-get chunk :session_id)))

(defun ghostherd-memory-page--resume-info (path chunks)
  "Whether the session at PATH can be resumed, as a plist for the page.
CHUNKS carry its agent, id and project.  When it can, remembers what a
resume runs."
  (remhash path ghostherd-memory-page--resumable)
  (let* ((c (car chunks))
         (agent (and c (plist-get c :agent)))
         (kind (and agent (intern-soft agent)))
         (id (and c (ghostherd-memory-page--conversation-id agent path c)))
         (dir (and c (ghostherd-memory-page--str (plist-get c :project))))
         (why
          (cond
           ((not c) "Nothing in this session")
           ((not (equal (plist-get c :source_kind) "transcript")) "Notes, not a conversation")
           ((not (and kind (assq kind ghostherd-agent-specs)
                      (ghostherd--resume-args kind id)))
            (format "%s cannot resume a conversation by id" agent))
           ((not (file-exists-p path))
            (if (equal agent "claude")
                "Claude Code deleted this transcript (cleanupPeriodDays); only the index is left"
              (format "%s no longer has this conversation; only the index is left" agent)))
           ((not (and dir (file-directory-p dir)))
            (format "Its directory %s is gone" (or dir "?"))))))
    (if why
        (list :ok :json-false :why why)
      (let ((age (float-time (time-subtract
                              nil (file-attribute-modification-time (file-attributes path))))))
        (puthash path (list kind id dir (plist-get c :title)) ghostherd-memory-page--resumable)
        (ghostherd-memory-page--resume-state path age)))))

(defun ghostherd-memory-page--resume-state (path &optional age)
  "The page's resume buttons for PATH, already found resumable.
AGE is how many seconds ago its transcript was written."
  (pcase-let* ((`(,kind ,id ,_dir ,_title) (gethash path ghostherd-memory-page--resumable))
               (running (ghostherd--resumed-by id)))
    (list :ok t
          :agent (symbol-name kind)
          :session (substring id 0 (min 8 (length id)))
          :fork (if (ghostherd--resume-args kind id t) t :json-false)
          :running (and running (ghostherd-session-name running))
          :recent (and age (< age ghostherd-memory-page--recent) (round age)))))

(defun ghostherd-memory-page--resume (path fork)
  "Resume (or with FORK, fork) the session at PATH in a new agent."
  (pcase-let ((`(,kind ,id ,dir ,title)
               (or (gethash path ghostherd-memory-page--resumable)
                   (user-error "That session cannot be resumed from here"))))
    (let ((running (and (not fork) (ghostherd--resumed-by id))))
      (if running
          ;; Already an agent on it: a second would be two writers.
          (ghostherd-visit running)
        (let ((session (ghostherd-resume kind id dir :fork fork :title title)))
          (ghostherd-memory-page--js
           "flash" (format "%s as %s" (if fork "Forked" "Resumed")
                           (ghostherd-session-name session)))
          (ghostherd-memory-page--js
           "setResume" (list :source_path path
                             :resume (ghostherd-memory-page--resume-state path))))))))

(defun ghostherd-memory-page--visit-agent (path)
  "Show the agent already running the session at PATH."
  (pcase-let ((`(,_kind ,id . ,_) (gethash path ghostherd-memory-page--resumable)))
    (if-let* ((running (and id (ghostherd--resumed-by id))))
        (ghostherd-visit running)
      (user-error "No agent is running that session now"))))


;;; Import progress

(defun ghostherd-memory-page--watch-import ()
  "Send the sidecar's import progress to the page until the import ends.
Also how a background import already running shows up on the page."
  (unless (timerp ghostherd-memory-page--progress-timer)
    (setq ghostherd-memory-page--progress-timer
          (run-at-time 0 ghostherd-memory-page--progress-every
                       #'ghostherd-memory-page--progress-tick))))

(defun ghostherd-memory-page--unwatch ()
  (when (timerp ghostherd-memory-page--progress-timer)
    (cancel-timer ghostherd-memory-page--progress-timer))
  (setq ghostherd-memory-page--progress-timer nil
        ghostherd-memory-page--progress-inflight nil))

(defun ghostherd-memory-page--progress (p)
  "The page's view of the sidecar's import progress P."
  (list :files (or (plist-get p :files) 0)
        :imported (or (plist-get p :imported) 0)
        :agent (or (plist-get p :agent) "")
        :current (let ((c (plist-get p :current)))
                   (if (stringp c) (file-name-nondirectory c) ""))))

(defun ghostherd-memory-page--progress-tick ()
  "Read the import progress once and pass it on."
  (cond
   ((not (xwapp-live-p ghostherd-memory-page--app))
    (ghostherd-memory-page--unwatch))
   (ghostherd-memory-page--progress-inflight nil)
   (t
    (setq ghostherd-memory-page--progress-inflight t)
    (ghostherd-memory-request-async
     "memory_status"
     (lambda (status)
       (setq ghostherd-memory-page--progress-inflight nil)
       (let ((p (plist-get status :import)))
         (cond
          ((and p (plist-get p :running))
           (setq ghostherd-memory-page--progress-shown t)
           (ghostherd-memory-page--js "importProgress" (ghostherd-memory-page--progress p)))
          ;; Asked, not yet begun: the sidecar has not picked it up.
          (ghostherd-memory-page--importing nil)
          (t
           (ghostherd-memory-page--unwatch)
           (when ghostherd-memory-page--progress-shown
             ;; A background import ended; ours report their own result.
             (setq ghostherd-memory-page--progress-shown nil)
             (ghostherd-memory-page--js "importProgress" nil)
             (ghostherd-memory-page--js "flash" "Import finished")
             (ghostherd-memory-page--send-context))))))
     nil
     (lambda (_err) (setq ghostherd-memory-page--progress-inflight nil))))))


;;; The log

(defconst ghostherd-memory-page--transition-re
  "\\`\\(\\S-+\\) → \\(\\S-+\\)\\(?:  \\(\\(?:.\\|\n\\)*\\)\\)?\\'"
  "A state entry's text, as `ghostherd--log-transition' writes it.")

(defun ghostherd-memory-page--log-entry (entry)
  "ENTRY, a `ghostherd-log-entry', as the page wants it.
A state entry comes apart into old, new and reason here, so the page
draws lanes from fields rather than parsing prose."
  (let* ((kind (ghostherd-log-entry-kind entry))
         (text (or (ghostherd-log-entry-text entry) ""))
         (screen (ghostherd-log-entry-screen entry))
         (why (ghostherd-log-entry-why entry)))
    (append (list :id (ghostherd-log-entry-id entry)
                  :t (* 1000 (float-time (ghostherd-log-entry-time entry)))
                  :session (format "%s" (or (ghostherd-log-entry-session entry) ""))
                  :kind (format "%s" (or kind ""))
                  :text text
                  :pinned (if (ghostherd-log-entry-pinned entry) t :json-false))
            (and why (list :why (format "%s" why)))
            (and (ghostherd-log-entry-note entry)
                 (list :note (ghostherd-log-entry-note entry)))
            (and (eq kind 'state)
                 (string-match ghostherd-memory-page--transition-re text)
                 (list :old (match-string 1 text)
                       :new (match-string 2 text)
                       :reason (or (match-string 3 text) "")))
            (and screen (list :screen screen)))))

(defun ghostherd-memory-page--send-log ()
  "Send the whole herd log, oldest first."
  ;; Reachable before `ghostherd-mode' has read the file, as `ghostherd-log' is.
  (unless ghostherd--log-loaded
    (ghostherd-log-load))
  (ghostherd-memory-page--js
   "renderLog"
   (list :entries (ghostherd-memory-page--vec
                   (mapcar #'ghostherd-memory-page--log-entry (reverse ghostherd--log))))))

(defun ghostherd-memory-page--on-log (entry)
  "Pass a new log ENTRY to the page, if it is open."
  (when (xwapp-live-p ghostherd-memory-page--app)
    (ghostherd-memory-page--js "logAdd" (ghostherd-memory-page--log-entry entry))))

(add-hook 'ghostherd-log-functions #'ghostherd-memory-page--on-log)

(defun ghostherd-memory-page--log-id (intent)
  "The log-entry id INTENT names, or a `user-error'."
  (let ((id (alist-get 'id intent)))
    (if (integerp id) id (user-error "No log entry named"))))

(defun ghostherd-memory-page--log-pin (intent)
  "Pin or un-pin the log entry INTENT names; tell the page the new state."
  (let* ((id (ghostherd-memory-page--log-id intent))
         (pinned (ghostherd-memory-page--truthy (alist-get 'pinned intent)))
         (entry (or (ghostherd-log-set-pinned id pinned)
                    (user-error "No such log entry"))))
    (ghostherd-memory-page--js
     "logPinned" (list :id id :pinned (if (ghostherd-log-entry-pinned entry)
                                          t :json-false)))))

(defun ghostherd-memory-page--log-delete (intent)
  "Delete the log entry INTENT names; tell the page it is gone."
  (let ((id (ghostherd-memory-page--log-id intent)))
    (when (ghostherd-log-delete id)
      (ghostherd-memory-page--js "logDeleted" (list :id id)))))


;;; The agents

(defconst ghostherd-memory-page--herd-every 5
  "Seconds between herd snapshots while the Agents tab is on screen.")

(defvar ghostherd-memory-page--herd-timer nil)
(defvar ghostherd-memory-page--usage-at 0
  "When this page last asked for fresh usage.")

(defconst ghostherd-memory-page--agent-name-re "\\`[A-Za-z0-9][A-Za-z0-9._-]*\\'"
  "A name the page may give a new agent: it becomes a tmux target.")

(defun ghostherd-memory-page--agent (s)
  "Session S as the page shows it."
  (let* ((buf (ghostherd-session-buffer s))
         (blocked (eq (ghostherd-session-state s) 'blocked))
         (err (and blocked (ghostherd--cli-error s)))
         (continue-at (and blocked (ghostherd--sleep-continue-at s)))
         (words (and blocked (ghostherd--continue-words s))))
    (list :name (ghostherd-session-name s)
          :kind (format "%s" (or (ghostherd-session-kind s) "shell"))
          :state (format "%s" (or (ghostherd-session-state s) "idle"))
          :reason (or (ghostherd-session-state-reason s) "")
          :prompt (ghostherd--prompt-json (ghostherd-session-prompt s))
          ;; A turn killed by an expired login offers a one-click /login;
          ;; the /login code-entry screen offers its URL to open/copy.  Only
          ;; while blocked on each, so the card's buttons are there exactly
          ;; when they would work.  (The phone path mirrors this in
          ;; `ghostherd--herd-snapshot'.)
          :login (if (eq (cdr err) 'login) t :json-false)
          :url (or (and blocked (ghostherd--login-url s)) "")
          ;; Who started it through `herd start', shown on the card.
          :startedBy (or (ghostherd-session-started-by s) "")
          ;; A turn killed by the Mac sleeping or a dropped connection: the
          ;; herd continues it itself.  `continueAt' (float ms) drives the
          ;; card's "continuing after sleep in Ns" countdown and its Cancel;
          ;; `continueStopped' is t once the herd has stopped retrying.
          ;; `continueAfter' / `continueAgain' say it in the cause's words
          ;; ("after a dropped connection", "the connection dropped again").
          :continueAt (if continue-at (* 1000 continue-at) 0)
          :continueStopped (if (ghostherd--sleep-continue-stopped-p s) t :json-false)
          :continueAfter (or (car words) "")
          :continueAgain (or (cdr words) "")
          :project (or (ghostherd--project-name (ghostherd-session-project s)) "")
          :role (or (ghostherd-session-role s) "")
          :work (ghostherd-memory-page--agent-work s)
          :links (ghostherd-memory-page--agent-links s)
          :notes (or (ghostherd-session-notes s) "")
          :backend (format "%s" (ghostherd-session-backend s))
          :detached (if (buffer-live-p buf) :json-false t)
          :manual (if (ghostherd-session-manual-state s) t :json-false)
          :since (let ((t0 (or (ghostherd-session-last-active s)
                               (ghostherd-session-started-at s))))
                   (and t0 (* 1000 (float-time t0))))
          :progress (ghostherd-session-progress-percent s)
          ;; What its screen says besides: work in the background, and
          ;; the prompt it suggests you send next.
          :background (ghostherd-memory-page--vec (ghostherd-session-background s))
          :suggestion (or (ghostherd-session-suggestion s) "")
          :said (ghostherd-memory-page--str (plist-get (ghostherd--link s) :last_head))
          :saidAt (plist-get (ghostherd--link s) :last_ms)
          ;; How its last turn began: an ask from another agent (its end is
          ;; not its report to the user -- whoever asked reports) or the
          ;; user's own prompt.  Drives whether a worker's turn end is an
          ;; Inbox item; leads and primary are surfaced by role regardless.
          :saidFromAsk (if (eq (ghostherd--turn-origin-of s) 'ask) t :json-false)
          ;; A lead the herd started to take a cross-project handoff: an
          ;; Update, kept until read, says so.  `handoffFrom' is who for.
          :handoffStarted (if (ghostherd--handoff-lead-for s) t :json-false)
          :handoffFrom (or (ghostherd--handoff-lead-for s) "")
          :conversation (ghostherd-session-conversation s)
          ;; Its place in the line of its name, and the CLIs before it.
          :generation (ghostherd-session-generation s)
          :line (ghostherd-memory-page--vec
                 (mapcar (lambda (a) (or (alist-get 'kind a) "?"))
                         (ghostherd-session-lineage s)))
          :relayTo (format "%s" (ghostherd-relay-default-kind s))
          :usedUp (or (ghostherd-usage-session-used-up-text s) "")
          :meter (ghostherd-memory-page--meter s)
          :subs (ghostherd-memory-page--subs-of s)
          :switching (if (gethash (ghostherd-session-id s) ghostherd--switching) t :json-false))))

;; Each agent's meter: what it runs on and how full its context is, read
;; by the sidecar from its transcript's tail -- asked again only when the
;; file has changed, as the room's columns are.

(defvar ghostherd-memory-page--meters (make-hash-table :test 'equal)
  "Agent name -> (SIGNATURE . METER): its transcript as last read, and
what the sidecar made of it, a plist or nil.")

(defvar ghostherd-memory-page--metering (make-hash-table :test 'equal)
  "Agent name -> t while the sidecar reads its meter.")

(defun ghostherd-memory-page--screen-model (s)
  "The model and effort S's own screen shows, for a CLI that keeps them in
its frame rather than a transcript the sidecar can read: (DISPLAY
. EFFORT), or nil.  Read from the screen the state poll already captured."
  (when-let* ((screen (gethash (ghostherd-session-id s) ghostherd--screens)))
    (pcase (ghostherd-session-kind s)
      ('grok (ghostherd--grok-model-line screen))
      ('agy (ghostherd--agy-model-line screen))
      (_ nil))))

(defconst ghostherd-memory-page--agy-window 1000000
  "agy's context window, in tokens.  Antigravity 1.4.0's `/context' panel
shows the same 1.0M window for every model it offers -- Gemini Flash/Pro,
Gemini 4 Argon, Claude Opus -- so this is the one window the CLI reports,
not a per-model table guessed here, and the number agy's transcript gives
can fill a bar like grok's and claude's.")

(defun ghostherd-memory-page--meter (s)
  "S's meter for the page, or false when there is nothing to show.
The sidecar gives the context tokens and the window they sit in; for
claude the model and effort come with it, for grok and agy off the
screen (they keep neither in a transcript).  agy's window is not in its
transcript, but its `/context' reports a uniform 1.0M, so it gets a bar
too."
  (let* ((m (cdr (gethash (ghostherd-session-name s) ghostherd-memory-page--meters)))
         (live (ghostherd-memory-page--screen-model s))
         (model (or (ghostherd-memory-page--str (plist-get m :model)) (car live)))
         (effort (or (ghostherd-memory-page--str (plist-get m :effort)) (cdr live)))
         (tokens (and (natnump (plist-get m :tokens)) (plist-get m :tokens))))
    (if (not (or model tokens))
        :json-false
      (list :model (or model "")
            :effort (or effort "")
            :tokens tokens
            :window (or (and (natnump (plist-get m :window)) (plist-get m :window))
                        (and tokens (eq (ghostherd-session-kind s) 'agy)
                             ghostherd-memory-page--agy-window))
            :pending (if (eq (plist-get m :pending) t) t :json-false)
            :compacted (if (eq (plist-get m :compacted) t) t :json-false)))))

(defun ghostherd-memory-page--ask-on-change (table busy name sig method path &optional notify)
  "Ask the sidecar METHOD about agent NAME's transcript PATH, unless SIG
is what TABLE holds from the last answer, or an answer is on its way
\(BUSY).  TABLE then holds (SIG . ANSWER); the page hears of a new one
on the next look.  When a new answer arrives, NOTIFY is called (it
defaults to sending the herd to the page); a look that wants to react
elsewhere -- the editor marks pushed to pr-view -- passes its own."
  (unless (or (equal sig (car (gethash name table))) (gethash name busy))
    (puthash name t busy)
    (let ((done (lambda (answer)
                  (remhash name busy)
                  (let ((had (cdr (gethash name table))))
                    (puthash name (cons sig answer) table)
                    (unless (equal had answer)
                      (funcall (or notify #'ghostherd-memory-page--send-herd-soon)))))))
      (condition-case nil
          (ghostherd-memory-request-async
           method done (list :path path)
           ;; A failed request is not an answer: clear the in-flight flag
           ;; and leave the cache be, so the next look asks again rather
           ;; than binding the agent to a sidecar that was gone or older
           ;; than the question until its (idle) transcript happens to move.
           (lambda (_e) (remhash name busy)))
        (error (remhash name busy))))))

(defun ghostherd-memory-page--meter-sig (s path)
  "Signature of what agent S's meter at PATH depends on.  grok keeps its
tokens and window in a `signals.json' beside the conversation, written a
beat after the transcript and otherwise unchanged while it idles; fold
that in, or the meter binds to the transcript's signature alone and goes
stale -- never asked again until the conversation itself moves."
  (let ((sig (ghostherd-memory-page--room-signature path)))
    (if (and sig (eq (ghostherd-session-kind s) 'grok))
        (cons sig (ghostherd-memory-page--room-signature
                   (expand-file-name "signals.json" (file-name-directory path))))
      sig)))

(defun ghostherd-memory-page--meter-look ()
  "Ask the sidecar again for the meter of each agent whose transcript
changed; the page hears of it on the next look."
  (dolist (s (ghostherd-sessions))
    (let* ((name (ghostherd-session-name s))
           (path (ghostherd-session-transcript s))
           (sig (ghostherd-memory-page--meter-sig s path)))
      (if (null sig)
          (remhash name ghostherd-memory-page--meters)
        (ghostherd-memory-page--ask-on-change
         ghostherd-memory-page--meters ghostherd-memory-page--metering
         name sig "herd_meter" path)))))

;; Each agent's sub-agents still running -- what each is and is doing --
;; read by the sidecar from their own transcripts, beside its own.
;; Asked again when its transcript changed, or one of the files the last
;; answer named: the folder a new one appears in, each running one's.

(defvar ghostherd-memory-page--subs (make-hash-table :test 'equal)
  "Agent name -> (SIGNATURE . ANSWER): what was watched when last asked,
and the sidecar's answer, a plist of :subs and :watch.")

(defvar ghostherd-memory-page--subs-reading (make-hash-table :test 'equal)
  "Agent name -> t while the sidecar reads its sub-agents.")

(defun ghostherd-memory-page--subs-of (s)
  "S's sub-agents still running, as the page takes them: field by field,
none once S is dead -- they stopped with it."
  (let ((str #'ghostherd-memory-page--str))
    (ghostherd-memory-page--vec
     (unless (eq (ghostherd-session-state s) 'dead)
       (mapcar (lambda (u)
                 (list :id (funcall str (plist-get u :id))
                       :who (funcall str (plist-get u :who))
                       :title (funcall str (plist-get u :title))
                       :started (funcall str (plist-get u :started))
                       :tools (if (natnump (plist-get u :tools)) (plist-get u :tools) 0)
                       :tokens (and (natnump (plist-get u :tokens)) (plist-get u :tokens))
                       :doing (funcall str (plist-get u :doing))))
               (seq-filter #'consp (plist-get (cdr (gethash (ghostherd-session-name s)
                                                             ghostherd-memory-page--subs))
                                               :subs)))))))

(defun ghostherd-memory-page--subs-look ()
  "Ask the sidecar again for the sub-agents of each live agent whose
transcript, or whose sub-agents' files, changed."
  (dolist (s (ghostherd-sessions))
    (let* ((name (ghostherd-session-name s))
           (path (ghostherd-session-transcript s))
           (watch (seq-filter #'stringp (plist-get (cdr (gethash name ghostherd-memory-page--subs))
                                                   :watch)))
           (sig (and (not (eq (ghostherd-session-state s) 'dead))
                     (ghostherd-memory-page--room-signature path))))
      (if (null sig)
          (remhash name ghostherd-memory-page--subs)
        (ghostherd-memory-page--ask-on-change
         ghostherd-memory-page--subs ghostherd-memory-page--subs-reading
         name (cons sig (mapcar #'ghostherd-memory-page--room-signature watch))
         "herd_subagents" path)))))

;; Each agent's recently edited files, read by the sidecar from its
;; transcript's tail (the files a confirmed edit tool named), so a diff's
;; file tree can mark which agent changed which file.  Asked again only
;; when the transcript has changed -- the paths are absolute here, turned
;; relative to a checkout where the editor map is built, so the one answer
;; serves both the checkout diff and the PR diff.

(defvar ghostherd-memory-page--edits (make-hash-table :test 'equal)
  "Agent name -> (SIGNATURE . ANSWER): its transcript as last read, and the
sidecar's answer, a plist whose :files is the absolute paths it edited.")

(defvar ghostherd-memory-page--editing (make-hash-table :test 'equal)
  "Agent name -> t while the sidecar reads its edited files.")

(defun ghostherd-memory-page--edits-changed ()
  "A fresh edits answer arrived: refresh the herd page, and push the marks
to pr-view if it is showing a diff -- so a diff opened before the cache was
warm gains its marks when the answer comes, with no manual refresh."
  (ghostherd-memory-page--send-herd-soon)
  (when (fboundp 'pr-view-refresh-editors)
    (ignore-errors (pr-view-refresh-editors))))

(defun ghostherd-memory-page--edits-look ()
  "Ask the sidecar again for the edited files of each agent whose transcript
changed; the editor map reads them from the cache on the next look.  Dead
agents are asked too -- what they edited is a fact of their last
conversation, and a file they touched is still theirs on a diff."
  (dolist (s (ghostherd-sessions))
    (let* ((name (ghostherd-session-name s))
           (path (ghostherd-session-transcript s))
           (sig (and (stringp path) (ghostherd-memory-page--room-signature path))))
      (if (null sig)
          (remhash name ghostherd-memory-page--edits)
        (ghostherd-memory-page--ask-on-change
         ghostherd-memory-page--edits ghostherd-memory-page--editing
         name sig "herd_edits" path #'ghostherd-memory-page--edits-changed)))))

(defvar ghostherd-memory-page--herd-soon nil
  "The timer of a herd about to be sent, or nil.")

(defun ghostherd-memory-page--send-herd-soon ()
  "Send the herd in a moment, while the page shows it: once for however
many meters came back together."
  (when (and (timerp ghostherd-memory-page--herd-timer)
             (not (timerp ghostherd-memory-page--herd-soon)))
    (setq ghostherd-memory-page--herd-soon
          (run-at-time 0.3 nil (lambda ()
                                 (setq ghostherd-memory-page--herd-soon nil)
                                 (ghostherd-memory-page--send-herd))))))

(defun ghostherd-memory-page--pickers ()
  "The kinds whose model the page can switch: kind -> models and efforts."
  (let (out)
    (dolist (kind (ghostherd--agent-kinds))
      (when-let* ((p (plist-get (ignore-errors (ghostherd--spec kind)) :model-picker)))
        (setq out (plist-put out (intern (format ":%s" kind))
                             (list :models (ghostherd-memory-page--vec (plist-get p :models))
                                   :efforts (ghostherd-memory-page--vec (plist-get p :efforts))
                                   :scope (symbol-name (or (plist-get p :scope) 'session)))))))
    (or out :json-false)))

(defun ghostherd-memory-page--usage ()
  "The usage cache, one entry per CLI, windows as used percent."
  (ghostherd-memory-page--vec
   (mapcar
    (lambda (entry)
      (let ((p (cdr entry)))
        (list :kind (format "%s" (car entry))
              :fetched (and (plist-get p :fetched) (* 1000 (plist-get p :fetched)))
              :error (or (plist-get p :error) "")
              :windows (ghostherd-memory-page--vec
                        (mapcar (lambda (w)
                                  (list :label (or (plist-get w :label) "")
                                        ;; Rounded: 100 - 99.3 is 0.7000000000000028.
                                        :used (or (ghostherd-usage--used (plist-get w :remaining)) 0)
                                        :resets (and (plist-get w :resets-at)
                                                     (* 1000 (plist-get w :resets-at)))))
                                (plist-get p :windows))))))
    (sort (copy-sequence ghostherd-usage--cache)
          (lambda (a b) (string< (format "%s" (car a)) (format "%s" (car b))))))))

(defun ghostherd-memory-page--ask (a)
  "Ask A, from the last `herd_tick', as the page shows it."
  (list :id (ghostherd-memory-page--str (plist-get a :id))
        :from (ghostherd-memory-page--str (plist-get a :from))
        :to (ghostherd-memory-page--str (plist-get a :to))
        :status (ghostherd-memory-page--str (plist-get a :status))
        :auto (if (plist-get a :auto) t :json-false)
        :error (ghostherd-memory-page--str (plist-get a :error))
        :head (ghostherd-memory-page--str (plist-get a :head))
        :replyHead (ghostherd-memory-page--str (plist-get a :reply_head))
        :created (plist-get a :created_ms)
        :answered (plist-get a :answered_ms)
        ;; Where it stands in its run: the ask its asker was answering,
        ;; the run's first ask, its round with its taker, and how many.
        :parent (plist-get a :parent)
        :run (plist-get a :run)
        :round (plist-get a :round)
        :of (plist-get a :of)
        ;; A handoff: the project it went to, and the one it came from.
        :project (ghostherd-memory-page--str (plist-get a :project))
        :fromProject (ghostherd-memory-page--str (plist-get a :from_project))
        ;; Its asker runs behind the herd's guard.
        :guarded (if (eq (plist-get a :guarded) t) t :json-false)))

;;; The hooks each CLI runs

(defconst ghostherd-memory-page--hooks-every 30
  "Seconds between hook inventories while the Agents tab is on screen.")

(defvar ghostherd-memory-page--hooks nil
  "The last `herd hooks list', as a plist; its :hooks are what the page
may ask to open, by index.")
(defvar ghostherd-memory-page--hooks-at 0)
(defvar ghostherd-memory-page--hooks-proc nil)

(defun ghostherd-memory-page--hooks-refresh ()
  "Take a fresh hook inventory in the background, then show it.
A subprocess, not a call: it reads a dozen files and must not stall
Emacs while it does."
  (when-let* ((client (and (not (process-live-p ghostherd-memory-page--hooks-proc))
                           (fboundp 'ghostherd-herd-client)
                           (ghostherd-herd-client))))
    (setq ghostherd-memory-page--hooks-at (float-time))
    (let* ((projects (delete-dups
                      (delq nil (mapcar (lambda (s)
                                          (let ((p (ghostherd-session-project s)))
                                            (and p (expand-file-name p))))
                                        (ghostherd-sessions)))))
           (out (generate-new-buffer " *ghostherd hooks*")))
      (setq ghostherd-memory-page--hooks-proc
            (make-process
             :name "ghostherd-hooks" :buffer out :noquery t
             :connection-type 'pipe
             :command (append (list client "hooks" "list" "--json")
                              (mapcan (lambda (p) (list "--project" p)) projects))
             :sentinel
             (lambda (proc _event)
               (unless (process-live-p proc)
                 (unwind-protect
                     (when (eq (process-exit-status proc) 0)
                       (setq ghostherd-memory-page--hooks
                             (ignore-errors
                               (with-current-buffer (process-buffer proc)
                                 (json-parse-string (buffer-string)
                                                    :object-type 'plist :array-type 'list
                                                    :null-object nil :false-object :json-false))))
                       (when (xwapp-live-p ghostherd-memory-page--app)
                         (ghostherd-memory-page--send-herd)))
                   (kill-buffer (process-buffer proc))))))))))

(defun ghostherd-memory-page--hooks-status ()
  "Whether ghostherd's hooks are in, per CLI, with every list a vector.
json.el writes an empty list as null, and a page reading `.length' of
null stopped drawing the Agents tab -- exactly when nothing is
installed, which is when this is worth reading."
  (let ((status (plist-get ghostherd-memory-page--hooks :ghostherd))
        out)
    (while status
      (let ((cli (pop status)) (st (pop status)))
        (setq out (append out (list cli (list :have (vconcat (plist-get st :have))
                                              :want (vconcat (plist-get st :want))))))))
    out))

(defun ghostherd-memory-page--hooks-payload ()
  "The inventory as the page wants it: each hook with its index."
  (let ((i -1))
    (list :status (ghostherd-memory-page--hooks-status)
          :list (ghostherd-memory-page--vec
                 (mapcar (lambda (h)
                           (setq i (1+ i))
                           (list :i i
                                 :file (plist-get h :file)
                                 :name (file-name-nondirectory (or (plist-get h :file) ""))
                                 :line (plist-get h :line)
                                 :readers (ghostherd-memory-page--vec (plist-get h :readers))
                                 :scope (plist-get h :scope)
                                 :source (plist-get h :source)
                                 :event (plist-get h :event)
                                 :matcher (plist-get h :matcher)
                                 :command (plist-get h :command)
                                 :ours (plist-get h :ours)
                                 :enabled (plist-get h :enabled)
                                 :error (plist-get h :error)))
                         (plist-get ghostherd-memory-page--hooks :hooks))))))

(defun ghostherd-memory-page--hook-open (intent)
  "Open the file of the hook INTENT names by index, at its line.
By index into the inventory Emacs took itself: the page never names a
path to open."
  (let* ((i (alist-get 'i intent))
         (h (and (natnump i) (nth i (plist-get ghostherd-memory-page--hooks :hooks))))
         (file (plist-get h :file)))
    (unless (and file (file-exists-p file))
      (user-error "No such hook"))
    (find-file-other-window file)
    (goto-char (point-min))
    (forward-line (1- (or (plist-get h :line) 1)))))

;;; Chores: the lookups, done while you are not typing
;;
;; A look at the herd's work is dozens of small calls that each hold
;; Emacs: git for a checkout, and for a pull request or a task an HTTPS
;; request whose setting up blocks some milliseconds.  Made at once they
;; held it a quarter of a second every minute, over half of one when the
;; answers went stale -- typing or not.  Each is a chore instead, and
;; the chores are done while you are not typing, a few milliseconds'
;; worth at a time.  An answer comes a little later; nothing waits on it.

(defconst ghostherd-memory-page--chore-idle 0.5
  "Seconds without a key before chores are done.")
(defconst ghostherd-memory-page--chore-budget 0.02
  "Seconds of chores done at a time; the first is always done.")
(defconst ghostherd-memory-page--chore-every 0.1
  "Seconds between looks at whether chores can be done.")

(defvar ghostherd-memory-page--chores nil
  "Chores waiting, oldest first, each (FN . ARGS).")
(defvar ghostherd-memory-page--chore-timer nil
  "Timer that does the chores while there are some.")

(defun ghostherd-memory-page--chore (fn &rest args)
  "Call FN with ARGS once you are not typing.
Asked again before then, it is still done once."
  (let ((chore (cons fn args)))
    (unless (member chore ghostherd-memory-page--chores)
      (setq ghostherd-memory-page--chores
            (nconc ghostherd-memory-page--chores (list chore)))))
  (unless (timerp ghostherd-memory-page--chore-timer)
    (setq ghostherd-memory-page--chore-timer
          (run-with-timer ghostherd-memory-page--chore-every ghostherd-memory-page--chore-every
                          #'ghostherd-memory-page--chores-work))))

(defun ghostherd-memory-page--chore-next ()
  "Do the oldest chore."
  (let ((chore (pop ghostherd-memory-page--chores)))
    (with-demoted-errors "ghostherd: %S"
      (apply (car chore) (cdr chore)))))

(defun ghostherd-memory-page--chores-work ()
  "Do chores for a moment, unless you are typing; stop when none are left."
  (let ((idle (current-idle-time)))
    (when (and idle (>= (float-time idle) ghostherd-memory-page--chore-idle))
      (let ((until (+ (float-time) ghostherd-memory-page--chore-budget)))
        (while (and ghostherd-memory-page--chores (< (float-time) until))
          (ghostherd-memory-page--chore-next)))))
  (unless ghostherd-memory-page--chores
    (when (timerp ghostherd-memory-page--chore-timer)
      (cancel-timer ghostherd-memory-page--chore-timer))
    (setq ghostherd-memory-page--chore-timer nil)))

;;; What a project works on: its branch's pull request, the tasks it cites
;;
;; The project's checkout, and an agent's where it differs -- an agent in
;; one of the project's worktrees is on that worktree's branch.  The PR
;; comes from pr-view, which holds the forge's token, and the ClickUp
;; tasks it cites from clickup-view, which holds ClickUp's; without
;; them nothing is shown.  Looked at once a minute while the Agents tab
;; is watched, and an answer kept five minutes: the forge and ClickUp
;; both count requests.

(defconst ghostherd-memory-page--work-every 60
  "Seconds between looks at the herd's checkouts.")
(defconst ghostherd-memory-page--work-ttl 300
  "Seconds a pull request or a task looked up is kept.")
(defconst ghostherd-memory-page--work-tasks 6
  "Most of a pull request's tasks looked up; the rest show their id.")
(defconst ghostherd-memory-page--pending-ttl 120
  "Seconds a look may be in flight before it is taken as abandoned.
A forge or ClickUp answer that never comes -- the Mac slept, the
connection dropped -- must not wedge a chip: once a look has hung this
long it no longer counts as in flight, so the next look asks again
instead of waiting on it forever.")

(defvar ghostherd-memory-page--work-at 0
  "When the herd's checkouts were last looked at.")
(defvar ghostherd-memory-page--checkouts (make-hash-table :test 'equal)
  "Project or (agent . NAME) -> the checkout it is in, (ROOT . BRANCH).")
(defvar ghostherd-memory-page--look nil
  "The look at the checkouts under way, as (CHECKOUTS . DIRS): what
becomes `ghostherd-memory-page--checkouts', and directory -> checkout,
so git is asked once a directory.  nil when none is.")
(defvar ghostherd-memory-page--prs (make-hash-table :test 'equal)
  "(ROOT . BRANCH) -> (:at WHEN :pr PR :pending BOOL), PR as
`pr-view-branch-pr' gave it.")
(defvar ghostherd-memory-page--task-briefs (make-hash-table :test 'equal)
  "Task id -> (:at WHEN :brief BRIEF :pending BOOL), BRIEF as
`clickup-view-task-brief' gave it.")
(defvar ghostherd-memory-page--links (make-hash-table :test 'equal)
  "Agent name -> (:sig SIG :links LINKS :pending BOOL): the pull requests
and tasks its transcript names, as the sidecar's `herd_links' read them
when the transcript was as SIG says.")
(defvar ghostherd-memory-page--pr-briefs (make-hash-table :test 'equal)
  "(HOST OWNER REPO ID) -> (:at WHEN :pr PR :pending BOOL): a pull
request a conversation named, as `herd_links' names it, and PR as
`pr-view-repo-pr-brief' gave it.")

(defvar ghostherd-memory-page--remotes (make-hash-table :test 'equal)
  "Checkout root -> (WHEN . REMOTE), REMOTE as `pr-view-remote' gave it.")

(defconst ghostherd-memory-page--links-max 12
  "Most pull requests, and most tasks, of one agent's conversation looked up.")
(defconst ghostherd-memory-page--closed-ttl 3600
  "Seconds a merged or declined pull request is kept: it will not change.")

(defun ghostherd-memory-page--checkout (dir)
  "The checkout DIR is in, as (ROOT . BRANCH), or nil."
  (when (and (stringp dir) (file-directory-p (expand-file-name dir)))
    (let ((default-directory (file-name-as-directory (expand-file-name dir))))
      (with-temp-buffer
        (when (eq 0 (ignore-errors
                      (call-process "git" nil '(t nil) nil
                                    "rev-parse" "--show-toplevel" "--abbrev-ref" "HEAD")))
          (pcase (split-string (buffer-string) "\n" t)
            (`(,root ,branch)
             (cons (abbreviate-file-name (file-name-as-directory root)) branch))))))))

(defun ghostherd-memory-page--have (fn feature)
  "Non-nil when FN can be called: defined, or once FEATURE is loaded."
  (or (fboundp fn) (and (require feature nil t) (fboundp fn))))

(defun ghostherd-memory-page--pending-p (entry)
  "Non-nil when ENTRY has a look still in flight -- one begun lately, not
one abandoned because its answer never came.  Pending is kept as the time
the look began; an older boolean `t' from before this counts as abandoned,
so it is asked afresh rather than waited on."
  (let ((p (and entry (plist-get entry :pending))))
    (and (numberp p) (< (- (float-time) p) ghostherd-memory-page--pending-ttl))))

(defun ghostherd-memory-page--stale-p (entry)
  (not (or (ghostherd-memory-page--pending-p entry)
           (and entry (plist-get entry :at)
                (< (- (float-time) (plist-get entry :at)) ghostherd-memory-page--work-ttl)))))

(defun ghostherd-memory-page--work-changed ()
  (when (timerp ghostherd-memory-page--herd-timer)
    (ghostherd-memory-page--send-herd)))

(defun ghostherd-memory-page--task-ask (id)
  "Look up task ID with clickup-view, unless what it said is fresh."
  (let ((had (gethash id ghostherd-memory-page--task-briefs)))
    (when (and (ghostherd-memory-page--stale-p had)
               (ghostherd-memory-page--have 'clickup-view-task-brief 'clickup-view))
      (puthash id (plist-put (copy-sequence had) :pending (float-time)) ghostherd-memory-page--task-briefs)
      (clickup-view-task-brief
       id (lambda (brief)
            (puthash id (list :at (float-time) :brief brief) ghostherd-memory-page--task-briefs)
            (ghostherd-memory-page--work-changed))))))

(defun ghostherd-memory-page--pr-ask (co)
  "Look up checkout CO's pull request with pr-view, unless what it said
is fresh; then the tasks it cites."
  (let ((had (gethash co ghostherd-memory-page--prs)))
    (when (ghostherd-memory-page--stale-p had)
      (puthash co (plist-put (copy-sequence had) :pending (float-time)) ghostherd-memory-page--prs)
      (pr-view-branch-pr
       (car co)
       (lambda (pr)
         (puthash co (list :at (float-time) :pr pr) ghostherd-memory-page--prs)
         (unless (alist-get 'error pr)
           (dolist (id (seq-take (alist-get 'tasks pr) ghostherd-memory-page--work-tasks))
             (ghostherd-memory-page--chore #'ghostherd-memory-page--task-ask id)))
         (ghostherd-memory-page--work-changed))))))

(defun ghostherd-memory-page--pr-ref-p (ref)
  "Non-nil when REF, as `herd_links' names a pull request, is one: a
forge, a repository's names, a number."
  (and (member (plist-get ref :host) '("github" "bitbucket"))
       (natnump (plist-get ref :id))
       (seq-every-p (lambda (k)
                      (let ((v (plist-get ref k)))
                        (and (stringp v) (string-match-p "\\`[[:alnum:]_][[:alnum:]_.-]*\\'" v))))
                    '(:owner :repo))))

(defun ghostherd-memory-page--pr-key (ref)
  "What pull request REF, as `herd_links' names one, is kept by."
  (list (plist-get ref :host) (plist-get ref :owner) (plist-get ref :repo) (plist-get ref :id)))

(defun ghostherd-memory-page--pr-remote (ref)
  "The repository of pull request REF, as `pr-view-remote' gives one."
  (list :kind (intern (plist-get ref :host)) :owner (plist-get ref :owner) :repo (plist-get ref :repo)))

(defun ghostherd-memory-page--pr-unsure-p (entry)
  "Non-nil when ENTRY's pull-request state cannot be trusted now: the last
look failed (`:miss'), or one has hung past `--pending-ttl' without an
answer.  Then the chip says it does not know rather than a state it can no
longer confirm."
  (and entry
       (or (plist-get entry :miss)
           (let ((p (plist-get entry :pending)))
             (and (numberp p) (>= (- (float-time) p) ghostherd-memory-page--pending-ttl))))))

(defun ghostherd-memory-page--pr-brief-ask (ref)
  "Look up pull request REF, as `herd_links' names one, unless what it
said is fresh: of whichever repository, by its forge.  A merged or
declined one is final and kept; an open one is re-checked.  A look that
fails keeps the last state known but marks it unsure, and one that never
answers (the Mac slept) is taken as abandoned and asked again -- so a chip
is never wedged on a state the forge has since changed."
  (let* ((key (ghostherd-memory-page--pr-key ref))
         (had (gethash key ghostherd-memory-page--pr-briefs))
         (ttl (if (member (alist-get 'state (plist-get had :pr)) '("MERGED" "DECLINED"))
                  ghostherd-memory-page--closed-ttl
                ghostherd-memory-page--work-ttl)))
    (when (and (not (ghostherd-memory-page--pending-p had))
               (or (null (plist-get had :at))
                   (> (- (float-time) (plist-get had :at)) ttl))
               (ghostherd-memory-page--have 'pr-view-repo-pr-brief 'pr-view))
      (puthash key (plist-put (copy-sequence had) :pending (float-time)) ghostherd-memory-page--pr-briefs)
      (pr-view-repo-pr-brief
       (ghostherd-memory-page--pr-remote ref) (plist-get ref :id)
       (lambda (pr)
         (let ((err (alist-get 'error pr))
               (prev (plist-get (gethash key ghostherd-memory-page--pr-briefs) :pr)))
           (puthash key
                    (if err
                        ;; A failed look does not overwrite a known state with
                        ;; nothing: it keeps the last one but notes the miss,
                        ;; so the chip can say it is unsure rather than show a
                        ;; state that may be stale.
                        (list :at (float-time) :pr prev :miss err)
                      (list :at (float-time) :pr pr))
                    ghostherd-memory-page--pr-briefs))
         (ghostherd-memory-page--work-changed))))))

(defun ghostherd-memory-page--remote (root)
  "The forge repository of checkout ROOT, asked of git once a while.
For a chore: the page is shown what the last one asked."
  (let ((had (gethash root ghostherd-memory-page--remotes)))
    (if (and had (< (- (float-time) (car had)) ghostherd-memory-page--work-ttl))
        (cdr had)
      (let ((remote (and (ghostherd-memory-page--have 'pr-view-remote 'pr-view)
                         (ignore-errors (pr-view-remote root)))))
        (puthash root (cons (float-time) remote) ghostherd-memory-page--remotes)
        remote))))

(defun ghostherd-memory-page--repo-p (pr remote)
  "Non-nil when PR, as `herd_links' names one, is of REMOTE's repository."
  (and remote
       (equal (plist-get pr :host) (symbol-name (plist-get remote :kind)))
       (equal (plist-get pr :owner) (downcase (plist-get remote :owner)))
       (equal (plist-get pr :repo) (downcase (plist-get remote :repo)))))

(defun ghostherd-memory-page--named-prs (links)
  "The pull requests LINKS name that are looked up, the most recently
named first: of any repository -- an agent may work on one beside its
project's."
  (seq-take (seq-filter #'ghostherd-memory-page--pr-ref-p (plist-get links :prs))
            ghostherd-memory-page--links-max))

(defun ghostherd-memory-page--links-root (s)
  "The checkout S works in: its pull requests are its project's own."
  (let ((co (or (gethash (cons 'agent (ghostherd-session-name s)) ghostherd-memory-page--checkouts)
                (gethash (ghostherd--project-name (ghostherd-session-project s))
                         ghostherd-memory-page--checkouts))))
    (and (consp co) (car co))))

(defun ghostherd-memory-page--links-look (id)
  "Have the sidecar read what session ID's transcript names, when it
changed; and look up those of its pull requests and tasks not looked up
lately."
  (when-let* ((s (ghostherd-get id)))
    (let* ((name (ghostherd-session-name s))
           (path (ghostherd-session-transcript s))
           (sig (ghostherd-memory-page--room-signature path))
           (had (gethash name ghostherd-memory-page--links)))
      (when (and sig (not (equal sig (plist-get had :sig)))
                 (not (ghostherd-memory-page--pending-p had)))
        (puthash name (plist-put (copy-sequence had) :pending (float-time)) ghostherd-memory-page--links)
        (ghostherd-memory-request-async
         "herd_links"
         (lambda (r)
           (puthash name (list :sig sig :links r) ghostherd-memory-page--links)
           (ghostherd-memory-page--chore #'ghostherd-memory-page--links-ask id)
           (ghostherd-memory-page--work-changed))
         (list :path path)
         (lambda (_e) (puthash name had ghostherd-memory-page--links))))
      (ghostherd-memory-page--links-ask id))))

(defun ghostherd-memory-page--links-ask (id)
  "Look up session ID's conversation's pull requests and its tasks, the
most recently named first; and which repository is its own."
  (when-let* ((s (ghostherd-get id)))
    (let ((links (plist-get (gethash (ghostherd-session-name s) ghostherd-memory-page--links) :links))
          (root (ghostherd-memory-page--links-root s)))
      (when root
        (ghostherd-memory-page--remote root))
      (dolist (ref (ghostherd-memory-page--named-prs links))
        (ghostherd-memory-page--chore #'ghostherd-memory-page--pr-brief-ask ref))
      (dolist (task (seq-take (plist-get links :tasks) ghostherd-memory-page--links-max))
        (ghostherd-memory-page--chore #'ghostherd-memory-page--task-ask (plist-get task :id))))))

(defun ghostherd-memory-page--agent-links (s)
  "What S's conversation names, as the page shows it: pull requests --
its project's own marked so, another's by its repository -- and its
tasks, most recently named first."
  (let* ((links (plist-get (gethash (ghostherd-session-name s) ghostherd-memory-page--links) :links))
         (root (ghostherd-memory-page--links-root s))
         (remote (and root (cdr (gethash root ghostherd-memory-page--remotes))))
         (prs (ghostherd-memory-page--named-prs links))
         (tasks (seq-take (plist-get links :tasks) ghostherd-memory-page--links-max)))
    (if (not (or prs tasks))
        :json-false
      (list :root root
            :prs (ghostherd-memory-page--vec
                  (mapcar (lambda (ref)
                            (let* ((entry (gethash (ghostherd-memory-page--pr-key ref)
                                                   ghostherd-memory-page--pr-briefs))
                                   (pr (plist-get entry :pr)))
                              (list :id (plist-get ref :id)
                                    :own (if (ghostherd-memory-page--repo-p ref remote) t :json-false)
                                    :host (plist-get ref :host)
                                    :owner (plist-get ref :owner)
                                    :repo (plist-get ref :repo)
                                    :title (ghostherd-memory-page--str (alist-get 'title pr))
                                    :state (ghostherd-memory-page--str (alist-get 'state pr))
                                    ;; The last look failed or hung: keep the
                                    ;; chip, but say it does not know the state
                                    ;; rather than assert a stale one.
                                    :unsure (if (ghostherd-memory-page--pr-unsure-p entry) t :json-false)
                                    :draft (if (eq (alist-get 'draft pr) t) t :json-false)
                                    :url (ghostherd-memory-page--str (alist-get 'url pr))
                                    :error (ghostherd-memory-page--str
                                            (or (plist-get entry :miss) (alist-get 'error pr)))
                                    :last (plist-get ref :last))))
                          prs))
            :tasks (ghostherd-memory-page--vec
                    (mapcar (lambda (ref)
                              (append (ghostherd-memory-page--task (plist-get ref :id))
                                      (list :last (plist-get ref :last))))
                            tasks))))))

(defun ghostherd-memory-page--work-look ()
  "Look at the herd's checkouts, then ask after the pull requests of those
whose answer is stale; and at what each conversation names.  As chores:
until they are done, the page shows the last look."
  (setq ghostherd-memory-page--work-at (float-time))
  (when (and (not ghostherd-memory-page--look)
             (ghostherd-memory-page--have 'pr-view-branch-pr 'pr-view))
    (setq ghostherd-memory-page--look (cons (make-hash-table :test 'equal)
                                            (make-hash-table :test 'equal)))
    (dolist (s (ghostherd-sessions))
      (ghostherd-memory-page--chore #'ghostherd-memory-page--look-at (ghostherd-session-id s)))
    (ghostherd-memory-page--chore #'ghostherd-memory-page--look-done))
  (dolist (s (ghostherd-sessions))
    (ghostherd-memory-page--chore #'ghostherd-memory-page--links-look (ghostherd-session-id s))
    ;; A worktree agent's branch-ahead and its worktree's size on disk.
    (when (ghostherd-session-worktree s)
      (ghostherd-memory-page--chore #'ghostherd-memory-page--worktree-ahead (ghostherd-session-id s))
      (ghostherd-memory-page--chore #'ghostherd-memory-page--worktree-size (ghostherd-session-id s)))
    ;; Worktrees of its project that no agent is in -- left by a killed one.
    (when-let* ((project (ghostherd-session-project s)))
      (ghostherd-memory-page--chore #'ghostherd-memory-page--strays-look project))))

(defun ghostherd-memory-page--look-at (id)
  "Put the checkouts of session ID, and of its project, in the look."
  (when-let* ((look ghostherd-memory-page--look)
              (s (ghostherd-get id)))
    (let ((checkout (lambda (dir)
                      (let ((key (and (stringp dir) (file-name-as-directory (expand-file-name dir)))))
                        (with-memoization (gethash key (cdr look))
                          (or (ghostherd-memory-page--checkout dir) 'none)))))
          (project (ghostherd--project-name (ghostherd-session-project s))))
      (when (and project (not (gethash project (car look))))
        (puthash project (funcall checkout project) (car look)))
      (puthash (cons 'agent (ghostherd-session-name s))
               (funcall checkout (ghostherd-session-directory s))
               (car look)))))

(defun ghostherd-memory-page--look-done ()
  "The look's checkouts are the herd's now; ask after their pull requests,
and count what they have not committed."
  (when-let* ((look ghostherd-memory-page--look))
    (setq ghostherd-memory-page--checkouts (car look)
          ghostherd-memory-page--look nil)
    (let (seen)
      (maphash (lambda (_k co)
                 (when (and (consp co) (not (member co seen)))
                   (push co seen)
                   (ghostherd-memory-page--chore #'ghostherd-memory-page--pr-ask co)
                   (ghostherd-memory-page--chore #'ghostherd-memory-page--changes-count (car co))))
               ghostherd-memory-page--checkouts))
    (ghostherd-memory-page--work-changed)))

;; What a checkout has not committed, in numbers: a chip on its work line
;; that opens pr-view's diff view of it.  pr-view counts it with git, as
;; a chore: at each look at the checkouts, and when an agent in one ends
;; a turn -- when what it has not committed most likely changed.  The
;; checkouts the herd is in are the ones pr-view's page may show.

(defvar ghostherd-memory-page--changes (make-hash-table :test 'equal)
  "Checkout root -> what it has not committed, as `pr-view-local-stat'
counted it.")

(defun ghostherd-memory-page--changes-count (root)
  "Count what checkout ROOT has not committed; the page is told when that
changed."
  (when (ghostherd-memory-page--have 'pr-view-local-stat 'pr-view)
    (let ((stat (ignore-errors (pr-view-local-stat root))))
      (unless (equal stat (gethash root ghostherd-memory-page--changes))
        (puthash root stat ghostherd-memory-page--changes)
        (ghostherd-memory-page--work-changed)))))

(defun ghostherd-memory-page--changes-on-state (session old new)
  "SESSION ended a turn, from OLD to NEW: count its checkouts again, while
the page is open."
  (when (and (eq old 'working) (memq new '(idle blocked done waiting))
             (xwapp-live-p ghostherd-memory-page--app))
    (dolist (key (list (cons 'agent (ghostherd-session-name session))
                       (ghostherd--project-name (ghostherd-session-project session))))
      (let ((co (gethash key ghostherd-memory-page--checkouts)))
        (when (consp co)
          (ghostherd-memory-page--chore #'ghostherd-memory-page--changes-count (car co)))))))

(add-hook 'ghostherd-state-change-hook #'ghostherd-memory-page--changes-on-state)

;; A worktree agent's card also shows how far its branch is ahead of its
;; base, and the worktree's size on disk.  Ahead is a cheap `git rev-list
;; --count', done as a chore at each look.  Size is `du', which can be
;; slow, so it runs in the background and is kept for a few minutes rather
;; than measured on every poll.

(defconst ghostherd-memory-page--size-ttl 300
  "Seconds a worktree's measured size is kept before `du' runs again.")
(defvar ghostherd-memory-page--ahead (make-hash-table :test 'equal)
  "Session id -> commits its worktree branch is ahead of its base.")
(defvar ghostherd-memory-page--size (make-hash-table :test 'equal)
  "Session id -> its worktree's size on disk in bytes, as `du' measured it.")
(defvar ghostherd-memory-page--size-at (make-hash-table :test 'equal)
  "Session id -> when its worktree size was last measured.")
(defvar ghostherd-memory-page--size-running (make-hash-table :test 'equal)
  "Session ids whose `du' is in flight, so it is not started twice.")

(defun ghostherd-memory-page--worktree-ahead (id)
  "Count how far session ID's worktree branch is ahead of its base; the
page is told when that changed."
  (when-let* ((s (ghostherd-get id))
              (wt (ghostherd-session-worktree s))
              (main (ghostherd-session-project s))
              (branch (plist-get wt :branch))
              (base (plist-get wt :base)))
    (let ((default-directory (file-name-as-directory (expand-file-name main))))
      (with-temp-buffer
        (when (eq 0 (ignore-errors
                      (call-process "git" nil '(t nil) nil "rev-list" "--count"
                                    (concat base ".." branch))))
          (let ((n (string-to-number (string-trim (buffer-string)))))
            (unless (equal n (gethash id ghostherd-memory-page--ahead))
              (puthash id n ghostherd-memory-page--ahead)
              (ghostherd-memory-page--work-changed))))))))

(defun ghostherd-memory-page--worktree-size (id)
  "Measure session ID's worktree size with `du' in the background, kept for
`ghostherd-memory-page--size-ttl'; the page is told when it changed.  Never
blocks Emacs, and never two `du's at once for one worktree."
  (when-let* ((s (ghostherd-get id))
              (wt (ghostherd-session-worktree s))
              (path (plist-get wt :path))
              ((file-directory-p (expand-file-name path)))
              ((not (gethash id ghostherd-memory-page--size-running)))
              ((let ((at (gethash id ghostherd-memory-page--size-at)))
                 (or (null at) (> (- (float-time) at) ghostherd-memory-page--size-ttl)))))
    (let ((buf (generate-new-buffer " *ghostherd-du*")))
      (condition-case err
          (progn
            (puthash id t ghostherd-memory-page--size-running)
            (make-process
             :name "ghostherd-du" :buffer buf :noquery t
             :command (list "du" "-sk" (expand-file-name path))
             :sentinel
             (lambda (proc _event)
               (unless (process-live-p proc)
                 (remhash id ghostherd-memory-page--size-running)
                 (puthash id (float-time) ghostherd-memory-page--size-at)
                 (when (buffer-live-p (process-buffer proc))
                   (when (eq (process-exit-status proc) 0)
                     (with-current-buffer (process-buffer proc)
                       (let ((kb (string-to-number (car (split-string (buffer-string))))))
                         (unless (equal (* kb 1024) (gethash id ghostherd-memory-page--size))
                           (puthash id (* kb 1024) ghostherd-memory-page--size)
                           (ghostherd-memory-page--work-changed)))))
                   (kill-buffer (process-buffer proc)))))))
        (error (remhash id ghostherd-memory-page--size-running)
               (when (buffer-live-p buf) (kill-buffer buf))
               (message "ghostherd: du on %s: %s" (abbreviate-file-name path)
                        (error-message-string err)))))))

;; A worktree a killed agent left behind -- kept because its branch was not
;; merged or its tree was not clean -- has no card of its own, so the
;; project it belongs to shows it: an idle worktree the user may remove with
;; `ghostherd-worktree-forget'.

(defvar ghostherd-memory-page--strays (make-hash-table :test 'equal)
  "Project name -> its git worktrees no live agent is in, (PATH . BRANCH) each.")

(defun ghostherd-memory-page--strays-look (project)
  "Find PROJECT's git worktrees that no live agent occupies; the page is
told when they change."
  (when (and (fboundp 'ghostherd--worktree-list) (stringp project))
    (let* ((busy (delq nil (mapcar (lambda (s)
                                     (when-let* ((p (plist-get (ghostherd-session-worktree s) :path)))
                                       (file-truename (directory-file-name p))))
                                   (ghostherd-sessions))))
           (strays (seq-remove (lambda (pb)
                                 (member (file-truename (directory-file-name (car pb))) busy))
                               (ghostherd--worktree-list project)))
           (key (ghostherd--project-name project)))
      (when key
        (unless (equal strays (gethash key ghostherd-memory-page--strays))
          (if strays (puthash key strays ghostherd-memory-page--strays)
            (remhash key ghostherd-memory-page--strays))
          (ghostherd-memory-page--work-changed))))))

(defun ghostherd-memory-page--herd-checkout-p (root)
  "Non-nil when ROOT is the root of a checkout the herd is in: one the
last look found, or one an agent's directory or project is in now."
  (and (stringp root)
       (or (seq-some (lambda (co) (and (consp co) (equal (car co) root)))
                     (hash-table-values ghostherd-memory-page--checkouts))
           (seq-some (lambda (s)
                       (seq-some (lambda (dir) (equal (car-safe (ghostherd-memory-page--checkout dir)) root))
                                 (list (ghostherd-session-directory s) (ghostherd-session-project s))))
                     (ghostherd-sessions)))
       t))

(add-hook 'pr-view-local-allowed-functions #'ghostherd-memory-page--herd-checkout-p)

;; A review of what a checkout has not committed -- comments on lines of
;; its diff, written in pr-view's page -- goes to one agent: its
;; project's lead first, when it has one, then each agent working in the
;; checkout, then a lead started for it, only when picked.  Not a shell:
;; what a shell is sent, it runs.

(defun ghostherd-memory-page--review-to (s)
  "Agent S as one a review may be sent to."
  (let ((name (ghostherd-session-name s)))
    (list :id name
          :label (format "%s · %s" name (ghostherd-session-kind s))
          :send (lambda (text)
                  (ghostherd-tell s text)
                  (format "Sent to %s" name)))))

(defun ghostherd-memory-page--start-a-lead (projects)
  "A `new lead' recipient to start, per leaderless PROJECT in PROJECTS."
  (mapcar (lambda (p)
            (list :id (concat "@lead " p)
                  :label (format "new lead%s · %s"
                                 (if (cdr projects)
                                     (concat " for " (file-name-nondirectory (directory-file-name p)))
                                   "")
                                 ghostherd-lead-kind)
                  :send (lambda (text)
                          (format "Started %s; it gets the comments once it is ready"
                                  (ghostherd-session-name (ghostherd-lead-say p text))))))
          projects))

(defun ghostherd-memory-page--command-agents ()
  "The live agents that run a command (not shells, not dead)."
  (seq-filter (lambda (s)
                (and (not (eq (ghostherd-session-state s) 'dead))
                     (plist-get (ghostherd--spec (ghostherd-session-kind s)) :command)))
              (ghostherd-sessions)))

(defun ghostherd-memory-page--agent-branch (s)
  "The branch agent S is on: its worktree's, else its checkout's (from the
cache the look fills -- where a non-checkout is the symbol `none', not a
cons -- then git as a fallback)."
  (or (plist-get (ghostherd-session-worktree s) :branch)
      (let ((co (gethash (cons 'agent (ghostherd-session-name s)) ghostherd-memory-page--checkouts)))
        (and (consp co) (cdr co)))
      (cdr (ghostherd-memory-page--checkout (ghostherd-session-directory s)))))

(defvar ghostherd-memory-page--project-remotes (make-hash-table :test 'equal)
  "Project name -> its origin remote URL, so a PR's repo is matched without
a git call each time.")

(defun ghostherd-memory-page--project-for-repo (repo)
  "A herd project whose origin remote names REPO (e.g. \"owner/name\"), or
nil.  REPO nil or unmatched gives nil."
  (and (stringp repo) (not (string-empty-p repo))
       (seq-find
        (lambda (p)
          (let ((remote (with-memoization (gethash p ghostherd-memory-page--project-remotes)
                          (let ((default-directory (file-name-as-directory (expand-file-name p))))
                            (with-temp-buffer
                              (if (eq 0 (ignore-errors
                                          (call-process "git" nil '(t nil) nil "remote" "get-url" "origin")))
                                  (string-trim (buffer-string)) ""))))))
            (and (stringp remote) (not (string-empty-p remote))
                 (string-match-p (regexp-quote repo) remote))))
        (delete-dups (delq nil (mapcar (lambda (s) (ghostherd--project-name (ghostherd-session-project s)))
                                       (ghostherd-sessions)))))))

(defun ghostherd-memory-page--agents-in-checkout (root)
  "The herd's command-agents working in checkout ROOT -- the set both a
checkout review's recipients and its editor marks draw from."
  (let* ((roots (make-hash-table :test 'equal))
         (root-of (lambda (dir)
                    (with-memoization (gethash dir roots)
                      (car-safe (ghostherd-memory-page--checkout dir))))))
    (seq-filter (lambda (s) (equal (funcall root-of (ghostherd-session-directory s)) root))
                (ghostherd-memory-page--command-agents))))

(defun ghostherd-memory-page--agents-on-pr (branch repo)
  "The herd's command-agents on a PR's head BRANCH in repo REPO -- the set
both a PR review's recipients and its editor marks draw from.  A repo the
page names must map to a herd project; only when the page could name none
do we fall back to the branch alone -- so a same-named branch in a repo
the herd does not have reaches nobody.  Empty for no such agent."
  (let* ((agents (ghostherd-memory-page--command-agents))
         (repo-project (ghostherd-memory-page--project-for-repo repo))
         (known (or repo-project (null repo) (and (stringp repo) (string-empty-p repo)))))
    (and known (stringp branch) (not (string-empty-p branch))
         (seq-filter (lambda (s)
                       (and (equal (ghostherd-memory-page--agent-branch s) branch)
                            (or (not repo-project)
                                (equal (ghostherd--project-name (ghostherd-session-project s))
                                       repo-project))))
                     agents))))

(defun ghostherd-memory-page--pr-recipients (branch repo)
  "Who a review of a PR on BRANCH (in repo REPO) may go to: the agent(s) on
that branch first, then the project's lead, then a lead to start -- the
order the user's ask asked for, the agent on the branch first.  None when
no herd agent is on the branch and REPO is not a herd project."
  (let* ((agents (ghostherd-memory-page--command-agents))
         (repo-project (ghostherd-memory-page--project-for-repo repo))
         (here (ghostherd-memory-page--agents-on-pr branch repo))
         (project (or repo-project
                      (and here (ghostherd--project-name (ghostherd-session-project (car here))))))
         (lead (let ((l (and project (ghostherd-project-lead project))))
                 (and l (memq l agents) l))))
    (append
     (mapcar #'ghostherd-memory-page--review-to (seq-remove (lambda (s) (eq s lead)) here))
     (and lead (list (ghostherd-memory-page--review-to lead)))
     (and project (not lead) (ghostherd-memory-page--start-a-lead (list project))))))

(defun ghostherd-memory-page--review-recipients (root &optional pr)
  "Who a review may go to, as `pr-view-local-recipients-functions' wants
them.  Without PR, of what checkout ROOT has not committed: its project's
lead first, then the agents in it, then a lead to start -- none for a
checkout the herd is not in.  With PR (a plist of :branch and :repo), of a
pull request: the agent(s) on its head branch first, then the lead -- see
`ghostherd-memory-page--pr-recipients'."
  (if pr
      (ghostherd-memory-page--pr-recipients (plist-get pr :branch) (plist-get pr :repo))
    (let* ((roots (make-hash-table :test 'equal))
           (root-of (lambda (dir)
                      (with-memoization (gethash dir roots)
                        (car-safe (ghostherd-memory-page--checkout dir)))))
           (agents (ghostherd-memory-page--command-agents))
           (here (ghostherd-memory-page--agents-in-checkout root))
           ;; The projects of those here, and those whose own checkout it is.
           (projects (delete-dups
                      (delq nil (mapcar (lambda (s)
                                          (let ((p (ghostherd--project-name (ghostherd-session-project s))))
                                            (and p (or (memq s here) (equal (funcall root-of p) root)) p)))
                                        (ghostherd-sessions)))))
           (leads (seq-filter (lambda (s) (memq s agents))
                              (delq nil (mapcar #'ghostherd-project-lead projects))))
           (leaderless (seq-remove #'ghostherd-project-lead projects)))
      (append
       (mapcar #'ghostherd-memory-page--review-to leads)
       (mapcar #'ghostherd-memory-page--review-to (seq-remove (lambda (s) (memq s leads)) here))
       (ghostherd-memory-page--start-a-lead leaderless)))))

(add-hook 'pr-view-local-recipients-functions #'ghostherd-memory-page--review-recipients)

;; Which agent changed which file, marked on a diff's file tree.  The
;; sidecar reads the files each agent's conversation edited (its transcript
;; tail), cached below like the meter and sub-agents; the map here turns
;; that into, per file of the diff, the agents of this checkout -- or of
;; the PR's head branch -- that edited it.  An agent's paths are relative
;; to its own checkout, so a worktree's edits never mark another tree's
;; file, and a file an agent touched outside the checkout is left unmarked.

(defun ghostherd-memory-page--agent-root (s)
  "The root of the checkout or worktree agent S works in, as its edited
paths are relative to -- its worktree's path, else its checkout's root."
  (or (plist-get (ghostherd-session-worktree s) :path)
      (car-safe (ghostherd-memory-page--checkout (ghostherd-session-directory s)))))

(defun ghostherd-memory-page--editor-mark (s)
  "Agent S as a mark on a file it edited: (:id NAME :label LABEL)."
  (list :id (ghostherd-session-name s)
        :label (format "%s · %s" (ghostherd-session-name s) (ghostherd-session-kind s))))

(defun ghostherd-memory-page--edited-under (name root)
  "The files agent NAME edited that lie under ROOT, each relative to it, in
order.  From the cache the look fills; empty when ROOT is unknown or NAME
has nothing cached (then the file is left unmarked -- never guessed)."
  (when (and (stringp root) (not (string-empty-p root)))
    (let ((base (file-name-as-directory (expand-file-name root)))
          out)
      (dolist (f (plist-get (cdr (gethash name ghostherd-memory-page--edits)) :files))
        (when (and (stringp f) (not (string-empty-p f)))
          (let ((rel (file-relative-name (expand-file-name f) base)))
            (unless (or (string-prefix-p "../" rel) (string-prefix-p "/" rel)
                        (equal rel "..") (equal rel "."))
              (push rel out)))))
      (nreverse out))))

(defun ghostherd-memory-page--editors (root &optional pr)
  "Which agent(s) edited each file of a diff, as
`pr-view-local-editors-functions' wants them: a list of (:file REL :by
MARKS), REL a path relative to the diff's tree, MARKS a list of
\(:id :label).  Without PR, of checkout ROOT -- the agents working in it.
With PR (a plist of :branch and :repo), only the agents on its head
branch -- the same set its review may go to.  Each agent's edits are
relative to its own checkout, so the PR's repo-relative tree and the
checkout's own tree both line up.

Asking also warms the cache: a diff opened before the herd tick has read
these agents' edits gets its marks when the answer arrives (it pushes them
to pr-view), with no manual refresh and no transcript re-read for one that
has not changed."
  (ghostherd-memory-page--edits-look)
  (let* ((agents (if pr
                     (ghostherd-memory-page--agents-on-pr (plist-get pr :branch) (plist-get pr :repo))
                   (ghostherd-memory-page--agents-in-checkout root)))
         (by-file (make-hash-table :test 'equal))
         (order nil))
    (dolist (s agents)
      (let ((mark (ghostherd-memory-page--editor-mark s)))
        (dolist (rel (ghostherd-memory-page--edited-under
                      (ghostherd-session-name s)
                      (ghostherd-memory-page--agent-root s)))
          (unless (gethash rel by-file) (push rel order))
          (puthash rel (cons mark (gethash rel by-file)) by-file))))
    (mapcar (lambda (rel)
              (list :file rel :by (nreverse (gethash rel by-file))))
            (nreverse order))))

(add-hook 'pr-view-local-editors-functions #'ghostherd-memory-page--editors)

(defun ghostherd-memory-page--open-changes (intent)
  "Open in pr-view what the checkout INTENT names has not committed."
  (let ((root (alist-get 'root intent)))
    (unless (ghostherd-memory-page--herd-checkout-p root)
      (user-error "No such checkout here"))
    (unless (ghostherd-memory-page--have 'pr-view-changes 'pr-view)
      (user-error "pr-view shows a checkout's changes; it is not here"))
    (pr-view-changes root)))

(defun ghostherd-memory-page--task (id)
  "Task ID as the page shows it."
  (let* ((entry (gethash id ghostherd-memory-page--task-briefs))
         (brief (plist-get entry :brief))
         (status (alist-get 'status brief)))
    (list :id id
          :name (ghostherd-memory-page--str (alist-get 'name brief))
          :status (ghostherd-memory-page--str (alist-get 'name status))
          :color (ghostherd-memory-page--str (alist-get 'color status))
          :url (ghostherd-memory-page--str (alist-get 'url brief))
          :error (ghostherd-memory-page--str (alist-get 'error brief))
          :done (if (member (alist-get 'type status) '("closed" "done")) t :json-false)
          :loading (if (and (null brief) entry) t :json-false))))

(defun ghostherd-memory-page--work (co)
  "Checkout CO as the page shows it: its branch, PR and tasks."
  (let* ((entry (gethash co ghostherd-memory-page--prs))
         (pr (plist-get entry :pr))
         (err (alist-get 'error pr))
         (tasks (and (not err) (alist-get 'tasks pr))))
    (list :root (car co)
          :branch (cdr co)
          :changes (let ((stat (gethash (car co) ghostherd-memory-page--changes)))
                     (if (and stat (> (plist-get stat :files) 0))
                         (list :files (plist-get stat :files)
                               :added (plist-get stat :added)
                               :deleted (plist-get stat :deleted)
                               ;; Of them, files git does not track yet.
                               :untracked (or (plist-get stat :untracked) 0)
                               :untrackedAdded (or (plist-get stat :untracked-added) 0))
                       :json-false))
          :pending (if (and (plist-get entry :pending) (null pr)) t :json-false)
          :error (ghostherd-memory-page--str err)
          :pr (if (and pr (not err))
                  (list :id (alist-get 'id pr)
                        :title (ghostherd-memory-page--str (alist-get 'title pr))
                        :state (ghostherd-memory-page--str (alist-get 'state pr))
                        :draft (if (eq (alist-get 'draft pr) t) t :json-false)
                        :url (ghostherd-memory-page--str (alist-get 'url pr)))
                :json-false)
          :tasks (ghostherd-memory-page--vec (mapcar #'ghostherd-memory-page--task tasks)))))

(defun ghostherd-memory-page--projects-work ()
  "Each project's checkout, as the page shows it, with any idle worktrees
it has left behind."
  (let (out)
    (maphash (lambda (k co)
               (when (and (stringp k) (consp co))
                 (push (append (list :project k
                                     :strays (ghostherd-memory-page--vec
                                              (mapcar (lambda (pb)
                                                        (list :path (abbreviate-file-name (car pb))
                                                              :branch (or (cdr pb) "")))
                                                      (gethash k ghostherd-memory-page--strays))))
                               (ghostherd-memory-page--work co))
                       out)))
             ghostherd-memory-page--checkouts)
    (ghostherd-memory-page--vec (nreverse out))))

(defun ghostherd-memory-page--worktree-deps (s)
  "S's worktree dependencies that could not be carried in, for the card: a
vector of {name, reason}; :json-false when all were fine or absent."
  (let ((skipped (seq-filter
                  (lambda (d) (and (eq (plist-get (cdr d) :status) 'skipped)
                                   (not (member (plist-get (cdr d) :reason) '("no-source" "exists")))))
                  (and (fboundp 'ghostherd-session-worktree-deps)
                       (ghostherd-session-worktree-deps s)))))
    (if skipped
        (ghostherd-memory-page--vec
         (mapcar (lambda (d) (list :name (car d) :reason (or (plist-get (cdr d) :reason) "")))
                 skipped))
      :json-false)))

(defun ghostherd-memory-page--agent-work (s)
  "S's checkout as the page shows it, when it is not its project's.
A herd-made worktree also carries how far it is ahead of its base, its
size on disk (both measured and cached by the look), and any dependency
that could not be carried in."
  (let ((co (gethash (cons 'agent (ghostherd-session-name s)) ghostherd-memory-page--checkouts))
        (project (gethash (ghostherd--project-name (ghostherd-session-project s))
                          ghostherd-memory-page--checkouts)))
    (if (and (consp co) (not (equal co project)))
        (let ((w (ghostherd-memory-page--work co))
              (wt (ghostherd-session-worktree s))
              (id (ghostherd-session-id s)))
          (if wt
              (append w (list :base (or (plist-get wt :base) "")
                              :ahead (or (gethash id ghostherd-memory-page--ahead) :json-false)
                              :size (or (gethash id ghostherd-memory-page--size) :json-false)
                              :deps (ghostherd-memory-page--worktree-deps s)))
            w))
      :json-false)))

(defconst ghostherd-memory-page--lead-named 6
  "Most pull requests and tasks a lead is told of that only its project's
conversations name.")

(defun ghostherd-memory-page--line (s)
  "S on one line, short enough to read."
  (truncate-string-to-width (replace-regexp-in-string "[\n\t ]+" " " (string-trim s))
                            120 nil nil "…"))

(defun ghostherd-memory-page--pr-said (pr)
  "PR, as the page has it, as a lead is told it; nil when it is not known."
  (when (and (consp pr) (plist-get pr :id) (not (plist-get pr :error)))
    (concat (if (eq (plist-get pr :own) :json-false)
                (format "pull request %s/%s#%s" (plist-get pr :owner) (plist-get pr :repo) (plist-get pr :id))
              (format "pull request #%s" (plist-get pr :id)))
            (when-let* ((state (plist-get pr :state))) (concat ", " (downcase state)))
            (when (eq (plist-get pr :draft) t) ", draft")
            (when-let* ((title (plist-get pr :title))) (concat ": " (ghostherd-memory-page--line title)))
            (when-let* ((url (plist-get pr :url))) (concat " -- " url)))))

(defun ghostherd-memory-page--task-said (task)
  "TASK, as the page has it, as a lead is told it."
  (concat (format "ClickUp task %s" (plist-get task :id))
          (when-let* ((status (plist-get task :status))) (concat ", " status))
          (when-let* ((name (plist-get task :name))) (concat ": " (ghostherd-memory-page--line name)))
          (when-let* ((url (plist-get task :url))) (concat " -- " url))))

(defun ghostherd-memory-page--lead-work (project)
  "What PROJECT works on, in lines for its lead: its checkout's branch,
pull request and the tasks that cites; an agent's, where it is on
another; then the open pull requests and tasks its agents'
conversations name, the most recently named first.  As the page's last
look found them -- starting another when that one is old."
  (when (> (- (float-time) ghostherd-memory-page--work-at) ghostherd-memory-page--work-every)
    (ghostherd-memory-page--work-look))
  (let* ((agents (sort (seq-filter (lambda (s) (equal (ghostherd--project-name (ghostherd-session-project s))
                                                      project))
                                   (ghostherd-sessions))
                       (lambda (a b) (string< (ghostherd-session-name a) (ghostherd-session-name b)))))
         (home (gethash project ghostherd-memory-page--checkouts))
         (named 0)
         seen lines)
    (cl-labels ((new-p (key) (unless (member key seen) (push key seen)))
                (checkout (co who)
                  (let* ((work (ghostherd-memory-page--work co))
                         (pr (plist-get work :pr))
                         (whose (if who (concat who "'s") "its")))
                    (push (format "%sbranch %s in %s" (if who (concat who " works on ") "") (cdr co) (car co))
                          lines)
                    (when-let* ((said (ghostherd-memory-page--pr-said pr)))
                      (new-p (or (plist-get pr :url) (cons (car co) (plist-get pr :id))))
                      (push (concat whose " " said) lines))
                    (seq-doseq (task (plist-get work :tasks))
                      (when (new-p (plist-get task :id))
                        (push (concat "a ticket " whose " pull request names: "
                                      (ghostherd-memory-page--task-said task))
                              lines))))))
      (when (consp home) (checkout home nil))
      (dolist (s agents)
        (let ((co (gethash (cons 'agent (ghostherd-session-name s)) ghostherd-memory-page--checkouts)))
          (when (and (consp co) (not (equal co home)))
            (checkout co (ghostherd-session-name s)))))
      (dolist (s agents)
        (let ((links (ghostherd-memory-page--agent-links s))
              (in (format "named in %s's conversation: " (ghostherd-session-name s))))
          (when (consp links)
            (seq-doseq (pr (plist-get links :prs))
              (when (and (< named ghostherd-memory-page--lead-named)
                         (equal (plist-get pr :state) "OPEN")
                         (new-p (or (plist-get pr :url) (ghostherd-memory-page--pr-key pr))))
                (cl-incf named)
                (push (concat in (ghostherd-memory-page--pr-said pr)) lines)))
            (seq-doseq (task (plist-get links :tasks))
              (when (and (< named ghostherd-memory-page--lead-named)
                         (plist-get task :name)
                         (not (eq (plist-get task :done) t))
                         (new-p (plist-get task :id)))
                (cl-incf named)
                (push (concat in (ghostherd-memory-page--task-said task)) lines)))))))
    (nreverse lines)))

(add-hook 'ghostherd-project-work-functions #'ghostherd-memory-page--lead-work)

;; Pushes: the page in front of you holds them back, and a push links to
;; its room -- under the address that keeps the page's secret out of the
;; chat's server.

(defun ghostherd-memory-page--present-p ()
  "Non-nil when the herd page is in front of you, here or on a phone."
  (xwapp-seen-p ghostherd-memory-page--app))

(add-hook 'ghostherd-present-functions #'ghostherd-memory-page--present-p)

(defun ghostherd-memory-page--room-url (project)
  "The address of PROJECT's room, as its chat, from your other devices --
without the page's secret -- or of the page's Agents tab, for nil; nil
when the page is served to this machine alone."
  (when xwapp-remote-hosts
    (xwapp-remote-open-url ghostherd-memory-page--app
                           (if project
                               (concat "room/chat/" (url-hexify-string project))
                             "agents"))))

(setq ghostherd-room-url-function #'ghostherd-memory-page--room-url)

(defun ghostherd-memory-page--open-pr (intent)
  "Open the pull request INTENT names: of a checkout the herd is in, by
its ROOT; or one a conversation named, by its REPO (\"github/owner/repo\")
-- with no checkout of it here."
  (let ((root (alist-get 'root intent))
        (repo (alist-get 'repo intent))
        (id (alist-get 'id intent)))
    (unless (integerp id)
      (user-error "No such pull request here"))
    (cond
     ((and (stringp root)
           (or (seq-some (lambda (co) (equal (car co) root))
                         (hash-table-keys ghostherd-memory-page--prs))
               (gethash root ghostherd-memory-page--remotes)))
      (pr-view-open-pr root id))
     ((and (stringp repo)
           (gethash (append (split-string repo "/") (list id)) ghostherd-memory-page--pr-briefs))
      (pcase-let ((`(,host ,owner ,name) (split-string repo "/")))
        (pr-view-open-remote-pr (ghostherd-memory-page--pr-remote (list :host host :owner owner :repo name))
                                id)))
     (t (user-error "No such pull request here")))))

(defun ghostherd-memory-page--open-task (intent)
  "Open the ClickUp task INTENT names: in clickup-view, else the browser."
  (let ((id (alist-get 'id intent)))
    (unless (and (stringp id) (string-match-p "\\`[0-9a-z]+\\'" id))
      (user-error "No such task"))
    (if (ghostherd-memory-page--have 'clickup-view-task 'clickup-view)
        (clickup-view-task id)
      (browse-url (concat "https://app.clickup.com/t/" id)))))


(defun ghostherd-memory-page--opens-elsewhere (var)
  "Whether the app in VAR opens on your other devices by its address
that holds no secret (`xwapp-remote-open-url'): loaded, and able to open
by itself.  Not loaded, a pull request or task chip there goes to the
forge or ClickUp instead, which want their own login."
  (if (and xwapp-remote-hosts (boundp var) (xwapp-reopen (symbol-value var)))
      t
    :json-false))

(defun ghostherd-memory-page--send-herd ()
  "Send the herd, its asks and the usage to the page."
  (ghostherd-memory-page--js
   "renderHerd"
   (list :agents (ghostherd-memory-page--vec
                  (mapcar #'ghostherd-memory-page--agent (ghostherd-sessions)))
         :asks (ghostherd-memory-page--vec
                (mapcar #'ghostherd-memory-page--ask ghostherd--herd-asks))
         :hooks (ghostherd-memory-page--hooks-payload)
         :work (ghostherd-memory-page--projects-work)
         :usage (ghostherd-memory-page--usage)
         :open (list :pr (ghostherd-memory-page--opens-elsewhere 'pr-view--app)
                     :task (ghostherd-memory-page--opens-elsewhere 'clickup-view--app))
         :kinds (ghostherd-memory-page--vec
                 (mapcar #'symbol-name
                         (seq-filter (lambda (k) (plist-get (ghostherd--spec k) :command))
                                     (ghostherd--agent-kinds))))
         :pickers (ghostherd-memory-page--pickers)
         ;; What guarded agents' guards refused, and what was approved once.
         :guard (ghostherd-memory-page--vec
                 (mapcar (lambda (r)
                           (list :id (plist-get r :id) :session (plist-get r :session)
                                 :tool (or (plist-get r :tool) "") :summary (or (plist-get r :summary) "")
                                 :reason (or (plist-get r :reason) "") :rule (or (plist-get r :rule) "")
                                 :kind (or (plist-get r :kind) "")
                                 :approvable (if (eq (plist-get r :approvable) t) t :json-false)
                                 :status (or (plist-get r :status) "")
                                 :created (plist-get r :created_ms)))
                         (and (boundp 'ghostherd--guard-requests) ghostherd--guard-requests)))
         ;; Primary, the herd-wide lead: where its room is, up or not --
         ;; the Agents tab opens it from its header, and the room's chat
         ;; starts it with the first message.
         :primary (let ((s (ghostherd--primary-session)))
                    (list :directory (ghostherd--project-name (ghostherd--primary-dir))
                          :name ghostherd--primary-name
                          :kind (symbol-name ghostherd-primary-kind)
                          :state (if (ghostherd--primary-up s)
                                     (symbol-name (ghostherd-session-state s))
                                   "")))
         ;; Where an agent's work can be relayed, the herd's choice first,
         ;; and which of them have no usage left.
         :relayKinds (ghostherd-memory-page--vec
                      (mapcar (lambda (k)
                                (list :kind (symbol-name k)
                                      :usedUp (or (ghostherd-usage-used-up-text k) "")))
                              (ghostherd--relay-kinds))))))

(defun ghostherd-memory-page--herd-tick ()
  "One snapshot; usage too when it is due and the page can be seen."
  (if (not (xwapp-live-p ghostherd-memory-page--app))
      (ghostherd-memory-page--herd-unwatch)
    (when (> (- (float-time) ghostherd-memory-page--work-at) ghostherd-memory-page--work-every)
      (ghostherd-memory-page--work-look))
    (when (and (> (- (float-time) ghostherd-memory-page--usage-at) ghostherd-usage-interval)
               (xwapp-seen-p ghostherd-memory-page--app))
      (setq ghostherd-memory-page--usage-at (float-time))
      (ghostherd-memory-page--chore #'ghostherd-usage-refresh))
    (when (> (- (float-time) ghostherd-memory-page--hooks-at) ghostherd-memory-page--hooks-every)
      (ghostherd-memory-page--hooks-refresh))
    (ghostherd-memory-page--meter-look)
    (ghostherd-memory-page--subs-look)
    (ghostherd-memory-page--edits-look)
    (ghostherd-memory-page--send-herd)))

(defun ghostherd-memory-page--herd-watch ()
  "Keep the Agents tab current while it is on screen."
  (ghostherd-memory-page--herd-unwatch)
  (setq ghostherd-memory-page--work-at 0)
  (setq ghostherd-memory-page--herd-timer
        (run-at-time 0 ghostherd-memory-page--herd-every #'ghostherd-memory-page--herd-tick)))

(defun ghostherd-memory-page--herd-unwatch ()
  (when (timerp ghostherd-memory-page--herd-timer)
    (cancel-timer ghostherd-memory-page--herd-timer))
  (setq ghostherd-memory-page--herd-timer nil))

(defun ghostherd-memory-page--on-herd-change (_entry)
  "A log entry is a change the Agents tab should show now, not in 5s."
  (when (timerp ghostherd-memory-page--herd-timer)
    (ghostherd-memory-page--send-herd)))

(add-hook 'ghostherd-log-functions #'ghostherd-memory-page--on-herd-change)


;;; The Memory tab: the shared-memory facts, live, editable
;;
;; The page watches one store (the user store or a project's) and the herd
;; pushes its facts, re-read from the sidecar every few seconds, so a fact
;; an agent's `herd remember' or claude's own write adds elsewhere appears
;; without a reload.  Edits and deletes go through the store's own code
;; (the index regenerated, provenance updated), guarded against path tricks
;; sidecar-side.

(defconst ghostherd-memory-page--memory-every 3
  "Seconds between looks at the shared-memory facts the tab shows.")
(defvar ghostherd-memory-page--memory-key nil
  "The store key (a slug, or `user') the Memory tab watches, or nil.")
(defvar ghostherd-memory-page--memory-timer nil)
(defvar ghostherd-memory-page--memory-sig nil
  "A signature of the facts last pushed, so an unchanged store is not re-sent
and the page is not re-rendered under an open editor every few seconds.")
(defvar ghostherd-memory-page--proposals-sig nil
  "A signature of the promotion proposals last pushed, like `--memory-sig'.")

(defun ghostherd-memory-page--memory-send-projects ()
  "Push the picker -- the user store and each project store -- to the page."
  (ghostherd-memory-request-async
   "herd_memory_projects"
   (lambda (result)
     (ghostherd-memory-page--js
      "setMemoryProjects"
      (list :user (plist-get result :user)
            :projects (ghostherd-memory-page--vec (plist-get result :projects)))))
   nil
   (ghostherd-memory-page--fail "Memory projects")))

(defun ghostherd-memory-page--memory-tick ()
  "Fetch the watched store's facts; push them to the page when they changed."
  (if (not (and ghostherd-memory-page--memory-key
                (xwapp-live-p ghostherd-memory-page--app)))
      (ghostherd-memory-page--memory-unwatch)
    (let ((key ghostherd-memory-page--memory-key))
      (ghostherd-memory-request-async
       "herd_memory_list"
       (lambda (result)
         (when (equal key ghostherd-memory-page--memory-key)
           (let* ((facts (ghostherd-memory-page--vec (plist-get result :facts)))
                  (sig (sxhash-equal (list key (append facts nil)))))
             (unless (equal sig ghostherd-memory-page--memory-sig)
               (setq ghostherd-memory-page--memory-sig sig)
               (ghostherd-memory-page--js
                "renderMemory"
                (list :key (or (plist-get result :key) key) :facts facts))))))
       ;; The watched store is a slug key (the picker, or `user') or a project
       ;; path (the default / a room link, which has a `/').  The sidecar's
       ;; `key' must be a slug; a path goes as `project' for it to slug.
       (ghostherd-memory-page--memory-where key)
       (ghostherd-memory-page--fail "Memory"))
      (ghostherd-memory-page--memory-proposals-tick key))))

(defun ghostherd-memory-page--memory-where (key)
  "The RPC params for watched store KEY: `:project' for a path, else `:key'."
  (if (string-match-p "/" key) (list :project key) (list :key key)))

(defun ghostherd-memory-page--proposal-for-page (p)
  "Proposal P with its `:facts' as a vector.  Each fact is a plist and the
facts come back from json.el as a list of them; left a list, `json-encode'
(xwapp-js) would re-read that as ONE alist -- collapsing every fact into a
single bogus {\"name\":[…]} object -- so the page's `renderMemProposal' throws
on `p.facts.map' and the whole Proposals section shows nothing.  A vector
encodes as the JSON array the page maps over."
  (let ((c (copy-sequence p)))
    (plist-put c :facts (ghostherd-memory-page--vec (plist-get p :facts)))))

(defun ghostherd-memory-page--memory-proposals-tick (key)
  "Fetch KEY's promotion proposals; push them when they changed."
  (ghostherd-memory-request-async
   "herd_memory_proposals"
   (lambda (result)
     (when (equal key ghostherd-memory-page--memory-key)
       (let* ((props (ghostherd-memory-page--vec
                      (mapcar #'ghostherd-memory-page--proposal-for-page
                              (plist-get result :proposals))))
              (sig (sxhash-equal (list key (append props nil)))))
         (unless (equal sig ghostherd-memory-page--proposals-sig)
           (setq ghostherd-memory-page--proposals-sig sig)
           (ghostherd-memory-page--js
            "renderProposals"
            (list :key (or (plist-get result :key) key) :proposals props))))))
   (ghostherd-memory-page--memory-where key)
   ;; Silent: proposals are a side panel, and the RPC may not be there yet
   ;; (before a sidecar restart lands it).  A failed facts fetch still shows.
   #'ignore))

(defun ghostherd-memory-page--memory-watch (intent)
  "Watch the store INTENT names (its `key', or a `project' path), pushing its
facts live.  Re-emitting with another key switches the watched store."
  (let ((key (ghostherd-memory-page--str (alist-get 'key intent)))
        (project (ghostherd-memory-page--str (alist-get 'project intent))))
    (setq ghostherd-memory-page--memory-key (or key project "user")
          ghostherd-memory-page--memory-sig nil     ; force a push for the new key
          ghostherd-memory-page--proposals-sig nil)
    (ghostherd-memory-page--memory-send-projects)
    (unless (timerp ghostherd-memory-page--memory-timer)
      (setq ghostherd-memory-page--memory-timer
            (run-at-time 0 ghostherd-memory-page--memory-every
                         #'ghostherd-memory-page--memory-tick)))
    (ghostherd-memory-page--memory-tick)))

(defun ghostherd-memory-page--memory-unwatch ()
  (when (timerp ghostherd-memory-page--memory-timer)
    (cancel-timer ghostherd-memory-page--memory-timer))
  (setq ghostherd-memory-page--memory-timer nil
        ghostherd-memory-page--memory-key nil
        ghostherd-memory-page--memory-sig nil
        ghostherd-memory-page--proposals-sig nil))

(defun ghostherd-memory-page--memory-edit (intent)
  "Save a fact's edit (description, type, body) through the store, then refresh."
  (let ((key (ghostherd-memory-page--str (alist-get 'key intent)))
        (name (ghostherd-memory-page--str (alist-get 'name intent)))
        (description (or (ghostherd-memory-page--str (alist-get 'description intent)) ""))
        (type (ghostherd-memory-page--str (alist-get 'type intent)))
        (body (let ((b (alist-get 'body intent))) (if (stringp b) b ""))))
    (unless (and key name) (user-error "No fact"))
    (ghostherd-memory-request-async
     "herd_memory_edit"
     (lambda (_r)
       (setq ghostherd-memory-page--memory-sig nil)  ; force a refresh now
       (ghostherd-memory-page--memory-tick)
       (ghostherd-memory-page--js "flash" "Saved"))
     (append (list :key key :name name :description description :body body)
             (and type (list :type type)))
     (ghostherd-memory-page--fail "Saving fact"))))

(defun ghostherd-memory-page--memory-delete (intent)
  "Delete a fact through the store (the page's confirmed delete), then refresh."
  (let ((key (ghostherd-memory-page--str (alist-get 'key intent)))
        (name (ghostherd-memory-page--str (alist-get 'name intent))))
    (unless (and key name) (user-error "No fact"))
    (ghostherd-memory-request-async
     "herd_memory_delete"
     (lambda (_r)
       (setq ghostherd-memory-page--memory-sig nil)
       (ghostherd-memory-page--memory-tick)
       (ghostherd-memory-page--js "flash" "Deleted"))
     (list :key key :name name)
     (ghostherd-memory-page--fail "Deleting fact"))))

(defun ghostherd-memory-page--memory-proposal-approve (intent)
  "Approve a promotion proposal: the herd asks the lead to apply its diff."
  (let ((id (ghostherd-memory-page--str (alist-get 'id intent))))
    (unless id (user-error "No proposal"))
    (ghostherd-memory-request-async
     "herd_memory_promote_approve"
     (lambda (r)
       (setq ghostherd-memory-page--proposals-sig nil
             ghostherd-memory-page--memory-sig nil)
       (ghostherd-memory-page--memory-tick)
       (ghostherd-memory-page--js
        "flash" (format "Approved — asked %s to apply it" (or (plist-get r :to) "the lead"))))
     (list :id id)
     (ghostherd-memory-page--fail "Approving proposal"))))

(defun ghostherd-memory-page--memory-proposal-reject (intent)
  "Reject a promotion proposal, recording why; the facts stay memories."
  (let ((id (ghostherd-memory-page--str (alist-get 'id intent)))
        (reason (or (ghostherd-memory-page--str (alist-get 'reason intent)) "")))
    (unless id (user-error "No proposal"))
    (ghostherd-memory-request-async
     "herd_memory_promote_reject"
     (lambda (_r)
       (setq ghostherd-memory-page--proposals-sig nil)
       (ghostherd-memory-page--memory-tick)
       (ghostherd-memory-page--js "flash" "Rejected"))
     (list :id id :reason reason)
     (ghostherd-memory-page--fail "Rejecting proposal"))))

(defun ghostherd-memory-page--memory-unpromote (intent)
  "Put a promoted fact back into the memory."
  (let ((key (ghostherd-memory-page--str (alist-get 'key intent)))
        (name (ghostherd-memory-page--str (alist-get 'name intent))))
    (unless (and key name) (user-error "No fact"))
    (ghostherd-memory-request-async
     "herd_memory_unpromote"
     (lambda (_r)
       (setq ghostherd-memory-page--memory-sig nil)
       (ghostherd-memory-page--memory-tick)
       (ghostherd-memory-page--js "flash" "Back in memory"))
     (list :key key :name name)
     (ghostherd-memory-page--fail "Unpromoting fact"))))

(defun ghostherd-memory-page--memory-suggest (intent)
  "Ask the project's lead (or a chosen agent) to review memory and propose."
  (let ((key (ghostherd-memory-page--str (alist-get 'key intent)))
        (project (ghostherd-memory-page--str (alist-get 'project intent)))
        (agent (ghostherd-memory-page--str (alist-get 'agent intent))))
    (ghostherd-memory-request-async
     "herd_memory_suggest"
     (lambda (r)
       (ghostherd-memory-page--js
        "flash" (format "Asked %s to suggest promotions" (or (plist-get r :to) "the lead"))))
     (append (ghostherd-memory-page--memory-where (or key project "user"))
             (and agent (list :agent agent)))
     (ghostherd-memory-page--fail "Suggesting promotions"))))


;;; A project's room: its agents' conversations side by side
;;
;; Each agent's transcript, read by the sidecar from its tail: what was
;; said to it -- by you, or by another agent through the herd -- and
;; what it answered.  Emacs only stats the files; it asks the sidecar
;; again when one has changed, and never parses a transcript itself.
;; An agent whose transcript is not known shows its screen instead.

(defconst ghostherd-memory-page--room-every 2
  "Seconds between looks at the room's transcripts and screens.")

(defvar ghostherd-memory-page--room nil
  "The project whose room the page shows, as the page names it, or nil.")
(defvar ghostherd-memory-page--room-screens nil
  "Names of the room's agents the page shows by their screen.")
(defvar ghostherd-memory-page--room-timer nil)
(defvar ghostherd-memory-page--room-sent (make-hash-table :test 'equal)
  "Agent name -> the transcript signature last sent for it, or `none'.")
(defvar ghostherd-memory-page--room-reading (make-hash-table :test 'equal)
  "Agent name -> t while the sidecar reads its transcript.")
(defvar ghostherd-memory-page--room-chat nil
  "Non-nil while the page shows the room as one chat, not in columns.")
(defvar ghostherd-memory-page--chat-sent nil
  "The transcripts' signatures the chat was last sent for.")
(defvar ghostherd-memory-page--chat-reading nil
  "Non-nil while the sidecar builds the chat.")
(defvar ghostherd-memory-page--room-limits (make-hash-table :test 'equal)
  "Agent name -> how many entries its column asked for, when more than the
default; nil for the default.  Kept so a `load earlier' does not shrink
back on the next poll.")
(defvar ghostherd-memory-page--chat-limit nil
  "How many items the chat asked for, when more than the default, or nil.")
(defvar ghostherd-memory-page--sub nil
  "The sub-agent transcript shown over the room: (NAME PATH SID), where
NAME is the parent agent and PATH its transcript; nil when none is open.")
(defvar ghostherd-memory-page--sub-sent nil
  "The signature last sent for the open sub-agent's transcript.")
(defvar ghostherd-memory-page--sub-reading nil
  "Non-nil while the sidecar reads the open sub-agent's transcript.")

(defun ghostherd-memory-page--forget-sidecar-answers ()
  "Drop every cache that binds an agent to what the sidecar last answered.
Each is keyed by a transcript's signature.  A restarted sidecar is a new
instance, and an idle agent's transcript will not change to re-trigger the
ask, so without this its meter -- and its sub-agents, its links and the
open conversations -- would stay bound to what the old sidecar said, empty
for one too old to read it.  Not the per-column `load earlier' limits:
those are the reader's intent, not the sidecar's answer."
  (dolist (tbl (list ghostherd-memory-page--meters ghostherd-memory-page--metering
                     ghostherd-memory-page--subs ghostherd-memory-page--subs-reading
                     ghostherd-memory-page--links
                     ghostherd-memory-page--room-sent ghostherd-memory-page--room-reading))
    (clrhash tbl))
  (setq ghostherd-memory-page--chat-sent nil
        ghostherd-memory-page--chat-reading nil
        ghostherd-memory-page--sub-sent nil
        ghostherd-memory-page--sub-reading nil))

(add-hook 'ghostherd-memory-stopped-hook #'ghostherd-memory-page--forget-sidecar-answers)

(defun ghostherd-memory-page--room-shows-p (s)
  "Non-nil when the page's room shows agent S and you could be looking:
the page in front of you, on S's project's room.  The columns show
every agent's conversation, or its screen when its transcript is not
known; the chat only those whose transcript is.  So a `done' S there is
seen, as it is when its buffer is in front of you."
  (and ghostherd-memory-page--room
       (equal (ghostherd--project-name (ghostherd-session-project s)) ghostherd-memory-page--room)
       (or (not ghostherd-memory-page--room-chat) (ghostherd-session-transcript s))
       (xwapp-seen-p ghostherd-memory-page--app)))

(add-hook 'ghostherd-watched-functions #'ghostherd-memory-page--room-shows-p)

(defun ghostherd-memory-page--room-sessions ()
  "The herd's agents in the room's project."
  (seq-filter (lambda (s) (equal (or (ghostherd--project-name (ghostherd-session-project s)) "")
                                 ghostherd-memory-page--room))
              (ghostherd-sessions)))

(defun ghostherd-memory-page--room-signature (path)
  "PATH with its size and mtime: a change in any is a new conversation."
  (when-let* ((a (and path (file-attributes path))))
    (list path (file-attribute-size a)
          (float-time (file-attribute-modification-time a)))))

(defun ghostherd-memory-page--room-entry (e)
  "Conversation entry E, from the sidecar, as the page takes it.
Field by field: what reaches the page is what is named here."
  (let ((str #'ghostherd-memory-page--str))
    (list :role (funcall str (plist-get e :role))
          :kind (funcall str (plist-get e :kind))
          :who (funcall str (plist-get e :who))
          :to (funcall str (plist-get e :to))
          :ask (funcall str (plist-get e :ask))
          :text (funcall str (plist-get e :text))
          :answer (plist-get e :answer)
          :ts (plist-get e :ts)
          :auto (if (plist-get e :auto) t :json-false)
          :failed (if (plist-get e :failed) t :json-false)
          :waiting (if (plist-get e :waiting) t :json-false)
          ;; A sub-agent's report: which, what it was asked, how it ended
          ;; and what it took.
          :sub (funcall str (plist-get e :sub))
          :title (funcall str (plist-get e :title))
          :status (funcall str (plist-get e :status))
          :tokens (and (natnump (plist-get e :tokens)) (plist-get e :tokens))
          :uses (and (natnump (plist-get e :uses)) (plist-get e :uses))
          :ms (and (natnump (plist-get e :ms)) (plist-get e :ms))
          :tools (ghostherd-memory-page--vec
                  (mapcar (lambda (tl) (list :name (funcall str (plist-get tl :name))
                                             :hint (funcall str (plist-get tl :hint))
                                             :sub (funcall str (plist-get tl :sub))
                                             :state (funcall str (plist-get tl :state))))
                          (plist-get e :tools))))))

(defun ghostherd-memory-page--sidecar-error (e)
  "What the page says when the sidecar failed with E."
  (if (string-match-p "Method not found" (format "%s" e))
      ;; Emacs reloaded, the sidecar did not.
      "the sidecar is older than the room: M-x ghostherd-memory-stop, then M-x ghostherd-memory-start"
    (format "%s" e)))

(defun ghostherd-memory-page--chat-entry (e)
  "Chat item E, from the sidecar, as the page takes it, field by field."
  (let ((str #'ghostherd-memory-page--str))
    (list :who (funcall str (plist-get e :who))
          :to (funcall str (plist-get e :to))
          :kind (funcall str (plist-get e :kind))
          :ask (funcall str (plist-get e :ask))
          :text (funcall str (plist-get e :text))
          :answer (plist-get e :answer)
          :ts (plist-get e :ts)
          :agent (funcall str (plist-get e :agent))
          :auto (if (plist-get e :auto) t :json-false)
          :failed (if (plist-get e :failed) t :json-false)
          :waiting (if (plist-get e :waiting) t :json-false))))

(defun ghostherd-memory-page--chat-read ()
  "Send the room's chat if any of its agents' transcripts changed.
One timeline for the project, which the sidecar builds from them all."
  (let* ((room ghostherd-memory-page--room)
         (sessions (ghostherd-memory-page--room-sessions))
         (known (delq nil (mapcar (lambda (s)
                                    (when-let* ((path (ghostherd-session-transcript s)))
                                      (cons (ghostherd-session-name s) path)))
                                  sessions)))
         (absent (seq-remove (lambda (n) (assoc n known))
                             (mapcar #'ghostherd-session-name sessions)))
         (limit ghostherd-memory-page--chat-limit)
         ;; The limit is part of the signature, so `load earlier' re-reads.
         (sig (list limit absent (mapcar (lambda (a) (cons (car a) (ghostherd-memory-page--room-signature (cdr a))))
                                         known))))
    (unless (or (equal sig ghostherd-memory-page--chat-sent) ghostherd-memory-page--chat-reading)
      (setq ghostherd-memory-page--chat-reading t)
      (let ((done (lambda (&rest fields)
                    (setq ghostherd-memory-page--chat-reading nil
                          ghostherd-memory-page--chat-sent sig)
                    (when (and (equal room ghostherd-memory-page--room) ghostherd-memory-page--room-chat)
                      (ghostherd-memory-page--js
                       "setChat" (append (list :project room :absent (ghostherd-memory-page--vec absent))
                                         fields))))))
        (condition-case err
            (ghostherd-memory-request-async
             "herd_chat"
             (lambda (r)
               (funcall done
                        :earlier (if (plist-get r :earlier) t :json-false)
                        :limit (and (natnump (plist-get r :limit)) (plist-get r :limit))
                        :missing (ghostherd-memory-page--vec
                                  (mapcar #'ghostherd-memory-page--str (plist-get r :missing)))
                        :entries (ghostherd-memory-page--vec
                                  (mapcar #'ghostherd-memory-page--chat-entry (plist-get r :entries)))))
             (append (list :agents (vconcat (mapcar (lambda (a) (list :name (car a) :path (cdr a))) known)))
                     (and limit (list :limit limit)))
             (lambda (e) (funcall done :entries [] :error (ghostherd-memory-page--sidecar-error e))))
          (error (funcall done :entries [] :error (error-message-string err))))))))

(defun ghostherd-memory-page--room-send (name &rest fields)
  (ghostherd-memory-page--js "setConversation" (append (list :name name) fields)))

(defun ghostherd-memory-page--room-read (s)
  "Send S's conversation to the page if its transcript changed, and its
screen if that is what the page shows of it."
  (let* ((name (ghostherd-session-name s))
         (room ghostherd-memory-page--room)
         (path (ghostherd-session-transcript s))
         (limit (gethash name ghostherd-memory-page--room-limits))
         ;; The limit is part of the signature, so `load earlier' re-reads.
         (sig (list (ghostherd-memory-page--room-signature path) limit)))
    (cond
     ((null (car sig))
      (unless (eq (gethash name ghostherd-memory-page--room-sent) 'none)
        (puthash name 'none ghostherd-memory-page--room-sent)
        (ghostherd-memory-page--room-send name :entries [] :none t)))
     ((or (equal sig (gethash name ghostherd-memory-page--room-sent))
          (gethash name ghostherd-memory-page--room-reading)))
     (t
      (puthash name t ghostherd-memory-page--room-reading)
      (let ((done (lambda (&rest fields)
                    (remhash name ghostherd-memory-page--room-reading)
                    (puthash name sig ghostherd-memory-page--room-sent)
                    (when (equal room ghostherd-memory-page--room)
                      (apply #'ghostherd-memory-page--room-send name :path path fields)))))
        (condition-case err
            (ghostherd-memory-request-async
             "herd_conversation"
             (lambda (r)
               (funcall done
                        :cli (ghostherd-memory-page--str (plist-get r :cli))
                        :earlier (if (plist-get r :earlier) t :json-false)
                        :limit (and (natnump (plist-get r :limit)) (plist-get r :limit))
                        :entries (ghostherd-memory-page--vec
                                  (mapcar #'ghostherd-memory-page--room-entry
                                          (plist-get r :entries)))))
             (if limit (list :path path :limit limit) (list :path path))
             (lambda (e)
               (funcall done :entries [] :error (ghostherd-memory-page--sidecar-error e))))
          (error (funcall done :entries [] :error (error-message-string err)))))))
    (when (or (null (car sig)) (member name ghostherd-memory-page--room-screens))
      (ghostherd-memory-page--js
       "setScreen" (list :name name
                         :text (or (ignore-errors (ghostherd--host-capture s))
                                   (gethash (ghostherd-session-id s) ghostherd--screens)
                                   ""))))))

(defun ghostherd-memory-page--sub-file (path sid)
  "Where sub-agent SID keeps its own transcript, beside parent PATH's."
  (and path sid (expand-file-name (format "subagents/agent-%s.jsonl" sid)
                                  (file-name-sans-extension path))))

(defun ghostherd-memory-page--sub-watch (intent)
  "Begin showing sub-agent SUB of agent NAME, and read it at once so the
page fills without waiting for the next tick.  The parent transcript is
resolved from NAME here, not taken from the page: the chat view keeps no
per-agent conversation, so the path it sends is empty there and serves
only as a fallback."
  (let* ((name (ghostherd-memory-page--str (alist-get 'name intent)))
         (given (ghostherd-memory-page--str (alist-get 'path intent)))
         (sid (ghostherd-memory-page--str (alist-get 'sub intent)))
         (s (and name (ghostherd-get name)))
         (path (or (and s (ghostherd-session-transcript s))
                   (and given (not (string-empty-p given)) given))))
    (when (and name path sid)
      (setq ghostherd-memory-page--sub (list name path sid)
            ghostherd-memory-page--sub-sent nil
            ghostherd-memory-page--sub-reading nil)
      (ghostherd-memory-page--sub-read))))

(defun ghostherd-memory-page--sub-forget ()
  "Stop showing a sub-agent's transcript."
  (setq ghostherd-memory-page--sub nil ghostherd-memory-page--sub-sent nil))

(defun ghostherd-memory-page--sub-read ()
  "Send the open sub-agent's conversation to the page if its transcript
changed, as the room does for a column.  Its records are all sidechain, so
the sidecar reads it with `read_sub', not the room reader."
  (when ghostherd-memory-page--sub
    (pcase-let* ((`(,name ,path ,sid) ghostherd-memory-page--sub)
                 (file (ghostherd-memory-page--sub-file path sid))
                 (sig (ghostherd-memory-page--room-signature file)))
      (unless (or (equal sig ghostherd-memory-page--sub-sent)
                  ghostherd-memory-page--sub-reading)
        (setq ghostherd-memory-page--sub-reading t)
        (let* ((open ghostherd-memory-page--sub)
               (done (lambda (&rest fields)
                       (setq ghostherd-memory-page--sub-reading nil
                             ghostherd-memory-page--sub-sent sig)
                       (when (equal open ghostherd-memory-page--sub)
                         (ghostherd-memory-page--js
                          "setSubConversation" (append (list :name name :sub sid) fields))))))
          (condition-case err
              (ghostherd-memory-request-async
               "herd_sub_conversation"
               (lambda (r)
                 (funcall done
                          :earlier (if (plist-get r :earlier) t :json-false)
                          :entries (ghostherd-memory-page--vec
                                    (mapcar #'ghostherd-memory-page--room-entry (plist-get r :entries)))))
               (list :path path :sub sid)
               (lambda (e) (funcall done :entries [] :error (ghostherd-memory-page--sidecar-error e))))
            (error (funcall done :entries [] :error (error-message-string err)))))))))

(defun ghostherd-memory-page--room-tick ()
  (if (not (and ghostherd-memory-page--room (xwapp-live-p ghostherd-memory-page--app)))
      (ghostherd-memory-page--room-close)
    (if ghostherd-memory-page--room-chat
        (ghostherd-memory-page--chat-read)
      (mapc #'ghostherd-memory-page--room-read (ghostherd-memory-page--room-sessions)))
    (ghostherd-memory-page--sub-read)))

(defun ghostherd-memory-page--room-open (intent)
  "Show the room INTENT names, its agents in the screens it lists by
their screen, or as one chat when its view is \"chat\".  The same room
again only changes which those are."
  (let* ((project (alist-get 'project intent))
         (screens (seq-filter #'stringp (append (alist-get 'screens intent) nil)))
         ;; The primary room is always a chat -- primary has no columns view
         ;; -- so a page that (on the desktop default) asks for columns still
         ;; gets its conversation, not an empty columns read it waits on
         ;; forever behind "Reading the conversations…".
         (chat (or (equal (alist-get 'view intent) "chat")
                   (and (stringp project)
                        (fboundp 'ghostherd--primary-dir-p)
                        (ghostherd--primary-dir-p project)))))
    (unless (and (stringp project) (not (string-empty-p project)))
      (user-error "No project"))
    (unless (equal project ghostherd-memory-page--room)
      (ghostherd-memory-page--room-close)
      (setq ghostherd-memory-page--room project))
    (let ((mode-changed (not (eq chat ghostherd-memory-page--room-chat))))
      (setq ghostherd-memory-page--room-chat chat)
      ;; Re-send the room's conversation when the page (re)opened it or changed
      ;; its view -- not on a mere screen toggle (same room and view), which
      ;; must not re-read the whole conversation.  `reopen' is set by the page
      ;; only in its openRoom, so a page that RELOADED (losing its chat/columns
      ;; while Emacs kept the room) gets the conversation again, rather than
      ;; the reopened same room reading as unchanged (`--chat-sent'/
      ;; `--room-sent') and sitting on "Reading the conversations…".
      (when (or mode-changed (alist-get 'reopen intent))
        (setq ghostherd-memory-page--chat-sent nil)
        (clrhash ghostherd-memory-page--room-sent)))
    (setq ghostherd-memory-page--room-screens screens)
    ;; How much of each column, and of the chat, the page has expanded with
    ;; `load earlier' -- so the next read stays that big, not back to the
    ;; default.  The limit is part of each read's signature, so a change
    ;; here re-reads on the next tick.
    (dolist (o (append (alist-get 'limits intent) nil))
      (when-let* ((name (ghostherd-memory-page--str (alist-get 'name o)))
                  (n (alist-get 'limit o)))
        (if (natnump n)
            (puthash name n ghostherd-memory-page--room-limits)
          (remhash name ghostherd-memory-page--room-limits))))
    (setq ghostherd-memory-page--chat-limit (and (natnump (alist-get 'chatLimit intent))
                                                 (alist-get 'chatLimit intent)))
    (unless (timerp ghostherd-memory-page--room-timer)
      (setq ghostherd-memory-page--room-timer
            (run-at-time 0 ghostherd-memory-page--room-every
                         #'ghostherd-memory-page--room-tick)))
    (ghostherd-memory-page--room-tick)))

(defun ghostherd-memory-page--room-close ()
  (when (timerp ghostherd-memory-page--room-timer)
    (cancel-timer ghostherd-memory-page--room-timer))
  (setq ghostherd-memory-page--room-timer nil
        ghostherd-memory-page--room nil
        ghostherd-memory-page--room-screens nil
        ghostherd-memory-page--room-chat nil
        ghostherd-memory-page--chat-sent nil
        ghostherd-memory-page--chat-reading nil
        ghostherd-memory-page--sub nil
        ghostherd-memory-page--sub-sent nil
        ghostherd-memory-page--sub-reading nil
        ghostherd-memory-page--chat-limit nil)
  (clrhash ghostherd-memory-page--room-sent)
  (clrhash ghostherd-memory-page--room-limits))

(defun ghostherd-memory-page--room-earlier (intent)
  "Read one earlier page of a room column's conversation -- agent NAME's
transcript, a page ending BEFORE from the newest -- and send it to the
page to prepend.  A one-shot read: the poll keeps refreshing the recent
tail, this fills older history a page at a time (`load earlier')."
  (let* ((name (ghostherd-memory-page--str (alist-get 'name intent)))
         (before (alist-get 'before intent))
         (s (and name (ghostherd-get name)))
         (path (and s (ghostherd-session-transcript s))))
    (when (and name path (natnump before))
      (let ((send (lambda (&rest fields)
                    (ghostherd-memory-page--js
                     "earlierConversation" (append (list :name name :before before) fields)))))
        (condition-case err
            (ghostherd-memory-request-async
             "herd_conversation"
             (lambda (r)
               (funcall send
                        :earlier (if (plist-get r :earlier) t :json-false)
                        :entries (ghostherd-memory-page--vec
                                  (mapcar #'ghostherd-memory-page--room-entry (plist-get r :entries)))))
             (list :path path :before before)
             (lambda (e) (funcall send :entries [] :error (ghostherd-memory-page--sidecar-error e))))
          (error (funcall send :entries [] :error (error-message-string err))))))))

(defun ghostherd-memory-page--chat-earlier (intent)
  "Read one earlier page of the room's merged chat -- a page ending BEFORE
from the newest -- and send it to the page to prepend."
  (let* ((before (alist-get 'before intent))
         (room ghostherd-memory-page--room)
         (known (delq nil (mapcar (lambda (s)
                                    (when-let* ((path (ghostherd-session-transcript s)))
                                      (cons (ghostherd-session-name s) path)))
                                  (ghostherd-memory-page--room-sessions)))))
    (when (and room (natnump before))
      (let ((send (lambda (&rest fields)
                    (ghostherd-memory-page--js
                     "earlierChat" (append (list :before before) fields)))))
        (condition-case err
            (ghostherd-memory-request-async
             "herd_chat"
             (lambda (r)
               (funcall send
                        :earlier (if (plist-get r :earlier) t :json-false)
                        :entries (ghostherd-memory-page--vec
                                  (mapcar #'ghostherd-memory-page--chat-entry (plist-get r :entries)))))
             (list :agents (vconcat (mapcar (lambda (a) (list :name (car a) :path (cdr a))) known))
                   :before before)
             (lambda (e) (funcall send :entries [] :error (ghostherd-memory-page--sidecar-error e))))
          (error (funcall send :entries [] :error (error-message-string err))))))))

(defun ghostherd-memory-page--session (intent)
  "The live herd session INTENT names, or a `user-error'."
  (let ((name (alist-get 'name intent)))
    (or (and (stringp name) (ghostherd-get name))
        (user-error "No agent named %s" name))))

(defun ghostherd-memory-page--agent-act (op intent)
  "Do OP to the agent INTENT names, then show the herd as it now is."
  (let* ((s (ghostherd-memory-page--session intent))
         (name (ghostherd-session-name s))
         (said
          (pcase op
            ("agent-visit" (ghostherd-visit s) nil)
            ("agent-prompt"
             (let ((text (alist-get 'text intent)))
               (unless (and (stringp text) (not (string-empty-p (string-trim text))))
                 (user-error "Nothing to send"))
               (ghostherd-tell s text)
               (format "Sent to %s" name)))
            ("agent-interrupt" (ghostherd-interrupt s) (format "Esc to %s" name))
            ("agent-login"
             ;; The card's one-click /login for an expired-login turn end.
             (ghostherd-send s "/login" t)
             (format "Running /login on %s" name))
            ("agent-sleep-cancel"
             ;; The card's Cancel on a pending continue-after-sleep: it stays
             ;; blocked, and the herd will not carry this turn on.
             (ghostherd-sleep-continue-cancel s)
             (format "%s stays blocked; the herd will not continue it" name))
            ("agent-abort" (ghostherd-abort s) (format "C-c to %s" name))
            ("agent-answer"
             (let ((n (alist-get 'n intent)))
               (unless (and (integerp n) (<= 1 n 9)) (user-error "Choice is 1 to 9"))
               (ghostherd-answer s n)
               (format "Answering %s with %d" name n)))
            ("agent-respawn"
             (ghostherd-respawn s (ghostherd-memory-page--truthy (alist-get 'continue intent)))
             (format "Respawned %s" name))
            ("agent-relay"
             (let ((kind (alist-get 'kind intent)))
               (unless (and (stringp kind) (memq (intern kind) (ghostherd--relay-kinds)))
                 (user-error "No CLI %s to relay to" kind))
               (ghostherd-relay s (intern kind))
               (format "Relaying %s to %s: its heir starts once its work is read" name kind)))
            ("agent-kill" (ghostherd-kill s t) (format "Killed %s" name))
            ("agent-notes"
             (ghostherd-set-notes s (or (alist-get 'text intent) ""))
             (format "Notes for %s saved" name))
            ("agent-model"
             (let ((model (alist-get 'model intent))
                   (effort (alist-get 'effort intent)))
               (unless (stringp model) (user-error "No model"))
               (setq effort (and (stringp effort) (not (string-empty-p effort)) effort))
               (ghostherd-switch-model
                s model effort
                (lambda (ok what)
                  (ghostherd-memory-page--js "flash" (if ok what (format "%s: %s" name what)))
                  (ghostherd-memory-page--send-herd)))
               (format "Switching %s to %s%s…" name model (if effort (concat ", " effort) ""))))
            ("agent-screen"
             (ghostherd-memory-page--js
              "setScreen" (list :name name
                                :text (or (ignore-errors (ghostherd--host-capture s))
                                          (gethash (ghostherd-session-id s) ghostherd--screens)
                                          "")))
             nil)
            ("agent-capture"
             ;; Pinned into the log; the Log tab hears it through `logAdd'.
             (let* ((note (alist-get 'note intent))
                    (note (and (stringp note) (not (string-empty-p (string-trim note)))
                               (string-trim note))))
               (ghostherd-capture-screen s note)
               (format "Saved %s's screen to the log" name))))))
    (when said (ghostherd-memory-page--js "flash" said))
    (ghostherd-memory-page--send-herd)))

(defconst ghostherd-memory-page--thumb-size 240
  "Longest side, in pixels, of an attached image's preview.")

(defun ghostherd-memory-page--thumb (file)
  "A small PNG of image FILE as a data URL, or nil when none can be made.
`sips' (macOS) scales it; without it, a file small enough is its own
preview.  A data URL because the page loads no image from anywhere."
  (let ((data
         (if (executable-find "sips")
             (let ((thumb (make-temp-file "ghostherd-thumb" nil ".png")))
               (unwind-protect
                   (and (eq 0 (call-process "sips" nil nil nil
                                            "-Z" (number-to-string ghostherd-memory-page--thumb-size)
                                            "-s" "format" "png" file "--out" thumb))
                        (with-temp-buffer
                          (set-buffer-multibyte nil)
                          (insert-file-contents-literally thumb)
                          (buffer-string)))
                 (ignore-errors (delete-file thumb))))
           (and (< (or (file-attribute-size (file-attributes file)) most-positive-fixnum)
                   (* 200 1024))
                (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (insert-file-contents-literally file)
                  (buffer-string))))))
    (and data (string-prefix-p "\x89PNG" data)
         (concat "data:image/png;base64," (base64-encode-string data t)))))

(defun ghostherd-memory-page--image-for (intent)
  "The name of the agent INTENT attaches an image for."
  (if (equal (alist-get 'name intent) "@lead")
      "@lead"                           ; a lead not started yet: it will be
    (ghostherd-session-name (ghostherd-memory-page--session intent))))

(defun ghostherd-memory-page--attach (name path)
  "Show image PATH in the page, attached to NAME's next prompt."
  (ghostherd-memory-page--js
   "attachImage" (list :name name :path path :thumb (ghostherd-memory-page--thumb path))))

(defun ghostherd-memory-page--agent-image (intent)
  "Save the clipboard's image for the agent INTENT names, and attach it.
The page cannot write a file, and an image is too big for the title it
speaks through, so Emacs reads the clipboard itself; the page gets the
file's name and a preview."
  (let ((name (ghostherd-memory-page--image-for intent)))
    (ghostherd-memory-page--attach name (ghostherd-save-clipboard-image name))))

(defun ghostherd-memory-page--agent-image-upload (intent)
  "Save the image INTENT carries, base64, for the agent it names, and
attach it.  It comes from the page on another device -- a phone's photo
-- whose clipboard is not Emacs's; in a request, not a title."
  (let* ((name (ghostherd-memory-page--image-for intent))
         (data (alist-get 'data intent))
         (bytes (or (and (stringp data) (ignore-errors (base64-decode-string data)))
                    (user-error "No image came"))))
    (ghostherd-memory-page--attach name (ghostherd-save-image bytes name))))

(defun ghostherd-memory-page--lead-say (intent)
  "Say INTENT's text to its project's lead, starting the lead if need be."
  (let ((project (alist-get 'project intent))
        (text (alist-get 'text intent)))
    (unless (and (stringp project) (file-directory-p (expand-file-name project)))
      (user-error "No project %s" project))
    (unless (and (stringp text) (not (string-empty-p (string-trim text))))
      (user-error "Nothing to send"))
    (let* ((had (ghostherd-project-lead project))
           (lead (ghostherd-lead-say project text)))
      (ghostherd-memory-page--js
       "flash" (if had
                   (format "Sent to %s" (ghostherd-session-name lead))
                 (format "Started %s; it gets your message once it is ready"
                         (ghostherd-session-name lead))))
      (ghostherd-memory-page--send-herd))))

(defun ghostherd-memory-page--primary-say (intent)
  "Say INTENT's text to primary, starting or resuming it if need be."
  (let ((text (alist-get 'text intent))
        (up (ghostherd--primary-up)))
    (unless (and (stringp text) (not (string-empty-p (string-trim text))))
      (user-error "Nothing to send"))
    (ghostherd-primary-say text)
    (ghostherd-memory-page--js
     "flash" (if up "Sent to primary"
               "Starting primary; it gets your message once it is ready"))
    (ghostherd-memory-page--send-herd)))

(defun ghostherd-memory-page--guard-approve (intent)
  "Approve INTENT's guard request once: the agent may run that exact call again."
  (let ((id (alist-get 'id intent)))
    (unless (and (stringp id) (string-match-p "\\`[0-9a-f]\\{4,32\\}\\'" id))
      (user-error "No such request"))
    (ghostherd-memory-request-async
     "herd_guard_approve"
     (lambda (_r)
       (ghostherd-memory-page--js "flash" "Approved once; the agent is told to run it again"))
     (list :id id)
     (lambda (err) (ghostherd-memory-page--js "showError" (format "Could not approve: %s" err))))))

(defun ghostherd-memory-page--agent-new (intent)
  "Spawn a KIND agent in PROJECT, as INTENT asks; it does not take focus.
With a true `worktree', give it its own git worktree off the project's
HEAD on a branch named for the agent; when git cannot, say so and spawn
nothing rather than a plain agent pretending to be in a worktree."
  (let* ((kind (intern-soft (or (alist-get 'kind intent) "")))
         (dir (alist-get 'project intent))
         (name (string-trim (or (alist-get 'name intent) "")))
         (worktree (eq (alist-get 'worktree intent) t)))
    (unless (and kind (memq kind (ghostherd--agent-kinds))
                 (plist-get (ghostherd--spec kind) :command))
      (user-error "No agent kind %s" (alist-get 'kind intent)))
    (unless (and (stringp dir) (file-directory-p dir))
      (user-error "No directory %s" dir))
    (unless (or (string-empty-p name) (string-match-p ghostherd-memory-page--agent-name-re name))
      (user-error "A name is letters, digits, . _ - (%s)" name))
    (let* ((defaults (ghostherd-project-defaults dir))
           (project (or (ghostherd--project-root dir) dir))
           (final (ghostherd--unique-name
                   (if (string-empty-p name)
                       (format "%s-%s" kind (file-name-nondirectory (directory-file-name dir)))
                     name)))
           (args (or (and (eq (car defaults) kind) (cdr defaults))
                     (plist-get (ghostherd--spec kind) :args)))
           (started (lambda (s)
                      (ghostherd-memory-page--js "flash" (format "Started %s" (ghostherd-session-name s)))
                      (ghostherd-memory-page--send-herd))))
      (if (not worktree)
          (funcall started (ghostherd-spawn kind :name final :project project
                                            :directory dir :args args :display nil))
        (condition-case err
            (let ((wt (ghostherd--worktree-create project final)))
              (ghostherd--worktree-deps-setup project (plist-get wt :path) final)
              (funcall started (ghostherd-spawn kind :name final :project project
                                                :directory (plist-get wt :path) :args args
                                                :worktree (plist-get wt :path)
                                                :branch (plist-get wt :branch)
                                                :base (plist-get wt :base) :display nil)))
          (error (ghostherd-memory-page--js
                  "flash" (format "No worktree: %s" (error-message-string err)))))))))


;;; Intents

(defun ghostherd-memory-page--ready ()
  "The page has loaded: show the log or the agents at once if asked,
then the sessions.  Neither needs the sidecar, so neither waits for it."
  (let ((view (plist-get ghostherd-memory-page--pending :view)))
    (when (member view '("log" "agents"))
      (setq ghostherd-memory-page--pending nil)
      (ghostherd-memory-page--js "showView" view)))
  (ghostherd-memory-page--send-context))

(defun ghostherd-memory-page--send-context ()
  "Send the sessions the index holds, and what to show first."
  (ghostherd-memory-page--watch-import)
  (ghostherd-memory-request-async
   "memory_list"
   (lambda (result)
     (let ((pending ghostherd-memory-page--pending))
       (setq ghostherd-memory-page--pending nil)
       (ghostherd-memory-page--js
        "setContext"
        (list :sources (ghostherd-memory-page--vec (plist-get result :sources))
              :total (or (plist-get result :total) 0)
              :current_project (or ghostherd-memory-page--project "")
              :view (or (plist-get pending :view) "")
              :query (or (plist-get pending :query) "")
              :project (or (plist-get pending :project) "")))))
   (list :limit 5000)
   (ghostherd-memory-page--fail "Listing sessions")))

(defun ghostherd-memory-page--search (intent)
  "Run INTENT's search: seq, q, agent, project, limit.
SEQ comes back with the hits, so the page can drop a slow answer to
an older query."
  (let ((seq (let ((n (alist-get 'seq intent))) (and (numberp n) n)))
        (q (or (ghostherd-memory-page--str (alist-get 'q intent)) ""))
        (agent (ghostherd-memory-page--str (alist-get 'agent intent)))
        (project (ghostherd-memory-page--str (alist-get 'project intent)))
        (limit (min 120 (max 1 (ghostherd-memory-page--int (alist-get 'limit intent) 30)))))
    (if (string-empty-p q)
        (ghostherd-memory-page--js "renderHits" (list :seq seq :q "" :hits []))
      (ghostherd-memory-request-async
       "memory_search"
       (lambda (result)
         (ghostherd-memory-page--js
          "renderHits"
          (list :seq seq :q q :hits (ghostherd-memory-page--vec (plist-get result :hits)))))
       (append (list :query q :limit limit)
               (and agent (list :agent agent))
               (and project (list :project project)))
       (lambda (err)
         (ghostherd-memory-page--js "searchFailed" (list :seq seq :error (format "%s" err))))))))

(defun ghostherd-memory-page--open-source (intent)
  "Send a window of INTENT's source: source_path, offset, limit.
Focus and mode ride along untouched; the page placed the window."
  (let ((path (ghostherd-memory-page--str (alist-get 'source_path intent)))
        (offset (max 0 (ghostherd-memory-page--int (alist-get 'offset intent) 0)))
        (limit (min ghostherd-memory-page--max-chunks
                    (max 1 (ghostherd-memory-page--int (alist-get 'limit intent) 160))))
        ;; Intents read JSON null as `:null', which json-encode refuses.
        (focus (let ((f (alist-get 'focus intent))) (and (numberp f) f)))
        (mode (or (ghostherd-memory-page--str (alist-get 'mode intent)) "open")))
    (unless path (user-error "No source to open"))
    (ghostherd-memory-request-async
     "memory_chunks"
     (lambda (result)
       (let ((chunks (plist-get result :chunks)))
         (ghostherd-memory-page--js
          "renderSource"
          (append
           (list :source_path path
                 :offset offset
                 :total (or (plist-get result :total) 0)
                 :chunks (ghostherd-memory-page--vec chunks)
                 :focus focus
                 :mode mode)
           (and (equal mode "open")
                (list :resume (ghostherd-memory-page--resume-info path chunks)))))))
     (list :source_path path :offset offset :limit limit)
     (ghostherd-memory-page--fail "Opening the session"))))

(defun ghostherd-memory-page--import ()
  "Import new transcripts in the sidecar, then send the sessions again.
Not `ghostherd-memory-import': that pops up the log beside the page."
  (setq ghostherd-memory-page--importing t)
  (ghostherd-memory-page--js "importProgress"
                             '(:files 0 :imported 0 :agent "" :current "starting"))
  (ghostherd-memory-page--watch-import)
  (let ((done (lambda ()
                (setq ghostherd-memory-page--importing nil
                      ghostherd-memory-page--progress-shown nil)
                (ghostherd-memory-page--unwatch)
                (ghostherd-memory-page--js "importProgress" nil))))
    (ghostherd-memory-request-async
     "memory_import"
     (lambda (result)
       (funcall done)
       (ghostherd-memory-page--js
        "flash"
        (if (plist-get result :busy)
            "A background import is already running"
          (format "Imported %s new chunks from %s sessions"
                  (or (plist-get result :imported) 0)
                  (or (plist-get result :sessions) 0))))
       ;; Watches again: a busy answer means the other import goes on.
       (ghostherd-memory-page--send-context))
     nil
     (lambda (err)
       (funcall done)
       (ghostherd-memory-page--js "showError" (format "Import: %s" err))))))

;;; Inbox -- one list of what needs the user, then updates
;;
;; The items are derived (ghostherd-inbox--*) from the same serialised state
;; the Agents tab gets -- an agent plist (`--agent'), the guard requests, the
;; asks -- plus the pending memory proposals fetched from the sidecar.  Each
;; has a STABLE id keyed by its cause, so the user's read/done/override on it
;; (kept in the sidecar) survives a reload and resolves itself when the cause
;; goes away.  The page renders and routes each action to the existing intent
;; (agent-answer, guard-approve, memory-proposal-*, lead-say/primary-say,
;; agent-login/relay), plus `inbox-set' for read/done/override.

(defconst ghostherd-inbox--spec
  '((blocked    "need" "Permission" "warn")
    (guard      "need" "Guard"      "red")
    (question   "need" "Question"   "accent")
    (proposal   "need" "Proposal"   "purple")
    (login      "need" "Login"      "warn")
    (relay      "need" "Relay"      "warn")
    (ask-failed "need" "Ask failed" "red")
    (report     "fyi"  "Report"     "green")
    (handoff    "fyi"  "Handoff"    "purple")
    (lead-started "fyi" "Lead"      "muted")
    (continue   "fyi"  "Continuing" "muted"))
  "Each Inbox item kind: (KIND GROUP TAG TONE).  GROUP is where it sits by
default; the user may override it.")

(defun ghostherd-inbox--clip (s n)
  (let ((s (string-trim (or s ""))))
    (if (> (length s) n) (concat (substring s 0 (1- n)) "…") s)))

(defun ghostherd-inbox--first-line (s)
  (or (car (split-string (string-trim (or s "")) "\n" t)) ""))

(defun ghostherd-inbox--asks-p (text)
  "Non-nil when TEXT ends by asking the user something: a `?' (ASCII or
full-width) in its last line or two."
  (when (and (stringp text) (not (string-empty-p (string-trim text))))
    (let* ((lines (split-string (string-trim text) "\n" t "[ \t]+"))
           (tail (last lines (min 2 (length lines)))))
      (and tail (cl-some (lambda (l) (string-match-p "[?？]" l)) tail) t))))

(defun ghostherd-inbox--ms (v)
  "V as epoch-ms: a number is kept, an ISO string is parsed, else 0."
  (cond ((numberp v) v)
        ((and (stringp v) (not (string-empty-p v)))
         (condition-case nil (* 1000.0 (float-time (date-to-time v))) (error 0)))
        (t 0)))

(defun ghostherd-inbox--item (id kind agent cli project title at &rest props)
  (let* ((spec (alist-get kind ghostherd-inbox--spec))
         (item (list :id id :kind (symbol-name kind)
                    :group (nth 0 spec) :tag (nth 1 spec) :tone (nth 2 spec)
                    :agent (or agent "") :cli (or cli "") :project (or project "")
                    :title (or title "") :at (ghostherd-inbox--ms at)
                    :whyLabel "" :body "" :preview "" :state "")))
    ;; props override the defaults in place, so the plist stays single-keyed
    ;; -- plist-get and json-encode then read the same value, not a stale "".
    (let ((p props))
      (while p (setq item (plist-put item (car p) (cadr p)) p (cddr p))))
    item))

(defun ghostherd-inbox--agent-items (a)
  "Inbox items (0+) from one serialised agent plist A."
  (let* ((name (plist-get a :name)) (cli (plist-get a :kind))
         (project (plist-get a :project)) (role (plist-get a :role))
         (state (plist-get a :state)) (at (plist-get a :since))
         (prompt (plist-get a :prompt))
         (opts (and (consp prompt) (plist-get prompt :options)))
         (login (eq (plist-get a :login) t))
         (cont (plist-get a :continueAt))
         (cont-stop (eq (plist-get a :continueStopped) t))
         (said (plist-get a :said)) (said-ms (plist-get a :saidAt))
         (said-from-ask (eq (plist-get a :saidFromAsk) t))
         (handoff-started (eq (plist-get a :handoffStarted) t))
         (used-up (plist-get a :usedUp)) (relay-to (plist-get a :relayTo))
         items)
    (cond
     (login
      (push (ghostherd-inbox--item
             (concat "login:" name) 'login name cli project "Login expired" at
             :state state :whyLabel "Its CLI stopped"
             :preview "Its turn stopped until someone runs /login"
             :body "The session token expired mid-turn.  Its turn waits until /login runs; the ask it holds is not lost.")
            items))
     ((and (numberp cont) (> cont 0) (not cont-stop))
      (push (ghostherd-inbox--item
             (concat "continue:" name) 'continue name cli project "Continuing on its own" at
             :state state :whyLabel "Auto-continue"
             :preview "The herd is picking its turn back up"
             :body (concat "A sleep or a dropped connection cut its turn short"
                           (let ((w (plist-get a :continueAfter)))
                             (if (and (stringp w) (not (string-empty-p w))) (concat " " w) ""))
                           "; the herd continues it on its own."))
            items))
     ((and opts (> (length opts) 0))
      (let ((q (plist-get prompt :question)))
        (push (ghostherd-inbox--item
               (concat "blocked:" name) 'blocked name cli project
               (ghostherd-inbox--clip (or (ghostherd-inbox--first-line q) "Waiting on a choice") 60) at
               :state state :whyLabel "It is waiting on a menu"
               :body (or q "")
               :preview (ghostherd-inbox--clip
                         (or (and (> (length opts) 0) (plist-get (aref opts 0) :label)) q "") 80)
               :options opts)
              items))))
    (when (and (stringp used-up) (not (string-empty-p used-up))
               (stringp relay-to) (not (equal relay-to cli)))
      (push (ghostherd-inbox--item
             (concat "relay:" name) 'relay name cli project "Out of usage — relay its work" at
             :state state :whyLabel "Its CLI is used up" :relayTo relay-to
             :preview (format "Relay to %s to carry its work on" relay-to)
             :body used-up)
            items))
    ;; A turn end becomes a Question (ends by asking) or a Report.  A lead
    ;; or primary is surfaced as before; any other agent is surfaced only
    ;; when the user started the turn -- a turn an ask from another agent
    ;; started stays out, since whoever asked reports.
    (when (and (member state '("idle" "done"))
               (stringp said) (not (string-empty-p (string-trim said)))
               (or (member role '("lead" "primary")) (not said-from-ask)))
      (let* ((asks (ghostherd-inbox--asks-p said))
             (kind (if asks 'question 'report))
             (id (format "%s:%s:%s" (symbol-name kind) name (or said-ms 0))))
        (push (ghostherd-inbox--item
               id kind name cli project (ghostherd-inbox--clip (ghostherd-inbox--first-line said) 60)
               (or said-ms at)
               :state state
               :whyLabel (if asks "It ended its turn with a question" "It reported")
               :body said :preview (ghostherd-inbox--clip (ghostherd-inbox--first-line said) 90)
               ;; A lead/primary question is answered through it; any other
               ;; agent's is answered to the agent itself.
               :replyVia (cond ((equal role "primary") "primary")
                               ((equal role "lead") "lead")
                               (t "agent")))
              items)))
    ;; A lead the herd started to take a cross-project handoff: an Update,
    ;; kept until read, so the user knows the herd spun one up.
    (when handoff-started
      (let ((by (plist-get a :handoffFrom)))
        (push (ghostherd-inbox--item
               (concat "lead-started:" name) 'lead-started name cli project
               (format "Lead started for %s"
                       (if (string-empty-p (or project ""))
                           "a handoff"
                         (file-name-nondirectory (directory-file-name project))))
               at
               :state state :whyLabel "The herd started a lead for a handoff"
               :body (format "The herd started %s to take a cross-project handoff%s."
                             name (if (and (stringp by) (not (string-empty-p by)))
                                      (concat " from " by) ""))
               :preview (if (and (stringp by) (not (string-empty-p by)))
                            (concat "Started to take a handoff from " by)
                          "Started to take a cross-project handoff"))
              items)))
    (nreverse items)))

(defun ghostherd-inbox--guard-items (guard)
  "Inbox items from the raw GUARD requests (pending and approvable)."
  (delq nil
        (mapcar
         (lambda (r)
           (when (and (equal (plist-get r :status) "pending") (plist-get r :approvable))
             (let ((call (or (plist-get r :summary) (plist-get r :tool) ""))
                   (rule (plist-get r :rule)))
               (ghostherd-inbox--item
                (concat "guard:" (plist-get r :id)) 'guard
                (plist-get r :session) "" "" "Needs your approval" (plist-get r :created_ms)
                :whyLabel "The guard denied this call"
                :body (or (plist-get r :reason) "") :code call
                :note (if (and (stringp rule) (not (string-empty-p rule))) (concat "Rule: " rule) "")
                :preview (ghostherd-inbox--clip call 90)
                :guardId (plist-get r :id)))))
         guard)))

(defun ghostherd-inbox--proposal-items (proposals)
  "Inbox items from the pending PROPOSALS (list of plists)."
  (delq nil
        (mapcar
         (lambda (p)
           (when (equal (plist-get p :status) "pending")
             (let* ((facts (append (plist-get p :facts) nil))
                    (titles (mapcar (lambda (f) (or (plist-get f :title) (plist-get f :name))) facts))
                    (file (plist-get p :file)) (n (length titles)))
               (ghostherd-inbox--item
                (concat "proposal:" (plist-get p :id)) 'proposal
                (plist-get p :author) "claude" (plist-get p :project)
                (format "Promote %d fact%s into %s" n (if (= n 1) "" "s") file)
                (plist-get p :created_at)
                :whyLabel "Memory promotion"
                :body (format "%d stable fact%s from the shared memory become part of %s, versioned with the code."
                              n (if (= n 1) "" "s") file)
                :facts (ghostherd-memory-page--vec titles) :diff (or (plist-get p :diff) "")
                :preview (ghostherd-inbox--clip (mapconcat #'identity titles " · ") 90)
                :proposalId (plist-get p :id)))))
         proposals)))

(defun ghostherd-inbox--ask-items (asks)
  "Inbox items from the serialised ASKS: a failed ask (need), an answered
cross-project handoff (fyi)."
  (delq nil
        (mapcar
         (lambda (k)
           (let ((status (plist-get k :status)) (project (plist-get k :project)))
             (cond
              ((equal status "failed")
               (ghostherd-inbox--item
                (concat "ask-failed:" (plist-get k :id)) 'ask-failed
                (plist-get k :to) "" (or project "") "An ask failed" (plist-get k :created)
                :whyLabel "The ask could not be delivered or answered"
                :body (or (plist-get k :error) (plist-get k :head) "")
                :preview (ghostherd-inbox--clip (or (plist-get k :error) (plist-get k :head) "") 90)))
              ((and (equal status "answered") (stringp project) (not (string-empty-p project)))
               (ghostherd-inbox--item
                (concat "handoff:" (plist-get k :id)) 'handoff
                (plist-get k :from) "" (plist-get k :fromProject)
                (ghostherd-inbox--clip (concat "Answered: " (or (plist-get k :head) "a handoff")) 60)
                (plist-get k :answered)
                :whyLabel "Cross-project handoff, answered"
                :body (or (plist-get k :replyHead) (plist-get k :head) "")
                :preview (ghostherd-inbox--clip (or (plist-get k :replyHead) (plist-get k :head) "") 90))))))
         asks)))

(defun ghostherd-inbox--all (proposals)
  "Every live Inbox item, from the sessions, guard, PROPOSALS and asks."
  (append
   (mapcan #'ghostherd-inbox--agent-items
           (mapcar #'ghostherd-memory-page--agent (ghostherd-sessions)))
   (ghostherd-inbox--guard-items
    (and (boundp 'ghostherd--guard-requests) ghostherd--guard-requests))
   (ghostherd-inbox--proposal-items proposals)
   (ghostherd-inbox--ask-items
    (mapcar #'ghostherd-memory-page--ask ghostherd--herd-asks))))

(defun ghostherd-inbox--need-p (it)
  "Non-nil when IT belongs in Needs-you, honouring the user's override."
  (pcase (plist-get it :override)
    ("need" t) ("update" nil)
    (_ (equal (plist-get it :group) "need"))))

(defvar ghostherd-inbox--watching nil)
(defvar ghostherd-inbox--timer nil)
(defvar ghostherd-inbox--sig nil "Signature of the last Inbox push, to skip an unchanged one.")
(defconst ghostherd-inbox--every 3 "Seconds between Inbox refreshes.")

(defun ghostherd-inbox--push (items state-list)
  "Merge STATE-LIST ({:id :read :done :override}...) into ITEMS, drop the
done ones, and push the rest with the Needs-you count (the tab badge)."
  (let ((by-id (make-hash-table :test #'equal)))
    (dolist (e (append state-list nil)) (puthash (plist-get e :id) e by-id))
    (let* ((merged
            (mapcar (lambda (it)
                      (let* ((st (gethash (plist-get it :id) by-id))
                             (over (and st (plist-get st :override))))
                        (append it (list :read (if (and st (eq (plist-get st :read) t)) t :json-false)
                                         :done (and st (eq (plist-get st :done) t))
                                         :override (and (stringp over) over)))))
                    items))
           (live (seq-remove (lambda (it) (plist-get it :done)) merged))
           (count (length (seq-filter #'ghostherd-inbox--need-p live)))
           (sig (sxhash-equal (list count (mapcar (lambda (it)
                                                    (list (plist-get it :id) (plist-get it :read)
                                                          (plist-get it :override) (plist-get it :preview)))
                                                  live)))))
      (unless (equal sig ghostherd-inbox--sig)
        (setq ghostherd-inbox--sig sig)
        (ghostherd-memory-page--js
         "renderInbox"
         (list :items (ghostherd-memory-page--vec
                       (mapcar (lambda (it) (ghostherd-memory-page--plist-no-done it)) live))
               :count count))))))

(defun ghostherd-memory-page--plist-no-done (it)
  "IT without the internal :done flag (the page only sees live items)."
  (cl-loop for (k v) on it by #'cddr unless (eq k :done) append (list k v)))

(defun ghostherd-inbox--tick ()
  "Fetch the pending proposals, build every item, merge the user's state
on them (and prune the gone ones), and push to the page.  Each sidecar
call degrades to none on failure (a sidecar without the Inbox RPCs yet),
so the session- and guard-derived items still show."
  (if (not (and ghostherd-inbox--watching (xwapp-live-p ghostherd-memory-page--app)))
      (ghostherd-inbox--unwatch)
    (ghostherd-memory-request-async
     "herd_pending_proposals"
     (lambda (result) (ghostherd-inbox--tick-items (plist-get result :proposals)))
     nil
     (lambda (_e) (ghostherd-inbox--tick-items nil)))))

(defun ghostherd-inbox--tick-items (proposals)
  "Build every item from PROPOSALS and the live state, then fetch the
user's state (or none) and push."
  (when ghostherd-inbox--watching
    (let* ((items (ghostherd-inbox--all (append proposals nil)))
           (ids (mapcar (lambda (it) (plist-get it :id)) items)))
      (ghostherd-memory-request-async
       "herd_inbox_state"
       (lambda (st)
         (when ghostherd-inbox--watching
           (ghostherd-inbox--push items (append (plist-get st :state) nil))))
       (list :ids (ghostherd-memory-page--vec ids))
       (lambda (_e)
         (when ghostherd-inbox--watching (ghostherd-inbox--push items nil)))))))

(defun ghostherd-inbox--watch ()
  (setq ghostherd-inbox--watching t ghostherd-inbox--sig nil)
  (unless (timerp ghostherd-inbox--timer)
    (setq ghostherd-inbox--timer
          (run-at-time 0 ghostherd-inbox--every #'ghostherd-inbox--tick)))
  (ghostherd-inbox--tick))

(defun ghostherd-inbox--unwatch ()
  (when (timerp ghostherd-inbox--timer) (cancel-timer ghostherd-inbox--timer))
  (setq ghostherd-inbox--timer nil ghostherd-inbox--watching nil ghostherd-inbox--sig nil))

(defun ghostherd-memory-page--inbox-set (intent)
  "Mark an Inbox item read / done, or override its group; then refresh."
  (let ((params (list :id (ghostherd-memory-page--str (alist-get 'id intent)))))
    (when (assq 'read intent)
      (setq params (append params (list :read (ghostherd-memory-page--truthy (alist-get 'read intent))))))
    (when (assq 'done intent)
      (setq params (append params (list :done (ghostherd-memory-page--truthy (alist-get 'done intent))))))
    (when (assq 'override intent)
      (setq params (append params (list :override (or (ghostherd-memory-page--str (alist-get 'override intent)) "")))))
    (ghostherd-memory-request-async
     "herd_inbox_set"
     (lambda (_r) (setq ghostherd-inbox--sig nil) (ghostherd-inbox--tick))
     params
     (ghostherd-memory-page--fail "Inbox"))))


(defun ghostherd-memory-page--handle (intent)
  "Dispatch INTENT from the page.  Errors are shown there, not lost."
  (condition-case e
      (pcase (alist-get 'op intent)
        ("ready" (ghostherd-memory-page--ready))
        ("log" (ghostherd-memory-page--send-log))
        ("log-pin" (ghostherd-memory-page--log-pin intent))
        ("log-delete" (ghostherd-memory-page--log-delete intent))
        ("refresh" (ghostherd-memory-page--send-context))
        ("search" (ghostherd-memory-page--search intent))
        ("open-source" (ghostherd-memory-page--open-source intent))
        ("import" (ghostherd-memory-page--import))
        ("resume" (ghostherd-memory-page--resume
                   (alist-get 'source_path intent)
                   (ghostherd-memory-page--truthy (alist-get 'fork intent))))
        ("visit-agent" (ghostherd-memory-page--visit-agent (alist-get 'source_path intent)))
        ("herd-watch"
         (setq ghostherd-memory-page--hooks-at 0)
         (ghostherd-memory-page--herd-watch))
        ("hook-open" (ghostherd-memory-page--hook-open intent))
        ("herd-unwatch"
         (ghostherd-memory-page--room-close)
         (ghostherd-memory-page--herd-unwatch))
        ("inbox-watch" (ghostherd-inbox--watch))
        ("inbox-unwatch" (ghostherd-inbox--unwatch))
        ("inbox-set" (ghostherd-memory-page--inbox-set intent))
        ("memory-watch" (ghostherd-memory-page--memory-watch intent))
        ("memory-unwatch" (ghostherd-memory-page--memory-unwatch))
        ("memory-edit" (ghostherd-memory-page--memory-edit intent))
        ("memory-delete" (ghostherd-memory-page--memory-delete intent))
        ("memory-proposal-approve" (ghostherd-memory-page--memory-proposal-approve intent))
        ("memory-proposal-reject" (ghostherd-memory-page--memory-proposal-reject intent))
        ("memory-unpromote" (ghostherd-memory-page--memory-unpromote intent))
        ("memory-suggest" (ghostherd-memory-page--memory-suggest intent))
        ("room-watch" (ghostherd-memory-page--room-open intent))
        ("room-earlier" (ghostherd-memory-page--room-earlier intent))
        ("chat-earlier" (ghostherd-memory-page--chat-earlier intent))
        ("room-close" (ghostherd-memory-page--room-close))
        ("sub-watch" (ghostherd-memory-page--sub-watch intent))
        ("sub-unwatch" (ghostherd-memory-page--sub-forget))
        ("usage-refresh"
         (setq ghostherd-memory-page--usage-at (float-time))
         (ghostherd-usage-refresh t)
         (ghostherd-memory-page--send-herd))
        ("agent-new" (ghostherd-memory-page--agent-new intent))
        ("agent-image" (ghostherd-memory-page--agent-image intent))
        ("agent-image-upload" (ghostherd-memory-page--agent-image-upload intent))
        ("lead-say" (ghostherd-memory-page--lead-say intent))
        ("primary-say" (ghostherd-memory-page--primary-say intent))
        ("guard-approve" (ghostherd-memory-page--guard-approve intent))
        ("open-pr" (ghostherd-memory-page--open-pr intent))
        ("open-task" (ghostherd-memory-page--open-task intent))
        ("open-changes" (ghostherd-memory-page--open-changes intent))
        ("paste" (ghostherd-memory-page-paste))
        ((and op (guard (and (stringp op) (string-prefix-p "agent-" op))))
         (ghostherd-memory-page--agent-act op intent))
        ("copy"
         (when-let* ((text (alist-get 'text intent)))
           (xwapp-copy text)
           (ghostherd-memory-page--js "flash" "Copied")))
        ("open-browser"
         (let ((url (alist-get 'url intent)))
           (when (and (stringp url) (string-match-p "\\`https?://" url))
             (browse-url url))))
        (_ nil))
    (error (ghostherd-memory-page--js
            "showError" (xwapp-scrub-error (error-message-string e))))))


;;; Commands

(defun ghostherd-memory-page-paste ()
  "Paste the clipboard into the page's focused box.
The page never gets a paste of its own: Cmd-V reaches the web view as a
key, the paste being the menu's to send, and with Emacs's keys it is
Emacs's.  So the page asks, and Emacs hands it the clipboard's text --
which goes in where the cursor is -- or none, and the clipboard's image
is attached to the prompt box the cursor is in."
  (interactive)
  (ghostherd-memory-page--js
   "paste" (list :text (or (ignore-errors (gui-get-selection 'CLIPBOARD 'STRING)) ""))))

(defvar-keymap ghostherd-memory-page-keys-mode-map
  :doc "Keys the herd page takes before Emacs does."
  "s-v" #'ghostherd-memory-page-paste)

(define-minor-mode ghostherd-memory-page-keys-mode
  "Cmd-V pastes into the herd page, an image included."
  :keymap ghostherd-memory-page-keys-mode-map)

(defun ghostherd-memory-page--open (view &optional query project)
  "Show the page on VIEW, with QUERY and PROJECT as the first search."
  ;; Here, not in the page's ready: a missing `uv' or a stale sidecar
  ;; reads better in the echo area than in a page that never filled.
  ;; The log needs no sidecar, so for it a failure only demotes.
  (if (member view '("log" "agents"))
      (with-demoted-errors "ghostherd: memory sidecar: %S"
        (ghostherd-memory-ensure))
    (ghostherd-memory-ensure))
  (setq ghostherd-memory-page--project (ghostherd-memory-page--current-project)
        ghostherd-memory-page--pending (list :view view :query query :project project))
  (setf (xwapp-poll-interval ghostherd-memory-page--app)
        ghostherd-memory-page-poll-interval)
  (xwapp-open ghostherd-memory-page--app)
  (when-let* ((buf (xwapp-buffer ghostherd-memory-page--app)))
    (with-current-buffer buf
      (ghostherd-memory-page-keys-mode 1))))

;;;###autoload
(defun ghostherd-memory-page-reopen ()
  "Make the herd page ready for a browser tab that comes back to it:
after a restart of Emacs, or once Emacs let it go.  It shows the tab
it was on, so nothing is asked of it here, and a sidecar that does not
start is only said.  xwapp calls this; autoloaded, so that once Emacs
is up xwapp can load the package through it, for that tab to find."
  (with-demoted-errors "ghostherd: memory sidecar: %S"
    (ghostherd-memory-ensure))
  (setq ghostherd-memory-page--project (ghostherd-memory-page--current-project)
        ghostherd-memory-page--pending nil))

;;;###autoload
(defun ghostherd-memory-page (&optional this-project)
  "Search past claude / grok / agy sessions in a page.
With a prefix argument THIS-PROJECT, start filtered to the current
project.  The default is every project: the usual failure is \"I
forgot which project\"."
  (interactive "P")
  (ghostherd-memory-page--open
   "search" nil (and this-project (ghostherd-memory-page--current-project))))

;;;###autoload
(defun ghostherd-memory-sessions ()
  "Browse the sessions the memory index holds, newest first."
  (interactive)
  (ghostherd-memory-page--open "sessions"))

;;;###autoload
(defun ghostherd-herd-page ()
  "The herd in a page: usage per CLI, agents grouped by project.
The overlay's operations are there too, for when the mouse is in hand."
  (interactive)
  (ghostherd-memory-page--open "agents"))

;;;###autoload
(defun ghostherd-log-page ()
  "What the herd did: a state lane per agent, the log entries below.
Kept screens open in place; new entries arrive as they are logged."
  (interactive)
  (ghostherd-memory-page--open "log"))

(provide 'ghostherd-memory-page)
;;; ghostherd-memory-page.el ends here

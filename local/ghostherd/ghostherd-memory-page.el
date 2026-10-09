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
                           (ghostherd-memory-page--herd-unwatch)))
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
   ((not (xwapp-buffer ghostherd-memory-page--app))
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
         (screen (ghostherd-log-entry-screen entry)))
    (append (list :t (* 1000 (float-time (ghostherd-log-entry-time entry)))
                  :session (format "%s" (or (ghostherd-log-entry-session entry) ""))
                  :kind (format "%s" (or kind ""))
                  :text text)
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
  (when (xwapp-session ghostherd-memory-page--app)
    (ghostherd-memory-page--js "logAdd" (ghostherd-memory-page--log-entry entry))))

(add-hook 'ghostherd-log-functions #'ghostherd-memory-page--on-log)


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
  (let ((buf (ghostherd-session-buffer s)))
    (list :name (ghostherd-session-name s)
          :kind (format "%s" (or (ghostherd-session-kind s) "shell"))
          :state (format "%s" (or (ghostherd-session-state s) "idle"))
          :reason (or (ghostherd-session-state-reason s) "")
          :project (or (ghostherd-session-project s) "")
          :notes (or (ghostherd-session-notes s) "")
          :backend (format "%s" (ghostherd-session-backend s))
          :detached (if (buffer-live-p buf) :json-false t)
          :manual (if (ghostherd-session-manual-state s) t :json-false)
          :since (let ((t0 (or (ghostherd-session-last-active s)
                               (ghostherd-session-started-at s))))
                   (and t0 (* 1000 (float-time t0))))
          :progress (ghostherd-session-progress-percent s)
          :said (ghostherd-memory-page--str (plist-get (ghostherd--link s) :last_head))
          :saidAt (plist-get (ghostherd--link s) :last_ms)
          :conversation (ghostherd-session-conversation s))))

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
        :answered (plist-get a :answered_ms)))

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
                       (when (xwapp-session ghostherd-memory-page--app)
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

(defun ghostherd-memory-page--send-herd ()
  "Send the herd, its asks and the usage to the page."
  (ghostherd-memory-page--js
   "renderHerd"
   (list :agents (ghostherd-memory-page--vec
                  (mapcar #'ghostherd-memory-page--agent (ghostherd-sessions)))
         :asks (ghostherd-memory-page--vec
                (mapcar #'ghostherd-memory-page--ask ghostherd--herd-asks))
         :hooks (ghostherd-memory-page--hooks-payload)
         :usage (ghostherd-memory-page--usage)
         :kinds (ghostherd-memory-page--vec
                 (mapcar #'symbol-name
                         (seq-filter (lambda (k) (plist-get (ghostherd--spec k) :command))
                                     (ghostherd--agent-kinds)))))))

(defun ghostherd-memory-page--herd-tick ()
  "One snapshot; usage too when it is due and the page can be seen."
  (if (not (xwapp-session ghostherd-memory-page--app))
      (ghostherd-memory-page--herd-unwatch)
    (when (and (> (- (float-time) ghostherd-memory-page--usage-at) ghostherd-usage-interval)
               (get-buffer-window (xwapp-buffer ghostherd-memory-page--app) t))
      (setq ghostherd-memory-page--usage-at (float-time))
      (ghostherd-usage-refresh))
    (when (> (- (float-time) ghostherd-memory-page--hooks-at) ghostherd-memory-page--hooks-every)
      (ghostherd-memory-page--hooks-refresh))
    (ghostherd-memory-page--send-herd)))

(defun ghostherd-memory-page--herd-watch ()
  "Keep the Agents tab current while it is on screen."
  (ghostherd-memory-page--herd-unwatch)
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

(defun ghostherd-memory-page--room-sessions ()
  "The herd's agents in the room's project."
  (seq-filter (lambda (s) (equal (or (ghostherd-session-project s) "")
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
          :tools (ghostherd-memory-page--vec
                  (mapcar (lambda (tl) (list :name (funcall str (plist-get tl :name))
                                             :hint (funcall str (plist-get tl :hint))))
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
         (sig (cons absent (mapcar (lambda (a) (cons (car a) (ghostherd-memory-page--room-signature (cdr a))))
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
                        :missing (ghostherd-memory-page--vec
                                  (mapcar #'ghostherd-memory-page--str (plist-get r :missing)))
                        :entries (ghostherd-memory-page--vec
                                  (mapcar #'ghostherd-memory-page--chat-entry (plist-get r :entries)))))
             (list :agents (vconcat (mapcar (lambda (a) (list :name (car a) :path (cdr a))) known)))
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
         (sig (ghostherd-memory-page--room-signature path)))
    (cond
     ((null sig)
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
                        :entries (ghostherd-memory-page--vec
                                  (mapcar #'ghostherd-memory-page--room-entry
                                          (plist-get r :entries)))))
             (list :path path)
             (lambda (e)
               (funcall done :entries [] :error (ghostherd-memory-page--sidecar-error e))))
          (error (funcall done :entries [] :error (error-message-string err)))))))
    (when (or (null sig) (member name ghostherd-memory-page--room-screens))
      (ghostherd-memory-page--js
       "setScreen" (list :name name
                         :text (or (ignore-errors (ghostherd--host-capture s))
                                   (gethash (ghostherd-session-id s) ghostherd--screens)
                                   ""))))))

(defun ghostherd-memory-page--room-tick ()
  (if (not (and ghostherd-memory-page--room (xwapp-session ghostherd-memory-page--app)))
      (ghostherd-memory-page--room-close)
    (if ghostherd-memory-page--room-chat
        (ghostherd-memory-page--chat-read)
      (mapc #'ghostherd-memory-page--room-read (ghostherd-memory-page--room-sessions)))))

(defun ghostherd-memory-page--room-open (intent)
  "Show the room INTENT names, its agents in the screens it lists by
their screen, or as one chat when its view is \"chat\".  The same room
again only changes which those are."
  (let ((project (alist-get 'project intent))
        (screens (seq-filter #'stringp (append (alist-get 'screens intent) nil)))
        (chat (equal (alist-get 'view intent) "chat")))
    (unless (and (stringp project) (not (string-empty-p project)))
      (user-error "No project"))
    (unless (equal project ghostherd-memory-page--room)
      (ghostherd-memory-page--room-close)
      (setq ghostherd-memory-page--room project))
    ;; A view the page had not been sent is sent whole: the columns it
    ;; left were not kept up while it showed the chat.
    (unless (eq chat ghostherd-memory-page--room-chat)
      (setq ghostherd-memory-page--room-chat chat
            ghostherd-memory-page--chat-sent nil)
      (clrhash ghostherd-memory-page--room-sent))
    (setq ghostherd-memory-page--room-screens screens)
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
        ghostherd-memory-page--chat-reading nil)
  (clrhash ghostherd-memory-page--room-sent))

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
               (ghostherd-prompt s text)
               (format "Sent to %s" name)))
            ("agent-interrupt" (ghostherd-interrupt s) (format "Esc to %s" name))
            ("agent-abort" (ghostherd-abort s) (format "C-c to %s" name))
            ("agent-answer"
             (let ((n (alist-get 'n intent)))
               (unless (and (integerp n) (<= 1 n 9)) (user-error "Choice is 1 to 9"))
               (ghostherd-answer s n)
               (format "Answered %d to %s" n name)))
            ("agent-respawn"
             (ghostherd-respawn s (ghostherd-memory-page--truthy (alist-get 'continue intent)))
             (format "Respawned %s" name))
            ("agent-kill" (ghostherd-kill s t) (format "Killed %s" name))
            ("agent-notes"
             (ghostherd-set-notes s (or (alist-get 'text intent) ""))
             (format "Notes for %s saved" name))
            ("agent-screen"
             (ghostherd-memory-page--js
              "setScreen" (list :name name
                                :text (or (ignore-errors (ghostherd--host-capture s))
                                          (gethash (ghostherd-session-id s) ghostherd--screens)
                                          "")))
             nil))))
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

(defun ghostherd-memory-page--agent-image (intent)
  "Save the clipboard's image for the agent INTENT names, and attach it.
The page cannot write a file, and an image is too big for the title it
speaks through, so Emacs reads the clipboard itself; the page gets the
file's name and a preview."
  (let* ((name (ghostherd-session-name (ghostherd-memory-page--session intent)))
         (path (ghostherd-save-clipboard-image name)))
    (ghostherd-memory-page--js
     "attachImage" (list :name name :path path :thumb (ghostherd-memory-page--thumb path)))))

(defun ghostherd-memory-page--agent-new (intent)
  "Spawn a KIND agent in PROJECT, as INTENT asks; it does not take focus."
  (let* ((kind (intern-soft (or (alist-get 'kind intent) "")))
         (dir (alist-get 'project intent))
         (name (string-trim (or (alist-get 'name intent) ""))))
    (unless (and kind (memq kind (ghostherd--agent-kinds))
                 (plist-get (ghostherd--spec kind) :command))
      (user-error "No agent kind %s" (alist-get 'kind intent)))
    (unless (and (stringp dir) (file-directory-p dir))
      (user-error "No directory %s" dir))
    (unless (or (string-empty-p name) (string-match-p ghostherd-memory-page--agent-name-re name))
      (user-error "A name is letters, digits, . _ - (%s)" name))
    (let* ((defaults (ghostherd-project-defaults dir))
           (s (ghostherd-spawn
               kind
               :name (ghostherd--unique-name
                      (if (string-empty-p name)
                          (format "%s-%s" kind (file-name-nondirectory (directory-file-name dir)))
                        name))
               :project (or (ghostherd--project-root dir) dir)
               :directory dir
               :args (or (and (eq (car defaults) kind) (cdr defaults))
                         (plist-get (ghostherd--spec kind) :args))
               :display nil)))
      (ghostherd-memory-page--js "flash" (format "Started %s" (ghostherd-session-name s)))
      (ghostherd-memory-page--send-herd))))


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

(defun ghostherd-memory-page--handle (intent)
  "Dispatch INTENT from the page.  Errors are shown there, not lost."
  (condition-case e
      (pcase (alist-get 'op intent)
        ("ready" (ghostherd-memory-page--ready))
        ("log" (ghostherd-memory-page--send-log))
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
        ("room-watch" (ghostherd-memory-page--room-open intent))
        ("room-close" (ghostherd-memory-page--room-close))
        ("usage-refresh"
         (setq ghostherd-memory-page--usage-at (float-time))
         (ghostherd-usage-refresh t)
         (ghostherd-memory-page--send-herd))
        ("agent-new" (ghostherd-memory-page--agent-new intent))
        ("agent-image" (ghostherd-memory-page--agent-image intent))
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

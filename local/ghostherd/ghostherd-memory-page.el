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
          :progress (ghostherd-session-progress-percent s))))

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
                                        :used (max 0 (min 100 (- 100 (or (plist-get w :remaining) 100))))
                                        :resets (and (plist-get w :resets-at)
                                                     (* 1000 (plist-get w :resets-at)))))
                                (plist-get p :windows))))))
    (sort (copy-sequence ghostherd-usage--cache)
          (lambda (a b) (string< (format "%s" (car a)) (format "%s" (car b))))))))

(defun ghostherd-memory-page--send-herd ()
  "Send the herd and the usage to the page."
  (ghostherd-memory-page--js
   "renderHerd"
   (list :agents (ghostherd-memory-page--vec
                  (mapcar #'ghostherd-memory-page--agent (ghostherd-sessions)))
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
        ("herd-watch" (ghostherd-memory-page--herd-watch))
        ("herd-unwatch" (ghostherd-memory-page--herd-unwatch))
        ("usage-refresh"
         (setq ghostherd-memory-page--usage-at (float-time))
         (ghostherd-usage-refresh t)
         (ghostherd-memory-page--send-herd))
        ("agent-new" (ghostherd-memory-page--agent-new intent))
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
  (xwapp-open ghostherd-memory-page--app))

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

;; -*- lexical-binding: t -*-
;; One Emacs serving the herd page as the herd does -- ghostherd's own
;; page code (ghostherd-memory-page.el) behind xwapp -- with a room of
;; four agents at work, for timing how a phone opens it
;; (herd-room-open.js).  Only the sidecar is played: each agent's
;; transcript is a file in E2E_DIR/transcripts/, a JSON entry a line as
;; the sidecar's reader makes them, and the conversation, the chat and a
;; find are read from those files -- so the test makes an agent say more
;; by adding a line.  The entries are made up at the sizes this machine's
;; transcripts had (62 of them, 2026-11-05): assistant turns mostly a few
;; hundred bytes, some kilobytes; tool calls a few lines; asks and
;; answers through the herd several kilobytes; a column's newest 60 some
;; 60 KB, 20 KB gzip'd -- the long end of what is real.  Every request
;; that reaches Emacs and every `next' answered is noted in log.jsonl;
;; the page's timings in timings.jsonl.  Started by appslib.js.
(require 'cl-lib)
(defvar e2e-here (file-name-directory (or load-file-name buffer-file-name)))
(defvar e2e-dir (getenv "E2E_DIR"))
(defvar e2e-local (or (getenv "E2E_LOCAL") (expand-file-name "../.." e2e-here)))
(add-to-list 'load-path (expand-file-name "xwapp" e2e-local))
(add-to-list 'load-path (expand-file-name "ghostherd" e2e-local))
;; Nothing of the user's: ghostherd keeps its files under here.
(setq user-emacs-directory (file-name-as-directory (expand-file-name "emacs.d" e2e-dir)))
(make-directory user-emacs-directory t)
(require 'xwapp)
(setq xwapp--keep t
      xwapp-state-file (expand-file-name "state.eld" e2e-dir)
      xwapp-port (string-to-number (getenv "E2E_PORT"))
      xwapp-remote-hosts '("box.tail0.ts.net")
      xwapp-remote-users '("me@example.com")
      xwapp-remote-socket (expand-file-name "remote.sock" e2e-dir)
      xwapp-browse-function #'ignore
      xwapp-tailscale-program nil)
(require 'ghostherd)
(require 'ghostherd-memory-page)
(setq ghostherd-log-file nil)

(defun e2e-note (obj)
  (write-region (concat (json-encode (list :at (float-time) :what obj)) "\n")
                nil (expand-file-name "log.jsonl" e2e-dir) t 'silent))

;; Nothing that looks outside: no CLI's usage, hooks, checkouts or tally.
(dolist (f '(ghostherd-usage-refresh ghostherd-memory-page--work-look ghostherd-memory-page--hooks-refresh
             ghostherd-memory-page--tally-look ghostherd-memory-page--meter-look
             ghostherd-memory-page--subs-look ghostherd-memory-page--edits-look ghostherd--herd-tick-async))
  (when (fboundp f) (advice-add f :override #'ignore)))
(advice-add 'ghostherd--host-capture :override (lambda (&rest _) "the screen"))

;; ---- Four agents, a transcript each

(defconst e2e-project "/Users/someone/src/fox/")
(defconst e2e-agents '("lead-fox" "rd-api" "rd-web" "rd-qa"))
(defvar e2e-transcripts (expand-file-name "transcripts" e2e-dir))
(make-directory e2e-transcripts t)
(defun e2e-transcript (name) (expand-file-name (concat name ".jsonl") e2e-transcripts))

(random "herd-room")
(defconst e2e-words
  (split-string "the agent page herd phone emacs inbox session list reply ask lead branch merge test commit fix
  socket thread gzip proxy secret file report question answer reload cold open draw wait time byte link cell
  wifi slow fast main block key read send write memory index sidecar json request response header body cache
  tag version build check suite run pass fail error warn note view tab sort filter search project worktree
  review diff patch rebase status log trace cost token tally count badge chip card row column width height
  scroll click tap swipe touch screen window frame buffer mode hook timer process network port host user
  function variable value string number list vector plist alist hash table key map region point mark
  defun defvar let lambda cond when unless dolist mapcar seq filter reduce format concat insert delete"))
(defun e2e-said (n)
  "N words, at random, in sentences."
  (let ((out nil))
    (dotimes (i n)
      (push (nth (random (length e2e-words)) e2e-words) out)
      (when (zerop (random 14)) (push (concat (pop out) ".") out))
      (when (zerop (random 40)) (push "\n\n" out)))
    (string-trim (mapconcat #'identity (nreverse out) " "))))

(defvar e2e-t0 (- (float-time) 86400))
(defun e2e-ts (n i)
  "When entry N of agent I was said."
  (format-time-string "%Y-%m-%dT%H:%M:%S.%3NZ" (+ e2e-t0 (* n 37) (* i 9)) t))

(defun e2e-entry (n i)
  "Entry N of agent I's conversation, at the sizes real ones have."
  (let ((r (random 100)) (ts (e2e-ts n i)) (other (nth (% (1+ i) 4) e2e-agents)))
    (cond
     ((< r 38) (list :role "assistant" :ts ts
                     :text (e2e-said (pcase (random 20)
                                       ((pred (> 13)) (+ 15 (random 60)))
                                       ((pred (> 19)) (+ 150 (random 300)))
                                       (_ (+ 700 (random 900)))))))
     ((< r 78) (list :role "tools" :ts ts
                     :tools (mapcar (lambda (_) (list :name (nth (random 6) '("Bash" "Read" "Edit" "Grep" "Write" "Agent"))
                                                      :hint (e2e-said (+ 4 (random 18)))))
                                    (make-list (+ 1 (random 10)) nil))))
     ((< r 84) (list :role "herd" :kind "ask" :to other :ask (format "%08x" (random #xfffffff)) :ts ts
                     :text (e2e-said (+ 400 (random 700))) :answer (e2e-said (+ 200 (random 700)))))
     ((< r 88) (list :role "herd" :kind "reply" :ask (format "%08x" (random #xfffffff)) :ts ts
                     :text (e2e-said (+ 400 (random 700)))))
     ((< r 92) (list :role "user" :kind "ask" :who other :ask (format "%08x" (random #xfffffff)) :ts ts
                     :text (e2e-said (+ 300 (random 500)))))
     ((< r 98) (list :role "user" :kind "prompt" :who "" :ts ts :text (e2e-said (+ 6 (random 50)))))
     (t (list :role "user" :kind "note" :who "" :ts ts :text "the conversation was compacted")))))

(defconst e2e-turns 150
  "Entries in each conversation: more than a column holds, to load earlier.")

;; Each transcript once, the same each run; a word to find, said once,
;; long before what a column holds.
(dotimes (i 4)
  (let ((name (nth i e2e-agents)))
    (with-temp-file (e2e-transcript name)
      (dotimes (n e2e-turns)
        (let ((e (e2e-entry n i)))
          (when (and (= i 0) (= n 20))
            (setq e (list :role "assistant" :ts (e2e-ts n i) :text "The zeppelin answer, long ago.")))
          (insert (json-encode e) "\n"))))))

(dolist (name e2e-agents)
  (puthash name (ghostherd-session--create :id name :name name :kind 'claude
                                           :state (if (equal name "rd-qa") 'idle 'working)
                                           :project e2e-project :backend 'tmux)
           ghostherd--sessions))
(advice-add 'ghostherd-session-transcript :override
            (lambda (s) (e2e-transcript (ghostherd-session-name s))))

;; ---- The sidecar, played: no process, no port

(advice-add 'ghostherd-memory-ensure :override #'always)

(defun e2e-read (path)
  "PATH's entries as the file has them now."
  (with-temp-buffer
    (insert-file-contents path)
    (mapcar (lambda (line) (json-parse-string line :object-type 'plist :array-type 'list
                                              :null-object nil :false-object nil))
            (split-string (buffer-string) "\n" t))))

(defun e2e-page (entries limit before)
  "A page of LIMIT of ENTRIES ending BEFORE from the newest, and whether
older ones precede it."
  (let* ((n (length entries))
         (end (max 0 (- n before)))
         (start (max 0 (- end limit))))
    (list (seq-subseq entries start end) (> start 0))))

(defun e2e-conversation (params)
  (let* ((all (e2e-read (plist-get params :path)))
         (limit (or (plist-get params :limit) 60)))
    (if-let* ((start (plist-get params :start)))
        (let ((page (seq-subseq all (min start (length all)) (min (length all) (+ start limit)))))
          (list :cli "claude" :entries page :start start :total (length all)
                :earlier (> start 0) :later (< (+ start (length page)) (length all))))
      (pcase-let ((`(,page ,earlier) (e2e-page all limit (or (plist-get params :before) 0))))
        (list :cli "claude" :entries page :earlier earlier :limit limit)))))

(defun e2e-chat (params)
  "The room as one chat: each agent's words, asks and prompts, by time."
  (let (items)
    (dolist (a (append (plist-get params :agents) nil))
      (let ((name (plist-get a :name)))
        (dolist (e (last (e2e-read (plist-get a :path)) 150))
          (pcase (list (plist-get e :role) (plist-get e :kind))
            (`("assistant" ,_) (push (list :who name :kind "turn" :text (plist-get e :text) :ts (plist-get e :ts) :agent name) items))
            (`("user" "prompt") (push (list :who "user" :to name :kind "prompt" :text (plist-get e :text) :ts (plist-get e :ts) :agent name) items))
            (`("herd" "ask") (push (list :who name :to (plist-get e :to) :kind "ask" :ask (plist-get e :ask) :text (plist-get e :text)
                                         :answer (plist-get e :answer) :ts (plist-get e :ts) :agent name)
                                   items))))))
    (setq items (sort items (lambda (a b) (string< (plist-get a :ts) (plist-get b :ts)))))
    (pcase-let ((`(,page ,earlier) (e2e-page items (or (plist-get params :limit) 200) (or (plist-get params :before) 0))))
      (list :entries page :earlier earlier :missing nil))))

(defun e2e-find (params)
  (let* ((all (e2e-read (plist-get params :path)))
         (words (split-string (downcase (plist-get params :query)) nil t))
         (hits nil) (i 0))
    (dolist (e all)
      (let ((text (downcase (or (plist-get e :text) ""))))
        (when (and words (seq-every-p (lambda (w) (string-search w text)) words))
          (push (list :i i :role (plist-get e :role) :kind (plist-get e :kind) :who (plist-get e :who)
                      :ts (plist-get e :ts) :head (substring (plist-get e :text) 0 (min 24 (length (plist-get e :text))))
                      :snippet (substring (plist-get e :text) 0 (min 160 (length (plist-get e :text)))))
                hits)))
      (setq i (1+ i)))
    (list :hits (nreverse hits) :total (length hits) :entries (length all))))

(defun e2e-sidecar (method callback &optional params _error-callback)
  "Answer METHOD as the sidecar would, a few milliseconds later."
  (e2e-note (list :rpc method :limit (plist-get params :limit)))
  (let ((result (pcase method
                  ("herd_conversation" (e2e-conversation params))
                  ("herd_chat" (e2e-chat params))
                  ("herd_conversation_find" (e2e-find params))
                  ("herd_inbox_state" (list :state nil))
                  ("herd_pending_proposals" (list :proposals nil))
                  ("herd_threads" (list :live 0 :threads nil))
                  (_ nil))))
    (run-at-time 0.015 nil (lambda () (funcall callback result)))
    nil))
(advice-add 'ghostherd-memory-request-async :override #'e2e-sidecar)

;; ---- What reached Emacs, and when; what each `next' carried.

(advice-add 'xwapp--answer :before
            (lambda (_proc request _transport)
              (e2e-note (list :req (nth 0 request) :target (replace-regexp-in-string
                                                           "/[0-9a-f]\\{32\\}/" "/SECRET/" (nth 1 request))))))
(advice-add 'xwapp--intent :before
            (lambda (_app _proc _page body)
              (let ((it (ignore-errors (json-parse-string body :object-type 'alist))))
                (e2e-note (list :intent (alist-get 'op it)
                                :first (and (equal (alist-get 'op it) "room-watch") (alist-get 'first it))
                                :split (and (equal (alist-get 'op it) "room-watch") (alist-get 'split it)))))))
(advice-add 'xwapp--flush :before
            (lambda (app)
              (when (xwapp--get app :waiting)
                (e2e-note (list :flush (vconcat
                                        (mapcar (lambda (c)
                                                  (list :fn (if (string-match "\"fn\":\"\\([^\"]+\\)\"" c) (match-string 1 c) "?")
                                                        :bytes (string-bytes c)))
                                                (reverse (xwapp--get app :outbox))))
                                :later (length (xwapp--get app :later)))))))

;; The page's timings: kept in E2E_DIR, and each record noted in
;; timings.jsonl, as JSON.
(setq xwapp--timing-save t
      xwapp-timing-file (expand-file-name "timings.eld" e2e-dir))
(add-hook 'xwapp-timing-functions
          (lambda (_app record)
            (write-region (concat (json-encode (plist-put (copy-sequence record) :reqs
                                                          (vconcat (plist-get record :reqs))))
                                  "\n")
                          nil (expand-file-name "timings.jsonl" e2e-dir) t 'silent)))

;; The herd's poll, played: its tick notes how late it ran, as the real
;; one does first -- no tmux to ask.
(setq ghostherd--poll-timer (run-with-timer 1.5 1.5 #'ghostherd--note-held))

;; Emacs busy, on the test's word: POST /e2e-stall?ms=N on the socket is
;; answered at once, and Emacs then runs Lisp for N ms, reading nothing,
;; as herd-open.el's does.
(advice-add 'xwapp--answer :before-until
            (lambda (proc request _transport)
              (when (string-prefix-p "/e2e-stall?" (or (nth 1 request) ""))
                (let ((ms (string-to-number
                           (or (cadr (assoc "ms" (url-parse-query-string
                                                  (cadr (split-string (nth 1 request) "?")))))
                               "0"))))
                  (xwapp--respond proc 204)
                  (run-at-time 0 nil (lambda ()
                                       (e2e-note (list :stall ms))
                                       (let ((end (+ (float-time) (/ ms 1000.0))))
                                         (while (< (float-time) end)))))
                  t))))

(write-region (json-encode (list (cons "herd" (xwapp-url ghostherd-memory-page--app))
                                 (cons "secret" (xwapp--secret ghostherd-memory-page--app))))
              nil (expand-file-name "urls.json" e2e-dir) nil 'silent)
;; Waiting a little at a time: a batch Emacs waiting longer runs a timer
;; made by another timer -- the sidecar's answer, xwapp's flush after it --
;; only once the wait is over, which would add up to a second each to
;; what the page waits for.  An Emacs at its command loop does not.
(when noninteractive
  (while t (accept-process-output nil 0.005)))

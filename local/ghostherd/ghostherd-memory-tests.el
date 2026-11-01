;;; ghostherd-memory-tests.el --- Tests for ghostherd-memory.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Part of ghostherd's tests; ghostherd-tests.el runs them all.  This
;; file alone:
;;
;;   emacs --batch --init-directory=~/.emacs.d/emacs-home/ \
;;         -L . -L ../xwapp -l ghostherd-memory-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ghostherd-test-helpers)

;;; Memory sidecar (pure: no uvicorn)

(ert-deftest ghostherd-test-memory-server-directory-finds-main ()
  (let ((dir (ghostherd-memory--server-directory)))
    (should dir)
    (should (file-exists-p (expand-file-name "main.py" dir)))))

(ert-deftest ghostherd-test-memory-server-follows-straight-symlink ()
  "straight build dirs have .elc plus a symlink .el; server/ is in the checkout."
  (let* ((src (make-temp-file "gh-src-" t))
         (build (make-temp-file "gh-build-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "server" src) t)
          (write-region "pass\n" nil (expand-file-name "server/main.py" src))
          (write-region "" nil (expand-file-name "ghostherd.el" src))
          (make-symbolic-link (expand-file-name "ghostherd.el" src)
                              (expand-file-name "ghostherd.el" build))
          (write-region "" nil (expand-file-name "ghostherd.elc" build))
          (cl-letf (((symbol-function 'locate-library)
                     (lambda (name &rest _)
                       (cond
                        ((equal name "ghostherd")
                         (expand-file-name "ghostherd.elc" build))
                        ((equal name "ghostherd-memory") nil)))))
            (let ((ghostherd-memory-server-directory nil)
                  (load-file-name nil)
                  (buffer-file-name nil))
              (should (file-equal-p
                       (ghostherd-memory--server-directory)
                       (expand-file-name "server" src))))))
      (delete-directory src t)
      (delete-directory build t))))

(ert-deftest ghostherd-test-memory-server-next-to-build-elc ()
  "ecloud-style `:files (\"server\")` puts main.py beside the .elc."
  (let* ((build (make-temp-file "gh-build-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "server" build) t)
          (write-region "pass\n" nil (expand-file-name "server/main.py" build))
          (write-region "" nil (expand-file-name "ghostherd.elc" build))
          (cl-letf (((symbol-function 'locate-library)
                     (lambda (name &rest _)
                       (when (equal name "ghostherd")
                         (expand-file-name "ghostherd.elc" build)))))
            (let ((ghostherd-memory-server-directory nil)
                  (load-file-name nil)
                  (buffer-file-name nil))
              (should (file-equal-p
                       (ghostherd-memory--server-directory)
                       (expand-file-name "server" build))))))
      (delete-directory build t))))

(ert-deftest ghostherd-test-memory-health-url-matches-rpc ()
  (let ((ghostherd-memory-host "127.0.0.1")
        (ghostherd-memory-port 49152)
        (ghostherd-memory--port 49160))
    (cl-letf (((symbol-function 'ghostherd-memory--token) (lambda () "tok")))
      (should (equal (ghostherd-memory--rpc-url)
                     "http://127.0.0.1:49160/jsonrpc/tok"))
      (should (equal (ghostherd-memory--health-url)
                     "http://127.0.0.1:49160/health"))
      (should (equal (ghostherd-memory--rpc-url 49153)
                     "http://127.0.0.1:49153/jsonrpc/tok")))))

(ert-deftest ghostherd-test-memory-port-candidates-start-high ()
  (let ((ghostherd-memory-port 49152)
        (ghostherd-memory-port-tries 32))
    (should (equal (car (ghostherd-memory--port-candidates)) 49152))
    (should (equal (car (last (ghostherd-memory--port-candidates))) 49183))
    (should (< 8766 (car (ghostherd-memory--port-candidates))))))

(ert-deftest ghostherd-test-memory-allocate-skips-occupied ()
  "A listener that is not ours is not a bind target."
  (let ((ghostherd-memory-port 49152)
        (ghostherd-memory-port-tries 4)
        (busy '(49152 49153)))
    (cl-letf (((symbol-function 'ghostherd-memory--listening-p)
               (lambda (port) (memq port busy))))
      (should (equal (ghostherd-memory--allocate-port) 49154)))))

(ert-deftest ghostherd-test-memory-find-running-requires-our-health ()
  "HTTP 200 on a busy port is not enough — ecloud is 200 too."
  (let ((ghostherd-memory-port 49152)
        (ghostherd-memory-port-tries 3))
    (cl-letf (((symbol-function 'ghostherd-memory--listening-p)
               (lambda (_) t))
              ((symbol-function 'ghostherd-memory--health-ours-p)
               (lambda (port) (= port 49153))))
      (should (equal (ghostherd-memory--find-running) 49153)))))

(ert-deftest ghostherd-test-memory-health-body-is-this-sidecar ()
  "Ours means our service *on our token*: a sidecar on another token,
or from before tokens, would refuse every call."
  (cl-letf (((symbol-function 'ghostherd-memory--token) (lambda () "abc")))
    (should (ghostherd-memory--health-body-ours-p
             "{\"status\":\"ok\",\"service\":\"ghostherd-memory\",\"token_id\":\"ba7816bf8f01\"}"))
    (should (eq (ghostherd-memory--health-kind
                 "{\"status\":\"ok\",\"service\":\"ghostherd-memory\",\"token_id\":\"000000000000\"}")
                'stale))
    (should (eq (ghostherd-memory--health-kind
                 "{\"status\":\"ok\",\"service\":\"ghostherd-memory\"}")
                'stale))
    (should-not (ghostherd-memory--health-body-ours-p
                 "{\"status\":\"ok\"}"))
    (should-not (ghostherd-memory--health-kind "not json")))
  (cl-letf (((symbol-function 'ghostherd-memory--token) (lambda () nil)))
    (should (eq (ghostherd-memory--health-kind
                 "{\"service\":\"ghostherd-memory\",\"token_id\":\"ba7816bf8f01\"}")
                'stale))))

(ert-deftest ghostherd-test-memory-token-from-the-data-directory ()
  "Read from rpc.token, which the sidecar writes; fingerprinted the way
its /health does (sha256(\"abc\") starts ba7816bf8f01)."
  (let* ((dir (make-temp-file "gh-mem-" t))
         (ghostherd-memory-data-directory dir))
    (unwind-protect
        (progn
          (should-not (ghostherd-memory--token))
          (should-error (ghostherd-memory--rpc-url 49152) :type 'user-error)
          (let ((ghostherd-memory--port 49152))
            (should-not (ghostherd-memory-rpc-url)))
          (write-region "abc\n" nil (expand-file-name "rpc.token" dir))
          (should (equal (ghostherd-memory--token) "abc"))
          (should (equal (ghostherd-memory--token-id "abc") "ba7816bf8f01"))
          (let ((ghostherd-memory--port 49152)
                (ghostherd-memory-host "127.0.0.1"))
            (should (equal (ghostherd-memory-rpc-url)
                           "http://127.0.0.1:49152/jsonrpc/abc"))))
      (delete-directory dir t))))

(ert-deftest ghostherd-test-memory-rpc-locator-is-private ()
  "rpc.url carries a token -- the agents', since an agent falls back to it,
never the herd's own: 0600, even over a file that was 0644."
  (let* ((home (make-temp-file "gh-home-" t))
         (user-emacs-directory (file-name-as-directory home))
         (file (expand-file-name "ghostherd-mail/rpc.url" home)))
    (unwind-protect
        (cl-letf (((symbol-function 'ghostherd-memory-agent-rpc-url)
                   (lambda () "http://127.0.0.1:49152/jsonrpc/abc"))
                  ((symbol-function 'ghostherd-memory-rpc-url)
                   (lambda () "http://127.0.0.1:49152/jsonrpc/admin")))
          (make-directory (file-name-directory file) t)
          (write-region "old" nil file)
          (set-file-modes file #o644)
          (ghostherd-memory--write-rpc-locator)
          (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string))
                         "http://127.0.0.1:49152/jsonrpc/abc"))
          (should (= (file-modes file) #o600)))
      (delete-directory home t))))

(ert-deftest ghostherd-test-memory-start-refuses-a-foreign-stale-sidecar ()
  "Walking past it would start a second sidecar on the same Qdrant lock."
  (let ((ghostherd-memory-port 49152)
        (ghostherd-memory-port-tries 2)
        (ghostherd-memory--process nil)
        (ghostherd-memory--port nil)
        spawned)
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/usr/bin/uv"))
              ((symbol-function 'ghostherd-memory-healthy-p) #'ignore)
              ((symbol-function 'ghostherd-memory--find-running) #'ignore)
              ((symbol-function 'ghostherd-memory--server-directory) (lambda () "/tmp"))
              ((symbol-function 'ghostherd-memory--listening-p) (lambda (p) (= p 49152)))
              ((symbol-function 'ghostherd-memory--health-kind-at)
               (lambda (&optional p) (and (eql p 49152) 'stale)))
              ((symbol-function 'ghostherd-memory--spawn)
               (lambda (&rest _) (setq spawned t))))
      (let ((err (should-error (ghostherd-memory-start) :type 'user-error)))
        (should (string-match-p "port 49152" (cadr err)))
        (should (string-match-p "lsof -ti tcp:49152" (cadr err))))
      (should-not spawned))))

(ert-deftest ghostherd-test-memory-start-restarts-its-own-stale-sidecar ()
  (let ((ghostherd-memory-port 49152)
        (ghostherd-memory--port 49152)
        stopped messages)
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/usr/bin/uv"))
              ((symbol-function 'ghostherd-memory--process-live-p) (lambda () (not stopped)))
              ((symbol-function 'ghostherd-memory--health-kind-at)
               (lambda (&optional _) (if stopped 'ours 'stale)))
              ((symbol-function 'ghostherd-memory-stop) (lambda () (setq stopped t)))
              ((symbol-function 'sleep-for) #'ignore)
              ((symbol-function 'ghostherd-memory-healthy-p) (lambda () stopped))
              ((symbol-function 'ghostherd-memory--write-rpc-locator) #'ignore)
              ((symbol-function 'ghostherd-memory--token) (lambda () "s3cret"))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (should (ghostherd-memory-start))
      (should stopped)
      ;; Messages name the port, never the URL: it carries the token.
      (should messages)
      (should-not (cl-some (lambda (m) (string-search "s3cret" m)) messages)))))

(ert-deftest ghostherd-test-memory-build-request-is-jsonrpc-2 ()
  (let ((ghostherd-memory--request-id 0)
        (req (ghostherd-memory--build-request "memory_search"
                                              (list :query "posframe"))))
    (should (equal (plist-get req :jsonrpc) "2.0"))
    (should (equal (plist-get req :method) "memory_search"))
    (should (equal (plist-get (plist-get req :params) :query) "posframe"))
    (should (numberp (plist-get req :id)))))

(ert-deftest ghostherd-test-memory-utf8-unibyte-json ()
  "url.el leaves JSON unibyte; 你 must not display as \\344\\275\\240."
  (let* ((json "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"text\":\"你分析\"}}")
         (raw (encode-coding-string json 'utf-8)))
    (should (equal (plist-get (ghostherd-memory--parse-response raw) :text)
                   "你分析"))))

(ert-deftest ghostherd-test-memory-parse-result-and-error ()
  (let ((ok (ghostherd-memory--parse-response
             "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"ok\":true},\"error\":null}")))
    (should (eq (plist-get ok :ok) t)))
  (should-error
   (ghostherd-memory--parse-response
    "{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":{\"code\":-32601,\"message\":\"nope\"}}")))

(ert-deftest ghostherd-test-memory-request-body-is-what-json-encode-writes ()
  "Written in C where it can be, by `json-encode' where it cannot; the
same JSON either way, in UTF-8 bytes."
  (let ((ghostherd-memory--request-id 0)
        (encoded nil)
        (real-encode (symbol-function 'json-encode)))
    (cl-letf (((symbol-function 'json-encode)
               (lambda (o) (setq encoded t) (funcall real-encode o))))
      (dolist (case `((nil (:sessions [(:name "rd" :screen "你好 ✓\n" :notes nil)] :ack_ids [] :replies []))
                      (nil (:id "a1" :dead :json-false :done t :n 2.5 :agents [(:name "x" :path "/p")]))
                      (nil nil)
                      (t (:agents ("claude" "grok")))
                      (t (:kind claude))
                      (t (:screen ,(string-to-multibyte "bad \377 byte")))))
        (setq encoded nil)
        (let* ((params (cadr case))
               (body (ghostherd-memory--request-body "herd_tick" params))
               (read (lambda (s) (json-parse-string (decode-coding-string s 'utf-8)
                                                    :object-type 'alist :null-object :null
                                                    :false-object :false))))
          (should (eq encoded (car case)))
          (should-not (multibyte-string-p body))
          (setq ghostherd-memory--request-id 0)
          (should (equal (funcall read body)
                         (funcall read (encode-coding-string
                                        (funcall real-encode
                                                 (ghostherd-memory--build-request "herd_tick" params))
                                        'utf-8))))
          (setq ghostherd-memory--request-id 0))))))

(ert-deftest ghostherd-test-memory-response-reads-as-json-read-did ()
  "Objects plists with keyword keys, arrays lists, null and false nil."
  (let ((body "{\"jsonrpc\":\"2.0\",\"id\":3,\"result\":{\"pending\":[{\"id\":1,\"to\":\"rd\",\"submit\":false}],\"asks\":[],\"links\":{},\"n\":1.5,\"ok\":true,\"none\":null}}"))
    (should (equal (ghostherd-memory--parse-response body)
                   '(:pending ((:id 1 :to "rd" :submit nil)) :asks nil :links nil :n 1.5 :ok t :none nil))))
  (should-error (ghostherd-memory--parse-response ""))
  (should-error (ghostherd-memory--parse-response "{\"result\":")))

(defmacro ghostherd-tests--with-sidecar-http (&rest body)
  "BODY with url.el faked: `answer' lands a reply on the last request.
Each request's buffer is in `bufs'; `timers' holds the deadlines set,
as (SECS . FN), and `later' what was put off until next tick."
  (declare (indent 0))
  `(let ((bufs nil) (timers nil) (later nil) (landing nil) (refuse nil))
     (cl-letf (((symbol-function 'ghostherd-memory--rpc-url) (lambda () "http://127.0.0.1:1/rpc"))
               ((symbol-function 'url-retrieve)
                (lambda (_url cb &rest _)
                  (when refuse (signal 'wrong-type-argument (list 'stringp nil)))
                  (let ((b (generate-new-buffer " *gh-sidecar-test*")))
                    (push b bufs)
                    (setq landing cb)
                    b)))
               ((symbol-function 'run-with-timer)
                (lambda (secs _repeat fn &rest args)
                  (push (cons secs (lambda () (apply fn args))) timers)
                  (car timers)))
               ((symbol-function 'cancel-timer)
                (lambda (timer) (setq timers (delq timer timers))))
               ((symbol-function 'run-at-time)
                (lambda (_secs _repeat fn &rest args)
                  (push (lambda () (apply fn args)) later))))
       (cl-flet ((answer (status &optional body)
                   (with-current-buffer (car bufs)
                     (insert "HTTP/1.1 200 OK\r\n\r\n" (or body ""))
                     (funcall landing status))))
         (unwind-protect (progn ,@body)
           (mapc #'kill-buffer (seq-filter #'buffer-live-p bufs)))))))

(ert-deftest ghostherd-test-memory-async-answer-leaves-no-buffer ()
  "Every answer's buffer goes, read or failed.  Kept, they piled up in
the tens of thousands, and each garbage collection walked them all."
  (ghostherd-tests--with-sidecar-http
    (let (got failed)
      (ghostherd-memory-request-async
       "herd_tick" (lambda (r) (push r got)) nil (lambda (e) (push e failed)))
      (answer nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"ok\":true}}")
      (should (equal got '((:ok t))))
      (should-not (buffer-live-p (car bufs)))
      (should-not timers)
      (ghostherd-memory-request-async
       "herd_tick" (lambda (r) (push r got)) nil (lambda (e) (push e failed)))
      (answer '(:error (error connection-failed "refused")))
      (should (equal failed '("(error connection-failed refused)")))
      (should-not (buffer-live-p (car bufs)))
      (should (= (length got) 1)))))

(ert-deftest ghostherd-test-memory-async-unanswered-is-given-up-once ()
  "url.el can drop a request; the herd tick would then wait forever."
  (ghostherd-tests--with-sidecar-http
    (let (got failed)
      (ghostherd-memory-request-async
       "herd_tick" (lambda (r) (push r got)) nil (lambda (e) (push e failed)))
      (should (equal (mapcar #'car timers) (list ghostherd-memory-request-timeout)))
      (funcall (cdar timers))
      (should (equal failed (list (format "herd_tick: no answer in %ss"
                                          ghostherd-memory-request-timeout))))
      (should-not (buffer-live-p (car bufs)))
      ;; Too late: it was given up.
      (let ((b (generate-new-buffer " *gh-sidecar-test*")))
        (push b bufs)
        (answer nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"ok\":true}}"))
      (funcall (cdar timers))
      (should-not got)
      (should (= (length failed) 1)))))

(ert-deftest ghostherd-test-memory-async-import-is-waited-on ()
  "An import answers when it is done; twenty minutes is not a failure."
  (ghostherd-tests--with-sidecar-http
    (ghostherd-memory-request-async "memory_import" #'ignore nil #'ignore)
    (should-not timers)))

(ert-deftest ghostherd-test-memory-async-refused-at-once-fails-later ()
  "url.el can fail the call itself; the caller still hears, once,
and only after the call returns, as from any answer."
  (ghostherd-tests--with-sidecar-http
    (setq refuse t)
    (let (failed)
      (should-not (ghostherd-memory-request-async
                   "herd_tick" #'ignore nil (lambda (e) (push e failed))))
      (should-not failed)
      (should-not timers)
      (mapc #'funcall later)
      (should (equal failed '("Wrong type argument: stringp, nil"))))))

(ert-deftest ghostherd-test-memory-menu-lists-search ()
  (should (equal (nth 2 (assoc "/" ghostherd-menu-choices))
                 'ghostherd-memory-search))
  (should (equal (nth 2 (assoc "v" ghostherd-menu-choices))
                 'ghostherd-memory-view))
  (should (equal (nth 2 (assoc "I" ghostherd-menu-choices))
                 'ghostherd-memory-import)))

(ert-deftest ghostherd-test-memory-source-label-uses-basename ()
  (should (string-match-p "chat_history.jsonl"
                          (ghostherd-memory--source-label
                           '(:agent "grok" :chunks 3 :project "/tmp/p"
                             :source_path "/tmp/p/sess/chat_history.jsonl")))))

(ert-deftest ghostherd-test-memory-cmd-import-force-flag ()
  "The CLI passes the string force, not a Lisp t, through emacsclient."
  (let ((got nil))
    (cl-letf (((symbol-function 'ghostherd-memory-ensure) #'ignore)
              ((symbol-function 'ghostherd-memory-request)
               (lambda (method params &optional _timeout)
                 (setq got (list method params))
                 '(:imported 0 :skipped 0 :sessions 0 :errors nil))))
      (ghostherd-cmd-memory-import "force")
      (should (equal (car got) "memory_import"))
      (should (eq (plist-get (cadr got) :force) t))
      (setq got nil)
      (ghostherd-cmd-memory-import)
      (should (equal (cadr got) nil)))))

;;; Background import

(defmacro ghostherd-test--with-auto (&rest body)
  "Run BODY with a temp data directory and sidecar requests recorded.
`methods' lists the methods asked, oldest first; POINTS is what
memory_status reports; FAIL, when a string, fails every request."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "gh-auto-" t))
          (ghostherd-memory-data-directory dir)
          (ghostherd-memory-auto-import-interval 86400)
          (ghostherd-memory--port 49152)
          (ghostherd-memory--auto-inflight nil)
          (ghostherd-memory--auto-failing nil)
          (methods nil) (points 5) (fail nil) (said nil))
     (unwind-protect
         (cl-letf (((symbol-function 'ghostherd-memory-request-async)
                    (lambda (method cb &optional _params err-cb)
                      (setq methods (append methods (list method)))
                      (cond (fail (funcall err-cb fail))
                            ((equal method "memory_status") (funcall cb (list :points points)))
                            (t (funcall cb '(:imported 12 :sessions 3))))))
                   ((symbol-function 'message)
                    (lambda (fmt &rest args) (push (apply #'format fmt args) said))))
           ,@body)
       (delete-directory dir t))))

(ert-deftest ghostherd-test-memory-auto-import-when-due ()
  "Once a day, stamped on disk so restarts do not repeat it."
  (ghostherd-test--with-auto
    (should (ghostherd-memory--auto-due-p))
    (ghostherd-memory--auto-import-maybe)
    (should (equal methods '("memory_status" "memory_import")))
    (should-not ghostherd-memory--auto-inflight)
    (should (string-match-p "added 12 chunks from 3 sessions" (car said)))
    (should-not (ghostherd-memory--auto-due-p))
    (should (ghostherd-memory--auto-due-p (time-add (current-time) 86401)))
    (setq methods nil)
    (ghostherd-memory--auto-import-maybe)
    (should-not methods)))

(ert-deftest ghostherd-test-memory-auto-import-never-the-first ()
  "An empty index waits for a deliberate first import: that one
downloads a model and embeds everything."
  (ghostherd-test--with-auto
    (setq points 0)
    (ghostherd-memory--auto-import-maybe)
    (should (equal methods '("memory_status")))
    (should-not (file-exists-p (ghostherd-memory--auto-stamp)))
    (should-not ghostherd-memory--auto-inflight)))

(ert-deftest ghostherd-test-memory-auto-import-retries-in-an-hour ()
  "A missed day is how transcripts are lost, so a failure is not a day."
  (ghostherd-test--with-auto
    (setq fail "connection refused")
    (ghostherd-memory--auto-import-maybe)
    (should-not ghostherd-memory--auto-inflight)
    (should (string-match-p "background import failed" (car said)))
    (should-not (ghostherd-memory--auto-due-p))
    (should-not (ghostherd-memory--auto-due-p (time-add (current-time) 3500)))
    (should (ghostherd-memory--auto-due-p (time-add (current-time) 3700)))
    ;; Said once: a sidecar that stays down is not news every 5 minutes.
    (set-file-times (ghostherd-memory--auto-stamp) (time-subtract (current-time) 90000))
    (ghostherd-memory--auto-import-maybe)
    (should (= (length said) 1))
    ;; A short interval retries within it, not an hour later.
    (let ((ghostherd-memory-auto-import-interval 300))
      (set-file-times (ghostherd-memory--auto-stamp) (time-subtract (current-time) 90000))
      (ghostherd-memory--auto-import-maybe)
      (should-not (ghostherd-memory--auto-due-p (time-add (current-time) 250)))
      (should (ghostherd-memory--auto-due-p (time-add (current-time) 310))))
    ;; A success clears it, so the next failure is reported again.
    (setq fail nil)
    (set-file-times (ghostherd-memory--auto-stamp) (time-subtract (current-time) 90000))
    (ghostherd-memory--auto-import-maybe)
    (should-not ghostherd-memory--auto-failing)))

(ert-deftest ghostherd-test-memory-auto-import-holds-back ()
  "Not while one runs, not without a sidecar, not when switched off."
  (ghostherd-test--with-auto
    (let ((ghostherd-memory--auto-inflight t))
      (ghostherd-memory--auto-import-maybe))
    (let ((ghostherd-memory--port nil))
      (ghostherd-memory--auto-import-maybe))
    (let ((ghostherd-memory-auto-import-interval nil))
      (ghostherd-memory--auto-import-maybe)
      (should-not (ghostherd-memory--auto-due-p)))
    (should-not methods)))

(ert-deftest ghostherd-test-memory-auto-import-timer ()
  (let ((ghostherd-memory--auto-timer nil)
        (ghostherd-memory-auto-import-interval 86400))
    (unwind-protect
        (progn
          (ghostherd-memory-auto-import-start)
          (should (timerp ghostherd-memory--auto-timer))
          (should (eq (timer--function ghostherd-memory--auto-timer)
                      #'ghostherd-memory--auto-import-maybe))
          (ghostherd-memory-auto-import-stop)
          (should-not ghostherd-memory--auto-timer)
          (let ((ghostherd-memory-auto-import-interval nil))
            (ghostherd-memory-auto-import-start)
            (should-not ghostherd-memory--auto-timer)))
      (ghostherd-memory-auto-import-stop))))

(provide 'ghostherd-memory-tests)
;;; ghostherd-memory-tests.el ends here

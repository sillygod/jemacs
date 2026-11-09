;;; xwapp-tests.el --- Tests for xwapp  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;   emacs --batch -L ~/.emacs.d/local/xwapp \
;;         -l xwapp.el -l xwapp-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'xwapp)

;; The tests stub xwidget primitives with `cl-letf'.  xwapp.el runs from
;; source here, so the stubs need no native trampoline -- and compiling
;; one would make the suite depend on a working libgccjit.
(setq native-comp-enable-subr-trampolines nil)

(ert-deftest xwapp-test-absolutize-location ()
  (should (equal (xwapp-absolutize
                  "https://api.bitbucket.org/2.0/x"
                  "https://api.bitbucket.org/2.0/y")
                 "https://api.bitbucket.org/2.0/x"))
  (should (equal (xwapp-absolutize
                  "/2.0/repositories/ws/r/diff/aa..bb"
                  "https://api.bitbucket.org/2.0/repositories/ws/r/pullrequests/1/diff")
                 "https://api.bitbucket.org/2.0/repositories/ws/r/diff/aa..bb")))

(ert-deftest xwapp-test-utf8-body-is-unibyte ()
  (let ((s (xwapp-utf8-bytes (json-encode '((title . "feat → master"))))))
    (should-not (multibyte-string-p s))
    (should (string-match-p "feat" s))))

(ert-deftest xwapp-test-scrub-hides-bearer ()
  (should (string-match-p "\\*\\*\\*"
                          (xwapp-scrub-error
                           "Authorization: Bearer ATATT-secret extra"))))

(ert-deftest xwapp-test-extra-headers-drop-accept ()
  (let ((h (xwapp-extra-headers
            '(("Accept" . "application/vnd.github.diff")
              ("Authorization" . "Bearer x")
              ("X-GitHub-Api-Version" . "2022-11-28")))))
    (should-not (assoc "Accept" h))
    (should (equal (cdr (assoc "Authorization" h)) "Bearer x"))
    (should (assoc "X-GitHub-Api-Version" h))))

(ert-deftest xwapp-test-extra-headers-json-content-type ()
  (let ((h (xwapp-extra-headers '(("Accept" . "application/json")) t)))
    (should (equal (cdr (assoc "Content-Type" h))
                   "application/json; charset=utf-8"))
    (should-not (assoc "Accept" h))))

(ert-deftest xwapp-test-scrub-hides-bare-token ()
  "ClickUp sends its token without Bearer; it must not leak either."
  (let ((out (xwapp-scrub-error "Authorization: pk_123_SECRET extra")))
    (should-not (string-match-p "pk_123_SECRET" out))
    (should (string-match-p "Authorization: \\*\\*\\* extra" out)))
  (let ((out (xwapp-scrub-error "Authorization: Bearer ATATT-secret extra")))
    (should-not (string-match-p "ATATT" out))
    (should (string-match-p "Authorization: Bearer \\*\\*\\*" out))))

(defun xwapp-test--run-callback (body code err ok-fn)
  "Feed BODY CODE ERR through `xwapp-json-callback'.
Return (OK-ARGS . FAIL-ARGS), each nil when not called."
  (let (ok fail)
    (funcall (xwapp-json-callback
              "u"
              (lambda (raw) (setq ok (list raw)) (funcall ok-fn raw))
              (lambda (&rest args) (setq fail args)))
             body code err)
    (cons ok fail)))

(ert-deftest xwapp-test-json-callback-ok ()
  (let ((r (xwapp-test--run-callback "{\"a\":1,\"f\":false,\"n\":null}" 200 nil #'ignore)))
    (should (equal (car r) '(((a . 1) (f) (n)))))
    (should-not (cdr r))))

(ert-deftest xwapp-test-json-callback-blank-body ()
  (let ((r (xwapp-test--run-callback "  " 204 nil #'ignore)))
    (should (equal (car r) '(nil)))
    (should-not (cdr r))))

(ert-deftest xwapp-test-json-callback-http-error ()
  (let ((r (xwapp-test--run-callback "{\"err\":\"nope\"}" 404 nil #'ignore)))
    (should-not (car r))
    (should (equal (cdr r) '(404 nil "{\"err\":\"nope\"}" "u")))))

(ert-deftest xwapp-test-json-callback-network-error ()
  (let ((r (xwapp-test--run-callback nil nil '(error connection-failed) #'ignore)))
    (should-not (car r))
    (should (equal (cdr r) '(nil (error connection-failed) nil "u")))))

(ert-deftest xwapp-test-json-callback-bad-json ()
  "A 2xx that does not parse reaches FAIL-FN with the condition."
  (let ((r (xwapp-test--run-callback "<html>" 200 nil #'ignore)))
    (should-not (car r))
    (should (equal (nth 0 (cdr r)) 200))
    (should (nth 1 (cdr r)))
    (should-not (nth 2 (cdr r)))))

(ert-deftest xwapp-test-json-callback-handler-error ()
  "A bug in OK-FN is reported, not lost in a url.el timer."
  (let ((r (xwapp-test--run-callback "{}" 200 nil
                                     (lambda (_) (error "boom")))))
    (should (equal (nth 0 (cdr r)) 200))
    (should (equal (nth 1 (cdr r)) '(error "boom")))))

(ert-deftest xwapp-test-http-json-forwards-method-and-body ()
  (let (seen got)
    (cl-letf (((symbol-function 'xwapp-http)
               (lambda (url headers cb method body)
                 (setq seen (list url headers method body))
                 (funcall cb "{\"x\":1}" 200 nil))))
      (xwapp-http-json "u" '(("A" . "b")) (lambda (raw) (setq got raw)) #'ignore
                       "PUT" '((status . "done"))))
    (should (equal seen '("u" (("A" . "b")) "PUT" ((status . "done")))))
    (should (equal got '((x . 1))))))

(ert-deftest xwapp-test-parse-intent ()
  (should (equal (alist-get 'op (xwapp-parse-intent "cu:{\"op\":\"open\"}" '("cu:")))
                 "open"))
  (should (equal (alist-get 'id (xwapp-parse-intent "old:{\"id\":3}" '("cu:" "old:")))
                 3))
  (should (equal (alist-get 'states (xwapp-parse-intent "cu:{\"states\":[\"a\"]}" '("cu:")))
                 '("a")))
  (should-not (xwapp-parse-intent "ClickUp" '("cu:")))
  (should-not (xwapp-parse-intent "cu:{not json" '("cu:")))
  (should-not (xwapp-parse-intent nil '("cu:"))))

(defun xwapp-test--app (&rest props)
  (apply #'xwapp-create
         (append props
                 (list :buffer-name " *xwapp-test*" :index "/nonexistent"
                       :prefixes '("t:") :namespace "T" :idle-title "Idle"))))

(ert-deftest xwapp-test-js-calls-namespace ()
  (let ((app (xwapp-test--app)) script)
    (cl-letf (((symbol-function 'xwapp-session) (lambda (_) 'xw))
              ((symbol-function 'xwidget-webkit-execute-script)
               (lambda (_xw s) (setq script s))))
      (xwapp-js app "render" '((a . "say \"hi\""))))
    (should (equal script
                   "window.T && T[\"render\"]({\"a\":\"say \\\"hi\\\"\"});"))))

(ert-deftest xwapp-test-js-without-page-is-noop ()
  (let ((app (xwapp-test--app)) called)
    (cl-letf (((symbol-function 'xwapp-session) (lambda (_) nil))
              ((symbol-function 'xwidget-webkit-execute-script)
               (lambda (&rest _) (setq called t))))
      (xwapp-js app "render" nil))
    (should-not called)))

(ert-deftest xwapp-test-poll-dispatches-once-and-resets-title ()
  (let* ((seen nil)
         (app (xwapp-test--app :handler (lambda (i) (push i seen))))
         (title "t:{\"op\":\"refresh\"}")
         scripts)
    (cl-letf (((symbol-function 'xwapp-session) (lambda (_) 'xw))
              ((symbol-function 'xwidget-webkit-title) (lambda (_) title))
              ((symbol-function 'xwidget-webkit-execute-script)
               (lambda (_xw s) (push s scripts))))
      (xwapp--poll app)
      ;; The title clear is async: a poll before it lands sees the same title.
      (xwapp--poll app)
      (should (= (length seen) 1))
      (should (equal (alist-get 'op (car seen)) "refresh"))
      (should (member (xwapp-test--title-answer "read" nil t) scripts))
      (setq title "Idle")
      (xwapp--poll app)
      (setq title "t:{\"op\":\"open\",\"id\":7}")
      (xwapp--poll app)
      (should (= (length seen) 2))
      (should (equal (alist-get 'id (car seen)) 7)))))

(ert-deftest xwapp-test-session-stays-in-its-buffer ()
  "With no live widget of its own, an app gets nil -- never the
global last session, which may be another app's page."
  (let ((app (xwapp-test--app)))
    (with-current-buffer (get-buffer-create " *xwapp-test*")
      (unwind-protect
          (cl-letf (((symbol-function 'get-buffer-xwidgets) (lambda (_) nil))
                    ((symbol-function 'xwidget-live-p) (lambda (xw) (eq xw 'mine)))
                    ((symbol-function 'xwidget-webkit-last-session) (lambda () 'other))
                    ((symbol-function 'xwidget-webkit-current-session) (lambda () 'other)))
            (should-not (xwapp-session app))
            (setq-local xwapp--xw 'mine)
            (should (eq (xwapp-session app) 'mine))
            (setq-local xwapp--xw 'dead)
            (should-not (xwapp-session app)))
        (kill-buffer)))))

;;; In a web browser
;;
;; A real server on 127.0.0.1 and a client that speaks HTTP to it, as a
;; browser would.  The page tree is a temp directory: app/ui is the
;; app's page, xwapp/ui the kit, the rest is what must not be served.

(defmacro xwapp-test--with-browser (&rest body)
  "Run BODY with `app' on a page tree under `root', served afresh.
`intents' gathers what its handler gets, `closed' counts its on-kill,
`reopened' its reopen, once BODY gives it one.  Addresses are kept in
`root', and only when BODY binds `xwapp--keep'."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "xwapp" t)))
          (xwapp--server nil)
          (xwapp--unix-server nil)
          (xwapp-remote-socket (expand-file-name "sock/remote.sock" root))
          (xwapp--served (make-hash-table :test 'eq))
          (xwapp-state-file (expand-file-name "state.eld" root))
          (xwapp--keep nil)
          (xwapp--state-ours nil)
          (intents nil)
          (closed 0)
          (reopened 0)
          (app (progn
                 (dolist (f '("app/ui/index.html" "app/ui/app.js" "app/notes.txt"
                              "xwapp/ui/xwapp.js" "other/ui/x.js"))
                   (make-directory (file-name-directory (expand-file-name f root)) t)
                   (with-temp-file (expand-file-name f root) (insert "<" f ">")))
                 (xwapp-test--app :index (expand-file-name "app/ui/index.html" root)
                                  :handler (lambda (i) (push i intents))
                                  :on-kill (lambda () (setq closed (1+ closed)))))))
     (ignore intents closed reopened)
     (unwind-protect (progn (xwapp-url app) ,@body)
       (maphash (lambda (a _) (dolist (k '(:hold :flush :gone :forget)) (xwapp--untimer a k)))
                xwapp--served)
       (when (get-buffer " *xwapp-test*")
         (let ((kill-buffer-hook nil)) (kill-buffer " *xwapp-test*")))
       (when xwapp--server (delete-process xwapp--server))
       (when xwapp--unix-server (delete-process xwapp--unix-server))
       (delete-directory root t))))

(defun xwapp-test--proxy-up ()
  "Bring up the proxy's socket, as `xwapp--port' does under
`xwapp-remote-hosts'.  For use once a test has bound the hosts."
  (xwapp--port)
  (unless (process-live-p xwapp--unix-server)
    (error "proxy socket not listening")))

(defun xwapp-test--ask (method path &optional body headers transport)
  "Send METHOD PATH with BODY and HEADERS.  Return a cell whose car
becomes (CODE . BODY) once the server closes; its cdr is the client.
TRANSPORT `proxy' goes over `xwapp-remote-socket', as the proxy does;
else loopback, as the browser does."
  (let* ((cell (list nil))
         (port (process-contact xwapp--server :service))
         (out "")
         (proc (if (eq transport 'proxy)
                   (make-network-process
                    :name "xwapp-test-proxy" :family 'local
                    :service (expand-file-name xwapp-remote-socket)
                    :coding 'binary :noquery t
                    :filter (lambda (_p s) (setq out (concat out s)))
                    :sentinel
                    (lambda (_p _e)
                      (unless (car cell)
                        (setcar cell
                                (if (string-match "\\`HTTP/1.1 \\([0-9]+\\)" out)
                                    (cons (string-to-number (match-string 1 out))
                                          (decode-coding-string
                                           (substring out (+ 4 (string-search "\r\n\r\n" out)))
                                           'utf-8))
                                  (cons 0 out))))))
                 (make-network-process
                  :name "xwapp-test-client" :host "127.0.0.1" :service port
                  :coding 'binary :noquery t
                  :filter (lambda (_p s) (setq out (concat out s)))
                  :sentinel
                  (lambda (_p _e)
                    (unless (car cell)
                      (setcar cell
                              (if (string-match "\\`HTTP/1.1 \\([0-9]+\\)" out)
                                  (cons (string-to-number (match-string 1 out))
                                        (decode-coding-string
                                         (substring out (+ 4 (string-search "\r\n\r\n" out)))
                                         'utf-8))
                                (cons 0 out))))))))
         (bytes (encode-coding-string (or body "") 'utf-8)))
    (setcdr cell proc)
    (process-send-string
     proc
     (concat method " " path " HTTP/1.1\r\n"
             (unless (assoc "Host" headers) (format "Host: 127.0.0.1:%d\r\n" port))
             (mapconcat (lambda (h) (format "%s: %s\r\n" (car h) (cdr h))) headers "")
             (format "Content-Length: %d\r\n\r\n" (length bytes))
             bytes))
    cell))

(defun xwapp-test--answer (cell)
  "The (CODE . BODY) CELL is to get, waiting for it."
  (with-timeout (5 (error "No answer"))
    (while (not (car cell))
      (accept-process-output nil 0.02)))
  (car cell))

(defun xwapp-test--do (method path &optional body headers transport)
  (xwapp-test--answer (xwapp-test--ask method path body headers transport)))

(defun xwapp-test--turn (&optional secs)
  "Let the server and its timers run for SECS, 0.1 by default."
  (let ((end (+ (float-time) (or secs 0.1))))
    (while (< (float-time) end)
      (accept-process-output nil 0.02))))

(defun xwapp-test--at (app path)
  (concat "/" (xwapp--secret app) "/" path))

(ert-deftest xwapp-test-browser-serves-the-page-and-the-kit-only ()
  "The page's own files and xwapp's kit, under the app's secret, to this
host; nothing else, whatever the path says."
  (xwapp-test--with-browser
    (should (string-match-p "\\`http://127\\.0\\.0\\.1:[0-9]+/[0-9a-f]\\{32\\}/app/ui/index\\.html\\'"
                            (xwapp-url app)))
    (should (equal (xwapp-test--do "GET" (xwapp-test--at app "app/ui/index.html"))
                   '(200 . "<app/ui/index.html>")))
    (should (equal (xwapp-test--do "GET" (xwapp-test--at app "xwapp/ui/xwapp.js"))
                   '(200 . "<xwapp/ui/xwapp.js>")))
    (dolist (path (list (xwapp-test--at app "app/notes.txt")
                        (xwapp-test--at app "app/ui/../notes.txt")
                        (xwapp-test--at app "other/ui/x.js")
                        (xwapp-test--at app "app/ui/")
                        "/0123456789abcdef0123456789abcdef/app/ui/index.html"
                        "/app/ui/index.html"))
      (should (equal (car (xwapp-test--do "GET" path)) 404)))
    ;; Another name rebound to this host, or another site's page.
    (should (equal (car (xwapp-test--do "GET" (xwapp-test--at app "app/ui/index.html") nil
                                        '(("Host" . "evil.example"))))
                   404))
    (should (equal (car (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p1") "{}"
                                        '(("Origin" . "http://evil.example"))))
                   404))
    ;; Before any page: an intent naming none is not one.
    (should (equal (car (xwapp-test--do "POST" (xwapp-test--at app "intent") "{\"op\":\"x\"}")) 410))
    (xwapp-test--turn)
    (should-not intents)
    (should-not (xwapp-live-p app))))

(ert-deftest xwapp-test-browser-round-trip ()
  "hello makes the page the app's.  Its intents reach the handler in
order, read as the title's are; the app's calls wait for its next, and
go together."
  (xwapp-test--with-browser
    (should (equal (car (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p1") "{}")) 204))
    (should (xwapp-live-p app))
    (let ((next (xwapp-test--ask "GET" (xwapp-test--at app "next?page=p1")))
          (said "{\"op\":\"say\",\"text\":\"你好\",\"n\":[1,2],\"f\":false}"))
      (should (equal (car (xwapp-test--do "POST" (xwapp-test--at app "intent?page=p1") said)) 204))
      (should (equal (car (xwapp-test--do "POST" (xwapp-test--at app "intent?page=p1")
                                          "{\"op\":\"refresh\"}"))
                     204))
      (xwapp-test--turn)
      (should (equal (reverse intents)
                     (list (xwapp-parse-intent (concat "t:" said) '("t:"))
                           '((op . "refresh")))))
      (should (equal (alist-get 'text (car (last intents))) "你好"))
      (should-not (car next))
      (xwapp-js app "render" '(:title "標題" :items [1 2]))
      (xwapp-js app "flash" "done")
      (should (equal (xwapp-test--answer next)
                     '(200 . "[{\"ns\":\"T\",\"fn\":\"render\",\"arg\":{\"title\":\"標題\",\"items\":[1,2]}},{\"ns\":\"T\",\"fn\":\"flash\",\"arg\":\"done\"}]"))))))

(ert-deftest xwapp-test-browser-long-intent-arrives-whole ()
  "A long prompt from a browser tab is taken whole, however the socket
cuts it: the page's longest (2M UTF-16 units of JSON) is 6 MB at most,
under `xwapp--max-request'; past that, 413, which the page drops.
The text is made up."
  (xwapp-test--with-browser
    (should (equal (car (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p1") "{}")) 204))
    (let* ((text (apply #'concat (make-list 60000 "長い中文  \"q\" \\ 😀\n")))
           (said (json-encode `((op . "say") (text . ,text)))))
      (should (> (string-bytes said) (* 1024 1024)))
      (should (equal (car (xwapp-test--do "POST" (xwapp-test--at app "intent?page=p1") said)) 204))
      (xwapp-test--turn)
      (should (equal (alist-get 'text (car intents)) text)))
    (let ((xwapp--max-request 1000))
      (should (equal (car (xwapp-test--do "POST" (xwapp-test--at app "intent?page=p1")
                                          (json-encode `((op . "say") (text . ,(make-string 2000 ?x))))))
                     413)))))

(ert-deftest xwapp-test-browser-idle-next-is-answered-empty ()
  (xwapp-test--with-browser
    (let ((xwapp--hold 0.1))
      (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p1") "{}")
      (should (equal (xwapp-test--do "GET" (xwapp-test--at app "next?page=p1"))
                     '(200 . "[]"))))))

(ert-deftest xwapp-test-browser-one-page-at-a-time ()
  "The page that said hello last is the app's: the one before is told,
and its intents are refused.  Moving is not closing."
  (xwapp-test--with-browser
    (let ((xwapp--gone-after 0.3))
      (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p1") "{}")
      (let ((old (xwapp-test--ask "GET" (xwapp-test--at app "next?page=p1"))))
        (xwapp-test--turn)
        (should (equal (car (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p2") "{}")) 204))
        (should (equal (car (xwapp-test--answer old)) 410)))
      (let ((next (xwapp-test--ask "GET" (xwapp-test--at app "next?page=p2"))))
        (dolist (path '("intent?page=p1" "intent?page=" "intent"))
          (should (equal (car (xwapp-test--do "POST" (xwapp-test--at app path) "{\"op\":\"x\"}")) 410)))
        (should (equal (car (xwapp-test--do "GET" (xwapp-test--at app "next?page=p1"))) 410))
        ;; Long enough for a stray timer from the page let go to fire.
        (xwapp-test--turn 0.5)
        (should-not (car next)))
      (should-not intents)
      (should (= closed 0))
      (should (xwapp-live-p app)))))

(ert-deftest xwapp-test-browser-let-go-is-not-closed ()
  "A tab let go -- as `xwapp-open' does, the page moving into Emacs -- is
told so, and the app is not closed: not then, nor when the tab's
request is gone."
  (xwapp-test--with-browser
    (let ((xwapp--gone-after 0.2))
      (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p1") "{}")
      (let ((next (xwapp-test--ask "GET" (xwapp-test--at app "next?page=p1"))))
        (xwapp-test--turn)
        (xwapp--let-go app)
        (should (equal (car (xwapp-test--answer next)) 410)))
      (xwapp-test--turn 0.5)
      (should (= closed 0))
      (should-not (xwapp-live-p app)))))

(ert-deftest xwapp-test-browser-takes-the-page-from-emacs ()
  "A tab's hello kills the app's buffer in Emacs, and the app is not
closed: it has moved.  Killed with no tab holding the page, it is."
  (xwapp-test--with-browser
    (let ((make (lambda ()
                  (with-current-buffer (get-buffer-create " *xwapp-test*")
                    (add-hook 'kill-buffer-hook (lambda () (xwapp--on-kill app)) nil t)))))
      (funcall make)
      (kill-buffer " *xwapp-test*")
      (should (= closed 1))
      (funcall make)
      (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p1") "{}")
      (should-not (get-buffer " *xwapp-test*"))
      (should (= closed 1))
      (should (xwapp-live-p app)))))

(ert-deftest xwapp-test-browser-a-closed-tab-closes-the-app ()
  "A tab that hangs up and does not come back is closed; one that comes
back -- a reload -- is not."
  (xwapp-test--with-browser
    (let ((xwapp--gone-after 0.3))
      (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p1") "{}")
      (let ((next (xwapp-test--ask "GET" (xwapp-test--at app "next?page=p1"))))
        (xwapp-test--turn)
        (delete-process (cdr next)))
      (xwapp-test--turn)
      ;; The reload: hello, then at once the next a page always has.
      (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p2") "{}")
      (let ((next (xwapp-test--ask "GET" (xwapp-test--at app "next?page=p2"))))
        (xwapp-test--turn 0.5)
        (should (= closed 0))
        (should (xwapp-live-p app))
        (delete-process (cdr next)))
      (xwapp-test--turn 0.5)
      (should (= closed 1))
      (should-not (xwapp-live-p app)))))

(ert-deftest xwapp-test-browser-says-whether-you-look ()
  "What the tab says of itself is xwapp's, not the app's."
  (xwapp-test--with-browser
    (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p1") "{}")
    (should-not (xwapp-seen-p app))
    (xwapp-test--do "POST" (xwapp-test--at app "intent?page=p1") "{\"op\":\"xwapp-seen\",\"seen\":true}")
    (should (xwapp-seen-p app))
    (xwapp-test--do "POST" (xwapp-test--at app "intent?page=p1") "{\"op\":\"xwapp-seen\",\"seen\":false}")
    (should-not (xwapp-seen-p app))
    (xwapp-test--turn)
    (should-not intents)))

(ert-deftest xwapp-test-browser-reload ()
  (xwapp-test--with-browser
    (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p1") "{}")
    (let ((next (xwapp-test--ask "GET" (xwapp-test--at app "next?page=p1"))))
      (xwapp-test--turn)
      (xwapp-reload app)
      (should (equal (xwapp-test--answer next) '(200 . "[{\"xwapp\":\"reload\"}]"))))))

(defun xwapp-test--why (answer)
  "The why of a 410 ANSWER."
  (and (equal (car answer) 410)
       (alist-get 'why (json-parse-string (cdr answer) :object-type 'alist))))

(defmacro xwapp-test--reopens (app)
  "Give APP a reopen that counts in `reopened'."
  `(xwapp--put ,app :reopen (lambda () (cl-incf reopened))))

(ert-deftest xwapp-test-browser-says-why ()
  "A page that is not the app's is told why: taken while another page
is, then free if the app can open again, else closed."
  (xwapp-test--with-browser
    (let ((xwapp--gone-after 0.2))
      (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p1") "{}")
      (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p2") "{}")
      (should (equal (xwapp-test--why (xwapp-test--do "GET" (xwapp-test--at app "next?page=p1")))
                     "taken"))
      (should (equal (xwapp-test--why (xwapp-test--do "POST" (xwapp-test--at app "intent?page=p1") "{}"))
                     "taken"))
      ;; p2 never asks: let go, and closed.
      (xwapp-test--turn 0.4)
      (should (= closed 1))
      (should (equal (xwapp-test--why (xwapp-test--do "GET" (xwapp-test--at app "next?page=p2")))
                     "closed"))
      (xwapp-test--reopens app)
      (should (equal (xwapp-test--why (xwapp-test--do "GET" (xwapp-test--at app "next?page=p2")))
                     "free"))
      (should (= reopened 0)))))

(ert-deftest xwapp-test-browser-a-page-comes-back-softly ()
  "A page coming back on its own takes the app only when it is free,
and the app is opened again for it.  A fresh load takes it anyway."
  (xwapp-test--with-browser
    (should (equal (xwapp-test--why (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p1&soft=1") "{}"))
                   "closed"))
    (xwapp-test--reopens app)
    (should (equal (car (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p1&soft=1") "{}")) 204))
    (xwapp-test--turn)
    (should (= reopened 1))
    (should (equal (xwapp-test--why (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p2&soft=1") "{}"))
                   "taken"))
    (should (equal (car (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p2") "{}")) 204))
    (xwapp-test--turn)
    (should (= reopened 1))
    (should (xwapp--current-p app "p2"))))

(ert-deftest xwapp-test-browser-what-emacs-opened-is-not-reopened ()
  "The tab `xwapp-browse' opened finds the app ready: its command did
that.  A reload of a tab whose app is free, not opened by Emacs, finds
it opened again."
  (xwapp-test--with-browser
    (let ((urls nil)
          (xwapp--gone-after 0.2))
      (xwapp-test--reopens app)
      (let ((xwapp-browse-function (lambda (url) (push url urls))))
        (with-temp-buffer
          (xwapp-browse app)))
      (should (equal urls (list (xwapp-url app))))
      (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p1") "{}")
      (xwapp-test--turn)
      (should (= reopened 0))
      (xwapp-test--turn 0.4)
      (should (= closed 1))
      (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p2") "{}")
      (xwapp-test--turn)
      (should (= reopened 1))
      ;; A tab Emacs opened that never loads: the app, closed meanwhile,
      ;; is opened again for the next tab that comes.
      (let ((xwapp-browse-function #'ignore))
        (xwapp-browse app))
      (xwapp-test--turn 0.3)
      (should (= closed 1))
      (xwapp-test--turn 0.5)
      (should (= closed 2))
      (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p3&soft=1") "{}")
      (xwapp-test--turn)
      (should (= reopened 2)))))

(ert-deftest xwapp-test-browser-opens-where-it-was-asked ()
  "A page opened by an intent from a browser tab opens in the browser,
whatever `xwapp-browser' says."
  (xwapp-test--with-browser
    (let* ((urls nil)
           (xwapp-browser nil)
           (xwapp-browse-function (lambda (url) (push url urls)))
           (other (xwapp-test--app :buffer-name " *xwapp-other*"
                                   :index (expand-file-name "other/ui/x.js" root))))
      (setf (xwapp-handler app) (lambda (_intent) (xwapp-open other)))
      (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p1") "{}")
      ;; Opened in Emacs instead, it fails here, not in a batch WebKit.
      (when (featurep 'xwidget-internal) (require 'xwidget))
      (cl-letf (((symbol-function 'xwidget-webkit--create-new-session-buffer)
                 (lambda (&rest _) (error "Opened in Emacs"))))
        (xwapp-test--do "POST" (xwapp-test--at app "intent?page=p1") "{\"op\":\"open\"}")
        (xwapp-test--turn))
      (should (equal urls (list (xwapp-url other))))
      (should-not xwapp--from-browser))))

(defmacro xwapp-test--keeping (&rest body)
  "Run BODY in `xwapp-test--with-browser', addresses kept."
  (declare (indent 0))
  `(xwapp-test--with-browser
     (let ((xwapp--keep t))
       ,@body)))

(defun xwapp-test--free-port ()
  "A port nothing listens on."
  (let* ((p (xwapp--listen t))
         (port (process-contact p :service)))
    (delete-process p)
    port))

(defun xwapp-test--keep-reopen () "A reopen with a name." nil)

(ert-deftest xwapp-test-browser-keeps-the-address-of-a-tab ()
  "While a page of an app that can open again is in a tab, its address
is kept, readable by you alone; when it leaves, it is not.  An app that
cannot open again keeps nothing."
  (xwapp-test--keeping
    (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p1") "{}")
    (should-not (file-exists-p xwapp-state-file))
    (xwapp--put app :reopen #'xwapp-test--keep-reopen)
    (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p2") "{}")
    (should (= (file-modes xwapp-state-file) #o600))
    (should (equal (xwapp--state)
                   (list :port (process-contact xwapp--server :service)
                         :pages (list (list " *xwapp-test*" (xwapp--secret app)
                                            'xwapp-test--keep-reopen)))))
    (xwapp--gone app)
    (should-not (file-exists-p xwapp-state-file))))

(ert-deftest xwapp-test-browser-leaves-another-emacss-address ()
  (xwapp-test--keeping
    (with-temp-file xwapp-state-file (insert "(:port 1 :pages ())"))
    (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p1") "{}")
    (xwapp--gone app)
    (should (file-exists-p xwapp-state-file))))

(ert-deftest xwapp-test-browser-a-tab-comes-back-after-a-restart ()
  "After a restart, an app made with a kept address listens there under
its old secret: the tab's next finds it free, and its soft hello takes
it.  The address is kept until the tab is gone again."
  (xwapp-test--keeping
    (let ((port (xwapp-test--free-port))
          (secret "0123456789abcdef0123456789abcdef"))
      (delete-process xwapp--server)
      (with-temp-file xwapp-state-file
        (prin1 (list :port port :pages (list (list " *xwapp-again*" secret 'xwapp-test--keep-reopen)))
               (current-buffer)))
      (let ((again (xwapp-create :buffer-name " *xwapp-again*"
                                 :index (expand-file-name "app/ui/index.html" root)
                                 :prefixes '("t:") :namespace "T"
                                 :handler #'ignore
                                 :reopen (lambda () (cl-incf reopened)))))
        (should (equal (process-contact xwapp--server :service) port))
        (should (equal (xwapp--secret again) secret))
        (should (string-prefix-p (format "http://127.0.0.1:%d/%s/" port secret) (xwapp-url again)))
        (should (equal (xwapp-test--why (xwapp-test--do "GET" (xwapp-test--at again "next?page=old")))
                       "free"))
        (should (equal (car (xwapp-test--do "POST" (xwapp-test--at again "hello?page=p1&soft=1") "{}"))
                       204))
        (xwapp-test--turn)
        (should (= reopened 1))
        (should (equal (caar (plist-get (xwapp--state) :pages)) " *xwapp-again*"))
        (xwapp--gone again)
        (should-not (file-exists-p xwapp-state-file))))))

(ert-deftest xwapp-test-browser-a-tab-that-does-not-come-back ()
  "A kept address whose tab does not come back is not kept; one whose
port another Emacs has is not taken at all."
  (xwapp-test--keeping
    (let* ((port (xwapp-test--free-port))
           (secret "0123456789abcdef0123456789abcdef")
           (keep (lambda ()
                   (with-temp-file xwapp-state-file
                     (prin1 (list :port port :pages (list (list " *xwapp-again*" secret nil)))
                            (current-buffer)))))
           (make (lambda ()
                   (xwapp-create :buffer-name " *xwapp-again*" :index "/nonexistent"
                                 :prefixes '("t:") :namespace "T" :handler #'ignore
                                 :reopen #'ignore))))
      (delete-process xwapp--server)
      (funcall keep)
      (let ((xwapp--come-back-within 0.2))
        (funcall make))
      (xwapp-test--turn 0.4)
      (should-not (file-exists-p xwapp-state-file))
      ;; Another Emacs listens on the port.
      (delete-process xwapp--server)
      (clrhash xwapp--served)
      (setq xwapp--state-ours nil)
      (funcall keep)
      (let ((theirs (xwapp--listen port)))
        (unwind-protect
            (let ((again (funcall make)))
              (should-not (equal (xwapp--secret again) secret))
              (should (file-exists-p xwapp-state-file)))
          (delete-process theirs))))))

(defconst xwapp-test--box "box.tail0.ts.net")

(defun xwapp-test--from-box (&optional login origin host)
  "Headers of a request `tailscale serve' forwards from the box to the
socket: the Host rewritten to \"localhost\" as serve does for a unix
target (HOST overrides, to show it is not consulted), the device's LOGIN
stamped in when non-nil, the page's ORIGIN added when the browser sends
one (serve leaves Origin alone, so it stays the tailnet host)."
  (append (list (cons "Host" (or host "localhost")))
          (when login (list (cons "Tailscale-User-Login" login)))
          (when origin (list (cons "Origin" origin)))))

(ert-deftest xwapp-test-browser-remote-hosts-for-their-users-only ()
  "Through the proxy -- `tailscale serve' rewriting the Host to
\"localhost\", stamping the login -- a page is served to one of
`xwapp-remote-users' alone: not without the login, not for another's.
The Host serve sends is not consulted -- the transport is the proof.
The page's own calls carry an Origin, which must name one of
`xwapp-remote-hosts' (its case need not match); a cross-site page's does
not, and gets nothing.  With no users, or no names, to none."
  (xwapp-test--with-browser
    (let ((xwapp-remote-hosts (list "Box.Tail0.ts.net"))  ; its case need not match the browser's
          (xwapp-remote-users (list "me@example.com"))
          (index (xwapp-test--at app "app/ui/index.html"))
          (https (concat "https://" xwapp-test--box))
          (http (concat "http://" xwapp-test--box)))
      (xwapp-test--proxy-up)
      ;; serve's request: Host localhost, login stamped, a navigation (no
      ;; Origin).  Served.
      (should (equal (xwapp-test--do "GET" index nil (xwapp-test--from-box "me@example.com") 'proxy)
                     '(200 . "<app/ui/index.html>")))
      ;; The Host serve sends is never consulted: even an odd one is served.
      (should (equal (car (xwapp-test--do "GET" index nil
                                          (xwapp-test--from-box "me@example.com" nil "whatever:8443")
                                          'proxy))
                     200))
      ;; No login, or another's: nothing.
      (dolist (h (list (xwapp-test--from-box nil)
                       (xwapp-test--from-box "someone@else.com")))
        (should (equal (car (xwapp-test--do "GET" index nil h 'proxy)) 404)))
      ;; The very same serve request on loopback -- a program dressing up
      ;; as the proxy -- is not answered: the transport is the proof.
      (should (equal (car (xwapp-test--do "GET" index nil (xwapp-test--from-box "me@example.com"))) 404))
      ;; The page there, its own origin (either scheme of a remote host,
      ;; case aside): hello, intents and calls.
      (dolist (o (list https http))
        (should (equal (car (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p1") "{}"
                                            (xwapp-test--from-box "me@example.com" o) 'proxy))
                       204)))
      (should (xwapp-live-p app))
      (should (equal (car (xwapp-test--do "POST" (xwapp-test--at app "intent?page=p1") "{\"op\":\"go\"}"
                                          (xwapp-test--from-box "me@example.com" https) 'proxy))
                     204))
      (xwapp-test--turn)
      (should (equal intents '(((op . "go")))))
      ;; A cross-site page on the device, or a loopback origin: turned away.
      (dolist (o (list "https://evil.example" "http://127.0.0.1:1"))
        (should (equal (car (xwapp-test--do "POST" (xwapp-test--at app "intent?page=p1") "{\"op\":\"no\"}"
                                            (xwapp-test--from-box "me@example.com" o) 'proxy))
                       404)))
      ;; Refused in words: an empty answer, a browser would download.
      (should (equal (xwapp-test--do "GET" index nil (xwapp-test--from-box nil) 'proxy) '(404 . "Not Found")))
      (let ((xwapp-remote-users nil))
        (should (equal (car (xwapp-test--do "GET" index nil (xwapp-test--from-box "me@example.com") 'proxy)) 404)))
      (let ((xwapp-remote-hosts nil))
        (should (equal (car (xwapp-test--do "GET" index nil (xwapp-test--from-box "me@example.com") 'proxy)) 404)))
      ;; This machine as before.
      (should (equal (car (xwapp-test--do "GET" index)) 200)))))

(ert-deftest xwapp-test-browser-a-fixed-port ()
  "`xwapp-port' is where pages are served, or it says why it cannot be."
  (xwapp-test--with-browser
    (let ((port (xwapp-test--free-port)))
      (delete-process xwapp--server)
      (let ((xwapp-port port))
        (should (string-prefix-p (format "http://127.0.0.1:%d/" port) (xwapp-url app)))
        (delete-process xwapp--server)
        (let ((theirs (xwapp--listen port)))
          (unwind-protect
              (should (string-match-p (format "cannot listen on port %d" port)
                                      (cadr (should-error (xwapp--port) :type 'user-error))))
            (delete-process theirs)))))))

(ert-deftest xwapp-test-browser-a-remote-address-outlives-emacs ()
  "Given out for another device, an address is kept with no tab open,
and over a restart for good -- not only until a tab comes back -- until
its secret is renewed.  Only a page that can open again has one."
  (xwapp-test--keeping
    (let* ((xwapp-remote-hosts (list xwapp-test--box))
           (xwapp-remote-users (list "me@example.com"))
           (port (xwapp-test--free-port))
           (xwapp-port port)
           (made (lambda ()
                   (xwapp-create :buffer-name " *xwapp-test*"
                                 :index (expand-file-name "app/ui/index.html" root)
                                 :prefixes '("t:") :namespace "T" :handler #'ignore
                                 :reopen #'xwapp-test--keep-reopen))))
      (should-error (xwapp-remote-url app) :type 'user-error)
      (delete-process xwapp--server)
      (clrhash xwapp--served)
      (let* ((app (funcall made))
             (url (xwapp-remote-url app))
             (secret (xwapp--secret app)))
        (should (equal url (format "https://%s/%s/app/ui/index.html" xwapp-test--box secret)))
        (let ((xwapp-remote-scheme "http"))
          (should (string-prefix-p "http://box" (xwapp-remote-url app))))
        (should (equal (plist-get (xwapp--state) :pages)
                       (list (list " *xwapp-test*" secret 'xwapp-test--keep-reopen))))
        ;; Restarted, the page nowhere: the same address, kept on.
        (delete-process xwapp--server)
        (clrhash xwapp--served)
        (setq xwapp--state-ours nil)
        (let* ((xwapp--come-back-within 0.1)
               (again (funcall made)))
          (should (equal (xwapp--secret again) secret))
          (xwapp-test--turn 0.3)
          (xwapp-test--proxy-up)
          (should (equal (cadar (plist-get (xwapp--state) :pages)) secret))
          (should (equal (car (xwapp-test--do "GET" (concat "/" secret "/app/ui/index.html") nil
                                              (xwapp-test--from-box "me@example.com") 'proxy))
                         200))
          ;; Renewed: the old address is no one's.
          (cl-letf (((symbol-function 'message) #'ignore))
            (xwapp-renew-secret again))
          (should-not (equal (xwapp--secret again) secret))
          (should (equal (cadar (plist-get (xwapp--state) :pages)) (xwapp--secret again)))
          (should (equal (car (xwapp-test--do "GET" (concat "/" secret "/app/ui/index.html") nil
                                              (xwapp-test--from-box "me@example.com") 'proxy))
                         404))
          ;; Copied, not shown.
          (let (copied said)
            (cl-letf (((symbol-function 'xwapp-copy) (lambda (s) (setq copied s)))
                      ((symbol-function 'message) (lambda (f &rest args) (push (apply #'format f args) said))))
              (xwapp-copy-remote-url again))
            (should (equal copied (xwapp-remote-url again)))
            (should (string-match-p "Copied the address of \\*xwapp-test\\*" (car said)))
            (should-not (string-match-p (xwapp--secret again) (car said)))))))))

(ert-deftest xwapp-test-browser-an-address-without-the-secret ()
  "`xwapp-remote-open-url' holds no secret: asked from your other
devices, by your login, it sends the browser on to the page under its
secret, the fragment left to the browser to keep.  From this machine,
another login, another package or a page that cannot open again, 404."
  (xwapp-test--keeping
    (let* ((xwapp-remote-hosts (list xwapp-test--box))
           (xwapp-remote-users (list "me@example.com"))
           (url nil))
      (should-error (xwapp-remote-open-url app) :type 'user-error)
      (xwapp--put app :reopen #'xwapp-test--keep-reopen)
      (setq url (xwapp-remote-open-url app "room/chat/%7Ew%2Fx%2F"))
      (should (equal url (format "https://%s/open/app#room/chat/%%7Ew%%2Fx%%2F" xwapp-test--box)))
      (should-not (string-match-p (xwapp--secret app) url))
      (should (equal (cadar (plist-get (xwapp--state) :pages)) (xwapp--secret app)))
      (xwapp-test--proxy-up)
      (let ((raw (lambda (headers transport)
                   (let* ((out "")
                          (proc (if (eq transport 'proxy)
                                    (make-network-process
                                     :name "xwapp-test-raw" :family 'local
                                     :service (expand-file-name xwapp-remote-socket)
                                     :coding 'binary :noquery t
                                     :filter (lambda (_p s) (setq out (concat out s))))
                                  (make-network-process
                                   :name "xwapp-test-raw" :host "127.0.0.1"
                                   :service (process-contact xwapp--server :service)
                                   :coding 'binary :noquery t
                                   :filter (lambda (_p s) (setq out (concat out s)))))))
                     (process-send-string
                      proc (concat "GET /open/app HTTP/1.1\r\n"
                                   (mapconcat (lambda (h) (format "%s: %s\r\n" (car h) (cdr h))) headers "")
                                   "\r\n"))
                     (with-timeout (5 (error "No answer"))
                       (while (process-live-p proc) (accept-process-output nil 0.02)))
                     out))))
        ;; Through the proxy: on to the page, the secret in the Location.
        (let ((out (funcall raw (xwapp-test--from-box "me@example.com") 'proxy)))
          (should (string-prefix-p "HTTP/1.1 302 Found\r\n" out))
          (should (string-match-p (format "\r\nLocation: /%s/app/ui/index\\.html\r\n" (xwapp--secret app)) out)))
        ;; The same request on loopback, dressed as the proxy, gets no
        ;; secret -- the whole of the hole seen 2026-10-13.
        (should (string-prefix-p "HTTP/1.1 404" (funcall raw (xwapp-test--from-box "me@example.com") nil)))
        (dolist (h (list `(("Host" . ,(format "127.0.0.1:%d" (process-contact xwapp--server :service))))
                         (xwapp-test--from-box "someone@else.com")
                         (xwapp-test--from-box nil)))
          (should (string-prefix-p "HTTP/1.1 404" (funcall raw h 'proxy)))))
      (dolist (path '("/open/other" "/open/ap" "/open/app/ui" "/open"))
        (should (equal (car (xwapp-test--do "GET" path nil (xwapp-test--from-box "me@example.com") 'proxy)) 404)))
      (should (equal (car (xwapp-test--do "POST" "/open/app" "{}" (xwapp-test--from-box "me@example.com") 'proxy)) 404))
      ;; A page given no address yet gets one there, kept as any is.
      (xwapp--put app :secret nil)
      (delete-file xwapp-state-file)
      (should (equal (car (xwapp-test--do "GET" "/open/app" nil (xwapp-test--from-box "me@example.com") 'proxy)) 302))
      (should (equal (cadar (plist-get (xwapp--state) :pages)) (xwapp--secret app)))
      (xwapp--put app :reopen nil)
      (should (equal (car (xwapp-test--do "GET" "/open/app" nil (xwapp-test--from-box "me@example.com") 'proxy)) 404)))))

(defun xwapp-test--json (s)
  (json-parse-string s :object-type 'alist :array-type 'list))

(ert-deftest xwapp-test-phone-in-the-xwidget ()
  "The page in Emacs says hello, and is told it can show its phone
button only when your other devices can reach it.  Asked, it gets its
address there to show as a QR code, or why there is none.  Neither
intent is the app's."
  (xwapp-test--with-browser
    (let* ((title nil) (scripts nil)
           (poll (lambda (intent)
                   (setq title (concat "t:" intent) scripts nil)
                   (xwapp--poll app)
                   (seq-keep (lambda (s) (and (string-match "\\`window\\.XW && XW\\.kit(\\(.*\\));\\'" s)
                                              (xwapp-test--json (match-string 1 s))))
                             scripts))))
      (cl-letf (((symbol-function 'xwapp-session) (lambda (_) 'xw))
                ((symbol-function 'xwidget-webkit-title) (lambda (_) title))
                ((symbol-function 'xwidget-webkit-execute-script)
                 (lambda (_xw s) (push s scripts))))
        (xwapp--put app :reopen #'xwapp-test--keep-reopen)
        (should-not (funcall poll "{\"op\":\"xwapp-hello\",\"n\":1}"))
        (let ((said (funcall poll "{\"op\":\"xwapp-qr\",\"n\":2}")))
          (should (equal (mapcar #'car (car said)) '(xwapp error)))
          (should (string-match-p "xwapp-remote-hosts.*this machine alone" (alist-get 'error (car said)))))
        (let ((xwapp-remote-hosts (list xwapp-test--box)))
          (should (equal (funcall poll "{\"op\":\"xwapp-hello\",\"n\":3}")
                         '(((xwapp . "phone")))))
          (should (equal (funcall poll "{\"op\":\"xwapp-qr\",\"n\":4}")
                         `(((xwapp . "qr") (url . ,(xwapp-remote-url app)) (host . ,xwapp-test--box)))))
          ;; A page that cannot open again cannot be reached from elsewhere.
          (xwapp--put app :reopen nil)
          (should-not (funcall poll "{\"op\":\"xwapp-hello\",\"n\":5}"))
          (should (string-match-p "cannot be reached from elsewhere"
                                  (alist-get 'error (car (funcall poll "{\"op\":\"xwapp-qr\",\"n\":6}"))))))
        (should-not intents)))))

(ert-deftest xwapp-test-paste-in-the-xwidget ()
  "⌘V in a field of the page, in the xwidget: the kit asks, and Emacs
answers with the clipboard's text -- \"\" when it holds none -- scrubbed
as any script is.  The intent is the kit's, never the app's.  A page in a
browser has a paste of its own: its ask is not answered, and Emacs's
clipboard is not read.  The text is made up."
  (xwapp-test--with-browser
    (let* ((title nil) (scripts nil) (read 0) (clip nil)
           (poll (lambda (intent)
                   (setq title (concat "t:" intent) scripts nil)
                   (xwapp--poll app)
                   (seq-keep (lambda (s) (and (string-match "\\`window\\.XW && XW\\.kit(\\(.*\\));\\'" s)
                                              (xwapp-test--json (match-string 1 s))))
                             scripts))))
      (cl-letf (((symbol-function 'xwapp-session) (lambda (_) 'xw))
                ((symbol-function 'xwidget-webkit-title) (lambda (_) title))
                ((symbol-function 'xwidget-webkit-execute-script) (lambda (_xw s) (push s scripts)))
                ((symbol-function 'gui-get-selection)
                 (lambda (sel type) (should (equal (list sel type) '(CLIPBOARD STRING))) (cl-incf read) clip)))
        (setq clip (propertize "line one\n\"two\" 你好 😀" 'face 'bold))
        (should (equal (funcall poll "{\"op\":\"xwapp-paste\",\"n\":1}")
                       '(((xwapp . "paste") (text . "line one\n\"two\" 你好 😀")))))
        (setq clip nil)
        (should (equal (funcall poll "{\"op\":\"xwapp-paste\",\"n\":2}")
                       '(((xwapp . "paste") (text . "")))))
        ;; A clipboard Emacs cannot read is one with no text.
        (cl-letf (((symbol-function 'gui-get-selection) (lambda (&rest _) (error "No selection"))))
          (should (equal (funcall poll "{\"op\":\"xwapp-paste\",\"n\":3}")
                         '(((xwapp . "paste") (text . ""))))))
        ;; A raw byte goes as U+FFFD, as in any script.
        (setq clip (concat "a" (string (unibyte-char-to-multibyte #xff)) "b"))
        (should (equal (funcall poll "{\"op\":\"xwapp-paste\",\"n\":4}")
                       '(((xwapp . "paste") (text . "a�b")))))
        (should (= read 3))
        (should-not intents)
        ;; In a browser.
        (xwapp--put app :page "p1")
        (xwapp--dispatch app '((op . "xwapp-paste")))
        (should (= read 3))
        (should-not (xwapp--get app :outbox))
        (should-not intents)))))

(ert-deftest xwapp-test-browser-phone ()
  "In a browser, the answer to the page's hello is the first call it
gets; its ask for the QR code is answered with the next."
  (xwapp-test--with-browser
    (let ((next (lambda (page)
                  (xwapp-test--json (cdr (xwapp-test--do "GET" (xwapp-test--at app (concat "next?page=" page))))))))
      (xwapp--put app :reopen #'xwapp-test--keep-reopen)
      ;; Not reachable from elsewhere: nothing said.
      (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p1") "{}")
      (should-not (xwapp--get app :outbox))
      (let ((xwapp-remote-hosts (list xwapp-test--box)))
        (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p2") "{}")
        (should (equal (funcall next "p2") '(((xwapp . "phone")))))
        (should (equal (car (xwapp-test--do "POST" (xwapp-test--at app "intent?page=p2")
                                            "{\"op\":\"xwapp-qr\"}"))
                       204))
        (should (equal (funcall next "p2")
                       `(((xwapp . "qr") (url . ,(xwapp-remote-url app)) (host . ,xwapp-test--box)))))
        (should-not intents)))))

(ert-deftest xwapp-test-browser-resume-loads-the-apps ()
  "Once Emacs is up, each kept page's app is loaded through its reopen."
  (xwapp-test--keeping
    (let ((file (expand-file-name "xwapp-test-resumed.el" root)))
      (with-temp-file file
        (insert "(defvar xwapp-test--resumed-here t)\n"
                "(defun xwapp-test--resumed-reopen () nil)\n"))
      (autoload 'xwapp-test--resumed-reopen file)
      (with-temp-file xwapp-state-file
        (prin1 (list :port 1 :pages (list (list "x" "0123456789abcdef0123456789abcdef"
                                                'xwapp-test--resumed-reopen)))
               (current-buffer)))
      (unwind-protect
          (progn
            (xwapp-resume)
            (should (boundp 'xwapp-test--resumed-here))
            (should-not (autoloadp (symbol-function 'xwapp-test--resumed-reopen))))
        (makunbound 'xwapp-test--resumed-here)
        (fmakunbound 'xwapp-test--resumed-reopen)))))

(ert-deftest xwapp-test-listens-from-the-start-for-other-devices ()
  "With remote hosts, Emacs listens once it is up -- a proxy cannot ask it
to begin -- though no page was kept and none is opened here; without
them it waits to be asked."
  (xwapp-test--with-browser
    (delete-process xwapp--server)
    (setq xwapp--server nil)
    (let ((startup (seq-filter (lambda (f) (string-match-p "xwapp-resume" (format "%S" f)))
                               emacs-startup-hook)))
      (should (= (length startup) 1))
      (should-not (file-exists-p xwapp-state-file))
      (let ((xwapp-remote-hosts nil))
        (funcall (car startup))
        (should-not (process-live-p xwapp--server)))
      (let ((xwapp-remote-hosts (list xwapp-test--box)))
        ;; A loose umask: the perms that stand are the ones xwapp pins.
        (with-file-modes #o777 (funcall (car startup)))
        (should (process-live-p xwapp--server))
        ;; and the proxy's socket too, made yours alone.
        (should (process-live-p xwapp--unix-server))
        (let ((sock (expand-file-name xwapp-remote-socket)))
          (should (eq (car (file-attributes sock)) nil))  ; a socket, not a dir
          (should (= (file-modes sock) #o600))
          (should (= (file-modes (file-name-directory sock)) #o700)))))))

(ert-deftest xwapp-test-browser-the-proxy-socket-is-the-proof ()
  "The transport, not a header, says a request came through the proxy: a
page and an `/open/' secret are answered on the socket, and refused on
loopback however the headers are dressed (the hole seen 2026-10-13)."
  (xwapp-test--keeping
    (let* ((xwapp-remote-hosts (list xwapp-test--box))
           (xwapp-remote-users (list "me@example.com"))
           (index (xwapp-test--at app "app/ui/index.html"))
           (box (xwapp-test--from-box "me@example.com")))
      (xwapp--put app :reopen #'xwapp-test--keep-reopen)
      (xwapp-remote-url app)               ; give it a secret, kept
      (xwapp-test--proxy-up)
      ;; On the socket: served, and `/open/' hands the secret on.
      (should (equal (car (xwapp-test--do "GET" index nil box 'proxy)) 200))
      (should (equal (car (xwapp-test--do "GET" "/open/app" nil box 'proxy)) 302))
      ;; The identical bytes on loopback -- a sandbox or another program
      ;; that reaches 127.0.0.1 and dresses up as the proxy -- get nothing.
      (should (equal (car (xwapp-test--do "GET" index nil box)) 404))
      (should (equal (car (xwapp-test--do "GET" "/open/app" nil box)) 404))
      ;; Loopback cannot win by claiming to be loopback: `/open' needs
      ;; `elsewhere', which loopback never is; and the trusted transport
      ;; still asks for the login a header alone cannot supply.
      (let ((loop (list (cons "Host" (format "127.0.0.1:%d" (process-contact xwapp--server :service))))))
        (should (equal (car (xwapp-test--do "GET" "/open/app" nil loop)) 404))
        (should (equal (car (xwapp-test--do "GET" index nil loop 'proxy)) 404)))
      ;; The command that points the proxy here names the socket.
      (should (string-match-p (concat "unix:" (regexp-quote (expand-file-name xwapp-remote-socket)) "\\'")
                              (xwapp-remote-serve-command)))
      (let ((xwapp-remote-scheme "http"))
        (should (string-match-p "--http=80 " (xwapp-remote-serve-command)))))))

;;; Keeping a crashed WebKit from wedging Emacs

(ert-deftest xwapp-test-scrub-keeps-clean-and-fixes-invalid ()
  "A clean string is returned untouched; a raw byte / lone surrogate is
made U+FFFD so the push is valid UTF-8 for the WebKit IPC channel."
  (let ((clean "hello 世界 ✓"))
    (should (eq clean (xwapp--scrub clean))))        ; no copy on the common path
  ;; An astral char (> BMP but <= #x10FFFF) is valid and left untouched -- the
  ;; regexp detection must not mistake it for a raw byte.
  (let ((astral "𐍈 😀 𝄞"))
    (should (eq astral (xwapp--scrub astral))))
  (let ((fixed (xwapp--scrub (string-to-multibyte (concat "a" (unibyte-string 255) "b")))))
    (should (= (length fixed) 3))
    (should-not (seq-some (lambda (c) (or (> c #x10FFFF) (<= #xD800 c #xDFFF))) fixed)))
  ;; A lone surrogate is caught and replaced too.
  (let ((fixed (xwapp--scrub (concat "x" (string #xD83D) "y"))))
    (should (= (length fixed) 3))
    (should-not (seq-some (lambda (c) (or (> c #x10FFFF) (<= #xD800 c #xDFFF))) fixed))))

(ert-deftest xwapp-test-script-safe-caps-size ()
  "A script within the cap is sent (scrubbed); an oversized one is dropped."
  (let ((xwapp-max-script-bytes 100))
    (should (equal "ok" (xwapp--script-safe "ok")))
    (should-not (xwapp--script-safe (make-string 101 ?x)))))

(ert-deftest xwapp-test-js-drops-oversized ()
  "An oversized page update never reaches execute-script."
  (let ((app (xwapp-test--app)) (xwapp-max-script-bytes 50) called)
    (cl-letf (((symbol-function 'xwapp-session) (lambda (_) 'xw))
              ((symbol-function 'xwidget-webkit-execute-script)
               (lambda (&rest _) (setq called t))))
      (xwapp-js app "render" (list :big (make-string 200 ?x))))
    (should-not called)))

(ert-deftest xwapp-test-poll-breaker-stops-a-crashed-page ()
  "A title read that keeps erroring (a crashed WebKit process still looks
live) stops the poll and marks the app wedged after the limit."
  (let ((app (xwapp-test--app)) (xwapp-poll-fail-max 3) stopped)
    (cl-letf (((symbol-function 'xwapp-session) (lambda (_) 'xw))
              ((symbol-function 'xwidget-webkit-title) (lambda (_) (error "ipc gone")))
              ((symbol-function 'xwidget-webkit-execute-script) #'ignore)
              ((symbol-function 'xwapp-stop-poll) (lambda (_) (setq stopped t))))
      (xwapp--poll app) (xwapp--poll app)
      (should (= (xwapp--get app :poll-fails) 2))
      (should-not (xwapp--get app :wedged))
      (xwapp--poll app)                              ; third failure trips it
      (should (xwapp--get app :wedged))
      (should stopped))))

(ert-deftest xwapp-test-poll-breaker-resets-on-success ()
  "A good read clears the failure count, so transient blips never wedge it."
  (let ((app (xwapp-test--app)) (xwapp-poll-fail-max 3) (ok nil))
    (cl-letf (((symbol-function 'xwapp-session) (lambda (_) 'xw))
              ((symbol-function 'xwidget-webkit-title)
               (lambda (_) (if ok "Idle" (error "blip"))))
              ((symbol-function 'xwidget-webkit-execute-script) #'ignore))
      (xwapp--poll app) (xwapp--poll app)
      (should (= (xwapp--get app :poll-fails) 2))
      (setq ok t)
      (xwapp--poll app)                              ; a good read resets
      (should (= (or (xwapp--get app :poll-fails) 0) 0))
      (should-not (xwapp--get app :wedged)))))

;;; A long intent, in parts
;;
;; WebKit hands Emacs 1000 UTF-16 units of a title, so the page sends a
;; longer intent as parts, a title each (ui/xwapp.js).  The text in
;; these is made up.

(defmacro xwapp-test--with-page (&rest body)
  "Run BODY with `app' polling a stubbed page.  (show TITLE) makes TITLE
the page's and polls once; `seen' gathers what the handler gets,
`scripts' what is run in the page, `said' the echo area's messages.
The timers parts start are stopped after, so none fires in a later test."
  (declare (indent 0))
  `(let* ((seen nil) (scripts nil) (said nil) (title nil)
          (app (xwapp-test--app :handler (lambda (i) (push i seen)))))
     (cl-letf (((symbol-function 'xwapp-session) (lambda (_) 'xw))
               ((symbol-function 'xwidget-webkit-title) (lambda (_) title))
               ((symbol-function 'xwidget-webkit-execute-script) (lambda (_xw s) (push s scripts)))
               ((symbol-function 'message)
                (lambda (f &rest args) (push (apply #'format f args) said))))
       (cl-flet ((show (s) (setq title s) (xwapp--poll app)))
         (unwind-protect (progn ,@body)
           (xwapp--parts-clear app))))))

(defun xwapp-test--parts (n json count)
  "Titles carrying JSON as intent N's COUNT parts, as the page sends them."
  (let ((size (ceiling (length json) (float count))))
    (cl-loop for i below count
             collect (concat "t:" (json-encode
                                   `((xwpart . ,n) (i . ,i) (n . ,count)
                                     (s . ,(substring json (min (length json) (* i size))
                                                      (min (length json) (* (1+ i) size))))))))))

(defun xwapp-test--title-answer (what id whole)
  "The script Emacs answers a title with: WHAT, ID (N . I), WHOLE."
  (concat "if (!(window.XW && XW.kit && XW.kit({\"xwapp\":\"" what "\""
          (if id (format ",\"n\":%d,\"i\":%d" (car id) (cdr id)) "")
          (if whole ",\"whole\":true" "")
          ",\"idle\":\"Idle\"}))) document.title = \"Idle\";"))

(defun xwapp-test--told (script)
  "What SCRIPT, an answer, tells the kit, as an alist; nil for another
script.  The title is cleared only when the kit does not take it."
  (and (string-match "\\`if (!(window\\.XW && XW\\.kit && XW\\.kit(\\({.*}\\)))) document\\.title = \"Idle\";\\'"
                     script)
       (json-parse-string (match-string 1 script) :object-type 'alist)))

(ert-deftest xwapp-test-parts-reassemble ()
  "A long intent comes as parts, each read and reset; the whole of it is
handed on once, when the last is in."
  (xwapp-test--with-page
    (let* ((text (apply #'concat (make-list 400 "長い中文  \"q\" \\ 😀\n    ")))
           (json (json-encode `((op . "agent-prompt") (n . 7) (name . "a") (text . ,text))))
           (titles (xwapp-test--parts 7 json 4)))
      (dolist (part (butlast titles)) (show part))
      (should-not seen)
      (should (equal (reverse scripts)
                     (mapcar (lambda (i) (xwapp-test--title-answer "read" (cons 7 i) nil)) '(0 1 2))))
      (show (car (last titles)))
      (should (equal (car scripts) (xwapp-test--title-answer "read" '(7 . 3) t)))
      (should (= (length seen) 1))
      (should (equal (alist-get 'text (car seen)) text))
      (should (equal (car seen) (xwapp-parse-intent (concat "t:" json) '("t:"))))
      (should-not (xwapp--get app :parts))
      (should-not (xwapp--get app :part-wait))
      (should-not said))))

(ert-deftest xwapp-test-parts-out-of-order ()
  "Parts are placed by their number, not by when they come; one read
twice counts once."
  (xwapp-test--with-page
    (let* ((json "{\"op\":\"say\",\"n\":9,\"text\":\"one two three four\"}")
           (titles (xwapp-test--parts 9 json 3)))
      (show (nth 2 titles))
      (show (nth 0 titles))
      (show "Idle")
      (show (nth 0 titles))
      (should-not seen)
      (should (= (plist-get (cdr (assoc 9 (xwapp--get app :parts))) :got) 2))
      (show (nth 1 titles))
      (should (equal seen (list (xwapp--read-intent json))))
      (should-not said))))

(ert-deftest xwapp-test-parts-read-sooner-while-coming ()
  "While an intent's parts are coming, the title is read every
`xwapp--part-poll' seconds; once it is whole, by the poll alone."
  (xwapp-test--with-page
    (let ((titles (xwapp-test--parts 4 "{\"op\":\"say\",\"n\":4}" 2)))
      (show (nth 0 titles))
      (should (timerp (xwapp--get app :part-poll)))
      (should (timerp (xwapp--get app :part-wait)))
      (show (nth 1 titles))
      (should (= (length seen) 1))
      (should-not (xwapp--get app :part-poll))
      (should-not (xwapp--get app :part-wait)))))

(ert-deftest xwapp-test-parts-missing-one-is-dropped-after-a-wait ()
  "An intent whose parts stop coming is dropped once
`xwapp--part-wait' passes without one, and said to be -- never its
words.  What comes of it later is the start of nothing."
  (xwapp-test--with-page
    (let* ((xwapp--part-wait 0.1)
           (json "{\"op\":\"say\",\"n\":3,\"text\":\"made-up words here\"}")
           (titles (xwapp-test--parts 3 json 3)))
      (show (nth 0 titles))
      (show (nth 2 titles))
      (xwapp-test--turn 0.3)
      (should-not seen)
      (should-not (xwapp--get app :parts))
      (should-not (xwapp--get app :part-wait))
      (should-not (xwapp--get app :part-poll))
      (should (= (length said) 1))
      (should (string-match-p "page sent a long message in 3 parts, 1 of which never came; it was dropped"
                              (car said)))
      (should-not (string-search "made-up" (car said)))
      (show (nth 1 titles))
      (should-not seen))))

(ert-deftest xwapp-test-parts-that-do-not-fit-are-dropped ()
  "Parts that disagree on their count, or name one past it, or make no
intent strung together, are dropped and said to be."
  (xwapp-test--with-page
    (show "t:{\"xwpart\":5,\"i\":0,\"n\":3,\"s\":\"{\\\"op\\\"\"}")
    (show "t:{\"xwpart\":5,\"i\":1,\"n\":4,\"s\":\":1}\"}")
    (should-not (xwapp--get app :parts))
    (should (string-match-p "parts that do not fit together" (car said)))
    (show "t:{\"xwpart\":6,\"i\":3,\"n\":3,\"s\":\"x\"}")
    (should (= (length said) 2))
    (show "t:{\"xwpart\":7,\"i\":0,\"n\":2,\"s\":\"{\\\"op\"}")
    (show "t:{\"xwpart\":7,\"i\":1,\"n\":2,\"s\":\"\\\":\"}")
    (should (string-match-p "a long message Emacs could not read" (car said)))
    (should-not seen)
    (should-not (xwapp--get app :parts))))

(ert-deftest xwapp-test-short-intent-unchanged ()
  "An intent that fits a title is read as it always was: at once, one
reset, no parts, no sooner read."
  (xwapp-test--with-page
    (show "t:{\"op\":\"open\",\"id\":7,\"xw\":{\"xwpart\":1}}")
    (should (equal seen '(((op . "open") (id . 7) (xw (xwpart . 1))))))
    (should (equal scripts (list (xwapp-test--title-answer "read" nil t))))
    (should-not (xwapp--get app :parts))
    (should-not (xwapp--get app :part-poll))
    (should-not said)))

(ert-deftest xwapp-test-unreadable-title-is-said-once ()
  "A title cut short -- WebKit keeps the first 1000 UTF-16 units of
one -- is not dropped in silence: said once, never its words, and the
page told, so it goes on and gives back what was lost."
  (xwapp-test--with-page
    (let ((cut (concat "t:{\"op\":\"agent-prompt\",\"text\":\"" (make-string 970 ?字))))
      (show cut)
      (show cut)
      (should-not seen)
      (should (= (length said) 1))
      (should (string-match-p (format "page sent a %d-character message Emacs could not read; it was dropped"
                                      (length cut))
                              (car said)))
      (should-not (string-search "字" (car said)))
      (should (equal scripts (list (xwapp-test--title-answer "unread" nil nil))))
      ;; A title that is no intent says nothing.
      (show "Idle")
      (show "Some page")
      (should (= (length said) 1)))))

(ert-deftest xwapp-test-answer-names-the-title ()
  "Each title read is answered by its intent's n and its part's i -- a
title of its own being part 0 -- never by its words; whole once Emacs
has all of the intent.  One cut short is named from its start, which
WebKit keeps."
  (xwapp-test--with-page
    (show "t:{\"op\":\"say\",\"n\":1793368514001,\"text\":\"a\"}")
    (should (equal (xwapp-test--told (car scripts))
                   '((xwapp . "read") (n . 1793368514001) (i . 0) (whole . t) (idle . "Idle"))))
    (let ((titles (xwapp-test--parts 1793368514002 "{\"op\":\"say\",\"n\":1793368514002}" 2)))
      (show (nth 0 titles))
      (should (equal (xwapp-test--told (car scripts))
                     '((xwapp . "read") (n . 1793368514002) (i . 0) (idle . "Idle"))))
      (show (nth 1 titles))
      (should (equal (xwapp-test--told (car scripts))
                     '((xwapp . "read") (n . 1793368514002) (i . 1) (whole . t) (idle . "Idle")))))
    ;; Cut short, so no intent: named from its start, and lost.
    (show (concat "t:{\"op\":\"a \\\"q\\\" op\",\"n\":1793368514003,\"text\":\"" (make-string 990 ?字)))
    (should (equal (xwapp-test--told (car scripts))
                   '((xwapp . "unread") (n . 1793368514003) (i . 0) (idle . "Idle"))))
    (show (concat "t:{\"xwpart\":1793368514004,\"i\":5,\"n\":9,\"s\":\"" (make-string 990 ?字)))
    (should (equal (xwapp-test--told (car scripts))
                   '((xwapp . "unread") (n . 1793368514004) (i . 5) (idle . "Idle"))))
    ;; No n of its own: not the kit's, so not named.
    (show "t:{\"op\":\"say\",\"n\":\"one\"}")
    (should (equal (xwapp-test--told (car scripts))
                   '((xwapp . "read") (whole . t) (idle . "Idle"))))
    ;; Every answer leaves the title to the kit while it is there.
    (should (cl-every #'xwapp-test--told scripts))))

(ert-deftest xwapp-test-answer-a-dropped-part-is-lost ()
  "A part of an intent Emacs dropped is answered as lost, so the page
gives the intent up rather than send the rest of it for nothing."
  (xwapp-test--with-page
    (show "t:{\"xwpart\":5,\"i\":0,\"n\":3,\"s\":\"{\\\"op\\\"\"}")
    (should (equal (alist-get 'xwapp (xwapp-test--told (car scripts))) "read"))
    (show "t:{\"xwpart\":5,\"i\":1,\"n\":4,\"s\":\":1}\"}")
    (should (equal (xwapp-test--told (car scripts))
                   '((xwapp . "unread") (n . 5) (i . 1) (idle . "Idle"))))
    (show "t:{\"xwpart\":7,\"i\":0,\"n\":2,\"s\":\"{\\\"op\"}")
    (show "t:{\"xwpart\":7,\"i\":1,\"n\":2,\"s\":\"\\\":\"}")
    (should (equal (xwapp-test--told (car scripts))
                   '((xwapp . "unread") (n . 7) (i . 1) (idle . "Idle"))))
    (should-not seen)))

(ert-deftest xwapp-test-answer-late-does-not-clear-the-next ()
  "Emacs read A late, and the page has since set B.  Emacs does not
clear the title, which is B now, but leaves it to the kit, which clears
only A; B is read next, as itself."
  (xwapp-test--with-page
    (show "t:{\"op\":\"a\",\"n\":11}")
    ;; The page gave A up and set B before A's answer landed.
    (show "t:{\"op\":\"b\",\"n\":12}")
    (should (equal (mapcar (lambda (i) (alist-get 'op i)) seen) '("b" "a")))
    (should (equal (mapcar (lambda (s) (alist-get 'n (xwapp-test--told s))) scripts) '(12 11)))
    (should-not (seq-some (lambda (s) (string-prefix-p "document.title" s)) scripts))))

(ert-deftest xwapp-test-parts-forgotten-with-the-page ()
  "A page killed or loaded afresh sends its own parts: those of the one
before are forgotten, timers and all."
  (xwapp-test--with-page
    (show (car (xwapp-test--parts 2 "{\"op\":\"say\",\"n\":2}" 2)))
    (should (xwapp--get app :parts))
    (xwapp--on-kill app)
    (should-not (xwapp--get app :parts))
    (should-not (xwapp--get app :part-wait))
    (should-not (xwapp--get app :part-poll))))

;;; Files an app serves besides its page

(defconst xwapp-test--id "0123456789abcdef0123456789abcdef")

(defun xwapp-test--raw (path &optional headers transport)
  "The whole response to GET PATH, as bytes: its head and its body."
  (let ((cell (xwapp-test--ask "GET" path nil headers transport))
        (out ""))
    (set-process-filter (cdr cell) (lambda (_p s) (setq out (concat out s))))
    (with-timeout (10 (error "No answer"))
      (while (process-live-p (cdr cell))
        (accept-process-output nil 0.02)))
    out))

(defun xwapp-test--raw-post (app path)
  "The whole response to POST PATH under APP's secret, with {}."
  (let ((cell (xwapp-test--ask "POST" (xwapp-test--at app path) "{}"))
        (out ""))
    (set-process-filter (cdr cell) (lambda (_p s) (setq out (concat out s))))
    (with-timeout (10 (error "No answer"))
      (while (process-live-p (cdr cell))
        (accept-process-output nil 0.02)))
    out))

(defmacro xwapp-test--serving (&rest body)
  "BODY in `xwapp-test--with-browser', its app serving the files of
`dir' named an id and .png (`xwapp-serve-files'); `file' is the one
there, and beside it what must not be served."
  (declare (indent 0))
  `(xwapp-test--with-browser
     (let* ((dir (file-name-as-directory (expand-file-name "files" root)))
            (file (concat xwapp-test--id ".png")))
       (make-directory dir t)
       (with-temp-file (expand-file-name file dir) (insert "PNGDATA"))
       (with-temp-file (expand-file-name "index.sqlite" dir) (insert "db"))
       (with-temp-file (expand-file-name (concat xwapp-test--id ".txt") dir) (insert "text"))
       ;; A symlink to elsewhere, and a directory, under a name that fits.
       (make-symbolic-link (expand-file-name "app/notes.txt" root)
                           (expand-file-name (concat (make-string 32 ?a) ".png") dir))
       (make-directory (expand-file-name (concat (make-string 32 ?b) ".png") dir))
       (xwapp-serve-files app (lambda () dir) "[0-9a-f]\\{32\\}\\.png")
       ,@body)))

(ert-deftest xwapp-test-browser-serves-an-apps-files-by-name ()
  "An app's files (`xwapp-serve-files'): by a name its regexp takes whole,
from its directory, under the page's secret -- kept by the browser, as
a name's file never changes.  Nothing else: a name no file has (a
guessed one), another name, one that climbs or holds a slash, a symlink,
a directory; not without the secret, nor to another host."
  (xwapp-test--serving
    (let ((raw (xwapp-test--raw (xwapp-test--at app (concat ".files/" file)))))
      (should (string-prefix-p "HTTP/1.1 200 OK\r\n" raw))
      (should (string-match-p "\r\nContent-Type: image/png\r\n" raw))
      (should (string-match-p "\r\nCache-Control: private, max-age=604800, immutable\r\n" raw))
      (should-not (string-match-p "no-store" raw))
      (should (string-match-p "\r\nX-Content-Type-Options: nosniff\r\n" raw))
      (should (string-suffix-p "\r\n\r\nPNGDATA" raw)))
    (dolist (name (list "ffffffffffffffffffffffffffffffff.png"     ; guessed: not there
                        (upcase file) (concat xwapp-test--id ".PNG")
                        "index.sqlite" (concat xwapp-test--id ".txt")
                        (concat (make-string 32 ?a) ".png")          ; a symlink
                        (concat (make-string 32 ?b) ".png")          ; a directory
                        (concat "..%2F" file) (concat "%2E%2E/" file)
                        (concat "../files/" file) (concat file "/x")
                        "../app/notes.txt" ".." ""))
      (should (equal (car (xwapp-test--do "GET" (xwapp-test--at app (concat ".files/" name)))) 404)))
    (dolist (path (list (concat "/" (make-string 32 ?0) "/.files/" file)
                        (concat "/.files/" file)
                        (xwapp-test--at app (concat "app/.files/" file))))
      (should (equal (car (xwapp-test--do "GET" path)) 404)))
    (should (equal (car (xwapp-test--do "GET" (xwapp-test--at app (concat ".files/" file)) nil
                                        '(("Host" . "evil.example"))))
                   404))
    ;; Only GET.
    (should (equal (car (xwapp-test--do "POST" (xwapp-test--at app (concat ".files/" file)) "x")) 404))
    ;; An app that serves none.
    (xwapp--put app :sets nil)
    (should (equal (car (xwapp-test--do "GET" (xwapp-test--at app (concat ".files/" file)))) 404))))

(ert-deftest xwapp-test-browser-an-apps-files-on-other-devices ()
  "Through the proxy an app's files go where its page does: to one of
`xwapp-remote-users', and to no one else."
  (xwapp-test--serving
    (let ((xwapp-remote-hosts (list xwapp-test--box))
          (xwapp-remote-users (list "me@example.com"))
          (path (xwapp-test--at app (concat ".files/" file))))
      (xwapp-test--proxy-up)
      (should (equal (xwapp-test--do "GET" path nil (xwapp-test--from-box "me@example.com") 'proxy)
                     '(200 . "PNGDATA")))
      (dolist (h (list (xwapp-test--from-box nil) (xwapp-test--from-box "someone@else.com")))
        (should (equal (car (xwapp-test--do "GET" path nil h 'proxy)) 404)))
      ;; The proxy's headers on loopback are no proof.
      (should (equal (car (xwapp-test--do "GET" path nil (xwapp-test--from-box "me@example.com"))) 404)))))

(defun xwapp-test--touch (file text secs)
  "Write TEXT to FILE, dated SECS seconds from now."
  (with-temp-file file (insert text))
  (set-file-times file (time-add nil secs)))

(ert-deftest xwapp-test-browser-own-files-revalidate ()
  "The page's own files, and the kit's, go with an ETag and no-cache: a
browser that has one asks again with it, and is answered 304 -- no body
-- while it is still the one.  Edited, a file has another, and comes
whole.  Its ETag follows a symlink, as straight's build is made of, to
the file itself.  What changes -- the page's calls -- is still never kept."
  (xwapp-test--with-browser
    (let* ((path (xwapp-test--at app "app/ui/app.js"))
           (raw (xwapp-test--raw path))
           (etag (xwapp-test--header raw "ETag")))
      (should (string-prefix-p "HTTP/1.1 200 OK\r\n" raw))
      (should (string-match-p "\\`\"[0-9a-f]\\{20\\}\"\\'" etag))
      (should (equal (xwapp-test--header raw "Cache-Control") "no-cache"))
      (should-not (string-match-p "no-store" raw))
      (should (string-suffix-p "\r\n\r\n<app/ui/app.js>" raw))
      ;; Asked again with it: 304, no body, no length -- whichever way
      ;; the browser says it.
      (dolist (asked (list etag (concat "W/" etag) (concat "\"other\", " etag) "*"))
        (let ((again (xwapp-test--raw path `(("If-None-Match" . ,asked)))))
          (should (string-prefix-p "HTTP/1.1 304 Not Modified\r\n" again))
          (should (string-suffix-p "\r\n\r\n" again))
          (should-not (xwapp-test--header again "Content-Length"))
          (should (equal (xwapp-test--header again "ETag") etag))
          (should (equal (xwapp-test--header again "Cache-Control") "no-cache"))))
      (should (string-prefix-p "HTTP/1.1 200 OK\r\n"
                               (xwapp-test--raw path '(("If-None-Match" . "\"other\"")))))
      ;; The kit's own, the same.
      (should (xwapp-test--header (xwapp-test--raw (xwapp-test--at app "xwapp/ui/xwapp.js")) "ETag"))
      ;; Edited.
      (xwapp-test--touch (expand-file-name "app/ui/app.js" root) "<new>" 5)
      (let ((fresh (xwapp-test--raw path `(("If-None-Match" . ,etag)))))
        (should (string-prefix-p "HTTP/1.1 200 OK\r\n" fresh))
        (should (string-suffix-p "\r\n\r\n<new>" fresh))
        (should-not (equal (xwapp-test--header fresh "ETag") etag))))
    ;; A symlink: the file it points at is what changes.
    (let ((real (expand-file-name "real.js" root))
          (link (xwapp-test--at app "app/ui/link.js")))
      (xwapp-test--touch real "one" 0)
      (make-symbolic-link real (expand-file-name "app/ui/link.js" root))
      (let ((etag (xwapp-test--header (xwapp-test--raw link) "ETag")))
        (should (string-prefix-p "HTTP/1.1 304" (xwapp-test--raw link `(("If-None-Match" . ,etag)))))
        (xwapp-test--touch real "two" 5)
        (should (string-suffix-p "\r\n\r\ntwo" (xwapp-test--raw link `(("If-None-Match" . ,etag)))))))
    ;; What changes.
    (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p1") "{}")
    (let ((xwapp--hold 0.1))
      (should (equal (xwapp-test--header (xwapp-test--raw (xwapp-test--at app "next?page=p1")) "Cache-Control")
                     "no-store")))))

;; Gzip'd files.  The text is made up.

(defun xwapp-test--body (raw)
  "The body of RAW, a whole response, as bytes."
  (substring raw (+ 4 (string-search "\r\n\r\n" raw))))

(defun xwapp-test--gunzip (bytes)
  "BYTES gunzip'd."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert bytes)
    (zlib-decompress-region (point-min) (point-max))
    (buffer-string)))

(defconst xwapp-test--js
  (apply #'concat (make-list 300 "function render(herd) { return herd.agents.map(row).join(''); }\n"))
  "Some 20 KB of a page's script, made up.")

(defmacro xwapp-test--gzipping (&rest body)
  "BODY in `xwapp-test--with-browser', its app.js `xwapp-test--js', the
gzip'd files kept afresh; `gzips' counts the gzip processes started."
  (declare (indent 0))
  `(xwapp-test--with-browser
     (let ((xwapp--gzipped (make-hash-table :test 'equal))
           (xwapp--gzipped-order nil)
           (xwapp--gzipping (make-hash-table :test 'equal))
           (gzips 0)
           (start (symbol-function 'xwapp--gzip-start)))
       (ignore gzips)
       (with-temp-file (expand-file-name "app/ui/app.js" root) (insert xwapp-test--js))
       (cl-letf (((symbol-function 'xwapp--gzip-start)
                  (lambda (&rest args) (cl-incf gzips) (apply start args))))
         ,@body))))

(ert-deftest xwapp-test-browser-files-go-gzipd ()
  "A file of the page's, text of 1 KB or more, goes gzip'd to a browser
whose Accept-Encoding takes gzip -- its bytes the file's, inflated --
and plain to one that does not; either way Vary: Accept-Encoding.  The
kit's too.  A file under 1 KB, or one not text, goes plain, with no Vary."
  (skip-unless (and (executable-find "gzip") (zlib-available-p)))
  (xwapp-test--gzipping
    (let ((path (xwapp-test--at app "app/ui/app.js")))
      (dolist (accept '("gzip, deflate" "gzip" "deflate, gzip;q=0.5" "*" "br, *;q=0.1" "x-gzip"))
        (let ((raw (xwapp-test--raw path `(("Accept-Encoding" . ,accept)))))
          (should (string-prefix-p "HTTP/1.1 200 OK\r\n" raw))
          (should (equal (xwapp-test--header raw "Content-Encoding") "gzip"))
          (should (equal (xwapp-test--header raw "Vary") "Accept-Encoding"))
          (should (< (length (xwapp-test--body raw)) (/ (length xwapp-test--js) 4)))
          (should (equal (string-to-number (xwapp-test--header raw "Content-Length"))
                         (length (xwapp-test--body raw))))
          (should (equal (xwapp-test--gunzip (xwapp-test--body raw)) xwapp-test--js))))
      (dolist (accept '(nil "identity" "br" "gzip;q=0" "*;q=0" "deflate, gzip;q=0, *"))
        (let ((raw (xwapp-test--raw path (and accept `(("Accept-Encoding" . ,accept))))))
          (should (string-prefix-p "HTTP/1.1 200 OK\r\n" raw))
          (should-not (xwapp-test--header raw "Content-Encoding"))
          (should (equal (xwapp-test--header raw "Vary") "Accept-Encoding"))
          (should (equal (xwapp-test--body raw) xwapp-test--js)))))
    ;; The kit's.
    (with-temp-file (expand-file-name "xwapp/ui/xwapp.js" root) (insert xwapp-test--js))
    (should (equal (xwapp-test--header (xwapp-test--raw (xwapp-test--at app "xwapp/ui/xwapp.js")
                                                        '(("Accept-Encoding" . "gzip")))
                                       "Content-Encoding")
                   "gzip"))
    ;; Small, or not text: plain, and no Vary.
    (with-temp-file (expand-file-name "app/ui/small.css" root) (insert (make-string 1023 ?x)))
    (with-temp-file (expand-file-name "app/ui/pic.png" root) (insert (make-string 4000 ?x)))
    (dolist (name '("small.css" "pic.png"))
      (let ((raw (xwapp-test--raw (xwapp-test--at app (concat "app/ui/" name)) '(("Accept-Encoding" . "gzip")))))
        (should (string-prefix-p "HTTP/1.1 200 OK\r\n" raw))
        (should-not (xwapp-test--header raw "Content-Encoding"))
        (should-not (xwapp-test--header raw "Vary"))))
    ;; The page's calls, on loopback: never (`xwapp-test-next-gzipd-through-the-proxy').
    (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p1") "{}")
    (let ((next (xwapp-test--ask "GET" (xwapp-test--at app "next?page=p1") nil '(("Accept-Encoding" . "gzip")))))
      (xwapp-test--turn)
      (xwapp-js app "render" xwapp-test--js)
      (should (equal (car (xwapp-test--answer next)) 200)))
    (let ((xwapp--hold 0.1))
      (should-not (xwapp-test--header (xwapp-test--raw (xwapp-test--at app "next?page=p1") '(("Accept-Encoding" . "gzip")))
                                      "Content-Encoding")))))

(ert-deftest xwapp-test-browser-gzipd-files-have-their-own-etag ()
  "The gzip'd bytes and the plain ones are other bytes, so each has an
ETag of its own, strong: a browser that has one is answered 304 for it,
and sent the other whole when it now takes the other.  Edited, the file
has two new ones."
  (skip-unless (and (executable-find "gzip") (zlib-available-p)))
  (xwapp-test--gzipping
    (let* ((path (xwapp-test--at app "app/ui/app.js"))
           (gz '(("Accept-Encoding" . "gzip, deflate")))
           (zipped (xwapp-test--header (xwapp-test--raw path gz) "ETag"))
           (plain (xwapp-test--header (xwapp-test--raw path) "ETag")))
      (should (string-match-p "\\`\"[0-9a-f]\\{20\\}\"\\'" plain))
      (should (equal zipped (concat (substring plain 0 -1) "-gz\"")))
      (should-not (string-prefix-p "W/" zipped))
      ;; Each answered 304 for its own, the other's no match.
      (pcase-dolist (`(,accept ,has ,code)
                     `((,gz ,zipped 304) (nil ,plain 304) (,gz ,(concat "W/" zipped) 304)
                       (,gz ,plain 200) (nil ,zipped 200)))
        (let ((raw (xwapp-test--raw path (cons (cons "If-None-Match" has) accept))))
          (should (string-prefix-p (format "HTTP/1.1 %d" code) raw))
          (when (= code 304)
            (should (equal (xwapp-test--header raw "ETag") (string-remove-prefix "W/" has)))
            (should (equal (xwapp-test--header raw "Vary") "Accept-Encoding"))
            (should-not (xwapp-test--header raw "Content-Encoding"))
            (should (string-suffix-p "\r\n\r\n" raw)))))
      ;; Edited: both anew.
      (xwapp-test--touch (expand-file-name "app/ui/app.js" root) (concat xwapp-test--js "// more\n") 5)
      (let ((raw (xwapp-test--raw path (cons (cons "If-None-Match" zipped) gz))))
        (should (string-prefix-p "HTTP/1.1 200" raw))
        (should-not (member (xwapp-test--header raw "ETag") (list zipped plain)))
        (should (string-suffix-p "// more\n" (xwapp-test--gunzip (xwapp-test--body raw))))))))

(ert-deftest xwapp-test-gzip-once-per-version-kept-within-bounds ()
  "A file is gzip'd once per version -- not per request, nor for each of
the requests that came while it was -- and what is kept stays within
`xwapp--gzip-keep', the least lately used going first."
  (skip-unless (and (executable-find "gzip") (zlib-available-p)))
  (xwapp-test--gzipping
    (let ((path (xwapp-test--at app "app/ui/app.js"))
          (gz '(("Accept-Encoding" . "gzip"))))
      ;; Three at once, then three more: one gzip.
      (let ((asked (list (xwapp-test--ask "GET" path nil gz) (xwapp-test--ask "GET" path nil gz)
                         (xwapp-test--ask "GET" path nil gz))))
        (dolist (cell asked) (should (equal (car (xwapp-test--answer cell)) 200))))
      (dotimes (_ 3) (xwapp-test--raw path gz))
      (should (= gzips 1))
      (should (= (hash-table-count xwapp--gzipped) 1))
      ;; Other files, and a bound that holds two of them.
      (let* ((one (length (gethash (car xwapp--gzipped-order) xwapp--gzipped)))
             (xwapp--gzip-keep (+ (* 2 one) (/ one 2))))
        (dolist (name '("a.js" "b.js" "c.js"))
          (with-temp-file (expand-file-name (concat "app/ui/" name) root)
            (insert xwapp-test--js "// " name "\n"))
          (xwapp-test--raw (xwapp-test--at app (concat "app/ui/" name)) gz))
        (should (= gzips 4))
        (should (= (hash-table-count xwapp--gzipped) 2))
        (should (<= (apply #'+ (mapcar (lambda (k) (length (gethash k xwapp--gzipped))) xwapp--gzipped-order))
                    xwapp--gzip-keep))
        ;; app.js went first: asked again, gzip'd again; c.js, kept, not.
        (xwapp-test--raw path gz)
        (should (= gzips 5))
        (xwapp-test--raw (xwapp-test--at app "app/ui/c.js") gz)
        (should (= gzips 5))))))

(ert-deftest xwapp-test-gzip-that-fails-sends-plain ()
  "No gzip, or one that fails or hangs: the file goes plain, under its
own ETag, and nothing is kept."
  (skip-unless (executable-find "sleep"))
  (xwapp-test--gzipping
    (let ((path (xwapp-test--at app "app/ui/app.js"))
          (gz '(("Accept-Encoding" . "gzip"))))
      (dolist (how '("/nonexistent/gzip" "false"))
        (let* ((xwapp--gzip-program how)
               (raw (xwapp-test--raw path gz)))
          (should (string-prefix-p "HTTP/1.1 200" raw))
          (should-not (xwapp-test--header raw "Content-Encoding"))
          (should-not (string-suffix-p "-gz\"" (xwapp-test--header raw "ETag")))
          (should (equal (xwapp-test--body raw) xwapp-test--js))))
      ;; One that hangs: plain after `xwapp--gzip-wait'.
      (let ((sleeper (expand-file-name "slow-gzip" root)))
        (with-temp-file sleeper (insert "#!/bin/sh\nexec sleep 30\n"))
        (set-file-modes sleeper #o755)
        (let* ((xwapp--gzip-program sleeper)
               (xwapp--gzip-wait 0.3)
               (raw (xwapp-test--raw path gz)))
          (should (string-prefix-p "HTTP/1.1 200" raw))
          (should (equal (xwapp-test--body raw) xwapp-test--js))))
      (should (= (hash-table-count xwapp--gzipped) 0))
      (should-not (seq-some (lambda (p) (string-prefix-p "xwapp-gzip" (process-name p))) (process-list)))
      ;; Off: plain, and no Vary.
      (let ((xwapp--gzip-program nil))
        (should-not (xwapp-test--header (xwapp-test--raw path gz) "Vary"))))))

;; The page's calls gzip'd, through the proxy; and never a secret.

(defconst xwapp-test--said
  (apply #'concat (make-list 400 "An agent said this, and then that. "))
  "Some 14 KB of what the page is told, made up: past `xwapp--gzip-next-min'.")

(defmacro xwapp-test--through-proxy (&rest body)
  "BODY in `xwapp-test--gzipping', the page served to your other devices
through the proxy, its page `p1' there; `ask' (HEADERS THUNK) asks its
`next' through the proxy with HEADERS (the proxy's own besides), calls
THUNK once it is held, and gives the whole answer.  Every answer whose
body goes gzip'd is inflated: those that hold a secret xwapp keeps
gather in `leaked', the count of all in `gzipped'."
  (declare (indent 0))
  `(xwapp-test--gzipping
     (let ((xwapp-remote-hosts (list xwapp-test--box))
           (xwapp-remote-users (list "me@example.com"))
           (leaked nil)
           (gzipped 0)
           (respond (symbol-function 'xwapp--respond)))
       (ignore leaked gzipped)
       (xwapp--put app :reopen #'xwapp-test--keep-reopen)
       (xwapp-test--proxy-up)
       (cl-letf (((symbol-function 'xwapp--respond)
                  (lambda (proc code &optional type body headers done)
                    (when (equal (cdr (assoc "Content-Encoding" headers)) "gzip")
                      (cl-incf gzipped)
                      (let ((plain (xwapp-test--gunzip body)))
                        (when (xwapp--holds-secret-p plain) (push plain leaked))))
                    (funcall respond proc code type body headers done))))
         (let ((box (xwapp-test--from-box "me@example.com"))
               (ask nil))
           (setq ask (lambda (headers thunk)
                       (let ((cell (xwapp-test--ask "GET" (xwapp-test--at app "next?page=p1") nil
                                                    (append box headers) 'proxy))
                             (out ""))
                         (set-process-filter (cdr cell) (lambda (_p s) (setq out (concat out s))))
                         (xwapp-test--turn)
                         (funcall thunk)
                         (with-timeout (10 (error "No answer"))
                           (while (process-live-p (cdr cell))
                             (accept-process-output nil 0.02)))
                         out)))
           (should (equal (car (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p1") "{}" box 'proxy)) 204))
           ;; Its greeting, the phone's button: out of the way.
           (funcall ask nil #'ignore)
           ,@body)))))

(defun xwapp-test--calls (raw)
  "The calls RAW, a whole answer to a `next', carries: inflated first
when it went gzip'd."
  (let ((body (xwapp-test--body raw)))
    (xwapp-test--json (decode-coding-string
                       (if (xwapp-test--header raw "Content-Encoding") (xwapp-test--gunzip body) body)
                       'utf-8))))

(ert-deftest xwapp-test-next-gzipd-through-the-proxy ()
  "The page's calls, asked through the proxy by a browser that takes
gzip, go gzip'd from `xwapp--gzip-next-min' on -- the same calls,
inflated, with Vary: Accept-Encoding and never kept.  Fewer bytes go
plain, as do the calls to a browser that takes no gzip, and to this
machine's on loopback, which is fast."
  (skip-unless (and (executable-find "gzip") (zlib-available-p)))
  (xwapp-test--through-proxy
    (let ((gz '(("Accept-Encoding" . "gzip, deflate, br"))))
      (let ((raw (funcall ask gz (lambda () (xwapp-js app "render" xwapp-test--said) (xwapp-js app "flash" "after")))))
        (should (string-prefix-p "HTTP/1.1 200 OK\r\n" raw))
        (should (equal (xwapp-test--header raw "Content-Encoding") "gzip"))
        (should (equal (xwapp-test--header raw "Vary") "Accept-Encoding"))
        (should (equal (xwapp-test--header raw "Cache-Control") "no-store"))
        (should (< (length (xwapp-test--body raw)) (/ (length xwapp-test--said) 6)))
        (should (equal (mapcar (lambda (c) (alist-get 'arg c)) (xwapp-test--calls raw))
                       (list xwapp-test--said "after"))))
      (should (= gzipped 1))
      ;; Small: plain.
      (let ((raw (funcall ask gz (lambda () (xwapp-js app "flash" "small")))))
        (should-not (xwapp-test--header raw "Content-Encoding"))
        (should (equal (alist-get 'arg (car (xwapp-test--calls raw))) "small")))
      ;; A browser that takes no gzip: plain.
      (dolist (accept '(nil (("Accept-Encoding" . "identity")) (("Accept-Encoding" . "gzip;q=0"))))
        (let ((raw (funcall ask accept (lambda () (xwapp-js app "render" xwapp-test--said)))))
          (should-not (xwapp-test--header raw "Content-Encoding"))
          (should (equal (alist-get 'arg (car (xwapp-test--calls raw))) xwapp-test--said))))
      (should (= gzipped 1))
      ;; This machine's browser, on loopback: plain.
      (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p2") "{}")
      (let ((next (xwapp-test--ask "GET" (xwapp-test--at app "next?page=p2") nil gz))
            (out ""))
        (set-process-filter (cdr next) (lambda (_p s) (setq out (concat out s))))
        (xwapp-test--turn)
        (xwapp-js app "render" xwapp-test--said)
        (with-timeout (10 (error "No answer"))
          (while (process-live-p (cdr next)) (accept-process-output nil 0.02)))
        (should (string-prefix-p "HTTP/1.1 200 OK\r\n" out))
        (should-not (xwapp-test--header out "Content-Encoding")))
      (should (= gzipped 1))
      (should-not leaked))))

(ert-deftest xwapp-test-a-secret-never-goes-gzipd ()
  "Nothing that holds a secret xwapp keeps goes gzip'd (BREACH), however
it came to be there: the page's address in its QR code, text that
quotes it -- in any case -- another page's, one kept over a restart, a
file of the page's.  The same bytes without it go gzip'd.  Everything
gzip'd is gzip'd by `xwapp--deflate', which looks: every answer that
went gzip'd here is inflated and looked in again."
  (skip-unless (and (executable-find "gzip") (zlib-available-p)))
  (xwapp-test--through-proxy
    (let* ((gz '(("Accept-Encoding" . "gzip")))
           (url (xwapp-remote-url app))
           (other (xwapp-test--app :index (expand-file-name "other/ui/index.html" root)))
           (plain-with (lambda (thunk what)
                         (let ((raw (funcall ask gz thunk)))
                           (should (string-prefix-p "HTTP/1.1 200 OK\r\n" raw))
                           (should-not (xwapp-test--header raw "Content-Encoding"))
                           (should (string-search what (xwapp-test--body raw)))))))
      ;; Without a secret: gzip'd.
      (should (xwapp-test--header (funcall ask gz (lambda () (xwapp-js app "render" xwapp-test--said)))
                                  "Content-Encoding"))
      ;; The QR code, the address in it, beside text from elsewhere.
      (funcall plain-with (lambda () (xwapp-js app "render" xwapp-test--said) (xwapp--show-qr app)) url)
      ;; Text that quotes it -- as written, and in capitals.
      (funcall plain-with (lambda () (xwapp-js app "render" (concat xwapp-test--said "see " url))) url)
      (funcall plain-with (lambda () (xwapp-js app "render" (concat xwapp-test--said (upcase url))))
               (upcase (xwapp--secret app)))
      ;; Another page's secret.
      (funcall plain-with (lambda () (xwapp-js app "render" (concat xwapp-test--said (xwapp--secret other))))
               (xwapp--secret other))
      ;; A file of the page's that holds it: plain, and so each time.
      (with-temp-file (expand-file-name "app/ui/told.js" root)
        (insert xwapp-test--js "const here = \"" url "\";\n"))
      (dotimes (_ 2)
        (let ((raw (xwapp-test--raw (xwapp-test--at app "app/ui/told.js") gz)))
          (should (string-prefix-p "HTTP/1.1 200 OK\r\n" raw))
          (should-not (xwapp-test--header raw "Content-Encoding"))
          (should (string-search url (xwapp-test--body raw)))))
      (should (xwapp-test--header (xwapp-test--raw (xwapp-test--at app "app/ui/app.js") gz) "Content-Encoding"))
      ;; Straight to the one that gzips: the same.
      (let ((got 'none))
        (xwapp--deflate (concat xwapp-test--said url) (lambda (b) (setq got b)))
        (should-not got))
      ;; A secret kept over a restart, its app not loaded yet.
      (let ((xwapp--keep t)
            (kept "fedcba9876543210fedcba9876543210"))
        (with-temp-file xwapp-state-file
          (prin1 `(:port 1 :pages (("*later*" ,kept nil))) (current-buffer)))
        (should (xwapp--holds-secret-p (concat "x" kept "x")))
        (delete-file xwapp-state-file))
      (should-not (xwapp--holds-secret-p xwapp-test--said))
      ;; The calls without a secret, and app.js.
      (should (= gzipped 2))
      (should-not leaked))))

(ert-deftest xwapp-test-an-answer-past-the-socket-goes-from-a-thread ()
  "An answer past `xwapp--send-aside' -- what the proxy's socket takes
at once -- goes from a thread, whole: Emacs, finding that socket full,
would wait 20 ms at a time with no key read.  One within it, at once."
  (skip-unless (fboundp 'make-thread))
  (xwapp-test--with-browser
    (let ((past (concat (make-string (1+ xwapp--send-aside) ?x) "end")))
      (with-temp-file (expand-file-name "app/ui/past.js" root) (insert past))
      (with-temp-file (expand-file-name "app/ui/within.js" root)
        (insert (make-string (- xwapp--send-aside 1024) ?x)))
      (xwapp-test--counting-threads
        (should (equal (cdr (xwapp-test--do "GET" (xwapp-test--at app "app/ui/within.js")))
                       (make-string (- xwapp--send-aside 1024) ?x)))
        (should-not made)
        (should (equal (cdr (xwapp-test--do "GET" (xwapp-test--at app "app/ui/past.js"))) past))
        (should (equal made '("xwapp-send")))))))

(ert-deftest xwapp-test-browser-a-free-page-is-told-what-it-is-made-of ()
  "A page's hello is answered with what it is made of -- its directory's
files and the kit's -- and a page let go that can come back (free) is
told it again, to come back where it is only while the two are one: a
ui/ file edited meanwhile makes another.  Taken, or closed, it is told
nothing of it."
  (xwapp-test--with-browser
    (let* ((xwapp--gone-after 0.2)
           (hello (xwapp-test--raw-post app "hello?page=p1"))
           (ui (xwapp-test--header hello "Xwapp-Ui"))
           (said (lambda (page)
                   (xwapp-test--json (cdr (xwapp-test--do "GET" (xwapp-test--at app (concat "next?page=" page))))))))
      (should (string-prefix-p "HTTP/1.1 204" hello))
      (should (string-match-p "\\`[0-9a-f]\\{16\\}\\'" ui))
      (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p2") "{}")
      (should (equal (funcall said "p1") '((why . "taken"))))
      (xwapp-test--turn 0.4)
      (should (= closed 1))
      (should (equal (funcall said "p2") '((why . "closed"))))
      (xwapp-test--reopens app)
      (should (equal (funcall said "p2") `((why . "free") (ui . ,ui))))
      (should (equal (alist-get 'ui (xwapp-test--json
                                     (cdr (xwapp-test--do "POST" (xwapp-test--at app "intent?page=p2") "{}"))))
                     ui))
      ;; The kit's file edited: another.
      (xwapp-test--touch (expand-file-name "xwapp/ui/xwapp.js" root) "<new kit>" 5)
      (let ((now (alist-get 'ui (funcall said "p2"))))
        (should (stringp now))
        (should-not (equal now ui))
        ;; Asked the same again, the same.
        (should (equal (alist-get 'ui (funcall said "p2")) now)))
      ;; A file added to the page's directory: another again.
      (let ((before (alist-get 'ui (funcall said "p2"))))
        (with-temp-file (expand-file-name "app/ui/more.css" root) (insert "x"))
        (should-not (equal (alist-get 'ui (funcall said "p2")) before))))))

(ert-deftest xwapp-test-ui-names-a-page-directory-only ()
  "A directory of more files than a page is made of is not looked through:
no fingerprint, and a page let go loads again, as it always did."
  (let* ((root (file-name-as-directory (make-temp-file "xwapp-ui" t)))
         (app (xwapp-test--app :index (expand-file-name "pkg/ui/index.html" root))))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "pkg/ui" root) t)
          (make-directory (expand-file-name "xwapp/ui" root) t)
          (with-temp-file (expand-file-name "pkg/ui/index.html" root) (insert "x"))
          (should (xwapp--ui app))
          (dotimes (i (1+ xwapp--ui-max))
            (with-temp-file (expand-file-name (format "pkg/ui/f%d.js" i) root) (insert "x")))
          (should-not (xwapp--ui app)))
      (delete-directory root t))))

(defmacro xwapp-test--counting-threads (&rest body)
  "Run BODY with `made' gathering the name of each thread made."
  (declare (indent 0))
  `(let ((made nil)
         (make (symbol-function 'make-thread)))
     (cl-letf (((symbol-function 'make-thread)
                (lambda (fn &optional tname &rest more)
                  (push tname made)
                  (apply make fn tname more))))
       ,@body)))

(ert-deftest xwapp-test-browser-a-big-file-goes-from-a-thread ()
  "Any answer past `xwapp--send-aside' is sent from a thread of its own,
whole, so a slow reader cannot hold Emacs: a file an app serves, and the
page's own -- the herd's app.js is past it.  A small one as before."
  (skip-unless (fboundp 'make-thread))
  (xwapp-test--serving
    (let* ((big (concat (make-string (* 3 1024 1024) ?x) "end"))
           (name (concat (make-string 32 ?c) ".png")))
      (with-temp-file (expand-file-name name dir) (insert big))
      (xwapp-test--counting-threads
        (should (equal (xwapp-test--do "GET" (xwapp-test--at app (concat ".files/" file)))
                       '(200 . "PNGDATA")))
        (should (equal (xwapp-test--do "GET" (xwapp-test--at app "app/ui/app.js"))
                       '(200 . "<app/ui/app.js>")))
        (should-not made)
        (let ((answer (xwapp-test--do "GET" (xwapp-test--at app (concat ".files/" name)))))
          (should (equal (car answer) 200))
          (should (= (length (cdr answer)) (length big)))
          (should (string-suffix-p "end" (cdr answer))))
        (should (equal made '("xwapp-send")))
        (with-temp-file (expand-file-name "app/ui/big.js" root) (insert big))
        (let ((answer (xwapp-test--do "GET" (xwapp-test--at app "app/ui/big.js"))))
          (should (= (length (cdr answer)) (length big)))
          (should (string-suffix-p "end" (cdr answer))))
        (should (equal made '("xwapp-send" "xwapp-send")))))))

(ert-deftest xwapp-test-browser-a-big-next-goes-from-a-thread ()
  "The page's calls, past `xwapp--send-aside' -- a herd -- go from a
thread too, whole and in order; a small answer as before.  The text is
made up."
  (skip-unless (fboundp 'make-thread))
  (xwapp-test--with-browser
    (xwapp-test--counting-threads
      (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p1") "{}")
      (let ((next (xwapp-test--ask "GET" (xwapp-test--at app "next?page=p1"))))
        (xwapp-test--turn)
        (xwapp-js app "flash" "small")
        (should (equal (xwapp-test--answer next) '(200 . "[{\"ns\":\"T\",\"fn\":\"flash\",\"arg\":\"small\"}]"))))
      (should-not made)
      (let ((next (xwapp-test--ask "GET" (xwapp-test--at app "next?page=p1")))
            (herd (apply #'concat (make-list 20000 "長い中文 agent "))))
        (xwapp-test--turn)
        (xwapp-js app "render" herd)
        (xwapp-js app "flash" "after")
        (let ((answer (xwapp-test--answer next)))
          (should (equal (car answer) 200))
          (should (equal (mapcar (lambda (c) (alist-get 'arg c)) (xwapp-test--json (cdr answer)))
                         (list herd "after")))))
      (should (equal made '("xwapp-send")))
      (xwapp-test--turn)
      (should-not (xwapp--get app :sending)))))

(ert-deftest xwapp-test-browser-calls-that-can-wait ()
  "Calls that can wait go after every other, a `next' carrying of them
only what `xwapp--later-budget' leaves room for -- one at least, so they
always move -- and the rest the next time the page asks.  Those of a
name are dropped when forgotten."
  (xwapp-test--with-browser
    (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p1") "{}")
    (let ((xwapp--later-budget 1000)
          (args (lambda (next) (mapcar (lambda (c) (alist-get 'arg c))
                                       (xwapp-test--json (cdr (xwapp-test--answer next))))))
          (big (lambda (c) (make-string 400 c))))
      (xwapp-js app "fill" (funcall big ?a) 'room)
      (xwapp-js app "fill" (funcall big ?b) 'room)
      (xwapp-js app "fill" (funcall big ?c) 'room)
      (xwapp-js app "flash" "now")
      (let ((next (xwapp-test--ask "GET" (xwapp-test--at app "next?page=p1"))))
        (should (equal (funcall args next) (list "now" (funcall big ?a) (funcall big ?b)))))
      ;; The page asks again: the rest, at once.
      (let ((next (xwapp-test--ask "GET" (xwapp-test--at app "next?page=p1"))))
        (should (equal (funcall args next) (list (funcall big ?c)))))
      ;; One at least, though the others fill the budget.
      (xwapp-js app "fill" (funcall big ?d) 'room)
      (xwapp-js app "flash" (make-string 1200 ?n))
      (let ((next (xwapp-test--ask "GET" (xwapp-test--at app "next?page=p1"))))
        (should (equal (funcall args next) (list (make-string 1200 ?n) (funcall big ?d)))))
      ;; Forgotten by name: the others stay.
      (xwapp-js app "fill" "old" 'room)
      (xwapp-js app "fill" "other" 'elsewhere)
      (xwapp-js-forget app 'room)
      (let ((next (xwapp-test--ask "GET" (xwapp-test--at app "next?page=p1"))))
        (should (equal (funcall args next) '("other"))))
      ;; A page that says hello anew has none of what waited.
      (xwapp-js app "fill" "stale" 'room)
      (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p2") "{}")
      (should-not (xwapp--get app :later)))))

(ert-deftest xwapp-test-browser-the-last-of-its-kind ()
  "A call of a KEY takes the place of the one of that KEY still waiting,
after the calls made since; calls of another key, or of none, stay.  A
`next' sent carries each kind once, and is told what it carried, by kind,
the biggest first -- names and bytes, nothing they said."
  (xwapp-test--with-browser
    (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p1") "{}")
    (let ((xwapp--reqs (make-vector xwapp--reqs-max nil)) (xwapp--reqs-at 0)
          (said (lambda (next) (mapcar (lambda (c) (list (alist-get 'fn c) (alist-get 'arg c)))
                                       (xwapp-test--json (cdr (xwapp-test--answer next)))))))
      (xwapp-js app "herd" "h1" nil "herd")
      (xwapp-js app "flash" "a")
      (xwapp-js app "screen" "s1" nil "screen:x")
      (xwapp-js app "herd" "h2" nil "herd")
      (xwapp-js app "screen" "y1" nil "screen:y")
      (xwapp-js app "flash" "b")
      (xwapp-js app "herd" "h3" nil "herd")
      (let ((next (xwapp-test--ask "GET" (xwapp-test--at app "next?page=p1"))))
        (should (equal (funcall said next)
                       '(("flash" "a") ("screen" "s1") ("screen" "y1") ("flash" "b") ("herd" "h3"))))
        (let ((req (seq-find (lambda (r) (and r (equal (plist-get r :path) "next") (plist-get r :calls)))
                             (append xwapp--reqs nil))))
          (should (equal (mapcar (lambda (c) (list (nth 0 c) (nth 1 c))) (plist-get req :calls))
                         '(("screen" 2) ("flash" 2) ("herd" 1))))
          (should (cl-every (lambda (c) (> (nth 2 c) 20)) (plist-get req :calls)))))
      ;; Sent: a new one is no longer of the one gone.
      (xwapp-js app "herd" "h4" nil "herd")
      (xwapp-js app "herd" "h5" nil "herd")
      (let ((next (xwapp-test--ask "GET" (xwapp-test--at app "next?page=p1"))))
        (should (equal (funcall said next) '(("herd" "h5")))))
      ;; A page that says hello anew starts with none of them.
      (xwapp-js app "herd" "stale" nil "herd")
      (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p2") "{}")
      (should-not (xwapp--get app :keyed))
      (xwapp-js app "herd" "h6" nil "herd")
      (should (= 1 (length (xwapp--get app :outbox)))))))

(ert-deftest xwapp-test-browser-let-go-is-said ()
  "Each time the app's page is let go -- another tab's hello, the same
tab's again (it comes back asking for what it shows), the page gone --
`xwapp-let-go-functions' are called with the app, each on its own: one
that fails is said, the next still called."
  (xwapp-test--with-browser
    (let* ((said nil)
           (xwapp-let-go-functions (list (lambda (_a) (error "Boom"))
                                         (lambda (a) (push (list (eq a app) (xwapp--get a :page)) said)))))
      (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p1") "{}")
      (setq said nil)
      ;; Another tab takes it: called before it is that tab's.
      (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p2") "{}")
      (should (equal said '((t nil))))
      (should (equal (xwapp--get app :page) "p2"))
      ;; The same tab again.
      (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p2") "{}")
      (should (= (length said) 2))
      ;; Gone.
      (xwapp--gone app)
      (should (= (length said) 3))
      (should-not (xwapp--get app :page)))))

(ert-deftest xwapp-test-calls-said ()
  "What a `next' carried, by the name of each call's function: how many
and how many bytes, the biggest first, `xwapp--calls-said-max' at most;
a call not of the form, `?'."
  (let ((c (lambda (fn n) (format "{\"ns\":\"GM\",\"fn\":\"%s\",\"arg\":\"%s\"}" fn (make-string n ?x)))))
    (should (equal (mapcar (lambda (x) (list (nth 0 x) (nth 1 x))) (xwapp--calls-said
                     (list (funcall c "renderHerd" 900) (funcall c "setChat" 100) (funcall c "renderHerd" 900)
                           "{\"xwapp\":\"phone\"}" (funcall c "a" 1) (funcall c "b" 2) (funcall c "c" 3))))
                   '(("renderHerd" 2) ("setChat" 1) ("c" 1) ("b" 1) ("a" 1))))
    (should (equal (car (car (xwapp--calls-said (list "{\"xwapp\":\"phone\"}")))) "?"))
    (should (equal (nth 2 (car (xwapp--calls-said (list (funcall c "renderHerd" 900)))))
                   (string-bytes (funcall c "renderHerd" 900))))))

(ert-deftest xwapp-test-timing-names-a-big-next ()
  "A slow moment's line names its biggest `next', when it was big, and
what it carried -- the kinds as the record keeps them."
  (let ((r (list :kind "room" :d 24.0 :sum '((emacs . 21.7) (page . 1.7))
                 :reqs (list (list :p "next" :n 14 :b 9000 :calls [["renderHerd" 1 120000]])
                             (list :p "next" :n 15 :b 374188
                                   :calls [["renderLog" 1 1500000] ["renderHerd" 3 360000] ["setChat" 1 15000]])
                             (list :p "intent" :n 13 :b 300)))))
    (should (equal (xwapp-timing-summary r)
                   (concat "room 24.0 s: Emacs answering 21.7 s · page 1.7 s"
                           " · next 365 KB: renderLog 1.4 MB, renderHerd ×3 352 KB, setChat 15 KB")))
    ;; Read back as before: the bytes are no phase.
    (should (equal (mapcar #'car (xwapp-timing-summary-phases (xwapp-timing-summary r))) '(emacs page)))
    ;; No big one: nothing said.
    (plist-put r :reqs (list (list :p "next" :n 14 :b 9000 :calls [["renderHerd" 1 120000]])))
    (should (equal (xwapp-timing-summary r) "room 24.0 s: Emacs answering 21.7 s · page 1.7 s"))))

(ert-deftest xwapp-test-browser-when-sent ()
  "What waits for the calls made so far is called once the `next' that
carries them is out -- not before, nor for one that can wait; with
nothing to send, at once; never for a page let go first."
  (xwapp-test--with-browser
    (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p1") "{}")
    (let (said)
      (xwapp-js app "flash" "first")
      (xwapp-js app "fill" "later" 'room)
      (xwapp-when-sent app (lambda () (push 'out said)))
      (xwapp-test--turn)
      (should-not said)
      (let ((next (xwapp-test--ask "GET" (xwapp-test--at app "next?page=p1"))))
        (should (equal (car (xwapp-test--answer next)) 200))
        (xwapp-test--turn)
        (should (equal said '(out))))
      ;; Nothing waiting to go: at once.
      (xwapp-when-sent app (lambda () (push 'now said)))
      (should (equal said '(now out)))
      ;; The page gone first: not at all.
      (xwapp-js app "flash" "second")
      (xwapp-when-sent app (lambda () (push 'never said)))
      (xwapp--let-go app)
      (xwapp-test--turn)
      (should (equal said '(now out))))))

(ert-deftest xwapp-test-timing-an-apps-own-moment ()
  "A moment the app begins and says is over -- the herd page's room, from
a tap to its conversation drawn -- is kept as a load's is, of the app's
kind, and said so.  A kind that is no word is not kept."
  (xwapp-test--timing
    (let ((base (- (float-time) 10)))
      (xwapp--timing-take
       app (xwapp-test--moment "room" base 2.4
                               '(((p . "intent") (n . 6) (s . 5) (q . 0) (w . 300) (dl . 0))
                                 ((p . "next") (n . 7) (s . 300) (q . 0) (w . 150) (dl . 1950)))
                               '((intent . 305) (room . 2400)))
       (list :app app :via 'proxy :path "intent" :n 8 :page xwapp-test--page-id))
      (let ((r (car xwapp--timings)))
        (should (equal (plist-get r :kind) "room"))
        (should (plist-get r :slow))
        (should (string-prefix-p "room 2.4 s: network" (xwapp-timing-summary r)))
        (should (equal (mapcar #'car (plist-get r :marks)) '("intent" "room"))))
      (dolist (kind '("Room" "room!" "a-very-long-moment-name-past-24" 7))
        (xwapp--timing-take app (xwapp-test--moment kind (+ base 1) 2.4 nil)
                            (list :app app :via 'proxy :path "intent" :n 9 :page xwapp-test--page-id)))
      (should (= (length xwapp--timings) 1)))))

(defun xwapp-test--slow-next (app page)
  "Ask APP's `next' for PAGE as a reader that does not read until told:
a plist whose :proc is the client, :bytes what it has read, :done t once
the server hung up.  Counted, not kept: an answer may be megabytes."
  (let* ((state (list :bytes 0 :done nil))
         (proc (make-network-process
                :name "xwapp-test-slow" :host "127.0.0.1"
                :service (process-contact xwapp--server :service)
                :coding 'binary :noquery t
                :filter (lambda (_p s) (plist-put state :bytes (+ (plist-get state :bytes) (length s))))
                :sentinel (lambda (_p _e) (plist-put state :done t)))))
    (plist-put state :proc proc)
    (process-send-string proc (format "GET %s HTTP/1.1\r\nHost: 127.0.0.1:%d\r\nContent-Length: 0\r\n\r\n"
                                      (xwapp-test--at app (concat "next?page=" page))
                                      (process-contact xwapp--server :service)))
    (stop-process proc)
    state))

(defun xwapp-test--herd-calls (app n)
  "Send APP's page N calls of 3 MB each: more than a socket holds."
  (let ((chunk (make-string (* 3 1024 1024) ?h)))
    (dotimes (_ n) (xwapp-js app "render" chunk))))

(ert-deftest xwapp-test-browser-a-slow-answer-is-not-a-page-gone ()
  "A page still reading a big answer -- a phone on a slow link -- cannot
ask again, so it is not taken for gone meanwhile: its ten seconds start
once the answer is out.  Emacs is free all the while.  Then, not asking
again, it is gone."
  (skip-unless (fboundp 'make-thread))
  (xwapp-test--with-browser
    (let ((xwapp--gone-after 0.3))
      (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p1") "{}")
      (let ((slow (xwapp-test--slow-next app "p1"))
            (ticks 0))
        (xwapp-test--turn)
        (xwapp-test--herd-calls app 8)
        ;; Emacs keeps running timers while the answer waits on the reader.
        (let ((tick (run-at-time 0 0.05 (lambda () (cl-incf ticks)))))
          (xwapp-test--turn 1.0)
          (cancel-timer tick))
        (should (> ticks 10))
        (should (process-live-p (xwapp--get app :sending)))
        (should (xwapp-live-p app))
        (should (= closed 0))
        ;; The phone reads it all; and asks no more.
        (continue-process (plist-get slow :proc))
        (with-timeout (20 (error "Not read"))
          (while (not (plist-get slow :done)) (accept-process-output nil 0.05)))
        (should (> (plist-get slow :bytes) (* 24 1024 1024)))
        (xwapp-test--turn 0.1)
        (should-not (xwapp--get app :sending))
        (should (xwapp-live-p app))
        (xwapp-test--turn 0.5)
        (should (= closed 1))
        (should-not (xwapp-live-p app))))))

(ert-deftest xwapp-test-browser-an-answer-not-read-is-given-up ()
  "A reader that stops for good -- the phone slept mid-answer, and the
proxy holds its side open -- does not hold the page, nor a thread,
forever: after `xwapp--send-patience' its connection is closed, and the
page, not asking again, is gone."
  (skip-unless (fboundp 'make-thread))
  (xwapp-test--with-browser
    (let ((xwapp--gone-after 0.3)
          (xwapp--send-patience 0.6))
      (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p1") "{}")
      (let ((slow (xwapp-test--slow-next app "p1")))
        (xwapp-test--turn)
        (xwapp-test--herd-calls app 8)
        (xwapp-test--turn 0.4)
        (should (xwapp-live-p app))
        (xwapp-test--turn 1.0)
        (should-not (process-live-p (xwapp--get app :sending)))
        (should-not (seq-some (lambda (th) (equal (thread-name th) "xwapp-send")) (all-threads)))
        (should (= closed 1))
        (should-not (xwapp-live-p app))
        (delete-process (plist-get slow :proc))))))

(ert-deftest xwapp-test-browser-a-page-taken-while-answered ()
  "A page let go while a big answer to it still goes -- another tab
took the app -- has the answer, and its time to ask again no more: the
new page is not let go for it."
  (skip-unless (fboundp 'make-thread))
  (xwapp-test--with-browser
    (let ((xwapp--gone-after 0.3))
      (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p1") "{}")
      (let ((slow (xwapp-test--slow-next app "p1")))
        (xwapp-test--turn)
        (xwapp-test--herd-calls app 8)
        (xwapp-test--turn 0.2)
        (should (equal (car (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p2") "{}")) 204))
        (let ((next (xwapp-test--ask "GET" (xwapp-test--at app "next?page=p2"))))
          (continue-process (plist-get slow :proc))
          (with-timeout (20 (error "Not read"))
            (while (not (plist-get slow :done)) (accept-process-output nil 0.05)))
          (xwapp-test--turn 0.6)
          (should (xwapp--current-p app "p2"))
          (should (= closed 0))
          (should-not (car next))
          (delete-process (cdr next)))))))

(ert-deftest xwapp-test-files-in-the-xwidget ()
  "In the xwidget, a page of file:, the page is told where its files are
on its hello: their directory as a file: URL.  In a browser, which
reaches them by name under its secret, it is told nothing."
  (xwapp-test--serving
    (let* ((title nil) (scripts nil)
           (poll (lambda (intent)
                   (setq title (concat "t:" intent) scripts nil)
                   (xwapp--poll app)
                   (seq-keep (lambda (s) (and (string-match "\\`window\\.XW && XW\\.kit(\\(.*\\));\\'" s)
                                              (xwapp-test--json (match-string 1 s))))
                             scripts))))
      (cl-letf (((symbol-function 'xwapp-session) (lambda (_) 'xw))
                ((symbol-function 'xwidget-webkit-title) (lambda (_) title))
                ((symbol-function 'xwidget-webkit-execute-script)
                 (lambda (_xw s) (push s scripts))))
        (should (equal (funcall poll "{\"op\":\"xwapp-hello\",\"n\":1}")
                       `(((xwapp . "files") (base . ,(concat "file://" dir))))))
        ;; Served anew while the page is open: it is told at once (a
        ;; reload of the app's code); the same again tells it nothing.
        (let ((other (file-name-as-directory (expand-file-name "my files" root)))
              (told (lambda () (seq-keep (lambda (s) (and (string-match "XW\\.kit(\\(.*\\));" s)
                                                         (xwapp-test--json (match-string 1 s))))
                                         scripts))))
          (setq scripts nil)
          (let ((where (lambda () other)))
            (xwapp-serve-files app where "x")
            (should (equal (alist-get 'base (car (funcall told)))
                           (concat "file://" (string-replace " " "%20" other))))
            (setq scripts nil)
            (xwapp-serve-files app where "x")
            (should-not (funcall told)))
          ;; A directory asked each time: a setting changed later counts.
          (should (equal (alist-get 'base (car (funcall poll "{\"op\":\"xwapp-hello\",\"n\":2}")))
                         (concat "file://" (string-replace " " "%20" other)))))
        (should-not intents)))
    (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p1") "{}")
    (should-not (xwapp--get app :outbox))))

;;; Documents: a set served apart, to be read and never run

(defmacro xwapp-test--documents (&rest body)
  "BODY in `xwapp-test--with-browser', its app serving as `docs' the
.html files of two directories, `one' and `two' (`xwapp-serve-files'),
sandboxed, by their exact names, `cap' bytes at most; and beside them
what must not be served."
  (declare (indent 0))
  `(xwapp-test--with-browser
     (let* ((one (file-name-as-directory (expand-file-name "docs" root)))
            (two (file-name-as-directory (expand-file-name "more docs" root)))
            (cap 4096)
            (doc (lambda (dir name text)
                   (make-directory dir t)
                   (with-temp-file (expand-file-name name dir) (insert text)))))
       (funcall doc one "guide.html" "<!doctype html><title>One</title>")
       (funcall doc one "notes.txt" "notes")
       (funcall doc one "big.html" (make-string (1+ cap) ?x))
       (funcall doc two "guide.html" "<title>Two's, shadowed</title>")
       (funcall doc two "other.html" "<title>Two</title><script>evil()</script>")
       ;; Outside both, and a symlink to it in one of them.
       (funcall doc root "secret.html" "<title>Not listed</title>")
       (make-symbolic-link (expand-file-name "secret.html" root) (expand-file-name "link.html" one))
       (make-directory (expand-file-name "dir.html" one))
       (xwapp-serve-files app (lambda () (list one two)) "[A-Za-z0-9][A-Za-z0-9._-]*\\.html"
                          :as "docs" :sandbox t :exact t :max-bytes cap :fresh t)
       ,@body)))

(defun xwapp-test--header (raw name)
  "The value of header NAME in RAW, a whole response, or nil."
  (let ((head (substring raw 0 (or (string-search "\r\n\r\n" raw) (length raw)))))
    (when (string-match (concat "\r\n" (regexp-quote name) ": \\([^\r]*\\)") head)
      (match-string 1 head))))

(ert-deftest xwapp-test-documents-served-sandboxed ()
  "A document of a :sandbox set goes with `xwapp-sandbox-policy' -- no
script, an origin of none, nothing loaded but inline styles, Google's
fonts and data: images -- under the page's secret, fresh each time; the
first directory that has a name serves it.  The policy says exactly that."
  (xwapp-test--documents
    (let ((raw (xwapp-test--raw (xwapp-test--at app ".docs/guide.html"))))
      (should (string-prefix-p "HTTP/1.1 200 OK\r\n" raw))
      (should (string-suffix-p "\r\n\r\n<!doctype html><title>One</title>" raw))
      (should (equal (xwapp-test--header raw "Content-Type") "text/html; charset=utf-8"))
      (should (equal (xwapp-test--header raw "Content-Security-Policy") xwapp-sandbox-policy))
      (should (equal (xwapp-test--header raw "Cache-Control") "no-store"))
      (should (equal (xwapp-test--header raw "X-Content-Type-Options") "nosniff"))
      (should (equal (xwapp-test--header raw "Referrer-Policy") "no-referrer")))
    (let ((raw (xwapp-test--raw (xwapp-test--at app ".docs/other.html"))))
      (should (string-suffix-p "<script>evil()</script>" raw))
      (should (equal (xwapp-test--header raw "Content-Security-Policy") xwapp-sandbox-policy)))
    (let ((policy (mapcar #'string-trim (split-string xwapp-sandbox-policy ";"))))
      ;; sandbox alone: no allow-scripts, no allow-same-origin, nothing allowed.
      (should (member "sandbox" policy))
      (should-not (string-match-p "allow-" xwapp-sandbox-policy))
      (should (member "default-src 'none'" policy))
      (should (member "img-src data:" policy))
      (should (member "form-action 'none'" policy))
      (should-not (string-match-p "'self'\\|script-src\\|connect-src\\|\\*" xwapp-sandbox-policy)))
    ;; The page's own files and its first set go as before: no policy.
    (should-not (xwapp-test--header (xwapp-test--raw (xwapp-test--at app "app/ui/index.html"))
                                    "Content-Security-Policy"))))

(ert-deftest xwapp-test-documents-refused ()
  "Nothing but a listed directory's own document, by its own name: not
another case of it, a name the set does not take (not .html), one past
the cap, a symlink, a directory, a name that climbs out or holds a slash
-- to a file in no listed directory -- a name no file has; not in a set
the app does not serve; not without the secret, under another's, or to
another host; only GET."
  (xwapp-test--documents
    (should (equal (car (xwapp-test--do "GET" (xwapp-test--at app ".docs/other.html"))) 200))
    (dolist (name (list "Guide.html" "GUIDE.HTML" "guide.HTML" "guide.htm"     ; another case, another name
                        "notes.txt" "big.html" "link.html" "dir.html"
                        "secret.html" "..%2Fsecret.html" "%2E%2E/secret.html" "../secret.html"
                        "..%2F..%2Fsecret.html" "guide.html/x" "a%00.html" "~/secret.html"
                        "missing.html" ".html" "-x.html" ".." "."))
      (should (equal (car (xwapp-test--do "GET" (xwapp-test--at app (concat ".docs/" name)))) 404)))
    ;; The same file system's other case, asked for straight.
    (should-not (xwapp--file-of app "Guide.html" "docs"))
    (should (equal (xwapp--file-of app "guide.html" "docs") (expand-file-name "guide.html" one)))
    (should (equal (xwapp--file-of app "other.html" "docs") (expand-file-name "other.html" two)))
    ;; Not a set the app serves, nor its first set by another's name.
    (dolist (path (list ".nope/guide.html" ".files/guide.html" ".DOCS/guide.html" ".docs" ".docs/"))
      (should (equal (car (xwapp-test--do "GET" (xwapp-test--at app path))) 404)))
    (dolist (path (list "/.docs/guide.html"
                        (concat "/" (make-string 32 ?0) "/.docs/guide.html")
                        (xwapp-test--at app "app/.docs/guide.html")))
      (should (equal (car (xwapp-test--do "GET" path)) 404)))
    (should (equal (car (xwapp-test--do "GET" (xwapp-test--at app ".docs/guide.html") nil
                                        '(("Host" . "evil.example"))))
                   404))
    (should (equal (car (xwapp-test--do "POST" (xwapp-test--at app ".docs/guide.html") "x")) 404))
    ;; A set's name is a word: one that could climb is refused outright.
    (should-error (xwapp-serve-files app one "x" :as "../x"))
    (should-error (xwapp-serve-files app one "x" :as "Docs"))))

(ert-deftest xwapp-test-documents-on-other-devices ()
  "Through the proxy a document goes where the page does -- sandboxed
there too -- to one of `xwapp-remote-users', and to no one else: not
without the login, not for another's, and the proxy's headers on
loopback are no proof."
  (xwapp-test--documents
    (let ((xwapp-remote-hosts (list xwapp-test--box))
          (xwapp-remote-users (list "me@example.com"))
          (path (xwapp-test--at app ".docs/guide.html")))
      (xwapp-test--proxy-up)
      (let ((raw (xwapp-test--raw path (xwapp-test--from-box "me@example.com") 'proxy)))
        (should (string-prefix-p "HTTP/1.1 200 OK\r\n" raw))
        (should (equal (xwapp-test--header raw "Content-Security-Policy") xwapp-sandbox-policy)))
      (dolist (h (list (xwapp-test--from-box nil) (xwapp-test--from-box "someone@else.com")
                       (xwapp-test--from-box "me@example.com" "https://evil.example")))
        (should (equal (car (xwapp-test--do "GET" path nil h 'proxy)) 404)))
      (should (equal (car (xwapp-test--do "GET" path nil (xwapp-test--from-box "me@example.com"))) 404)))))

(ert-deftest xwapp-test-documents-in-the-xwidget ()
  "In the xwidget a document is never a file: URL.  The page is told
where its first set's files are, never another's; the kit gives none for
one; and Emacs shows one over loopback, under the secret, so with its
policy -- in the browser when that is where pages go."
  (xwapp-test--documents
    (let ((dir (file-name-as-directory (expand-file-name "files" root)))
          scripts title browsed)
      (make-directory dir t)
      (xwapp-serve-files app dir "x")
      (cl-letf (((symbol-function 'xwapp-session) (lambda (_) 'xw))
                ((symbol-function 'xwidget-webkit-title) (lambda (_) title))
                ((symbol-function 'xwidget-webkit-execute-script)
                 (lambda (_xw s) (push s scripts))))
        (setq title "t:{\"op\":\"xwapp-hello\",\"n\":1}" scripts nil)
        (xwapp--poll app)
        (let ((told (seq-filter (lambda (s) (string-match-p "XW\\.kit({\"xwapp\":\"files\"" s)) scripts)))
          (should (= (length told) 1))
          (should (string-match-p (regexp-quote (concat "file://" dir)) (car told)))
          (should-not (string-match-p "docs" (car told)))))
      (let ((xwapp-browser t)
            (xwapp-browse-function (lambda (url) (push url browsed))))
        (xwapp-show-file app "guide.html" "docs")
        (should (equal browsed (list (format "http://127.0.0.1:%d/%s/.docs/guide.html"
                                             (process-contact xwapp--server :service)
                                             (xwapp--secret app)))))
        (should (equal (xwapp-file-url app "guide.html" "docs") (car browsed)))
        ;; What the set does not serve is shown nowhere.
        (dolist (name '("link.html" "secret.html" "../secret.html" "Guide.html"))
          (should-error (xwapp-show-file app name "docs") :type 'user-error))
        (should (= (length browsed) 1)))
      ;; And it is that address that serves it, sandboxed.
      (let ((raw (xwapp-test--raw (substring (xwapp-file-url app "guide.html" "docs")
                                             (length (format "http://127.0.0.1:%d"
                                                             (process-contact xwapp--server :service)))))))
        (should (equal (xwapp-test--header raw "Content-Security-Policy") xwapp-sandbox-policy)))
      ;; Without xwidgets, an error -- never a file: URL in some browser.
      (unless (featurep 'xwidget-internal)
        (let ((xwapp-browser nil))
          (should-error (xwapp-show-file app "guide.html" "docs") :type 'user-error))))))

;;; How long the page took

(defconst xwapp-test--page-id "0123456789abcdef")

(defmacro xwapp-test--timing (&rest body)
  "BODY in `xwapp-test--with-browser', the page served through the proxy
\(`box', its headers), its timings and the ring of requests afresh, kept
in `root' (`xwapp-timing-file').  Tailscale is played: `looks' counts
the times it is asked; it says `status'.  `told' gathers what
`xwapp-timing-functions' are told."
  (declare (indent 0))
  `(xwapp-test--with-browser
     (let* ((xwapp-remote-hosts (list xwapp-test--box))
            (xwapp-remote-users (list "me@example.com"))
            (xwapp-timing t)
            (xwapp-timing-slow 1.5)
            (xwapp-timing-file (expand-file-name "timings/timings.eld" root))
            (xwapp--timing-save t)
            (xwapp--timing-keep-slow 100)
            (xwapp--timing-keep-normal 20)
            (xwapp--timings nil)
            (xwapp--timings-loaded nil)
            (xwapp--timing-id 0)
            (xwapp--reqs-max 256)
            (xwapp--reqs (make-vector 256 nil))
            (xwapp--reqs-at 0)
            (xwapp--timing-clocks nil)
            (xwapp--tailscale-said nil)
            (xwapp--tailscale-waiting nil)
            (xwapp-tailscale-program "tailscale")
            (looks 0)
            (status nil)
            (told nil)
            (xwapp-timing-functions (list (lambda (_app r) (push r told))))
            (box (xwapp-test--from-box "me@example.com")))
       (ignore looks status told box)
       (xwapp--put app :reopen #'xwapp-test--keep-reopen)
       (xwapp-test--proxy-up)
       (cl-letf (((symbol-function 'xwapp--tailscale-run)
                  (lambda (then) (cl-incf looks) (funcall then status))))
         ,@body))))

(defun xwapp-test--whole (method path &optional body headers transport)
  "The whole response to METHOD PATH, head and body, as bytes."
  (let ((cell (xwapp-test--ask method path body headers transport))
        (out ""))
    (set-process-filter (cdr cell) (lambda (_p s) (setq out (concat out s))))
    (with-timeout (10 (error "No answer"))
      (while (process-live-p (cdr cell))
        (accept-process-output nil 0.02)))
    out))

(defun xwapp-test--kit (app call n &optional more)
  "The path of the kit's CALL (hello, next, intent) numbered N, of the
page `xwapp-test--page-id', under APP's secret; MORE, more of its query."
  (xwapp-test--at app (format "%s?page=%s&n=%d%s" call xwapp-test--page-id n (or more ""))))

(ert-deftest xwapp-test-timing-each-request-is-timed ()
  "Each request Emacs serves is timed, in a ring: when its bytes came,
when its answer began and when its last byte was out; a `next', when
Emacs had calls for it; an intent, its op and its handler.  The kit's
are known by their page and number.  No path is kept with the secret."
  (xwapp-test--timing
    (should (equal (car (xwapp-test--do "POST" (xwapp-test--kit app "hello" 1) "{}" box 'proxy)) 204))
    (should (equal (car (xwapp-test--do "GET" (xwapp-test--at app "app/ui/app.js") nil box 'proxy)) 200))
    (should (equal (car (xwapp-test--do "POST" (xwapp-test--kit app "intent" 2) "{\"op\":\"refresh\"}" box 'proxy)) 204))
    (xwapp-test--turn)
    ;; The greeting waits: this one is answered with it.
    (should (equal (car (xwapp-test--do "GET" (xwapp-test--kit app "next" 3) nil box 'proxy)) 200))
    (let* ((reqs (xwapp--reqs-list))
           (by (lambda (p) (seq-find (lambda (r) (equal (plist-get r :path) p)) reqs)))
           (hello (funcall by "hello")) (file (funcall by "app/ui/app.js"))
           (intent (funcall by "intent")) (next (funcall by "next")))
      (should (= (length reqs) 4))
      (dolist (r reqs)
        (should (eq (plist-get r :app) app))
        (should (eq (plist-get r :via) 'proxy))
        (should (<= (plist-get r :t0) (plist-get r :t1) (plist-get r :t2) (plist-get r :t3)))
        (should-not (string-search (xwapp--secret app) (plist-get r :path))))
      (should (equal (mapcar (lambda (r) (plist-get r :n)) (list hello file intent next)) '(1 nil 2 3)))
      (should (equal (plist-get next :page) xwapp-test--page-id))
      (should (equal (plist-get intent :op) "refresh"))
      (should (equal (alist-get 'op (car intents)) "refresh"))
      (should (<= (plist-get intent :t2) (plist-get intent :th) (plist-get intent :tw)))
      (should (<= (plist-get next :t1) (plist-get next :tf) (plist-get next :t2)))
      (should (= (plist-get file :code) 200))
      (should (> (plist-get file :bytes) 0)))))

(ert-deftest xwapp-test-timing-ring-keeps-the-latest ()
  "The ring keeps the latest `xwapp--reqs-max' requests, the latest first."
  (let ((xwapp--reqs-max 3) (xwapp--reqs (make-vector 3 nil)) (xwapp--reqs-at 0))
    (dotimes (i 5) (xwapp--reqs-note (list :n i)))
    (should (equal (mapcar (lambda (r) (plist-get r :n)) (xwapp--reqs-list)) '(4 3 2)))))

(ert-deftest xwapp-test-timing-hello-says-so-and-a-wake-is-answered-at-once ()
  "The hello says Emacs takes the page's timings, and from when they are
slow, in ms (Xwapp-Timing); nothing when `xwapp-timing' is nil -- as an
Emacs from before says nothing.  A `next' asked with wake=1 is answered
at once, calls or none; one without is held."
  (xwapp-test--timing
    (let ((raw (xwapp-test--whole "POST" (xwapp-test--kit app "hello" 1) "{}" box 'proxy)))
      (should (string-prefix-p "HTTP/1.1 204" raw))
      (should (equal (xwapp-test--header raw "Xwapp-Timing") "1500")))
    (let ((xwapp-timing nil))
      (should-not (xwapp-test--header (xwapp-test--whole "POST" (xwapp-test--kit app "hello" 2) "{}" box 'proxy)
                                      "Xwapp-Timing")))
    ;; The greeting, out of the way.
    (should (equal (car (xwapp-test--do "GET" (xwapp-test--kit app "next" 3) nil box 'proxy)) 200))
    (let ((held (xwapp-test--ask "GET" (xwapp-test--kit app "next" 4) nil box 'proxy)))
      (xwapp-test--turn 0.3)
      (should-not (car held))
      (let ((start (float-time))
            (woke (xwapp-test--do "GET" (xwapp-test--kit app "next" 5 "&wake=1") nil box 'proxy)))
        (should (equal woke '(200 . "[]")))
        (should (< (- (float-time) start) 0.5))
        ;; The one it was asked over, answered too, empty.
        (should (equal (xwapp-test--answer held) '(200 . "[]")))))))

(defun xwapp-test--emacs-side (app path n page t0 &rest more)
  "A request of APP's page PAGE to PATH, numbered N, as Emacs timed it:
read at T0, answered as MORE says (:tf :t2 :t3 ...), or at once."
  (let ((r (apply #'list :app app :via 'proxy :path path :n n :page page
                  :t0 t0 :t1 t0 :gc 0.0 more)))
    (unless (plist-get r :t2) (plist-put r :t2 t0))
    (unless (plist-get r :t3) (plist-put r :t3 (plist-get r :t2)))
    (plist-put r :code 200)
    (xwapp--reqs-note r)
    r))

(defun xwapp-test--moment (kind base d reqs &optional marks)
  "The kit's intent for a moment of KIND begun at BASE (seconds), D long,
of REQS (alists in ms, as the page sends them) and MARKS."
  `((op . "xwapp-timing") (k . ,kind) (at . ,(* 1000 base)) (d . ,(* 1000 d)) (end . "ready")
    (m . ,(or marks '((boot . 300) (hello . 400)))) (r . ,reqs)))

(defun xwapp-test--its (record cat)
  "The seconds RECORD's moment waited on CAT."
  (alist-get cat (plist-get record :sum)))

(defun xwapp-test--a-moment (app base &rest how)
  "Take a moment of APP's page begun at BASE: its hello sent at 0.1 s and
answered at 2.2 s, an intent and a `next' after.  HOW says when Emacs
read the hello (:hello-at, from BASE) and when the intent went and came
\(:intent-there, :intent-back, each way); its own request, that of the
record, has the address HOW's :from.  The record, as kept."
  (let* ((page xwapp-test--page-id)
         (hello-at (or (plist-get how :hello-at) 2.15))
         (there (or (plist-get how :intent-there) 0.05))
         (back (or (plist-get how :intent-back) 0.05))
         (d (or (plist-get how :d) 2.6))
         (own (list :app app :via 'proxy :path "intent" :n 9 :page page
                               :fwd (plist-get how :from))))
    (xwapp-test--emacs-side app "hello" 1 page (+ base hello-at))
    (xwapp-test--emacs-side app "intent" 2 page (+ base 2.25 there))
    (xwapp-test--emacs-side app "next" 3 page (+ base 2.25 there)
                            :tf (+ base 2.25 there 0.1) :t2 (+ base 2.25 there 0.1))
    (xwapp--timing-take
     app (xwapp-test--moment
          "open" base d
          `(((p . "hello") (n . 1) (s . 100) (q . 0) (w . 2100) (dl . 5))
            ((p . "intent") (n . 2) (s . 2250) (q . 0) (w . ,(round (* 1000 (+ there back)))) (dl . 0))
            ((p . "next") (n . 3) (s . 2250) (q . 0) (w . ,(round (* 1000 (+ there back 0.1)))) (dl . 0))))
     own)
    (car xwapp--timings)))

(ert-deftest xwapp-test-timing-emacs-busy-shows-as-emacs-busy ()
  "A request that reached Emacs while it was busy -- read 2 s after it
was sent, its answer back as fast as ever -- is Emacs busy, not the
network: the page's clock and Emacs's are lined up by the quickest
round trip, and the way there past the way back is Emacs's."
  (xwapp-test--timing
    (let* ((base (- (float-time) 10))
           (r (xwapp-test--a-moment app base :hello-at 2.15)))
      (should r)
      (should (plist-get r :slow))
      (should (equal (plist-get r :kind) "open"))
      (should (< (abs (plist-get r :offset)) 0.002))
      (should (< 1.9 (xwapp-test--its r 'busy) 2.1))
      (should (< (xwapp-test--its r 'net) 0.3))
      (should (string-prefix-p "open 2.6 s: Emacs busy 2.0 s" (xwapp-timing-summary r)))
      (let ((hello (seq-find (lambda (q) (equal (plist-get q :p) "hello")) (plist-get r :reqs))))
        (should (< 1.9 (plist-get hello :busy) 2.1)))
      ;; From the proxy, slow: tailscale asked, though it knows no device.
      (should (= looks 1)))))

(ert-deftest xwapp-test-timing-a-busy-wake-by-the-clock-kept ()
  "A wake whose every request waited on a busy Emacs has no quick round
trip to line the clocks up by: the device's of late is used -- kept from
its open -- and its wait is Emacs busy.  With none kept, it cannot be
told from a slow network, and is not said to be Emacs."
  (xwapp-test--timing
    (let* ((base (- (float-time) 20))
           (wake (lambda (at)
                   ;; Its next, sent at AT+0.01, read 2 s late, back at once.
                   (xwapp-test--emacs-side app "next" 7 xwapp-test--page-id (+ at 2.05))
                   (xwapp--timing-take
                    app (xwapp-test--moment "wake" at 2.1
                                            '(((p . "next") (n . 7) (s . 10) (q . 0) (w . 2080) (dl . 0))))
                    (list :app app :via 'proxy :path "intent" :n 8 :page xwapp-test--page-id))
                   (car xwapp--timings))))
      (let ((r (funcall wake base)))
        (should (plist-get r :slow))
        (should (< (xwapp-test--its r 'busy) 0.1))
        (should (equal (plist-get r :clock) "own")))
      (xwapp-test--a-moment app (+ base 5) :hello-at 0.15 :d 0.9)
      (let ((r (funcall wake (+ base 10))))
        (should (equal (plist-get r :clock) "kept"))
        (should (< (abs (plist-get r :offset)) 0.002))
        (should (< 1.9 (xwapp-test--its r 'busy) 2.1))
        (should (string-match-p "clock [-+]0\\.00[0-9] (kept)" (xwapp-timing-describe r)))))))

(ert-deftest xwapp-test-timing-a-slow-network-shows-as-network ()
  "A link slow both ways -- the hello a second there and a second back,
the intent too -- is the network, and Emacs not busy."
  (xwapp-test--timing
    (let* ((base (- (float-time) 10))
           (r (xwapp-test--a-moment app base :hello-at 1.15 :intent-there 1.0 :intent-back 1.0 :d 4.6)))
      (should (plist-get r :slow))
      (should (< (abs (plist-get r :offset)) 0.002))
      (should (< (xwapp-test--its r 'busy) 0.05))
      (should (> (xwapp-test--its r 'net) 3.5))
      (should (string-prefix-p "open 4.6 s: network" (xwapp-timing-summary r))))))

(defun xwapp-test--tailnet (addr relay)
  "Tailscale's status, as its JSON parses, with the phone at ADDR -- \"\"
when not straight -- through RELAY's DERP."
  `((Self (HostName . "the-mac"))
    (Peer (nodekey:aa (DNSName . "iphone171.tail0.ts.net.") (HostName . "localhost") (OS . "iOS")
                      (TailscaleIPs "100.94.115.122" "fd7a:115c:a1e0::1") (CurAddr . ,addr)
                      (Relay . ,relay) (PeerRelay . "") (Active . t))
          (nodekey:bb (DNSName . "desk.tail0.ts.net.") (HostName . "desk") (OS . "macOS")
                      (TailscaleIPs "100.64.0.2") (CurAddr . "") (Relay . "tok") (Active . t)))))

(ert-deftest xwapp-test-timing-a-relay-is-said ()
  "A slow moment from another device says how tailscale reaches it now --
relayed, and through which DERP; straight, and on a LAN -- the device
known by the address the proxy says the request came from.  Tailscale
is asked once in `xwapp--tailscale-fresh' seconds, and not for a moment
that was not slow."
  (xwapp-test--timing
    (let ((base (- (float-time) 10)))
      (setq status (xwapp-test--tailnet "" "hkg"))
      (let ((r (xwapp-test--a-moment app base :hello-at 1.15 :intent-there 1.0 :intent-back 1.0 :d 4.6
                                     :from "100.94.115.122")))
        (should (equal (plist-get r :peer) '(:name "iphone171" :os "ios" :path "relay" :relay "hkg" :lan nil)))
        (should (string-suffix-p " · relay hkg" (xwapp-timing-summary r)))
        (should (string-match-p "iphone171 relay hkg" (xwapp-timing-describe r))))
      ;; Straight now -- but what tailscale said is fresh still.
      (setq status (xwapp-test--tailnet "192.168.1.20:41641" "hkg"))
      (xwapp-test--a-moment app (+ base 5) :hello-at 1.15 :intent-there 1.0 :intent-back 1.0 :d 4.6
                            :from "100.94.115.122")
      (should (= looks 1))
      (setq xwapp--tailscale-said nil)
      (let ((r (xwapp-test--a-moment app (+ base 8) :hello-at 1.15 :intent-there 1.0 :intent-back 1.0 :d 4.6
                                     :from "100.94.115.122")))
        (should (= looks 2))
        (should (equal (plist-get (plist-get r :peer) :path) "direct"))
        (should (string-suffix-p " · direct, LAN" (xwapp-timing-summary r))))
      ;; A quick one: kept, not slow, tailscale not asked.
      (let ((r (xwapp-test--a-moment app (+ base 9) :hello-at 0.15 :d 0.9)))
        (should-not (plist-get r :slow))
        (should-not (plist-get r :peer))
        (should (= looks 2))))))

(ert-deftest xwapp-test-timing-kept-bounded-and-over-a-restart ()
  "The slow records and the others are kept apart, the latest of each up
to their bounds, in `xwapp-timing-file' as well -- yours alone -- and
read back from it after a restart, the next id past theirs."
  (xwapp-test--timing
    (let ((xwapp--timing-keep-slow 2)
          (xwapp--timing-keep-normal 1)
          (base (- (float-time) 100)))
      (dotimes (i 3) (xwapp-test--a-moment app (+ base (* 10 i)) :hello-at 2.15))
      (dotimes (i 2) (xwapp-test--a-moment app (+ base 50 (* 10 i)) :hello-at 0.15 :d 0.9))
      (should (equal (mapcar (lambda (r) (plist-get r :id)) xwapp--timings) '(5 3 2)))
      (should (equal (mapcar (lambda (r) (plist-get r :slow)) xwapp--timings) '(nil t t)))
      (should (= (logand (file-modes xwapp-timing-file) #o077) 0))
      (let ((kept xwapp--timings))
        (setq xwapp--timings nil xwapp--timings-loaded nil xwapp--timing-id 0)
        (xwapp--timings-load)
        (should (equal xwapp--timings kept))
        (should (= xwapp--timing-id 5))))))

(ert-deftest xwapp-test-timing-the-kits-intent-goes-to-the-kit ()
  "The page's timings come as the kit's intent: taken by xwapp, the app
not handed it -- nor, when `xwapp-timing' is nil, anyone."
  (xwapp-test--timing
    (should (equal (car (xwapp-test--do "POST" (xwapp-test--kit app "hello" 1) "{}" box 'proxy)) 204))
    (let ((now (round (* 1000 (float-time)))))
      (should (equal (car (xwapp-test--do "POST" (xwapp-test--kit app "intent" 2)
                                          (json-encode (xwapp-test--moment "wake" (/ (- now 900) 1000.0) 0.4 []))
                                          box 'proxy))
                     204))
      (xwapp-test--turn 0.2)
      (should (= (length told) 1))
      (should (equal (plist-get (car told) :kind) "wake"))
      (should (eq (plist-get (car told) :via) 'proxy))
      ;; The same sent again -- its first sending cut after Emacs had it: kept once.
      (xwapp-test--do "POST" (xwapp-test--kit app "intent" 3)
                      (json-encode (xwapp-test--moment "wake" (/ (- now 900) 1000.0) 0.4 []))
                      box 'proxy)
      (xwapp-test--turn 0.2)
      (should (= (length told) 1))
      (should (= (length xwapp--timings) 1))
      (let ((xwapp-timing nil))
        (xwapp-test--do "POST" (xwapp-test--kit app "intent" 4)
                        (json-encode (xwapp-test--moment "wake" (/ (- now 500) 1000.0) 0.4 []))
                        box 'proxy)
        (xwapp-test--turn 0.2))
      (should (= (length told) 1))
      (should-not intents))))

(ert-deftest xwapp-test-timing-never-keeps-a-secret ()
  "No record -- kept, in the file, told, or said -- holds the page's
secret, whatever the page sends: the secret in a path plain, in upper
case or %-escaped, as a mark's name.  The requests Emacs kept are
without it.  And a record is kept all the same: this fails if a secret
gets through, and if the record is dropped for one."
  (xwapp-test--timing
    (let* ((secret (xwapp--secret app))
           (escaped (mapconcat (lambda (c) (format "%%%02X" c)) secret ""))
           (base (- (float-time) 10)))
      (xwapp-test--do "GET" (xwapp-test--at app "app/ui/app.js") nil box 'proxy)
      (xwapp-test--do "GET" (xwapp-test--at app (concat "app/ui/" secret ".js")) nil box 'proxy)
      (dolist (r (xwapp--reqs-list))
        (should-not (string-search secret (downcase (plist-get r :path)))))
      ;; A load's, and a moment of the app's own -- a room opened -- alike;
      ;; and one whose kind is the secret, not kept at all.
      (dolist (kind (list "open" "room" secret))
        (xwapp--timing-take
         app (xwapp-test--moment
              kind (if (equal kind "open") base (+ base 1)) 2.0
              `(((p . ,(concat "app/ui/" secret ".js")) (s . 10) (q . 0) (w . 50) (dl . 1))
                ((p . ,(upcase secret)) (s . 20) (q . 0) (w . 50) (dl . 1))
                ((p . ,(concat "app/ui/" escaped)) (s . 30) (q . 0) (w . 50) (dl . 1))
                ((p . "app/ui/app.js") (s . 40) (q . 0) (w . 50) (dl . 1)))
              `((,(intern secret) . 5) (,(intern (concat "a" secret)) . 6) (hello . 7)))
         (list :app app :via 'proxy :path "intent" :n 1 :page xwapp-test--page-id)))
      (should (= (length told) 2))
      (should (= (length xwapp--timings) 2))
      (should (equal (mapcar (lambda (r) (plist-get r :kind)) xwapp--timings) '("room" "open")))
      (let ((leaks (lambda (s)
                     (let ((s (downcase (url-unhex-string s))))
                       (or (string-search secret s) (xwapp--holds-secret-p s))))))
        (dolist (r (append told xwapp--timings))
          (let ((print-length nil) (print-level nil))
            (should-not (funcall leaks (prin1-to-string r))))
          (should-not (funcall leaks (xwapp-timing-summary r)))
          (should-not (funcall leaks (xwapp-timing-describe r))))
        (should-not (funcall leaks (with-temp-buffer (insert-file-contents xwapp-timing-file) (buffer-string)))))
      ;; What was not the secret is kept as it was.
      (dolist (r told)
        (should (member "app/ui/app.js" (mapcar (lambda (q) (plist-get q :p)) (plist-get r :reqs))))
        (should (equal (mapcar #'car (plist-get r :marks)) '("hello")))))))

(ert-deftest xwapp-test-timing-a-tap-queued-in-the-page ()
  "An intent slow from when the page queued it -- its turn waited for,
behind another -- is said to have waited queued in the page, for as long
as its mark \"sent\" says; its own round trip, the network's."
  (xwapp-test--timing
    (let ((base (- (float-time) 10)))
      (xwapp-test--emacs-side app "intent" 5 xwapp-test--page-id (+ base 2.025))
      (xwapp--timing-take
       app `((op . "xwapp-timing") (k . "intent") (at . ,(* 1000 base)) (d . 2050) (end . "answered")
             (m . ((sent . 2000)))
             (r . (((p . "intent") (n . 5) (s . 2000) (q . 0) (w . 50) (dl . 0)))))
       (list :app app :via 'proxy :path "intent" :n 6 :page xwapp-test--page-id))
      (let ((r (car xwapp--timings)))
        (should (plist-get r :slow))
        (should (< 1.99 (xwapp-test--its r 'queue) 2.01))
        (should (string-prefix-p "intent 2.0 s: queued in the page 2.0 s · network 0.1 s" (xwapp-timing-summary r)))
        (should (equal (mapcar #'car (xwapp-timing-summary-phases (xwapp-timing-summary r))) '(queue net)))))))

;; Kicking the path

(defconst xwapp-test--phone "100.94.115.122"
  "The phone's tailnet address, as `tailscale serve' forwards it.")

(defun xwapp-test--tailscale (dir pongs &optional code)
  "A tailscale played, in DIR: `ping' says PONGS -- ((SECONDS . LINE)
...), each after waiting so long -- then exits with CODE (0); every run's
arguments noted in DIR/calls.  Its path."
  (let ((script (expand-file-name "tailscale" dir)))
    (with-temp-file (expand-file-name "pongs" dir)
      (dolist (p pongs) (insert (format "%s %s\n" (car p) (cdr p)))))
    (with-temp-file script
      (insert "#!/bin/sh\n"
              (format "echo \"$@\" >> '%s'\n" (expand-file-name "calls" dir))
              (format "while read -r d line; do sleep \"$d\"; echo \"$line\"; done < '%s'\n"
                      (expand-file-name "pongs" dir))
              (format "exit %d\n" (or code 0))))
    (set-file-modes script #o700)
    script))

(defun xwapp-test--pong (path ms)
  (format "pong from iphone171 (%s) via %s in %dms" xwapp-test--phone path ms))

(defun xwapp-test--calls-made (dir)
  (let ((f (expand-file-name "calls" dir)))
    (if (file-exists-p f)
        (split-string (with-temp-buffer (insert-file-contents f) (buffer-string)) "\n" t)
      nil)))

(defmacro xwapp-test--kicking (pongs code &rest body)
  "BODY in `xwapp-test--timing', tailscale played as PONGS and CODE say
\(`xwapp-test--tailscale'), in `kdir'; `phone' the proxy's headers for
a request from the phone, `hello' says its page's hello."
  (declare (indent 2))
  `(xwapp-test--timing
     (let* ((kdir (make-temp-file "xwapp-kick" t))
            (xwapp-tailscale-program (xwapp-test--tailscale kdir ,pongs ,code))
            (xwapp--peers (make-hash-table :test 'equal))
            (xwapp-tailscale-hold 4)
            (xwapp--kick-for 15)
            (phone (append box (list (cons "X-Forwarded-For" xwapp-test--phone))))
            (hello (lambda (&optional page)
                     (xwapp-test--do "POST" (xwapp-test--at app (concat "hello?page=" (or page "p1"))) "{}"
                                     phone 'proxy))))
       (ignore hello)
       (unwind-protect (progn ,@body)
         (when-let* ((p (xwapp--peer-get xwapp-test--phone :proc)))
           (when (process-live-p p) (delete-process p)))
         (delete-directory kdir t)))))

(defun xwapp-test--wait-kick (&optional secs)
  "Wait until the phone's kick is over, SECS (5) at most; its record."
  (with-timeout ((or secs 5) nil)
    (while (process-live-p (xwapp--peer-get xwapp-test--phone :proc))
      (accept-process-output nil 0.02)))
  (xwapp--peer-get xwapp-test--phone :kick))

(ert-deftest xwapp-test-kick-finds-a-direct-path ()
  "The page's hello from a device through the proxy has Emacs ping it
until its path is direct, in the background: the first pong is its path
then, a direct one when it turned so.  Once at a time and once in a
while; never for an address not the tailnet's."
  (xwapp-test--kicking `((0.2 . ,(xwapp-test--pong "DERP(hkg)" 380))
                         (0.4 . ,(xwapp-test--pong "192.168.50.104:41641" 19)))
      0
    (let ((t0 (float-time)))
      (funcall hello)
      ;; Answered at once: Emacs does not wait for the ping's 0.6 s.
      (should (< (- (float-time) t0) 0.5)))
    (should (process-live-p (xwapp--peer-get xwapp-test--phone :proc)))
    (let ((kick (xwapp-test--wait-kick)))
      (should (eq (plist-get kick :how) 'direct))
      (should (equal (car (plist-get kick :first)) "relay hkg"))
      (should (< 0.5 (plist-get kick :direct) 2.5))
      (should (equal (xwapp--peer-path-at xwapp-test--phone (float-time)) "direct"))
      (should (equal (xwapp--peer-path-at xwapp-test--phone (+ (plist-get kick :at) 0.1)) "relay hkg")))
    (should (equal (xwapp-test--calls-made kdir)
                   (list (concat "ping --until-direct --c 15 --timeout 2s " xwapp-test--phone))))
    ;; Again at once -- a reload, a wake: not kicked again so soon.
    (funcall hello "p2")
    (xwapp-test--do "GET" (xwapp-test--at app "next?page=p2&wake=1") nil phone 'proxy)
    (should (= (length (xwapp-test--calls-made kdir)) 1))
    ;; Not a tailnet address, or more than one: nothing runs.
    (let ((xwapp--peers (make-hash-table :test 'equal)))
      (dolist (addr (list "10.0.0.5" (concat xwapp-test--phone " ; touch x") "100.200.1.1" "fd00::1"))
        (xwapp-test--do "POST" (xwapp-test--at app "hello?page=p3") "{}"
                        (append box (list (cons "X-Forwarded-For" addr))) 'proxy))
      (should (zerop (hash-table-count xwapp--peers))))
    (should (= (length (xwapp-test--calls-made kdir)) 1))))

(ert-deftest xwapp-test-kick-that-finds-none ()
  "A kick that finds no direct path says so -- relayed throughout -- and
one that runs past `xwapp--kick-for' is stopped, no pong heard."
  (xwapp-test--kicking `((0.1 . ,(xwapp-test--pong "DERP(hkg)" 380))
                         (0.1 . ,(xwapp-test--pong "DERP(hkg)" 400)))
      1
    (funcall hello)
    (let ((kick (xwapp-test--wait-kick)))
      (should (eq (plist-get kick :how) 'relay))
      (should-not (plist-get kick :direct))
      (should (equal (xwapp--peer-path-at xwapp-test--phone (float-time)) "relay hkg"))))
  (xwapp-test--kicking '((5 . "pong never")) 0
    (let ((xwapp--kick-for 0.4) (t0 (float-time)))
      (funcall hello)
      (let ((kick (xwapp-test--wait-kick)))
        (should (eq (plist-get kick :how) 'none))
        (should (< (- (float-time) t0) 2))))))

(ert-deftest xwapp-test-kick-without-tailscale ()
  "Without tailscale nothing is run, and nothing waits."
  (xwapp-test--kicking nil 0
    (let ((xwapp-tailscale-program nil))
      (funcall hello)
      (should-not (xwapp--peer-get xwapp-test--phone :kick))
      (should-not (xwapp--holding-p app))
      (xwapp-js app "fill" "later" 'room)
      (let ((next (xwapp-test--ask "GET" (xwapp-test--at app "next?page=p1") nil phone 'proxy)))
        ;; xwapp's own greeting aside.
        (should (equal (delq nil (mapcar (lambda (c) (alist-get 'arg c))
                                         (xwapp-test--json (cdr (xwapp-test--answer next)))))
                       '("later")))))))

(ert-deftest xwapp-test-kick-holds-what-can-wait ()
  "While the page's device is kicked, not direct yet, its calls that can
wait wait -- every other goes -- until the path is direct; or
`xwapp-tailscale-hold' at most, when it is not soon."
  (let ((args (lambda (next) (delq nil (mapcar (lambda (c) (alist-get 'arg c)) ; xwapp's greeting aside
                                               (xwapp-test--json (cdr (xwapp-test--answer next))))))))
    (xwapp-test--kicking `((0.2 . ,(xwapp-test--pong "DERP(hkg)" 380))
                           (0.6 . ,(xwapp-test--pong "192.168.50.104:41641" 19)))
        0
      (funcall hello)
      (xwapp-js app "fill" "older" 'room)
      (xwapp-js app "flash" "now")
      (should (xwapp--holding-p app))
      (let ((next (xwapp-test--ask "GET" (xwapp-test--at app "next?page=p1") nil phone 'proxy)))
        (should (equal (funcall args next) '("now"))))
      ;; Held, not answered empty: the page would ask again at once.
      (let* ((t0 (float-time))
             (next (xwapp-test--ask "GET" (xwapp-test--at app "next?page=p1") nil phone 'proxy)))
        (should (equal (funcall args next) '("older")))
        (should (< 0.3 (- (float-time) t0) 1.5))
        (should (plist-get (xwapp--peer-get xwapp-test--phone :kick) :direct))))
    ;; Never direct: they go once the hold is up, the kick still under way.
    (xwapp-test--kicking `((0.1 . ,(xwapp-test--pong "DERP(hkg)" 380)) (3 . ,(xwapp-test--pong "DERP(hkg)" 380))) 1
      (let ((xwapp-tailscale-hold 0.5))
        (funcall hello)
        (xwapp-js app "fill" "older" 'room)
        ;; The greeting goes at once; what can wait waits.
        (should-not (funcall args (xwapp-test--ask "GET" (xwapp-test--at app "next?page=p1") nil phone 'proxy)))
        (let* ((t0 (float-time))
               (next (xwapp-test--ask "GET" (xwapp-test--at app "next?page=p1") nil phone 'proxy)))
          (should (equal (funcall args next) '("older")))
          (should (< 0.4 (- (float-time) t0) 1.5))
          (should (process-live-p (xwapp--peer-get xwapp-test--phone :proc))))))))

(ert-deftest xwapp-test-kick-holds-nothing-by-default ()
  "By default (`xwapp-tailscale-hold' nil), what can wait does not wait
for the path, a kick under way or not."
  (xwapp-test--kicking `((0.1 . ,(xwapp-test--pong "DERP(hkg)" 380)) (2 . ,(xwapp-test--pong "DERP(hkg)" 380))) 1
    (let ((xwapp-tailscale-hold (eval (car (get 'xwapp-tailscale-hold 'standard-value)) t)))
      (should-not xwapp-tailscale-hold)
      (funcall hello)
      (should (process-live-p (xwapp--peer-get xwapp-test--phone :proc)))
      (should-not (xwapp--holding-p app))
      (xwapp-js app "fill" "older" 'room)
      (let* ((t0 (float-time))
             (next (xwapp-test--ask "GET" (xwapp-test--at app "next?page=p1") nil phone 'proxy)))
        (should (member "older" (mapcar (lambda (c) (alist-get 'arg c))
                                        (xwapp-test--json (cdr (xwapp-test--answer next))))))
        (should (< (- (float-time) t0) 0.5))))))

(ert-deftest xwapp-test-timing-says-the-path-and-the-kick ()
  "A moment's record says the path at its start and at its end, and the
kick it fell in -- how soon direct -- in its line and its phases; never
the device's address."
  (xwapp-test--kicking `((0.2 . ,(xwapp-test--pong "DERP(hkg)" 380))
                         (0.3 . ,(xwapp-test--pong "192.168.50.104:41641" 19)))
      0
    (let ((base (float-time)))
      (funcall hello)
      (xwapp-test--wait-kick)
      (xwapp--timing-take
       app (xwapp-test--moment "room" base 2.0
                               '(((p . "next") (n . 7) (s . 300) (q . 0) (w . 150) (dl . 1500))))
       (list :app app :via 'proxy :path "intent" :n 8 :page xwapp-test--page-id :fwd xwapp-test--phone))
      (let* ((r (car xwapp--timings))
             (kick (plist-get r :kick)))
        (should (equal (plist-get r :path0) "relay hkg"))
        (should (equal (plist-get r :path1) "direct"))
        (should (equal (plist-get kick :how) "direct"))
        (should (equal (plist-get kick :first) "relay hkg"))
        (should (< 0.4 (plist-get kick :direct) 1.2))
        (should (string-match-p " · relay hkg → direct · kicked: direct in 0\\.[0-9] s\\'" (xwapp-timing-summary r)))
        (should (string-match-p "^path: relay hkg → direct$" (xwapp-timing-describe r)))
        (should (string-match-p "kicked: direct in 0\\.[0-9] s" (xwapp-timing-describe r)))
        (let ((print-length nil) (print-level nil))
          (should-not (string-search xwapp-test--phone (prin1-to-string r))))))))

(ert-deftest xwapp-test-timing-summary-read-back ()
  "What a summary line says the moment waited on is read back from it,
as written."
  (should (equal (xwapp-timing-summary-phases
                  "slow open 3.4 s: network 2.1 s · Emacs busy 0.6 s · page 0.5 s · relay hkg")
                 '((net "network" 2.1) (busy "Emacs busy" 0.6) (page "page" 0.5))))
  (let ((r (list :kind "wake" :d 2.3 :sum '((net . 0.2) (busy . 0.0) (emacs . 1.8) (prep . 0.2) (page . 0.1)))))
    (should (equal (xwapp-timing-summary r) "wake 2.3 s: Emacs answering 1.8 s · network 0.2 s · Emacs preparing 0.2 s · page 0.1 s"))
    (should (equal (mapcar #'car (xwapp-timing-summary-phases (xwapp-timing-summary r)))
                   '(emacs net prep page)))))

(ert-deftest xwapp-test-timing-tailscale-peer ()
  "The peer a request came from, by its tailnet address; or the one
active peer, when the proxy said none -- not when there are two."
  (let ((status (xwapp-test--tailnet "" "hkg")))
    (should (equal (plist-get (xwapp--tailscale-peer status "100.94.115.122") :name) "iphone171"))
    (should-not (xwapp--tailscale-peer status nil))
    (should-not (xwapp--tailscale-peer status "100.1.1.1"))
    (should (xwapp--tailscale-private-p "192.168.1.20:41641"))
    (should (xwapp--tailscale-private-p "172.20.0.4:41641"))
    (should-not (xwapp--tailscale-private-p "203.0.113.9:41641"))))

(provide 'xwapp-tests)
;;; xwapp-tests.el ends here

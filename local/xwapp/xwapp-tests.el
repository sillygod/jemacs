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
      (should (member "document.title = \"Idle\";" scripts))
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

(ert-deftest xwapp-test-browser-a-big-file-goes-from-a-thread ()
  "A file an app serves past `xwapp--send-aside' is sent from a thread
of its own, whole, so a slow reader cannot hold Emacs; a small one as
before, and the page's own files as before, whatever their size."
  (skip-unless (fboundp 'make-thread))
  (xwapp-test--serving
    (let* ((big (concat (make-string (* 3 1024 1024) ?x) "end"))
           (name (concat (make-string 32 ?c) ".png"))
           (made nil)
           (make (symbol-function 'make-thread)))
      (with-temp-file (expand-file-name name dir) (insert big))
      (cl-letf (((symbol-function 'make-thread)
                 (lambda (fn &optional tname &rest more)
                   (push tname made)
                   (apply make fn tname more))))
        (should (equal (xwapp-test--do "GET" (xwapp-test--at app (concat ".files/" file)))
                       '(200 . "PNGDATA")))
        (should-not made)
        (let ((answer (xwapp-test--do "GET" (xwapp-test--at app (concat ".files/" name)))))
          (should (equal (car answer) 200))
          (should (= (length (cdr answer)) (length big)))
          (should (string-suffix-p "end" (cdr answer))))
        (should (equal made '("xwapp-send")))
        (with-temp-file (expand-file-name "app/ui/big.js" root) (insert big))
        (should (= (length (cdr (xwapp-test--do "GET" (xwapp-test--at app "app/ui/big.js")))) (length big)))
        (should (equal made '("xwapp-send")))))))

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
        (let ((told (seq-filter (lambda (s) (string-match-p "XW\\.kit" s)) scripts)))
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

(provide 'xwapp-tests)
;;; xwapp-tests.el ends here

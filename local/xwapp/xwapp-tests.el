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
`intents' gathers what its handler gets, `closed' counts its on-kill."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "xwapp" t)))
          (xwapp--server nil)
          (xwapp--served (make-hash-table :test 'eq))
          (intents nil)
          (closed 0)
          (app (progn
                 (dolist (f '("app/ui/index.html" "app/ui/app.js" "app/notes.txt"
                              "xwapp/ui/xwapp.js" "other/ui/x.js"))
                   (make-directory (file-name-directory (expand-file-name f root)) t)
                   (with-temp-file (expand-file-name f root) (insert "<" f ">")))
                 (xwapp-test--app :index (expand-file-name "app/ui/index.html" root)
                                  :handler (lambda (i) (push i intents))
                                  :on-kill (lambda () (setq closed (1+ closed)))))))
     (ignore intents closed)
     (unwind-protect (progn (xwapp-url app) ,@body)
       (maphash (lambda (a _) (dolist (k '(:hold :flush :gone)) (xwapp--untimer a k)))
                xwapp--served)
       (when (get-buffer " *xwapp-test*")
         (let ((kill-buffer-hook nil)) (kill-buffer " *xwapp-test*")))
       (when xwapp--server (delete-process xwapp--server))
       (delete-directory root t))))

(defun xwapp-test--ask (method path &optional body headers)
  "Send METHOD PATH with BODY and HEADERS.  Return a cell whose car
becomes (CODE . BODY) once the server closes; its cdr is the client."
  (let* ((cell (list nil))
         (port (process-contact xwapp--server :service))
         (out "")
         (proc (make-network-process
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
                              (cons 0 out)))))))
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

(defun xwapp-test--do (method path &optional body headers)
  (xwapp-test--answer (xwapp-test--ask method path body headers)))

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

(provide 'xwapp-tests)
;;; xwapp-tests.el ends here

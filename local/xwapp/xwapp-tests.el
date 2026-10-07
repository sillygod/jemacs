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

(provide 'xwapp-tests)
;;; xwapp-tests.el ends here

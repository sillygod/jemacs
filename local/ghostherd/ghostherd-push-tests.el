;;; ghostherd-push-tests.el --- Tests for ghostherd-push.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Part of ghostherd's tests; ghostherd-tests.el runs them all.  This
;; file alone:
;;
;;   emacs --batch --init-directory=~/.emacs.d/emacs-home/ \
;;         -L . -L ../xwapp -l ghostherd-push-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ghostherd-test-helpers)

(declare-function ghostherd-memory-page--present-p "ghostherd-memory-page" ())
(declare-function ghostherd-memory-page--room-url "ghostherd-memory-page" (project))
(defvar ghostherd-memory-page--app)

;;; Pushes to your phone

(ert-deftest ghostherd-test-push-only-while-you-are-away ()
  "A blocked agent is pushed -- with its reason, project and room -- only
when the bot is set up, you look at neither it nor the herd page, and
this machine has gone untouched; nil `ghostherd-push-away-after' leaves
the last out, and an idle time that cannot be read counts as away."
  (ghostherd-test--pushing
    (let* ((rd (ghostherd-tests--session :name "rd" :kind 'claude :state 'idle :project "/p/shop/"))
           (project (ghostherd--project-name "/p/shop/"))
           (block (lambda ()
                    (setq ghostherd--herd-pushes nil)
                    (ghostherd--set-state rd 'idle)
                    (ghostherd--set-state rd 'blocked "Allow npm test?")
                    ghostherd--herd-pushes)))
      (should (equal (funcall block)
                     `((:kind "blocked" :session "rd" :project ,project :url ,(concat "room:" project)
                              :images t :reports t :reason "Allow npm test?"))))
      ;; Images off: the bot sends a report's text alone.
      (let ((ghostherd-push-images nil))
        (should (eq (plist-get (car (funcall block)) :images) :json-false)))
      ;; HTML reports off, apart.
      (let ((ghostherd-push-reports nil))
        (let ((push (car (funcall block))))
          (should (eq (plist-get push :reports) :json-false))
          (should (eq (plist-get push :images) t))))
      (setq idle 30)
      (should-not (funcall block))
      (setq idle 600)
      (let ((ghostherd-present-functions (list (lambda () t)))) (should-not (funcall block)))
      (let ((ghostherd-watched-functions (list (lambda (s) (eq s rd))))) (should-not (funcall block)))
      (let ((ghostherd-push '(report))) (should-not (funcall block)))
      (let ((process-environment (cons "GHOSTHERD_TELEGRAM_CHAT_IDS" process-environment)))
        (should-not (funcall block)))
      (let ((process-environment (cons "GHOSTHERD_TELEGRAM_TOKEN" process-environment)))
        (should-not (funcall block)))
      (setq idle 0)
      (let ((ghostherd-push-away-after nil)) (should (funcall block)))
      (setq idle nil)
      (should (funcall block))
      (setq idle 600)
      (let ((ghostherd-room-url-function (lambda (_) (error "no address"))))
        (should (equal (plist-get (car (funcall block)) :url) ""))))))

(ert-deftest ghostherd-test-push-a-leads-report ()
  "A lead done with its turn is pushed as a report, with when that turn
began: its words are what it said after.  Another agent done is not."
  (ghostherd-test--pushing
    (let ((lead (ghostherd-tests--session :name "lead-shop" :kind 'claude :state 'idle :project "/p/shop/"))
          (rd (ghostherd-tests--session :name "rd" :kind 'claude :state 'idle :project "/p/shop/")))
      (puthash "lead-shop" "lead" ghostherd--roles)
      (dolist (s (list lead rd)) (ghostherd--set-state s 'working))
      (let ((began (gethash "lead-shop" ghostherd--working-since)))
        (should (numberp began))
        (dolist (s (list rd lead)) (ghostherd--set-state s 'done))
        (should (equal (mapcar (lambda (p) (list (plist-get p :kind) (plist-get p :session) (plist-get p :since)))
                               ghostherd--herd-pushes)
                       `(("report" "lead-shop" ,began))))))))

(ert-deftest ghostherd-test-push-a-failed-ask ()
  "An agent's ask that fails is pushed once, with whom it asked, why it
failed and what it asked -- also one failed before a tick saw it, but
not those already failed when this Emacs first hears of the asks, nor
one a person asked."
  (ghostherd-test--pushing
    (let ((ghostherd--asks-open (make-hash-table :test 'equal))
          (ghostherd--asks-settled (make-hash-table :test 'equal))
          (ghostherd--herd-asks nil)
          (ghostherd--herd-asks-taken nil)
          (ghostherd--log nil))
      (ghostherd-tests--session :name "lead-shop" :kind 'claude :state 'working :project "/p/shop/")
      (let ((ask (lambda (id status &optional from)
                   (list :id id :from (or from "lead-shop") :to "qa" :status status
                         :error (if (equal status "failed") "error: unrecognized arguments: -" "")
                         :head "Test the login flow"))))
        (ghostherd--herd-take-asks (list (funcall ask "old" "failed")))
        (should-not ghostherd--herd-pushes)
        (ghostherd--herd-take-asks (list (funcall ask "a1" "queued") (funcall ask "old" "failed")))
        (should-not ghostherd--herd-pushes)
        (ghostherd--herd-take-asks (list (funcall ask "a1" "failed") (funcall ask "old" "failed")))
        (should (equal (mapcar (lambda (p) (list (plist-get p :kind) (plist-get p :session) (plist-get p :ask)
                                                 (plist-get p :to) (plist-get p :error) (plist-get p :head)))
                               ghostherd--herd-pushes)
                       '(("ask-failed" "lead-shop" "a1" "qa" "error: unrecognized arguments: -" "Test the login flow"))))
        (ghostherd--herd-take-asks (list (funcall ask "a1" "failed")))
        (should (= (length ghostherd--herd-pushes) 1))
        (ghostherd--herd-take-asks (list (funcall ask "a2" "failed") (funcall ask "a3" "failed" "telegram")))
        (should (equal (mapcar (lambda (p) (plist-get p :ask)) ghostherd--herd-pushes) '("a2" "a1")))))))

(ert-deftest ghostherd-test-herd-tick-carries-pushes ()
  "Pushes go to the sidecar oldest first, in the next tick; a tick that
fails keeps them, ahead of none queued meanwhile."
  (ghostherd-test--pushing
    (let (params fail)
      (cl-letf (((symbol-function 'ghostherd-memory-rpc-url) (lambda () "http://x"))
                ((symbol-function 'ghostherd--herd-snapshot) (lambda () nil))
                ((symbol-function 'ghostherd-memory-request-async)
                 (lambda (_m _ok p err) (setq params p fail err))))
        (let ((ghostherd--herd-tick-inflight nil)
              (ghostherd--herd-replies nil)
              (ghostherd--herd-ack-ids nil)
              (ghostherd--herd-pushes (list '(:kind "report" :session "b") '(:kind "blocked" :session "a"))))
          (ghostherd--herd-tick-async)
          (should (equal (plist-get params :pushes) [(:kind "blocked" :session "a") (:kind "report" :session "b")]))
          (should-not ghostherd--herd-pushes)
          (push '(:kind "blocked" :session "c") ghostherd--herd-pushes)
          (funcall fail "down")
          (should (equal (mapcar (lambda (p) (plist-get p :session)) ghostherd--herd-pushes) '("c" "b" "a"))))))))

;;; A guard's request, told of once

(defmacro ghostherd-test--guard-telling (&rest body)
  "BODY with pushes on (`ghostherd-test--pushing'), guard pushes too, the
log and the guard's state fresh, and `notes' the desktop notifications
\(TITLE BODY), newest first; agy-shop is up in /p/shop/."
  (declare (indent 0))
  `(ghostherd-test--pushing
     (let ((ghostherd-push '(blocked guard))
           (ghostherd-notify-on-guard t)
           (ghostherd--guard-told (make-hash-table :test 'equal))
           (ghostherd--guard-told-ids nil)
           (ghostherd--guard-seen (make-hash-table :test 'equal))
           (ghostherd--guard-requests nil)
           (ghostherd--log nil)
           (notes nil))
       (cl-letf (((symbol-function 'ghostherd--notify) (lambda (title body) (push (list title body) notes))))
         (ghostherd-tests--session :name "agy-shop" :kind 'agy :state 'working :project "/p/shop/")
         ,@body))))

(defun ghostherd-test--guard-request (id status &rest more)
  "A guard request ID of agy-shop's, as a `herd_tick' gives it."
  (append more (list :id id :session "agy-shop" :tool "run_command" :summary "run_command: npm publish"
                     :status status :told :json-false)))

(ert-deftest ghostherd-test-guard-request-told-once ()
  "A new request waiting for the user notifies once -- the agent and the
call -- and is pushed, both said to the sidecar in the next tick; the
same request in later ticks (the same call refused again while it
waits) does not, nor one never approvable, kept denied or expired."
  (ghostherd-test--guard-telling
    (ghostherd--herd-take-guard (list (ghostherd-test--guard-request "g1" "pending")
                                      (ghostherd-test--guard-request "g2" "denied")
                                      (ghostherd-test--guard-request "g3" "declined")
                                      (ghostherd-test--guard-request "g4" "expired")
                                      (ghostherd-test--guard-request "g5" "approved")))
    (should (equal notes '(("ghostherd · agy-shop needs your approval"
                            "run_command: npm publish -- approve it or keep it denied on the herd page"))))
    (let ((project (ghostherd--project-name "/p/shop/")))
      (should (equal ghostherd--herd-pushes
                     `((:kind "guard" :session "agy-shop" :project ,project :url ,(concat "room:" project)
                              :images t :reports t
                              :request "g1" :tool "run_command" :summary "run_command: npm publish")))))
    (should (equal ghostherd--guard-told-ids '("g1")))
    ;; The next tick, and the one after: the same request, refused again.
    (dotimes (_ 2)
      (ghostherd--herd-take-guard (list (ghostherd-test--guard-request "g1" "pending"))))
    (should (= (length notes) 1))
    (should (= (length ghostherd--herd-pushes) 1))
    (should (equal ghostherd--guard-told-ids '("g1")))
    ;; Logged as ever.
    (should (cl-some (lambda (e) (string-match-p "guard needs approval g1: run_command: npm publish"
                                                 (format "%s" e)))
                     ghostherd--log))))

(ert-deftest ghostherd-test-guard-request-not-told-again-after-a-restart ()
  "A restarted Emacs, its own memory gone, tells of none the sidecar says
it told of; a request never told of -- the Emacs gone before its tick
-- it tells of then."
  (ghostherd-test--guard-telling
    (ghostherd--herd-take-guard (list (ghostherd-test--guard-request "g1" "pending" :told t)
                                      (ghostherd-test--guard-request "g2" "pending")))
    (should (equal (mapcar #'car notes) '("ghostherd · agy-shop needs your approval")))
    (should (equal (mapcar (lambda (p) (plist-get p :request)) ghostherd--herd-pushes) '("g2")))
    (should (equal ghostherd--guard-told-ids '("g2")))))

(ert-deftest ghostherd-test-guard-request-told-as-its-switches-say ()
  "`ghostherd-notify-on-guard' nil: no notification, the push still;
`guard' not in `ghostherd-push', or you at the desk: no push, the
notification still.  Told either way -- and of a gone agent, notified.
The call is cut short, on one line."
  (ghostherd-test--guard-telling
    (let ((ghostherd-notify-on-guard nil))
      (ghostherd--herd-take-guard (list (ghostherd-test--guard-request "g1" "pending"))))
    (should-not notes)
    (should (= (length ghostherd--herd-pushes) 1))
    (let ((ghostherd-push '(blocked)))
      (ghostherd--herd-take-guard (list (ghostherd-test--guard-request "g2" "pending"))))
    (setq idle 0)
    (ghostherd--herd-take-guard (list (ghostherd-test--guard-request "g3" "pending")))
    (should (= (length notes) 2))
    (should (= (length ghostherd--herd-pushes) 1))
    (ghostherd--herd-take-guard
     (list (ghostherd-test--guard-request "g4" "pending" :session "agy-gone" :tool "write_to_file"
                                          :summary (concat "/w/a.txt\n" (make-string 300 ?x)))))
    (should (equal (car (car notes)) "ghostherd · agy-gone needs your approval"))
    (should (string-prefix-p "write_to_file: /w/a.txt xxx" (cadr (car notes))))
    (should (string-match-p "… -- approve it" (cadr (car notes))))
    (should (< (length (cadr (car notes))) 170))
    (should (= (length ghostherd--herd-pushes) 1))
    (should (equal ghostherd--guard-told-ids '("g4" "g3" "g2" "g1")))))

(ert-deftest ghostherd-test-herd-tick-carries-the-guard-told ()
  "The requests told of go to the sidecar in the next tick, oldest first;
a tick that fails keeps them, ahead of none told of meanwhile."
  (ghostherd-test--pushing
    (let (params fail)
      (cl-letf (((symbol-function 'ghostherd-memory-rpc-url) (lambda () "http://x"))
                ((symbol-function 'ghostherd--herd-snapshot) (lambda () nil))
                ((symbol-function 'ghostherd-memory-request-async)
                 (lambda (_m _ok p err) (setq params p fail err))))
        (let ((ghostherd--herd-tick-inflight nil)
              (ghostherd--herd-replies nil)
              (ghostherd--herd-ack-ids nil)
              (ghostherd--guard-told-ids (list "g2" "g1")))
          (ghostherd--herd-tick-async)
          (should (equal (plist-get params :guard_told) ["g1" "g2"]))
          (should-not ghostherd--guard-told-ids)
          (push "g3" ghostherd--guard-told-ids)
          (funcall fail "down")
          (should (equal ghostherd--guard-told-ids '("g3" "g2" "g1")))
          (setq ghostherd--herd-tick-inflight nil)
          (ghostherd--herd-tick-async)
          (should (equal (plist-get params :guard_told) ["g1" "g2" "g3"]))
          (setq ghostherd--herd-tick-inflight nil ghostherd--guard-told-ids nil)
          (ghostherd--herd-tick-async)
          (should (equal (plist-get params :guard_told) [])))))))

(ert-deftest ghostherd-test-push-test ()
  "A test push goes whether you are away or not -- not without the bot."
  (ghostherd-test--pushing
    (setq idle 0)
    (let ((ghostherd-present-functions (list (lambda () t))))
      (cl-letf (((symbol-function 'message) #'ignore))
        (ghostherd-push-test)))
    (should (equal ghostherd--herd-pushes '((:kind "test" :session "ghostherd" :project "" :url ""))))
    (let ((process-environment (cons "GHOSTHERD_TELEGRAM_TOKEN" process-environment)))
      (should-error (ghostherd-push-test) :type 'user-error))))

(ert-deftest ghostherd-test-herd-run-command-lead-say ()
  "A reply to a push tells its project's lead -- one there is or one
started for it -- and says whom; a project not there, or nothing to
say, is said back instead."
  (ghostherd-tests--with-herd ()
    (let ((dir (file-name-as-directory (make-temp-file "gh-project" t)))
          got)
      (unwind-protect
          (cl-letf (((symbol-function 'ghostherd-lead-say)
                     (lambda (project text)
                       (setq got (list project text))
                       (ghostherd-tests--session :name "lead-x"))))
            (should (equal (ghostherd--herd-run-command
                            `(:id "c1" :op "lead-say" :session ,dir :args (:project ,dir :body "  go on  ")))
                           '(:id "c1" :text "sent to lead-x")))
            (should (equal got (list dir "go on")))
            (setq got nil)
            (should (string-match-p "No project" (plist-get (ghostherd--herd-run-command
                                                              '(:id "c2" :op "lead-say" :session "x"
                                                                    :args (:project "/no/such/dir/" :body "hi")))
                                                             :text)))
            (should (equal (plist-get (ghostherd--herd-run-command
                                       `(:id "c3" :op "lead-say" :session ,dir :args (:project ,dir :body "  ")))
                                      :text)
                           "Nothing to send"))
            (should-not got))
        (delete-directory dir t)))))

(ert-deftest ghostherd-test-idle-seconds-from-ioreg ()
  "How long this Mac has gone untouched, from ioreg's HIDIdleTime in
nanoseconds; nil without ioreg or its answer."
  (cl-letf (((symbol-function 'executable-find) (lambda (p) (and (equal p "ioreg") "/usr/sbin/ioreg")))
            ((symbol-function 'call-process)
             (lambda (&rest _)
               (insert "+-o IOHIDSystem  <class IOHIDSystem>\n    | |   \"HIDIdleTime\" = 384807920541\n")
               0)))
    (should (equal (ghostherd--idle-seconds) 384.807920541)))
  (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
    (should-not (ghostherd--idle-seconds)))
  (cl-letf (((symbol-function 'executable-find) (lambda (_) "/usr/sbin/ioreg"))
            ((symbol-function 'call-process) (lambda (&rest _) (insert "nothing") 0)))
    (should-not (ghostherd--idle-seconds))))

(ert-deftest ghostherd-test-page-holds-pushes-and-links-rooms ()
  "The herd page in front of you holds pushes back; a push links to a
project's room as its chat, by the page's address that has no secret --
none when the page is this machine's alone."
  (require 'ghostherd-memory-page)
  (should (memq #'ghostherd-memory-page--present-p ghostherd-present-functions))
  (should (eq ghostherd-room-url-function #'ghostherd-memory-page--room-url))
  (cl-letf (((symbol-function 'xwapp-seen-p) (lambda (app) (eq app ghostherd-memory-page--app))))
    (should (ghostherd-memory-page--present-p)))
  (cl-letf (((symbol-function 'xwapp-seen-p) #'ignore))
    (should-not (ghostherd-memory-page--present-p)))
  (let (asked)
    (cl-letf (((symbol-function 'xwapp-remote-open-url)
               (lambda (app fragment) (setq asked (list app fragment)) "http://box/open/ghostherd#…")))
      (let ((xwapp-remote-hosts nil))
        (should-not (ghostherd-memory-page--room-url "~/w/shop/")))
      (should-not asked)
      (let ((xwapp-remote-hosts '("box")))
        (should (equal (ghostherd-memory-page--room-url "~/w/shop/") "http://box/open/ghostherd#…"))
        (should (equal asked (list ghostherd-memory-page--app "room/chat/~%2Fw%2Fshop%2F")))
        ;; No project: the page itself, on its Agents tab.
        (ghostherd-memory-page--room-url nil)
        (should (equal asked (list ghostherd-memory-page--app "agents")))))))

(ert-deftest ghostherd-test-the-bot-is-told-the-pages-addresses ()
  "Each tick tells the sidecar the herd page's address from your phone,
and each agent's room's, for the bot to link to.  Each is asked of the
page once -- asking keeps xwapp's address, which writes a file, and the
tick runs every second -- and again only when the hosts change."
  (ghostherd-tests--with-herd ()
    (let* ((ghostherd--room-urls (make-hash-table :test 'equal))
           (asked nil)
           (ghostherd-room-url-function
            (lambda (p) (push p asked) (if p (concat "room:" p) "page")))
           params
           (tick (lambda ()
                   (let ((ghostherd--herd-tick-inflight nil) (ghostherd--herd-replies nil)
                         (ghostherd--herd-ack-ids nil) (ghostherd--herd-pushes nil))
                     (ghostherd--herd-tick-async))
                   (list (plist-get params :page_url)
                         (sort (mapcar (lambda (r) (list (plist-get r :name) (plist-get r :room)))
                                       (append (plist-get params :sessions) nil))
                               (lambda (a b) (string< (car a) (car b))))))))
      (ghostherd-tests--session :name "rd" :kind 'claude :project "/w/shop/")
      (ghostherd-tests--session :name "qa" :kind 'grok :project "/w/shop/")
      (ghostherd-tests--session :name "sh" :kind 'shell)
      (cl-letf (((symbol-function 'ghostherd-memory-rpc-url) (lambda () "http://x"))
                ((symbol-function 'ghostherd-memory-request-async) (lambda (_m _ok p _err) (setq params p))))
        (dotimes (_ 3)
          (should (equal (funcall tick)
                         '("page" (("qa" "room:/w/shop/") ("rd" "room:/w/shop/") ("sh" ""))))))
        (should (equal (sort (copy-sequence asked) (lambda (a b) (string< (or a "") (or b ""))))
                       '(nil "/w/shop/")))
        ;; Another host: asked again.
        (let ((xwapp-remote-hosts '("elsewhere")))
          (funcall tick))
        (should (= (length asked) 4))
        ;; A push's room is the same address, not asked again.
        (should (equal (ghostherd--room-url "/w/shop/") "room:/w/shop/"))
        (should (= (length asked) 4))
        ;; Nothing to link to: empty, on the page and each room.
        (let ((ghostherd-room-url-function nil))
          (should (equal (funcall tick) '("" (("qa" "") ("rd" "") ("sh" ""))))))
        (let ((ghostherd-room-url-function (lambda (_) (error "No address"))))
          (should (equal (car (funcall tick)) "")))))))

(ert-deftest ghostherd-test-the-herd-page-is-there-for-your-phone ()
  "With remote hosts, the herd page loads with the mode -- its address
without the secret wants it there -- and not otherwise; failing to load
does not stop the mode."
  (let (required)
    (cl-letf (((symbol-function 'require) (lambda (f &rest _) (push f required) t)))
      (let ((xwapp-remote-hosts nil))
        (ghostherd--herd-page-for-phone))
      (should-not required)
      (let ((xwapp-remote-hosts '("box")))
        (ghostherd--herd-page-for-phone))
      (should (equal required '(ghostherd-memory-page))))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) (error "Broken")))
              ((symbol-function 'message) #'ignore))
      (let ((xwapp-remote-hosts '("box")))
        (should-not (ghostherd--herd-page-for-phone))))
    (should (string-match-p "(ghostherd--herd-page-for-phone)"
                            (with-temp-buffer
                              (insert-file-contents (locate-library "ghostherd.el" t))
                              (buffer-substring (search-forward "(define-minor-mode ghostherd-mode")
                                                (search-forward "(ghostherd-memory-auto-import-start)")))))))

(provide 'ghostherd-push-tests)
;;; ghostherd-push-tests.el ends here

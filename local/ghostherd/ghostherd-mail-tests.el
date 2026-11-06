;;; ghostherd-mail-tests.el --- Tests for ghostherd-mail.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Part of ghostherd's tests; ghostherd-tests.el runs them all.  This
;; file alone:
;;
;;   emacs --batch --init-directory=~/.emacs.d/emacs-home/ \
;;         -L . -L ../xwapp -l ghostherd-mail-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ghostherd-test-helpers)

(ert-deftest ghostherd-test-herd-snapshot-json-is-an-array ()
  "json.el treats a list of plists as one alist.  The sidecar wants
an array of objects, so the payload must go out as a vector."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--session :name "a" :kind 'agy :state 'idle)
    (let* ((json-object-type 'plist)
           (json-array-type 'list)
           (json-key-type 'keyword)
           (payload (json-encode
                     (list :sessions (vconcat (ghostherd--herd-snapshot))
                           :ack_ids [])))
           (parsed (json-read-from-string payload))
           (sessions (plist-get parsed :sessions)))
      (should (listp sessions))
      (should (equal (plist-get (car sessions) :name) "a"))
      (should (equal (plist-get (car sessions) :kind) "agy")))))

(ert-deftest ghostherd-test-herd-snapshot-includes-screen ()
  "Telegram screen reads this field off sqlite, so the last capture
has to ride with the snapshot rather than wait for a command round trip."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy :state 'idle)))
      (puthash (ghostherd-session-id s) "hello pane\n" ghostherd--screens)
      (let ((row (car (ghostherd--herd-snapshot))))
        (should (equal (plist-get row :screen) "hello pane\n"))))))

(ert-deftest ghostherd-test-herd-deliver-uses-message ()
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--session :name "grok-dev")
    (let ((got nil))
      (cl-letf (((symbol-function 'ghostherd-message)
                 (lambda (from to body &rest keys)
                   (setq got (list from to body keys)))))
        (ghostherd--herd-deliver-one
         '(:id "1" :from "claude-research" :to "grok-dev"
               :body "hi" :handoff nil :submit t)))
      (should (equal (nth 0 got) "claude-research"))
      (should (equal (nth 2 got) "hi"))
      (should (eq (plist-get (nth 3 got) :submit) t)))))

(ert-deftest ghostherd-test-poll-tick-does-not-http ()
  "Typing must not compete with sidecar `url-retrieve'.  Mail/Telegram
ticks live on the idle timer instead."
  (let ((http nil))
    (cl-letf (((symbol-function 'ghostherd--herd-tick-async)
               (lambda () (setq http t)))
              ((symbol-function 'ghostherd-poll-all) #'ignore)
              ((symbol-function 'ghostherd--sidebar-on-screen-p)
               (lambda () nil)))
      (ghostherd--poll-tick)
      (should-not http))))

(ert-deftest ghostherd-test-herd-run-command-interrupt ()
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--session :name "a")
    (let ((got nil))
      ;; What was done, as `ghostherd-interrupt' says it: Esc, or a grok's
      ;; Ctrl+C -- or why nothing was.
      (cl-letf (((symbol-function 'ghostherd-interrupt)
                 (lambda (s) (setq got s) (format "Esc to %s" s))))
        (let ((out (ghostherd--herd-run-command
                    '(:id "1" :op "interrupt" :session "a"))))
          (should (equal got "a"))
          (should (equal (plist-get out :id) "1"))
          (should (equal (plist-get out :text) "Esc to a")))))))

(ert-deftest ghostherd-test-a-cancelled-ask-interrupts-its-taker ()
  "`herd cancel' of an ask its taker is at work on: the sidecar's
`interrupt' command, the ask and its asker with it, reaches the taker
through the tick's commands, and what was done goes back as its reply."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--session :name "rd")
    (let ((got nil) (ghostherd--herd-replies nil))
      (cl-letf (((symbol-function 'ghostherd-interrupt)
                 (lambda (s) (push s got) (format "Ctrl+C to %s: stops its turn" s))))
        (ghostherd--herd-run-commands
         '(:commands ((:id "c1" :op "interrupt" :session "rd"
                       :args (:ask "1a2b3c4d" :by "lead-x") :chat_id nil))))
        (should (equal got '("rd")))
        (should (equal ghostherd--herd-replies '((:id "c1" :text "Ctrl+C to rd: stops its turn"))))))))

(ert-deftest ghostherd-test-herd-run-command-answer ()
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--session :name "a")
    (let ((got nil))
      (cl-letf (((symbol-function 'ghostherd-answer)
                 (lambda (s n) (setq got (list s n)))))
        (ghostherd--herd-run-command
         '(:id "2" :op "answer" :session "a" :args (:n 3)))
        (should (equal got '("a" 3)))))))

(ert-deftest ghostherd-test-herd-run-command-unknown-is-text ()
  (let ((out (ghostherd--herd-run-command
              '(:id "x" :op "nope" :session "a"))))
    (should (string-match-p "unknown op" (plist-get out :text)))))

(ert-deftest ghostherd-test-herd-deliver-handoff ()
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--session :name "grok-dev")
    (let ((got nil))
      (cl-letf (((symbol-function 'ghostherd-handoff)
                 (lambda (to text &optional from &rest _)
                   (setq got (list to text from)))))
        (ghostherd--herd-deliver-one
         '(:id "1" :from "claude-research" :to "grok-dev"
               :body "hi" :handoff t)))
      (should (equal got '("grok-dev" "hi" "claude-research"))))))

;;; Asks

(ert-deftest ghostherd-test-ask-deliver-says-how-to-answer ()
  "The taker needs no skill: the paste carries the exact command, the
heredoc's closing line at column 0 so a copy of it works."
  (ghostherd-tests--with-asks
    (let ((taker (ghostherd-tests--session :name "agy-a" :kind 'agy :state 'idle))
          got)
      (cl-letf (((symbol-function 'ghostherd-message)
                 (lambda (from to body &rest keys) (setq got (list from to body keys)))))
        (ghostherd--herd-deliver-one
         '(:id "m1" :from "claude-main" :to "agy-a" :body "fetch the docs\nall of them"
               :handoff nil :submit t :ask "a1b2c3d4" :rounds 4)))
      (should (equal (nth 0 got) "claude-main"))
      (should (eq (nth 1 got) taker))
      (let ((text (nth 2 got)))
        (should (string-prefix-p "[ask a1b2c3d4 " text))
        (should (string-search "\nfetch the docs\nall of them\n" text))
        (should (string-search "\n/p/bin/herd reply a1b2c3d4 <<'HERD'\n<your answer>\nHERD\n" text))
        ;; How many rounds it has, after what the room shows of an ask.
        (should (string-search ",\nand any one of them 4 times at most.\n" text))
        ;; Then, last, what to do with what another project needs.
        (should (string-suffix-p "and whoever asked takes it on." text))
        (should (< (string-search "When you are done" text)
                   (string-search "any one of them" text))))
      (should-not (string-search "any one of them" (ghostherd--ask-text "a1" "x")))
      (should (eq (plist-get (nth 3 got) :submit) t))
      (should (equal (gethash "a1b2c3d4" ghostherd--asks-open) "agy-a"))
      (let ((entry (car ghostherd--log)))
        (should (eq (ghostherd-log-entry-kind entry) 'ask))
        (should (equal (ghostherd-log-entry-session entry) "agy-a"))
        (should (equal (ghostherd-log-entry-text entry)
                       "ask a1b2c3d4 from claude-main: fetch the docs"))))))

(ert-deftest ghostherd-test-ask-settles-when-the-taker-stops ()
  "Idle after the paste closes the ask with the screen, once.  Working
and blocked do not: the taker is still at it."
  (ghostherd-tests--with-asks
    (let ((taker (ghostherd-tests--session :name "agy-a" :kind 'agy :state 'idle
                                           :backend 'fake)))
      (setq ghostherd-tests--fake-screen "the page says X\n\n")
      (puthash "a1" "agy-a" ghostherd--asks-open)
      (ghostherd--set-state taker 'working)
      (ghostherd--set-state taker 'blocked)
      (should-not requests)
      (ghostherd--set-state taker 'idle)
      (should (equal requests
                     '(("herd_settle" (:id "a1" :screen "the page says X" :dead :json-false)))))
      (should-not (gethash "a1" ghostherd--asks-open))
      (ghostherd--set-state taker 'working)
      (ghostherd--set-state taker 'done)
      (should (= (length requests) 1)))))

(ert-deftest ghostherd-test-an-ask-stuck-in-the-input-box-stays-open ()
  "The Return that submits an ask can be lost: the taker goes idle with
the ask in its input box, never having read it.  That screen is no
answer, so the ask stays open -- until the box is empty."
  (ghostherd-tests--with-asks
    (let ((taker (ghostherd-tests--session :name "qa" :kind 'claude :state 'working
                                           :backend 'fake)))
      (setq ghostherd-tests--fake-screen (ghostherd-test--claude-screen "❯ [Pasted text #3 +22 lines]"))
      (puthash "a1" "qa" ghostherd--asks-open)
      (ghostherd--set-state taker 'done)
      (should-not requests)
      (should (gethash "a1" ghostherd--asks-open))
      (should (string-match-p "a1 is still in its input box"
                              (ghostherd-log-entry-text (car ghostherd--log))))
      (ghostherd--set-state taker 'working)
      (setq ghostherd-tests--fake-screen (ghostherd-test--claude-screen "❯ "))
      (ghostherd--set-state taker 'idle)
      (should (equal (mapcar #'car requests) '("herd_settle")))
      (should-not (gethash "a1" ghostherd--asks-open)))))

(ert-deftest ghostherd-test-an-ask-never-submitted-fails-with-why ()
  "Return pressed again and again, and the ask still in the input box:
fail it, saying so, so the asker hears now instead of waiting on an
agent that never read it.  Other text, or tries left, fails nothing."
  (ghostherd-tests--with-asks
    (let* ((taker (ghostherd-tests--session :name "qa" :kind 'claude :state 'idle :backend 'fake))
           (ask (concat (format ghostherd-message-template "rd" "qa" "[ask a1 -- whoever asked is waiting]\nTest it.")))
           (ghostherd-tests--fake-screen (ghostherd-test--claude-screen "❯ [Pasted text #2 +20 lines]")))
      (puthash "a1" "qa" ghostherd--asks-open)
      (cl-letf (((symbol-function 'ghostherd--host-send-keys) #'ignore)
                ((symbol-function 'run-at-time) #'ignore))
        (ghostherd--confirm-submit taker ask 1)
        (ghostherd--confirm-submit taker "[Pasted text, not an ask]" 0)
        (should-not requests)
        (ghostherd--confirm-submit taker ask 0))
      (should (equal requests
                     '(("herd_settle" (:id "a1" :screen "" :dead :json-false
                                       :error "not submitted: it sat in qa's input box")))))
      (should-not (gethash "a1" ghostherd--asks-open)))))

(ert-deftest ghostherd-test-a-taker-waiting-on-its-own-ask-is-watched-again ()
  "rd ended its turn to wait on qa: the sidecar leaves the lead's ask
open, and Emacs watches it again for the next time rd stops."
  (ghostherd-tests--with-asks
    (let ((taker (ghostherd-tests--session :name "rd" :kind 'claude :state 'working :backend 'fake)))
      (setq ghostherd-tests--fake-screen "handed it to qa\n")
      (puthash "a1" "rd" ghostherd--asks-open)
      (cl-letf (((symbol-function 'ghostherd-memory-request-async)
                 (lambda (method cb &optional params _err)
                   (setq requests (append requests (list (list method params))))
                   (funcall cb '(:id "a1" :status "delivered" :waiting_on ("b2"))))))
        (ghostherd--set-state taker 'done))
      (should (equal (mapcar #'car requests) '("herd_settle")))
      (should (equal (gethash "a1" ghostherd--asks-open) "rd"))
      (should-not (gethash "a1" ghostherd--asks-settled))
      (should (string-match-p "a1 left open: rd waits on b2"
                              (ghostherd-log-entry-text (car ghostherd--log))))
      ;; Waiting on its own asks settles by itself: no issue.
      (should-not (ghostherd-tests--issue-p "a1 left open")))))

(ert-deftest ghostherd-test-ask-fails-when-the-taker-dies ()
  (ghostherd-tests--with-asks
    (let ((a (ghostherd-tests--session :name "agy-a" :kind 'agy :state 'working))
          (b (ghostherd-tests--session :name "agy-b" :kind 'agy :state 'working)))
      (puthash "a1" "agy-a" ghostherd--asks-open)
      (puthash "b1" "agy-b" ghostherd--asks-open)
      (ghostherd--set-state a 'dead)
      (run-hook-with-args 'ghostherd-session-removed-hook b)
      (should (equal requests
                     '(("herd_settle" (:id "a1" :screen "" :dead t))
                       ("herd_settle" (:id "b1" :screen "" :dead t))))))))

(ert-deftest ghostherd-test-ask-tick-logs-what-closed ()
  (ghostherd-tests--with-asks
    (ghostherd-tests--session :name "agy-a" :kind 'agy :state 'working)
    (puthash "a1" "agy-a" ghostherd--asks-open)
    (ghostherd--herd-take-asks
     '((:id "a1" :from "claude-main" :to "agy-a" :status "delivered")
       (:id "a2" :from "claude-main" :to "agy-a" :status "queued")))
    (should-not ghostherd--log)
    (ghostherd--herd-take-asks
     '((:id "a1" :from "claude-main" :to "agy-a" :status "answered"
            :auto nil :reply_head "the docs say X")
       (:id "a2" :from "claude-main" :to "agy-a" :status "failed" :error "agy-a is gone")))
    (should (equal (mapcar #'ghostherd-log-entry-text (reverse ghostherd--log))
                   '("answered ask a1 for claude-main: the docs say X"
                     "ask a2 from claude-main failed: agy-a is gone")))
    (should (ghostherd-tests--issue-p "a2 from claude-main failed"))
    (should-not (ghostherd-tests--issue-p "answered ask a1"))
    (should-not (gethash "a1" ghostherd--asks-open))
    (should (= (length ghostherd--herd-asks) 2))))

(ert-deftest ghostherd-test-ask-tick-adopts-asks-from-before ()
  "An ask delivered by an earlier Emacs: watched from now, and closed
at once when its taker already sits idle."
  (ghostherd-tests--with-asks
    (ghostherd-tests--session :name "agy-busy" :kind 'agy :state 'working)
    (ghostherd-tests--session :name "agy-idle" :kind 'agy :state 'idle :backend 'fake)
    (setq ghostherd-tests--fake-screen "done it")
    (puthash "old" t ghostherd--asks-settled)
    (ghostherd--herd-take-asks
     '((:id "b1" :from "c" :to "agy-busy" :status "delivered")
       (:id "i1" :from "c" :to "agy-idle" :status "delivered")
       (:id "old" :from "c" :to "agy-idle" :status "delivered")))
    (should (equal (gethash "b1" ghostherd--asks-open) "agy-busy"))
    (should (equal requests '(("herd_settle" (:id "i1" :screen "done it" :dead :json-false)))))))

(ert-deftest ghostherd-test-agent-environment-gives-the-herd-client ()
  (cl-letf (((symbol-function 'ghostherd-herd-client) (lambda () "/p/bin/herd"))
            ((symbol-function 'ghostherd-memory--mail-directory) (lambda () "/m/ghostherd-mail")))
    (let ((env (ghostherd-agent-environment 'tmux '(:name "a"))))
      (should (member "GHOSTHERD_HERD=/p/bin/herd" env))
      (should (member "GHOSTHERD_RPC_FILE=/m/ghostherd-mail/rpc.url" env))))
  (cl-letf (((symbol-function 'ghostherd-herd-client) #'ignore))
    (should-not (cl-find "GHOSTHERD_HERD=" (ghostherd-agent-environment 'tmux '(:name "a"))
                         :test (lambda (pre s) (string-prefix-p pre s))))))

(ert-deftest ghostherd-test-herd-client-is-the-checkouts ()
  (let ((client (ghostherd-herd-client)))
    (should client)
    (should (file-name-absolute-p client))
    (should (string-suffix-p "/bin/herd" client))
    (should (file-executable-p client))))

;;; The sidecar tick

(ert-deftest ghostherd-test-herd-tick-keeps-coming-while-emacs-is-idle ()
  "Mail and reports come on the tick.  It waits while you type, and goes
on however long Emacs is left alone: an idle timer fired once per idle
spell, and an ask answered in an empty room reached its asker only when
someone came back 36 minutes later."
  (let ((ghostherd--herd-tick-timer nil)
        (ticks 0)
        idle)
    (cl-letf (((symbol-function 'ghostherd--herd-tick-async) (lambda () (setq ticks (1+ ticks))))
              ((symbol-function 'current-idle-time) (lambda () (and idle (seconds-to-time idle)))))
      (dolist (i '(nil 0.2 2 2400))
        (setq idle i)
        (ghostherd--herd-tick-when-idle))
      (should (= ticks 2))
      (unwind-protect
          (progn
            (ghostherd--ensure-herd-tick-timer)
            (should-not (timer--idle-delay ghostherd--herd-tick-timer))
            (should (equal (timer--repeat-delay ghostherd--herd-tick-timer) ghostherd-herd-tick-idle)))
        (ghostherd--stop-herd-tick-timer)))))

(provide 'ghostherd-mail-tests)
;;; ghostherd-mail-tests.el ends here

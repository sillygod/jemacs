;;; ghostherd-comm-tests.el --- Tests for ghostherd-comm.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Part of ghostherd's tests; ghostherd-tests.el runs them all.  This
;; file alone:
;;
;;   emacs --batch --init-directory=~/.emacs.d/emacs-home/ \
;;         -L . -L ../xwapp -l ghostherd-comm-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ghostherd-test-helpers)

(declare-function ghostherd-inbox--agent-items "ghostherd-memory-page" (a))
(declare-function ghostherd-memory-page--agent "ghostherd-memory-page" (s))

;;; Control keys

(ert-deftest ghostherd-test-send-keys-resolves-aliases ()
  "Callers say \"esc\" or \"C-c\"; ghostel's encoder wants a name and a
modifier string."
  (ghostherd-tests--with-herd ()
    (let* ((s (ghostherd-tests--session :name "a"))
           (sent (ghostherd-tests--recording-keys
                   (ghostherd-send-keys s "esc" "C-c" "down" "return"))))
      (should (equal sent '(("escape" . nil)
                            ("c" . "ctrl")
                            ("down" . nil)
                            ("return" . nil)))))))

(ert-deftest ghostherd-test-send-keys-passes-unknown-through ()
  "Unlisted names reach ghostel unchanged, so its full vocabulary stays
reachable without ghostherd having to mirror it."
  (ghostherd-tests--with-herd ()
    (let* ((s (ghostherd-tests--session :name "a"))
           (sent (ghostherd-tests--recording-keys
                   (ghostherd-send-keys s "f5"))))
      (should (equal sent '(("f5" . nil)))))))

(ert-deftest ghostherd-test-send-keys-does-not-claim-working ()
  "Sending Escape means stop.  Marking the session `working' -- which is
what `ghostherd-send' does -- would be exactly backwards."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :state 'blocked)))
      (ignore (ghostherd-tests--recording-keys (ghostherd-send-keys s "esc")))
      (should (eq (ghostherd-session-state s) 'blocked)))))

(ert-deftest ghostherd-test-send-vs-send-keys-state ()
  "The contrast is the point: text input does imply work started."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :state 'idle)))
      (ignore (ghostherd-tests--recording-keys (ghostherd-send s "hello" t)))
      (should (eq (ghostherd-session-state s) 'working)))))

(ert-deftest ghostherd-test-interrupt-and-abort ()
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a")))
      (should (equal (ghostherd-tests--recording-keys (ghostherd-interrupt s))
                     '(("escape" . nil))))
      (should (equal (ghostherd-tests--recording-keys (ghostherd-abort s))
                     '(("c" . "ctrl")))))))

(ert-deftest ghostherd-test-send-keys-requires-live-buffer ()
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a")))
      (kill-buffer (ghostherd-session-buffer s))
      (should-error (ghostherd-send-keys s "esc") :type 'user-error))))

;;; Waiting on output

(ert-deftest ghostherd-test-output-matches-returns-match ()
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a")))
      (with-current-buffer (ghostherd-session-buffer s)
        (insert "running\n42 tests, 0 failures\n"))
      (should (equal (ghostherd-output-matches s "[0-9]+ failures")
                     "0 failures"))
      (should-not (ghostherd-output-matches s "no such text")))))

(ert-deftest ghostherd-test-output-matches-is-case-sensitive ()
  "A regexp for a prompt should mean what it says."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a")))
      (with-current-buffer (ghostherd-session-buffer s) (insert "ERROR: bad\n"))
      (should (ghostherd-output-matches s "ERROR"))
      (should-not (ghostherd-output-matches s "error"))
      ;; ...even when the caller's environment says otherwise
      (let ((case-fold-search t))
        (should-not (ghostherd-output-matches s "error"))))))

(ert-deftest ghostherd-test-output-matches-honours-lines ()
  "Only the tail is searched, so old output stops matching."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a")))
      (with-current-buffer (ghostherd-session-buffer s)
        (insert "needle\n")
        (insert (make-string 50 ?\n)))
      (should-not (ghostherd-output-matches s "needle" 5))
      (should (ghostherd-output-matches s "needle" 100)))))

(ert-deftest ghostherd-test-output-matches-dead-buffer ()
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a")))
      (with-current-buffer (ghostherd-session-buffer s) (insert "x\n"))
      (kill-buffer (ghostherd-session-buffer s))
      (should-not (ghostherd-output-matches s "x")))))

(ert-deftest ghostherd-test-wait-output-returns-without-sleeping ()
  "Output can arrive between whatever triggered the wait and the wait
itself; a first check one interval late would miss it."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a"))
          (slept nil))
      (with-current-buffer (ghostherd-session-buffer s) (insert "done\n"))
      (cl-letf (((symbol-function 'sit-for)
                 (lambda (&rest _) (setq slept t))))
        (should (equal (ghostherd-wait-output s "done" 10) "done")))
      (should-not slept))))

(ert-deftest ghostherd-test-wait-output-timeout-signals ()
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a"))
          (ghostherd-wait-poll-interval 0))
      (should (string-match-p
               "Timeout waiting"
               (cadr (should-error (ghostherd-wait-output s "never" 0.05)
                                   :type 'user-error)))))))

(ert-deftest ghostherd-test-wait-output-noerror-returns-nil ()
  "Branching on \"did it happen\" should not require `condition-case'."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a"))
          (ghostherd-wait-poll-interval 0))
      (should-not (ghostherd-wait-output s "never" 0.05 t)))))

(ert-deftest ghostherd-test-wait-output-gives-up-on-dead-session ()
  "A dead agent will never produce the text; waiting out the full
timeout for it is just a hang."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a"))
          (ghostherd-wait-poll-interval 0)
          (slept 0))
      (kill-buffer (ghostherd-session-buffer s))
      (cl-letf (((symbol-function 'sit-for)
                 (lambda (&rest _) (setq slept (1+ slept)))))
        (should-not (ghostherd-wait-output s "never" 600 t)))
      (should (= slept 0)))))

;;; Handoff pipeline

(defmacro ghostherd-tests--with-fake-terminal (&rest body)
  "Run BODY with ghostel send functions and notification stubbed out."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'ghostel-send-key) (lambda (&rest _) nil))
             ((symbol-function 'ghostel-paste-string) (lambda (&rest _) nil))
             ((symbol-function 'derived-mode-p) (lambda (&rest _) t))
             ((symbol-function 'ghostherd--notify) (lambda (&rest _) nil)))
     ,@body))

(defun ghostherd-tests--tick (session &rest args)
  "Run one handoff tick for SESSION with ARGS (deadline grace callback)."
  (apply #'ghostherd--handoff-tick (ghostherd-session-id session) args))

(ert-deftest ghostherd-test-handoff-does-not-block ()
  "The point of the pipeline is to keep working while the other agent
runs, so the handoff itself must never call `sit-for'."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "reviewer" :kind 'agy))
          (slept nil))
      (ghostherd-tests--with-fake-terminal
        (cl-letf (((symbol-function 'sit-for)
                   (lambda (&rest _) (setq slept t))))
          (ghostherd-handoff s "please review")))
      (should-not slept)
      (should (gethash "reviewer" ghostherd--handoff-watches))
      (ghostherd--handoff-cancel "reviewer"))))

(ert-deftest ghostherd-test-handoff-grace-suppresses-early-settle ()
  "An agent does not start the instant it is handed something; until it
prints, the screen rules still see the idle prompt it had before."
  (ghostherd-tests--with-herd ()
    (let* ((s (ghostherd-tests--session :name "a" :kind 'agy :state 'idle))
           (calls nil)
           (callback (lambda (_s state) (push state calls))))
      (with-current-buffer (ghostherd-session-buffer s) (insert "> \n"))
      ;; still inside the grace window
      (ghostherd-tests--tick s nil (+ (float-time) 100) callback)
      (should-not calls)
      ;; grace elapsed
      (ghostherd-tests--tick s nil (- (float-time) 1) callback)
      (should (equal calls '(idle))))))

(ert-deftest ghostherd-test-handoff-reports-timeout ()
  (ghostherd-tests--with-herd ()
    (let* ((s (ghostherd-tests--session :name "a" :kind 'agy))
           (calls nil)
           (callback (lambda (_s state) (push state calls))))
      ;; never settles: no rules match a blank screen for a known agent...
      (with-current-buffer (ghostherd-session-buffer s) (insert "Thinking\n"))
      (ghostherd-tests--tick s (- (float-time) 1) (- (float-time) 10) callback)
      (should (equal calls '(timeout))))))

(ert-deftest ghostherd-test-handoff-replaces-existing-watch ()
  "Two handoffs to one agent must not leave two timers polling it."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy)))
      (ghostherd-tests--with-fake-terminal
        (ghostherd-handoff s "one")
        (let ((first (gethash "a" ghostherd--handoff-watches)))
          (ghostherd-handoff s "two")
          (let ((second (gethash "a" ghostherd--handoff-watches)))
            (should-not (eq first second))
            (should-not (memq first timer-list))
            (should (memq second timer-list)))))
      (ghostherd--handoff-cancel "a")
      (should-not (gethash "a" ghostherd--handoff-watches)))))

(ert-deftest ghostherd-test-handoff-tick-survives-broken-callback ()
  "A callback that throws must not leave the timer polling forever."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy :state 'idle)))
      (with-current-buffer (ghostherd-session-buffer s) (insert "> \n"))
      (puthash "a" (run-with-timer 100 100 #'ignore) ghostherd--handoff-watches)
      (ghostherd-tests--with-fake-terminal
        (ghostherd-tests--tick s nil (- (float-time) 1)
                               (lambda (&rest _) (error "boom"))))
      (should-not (gethash "a" ghostherd--handoff-watches)))))

(ert-deftest ghostherd-test-handoff-tick-drops-vanished-session ()
  (ghostherd-tests--with-herd ()
    (puthash "gone" (run-with-timer 100 100 #'ignore) ghostherd--handoff-watches)
    (ghostherd--handoff-tick "gone" nil 0 #'ignore)
    (should-not (gethash "gone" ghostherd--handoff-watches))))

(ert-deftest ghostherd-test-handoff-rejects-unknown-target ()
  (ghostherd-tests--with-herd ()
    (should-error (ghostherd-handoff "nope" "hi") :type 'user-error)))

;;; Agent-shell command surface

(ert-deftest ghostherd-test-cmd-list-survives-hostile-notes ()
  "The old tab/equals format had no escaping, and notes are free text: a
tab or newline in one turned a single record into several."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--session :name "a" :kind 'agy :state 'idle
                              :notes "review\tthen\nreport \"it\"")
    (let* ((parsed (ghostherd-tests--parse-json (ghostherd-cmd-list))))
      (should (= (length parsed) 1))
      (should (equal (alist-get 'notes (car parsed))
                     "review\tthen\nreport \"it\"")))))

(ert-deftest ghostherd-test-cmd-list-survives-regexp-reason ()
  "`reason' holds a raw :screen-rules pattern, which can contain anything."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy :state 'blocked)))
      (setf (ghostherd-session-state-reason s) "❯ 1\\. Yes\t\"quoted\"")
      (let ((parsed (ghostherd-tests--parse-json (ghostherd-cmd-list))))
        (should (equal (alist-get 'reason (car parsed))
                       "❯ 1\\. Yes\t\"quoted\""))))))

(ert-deftest ghostherd-test-cmd-list-empty-is-valid-json ()
  "An empty herd must still parse, not be the string \"(no sessions)\"."
  (ghostherd-tests--with-herd ()
    (should (equal (ghostherd-tests--parse-json (ghostherd-cmd-list)) '()))))

(ert-deftest ghostherd-test-cmd-list-fields ()
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--session :name "rev" :kind 'grok :state 'idle
                              :project "/tmp/p/" :notes "n")
    (let ((row (car (ghostherd-tests--parse-json (ghostherd-cmd-list)))))
      (dolist (field '(name kind state reason project notes age))
        (should (assq field row)))
      (should (equal (alist-get 'name row) "rev"))
      (should (equal (alist-get 'kind row) "grok")))))

(ert-deftest ghostherd-test-cmd-state-reports-unknown-as-json ()
  "An error has to be machine-readable too, or the caller cannot tell it
apart from a session literally named \"unknown session: x\"."
  (ghostherd-tests--with-herd ()
    (let ((parsed (ghostherd-tests--parse-json (ghostherd-cmd-state "nope"))))
      (should (equal (alist-get 'error parsed) "unknown session"))
      (should (equal (alist-get 'name parsed) "nope")))))

(ert-deftest ghostherd-test-cmd-state-known ()
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--session :name "a" :kind 'agy :state 'blocked)
    (let ((parsed (ghostherd-tests--parse-json (ghostherd-cmd-state "a"))))
      (should (equal (alist-get 'state parsed) "blocked"))
      (should-not (assq 'error parsed)))))

;;; A paste that must really be submitted

(ert-deftest ghostherd-test-a-big-paste-settles-before-return ()
  "A multi-line or long paste gets a moment before Return, on both
backends; a short line, or text not submitted, does not wait."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--tmux-session :name "a"))
          (slept (lambda () (assoc "sleep" ghostherd-tests--tmux-calls))))
      (cl-letf (((symbol-function 'sleep-for)
                 (lambda (sec &rest _) (push (list "sleep" sec) ghostherd-tests--tmux-calls))))
        (ghostherd-tests--with-tmux nil
          (ghostherd--host-send-text s "line 1\nline 2" t))
        ;; The screen first: no paste goes into a box in shell mode.
        (should (equal (mapcar #'car ghostherd-tests--tmux-calls)
                       '("capture-pane" "load-buffer" "paste-buffer" "sleep" "send-keys")))
        (should (equal (cadr (funcall slept)) ghostherd-submit-delay))
        (ghostherd-tests--with-tmux nil
          (ghostherd--host-send-text s (make-string 300 ?x) t))
        (should (funcall slept))
        (dolist (args '(("hi" t) ("line 1\nline 2" nil)))
          (ghostherd-tests--with-tmux nil
            (apply #'ghostherd--host-send-text s args))
          (should-not (funcall slept)))
        (let ((ghostherd-submit-delay 0))
          (ghostherd-tests--with-tmux nil
            (ghostherd--host-send-text s "a\nb" t))
          (should-not (funcall slept)))))
    (let ((g (ghostherd-tests--session :name "g")))
      (should (equal (ghostherd-tests--recording-keys
                       (cl-letf (((symbol-function 'sleep-for) (lambda (&rest _) (push '(:sleep) sent))))
                         (ghostherd--host-send-text g "a\nb" t)))
                     '((:text . "a\nb") (:sleep) ("return" . nil)))))))

(ert-deftest ghostherd-test-unsent-text-is-seen-in-the-input-box ()
  "Text pasted and never submitted sits in the box between the last two
rules: claude's fold of a big paste, or its first words.  Not the same
words up in the history, not an empty box, not what you typed."
  (let ((msg "[ghostherd message from rd → qa]\n[ask 26910956 -- whoever asked is waiting]\nRound 2."))
    (should (ghostherd--unsent-p (ghostherd-test--claude-screen "❯ [Pasted text #2 +20 lines]") msg))
    (should (ghostherd--unsent-p (ghostherd-test--claude-screen "❯ please review the diff")
                                 "please review the diff"))
    (should-not (ghostherd--unsent-p (ghostherd-test--claude-screen "❯ ") msg))
    (should-not (ghostherd--unsent-p (ghostherd-test--claude-screen "❯ my own half-typed words") msg))
    (should-not (ghostherd--unsent-p
                 (ghostherd-test--claude-screen "❯ " "> please review the diff\n\n  Reviewing.\n\n")
                 "please review the diff"))
    ;; grok draws a box, not two rules.
    (should (ghostherd--unsent-p (concat "╭" (make-string 40 ?─) "╮\n│ › please review the diff │\n╰"
                                         (make-string 40 ?─) "╯")
                                 "please review the diff"))
    ;; No box to read: no claim.
    (should-not (ghostherd--unsent-p "❯ please review the diff" "please review the diff"))))

(defmacro ghostherd-test--confirming (screen &rest body)
  "BODY with claude agent `s' showing SCREEN; keys sent in `keys', timers in `later'."
  (declare (indent 1))
  `(ghostherd-tests--with-herd ()
     (ghostherd-tests--with-log
       (let* ((ghostherd-tests--fake-screen ,screen)
              (ghostherd-tests--fake-live t)
              (keys nil)
              (later nil)
              (s (ghostherd-tests--session :name "qa" :kind 'claude :backend 'fake)))
         (cl-letf (((symbol-function 'ghostherd--host-send-keys) (lambda (_s k) (push k keys)))
                   ((symbol-function 'run-at-time) (lambda (_t _r f &rest args) (push (cons f args) later))))
           ,@body)))))

(ert-deftest ghostherd-test-unsent-text-gets-return-again ()
  "Left in the input box of an idle agent: Return again, logged with the
screen, and a look once more -- one try fewer each time, then a warning."
  (let ((msg "[ghostherd message from rd → qa]\nRound 2.\n"))
    (ghostherd-test--confirming (ghostherd-test--claude-screen "❯ [Pasted text #2 +20 lines]")
      (ghostherd--confirm-submit s msg 2)
      (should (equal keys '(("return"))))
      (should (equal later (list (list #'ghostherd--confirm-submit s msg 1))))
      (should (string-match-p "again" (ghostherd-log-entry-text (car ghostherd--log))))
      (should (ghostherd-log-entry-screen (car ghostherd--log)))
      (setq keys nil later nil)
      (ghostherd--confirm-submit s msg 0)
      (should-not (or keys later))
      (should (string-match-p "not submitted" (ghostherd-log-entry-text (car ghostherd--log))))
      (should (ghostherd-tests--issue-p "not submitted")))))

(ert-deftest ghostherd-test-return-is-not-pressed-unless-it-submits ()
  "Return on a menu is an answer and on a working agent a second prompt;
submitted text, or an agent no longer in the herd, wants nothing.  A
menu takes the input box's place: words of one above a box that still
holds the text are the conversation's, and Return goes."
  (let ((msg "please review the diff"))
    (dolist (screen (list (ghostherd-test--claude-screen "❯ please review the diff"
                                                         "  ⠋ Thinking… (esc to interrupt)\n\n")
                          ;; At work in a narrow pane: its spinner alone says so.
                          (ghostherd-test--claude-screen "❯ please review the diff"
                                                         "✽ Bloviating… (37s · ↓ 3.3k tokens)\n")
                          ghostherd-test--claude-permission
                          (ghostherd-test--claude-screen "❯ ")))
      (ghostherd-test--confirming screen
        (ghostherd--confirm-submit s msg 2)
        (should-not (or keys later))))
    (ghostherd-test--confirming (ghostherd-test--claude-screen "❯ please review the diff"
                                                               "  It asked: Do you want to proceed?\n")
      (ghostherd--confirm-submit s msg 2)
      (should (equal keys '(("return")))))
    (ghostherd-test--confirming (ghostherd-test--claude-screen "❯ please review the diff")
      (remhash (ghostherd-session-id s) ghostherd--sessions)
      (ghostherd--confirm-submit s msg 2)
      (should-not keys))))

(ert-deftest ghostherd-test-submitting-looks-again-later ()
  "Text sent with Return is looked at once more; without Return, or with
the look turned off, it is not."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a"))
          (later nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (time _r f &rest args)
                   (when (eq f #'ghostherd--confirm-submit) (push (cons time args) later)))))
        (ghostherd-tests--recording-keys
          (ghostherd-send s "go" t)
          (ghostherd-send s "draft" nil)
          (let ((ghostherd-submit-check-after nil))
            (ghostherd-send s "go" t))))
      (should (equal later (list (list ghostherd-submit-check-after s "go" ghostherd-submit-retries)))))))

;;; A delivery confirmed, not assumed (ask 0d5895f1): ask 2b76d37d sat
;;; unsent for 7 minutes as "[Image #80][Pasted text #81 +62 lines]".

(defconst ghostherd-test--incident-text
  (concat "[ghostherd message from lead-x → qa]\n"
          "[ask 2b76d37d -- whoever asked is waiting for your answer]\n"
          "Investigate the CPU bursts.  The screenshot:\n"
          "/Users/u/.emacs.d/emacs-home/ghostherd-images/lead-x-20261023-222543-108.png\n"
          (mapconcat (lambda (i) (format "detail %d" i)) (number-sequence 1 60) "\n"))
  "An ask shaped like the incident's: a screenshot path, then 60 lines.")

(defun ghostherd-test--claude-footer-screen (box footer)
  "A claude screen whose input box holds BOX, FOOTER under it -- as
claude draws them while it takes a paste in, and after."
  (concat "  The tests pass.\n\n" ghostherd-test--rule "\n"
          (replace-regexp-in-string "\\`❯ " "❯ " box) "\n"
          ghostherd-test--rule "\n  " footer))

(defmacro ghostherd-test--sending (screen &rest body)
  "BODY in `ghostherd-test--confirming', SCREEN shown, `send' sending as
the herd does (nothing pasted for real) and no poll timer started."
  (declare (indent 1))
  `(ghostherd-test--confirming ,screen
     (cl-letf (((symbol-function 'ghostherd--host-send-text) #'ignore)
               ((symbol-function 'run-with-timer) #'ignore))
       (cl-flet ((send (text) (ghostherd-send s text t) (setq later nil keys nil)))
         ,@body))))

(ert-deftest ghostherd-test-a-folded-paste-is-seen-unsent ()
  "Each CLI's fold of a paste is the herd's text still in the box: the
incident's picture-and-text fold, claude's truncated fold, agy's counted
in chars, grok's named picture.  A picture's fold is the herd's only
when its text names a picture -- one you pasted yourself is not."
  (let ((msg ghostherd-test--incident-text)
        (plain "[ghostherd message from lead-x → qa]\nNo picture here.\nAnd more."))
    (should (ghostherd--unsent-p
             (ghostherd-test--claude-screen "❯ [Image #80][Pasted text #81 +62 lines]") msg))
    (should (ghostherd--unsent-p
             (ghostherd-test--claude-screen "❯ [...Truncated text #2 +900 lines...]") plain))
    ;; agy: two rules and a >; its fold counts chars.
    (should (ghostherd--unsent-p
             (concat ghostherd-test--rule "\n> [Pasted text #3 1234 chars]\n"
                     ghostherd-test--rule "\n? for shortcuts")
             plain))
    ;; grok: a box, its picture's fold named.
    (should (ghostherd--unsent-p
             (concat "╭" (make-string 40 ?─) "╮\n│ › [Image #1: lead-x.png] see │\n╰"
                     (make-string 40 ?─) "╯")
             msg))
    (should-not (ghostherd--unsent-p (ghostherd-test--claude-screen "❯ [Image #1]") plain))
    (should-not (ghostherd--unsent-p
                 (ghostherd-test--claude-screen "❯ [Image #1][Pasted text #2 +5 lines]") plain))
    ;; "Pasting…" is the footer's only, not words up in the conversation.
    (should (ghostherd--pasting-p (ghostherd-test--claude-footer-screen "❯ " "Pasting…")))
    (should-not (ghostherd--pasting-p
                 (ghostherd-test--claude-screen "❯ " "Pasting… the diff below.\n\n")))))

(ert-deftest ghostherd-test-a-paste-taken-in-late-gets-return-again-bounded ()
  "The incident, as claude drew it: its box empty while it takes the
picture in (\"Pasting…\"), the Return pressed then lost, then the fold
left in the box.  Not concluded while it pastes; Return again once the
fold shows, `ghostherd-submit-retries' times; then given up on -- on its
card and in the Inbox -- with no Return more."
  (let ((msg ghostherd-test--incident-text)
        (stuck (ghostherd-test--claude-footer-screen
                "❯ [Image #80][Pasted text #81 +62 lines]" "paste again to expand")))
    (ghostherd-test--sending (ghostherd-test--claude-footer-screen "❯ " "Pasting…")
      (send msg)
      ;; A big picture takes a while: two looks, each only a look again.
      (dotimes (_ 2)
        (ghostherd--confirm-submit s msg 2)
        (should-not keys)
        (should (equal later (list (list #'ghostherd--confirm-submit s msg 2))))
        (setq later nil))
      (setq ghostherd-tests--fake-screen stuck)
      (ghostherd--confirm-submit s msg 2)
      (should (equal keys '(("return"))))
      (should (equal later (list (list #'ghostherd--confirm-submit s msg 1))))
      (setq keys nil later nil)
      (ghostherd--confirm-submit s msg 1)
      (should (equal keys '(("return"))))
      (setq keys nil later nil)
      (ghostherd--confirm-submit s msg 0)
      (should-not (or keys later))
      (should (string-match-p "not submitted" (ghostherd-log-entry-text (car ghostherd--log))))
      (should (equal (ghostherd--unsent-head s) "[ghostherd message from lead-x → qa]"))
      ;; Given up on, it is not looked at again.
      (ghostherd--confirm-submit s msg 2)
      (should-not (or keys later)))))

(ert-deftest ghostherd-test-an-unsent-message-is-on-the-inbox ()
  "A message left unsent is a Needs-you item that offers Return; one that
held an ask is its failed ask's item already."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let* ((a (list :name "qa" :kind "claude" :state "idle" :role ""
                  :unsent "[ghostherd message from lead-x → qa]" :unsentAsk :json-false))
         (it (seq-find (lambda (x) (equal (plist-get x :kind) "unsent"))
                       (ghostherd-inbox--agent-items a))))
    (should it)
    (should (equal (plist-get it :id) "unsent:qa"))
    (should (equal (plist-get it :group) "need"))
    (should (string-match-p "still there, unsent" (plist-get it :body)))
    (should-not (seq-find (lambda (x) (equal (plist-get x :kind) "unsent"))
                          (ghostherd-inbox--agent-items (plist-put a :unsentAsk t))))))

(ert-deftest ghostherd-test-a-submit-its-hook-confirms-is-not-looked-at ()
  "Its CLI's prompt-submit hook says a turn began: the text sent to the
resting agent was taken, and no screen is read or key pressed for it.
Sent to an agent at work, the hook's word is about the turn under way
-- agy reports each step -- so the screen decides."
  (ghostherd-test--sending (ghostherd-test--claude-screen "❯ please review the diff")
    (send "please review the diff")
    (ghostherd-report s 'working)
    (should (eq (plist-get (ghostherd--submit-of s) :state) 'confirmed))
    (ghostherd--confirm-submit s "please review the diff" 2)
    (should-not (or keys later))
    (setf (ghostherd-session-state s) 'working)
    (send "and this too")
    (ghostherd-report s 'working)
    (should (eq (plist-get (ghostherd--submit-of s) :state) 'pending))))

(ert-deftest ghostherd-test-a-clear-box-counts-once-it-stays-clear ()
  "No hook word, and the box clear: one more look before it counts as
taken -- a CLI may clear its box a moment before the fold shows -- then
taken, without a Return."
  (ghostherd-test--sending (ghostherd-test--claude-screen "❯ ")
    (send "please review the diff")
    (ghostherd--confirm-submit s "please review the diff" 2)
    (should-not keys)
    (should (equal later (list (list #'ghostherd--confirm-submit s "please review the diff" 2))))
    (setq later nil)
    (ghostherd--confirm-submit s "please review the diff" 2)
    (should-not (or keys later))
    (should (eq (plist-get (ghostherd--submit-of s) :state) 'confirmed))))

;;; Not into a CLI that cannot take a paste, and no answer from a turn
;;; that never began (ask 6a8f023a).  Ask c644aebc: grok-review started
;;; on grok's folder-trust prompt, which read as idle; the ask's paste
;;; went by it, the Return after it said "Yes, proceed", and the ask was
;;; closed 11 seconds later with grok's welcome screen as its answer.

(defconst ghostherd-test--grok-trust-screen
  (concat "\n  /U/u/.g/w/app/grok-review\n\n\n\n\n"
          (mapconcat (lambda (l) (concat (make-string 48 ?\s) l "\n"))
                     '("⠀⠀⠀⠀⠀⠀⣀⣀⡀⠀⠀⠀⢀⠄" "⠀⠀⠀⣠⣾⠿⠛⠛⠛⠛⢀⡴⠁⠀" "⠀⠀⣼⡟⠁⠀⠀⠀⢀⡴⠻⣿⡀⠀"
                       "⠀⠀⣿⡇⠀⠀⠀⠔⠁⠀⠀⣿⡇⠀" "⠀⠀⢹⣷⠀⠀⠀⠀⠀⢀⣴⡿⠀⠀" "⠀⢀⠞⠁⠠⢶⣶⣶⣶⠿⠋⠀⠀⠀"
                       "⠐⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀")
                     "")
          "\n\n"
          "                                 Do you trust the contents of this directory?\n"
          "/Users/u/.ghostherd/worktrees/app/grok-review\n\n"
          "                           Grok Build may run or modify contents in this directory,\n"
          "                                            posing security risks.\n\n"
          "                                        Yes, proceed                 y\n"
          "                                        No, quit                     n\n"
          (make-string 14 ?\n)
          "                                                                                          Grok Build  1.0.70\n")
  "grok 1.0.70's folder-trust prompt, captured in a directory new to it.")

(ert-deftest ghostherd-test-grok-trust-prompt-is-a-menu ()
  "grok's folder-trust prompt parks it on a menu, which says what it
asks -- not idle, as it read when ask c644aebc was handed it.  Its
welcome screen, the box empty, is idle; the question quoted over its
input box is no prompt."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "grok-review" :kind 'grok :backend 'fake
                                       :state 'starting)))
      (let* ((ghostherd-tests--fake-screen ghostherd-test--grok-trust-screen)
             (state (ghostherd--detect-state s)))
        (should (eq (car state) 'blocked))
        (should (string-match-p "Do you trust the contents of this directory" (cdr state))))
      (let ((ghostherd-tests--fake-screen ghostherd-test--grok-welcome-screen))
        (should (eq (car (ghostherd--detect-state s)) 'idle)))
      (let ((ghostherd-tests--fake-screen
             (replace-regexp-in-string
              "Update: .*" (concat "It drew:\n    Do you trust the contents of this directory?\n"
                                   "    Yes, proceed   y\n    No, quit   n")
              ghostherd-test--grok-welcome-screen)))
        (should (eq (car (ghostherd--detect-state s)) 'idle))))))

;;; Review round 1 (ask cf9873be).

(defun ghostherd-test--pane (rows dialog)
  "A pane ROWS high with DIALOG drawn from its top, as each CLI draws its
trust prompt: blank rows under it, to the foot."
  (let ((lines (split-string dialog "\n")))
    (string-join (append lines (make-list (max 0 (- rows (length lines))) "")) "\n")))

(defconst ghostherd-test--agy-trust
  (concat "Accessing workspace:\n\n/Users/u/.ghostherd/worktrees/app/agy-a\n\n"
          "Do you trust the contents of this project?\n\n"
          "Antigravity CLI requires permission to read, edit, and execute files here.\n\n"
          "> Yes, I trust this folder\n  No, exit\n\n  ↑/↓ Navigate · enter Confirm\n"
          (make-string 78 ?\s) "Gemini 4 Argon · high · AI: Out of credits")
  "agy's trust prompt as it draws it, from the top of the pane.")

(ert-deftest ghostherd-test-grok-words-over-its-box-are-no-menu ()
  "grok at rest, its input box at its foot, wrote words its `blocked'
rules know: a review's \"Approve\", a \"(y/n)\" question.  That is its
conversation, not a menu -- it is idle, and gets its mail.  Its menus
take the box's place: one drawn so is blocked."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "grok-review" :kind 'grok :backend 'fake :state 'idle)))
      (dolist (said '("Verdict: Approve -- safe to merge."
                      "Shall I commit it? (y/n)"
                      "Allow this to run once the lead agrees."
                      "Do you want to proceed with the merge, or wait?"))
        (let ((screen (ghostherd-test--grok-at-rest said)))
          (should (eq (car (ghostherd--detect-state s screen)) 'idle))
          (should-not (ghostherd--herd-held-p '(:to "grok-review")))))
      ;; A card in the box's place, its shortcuts bar under it.
      (let ((card (concat "     ◆ Thought for 0.9s\n\n"
                          "  ┃  Allow this command?  rm -rf build/\n  ┃\n"
                          "  ┃  1 (○) Allow once\n  ┃  2 (○) Always allow\n  ┃  3 (○) Reject\n  ┃\n"
                          "  Tab:next answer  │  Esc:scrollback  │  Shift+x:dismiss")))
        (should (eq (car (ghostherd--detect-state s card)) 'blocked))
        (should (ghostherd--herd-held-p '(:to "grok-review")))))))

(ert-deftest ghostherd-test-a-dialog-above-the-tail-is-read-and-held ()
  "Each CLI's trust prompt is drawn from the top of its pane.  In a tall
one the 40 lines the poll reads are blank rows, no box in them: it read
idle and was handed mail, whose Return answered it.  With nothing read
and no box at the foot, the whole pane is read: blocked, saying what it
asks.  And mail and a message waiting for its prompt are held while no
box is drawn, read or not -- until the box is back."
  (ghostherd-tests--with-herd ()
    (dolist (case `((grok . ,ghostherd-test--grok-trust-screen)
                    (claude . ,ghostherd-test--claude-trust)
                    (agy . ,ghostherd-test--agy-trust)))
      (let* ((pane (ghostherd-test--pane 70 (cdr case)))
             (tail (ghostherd--string-tail pane ghostherd-screen-tail-lines))
             (name (symbol-name (car case)))
             (s (ghostherd-tests--session :name name :kind (car case) :backend 'fake :state 'starting)))
        (let ((ghostherd-tests--fake-screen pane))
          (let ((state (ghostherd--detect-state s tail)))
            (should (eq (car state) 'blocked))
            (should (string-match-p "trust\\|Quick safety check" (cdr state))))
          (should (ghostherd--herd-held-p (list :to name)))
          ;; Read idle, as on master: held all the same, by the box.
          (setf (ghostherd-session-state s) 'idle)
          (should (ghostherd--herd-held-p (list :to name)))
          (let ((ran nil))
            (cl-letf (((symbol-function 'run-at-time) #'ignore))
              (ghostherd--when-ready s (lambda () (setq ran t))))
            (should-not ran)))))
    ;; Its box back at the foot: mail goes, the message is sent.
    (let ((s (ghostherd-get "grok")))
      (puthash "grok" ghostherd-test--grok-welcome-screen ghostherd--screens)
      (should-not (ghostherd--herd-held-p '(:to "grok")))
      (let ((ran nil))
        (ghostherd--when-ready s (lambda () (setq ran t)))
        (should ran)))
    ;; A CLI that draws from the top of a tall pane, its box there and
    ;; blank rows under it: the tail is blank, the pane has the box.
    (let ((s (ghostherd-get "agy"))
          (pane (ghostherd-test--pane 70 (ghostherd-test--claude-screen "❯ "))))
      (puthash "agy" (ghostherd--string-tail pane ghostherd-screen-tail-lines) ghostherd--screens)
      (let ((ghostherd-tests--fake-screen pane))
        (should-not (ghostherd--herd-held-p '(:to "agy")))
        (let ((ran nil))
          (ghostherd--when-ready s (lambda () (setq ran t)))
          (should ran))))
    ;; A 40-row pane needs no second read.
    (let ((s (ghostherd-get "claude"))
          (captures 0))
      (cl-letf (((symbol-function 'ghostherd--host-capture)
                 (lambda (&rest _) (cl-incf captures) (ghostherd-test--claude-screen "❯ "))))
        (ghostherd--detect-state s (ghostherd-test--claude-screen "❯ ")))
      (should (= captures 0)))))

;;; Review round 2 (ask 50461410).

(ert-deftest ghostherd-test-a-box-whose-text-says-1-is-still-a-box ()
  "Text in the input box may hold \"> 1.\" -- a draft \"x > 1.5\" -- and the
box is still the box: the words above it are the conversation's, the
agent is idle and gets its mail.  A menu's first choice, its number at
the sign, framed or not, is no box."
  (ghostherd-tests--with-herd ()
    (let* ((draft "check that x > 1.5 still holds")
           (c (ghostherd-tests--session :name "qa" :kind 'claude :backend 'fake :state 'idle))
           (g (ghostherd-tests--session :name "grok-review" :kind 'grok :backend 'fake :state 'idle))
           (cases `((,c . ,(ghostherd-test--claude-screen
                             (concat "❯ " draft) "  It asked: Do you want to proceed?\n\n"))
                    (,g . ,(replace-regexp-in-string
                             "│ ❯ " (concat "│ ❯ " draft)
                             (ghostherd-test--grok-at-rest "Do you want to proceed?") t t)))))
      (pcase-dolist (`(,s . ,screen) cases)
        (should (ghostherd--input-at-foot-p screen))
        (should (ghostherd--box-top (vconcat (split-string screen "\n"))))
        (should (eq (car (ghostherd--detect-state s screen)) 'idle))
        (should-not (ghostherd--herd-held-p (list :to (ghostherd-session-name s)))))
      (dolist (menu (list (concat ghostherd-test--rule "\n❯ 1. Yes\n  2. No\n")
                          (concat "  ╭" (make-string 40 ?─) "╮\n  │ ❯ 1. Allow once │\n"
                                  "  │   2. Reject │\n  ╰" (make-string 40 ?─) "╯\n")))
        (should-not (ghostherd--input-at-foot-p menu))
        (should-not (ghostherd--box-top (vconcat (split-string menu "\n"))))))))

(ert-deftest ghostherd-test-a-ghostel-screen-is-its-rows-not-its-scrollback ()
  "On ghostel the buffer holds the scrollback: the whole screen is its
last rows, as many as its terminal has, so an old prompt up in the
history is not read as the screen.  Rows not known: no more than the
tail."
  (with-temp-buffer
    (insert "  Do you want to proceed?\n  ❯ 1. Yes\n  2. No\n")
    (dotimes (i 50) (insert (format "line %d\n" i)))
    (setq-local ghostel--term-rows 5)
    (let ((buf (current-buffer)))
      (cl-letf (((symbol-function 'ghostherd--ghostel-buffer) (lambda (_) buf)))
        (let ((pane (ghostherd-backend-pane 'ghostel nil)))
          (should (string-match-p "line 49" pane))
          (should-not (string-match-p "proceed\\|line 44" pane)))
        (setq-local ghostel--term-rows nil)
        (should (= (length (split-string (ghostherd-backend-pane 'ghostel nil) "\n" t))
                   ghostherd-screen-tail-lines))))))

(ert-deftest ghostherd-test-a-held-screen-is-not-captured-again ()
  "A screen no rule reads and with no box: read whole from the pane the
batched fetch kept beside its tail -- for its state every poll, for its
mail every tick -- not captured again.  A tail it was not kept with is."
  (ghostherd-tests--with-herd ()
    (let* ((s (ghostherd-tests--session :name "grok" :kind 'grok :backend 'fake :state 'idle))
           (pane (ghostherd-test--pane 70 ghostherd-test--grok-trust-screen))
           (tail (ghostherd--string-tail pane ghostherd-screen-tail-lines))
           (captures 0))
      (cl-letf (((symbol-function 'ghostherd-backend-pane)
                 (lambda (&rest _) (cl-incf captures) pane)))
        (puthash "grok" (cons tail pane) ghostherd--panes)
        (should (eq (car (ghostherd--detect-state s tail)) 'blocked))
        (setf (ghostherd-session-state s) 'idle)
        (dotimes (_ 3) (should (ghostherd--herd-held-p '(:to "grok"))))
        (should (= captures 0))
        (puthash "grok" (cons "another screen" pane) ghostherd--panes)
        (should (ghostherd--herd-held-p '(:to "grok")))
        (should (= captures 1))))))

(ert-deftest ghostherd-test-a-quick-turn-its-hooks-saw-is-taken ()
  "A turn so quick its `working' and `done' reached the herd in one
tick -- which carries each agent's newest report only -- never said
`working' here.  The sidecar keeps when the turn began (the link's
`:turn_ms'): after the send, the text was taken, and the stop that left
its ask open is read then.  A turn from before the send is not this
one."
  (let ((msg (format ghostherd-message-template "lead-x" "qa"
                     "[ask a1 -- whoever asked is waiting for your answer]\nReview it."))
        (ghostherd--herd-links (make-hash-table :test 'equal)))
    (ghostherd-tests--with-asks
     (ghostherd-tests--with-log
      (let ((s (ghostherd-tests--session :name "qa" :kind 'claude :state 'idle :backend 'fake))
            (ghostherd-tests--fake-screen (ghostherd-test--claude-screen "❯ ")))
        (puthash "a1" "qa" ghostherd--asks-open)
        (cl-letf (((symbol-function 'ghostherd--host-send-text) #'ignore)
                  ((symbol-function 'run-at-time) #'ignore)
                  ((symbol-function 'run-with-timer) #'ignore))
          (puthash "qa" (list :session "qa" :cli "claude" :turn_ms (* 1000 (- (float-time) 60)))
                   ghostherd--herd-links)
          (ghostherd-send s msg t)
          (ghostherd--confirm-submit s msg 2)
          (should (eq (plist-get (ghostherd--submit-of s) :state) 'pending))
          ;; It answered in prose and stopped; only its `done' came.
          (ghostherd--set-state s 'done)
          (should-not requests)
          (puthash "qa" (list :session "qa" :cli "claude"
                              :turn_ms (* 1000 (+ (plist-get (ghostherd--submit-of s) :at) 0.4)))
                   ghostherd--herd-links)
          (ghostherd--confirm-submit s msg 2)
          (should (eq (plist-get (ghostherd--submit-of s) :state) 'confirmed))
          (should (equal (mapcar #'car requests) '("herd_settle")))))))))

(ert-deftest ghostherd-test-a-lost-text-gives-a-lead-its-role-back ()
  "A lead started on a CLI that takes no role is owed it with its first
message.  That message never reached it: the role is owed again, for
the asker's re-send to carry.  A text that did not carry it gives none."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (let ((s (ghostherd-tests--session :name "lead-app" :kind 'grok :state 'idle)))
        (cl-letf (((symbol-function 'ghostherd--ask-settle) #'ignore))
          (puthash "lead-app" "You are the lead of this project's herd." ghostherd--role-owed)
          (let ((owed (ghostherd--role-take s)))
            (should-not (gethash "lead-app" ghostherd--role-owed))
            (ghostherd--submit-lost s "Plan the release." "")
            (should-not (gethash "lead-app" ghostherd--role-owed))
            (ghostherd--submit-lost s (concat "Plan the release.\n\n---\n\n" owed) "")
            (should (equal (gethash "lead-app" ghostherd--role-owed) owed))))))))

(ert-deftest ghostherd-test-mail-waits-while-its-taker-cannot-take-it ()
  "Mail for an agent on a menu, or still starting, is not pasted -- the
menu would read the Return after it as its answer -- nor acknowledged:
the sidecar sends it again on a later tick, and once the agent is at
its prompt it goes."
  (ghostherd-tests--with-asks
    (let ((taker (ghostherd-tests--session :name "grok-review" :kind 'grok :state 'blocked))
          (pasted nil)
          (ghostherd--herd-ack-ids nil))
      (cl-letf (((symbol-function 'ghostherd--herd-deliver-one) (lambda (m) (push (plist-get m :id) pasted))))
        (dolist (state '(blocked starting))
          (setf (ghostherd-session-state taker) state)
          (ghostherd--herd-deliver '(:pending ((:id "m1" :from "lead" :to "grok-review" :body "hi"))))
          (should-not (or pasted ghostherd--herd-ack-ids)))
        (setf (ghostherd-session-state taker) 'idle)
        (ghostherd--herd-deliver '(:pending ((:id "m1" :from "lead" :to "grok-review" :body "hi"))))
        (should (equal pasted '("m1")))
        (should (equal ghostherd--herd-ack-ids '("m1")))))))

(ert-deftest ghostherd-test-a-paste-no-turn-took-is-lost-and-its-ask-fails ()
  "Of an agent whose hooks report, a prompt sent at rest is taken only
when a turn begins on it -- its hook says so, or its screen is at work.
A box clear is no proof: c644aebc's was clear, the paste gone by a
dialog.  Looked at until its looks run out, then `lost' -- no Return
pressed, nothing on the card -- and the ask in it fails, saying so.  An
agent with no hook to say, or a slash command, keeps the box's word."
  (let ((msg (format ghostherd-message-template "lead-x" "qa"
                     "[ask c644aebc -- whoever asked is waiting for your answer]\nReview it."))
        (settled nil)
        (ghostherd--herd-links (make-hash-table :test 'equal))
        (ghostherd--asks-open (make-hash-table :test 'equal)))
    (ghostherd-test--sending (ghostherd-test--claude-screen "❯ ")
      (cl-letf (((symbol-function 'ghostherd--ask-settle)
                 (lambda (id _s _dead &optional error) (push (list id error) settled))))
        (cl-flet ((resend (text)
                    (setf (ghostherd-session-state s) 'idle)
                    (send text)))
          (puthash "qa" '(:session "qa" :cli "claude") ghostherd--herd-links)
          (puthash "c644aebc" (ghostherd-session-id s) ghostherd--asks-open)
          (resend msg)
          (dotimes (_ ghostherd-submit-looks)
            (ghostherd--confirm-submit s msg 2)
            (should (equal later (list (list #'ghostherd--confirm-submit s msg 2))))
            (setq later nil))
          (should-not (or keys settled))
          (ghostherd--confirm-submit s msg 2)
          (should-not (or keys later))
          (should (eq (plist-get (ghostherd--submit-of s) :state) 'lost))
          (should (string-match-p "never taken" (ghostherd-log-entry-text (car ghostherd--log))))
          (should (ghostherd-tests--issue-p "never taken"))
          (should (equal (caar settled) "c644aebc"))
          (should (string-match-p "not taken: qa's CLI started no turn on it" (cadar settled)))
          (should-not (ghostherd--unsent-head s))
          (setq settled nil)
          ;; Its hook says a turn began: taken.
          (resend msg)
          (ghostherd--confirm-submit s msg 2)
          (ghostherd-report s 'working)
          (should (eq (plist-get (ghostherd--submit-of s) :state) 'confirmed))
          ;; Its screen at work: taken.
          (resend msg)
          (let ((ghostherd-tests--fake-screen
                 (ghostherd-test--claude-screen "❯ " "✽ Bloviating… (3s · ↓ 1.2k tokens)\n")))
            (ghostherd--confirm-submit s msg 2))
          (should (eq (plist-get (ghostherd--submit-of s) :state) 'confirmed))
          ;; A slash command begins no turn: the box's word.
          (resend "/model opus")
          (ghostherd--confirm-submit s "/model opus" 2)
          (ghostherd--confirm-submit s "/model opus" 2)
          (should (eq (plist-get (ghostherd--submit-of s) :state) 'confirmed))
          ;; No hook to say: the box's word.
          (clrhash ghostherd--herd-links)
          (resend msg)
          (ghostherd--confirm-submit s msg 2)
          (ghostherd--confirm-submit s msg 2)
          (should (eq (plist-get (ghostherd--submit-of s) :state) 'confirmed))
          (should-not settled))))))

(ert-deftest ghostherd-test-an-ask-no-turn-took-is-not-answered-by-a-stop ()
  "The taker reads idle again before its submit check has ended: the ask
in text not yet known taken is left open -- not settled with a screen
that never read it -- and settled at its turn's end once taken."
  (ghostherd-tests--with-asks
   (ghostherd-tests--with-log
    (let* ((taker (ghostherd-tests--session :name "qa" :kind 'claude :state 'idle :backend 'fake))
           (msg (format ghostherd-message-template "lead-x" "qa"
                        "[ask a1 -- whoever asked is waiting for your answer]\nReview it."))
           (ghostherd--herd-links (make-hash-table :test 'equal))
           (ghostherd-tests--fake-screen (ghostherd-test--claude-screen "❯ ")))
      (puthash "qa" '(:session "qa" :cli "claude") ghostherd--herd-links)
      (puthash "a1" "qa" ghostherd--asks-open)
      (cl-letf (((symbol-function 'ghostherd--host-send-text) #'ignore)
                ((symbol-function 'run-at-time) #'ignore)
                ((symbol-function 'run-with-timer) #'ignore))
        (ghostherd-send taker msg t)
        (ghostherd--set-state taker 'done)
        (should-not requests)
        (should (gethash "a1" ghostherd--asks-open))
        (should (string-match-p "a1 left open: qa has begun no turn on it"
                                (ghostherd-log-entry-text (car ghostherd--log))))
        (should (ghostherd-tests--issue-p "a1 left open"))
        ;; Its hook: the turn began on it; its end settles the ask.
        (ghostherd-report taker 'working)
        (ghostherd--set-state taker 'working)
        (ghostherd--set-state taker 'done)
        (should (equal (mapcar #'car requests) '("herd_settle"))))))))

(ert-deftest ghostherd-test-return-again-on-request-only-while-unsent ()
  "The card's and the Inbox's Press Return: pressed only while the text
is verifiably still in the box, then watched as a fresh submit.  Gone
from the box, nothing is pressed and it is no longer shown unsent."
  (let ((stuck (ghostherd-test--claude-screen "❯ [Pasted text #2 +20 lines]"))
        (msg "[ghostherd message from rd → qa]\nRound 2.\nMore."))
    (ghostherd-test--sending stuck
      (send msg)
      (ghostherd--confirm-submit s msg 0)
      (should (ghostherd--unsent-head s))
      (setq keys nil)
      (ghostherd-submit-again s)
      (should (equal keys '(("return"))))
      (should (eq (plist-get (ghostherd--submit-of s) :state) 'pending))
      (should (equal later (list (list #'ghostherd--confirm-submit s msg ghostherd-submit-retries))))
      (ghostherd--confirm-submit s msg 0)
      (setq keys nil ghostherd-tests--fake-screen (ghostherd-test--claude-screen "❯ "))
      (should-error (ghostherd-submit-again s) :type 'user-error)
      (should-not keys)
      (should-not (ghostherd--unsent-head s)))))

(ert-deftest ghostherd-test-an-unsent-text-cleared-by-hand-is-no-longer-shown ()
  "You pressed Return yourself: the next poll's screen no longer holds
it, and the card and the Inbox no longer say it is unsent."
  (ghostherd-test--sending (ghostherd-test--claude-screen "❯ [Pasted text #2 +20 lines]")
    (send "[ghostherd message from rd → qa]\nRound 2.\nMore.")
    (ghostherd--confirm-submit s "[ghostherd message from rd → qa]\nRound 2.\nMore." 0)
    (should (ghostherd--unsent-head s))
    ;; The screen called it idle meanwhile, the text still in its box.
    (puthash (ghostherd-session-id s) ghostherd-tests--fake-screen ghostherd--screens)
    (ghostherd--set-state s 'idle "no rule matched")
    (should (ghostherd--unsent-head s))
    (puthash (ghostherd-session-id s) (ghostherd-test--claude-screen "❯ ") ghostherd--screens)
    (ghostherd--set-state s 'working "reported working")
    (should-not (ghostherd--unsent-head s))))

(ert-deftest ghostherd-test-an-ask-turn-that-starts-late-is-still-the-asks ()
  "The incident's turn origin: the herd moves the taker to `working' as
it sends, the Return is lost, so that \"turn\" ends at once -- and the
real one starts minutes later, when Return is pressed again.  That turn
is the ask's (its end no report to the user): the ask's flag waits for
its prompt to reach the CLI.  When the hook confirms at once, the turn
under way is the ask's and the flag is spent.  Either way the next turn,
the user's own prompt, is the user's."
  (ghostherd-tests--with-asks
    (ghostherd-tests--with-log
      (let ((s (ghostherd-tests--session :name "qa" :kind 'claude :state 'idle :backend 'fake))
            (ghostherd-tests--fake-live t))
        (cl-letf (((symbol-function 'ghostherd--host-send-text) #'ignore)
                  ((symbol-function 'run-at-time) #'ignore)
                  ((symbol-function 'run-with-timer) #'ignore))
          ;; Late: delivered, the Return lost, the screen calls it done.
          (ghostherd--ask-deliver "2b76d37d" "lead-x" "qa" "Investigate the CPU bursts." 5)
          (should (eq (ghostherd--turn-origin-of s) 'ask))
          (ghostherd--set-state s 'done "no rule matched")
          (ghostherd--set-state s 'idle "seen")
          ;; Minutes later Return is pressed: its hook, then the poll.
          (ghostherd-report s 'working)
          (ghostherd--set-state s 'working "reported working")
          (should (eq (ghostherd--turn-origin-of s) 'ask))
          (should-not (gethash "qa" ghostherd--turn-ask-pending))
          (ghostherd--set-state s 'done "finished its turn")
          (ghostherd--set-state s 'idle "seen")
          (ghostherd-send s "thanks -- now the next thing" t)
          (ghostherd-report s 'working)
          (should (eq (ghostherd--turn-origin-of s) 'user))
          ;; At once: the hook confirms while the herd has it working.
          (ghostherd--set-state s 'done "finished its turn")
          (ghostherd--ask-deliver "3c87e48e" "lead-x" "qa" "And another." 5)
          (ghostherd-report s 'working)
          (should (eq (ghostherd--turn-origin-of s) 'ask))
          (should-not (gethash "qa" ghostherd--turn-ask-pending))
          (ghostherd--set-state s 'done "finished its turn")
          (ghostherd-send s "one of mine" t)
          (should (eq (ghostherd--turn-origin-of s) 'user)))))))

(ert-deftest ghostherd-test-a-pair-is-not-labelled-against-its-names ()
  "The pair's names say their roles; notes that said otherwise once told
the herd the agent called qa was the implementer."
  (ghostherd-tests--with-herd ()
    (let (spawned)
      (cl-letf (((symbol-function 'ghostherd--project-root) (lambda (&rest _) "/p/"))
                ((symbol-function 'completing-read) (lambda (&rest _) "claude"))
                ((symbol-function 'read-string) (let ((names '("qa" "rd"))) (lambda (&rest _) (pop names))))
                ((symbol-function 'ghostherd-spawn)
                 (lambda (_k &rest pl)
                   (push pl spawned)
                   (ghostherd-tests--session :name (plist-get pl :name))))
                ((symbol-function 'ghostherd--sidebar-leave-overlay) #'ignore)
                ((symbol-function 'ghostherd--host-view) (lambda (&rest _) (current-buffer)))
                ((symbol-function 'ghostherd--sync-view-size) #'ignore)
                ((symbol-function 'delete-other-windows) #'ignore)
                ((symbol-function 'split-window-right) #'ignore)
                ((symbol-function 'other-window) #'ignore)
                ((symbol-function 'switch-to-buffer) #'ignore))
        (ghostherd-new-pair))
      (should (equal (mapcar (lambda (pl) (plist-get pl :name)) (reverse spawned)) '("qa" "rd")))
      (should-not (seq-some (lambda (pl) (plist-member pl :notes)) spawned)))))

;;; Images pasted into a prompt

(ert-deftest ghostherd-test-clipboard-image-is-saved ()
  "The clipboard's PNG lands byte for byte in a file named for the agent,
readable only by you, in a directory only you can list."
  (ghostherd-test--with-clipboard ghostherd-test--png-sh
    (let ((file (ghostherd-save-clipboard-image "claude api/x")))
      (should (equal (file-name-directory file) (expand-file-name ghostherd-image-directory)))
      (should (string-match-p "\\`claude-api-x-[0-9]\\{8\\}-[0-9]\\{6\\}-[0-9]\\{3\\}\\.png\\'"
                              (file-name-nondirectory file)))
      (should (equal (with-temp-buffer
                       (set-buffer-multibyte nil)
                       (insert-file-contents-literally file)
                       (buffer-string))
                     ghostherd-test--png))
      (should (= (file-modes file) #o600))
      (should (= (file-modes ghostherd-image-directory) #o700))
      (should (string-prefix-p "image-" (file-name-nondirectory (ghostherd-save-clipboard-image)))))))

(ert-deftest ghostherd-test-clipboard-without-an-image ()
  "Text, an empty clipboard or a failing command save nothing, and say so;
with no command installed, it names the one to install."
  (dolist (script '("printf 'just text'" "exit 1" "printf '\\211PNG\\r\\n\\032\\n'; exit 1"))
    (ghostherd-test--with-clipboard script
      (should (equal (cadr (should-error (ghostherd-save-clipboard-image "a") :type 'user-error))
                     "No image on the clipboard"))
      (should-not (file-exists-p ghostherd-image-directory))))
  (let ((ghostherd-clipboard-image-commands '(("ghostherd-no-such-program" "-"))))
    (should (string-match-p "pngpaste" (cadr (should-error (ghostherd-save-clipboard-image)
                                                           :type 'user-error))))))

(ert-deftest ghostherd-test-old-pasted-images-go ()
  "Saving an image removes pasted images older than the days kept, and
nothing else: not other files, not the one just saved.  nil keeps all."
  (ghostherd-test--with-clipboard ghostherd-test--png-sh
    (make-directory ghostherd-image-directory t)
    (let* ((at (lambda (f) (expand-file-name f ghostherd-image-directory)))
           (month-ago (time-subtract nil (* 31 86400))))
      (dolist (f '("a-old.png" "a-old.jpg" "a-recent.png" "notes.txt"))
        (write-region "x" nil (funcall at f) nil 'silent))
      (set-file-times (funcall at "a-old.png") month-ago)
      (set-file-times (funcall at "a-old.jpg") month-ago)
      (set-file-times (funcall at "notes.txt") month-ago)
      (let ((ghostherd-image-keep-days nil))
        (ghostherd-save-clipboard-image "a")
        (should (file-exists-p (funcall at "a-old.png"))))
      (let ((new (ghostherd-save-clipboard-image "a")))
        (should-not (file-exists-p (funcall at "a-old.png")))
        (should-not (file-exists-p (funcall at "a-old.jpg")))
        (should (file-exists-p (funcall at "a-recent.png")))
        (should (file-exists-p (funcall at "notes.txt")))
        (should (file-exists-p new)))
      (let* ((ghostherd-image-keep-days 0)
             (new (ghostherd-save-clipboard-image "a")))
        (should (file-exists-p new))
        (should-not (file-exists-p (funcall at "a-recent.png")))))))

;;; Answering a menu

(defun ghostherd-test--answer (choice &rest how)
  "Answer CHOICE on a model of agy's menu, which wraps around at either end.
HOW: :on where its highlight starts, :lag looks that still see it there
once moved, :stuck when arrows do nothing, :went when the menu goes once
a key is sent, :gone when the agent leaves the herd.  Returns (:keys the
keys sent :pressed the choice Return went on :entry the log's last :looks)."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (let* ((ghostherd-tests--fake-live t)
             (s (ghostherd-tests--session :name "agy-c" :kind 'agy :backend 'fake))
             (on (or (plist-get how :on) 1))
             (shown on)
             (lag (or (plist-get how :lag) 0))
             (looks 0) keys pressed later)
        (cl-letf (((symbol-function 'ghostherd--host-capture)
                   (lambda (_s &optional _n)
                     (cl-incf looks)
                     (if (and keys (> lag 0)) (cl-decf lag) (setq shown on))
                     (if (and keys (plist-get how :went)) "> \n" (ghostherd-test--agy-menu-on shown))))
                  ((symbol-function 'ghostherd--host-send-keys)
                   (lambda (_s ks)
                     (dolist (k ks)
                       (push k keys)
                       (unless (plist-get how :stuck)
                         (pcase k
                           ("up" (setq on (1+ (mod (- on 2) 4))))
                           ("down" (setq on (1+ (mod on 4))))))
                       (when (equal k "return") (setq pressed on)))))
                  ((symbol-function 'run-at-time)
                   (lambda (_t _r f &rest args) (setq later (append later (list (cons f args)))))))
          (ghostherd-answer s choice)
          (when (plist-get how :gone)
            (remhash (ghostherd-session-id s) ghostherd--sessions))
          (cl-loop repeat 50 while later
                   do (let ((next (pop later))) (apply (car next) (cdr next))))
          (should-not later)
          (list :keys (reverse keys) :pressed pressed :entry (car ghostherd--log) :looks looks))))))

(ert-deftest ghostherd-test-answer-moves-from-the-highlight ()
  "Every choice of a menu that wraps around, from wherever its highlight
starts: the moves count from there, and Return goes on that choice.
Ups enough to reach the top, then Downs, took agy's Yes round to its
last choice, \"No, cancel\"."
  (dolist (from '(1 2 3 4))
    (dolist (to '(1 2 3 4))
      (let ((got (ghostherd-test--answer to :on from)))
        (should (eql (plist-get got :pressed) to))
        (should (equal (plist-get got :keys)
                       (append (make-list (abs (- to from)) (if (> to from) "down" "up"))
                               '("return"))))))))

(ert-deftest ghostherd-test-answer-returns-on-the-screens-word ()
  "Return waits for the screen to show the highlight on the choice; the
answer is logged in its words.  Arrows that went nowhere, or a menu
gone, leave it unanswered after a few looks, logged with the screen; an
agent gone from the herd is let be."
  (let ((got (ghostherd-test--answer 3 :lag 2)))
    (should (eql (plist-get got :pressed) 3))
    (should (equal (plist-get got :keys) '("down" "down" "return")))
    (should (equal (ghostherd-log-entry-text (plist-get got :entry))
                   "← 3. Yes, and always allow for commands that start with 'grep' (Persist to settings.json)")))
  (let ((got (ghostherd-test--answer 2 :stuck t)))
    (should-not (plist-get got :pressed))
    (should (equal (plist-get got :keys) '("down")))
    (should (= (plist-get got :looks) (+ 2 ghostherd--answer-looks)))
    (should (equal (ghostherd-log-entry-text (plist-get got :entry))
                   "2 not answered: the highlight stayed on 1"))
    (should (ghostherd-log-issue-p (plist-get got :entry)))
    (should (string-match-p "> 1\\. Yes" (ghostherd-log-entry-screen (plist-get got :entry)))))
  (let ((got (ghostherd-test--answer 2 :went t)))
    (should-not (plist-get got :pressed))
    (should (equal (ghostherd-log-entry-text (plist-get got :entry)) "2 not answered: the menu went")))
  (let ((got (ghostherd-test--answer 2 :gone t)))
    (should (equal (plist-get got :keys) '("down")))
    (should-not (plist-get got :entry))))

(ert-deftest ghostherd-test-answer-only-a-menu-it-can-read ()
  "No menu with numbers, no such choice, no highlight to count from, or
no host: refused, and no key sent."
  (ghostherd-tests--with-herd ()
    (let* ((ghostherd-tests--fake-live t)
           (s (ghostherd-tests--session :name "agy-c" :kind 'agy :backend 'fake))
           (did nil))
      (cl-letf (((symbol-function 'ghostherd--host-send-keys) (lambda (_s k) (push k did)))
                ((symbol-function 'run-at-time) (lambda (&rest a) (push a did))))
        (pcase-dolist (`(,screen . ,choice)
                       (list (cons (ghostherd-test--claude-screen "❯ ") 1)
                             (cons ghostherd-test--agy-menu 0)
                             (cons ghostherd-test--agy-menu 5)
                             (cons (ghostherd-test--agy-menu-on nil) 1)))
          (let ((ghostherd-tests--fake-screen screen))
            (should-error (ghostherd-answer s choice) :type 'user-error)))
        (let ((ghostherd-tests--fake-screen (ghostherd-test--claude-screen "❯ ")))
          (should (equal (cadr (should-error (ghostherd-answer s 1) :type 'user-error))
                         "agy-c is on no menu with numbers")))
        (let ((ghostherd-tests--fake-live nil)
              (ghostherd-tests--fake-screen ghostherd-test--agy-menu))
          (should-error (ghostherd-answer s 1) :type 'user-error))
        (should-not did)
        (let ((ghostherd-tests--fake-screen ghostherd-test--agy-menu))
          (ghostherd-answer s 1)
          (should did))))))

(ert-deftest ghostherd-test-prompt-not-the-conversations-numbers ()
  "Numbers that are the conversation's, not a menu's: too far up, not
from 1, one alone, or too far apart -- no menu."
  (dolist (screen (list (concat "Plan:\n1. Read the code\n2. Write the test\n3. Ship it\n"
                                (make-string 9 ?\n) "Allow this? [y/N]\n")
                        "Steps so far:\n  3. Wrote the test\n  4. Ran it\n> \n"
                        "> 1. Only one\nesc\n"
                        (concat "> 1. Yes\n" (make-string 6 ?\n) "  2. No\n")))
    (should-not (ghostherd--prompt screen))))

(ert-deftest ghostherd-test-session-prompt-only-blocked ()
  "Read off its last screen, while it is blocked; for the page and the
sidecar as JSON, false without one."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "agy-c" :kind 'agy :state 'blocked)))
      (puthash "agy-c" ghostherd-test--agy-menu ghostherd--screens)
      (should (equal (plist-get (ghostherd-session-prompt s) :options)
                     (plist-get (ghostherd--prompt ghostherd-test--agy-menu) :options)))
      (let ((json (ghostherd--prompt-json (ghostherd-session-prompt s))))
        (should (equal (aref (plist-get json :options) 3) '(:n 4 :label "No, cancel"))))
      (should (equal (plist-get (car (ghostherd--herd-snapshot)) :prompt)
                     (ghostherd--prompt-json (ghostherd-session-prompt s))))
      (require 'ghostherd-memory-page)
      (cl-letf (((symbol-function 'ghostherd-memory-page--agent-work) #'ignore)
                ((symbol-function 'ghostherd-memory-page--agent-links) #'ignore))
        (should (equal (plist-get (ghostherd-memory-page--agent s) :prompt)
                       (ghostherd--prompt-json (ghostherd-session-prompt s)))))
      (setf (ghostherd-session-state s) 'idle)
      (should-not (ghostherd-session-prompt s))
      (should (eq (ghostherd--prompt-json nil) :json-false))
      (should (eq (plist-get (car (ghostherd--herd-snapshot)) :prompt) :json-false)))))

;;; The user's own `!' command
;;
;; A backend that records what it is sent and shows the screen a test
;; sets, so the send path is driven whole: the screen it reads, the keys
;; and the text it sends, in order.

(defvar ghostherd-tests--shrec nil
  "What the `shrec' backend was sent, oldest first: (:text TEXT SUBMIT)
or (:keys . KEYS).")

(defvar ghostherd-tests--shrec-screen ""
  "The screen the `shrec' backend shows: a string, or a function of none.")

(cl-defmethod ghostherd-backend-send-text ((_backend (eql shrec)) _session text submit)
  (setq ghostherd-tests--shrec (append ghostherd-tests--shrec (list (list :text text submit)))))

(cl-defmethod ghostherd-backend-send-keys ((_backend (eql shrec)) _session keys)
  (setq ghostherd-tests--shrec (append ghostherd-tests--shrec (list (cons :keys keys)))))

(cl-defmethod ghostherd-backend-capture ((_backend (eql shrec)) _session &optional _n)
  (if (functionp ghostherd-tests--shrec-screen)
      (funcall ghostherd-tests--shrec-screen)
    ghostherd-tests--shrec-screen))

(cl-defmethod ghostherd-backend-live-p ((_backend (eql shrec)) _session) t)

(defun ghostherd-tests--shrec-session (&rest args)
  "A claude session on the `shrec' backend, from ARGS."
  (apply #'ghostherd-tests--session (append args '(:backend shrec :kind claude :state idle))))

(defconst ghostherd-tests--claude-shell (ghostherd-test--claude-screen "! ")
  "claude with `!' typed on its empty prompt: its box in shell mode.")

(defmacro ghostherd-tests--with-shell (&rest body)
  "BODY in a clean herd with leads, `shrec' recording, claude at its empty
prompt, no submit looked at, and the shell mode's looks run by hand:
timers go to `later' (newest first)."
  (declare (indent 0))
  `(ghostherd-tests--with-leads
     (let ((ghostherd-tests--shrec nil)
           (ghostherd-tests--shrec-screen (ghostherd-test--claude-screen "❯ "))
           (ghostherd--shell-pending (make-hash-table :test 'equal))
           (ghostherd-submit-check-after nil)
           (ghostherd--log nil))
       ,@body)))

(defun ghostherd-tests--run-later (later)
  "Run the timers in LATER, newest first as pushed, oldest first."
  (dolist (call (reverse later)) (apply (car call) (cdr call))))

(ert-deftest ghostherd-test-a-bang-command-is-the-clis-own-rule ()
  "A `!' command: `!' its very first character and a command after it on
that line, as the CLIs take `!' on an empty prompt.  The command is the
rest, the `!' and the blanks after it off; several lines are one script.
A leading space, `!' alone, a `!' later on, a full-width `！' are words."
  (should (equal (ghostherd--bang-command "!ls -la") "ls -la"))
  (should (equal (ghostherd--bang-command "! bash /tmp/x.sh --site 75ue-2") "bash /tmp/x.sh --site 75ue-2"))
  (should (equal (ghostherd--bang-command "!\tcd /tmp && ls\n") "cd /tmp && ls"))
  (should (equal (ghostherd--bang-command "!cd /tmp\n  npm test\n\necho done \n\n") "cd /tmp\n  npm test\n\necho done"))
  (should (equal (ghostherd--bang-command "!!x") "!x"))
  (dolist (text '(" !ls" "\n!ls" "!" "!   " "!\nls" "say !ls" "！ls" "" nil 42))
    (should-not (ghostherd--bang-command text))))

(ert-deftest ghostherd-test-shell-mode-is-read-off-the-input-box ()
  "Shell mode: `!' where the box's prompt sign was -- claude's and agy's,
grok's inside its frame, empty or with a command.  Not a `!' the box
holds as text after its sign, nor a command up in the history."
  (dolist (box '("! " "!  echo b" "! [Pasted text #2]"))
    (should (ghostherd--shell-mode-p (ghostherd-test--claude-screen box))))
  (should (ghostherd--shell-mode-p (replace-regexp-in-string "│ ❯ " "│ ! echo hi" ghostherd-test--grok-welcome-screen t t)))
  (should (ghostherd--shell-mode-p (concat ghostherd-test--rule "\n!\n" ghostherd-test--rule "\n activated bash mode · esc to cancel")))
  (dolist (box '("❯ " "❯ !echo a" "❯ hello !echo e"))
    (should-not (ghostherd--shell-mode-p (ghostherd-test--claude-screen box "! echo old\n  ⎿  old\n\n"))))
  (should-not (ghostherd--shell-mode-p ghostherd-test--grok-welcome-screen))
  (should-not (ghostherd--shell-mode-p ghostherd-tests--permission-screen))
  (should-not (ghostherd--shell-mode-p nil)))

(ert-deftest ghostherd-test-the-herds-paste-never-opens-shell-mode ()
  "Every text the herd pastes reaches the backend with a space before a
leading `!' (claude takes a paste that begins with one as its shell
mode); other text as it was.  On tmux the screen is read first."
  (ghostherd-tests--with-shell
    (let ((s (ghostherd-tests--shrec-session :name "a")))
      (ghostherd--host-send-text s "!touch /tmp/pwned" t)
      (ghostherd--host-send-text s "hi !there" nil)
      (should (equal ghostherd-tests--shrec '((:text " !touch /tmp/pwned" t) (:text "hi !there" nil))))))
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--tmux-session :name "t")))
      (ghostherd-tests--with-tmux nil
        (ghostherd--host-send-text s "!ls" t))
      (should (equal (car ghostherd-tests--tmux-calls) '("capture-pane" "-p" "-t" "=gh-abc-rev:")))
      (should (equal (nth 4 (ghostherd-tests--tmux-call "load-buffer")) " !ls")))))

(ert-deftest ghostherd-test-a-box-in-shell-mode-takes-no-paste ()
  "A box in shell mode -- you typed `!' in the terminal and left it --
takes nothing the herd pastes: submitted, it would run as a command.
A `user-error' says so, and nothing is sent."
  (ghostherd-tests--with-shell
    (let ((s (ghostherd-tests--shrec-session :name "a"))
          (ghostherd-tests--shrec-screen ghostherd-tests--claude-shell))
      (should (string-match-p "input box is in shell mode"
                              (cadr (should-error (ghostherd-send s "please go on" t) :type 'user-error))))
      (should-error (ghostherd--host-send-text s "/usage" t) :type 'user-error)
      (should-not ghostherd-tests--shrec))))

(ert-deftest ghostherd-test-your-bang-command-types-bang-then-pastes ()
  "Your `!' command: `!' typed as a key; once the box shows shell mode,
the command alone pasted and submitted -- not a paste the herd's guard
would refuse, nor a lead's notes or owed role before it, which stay
owed.  The log keeps your words; the herd's mail waits meanwhile."
  (ghostherd-tests--with-shell
    (let* ((lead (ghostherd-tests--shrec-session :name "lead-p" :project "/p/"))
           (ghostherd-tests--shrec-screen (ghostherd-test--claude-screen "❯ ")))
      (puthash "lead-p" "lead" ghostherd--roles)
      (puthash "lead-p" "You are the lead." ghostherd--role-owed)
      (puthash "/p/" '(("rd" . "fix it")) ghostherd--lead-unheard)
      (should (equal (ghostherd-user-shell-command lead "! bash /tmp/x.sh\n") "bash /tmp/x.sh"))
      (should (equal ghostherd-tests--shrec '((:keys "!"))))
      (should (gethash "lead-p" ghostherd--shell-pending))
      (should (ghostherd--herd-held-p '(:to "lead-p" :body "hi")))
      (should (ghostherd--herd-held-p '(:to "lead-p" :body "hi" :handoff t)))
      ;; Not yet in shell mode: it looks again.
      (let ((first later))
        (setq later nil)
        (ghostherd-tests--run-later first))
      (should (equal ghostherd-tests--shrec '((:keys "!"))))
      (should (= (length later) 1))
      (setq ghostherd-tests--shrec-screen ghostherd-tests--claude-shell)
      (let ((next later))
        (setq later nil)
        (ghostherd-tests--run-later next))
      (should (equal ghostherd-tests--shrec '((:keys "!") (:text "bash /tmp/x.sh" t))))
      (should-not (gethash "lead-p" ghostherd--shell-pending))
      (should-not (ghostherd--herd-held-p '(:to "lead-p" :body "hi")))
      (should (equal (gethash "lead-p" ghostherd--role-owed) "You are the lead."))
      (should (gethash "/p/" ghostherd--lead-unheard))
      (should (eq (ghostherd-session-state lead) 'working))
      (should (seq-find (lambda (e) (equal (ghostherd-log-entry-text e) "← ! bash /tmp/x.sh")) ghostherd--log)))))

(ert-deftest ghostherd-test-your-bang-command-is-watched-as-a-shell-submit ()
  "Submitted, your command is made sure of as any text is -- Return
again while it sits in the box as \"! cmd\" -- but not lost for a hook
that never says a turn began: claude's and grok's say nothing of one."
  (ghostherd-tests--with-shell
    (let* ((s (ghostherd-tests--shrec-session :name "a"))
           (ghostherd-submit-check-after 1.5))
      (setq ghostherd-tests--shrec-screen ghostherd-tests--claude-shell)
      (ghostherd--shell-command-go s "echo hi" "!echo hi")
      (should (plist-get (ghostherd--submit-of s) :shell))
      (setq ghostherd-tests--shrec-screen (ghostherd-test--claude-screen "! echo hi"))
      (should (ghostherd--unsent-p ghostherd-tests--shrec-screen "echo hi"))
      (setf (ghostherd-session-state s) 'idle)
      (cl-letf (((symbol-function 'ghostherd--hooked-p) (lambda (_) t)))
        (ghostherd--confirm-submit s "echo hi" 1)
        (should (equal (car (last ghostherd-tests--shrec)) '(:keys "return")))
        (setq ghostherd-tests--shrec-screen (ghostherd-test--claude-screen "❯ "))
        (ghostherd--confirm-submit s "echo hi" 0)
        (ghostherd--confirm-submit s "echo hi" 0)
        (should (eq (plist-get (ghostherd--submit-of s) :state) 'confirmed))))))

(ert-deftest ghostherd-test-your-bang-command-gives-up-and-takes-its-bang-back ()
  "No shell mode in all its looks: the command is not run, you are told
why, the log says so.  A `!' that landed after text in the box -- a
draft there -- is taken back, so the draft is as it was."
  (ghostherd-tests--with-shell
    (let* ((s (ghostherd-tests--shrec-session :name "a"))
           (ghostherd--shell-looks 1)
           (why nil))
      (setq ghostherd-tests--shrec-screen (ghostherd-test--claude-screen "❯ draft"))
      (ghostherd-user-shell-command s "!ls" (lambda (w) (setq why w)))
      (setq ghostherd-tests--shrec-screen (ghostherd-test--claude-screen "❯ draft!"))
      (dotimes (_ 3) (let ((now later)) (setq later nil) (ghostherd-tests--run-later now)))
      (should (equal ghostherd-tests--shrec '((:keys "!") (:keys "backspace"))))
      (should (string-match-p "a's input box has text in it" why))
      (should-not (gethash "a" ghostherd--shell-pending))
      (should (seq-find (lambda (e) (string-prefix-p "! not run" (ghostherd-log-entry-text e))) ghostherd--log))
      ;; A `!' that did not land after the draft is not taken back.
      (setq ghostherd-tests--shrec nil why nil)
      (setq ghostherd-tests--shrec-screen (ghostherd-test--claude-screen "❯ draft"))
      (ghostherd-user-shell-command s "!ls" (lambda (w) (setq why w)))
      (dotimes (_ 3) (let ((now later)) (setq later nil) (ghostherd-tests--run-later now)))
      (should (equal ghostherd-tests--shrec '((:keys "!"))))
      (should (string-match-p "did not go into its shell mode" why)))))

(ert-deftest ghostherd-test-your-bang-command-wants-the-prompt ()
  "Not at its prompt -- a menu, a dialog, its box in shell mode already --
or still taking the last command, no `!' is typed: a `user-error'.  A
kind with no shell mode, or words that are no `!' command: nil, nothing
sent, and the caller sends them as words."
  (ghostherd-tests--with-shell
    (let ((s (ghostherd-tests--shrec-session :name "a")))
      (dolist (screen (list ghostherd-tests--permission-screen ghostherd-test--claude-trust
                            ghostherd-tests--claude-shell))
        (setq ghostherd-tests--shrec-screen screen)
        (should-error (ghostherd-user-shell-command s "!ls") :type 'user-error))
      (setq ghostherd-tests--shrec-screen (ghostherd-test--claude-screen "❯ "))
      (puthash "a" t ghostherd--shell-pending)
      (should-error (ghostherd-user-shell-command s "!ls") :type 'user-error)
      (remhash "a" ghostherd--shell-pending)
      (should-not (ghostherd-user-shell-command s " !ls"))
      (should-not (ghostherd-user-shell-command s "hello"))
      (should-not (ghostherd-user-shell-command
                   (ghostherd-tests--shrec-session :name "sh" :kind 'shell) "!ls"))
      (should-not ghostherd-tests--shrec)
      (should-not later)))
  (dolist (kind '(claude grok agy))
    (should (plist-get (ghostherd--spec kind) :shell-mode))))

(defun ghostherd-tests--names-p (form symbol)
  "Non-nil when FORM names SYMBOL anywhere in it."
  (cond ((eq form symbol) t)
        ((consp form) (or (ghostherd-tests--names-p (car form) symbol)
                          (ghostherd-tests--names-p (cdr form) symbol)))
        ((vectorp form) (seq-some (lambda (x) (ghostherd-tests--names-p x symbol)) form))))

(defun ghostherd-tests--source-forms ()
  "Every top-level form of ghostherd's own sources -- not its tests -- as
(FILE NAME . FORM), NAME what it defines or nil."
  (let ((dir (file-name-directory (locate-library "ghostherd-comm.el")))
        forms)
    (dolist (file (directory-files dir t "\\`ghostherd.*\\.el\\'"))
      (unless (string-match-p "-tests\\.el\\'\\|test-helpers" file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (condition-case nil
              (while t
                (let ((form (read (current-buffer))))
                  (push (cons (file-name-nondirectory file)
                              (cons (and (consp form)
                                         (memq (car form) '(defun cl-defun defmacro defsubst cl-defmethod
                                                                  cl-defgeneric defvar defconst defcustom))
                                         (cadr form))
                                    form))
                        forms)))
            (end-of-file nil)))))
    forms))

(defun ghostherd-tests--named-by (symbol forms)
  "What, in FORMS (`ghostherd-tests--source-forms'), names SYMBOL, but its
own definition: the definitions' names, sorted, the file for a form that
defines nothing."
  (sort (delete-dups
         (delq nil (mapcar (lambda (f)
                             (when (and (not (eq (cadr f) symbol))
                                        (ghostherd-tests--names-p (cddr f) symbol))
                               (or (cadr f) (intern (car f)))))
                           forms)))
        #'string<))

(defun ghostherd-tests--calls (fn forms)
  "The definitions in FORMS that call FN: name it first in a form, or hand
it to `funcall', `apply' or `call-interactively'.  A key bound to it does not."
  (let (found)
    (dolist (f forms)
      (let ((walk nil))
        (setq walk (lambda (x)
                     ;; Quoted data -- a help table, a menu's rows -- is no call.
                     (when (and (consp x) (not (eq (car x) 'quote)))
                       (when (or (eq (car x) fn)
                                 (and (memq (car x) '(funcall apply call-interactively))
                                      (member (cadr x) (list (list 'function fn) (list 'quote fn)))))
                         (push (or (cadr f) (car f)) found))
                       ;; Each element, not each tail: a tail is no form.
                       (while (consp x) (funcall walk (car x)) (setq x (cdr x))))))
        (unless (eq (cadr f) fn) (funcall walk (cddr f)))))
    (delete-dups found)))

(defun ghostherd-tests--keys-with-bang (forms)
  "The definitions in FORMS where a key sender is handed a `!'."
  (let (found)
    (dolist (f forms)
      (let ((walk nil))
        (setq walk (lambda (x)
                     (when (consp x)
                       (when (and (memq (car x) '(ghostherd--host-send-keys ghostherd-send-keys
                                                                            ghostherd-backend-send-keys))
                                  (ghostherd-tests--names-p
                                   (mapcar (lambda (a) (if (equal a "!") 'BANG a))
                                           (flatten-tree (cdr x)))
                                   'BANG))
                         (push (cadr f) found))
                       (while (consp x) (funcall walk (car x)) (setq x (cdr x))))))
        (funcall walk (cddr f))))
    (delete-dups found)))

(ert-deftest ghostherd-test-shell-mode-only-from-the-user ()
  "Shell mode runs a command asking no permission, so only your own words
reach it -- never anything an agent sends or the herd composes, however
its text begins.  By what they do: an agent's every way to text an agent
-- its prompt and send through ghostel_cmd, a message, a handoff, an ask,
the herd's mail and its commands, a relay's or nudge's `ghostherd-tell'
-- with \"!touch /tmp/pwned\": no `!' key, no text beginning with `!'; and
into a box in shell mode, nothing at all.  By how they are built: only
`ghostherd--host-send-text' (the guarded paste) and
`ghostherd--shell-command-go' paste into an agent, the second reached from
the page's and Emacs's own prompt commands alone, and only
`ghostherd-user-shell-command' types `!'.  A new caller fails here until
it is shown to be yours."
  (let ((bang "!touch /tmp/pwned"))
    (dolist (screen (list (ghostherd-test--claude-screen "❯ ") ghostherd-tests--claude-shell))
      (ghostherd-tests--with-shell
        (let ((s (ghostherd-tests--shrec-session :name "a" :project "/p/"))
              (ghostherd-tests--shrec-screen screen)
              (ghostherd--asks-open (make-hash-table :test 'equal))
              (ghostherd--asks-settled (make-hash-table :test 'equal))
              (ghostherd--herd-asks nil))
          (ghostherd-tests--shrec-session :name "lead-p" :project "/p/")
          (puthash "lead-p" "lead" ghostherd--roles)
          (cl-letf (((symbol-function 'run-with-timer) #'ignore)
                    ((symbol-function 'ghostherd-herd-client) (lambda () "/p/bin/herd"))
                    ((symbol-function 'ghostherd-memory-request-async) #'ignore))
            (dolist (path (list (lambda () (ghostherd-send s bang t))
                                (lambda () (ghostherd-send s bang nil))
                                (lambda () (ghostherd-prompt s bang))
                                (lambda () (ghostherd-message "rd" s bang :submit t))
                                (lambda () (ghostherd-handoff s bang))
                                (lambda () (ghostherd-handoff s bang "rd"))
                                (lambda () (ghostherd-cmd-send "a" bang))
                                (lambda () (ghostherd-cmd-prompt "a" bang))
                                (lambda () (ghostherd-cmd-message "a" bang "rd"))
                                (lambda () (ghostherd-tell s bang nil t))
                                (lambda () (ghostherd-tell s bang))
                                (lambda () (ghostherd-tell (ghostherd-get "lead-p") bang))
                                (lambda () (ghostherd-lead-say "/p/" bang))
                                (lambda () (ghostherd--herd-deliver-one `(:id "1" :from "rd" :to "a" :body ,bang :submit t)))
                                (lambda () (ghostherd--herd-deliver-one `(:id "2" :from "rd" :to "a" :body ,bang :handoff t)))
                                (lambda () (ghostherd--herd-deliver-one `(:id "3" :from "rd" :to "a" :body ,bang :ask "1a2b3c4d")))
                                (lambda () (ghostherd--herd-run-command `(:id "4" :op "prompt" :session "a" :args (:body ,bang))))))
              (ignore-errors (funcall path))))
          (let ((texts (mapcar #'cadr (seq-filter (lambda (e) (eq (car e) :text)) ghostherd-tests--shrec)))
                (keys (apply #'append (mapcar #'cdr (seq-filter (lambda (e) (eq (car e) :keys)) ghostherd-tests--shrec)))))
            (should-not (member "!" keys))
            (should-not (seq-find (lambda (text) (string-prefix-p "!" text)) texts))
            (if (ghostherd--shell-mode-p screen)
                (should-not ghostherd-tests--shrec)
              ;; Each of the 17 did send: the test reaches what it says it does.
              (should (= (length texts) 17))
              (should (seq-find (lambda (text) (string-search " !touch /tmp/pwned" text)) texts))))))))
  (let ((forms (ghostherd-tests--source-forms)))
    (should (equal (ghostherd-tests--named-by 'ghostherd-backend-send-text forms)
                   '(ghostherd--host-send-text ghostherd--shell-command-go)))
    (should (equal (ghostherd-tests--named-by 'ghostherd--shell-command-go forms)
                   '(ghostherd--shell-command-look)))
    (should (equal (ghostherd-tests--named-by 'ghostherd--shell-command-look forms)
                   '(ghostherd-user-shell-command)))
    (should (equal (ghostherd-tests--named-by 'ghostherd-user-shell-command forms)
                   '(ghostherd-memory-page--shell-command ghostherd-prompt-interactive ghostherd-sidebar-prompt)))
    ;; The page's: what you send from it -- an agent's card, the room, the
    ;; Inbox -- and nothing else.
    (should (equal (ghostherd-tests--named-by 'ghostherd-memory-page--shell-command forms)
                   '(ghostherd-memory-page--agent-act ghostherd-memory-page--say-shell-command)))
    (should (equal (ghostherd-tests--named-by 'ghostherd-memory-page--say-shell-command forms)
                   '(ghostherd-memory-page--lead-say ghostherd-memory-page--primary-say)))
    (dolist (page '(ghostherd-memory-page--agent-act ghostherd-memory-page--lead-say
                                                     ghostherd-memory-page--primary-say))
      (should (equal (ghostherd-tests--named-by page forms) '(ghostherd-memory-page--handle))))
    ;; Emacs's: commands you run, bound to keys -- no code calls them.
    (dolist (cmd '(ghostherd-prompt-interactive ghostherd-sidebar-prompt))
      (should-not (ghostherd-tests--calls cmd forms)))
    (should (equal (ghostherd-tests--keys-with-bang forms) '(ghostherd-user-shell-command)))))

(provide 'ghostherd-comm-tests)
;;; ghostherd-comm-tests.el ends here

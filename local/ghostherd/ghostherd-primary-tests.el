;;; ghostherd-primary-tests.el --- Tests for ghostherd-primary.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Part of ghostherd's tests; ghostherd-tests.el runs them all.  This
;; file alone:
;;
;;   emacs --batch --init-directory=~/.emacs.d/emacs-home/ \
;;         -L . -L ../xwapp -l ghostherd-primary-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ghostherd-test-helpers)

(declare-function ghostherd-memory-page--memory-proposals-tick "ghostherd-memory-page" (key))
(declare-function ghostherd-memory-page--primary-say "ghostherd-memory-page" (intent))
(declare-function ghostherd-memory-page--send-herd "ghostherd-memory-page" ())

;;; Relay: an heir takes an agent's name and its work

(ert-deftest ghostherd-test-relay-hands-the-name-and-the-work ()
  "rd on claude, relayed to grok: grok takes the name, the notes and the
project, the second of the name; it is told the brief; the ask rd was
answering is its own, not failed with rd; rd's link is forgotten."
  (ghostherd-tests--relaying
    (let ((rd (ghostherd-tests--session :name "rd" :kind 'claude :state 'idle :backend 'fake
                                        :project "/p/shop/" :notes "implementer"
                                        :args '("--model" "opus"))))
      (puthash "a1" "rd" ghostherd--asks-open)
      (ghostherd--herd-take-links '((:session "rd" :cli "claude" :conversation "c1")))
      (should (equal (ghostherd-session-conversation rd) "c1"))
      (should-not (ghostherd-relay "rd" 'grok "it wedged"))
      (let ((ask (cadr (assoc "herd_relay_brief" requests))))
        (should (equal (plist-get ask :name) "rd"))
        (should (equal (list (plist-get ask :from) (plist-get ask :to)) '("claude" "grok")))
        (should (equal (plist-get ask :generation) 2))
        (should (equal (plist-get ask :reason) "it wedged"))
        ;; Idle, its usage left: its turn had ended, and the heir waits.
        (should (eq (plist-get ask :cut) :json-false))
        (should (equal (plist-get ask :notes) "implementer"))
        (should (equal (plist-get ask :project) "/p/shop/"))
        (should (equal (plist-get ask :client) "/p/bin/herd")))
      (should (equal (cadr (assoc "herd_unlink" requests)) '(:session "rd")))
      ;; Not failed: no settle for a1 when rd was killed.
      (should-not (assoc "herd_settle" requests))
      (should (eq (car spawned) 'grok))
      (let ((plist (cdr spawned)))
        (should (equal (plist-get plist :name) "rd"))
        (should (equal (plist-get plist :notes) "implementer"))
        (should (equal (plist-get plist :project) "/p/shop/"))
        ;; Claude's flags are nothing to grok.
        (should (equal (plist-get plist :args) nil))
        (let ((line (plist-get plist :lineage)))
          (should (= (length line) 1))
          (should (equal (alist-get 'kind (car line)) "claude"))
          (should (equal (alist-get 'conversation (car line)) "c1"))
          (should (equal (alist-get 'reason (car line)) "it wedged"))))
      (let ((heir (ghostherd-get "rd")))
        (should-not (eq heir rd))
        (should (eq (ghostherd-session-kind heir) 'grok))
        (should (= (ghostherd-session-generation heir) 2))
        (should (equal told (list (list 'prompt heir "BRIEF" nil))))
        (should (equal (gethash "a1" ghostherd--asks-open) "rd"))
        (should (zerop (hash-table-count ghostherd--relay-held)))
        ;; Its predecessor's link is gone: not its conversation.
        (should-not (ghostherd-session-conversation heir))
        (should-not (gethash "rd" ghostherd--relaying))
        (should (seq-find (lambda (e) (string-match-p "rd II, on grok: took over from claude -- it wedged"
                                                      (ghostherd-log-entry-text e)))
                          ghostherd--log))))))

(ert-deftest ghostherd-test-relay-holds-the-asks-until-the-heir-is-told ()
  "Between the old one's going and the heir's being told, the ask is
neither failed, nor taken by a tick as an ask nobody watched, nor
closed by the heir going idle at its first prompt."
  (ghostherd-tests--relaying
    (setq hold t)
    (ghostherd-tests--session :name "rd" :kind 'claude :state 'working :backend 'fake)
    (puthash "a1" "rd" ghostherd--asks-open)
    (ghostherd-relay "rd" 'agy)
    (let ((heir (ghostherd-get "rd")))
      (should (equal (gethash "a1" ghostherd--relay-held) "rd"))
      (should-not (gethash "a1" ghostherd--asks-open))
      (ghostherd--herd-take-asks '((:id "a1" :from "lead" :to "rd" :status "delivered")))
      (setf (ghostherd-session-state heir) 'starting)
      (ghostherd--set-state heir 'idle)
      (should-not (assoc "herd_settle" requests))
      (should-not (gethash "a1" ghostherd--asks-open))
      (funcall (car ready))
      (should (equal (gethash "a1" ghostherd--asks-open) "rd"))
      (should-not (gethash "a1" ghostherd--relay-held))
      ;; Its turn done, the ask closes with its answer, as any taker's.
      (ghostherd--set-state heir 'working)
      (ghostherd--set-state heir 'idle)
      (should (equal (car (assoc "herd_settle" requests)) "herd_settle")))))

(ert-deftest ghostherd-test-relay-an-heir-that-never-comes-fails-its-asks ()
  (ghostherd-tests--relaying
    (setq hold t)
    (ghostherd-tests--session :name "rd" :kind 'claude :state 'idle :backend 'fake)
    (puthash "a1" "rd" ghostherd--asks-open)
    (ghostherd-relay "rd" 'grok)
    (funcall (cadr ready))
    (should (equal (cadr (assoc "herd_settle" requests))
                   '(:id "a1" :screen "" :dead :json-false :error "rd's heir never came to its prompt")))
    (should-not (gethash "a1" ghostherd--relay-held))))

(ert-deftest ghostherd-test-relay-of-a-lead ()
  "A lead's heir leads: its role in its flags where its CLI takes one,
in its first message where not; told what the project works on again;
on the same CLI, its own flags kept, less what picked its conversation."
  (ghostherd-tests--relaying
    (pcase-dolist (`(,to ,args ,role)
                   `((agy nil t)
                     (grok ("--rules" ,ghostherd--lead-role) nil)
                     (claude ("--model" "opus" "--append-system-prompt" ,ghostherd--lead-role) nil)))
      (setq told nil)
      (remhash "lead-shop" ghostherd--sessions)
      (let ((lead (ghostherd-tests--session :name "lead-shop" :kind 'claude :state 'idle :backend 'fake
                                            :project "/p/shop/"
                                            :args '("--model" "opus" "-r" "c0"
                                                    "--append-system-prompt" "an old role"))))
        (puthash "lead-shop" "lead" ghostherd--roles)
        (ghostherd--herd-take-links '((:session "lead-shop" :cli "claude" :conversation "c0")))
        (puthash "lead-shop" '("what it was told") ghostherd--lead-work-told)
        (ghostherd-relay lead to)
        (should (equal (plist-get (cdr spawned) :role) "lead"))
        (should (equal (plist-get (cdr spawned) :args) args))
        (should-not (gethash "lead-shop" ghostherd--lead-work-told))
        ;; Even on the same CLI, the old conversation is not the heir's.
        (should-not (ghostherd-session-conversation (ghostherd-get "lead-shop")))
        (pcase-let ((`(,fn ,_s ,text ,given) (car told)))
          (should (eq fn 'tell-by-herd))       ; the brief is the herd's, not the user's
          (should (equal text "BRIEF"))
          (if role
              (should (string-prefix-p ghostherd--lead-role given))
            (should-not given)))))))

(ert-deftest ghostherd-test-relay-the-third-of-the-name ()
  "Each relay adds one to the line; the brief is asked for the next."
  (ghostherd-tests--relaying
    (ghostherd-tests--session :name "rd" :kind 'claude :state 'idle :backend 'fake)
    (ghostherd-relay "rd" 'grok)
    (ghostherd-relay "rd" 'agy)
    (let ((heir (ghostherd-get "rd")))
      (should (= (ghostherd-session-generation heir) 3))
      (should (equal (mapcar (lambda (a) (alist-get 'kind a)) (ghostherd-session-lineage heir))
                     '("claude" "grok")))
      (should (equal (mapcar (lambda (r) (plist-get (cadr r) :generation))
                             (seq-filter (lambda (r) (equal (car r) "herd_relay_brief")) requests))
                     '(2 3))))
    (should (equal (mapcar #'ghostherd--roman '(1 2 3 4 9 14 39 40)) '("I" "II" "III" "IV" "IX" "XIV" "XXXIX" "40")))))

(ert-deftest ghostherd-test-relay-refuses ()
  (ghostherd-tests--relaying
    (ghostherd-tests--session :name "sh" :kind 'shell :state 'idle)
    (ghostherd-tests--session :name "rd" :kind 'claude :state 'idle :backend 'fake)
    (should (string-match-p "a shell, not an agent" (cadr (should-error (ghostherd-relay "sh" 'grok) :type 'user-error))))
    (should (string-match-p "shell runs no agent" (cadr (should-error (ghostherd-relay "rd" 'shell) :type 'user-error))))
    (puthash "rd" t ghostherd--relaying)
    (should (string-match-p "already being relayed" (cadr (should-error (ghostherd-relay "rd" 'grok) :type 'user-error))))
    (remhash "rd" ghostherd--relaying)
    (cl-letf (((symbol-function 'ghostherd-memory-rpc-url) #'ignore))
      (should (string-match-p "needs the herd.s sidecar" (cadr (should-error (ghostherd-relay "rd" 'grok) :type 'user-error)))))
    (should-not spawned)
    ;; The sidecar fails: rd stays, and can be relayed again.
    (setq brief-error "no such transcript")
    (ghostherd-relay "rd" 'grok)
    (should-not spawned)
    (should (eq (ghostherd-session-kind (ghostherd-get "rd")) 'claude))
    (should-not (gethash "rd" ghostherd--relaying))
    (should (string-match-p "relay to grok failed: no such transcript" (ghostherd-log-entry-text (car ghostherd--log))))))

(ert-deftest ghostherd-test-relay-of-one-that-left-meanwhile ()
  (ghostherd-tests--relaying
    (let ((rd (ghostherd-tests--session :name "rd" :kind 'claude :state 'idle :backend 'fake)))
      (remhash "rd" ghostherd--sessions)
      (should-not (ghostherd--relay-hand rd 'grok "BRIEF" nil))
      (should-not spawned))))

(ert-deftest ghostherd-test-usage-agy-used-up-is-per-model ()
  "An agy's used-up is by its own model's group: on Gemini 3.8 Flash (gem,
with quota) it is not used up because Gemini 4 Argon (g4) is spent; on
Argon it is.  A model that cannot be read is not called used up, and grok
with its one group is unchanged."
  (ghostherd-tests--with-herd ()
    (let ((ghostherd-usage--cache nil)
          (ghostherd--screens (make-hash-table :test 'equal))
          (flash (ghostherd-tests--session :name "jf" :kind 'agy :state 'idle :backend 'fake))
          (argon (ghostherd-tests--session :name "ja" :kind 'agy :state 'idle :backend 'fake))
          (blank (ghostherd-tests--session :name "jb" :kind 'agy :state 'idle :backend 'fake)))
      (setf (alist-get 'agy ghostherd-usage--cache)
            (list :windows (list (list :label "gem-wk" :remaining 100 :resets-at nil)
                                 (list :label "gem-5h" :remaining 100 :resets-at nil)
                                 (list :label "g4-wk" :remaining 0 :resets-at (+ (float-time) 600000)))
                  :fetched (float-time)))
      (puthash "jf" "? for shortcuts    Gemini 3.8 Flash · high" ghostherd--screens)
      (puthash "ja" "? for shortcuts    Gemini 4 Argon · medium" ghostherd--screens)
      (puthash "jb" "? for shortcuts\n> \n" ghostherd--screens)
      ;; The agy kind as a whole is used up (g4 is spent) ...
      (should (ghostherd-usage-used-up 'agy))
      ;; ... but the Flash agent, on gem, is not.
      (should-not (ghostherd-usage-session-used-up flash))
      (should-not (ghostherd-usage-session-used-up-text flash))
      ;; The Argon agent, on g4, is.
      (should (ghostherd-usage-session-used-up argon))
      (should (string-match-p "g4-wk" (ghostherd-usage-session-used-up-text argon)))
      ;; A model that cannot be read: not called used up.
      (should-not (ghostherd-usage-session-used-up blank))
      ;; grok has one group, so its session is used up when the kind is.
      (let ((g (ghostherd-tests--session :name "gk" :kind 'grok :state 'idle :backend 'fake)))
        (ghostherd-tests--used-up 'grok)
        (should (ghostherd-usage-session-used-up g))))))

(ert-deftest ghostherd-test-usage-agy-footer-credits-not-used-up ()
  "agy's `· AI: Out of credits' footer is its paid overage, not the model
quota: a fresh session with the quota full shows it and its requests still
go through, so it never, on its own, marks a session used up.  Only the
per-group usage poll (feadc38) decides -- used up when the agent's own
group is spent, not when it has quota, whatever the footer says and
whether or not the poll has a reading."
  (ghostherd-tests--with-herd ()
    (let ((ghostherd-usage--cache nil)
          (ghostherd--screens (make-hash-table :test 'equal))
          (s (ghostherd-tests--session :name "ja" :kind 'agy :state 'idle :backend 'fake)))
      (puthash "ja" (concat "⏺ some work\n────\n>\n────\n"
                            "? for shortcuts    accept-edits · Gemini 4 Argon · medium · AI: Out of credits")
               ghostherd--screens)
      ;; No poll reading at all: the footer does not stand in for one.
      (should-not (ghostherd-usage-session-used-up s))
      (should-not (ghostherd-usage-session-used-up-text s))
      ;; A fresh poll with the g4 group full: not used up, footer ignored.
      (setf (alist-get 'agy ghostherd-usage--cache)
            (list :windows (list (list :label "g4-wk" :remaining 100 :resets-at nil))
                  :fetched (float-time)))
      (should-not (ghostherd-usage-session-used-up s))
      (should-not (ghostherd-usage-session-used-up-text s))
      ;; The g4 group spent: used up -- by the poll, not the footer.
      (setf (alist-get 'agy ghostherd-usage--cache)
            (list :windows (list (list :label "g4-wk" :remaining 0
                                       :resets-at (+ (float-time) 600000)))
                  :fetched (float-time)))
      (should (ghostherd-usage-session-used-up s))
      (should (string-match-p "g4-wk" (ghostherd-usage-session-used-up-text s))))))

(ert-deftest ghostherd-test-relay-request-guards ()
  "`herd relay's guardrails: only a project's lead, only its own project's
agents, not itself, not one (or itself) already in a relay; and when it
passes it reaches `ghostherd-relay' with the kind and reason -- the kind
left empty taking the herd's default."
  (ghostherd-tests--with-herd ((ghostherd--roles (make-hash-table :test 'equal))
                               (ghostherd--relaying (make-hash-table :test 'equal))
                               (ghostherd-usage--cache nil))
    (let (relayed)
      (cl-letf (((symbol-function 'ghostherd-relay)
                 (lambda (s kind &optional reason)
                   (setq relayed (list (ghostherd-session-name s) kind reason)))))
        (let ((lead (ghostherd-tests--session :name "lead-p" :kind 'claude :project "/p/" :backend 'fake))
              (rd (ghostherd-tests--session :name "rd" :kind 'agy :project "/p/" :backend 'fake))
              (x (ghostherd-tests--session :name "x" :kind 'agy :project "/q/" :backend 'fake))
              (nolead (ghostherd-tests--session :name "nolead" :kind 'claude :backend 'fake))
              (noproj (ghostherd-tests--session :name "noproj" :kind 'agy :backend 'fake)))
          (puthash (ghostherd-session-id lead) "lead" ghostherd--roles)
          (puthash (ghostherd-session-id nolead) "lead" ghostherd--roles)
          ;; not a lead
          (should (string-match-p "not a lead" (ghostherd--relay-request "rd" "lead-p" "grok" nil)))
          ;; itself
          (should (string-match-p "itself" (ghostherd--relay-request "lead-p" "lead-p" "grok" nil)))
          ;; another project's agent
          (should (string-match-p "own project" (ghostherd--relay-request "lead-p" "x" "grok" nil)))
          ;; two project-less sessions do not read as the same project
          (should (string-match-p "own project" (ghostherd--relay-request "nolead" "noproj" "grok" nil)))
          ;; target already relaying
          (puthash (ghostherd-session-id rd) t ghostherd--relaying)
          (should (string-match-p "already being relayed" (ghostherd--relay-request "lead-p" "rd" "grok" nil)))
          (remhash (ghostherd-session-id rd) ghostherd--relaying)
          ;; the asker itself in a relay
          (puthash (ghostherd-session-id lead) t ghostherd--relaying)
          (should (string-match-p "being relayed itself" (ghostherd--relay-request "lead-p" "rd" "grok" nil)))
          (remhash (ghostherd-session-id lead) ghostherd--relaying)
          ;; unknown target
          (should (string-match-p "no such agent" (ghostherd--relay-request "lead-p" "ghost" "grok" nil)))
          ;; passes: reaches ghostherd-relay with the kind and reason
          (setq relayed nil)
          (should (equal (ghostherd--relay-request "lead-p" "rd" "grok" "stuck") "relaying rd to grok"))
          (should (equal relayed '("rd" grok "stuck")))
          ;; kind omitted: the herd's default is used, not refused
          (setq relayed nil)
          (should (string-prefix-p "relaying rd to" (ghostherd--relay-request "lead-p" "rd" "" nil)))
          (should (eq (nth 1 relayed) (ghostherd-relay-default-kind rd))))))))

(ert-deftest ghostherd-test-relay-goes-where-usage-is-left ()
  (ghostherd-tests--relaying
    (let ((rd (ghostherd-tests--session :name "rd" :kind 'claude :state 'idle :backend 'fake)))
      (should (eq (ghostherd-relay-default-kind rd) 'grok))
      (ghostherd-tests--used-up 'grok)
      (should (eq (ghostherd-relay-default-kind rd) 'agy))
      (ghostherd-tests--used-up 'agy)
      (should (eq (ghostherd-relay-default-kind rd) 'claude))
      ;; A reading three hours old, or a window that has reset since: not used up.
      (ghostherd-tests--used-up 'grok 3600 (* 3 3600))
      (should (eq (ghostherd-relay-default-kind rd) 'grok))
      (ghostherd-tests--used-up 'grok -60)
      (should (eq (ghostherd-relay-default-kind rd) 'grok))
      ;; Under 1% left is used up; 5% is not.
      (setf (alist-get 'grok ghostherd-usage--cache)
            (list :windows (list (list :label "day" :remaining 5 :resets-at (+ (float-time) 600)))
                  :fetched (float-time)))
      (should-not (ghostherd-usage-used-up 'grok))
      (setf (plist-get (car (plist-get (alist-get 'grok ghostherd-usage--cache) :windows)) :remaining) 0.5)
      (should (ghostherd-usage-used-up 'grok))
      (setf (alist-get 'grok ghostherd-usage--cache) nil)
      ;; Used up, it says so, and is the reason by default.
      (ghostherd-tests--used-up 'claude 600 0 "5h")
      (should (string-match-p "\\`its claude usage (5h) is used up, until [0-9][0-9]:[0-9][0-9]\\'"
                              (ghostherd-usage-used-up-text 'claude)))
      (should-not (ghostherd-usage-used-up-text 'grok))
      (ghostherd-relay rd 'grok)
      (let ((ask (cadr (assoc "herd_relay_brief" requests))))
        (should (string-match-p "\\`its claude usage (5h) is used up" (plist-get ask :reason)))
        ;; Its usage ran out: its turn was cut short, and the heir goes on.
        (should (eq (plist-get ask :cut) t))))))

(ert-deftest ghostherd-test-relay-of-one-at-work-goes-on ()
  "An agent relayed while it works, or blocked on a question, was cut
short: the heir goes on with its work.  Idle, it had finished its turn."
  (ghostherd-tests--relaying
    (dolist (case '((working . t) (blocked . t) (idle . :json-false) (done . :json-false)))
      (setq requests nil)
      (clrhash ghostherd--relaying)
      (let ((s (ghostherd-tests--session :name (format "rd-%s" (car case)) :kind 'claude
                                         :state (car case) :backend 'fake)))
        (ghostherd-relay s 'grok)
        (should (eq (plist-get (cadr (assoc "herd_relay_brief" requests)) :cut) (cdr case)))))))

(ert-deftest ghostherd-test-relay-offered-when-usage-runs-out ()
  "An agent that stops working with its CLI used up is offered its relay,
once for that window: in the log, the echo area and a push."
  (ghostherd-tests--relaying
    (let ((rd (ghostherd-tests--session :name "rd" :kind 'claude :state 'working :backend 'fake))
          pushed)
      (cl-letf (((symbol-function 'ghostherd--push)
                 (lambda (kind s &rest fields)
                   (when (eq kind 'used-up)
                     (push (list kind (ghostherd-session-name s) fields) pushed))))
                ((symbol-function 'ghostherd--maybe-notify-state) #'ignore))
        (ghostherd--set-state rd 'idle)
        (should-not pushed)
        (ghostherd-tests--used-up 'claude)
        (ghostherd--set-state rd 'working)
        (ghostherd--set-state rd 'idle)
        (should (= (length pushed) 1))
        (pcase-let ((`(,kind ,name ,fields) (car pushed)))
          (should (eq kind 'used-up))
          (should (equal name "rd"))
          (should (equal (plist-get fields :from) "claude"))
          (should (equal (plist-get fields :to) "grok"))
          (should (string-match-p "its claude usage (5h) is used up" (plist-get fields :reason))))
        (should (string-match-p "A relay hands its work to grok" (ghostherd-log-entry-text (car ghostherd--log))))
        (ghostherd--set-state rd 'working)
        (ghostherd--set-state rd 'blocked)
        (should (= (length pushed) 1))
        ;; One that was not working -- just started -- has stopped nothing.
        (ghostherd-tests--session :name "new" :kind 'claude :state 'starting :backend 'fake)
        (ghostherd--set-state (ghostherd-get "new") 'idle)
        (should (= (length pushed) 1))
        ;; Nowhere else to go: nothing offered.
        (ghostherd-tests--session :name "qa" :kind 'claude :state 'working :backend 'fake)
        (ghostherd-tests--used-up 'grok)
        (ghostherd-tests--used-up 'agy)
        (ghostherd--set-state (ghostherd-get "qa") 'idle)
        (should (= (length pushed) 1))))))

(ert-deftest ghostherd-test-relay-offer-forgotten-when-quota-returns ()
  "Once an agent is no longer used up, settling forgets its relay offer, so
a later, real used-up offers again rather than being taken for the one
already sent."
  (ghostherd-tests--relaying
    (let ((rd (ghostherd-tests--session :name "rd" :kind 'claude :state 'working :backend 'fake))
          pushed)
      (cl-letf (((symbol-function 'ghostherd--push)
                 (lambda (kind s &rest _)
                   (when (eq kind 'used-up) (push (ghostherd-session-name s) pushed))))
                ((symbol-function 'ghostherd--maybe-notify-state) #'ignore))
        (ghostherd-tests--used-up 'claude)
        (ghostherd--set-state rd 'working)
        (ghostherd--set-state rd 'idle)
        (should (= (length pushed) 1))
        (should (gethash (ghostherd-session-id rd) ghostherd--relay-offered))
        ;; Its quota comes back: settling now forgets the offer, and the
        ;; card (which reads the live used-up) clears with it.
        (setq ghostherd-usage--cache nil)
        (ghostherd--set-state rd 'working)
        (ghostherd--set-state rd 'idle)
        (should (= (length pushed) 1))
        (should-not (gethash (ghostherd-session-id rd) ghostherd--relay-offered))
        ;; Out again: offered afresh, not suppressed by the stale memo.
        (ghostherd-tests--used-up 'claude)
        (ghostherd--set-state rd 'working)
        (ghostherd--set-state rd 'idle)
        (should (= (length pushed) 2))))))

(ert-deftest ghostherd-test-relay-a-link-another-cli-left-is-not-the-heirs ()
  "A hook of the killed claude reporting late under rd's name: the grok
heir does not take its conversation for its own."
  (ghostherd-tests--with-herd ((ghostherd--herd-links (make-hash-table :test 'equal)))
    (let ((rd (ghostherd-tests--session :name "rd" :kind 'grok)))
      (ghostherd--herd-take-links '((:session "rd" :cli "claude" :conversation "c1")))
      (should-not (ghostherd-session-conversation rd))
      (setf (ghostherd-session-kind rd) 'claude)
      (should (equal (ghostherd-session-conversation rd) "c1"))
      (ghostherd--herd-take-links '((:session "rd" :conversation "c2")))
      (setf (ghostherd-session-kind rd) 'grok)
      (should (equal (ghostherd-session-conversation rd) "c2")))))

(ert-deftest ghostherd-test-relay-the-line-outlives-emacs ()
  "Written onto tmux with the recipe, and read back by restore."
  (let ((line '(((kind . "claude") (conversation . "c1") (ended . "2026-10-12T15:00:00+0800")
                 (reason . "its claude usage (5h) is used up")))))
    (should (equal (ghostherd-tmux--decode :lineage (ghostherd-tmux--encode :lineage line)) line))
    (should (equal (ghostherd-tmux--encode :lineage nil) ""))
    (should-not (ghostherd-tmux--decode :lineage ""))
    (should-not (ghostherd-tmux--decode :lineage "not json"))
    (should (assq :lineage ghostherd-tmux--recipe-options))
    (ghostherd-tests--with-herd ((ghostherd--lineage (make-hash-table :test 'equal))
                                 (ghostherd--roles (make-hash-table :test 'equal))
                                 (ghostherd-known-backends '(fake))
                                 (ghostherd-tests--fake-recipes
                                  `((:name "rd" :kind grok :host-id "gh-rd" :lineage ,line))))
      (cl-letf (((symbol-function 'ghostherd--ensure-poll-timer) #'ignore)
                ((symbol-function 'ghostherd--sidebar-refresh) #'ignore))
        (ghostherd-restore))
      (let ((rd (ghostherd-get "rd")))
        (should (= (ghostherd-session-generation rd) 2))
        (should (equal (plist-get (ghostherd-session-recipe rd) :lineage) line))))))

(ert-deftest ghostherd-test-relay-spawn-keeps-the-line ()
  "An heir's line goes to the host with its recipe, and comes back in it;
an agent spawned without one is the first of its name."
  (ghostherd-tests--with-herd ((ghostherd--lineage (make-hash-table :test 'equal))
                               (ghostherd--roles (make-hash-table :test 'equal))
                               (ghostherd-tests--fake-spawned nil))
    ;; Its paths are labels: nothing starts in them, so none is missing.
    (cl-letf (((symbol-function 'ghostherd--directory-missing-p) #'ignore)
              ((symbol-function 'ghostherd--install-hooks) #'ignore)
              ((symbol-function 'ghostherd--register-eval-cmds) #'ignore)
              ((symbol-function 'ghostherd--ensure-poll-timer) #'ignore)
              ((symbol-function 'ghostherd--sidebar-refresh) #'ignore))
      (let* ((line '(((kind . "claude") (conversation . "c1") (ended . "x") (reason . ""))))
             (rd (ghostherd-spawn 'grok :name "rd" :project "/p/" :backend 'fake :lineage line :display nil)))
        (should (equal (plist-get ghostherd-tests--fake-spawned :lineage) line))
        (should (= (ghostherd-session-generation rd) 2))
        (should (equal (plist-get (ghostherd-session-recipe rd) :lineage) line))
        (ghostherd-kill rd)
        (should (= (ghostherd-session-generation
                    (ghostherd-spawn 'grok :name "rd" :project "/p/" :backend 'fake :display nil))
                   1))))))

;;; Primary, the herd-wide lead (ask 9622f1e9)

(ert-deftest ghostherd-test-primary-starts-in-the-herds-own-place ()
  "Primary is one agent, `primary', role primary, in the herd's directory
-- outside every repo, made if need be -- its project; claude takes its
role as it starts.  Up, it is the one returned again."
  (ghostherd-tests--with-primary
    (let ((p (ghostherd-primary-start)))
      (should (equal (ghostherd-session-name p) "primary"))
      (should (equal (ghostherd-session-role p) "primary"))
      (should (file-directory-p ghostherd-primary-directory))
      (should (ghostherd--primary-dir-p (ghostherd-session-project p)))
      (should (equal (last (plist-get ghostherd-tests--fake-spawned :args) 2)
                     (list "--append-system-prompt" ghostherd--primary-role)))
      (should-not (gethash "primary" ghostherd--role-owed))
      (should (string-match-p "started, a fresh" (ghostherd-log-entry-text (car ghostherd--log))))
      (should (eq (ghostherd-primary-start) p))
      (should (= (length (seq-filter (lambda (s) (equal (ghostherd-session-role s) "primary"))
                                     (ghostherd-sessions)))
                 1)))))

(ert-deftest ghostherd-test-primary-resumes-its-last-conversation ()
  "Stopped, or gone with its host, primary comes back on its conversation:
the one its hooks named, else the one kept for it beside its directory --
only on the same CLI; another starts afresh."
  (ghostherd-tests--with-primary
    (let ((p (ghostherd-primary-start)))
      ;; Its hooks name its conversation; the tick keeps it for later.
      (ghostherd--herd-take-links '((:session "primary" :cli "claude" :conversation "c-777")))
      (should (equal (ghostherd--primary-remembered) '(:kind claude :conversation "c-777")))
      ;; What its hooks named comes before what was kept, which may be older.
      (with-temp-file (ghostherd--primary-state-file)
        (prin1 '(:kind claude :conversation "c-old") (current-buffer)))
      ;; Its host gone: the next start resumes that conversation.
      (setf (ghostherd-session-state p) 'dead)
      (let ((again (ghostherd-primary-start)))
        (should-not (eq again p))
        (should (equal (last (plist-get ghostherd-tests--fake-spawned :args) 2) '("-r" "c-777")))
        (should (string-match-p "resumed on its conversation c-777"
                                (ghostherd-log-entry-text (car ghostherd--log))))
        ;; Gone from the herd altogether -- Emacs and the host restarted:
        ;; the kept one.
        (ghostherd-kill again t)
        (clrhash ghostherd--herd-links)
        (ghostherd-primary-start)
        (should (equal (last (plist-get ghostherd-tests--fake-spawned :args) 2) '("-r" "c-old")))
        ;; Another CLI cannot resume claude's conversation: afresh.
        (ghostherd-kill (ghostherd--primary-session) t)
        (let ((ghostherd-primary-kind 'grok))
          (ghostherd-primary-start)
          (should-not (member "c-old" (plist-get ghostherd-tests--fake-spawned :args)))
          ;; grok's rules: primary's role, then the shared-memory block.
          (should (string-prefix-p ghostherd--primary-role
                                   (cadr (member "--rules" (plist-get ghostherd-tests--fake-spawned :args))))))))))

(ert-deftest ghostherd-test-primary-is-told-once-it-is-up ()
  "Said to while up: told now.  Not up: started, and told once ready.  On
a CLI that takes no role as it starts (agy), its first message carries
primary's role."
  (ghostherd-tests--with-primary
    (let ((ready nil))
      (cl-letf (((symbol-function 'ghostherd--when-ready) (lambda (s f &rest _) (push (cons s f) ready))))
        (let ((ghostherd-primary-kind 'agy))
          (let ((p (ghostherd-primary-say "what is everyone doing?")))
            (should (eq (ghostherd-session-kind p) 'agy))
            (should-not said)
            (should (equal (gethash "primary" ghostherd--role-owed) ghostherd--primary-role))
            (funcall (cdar ready))
            (should (equal (caar said) "primary"))
            (should (string-prefix-p ghostherd--primary-role (cdar said)))
            (should (string-suffix-p "\n---\n\nwhat is everyone doing?" (cdar said)))
            ;; Up now: told at once, the role not again.
            (setf (ghostherd-session-state p) 'idle)
            (ghostherd-primary-say "and the login?")
            (should (equal (car said) '("primary" . "and the login?")))))))))

(ert-deftest ghostherd-test-primary-is-reached-by-telegram-and-replies ()
  "Telegram's free-form words (`primary-say') reach it, started if need
be; a reply to its own report (`lead-say' for its directory) reaches it,
not a lead started in its place."
  (ghostherd-tests--with-primary
    (cl-letf (((symbol-function 'ghostherd--when-ready) (lambda (_s f &rest _) (funcall f))))
      (let ((r (ghostherd--herd-run-command
                '(:id "c1" :op "primary-say" :session "primary" :args (:body "fix the login everywhere")))))
        (should (equal (plist-get r :text) "started primary; it reads this once it is up"))
        (should (equal (car said) '("primary" . "fix the login everywhere"))))
      (setf (ghostherd-session-state (ghostherd--primary-session)) 'idle)
      (let ((r (ghostherd--herd-run-command
                `(:id "c2" :op "lead-say" :session "x"
                      :args (:project ,ghostherd-primary-directory :body "and the other one?")))))
        (should (string-match-p "sent to primary" (plist-get r :text)))
        (should (equal (car said) '("primary" . "and the other one?")))
        (should-not (ghostherd-project-lead ghostherd-primary-directory))))))

(ert-deftest ghostherd-test-primarys-report-is-pushed-as-a-leads ()
  "Its turn done while you are away: pushed as a report, as a lead's is."
  (ghostherd-test--pushing
    (let ((p (ghostherd-tests--session :name "primary" :kind 'claude :state 'working :project "~/.ghostherd/primary/")))
      (puthash "primary" "primary" ghostherd--roles)
      (ghostherd--set-state p 'done)
      (should (equal (mapcar (lambda (x) (list (plist-get x :kind) (plist-get x :session))) ghostherd--herd-pushes)
                     '(("report" "primary")))))))

(ert-deftest ghostherd-test-primary-will-not-take-anothers-name ()
  "An agent the user named `primary' is not the herd's primary: no clobber."
  (ghostherd-tests--with-primary
    (ghostherd-tests--session :name "primary" :kind 'claude :backend 'fake :project repo)
    (should-error (ghostherd-primary-start) :type 'user-error)))

(ert-deftest ghostherd-test-a-lead-started-for-primary-says-for-the-user ()
  "A lead the herd starts for primary's handoff is for the user, from no
project of primary's."
  (ghostherd-tests--with-handoff
    (let ((ghostherd-primary-directory (concat base "herd/primary/")))
      (make-directory ghostherd-primary-directory t)
      (ghostherd-tests--session :name "primary" :kind 'claude :backend 'fake :project ghostherd-primary-directory)
      (puthash "primary" "primary" ghostherd--roles)
      (let ((r (ghostherd-handoff-lead "primary" other ghostherd-primary-directory "x")))
        (should (string-suffix-p "for a handoff from primary, for the user" (cdr r)))
        (should (equal (plist-get (car ghostherd--herd-pushes) :from) ""))))))

(ert-deftest ghostherd-test-primarys-role-says-what-it-is ()
  "It routes and tracks, never does: the commands it has, the refusals it
meets, what it leaves the user, how it reports, no timers."
  (dolist (words '("You never write code and never edit a checkout"
                   "\"$GHOSTHERD_HERD\" list" "\"$GHOSTHERD_HERD\" asks"
                   "\"$GHOSTHERD_HERD\" recall --project P"
                   "\"$GHOSTHERD_HERD\" handoff PROJECT --wait"
                   "You do not ask any project's agents yourself -- the herd refuses it"
                   "you recommend and leave to them"
                   "report what went where, who is on it and what\nto wait for"
                   "Your memory is your own store"
                   "No\npolling, no loops, nothing on a timer"))
    (should (string-search words ghostherd--primary-role))))

(ert-deftest ghostherd-test-the-page-has-primary-and-its-room-writes-to-it ()
  "The page knows where primary's room is, up or not; its chat's message
starts it."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (ghostherd-tests--with-primary
    (let ((sent nil))
      (cl-letf (((symbol-function 'ghostherd-memory-page--js) (lambda (f &rest a) (push (cons f a) sent)))
                ((symbol-function 'ghostherd--when-ready) (lambda (_s f &rest _) (funcall f)))
                ((symbol-function 'ghostherd-memory-page--hooks-payload) #'ignore)
                ((symbol-function 'ghostherd-memory-page--projects-work) #'ignore)
                ((symbol-function 'ghostherd-memory-page--usage) #'ignore))
        (ghostherd-memory-page--send-herd)
        (let ((p (plist-get (cadr (assoc "renderHerd" sent)) :primary)))
          (should (equal (plist-get p :directory) (ghostherd--project-name ghostherd-primary-directory)))
          (should (equal (plist-get p :state) ""))
          (should (json-encode p)))
        (ghostherd-memory-page--primary-say '((text . "what is stuck?")))
        (should (equal (car said) '("primary" . "what is stuck?")))
        (should (equal (cadr (assoc "flash" sent)) "Starting primary; it gets your message once it is ready"))))))

(ert-deftest ghostherd-test-proposals-facts-reach-the-page-as-an-array ()
  "A proposal's facts survive the page encode (json.el, as xwapp-js sends it) as
a JSON array of fact objects -- never one collapsed {\"name\":[…]} object.  That
collapse was the live bug: the page's `renderMemProposal' threw on
`p.facts.map' and the Memory tab's whole Proposals section rendered empty.  See
`ghostherd-memory-page--proposal-for-page'."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let ((ghostherd-memory-page--memory-key "-k")
        (ghostherd-memory-page--proposals-sig nil)
        (captured nil)
        ;; As json.el parses the sidecar's reply (the page code's own settings):
        ;; each proposal a plist, its `:facts' a LIST of fact plists.
        (result (list :key "-k" :proposals
                      (list (list :id "p1" :status "pending" :file "CLAUDE.md"
                                  :diff "--- a/CLAUDE.md\n+++ b/CLAUDE.md\n@@ -1 +1,2 @@\n ctx\n+add"
                                  :author "lead"
                                  :facts (list (list :name "a" :title "A" :gone nil :promoted nil)
                                               (list :name "b" :title "B" :gone nil :promoted nil)))))))
    (cl-letf (((symbol-function 'ghostherd-memory-request-async)
               (lambda (_method cb &rest _) (funcall cb result)))
              ((symbol-function 'ghostherd-memory-page--js)
               (lambda (_fn obj) (setq captured obj))))
      (ghostherd-memory-page--memory-proposals-tick "-k"))
    (should captured)
    ;; Round-trip through json.el exactly as xwapp-js does.
    (let ((json (json-encode captured)))
      (should (string-match-p "\"facts\":\\[{" json))   ; an array of objects
      (should-not (string-match-p "\"facts\":{" json))  ; not a collapsed object
      (let* ((back (json-parse-string json :object-type 'plist :array-type 'list))
             (facts (plist-get (car (plist-get back :proposals)) :facts)))
        (should (= 2 (length facts)))                    ; both facts kept, not one
        (should (equal "a" (plist-get (car facts) :name)))
        (should (equal "b" (plist-get (cadr facts) :name)))))))

(provide 'ghostherd-primary-tests)
;;; ghostherd-primary-tests.el ends here

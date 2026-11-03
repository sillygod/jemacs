;;; ghostherd-log-tests.el --- Tests for ghostherd-log.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Part of ghostherd's tests; ghostherd-tests.el runs them all.  This
;; file alone:
;;
;;   emacs --batch --init-directory=~/.emacs.d/emacs-home/ \
;;         -L . -L ../xwapp -l ghostherd-log-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ghostherd-test-helpers)

;;; Herd log

(ert-deftest ghostherd-test-log-records-transitions ()
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (let ((s (ghostherd-tests--session :name "rev" :kind 'agy :state 'idle)))
        (ghostherd--set-state s 'working "input sent")
        (should (= (length ghostherd--log) 1))
        (let ((entry (car ghostherd--log)))
          (should (equal (ghostherd-log-entry-session entry) "rev"))
          (should (eq (ghostherd-log-entry-kind entry) 'state))
          (should (string-match-p "idle → working" (ghostherd-log-entry-text entry)))
          (should (string-match-p "input sent" (ghostherd-log-entry-text entry))))
        ;; a no-op transition is not an event
        (ghostherd--set-state s 'working "again")
        (should (= (length ghostherd--log) 1))))))

(ert-deftest ghostherd-test-log-issues-are-the-lines-worth-a-look ()
  "The Log tab's Issues: a block, a death, a screen kept for a fault --
by what the writers put in the log, not by a loose word in a line.  The
other writers' lines are asked beside their own tests
\(`ghostherd-tests--issue-p')."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (let ((s (ghostherd-tests--session :name "rev" :kind 'agy :state 'idle)))
        (ghostherd--set-state s 'working "input sent")
        (should-not (ghostherd-tests--issue-p "idle → working"))
        (ghostherd--set-state s 'blocked "Approve? blocked on you")
        (should (ghostherd-tests--issue-p "working → blocked"))
        (ghostherd--set-state s 'done "blocked earlier, now done")
        (should-not (ghostherd-tests--issue-p "blocked → done"))
        (ghostherd--set-state s 'dead "exited")
        (should (ghostherd-tests--issue-p "→ dead")))))
  (let ((line (lambda (kind text &optional why)
                (ghostherd-log-entry--create :session "a" :kind kind :text text :why why))))
    (dolist (why '(limit menu mismatch manual))
      (should (ghostherd-log-issue-p
               (funcall line 'screen (ghostherd--capture-text "a" why nil) why))))
    ;; A /compact's capture is a sample for the screen rules, not a fault.
    (should-not (ghostherd-log-issue-p
                 (funcall line 'screen (ghostherd--capture-text "a" 'compaction nil) 'compaction)))
    ;; The same words, in lines that are no issue: the user's own acts,
    ;; a retry that went in, an ask's head, a report.
    (dolist (ok '((input "← commit  (not submitted)")
                  (input "↵ again: it was still in the input box")
                  (input "← tell me why it failed: twice")
                  (ask "ask a1b2c3d4 from lead-x: why was it blocked? it failed: twice")
                  (ask "ask a1b2c3d4 from lead-x cancelled")
                  (ask "ask a1b2c3d4 left open: rd was only just handed it")
                  (life "killed")
                  (life "released from the herd")
                  (life "its directory is back")
                  (life "stopped by the user: its directory was gone")
                  (life "relaying to grok: reading where its work stands")
                  (guard "guard g1 kept denied by the user: run_command: cat .env")
                  (report "reports blocked")))
      (should-not (ghostherd-log-issue-p (apply line ok))))))

(ert-deftest ghostherd-test-log-keeps-the-screen-behind-a-block ()
  "The reason the log keeps screens at all: `ghostherd-explain' can only
answer while the agent is still sitting on the prompt, so a rule that
fired at 02:00 would otherwise be unarguable by morning."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (let ((s (ghostherd-tests--session :name "rev" :kind 'agy :backend 'fake))
            (ghostherd-tests--fake-screen ghostherd-tests--permission-screen))
        (ghostherd--set-state s 'blocked "proceed?")
        (should (string-match-p "npm publish"
                                (ghostherd-log-entry-screen (car ghostherd--log))))
        ;; every other transition is far too common to keep screens for
        (ghostherd--set-state s 'working "off it goes")
        (should-not (ghostherd-log-entry-screen (car ghostherd--log)))))))

(ert-deftest ghostherd-test-log-caps-screens-not-entries ()
  "Five hundred one-line entries cost nothing; twenty 40x120 screens are
the actual memory.  Old entries stay, they just stop carrying one."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (let ((s (ghostherd-tests--session :name "rev" :kind 'agy :backend 'fake))
            (ghostherd-tests--fake-screen "a screen")
            (ghostherd-log-screens 2))
        (dotimes (_ 5)
          (ghostherd--set-state s 'working "w")
          (ghostherd--set-state s 'blocked "b"))
        (should (= (length ghostherd--log) 10))
        (should (= (cl-count-if #'ghostherd-log-entry-screen ghostherd--log) 2))
        ;; ...and they are the newest two blocks: entries alternate
        ;; working/blocked, so the screens sit at 0 and 2, not 0 and 1.
        (should (ghostherd-log-entry-screen (nth 0 ghostherd--log)))
        (should (ghostherd-log-entry-screen (nth 2 ghostherd--log)))
        (should-not (ghostherd-log-entry-screen (nth 4 ghostherd--log)))))))

(ert-deftest ghostherd-test-log-trims-to-max ()
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (let ((s (ghostherd-tests--session :name "rev" :kind 'agy :state 'idle))
            (ghostherd-log-max 4)
            (ghostherd-log-screens 0))
        (dotimes (_ 10)
          (ghostherd--set-state s 'working "w")
          (ghostherd--set-state s 'idle "i"))
        (should (= (length ghostherd--log) 4))))))

(ert-deftest ghostherd-test-log-keeps-lines-by-day-not-count ()
  "Retention is by age: a line older than `ghostherd-log-days' goes, a
younger one stays, however many there are."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (let ((ghostherd-log-days 7) (ghostherd-log-max 0))  ; time only, no ceiling
        (ghostherd--log-add "a" 'life "old")
        (setf (ghostherd-log-entry-time (car ghostherd--log))
              (time-subtract (current-time) (seconds-to-time (* 8 86400))))
        (dotimes (i 20) (ghostherd--log-add "a" 'life (format "fresh-%d" i)))
        (let ((texts (mapcar #'ghostherd-log-entry-text ghostherd--log)))
          (should (= (length ghostherd--log) 20))      ; all 20 fresh kept
          (should-not (member "old" texts)))))))        ; the 8-day line gone

(ert-deftest ghostherd-test-log-pins-survive-every-cap ()
  "A pin is a promise: age, the ceiling and the screen cap all pass it by."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (let ((ghostherd-log-max 2) (ghostherd-log-screens 1) (ghostherd-log-days 1))
        (let ((pid (ghostherd--log-add "a" 'screen "pinned" "SCREEN" 'manual t)))
          (setf (ghostherd-log-entry-time (ghostherd--log-entry-by-id pid))
                (time-subtract (current-time) (seconds-to-time (* 10 86400))))
          (dotimes (i 5) (ghostherd--log-add "a" 'screen (format "s%d" i) "SC" 'blocked))
          (let ((e (ghostherd--log-entry-by-id pid)))
            (should e)                                  ; kept, 10 days old, past the ceiling
            (should (ghostherd-log-entry-screen e))     ; its screen kept, past the screen cap
            (should (ghostherd-log-entry-pinned e)))
          ;; The unpinned flood is still held to the caps.
          (should (= (cl-count 'blocked ghostherd--log :key #'ghostherd-log-entry-why) 2))
          (should (= (cl-count-if (lambda (e) (and (ghostherd-log-entry-screen e)
                                                   (not (ghostherd-log-entry-pinned e))))
                                  ghostherd--log)
                     1)))))))

(ert-deftest ghostherd-test-capture-screen-pins-with-a-note ()
  "The button saves the current screen pinned, tagged `manual', with the
note the user typed."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (let* ((s (ghostherd-tests--session :name "grok-x" :kind 'grok :backend 'fake))
             (ghostherd-tests--fake-screen "the quota is gone\n❯ \n")
             (id (ghostherd-capture-screen s "look at this")))
        (let ((e (ghostherd--log-entry-by-id id)))
          (should (eq (ghostherd-log-entry-kind e) 'screen))
          (should (eq (ghostherd-log-entry-why e) 'manual))
          (should (ghostherd-log-entry-pinned e))
          (should (equal (ghostherd-log-entry-note e) "look at this"))
          (should (string-match-p "the quota is gone" (ghostherd-log-entry-screen e)))
          (should (string-match-p "look at this" (ghostherd-log-entry-text e))))))))

(ert-deftest ghostherd-test-log-pin-unpin-and-delete-by-id ()
  "The Log tab names an entry by its id to pin, un-pin or delete it."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (let ((id (ghostherd--log-add "a" 'screen "cap" "SCREEN" 'menu)))
        (should-not (ghostherd-log-entry-pinned (ghostherd--log-entry-by-id id)))
        (ghostherd-log-set-pinned id t)
        (should (ghostherd-log-entry-pinned (ghostherd--log-entry-by-id id)))
        (ghostherd-log-set-pinned id nil)
        (should-not (ghostherd-log-entry-pinned (ghostherd--log-entry-by-id id)))
        (should (ghostherd-log-delete id))
        (should-not (ghostherd--log-entry-by-id id))
        (should-not (ghostherd-log-delete id))))))

(ert-deftest ghostherd-test-log-ids-persist-across-a-restart ()
  "An entry's id survives the file, so a pin made on the page outlives the
Emacs that drew it, and a new entry never reuses an old id."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log-file
      (let ((a (ghostherd--log-add "x" 'life "one"))
            (b (ghostherd--log-add "x" 'life "two")))
        (should (= b (1+ a)))
        (let ((ghostherd--log nil) (ghostherd--log-loaded nil) (ghostherd--log-seq 0))
          (ghostherd-log-load)
          (let ((ids (delq nil (mapcar #'ghostherd-log-entry-id ghostherd--log))))
            (should (member a ids))
            (should (member b ids)))
          (should (> (ghostherd--log-add "x" 'life "three") b)))))))

(ert-deftest ghostherd-test-log-entry-round-trips-its-new-fields ()
  "why / pinned / note / id survive the plist on disk."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (let* ((id (ghostherd--log-add "a" 'screen "t" "SCREEN" 'limit t "a note"))
             (form (ghostherd--log-form (ghostherd--log-entry-by-id id)))
             (back (ghostherd--log-entry-from-form (read (prin1-to-string form)))))
        (should (eq (ghostherd-log-entry-why back) 'limit))
        (should (ghostherd-log-entry-pinned back))
        (should (equal (ghostherd-log-entry-note back) "a note"))
        (should (eql (ghostherd-log-entry-id back) id))
        (should (equal (ghostherd-log-entry-screen back) "SCREEN"))))))

(ert-deftest ghostherd-test-auto-capture-why-names-the-trigger ()
  "Each suspicious screen names the one reason it was kept, most
actionable first; a clean one names none."
  (let ((menu "Pick one:\n❯ 1. Keep it\n  2. Drop it\n")
        (scrollback (concat "we discussed the rate limit earlier\n"
                            (mapconcat (lambda (_) "doing some work here")
                                       (number-sequence 1 20) "\n")
                            "\nall done\n")))
    ;; (a) a limit word at the foot...
    (should (eq (ghostherd--auto-capture-why "working\n❯ \nout of credits\n" nil nil) 'limit))
    ;; ...but the same word only up in the scrollback is not it.
    (should-not (ghostherd--auto-capture-why scrollback nil nil))
    ;; (d) a /compact in flight.
    (should (eq (ghostherd--auto-capture-why "✽ Compacting conversation… (22s)\n" nil nil)
                'compaction))
    ;; (b) a menu no `blocked' rule matched (the hit is not blocked).
    (should (eq (ghostherd--auto-capture-why menu nil nil) 'menu))
    ;; ...but a recognised block (hit blocked) is not re-kept as a menu.
    (should-not (ghostherd--auto-capture-why menu '(blocked . "x") nil))
    ;; (c) the hook says busy, the screen does not.
    (should (eq (ghostherd--auto-capture-why "plain idle text\n" nil '(working . "r"))
                'mismatch))
    ;; order: a limit word wins over a compaction and a mismatch together.
    (should (eq (ghostherd--auto-capture-why
                 "✽ Compacting conversation…\nout of quota\n" nil '(working . "r"))
                'limit))
    ;; a clean working screen keeps nothing.
    (should-not (ghostherd--auto-capture-why
                 "✶ Thinking… (3s · esc to interrupt)\n❯ \n" '(working . "s") nil))))

(defconst ghostherd-tests--idle-box
  "────────────────────────────────────────\n❯ \n────────────────────────────────────────\n"
  "An idle input box, as claude draws it: a rule, the prompt sign, a rule.")

(ert-deftest ghostherd-test-auto-capture-limit-is-the-clis-notice-not-words ()
  "A limit capture is the CLI's own notice -- under its input box, or in
its place -- not the conversation above the box, not agy's paid-credit
footer, not a panel the herd itself opened (ask d197da21)."
  (let ((agy (ghostherd--spec 'agy))
        (claude (ghostherd--spec 'claude))
        (grok (ghostherd--spec 'grok)))
    ;; agy's footer: paid overage credit, an agent with quota left.
    (should-not (ghostherd--auto-capture-why
                 (concat "Done with the change.\n" ghostherd-tests--idle-box
                         "  ? for shortcuts      Model X · high · AI: Out of credits\n")
                 nil nil agy))
    ;; agy's /usage panel, opened by the herd to read its quota: all
    ;; "quota", and a view paged over the conversation, not its state.
    (should-not (ghostherd--auto-capture-why
                 (concat "GROUP ONE\n  Weekly Limit Remaining\n    [█████] 100.00%\n"
                         "    Quota available\n  Some text about a separate quota\n"
                         "  (1–9 of 12 lines)\n"
                         "  ↑/↓ Scroll · pgup/pgdown Page · ctrl+end Bottom · ctrl+home Top · esc Close\n"
                         "                     Model X · high · AI: Out of credits\n")
                 nil nil agy))
    ;; A reply about rate limits, a test's 429, a spinner's token count:
    ;; words above the box.
    (should-not (ghostherd--auto-capture-why
                 (concat "⏺ The client must back off on a 429 and on the rate limit.\n"
                         "  It retries once the quota resets.\n"
                         "✻ Pondering… (11s · ↓ 429 tokens)\n" ghostherd-tests--idle-box
                         "  ⏵⏵ auto mode on (shift+tab to cycle)\n")
                 nil nil claude))
    ;; The CLI's own notice, under its box: kept.
    (should (eq (ghostherd--auto-capture-why
                 (concat "⏺ Working on it.\n" ghostherd-tests--idle-box
                         "  ⚠ Usage limit reached · limit resets 1am\n"
                         "  ⏵⏵ auto mode on (shift+tab to cycle)\n")
                 nil nil claude)
                'limit))
    ;; A quota notice under agy's box is kept, credits footer and all.
    (should (eq (ghostherd--auto-capture-why
                 (concat "Done.\n" ghostherd-tests--idle-box
                         "  Quota exceeded for this model\n"
                         "  ? for shortcuts      Model X · high · AI: Out of credits\n")
                 nil nil agy)
                'limit))
    ;; A card in the box's place: kept.
    (should (eq (ghostherd--auto-capture-why
                 (concat "  ┃  You hit your free usage limit.\n  ┃\n"
                         "  ┃  1 (○) Upgrade to Plan A\n  ┃  2 (○) Upgrade to Plan B\n"
                         "  ┃  ↑/↓ navigate                    Enter:submit\n"
                         "  Tab:next answer  │  Esc:scrollback\n")
                 nil nil grok)
                'limit))
    ;; With the ignore unset, the footer counts as it did.
    (let ((ghostherd-auto-capture-limit-ignore-re nil))
      (should (eq (ghostherd--auto-capture-why
                   (concat "Done.\n" ghostherd-tests--idle-box
                           "  ? for shortcuts      Model X · AI: Out of credits\n")
                   nil nil agy)
                  'limit)))))

(ert-deftest ghostherd-test-auto-capture-menu-is-read-as-detection-reads-it ()
  "A menu capture is a menu with no input box under it, as
`ghostherd--detect-state' reads one: not a numbered list in a reply above
an idle box, not the model picker (ask d197da21)."
  (let ((claude (ghostherd--spec 'claude))
        (reply (concat "⏺ Three things to do next:\n"
                       "  ❯ 1. Fix the parser\n    2. Add a test\n    3. Ship it\n"
                       ghostherd-tests--idle-box "  ⏵⏵ auto mode on (shift+tab to cycle)\n")))
    (should-not (ghostherd--auto-capture-why reply nil nil claude))
    (should-not (ghostherd--auto-capture-why reply '(working . "s") nil claude))
    ;; A real menu, the box gone: kept.
    (should (eq (ghostherd--auto-capture-why ghostherd-tests--claude-login-menu nil nil claude)
                'menu))
    ;; The model picker replaces the box too, but the herd drives it.
    (should-not (ghostherd--auto-capture-why
                 (concat " Select model\n"
                         " ❯ 1. Model One ✔   The newest\n   2. Model Two   Older\n"
                         " ◉ High effort ←/→ to adjust\n"
                         " Enter to confirm · s to use this session only · Esc to exit\n")
                 nil nil claude))
    ;; A kind whose menus do not take the box's place reads as before.
    (should (eq (ghostherd--auto-capture-why reply nil nil nil) 'menu))))

(ert-deftest ghostherd-test-auto-capture-reads-the-agents-own-kind ()
  "The poll's capture passes the agent's spec: agy's credits footer is
not kept, a claude reply's numbered list is not kept."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (let ((a (ghostherd-tests--session :name "agy-x" :kind 'agy :state 'idle))
            (c (ghostherd-tests--session :name "claude-x" :kind 'claude :state 'idle)))
        (puthash "agy-x" (concat "Done.\n" ghostherd-tests--idle-box
                                 "  ? for shortcuts      Model X · high · AI: Out of credits\n")
                 ghostherd--screens)
        (puthash "claude-x" (concat "⏺ Options:\n  ❯ 1. One\n    2. Two\n" ghostherd-tests--idle-box)
                 ghostherd--screens)
        (ghostherd--auto-capture a)
        (ghostherd--auto-capture c)
        (should-not (cl-find 'screen ghostherd--log :key #'ghostherd-log-entry-kind))
        (puthash "claude-x" ghostherd-tests--claude-login-menu ghostherd--screens)
        (ghostherd--auto-capture c)
        (should (eq (ghostherd-log-entry-why (car ghostherd--log)) 'menu))))))

(ert-deftest ghostherd-test-auto-capture-keeps-a-screen-once ()
  "A suspicious screen is kept unpinned and tagged; the same one is not
kept twice, and the trigger is throttled per agent."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (let* ((s (ghostherd-tests--session :name "grok-x" :kind 'grok :state 'idle))
             (id (ghostherd-session-id s))
             (limits (lambda () (cl-count 'limit ghostherd--log
                                          :key #'ghostherd-log-entry-why))))
        (puthash id "the agent\nis out of quota now\n❯ \n" ghostherd--screens)
        (ghostherd--auto-capture s)
        (should (= (funcall limits) 1))
        (let ((e (cl-find 'limit ghostherd--log :key #'ghostherd-log-entry-why)))
          (should (eq (ghostherd-log-entry-kind e) 'screen))
          (should-not (ghostherd-log-entry-pinned e)))   ; auto, not pinned
        ;; The identical screen again: deduped.
        (ghostherd--auto-capture s)
        (should (= (funcall limits) 1))
        ;; A different limit screen, still inside the throttle: not kept.
        (puthash id "the agent\nis rate limited now\n❯ \n" ghostherd--screens)
        (ghostherd--auto-capture s)
        (should (= (funcall limits) 1))
        ;; Past the throttle, the new screen is kept.
        (let ((ghostherd-auto-capture-throttle 0))
          (ghostherd--auto-capture s)
          (should (= (funcall limits) 2)))))))

(ert-deftest ghostherd-test-auto-capture-mismatch-must-hold ()
  "A screen/hook mismatch is kept only once it has held the grace: a
turn-end flicker is dropped, a mismatch that persists is kept."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (let* ((s (ghostherd-tests--session :name "grok-x" :kind 'grok :state 'idle))
             (id (ghostherd-session-id s))
             (miss (lambda () (cl-count 'mismatch ghostherd--log
                                        :key #'ghostherd-log-entry-why))))
        ;; The hook says working; the screen reads idle (no spinner/menu/limit).
        (puthash id "just some finished prose here\n" ghostherd--screens)
        (puthash id (list 'working "r" (current-time)) ghostherd--reports)
        ;; Within the grace: seen, not kept.
        (ghostherd--auto-capture s)
        (should (= (funcall miss) 0))
        (should (gethash id ghostherd--mismatch-since))
        ;; Held past the grace: kept.
        (let ((ghostherd-auto-capture-mismatch-grace 0))
          (ghostherd--auto-capture s)
          (should (= (funcall miss) 1)))
        ;; A spinner shows: the screen agrees again, the mismatch clears.
        (puthash id "✶ Thinking… (3s · esc to interrupt)\n" ghostherd--screens)
        (ghostherd--auto-capture s)
        (should-not (gethash id ghostherd--mismatch-since))))))

(ert-deftest ghostherd-test-log-screens-text-filters ()
  "What `herd screens' prints: the saved screens, filtered by agent, time
and trigger, newest first."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (ghostherd--log-add "grok-x" 'screen "grok: a limit" "QUOTA GONE" 'limit)
      (ghostherd--log-add "claude-y" 'screen "claude: a menu" "A MENU" 'menu)
      (ghostherd--log-add "grok-x" 'state "working → blocked  Approve?" "BLOCKED SCREEN" 'blocked)
      (ghostherd--log-add "grok-x" 'life "no screen here")
      (let ((all (ghostherd--log-screens-text nil nil nil)))
        (should (string-match-p "3 screens" all))
        (should (string-match-p "QUOTA GONE" all))
        (should (string-match-p "BLOCKED SCREEN" all))
        (should-not (string-match-p "no screen here" all)))
      (let ((g (ghostherd--log-screens-text "grok-x" nil nil)))
        (should (string-match-p "2 screens" g))
        (should-not (string-match-p "claude-y" g)))
      (let ((lim (ghostherd--log-screens-text nil nil "limit")))
        (should (string-match-p "1 screen:" lim))
        (should (string-match-p "QUOTA GONE" lim))
        (should-not (string-match-p "A MENU" lim)))
      ;; By time: backdate the limit screen past a one-day cutoff.
      (let ((e (cl-find 'limit ghostherd--log :key #'ghostherd-log-entry-why)))
        (setf (ghostherd-log-entry-time e)
              (time-subtract (current-time) (seconds-to-time (* 3 86400)))))
      (let ((recent (ghostherd--log-screens-text nil (- (float-time) 86400) nil)))
        (should-not (string-match-p "QUOTA GONE" recent))
        (should (string-match-p "A MENU" recent)))
      (should (equal (ghostherd--log-screens-text "nobody" nil nil)
                     "no saved screens match")))))

(ert-deftest ghostherd-test-log-reads-in-causal-order ()
  "What you sent, then what it did -- not the other way round."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (let ((s (ghostherd-tests--session :name "rev" :kind 'agy :state 'idle)))
        (ignore (ghostherd-tests--recording-keys
                  (ghostherd-send s "please review the diff" t)))
        (let ((texts (mapcar #'ghostherd-log-entry-text (reverse ghostherd--log))))
          (should (= (length texts) 2))
          (should (string-match-p "please review the diff" (nth 0 texts)))
          (should (string-match-p "→ working" (nth 1 texts))))))))

(ert-deftest ghostherd-test-log-renders-without-a-session ()
  "The log outlives the sessions it describes, so rendering must not
depend on any of them still existing."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (ghostherd--log-add "gone" 'life "killed")
      (with-temp-buffer
        (ghostherd-log-mode)
        (ghostherd--log-render (current-buffer))
        (should (string-match-p "gone" (buffer-string)))
        (should (string-match-p "killed" (buffer-string)))))))

;;; A log that outlives Emacs

(defmacro ghostherd-tests--with-log-file (&rest body)
  "Run BODY with the herd log persisted to a temporary file."
  (declare (indent 0) (debug t))
  `(let* ((file (make-temp-file "ghostherd-log-test" nil ".eld"))
          (ghostherd-log-file file)
          (ghostherd--log nil)
          (ghostherd--log-loaded nil)
          (ghostherd--log-writable t)
          (ghostherd--log-disk-lines 0)
          (ghostherd--log-disk-screens 0)
          (ghostherd-log-days 7)
          (ghostherd--log-seq 0)
          (ghostherd-log-max 500)
          (ghostherd-log-screens 20))
     (unwind-protect
         (cl-letf (((symbol-function 'ghostherd--notify) (lambda (&rest _) nil)))
           ,@body)
       (ignore-errors (delete-file file)))))

(defun ghostherd-tests--log-lines (file)
  "Return the lines of FILE."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents file))
    (split-string (buffer-string) "\n" t)))

(ert-deftest ghostherd-test-log-survives-a-restart ()
  "The point of the whole thing: what the previous Emacs saw is still
there, screens included, because a rule that fired at 02:00 is otherwise
unarguable by morning."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log-file
      (ghostherd--log-add "rev" 'state "working → blocked  Approve?"
                          "❯ 1. Yes\n│ box │\n> ")
      (ghostherd--log-add "rev" 'input "← go on")
      ;; A second Emacs, same file.
      (let ((ghostherd--log nil)
            (ghostherd--log-loaded nil))
        (should (= (ghostherd-log-load) 2))
        ;; Newest first, and one marker on top saying where the seam is.
        (should (= (length ghostherd--log) 3))
        (should (string-match-p "resumed 2 entries"
                                (ghostherd-log-entry-text (car ghostherd--log))))
        (let ((blocked (cl-find 'state ghostherd--log
                                :key #'ghostherd-log-entry-kind)))
          (should (equal (ghostherd-log-entry-session blocked) "rev"))
          (should (equal (ghostherd-log-entry-text blocked)
                         "working → blocked  Approve?"))
          (should (equal (ghostherd-log-entry-screen blocked)
                         "❯ 1. Yes\n│ box │\n> ")))))))

(ert-deftest ghostherd-test-log-is-one-line-per-entry ()
  "A screen is forty lines of box drawing, and it has to print as one:
appending is only cheap because a crash can damage nothing but the last
line."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log-file
      (ghostherd--log-add "a" 'state "one" "top\nmiddle\nbottom")
      (ghostherd--log-add "a" 'state "two")
      (should (= (length (ghostherd-tests--log-lines file)) 2)))))

(ert-deftest ghostherd-test-log-print-settings-are-not-the-users ()
  "`print-length' and `print-level' are user settings, and writing
\"...\" into somebody's log instead of their agent's screen would be a
silent corruption."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log-file
      (let ((print-length 2) (print-level 1))
        (ghostherd--log-add "a" 'state "text" "s1\ns2\ns3"))
      (let ((ghostherd--log nil) (ghostherd--log-loaded nil))
        (ghostherd-log-load)
        (let ((entry (cl-find 'state ghostherd--log
                              :key #'ghostherd-log-entry-kind)))
          (should (equal (ghostherd-log-entry-text entry) "text"))
          (should (equal (ghostherd-log-entry-screen entry) "s1\ns2\ns3")))))))

(ert-deftest ghostherd-test-log-lines-are-plists-not-structs ()
  "Printing the struct would read back into the struct *as it is then*:
one added slot and every line written before today comes back with its
fields shifted along."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log-file
      (ghostherd--log-add "a" 'life "spawned")
      (let ((form (car (read-from-string (car (ghostherd-tests--log-lines file))))))
        (should (listp form))
        (should-not (recordp form))
        (should (equal (plist-get form :text) "spawned"))))))

(ert-deftest ghostherd-test-log-tolerates-a-truncated-tail ()
  "The one corruption to expect: Emacs died mid-write."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log-file
      (ghostherd--log-add "a" 'state "first")
      (let ((coding-system-for-write 'utf-8))
        (write-region "(:time (26000 1) :session \"a\" :kind sta" nil file t 'silent))
      (let ((ghostherd--log nil) (ghostherd--log-loaded nil))
        (should (= (ghostherd-log-load) 1))
        (should (cl-find "first" ghostherd--log
                         :key #'ghostherd-log-entry-text :test #'equal))))))

(ert-deftest ghostherd-test-log-compacts-on-load ()
  "Appending is what keeps logging cheap, so the file grows with the
session and the caps are applied once, when something is reading anyway."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log-file
      (dotimes (i 8)
        (ghostherd--log-add "a" 'state (format "entry %d" i) (format "screen %d" i)))
      (should (= (length (ghostherd-tests--log-lines file)) 8))
      (let ((ghostherd--log nil)
            (ghostherd--log-loaded nil)
            (ghostherd-log-max 5)
            (ghostherd-log-screens 2))
        (should (= (ghostherd-log-load) 5))
        ;; The cap holds *including* the resume marker: it is a log line
        ;; like any other, so the oldest of the five kept is pushed out.
        (should (= (length ghostherd--log) 5))
        ;; Only the newest two still carry a screen -- the screens are the
        ;; actual disk.
        (should (= (length (seq-filter #'ghostherd-log-entry-screen ghostherd--log))
                   2))
        (should (equal (ghostherd-log-entry-screen
                        (cl-find 'state ghostherd--log
                                 :key #'ghostherd-log-entry-kind))
                       "screen 7"))
        ;; And the file was rewritten rather than appended to, so it does
        ;; not grow forever: five compacted lines plus the marker.
        (should (= (length (ghostherd-tests--log-lines file)) 6))))))

(ert-deftest ghostherd-test-log-compacts-on-write ()
  "An Emacs that stays up for weeks never calls `ghostherd-log-load'
again; `ghostherd--log-add' rewrites the file once lines or screens on
disk reach twice the cap."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log-file
      (let ((ghostherd-log-max 5)
            (ghostherd-log-screens 2))
        ;; Four screens reach 2x `ghostherd-log-screens' and compact on disk.
        (dotimes (i 4)
          (ghostherd--log-add "a" 'state (format "s %d" i) (format "screen %d" i)))
        (let* ((forms (mapcar (lambda (l) (car (read-from-string l)))
                              (ghostherd-tests--log-lines file)))
               (screens (seq-filter (lambda (f) (plist-get f :screen)) forms)))
          (should (= (length forms) 4))
          (should (= (length screens) 2))
          (should (equal (mapcar (lambda (f) (plist-get f :screen)) screens)
                         '("screen 2" "screen 3"))))
        ;; Many plain lines without screens also compact at 2x `ghostherd-log-max'.
        (dotimes (i 12)
          (ghostherd--log-add "a" 'state (format "line %d" i)))
        (let ((lines (ghostherd-tests--log-lines file)))
          (should (< (length lines) (* 2 ghostherd-log-max)))
          (should (equal (plist-get (car (read-from-string (car (last lines)))) :text)
                         "line 11")))))))

(ert-deftest ghostherd-test-log-keeps-going-when-the-file-cannot-be-written ()
  "The write happens per state transition, so a read-only directory must
cost one warning rather than one message per transition."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log-file
      (let ((ghostherd-log-file (expand-file-name "nope/deeper/log.eld" file))
            (warnings 0))
        (cl-letf (((symbol-function 'display-warning)
                   (lambda (&rest _) (setq warnings (1+ warnings)))))
          (ghostherd--log-add "a" 'state "first")
          (ghostherd--log-add "a" 'state "second"))
        (should (= warnings 1))
        (should-not ghostherd--log-writable)
        (should (= (length ghostherd--log) 2))))))

(ert-deftest ghostherd-test-log-clear-forgets-the-file-too ()
  "Quitting Emacs used to do this by accident."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log-file
      (ghostherd--log-add "a" 'state "first")
      (should (file-exists-p file))
      (ghostherd-log-clear)
      (should-not ghostherd--log)
      (should-not (file-exists-p file)))))

(ert-deftest ghostherd-test-log-file-nil-keeps-it-in-memory ()
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log-file
      (let ((ghostherd-log-file nil))
        (ghostherd--log-add "a" 'state "first")
        (should (= (length ghostherd--log) 1))
        (should (= (length (ghostherd-tests--log-lines file)) 0))))))

;;; What the agent says about itself

(defun ghostherd-tests--reporting (screen state &optional reason)
  "Detect state for a fake agy session showing SCREEN that reported STATE."
  (let ((s (ghostherd-tests--session :name "a" :kind 'agy :state 'idle)))
    (with-current-buffer (ghostherd-session-buffer s) (insert screen))
    (cl-letf (((symbol-function 'run-with-timer) (lambda (&rest _) nil)))
      (ghostherd-report s state reason))
    (ghostherd--detect-state s)))

(ert-deftest ghostherd-test-report-beats-a-quiet-screen ()
  "The whole point: an idle-looking prompt is what the rules fall back to,
and the agent knows better."
  (ghostherd-tests--with-herd ()
    (should (equal (ghostherd-tests--reporting "> \n" 'blocked "may I rm -rf?")
                   '(blocked . "may I rm -rf?")))))

(ert-deftest ghostherd-test-report-without-a-reason-says-so ()
  (ghostherd-tests--with-herd ()
    (should (equal (ghostherd-tests--reporting "> \n" 'done)
                   '(done . "reported done")))))

(ert-deftest ghostherd-test-screen-blocked-still-beats-a-report ()
  "A prompt on the screen is the one thing nothing may mask -- not a stale
progress report, and not a stale report from the agent either."
  (ghostherd-tests--with-herd ()
    (should (eq (car (ghostherd-tests--reporting "Do you want to proceed\n"
                                                 'working))
                'blocked))))

(ert-deftest ghostherd-test-screen-working-vetoes-a-claimed-blocked ()
  "`working' patterns match a spinner, and an agent generating tokens is
not sitting on a prompt whatever its last hook said.  `idle' gets no such
veto -- it is the fallback, not evidence."
  (ghostherd-tests--with-herd ()
    (should (eq (car (ghostherd-tests--reporting "Thinking…\n" 'blocked))
                'working))
    (should (eq (car (ghostherd-tests--reporting "> \n" 'blocked))
                'blocked))))

(ert-deftest ghostherd-test-report-expires ()
  "Expiry costs nothing when the report was true -- the prompt is still on
the screen -- and is the only cure when it was stale."
  (ghostherd-tests--with-herd ((ghostherd-report-ttl 60))
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy :state 'idle)))
      (with-current-buffer (ghostherd-session-buffer s) (insert "> \n"))
      (puthash "a" (list 'blocked "old news"
                         (time-subtract (current-time) 61))
               ghostherd--reports)
      (should (eq (car (ghostherd--detect-state s)) 'idle))
      (should-not (ghostherd--fresh-report s)))))

(ert-deftest ghostherd-test-report-cannot-claim-dead ()
  "Liveness is the host's answer, and an agent able to report is not dead."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy)))
      (should-error (ghostherd-report s 'dead) :type 'user-error))))

(ert-deftest ghostherd-test-report-auto-withdraws-it ()
  "Unlike `ghostherd-mark-state', which is sticky by design."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy :state 'idle)))
      (with-current-buffer (ghostherd-session-buffer s) (insert "> \n"))
      (cl-letf (((symbol-function 'run-with-timer) (lambda (&rest _) nil)))
        (ghostherd-report s 'blocked)
        (should (eq (car (ghostherd--detect-state s)) 'blocked))
        (ghostherd-report s 'auto))
      (should (eq (car (ghostherd--detect-state s)) 'idle)))))

(ert-deftest ghostherd-test-report-does-not-poll-on-the-spot ()
  "The shortest way here is `ghostel_cmd', dispatched inside ghostel's VT
parser: a capture there is a subprocess in the middle of drawing a
terminal.  Recording is not deciding, the same as OSC progress."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy))
          (polled nil)
          (deferred nil))
      (cl-letf (((symbol-function 'ghostherd-poll-session)
                 (lambda (&rest _) (setq polled t)))
                ((symbol-function 'run-with-timer)
                 (lambda (&rest _) (setq deferred t))))
        (ghostherd-report s 'blocked))
      (should-not polled)
      (should deferred))))

(ert-deftest ghostherd-test-reported-done-is-not-held-by-idle-settle ()
  "Hysteresis exists because screens lie between two tool calls.  A hook
firing at the moment the CLI stops needs none, and would be ruined by it."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy :state 'working
                                       :seen nil)))
      (with-current-buffer (ghostherd-session-buffer s) (insert "> \n"))
      (cl-letf (((symbol-function 'run-with-timer) (lambda (&rest _) nil)))
        (ghostherd-report s 'done "finished the review"))
      (should (eq (ghostherd-poll-session s) 'done))
      (should (equal (ghostherd-session-state-reason s) "finished the review")))))

(ert-deftest ghostherd-test-reported-done-while-watching-does-not-banner ()
  "`done' means finished while you were not looking, whoever said so."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy :state 'working
                                       :seen nil)))
      (with-current-buffer (ghostherd-session-buffer s) (insert "> \n"))
      (cl-letf (((symbol-function 'run-with-timer) (lambda (&rest _) nil))
                ((symbol-function 'ghostherd--watched-p) (lambda (_s) t)))
        (ghostherd-report s 'done)
        (should (eq (ghostherd-poll-session s) 'idle))))))

(ert-deftest ghostherd-test-a-reported-done-once-seen-stays-seen ()
  "Seen in the room, or in its buffer, an agent's own `done' is seen for
good: looking away while its report is still trusted does not make it
`done' again, nor announce it twice.  The next report is news."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy :state 'working
                                       :seen nil))
          (watching t)
          (told 0))
      (with-current-buffer (ghostherd-session-buffer s) (insert "> \n"))
      (cl-letf (((symbol-function 'run-with-timer) (lambda (&rest _) nil))
                ((symbol-function 'ghostherd--watched-p) (lambda (_s) watching))
                ((symbol-function 'ghostherd--maybe-notify-state)
                 (lambda (_s _old new) (when (eq new 'done) (cl-incf told)))))
        (ghostherd-report s 'done "finished its turn")
        (should (eq (ghostherd-poll-session s) 'idle))
        ;; Still its own word -- now that it is idle -- for the time left.
        (should (eq (car (ghostherd--fresh-report s)) 'idle))
        (setq watching nil)
        (should (eq (ghostherd-poll-session s) 'idle))
        (should (eq (ghostherd-poll-session s) 'idle))
        (should (= told 0))
        ;; Its next turn ends: that is done, and said.
        (ghostherd-report s 'done "finished its turn")
        (should (eq (ghostherd-poll-session s) 'done))
        (should (= told 1))
        ;; Unseen, it is held past the report's trust, as before.
        (puthash (ghostherd-session-id s) (list 'done "old" (time-subtract nil 3600)) ghostherd--reports)
        (should (eq (ghostherd-poll-session s) 'done))))))

(ert-deftest ghostherd-test-waiting-is-not-done ()
  "A turn ended with sub-agents running, as the sidecar says: `waiting'.
Seen live (2026-10-15) as `idle' -- a `done' banner had you looked away,
and the ask it answered settled with \"still in progress\".  Held past
the report's trust, since the screen would call it idle, until a
`working' screen, a prompt or its next report says otherwise."
  (ghostherd-tests--with-herd ((ghostherd-report-ttl 60)
                               (ghostherd--asks-open (make-hash-table :test 'equal)))
    (let* ((s (ghostherd-tests--session :name "a" :kind 'agy :state 'working :seen nil))
           (screen (lambda (text)
                     (with-current-buffer (ghostherd-session-buffer s)
                       (erase-buffer)
                       (insert text))))
           bannered settled)
      (funcall screen "> \n")
      (puthash "a1" "a" ghostherd--asks-open)
      (cl-letf (((symbol-function 'run-with-timer) #'ignore)
                ((symbol-function 'ghostherd--watched-p) #'ignore)
                ((symbol-function 'ghostherd--notify) (lambda (&rest _) (setq bannered t)))
                ((symbol-function 'ghostherd--ask-settle)
                 (lambda (id &rest _) (push id settled))))
        (ghostherd--herd-take-reports
         '((:session "a" :state "waiting" :reason "waiting on 4 sub-agents")))
        (should (eq (ghostherd-poll-session s) 'waiting))
        (should (equal (ghostherd-session-state-reason s) "waiting on 4 sub-agents"))
        ;; An hour on, its screen an idle prompt all along.
        (puthash "a" (list 'waiting "waiting on 4 sub-agents" (time-subtract nil 3600))
                 ghostherd--reports)
        (should (eq (ghostherd-poll-session s) 'waiting))
        ;; A sub-agent back wakes it before its hook says so.
        (funcall screen "Thinking…\n")
        (should (eq (ghostherd-poll-session s) 'working))
        (funcall screen "> \n")
        (should (eq (ghostherd-poll-session s) 'waiting))
        ;; A prompt sent to it: not awake to it yet, nor waiting.
        (puthash "a" (float-time) ghostherd--input-at)
        (should (eq (ghostherd-poll-session s) 'working))
        (remhash "a" ghostherd--input-at)
        (should (eq (ghostherd-poll-session s) 'waiting))
        (should-not bannered)
        (should-not settled)
        ;; The turn that ends with nothing out.
        (ghostherd--herd-take-reports
         '((:session "a" :state "done" :reason "finished its turn")))
        (should (eq (ghostherd-poll-session s) 'done))
        (should bannered)
        (should (equal settled '("a1")))))))

(ert-deftest ghostherd-test-waiting-reads-as-such ()
  "Its own glyph, counted on the mode line, in the legend; its box free
for a message once it is ready."
  (ghostherd-tests--with-herd ((ghostherd--log nil))
    (let ((s (ghostherd-tests--session :name "a" :kind 'claude :state 'waiting))
          said)
      (should (equal (ghostherd--state-glyph 'waiting) "◷"))
      (should-not (eq (ghostherd--state-face 'waiting) 'default))
      (should (string-match-p "◷1" (substring-no-properties (ghostherd--mode-line-segment))))
      (should (string-match-p "◷ waiting" (ghostherd--sidebar-help-text)))
      (ghostherd--when-ready s (lambda () (push 'told said)) 0 (lambda () (push 'never said)))
      (should (equal said '(told))))))

(ert-deftest ghostherd-test-rename-carries-the-report ()
  "The report is keyed by id, and a rename changes the id -- which used to
drop the input grace and the settle timestamp silently."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy :state 'idle)))
      (with-current-buffer (ghostherd-session-buffer s) (insert "> \n"))
      (cl-letf (((symbol-function 'run-with-timer) (lambda (&rest _) nil)))
        (ghostherd-report s 'blocked "waiting on you"))
      (ghostherd-rename s "b")
      (should (equal (ghostherd--fresh-report s) '(blocked . "waiting on you")))
      (should-not (gethash "a" ghostherd--reports)))))

(ert-deftest ghostherd-test-cmd-report-is-how-a-hook-calls-in ()
  "Strings in, because this arrives through `ghostel_cmd' word-splitting."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "impl" :kind 'agy :state 'idle)))
      (with-current-buffer (ghostherd-session-buffer s)
        (with-current-buffer (ghostherd-session-buffer s) (insert "> \n"))
        (cl-letf (((symbol-function 'run-with-timer) (lambda (&rest _) nil)))
          (ghostherd-cmd-report "self" "blocked" "chose a file"))
        (should (equal (ghostherd--detect-state s)
                       '(blocked . "chose a file")))))))

(provide 'ghostherd-log-tests)
;;; ghostherd-log-tests.el ends here

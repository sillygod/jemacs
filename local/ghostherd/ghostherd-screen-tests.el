;;; ghostherd-screen-tests.el --- Tests for ghostherd-screen.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Part of ghostherd's tests; ghostherd-tests.el runs them all.  This
;; file alone:
;;
;;   emacs --batch --init-directory=~/.emacs.d/emacs-home/ \
;;         -L . -L ../xwapp -l ghostherd-screen-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ghostherd-test-helpers)

(declare-function ghostherd-inbox--agent-items "ghostherd-memory-page" (a))
(declare-function ghostherd-memory-page--agent "ghostherd-memory-page" (s))
(declare-function ghostherd-memory-page--handle "ghostherd-memory-page" (intent))
(declare-function ghostherd-memory-page--room-shows-p "ghostherd-memory-page" (s))

;;; Screen rule matching

(defconst ghostherd-tests--rules
  '((blocked . ("Do you want to proceed" "permission"))
    (working . ("esc to interrupt" "Thinking"))
    (idle    . ("^> ")))
  "A miniature rule set with one pattern per state that can co-occur.")

(ert-deftest ghostherd-test-match-rules-prefers-blocked ()
  "Blocked outranks working and idle even when all three match."
  (let ((text "Thinking\nesc to interrupt\nDo you want to proceed\n> "))
    (should (equal (ghostherd--match-rules text ghostherd-tests--rules)
                   '(blocked . "Do you want to proceed")))))

(ert-deftest ghostherd-test-match-rules-prefers-working-over-idle ()
  (let ((text "Thinking\n> "))
    (should (equal (car (ghostherd--match-rules text ghostherd-tests--rules))
                   'working))))

(ert-deftest ghostherd-test-match-rules-no-match-is-nil ()
  (should-not (ghostherd--match-rules "nothing here" ghostherd-tests--rules)))

(ert-deftest ghostherd-test-rules-ignore-ambient-case-fold ()
  "The bug was never the value, it was that there was no value: rules
matched under whatever `case-fold-search' the buffer current at poll
time happened to have, so one pattern could mean two things on two
consecutive ticks.  Nothing a rule author writes -- or can see --
chooses that buffer."
  (let ((text "Do you want to proceed"))
    (dolist (ambient '(t nil))
      (let ((case-fold-search ambient))
        (should (ghostherd--rule-matches-p "Do you want to proceed" text))
        (should-not (ghostherd--rule-matches-p "do you want to proceed" text))))))

(ert-deftest ghostherd-test-rules-are-case-sensitive-both-ways ()
  "Case-sensitive is the authored intent: the patterns were written by
reading an agent's screen and copying what was on it.  It is also the
safer direction under `prefer a missed blocked to a false one' --
folding made `Running' match the word \"running\" in ordinary prose."
  (let ((rules '((working . ("Running"))))
        (case-fold-search t))
    (should (equal (ghostherd--match-rules "Running tests\n" rules)
                   '(working . "Running")))
    (should-not (ghostherd--match-rules "running the suite\n" rules))
    ;; ...and a rule that wants both says so, rather than hoping.
    (should (ghostherd--match-rules
             "running the suite\n" '((working . ("[Rr]unning")))))))

(ert-deftest ghostherd-test-explain-agrees-with-detection ()
  "`ghostherd-explain' exists to show why a rule won or lost, so it has
to match on exactly the same terms -- an explain that folds where
detection does not would confidently explain a match that never
happened."
  (let ((rules '((blocked . ("Approve")) (idle . ("^> "))))
        (text "approve this\n> ")
        (case-fold-search t))
    ;; The winner falls through to idle rather than a folded `blocked'...
    (should (equal (ghostherd--match-rules text rules) '(idle . "^> ")))
    ;; ...and explain must not list a blocked hit that detection did not see.
    (should-not (cl-find 'blocked (ghostherd--match-all-rules text rules)
                         :key #'car))))

(ert-deftest ghostherd-test-match-all-rules-reports-losers ()
  "`--match-all-rules' is what makes `ghostherd-explain' able to show why
a rule lost, so it must report every hit, in precedence order."
  (let* ((text "Thinking\nesc to interrupt\nDo you want to proceed\n> ")
         (hits (ghostherd--match-all-rules text ghostherd-tests--rules)))
    (should (equal (mapcar #'car hits) '(blocked working working idle)))
    (should (member '(working . "Thinking") hits))))

;;; What a state reason says

(ert-deftest ghostherd-test-reason-is-the-screen-not-the-pattern ()
  "The reason used to hold the regexp that matched -- a debugging
artifact that leaked into the notification, the switcher and the JSON
listing, so \"needs attention\" was followed by a pattern rather than by
the question."
  (let* ((hit '(blocked . "Do you want to proceed"))
         (reason (ghostherd--rule-reason
                  ghostherd-tests--permission-screen hit)))
    (should-not (equal reason (cdr hit)))
    (should (string-match-p "Do you want to proceed\\?" reason))
    ;; and none of the box it was drawn in
    (should-not (string-match-p "[│╭╰]" reason))))

(ert-deftest ghostherd-test-reason-leads-with-the-subject ()
  "The matched line is the generic half.  What you are being asked to
approve is above it, and it is the half that must survive truncation."
  (let ((reason (ghostherd--rule-reason
                 ghostherd-tests--permission-screen
                 '(blocked . "Do you want to proceed"))))
    (should (string-prefix-p "Bash(rm -rf build/ && npm publish)" reason))
    (should (< (cl-search "Bash" reason) (cl-search "proceed" reason)))))

(ert-deftest ghostherd-test-reason-subject-must-carry-words ()
  "Caught on the herd log's first run: agent CLIs draw logos out of block
glyphs, and the subject lookup happily picked one up, so a reason came
out as a row of half-blocks followed by the command actually typed.
Enumerating the glyphs is a losing game; requiring words is not."
  (let ((screen "▛▀▖ ▞▀▖\n▙▄▘ ▝▄▘\n> /model\n"))
    (should (equal (ghostherd--rule-reason screen '(idle . "^> ")) "/model")))
  ;; a real subject still comes through
  (let ((screen "Bash(npm publish)\n> /model\n"))
    (should (equal (ghostherd--rule-reason screen '(idle . "^> "))
                   "Bash(npm publish) — /model"))))

(ert-deftest ghostherd-test-reason-context-is-bounded ()
  "Looking back forever would attribute any earlier output to the prompt."
  (let* ((screen (concat "the subject\n" (make-string 8 ?\n)
                         "Do you want to proceed?\n"))
         (hit '(blocked . "Do you want to proceed")))
    (let ((ghostherd-reason-context 3))
      (should-not (string-match-p "subject"
                                  (ghostherd--rule-reason screen hit))))
    (let ((ghostherd-reason-context 20))
      (should (string-match-p "subject"
                              (ghostherd--rule-reason screen hit))))))

(ert-deftest ghostherd-test-reason-falls-back-for-bare-prompts ()
  "An anchored prompt cleans away to nothing.  Dressing it with whatever
happened to precede it would attribute an unrelated line to the reason,
so it keeps the pattern instead."
  (should (equal (ghostherd--rule-reason "some earlier output\n> \n"
                                         '(idle . "^> "))
                 "^> ")))

(ert-deftest ghostherd-test-detect-state-reason-comes-from-the-screen ()
  "Pins the call site, not just the helper.  A mutation that rewires
`ghostherd--detect-state' back to storing the pattern passed every test
written against `ghostherd--rule-reason' alone -- which is precisely the
bug that shipped for two phases."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy :backend 'fake))
          (ghostherd-tests--fake-screen ghostherd-tests--permission-screen))
      (pcase-let ((`(,state . ,reason) (ghostherd--detect-state s)))
        (should (eq state 'blocked))
        ;; not the pattern...
        (should-not (member reason
                            (alist-get 'blocked
                                       (plist-get (ghostherd--spec 'agy)
                                                  :screen-rules))))
        ;; ...but what was on the screen
        (should (string-match-p "npm publish" reason))))))

(ert-deftest ghostherd-test-detect-working-reason-comes-from-the-screen ()
  "Same for the other state a rule can win: the =working= branch is a
separate call site and was missed once already."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy :backend 'fake))
          (ghostherd-tests--fake-screen "  compiling the parser\n  Working…\n"))
      (pcase-let ((`(,state . ,reason) (ghostherd--detect-state s)))
        (should (eq state 'working))
        (should-not (equal reason "Working"))
        (should (string-match-p "compiling the parser" reason))))))

(ert-deftest ghostherd-test-an-unchanged-screen-is-read-once ()
  "A screen the same as the last poll's is not read for its state again:
its rules, its reason, whether to keep it -- found with the screen, the
answer as read the first time.  Another screen is read; so is the same
one under other settings."
  (ghostherd-tests--with-herd ((ghostherd--log nil) (ghostherd--screens (make-hash-table :test 'equal)))
    (let* ((s (ghostherd-tests--session :name "a" :kind 'agy :backend 'fake))
           (reads 0)
           (match (symbol-function 'ghostherd--match-rules))
           (reason (symbol-function 'ghostherd--rule-reason))
           (fresh (lambda () (concat "  compiling the parser\n" "  Working…\n"))))
      (cl-letf (((symbol-function 'ghostherd--match-rules) (lambda (&rest a) (cl-incf reads) (apply match a)))
                ((symbol-function 'ghostherd--rule-reason) (lambda (&rest a) (cl-incf reads) (apply reason a))))
        (let ((first (ghostherd--detect-state s (funcall fresh))))
          (should (eq (car first) 'working))
          (should (string-match-p "compiling the parser" (cdr first)))
          (should (= reads 2))
          (ghostherd--auto-capture s)                ; reads the rules of the same screen
          (should (= reads 2))
          ;; The next poll's screen, the same text as another string.
          (should (equal (ghostherd--detect-state s (funcall fresh)) first))
          (should (= reads 2))
          (let ((ghostherd-reason-context 0))         ; another setting: read again
            (ghostherd--detect-state s (funcall fresh))
            (should (= reads 3)))
          (should (eq (car (ghostherd--detect-state s ghostherd-tests--permission-screen)) 'blocked))
          (should (= reads 5)))))
    ;; What claude's foot says besides the state: read once too.
    (let* ((c (ghostherd-tests--session :name "c" :kind 'claude :backend 'fake))
           (feet 0)
           (foot (symbol-function 'ghostherd--foot))
           (screen (lambda () (concat "⏺ Done.\n\n" (make-string 40 ?─) "\n❯ run the tests\n"
                                      (make-string 40 ?─) "\n  1 shell · ? for shortcuts\n"))))
      (cl-letf (((symbol-function 'ghostherd--foot) (lambda (&rest a) (cl-incf feet) (apply foot a)))
                ((symbol-function 'ghostherd-backend-styled-screen) #'ignore))
        (ghostherd--detect-state c (funcall screen))
        (ghostherd--detect-state c (funcall screen))
        (should (= feet 1))
        (should (equal (gethash "c" ghostherd--feet) '(:background ("1 shell") :input "run the tests" :suggestion nil)))))))

(ert-deftest ghostherd-test-the-mode-line-is-made-again-when-a-count-changes ()
  "The segment is made once for the herd's counts, and a fresh copy is
given each time, as it always was."
  (ghostherd-tests--with-herd ((ghostherd--mode-line-kept nil))
    (let ((a (ghostherd-tests--session :name "a" :kind 'claude :state 'working))
          (made 0))
      (ghostherd-tests--session :name "b" :kind 'claude :state 'blocked)
      (cl-letf* ((make (symbol-function 'ghostherd--mode-line-make))
                 ((symbol-function 'ghostherd--mode-line-make) (lambda (&rest a) (cl-incf made) (apply make a))))
        (let ((one (ghostherd--mode-line-segment))
              (two (ghostherd--mode-line-segment)))
          (should (equal (substring-no-properties one) " 🐄2 ⚠1 ⟳1"))
          (should (eq (get-text-property 1 'face one) 'error))
          (should (equal-including-properties one two))
          (should-not (eq one two))
          (should (= made 1))
          (setf (ghostherd-session-state a) 'done)
          (should (equal (substring-no-properties (ghostherd--mode-line-segment)) " 🐄2 ⚠1 ✓1"))
          (should (= made 2)))))))

(ert-deftest ghostherd-test-a-project-name-is-kept-for-an-absolute-path ()
  "Written once per absolute path; a relative one, from where it is asked;
another `directory-abbrev-alist', written again."
  (let ((ghostherd--project-names (make-hash-table :test 'equal))
        (ghostherd--project-names-by nil)
        (directory-abbrev-alist nil))
    (should (equal (ghostherd--project-name (expand-file-name "~/src/x")) "~/src/x/"))
    (should (equal (ghostherd--project-name "/opt/p") "/opt/p/"))
    (should (= (hash-table-count ghostherd--project-names) 2))
    (let ((default-directory "/opt/"))
      (should (equal (ghostherd--project-name "q") "/opt/q/")))
    (let ((default-directory "/srv/"))
      (should (equal (ghostherd--project-name "q") "/srv/q/")))
    (should (= (hash-table-count ghostherd--project-names) 2))
    (let ((directory-abbrev-alist '(("\\`/opt/" . "/o/"))))
      (should (equal (ghostherd--project-name "/opt/p") "/o/p/")))))

(ert-deftest ghostherd-test-reason-cannot-change-state ()
  "It is a display heuristic and is allowed to be one, on the condition
that it never reaches the state machine."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy :backend 'fake))
          (ghostherd-tests--fake-screen ghostherd-tests--permission-screen))
      (dolist (context '(0 3 50))
        (let ((ghostherd-reason-context context))
          (should (eq (car (ghostherd--detect-state s)) 'blocked)))))))

(ert-deftest ghostherd-test-claude-login-menu-is-blocked ()
  "The /login method picker reads as blocked with its options, by the
general menu rule -- `ghostherd--prompt' on a box that replaced the input
-- not a rule written for it.  Real sample, ask a74e0a82."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "lead-x" :kind 'claude :backend 'fake))
          (ghostherd-tests--fake-screen ghostherd-tests--claude-login-menu))
      (should (eq (car (ghostherd--detect-state s)) 'blocked))
      (ghostherd-poll-session s)
      (should (eq (ghostherd-session-state s) 'blocked))
      (let ((p (ghostherd-session-prompt s)))
        (should p)
        (should (= (length (plist-get p :options)) 3))
        (should (string-match-p "Claude account"
                                (cdr (assq 1 (plist-get p :options))))))
      (should (string-match-p "login method" (cdr (ghostherd--detect-state s)))))))

(ert-deftest ghostherd-test-a-menu-quoted-in-the-conversation-does-not-block ()
  "The `:prompt-replaces-input' guard holds: the same menu with a real
input box still drawn under it -- quoted in the chat, not the live one --
does not block."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "lead-y" :kind 'claude :backend 'fake))
          (ghostherd-tests--fake-screen
           (concat ghostherd-tests--claude-login-menu
                   "\n────────────────────────────────────────\n"
                   "❯ \n"
                   "────────────────────────────────────────\n"
                   "  ⏵⏵ auto mode on\n")))
      (should-not (eq (car (ghostherd--detect-state s)) 'blocked)))))

(ert-deftest ghostherd-test-cli-error-reads-as-blocked ()
  "A turn that ends on a CLI error -- login expired, the computer slept --
reads as blocked with a plain reason, not idle.  Ask a74e0a82."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "c" :kind 'claude :backend 'fake))
          (box "\n────────────────────\n❯ \n────────────────────\n  ⏵⏵ auto mode on\n"))
      (dolist (case `(("⏺ Login expired · Please run /login" . "login expired")
                      ("⏺ API Error: Your computer went to sleep mid-response" . "computer slept")))
        (let ((ghostherd-tests--fake-screen (concat "⏺ earlier work\n\n" (car case) box)))
          (pcase-let ((`(,state . ,reason) (ghostherd--detect-state s)))
            (should (eq state 'blocked))
            (should (string-match-p (cdr case) reason)))))
      ;; The login one carries the `login' action for the card.
      (let ((ghostherd-tests--fake-screen (concat "⏺ Login expired · Please run /login" box)))
        (should (eq (cdr (ghostherd--cli-error s ghostherd-tests--fake-screen)) 'login))))))

(ert-deftest ghostherd-test-a-cli-error-holds-its-open-ask ()
  "A CLI error leaves the taker's ask open: it reads as blocked, and a
blocked taker is not settled -- so the asker gets the real answer later,
not the error screen."
  (ghostherd-tests--with-herd ()
    (let* ((s (ghostherd-tests--session :name "c" :kind 'claude :backend 'fake))
           (ghostherd--asks-open (make-hash-table :test 'equal))
           (ghostherd--asks-settled (make-hash-table :test 'equal))
           (settled nil))
      (puthash "ask1" (ghostherd-session-id s) ghostherd--asks-open)
      (cl-letf (((symbol-function 'ghostherd--ask-settle)
                 (lambda (&rest _) (setq settled t))))
        (ghostherd--set-state s 'working "on it")
        (let ((ghostherd-tests--fake-screen
               "⏺ Login expired · Please run /login\n────────\n❯ \n────────\n"))
          (ghostherd-poll-session s))
        (should (eq (ghostherd-session-state s) 'blocked))
        (should-not settled)))))

(ert-deftest ghostherd-test-login-code-screen-surfaces-its-url ()
  "The /login code-entry screen -- a URL to open, a field for the code --
reads as blocked and carries the URL to the card.  Reconstructed from the
flow's shape, not a real capture (a scratch /login shares the user's
credentials); `ghostherd-login-code-re' is tunable against a real sample."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "c" :kind 'claude :backend 'fake))
          (url "https://claude.ai/oauth/authorize?client=cli&state=abc123"))
      (let ((ghostherd-tests--fake-screen
             (concat "Visit this URL to authenticate:\n" url "\n\n"
                     "Paste code here if prompted:\n> \n")))
        (should (eq (car (ghostherd--detect-state s)) 'blocked))
        (should (string-match-p "sign in" (cdr (ghostherd--detect-state s))))
        (should (equal (ghostherd--login-url s ghostherd-tests--fake-screen) url)))
      ;; A plain response that merely links to claude.ai is not the flow.
      (let ((ghostherd-tests--fake-screen "⏺ see https://claude.ai/docs\n────\n❯ \n────\n"))
        (should-not (ghostherd--login-url s ghostherd-tests--fake-screen))))))


;;; Only the CLI's own error ends a turn (ask eb81eb50)
;;
;; Real samples, 2026-10-21.  The screens are auto-captures the herd took
;; when it read each agent as blocked; the transcript entries are claude's
;; own, trimmed of paths and ids.

(defconst ghostherd-tests--quoted-error-screen
  (concat
   "  - 你同意才改： 沒有你點頭，絕不寫進指示檔。\n"
   "  - 升級後： 那條記憶會從記憶裡移除，留一句說明搬去了哪裡，避免同一件事被讀兩次。\n"
   "  - 哪些不升級： 跟這台電腦或你個人有關的，例如路徑、帳號設定、個人偏好，一律留在記憶裡。\n"
   "\n"
   "  第一階段做完，我會連同各專案搬了多少條記憶一起跟你說。\n"
   "\n"
   "✻ Cooked for 20s · done 10:45 PM · 2 shells still running\n"
   "\n"
   "❯ [Image #34]然後問一下 這個是不是誤判block了\n"
   "  ⎿ \u00a0[Image #34]\n"
   "\n"
   "  Ran 1 shell command\n"
   "\n"
   "⏺ 不是誤判，它真的停下來了。看截圖最後一行：\n"
   "\n"
   "  ▎ API Error: Connection lost mid-response. The response above may be incomplete.\n"
   "\n"
   "  claude-jnotes 正在用瀏覽器驗證時網路斷了，那一輪就中途結束。herd 的規則是 claude 的回應出現「API\n"
   "  Error:」就標成 blocked、原因寫「its turn ended on an API\n"
   "  error」，意思是「它停在半路，需要你推一下」，不是在等你回答問題。卡片上的「2 shells\n"
   "  running」是它驗證用的後端還在背景跑，不影響。\n"
   "\n"
   "  現在的處理： 在它的輸入框送一句「繼續」就好。它會從中斷的地方接著做，之前的對話和背景跑的 shell\n"
   "  都還在。它是 jnotes 專案的 agent，不是我這邊的，所以我沒有直接幫它送。\n"
   "\n"
   "  可以改進的地方： 「Connection lost\n"
   "  mid-response」跟之前做的「電腦睡著被打斷」性質一樣，都是暫時性的斷線，不是真的出錯。現在只有睡著那種會\n"
   "  自動接著做，因為當初我們定的是「其他錯誤一律停住等你」。我建議把「Connection lost」也加進自動接續：\n"
   "\n"
   "  - 一樣等 45 秒、每輪最多接一次。網路如果真的還沒恢復，再斷一次就停住，不會一直重試。\n"
   "  - 「Login expired」和其他 API 錯誤（例如額度、權限問題）照舊停住等你。\n"
   "\n"
   "  這是設定裡加一行再補測試的事。要的話我排在共用記憶後面交給 claude-.emacs.d。\n"
   "\n"
   "✻ Churned for 19s · done 10:48 PM · 2 shells still running\n"
   "                                                                 ✔ Update installed · Restart to update\n"
   "─────────────────────────────────────────────────────────────────────────────────────────────────────────\n"
   "❯\u00a0\n"
   "─────────────────────────────────────────────────────────────────────────────────────────────────────────\n"
   "  ⏵⏵ auto mode on · 2 shells · ← for agents · ↓ to manage\n")
  "lead-.emacs.d's whole screen, auto-captured blocked at 22:48.  Its turn
had ended normally: its reply quotes another agent's \"API Error:
Connection lost mid-response\" in a blockquote (claude draws it \"▎\")
and says 「Login expired」 in prose, and the old foot rule read both as
claude's own errors -- the card even offered /login.")

(defconst ghostherd-tests--mentioned-error-screens
  (list
   (concat
   "     '((claude\n"
   "     (\\\"Login expired\\\\\\\\|Please run /login\\\" login \\\"its login expired\n"
   "     -- run /login\\\")\n"
   "     (\\\"went to sleep mid-response\\\" continue \\\"its turn was cut off:\n"
   "     the computer slept\\\")\n"
   "     (\\\"API Error:\\\" nil \\\"its turn ended on an API error\\\"))))\n"
   "     (fo…\n"
   "\n"
   "✻ Whatchamacalliting… (24m 21s · ↓ 89.2k tokens)\n"
   "                                ✔ Update installed · Restart to update\n"
   "────────────────────────────────────────────────────────────────────────\n"
   "❯\u00a0\n"
   "────────────────────────────────────────────────────────────────────────\n"
   "  ⏵⏵ auto mode on (shift+tab to cycle) · ⧉ herd-gate · esc to interru…\n")
   (concat
   "  the existing tests use as sample data.\n"
   "\n"
   "⏺ Running 1 shell command…\n"
   "  ⎿  $ cd /Users/jing/.ghostherd/worktrees/.emacs.d/claude-.e\n"
   "     macs.d-2/local/ghostherd && grep -n \"went to\n"
   "     sleep\\|Login expired\\|API Error\\|cli-error\"\n"
   "     ghostherd-tests.el | head -60\n"
   "\n"
   "✶ Sautéing… (1m 50s · ↓ 6.1k tokens · thought for 5s)\n"
   "                   ✘ Auto-update failed · Run claude doctor\n"
   "─────────────────────────────────────────────────────────────\n"
   "❯\u00a0\n"
   "─────────────────────────────────────────────────────────────\n"
   "  ⏵⏵ auto mode on (shift+tab to cycle) · esc to interrupt\n"))
  "Agents that wrote the error words while working, auto-captured blocked
at 13:44 (claude-.emacs.d setting `ghostherd-cli-errors' live) and 22:53
(this very fix, grepping its tests): the words in tool output, a spinner
under them.")

(defconst ghostherd-tests--connection-lost-screen
  (concat
   "\n"
   "⏺ API Error: Connection lost mid-response. The response above may be incomplete.\n"
   "\n"
   "✻ Cooked for 22m 42s · done 10:42 PM · 2 shells still running\n"
   "                                                                 ✔ Update installed · Restart to update\n"
   "─────────────────────────────────────────────────────────────────────────────────────────────────────────\n"
   "❯\u00a0\n"
   "─────────────────────────────────────────────────────────────────────────────────────────────────────────\n"
   "  ⏵⏵ auto mode on · 2 shells · ← for agents · ↓ to manage\n")
  "claude-jnotes, auto-captured blocked at 22:42: a real \"API Error:
Connection lost mid-response\", drawn as a message of its own above the
turn's end.")

(defconst ghostherd-tests--prompted-after-error-screen
  (concat
   "⏺ API Error: Connection lost mid-response. The response above may be incomplete.\n"
   "\n"
   "✻ Cooked for 22m 42s · done 10:42 PM · 2 shells still running\n"
   "\n"
   "❯ 你說的api error 是什麼\n"
   "\n"
   "✽ Mustering…\n"
   "                                                       ✔ Update installed · Restart to update\n"
   "───────────────────────────────────────────────────────────────────────────────────────────────\n"
   "❯\u00a0\n"
   "───────────────────────────────────────────────────────────────────────────────────────────────\n"
   "  ⏵⏵ auto mode on · 2 shells · esc to interrupt · ← for agents · ↓ to manage\n")
  "claude-jnotes five minutes on, still read as blocked: the error is on
screen, but a prompt was typed since and a new turn is running.")

(defconst ghostherd-tests--claude-entries
  '((dropped .
     "{\"isSidechain\":false,\"type\":\"assistant\",\"timestamp\":\"2026-10-21T14:42:33.859Z\",\"message\":{\"model\":\"<synthetic>\",\"role\":\"assistant\",\"stop_reason\":\"stop_sequence\",\"stop_sequence\":\"\",\"type\":\"message\",\"content\":[{\"type\":\"text\",\"text\":\"API Error: Connection lost mid-response. The response above may be incomplete.\"}]},\"error\":\"server_error\",\"truncatedAfterOutput\":true,\"isApiErrorMessage\":true}")
    (dropped-turn .
     "{\"isSidechain\":false,\"type\":\"system\",\"subtype\":\"turn_duration\",\"durationMs\":1361697,\"messageCount\":7845,\"timestamp\":\"2026-10-21T14:42:33.988Z\",\"isMeta\":false}")
    (prompt-after .
     "{\"isSidechain\":false,\"type\":\"user\",\"message\":{\"role\":\"user\",\"content\":\"你說的api error 是什麼\"},\"timestamp\":\"2026-10-21T14:47:36.634Z\",\"permissionMode\":\"auto\",\"origin\":{\"kind\":\"human\"},\"promptSource\":\"typed\",\"turnOrigin\":\"human\"}")
    (slept .
     "{\"isSidechain\":false,\"type\":\"assistant\",\"timestamp\":\"2026-10-20T10:14:28.796Z\",\"message\":{\"model\":\"<synthetic>\",\"role\":\"assistant\",\"stop_reason\":\"stop_sequence\",\"stop_sequence\":\"\",\"type\":\"message\",\"content\":[{\"type\":\"text\",\"text\":\"API Error: Your computer went to sleep mid-response. The response above may be incomplete.\"}]},\"error\":\"server_error\",\"truncatedAfterOutput\":true,\"isApiErrorMessage\":true}")
    (login .
     "{\"isSidechain\":false,\"type\":\"assistant\",\"timestamp\":\"2026-10-11T01:40:31.714Z\",\"message\":{\"model\":\"<synthetic>\",\"role\":\"assistant\",\"stop_reason\":\"stop_sequence\",\"stop_sequence\":\"\",\"type\":\"message\",\"content\":[{\"type\":\"text\",\"text\":\"Login expired · Please run /login\"}]},\"error\":\"authentication_failed\",\"isApiErrorMessage\":true,\"perTurnEffort\":\"xhigh\"}")
    (login-turn .
     "{\"isSidechain\":false,\"type\":\"system\",\"subtype\":\"turn_duration\",\"durationMs\":611,\"messageCount\":8622,\"timestamp\":\"2026-10-11T01:40:31.727Z\",\"isMeta\":false}")
    (login-cmd .
     "{\"isSidechain\":false,\"type\":\"system\",\"subtype\":\"local_command\",\"content\":\"<command-name>/login</command-name>\\n            <command-message>login</command-message>\\n            <command-args></command-args>\",\"level\":\"info\",\"timestamp\":\"2026-10-11T01:53:42.661Z\",\"isMeta\":false}")
    (login-out .
     "{\"isSidechain\":false,\"type\":\"system\",\"subtype\":\"local_command\",\"content\":\"<local-command-stdout>Login successful</local-command-stdout>\",\"level\":\"info\",\"timestamp\":\"2026-10-11T01:53:42.661Z\",\"isMeta\":false,\"commandRun\":{\"command\":\"login\",\"args\":\"\"}}")
    (stalled .
     "{\"isSidechain\":false,\"type\":\"assistant\",\"timestamp\":\"2026-10-16T16:00:00.858Z\",\"message\":{\"model\":\"<synthetic>\",\"role\":\"assistant\",\"stop_reason\":\"stop_sequence\",\"stop_sequence\":\"\",\"type\":\"message\",\"content\":[{\"type\":\"text\",\"text\":\"API Error: The response stopped arriving. The response above may be incomplete.\"}]},\"error\":\"server_error\",\"truncatedAfterOutput\":true,\"isApiErrorMessage\":true}")
    (offline .
     "{\"isSidechain\":false,\"type\":\"assistant\",\"timestamp\":\"2026-10-09T01:37:31.761Z\",\"message\":{\"model\":\"<synthetic>\",\"role\":\"assistant\",\"stop_reason\":\"stop_sequence\",\"stop_sequence\":\"\",\"type\":\"message\",\"content\":[{\"type\":\"text\",\"text\":\"API Error: Can't reach the API server — check your internet or DNS (ENOTFOUND)\"}]},\"error\":\"server_error\",\"isApiErrorMessage\":true,\"perTurnEffort\":\"xhigh\"}")
    (server .
     "{\"isSidechain\":false,\"type\":\"assistant\",\"timestamp\":\"2026-09-10T06:32:16.346Z\",\"message\":{\"model\":\"<synthetic>\",\"role\":\"assistant\",\"stop_reason\":\"stop_sequence\",\"stop_sequence\":\"\",\"type\":\"message\",\"content\":[{\"type\":\"text\",\"text\":\"API Error: 500 Internal server error. This is a server-side issue, usually temporary — try again in a moment. If it persists, check https://status.claude.com.\"}]},\"error\":\"server_error\",\"isApiErrorMessage\":true,\"apiErrorStatus\":500}")
    (model .
     "{\"isSidechain\":false,\"type\":\"assistant\",\"timestamp\":\"2026-10-21T07:25:20.423Z\",\"message\":{\"model\":\"<synthetic>\",\"role\":\"assistant\",\"stop_reason\":\"stop_sequence\",\"stop_sequence\":\"\",\"type\":\"message\",\"content\":[{\"type\":\"text\",\"text\":\"There's an issue with the selected model (no-such-model-xyz). It may not exist or you may not have access to it. Run --model to pick a different model.\"}]},\"error\":\"model_not_found\",\"isApiErrorMessage\":true,\"apiErrorStatus\":404,\"perTurnEffort\":null}")
    (lead-reply .
     "{\"isSidechain\":false,\"message\":{\"model\":\"claude-opus-5-5\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"不是誤判，它真的停下來了。看截圖最後一行：\\n\\n> API Error: Connection lost mid-response. The response above may be incomplete.\\n\\nclaude-jnotes 正在用瀏覽器驗證時網路斷了，那一輪就中途結束。herd 的規則是 claude 的回應出現「API Error:」就標成 blocked、原因寫「its turn ended on an API error」，意思是「它停在半路，需要你推一下」，不是在等你回答問題。卡片上的「2 shells running」是它驗證用的後端還在背景跑，不影響。\\n\\n**現在的處理：** 在它的輸入框送一句「繼續」就好。它會從中斷的地方接著做，之前的對話和背景跑的 shell 都還在。它是 jnotes 專案的 agent，不是我這邊的，所以我沒有直接幫它送。\\n\\n**可以改進的地方：** 「Connection lost mid-response」跟之前做的「電腦睡著被打斷」性質一樣，都是暫時性的斷線，不是真的出錯。現在只有睡著那種會自動接著做，因為當初我們定的是「其他錯誤一律停住等你」。我建議把「Connection lost」也加進自動接續：\\n\\n- 一樣等 45 秒、每輪最多接一次。網路如果真的還沒恢復，再斷一次就停住，不會一直重試。\\n- 「Login expired」和其他 API 錯誤（例如額度、權限問題）照舊停住等你。\\n\\n這是設定裡加一行再補測試的事。要的話我排在共用記憶後面交給 claude-.emacs.d。\"}],\"stop_reason\":\"end_turn\",\"stop_sequence\":null,\"input_transformations\":[]},\"serverClassifierRequest\":\"a6bdebad-3fd8-475d-8aef-ad9db5b11da8\",\"apiBlockIndex\":1,\"requestRef\":\"e5b169ea-3122-4f98-b092-473db326c61c\",\"requestedModel\":\"claude-opus-5-5[1m]\",\"type\":\"assistant\",\"timestamp\":\"2026-10-21T14:48:30.570Z\",\"advisorModel\":\"claude-opus-5-5\",\"effort\":\"xhigh\",\"perTurnEffort\":\"xhigh\"}")
    (lead-turn .
     "{\"isSidechain\":false,\"type\":\"system\",\"subtype\":\"stop_hook_summary\",\"hookCount\":1,\"hookErrors\":[],\"hookAdditionalContext\":[],\"preventedContinuation\":false,\"stopReason\":\"\",\"hasOutput\":false,\"level\":\"suggestion\",\"timestamp\":\"2026-10-21T14:48:30.678Z\",\"toolUseID\":\"04ab60ea-92f6-4247-8880-d03d8e0e46a7\"}"))
  "Real claude transcript lines.  Its own errors carry `isApiErrorMessage'
and model \"<synthetic>\"; lead-reply is lead-.emacs.d's reply behind
`ghostherd-tests--quoted-error-screen', a model's message with no mark.
The rest is what claude writes after an error: the turn's duration, a
/login run, the stop hook's summary, the next prompt.")

(defun ghostherd-tests--entries (&rest tags)
  "The transcript lines TAGS name in `ghostherd-tests--claude-entries'."
  (mapcar (lambda (tag) (alist-get tag ghostherd-tests--claude-entries)) tags))

(defmacro ghostherd-tests--with-transcript (session lines &rest body)
  "Run BODY with SESSION's hooks naming a transcript of LINES, bound to
`path' so BODY can append to it."
  (declare (indent 2))
  `(let* ((dir (make-temp-file "gh-transcript-" t))
          (path (expand-file-name "conv.jsonl" dir))
          (ghostherd--herd-links (make-hash-table :test 'equal)))
     (unwind-protect
         (progn
           (with-temp-file path
             (insert (string-join ,lines "\n") "\n"))
           (puthash (ghostherd-session-name ,session)
                    (list :cli "claude" :transcript path)
                    ghostherd--herd-links)
           ,@body)
       (delete-directory dir t))))

(defconst ghostherd-tests--idle-screen
  "⏺ earlier work\n\n✻ Baked for 1s\n────────────────────\n❯ \n────────────────────\n  ⏵⏵ auto mode on\n"
  "A claude screen with no error on it.")

(ert-deftest ghostherd-test-a-quoted-error-does-not-block ()
  "An agent quoting or mentioning a CLI error is not stuck on one: not by
its transcript, where its reply is the model's, and not by the screen,
where its last message does not start with the error.  The real false
blocks of 2026-10-21; each would have tripped the old foot rule."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "lead-x" :kind 'claude :backend 'fake)))
      (dolist (screen (cons ghostherd-tests--quoted-error-screen
                            ghostherd-tests--mentioned-error-screens))
        (should (string-match-p "Login expired\\|went to sleep\\|API Error:"
                                (string-join (ghostherd--screen-foot screen 10) "\n")))
        ;; The screen alone: an agent with no transcript known yet.
        (should-not (ghostherd--cli-error s screen))
        (let ((ghostherd-tests--fake-screen screen))
          (should-not (eq (car (ghostherd--detect-state s)) 'blocked))))
      ;; Its transcript: the reply is the last entry, and a model's.
      (ghostherd-tests--with-transcript s (ghostherd-tests--entries 'lead-reply 'lead-turn)
        (let ((ghostherd-tests--fake-screen ghostherd-tests--quoted-error-screen))
          (should (eq (ghostherd--transcript-turn-end s) :clean))
          (should-not (ghostherd--cli-error s))
          (should-not (eq (car (ghostherd--detect-state s)) 'blocked)))))))

(ert-deftest ghostherd-test-claude-own-errors-still-block ()
  "claude's own errors, as its transcript marks them, block with the right
action -- whatever the screen shows -- and so does a real one on the
screen of an agent whose transcript is not known."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "c" :kind 'claude :backend 'fake)))
      (pcase-dolist (`(,tag ,action ,reason)
                     '((login login "login expired")
                       (slept continue "computer slept")
                       (dropped continue "connection dropped")
                       (model nil "selected model")))
        (ghostherd-tests--with-transcript s
            (append (ghostherd-tests--entries 'lead-reply tag) (ghostherd-tests--entries 'login-turn))
          (let ((ghostherd-tests--fake-screen ghostherd-tests--idle-screen))
            (should (equal (car (ghostherd--transcript-turn-end s)) :error))
            (should (eq (cdr (ghostherd--cli-error s)) action))
            (pcase-let ((`(,state . ,why) (ghostherd--detect-state s)))
              (should (eq state 'blocked))
              (should (string-match-p reason why))))))
      ;; No transcript: the real connection-lost screen still blocks.
      (let ((ghostherd-tests--fake-screen ghostherd-tests--connection-lost-screen))
        (should (equal (ghostherd--last-message ghostherd-tests--connection-lost-screen)
                       "API Error: Connection lost mid-response. The response above may be incomplete."))
        (should (equal (ghostherd--detect-state s)
                       '(blocked . "its turn was cut off: the connection dropped")))))))

(ert-deftest ghostherd-test-a-prompt-after-the-error-ends-it ()
  "A prompt typed after the error starts a new turn: no longer stuck,
by the transcript or by the screen.  What claude writes after an error
that is not the conversation -- the turn's duration, a /login run, in
the old shape or the new -- leaves it stuck."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "c" :kind 'claude :backend 'fake)))
      (should-not (ghostherd--cli-error s ghostherd-tests--prompted-after-error-screen))
      (ghostherd-tests--with-transcript s (ghostherd-tests--entries 'dropped 'dropped-turn 'prompt-after)
        (should (eq (ghostherd--transcript-turn-end s) :clean))
        (should-not (ghostherd--cli-error s ghostherd-tests--connection-lost-screen)))
      (ghostherd-tests--with-transcript s
          (append (ghostherd-tests--entries 'login 'login-turn 'login-cmd 'login-out)
                  (list "{\"type\":\"user\",\"isMeta\":true,\"message\":{\"role\":\"user\",\"content\":\"<local-command-caveat>Caveat</local-command-caveat>\"}}"
                        "{\"type\":\"user\",\"message\":{\"role\":\"user\",\"content\":\"<command-name>/login</command-name>\"}}"
                        "{\"type\":\"user\",\"message\":{\"role\":\"user\",\"content\":\"<local-command-stdout>Login interrupted</local-command-stdout>\"}}"
                        "{\"type\":\"last-prompt\",\"lastPrompt\":\"go on\"}"))
        ;; Still stuck on the error.  A /login went through (login-out),
        ;; so what it needs is to go on, not /login (ask 3346c963).
        (should (eq (car (ghostherd--transcript-turn-end s)) :error))
        (should (eq (cdr (ghostherd--cli-error s ghostherd-tests--idle-screen)) 'carry-on)))
      ;; A /login that did not go through -- run, or interrupted -- is /login again.
      (ghostherd-tests--with-transcript s
          (append (ghostherd-tests--entries 'login 'login-turn 'login-cmd)
                  (list "{\"type\":\"user\",\"message\":{\"role\":\"user\",\"content\":\"<local-command-stdout>Login interrupted</local-command-stdout>\"}}"))
        (should (eq (cdr (ghostherd--cli-error s ghostherd-tests--idle-screen)) 'login))))))

;;; /login's last step: signed in, waiting on Return (ask 25b5f652)

(defconst ghostherd-tests--signed-in-screen
  (concat
   "⏺ A made-up reply, before the login ran out.\n\n"
   "✻ Simmered for 1s · done 8:00 AM\n"
   "▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔\n"
   "   Login\n\n"
   "   Logged in as someone@example.com\n"
   "   Login successful. Press Enter to continue…\n\n\n")
  "A made-up claude screen on /login's last step, shaped by claude's own
text and `ghostherd-login-done-re', not a capture: the step drawn in the
input box's place.")

(defconst ghostherd-tests--code-entry-screen
  (concat "Visit this URL to sign in:\nhttps://example.com/oauth/authorize?state=made-up\n\n"
          "Paste code here if prompted:\n> \n")
  "A made-up /login code-entry screen, shaped by `ghostherd-login-code-re'.")

(ert-deftest ghostherd-test-signed-in-step-reads-blocked ()
  "/login's last step -- signed in, \"Press Enter to continue\" -- reads
as blocked on the user, with or without a transcript still ending on the
expired login, and wrapped in a narrow pane.  The same words over an
input box -- quoted in a reply -- or on another kind's screen are not
the step."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "c" :kind 'claude :backend 'fake))
          (g (ghostherd-tests--session :name "g" :kind 'grok :backend 'fake))
          (step '(blocked . "signed in: press Return to continue")))
      (let ((ghostherd-tests--fake-screen ghostherd-tests--signed-in-screen))
        (should (ghostherd--login-done s ghostherd-tests--signed-in-screen))
        (should (equal (ghostherd--detect-state s) step))
        ;; Its transcript still ends on the expired login -- a /login run
        ;; is not the conversation -- and the step is what shows.
        (ghostherd-tests--with-transcript s (ghostherd-tests--entries 'login 'login-turn 'login-cmd)
          (should (eq (cdr (ghostherd--cli-error s)) 'login))
          (should (equal (ghostherd--detect-state s) step)))
        (should-not (ghostherd--login-done g ghostherd-tests--signed-in-screen)))
      ;; Wrapped in a narrow pane.
      (should (ghostherd--login-done
               s (concat "   Login\n\n   Logged in as\n   someone@example.com\n"
                         "   Login successful. Press Enter\n   to continue…\n")))
      ;; Quoted in a reply, right over an input box: not the step.
      (let ((quoted (concat "⏺ It then says: Login successful. Press Enter to continue…\n"
                            "────────────────────\n❯ \n────────────────────\n")))
        (should (string-match-p ghostherd-login-done-re
                                (string-join (ghostherd--screen-foot quoted ghostherd--login-done-foot) "\n")))
        (should-not (ghostherd--login-done s quoted))
        (let ((ghostherd-tests--fake-screen quoted))
          (should-not (equal (ghostherd--detect-state s) step))))
      ;; Up in the screen, under other lines since: not at the foot.
      (should-not (ghostherd--login-done
                   s (concat ghostherd-tests--signed-in-screen
                             "   one\n   two\n   three\n   four\n"))))))

(ert-deftest ghostherd-test-signed-in-return-only-while-on-the-step ()
  "The Press Return for /login's signed-in step reads the live screen at
the click, and presses Return only while the step is still on it --
logged with that screen.  Moved on since the last poll -- its input box
back, a menu in its place -- or its host gone, it is refused and nothing
is pressed."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (let ((ghostherd-tests--fake-screen ghostherd-tests--signed-in-screen)
            (keys nil)
            (s (ghostherd-tests--session :name "c" :kind 'claude :backend 'fake :state 'blocked)))
        (puthash "c" ghostherd-tests--signed-in-screen ghostherd--screens)
        (cl-letf (((symbol-function 'ghostherd--host-send-keys) (lambda (_s k) (push k keys))))
          (should (equal (ghostherd-login-continue s) "c"))
          (should (equal keys '(("return"))))
          (should (string-match-p "finish its sign-in" (ghostherd-log-entry-text (car ghostherd--log))))
          (should (equal (ghostherd-log-entry-screen (car ghostherd--log))
                         ghostherd-tests--signed-in-screen))
          (setq keys nil)
          ;; The last poll still saw the step; the live screen has moved on.
          (dolist (now (list ghostherd-tests--idle-screen ghostherd-tests--claude-login-menu
                             ghostherd-tests--code-entry-screen))
            (let ((ghostherd-tests--fake-screen now))
              (should-error (ghostherd-login-continue s) :type 'user-error)))
          (let ((ghostherd-tests--fake-live nil))
            (should-error (ghostherd-login-continue s) :type 'user-error))
          (should-not keys))))))

(ert-deftest ghostherd-test-signed-in-card-offers-return-not-login ()
  "On the signed-in step the card -- and the snapshot, the phone's path --
carry `loginContinue', and no /login, which would take the step's
Return; the Inbox has the step, not the expired login.  On the
code-entry screen, the link and no /login, which would be typed as the
code.  Back at its input box, the expired login offers /login again."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (let* ((s (ghostherd-tests--session :name "c" :kind 'claude :backend 'fake))
             (card (lambda (screen)
                     (let ((ghostherd-tests--fake-screen screen))
                       (ghostherd-poll-session s))
                     (let ((a (ghostherd-memory-page--agent s))
                           (snap (car (ghostherd--herd-snapshot))))
                       (dolist (k '(:login :loginContinue :url))
                         (should (equal (plist-get a k) (plist-get snap k))))
                       a))))
        (ghostherd-tests--with-transcript s (ghostherd-tests--entries 'login 'login-turn 'login-cmd)
          (let ((a (funcall card ghostherd-tests--signed-in-screen)))
            (should (equal (plist-get a :state) "blocked"))
            (should (equal (plist-get a :reason) "signed in: press Return to continue"))
            (should (eq (plist-get a :loginContinue) t))
            (should (eq (plist-get a :login) :json-false))
            (let ((items (ghostherd-inbox--agent-items a)))
              (should (equal (ghostherd-tests--inbox-kinds items) '("login-continue")))
              (should (equal (plist-get (car items) :id) "login-continue:c"))
              (should (equal (plist-get (car items) :group) "need"))))
          (let ((a (funcall card ghostherd-tests--code-entry-screen)))
            (should (equal (plist-get a :url) "https://example.com/oauth/authorize?state=made-up"))
            (should (eq (plist-get a :login) :json-false))
            (should (eq (plist-get a :loginContinue) :json-false)))
          (let ((a (funcall card ghostherd-tests--idle-screen)))
            (should (eq (plist-get a :login) t))
            (should (eq (plist-get a :loginContinue) :json-false))
            (should (equal (ghostherd-tests--inbox-kinds (ghostherd-inbox--agent-items a)) '("login")))))))))

(ert-deftest ghostherd-test-page-presses-the-signed-in-return ()
  "The page's `agent-login-continue' -- the card's and the Inbox's Press
Return -- presses it on the signed-in step and says so; moved on, the
page shows the refusal and nothing is pressed."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (let ((ghostherd-tests--fake-screen ghostherd-tests--signed-in-screen)
            (keys nil) (pushes nil))
        (ghostherd-tests--session :name "c" :kind 'claude :backend 'fake :state 'blocked)
        (cl-letf (((symbol-function 'ghostherd--host-send-keys) (lambda (_s k) (push k keys)))
                  ((symbol-function 'ghostherd-memory-page--js) (lambda (fn obj) (push (cons fn obj) pushes)))
                  ((symbol-function 'ghostherd-memory-page--send-herd) #'ignore)
                  ((symbol-function 'ghostherd-memory-page--serve-files) #'ignore))
          (ghostherd-memory-page--handle '((op . "agent-login-continue") (name . "c")))
          (should (equal keys '(("return"))))
          (should (equal (cdr (assoc "flash" pushes)) "Pressed Return: c's sign-in is done"))
          (setq keys nil pushes nil)
          (let ((ghostherd-tests--fake-screen ghostherd-tests--idle-screen))
            (ghostherd-memory-page--handle '((op . "agent-login-continue") (name . "c"))))
          (should-not keys)
          (should (string-match-p "no longer on its sign-in" (cdr (assoc "showError" pushes)))))))))

;;; Signed in again: its turn waits to go on (ask 3346c963)

(defconst ghostherd-tests--signed-in-again
  '(login login-turn login-cmd login-out)
  "A claude transcript's tail after its turn died on an expired login and
a /login went through: the real entries, the error and its /login.")

(ert-deftest ghostherd-test-signed-in-again-card-and-inbox ()
  "Signed in again after the expired login, the turn is still stuck: the
agent reads blocked -- an ask it holds held -- but says it is signed in
and its turn stopped, the card and the snapshot offer Continue and no
/login, and the Inbox has that, not \"Login expired\".  Never carried on
by the timer: that is the cut-short errors'.  The newer shape of the
/login's output reads the same."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (let* ((s (ghostherd-tests--session :name "c" :kind 'claude :backend 'fake))
             (ghostherd-tests--fake-screen ghostherd-tests--idle-screen)
             (ghostherd--asks-open (make-hash-table :test 'equal))
             (ghostherd--asks-settled (make-hash-table :test 'equal))
             (settled nil)
             (newer "{\"type\":\"user\",\"message\":{\"role\":\"user\",\"content\":\"<local-command-stdout>Login successful</local-command-stdout>\"}}"))
        (puthash "ask1" (ghostherd-session-id s) ghostherd--asks-open)
        (cl-letf (((symbol-function 'ghostherd--ask-settle) (lambda (&rest _) (setq settled t))))
          (dolist (lines (list (apply #'ghostherd-tests--entries ghostherd-tests--signed-in-again)
                               (append (ghostherd-tests--entries 'login 'login-turn 'login-cmd) (list newer))))
            (ghostherd-tests--with-transcript s lines
              (ghostherd--set-state s 'working "on it")
              (ghostherd-poll-session s)
              (should (eq (ghostherd-session-state s) 'blocked))
              (should (equal (ghostherd-session-state-reason s)
                             "signed in again: its turn stopped at the expired login"))
              (should-not settled)
              (should-not (ghostherd--continue-cause s))
              (should-not (ghostherd--sleep-continue-at s))
              (let ((a (ghostherd-memory-page--agent s))
                    (snap (car (ghostherd--herd-snapshot))))
                (dolist (k '(:login :loginContinue :carryOn))
                  (should (equal (plist-get a k) (plist-get snap k))))
                (should (eq (plist-get a :carryOn) t))
                (should (eq (plist-get a :login) :json-false))
                (let ((items (ghostherd-inbox--agent-items a)))
                  (should (equal (ghostherd-tests--inbox-kinds items) '("carry-on")))
                  (should (equal (plist-get (car items) :id) "carry-on:c"))
                  (should (equal (plist-get (car items) :group) "need"))))))
          ;; Still on the sign-in's last step: its Return first, no Continue.
          (ghostherd-tests--with-transcript s (apply #'ghostherd-tests--entries ghostherd-tests--signed-in-again)
            (let ((ghostherd-tests--fake-screen ghostherd-tests--signed-in-screen))
              (ghostherd-poll-session s))
            (let ((a (ghostherd-memory-page--agent s)))
              (should (eq (plist-get a :loginContinue) t))
              (should (eq (plist-get a :carryOn) :json-false))
              (should (equal (ghostherd-tests--inbox-kinds (ghostherd-inbox--agent-items a)) '("login-continue"))))))))))

(ert-deftest ghostherd-test-carry-on-sends-the-continue-prompt ()
  "Continue, signed in again, sends the continue prompt for an expired
login -- the herd's way of carrying a stopped turn on, as after a sleep
-- with Return, and logs it.  Refused, nothing sent, when the turn is not
that: no /login gone through, or its screen with no input box for the
prompt (a sign-in step, a menu), or its host gone.  From the page too."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (let ((s (ghostherd-tests--session :name "c" :kind 'claude :backend 'fake :state 'blocked))
            (ghostherd-tests--fake-screen ghostherd-tests--idle-screen)
            (sends nil) (pushes nil))
        (cl-letf (((symbol-function 'ghostherd-send)
                   (lambda (_s text &optional submit) (push (list text submit) sends)))
                  ((symbol-function 'ghostherd-memory-page--js) (lambda (fn obj) (push (cons fn obj) pushes)))
                  ((symbol-function 'ghostherd-memory-page--send-herd) #'ignore)
                  ((symbol-function 'ghostherd-memory-page--serve-files) #'ignore))
          (ghostherd-tests--with-transcript s (apply #'ghostherd-tests--entries ghostherd-tests--signed-in-again)
            (should (equal (ghostherd-carry-on s) "c"))
            (should (equal sends (list (list ghostherd-expired-continue-prompt t))))
            (should (string-match-p "\\`↻ continuing a turn an expired login cut short, as asked"
                                    (ghostherd-log-entry-text (car ghostherd--log))))
            (should (eq (ghostherd-log-entry-kind (car ghostherd--log)) 'continue))
            (should (ghostherd-tests--issue-p "continuing a turn"))
            ;; Through the page: the same.
            (setq sends nil)
            (ghostherd-memory-page--handle '((op . "agent-carry-on") (name . "c")))
            (should (equal (caar sends) ghostherd-expired-continue-prompt))
            (should (equal (cdr (assoc "flash" pushes)) "Continuing c's turn"))
            (setq sends nil)
            ;; No input box on its screen: nowhere for the prompt.
            (dolist (screen (list ghostherd-tests--signed-in-screen ghostherd-tests--claude-login-menu))
              (let ((ghostherd-tests--fake-screen screen))
                (should-error (ghostherd-carry-on s) :type 'user-error)))
            (let ((ghostherd-tests--fake-live nil))
              (should-error (ghostherd-carry-on s) :type 'user-error)))
          ;; Not signed in again: /login is what it needs.
          (ghostherd-tests--with-transcript s (ghostherd-tests--entries 'login 'login-turn 'login-cmd)
            (should-error (ghostherd-carry-on s) :type 'user-error))
          (should-not sends))))))

(ert-deftest ghostherd-test-login-back-when-it-expires-again ()
  "Signed in again and carried on, a login that expires again ends the
new turn on a new error: /login is back on the card and in the Inbox,
Continue gone -- the /login that went through was for the error before."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (let ((s (ghostherd-tests--session :name "c" :kind 'claude :backend 'fake))
            (ghostherd-tests--fake-screen ghostherd-tests--idle-screen)
            (go-on (format "{\"type\":\"user\",\"message\":{\"role\":\"user\",\"content\":%s}}"
                           (json-encode ghostherd-expired-continue-prompt))))
        (ghostherd-tests--with-transcript s
            (append (apply #'ghostherd-tests--entries ghostherd-tests--signed-in-again)
                    (list go-on)
                    (ghostherd-tests--entries 'login 'login-turn))
          (ghostherd-poll-session s)
          (should (equal (ghostherd--detect-state s) '(blocked . "its login expired -- run /login")))
          (let ((a (ghostherd-memory-page--agent s)))
            (should (eq (plist-get a :login) t))
            (should (eq (plist-get a :carryOn) :json-false))
            (should (equal (ghostherd-tests--inbox-kinds (ghostherd-inbox--agent-items a)) '("login")))))
        ;; Carried on and still going: no error at its end, nothing stuck.
        (ghostherd-tests--with-transcript s
            (append (apply #'ghostherd-tests--entries ghostherd-tests--signed-in-again) (list go-on))
          (should (eq (ghostherd--transcript-turn-end s) :clean))
          (should-not (ghostherd--cli-error s)))))))

(ert-deftest ghostherd-test-the-turn-end-is-read-past-a-large-entry ()
  "The transcript is read back from its end a chunk at a time, growing:
an entry larger than a chunk after the error does not hide it."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "c" :kind 'claude :backend 'fake)))
      (ghostherd-tests--with-transcript s
          (append (ghostherd-tests--entries 'lead-reply 'slept)
                  (list (format "{\"type\":\"file-history-snapshot\",\"blob\":\"%s\"}"
                                (make-string 300000 ?x))))
        (should (equal (ghostherd--transcript-turn-end s)
                       '(:error "API Error: Your computer went to sleep mid-response. The response above may be incomplete.")))))))

(ert-deftest ghostherd-test-the-turn-end-is-read-once-per-change ()
  "The poll asks every second; an unchanged transcript is read once, and
a changed one again."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "c" :kind 'claude :backend 'fake))
          (reads 0)
          (read-turn-end (symbol-function 'ghostherd--claude-read-turn-end)))
      (cl-letf (((symbol-function 'ghostherd--claude-read-turn-end)
                 (lambda (&rest args) (cl-incf reads) (apply read-turn-end args))))
        (ghostherd-tests--with-transcript s (ghostherd-tests--entries 'dropped 'dropped-turn)
          (dotimes (_ 3)
            (should (eq (car (ghostherd--transcript-turn-end s)) :error)))
          (should (= reads 1))
          (write-region (concat (alist-get 'prompt-after ghostherd-tests--claude-entries) "\n")
                        nil path t 'silent)
          (should (eq (ghostherd--transcript-turn-end s) :clean))
          (should (= reads 2)))))))

(ert-deftest ghostherd-test-turn-ends-prune-drops-dead-transcripts ()
  "The turn-end cache keeps only transcripts live agents still follow: a
removed agent's, or an old conversation a live one has left, is dropped;
the one a live agent is on now is kept."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--session :name "claude-a" :kind 'claude)
    (ghostherd-tests--session :name "claude-b" :kind 'claude)
    (cl-letf (((symbol-function 'ghostherd-session-transcript)
               (lambda (s) (pcase (ghostherd-session-name s)
                             ("claude-a" "/t/a.jsonl")
                             ("claude-b" "/t/b-new.jsonl")))))
      (dolist (p '("/t/a.jsonl" "/t/b-old.jsonl" "/t/b-new.jsonl" "/t/gone.jsonl"))
        (puthash p '(1 nil . :clean) ghostherd--turn-ends))
      (ghostherd--prune-turn-ends)
      (should (gethash "/t/a.jsonl" ghostherd--turn-ends))       ; a's current, kept
      (should (gethash "/t/b-new.jsonl" ghostherd--turn-ends))   ; b's current, kept
      (should-not (gethash "/t/b-old.jsonl" ghostherd--turn-ends)) ; b left it, dropped
      (should-not (gethash "/t/gone.jsonl" ghostherd--turn-ends))  ; no agent, dropped
      (should (= (hash-table-count ghostherd--turn-ends) 2)))))

(ert-deftest ghostherd-test-turn-ends-pruned-on-removal-and-capped ()
  "Removing an agent prunes the cache (`ghostherd-session-removed-hook'); and
past the ceiling the cache is cleared, never grown without bound."
  (ghostherd-tests--with-herd ()
    (let ((a (ghostherd-tests--session :name "claude-a" :kind 'claude)))
      (cl-letf (((symbol-function 'ghostherd-session-transcript)
                 (lambda (s) (and (ghostherd-get (ghostherd-session-id s)) "/t/a.jsonl")))
                ((symbol-function 'ghostherd--host-kill) #'ignore))
        (puthash "/t/a.jsonl" '(1 nil . :clean) ghostherd--turn-ends)
        (puthash "/t/stale.jsonl" '(1 nil . :clean) ghostherd--turn-ends)
        ;; Killing a removes it: the hook prunes, and a is gone from the
        ;; registry, so both its transcript and the stale one go.
        (ghostherd-kill a t)
        (should (zerop (hash-table-count ghostherd--turn-ends))))
      ;; The cap: more followed transcripts than the ceiling clears the lot.
      (cl-letf (((symbol-function 'ghostherd-session-transcript) (lambda (_s) "/t/x.jsonl")))
        (ghostherd-tests--session :name "claude-c" :kind 'claude)
        (let ((ghostherd--turn-ends-max 0))   ; any followed entry is "over"
          (puthash "/t/x.jsonl" '(1 nil . :clean) ghostherd--turn-ends)
          (ghostherd--prune-turn-ends)
          (should (zerop (hash-table-count ghostherd--turn-ends))))))))

;;; Carry a turn on after the Mac slept (ask e8dbef6d)

(defconst ghostherd-tests--slept-screen
  (concat "⏺ earlier work\n\n"
          "⏺ API Error: Your computer went to sleep mid-response\n"
          "────────────────────\n❯ \n────────────────────\n"
          "  ⏵⏵ auto mode on\n")
  "A claude turn killed by the Mac going to sleep mid-response.")

(ert-deftest ghostherd-test-a-slept-turn-reads-as-continue ()
  "The computer-slept error carries the `continue' action; login-expired
and a bare API error do not -- only a slept turn is safe to auto-continue."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "c" :kind 'claude :backend 'fake))
          (box "\n────────\n❯ \n────────\n"))
      (let ((ghostherd-tests--fake-screen ghostherd-tests--slept-screen))
        (should (eq (cdr (ghostherd--cli-error s ghostherd-tests--fake-screen)) 'continue)))
      (let ((ghostherd-tests--fake-screen (concat "⏺ Login expired · Please run /login" box)))
        (should (eq (cdr (ghostherd--cli-error s ghostherd-tests--fake-screen)) 'login)))
      (let ((ghostherd-tests--fake-screen (concat "⏺ API Error: service unavailable" box)))
        (should (null (cdr (ghostherd--cli-error s ghostherd-tests--fake-screen))))))))

(ert-deftest ghostherd-test-a-slept-turn-is-continued-after-a-delay ()
  "A turn the Mac's sleep cut short is read as blocked, and once the settle
delay is up the herd sends the continue prompt itself -- not before -- and
logs it.  Until then the card has a countdown, not a `stopped'."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (let ((s (ghostherd-tests--session :name "c" :kind 'claude :backend 'fake))
            (ghostherd-tests--fake-screen ghostherd-tests--slept-screen)
            (ghostherd-sleep-continue-delay 60)
            (ghostherd-sleep-continue-max 1)
            (sends nil))
        (cl-letf (((symbol-function 'ghostherd-send)
                   (lambda (_s text &optional submit) (push (list text submit) sends))))
          ;; First poll: blocked, an episode opened, nothing sent yet (the
          ;; delay has not passed), and the card counts down to a continue.
          (ghostherd-poll-session s)
          (should (eq (ghostherd-session-state s) 'blocked))
          (should-not sends)
          (should (ghostherd--sleep-continue-at s))
          (should-not (ghostherd--sleep-continue-stopped-p s))
          ;; Wind the episode's clock back past the delay, then poll again.
          (plist-put (gethash (ghostherd-session-id s) ghostherd--sleep-continue)
                     :since (- (float-time) 120))
          (ghostherd-poll-session s)
          (should (= (length sends) 1))
          (should (equal (caar sends) ghostherd-sleep-continue-prompt))
          (should (eq (cadr (car sends)) t))   ; submitted with Return
          (should (cl-find 'continue ghostherd--log
                           :key #'ghostherd-log-entry-kind)))))))

(ert-deftest ghostherd-test-a-slept-turn-is-continued-at-most-once ()
  "Once the budget is spent, a turn that dies the same way again stays
blocked -- the herd does not keep prodding it -- and the card says it has
stopped retrying rather than counting down."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (let ((s (ghostherd-tests--session :name "c" :kind 'claude :backend 'fake))
            (ghostherd-tests--fake-screen ghostherd-tests--slept-screen)
            (ghostherd-sleep-continue-delay 0)
            (ghostherd-sleep-continue-max 1)
            (sends nil))
        (cl-letf (((symbol-function 'ghostherd-send)
                   (lambda (&rest _) (push t sends))))
          (ghostherd-poll-session s)        ; delay 0: continues at once
          (should (= (length sends) 1))
          (ghostherd-poll-session s)        ; still the slept error: no retry
          (ghostherd-poll-session s)
          (should (= (length sends) 1))
          (should (eq (ghostherd-session-state s) 'blocked))
          (should (ghostherd--sleep-continue-stopped-p s))
          (should-not (ghostherd--sleep-continue-at s)))))))

(ert-deftest ghostherd-test-a-slept-continue-respects-its-guards ()
  "At the end of the delay the herd does not continue if the user has typed
into the agent, or the error has left the screen."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (let ((s (ghostherd-tests--session :name "c" :kind 'claude :backend 'fake))
            (ghostherd-sleep-continue-delay 30)
            (ghostherd-idle-settle 0)
            (sends nil))
        (cl-letf (((symbol-function 'ghostherd-send)
                   (lambda (&rest _) (push t sends))))
          (cl-flet ((seed ()
                      (puthash (ghostherd-session-id s)
                               (list :since (- (float-time) 60) :attempts 0 :cancelled nil)
                               ghostherd--sleep-continue)))
            ;; (a) the user typed after the wait began -> left alone.
            (seed)
            (puthash (ghostherd-session-id s) (float-time) ghostherd--input-at)
            (let ((ghostherd-tests--fake-screen ghostherd-tests--slept-screen))
              (ghostherd-poll-session s))
            (should-not sends)
            ;; (b) the error has left the screen -> left alone, episode cleared.
            (remhash (ghostherd-session-id s) ghostherd--input-at)
            (seed)
            (let ((ghostherd-tests--fake-screen
                   "⏺ back to work\n────────\n❯ \n────────\n  ⏵⏵ auto mode on\n"))
              (ghostherd-poll-session s))
            (should-not sends)))))))

(ert-deftest ghostherd-test-a-new-slept-turn-is-a-fresh-continue ()
  "The budget is per stuck turn: once the agent reaches a settled state on
its own, a later sleep is continued again."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (let ((s (ghostherd-tests--session :name "c" :kind 'claude :backend 'fake))
            (ghostherd-sleep-continue-delay 0)
            (ghostherd-sleep-continue-max 1)
            (ghostherd-idle-settle 0)
            (sends nil))
        (cl-letf (((symbol-function 'ghostherd-send)
                   (lambda (&rest _) (push t sends))))
          (let ((ghostherd-tests--fake-screen ghostherd-tests--slept-screen))
            (ghostherd-poll-session s))            ; continues (1)
          (should (= (length sends) 1))
          ;; It finishes on its own: a settled screen clears the episode.
          (let ((ghostherd-tests--fake-screen
                 "⏺ all done\n────────\n❯ \n────────\n  ⏵⏵ auto mode on\n"))
            (ghostherd-poll-session s))
          (should-not (gethash (ghostherd-session-id s) ghostherd--sleep-continue))
          ;; A fresh sleep later is a fresh continue.
          (let ((ghostherd-tests--fake-screen ghostherd-tests--slept-screen))
            (ghostherd-poll-session s))            ; continues (2)
          (should (= (length sends) 2)))))))

(ert-deftest ghostherd-test-cancelling-a-slept-continue-leaves-it-blocked ()
  "The card's Cancel waves the auto-continue off: the agent stays blocked
and the prompt is never sent."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (let ((s (ghostherd-tests--session :name "c" :kind 'claude :backend 'fake))
            (ghostherd-tests--fake-screen ghostherd-tests--slept-screen)
            (ghostherd-sleep-continue-delay 0)
            (sends nil))
        (cl-letf (((symbol-function 'ghostherd-send)
                   (lambda (&rest _) (push t sends))))
          (puthash (ghostherd-session-id s)
                   (list :since (- (float-time) 60) :attempts 0 :cancelled nil)
                   ghostherd--sleep-continue)
          (ghostherd-sleep-continue-cancel s)
          (ghostherd-poll-session s)
          (should-not sends)
          (should (eq (ghostherd-session-state s) 'blocked))
          (should (ghostherd--sleep-continue-stopped-p s))
          (should-not (ghostherd--sleep-continue-at s)))))))

(ert-deftest ghostherd-test-the-snapshot-carries-the-continue-countdown ()
  "The card's countdown and Cancel are driven by `continueAt' /
`continueStopped' in the snapshot pushed to the page."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (let ((s (ghostherd-tests--session :name "c" :kind 'claude :backend 'fake))
            (ghostherd-tests--fake-screen ghostherd-tests--slept-screen)
            (ghostherd-sleep-continue-delay 60))
        (ghostherd-poll-session s)
        (let ((snap (car (ghostherd--herd-snapshot))))
          (should (> (plist-get snap :continueAt) 0))
          (should (eq (plist-get snap :continueStopped) :json-false)))))))

;;; A dropped connection is carried on too, in its own words (ask eb81eb50)

(ert-deftest ghostherd-test-only-the-cut-short-errors-continue ()
  "Of claude's own errors, the Mac sleeping and the connection dropping
are continued, each with its cause; an expired login and every other
API error -- a stalled response, no network, a 500, an unknown model --
still block and wait.  Real transcript entries."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "c" :kind 'claude :backend 'fake)))
      (pcase-dolist (`(,tag ,action ,cause)
                     '((slept continue slept)
                       (dropped continue dropped)
                       (login login nil)
                       (stalled nil nil)
                       (offline nil nil)
                       (server nil nil)
                       (model nil nil)))
        (ghostherd-tests--with-transcript s (ghostherd-tests--entries tag 'login-turn)
          (should (eq (cdr (ghostherd--cli-error s ghostherd-tests--idle-screen)) action))
          (should (eq (ghostherd--continue-cause s ghostherd-tests--idle-screen) cause))))
      ;; The screen, for an agent with no transcript known, says the same.
      (should (eq (ghostherd--continue-cause s ghostherd-tests--connection-lost-screen) 'dropped))
      (should (eq (ghostherd--continue-cause s ghostherd-tests--slept-screen) 'slept))
      ;; A `continue' rule that names no cause -- the shape of the value a
      ;; live Emacs keeps until it is set again -- is the slept one.
      (let ((ghostherd-cli-errors
             '((claude ("went to sleep mid-response" continue "the computer slept")))))
        (should (eq (ghostherd--continue-cause s ghostherd-tests--slept-screen) 'slept))))))

(ert-deftest ghostherd-test-a-dropped-connection-is-continued-in-its-own-words ()
  "claude-jnotes's turn, cut by a dropped connection: blocked, then after
the same delay the herd sends a prompt that says the connection dropped
-- never that the computer slept -- logs it so, and the card counts down
and stops in those words.  The same guards hold, and the same budget: a
continued turn that drops again stays blocked."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (let ((s (ghostherd-tests--session :name "c" :kind 'claude :backend 'fake))
            (ghostherd-tests--fake-screen ghostherd-tests--connection-lost-screen)
            (ghostherd-sleep-continue-delay 60)
            (ghostherd-sleep-continue-max 1)
            (sends nil))
        (cl-letf (((symbol-function 'ghostherd-send)
                   (lambda (_s text &optional submit) (push (list text submit) sends))))
          (ghostherd-tests--with-transcript s (ghostherd-tests--entries 'lead-reply 'dropped 'dropped-turn)
            (ghostherd-poll-session s)
            (should (eq (ghostherd-session-state s) 'blocked))
            (should-not sends)
            (let ((snap (car (ghostherd--herd-snapshot))))
              (should (> (plist-get snap :continueAt) 0))
              (should (equal (plist-get snap :continueAfter) "after a dropped connection")))
            ;; Typed into since the wait began: left alone.
            (plist-put (gethash (ghostherd-session-id s) ghostherd--sleep-continue)
                       :since (- (float-time) 120))
            (puthash (ghostherd-session-id s) (float-time) ghostherd--input-at)
            (ghostherd-poll-session s)
            (should-not sends)
            (remhash (ghostherd-session-id s) ghostherd--input-at)
            ;; Past the delay, untouched: continued once, in its own words.
            (ghostherd-poll-session s)
            (should (equal sends (list (list ghostherd-dropped-continue-prompt t))))
            (should-not (string-match-p "sleep\\|slept" ghostherd-dropped-continue-prompt))
            (should (string-match-p "connection" ghostherd-dropped-continue-prompt))
            (let ((entry (cl-find 'continue ghostherd--log :key #'ghostherd-log-entry-kind)))
              (should (string-match-p "a dropped connection cut short"
                                      (ghostherd-log-entry-text entry))))
            ;; The continued turn drops again: blocked, not prodded twice.
            (write-region (concat (alist-get 'prompt-after ghostherd-tests--claude-entries) "\n"
                                  (alist-get 'dropped ghostherd-tests--claude-entries) "\n")
                          nil path t 'silent)
            (ghostherd-poll-session s)
            (ghostherd-poll-session s)
            (should (= (length sends) 1))
            (should (eq (ghostherd-session-state s) 'blocked))
            (should (ghostherd--sleep-continue-stopped-p s))
            (let ((snap (car (ghostherd--herd-snapshot))))
              (should (eq (plist-get snap :continueStopped) t))
              (should (equal (plist-get snap :continueAgain) "the connection dropped again")))))))))

;;; The view has to be the size of the window

(ert-deftest ghostherd-test-visiting-resizes-the-terminal ()
  "ghostel sizes a terminal when it is created and thereafter only from
`window-size-change-functions'.  Displaying a buffer in an existing
window is not a resize, so every tmux attach -- which must create the
client before there is anything to show -- kept whatever size it was
born with, and the agent drew into a fraction of the window."
  (ghostherd-tests--with-herd ()
    (let* ((s (ghostherd-tests--session :name "a" :backend 'fake))
           (forced nil))
      (cl-letf (((symbol-function 'pop-to-buffer) (lambda (&rest _) nil))
                ((symbol-function 'get-buffer-window)
                 (lambda (&rest _) 'the-window))
                ((symbol-function 'ghostel--adjust-size)
                 (lambda (window force) (setq forced (list window force)))))
        (ghostherd-visit s))
      ;; forced, because the terminal's own idea of its size is the stale
      ;; thing -- an unforced adjust returns early when it sees no change
      (should (equal forced '(the-window t))))))

(ert-deftest ghostherd-test-undisplayed-view-is-not-resized ()
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :backend 'fake))
          (called nil))
      (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil))
                ((symbol-function 'ghostel--adjust-size)
                 (lambda (&rest _) (setq called t))))
        (ghostherd--sync-view-size (ghostherd-session-buffer s)))
      (should-not called))))

(ert-deftest ghostherd-test-brief-idle-does-not-finish-the-work ()
  "The flap that produced four \"ready for review\" notifications in three
seconds: agy draws its prompt whether or not it is working, so what
makes a screen read as busy is the spinner -- and that blinks out
between two tool calls.  A poll landing in the gap saw nothing working
and called it done."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy :backend 'fake
                                       :state 'working))
          (ghostherd-tests--fake-screen "> \n")
          (ghostherd-idle-settle 60))
      (setf (ghostherd-session-seen s) nil)
      (cl-letf (((symbol-function 'ghostherd--notify) (lambda (&rest _) nil)))
        ;; the gap between two tool calls is not the end of the work
        (should (eq (ghostherd-poll-session s) 'working))
        (should (eq (ghostherd-poll-session s) 'working))
        ;; ...but a screen that stays quiet is
        (let ((ghostherd-idle-settle 0))
          (should (eq (ghostherd-poll-session s) 'done)))))))

(ert-deftest ghostherd-test-leaving-idle-is-immediate ()
  "Only entering idle waits.  Something appearing on the screen is a
positive signal and must not be held back by a settle window."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy :backend 'fake
                                       :state 'idle))
          (ghostherd-tests--fake-screen "⣾ Working...\n")
          (ghostherd-idle-settle 60))
      (cl-letf (((symbol-function 'ghostherd--notify) (lambda (&rest _) nil)))
        (should (eq (ghostherd-poll-session s) 'working))))))

(ert-deftest ghostherd-test-an-unfocused-emacs-is-not-watching ()
  "Being displayed is not being watched.  `get-buffer-window\=' with
ALL-FRAMES t counts frames that are iconified or on another desktop --
which is exactly when you want the banner.  Switching to a browser must
put the notification back."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy :backend 'fake
                                       :state 'working))
          (ghostherd-tests--fake-screen "> \n")
          (ghostherd-idle-settle 0)
          (notified nil))
      (setf (ghostherd-session-seen s) nil)
      (cl-letf (((symbol-function 'ghostherd--notify)
                 (lambda (&rest _) (setq notified t)))
                ((symbol-function 'get-buffer-window) (lambda (&rest _) t))
                ((symbol-function 'window-frame) (lambda (&rest _) 'a-frame))
                ((symbol-function 'frame-focus-state) (lambda (&rest _) nil)))
        (should (eq (ghostherd-poll-session s) 'done))
        (should notified)))))

(ert-deftest ghostherd-test-an-agent-on-screen-has-been-seen ()
  "`done' means finished while you were not looking.  Announcing it
about an agent in a window you are looking at was most of the noise."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy :backend 'fake
                                       :state 'working))
          (ghostherd-tests--fake-screen "> \n")
          (ghostherd-idle-settle 0)
          (notified nil))
      (setf (ghostherd-session-seen s) nil)
      (cl-letf (((symbol-function 'ghostherd--notify)
                 (lambda (&rest _) (setq notified t)))
                ((symbol-function 'get-buffer-window) (lambda (&rest _) t))
                ((symbol-function 'window-frame) (lambda (&rest _) 'a-frame))
                ((symbol-function 'frame-focus-state) (lambda (&rest _) t)))
        (should (eq (ghostherd-poll-session s) 'idle))
        (should-not notified)
        ;; and the log says so, since that is what the manual sends you
        ;; to read when no banner arrives
        (should (equal (ghostherd-session-state-reason s)
                       "done, but you are watching"))))))

(ert-deftest ghostherd-test-a-glance-while-it-works-is-not-watching-it-finish ()
  "Sending a prompt means looking at the agent, and the poll a second
later latched `seen\='.  Nothing cleared it before the work ended, so the
run finished silently however long you had been away -- the whole herd
log this was written from contains no `done\=' at all."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'claude :backend 'fake
                                       :state 'working))
          (ghostherd-idle-settle 0)
          (notified nil)
          (watching t))
      (setf (ghostherd-session-seen s) nil)
      (cl-letf (((symbol-function 'ghostherd--notify)
                 (lambda (&rest _) (setq notified t)))
                ((symbol-function 'get-buffer-window)
                 (lambda (&rest _) (and watching t)))
                ((symbol-function 'window-frame) (lambda (&rest _) 'a-frame))
                ((symbol-function 'frame-focus-state)
                 (lambda (&rest _) watching)))
        ;; one poll with the session on your focused frame
        (let ((ghostherd-tests--fake-screen "· esc to interrupt\n"))
          (should (eq (ghostherd-poll-session s) 'working)))
        (should (ghostherd-session-seen s))
        ;; you switch to the browser; it finishes there
        (setq watching nil)
        (let ((ghostherd-tests--fake-screen "> \n"))
          (should (eq (ghostherd-poll-session s) 'done)))
        (should notified)))))

(ert-deftest ghostherd-test-done-holds-until-the-session-is-viewed ()
  "`done\=' is the one state no screen can show, so a poll that re-reads
the screen has nothing to re-derive it from and it decayed back to
`idle\=' on the next tick -- 1.5 seconds of ✓ for work nobody had looked
at yet."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'claude :backend 'fake
                                       :state 'working))
          (ghostherd-tests--fake-screen "> \n")
          (ghostherd-idle-settle 0))
      (setf (ghostherd-session-seen s) nil)
      (cl-letf (((symbol-function 'ghostherd--notify) (lambda (&rest _) nil))
                ((symbol-function 'get-buffer-window) (lambda (&rest _) nil)))
        (should (eq (ghostherd-poll-session s) 'done))
        (should (eq (ghostherd-poll-session s) 'done))
        ;; ...and lets go once it has been visited
        (setf (ghostherd-session-seen s) t)
        (should (eq (ghostherd-poll-session s) 'idle))))))

(ert-deftest ghostherd-test-done-is-seen-where-else-it-shows ()
  "An agent with no view of its own -- detached -- is seen where another
place shows it: `ghostherd-watched-functions', asked of it alone."
  (ghostherd-tests--with-herd ()
    (let ((a (ghostherd-tests--session :name "a" :kind 'claude :backend 'fake :state 'working))
          (b (ghostherd-tests--session :name "b" :kind 'claude :backend 'fake :state 'working))
          (ghostherd-tests--fake-screen "> \n")
          (ghostherd-idle-settle 0)
          (ghostherd-watched-functions nil))
      (setf (ghostherd-session-seen a) nil (ghostherd-session-seen b) nil)
      (cl-letf (((symbol-function 'ghostherd--notify) (lambda (&rest _) nil))
                ((symbol-function 'get-buffer-window) (lambda (&rest _) nil)))
        (should (eq (ghostherd-poll-session a) 'done))
        (should (eq (ghostherd-poll-session b) 'done))
        (should (eq (ghostherd-poll-session a) 'done))
        (add-hook 'ghostherd-watched-functions (lambda (s) (equal (ghostherd-session-name s) "a")))
        (should (eq (ghostherd-poll-session a) 'idle))
        (should (equal (ghostherd-session-state-reason a) "seen"))
        (should (eq (ghostherd-poll-session b) 'done))))))

(ert-deftest ghostherd-test-the-room-shows-its-agents ()
  "The page in front of you on a project's room shows its agents: each
in the columns, those with a transcript in the chat; no other project's,
and none when the page is not where you are looking."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--session :name "rd" :kind 'claude :backend 'fake :project "/p/")
    (ghostherd-tests--session :name "qa" :kind 'claude :backend 'fake :project "/p/")
    (ghostherd-tests--session :name "x" :kind 'claude :backend 'fake :project "/q/")
    (let ((ghostherd-memory-page--room nil)
          (ghostherd-memory-page--room-chat nil)
          (looking t))
      (cl-letf (((symbol-function 'xwapp-seen-p) (lambda (_app) looking))
                ((symbol-function 'ghostherd-session-transcript)
                 (lambda (s) (and (equal (ghostherd-session-name (ghostherd-get s)) "rd") "/t/rd.jsonl"))))
        (let ((shown (lambda () (sort (mapcar #'ghostherd-session-name
                                              (seq-filter #'ghostherd-memory-page--room-shows-p (ghostherd-sessions)))
                                      #'string<))))
          (should-not (funcall shown))
          (setq ghostherd-memory-page--room (ghostherd--project-name "/p/"))
          (should (equal (funcall shown) '("qa" "rd")))
          (should (ghostherd--watched-p (ghostherd-get "qa")))
          (setq ghostherd-memory-page--room-chat t)
          (should (equal (funcall shown) '("rd")))
          (setq looking nil)
          (should-not (funcall shown))
          (should-not (ghostherd--watched-p (ghostherd-get "rd"))))))))

;;; Not idle, just slow

(ert-deftest ghostherd-test-fresh-prompt-is-not-idle ()
  "An agent does not start the instant you press Return.  The poll one
second later reads the screen it had before, calls it idle, promotes it
to `done' because work was in flight, and tells you an agent that has
not read your prompt is ready for review.  `ghostherd-handoff' guarded
its own watch against this from the start; the poll path did not."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy :backend 'fake
                                       :state 'idle))
          (ghostherd-tests--fake-screen "> \n")
          (ghostherd-input-grace 60))
      (cl-letf (((symbol-function 'ghostherd--notify) (lambda (&rest _) nil))
                ((symbol-function 'ghostherd--host-send-text)
                 (lambda (&rest _) nil)))
        (ghostherd-send s "do the thing" t))
      ;; the screen still shows the prompt it showed before
      (should (eq (ghostherd-poll-session s) 'working))
      ;; ...and once both windows are up, the same screen means what it says
      (let ((ghostherd-input-grace 0)
            (ghostherd-idle-settle 0))
        (should (eq (ghostherd-poll-session s) 'done))))))

(ert-deftest ghostherd-test-grace-does-not-outrank-a-prompt ()
  "Only `idle' is disbelieved.  An agent that woke up and immediately
asked something must still report `blocked' inside the window, or the
grace would hide the very thing it exists to wait for."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy :backend 'fake
                                       :state 'idle))
          (ghostherd-tests--fake-screen "Do you want to proceed?\n")
          (ghostherd-input-grace 60))
      (puthash "a" (float-time) ghostherd--input-at)
      (should (eq (car (ghostherd--detect-state s)) 'blocked))
      (cl-letf (((symbol-function 'ghostherd--notify) (lambda (&rest _) nil)))
        (should (eq (ghostherd-poll-session s) 'blocked))))))

(ert-deftest ghostherd-test-idle-rules-want-an-empty-prompt ()
  "The finding that started this: these agents scroll rather than
repaint, so every line you have ever typed stays on screen behind its
prompt glyph.  A rule matching those reports `idle' from the first time
you press Return, forever -- and the reason field said so out loud, by
quoting a command sent half a minute earlier."
  (let ((transcript "> execute date command\n\n● Bash(date)\n\n>\n? for shortcuts\n"))
    (dolist (kind '(claude grok agy))
      (let* ((rules (plist-get (ghostherd--spec kind) :screen-rules))
             (echo-only (car (split-string transcript "\n>\n"))))
        ;; the empty prompt at the bottom is idle
        (should (eq (car (ghostherd--match-rules transcript rules)) 'idle))
        ;; the echoed command on its own is not
        (should-not (ghostherd--match-rules echo-only rules))))))

(ert-deftest ghostherd-test-claudes-empty-prompt-is-idle-by-rule ()
  "claude follows its ❯ with a no-break space.  With an ordinary one in
the rule, its empty prompt matched nothing, and every idle claude in
the log said \"no rule matched\" -- the line that means the rules do
not know this screen."
  (let ((rules (plist-get (ghostherd--spec 'claude) :screen-rules))
        (rule (lambda (box) (concat "  Done.\n\n" (make-string 40 ?─) "\n" box "\n"
                                    (make-string 40 ?─) "\n  ⏵⏵ auto mode on\n"))))
    (should (eq (car (ghostherd--match-rules (funcall rule "❯\u00a0") rules)) 'idle))
    (should (eq (car (ghostherd--match-rules (funcall rule "❯ ") rules)) 'idle))
    (should-not (ghostherd--match-rules (funcall rule "❯\u00a0half typed") rules))))

(ert-deftest ghostherd-test-a-mode-banner-is-not-a-permission-prompt ()
  "grok draws its permission mode into the border of the input box, so
`always-approve\=' -- the mode in which it does *not* stop to ask -- was
on screen for as long as the mode was on.  `blocked\=' outranks
`working\=', so an agent three minutes into an edit, spinner running,
sat in the sidebar asking for attention it did not want."
  (let ((rules (plist-get (ghostherd--spec 'grok) :screen-rules))
        (screen (concat
                 "  \u2503  \u25c6 Thinking\u2026\n"
                 "  \u2503  Now update activation columns.\n"
                 "    \u2846 Thinking\u2026 1.9s              3m33s [stop]\n"
                 "  \u256d\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u256e\n"
                 "  \u2502 \u276f                                  \u2502\n"
                 "  \u2570\u2500\u2500 Grok 4.6 (xhigh) \u00b7 always-approve \u2500\u256f\n")))
    (should (eq (car (ghostherd--match-rules screen rules)) 'working))))

(defconst ghostherd-tests--agy-generating
  (concat
   "\u25cf Bash(git status)\n"
   "\u25cf Bash(git diff local/pr-view/readme.org)\n"
   "\u283f  Generating\u2026\n"
   "\u2514 Tip: Press ctrl+b to send a long task to the background.\n"
   "\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n"
   ">\n"
   "\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n"
   "esc to cancel                              Gemini 3.8 Flash \u00b7 high\n")
  "agy midway through a task.
Its input box is drawn the whole time, so the empty-prompt idle rule
matches here too -- only precedence keeps the two apart.")

(ert-deftest ghostherd-test-agy-generating-is-working ()
  "Reported as idle while the agent was visibly generating.
Three separate working signals were on screen and the rules knew none
of them: agy labels its spinner `Generating', says `esc to cancel'
rather than `esc to interrupt', and spins the 8-dot braille family."
  (let ((rules (plist-get (ghostherd--spec 'agy) :screen-rules)))
    (should (eq (car (ghostherd--match-rules
                      ghostherd-tests--agy-generating rules))
                'working))
    ;; The empty prompt still matches; precedence is what decides.
    (should (cl-find 'idle
                     (ghostherd--match-all-rules
                      ghostherd-tests--agy-generating rules)
                     :key #'car))))

(ert-deftest ghostherd-test-agy-working-signals-stand-alone ()
  "Each signal has to carry the screen on its own -- agy does not
always print all three, and the spinner frame changes every tick."
  (let ((rules (plist-get (ghostherd--spec 'agy) :screen-rules)))
    (dolist (line '("\u28ff  Generating\u2026\n>\n"
                    "still going\n>\nesc to cancel\n"
                    ;; every 8-dot spinner frame, and a 6-dot one
                    "\u28fe x\n>\n"
                    "\u28fd x\n>\n"
                    "\u28fb x\n>\n"
                    "\u28bf x\n>\n"
                    "\u287f x\n>\n"
                    "\u28df x\n>\n"
                    "\u28ef x\n>\n"
                    "\u28f7 x\n>\n"
                    "\u280b x\n>\n"))
      (should (eq (car (ghostherd--match-rules line rules)) 'working)))))

(ert-deftest ghostherd-test-agy-idle-screen-is-still-idle ()
  "The new rules must not make a parked agy look busy.
`? for shortcuts\' and the model footer are chrome: they are on screen
whether or not anything is running."
  (let ((rules (plist-get (ghostherd--spec 'agy) :screen-rules))
        (screen (concat
                 "\u25cf Bash(git status)\n"
                 "  The changes have been committed.\n"
                 ">\n"
                 "? for shortcuts                 Gemini 3.8 Flash \u00b7 high\n")))
    (should (eq (car (ghostherd--match-rules screen rules)) 'idle))))

(defconst ghostherd-tests--claude-footer-idle
  (concat
   "  • claude / grok 我沒有動。它們的 esc to interrupt 實際是印成\n"
   "    (esc to interrupt · ctrl+t …) 這種形式。\n"
   "\n"
   "✻ Sautéed for 3m 23s · done 5:51 PM\n"
   "                                 ✔ Update installed · Restart to update\n"
   "──────────\n"
   "❯ commit 這個\n"
   "──────────\n"
   "  ⏵⏵ auto mode on (shift+tab to cycle) · ← for agents\n")
  "claude parked, having just printed an explanation about these rules.
Its footer drops `esc to interrupt' when nothing is running; the words
on screen are the ones it wrote.")

(defconst ghostherd-tests--claude-footer-working
  (concat
   "  • claude / grok 我沒有動。\n"
   "\n"
   "· Crafting… (18s · ↓ 996 tokens)\n"
   "                                 ✔ Update installed · Restart to update\n"
   "──────────\n"
   "❯ \n"
   "──────────\n"
   "  ⏵⏵ auto mode on (shift+tab to cycle) · esc to interrupt · ← for agents\n")
  "The same claude mid-task, captured from its own pane.
The hint is back, mid-line inside the footer -- which is why anchoring
to the start of a line cannot be the discriminator for this kind.")

(ert-deftest ghostherd-test-claude-footer-decides-working ()
  "Stuck at `working' while idle: the only match was `esc to interrupt'
inside prose the agent had printed about these very rules.  The footer
is the evidence -- claude drops the hint from it when nothing runs."
  (let ((rules (plist-get (ghostherd--spec 'claude) :screen-rules)))
    ;; The prompt has half-typed text in it, so no rule fires at all and
    ;; detection falls back to idle -- what matters is that `working'
    ;; is not among them.
    (should-not (eq (car (ghostherd--match-rules
                          ghostherd-tests--claude-footer-idle rules))
                    'working))
    (should-not (cl-find 'working
                         (ghostherd--match-all-rules
                          ghostherd-tests--claude-footer-idle rules)
                         :key #'car))
    (should (eq (car (ghostherd--match-rules
                      ghostherd-tests--claude-footer-working rules))
                'working))))

(defconst ghostherd-tests--claude-narrow-working
  (concat
   "  ⎿  $ tmux -L ghostherd capture-pane -p -t %74 |\n"
   "     tail -14 | nl -ba | cut -c1-110\n"
   "\n"
   "✽ Bloviating… (37s · ↓ 3.3k tokens)\n"
   "            ✔ Update installed · Restart to update\n"
   "────────────────────────────────────────────────────\n"
   "❯ \n"
   "────────────────────────────────────────────────────\n"
   "  ⏵⏵ auto mode on (shift+tab to cycle) · ⧉ herd-ga\n"
   "\n\n")
  "claude mid-turn in a pane 52 columns wide, captured from its own pane
\(2026-10-15): its footer cut off before `esc to interrupt', its spinner's
verb one no rule knew.  Read as idle, it was `done' minutes at a time.")

(defconst ghostherd-tests--claude-todo-working
  (concat
   "⏺ Now the rule itself.\n"
   "\n"
   "✶ Flummoxing… (1h 2m · still thinking with xhigh effort)\n"
   "  ⎿  ☒ Read the log\n"
   "     ☐ Fix the rule\n"
   "     ☐ Write the test\n"
   "     ☐ Break it on purpose\n"
   "            ✔ Update installed · Restart to update\n"
   "────────────────────────────────────────────────────\n"
   "❯ \n"
   "────────────────────────────────────────────────────\n"
   "  ⏵⏵ auto mode on (shift+tab to cycle) · ⧉ herd-ga\n")
  "The same with its todo list under its spinner, an hour in: the spinner
is ten lines up, far outside the status area.")

(ert-deftest ghostherd-test-claude-spinner-says-working-in-a-narrow-pane ()
  "Its spinner line, known by its shape -- glyph, a word, …, and the time
when it has one -- and read where claude draws it, above the input box:
the first line there that is not indented.  Seen (2026-10-15): twelve
turns since 10-01 read as done mid-turn, one quoting its spinner as the
reason."
  (let* ((spec (ghostherd--spec 'claude))
         (rules (plist-get spec :screen-rules))
         (spinner (plist-get spec :spinner))
         (narrow ghostherd-tests--claude-narrow-working)
         (with-box (lambda (above)
                     (concat above "\n" (make-string 52 ?─) "\n❯ \n" (make-string 52 ?─)
                             "\n  ⏵⏵ auto mode on (shift+tab to cycle) · ⧉ herd-ga\n"))))
    ;; As the herd read it.
    (should-not (eq (car (ghostherd--match-rules narrow rules t)) 'working))
    (should (equal (ghostherd--match-rules narrow rules t spinner) (cons 'working spinner)))
    (should (equal (ghostherd--rule-reason narrow (cons 'working spinner))
                   "✽ Bloviating… (37s · ↓ 3.3k tokens)"))
    (should (member (cons 'working spinner) (ghostherd--match-all-rules narrow rules t spinner)))
    ;; However far up its todo list pushes it.
    (should (equal (ghostherd--spinner-line ghostherd-tests--claude-todo-working spinner)
                   "✶ Flummoxing… (1h 2m · still thinking with xhigh effort)"))
    (should (eq (car (ghostherd--match-rules ghostherd-tests--claude-todo-working rules t spinner))
                'working))
    (should (equal (ghostherd--rule-reason ghostherd-tests--claude-todo-working (cons 'working spinner))
                   "✶ Flummoxing… (1h 2m · still thinking with xhigh effort)"))
    ;; Its glyphs, and its time as it grows.
    (dolist (line '("· Considering… (1m 30s · ↓ 9.3k tokens)" "✢ Pontificating… (18s · ↓ 880 tokens)"
                    "✳ Moseying… (0s)\n" "✻ Ruminating… (2h 1m · thought for 5s)"
                    ;; And its first frame, before the elapsed-time counter is
                    ;; drawn: a turn just begun (ask aed900f7).  Two words, too.
                    "· Hyperspacing…" "✽ Compacting conversation…"))
      (should (eq (car (ghostherd--match-rules (funcall with-box line) rules t spinner)) 'working)))
    ;; Its turn over: past tense.  Its own words, however they read:
    ;; what it said starts at ⏺, and a line of it quoted is indented.
    (dolist (above '("✻ Cogitated for 10s · done 2:34 PM"
                     "✻ Baked for 1s · done 9:20 AM\n            ✔ Update installed · Restart to update"
                     "⏺ Bloviating… (37s is what it said, verb and all)"
                     "⏺ It read ✽ Bloviating… (37s, mid-line"
                     "⏺ The spinner read:\n  ✽ Bloviating… (37s · ↓ 3.3k tokens)\n  and nothing matched it."
                     "Bloviating… (37s, said plainly"
                     ;; Above what it said last is the conversation.
                     "✽ Bloviating… (37s · ↓ 3.3k tokens)\n⏺ Done: the tests pass."))
      (should-not (eq (car (ghostherd--match-rules (funcall with-box above) rules t spinner)) 'working)))
    ;; No box at its foot -- a menu: nothing above it to read.
    (should-not (ghostherd--spinner-line (concat "✽ Bloviating… (37s)\n" (make-string 52 ?─) "\n❯ 1. Yes\n  2. No\n")
                                         spinner))
    ;; Nor an old prompt over one: a box opens under a rule.
    (should-not (ghostherd--spinning-p
                 "✽ Bloviating… (37s · ↓ 3.3k tokens)\n\n> run the tests\n Do you want to proceed?\n ❯ 1. Yes\n   2. No\n"
                 spinner))
    ;; Nor for a kind without one.
    (should-not (ghostherd--spinning-p narrow nil))))

(ert-deftest ghostherd-test-claude-spinner-through-detection ()
  "The poll asks it, and explain lists it."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "claude-x" :kind 'claude :backend 'fake))
          (ghostherd-tests--fake-live t))
      (setq ghostherd-tests--fake-screen ghostherd-tests--claude-narrow-working)
      (should (equal (ghostherd--detect-state s) '(working . "✽ Bloviating… (37s · ↓ 3.3k tokens)")))
      (save-window-excursion (ghostherd-explain s))
      (with-current-buffer "*ghostherd explain*"
        (should (string-match-p "^  working +->" (buffer-string)))))))

(defconst ghostherd-tests--claude-compacting
  (concat
   "⏺ I'll keep going.\n"
   "\n"
   "✽ Compacting conversation… (22s)\n"
   "            ✔ Update installed · Restart to update\n"
   "────────────────────────────────────────────────────\n"
   "❯ \n"
   "────────────────────────────────────────────────────\n"
   "  ⏵⏵ auto mode on (shift+tab to cycle) · ⧉ herd-gate · esc to interru\n")
  "claude auto-compacting mid-turn (2026-10-17): its spinner label is two
words -- \"Compacting conversation…\" -- and its footer is cut before `esc
to interrupt'.  Read as idle, its ask was settled from the screen while it
was still working.")

(ert-deftest ghostherd-test-claude-compaction-is-working ()
  "Auto-compaction is work, not a stop.  Its spinner label is two words, so
the one-word spinner missed it; its footer is cut before `esc to
interrupt', so no rule fired either -- and the agent read as done, its ask
settled from the screen mid-compaction.  The spinner, read where claude
draws it, is the evidence that it is working."
  (let* ((spec (ghostherd--spec 'claude))
         (rules (plist-get spec :screen-rules))
         (spinner (plist-get spec :spinner))
         (screen ghostherd-tests--claude-compacting))
    ;; The cut footer leaves no working rule to fire...
    (should-not (eq (car (ghostherd--match-rules screen rules t)) 'working))
    (should-not (cl-find 'working (ghostherd--match-all-rules screen rules t) :key #'car))
    ;; ...but the spinner line is found, two-word label and all, and it wins.
    (should (equal (ghostherd--spinner-line screen spinner)
                   "✽ Compacting conversation… (22s)"))
    (should (equal (ghostherd--match-rules screen rules t spinner) (cons 'working spinner)))))

(ert-deftest ghostherd-test-claude-compaction-through-detection ()
  "The poll reads the compacting screen as working, end to end."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "claude-c" :kind 'claude :backend 'fake))
          (ghostherd-tests--fake-live t))
      (setq ghostherd-tests--fake-screen ghostherd-tests--claude-compacting)
      (should (equal (ghostherd--detect-state s)
                     '(working . "✽ Compacting conversation… (22s)"))))))

(defconst ghostherd-tests--grok-working
  (concat
   "  ┃  ◆ Thinking…\n"
   "  ┃\n"
   "  ┃  Shioaji isn't logged in because the stack was down.\n"
   "                                                             █\n"
   "\n"
   "    ⠦ Thinking… 3.1s                     12m12s ⇣281k [stop]\n"
   "\n"
   "  ╭──────────────────────────────────────────────────────────╮\n"
   "  │ ❯                                                        │\n"
   "  ╰─────────────────────── Grok 4.7 (high) · always-approve ─╯\n"
   "\n"
   "  Shift+Tab:mode  │  Ctrl+c:cancel  │  Ctrl+x:shortcuts\n")
  "grok mid-turn, captured from its pane.
The spinner is seven lines from the bottom -- above the input box and
outside the status area -- so only the footer can say `working'.")

(defconst ghostherd-tests--grok-idle
  (concat
   "     按眼睛即可。它現在的狀態是已停止。\n"
   "\n"
   "     Worked for 13m10s\n"
   "                                                             █\n"
   "\n"
   "  ╭──────────────────────────────────────────────────────────╮\n"
   "  │ ❯                                                        │\n"
   "  ╰─────────────────────── Grok 4.7 (high) · always-approve ─╯\n"
   "\n"
   "  Shift+Tab:mode  │  Ctrl+x:shortcuts\n")
  "The same grok after the turn ended: the footer drops `Ctrl+c:cancel'.")

(defconst ghostherd-tests--grok-waiting
  (concat
   "     ◈ Read 1 skill                                                              █\n"
   "                                                                                 █\n"
   "     The command is a 30-second sleep, so I'll start it in the         4:32 PM   █\n"
   "     background and then wait on that task.                                      █\n"
   "                                                                                 █\n"
   "     ◆ Task started: Sleep 30s then print probe-done                             █\n"
   "                                                                                 █\n"
   "\n"
   "   ⠴ Sleep 30s then print probe-done… 0.3s                     24s ⇣27.3k [stop]\n"
   "\n"
   "   ▾ Tasks 1\n"
   "   : Run Sleep 30s then print probe-done                                      3s\n"
   "\n"
   "  ╭────────────────────────────────────────────────────────────────────────────╮\n"
   "  │ ❯                                                                          │\n"
   "  ╰──────────────────────────────────────── Grok 4.7 (xhigh) · always-approve ─╯\n"
   "\n"
   "  Shift+Tab:mode  │  Ctrl+x:shortcuts\n")
  "grok waiting on a command it sent to the background, captured from a
grok 1.0.50 pane (2026-10-15): `Ctrl+c:cancel' gone from its footer, its
spinner eleven lines up, over its task list.")

(ert-deftest ghostherd-test-grok-waiting-on-a-task-is-working ()
  "Read idle for as long as it waited on a task of its own -- twice on
10-12, two minutes once: its footer drops `Ctrl+c:cancel' then.  Its
spinner, braille to its [stop] button, says so wherever it sits."
  (let* ((spec (ghostherd--spec 'grok))
         (rules (plist-get spec :screen-rules))
         (spinner (plist-get spec :spinner))
         (waiting ghostherd-tests--grok-waiting))
    (should-not (eq (car (ghostherd--match-rules waiting rules)) 'working))
    (should (equal (ghostherd--match-rules waiting rules nil spinner) (cons 'working spinner)))
    (should (equal (ghostherd--rule-reason waiting (cons 'working spinner))
                   "⠴ Sleep 30s then print probe-done… 0.3s 24s ⇣27.3k [stop]"))
    ;; Five more tasks: still found.
    (should (eq (car (ghostherd--match-rules
                      (replace-regexp-in-string
                       "   : Run Sleep 30s then print probe-done +3s\n"
                       (lambda (m) (apply #'concat (make-list 6 m))) waiting)
                      rules nil spinner))
                'working))
    ;; Its spinner seven lines up, with no tasks.
    (should (ghostherd--spinning-p ghostherd-tests--grok-working spinner))
    ;; Thirty lines up at most: a screen is not read to its top.
    (should-not (ghostherd--spinning-p
                 (replace-regexp-in-string
                  "   : Run Sleep 30s then print probe-done +3s\n"
                  (lambda (m) (apply #'concat (make-list 30 m))) waiting)
                 spinner))
    ;; Its turn over; a line of what it said that reads like it ends at
    ;; the scrollbar; the box a menu's.
    (should-not (ghostherd--spinning-p ghostherd-tests--grok-idle spinner))
    (should-not (ghostherd--spinning-p
                 (replace-regexp-in-string "   ⠴ Sleep 30s.*\\[stop\\]\n"
                                           "     ⠴ it showed … 24s ⇣27.3k [stop]                       █\n"
                                           waiting)
                 spinner))
    (should-not (ghostherd--spinning-p
                 (replace-regexp-in-string "  │ ❯ " "  │ 1. Yes" waiting) spinner))))

(ert-deftest ghostherd-test-grok-footer-decides-working ()
  "Reported idle mid-turn: the spinner fell outside the status area and
no working rule knew grok's footer hint."
  (let ((rules (plist-get (ghostherd--spec 'grok) :screen-rules)))
    (should (eq (car (ghostherd--match-rules
                      ghostherd-tests--grok-working rules))
                'working))
    (should-not (cl-find 'working
                         (ghostherd--match-all-rules
                          ghostherd-tests--grok-idle rules)
                         :key #'car))))

;;; What a screen says besides the state

(defconst ghostherd-tests--rule (make-string 60 ?─))

(defun ghostherd-tests--claude-screen (box &optional footer above)
  "A claude screen: ABOVE, then its input box holding BOX's lines, then
FOOTER under it -- padded to the pane's height, as a capture is."
  (string-join
   (append (or above '("⏺ Done: the tests pass." ""
                       "✻ Sautéed for 1m 25s · done 5:28 PM"))
           (list ghostherd-tests--rule)
           box
           (list ghostherd-tests--rule
                 (or footer "  ⏵⏵ auto mode on (shift+tab to cycle)")
                 "" ""))
   "\n"))

(ert-deftest ghostherd-test-a-screens-foot ()
  "What claude's foot says: its input box's text -- a wrapped one joined,
with a space between words and none between ideographs -- and the
background work its footer counts.  An empty box, a menu, an old prompt
up in the conversation say nothing."
  (let ((nbsp " "))
    (should (equal (ghostherd--foot (ghostherd-tests--claude-screen
                                     (list (concat "❯" nbsp "好，等你看完再說"))
                                     "  ⏵⏵ auto mode on · 1 shell · ← for agents · ↓ to manage"))
                   '(:input "好，等你看完再說" :background ("1 shell"))))
    (should (equal (ghostherd--foot (ghostherd-tests--claude-screen
                                     '("❯ Add a test for the" "  retry path")
                                     "  2 shells · 1 agent · 3 monitors · ↓ to manage"))
                   '(:input "Add a test for the retry path" :background ("2 shells" "1 agent" "3 monitors"))))
    (should (equal (plist-get (ghostherd--foot (ghostherd-tests--claude-screen '("❯ 做 v2 的 diff 行評論" "  送給 agent"))) :input)
                   "做 v2 的 diff 行評論送給 agent"))
    (should (equal (ghostherd--foot (ghostherd-tests--claude-screen (list (concat "❯" nbsp))))
                   '(:input nil :background nil)))
    ;; Not a box: a menu's first choice; an old prompt above the foot.
    (should-not (ghostherd--foot (ghostherd-tests--claude-screen '("❯ 1. Yes"))))
    (should-not (ghostherd--foot (string-join (list ghostherd-tests--rule "❯ an old prompt" ghostherd-tests--rule
                                                     "⏺ and much said since" "" "it goes on" "and on")
                                               "\n")))
    (should-not (ghostherd--foot ""))
    (should-not (ghostherd--foot nil))))

(ert-deftest ghostherd-test-a-suggestion-is-faint ()
  "A suggestion is drawn faint in the box; what you typed there is not."
  (let ((faint (ghostherd-tests--claude-screen
                (list "\e[39m❯ \e[2m好，等你看完再說\e[0m")
                "\e[39m  \e[38;5;220m⏵⏵ auto mode on\e[38;5;246m · \e[38;5;44m1 shell\e[39m"
                '("\e[38;5;246m✻\e[39m done" "")))
        (typed (ghostherd-tests--claude-screen (list "\e[39m❯ hello\e[0m"))))
    (should (ghostherd--faint-input-p faint))
    (should-not (ghostherd--faint-input-p typed))
    (should (ghostherd--faint-input-p (ghostherd-tests--claude-screen (list "❯ \e[1;2mfaint and bold\e[0m"))))
    ;; Faint words higher up, in the conversation, are not the box's.
    (should-not (ghostherd--faint-input-p
                 (ghostherd-tests--claude-screen (list "❯ typed")
                                                 nil '("\e[2m❯ an old faint line\e[0m" ""))))
    (should-not (ghostherd--faint-input-p nil))))

(ert-deftest ghostherd-test-the-poll-reads-a-screens-foot ()
  "Each poll keeps what a claude's foot says.  New text in its box: the
host is asked, once, for the screen with its styles -- faint, it is a
suggestion, kept while the box holds it.  Typed, it is none.  Not
claude: nothing is read."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (ghostherd-tests--with-herd ((ghostherd--feet (make-hash-table :test 'equal))
                               (ghostherd--screens (make-hash-table :test 'equal)))
    (let* ((s (ghostherd-tests--session :name "rd" :kind 'claude :backend 'fake))
           (g (ghostherd-tests--session :name "gk" :kind 'grok :backend 'fake))
           (asked 0)
           (styled nil)
           (screen (lambda (box &optional footer) (ghostherd-tests--claude-screen (list box) footer))))
      (cl-letf (((symbol-function 'ghostherd-backend-styled-screen)
                 (lambda (_backend _session cb) (cl-incf asked) (funcall cb styled))))
        (setq styled (funcall screen "❯ \e[2mRun the tests\e[0m"))
        (ghostherd--read-foot s (funcall screen "❯ Run the tests" "  1 shell · ↓ to manage"))
        (should (equal (ghostherd-session-suggestion s) "Run the tests"))
        (should (equal (ghostherd-session-background s) '("1 shell")))
        (should (= asked 1))
        ;; The same text: kept, not asked again.
        (ghostherd--read-foot s (funcall screen "❯ Run the tests"))
        (should (equal (ghostherd-session-suggestion s) "Run the tests"))
        (should-not (ghostherd-session-background s))
        (should (= asked 1))
        ;; Taken, and typed on: the suggestion is gone at once, not when
        ;; the host has answered.
        (let (later)
          (cl-letf (((symbol-function 'ghostherd-backend-styled-screen)
                     (lambda (_b _s cb) (setq later cb))))
            (ghostherd--read-foot s (funcall screen "❯ Run the tests now"))
            (should-not (ghostherd-session-suggestion s))
            (funcall later (funcall screen "❯ Run the tests now"))
            (should-not (ghostherd-session-suggestion s))))
        ;; Sent: the box is empty, and so is the suggestion.
        (ghostherd--read-foot s (funcall screen "❯ "))
        (should-not (ghostherd-session-suggestion s))
        ;; Typed at the desk: asked, and not faint.
        (setq styled (funcall screen "❯ my own words"))
        (ghostherd--read-foot s (funcall screen "❯ my own words"))
        (should (= asked 2))
        (should-not (ghostherd-session-suggestion s))
        ;; A styled screen that comes too late, the box moved on: not taken.
        (let (later)
          (cl-letf (((symbol-function 'ghostherd-backend-styled-screen)
                     (lambda (_b _s cb) (setq later cb))))
            (ghostherd--read-foot s (funcall screen "❯ Ship it"))
            (ghostherd--read-foot s (funcall screen "❯ "))
            (funcall later (funcall screen "❯ \e[2mShip it\e[0m"))
            (should-not (ghostherd-session-suggestion s))))
        ;; Another kind's foot is not read.
        (ghostherd--read-foot g (funcall screen "❯ Run the tests" "  1 shell"))
        (should-not (gethash (ghostherd-session-id g) ghostherd--feet))
        ;; Through the poll, from the screen it has; to the page.
        (setq styled (funcall screen "❯ \e[2mCommit it\e[0m"))
        (cl-letf (((symbol-function 'ghostherd--session-live-p) (lambda (_s) t))
                  ((symbol-function 'ghostherd--host-exited-p) (lambda (_s) nil)))
          (ghostherd--detect-state s (funcall screen "❯ Commit it" "  2 shells · ↓ to manage")))
        (should (equal (ghostherd-session-suggestion s) "Commit it"))
        (let ((page (ghostherd-memory-page--agent s)))
          (should (equal (plist-get page :suggestion) "Commit it"))
          (should (equal (plist-get page :background) ["2 shells"])))
        ;; Killed: forgotten.
        (cl-letf (((symbol-function 'ghostherd--host-kill) #'ignore))
          (ghostherd-kill s))
        (should-not (gethash (ghostherd-session-id s) ghostherd--feet))))))

(ert-deftest ghostherd-test-status-region-is-the-bottom ()
  "The region is the live status area, not the scrollback above it."
  (let ((ghostherd-status-lines 3))
    (should (equal (ghostherd--status-region "a\nb\nc\nd\ne") "c\nd\ne"))
    ;; A capture is padded to the pane height; the padding is not status.
    (should (equal (ghostherd--status-region "a\nb\nc\nd\ne\n\n   \n\n")
                   "c\nd\ne"))
    (should (equal (ghostherd--status-region "only") "only"))
    (should (equal (ghostherd--status-region "") ""))))

(ert-deftest ghostherd-test-reason-quotes-the-deciding-line ()
  "The reason used to quote the first line matching the pattern, which
after this change is a line that did not decide anything."
  (let* ((rules (plist-get (ghostherd--spec 'claude) :screen-rules))
         (hit (ghostherd--match-rules
               ghostherd-tests--claude-footer-working rules))
         (reason (ghostherd--rule-reason
                  ghostherd-tests--claude-footer-working hit)))
    (should (eq (car hit) 'working))
    (should (string-match-p "auto mode on" reason))
    (should-not (string-match-p "我沒有動" reason))))

(defconst ghostherd-tests--agy-talks-about-its-own-rules
  (concat
   "\u25cf Bash(git commit -m \"fix(ghostherd): recognize agy busy indicators\")\n"
   "\n"
   "  The changes have been committed directly to master.\n"
   "\n"
   "  \u2022 ghostherd.el:\n"
   "      \u2022 Updated agy working screen rules to include \"Generating\",\n"
   "      \"esc to cancel\" (which agy prints instead of \"esc to interrupt\"),\n"
   "      and the full Unicode braille range \"[\u2801-\u28ff]\" (covering the\n"
   "      8-dot braille spinner family used by agy).\n"
   "\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n"
   ">\n"
   "\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n"
   "? for shortcuts                            Gemini 3.8 Flash \u00b7 high\n")
  "agy idle, having just written a commit summary about these rules.
Every working indicator is on screen -- as prose it quoted, not as its
own status line.")

(ert-deftest ghostherd-test-agy-writing-about-a-spinner-is-not-a-spinner ()
  "Stuck at `working' for as long as the summary stayed on screen.
The agent had committed a change to these very rules and quoted them
back; the tail keeps everything it ever printed, so a substring match
read the transcript instead of the state."
  (let ((rules (plist-get (ghostherd--spec 'agy) :screen-rules)))
    (should (eq (car (ghostherd--match-rules
                      ghostherd-tests--agy-talks-about-its-own-rules rules))
                'idle))
    (should-not (cl-find 'working
                         (ghostherd--match-all-rules
                          ghostherd-tests--agy-talks-about-its-own-rules rules)
                         :key #'car))))

(ert-deftest ghostherd-test-agy-status-line-survives-a-box-edge ()
  "Anchoring must allow the indentation a TUI actually uses: leading
whitespace and a box edge, and nothing else -- a bullet is prose."
  (let ((rules (plist-get (ghostherd--spec 'agy) :screen-rules)))
    (dolist (line '("\u28ff Generating\u2026\n>\n"
                    "   \u28ff Generating\u2026\n>\n"
                    "\u2502  \u28ff Generating\u2026\n>\n"
                    "\u2503 esc to cancel\n>\n"))
      (should (eq (car (ghostherd--match-rules line rules)) 'working)))
    (dolist (line '("  \u2022 it prints \u28ff Generating\u2026 while busy\n>\n"
                    "  \u2022 Generating is the label agy uses\n>\n"
                    "  say \"esc to cancel\" to stop it\n>\n"))
      (should (eq (car (ghostherd--match-rules line rules)) 'idle)))))

(defconst ghostherd-tests--agy-permissions-json-idle
  (concat
   "  我已經幫你把 settings.json 更新好了\n"
   "      \"permissions\": {\n"
   "        \"allow\": [\n"
   "          \"command(git commit)\"\n"
   "        ],\n"
   ">\n"
   "? for shortcuts                                               Gemini 3.1 Pro\n")
  "agy parked at a prompt after editing a permissions allow-list.
The JSON key used to fire the `permission' blocked rule.")

(ert-deftest ghostherd-test-permissions-json-is-not-a-prompt ()
  "agy-ghost-commit sat at blocked on this screen: the agent was
done, the prompt was empty, and the word that matched was the JSON
key `permissions'.  Prefer a missed blocked to a false one."
  (dolist (kind '(agy claude grok))
    (let ((rules (plist-get (ghostherd--spec kind) :screen-rules)))
      (should (eq (car (ghostherd--match-rules
                        ghostherd-tests--agy-permissions-json-idle rules))
                  'idle))
      (should-not (cl-find 'blocked
                           (ghostherd--match-all-rules
                            ghostherd-tests--agy-permissions-json-idle rules)
                           :key #'car)))))

(ert-deftest ghostherd-test-permission-as-a-word-still-blocks ()
  "Tightening must not throw away a real prompt that says permission."
  (dolist (kind '(agy claude grok))
    (let ((rules (plist-get (ghostherd--spec kind) :screen-rules)))
      (should (eq (car (ghostherd--match-rules
                        "Waiting for permission to continue\n" rules))
                  'blocked))))
  ;; And a proceed-box is still a proceed-box.
  (let ((rules (plist-get (ghostherd--spec 'agy) :screen-rules)))
    (should (eq (car (ghostherd--match-rules
                      ghostherd-tests--permission-screen rules))
                'blocked))))

(ert-deftest ghostherd-test-permission-mode-is-not-a-prompt ()
  "The hyphen is a word boundary for `\\<', which is why this pattern
does not use one: grok prints `permission-mode' in chrome that stays
on screen at idle."
  (dolist (kind '(agy claude grok))
    (let ((rules (plist-get (ghostherd--spec kind) :screen-rules)))
      (should-not (cl-find 'blocked
                           (ghostherd--match-all-rules
                            "Grok 4.6 · permission-mode always-approve\n> \n"
                            rules)
                           :key #'car)))))

;;; OSC 9;4 progress

(ert-deftest ghostherd-test-progress-fresh-only-while-running ()
  "`remove' and `pause' mean reporting stopped; scraping should take over."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a")))
      (setf (ghostherd-session-progress-at s) (current-time))
      (dolist (state '(set indeterminate))
        (setf (ghostherd-session-progress-state s) state)
        (should (ghostherd--progress-fresh-p s)))
      (dolist (state '(remove pause error nil))
        (setf (ghostherd-session-progress-state s) state)
        (should-not (ghostherd--progress-fresh-p s))))))

(ert-deftest ghostherd-test-progress-expires ()
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a"))
          (ghostherd-progress-ttl 5))
      (setf (ghostherd-session-progress-state s) 'set
            (ghostherd-session-progress-at s) (time-subtract (current-time) 60))
      (should-not (ghostherd--progress-fresh-p s)))))

(ert-deftest ghostherd-test-progress-respects-opt-out ()
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a"))
          (ghostherd-use-osc-progress nil))
      (setf (ghostherd-session-progress-state s) 'set
            (ghostherd-session-progress-at s) (current-time))
      (should-not (ghostherd--progress-fresh-p s)))))

(ert-deftest ghostherd-test-progress-reason-includes-percent ()
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a")))
      (setf (ghostherd-session-progress-percent s) 42)
      (should (equal (ghostherd--progress-reason s) "osc progress 42%"))
      (setf (ghostherd-session-progress-percent s) nil)
      (should (equal (ghostherd--progress-reason s) "osc progress")))))

(ert-deftest ghostherd-test-progress-handler-chains ()
  "`ghostel-progress-function' is a single global, very likely someone's
spinner, so ghostherd must chain to it and hand it back on teardown."
  (ghostherd-tests--with-herd ()
    (let* ((seen nil)
           (original (lambda (state percent) (setq seen (list state percent))))
           (ghostel-progress-function original)
           (ghostherd--saved-progress-function nil)
           (s (ghostherd-tests--session :name "a")))
      (ghostherd--install-hooks)
      (should (eq ghostel-progress-function #'ghostherd--on-progress))
      (should (eq ghostherd--saved-progress-function original))
      (with-current-buffer (ghostherd-session-buffer s)
        (funcall ghostel-progress-function 'set 42))
      (should (equal seen '(set 42)))
      (should (eq (ghostherd-session-progress-state s) 'set))
      (should (equal (ghostherd-session-progress-percent s) 42))
      (ghostherd--remove-hooks)
      (should (eq ghostel-progress-function original)))))

;;; State detection precedence

(defun ghostherd-tests--detect (screen &optional progress-state)
  "Detect state for a fake agy session showing SCREEN."
  (let ((s (ghostherd-tests--session :name "a" :kind 'agy :state 'idle)))
    (with-current-buffer (ghostherd-session-buffer s) (insert screen))
    (when progress-state
      (setf (ghostherd-session-progress-state s) progress-state
            (ghostherd-session-progress-at s) (current-time)))
    (ghostherd--detect-state s)))

(ert-deftest ghostherd-test-detect-blocked-beats-fresh-progress ()
  "A stale-but-fresh progress report must never mask a real prompt."
  (ghostherd-tests--with-herd ()
    (should (eq (car (ghostherd-tests--detect "Do you want to proceed\n" 'set))
                'blocked))))

(ert-deftest ghostherd-test-detect-progress-beats-quiet-screen ()
  "Scraping cannot tell a working agent from a quiet one; progress can."
  (ghostherd-tests--with-herd ()
    (should (eq (car (ghostherd-tests--detect "> \n" 'set)) 'working))
    (should (eq (car (ghostherd-tests--detect "> \n" nil)) 'idle))))

(ert-deftest ghostherd-test-detect-manual-state-wins ()
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy)))
      (setf (ghostherd-session-manual-state s) 'blocked)
      (should (equal (ghostherd--detect-state s) '(blocked . "manual"))))))

(ert-deftest ghostherd-test-detect-dead-buffer ()
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy)))
      (kill-buffer (ghostherd-session-buffer s))
      (should (equal (car (ghostherd--detect-state s)) 'dead)))))

;;; A menu prompt, read off the screen

(defconst ghostherd-test--claude-question
  "│ Turn it on now?
│ I set the port, then run `tailscale serve --bg` to it.
❯ 1. Yes, turn it on
     I do all of the above and report what the checks show.
  2. Not yet, and leave the code loaded but inactive until I say so
     I stop here; the code is loaded but inactive.
  3. Type something.
──────────────────────────────────────────────────────────────────────────────────
  4. Chat about this
Enter to select · Tab/Arrow keys to navigate · Esc to cancel
"
  "claude asking a question of its own, each choice described below it.")

(defconst ghostherd-test--claude-ask
  "│ DESIGN.md's markdown reading style never took effect; how to handle?
❯ 1. Make it work  (Recommended)
     Move the .wmde-markdown block out of @layer components.
  2. Only the canvas cards
     Keep the workspace reading area as it is.
  3. Leave it for now
     Keep just the known-issue note.
  4. Type something.
  5. Chat about this
Enter to select · Tab/Arrow keys to navigate · Esc to cancel
"
  "claude's AskUserQuestion menu whose choices say nothing a `Yes'/`proceed'
rule would catch: only its footer marks it as waiting.")

(defconst ghostherd-test--claude-ask-preview
  "⏺ 我算出了調整方案：淺色 primary 改為 #8a5034、深色 destructive 改為
  #e08478（按鈕文字需同步改為深墨色）以通過對比度檢查。搜尋命中字和深色
  destructive 這兩項我會直接照既有模式修改，但 primary
  屬於品牌色，想先跟你確認再動手。

──────────────────────────────────────────────────────────────────────────────────
 ☐ Primary

│ 淺色主題的 primary（赤陶）當文字疊在淡色底上不夠清楚（啟用狀態、badge、選取項約
│ 35 處），要怎麼修？

❯ 1. 淺色 primary 調深            ┌──────────────────────────────────────────┐
    (Recommended)                 │ 淺色 --primary  #9a5b3c → #8a5034        │
  2. 品牌色不動，加新 token       │                                          │
                                  │   當文字                                 │
                                  │     選取底 sel      3.85 → 4.62          │
                                  │     chip           4.14 → 4.98           │
                                  │     primary/10     4.42 → 5.25           │
                                  │     紙張底（連結） 4.82 → 5.79           │
                                  │   當按鈕底（白字）  5.14 → 6.17          │
                                  │                                          │
                                  │ 改動：DESIGN.md + index.css 各 1 行      │
                                  └──────────────────────────────────────────┘

                                  Notes: press n to add notes

──────────────────────────────────────────────────────────────────────────────────
  Chat about this

Enter to select · ↑/↓ to navigate · n to add notes · Esc to cancel
"
  "claude's AskUserQuestion with a preview box drawn to the right of the
choices, a `(Recommended)' tag wrapped under choice 1, a checkbox header,
and the footer far below the choices -- the variant 7b082a8 did not cover.")

(ert-deftest ghostherd-test-prompt-read-off-the-screen ()
  "A menu's question and its choices, numbered as you answer them: the
lines above the first choice up to a rule, each choice's first line."
  (should (equal (ghostherd--prompt ghostherd-test--agy-menu)
                 '(:question "Requesting permission for:\ngrep -rn -C 3 \"respawn\" local/ghostherd/\nRun this command?"
                   :options ((1 . "Yes, run command")
                             (2 . "Yes, and always allow in this conversation for commands that start with 'grep'")
                             (3 . "Yes, and always allow for commands that start with 'grep' (Persist to settings.json)")
                             (4 . "No, cancel"))
                   :on 1)))
  (should (equal (ghostherd--prompt ghostherd-test--claude-question)
                 '(:question "Turn it on now?\nI set the port, then run `tailscale serve --bg` to it."
                   :options ((1 . "Yes, turn it on")
                             (2 . "Not yet, and leave the code loaded but inactive until I say so")
                             (3 . "Type something.") (4 . "Chat about this"))
                   :on 1)))
  (should (equal (ghostherd--prompt ghostherd-test--claude-permission)
                 '(:question "Bash command\nnpm test -- --runInBand\nRun the test suite\nDo you want to proceed?"
                   :options ((1 . "Yes")
                             (2 . "Yes, and don't ask again for npm test commands in /x")
                             (3 . "No, and tell Claude what to do differently (esc)"))
                   :on 1)))
  ;; On: the choice its mark is on, wherever; none marked, none.
  (should (eql (plist-get (ghostherd--prompt (ghostherd-test--agy-menu-on 3)) :on) 3))
  (should (equal (plist-get (ghostherd--prompt (ghostherd-test--agy-menu-on nil)) :options)
                 (plist-get (ghostherd--prompt ghostherd-test--agy-menu) :options)))
  (should-not (plist-get (ghostherd--prompt (ghostherd-test--agy-menu-on nil)) :on))
  ;; In a box, a choice's description is not its words wrapped, however
  ;; wide the box's padding makes its line.
  (should (equal (plist-get (ghostherd--prompt (concat "│ Run it?                                                         │\n"
                                                       "│ ❯ 1. Yes                                                        │\n"
                                                       "│ Runs it once, here                                              │\n"
                                                       "│   2. No                                                         │\n"))
                            :options)
                 '((1 . "Yes") (2 . "No"))))
  ;; A long choice, cut; six lines of question at most.  A hint under a
  ;; short last choice is not part of it.
  (let ((p (ghostherd--prompt (concat (mapconcat (lambda (i) (format "line %d" i)) (number-sequence 1 9) "\n")
                                      "\n> 1. " (make-string 120 ?x) "\n  2. No\n"))))
    (should (= (length (cdr (assq 1 (plist-get p :options)))) 100))
    (should (equal (plist-get p :question) "line 4\nline 5\nline 6\nline 7\nline 8\nline 9"))))

(ert-deftest ghostherd-test-claude-askuserquestion-is-blocked ()
  "claude parked on an AskUserQuestion menu reads blocked -- so the herd
page offers its choices, which `ghostherd--prompt' reads -- by its footer,
not by any \"Yes\"/\"proceed\" wording.  An empty prompt is not blocked by
the new rule, and a spinner above the box is still working."
  (let* ((spec (ghostherd--spec 'claude))
         (rules (plist-get spec :screen-rules))
         (ri (plist-get spec :prompt-replaces-input))
         (sp (plist-get spec :spinner))
         ;; With the footer rule gone, nothing else in this menu is blocked.
         (bare (cons (cons 'blocked (seq-remove (lambda (r) (member r '("Tab/Arrow keys to navigate" "Enter to select")))
                                                (alist-get 'blocked rules)))
                     (assq-delete-all 'blocked (copy-alist rules)))))
    (should (eq (car (ghostherd--match-rules ghostherd-test--claude-ask rules ri sp)) 'blocked))
    (should-not (eq (car (ghostherd--match-rules ghostherd-test--claude-ask bare ri sp)) 'blocked))
    (should (plist-get (ghostherd--prompt ghostherd-test--claude-ask) :options))
    (should (eq (car (ghostherd--match-rules "⏺ done.\n────\n❯ \n────\n" rules ri sp)) 'idle))
    (should (eq (car (ghostherd--match-rules "✻ Baking… (3s · ↓ 1k tokens)\n────\n❯ \n────\n" rules ri sp))
                'working))))

(ert-deftest ghostherd-test-prompt-reads-the-preview-variant ()
  "AskUserQuestion with a preview box to the right of the choices: read
with the box's │…│ kept out, a wrapped `(Recommended)' rejoined onto its
choice, the checkbox header left out of the question, and the footer far
below the choices still anchoring them.  The card's reason is the
question, not the `Chat about this' line the footer sits under."
  (should (equal (ghostherd--prompt ghostherd-test--claude-ask-preview)
                 '(:question "淺色主題的 primary（赤陶）當文字疊在淡色底上不夠清楚（啟用狀態、badge、選取項約\n35 處），要怎麼修？"
                   :options ((1 . "淺色 primary 調深 (Recommended)")
                             (2 . "品牌色不動，加新 token"))
                   :on 1)))
  (should (equal (ghostherd--prompt-reason ghostherd-test--claude-ask-preview)
                 "淺色主題的 primary（赤陶）當文字疊在淡色底上不夠清楚（啟用狀態、badge、選取項約 35 處），要怎麼修？"))
  ;; The whole way through: found blocked by the footer, its reason the
  ;; question rather than "Chat about this".
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "cj" :kind 'claude :backend 'fake))
          (ghostherd-tests--fake-screen ghostherd-test--claude-ask-preview))
      (pcase-let ((`(,state . ,reason) (ghostherd--detect-state s)))
        (should (eq state 'blocked))
        (should-not (string-match-p "Chat about this" reason))
        (should (string-match-p "要怎麼修？" reason))))))

(defconst ghostherd-test--claude-trust-yes
  (replace-regexp-in-string
   "   Yes, I trust this folder" " ❯ Yes, I trust this folder"
   (replace-regexp-in-string " ❯ No, exit" "   No, exit"
                             ghostherd-test--claude-trust))
  "The trust prompt with its highlight moved to the second choice.")

(defconst ghostherd-test--claude-trust-quoted
  "⏺ I read the onboarding note: \"Quick safety check: Is this a project you
  created or one you trust?  …  Yes, I trust this folder\".  I summarised it.

 ─────────────────────────────────────────────────────────────
❯
 ─────────────────────────────────────────────────────────────
  ⏵⏵ auto mode on (shift+tab to cycle)
"
  "The trust prompt's words merely quoted in the conversation, with claude's
input box drawn at the foot: `:prompt-replaces-input' must keep it unblocked.")

(ert-deftest ghostherd-test-claude-trust-prompt-is-blocked ()
  "claude parked on its trust-folder prompt reads blocked -- by the safety
question (which becomes the card's reason), not by any \"Yes\"/\"proceed\"
wording -- so the page offers its two choices, which `ghostherd--prompt'
reads off the marked [❯] menu though they carry no numbers.  The same
sentence quoted in the chat, over a drawn input box, is not blocked; an
empty prompt stays idle and a spinner stays working."
  (let* ((spec (ghostherd--spec 'claude))
         (rules (plist-get spec :screen-rules))
         (ri (plist-get spec :prompt-replaces-input))
         (sp (plist-get spec :spinner))
         ;; With the trust rules gone, nothing else in this screen is blocked.
         (bare (cons (cons 'blocked (seq-remove (lambda (r) (member r '("Quick safety check" "Yes, I trust this folder")))
                                                (alist-get 'blocked rules)))
                     (assq-delete-all 'blocked (copy-alist rules))))
         (hit (ghostherd--match-rules ghostherd-test--claude-trust rules ri sp)))
    ;; Found blocked, and by the trust rule -- not some other match.
    (should (eq (car hit) 'blocked))
    (should-not (eq (car (ghostherd--match-rules ghostherd-test--claude-trust bare ri sp)) 'blocked))
    ;; The reason names the problem itself.
    (should (string-match-p "Quick safety check" (ghostherd--rule-reason ghostherd-test--claude-trust hit)))
    ;; Both un-numbered choices read off the menu, highlight on the first.
    (should (equal (ghostherd--prompt ghostherd-test--claude-trust)
                   '(:question ""
                     :options ((1 . "No, exit") (2 . "Yes, I trust this folder"))
                     :on 1)))
    ;; Highlight tracked to the other choice (so `ghostherd-answer' counts right).
    (should (= (plist-get (ghostherd--prompt ghostherd-test--claude-trust-yes) :on) 2))
    ;; Quoted in the chat with the input box drawn: not a prompt.
    (should-not (eq (car (ghostherd--match-rules ghostherd-test--claude-trust-quoted rules ri sp)) 'blocked))
    (should-not (ghostherd--prompt ghostherd-test--claude-trust-quoted))
    ;; Regressions: an empty prompt is idle, a spinner above the box working.
    (should (eq (car (ghostherd--match-rules "⏺ done.\n────\n❯ \n────\n" rules ri sp)) 'idle))
    (should (eq (car (ghostherd--match-rules "✻ Baking… (3s · ↓ 1k tokens)\n────\n❯ \n────\n" rules ri sp))
                'working))))

(ert-deftest ghostherd-test-claude-trust-prompt-detect-and-reason ()
  "End to end: a claude sitting on the trust prompt is detected blocked and
its reason names the safety check, so the card does not read it as ready."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "ct" :kind 'claude :backend 'fake))
          (ghostherd-tests--fake-screen ghostherd-test--claude-trust))
      (pcase-let ((`(,state . ,reason) (ghostherd--detect-state s)))
        (should (eq state 'blocked))
        (should (string-match-p "Quick safety check" reason))))))

;;; A blocked pattern above the input box is the conversation's

(defconst ghostherd-test--claude-wrote-permission
  (concat "● The parser tests use real screens: agy's prompt from this time, claude's question menu\n"
          "  and claude's permission prompt.\n\n"
          "✻ Bunning… (34s · ↓ 2.3k tokens)\n"
          "                                          ✔ Update installed · Restart to update\n"
          (make-string 82 ?─) "\n"
          "❯ \n"
          (make-string 82 ?─) "\n"
          "  ⏵⏵ auto mode on (shift+tab to cycle) · esc to interrupt · ← for agents\n")
  "claude at work, having written of a permission prompt: its own words
on screen, its input box at the foot.")

(defun ghostherd-test--rules-of (kind)
  (let ((spec (ghostherd--spec kind)))
    (list (plist-get spec :screen-rules) (plist-get spec :prompt-replaces-input))))

(ert-deftest ghostherd-test-blocked-not-the-conversations-words ()
  "For a kind whose menus take its input box's place, a `blocked'
pattern with the box drawn at the foot is the conversation's: working
or idle as the rest says.  The menu itself, the box gone, is blocked.
grok's question card is drawn so too, its box framed (ask cf9873be)."
  (pcase-let ((`(,claude ,replaces) (ghostherd-test--rules-of 'claude)))
    (should replaces)
    (should (ghostherd--input-at-foot-p ghostherd-test--claude-wrote-permission))
    (should (eq (car (ghostherd--match-rules ghostherd-test--claude-wrote-permission claude t)) 'working))
    (should (eq (car (ghostherd--match-rules ghostherd-test--claude-wrote-permission claude)) 'blocked))
    (let ((idle (replace-regexp-in-string "✻ Bunning.*\n" "" (replace-regexp-in-string " · esc to interrupt" "" ghostherd-test--claude-wrote-permission))))
      (should (eq (car (ghostherd--match-rules idle claude t)) 'idle)))
    (should-not (ghostherd--input-at-foot-p ghostherd-test--claude-question))
    (should (eq (car (ghostherd--match-rules ghostherd-test--claude-question claude t)) 'blocked))
    (should (eq (car (ghostherd--match-rules ghostherd-test--claude-permission claude t)) 'blocked))
    ;; What `ghostherd-explain' lists agrees with the state.
    (should-not (assq 'blocked (ghostherd--match-all-rules ghostherd-test--claude-wrote-permission claude t)))
    (should (assq 'blocked (ghostherd--match-all-rules ghostherd-test--claude-question claude t))))
  (pcase-let ((`(,agy ,replaces) (ghostherd-test--rules-of 'agy)))
    (should replaces)
    ;; Its menu: a rule and "> " higher up are an old prompt, not the box.
    (should-not (ghostherd--input-at-foot-p ghostherd-test--agy-menu))
    (should (eq (car (ghostherd--match-rules ghostherd-test--agy-menu agy t)) 'blocked))
    (let ((wrote (concat "  I'd need your permission to push.\n" (make-string 82 ?─) "\n>\n" (make-string 82 ?─) "\n"
                         "? for shortcuts                       Gemini 3.8 Flash · high\n")))
      (should (eq (car (ghostherd--match-rules wrote agy t)) 'idle))))
  (pcase-let ((`(,grok ,replaces) (ghostherd-test--rules-of 'grok)))
    (should replaces)
    (should (ghostherd--input-at-foot-p ghostherd-test--grok-welcome-screen))
    ;; Nothing matched: idle, by the fallback.
    (should-not (ghostherd--match-rules (ghostherd-test--grok-at-rest "Approve it, then.") grok t))
    (should (eq (car (ghostherd--match-rules ghostherd-test--claude-wrote-permission grok t)) 'working))))

(ert-deftest ghostherd-test-input-box-not-a-menus-first-choice ()
  "A menu's first choice carries the prompt sign too, even under a rule:
it is not the input box; nor is a quoted line of what it asks."
  (should-not (ghostherd--input-at-foot-p (concat (make-string 82 ?─) "\n❯ 1. Yes\n  2. No\n")))
  ;; Nor a line of what a menu asks that quotes with the sign: the box
  ;; opens under a rule.
  (should-not (ghostherd--input-at-foot-p "Allow this edit?\n> src/app.js: replace 3 lines\n  1. Yes\n  2. No\nesc to cancel\n"))
  (should (ghostherd--input-at-foot-p (concat (make-string 82 ?─) "\n❯ fix the 1. step\n" (make-string 82 ?─) "\n"))))

(ert-deftest ghostherd-test-explain-says-the-box-shadows-blocked ()
  "Explained, a claude agent that wrote of a permission prompt: no
`blocked' winner, and why."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "claude-x" :kind 'claude :backend 'fake))
          (ghostherd-tests--fake-live t))
      (setq ghostherd-tests--fake-screen ghostherd-test--claude-wrote-permission)
      (save-window-excursion (ghostherd-explain s))
      (with-current-buffer "*ghostherd explain*"
        (should (string-search "->  esc to interrupt" (buffer-string)))
        (should-not (string-match-p "^  blocked" (buffer-string)))
        (should (string-search "what `blocked' matched there is the conversation's" (buffer-string))))
      (setq ghostherd-tests--fake-screen ghostherd-test--claude-question)
      (save-window-excursion (ghostherd-explain s))
      (with-current-buffer "*ghostherd explain*"
        (should (string-match-p "^  blocked  ->" (buffer-string)))
        (should-not (string-search "the conversation's, not a prompt" (buffer-string)))))))

(ert-deftest ghostherd-test-detect-state-reads-the-box ()
  "A claude agent that wrote of a permission prompt is working, as its
spinner says -- not blocked on what it wrote."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "claude-x" :kind 'claude :state 'working)))
      (cl-letf (((symbol-function 'ghostherd--session-live-p) (lambda (_) t))
                ((symbol-function 'ghostherd--host-exited-p) #'ignore))
        (should (eq (car (ghostherd--detect-state s ghostherd-test--claude-wrote-permission)) 'working))
        (should (eq (car (ghostherd--detect-state s ghostherd-test--claude-question)) 'blocked))))))

;;; A view scrolled back is not the agent's state (ask b77cf6dd)

(defconst ghostherd-tests--claude-scrolled-back
  "❯ [Image #69]
      3273 +    if (!box) return;
      3274 +    const pend = (mem.proposals || []).filter((p)
           +=> p.status === \"pending\");
      3275 +    if (!pend.length) { box.innerHTML = \"\"; return
           +; }
      3276 +    box.innerHTML = '<div class=\"mprops\"><h3 class
           +=\"mprops-h\">Proposals</h3>' +
      3277 +      pend.map(renderMemProposal).join(\"\") + \"</di
           +v>\";
      3278 +  }
      3279 +
      3280 +  function renderMemProposal(p) {
      3281 +    const facts = (p.facts || []).map((f) =>
      3282 +      '<span class=\"mpfact' + (f.gone ? \" gone\" :
           +\"\") + '\" title=\"' + esc(f.name) + '\">' +
      3283 +      esc(f.title || f.name) + \"</span>\").join(\"\")
           +;
      3284 +    const rejecting = mem.rejecting === p.id;
      3285 +    const acts = rejecting
      3286 +      ? '<div class=\"mpreject\"><input id=\"mem-prej
           +\" type=\"text\" placeholder=\"Why? (optional)\" value=
           +\"\">' +
      3287 +        '<button type=\"button\" class=\"ghost small
           +danger\" data-act=\"prop-reject-send\" data-id=\"' + e
           +sc(p.id) + '\">Reject</button>' +
      3288 +        '<button type=\"button\" class=\"ghost small\"
           + data-act=\"prop-reject-cancel\" data-id=\"' + esc(p.
           +id) + '\">Keep</button></div>'
      3289 +      : '<div class=\"mpacts\">' +
      3290 +        '<button type=\"button\" class=\"primary smal
           +l\" data-act=\"prop-approve\" data-id=\"' + esc(p.id)
           ++ '\">Approve</button>' +
      3291 +        '<button type=\"button\" class=\"ghost small\"
           + data-act=\"prop-reject\" data-id=\"' + esc(p.id) + '
           +\">Reject</button></div>';
      3292 +    return Jump to bottom (click) ↓
                             ✔ Update installed · Restart to update
─────────────────────────────────────────────────────────────────────
❯ 
─────────────────────────────────────────────────────────────────────
  ⏵⏵ auto mode on (shift+tab to cycle) · ⧉ herd-gate · esc to inte…

"
  "The screen ask 18f57d20 was closed with (2026-10-22 04:43:28Z), from the
lead's transcript: claude-.emacs.d 23 minutes into its turn, its view
scrolled back -- \"Jump to bottom (click) ↓\" over the view's last line,
the spinner out of it, the footer's `esc to interrupt' cut off.")

(defconst ghostherd-tests--agy-usage-panel
  (concat "  GEMINI 4 ARGON\n"
          "    Gemini 4 Argon Weekly Limit\n"
          "      [██████████████████████████████████████████████████] 100.00%\n"
          "    (1–29 of 38 lines)\n\n"
          "    ↑/↓ Scroll · pgup/pgdown Page · ctrl+end Bottom · ctrl+home Top · esc Close\n"
          "                                                             Gemini 3.8 Flash · high\n")
  "agy's `/usage' panel paged over its conversation, from the herd log
(entry 2519): no input box, the panel's own footer.")

(ert-deftest ghostherd-test-a-scrolled-back-claude-view-reads-as-it-was ()
  "The real screen reads idle by the rules -- the bug -- and is known for a
view scrolled back, which keeps the state the agent was in."
  (ghostherd-tests--with-herd ()
    (let* ((spec (alist-get 'claude ghostherd-agent-specs))
           (screen ghostherd-tests--claude-scrolled-back)
           (s (ghostherd-tests--session :name "c" :kind 'claude :backend 'fake :state 'working))
           (ghostherd-tests--fake-screen screen))
      ;; What closed the ask: the rules alone call it idle.
      (should (eq (car (ghostherd--match-rules screen (plist-get spec :screen-rules) t
                                               (plist-get spec :spinner)))
                  'idle))
      (should (ghostherd--scrolled-back-p screen (plist-get spec :scrolled)))
      (pcase-let ((`(,state . ,reason) (ghostherd--detect-state s)))
        (should (eq state 'working))
        (should (string-match-p "scrolled back" reason)))
      ;; Idle before, idle still: it tells nothing either way.
      (setf (ghostherd-session-state s) 'idle)
      (should (eq (car (ghostherd--detect-state s)) 'idle))
      ;; What the agent says of itself, while believed, still wins.
      (puthash "c" (list 'done "finished its turn" (current-time)) ghostherd--reports)
      (should (equal (ghostherd--detect-state s) '(done . "finished its turn"))))))

(ert-deftest ghostherd-test-a-watched-agent-scrolled-back-stays-working ()
  "The case itself: the user watching an agent mid-turn scrolls its view
back.  The poll keeps it working -- no `done, but you are watching', no
idle -- however long, so an ask it holds is not settled."
  (ghostherd-tests--with-herd ((ghostherd-idle-settle 0)
                               (ghostherd-watched-functions (list (lambda (_) t))))
    (let ((s (ghostherd-tests--session :name "c" :kind 'claude :backend 'fake :state 'working))
          (ghostherd-tests--fake-screen ghostherd-tests--claude-scrolled-back)
          (settled nil))
      (cl-letf (((symbol-function 'ghostherd--ask-settle) (lambda (&rest _) (setq settled t)))
                ((symbol-function 'ghostherd--maybe-notify-state) #'ignore))
        (dotimes (_ 3) (ghostherd-poll-session s))
        (should (eq (ghostherd-session-state s) 'working))
        (should-not settled)
        ;; Back at the bottom with the turn over: idle, as ever.
        (let ((ghostherd-tests--fake-screen
               (concat "⏺ Done.\n\n✻ Cooked for 23m 41s\n\n"
                       "────────────────────\n❯ \n────────────────────\n"
                       "  ⏵⏵ auto mode on (shift+tab to cycle)\n")))
          (ghostherd-poll-session s))
        (should (eq (ghostherd-session-state s) 'idle))))))

(ert-deftest ghostherd-test-the-scrolled-marker-every-way-claude-draws-it ()
  "However claude words its way back -- new messages, the key it names, the
scroll hint -- and only at the foot of the view: quoted higher up in the
conversation it is not the marker."
  (let* ((rx (plist-get (alist-get 'claude ghostherd-agent-specs) :scrolled))
         (box "                       ✔ Update installed\n────────\n❯ \n────────\n  ⏵⏵ auto mode on\n")
         (at-foot (lambda (line) (ghostherd--scrolled-back-p (concat "⏺ work\n  more\n" line "\n" box) rx))))
    (dolist (marker '("      return Jump to bottom (click) ↓"
                      "            3 new messages (click) ↓"
                      "            1 new message (ctrl+End) ↓"
                      "            Jump to bottom ↓"
                      "            Jump to bottom: fn+↓ to scroll"))
      (should (funcall at-foot marker)))
    (should-not (funcall at-foot "  I added the Jump to bottom check"))
    ;; Higher up than the view's foot: the conversation quoting it.
    (should-not (ghostherd--scrolled-back-p
                 (concat "⏺ it drew \"Jump to bottom (click) ↓\" over the view\n"
                         "  line\n  line\n  line\n  line\n  line\n" box)
                 rx))
    ;; A view at the bottom.
    (should-not (ghostherd--scrolled-back-p (concat "⏺ Done.\n" box) rx))))

(ert-deftest ghostherd-test-a-turn-end-report-settles-a-taker-already-stopped ()
  "Its screen read it stopped and the sidecar left its ask open; its
hook's turn end, when it comes, settles the ask -- though the state it
reports changes nothing.  A `working' report does not."
  (ghostherd-tests--with-asks
    (let ((taker (ghostherd-tests--session :name "rd" :kind 'claude :state 'done :backend 'fake))
          (ghostherd--reports (make-hash-table :test 'equal)))
      (setq ghostherd-tests--fake-screen "⏺ Done.\n")
      (puthash "a1" "rd" ghostherd--asks-open)
      (cl-letf (((symbol-function 'run-with-timer) #'ignore))
        (ghostherd--herd-take-reports '((:session "rd" :state "working")))
        (should-not requests)
        (ghostherd--herd-take-reports '((:session "rd" :state "done" :reason "finished its turn"))))
      (should (equal (mapcar #'car requests) '("herd_settle")))
      (should (equal (plist-get (cadr (car requests)) :id) "a1"))
      (should-not (gethash "a1" ghostherd--asks-open))
      (ignore taker))))

(ert-deftest ghostherd-test-agys-panel-keeps-its-state ()
  "agy's `/usage' panel, paged over its conversation with no input box:
by the rules alone, idle; known for a panel, the state it was in."
  (ghostherd-tests--with-herd ()
    (let* ((spec (alist-get 'agy ghostherd-agent-specs))
           (s (ghostherd-tests--session :name "a" :kind 'agy :backend 'fake :state 'working))
           (ghostherd-tests--fake-screen ghostherd-tests--agy-usage-panel))
      (should (ghostherd--scrolled-back-p ghostherd-tests--agy-usage-panel (plist-get spec :scrolled)))
      (should (eq (car (ghostherd--detect-state s)) 'working)))))

(ert-deftest ghostherd-test-an-ask-left-open-on-a-turn-still-on-says-so ()
  "The sidecar leaves the ask open -- the taker's hook has not said its
turn ended -- and Emacs watches it again, saying why."
  (ghostherd-tests--with-asks
    (let ((taker (ghostherd-tests--session :name "rd" :kind 'claude :state 'working :backend 'fake)))
      (setq ghostherd-tests--fake-screen ghostherd-tests--claude-scrolled-back)
      (puthash "a1" "rd" ghostherd--asks-open)
      (cl-letf (((symbol-function 'ghostherd-memory-request-async)
                 (lambda (method cb &optional params _err)
                   (setq requests (append requests (list (list method params))))
                   (funcall cb '(:id "a1" :status "delivered" :turn_on t)))))
        (ghostherd--set-state taker 'idle))
      (should (equal (mapcar #'car requests) '("herd_settle")))
      (should (equal (gethash "a1" ghostherd--asks-open) "rd"))
      (should (string-match-p "a1 left open: rd's turn has not ended by its hook or transcript"
                              (ghostherd-log-entry-text (car ghostherd--log))))
      (should-not (ghostherd-tests--issue-p "a1 left open")))))

(provide 'ghostherd-screen-tests)
;;; ghostherd-screen-tests.el ends here

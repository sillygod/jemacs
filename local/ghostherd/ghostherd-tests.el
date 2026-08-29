;;; ghostherd-tests.el --- Tests for ghostherd  -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for ghostherd's pure helpers -- the parts that need no PTY.
;; The interactive spawn/kill paths still need the manual plan in
;; docs/testing.org; driving real ghostel terminals headlessly is what
;; Phase 1 deferred.
;;
;; Run:
;;
;;   emacs --batch --init-directory=~/.emacs.d/emacs-home/ \
;;         -L . -l ghostherd.el -l ghostherd-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'ghostherd)
;; For `server-name', which `ghostherd-agent-environment' reads and one
;; test binds -- ghostherd only `defvar's it, so without this a `let'
;; around it is lexical and the function still sees it unbound.
(require 'server)

;;; Fixtures

;; ghostherd only `defvar's these so it can compile without ghostel loaded,
;; which leaves them unbound; `buffer-local-value' on an unbound symbol
;; errors, so give them defaults the way a loaded ghostel would.
(defvar-local ghostel--title nil)
(defvar-local ghostel--process nil)
(defvar-local ghostel--input-mode nil)
(defvar ghostel-progress-function nil)
(defvar evil-local-mode nil)

(defmacro ghostherd-tests--with-herd (bindings &rest body)
  "Run BODY with an empty session registry.
BINDINGS are extra `let' bindings evaluated inside the clean registry."
  (declare (indent 1) (debug t))
  `(let ((ghostherd--sessions (make-hash-table :test 'equal))
         ;; Keyed by session id and global, so without this a test that
         ;; sends input to "a" leaves the next test's "a" inside the
         ;; input grace -- which is exactly how this line got written.
         (ghostherd--input-at (make-hash-table :test 'equal))
         (ghostherd--idle-since (make-hash-table :test 'equal))
         (ghostherd--reports (make-hash-table :test 'equal))
         (ghostherd--screens (make-hash-table :test 'equal))
         ;; Every logged transition now appends to a file, and the default
         ;; is the user's real one.  Persistence gets tested deliberately,
         ;; with a temporary file, and nowhere else.
         (ghostherd-log-file nil)
         (ghostherd--log-writable t)
         (ghostherd--log-loaded t)
         (ghostherd--sidebar-query "")
         (ghostherd--sidebar-filtering nil)
         (ghostherd--sidebar-help-visible nil)
         ,@bindings)
     ,@body))

(defun ghostherd-tests--session (&rest args)
  "Register and return a session built from ARGS, with a linked buffer."
  (let* ((name (or (plist-get args :name) "agent"))
         (id (or (plist-get args :id) name))
         (buf (generate-new-buffer (format "*ghostherd:%s*" name)))
         (session (apply #'ghostherd-session--create
                         :id id :buffer buf
                         (append args (list :name name)))))
    (with-current-buffer buf
      (setq-local ghostherd-session-id id))
    (puthash id session ghostherd--sessions)
    session))

;;; A backend that answers with whatever a test wants

(defvar ghostherd-tests--fake-screen ""
  "Screen the `fake' backend reports.")

(defvar ghostherd-tests--fake-live t
  "What the `fake' backend says about liveness.")

(defvar ghostherd-tests--fake-recipes nil
  "Recipes the `fake' backend offers `ghostherd-restore'.")

(cl-defmethod ghostherd-backend-capture
  ((_backend (eql fake)) _session &optional _n)
  ghostherd-tests--fake-screen)

(cl-defmethod ghostherd-backend-live-p ((_backend (eql fake)) _session)
  ghostherd-tests--fake-live)

(cl-defmethod ghostherd-backend-list ((_backend (eql fake)))
  ghostherd-tests--fake-recipes)

(defmacro ghostherd-tests--recording-keys (&rest body)
  "Run BODY with ghostel's send functions recorded into `sent'.
`sent' collects (KEY-NAME . MODS) for keys and (:text . STRING) for text."
  (declare (indent 0) (debug t))
  `(let ((sent nil))
     (cl-letf (((symbol-function 'ghostel-send-key)
                (lambda (name &optional mods) (push (cons name mods) sent)))
               ((symbol-function 'ghostel-paste-string)
                (lambda (text) (push (cons :text text) sent)))
               ((symbol-function 'derived-mode-p) (lambda (&rest _) t)))
       ,@body
       (nreverse sent))))

(defvar ghostherd-tests--tmux-calls nil
  "Argument lists handed to tmux by the code under test.")

(defun ghostherd-tests--tmux-panes ()
  "Fake `list-panes' output showing every registered tmux session alive."
  (mapconcat (lambda (s)
               (format "%s\t0\t\n" (ghostherd-session-host-id s)))
             (cl-remove-if-not
              (lambda (s) (eq (ghostherd-session-backend s) 'tmux))
              (hash-table-values ghostherd--sessions))
             ""))

(defmacro ghostherd-tests--with-tmux (responses &rest body)
  "Run BODY with tmux stubbed, recording calls in `ghostherd-tests--tmux-calls'.

RESPONSES maps a tmux subcommand to the (STATUS . OUTPUT) it answers
with.  `list-panes' defaults to reporting every registered tmux session
alive, since almost everything checks liveness on the way past;
anything else unlisted succeeds silently.

The recording is not `let'-bound, so assertions after the form still
see the calls."
  (declare (indent 1) (debug t))
  `(progn
     (setq ghostherd-tests--tmux-calls nil)
     (let ((ghostherd-tmux--status-cache nil))
       (cl-letf (((symbol-function 'ghostherd-tmux--call)
                  (lambda (args)
                    (push args ghostherd-tests--tmux-calls)
                    (or (alist-get (car args) ,responses nil nil #'equal)
                        (if (equal (car args) "list-panes")
                            (cons 0 (ghostherd-tests--tmux-panes))
                          (cons 0 ""))))))
         ,@body))
     (setq ghostherd-tests--tmux-calls
           (nreverse ghostherd-tests--tmux-calls))))

(defun ghostherd-tests--tmux-call (subcommand)
  "Return the recorded tmux call for SUBCOMMAND, or nil."
  (cl-find subcommand ghostherd-tests--tmux-calls
           :key #'car :test #'equal))

(defun ghostherd-tests--tmux-options ()
  "Return the (OPTION . VALUE) pairs written by recorded `set-option' calls."
  (mapcar (lambda (call) (cons (nth 3 call) (nth 4 call)))
          (cl-remove-if-not (lambda (call) (equal (car call) "set-option"))
                            ghostherd-tests--tmux-calls)))

(defun ghostherd-tests--tmux-session (&rest args)
  "Register a tmux-backed session from ARGS."
  (apply #'ghostherd-tests--session
         :backend 'tmux :host-id "gh-abc-rev" args))

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

(defconst ghostherd-tests--permission-screen
  (concat "╭────────────────────────────────────────────╮\n"
          "│ Bash(rm -rf build/ && npm publish)         │\n"
          "│                                            │\n"
          "│ Do you want to proceed?                    │\n"
          "│ ❯ 1. Yes                                   │\n"
          "╰────────────────────────────────────────────╯\n")
  "A permission prompt as an agent actually draws one.")

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

(ert-deftest ghostherd-test-reason-cannot-change-state ()
  "It is a display heuristic and is allowed to be one, on the condition
that it never reaches the state machine."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy :backend 'fake))
          (ghostherd-tests--fake-screen ghostherd-tests--permission-screen))
      (dolist (context '(0 3 50))
        (let ((ghostherd-reason-context context))
          (should (eq (car (ghostherd--detect-state s)) 'blocked)))))))

(ert-deftest ghostherd-test-wheel-scrolls-the-host-view ()
  "The buffer behind tmux holds one screen, so scrolling it in Emacs
moves nothing.  The wheel has to drive the host's own view, in one
invocation, since this runs per click."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--tmux-session :name "a")))
      (with-current-buffer (ghostherd-session-buffer s)
        (ghostherd-tests--with-tmux nil
          (let ((ghostherd-scroll-lines 3))
            (ghostherd-scroll-up))))
      (let ((call (ghostherd-tests--tmux-call "copy-mode")))
        ;; -e is what keeps it invisible: reaching the bottom leaves copy
        ;; mode on its own, so there is no mode to get stuck in
        (should (member "-e" call))
        (should (member "scroll-up" call))
        (should (member "3" call))
        ;; one invocation, not two
        (should (= (length ghostherd-tests--tmux-calls) 1))))))

(ert-deftest ghostherd-test-scrolling-down-goes-the-other-way ()
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--tmux-session :name "a")))
      (with-current-buffer (ghostherd-session-buffer s)
        (ghostherd-tests--with-tmux nil (ghostherd-scroll-down)))
      (should (member "scroll-down" (ghostherd-tests--tmux-call "copy-mode"))))))

(ert-deftest ghostherd-test-a-host-that-cannot-scroll-lets-emacs-do-it ()
  "The ghostel backend keeps its output in the buffer, where Emacs has
always been able to scroll it -- so the same keys must fall through
rather than erroring."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a"))
          (scrolled nil))
      (with-current-buffer (ghostherd-session-buffer s)
        (cl-letf (((symbol-function 'scroll-down)
                   (lambda (&optional n) (setq scrolled n))))
          (let ((ghostherd-scroll-lines 3))
            (ghostherd-scroll-up))))
      (should (equal scrolled 3)))))

(ert-deftest ghostherd-test-copy-mode-is-on-c-z ()
  (should (eq (lookup-key ghostherd-terminal-mode-map (kbd "C-z"))
              #'ghostherd-copy-mode)))

(ert-deftest ghostherd-test-bind-evil-copy-mode-without-evil ()
  "A no-op when `evil-define-key*' is missing, never a macro call."
  (let ((saved (and (fboundp 'evil-define-key*)
                    (symbol-function 'evil-define-key*))))
    (when saved
      (fmakunbound 'evil-define-key*))
    (unwind-protect
        (ghostherd--bind-evil-copy-mode)
      (when saved
        (fset 'evil-define-key* saved)))))

(ert-deftest ghostherd-test-copy-mode-syncs-evil ()
  "Copy mode with Evil emacs state makes `?' a self-insert, and
`ghostel-readonly-fast-exit' then leaves copy mode on the first
search key."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a"))
          (state nil))
      (with-current-buffer (ghostherd-session-buffer s)
        (setq-local ghostel--input-mode 'copy)
        (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
                  ((symbol-function 'evil-normal-state)
                   (lambda (&rest _) (setq state 'normal)))
                  ((symbol-function 'evil-emacs-state)
                   (lambda (&rest _) (setq state 'emacs)))
                  (evil-local-mode t))
          (ghostherd--copy-mode-sync-evil)
          (should (eq state 'normal))
          (setq-local ghostel--input-mode 'semi-char)
          (ghostherd--copy-mode-sync-evil)
          (should (eq state 'emacs)))))))

(ert-deftest ghostherd-test-copy-mode-scrolls-the-frozen-buffer ()
  "While copy mode holds the view, the wheel must not drive tmux --
that would move a host the frozen client no longer shows."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--tmux-session :name "a"))
          (scrolled nil))
      (with-current-buffer (ghostherd-session-buffer s)
        (setq-local ghostel--input-mode 'copy)
        (cl-letf (((symbol-function 'scroll-down)
                   (lambda (&optional n) (setq scrolled n))))
          (ghostherd-tests--with-tmux nil
            (let ((ghostherd-scroll-lines 3))
              (ghostherd-scroll-up)))))
      (should (equal scrolled 3))
      (should-not (ghostherd-tests--tmux-call "copy-mode")))))

;;; The view has to be the size of the window

(cl-defmethod ghostherd-backend-view ((_backend (eql fake)) session)
  (ghostherd-session-buffer session))

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

;;; Herd log

(defmacro ghostherd-tests--with-log (&rest body)
  "Run BODY with an empty herd log."
  (declare (indent 0) (debug t))
  `(let ((ghostherd--log nil)
         (ghostherd-log-max 500)
         (ghostherd-log-screens 20))
     (cl-letf (((symbol-function 'ghostherd--notify) (lambda (&rest _) nil)))
       ,@body)))

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

;;; Sidebar column fitting

(defun ghostherd-tests--columns-width (columns)
  "Characters `tabulated-list' needs to draw COLUMNS."
  (+ tabulated-list-padding
     (apply #'+ (mapcar (lambda (c) (nth 2 c)) columns))
     (1- (length columns))))

(ert-deftest ghostherd-test-sidebar-columns-always-fit ()
  "The regression this replaced: fixed widths totalling ~70 against a
default sidebar width of 36, so every row wrapped."
  (let ((tabulated-list-padding 1))
    (dolist (show '(nil t))
      (dolist (width '(20 24 30 36 50 70 100 200))
        (let* ((ghostherd-sidebar-width width)
               (ghostherd-sidebar-show-title show)
               (columns (ghostherd--sidebar-visible-columns)))
          (should columns)
          (should (<= (ghostherd-tests--columns-width columns) width)))))))

(ert-deftest ghostherd-test-sidebar-columns-keep-mandatory ()
  "Below the width that fits a name there is nothing sensible to drop to,
so glyph and name are kept and allowed to overflow -- an empty row would
be worse than a clipped one."
  (let ((tabulated-list-padding 1))
    (dolist (width '(1 5 10 18))
      (let ((ghostherd-sidebar-width width))
        (should (equal (mapcar #'car (ghostherd--sidebar-visible-columns))
                       '(glyph name)))))))

(ert-deftest ghostherd-test-sidebar-columns-drop-optional-when-tight ()
  "Optional columns must actually be dropped, not merely reordered."
  (let ((tabulated-list-padding 1)
        (ghostherd-sidebar-width 22))
    (should (equal (mapcar #'car (ghostherd--sidebar-visible-columns))
                   '(glyph name)))))

(ert-deftest ghostherd-test-sidebar-columns-grow-with-width ()
  (let ((tabulated-list-padding 1))
    (let ((narrow (let ((ghostherd-sidebar-width 36))
                    (mapcar #'car (ghostherd--sidebar-visible-columns))))
          (wide (let ((ghostherd-sidebar-width 100))
                  (mapcar #'car (ghostherd--sidebar-visible-columns)))))
      (should (< (length narrow) (length wide)))
      ;; project is the widest and least informative, so it goes last
      (should-not (memq 'project narrow))
      (should (memq 'project wide)))))

(ert-deftest ghostherd-test-sidebar-title-only-when-enabled ()
  (let ((tabulated-list-padding 1)
        (ghostherd-sidebar-width 100))
    (should-not (memq 'title (let ((ghostherd-sidebar-show-title nil))
                               (mapcar #'car (ghostherd--sidebar-visible-columns)))))
    (should (memq 'title (let ((ghostherd-sidebar-show-title t))
                           (mapcar #'car (ghostherd--sidebar-visible-columns)))))))

(ert-deftest ghostherd-test-sidebar-header-and-cells-agree ()
  "Header and cells come from one column list precisely so they cannot
drift; check that they really do line up at both extremes."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--session :name "a" :kind 'agy :state 'idle)
    (dolist (width '(36 100))
      (dolist (show '(nil t))
        (let ((ghostherd-sidebar-width width)
              (ghostherd-sidebar-show-title show))
          (with-temp-buffer
            (ghostherd-sidebar-mode)
            (ghostherd--sidebar-build-entries)
            (should (= (length tabulated-list-format)
                       (length (cadr (car tabulated-list-entries)))))))))))

(ert-deftest ghostherd-test-sidebar-marks-detached-rows ()
  "A herd running headless must not look identical to one you are
watching.  The mark rides in the spare character of the glyph column,
because at the default sidebar width a column of its own would be
fitted away exactly where it is needed."
  (ghostherd-tests--with-herd ()
    (let* ((s (ghostherd-tests--session :name "a" :kind 'agy :backend 'tmux))
           (attached (ghostherd--sidebar-cell s 'glyph 'idle 'default)))
      (should (equal attached (ghostherd--state-glyph 'idle)))
      (setf (ghostherd-session-buffer s) nil)
      (let ((detached (ghostherd--sidebar-cell s 'glyph 'idle 'default)))
        (should-not (equal detached attached))
        (should (string-prefix-p attached detached))
        ;; It has to fit the column it borrows, or it pushes the row out.
        (should (<= (string-width detached)
                    (nth 2 (assq 'glyph ghostherd--sidebar-column-specs))))))))

(ert-deftest ghostherd-test-sidebar-mark-is-silent-on-ghostel ()
  "On the ghostel backend a registered session always has its buffer, so
the mark must never appear there -- the concept does not exist."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy)))
      (should (equal (ghostherd--sidebar-cell s 'glyph 'idle 'default)
                     (ghostherd--state-glyph 'idle))))))

;;; Session list display (posframe overlay vs side window)

(ert-deftest ghostherd-test-posframe-off-when-disabled ()
  "Nil forces the side window even when posframe could display."
  (cl-letf (((symbol-function 'ghostherd--posframe-available-p)
             (lambda () t)))
    (let ((ghostherd-sidebar-use-posframe nil))
      (should-not (ghostherd--use-posframe-p)))))

(ert-deftest ghostherd-test-posframe-auto-needs-a-working-display ()
  "Batch and tty cannot host a child frame, so the overlay must fall back."
  (let ((ghostherd-sidebar-use-posframe t))
    (cl-letf (((symbol-function 'ghostherd--posframe-available-p)
               (lambda () nil)))
      (should-not (ghostherd--use-posframe-p)))
    (cl-letf (((symbol-function 'ghostherd--posframe-available-p)
               (lambda () t)))
      (should (ghostherd--use-posframe-p)))))

(ert-deftest ghostherd-test-sidebar-display-chooses-posframe-when-available ()
  (ghostherd-tests--with-herd ()
    (let ((shown nil)
          (ghostherd-sidebar-use-posframe t))
      (cl-letf (((symbol-function 'ghostherd--posframe-available-p)
                 (lambda () t))
                ((symbol-function 'ghostherd--sidebar-show-posframe)
                 (lambda (buf) (setq shown buf)))
                ((symbol-function 'ghostherd--sidebar-show-side-window)
                 (lambda (_) (error "should not show side window")))
                ((symbol-function 'ghostherd--sidebar-prepare-buffer)
                 (lambda () 'buf)))
        (ghostherd-sidebar)
        (should (eq shown 'buf))))))

(ert-deftest ghostherd-test-sidebar-display-falls-back-without-posframe ()
  "The original side window is what you get when posframe cannot display."
  (ghostherd-tests--with-herd ()
    (let ((shown nil)
          (ghostherd-sidebar-use-posframe t))
      (cl-letf (((symbol-function 'ghostherd--posframe-available-p)
                 (lambda () nil))
                ((symbol-function 'ghostherd--sidebar-show-posframe)
                 (lambda (_) (error "should not show posframe")))
                ((symbol-function 'ghostherd--sidebar-show-side-window)
                 (lambda (buf) (setq shown buf)))
                ((symbol-function 'ghostherd--sidebar-prepare-buffer)
                 (lambda () 'buf)))
        (ghostherd-sidebar)
        (should (eq shown 'buf))))))

(ert-deftest ghostherd-test-leave-overlay-is-idle-without-posframe ()
  (cl-letf (((symbol-function 'ghostherd--sidebar-hide-posframe)
             (lambda () (error "should not hide"))))
    (ghostherd--sidebar-leave-overlay)))

(ert-deftest ghostherd-test-visit-dismisses-posframe-overlay ()
  "pop-to-buffer from a dedicated unsplittable child frame has nowhere
to put the agent.  Visit must dismiss first."
  (ghostherd-tests--with-herd ()
    (let* ((s (ghostherd-tests--session :name "a" :kind 'agy))
           (left nil))
      (cl-letf (((symbol-function 'ghostherd--sidebar-posframe-showing-p)
                 (lambda () t))
                ((symbol-function 'ghostherd--sidebar-hide-posframe)
                 (lambda () (setq left t)))
                ((symbol-function 'ghostherd--host-view)
                 (lambda (_) (ghostherd-session-buffer s)))
                ((symbol-function 'pop-to-buffer) (lambda (&rest _) nil))
                ((symbol-function 'ghostherd--sync-view-size) (lambda (_) nil)))
        (ghostherd-visit s)
        (should left)))))

(ert-deftest ghostherd-test-sidebar-columns-use-posframe-budget ()
  "The overlay is wider than the side window, so it should keep columns
the 36-column dashboard drops -- otherwise the extra width is wasted."
  (let ((tabulated-list-padding 1))
    (let ((side (let ((ghostherd-sidebar-width 36)
                      (ghostherd--sidebar-target-width nil))
                  (mapcar #'car (ghostherd--sidebar-visible-columns))))
          (overlay (let ((ghostherd--sidebar-target-width 72))
                     (mapcar #'car (ghostherd--sidebar-visible-columns)))))
      (should (memq 'kind overlay))
      (should-not (memq 'kind side)))))

(ert-deftest ghostherd-test-sidebar-grow-uses-leftover-width ()
  "A 72-column overlay that still clips Project at 20 wasted the width."
  (let ((tabulated-list-padding 1)
        (ghostherd--sidebar-target-width 72)
        (ghostherd-sidebar-show-title nil))
    (let ((cols (ghostherd--sidebar-visible-columns)))
      (should (eq (caar (last cols)) 'project))
      (should (> (nth 2 (car (last cols))) 20))
      (should (<= (ghostherd-tests--columns-width cols) 72)))))

(ert-deftest ghostherd-test-sidebar-grow-still-fits ()
  (let ((tabulated-list-padding 1)
        (ghostherd-sidebar-show-title nil))
    (dolist (width '(36 72 100 200))
      (let* ((ghostherd--sidebar-target-width width)
             (cols (ghostherd--sidebar-visible-columns)))
        (should (<= (ghostherd-tests--columns-width cols) width))))))

(ert-deftest ghostherd-test-posframe-width-scales-with-frame ()
  (let ((ghostherd-sidebar-posframe-width 72)
        (ghostherd-sidebar-posframe-width-ratio 0.6))
    (cl-letf (((symbol-function 'ghostherd--sidebar-posframe-parent-frame)
               (lambda () 'parent))
              ((symbol-function 'frame-width)
               (lambda (&optional _) 80)))
      (let ((narrow (ghostherd--sidebar-posframe-char-width)))
        (should (<= narrow 80))
        (should (>= narrow 48))))
    (cl-letf (((symbol-function 'ghostherd--sidebar-posframe-parent-frame)
               (lambda () 'parent))
              ((symbol-function 'frame-width)
               (lambda (&optional _) 220)))
      (should (equal (ghostherd--sidebar-posframe-char-width)
                     (round (* 0.6 220)))))))

(ert-deftest ghostherd-test-posframe-width-can-be-fixed ()
  "A nil ratio is the old stamp: 72 columns no matter how wide Emacs is."
  (let ((ghostherd-sidebar-posframe-width 72)
        (ghostherd-sidebar-posframe-width-ratio nil))
    (cl-letf (((symbol-function 'ghostherd--sidebar-posframe-parent-frame)
               (lambda () 'parent))
              ((symbol-function 'frame-width)
               (lambda (&optional _) 220)))
      (should (equal (ghostherd--sidebar-posframe-char-width) 72)))))

(ert-deftest ghostherd-test-posframe-width-never-exceeds-frame ()
  (let ((ghostherd-sidebar-posframe-width 72)
        (ghostherd-sidebar-posframe-width-ratio 0.9))
    (cl-letf (((symbol-function 'ghostherd--sidebar-posframe-parent-frame)
               (lambda () 'parent))
              ((symbol-function 'frame-width)
               (lambda (&optional _) 50)))
      (should (<= (ghostherd--sidebar-posframe-char-width) 50)))))

;;; Live-narrow query

(ert-deftest ghostherd-test-flex-matches-subsequence ()
  (should (ghostherd--flex-match-p "agc" "agy-commit"))
  (should (ghostherd--flex-match-p "AGY" "agy-commit"))
  (should (ghostherd--flex-match-p "emacs.d" "/Users/jing/.emacs.d/local/ghostherd"))
  (should-not (ghostherd--flex-match-p "xyz" "agy-commit"))
  (should-not (ghostherd--flex-match-p "commitx" "agy-commit")))

(ert-deftest ghostherd-test-sidebar-query-matches-fields ()
  "Name, kind, state, project and notes are all searchable; tokens AND."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session
              :name "agy-commit" :kind 'agy :state 'idle
              :project "/Users/jing/.emacs.d/local/ghostherd"
              :notes "reviews auth")))
      (should (ghostherd--sidebar-query-matches-p s "agy"))
      (should (ghostherd--sidebar-query-matches-p s "idle"))
      (should (ghostherd--sidebar-query-matches-p s "ghostherd"))
      (should (ghostherd--sidebar-query-matches-p s "auth"))
      (should (ghostherd--sidebar-query-matches-p s "agy idle"))
      (should-not (ghostherd--sidebar-query-matches-p s "agy blocked"))
      (should (ghostherd--sidebar-query-matches-p s ""))
      (should (ghostherd--sidebar-query-matches-p s "   ")))))

(ert-deftest ghostherd-test-sidebar-query-narrows-rows ()
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--session :name "agy-commit" :kind 'agy :id "agy-commit")
    (ghostherd-tests--session :name "grok-dev" :kind 'grok :id "grok-dev")
    (let ((ghostherd--sidebar-query "agy")
          (tabulated-list-padding 1)
          (ghostherd-sidebar-width 72)
          (ghostherd--sidebar-target-width 72))
      (with-temp-buffer
        (ghostherd-sidebar-mode)
        (ghostherd--sidebar-build-entries)
        (should (equal (mapcar #'car tabulated-list-entries) '("agy-commit")))
        (should (= ghostherd--sidebar-match-count 1))
        (should (= ghostherd--sidebar-total-count 2))))))

(ert-deftest ghostherd-test-sidebar-query-ands-project-filter ()
  "The live query runs on whatever the project filter already kept,
not on the whole herd -- otherwise `/agy` would bring the other
project's agy back."
  (ghostherd-tests--with-herd ()
    (let* ((a (ghostherd-tests--session :name "agy-a" :kind 'agy :id "agy-a"))
           (b (ghostherd-tests--session :name "agy-b" :kind 'agy :id "agy-b"))
           (ghostherd--sidebar-filter-project "/ignored/")
           (ghostherd--sidebar-query "agy")
           (tabulated-list-padding 1)
           (ghostherd-sidebar-width 72)
           (ghostherd--sidebar-target-width 72))
      (cl-letf (((symbol-function 'ghostherd-sessions)
                 (lambda (&optional project)
                   (if project (list a) (list a b)))))
        (with-temp-buffer
          (ghostherd-sidebar-mode)
          (ghostherd--sidebar-build-entries)
          (should (equal (mapcar #'car tabulated-list-entries) '("agy-a")))
          (should (= ghostherd--sidebar-total-count 1))
          (should (= ghostherd--sidebar-match-count 1)))))))

(ert-deftest ghostherd-test-sidebar-quit-clears-query-first ()
  "Esc with a query is still using the list; only empty-Esc dismisses."
  (let ((ghostherd--sidebar-query "agy")
        (hidden nil))
    (cl-letf (((symbol-function 'ghostherd--sidebar-hide-posframe)
               (lambda () (setq hidden t)))
              ((symbol-function 'ghostherd--sidebar-posframe-showing-p)
               (lambda () t)))
      (ghostherd-sidebar-quit)
      (should (equal ghostherd--sidebar-query ""))
      (should-not hidden)
      (ghostherd-sidebar-quit)
      (should hidden))))

;;; Overlay screen preview

(ert-deftest ghostherd-test-detect-stashes-the-screen ()
  "The overlay preview must not recapture what the poll already paid for."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy :backend 'fake))
          (ghostherd-tests--fake-screen "hello from the pane\n> \n"))
      (ghostherd--detect-state s)
      (should (string-match-p "hello from the pane"
                              (gethash "a" ghostherd--screens))))))

(ert-deftest ghostherd-test-kill-drops-the-screen ()
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy)))
      (puthash "a" "cached\n" ghostherd--screens)
      (cl-letf (((symbol-function 'ghostherd--host-kill) #'ignore)
                ((symbol-function 'ghostherd--notify) #'ignore))
        (ghostherd-kill s t))
      (should-not (gethash "a" ghostherd--screens)))))

(ert-deftest ghostherd-test-rename-carries-the-screen ()
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy)))
      (puthash "a" "cached\n" ghostherd--screens)
      (cl-letf (((symbol-function 'ghostherd--host-rename) #'ignore))
        (ghostherd-rename s "b"))
      (should-not (gethash "a" ghostherd--screens))
      (should (equal (gethash "b" ghostherd--screens) "cached\n")))))

(ert-deftest ghostherd-test-sidebar-preview-is-the-tail ()
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--session :name "a" :kind 'agy :state 'idle)
    (puthash "a" "one\ntwo\nthree\nfour\n" ghostherd--screens)
    (let ((ghostherd-sidebar-show-preview t)
          (ghostherd-sidebar-preview-lines 2)
          (tabulated-list-padding 1)
          (ghostherd-sidebar-width 72)
          (ghostherd--sidebar-target-width 72))
      (with-temp-buffer
        (ghostherd-sidebar-mode)
        (ghostherd--sidebar-build-entries)
        (ghostherd--sidebar-print)
        (should ghostherd--sidebar-preview-start)
        (let ((preview (buffer-substring-no-properties
                        ghostherd--sidebar-preview-start (point-max))))
          (should (string-match-p "three" preview))
          (should (string-match-p "four" preview))
          (should-not (string-match-p "one" preview)))
        (should (equal (tabulated-list-get-id) "a"))
        (should (< (point) ghostherd--sidebar-preview-start))))))

(ert-deftest ghostherd-test-sidebar-help-keys-are-local ()
  "Leader keys are not overlay keys.  Listing them made `?' look like
a global cheat-sheet, and opening that cheat-sheet killed the overlay."
  (let ((keys (ghostherd--sidebar-help-keys 'ghostherd-new)))
    (should (member "N" keys))
    (should-not (cl-find "SPC" keys :test #'string-prefix-p))))

(ert-deftest ghostherd-test-sidebar-help-stays-in-the-overlay ()
  (let ((left nil)
        (ghostherd--sidebar-help-visible nil))
    (cl-letf (((symbol-function 'ghostherd--sidebar-leave-overlay)
               (lambda () (setq left t)))
              ((symbol-function 'ghostherd--sidebar-draw-preview) #'ignore)
              ((symbol-function 'ghostherd--sidebar-posframe-showing-p)
               (lambda () nil)))
      (ghostherd-sidebar-help)
      (should ghostherd--sidebar-help-visible)
      (should-not left)
      (ghostherd-sidebar-help)
      (should-not ghostherd--sidebar-help-visible))))

(ert-deftest ghostherd-test-sidebar-help-text-omits-leader-keys ()
  (let ((text (ghostherd--sidebar-help-text)))
    (should (string-match-p "Visit" text))
    (should (string-match-p (regexp-quote (ghostherd--state-glyph 'blocked))
                            text))
    (should-not (string-match-p "SPC a h" text))))

(ert-deftest ghostherd-test-sidebar-quit-closes-help-before-overlay ()
  "Esc peels the legend first, the way it peels a live query."
  (let ((hidden nil)
        (ghostherd--sidebar-help-visible t)
        (ghostherd--sidebar-query ""))
    (cl-letf (((symbol-function 'ghostherd--sidebar-hide-posframe)
               (lambda () (setq hidden t)))
              ((symbol-function 'ghostherd--sidebar-posframe-showing-p)
               (lambda () t))
              ((symbol-function 'ghostherd--sidebar-draw-preview) #'ignore)
              ((symbol-function 'ghostherd--sidebar-show-posframe) #'ignore))
      (ghostherd-sidebar-quit)
      (should-not ghostherd--sidebar-help-visible)
      (should-not hidden)
      (ghostherd-sidebar-quit)
      (should hidden))))

(ert-deftest ghostherd-test-sidebar-message-submits-as-keyword ()
  "The overlay is not an agent, and SUBMIT is a keyword.

A bare fourth argument `t' is `Keyword argument t not one of
(:submit)' -- overlay `m' failed that way while `SPC a h m' did not."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "rev" :kind 'agy))
          (got nil))
      (cl-letf (((symbol-function 'ghostherd-message)
                 (lambda (from to body &rest keys)
                   (setq got (list from to body keys))))
                ((symbol-function 'read-string) (lambda (&rest _) "hello"))
                ((symbol-function 'ghostherd--sidebar-session-at-point)
                 (lambda () s)))
        (ghostherd-sidebar-message)
        (should (equal (nth 0 got) "user"))
        (should (eq (nth 1 got) s))
        (should (equal (nth 2 got) "hello"))
        (should (eq (plist-get (nth 3 got) :submit) t))))))

(ert-deftest ghostherd-test-sidebar-preview-can-be-off ()
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--session :name "a" :kind 'agy :state 'idle)
    (puthash "a" "secret-tail\n" ghostherd--screens)
    (let ((ghostherd-sidebar-show-preview nil)
          (tabulated-list-padding 1)
          (ghostherd-sidebar-width 72)
          (ghostherd--sidebar-target-width 72))
      (with-temp-buffer
        (ghostherd-sidebar-mode)
        (ghostherd--sidebar-build-entries)
        (ghostherd--sidebar-print)
        (should-not (string-match-p "secret-tail" (buffer-string)))
        (should-not ghostherd--sidebar-preview-start)))))

;;; Terminal title

(ert-deftest ghostherd-test-session-title-filters-noise ()
  "A title equal to the buffer name is the shell echoing us back."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a")))
      (with-current-buffer (ghostherd-session-buffer s)
        (dolist (noise (list nil "" "   "))
          (setq-local ghostel--title noise)
          (should-not (ghostherd--session-title s)))
        (setq-local ghostel--title (buffer-name))
        (should-not (ghostherd--session-title s))
        (setq-local ghostel--title "  refactor auth  ")
        (should (equal (ghostherd--session-title s) "refactor auth"))))))

(ert-deftest ghostherd-test-session-title-dead-buffer ()
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a")))
      (kill-buffer (ghostherd-session-buffer s))
      (should-not (ghostherd--session-title s)))))

;;; Respawn recipe

(ert-deftest ghostherd-test-recipe-round-trips-identity ()
  "The recipe is everything that can be persisted; losing a field here
means a respawned agent quietly comes back as something else."
  (ghostherd-tests--with-herd ()
    (let* ((s (ghostherd-tests--session
               :name "reviewer" :kind 'grok :project "/tmp/proj/"
               :command "grok" :args '("--model" "x") :notes "review only"))
           (recipe (ghostherd-session-recipe s)))
      (should (equal (plist-get recipe :name) "reviewer"))
      (should (equal (plist-get recipe :command) "grok"))
      (should (equal (plist-get recipe :args) '("--model" "x")))
      (should (equal (plist-get recipe :notes) "review only"))
      (should (equal (plist-get recipe :project) "/tmp/proj/")))))

(ert-deftest ghostherd-test-continue-args-appended-not-replacing ()
  "Resuming must add to the recipe's args, not stand in for them --
otherwise a session spawned with --model comes back on the default."
  (let* ((spec (ghostherd--spec 'claude))
         (base '("--permission-mode" "acceptEdits"))
         (composed (append base (plist-get spec :continue-args))))
    (should (equal composed '("--permission-mode" "acceptEdits" "--continue")))))

(ert-deftest ghostherd-test-continue-args-declared-per-kind ()
  "Declared in the spec rather than hardcoded, so a CLI without a resume
flag is representable."
  (dolist (kind '(claude grok agy))
    (should (plist-get (ghostherd--spec kind) :continue-args)))
  (should-not (plist-get (ghostherd--spec 'shell) :continue-args)))

(ert-deftest ghostherd-test-respawn-rejects-continue-without-flag ()
  "Match the message, not just `user-error'.  Falling through to
`ghostherd-spawn' also raises a user-error (no ghostel here), so a
type-only assertion passes whether or not the guard exists."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "sh" :kind 'shell)))
      (should (string-match-p
               "no resume flag"
               (cadr (should-error (ghostherd-respawn s t)
                                   :type 'user-error)))))))

(ert-deftest ghostherd-test-respawn-frees-name-before-spawning ()
  "`ghostherd-spawn' refuses to clobber an existing buffer, so respawn
has to tear the old one down first or it can never succeed twice."
  (ghostherd-tests--with-herd ()
    (let* ((s (ghostherd-tests--session :name "a" :kind 'agy))
           (buf (ghostherd-session-buffer s))
           (spawned nil))
      ;; Stand in for the PTY: record the call, assert the way is clear.
      (cl-letf (((symbol-function 'ghostherd-spawn)
                 (lambda (kind &rest plist)
                   (setq spawned (cons kind plist))
                   (should-not (buffer-live-p buf))
                   (should-not (gethash "a" ghostherd--sessions))
                   nil)))
        (ghostherd-respawn s nil))
      (should (eq (car spawned) 'agy))
      (should (equal (plist-get (cdr spawned) :name) "a")))))

(ert-deftest ghostherd-test-respawn-passes-continue-args ()
  (ghostherd-tests--with-herd ()
    (let* ((s (ghostherd-tests--session :name "a" :kind 'agy
                                        :args '("--effort" "high")))
           (spawned nil))
      (cl-letf (((symbol-function 'ghostherd-spawn)
                 (lambda (kind &rest plist)
                   (ignore kind)
                   (setq spawned plist) nil)))
        (ghostherd-respawn s t))
      (should (equal (plist-get spawned :args)
                     '("--effort" "high" "--continue"))))))

;;; Switcher annotation and notes

(ert-deftest ghostherd-test-candidate-is-just-the-name ()
  "Detail lives in the annotation, so it cannot widen what input matches."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "reviewer" :kind 'grok
                                       :project "/tmp/proj/")))
      (should (equal (ghostherd--format-candidate s) "reviewer")))))

(ert-deftest ghostherd-test-annotation-carries-detail ()
  (ghostherd-tests--with-herd ()
    (let* ((s (ghostherd-tests--session
               :name "reviewer" :kind 'grok :state 'blocked
               :project "/tmp/proj/" :notes "reviews auth"))
           (annotation (substring-no-properties
                        (ghostherd--session-annotation s))))
      (dolist (fragment '("grok" "blocked" "/tmp/proj/" "reviews auth"))
        (should (string-match-p (regexp-quote fragment) annotation))))))

(ert-deftest ghostherd-test-annotation-elides-long-notes ()
  (ghostherd-tests--with-herd ()
    (let* ((s (ghostherd-tests--session :name "a" :notes (make-string 200 ?x)))
           (ghostherd-annotation-note-width 12)
           (annotation (substring-no-properties
                        (ghostherd--session-annotation s))))
      (should-not (string-match-p (make-string 20 ?x) annotation))
      (should (< (length annotation) 100)))))

(ert-deftest ghostherd-test-annotation-omits-absent-note ()
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy)))
      ;; no trailing separator run where the note would have been
      (should-not (string-match-p
                   "  \\'" (substring-no-properties
                            (ghostherd--session-annotation s)))))))

(ert-deftest ghostherd-test-affixation-maps-back-to-sessions ()
  "Affixation is handed bare candidate strings, so the name must be
enough to find the session again."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--session :name "a" :kind 'agy :state 'blocked)
    (ghostherd-tests--session :name "b" :kind 'grok :state 'idle)
    (let ((rows (ghostherd--session-affixation '("a" "b"))))
      (should (equal (mapcar #'car rows) '("a" "b")))
      (should (string-match-p (regexp-quote (ghostherd--state-glyph 'blocked))
                              (nth 1 (car rows))))
      (should (string-match-p "agy" (nth 2 (car rows)))))))

(ert-deftest ghostherd-test-affixation-tolerates-stale-candidate ()
  "A session can die between building the list and rendering it."
  (ghostherd-tests--with-herd ()
    (should (equal (ghostherd--session-affixation '("gone"))
                   '(("gone" "" ""))))))

(ert-deftest ghostherd-test-completion-table-advertises-category ()
  "The category is what lets consult and marginalia recognise these."
  (let* ((table (ghostherd--session-completion-table '("a")))
         (metadata (funcall table "" nil 'metadata)))
    (should (eq (alist-get 'category (cdr metadata)) 'ghostherd-session))
    (should (eq (alist-get 'affixation-function (cdr metadata))
                'ghostherd--session-affixation))))

(ert-deftest ghostherd-test-set-notes-normalises-blank ()
  "Blank input clears the note rather than storing whitespace that would
render as an empty annotation field."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a")))
      (ghostherd-set-notes s "  reviews auth  ")
      (should (equal (ghostherd-session-notes s) "reviews auth"))
      (ghostherd-set-notes s "   ")
      (should-not (ghostherd-session-notes s))
      (ghostherd-set-notes s nil)
      (should-not (ghostherd-session-notes s)))))

;;; Per-project defaults

(defmacro ghostherd-tests--with-dir-locals (contents &rest body)
  "Run BODY with `dir' bound to a temp directory holding CONTENTS.
CONTENTS is written verbatim as .dir-locals.el, or nil for none."
  (declare (indent 1) (debug t))
  `(let ((dir (file-name-as-directory (make-temp-file "ghostherd-dl" t))))
     (unwind-protect
         (progn
           (when ,contents
             (with-temp-file (expand-file-name ".dir-locals.el" dir)
               (insert ,contents)))
           ,@body)
       (delete-directory dir t))))

(ert-deftest ghostherd-test-project-defaults-read-from-directory ()
  "Read by directory, not from the current buffer: `ghostherd-new' is
often invoked from the sidebar, which never picked up dir-locals."
  (ghostherd-tests--with-dir-locals
      "((nil . ((ghostherd-project-kind . grok)
                (ghostherd-project-args . (\"--effort\" \"high\")))))"
    (let ((defaults (ghostherd-project-defaults dir)))
      (should (eq (car defaults) 'grok))
      (should (equal (cdr defaults) '("--effort" "high"))))))

(ert-deftest ghostherd-test-project-defaults-absent ()
  (ghostherd-tests--with-dir-locals nil
    (should (equal (ghostherd-project-defaults dir) '(nil . nil)))))

(ert-deftest ghostherd-test-project-defaults-partial ()
  "Setting only one of the two must not fabricate the other."
  (ghostherd-tests--with-dir-locals
      "((nil . ((ghostherd-project-kind . claude))))"
    (let ((defaults (ghostherd-project-defaults dir)))
      (should (eq (car defaults) 'claude))
      (should-not (cdr defaults)))))

(ert-deftest ghostherd-test-project-defaults-survive-malformed-file ()
  "A broken .dir-locals.el costs the defaults, not the ability to start
an agent.  This asserts the contract, not the mechanism: Emacs warns and
yields nothing here rather than signalling, so it does not exercise the
`ignore-errors' around the read."
  (ghostherd-tests--with-dir-locals "((nil . ((ghostherd-project-kind"
    (should (equal (ghostherd-project-defaults dir) '(nil . nil)))))

(ert-deftest ghostherd-test-project-defaults-safe-predicates ()
  "Marked safe so a project preference does not trigger a prompt, but
only for the shapes actually expected."
  (should (funcall (get 'ghostherd-project-kind 'safe-local-variable) 'claude))
  (should-not (funcall (get 'ghostherd-project-kind 'safe-local-variable) "claude"))
  (let ((safe-args (get 'ghostherd-project-args 'safe-local-variable)))
    (should (funcall safe-args '("--effort" "high")))
    (should (funcall safe-args nil))
    (should-not (funcall safe-args '("--effort" 3)))
    (should-not (funcall safe-args "--effort"))))

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

(ert-deftest ghostherd-test-answer-homes-before-descending ()
  "The highlighted option is not necessarily the first, so counting down
from wherever the cursor happens to be would pick the wrong answer."
  (ghostherd-tests--with-herd ()
    (let* ((s (ghostherd-tests--session :name "a"))
           (sent (ghostherd-tests--recording-keys (ghostherd-answer s 3)))
           (names (mapcar #'car sent)))
      (should (equal (last names) '("return")))
      (should (= (cl-count "down" names :test #'equal) 2))
      (should (> (cl-count "up" names :test #'equal) 1))
      ;; every `up' precedes every `down'
      (should (< (cl-position "up" names :test #'equal :from-end t)
                 (cl-position "down" names :test #'equal))))))

(ert-deftest ghostherd-test-answer-rejects-zero ()
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a")))
      (should-error (ghostherd-answer s 0) :type 'user-error))))

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

(defun ghostherd-tests--parse-json (string)
  "Parse STRING as JSON into alists with symbol keys."
  (json-parse-string string :object-type 'alist :array-type 'list))

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

;;; Knowing your own name

(ert-deftest ghostherd-test-agent-environment-carries-the-name ()
  "The one thing an agent cannot be told after it starts."
  (let ((env (ghostherd-agent-environment
              'tmux (list :name "reviewer" :kind 'agy))))
    (should (member "GHOSTHERD_SESSION=reviewer" env))
    (should (member "GHOSTHERD_BACKEND=tmux" env))))

(ert-deftest ghostherd-test-agent-environment-names-the-emacs-socket ()
  "`bin/ghostherd' is the only way back for a tmux-hosted agent, and it
should reach *this* Emacs without being configured."
  (let ((server-name "herd"))
    (should (member "GHOSTHERD_SOCKET=herd"
                    (ghostherd-agent-environment 'tmux '(:name "a"))))))

(ert-deftest ghostherd-test-agent-environment-advertises-rpc ()
  (cl-letf (((symbol-function 'ghostherd-memory-rpc-url)
             (lambda () "http://127.0.0.1:49152/jsonrpc")))
    (should (member "GHOSTHERD_RPC=http://127.0.0.1:49152/jsonrpc"
                    (ghostherd-agent-environment 'tmux '(:name "a"))))))

(ert-deftest ghostherd-test-agent-environment-omits-rpc-until-sidecar ()
  (cl-letf (((symbol-function 'ghostherd-memory-rpc-url) (lambda () nil)))
    (should-not
     (cl-find "GHOSTHERD_RPC="
              (ghostherd-agent-environment 'tmux '(:name "a"))
              :test (lambda (pre s) (string-prefix-p pre s))))))

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

(ert-deftest ghostherd-test-cmd-self-is-the-calling-terminal ()
  "`ghostel_cmd' is dispatched from the asking terminal's VT parser, so
the caller is identifiable with no environment at all."
  (ghostherd-tests--with-herd ()
    (let ((session (ghostherd-tests--session :name "impl" :kind 'agy
                                             :state 'working)))
      (with-current-buffer (ghostherd-session-buffer session)
        (let ((parsed (ghostherd-tests--parse-json (ghostherd-cmd-state "self"))))
          (should (equal (alist-get 'name parsed) "impl")))))))

(ert-deftest ghostherd-test-cmd-self-outside-an-agent-is-an-error ()
  "Better than resolving to whatever buffer happened to be current."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--session :name "impl" :kind 'agy)
    (with-temp-buffer
      (should-error (ghostherd-cmd-state "self") :type 'user-error))))

(ert-deftest ghostherd-test-cmd-message-attributes-the-caller ()
  "FROM used to default to \"user\", which was a lie whenever the caller
was another agent -- and an agent had no way to say otherwise."
  (ghostherd-tests--with-herd ()
    (let ((impl (ghostherd-tests--session :name "impl" :kind 'agy))
          (rev (ghostherd-tests--session :name "rev" :kind 'agy))
          (sent nil))
      (ignore rev)
      (cl-letf (((symbol-function 'ghostherd-send)
                 (lambda (_session text &optional _submit) (setq sent text))))
        (with-current-buffer (ghostherd-session-buffer impl)
          (ghostherd-cmd-message "rev" "have a look"))
        (should (string-match-p "impl" sent))
        (should-not (string-match-p "user" sent))))))

;;; Naming and formatting

(ert-deftest ghostherd-test-unique-name ()
  (ghostherd-tests--with-herd ()
    (should (equal (ghostherd--unique-name "rev") "rev"))
    (ghostherd-tests--session :name "rev")
    (should (equal (ghostherd--unique-name "rev") "rev-2"))
    (ghostherd-tests--session :name "rev-2")
    (should (equal (ghostherd--unique-name "rev") "rev-3"))))

(ert-deftest ghostherd-test-age-string ()
  (let ((now (current-time)))
    (should (equal (ghostherd--age-string nil) "-"))
    (should (equal (ghostherd--age-string now) "0s"))
    (should (equal (ghostherd--age-string (time-subtract now 45)) "45s"))
    (should (equal (ghostherd--age-string (time-subtract now 90)) "1m"))
    (should (equal (ghostherd--age-string (time-subtract now 3720)) "1h02m"))
    ;; a clock skew into the future must not render as negative
    (should (equal (ghostherd--age-string (time-add now 60)) "0s"))))

(ert-deftest ghostherd-test-state-glyphs-are-distinct ()
  "The glyph column is the whole at-a-glance signal; collisions defeat it."
  (let ((glyphs (mapcar #'ghostherd--state-glyph
                        '(blocked working done idle dead))))
    (should (= (length glyphs) (length (delete-dups (copy-sequence glyphs)))))))

;;; The backend slot
;;
;; The point of the slot is that everything above it goes through it, so
;; these tests pin the call sites rather than either implementor: a fake
;; backend returns a known screen, and breaking the wiring fails a test
;; without tmux or a PTY being anywhere near it.

(ert-deftest ghostherd-test-detect-reads-through-the-backend ()
  "Detection must take the screen from the backend, not from a buffer.
A session on a backend with no buffer at all still has to be detectable
-- that is the whole reason the slot exists."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy :backend 'fake))
          (ghostherd-tests--fake-screen "Do you want to proceed\n"))
      ;; the buffer says nothing; the backend says blocked
      (should (eq (car (ghostherd--detect-state s)) 'blocked)))))

(ert-deftest ghostherd-test-read-and-wait-read-through-the-backend ()
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy :backend 'fake))
          (ghostherd-tests--fake-screen "42 tests, 0 failures\n"))
      (should (equal (ghostherd-read s) "42 tests, 0 failures\n"))
      (should (equal (ghostherd-output-matches s "[0-9]+ failures")
                     "0 failures")))))

(ert-deftest ghostherd-test-live-p-is-the-host-not-the-view ()
  "A detached session -- alive, no buffer -- is live.  This is the
distinction the whole phase turns on."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :backend 'fake))
          (ghostherd-tests--fake-live t))
      (kill-buffer (ghostherd-session-buffer s))
      (setf (ghostherd-session-buffer s) nil)
      (should (ghostherd--session-live-p s))
      (let ((ghostherd-tests--fake-live nil))
        (should-not (ghostherd--session-live-p s))))))

(ert-deftest ghostherd-test-ghostel-session-live-p-unchanged ()
  "The default backend must still mean exactly what it did."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a")))
      (should (eq (ghostherd-session-backend s) 'ghostel))
      (should (ghostherd--session-live-p s))
      (kill-buffer (ghostherd-session-buffer s))
      (should-not (ghostherd--session-live-p s)))))

(ert-deftest ghostherd-test-string-tail-keeps-the-top-line ()
  "A capture of exactly N rows ends in a newline.  Splitting it naively
yields N+1 fields, and taking the last N then drops the *first* line --
which is how the first thing an agent printed goes missing from a
screen that is plainly still showing it."
  (let ((screen (mapconcat #'number-to-string (number-sequence 1 40) "\n")))
    (should (equal (ghostherd--string-tail (concat screen "\n") 40) screen))
    (should (equal (ghostherd--string-tail (concat screen "\n") 3) "38\n39\n40"))))

;;; Surviving a reload

(ert-deftest ghostherd-test-stale-session-names-itself ()
  "Adding a slot to `ghostherd-session' does not migrate the structs
already in the registry, so every field after it reads shifted and a
project path arrives where a backend symbol belongs.  Left alone that
surfaces as `cl-no-applicable-method', once per redisplay, naming
neither the cause nor the cure."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "old")))
      (setf (ghostherd-session-backend s) "/src/app/")
      (let ((message (cadr (should-error (ghostherd--host-live-p s)))))
        (should (string-match-p "clrhash ghostherd--sessions" message))
        (should (string-match-p "old" message))))))

(ert-deftest ghostherd-test-mode-line-asks-no-host ()
  "Redisplay is no place for I/O, and no place to find out that the I/O
failed: a segment that signals breaks the whole frame's redisplay, and
repeats for as long as the session is registered."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :state 'blocked)))
      ;; the worst case: a session whose host cannot even be identified
      (setf (ghostherd-session-backend s) "/src/app/")
      (let ((segment (substring-no-properties
                      (or (ghostherd--mode-line-segment) ""))))
        (should (string-match-p "1" segment))))))

(ert-deftest ghostherd-test-ensure-sessions-keeps-what-it-cannot-ask ()
  "Pruning on a failed question would delete the herd rather than
report it -- and a host is far more likely to be briefly unreachable
than actually gone."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :backend 'fake))
          (ghostherd-tests--fake-live t))
      (cl-letf (((symbol-function 'ghostherd--host-live-p)
                 (lambda (&rest _) (error "host unreachable"))))
        (ghostherd--ensure-sessions))
      (should (eq (ghostherd-get "a") s)))))

;;; Detach is not death

(ert-deftest ghostherd-test-view-exit-kills-only-on-ghostel ()
  "The easy bug.  On a backend where the buffer runs `tmux attach', the
process exiting means a client detached; marking the session dead would
turn every closed window into a crash notification, fire a handoff
callback, and be reverted by the next poll anyway."
  (ghostherd-tests--with-herd ()
    (cl-letf (((symbol-function 'ghostherd--notify) (lambda (&rest _) nil)))
      (let* ((ghostel (ghostherd-tests--session
                       :name "g" :kind 'agy :state 'working))
             (tmux (ghostherd-tests--session
                    :name "t" :kind 'agy :state 'working :backend 'tmux)))
        (ghostherd--on-ghostel-exit (ghostherd-session-buffer ghostel)
                                    "finished\n")
        (should (eq (ghostherd-session-state ghostel) 'dead))

        (ghostherd--on-ghostel-exit (ghostherd-session-buffer tmux)
                                    "finished\n")
        (should (eq (ghostherd-session-state tmux) 'working))
        (should-not (ghostherd-session-buffer tmux))))))

(ert-deftest ghostherd-test-view-is-host-p-declared-per-backend ()
  (should (ghostherd-backend-view-is-host-p 'ghostel))
  (should-not (ghostherd-backend-view-is-host-p 'tmux))
  ;; An unknown backend must default to the safe answer: a view that is
  ;; not known to own the agent cannot be allowed to declare it dead.
  (should-not (ghostherd-backend-view-is-host-p 'something-new)))

;;; Restore

(ert-deftest ghostherd-test-restore-registers-detached-sessions ()
  (ghostherd-tests--with-herd ()
    (let ((ghostherd-known-backends '(fake))
          (ghostherd-tests--fake-recipes
           '((:host-id "gh-abc-rev" :name "rev" :kind agy :project "/tmp/p/"
              :command "agy" :args ("--effort" "high") :notes "reviews auth"))))
      (should (= (ghostherd-restore) 1))
      (let ((s (ghostherd-get "rev")))
        (should (eq (ghostherd-session-backend s) 'fake))
        (should (equal (ghostherd-session-host-id s) "gh-abc-rev"))
        (should (equal (ghostherd-session-args s) '("--effort" "high")))
        (should (equal (ghostherd-session-notes s) "reviews auth"))
        ;; Detached, not dead: alive with nobody looking.
        (should-not (ghostherd-session-buffer s))
        ;; And not freshly finished, so it must not queue a `done' notify.
        (should (ghostherd-session-seen s))))))

(ert-deftest ghostherd-test-restore-ignores-known-sessions ()
  "Restore runs again whenever the registry looks empty, so it has to be
idempotent -- otherwise it clobbers the live session it just found."
  (ghostherd-tests--with-herd ()
    (let ((ghostherd-known-backends '(fake))
          (ghostherd-tests--fake-recipes
           '((:host-id "gh-abc-rev" :name "rev" :kind agy))))
      (let ((first (ghostherd-tests--session :name "rev" :kind 'grok)))
        (should (= (ghostherd-restore) 0))
        (should (eq (ghostherd-get "rev") first))
        (should (eq (ghostherd-session-kind (ghostherd-get "rev")) 'grok))))))

(ert-deftest ghostherd-test-restore-runs-again-when-the-herd-looks-empty ()
  "`ghostherd--ensure-sessions' runs from the mode-line and prunes what
it cannot reach, so one unreachable moment must not cost the herd until
the mode is toggled."
  (ghostherd-tests--with-herd ()
    (let ((ghostherd-known-backends '(fake))
          (ghostherd-tests--fake-recipes
           '((:host-id "gh-abc-rev" :name "rev" :kind agy))))
      (ghostherd--maybe-restore)
      (should (ghostherd-get "rev"))
      ;; ...and it must not keep re-running once there is something there
      (let ((ghostherd-tests--fake-recipes
             '((:host-id "gh-abc-imp" :name "imp" :kind agy))))
        (ghostherd--maybe-restore)
        (should-not (ghostherd-get "imp"))))))

;;; tmux naming and targets

(ert-deftest ghostherd-test-tmux-name-has-no-target-punctuation ()
  "tmux parses a target as `session:window.pane', so a colon or a dot in
a session name is a bug rather than a style problem."
  (let ((name (ghostherd-tmux-session-name "/tmp/p/" "rev:iew.er 2")))
    (should-not (string-match-p "[:.]" name))
    (should (string-prefix-p "gh-" name))
    (should (string-suffix-p "-rev_iew_er_2" name))))

(ert-deftest ghostherd-test-tmux-name-separates-projects ()
  "Two agents called `reviewer' in two projects share one socket."
  (should-not (equal (ghostherd-tmux-session-name "/tmp/a/" "rev")
                     (ghostherd-tmux-session-name "/tmp/b/" "rev")))
  ;; ...and the same project must hash the same across Emacs restarts,
  ;; which is what restore matches on.
  (should (equal (ghostherd-tmux-session-name "/tmp/a/" "rev")
                 (ghostherd-tmux-session-name "/tmp/a" "rev"))))

(ert-deftest ghostherd-test-tmux-name-survives-an-empty-name ()
  "A name with nothing usable in it still has to produce a target."
  (should (string-suffix-p "-agent" (ghostherd-tmux-session-name nil "")))
  (should-not (string-match-p "[:.]" (ghostherd-tmux-session-name nil "..."))))

(ert-deftest ghostherd-test-tmux-target-is-exact ()
  "Without the `=' tmux prefix-matches, so `gh-abc-rev' silently drives
`gh-abc-reviewer'; without the trailing colon the commands that parse a
pane target resolve a bare name against the current session instead."
  (should (equal (ghostherd-tmux--target "gh-abc-rev") "=gh-abc-rev:"))
  (should-error (ghostherd-tmux--target "gh-abc-rev:iew")))

;;; tmux server config

(ert-deftest ghostherd-test-tmux-config-is-generated-and-absolute ()
  "Generated rather than shipped, because a lone .conf beside the source
is not on any package manager's default file list -- and the resulting
default `remain-on-exit off' fails a long way from its cause.

Absolute, because `locate-user-emacs-file' returns \"~/.emacs.d/...\",
Emacs expands a tilde in a file name and tmux does not: it would take
the config as missing and start with its own defaults, silently."
  (let* ((ghostherd-tmux-config nil)
         (ghostherd-tmux--generated-config nil)
         (user-emacs-directory (file-name-as-directory
                                (make-temp-file "ghostherd-cfg" t)))
         (file (ghostherd-tmux--config-file)))
    (unwind-protect
        (progn
          (should file)
          (should (file-name-absolute-p file))
          (should-not (string-prefix-p "~" file))
          (should (equal (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))
                         ghostherd-tmux-config-text))
          ;; and it is on every invocation, since only the process that
          ;; starts the server reads it
          (should (member "-f" (ghostherd-tmux--socket-args))))
      (delete-directory user-emacs-directory t))))

(ert-deftest ghostherd-test-tmux-config-declares-what-the-code-assumes ()
  "Three options the Elisp is written against.  If the config stops
setting them the failures are remote: sessions vanish when an agent
exits, the pane loses a row to a status line the moment a client
attaches, and Escape lags."
  (dolist (line '("set -g remain-on-exit on"
                  "set -g status off"
                  "set -g prefix None"))
    (should (string-match-p (regexp-quote line) ghostherd-tmux-config-text))))

;;; tmux command construction
;;
;; `ghostherd-tmux--call' is the single seam every tmux invocation passes
;; through, so stubbing it pins the actual command lines -- which is
;; where this backend's mistakes live.

(ert-deftest ghostherd-test-tmux-capture-takes-the-visible-pane ()
  "No -e (rules match text, not SGR), no -J (joining wrapped lines
invents rows the agent never drew), and no scrollback (an alt-screen
TUI does not keep its UI in history, so history is where you match a
permission prompt that scrolled away and is no longer real)."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--tmux-session :name "a")))
      (ghostherd-tests--with-tmux '(("capture-pane" . (0 . "hello\n")))
        (should (equal (ghostherd--host-capture s) "hello")))
      (let ((call (ghostherd-tests--tmux-call "capture-pane")))
        (should (equal call '("capture-pane" "-p" "-t" "=gh-abc-rev:")))
        (should-not (member "-e" call))
        (should-not (member "-J" call))
        (should-not (member "-S" call))))))

(ert-deftest ghostherd-test-tmux-paste-does-not-submit-each-line ()
  "paste-buffer turns every linefeed into a carriage return by default,
which submits each line of a multi-line prompt separately.  -r keeps
them as newlines; -p brackets the paste when the agent asked for it."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--tmux-session :name "a")))
      (ghostherd-tests--with-tmux nil
        (ghostherd--host-send-text s "alpha\nbeta" nil))
      (should (equal (ghostherd-tests--tmux-call "set-buffer")
                     '("set-buffer" "-b" "ghostherd-paste" "--" "alpha\nbeta")))
      (let ((paste (ghostherd-tests--tmux-call "paste-buffer")))
        (should (member "-r" paste))
        (should (member "-p" paste))
        (should (member "=gh-abc-rev:" paste)))
      (should-not (ghostherd-tests--tmux-call "send-keys")))))

(ert-deftest ghostherd-test-tmux-submit-presses-enter ()
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--tmux-session :name "a")))
      (ghostherd-tests--with-tmux nil
        (ghostherd--host-send-text s "hi" t))
      (should (equal (ghostherd-tests--tmux-call "send-keys")
                     '("send-keys" "-t" "=gh-abc-rev:" "Enter"))))))

(ert-deftest ghostherd-test-tmux-key-names ()
  "The friendly names are the contract; each backend translates them
into its own vocabulary, and anything unlisted passes through so tmux's
full key set stays reachable."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--tmux-session :name "a")))
      (ghostherd-tests--with-tmux nil
        (ghostherd--host-send-keys s '("esc" "C-c" "down" "return" "F5")))
      (should (equal (ghostherd-tests--tmux-call "send-keys")
                     '("send-keys" "-t" "=gh-abc-rev:"
                       "Escape" "C-c" "Down" "Enter" "F5"))))))

(ert-deftest ghostherd-test-tmux-send-keys-still-does-not-claim-working ()
  "Same contract on either host: sending Escape means stop."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--tmux-session :name "a" :state 'blocked)))
      (ghostherd-tests--with-tmux nil
        (ghostherd-send-keys s "esc"))
      (should (eq (ghostherd-session-state s) 'blocked)))))

(ert-deftest ghostherd-test-tmux-spawn-execs-directly ()
  "No shell, so no `ghostherd-spawn-delay' guesswork and no quoting:
tmux hands multiple trailing arguments to execvp as they are."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-tmux '(("has-session" . (1 . "")))
      (let ((spawned (ghostherd-backend-spawn
                      'tmux
                      (list :name "rev" :kind 'agy :command "agy"
                            :args '("--effort" "high" "a b")
                            :directory "/tmp/" :project "/tmp/"
                            :notes "n"))))
        (should (equal (plist-get spawned :host-id)
                       (ghostherd-tmux-session-name "/tmp/" "rev")))
        ;; Detached: spawning is not visiting.
        (should-not (plist-get spawned :buffer))))
    (let ((call (ghostherd-tests--tmux-call "new-session")))
      (should (member "-d" call))
      (should (equal (last call 4) '("agy" "--effort" "high" "a b"))))))

(ert-deftest ghostherd-test-tmux-spawn-injects-identity ()
  "`-e' before the command, and the command still last: everything after
it belongs to execvp."
  (ghostherd-tests--with-herd ()
    (let ((ghostherd-tmux--env-supported 'unknown))
      (ghostherd-tests--with-tmux '(("has-session" . (1 . "")))
        (ghostherd-backend-spawn
         'tmux (list :name "rev" :kind 'agy :command "agy" :args '("--effort")
                     :directory "/tmp/" :project "/tmp/"))))
    (let ((call (ghostherd-tests--tmux-call "new-session")))
      (should (member "GHOSTHERD_SESSION=rev" call))
      (should (member "GHOSTHERD_BACKEND=tmux" call))
      (should (equal (last call 2) '("agy" "--effort")))
      ;; -e has to come before the command, or tmux hands it to the agent.
      (should (< (cl-position "-e" call :test #'equal)
                 (cl-position "agy" call :test #'equal))))))

(ert-deftest ghostherd-test-tmux-spawn-retries-without-identity ()
  "`-e' is tmux 3.2+.  The version is not parsed -- `tmux -V' says things
like \"next-3.6\" -- so the spawn is its own probe: an old tmux gets one
failed call and then the herd it always had, minus identity."
  (ghostherd-tests--with-herd ()
    (let ((ghostherd-tmux--env-supported 'unknown)
          (calls nil))
      (cl-letf (((symbol-function 'ghostherd-tmux--call)
                 (lambda (args)
                   (push args calls)
                   (cond
                    ;; Failure means "no such session"; answering 0 here
                    ;; tells `--unique-id' every name is taken, and it
                    ;; looks for a free one forever.
                    ((equal (car args) "has-session") (cons 1 ""))
                    ((member "-e" args) (cons 1 "unknown option -- e"))
                    (t (cons 0 ""))))))
        (should (ghostherd-backend-spawn
                 'tmux (list :name "rev" :kind 'agy :command "agy"
                             :directory "/tmp/" :project "/tmp/")))
        ;; And the answer is remembered: the next spawn does not pay for it.
        (should (eq ghostherd-tmux--env-supported nil))
        (setq calls nil)
        (ghostherd-backend-spawn
         'tmux (list :name "rev2" :kind 'agy :command "agy"
                     :directory "/tmp/" :project "/tmp/"))
        (should-not (cl-find-if (lambda (args) (member "-e" args)) calls))))))

(ert-deftest ghostherd-test-tmux-spawn-failure-is-still-an-error ()
  "The retry must not swallow a spawn that failed for a real reason."
  (ghostherd-tests--with-herd ()
    (let ((ghostherd-tmux--env-supported 'unknown))
      (cl-letf (((symbol-function 'ghostherd-tmux--call)
                 (lambda (_args) (cons 1 "no space left on device"))))
        (should-error (ghostherd-backend-spawn
                       'tmux (list :name "rev" :kind 'agy :command "agy"
                                   :directory "/tmp/" :project "/tmp/"))
                      :type 'user-error)))))

(ert-deftest ghostherd-test-tmux-spawn-writes-the-recipe ()
  "The tmux session name is not big enough to hold a recipe, and a
sidecar file can be deleted while the process it describes is running."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-tmux '(("has-session" . (1 . "")))
      (ghostherd-backend-spawn
       'tmux (list :name "rev" :kind 'agy :command "agy"
                   :args '("--effort" "high") :directory "/tmp/"
                   :project "/tmp/" :notes "reviews auth")))
    (let ((written (ghostherd-tests--tmux-options)))
      (pcase-dolist (`(,option . ,value)
                     '(("@ghostherd-name"  . "rev")
                       ("@ghostherd-kind"  . "agy")
                       ("@ghostherd-notes" . "reviews auth")
                       ("@ghostherd-args"  . "[\"--effort\",\"high\"]")))
        (should (equal (alist-get option written nil nil #'equal) value)))
      ;; Every set-option must address the session exactly, or a name
      ;; that is a prefix of another quietly gets someone else's recipe.
      (let ((target (ghostherd-tmux--target
                     (ghostherd-tmux-session-name "/tmp/" "rev"))))
        (dolist (call ghostherd-tests--tmux-calls)
          (when (equal (car call) "set-option")
            (should (equal (nth 2 call) target))))))))

(ert-deftest ghostherd-test-tmux-args-round-trip-through-json ()
  "Arguments are a list of free text: spaces, quotes and backslashes all
occur, and none of them can go into one tmux option value untouched."
  (dolist (args '(nil ("--model" "x y") ("--system-prompt" "say \"hi\"\\n")))
    (should (equal (ghostherd-tmux--decode
                    :args (ghostherd-tmux--encode :args args))
                   args)))
  (should (equal (ghostherd-tmux--decode :kind (ghostherd-tmux--encode :kind 'agy))
                 'agy))
  ;; An unset option must not become the symbol nil or the empty string.
  (should-not (ghostherd-tmux--decode :notes nil))
  (should-not (ghostherd-tmux--decode :kind "")))

(ert-deftest ghostherd-test-tmux-list-only-claims-its-own ()
  "The socket is ghostherd's, but a stray session on it is not: only the
`gh-' prefix marks one this package made."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-tmux
        '(("list-sessions" . (0 . "gh-abc-rev\nscratch\ngh-abc-imp\n")))
      (cl-letf (((symbol-function 'ghostherd-tmux--read-recipe)
                 (lambda (id) (list :name (concat "n:" id)))))
        (let ((listed (ghostherd-backend-list 'tmux)))
          (should (equal (mapcar (lambda (r) (plist-get r :host-id)) listed)
                         '("gh-abc-rev" "gh-abc-imp"))))))))

(ert-deftest ghostherd-test-tmux-live-p-uses-one-snapshot ()
  "Liveness is asked far more often than it changes -- every poll and
every mode-line redisplay -- so one `list-panes' has to answer for the
whole herd rather than costing a subprocess per session."
  (ghostherd-tests--with-herd ()
    (let ((a (ghostherd-tests--session :name "a" :backend 'tmux
                                       :host-id "gh-abc-a"))
          (b (ghostherd-tests--session :name "b" :backend 'tmux
                                       :host-id "gh-abc-b")))
      (ghostherd-tests--with-tmux
          '(("list-panes" . (0 . "gh-abc-a\t0\tclaude\ngh-abc-b\t1\t\n")))
        (should (ghostherd--host-live-p a))
        (should (ghostherd--host-live-p b))
        (should-not (ghostherd--host-exited-p a))
        ;; `remain-on-exit' keeps a dead pane capturable: still live, but
        ;; the agent behind it is gone.
        (should (equal (ghostherd--host-exited-p b) "process exited"))
        (should (equal (ghostherd--host-title a) "claude")))
      (should (= (cl-count "list-panes" ghostherd-tests--tmux-calls
                           :key #'car :test #'equal)
                 1)))))

(ert-deftest ghostherd-test-tmux-unknown-host-is-not-live ()
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :backend 'tmux
                                       :host-id "gh-abc-gone")))
      (ghostherd-tests--with-tmux '(("list-panes" . (0 . "gh-abc-other\t0\t\n")))
        (should-not (ghostherd--host-live-p s))
        (should (equal (car (ghostherd--detect-state s)) 'dead))))))

(ert-deftest ghostherd-test-tmux-missing-binary-is-a-dead-host ()
  "A machine without tmux must not break the ghostel backend, so a
missing binary reports a gone host rather than signalling."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :backend 'tmux
                                       :host-id "gh-abc-a"))
          (ghostherd-tmux--status-cache nil)
          (ghostherd-tmux-executable "ghostherd-no-such-tmux"))
      (should-not (ghostherd--host-live-p s))
      (should-not (ghostherd-tmux-available-p)))))

;;; One round trip for the whole herd

(ert-deftest ghostherd-test-screens-args-put-a-marker-before-each-pane ()
  "The status fields ride along in front, then one delimiter per pane --
so a single invocation answers liveness, titles and every screen."
  (let* ((marker "@@m@@")
         (args (ghostherd-tmux--screens-args '("gh-a" "gh-b") marker)))
    (should (equal (seq-take args 4)
                   (list "list-panes" "-a" "-F"
                         ghostherd-tmux--status-format)))
    (should (equal (nthcdr 4 args)
                   (list ";" "display-message" "-p" "@@m@@ gh-a"
                         ";" "capture-pane" "-p" "-t" "=gh-a:"
                         ";" "display-message" "-p" "@@m@@ gh-b"
                         ";" "capture-pane" "-p" "-t" "=gh-b:")))))

(ert-deftest ghostherd-test-screens-marker-is-not-guessable ()
  "The text being delimited is whatever an agent chose to print, and an
agent can print anything -- including a fixed \"unlikely\" delimiter."
  (should-not (equal (ghostherd-tmux--marker) (ghostherd-tmux--marker))))

(ert-deftest ghostherd-test-parse-screens-splits-status-from-panes ()
  (pcase-let ((`(,status . ,screens)
               (ghostherd-tmux--parse-screens
                (concat "gh-a\t0\ttitle a\ngh-b\t0\ttitle b\n"
                        "@@m@@ gh-a\nAAA\nAAA2\n"
                        "@@m@@ gh-b\nBBB\n")
                "@@m@@")))
    (should (equal status "gh-a\t0\ttitle a\ngh-b\t0\ttitle b"))
    (should (equal (alist-get "gh-a" screens nil nil #'equal) "AAA\nAAA2"))
    (should (equal (alist-get "gh-b" screens nil nil #'equal) "BBB"))))

(ert-deftest ghostherd-test-parse-screens-tolerates-an-abandoned-list ()
  "tmux abandons the rest of a command list at the first failure, which a
pane killed from outside is enough to cause.  What came back is used; the
pane whose marker printed with nothing after it is *not* reported as an
empty screen, because an empty screen would read as `idle'."
  (pcase-let ((`(,_status . ,screens)
               (ghostherd-tmux--parse-screens
                (concat "gh-a\t0\tt\n"
                        "@@m@@ gh-a\nAAA\n"
                        "@@m@@ gh-gone\n")
                "@@m@@")))
    (should (equal (mapcar #'car screens) '("gh-a")))))

(ert-deftest ghostherd-test-fetch-stamps-the-status-cache ()
  "Liveness came back in the same round trip, so asking for it again --
synchronously, on the timer -- is the call this whole path removes."
  (ghostherd-tests--with-herd ()
    (let ((ghostherd-tmux--status-cache nil)
          (ghostherd-tmux--fetch nil)
          (got 'unset)
          (buffer (generate-new-buffer " *fetch-test*")))
      (with-current-buffer buffer
        (insert "gh-abc-rev\t0\tagy\n@@m@@ gh-abc-rev\nThinking…\n"))
      (ghostherd-tmux--fetch-done buffer "@@m@@" '(("gh-abc-rev" . "rev")) 40
                                  (lambda (screens) (setq got screens)))
      (should (equal got '(("rev" . "Thinking…"))))
      (should (equal (ghostherd-tmux--parse-snapshot "gh-abc-rev\t0\tagy")
                     (cdr ghostherd-tmux--status-cache)))
      ;; Fresh by definition: it was stamped just now, so no reader on this
      ;; tick pays for a second list-panes.
      (should (assoc "gh-abc-rev" (ghostherd-tmux--snapshot))))))

(ert-deftest ghostherd-test-fetch-does-not-stack ()
  "A second fetch against a slow server is how a pile-up starts."
  (ghostherd-tests--with-herd ()
    (let* ((s (ghostherd-tests--tmux-session :name "rev"))
           (started 0)
           (fake (start-process "ghostherd-fetch-stub" nil "sleep" "30")))
      (unwind-protect
          (let ((ghostherd-tmux--fetch fake)
                (ghostherd-tmux--fetch-started (float-time)))
            (cl-letf (((symbol-function 'make-process)
                       (lambda (&rest _) (setq started (1+ started)) fake)))
              (should (ghostherd-backend-screens 'tmux (list s) 40 #'ignore)))
            (should (= started 0)))
        (delete-process fake)))))

(ert-deftest ghostherd-test-poll-uses-the-screen-it-was-given ()
  "The whole point: the poll path reads each screen once, in a batch, and
detection must not go back to the host for what it already has."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy :state 'idle)))
      (cl-letf (((symbol-function 'ghostherd--host-capture)
                 (lambda (&rest _) (error "captured again"))))
        (should (eq (ghostherd-poll-session s "Thinking…\n") 'working))))))

(ert-deftest ghostherd-test-poll-all-falls-back-per-session ()
  "A host whose screen is already an Emacs buffer says nothing here, and
gets read session by session -- which costs nothing there."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy :state 'idle
                                       :backend 'fake))
          (ghostherd-tests--fake-screen "Thinking…\n"))
      (ghostherd-poll-all)
      (should (eq (ghostherd-session-state s) 'working)))))

(ert-deftest ghostherd-test-poll-all-groups-by-backend ()
  "One round trip is a claim only a single host can make, and a herd may
straddle two."
  (ghostherd-tests--with-herd ()
    (let ((tmux-session (ghostherd-tests--tmux-session :name "t" :kind 'agy
                                                       :state 'idle))
          (fake-session (ghostherd-tests--session :name "f" :kind 'agy
                                                  :state 'idle :backend 'fake))
          (ghostherd-tests--fake-screen "Thinking…\n")
          (asked nil))
      (cl-letf (((symbol-function 'ghostherd-backend-screens)
                 (lambda (backend sessions _n callback)
                   (push (cons backend (mapcar #'ghostherd-session-name sessions))
                         asked)
                   (when (eq backend 'tmux)
                     (funcall callback '(("t" . "Do you want to proceed\n")))
                     t))))
        ;; The liveness sweep still runs, and without the stub it would ask
        ;; the real socket about a session that is not there.
        (ghostherd-tests--with-tmux nil
          (ghostherd-poll-all)))
      (should (equal (alist-get 'tmux asked) '("t")))
      (should (equal (alist-get 'fake asked) '("f")))
      (should (eq (ghostherd-session-state tmux-session) 'blocked))
      (should (eq (ghostherd-session-state fake-session) 'working)))))

(ert-deftest ghostherd-test-late-callbacks-keep-their-own-group ()
  "A fetch answers after the loop that started it has moved on, so each
group's callback has to still be about *that* group's sessions."
  (ghostherd-tests--with-herd ()
    (let ((a (ghostherd-tests--session :name "a" :kind 'agy :state 'idle
                                       :backend 'one))
          (b (ghostherd-tests--session :name "b" :kind 'agy :state 'idle
                                       :backend 'two))
          (pending nil))
      (cl-letf (((symbol-function 'ghostherd-backend-screens)
                 (lambda (backend sessions _n callback)
                   ;; Hold the callback, the way a real fetch does.
                   (push (cons backend (lambda (screens)
                                         (ignore sessions)
                                         (funcall callback screens)))
                         pending)
                   t))
                ((symbol-function 'ghostherd--session-live-p) (lambda (_s) t)))
        (ghostherd-poll-all)
        (should (= (length pending) 2))
        ;; Deliver in the opposite order, and give both the same screen
        ;; text: if a callback had captured the wrong group, one of these
        ;; sessions would be left untouched.
        (funcall (cdr (assq 'one pending)) '(("a" . "Do you want to proceed\n")))
        (funcall (cdr (assq 'two pending)) '(("b" . "Thinking…\n")))
        (should (eq (ghostherd-session-state a) 'blocked))
        (should (eq (ghostherd-session-state b) 'working))))))

(ert-deftest ghostherd-test-tmux-batch-round-trip ()
  "Two real panes, one invocation, both screens back -- and stderr from a
pane that is not there kept out of them."
  (skip-unless (executable-find "tmux"))
  (ghostherd-tests--with-herd ()
    (let* ((ghostherd-tmux-socket "ghostherd-ert-batch")
           (ghostherd-tmux--status-cache nil)
           (ghostherd-tmux--fetch nil)
           (sessions nil)
           (screens 'unset))
      (unwind-protect
          (progn
            (dolist (name '("batch-a" "batch-b"))
              (push (ghostherd-spawn
                     'shell :name name :backend 'tmux
                     :project temporary-file-directory
                     :directory temporary-file-directory
                     :command "sh"
                     :args (list "-c" (format "echo screen-of-%s; cat" name))
                     :display nil)
                    sessions))
            (sleep-for 0.5)
            (should (ghostherd-backend-screens
                     'tmux sessions 40 (lambda (s) (setq screens s))))
            (with-timeout (10 (error "batched fetch never came back"))
              (while (eq screens 'unset)
                (accept-process-output nil 0.05)))
            (should (equal (sort (mapcar #'car screens) #'string<)
                           '("batch-a" "batch-b")))
            (should (string-match-p "screen-of-batch-a"
                                    (alist-get "batch-a" screens nil nil #'equal)))
            (should (string-match-p "screen-of-batch-b"
                                    (alist-get "batch-b" screens nil nil #'equal)))
            ;; And the same round trip answered liveness.
            (should (assoc (ghostherd-session-host-id (car sessions))
                           (cdr ghostherd-tmux--status-cache))))
        (dolist (session sessions)
          (ignore-errors (ghostherd--host-kill session)))
        (when (timerp ghostherd--poll-timer)
          (cancel-timer ghostherd--poll-timer)
          (setq ghostherd--poll-timer nil))
        (call-process "tmux" nil nil nil "-L" "ghostherd-ert-batch"
                      "kill-server")))))

;;; Optional: a real tmux round trip

(ert-deftest ghostherd-test-tmux-round-trip ()
  "Spawn, capture, send, list and kill against a real tmux.
Everything above stubs the seam; this is the one that would catch tmux
itself changing its mind about a flag."
  (skip-unless (executable-find "tmux"))
  (ghostherd-tests--with-herd ()
    (let* ((ghostherd-tmux-socket "ghostherd-ert")
           (ghostherd-tmux--status-cache nil)
           (session nil))
      (unwind-protect
          (progn
            (setq session (ghostherd-spawn
                           'shell
                           :name "ert-probe" :backend 'tmux
                           :project temporary-file-directory
                           :directory temporary-file-directory
                           :command "cat" :args nil :display nil))
            (should (ghostherd-session-host-id session))
            ;; Detached from birth: nothing was attached to it.
            (should-not (ghostherd-session-buffer session))
            (should (ghostherd--host-live-p session))
            (ghostherd--host-send-text session "ping-from-ert" t)
            (sleep-for 0.5)
            (ghostherd-tmux--forget)
            (should (string-match-p "ping-from-ert"
                                    (ghostherd--host-capture session)))
            ;; The recipe is on the host, so a fresh registry can rebuild it.
            (clrhash ghostherd--sessions)
            (let ((ghostherd-known-backends '(tmux)))
              (should (= (ghostherd-restore) 1)))
            (should (ghostherd-get "ert-probe")))
        (when session
          (ignore-errors (ghostherd--host-kill session)))
        (when (timerp ghostherd--poll-timer)
          (cancel-timer ghostherd--poll-timer)
          (setq ghostherd--poll-timer nil))
        (call-process "tmux" nil nil nil "-L" "ghostherd-ert" "kill-server")))))

;; NOTE: an older test asserted the candidate string itself contained the
;; kind and project.  That was true while everything was crammed into one
;; string; it is superseded by `ghostherd-test-candidate-is-just-the-name'
;; and `ghostherd-test-annotation-carries-detail'.

;;; Memory sidecar (pure: no uvicorn)

(ert-deftest ghostherd-test-memory-server-directory-finds-main ()
  (let ((dir (ghostherd-memory--server-directory)))
    (should dir)
    (should (file-exists-p (expand-file-name "main.py" dir)))))

(ert-deftest ghostherd-test-memory-server-follows-straight-symlink ()
  "straight build dirs have .elc plus a symlink .el; server/ is in the checkout."
  (let* ((src (make-temp-file "gh-src-" t))
         (build (make-temp-file "gh-build-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "server" src) t)
          (write-region "pass\n" nil (expand-file-name "server/main.py" src))
          (write-region "" nil (expand-file-name "ghostherd.el" src))
          (make-symbolic-link (expand-file-name "ghostherd.el" src)
                              (expand-file-name "ghostherd.el" build))
          (write-region "" nil (expand-file-name "ghostherd.elc" build))
          (cl-letf (((symbol-function 'locate-library)
                     (lambda (name &rest _)
                       (cond
                        ((equal name "ghostherd")
                         (expand-file-name "ghostherd.elc" build))
                        ((equal name "ghostherd-memory") nil)))))
            (let ((ghostherd-memory-server-directory nil)
                  (load-file-name nil)
                  (buffer-file-name nil))
              (should (file-equal-p
                       (ghostherd-memory--server-directory)
                       (expand-file-name "server" src))))))
      (delete-directory src t)
      (delete-directory build t))))

(ert-deftest ghostherd-test-memory-server-next-to-build-elc ()
  "ecloud-style `:files (\"server\")` puts main.py beside the .elc."
  (let* ((build (make-temp-file "gh-build-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "server" build) t)
          (write-region "pass\n" nil (expand-file-name "server/main.py" build))
          (write-region "" nil (expand-file-name "ghostherd.elc" build))
          (cl-letf (((symbol-function 'locate-library)
                     (lambda (name &rest _)
                       (when (equal name "ghostherd")
                         (expand-file-name "ghostherd.elc" build)))))
            (let ((ghostherd-memory-server-directory nil)
                  (load-file-name nil)
                  (buffer-file-name nil))
              (should (file-equal-p
                       (ghostherd-memory--server-directory)
                       (expand-file-name "server" build))))))
      (delete-directory build t))))

(ert-deftest ghostherd-test-memory-health-url-matches-rpc ()
  (let ((ghostherd-memory-host "127.0.0.1")
        (ghostherd-memory-port 49152)
        (ghostherd-memory--port 49160))
    (should (equal (ghostherd-memory--rpc-url)
                   "http://127.0.0.1:49160/jsonrpc"))
    (should (equal (ghostherd-memory--health-url)
                   "http://127.0.0.1:49160/health"))
    (should (equal (ghostherd-memory--rpc-url 49153)
                   "http://127.0.0.1:49153/jsonrpc"))))

(ert-deftest ghostherd-test-memory-port-candidates-start-high ()
  (let ((ghostherd-memory-port 49152)
        (ghostherd-memory-port-tries 32))
    (should (equal (car (ghostherd-memory--port-candidates)) 49152))
    (should (equal (car (last (ghostherd-memory--port-candidates))) 49183))
    (should (< 8766 (car (ghostherd-memory--port-candidates))))))

(ert-deftest ghostherd-test-memory-allocate-skips-occupied ()
  "A listener that is not ours is not a bind target."
  (let ((ghostherd-memory-port 49152)
        (ghostherd-memory-port-tries 4)
        (busy '(49152 49153)))
    (cl-letf (((symbol-function 'ghostherd-memory--listening-p)
               (lambda (port) (memq port busy))))
      (should (equal (ghostherd-memory--allocate-port) 49154)))))

(ert-deftest ghostherd-test-memory-find-running-requires-our-health ()
  "HTTP 200 on a busy port is not enough — ecloud is 200 too."
  (let ((ghostherd-memory-port 49152)
        (ghostherd-memory-port-tries 3))
    (cl-letf (((symbol-function 'ghostherd-memory--listening-p)
               (lambda (_) t))
              ((symbol-function 'ghostherd-memory--health-ours-p)
               (lambda (port) (= port 49153))))
      (should (equal (ghostherd-memory--find-running) 49153)))))

(ert-deftest ghostherd-test-memory-health-body-is-this-sidecar ()
  (should (ghostherd-memory--health-body-ours-p
           "{\"status\":\"ok\",\"service\":\"ghostherd-memory\"}"))
  (should-not (ghostherd-memory--health-body-ours-p
               "{\"status\":\"ok\"}"))
  (should-not (ghostherd-memory--health-body-ours-p
               "not json")))

(ert-deftest ghostherd-test-memory-build-request-is-jsonrpc-2 ()
  (let ((ghostherd-memory--request-id 0)
        (req (ghostherd-memory--build-request "memory_search"
                                              (list :query "posframe"))))
    (should (equal (plist-get req :jsonrpc) "2.0"))
    (should (equal (plist-get req :method) "memory_search"))
    (should (equal (plist-get (plist-get req :params) :query) "posframe"))
    (should (numberp (plist-get req :id)))))

(ert-deftest ghostherd-test-memory-utf8-unibyte-json ()
  "url.el leaves JSON unibyte; 你 must not display as \\344\\275\\240."
  (let* ((json "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"text\":\"你分析\"}}")
         (raw (encode-coding-string json 'utf-8)))
    (should (equal (plist-get (ghostherd-memory--parse-response raw) :text)
                   "你分析"))))

(ert-deftest ghostherd-test-memory-parse-result-and-error ()
  (let ((ok (ghostherd-memory--parse-response
             "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"ok\":true},\"error\":null}")))
    (should (eq (plist-get ok :ok) t)))
  (should-error
   (ghostherd-memory--parse-response
    "{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":{\"code\":-32601,\"message\":\"nope\"}}")))

(ert-deftest ghostherd-test-memory-menu-lists-search ()
  (should (equal (nth 2 (assoc "/" ghostherd-menu-choices))
                 'ghostherd-memory-search))
  (should (equal (nth 2 (assoc "v" ghostherd-menu-choices))
                 'ghostherd-memory-view))
  (should (equal (nth 2 (assoc "I" ghostherd-menu-choices))
                 'ghostherd-memory-import)))

(ert-deftest ghostherd-test-memory-source-label-uses-basename ()
  (should (string-match-p "chat_history.jsonl"
                          (ghostherd-memory--source-label
                           '(:agent "grok" :chunks 3 :project "/tmp/p"
                             :source_path "/tmp/p/sess/chat_history.jsonl")))))

(ert-deftest ghostherd-test-memory-cmd-import-force-flag ()
  "The CLI passes the string force, not a Lisp t, through emacsclient."
  (let ((got nil))
    (cl-letf (((symbol-function 'ghostherd-memory-ensure) #'ignore)
              ((symbol-function 'ghostherd-memory-request)
               (lambda (method params &optional _timeout)
                 (setq got (list method params))
                 '(:imported 0 :skipped 0 :sessions 0 :errors nil))))
      (ghostherd-cmd-memory-import "force")
      (should (equal (car got) "memory_import"))
      (should (eq (plist-get (cadr got) :force) t))
      (setq got nil)
      (ghostherd-cmd-memory-import)
      (should (equal (cadr got) nil)))))

(provide 'ghostherd-tests)
;;; ghostherd-tests.el ends here

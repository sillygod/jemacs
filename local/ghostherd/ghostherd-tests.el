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
;;         -L . -L ../xwapp -l ghostherd.el -l ghostherd-tests.el \
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
               (format "%s\t0\t\t\n" (ghostherd-session-host-id s)))
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

(ert-deftest ghostherd-test-sidebar-mode-map-has-help ()
  (should (eq (lookup-key ghostherd-sidebar-mode-map "?")
              #'ghostherd-sidebar-help)))

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
  "Its spinner line, known by its shape -- glyph, a word, …, the time --
and read where claude draws it, above the input box: the first line
there that is not indented.  Seen (2026-10-15): twelve turns since 10-01
read as done mid-turn, one quoting its spinner as the reason."
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
                    "✳ Moseying… (0s)\n" "✻ Ruminating… (2h 1m · thought for 5s)"))
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
          (ghostherd--log-disk-lines 0)
          (ghostherd--log-disk-screens 0)
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

(ert-deftest ghostherd-test-sidebar-names-an-heir-by-its-place-in-the-line ()
  "An heir is rd II in the sidebar, as on the herd page, the CLIs of its
name on hover; the first of a name is its name alone."
  (ghostherd-tests--with-herd ((ghostherd--lineage (make-hash-table :test 'equal)))
    (let* ((rd (ghostherd-tests--session :name "rd" :kind 'grok :backend 'fake))
           (first (ghostherd--sidebar-cell rd 'name 'idle 'default)))
      (should (equal first "rd"))
      (should-not (get-text-property 0 'help-echo first))
      (puthash (ghostherd-session-id rd) '(((kind . "claude")) ((kind . "agy"))) ghostherd--lineage)
      (let ((cell (ghostherd--sidebar-cell rd 'name 'idle 'default)))
        (should (equal cell "rd III"))
        (should (equal (get-text-property 0 'help-echo cell)
                       "rd III, the CLIs of its name: claude → agy → grok"))
        (should (equal (get-text-property 5 'help-echo cell) (get-text-property 0 'help-echo cell)))
        ;; The name in its state's face, the numeral bold in it too.
        (should (eq (get-text-property 0 'face cell) 'default))
        (should (equal (get-text-property 4 'face cell) '(bold default))))
      ;; The row drawn has it in its name column.
      (let ((ghostherd-sidebar-width 60)
            (ghostherd--sidebar-filter-project nil)
            (ghostherd--sidebar-query nil))
        (with-temp-buffer
          (ghostherd-sidebar-mode)
          (cl-letf (((symbol-function 'ghostherd--sidebar-available-width) (lambda () 60)))
            (ghostherd--sidebar-build-entries)
            (let* ((names (mapcar #'car (ghostherd--sidebar-visible-columns)))
                   (row (cadr (assoc (ghostherd-session-id rd) tabulated-list-entries))))
              (should (equal (aref row (seq-position names 'name)) "rd III")))))))))

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

(defmacro ghostherd-tests--with-projects (names &rest body)
  "Run BODY with NAMES bound to fresh real directories.
`file-equal-p\' is only dependable on files that exist, and both
the filter and its candidate list lean on it."
  (declare (indent 1) (debug t))
  `(let ,(mapcar (lambda (n)
                   `(,n (file-name-as-directory
                         (make-temp-file ,(format "gh-%s-" n) t))))
                 names)
     (unwind-protect (progn ,@body)
       ,@(mapcar (lambda (n) `(delete-directory ,n t)) names))))

(ert-deftest ghostherd-test-sidebar-project-table-counts-sessions ()
  "The candidates are the projects the herd is actually in."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-projects (a b)
      (ghostherd-tests--session :name "one" :id "one" :project a)
      (ghostherd-tests--session :name "two" :id "two" :project a)
      (ghostherd-tests--session :name "three" :id "three" :project b)
      (ghostherd-tests--session :name "loose" :id "loose")
      (let ((table (ghostherd--sidebar-project-table)))
        (should (= (length table) 2))
        (should (equal (sort (mapcar (lambda (c) (nth 2 c)) table) #'<)
                       '(1 2)))
        ;; A session with no project cannot be filtered to, so it is
        ;; not offered.
        (should (equal (sort (mapcar #'car table) #'string<)
                       (sort (list (ghostherd--abbreviate a)
                                   (ghostherd--abbreviate b))
                             #'string<)))))))

(ert-deftest ghostherd-test-sidebar-picker-offers-every-project ()
  "`s' must reach the other projects, not just the one it was opened over."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-projects (a b)
      (ghostherd-tests--session :name "one" :id "one" :project a)
      (ghostherd-tests--session :name "two" :id "two" :project b)
      (let ((ghostherd--sidebar-filter-project nil)
            (ghostherd--sidebar-picking nil))
        (cl-letf (((symbol-function 'ghostherd--sidebar-pick-redraw) #'ignore)
                  ((symbol-function 'ghostherd--set-picking)
                   (lambda (c) (setq ghostherd--sidebar-picking c)))
                  ((symbol-function 'ghostherd--sidebar-session-at-point)
                   (lambda () nil))
                  ((symbol-function 'ghostherd--sidebar-caller-project)
                   (lambda () a)))
          (ghostherd-sidebar-project-filter)
          (should (equal (mapcar #'car ghostherd--sidebar-picking)
                         (list ghostherd--sidebar-all-projects
                               (ghostherd--abbreviate a)
                               (ghostherd--abbreviate b))))
          ;; `All projects' counts the whole herd, not one project.
          (should (= (nth 2 (car ghostherd--sidebar-picking)) 2)))))))

(ert-deftest ghostherd-test-sidebar-picker-opens-on-row-under-point ()
  "The row under point is the one thing on screen that says which project."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-projects (a b)
      (ghostherd-tests--session :name "one" :id "one" :project a)
      (let ((two (ghostherd-tests--session :name "two" :id "two" :project b))
            (ghostherd--sidebar-filter-project nil)
            (ghostherd--sidebar-picking nil)
            (ghostherd--sidebar-pick-index 0))
        (cl-letf (((symbol-function 'ghostherd--sidebar-pick-redraw) #'ignore)
                  ((symbol-function 'ghostherd--set-picking)
                   (lambda (c) (setq ghostherd--sidebar-picking c)))
                  ((symbol-function 'ghostherd--sidebar-session-at-point)
                   (lambda () two))
                  ((symbol-function 'ghostherd--sidebar-caller-project)
                   (lambda () a)))
          (ghostherd-sidebar-project-filter)
          (should (equal (car (nth ghostherd--sidebar-pick-index
                                   ghostherd--sidebar-picking))
                         (ghostherd--abbreviate b))))))))

(ert-deftest ghostherd-test-sidebar-picker-confirm-applies-selection ()
  "RET filters to the highlighted row and closes the picker."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-projects (a b)
      (ghostherd-tests--session :name "one" :id "one" :project a)
      (ghostherd-tests--session :name "two" :id "two" :project b)
      (let ((ghostherd--sidebar-filter-project nil)
            (ghostherd--sidebar-picking (ghostherd--sidebar-pick-candidates))
            (ghostherd--sidebar-pick-index 0)
            (applied 'unset))
        (cl-letf (((symbol-function 'ghostherd--sidebar-pick-redraw) #'ignore)
                  ((symbol-function 'ghostherd--set-picking)
                   (lambda (c) (setq ghostherd--sidebar-picking c)))
                  ((symbol-function 'ghostherd--sidebar-apply-filter)
                   (lambda (root) (setq applied root))))
          (ghostherd-sidebar-pick-next)
          (ghostherd-sidebar-pick-confirm)
          (should-not ghostherd--sidebar-picking)
          (should (file-equal-p applied a))
          ;; Wrapping past the end lands back on `All projects', which
          ;; is how the picker clears the filter.
          (setq ghostherd--sidebar-picking (ghostherd--sidebar-pick-candidates)
                ghostherd--sidebar-pick-index 0)
          (ghostherd-sidebar-pick-previous)
          (ghostherd-sidebar-pick-next)
          (ghostherd-sidebar-pick-confirm)
          (should-not applied))))))

(ert-deftest ghostherd-test-sidebar-picker-opens-on-all-when-filtered ()
  "With a filter on, RET still just turns it off."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-projects (a)
      (ghostherd-tests--session :name "one" :id "one" :project a)
      (let ((ghostherd--sidebar-filter-project a)
            (ghostherd--sidebar-picking nil)
            (ghostherd--sidebar-pick-index 3))
        (cl-letf (((symbol-function 'ghostherd--sidebar-pick-redraw) #'ignore)
                  ((symbol-function 'ghostherd--set-picking)
                   (lambda (c) (setq ghostherd--sidebar-picking c))))
          (ghostherd-sidebar-project-filter)
          (should (= ghostherd--sidebar-pick-index 0))
          (should (equal (car (car ghostherd--sidebar-picking))
                         ghostherd--sidebar-all-projects)))))))

(ert-deftest ghostherd-test-sidebar-picker-swallows-stray-keys ()
  "Modal on purpose: `x' while picking must not kill the session at point."
  (should (eq (lookup-key ghostherd-sidebar-pick-map "x" t)
              #'ghostherd-sidebar-pick-ignore))
  (should (eq (lookup-key ghostherd-sidebar-pick-map (kbd "RET"))
              #'ghostherd-sidebar-pick-confirm))
  (should (eq (lookup-key ghostherd-sidebar-pick-map [escape])
              #'ghostherd-sidebar-pick-cancel))
  (should (eq (lookup-key ghostherd-sidebar-pick-map "3")
              #'ghostherd-sidebar-pick-digit)))

(ert-deftest ghostherd-test-sidebar-picker-draws-in-the-pane ()
  "The list draws in the overlay pane, not in a minibuffer prompt."
  (let* ((ghostherd-sidebar-width 72)
         (ghostherd--sidebar-target-width 72)
         (ghostherd--sidebar-picking
          (list (list ghostherd--sidebar-all-projects nil 3)
                (list "~/src/alpha/" "/src/alpha/" 2)
                (list "~/src/beta/" "/src/beta/" 1)))
         (ghostherd--sidebar-pick-index 1)
         (text (substring-no-properties (ghostherd--sidebar-pick-text)))
         (lines (split-string text "\n")))
    (should (string-match-p "Show project" (nth 0 lines)))
    (should (string-match-p "1  All projects" (nth 1 lines)))
    (should (string-match-p "3 in all" (nth 1 lines)))
    ;; The selected row is the marked one, and only it.
    (should (string-match-p "\\`› 2  ~/src/alpha/ +2 sessions\\'" (nth 2 lines)))
    (should (string-match-p "\\`  3  ~/src/beta/ +1 session\\'" (nth 3 lines)))
    (should (eq (get-text-property 0 'face (nth 2 (split-string
                                                   (ghostherd--sidebar-pick-text)
                                                   "\n")))
                'highlight))))

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

(ert-deftest ghostherd-test-return-event-p ()
  (should (ghostherd--return-event-p 'return))
  (should (ghostherd--return-event-p 'kp-enter))
  (should (ghostherd--return-event-p ?\r))
  (should (ghostherd--return-event-p ?\n))
  (should-not (ghostherd--return-event-p ?g))
  (should-not (ghostherd--return-event-p ?\C-g)))

(ert-deftest ghostherd-test-filter-map-letters-self-insert ()
  "Printable overlay keys must search while narrowing, not fire commands."
  (should (eq (lookup-key ghostherd-sidebar-filter-map "a")
              #'ghostherd-sidebar-filter-self-insert))
  (should (eq (lookup-key ghostherd-sidebar-filter-map "e")
              #'ghostherd-sidebar-filter-self-insert))
  (should (eq (lookup-key ghostherd-sidebar-filter-map "r")
              #'ghostherd-sidebar-filter-self-insert))
  (should (eq (lookup-key ghostherd-sidebar-filter-map "/")
              #'ghostherd-sidebar-filter-self-insert))
  (should (eq (lookup-key ghostherd-sidebar-filter-map "n")
              #'next-line))
  (should (eq (lookup-key ghostherd-sidebar-filter-map "p")
              #'previous-line))
  (should (eq (lookup-key ghostherd-sidebar-filter-map (kbd "RET"))
              #'ghostherd-sidebar-filter-confirm)))

(ert-deftest ghostherd-test-filter-confirm-keeps-query ()
  "RET while narrowing applies the query; it does not visit.
j/k and a second RET then belong to the overlay."
  (let ((ghostherd--sidebar-query "agy")
        (ghostherd--sidebar-filtering t)
        (visited nil))
    (cl-letf (((symbol-function 'ghostherd-sidebar-visit)
               (lambda () (interactive) (setq visited t))))
      (ghostherd-sidebar-filter-confirm)
      (should-not ghostherd--sidebar-filtering)
      (should (equal ghostherd--sidebar-query "agy"))
      (should-not visited)
      (should (eq (lookup-key ghostherd-sidebar-filter-map (kbd "RET"))
                  #'ghostherd-sidebar-filter-confirm))
      (should (eq (lookup-key ghostherd-sidebar-mode-map (kbd "RET"))
                  #'ghostherd-sidebar-visit)))))

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

(ert-deftest ghostherd-test-respawn-fresh-is-fresh ()
  "A recipe keeps the flags it was last started with: a continuing
respawn's `--continue', the `-r' of a resume.  Fresh leaves them off,
or agy-emacs-commit, respawned fresh, went on with its conversation;
continuing again does not say `--continue' twice."
  (ghostherd-tests--with-herd ()
    (let (spawned)
      (cl-letf (((symbol-function 'ghostherd-spawn)
                 (lambda (_kind &rest plist) (setq spawned plist) nil)))
        (pcase-dolist (`(,kind ,args ,fresh ,continued)
                       '((agy ("--effort" "high" "--continue") ("--effort" "high") ("--effort" "high" "--continue"))
                         (agy ("--conversation" "2f08522d" "--effort" "high") ("--effort" "high") ("--effort" "high" "--continue"))
                         (claude ("--model" "opus" "-r" "b25467a0") ("--model" "opus") ("--model" "opus" "--continue"))))
          (ghostherd-respawn (ghostherd-tests--session :name "a" :kind kind :args args) nil)
          (should (equal (plist-get spawned :args) fresh))
          (ghostherd-respawn (ghostherd-tests--session :name "a" :kind kind :args args) t)
          (should (equal (plist-get spawned :args) continued)))))))

;;; Its model, switched for this session only

(defconst ghostherd-tests--picker-models
  '(("Default (recommended)" "low" "medium" "high" "xhigh" "max")
    ("Opus 5.5" "low" "medium" "high" "xhigh" "max")
    ("Sonnet 5.5" "low" "medium" "high" "xhigh" "max")
    ("Haiku 5.5" "low" "medium" "high")
    ("Haiku 4.5"))
  "The fake claude's models, newest of a family first, and the efforts
each takes -- none for one without.")

(defvar ghostherd-tests--picker nil
  "The fake claude: (:model NAME :effort LEVEL :open BOOL :cursor N :at
\(ROW . LEVEL)... :said TEXT :sent KEYS :deaf BOOL).")

(defun ghostherd-tests--picker-get (key) (plist-get ghostherd-tests--picker key))
(defun ghostherd-tests--picker-set (key value)
  (setq ghostherd-tests--picker (plist-put ghostherd-tests--picker key value)))

(defun ghostherd-tests--picker-screen ()
  "The fake claude's screen: a conversation with a numbered list of its
own, and the picker under it while it is open."
  (let ((head "⏺ Done:\n1. Fixed the retry\n2. Added a test\n"))
    (if (not (ghostherd-tests--picker-get :open))
        (concat head (if (ghostherd-tests--picker-get :said)
                         (concat "❯ /model\n  ⎿  " (ghostherd-tests--picker-get :said) "\n")
                       "")
                "────\n❯ \n────\n")
      (let* ((cursor (ghostherd-tests--picker-get :cursor))
             (effort (cdr (assoc cursor (ghostherd-tests--picker-get :at)))))
        (concat head
                "   Select model\n   Switch between Claude models.  Your pick becomes the default.\n\n"
                (mapconcat (lambda (i)
                             (let ((label (car (nth (1- i) ghostherd-tests--picker-models))))
                               (format "   %s %d.  %-22s Best for something\n"
                                       (if (= i cursor) "❯" " ") i
                                       (if (equal label (ghostherd-tests--picker-get :model))
                                           (concat label " ✔")
                                         label))))
                           (number-sequence 1 (length ghostherd-tests--picker-models)) "")
                (if effort
                    (format "\n   ◉ %s effort ←/→ to adjust\n"
                            (if (equal effort "xhigh") "xHigh" (capitalize effort)))
                  "\n")
                "\n   Enter to set as default · s to use this session only · Esc to cancel\n")))))

(cl-defmethod ghostherd-backend-capture ((_backend (eql picker)) _session &optional _n)
  (ghostherd-tests--picker-screen))

(cl-defmethod ghostherd-backend-live-p ((_backend (eql picker)) _session) t)

(cl-defmethod ghostherd-backend-send-text ((_backend (eql picker)) _session text submit)
  (ghostherd-tests--picker-set :sent (append (ghostherd-tests--picker-get :sent)
                                             (list (if submit (list :submit text) text))))
  (when (and submit (equal text "/model") (not (ghostherd-tests--picker-get :deaf)))
    (let ((now (1+ (cl-position (ghostherd-tests--picker-get :model) ghostherd-tests--picker-models
                                :key #'car :test #'equal))))
      (ghostherd-tests--picker-set :open t)
      (ghostherd-tests--picker-set :cursor now)
      ;; The session's effort on its own model; another's default.
      (ghostherd-tests--picker-set
       :at (cl-loop for (_ . levels) in ghostherd-tests--picker-models
                    for i from 1
                    collect (cons i (cond ((null levels) nil)
                                          ((= i now) (ghostherd-tests--picker-get :effort))
                                          (t "medium"))))))))

(cl-defmethod ghostherd-backend-send-keys ((_backend (eql picker)) _session keys)
  (ghostherd-tests--picker-set :sent (append (ghostherd-tests--picker-get :sent) keys))
  (dolist (key keys)
    (when (ghostherd-tests--picker-get :open)
      (let* ((cursor (ghostherd-tests--picker-get :cursor))
             (levels (cdr (nth (1- cursor) ghostherd-tests--picker-models)))
             (at (assoc cursor (ghostherd-tests--picker-get :at)))
             (i (cl-position (cdr at) levels :test #'equal)))
        (pcase key
          ("down" (ghostherd-tests--picker-set :cursor (min (length ghostherd-tests--picker-models) (1+ cursor))))
          ("up" (ghostherd-tests--picker-set :cursor (max 1 (1- cursor))))
          ("right" (when i (setcdr at (nth (min (1- (length levels)) (1+ i)) levels))))
          ("left" (when i (setcdr at (nth (max 0 (1- i)) levels))))
          ("esc" (ghostherd-tests--picker-set :open nil)
                 (ghostherd-tests--picker-set :said (concat "Kept model as " (ghostherd-tests--picker-get :model))))
          ("s" (let ((model (car (nth (1- cursor) ghostherd-tests--picker-models))))
                 (ghostherd-tests--picker-set :open nil)
                 (ghostherd-tests--picker-set :model model)
                 (ghostherd-tests--picker-set :effort (cdr at))
                 (ghostherd-tests--picker-set
                  :said (concat "Set model to " model " for this session only"
                                (if (cdr at) (concat " with " (cdr at) " effort") ""))))))))))

(defmacro ghostherd-tests--with-picker (&rest body)
  "Run BODY with `s', an idle claude agent on the fake picker backend:
on Opus 5.5 at xhigh, its picker answering as claude's does."
  (declare (indent 0) (debug t))
  `(ghostherd-tests--with-herd ()
     (ghostherd-tests--with-log
       (let ((ghostherd-tests--picker (list :model "Opus 5.5" :effort "xhigh"))
             (ghostherd-model-switch-wait 1)
             (s (ghostherd-tests--session :name "rd" :kind 'claude :backend 'picker :state 'idle)))
         ,@body))))

(defun ghostherd-tests--switch (s model &optional effort)
  "Switch S to MODEL at EFFORT, and wait: (OK . WHAT)."
  (let (got)
    (ghostherd-switch-model s model effort (lambda (ok what) (setq got (cons ok what))))
    (with-timeout (10 (ert-fail "the switch never finished"))
      (while (not got) (accept-process-output nil 0.02)))
    got))

(ert-deftest ghostherd-test-model-picker-is-read-under-its-title ()
  "Its rows, the one it is on and that row's effort -- and not the
numbered list the conversation above it holds."
  (let ((ghostherd-tests--picker (list :model "Opus 5.5" :effort "xhigh" :open t :cursor 2
                                       :at '((1 . "medium") (2 . "xhigh") (3 . "medium") (4 . "medium") (5)))))
    (should (equal (ghostherd--model-picker (ghostherd-tests--picker-screen))
                   '(:rows ((1 . "Default (recommended)") (2 . "Opus 5.5") (3 . "Sonnet 5.5")
                            (4 . "Haiku 5.5") (5 . "Haiku 4.5"))
                     :cursor 2 :effort "xhigh")))
    (ghostherd-tests--picker-set :cursor 5)
    (should (null (plist-get (ghostherd--model-picker (ghostherd-tests--picker-screen)) :effort)))
    (ghostherd-tests--picker-set :open nil)
    (should-not (ghostherd--model-picker (ghostherd-tests--picker-screen))))
  ;; As claude draws it: a scrolled list, an older tick, a default effort.
  (should (equal (ghostherd--model-picker
                  (concat " Select model\n"
                          "     1.  Default (recommended)  Opus 5.5 · Best for everyday\n"
                          " ❯ 2. Opus ✓                    Opus 5.5 · Best for everyday\n"
                          "   ↓ 10. Opus 4.8               Best for everyday\n"
                          "      … +3 models\n"
                          "   ◐ Medium effort (default) ←/→ to adjust\n"
                          "   Enter to set as default · s to use this session only · Esc to cancel\n"))
                 '(:rows ((1 . "Default (recommended)") (2 . "Opus") (10 . "Opus 4.8"))
                   :cursor 2 :effort "medium"))))

(ert-deftest ghostherd-test-picker-row-is-a-familys-newest ()
  (let ((rows '((1 . "Default (recommended)") (2 . "Opus 5.5") (4 . "Sonnet 5.5")
                (8 . "Opus 5") (9 . "Haiku 4.5"))))
    (should (eql (ghostherd--picker-row rows "Opus") 2))
    (should (eql (ghostherd--picker-row rows "default") 1))
    (should (eql (ghostherd--picker-row rows "Haiku") 9))
    (should-not (ghostherd--picker-row rows "Fable"))
    (should-not (ghostherd--picker-row '((1 . "Opusplan")) "Opus"))))

(ert-deftest ghostherd-test-model-said-is-the-last-models ()
  (should (equal (ghostherd--model-said
                  (concat "❯ /model\n  ⎿  Set model to Haiku 5.5 for this session only\n"
                          "⏺ ok\n❯ /model\n  ⎿  Set model to Sonnet 5.5 for this session only with high effort\n"
                          "────\n❯ \n"))
                 "Set model to Sonnet 5.5 for this session only with high effort"))
  (should (equal (ghostherd--model-said "❯ /model\n  ⎿  Kept model as Opus 5.5\n") "Kept model as Opus 5.5"))
  (should-not (ghostherd--model-said "⏺ I will set model to Haiku\n❯ \n")))

(ert-deftest ghostherd-test-switch-model-drives-the-picker ()
  "`/model', down to the row, → to the effort, `s': this session alone,
the way you would -- and said in the log."
  (ghostherd-tests--with-picker
    (should (equal (ghostherd-tests--switch s "Sonnet" "high")
                   '(t . "Set model to Sonnet 5.5 for this session only with high effort")))
    (should (equal (ghostherd-tests--picker-get :sent) '((:submit "/model") "down" "right" "s")))
    (should (equal (list (ghostherd-tests--picker-get :model) (ghostherd-tests--picker-get :effort))
                   '("Sonnet 5.5" "high")))
    (should (equal (ghostherd-log-entry-text (car ghostherd--log))
                   "Set model to Sonnet 5.5 for this session only with high effort"))
    (should-not (gethash (ghostherd-session-id s) ghostherd--switching))
    ;; Up, and down the efforts; none asked, none touched.
    (ghostherd-tests--picker-set :sent nil)
    (should (car (ghostherd-tests--switch s "Default" "low")))
    (should (equal (ghostherd-tests--picker-get :sent) '((:submit "/model") "up" "up" "left" "s")))
    (ghostherd-tests--picker-set :sent nil)
    (should (car (ghostherd-tests--switch s "Opus")))
    (should (equal (ghostherd-tests--picker-get :sent) '((:submit "/model") "down" "s")))
    (should (equal (ghostherd-tests--picker-get :effort) "medium"))))

(ert-deftest ghostherd-test-switch-model-takes-what-a-model-has ()
  "An effort past a model's top stops at its top, and one without an
effort is switched all the same: what it set is what it says."
  (ghostherd-tests--with-picker
    (should (equal (ghostherd-tests--switch s "Haiku" "max")
                   '(t . "Set model to Haiku 5.5 for this session only with high effort")))
    (should (equal (ghostherd-tests--switch s "Default" "high")
                   '(t . "Set model to Default (recommended) for this session only with high effort")))
    (ghostherd-tests--picker-set :sent nil)
    ;; Haiku 4.5, the second Haiku, is not a name the herd picks by;
    ;; a picker without the name is let go of.
    (should (equal (ghostherd-tests--switch s "Fable")
                   '(nil . "its picker has no Fable")))
    (should (equal (ghostherd-tests--picker-get :sent) '((:submit "/model") "esc")))
    (should-not (ghostherd-tests--picker-get :open))
    (should (equal (ghostherd-log-entry-text (car ghostherd--log))
                   "model not switched: its picker has no Fable"))))

(ert-deftest ghostherd-test-switch-model-gives-up-on-a-picker-that-does-not-open ()
  (ghostherd-tests--with-picker
    (ghostherd-tests--picker-set :deaf t)
    (should (equal (ghostherd-tests--switch s "Sonnet")
                   '(nil . "its model picker did not open")))
    ;; Nothing pressed into a screen it could not read.
    (should (equal (ghostherd-tests--picker-get :sent) '((:submit "/model"))))
    (should-not (gethash (ghostherd-session-id s) ghostherd--switching))))

(ert-deftest ghostherd-test-switch-model-refuses-what-it-should-not-touch ()
  (ghostherd-tests--with-picker
    (setf (ghostherd-session-state s) 'working)
    (should-error (ghostherd-switch-model s "Sonnet") :type 'user-error)
    ;; Waiting on its sub-agents: its box is free.
    (setf (ghostherd-session-state s) 'waiting)
    ;; Words of yours in its input box -- a suggestion is not yours.
    (puthash (ghostherd-session-id s) '(:input "half a thought") ghostherd--feet)
    (should-error (ghostherd-switch-model s "Sonnet") :type 'user-error)
    (puthash (ghostherd-session-id s) '(:input "run the tests" :suggestion "run the tests") ghostherd--feet)
    (should (car (ghostherd-tests--switch s "Sonnet")))
    (remhash (ghostherd-session-id s) ghostherd--feet)
    (should-error (ghostherd-switch-model s "GPT") :type 'user-error)
    (should-error (ghostherd-switch-model s "Sonnet" "ludicrous") :type 'user-error)
    (puthash (ghostherd-session-id s) t ghostherd--switching)
    (should-error (ghostherd-switch-model s "Sonnet") :type 'user-error)
    (remhash (ghostherd-session-id s) ghostherd--switching)
    (let ((g (ghostherd-tests--session :name "g" :kind 'grok :backend 'picker :state 'idle)))
      ;; grok has a picker now, but "Sonnet" is not one of its models.
      (should-error (ghostherd-switch-model g "Sonnet") :type 'user-error))
    (should (equal (ghostherd-tests--picker-get :sent) '((:submit "/model") "down" "s")))))

;;; Its model -- grok, by command

(defvar ghostherd-tests--grok nil
  "The fake grok: (:model DISPLAY :effort LEVEL :sent KEYS :deaf BOOL).")
(defun ghostherd-tests--grok-get (k) (plist-get ghostherd-tests--grok k))
(defun ghostherd-tests--grok-set (k v) (setq ghostherd-tests--grok (plist-put ghostherd-tests--grok k v)))

(defconst ghostherd-tests--grok-labels
  '(("grok-4.7" . "Grok 4.7") ("grok-4.7-build-fast" . "Grok 4.7 Fast")
    ("grok-4.6" . "Grok 4.6") ("grok-4.5" . "Grok 4.5"))
  "The fake grok's id -> the name its border shows, as the spec's `:labels'.")

(defun ghostherd-tests--grok-screen ()
  "The fake grok's screen: its model and effort in the input box's border."
  (format "⏺ working on it\n ╭──────────────────────────────╮\n │ ❯                            │\n ╰───────── %s (%s) · always-approve ─╯\n Ctrl+x:shortcuts\n"
          (ghostherd-tests--grok-get :model) (ghostherd-tests--grok-get :effort)))

(cl-defmethod ghostherd-backend-capture ((_backend (eql grok-cmd)) _session &optional _n)
  (ghostherd-tests--grok-screen))
(cl-defmethod ghostherd-backend-live-p ((_backend (eql grok-cmd)) _session) t)
(cl-defmethod ghostherd-backend-send-text ((_backend (eql grok-cmd)) _session text submit)
  (ghostherd-tests--grok-set :sent (append (ghostherd-tests--grok-get :sent)
                                           (list (if submit (list :submit text) text))))
  (when (and submit (not (ghostherd-tests--grok-get :deaf))
             (string-match "\\`/model[[:blank:]]+\\(.+?\\)\\(?:[[:blank:]]+\\(low\\|medium\\|high\\|xhigh\\)\\)?[[:blank:]]*\\'" text))
    (let ((disp (cdr (assoc (match-string 1 text) ghostherd-tests--grok-labels)))
          (eff (match-string 2 text)))
      (when disp
        (ghostherd-tests--grok-set :model disp)
        (when eff (ghostherd-tests--grok-set :effort eff))))))
(cl-defmethod ghostherd-backend-send-keys ((_backend (eql grok-cmd)) _session keys)
  (ghostherd-tests--grok-set :sent (append (ghostherd-tests--grok-get :sent) keys)))

(defmacro ghostherd-tests--with-grok (&rest body)
  "Run BODY with `s', an idle grok agent on the fake command backend, on
Grok 4.7 at xhigh."
  (declare (indent 0) (debug t))
  `(ghostherd-tests--with-herd ()
     (ghostherd-tests--with-log
       (let ((ghostherd-tests--grok (list :model "Grok 4.7" :effort "xhigh"))
             (ghostherd-model-switch-wait 1)
             (s (ghostherd-tests--session :name "rd" :kind 'grok :backend 'grok-cmd :state 'idle)))
         ,@body))))

(ert-deftest ghostherd-test-grok-model-line-reads-the-border ()
  "The model and effort from grok's border, the last one -- a model
list's \"(current)\" is not an effort and is passed over."
  (should (equal (ghostherd--grok-model-line " ╰── Grok 4.6 (medium) · always-approve ─╯\n")
                 '("Grok 4.6" . "medium")))
  (should (equal (ghostherd--grok-model-line
                  "   ❯ Grok 4.7 (current) frontier\n ╰── Grok 4.5 (high) · always-approve ─╯\n")
                 '("Grok 4.5" . "high")))
  (should-not (ghostherd--grok-model-line "nothing on the screen\n")))

(ert-deftest ghostherd-test-grok-switches-by-command ()
  "`/model ID EFFORT' typed as one line; the border read back, and said
to have become the default."
  (ghostherd-tests--with-grok
    (should (equal (ghostherd-tests--switch s "grok-4.6" "high")
                   '(t . "Set model to Grok 4.6 with high effort (now its default)")))
    (should (equal (ghostherd-tests--grok-get :sent) '((:submit "/model grok-4.6 high"))))
    (should (equal (list (ghostherd-tests--grok-get :model) (ghostherd-tests--grok-get :effort))
                   '("Grok 4.6" "high")))
    (should (equal (ghostherd-log-entry-text (car ghostherd--log))
                   "Set model to Grok 4.6 with high effort (now its default)"))
    ;; No effort asked: the model changes, the effort is left as it was.
    (ghostherd-tests--grok-set :sent nil)
    (should (equal (ghostherd-tests--switch s "grok-4.5")
                   '(t . "Set model to Grok 4.5 with high effort (now its default)")))
    (should (equal (ghostherd-tests--grok-get :sent) '((:submit "/model grok-4.5"))))))

(ert-deftest ghostherd-test-grok-gives-up-when-the-model-does-not-change ()
  (ghostherd-tests--with-grok
    (ghostherd-tests--grok-set :deaf t)
    (should (equal (ghostherd-tests--switch s "grok-4.6")
                   '(nil . "it did not switch to Grok 4.6")))
    (should-not (gethash (ghostherd-session-id s) ghostherd--switching))))

;;; Its model -- agy, by its search picker

(defconst ghostherd-tests--agy-models
  '(("Gemini 4 Argon" "low" "medium" "high")
    ("Gemini 3.8 Flash" "low" "medium" "high")
    ("Claude Opus 5.5" "low" "medium" "high")
    ("Claude Sonnet 5.5" "low" "medium" "high")
    ("GPT-OSS 120B" "medium"))
  "The fake agy's models and the efforts each offers.")

(defvar ghostherd-tests--agy nil
  "The fake agy: (:model NAME :effort LEVEL :open BOOL :search STR :cursor
NAME :at (NAME . LEVEL)... :said TEXT :sent KEYS :deaf BOOL).")
(defun ghostherd-tests--agy-get (k) (plist-get ghostherd-tests--agy k))
(defun ghostherd-tests--agy-set (k v) (setq ghostherd-tests--agy (plist-put ghostherd-tests--agy k v)))

(defun ghostherd-tests--agy-efforts (model) (cdr (assoc model ghostherd-tests--agy-models)))
(defun ghostherd-tests--agy-start-effort (model)
  "Where MODEL's slider starts: its session effort when it is the live
model, else medium, or its only step."
  (if (equal model (ghostherd-tests--agy-get :model))
      (ghostherd-tests--agy-get :effort)
    (let ((es (ghostherd-tests--agy-efforts model)))
      (if (member "medium" es) "medium" (car es)))))

(defun ghostherd-tests--agy-rows ()
  "The models whose name holds the search, in order."
  (let ((q (downcase (or (ghostherd-tests--agy-get :search) ""))))
    (seq-filter (lambda (m) (string-search q (downcase (car m)))) ghostherd-tests--agy-models)))

(defun ghostherd-tests--agy-slider (effort efforts)
  "agy's slider for EFFORT among EFFORTS: ● before it, ◉ on it, ○ after."
  (let ((i (cl-position effort efforts :test #'equal)))
    (concat "  Effort  ◂  "
            (mapconcat (lambda (e) (let ((j (cl-position e efforts :test #'equal)))
                                     (cond ((= j i) "◉") ((< j i) "●") (t "○"))))
                       efforts "━━━━━━")
            "  ▸")))

(defun ghostherd-tests--agy-screen ()
  "The fake agy's screen: the prompt, or the Switch Model picker over it."
  (if (not (ghostherd-tests--agy-get :open))
      (concat "  Antigravity CLI\n"
              (if (ghostherd-tests--agy-get :said)
                  (concat "> /model\n  ⎿  " (ghostherd-tests--agy-get :said) "\n") "")
              "────\n>\n────\n? for shortcuts    "
              (ghostherd-tests--agy-get :model) " · " (downcase (ghostherd-tests--agy-get :effort)) "\n")
    (let* ((rows (ghostherd-tests--agy-rows))
           (cursor (ghostherd-tests--agy-get :cursor))
           (ceff (cdr (assoc cursor (ghostherd-tests--agy-get :at))))
           (efforts (ghostherd-tests--agy-efforts cursor)))
      (concat "Switch Model\n\n"
              "  Search: " (or (ghostherd-tests--agy-get :search) "") "\n"
              "          ────────────────────\n\n"
              (mapconcat (lambda (m)
                           (format "%s %s%s"
                                   (if (equal (car m) cursor) ">" " ") (car m)
                                   (if (equal (car m) (ghostherd-tests--agy-get :model)) "      (current)" "")))
                         rows "\n")
              "\n\n"
              (ghostherd-tests--agy-slider ceff efforts) "\n"
              "          " (string-join efforts "          ") "\n"
              (format "  [1-%d of %d items]\n" (length rows) (length rows))
              "Keyboard: ↑/↓ Navigate  ←/→ Effort  enter Select  esc Go Back\n"))))

(cl-defmethod ghostherd-backend-capture ((_backend (eql agy-picker)) _session &optional _n)
  (ghostherd-tests--agy-screen))
(cl-defmethod ghostherd-backend-live-p ((_backend (eql agy-picker)) _session) t)
(cl-defmethod ghostherd-backend-send-text ((_backend (eql agy-picker)) _session text submit)
  (ghostherd-tests--agy-set :sent (append (ghostherd-tests--agy-get :sent)
                                          (list (if submit (list :submit text) text))))
  (cond
   ((and submit (equal text "/model") (not (ghostherd-tests--agy-get :deaf)))
    (ghostherd-tests--agy-set :open t)
    (ghostherd-tests--agy-set :search "")
    (ghostherd-tests--agy-set :cursor (ghostherd-tests--agy-get :model))
    (ghostherd-tests--agy-set
     :at (mapcar (lambda (m) (cons (car m) (ghostherd-tests--agy-start-effort (car m))))
                 ghostherd-tests--agy-models)))
   ((and (not submit) (ghostherd-tests--agy-get :open))
    (ghostherd-tests--agy-set :search text)
    (ghostherd-tests--agy-set :cursor (car (car (ghostherd-tests--agy-rows)))))))
(cl-defmethod ghostherd-backend-send-keys ((_backend (eql agy-picker)) _session keys)
  (ghostherd-tests--agy-set :sent (append (ghostherd-tests--agy-get :sent) keys))
  (dolist (key keys)
    (when (ghostherd-tests--agy-get :open)
      (let* ((cursor (ghostherd-tests--agy-get :cursor))
             (at (assoc cursor (ghostherd-tests--agy-get :at)))
             (efforts (and cursor (ghostherd-tests--agy-efforts cursor)))
             (i (and at (cl-position (cdr at) efforts :test #'equal))))
        (pcase key
          ("right" (when i (setcdr at (nth (min (1- (length efforts)) (1+ i)) efforts))))
          ("left" (when i (setcdr at (nth (max 0 (1- i)) efforts))))
          ("return"
           (when cursor
             (ghostherd-tests--agy-set :model cursor)
             (ghostherd-tests--agy-set :effort (cdr at))
             (ghostherd-tests--agy-set :open nil)
             (ghostherd-tests--agy-set :said (format "Model set to %s (%s)" cursor (capitalize (cdr at))))))
          ("esc"
           (if (not (string-empty-p (or (ghostherd-tests--agy-get :search) "")))
               (progn (ghostherd-tests--agy-set :search "")
                      (ghostherd-tests--agy-set :cursor (ghostherd-tests--agy-get :model)))
             (ghostherd-tests--agy-set :open nil)
             (ghostherd-tests--agy-set :said "Exited /model command"))))))))

(defmacro ghostherd-tests--with-agy (&rest body)
  "Run BODY with `s', an idle agy agent on the fake picker backend, on
Gemini 3.8 Flash at high."
  (declare (indent 0) (debug t))
  `(ghostherd-tests--with-herd ()
     (ghostherd-tests--with-log
       (let ((ghostherd-tests--agy (list :model "Gemini 3.8 Flash" :effort "high"))
             (ghostherd-model-switch-wait 1)
             (s (ghostherd-tests--session :name "ag" :kind 'agy :backend 'agy-picker :state 'idle)))
         ,@body))))

(ert-deftest ghostherd-test-agy-picker-reads-rows-cursor-and-effort ()
  "The visible rows, the `>' cursor, the slider's steps and where its ◉
sits -- and a model that offers one effort shows only it."
  (let ((ghostherd-tests--agy
         (list :model "Gemini 3.8 Flash" :effort "high" :open t :search "" :cursor "Claude Opus 5.5"
               :at '(("Gemini 4 Argon" . "medium") ("Gemini 3.8 Flash" . "high")
                     ("Claude Opus 5.5" . "medium") ("Claude Sonnet 5.5" . "medium")
                     ("GPT-OSS 120B" . "medium")))))
    (let ((p (ghostherd--agy-picker (ghostherd-tests--agy-screen))))
      (should (equal (plist-get p :rows)
                     '("Gemini 4 Argon" "Gemini 3.8 Flash" "Claude Opus 5.5"
                       "Claude Sonnet 5.5" "GPT-OSS 120B")))
      (should (equal (plist-get p :cursor) "Claude Opus 5.5"))
      (should (equal (plist-get p :efforts) '("low" "medium" "high")))
      (should (equal (plist-get p :effort) "medium")))
    (ghostherd-tests--agy-set :search "GPT")
    (ghostherd-tests--agy-set :cursor "GPT-OSS 120B")
    (let ((p (ghostherd--agy-picker (ghostherd-tests--agy-screen))))
      (should (equal (plist-get p :rows) '("GPT-OSS 120B")))
      (should (equal (plist-get p :efforts) '("medium")))
      (should (equal (plist-get p :effort) "medium")))
    (ghostherd-tests--agy-set :open nil)
    (should-not (ghostherd--agy-picker (ghostherd-tests--agy-screen)))))

(ert-deftest ghostherd-test-agy-said-is-the-last ()
  (should (equal (ghostherd--agy-said
                  "x\n> /model\n  ⎿  Model set to Claude Opus 5.5 (High)\n>\n")
                 "Model set to Claude Opus 5.5 (High)"))
  (should (equal (ghostherd--agy-said "  ⎿  Exited /model command\n") "Exited /model command"))
  (should-not (ghostherd--agy-said "nothing happened here\n")))

(ert-deftest ghostherd-test-agy-switches-by-search-and-slider ()
  "`/model', the name typed to pare the list to one, → to the effort,
Enter; said to have become the default."
  (ghostherd-tests--with-agy
    (should (equal (ghostherd-tests--switch s "Claude Opus 5.5" "high")
                   '(t . "Model set to Claude Opus 5.5 (High) (now its default)")))
    (should (equal (ghostherd-tests--agy-get :sent)
                   '((:submit "/model") "Claude Opus 5.5" "right" "return")))
    (should (equal (list (ghostherd-tests--agy-get :model) (ghostherd-tests--agy-get :effort))
                   '("Claude Opus 5.5" "high")))
    (should (equal (ghostherd-log-entry-text (car ghostherd--log))
                   "Model set to Claude Opus 5.5 (High) (now its default)"))))

(ert-deftest ghostherd-test-agy-takes-the-effort-a-model-has ()
  "An effort a model does not offer leaves it where it is; no effort
asked, it is switched at the model's own."
  (ghostherd-tests--with-agy
    (should (equal (ghostherd-tests--switch s "GPT-OSS 120B" "high")
                   '(t . "Model set to GPT-OSS 120B (Medium) (now its default)")))
    (should (equal (ghostherd-tests--agy-get :sent)
                   '((:submit "/model") "GPT-OSS 120B" "return")))
    (ghostherd-tests--agy-set :sent nil)
    (should (car (ghostherd-tests--switch s "Gemini 4 Argon")))
    (should (equal (ghostherd-tests--agy-get :model) "Gemini 4 Argon"))))

(ert-deftest ghostherd-test-agy-gives-up-on-a-picker-that-does-not-open ()
  (ghostherd-tests--with-agy
    (ghostherd-tests--agy-set :deaf t)
    (should (equal (ghostherd-tests--switch s "Claude Opus 5.5")
                   '(nil . "its model picker did not open")))
    (should (equal (ghostherd-tests--agy-get :sent) '((:submit "/model"))))
    (should-not (gethash (ghostherd-session-id s) ghostherd--switching))))

(ert-deftest ghostherd-test-agy-model-line-reads-the-footer ()
  "The model and effort from agy's footer, the last one, whether or not
more follows the effort."
  (should (equal (ghostherd--agy-model-line "? for shortcuts        Claude Opus 5.5 · high\n")
                 '("Claude Opus 5.5" . "high")))
  (should (equal (ghostherd--agy-model-line "Gemini 3.8 Flash · high · AI: Out of credits\n")
                 '("Gemini 3.8 Flash" . "high")))
  (should-not (ghostherd--agy-model-line "nothing on the footer\n")))

(ert-deftest ghostherd-test-meter-falls-back-to-the-screen ()
  "A CLI the sidecar reads no meter for shows the model its own screen
does -- grok's border, agy's footer -- so its chip is not left bare."
  (ghostherd-tests--with-herd ((ghostherd-memory-page--meters (make-hash-table :test 'equal)))
    (let ((g (ghostherd-tests--session :name "rd" :kind 'grok :state 'idle))
          (a (ghostherd-tests--session :name "ag" :kind 'agy :state 'idle)))
      (puthash (ghostherd-session-id g) " ╰── Grok 4.6 (high) · always-approve ─╯\n" ghostherd--screens)
      (puthash (ghostherd-session-id a) "? for shortcuts    Gemini 3.8 Flash · medium\n" ghostherd--screens)
      (should (equal (ghostherd-memory-page--meter g)
                     '(:model "Grok 4.6" :effort "high" :tokens nil :window nil
                       :pending :json-false :compacted :json-false)))
      (should (equal (ghostherd-memory-page--meter a)
                     '(:model "Gemini 3.8 Flash" :effort "medium" :tokens nil :window nil
                       :pending :json-false :compacted :json-false)))
      ;; No screen and no transcript meter: none, so the bare button stands.
      (remhash (ghostherd-session-id g) ghostherd--screens)
      (should (eq (ghostherd-memory-page--meter g) :json-false)))))

(ert-deftest ghostherd-test-meter-merges-sidecar-tokens-with-screen-model ()
  "The sidecar gives grok/agy their context tokens (and grok its window);
the model and effort come off the screen.  Merged into one meter: grok
with a window for a bar, agy with the number alone."
  (ghostherd-tests--with-herd ((ghostherd-memory-page--meters (make-hash-table :test 'equal)))
    (let ((g (ghostherd-tests--session :name "rd" :kind 'grok :state 'idle))
          (a (ghostherd-tests--session :name "ag" :kind 'agy :state 'idle)))
      (puthash (ghostherd-session-id g) " ╰── Grok 4.6 (high) · always-approve ─╯\n" ghostherd--screens)
      (puthash (ghostherd-session-id a) "? for shortcuts    Gemini 3.8 Flash · medium\n" ghostherd--screens)
      (puthash "rd" (cons "s1" '(:model "" :effort "" :tokens 163889 :window 256000
                                 :pending :json-false :compacted :json-false))
               ghostherd-memory-page--meters)
      (should (equal (ghostherd-memory-page--meter g)
                     '(:model "Grok 4.6" :effort "high" :tokens 163889 :window 256000
                       :pending :json-false :compacted :json-false)))
      (puthash "ag" (cons "s2" '(:model "" :effort "" :tokens 189225 :window nil
                                 :pending :json-false :compacted :json-false))
               ghostherd-memory-page--meters)
      (should (equal (ghostherd-memory-page--meter a)
                     '(:model "Gemini 3.8 Flash" :effort "medium" :tokens 189225 :window nil
                       :pending :json-false :compacted :json-false))))))

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
      (cl-letf (((symbol-function 'ghostherd-interrupt)
                 (lambda (s) (setq got s))))
        (let ((out (ghostherd--herd-run-command
                    '(:id "1" :op "interrupt" :session "a"))))
          (should (equal got "a"))
          (should (equal (plist-get out :id) "1"))
          (should (equal (plist-get out :text) "interrupted")))))))

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

(defmacro ghostherd-tests--with-asks (&rest body)
  "BODY in a clean herd with ask state of its own.
Sidecar requests are recorded in `requests' as (METHOD PARAMS), oldest
first, and the client is at /p/bin/herd."
  (declare (indent 0))
  `(ghostherd-tests--with-herd
       ((ghostherd--asks-open (make-hash-table :test 'equal))
        (ghostherd--asks-settled (make-hash-table :test 'equal))
        (ghostherd--herd-asks nil)
        (ghostherd--log nil)
        (requests nil))
     (cl-letf (((symbol-function 'ghostherd-herd-client) (lambda () "/p/bin/herd"))
               ((symbol-function 'ghostherd-memory-request-async)
                (lambda (method _cb &optional params _err)
                  (setq requests (append requests (list (list method params)))))))
       ,@body)))

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
        (should (string-suffix-p ",\nand any one of them 4 times at most." text))
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
                              (ghostherd-log-entry-text (car ghostherd--log)))))))

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

;;; What agents say about themselves

(defconst ghostherd-tests--conv "c0ffee00-1111-2222-3333-444455556666")

(ert-deftest ghostherd-test-hook-reports-go-through-report ()
  "A hook's state keeps the report's TTL and authority: it is a report."
  (ghostherd-tests--with-herd ((ghostherd--log nil))
    (let ((s (ghostherd-tests--session :name "agy-a" :kind 'agy :state 'idle)))
      (cl-letf (((symbol-function 'run-with-timer) #'ignore))
        (ghostherd--herd-take-reports
         '((:session "agy-a" :state "blocked" :reason "Approve run_command?")
           (:session "agy-a" :state "sleeping" :reason "")
           (:session "nobody" :state "done" :reason ""))))
      (should (equal (seq-take (gethash "agy-a" ghostherd--reports) 2)
                     '(blocked "Approve run_command?")))
      (should (equal (ghostherd--fresh-report s) '(blocked . "Approve run_command?")))
      (cl-letf (((symbol-function 'run-with-timer) #'ignore))
        (ghostherd--herd-take-reports '((:session "agy-a" :state "auto" :reason ""))))
      (should-not (gethash "agy-a" ghostherd--reports)))))

(ert-deftest ghostherd-test-hook-links-name-the-conversation ()
  (ghostherd-tests--with-herd ((ghostherd--herd-links (make-hash-table :test 'equal)))
    (let ((s (ghostherd-tests--session :name "claude-a" :kind 'claude)))
      (ghostherd--herd-take-links
       `((:session "claude-a" :cli "claude" :conversation ,ghostherd-tests--conv
                   :transcript "/t.jsonl" :last_head "Done." :last_ms 1.0)))
      (should (equal (ghostherd-session-conversation s) ghostherd-tests--conv))
      (should (equal (plist-get (ghostherd--link s) :last_head) "Done."))
      (ghostherd--herd-take-links '((:session "claude-a" :cli "claude" :conversation "")))
      (should-not (ghostherd-session-conversation s)))))

(ert-deftest ghostherd-test-respawn-resumes-its-own-conversation ()
  "`--continue' takes the newest conversation in the directory -- with two
claudes in one project, possibly the other's.  The hooks know which."
  (ghostherd-tests--with-herd ((ghostherd--herd-links (make-hash-table :test 'equal)))
    (let ((s (ghostherd-tests--session :name "claude-a" :kind 'claude
                                       :args '("--model" "opus" "-r" "old-one" "--fork-session")))
          spawned)
      (ghostherd--herd-take-links
       `((:session "claude-a" :cli "claude" :conversation ,ghostherd-tests--conv)))
      (cl-letf (((symbol-function 'ghostherd-spawn)
                 (lambda (_kind &rest plist) (setq spawned plist) nil)))
        (ghostherd-respawn s t))
      (should (equal (plist-get spawned :args)
                     (list "--model" "opus" "-r" ghostherd-tests--conv))))))

(ert-deftest ghostherd-test-respawn-falls-back-to-continue ()
  (ghostherd-tests--with-herd ((ghostherd--herd-links (make-hash-table :test 'equal)))
    (let ((s (ghostherd-tests--session :name "claude-a" :kind 'claude :args '("--model" "opus")))
          spawned)
      (cl-letf (((symbol-function 'ghostherd-spawn)
                 (lambda (_kind &rest plist) (setq spawned plist) nil)))
        (ghostherd-respawn s t))
      (should (equal (plist-get spawned :args) '("--model" "opus" "--continue"))))))

(ert-deftest ghostherd-test-resumed-by-trusts-the-hooks ()
  "An agent that got onto a conversation by itself is found by its hooks;
one whose hooks name another conversation is not found by stale args."
  (ghostherd-tests--with-herd ((ghostherd--herd-links (make-hash-table :test 'equal)))
    (ghostherd-tests--session :name "claude-a" :kind 'claude :args nil)
    (ghostherd-tests--session :name "claude-b" :kind 'claude :args '("-r" "id-1"))
    (ghostherd--herd-take-links
     '((:session "claude-a" :conversation "id-1")
       (:session "claude-b" :conversation "id-2")))
    (cl-letf (((symbol-function 'ghostherd--session-live-p) (lambda (_) t)))
      (should (equal (ghostherd-session-name (ghostherd--resumed-by "id-1")) "claude-a"))
      (should (equal (ghostherd-session-name (ghostherd--resumed-by "id-2")) "claude-b")))))

(ert-deftest ghostherd-test-explain-names-the-conversation ()
  (ghostherd-tests--with-herd ((ghostherd--herd-links (make-hash-table :test 'equal)))
    (let ((s (ghostherd-tests--session :name "agy-a" :kind 'agy :backend 'fake)))
      (setq ghostherd-tests--fake-screen "")
      (ghostherd--herd-take-links
       '((:session "agy-a" :cli "agy" :conversation "a-456" :last_head "Committed ee5fee6.")))
      (save-window-excursion (ghostherd-explain s))
      (with-current-buffer "*ghostherd explain*"
        (should (string-search "talks    a-456  (agy, by its hooks)" (buffer-string)))
        (should (string-search "said     Committed ee5fee6." (buffer-string))))
      ;; Waiting is trusted for as long as it is out, not for a TTL.
      (puthash "agy-a" (list 'waiting "waiting on qa" (time-subtract nil 600)) ghostherd--reports)
      (save-window-excursion (ghostherd-explain s))
      (with-current-buffer "*ghostherd explain*"
        (should (string-search "waiting on qa  (10m ago, trusted until replaced)" (buffer-string)))
        (should-not (string-search "expired" (buffer-string)))))))

(ert-deftest ghostherd-test-ask-outcome-says-whose-words ()
  (should (equal (ghostherd--ask-outcome
                  '(:id "a1" :from "claude-main" :status "answered" :auto t :auto_source "message"))
                 "stopped without answering ask a1; claude-main got its last message"))
  (should (equal (ghostherd--ask-outcome
                  '(:id "a1" :from "claude-main" :status "answered" :auto t :auto_source "screen"))
                 "stopped without answering ask a1; claude-main got the screen")))

(ert-deftest ghostherd-test-install-hooks-shows-then-asks ()
  "Nothing is written unless the plan shown is accepted."
  (let (calls)
    (cl-letf (((symbol-function 'ghostherd-herd-client) (lambda () "/p/bin/herd"))
              ((symbol-function 'call-process)
               (lambda (_prog _in _buf _disp &rest args)
                 (push args calls) (insert "plan") 0))
              ((symbol-function 'yes-or-no-p) (lambda (_) nil)))
      (save-window-excursion (ghostherd-install-hooks)))
    (should (equal calls '(("hooks" "install" "--dry-run"))))))

(defmacro ghostherd-tests--with-transcripts (&rest body)
  "BODY with each kind's transcripts at ROOT/KIND/*/ID.jsonl, ROOT temporary."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "gh-transcripts" t)))
          (ghostherd--transcripts (make-hash-table :test 'equal))
          (ghostherd-agent-specs
           (mapcar (lambda (e)
                     (let ((p (copy-sequence (cdr e))))
                       (cons (car e)
                             (if (plist-get p :transcript)
                                 (plist-put p :transcript
                                            (concat root (symbol-name (car e)) "/*/%s.jsonl"))
                               p))))
                   ghostherd-agent-specs)))
     (unwind-protect (progn ,@body)
       (delete-directory root t))))

(defun ghostherd-tests--touch (file)
  (make-directory (file-name-directory file) t)
  (write-region "" nil file nil 'silent)
  file)

(ert-deftest ghostherd-test-args-name-the-conversation ()
  (should (equal (ghostherd--args-conversation 'claude '("--model" "opus" "-r" "abc-1")) "abc-1"))
  (should (equal (ghostherd--args-conversation 'agy '("--conversation" "c1")) "c1"))
  ;; A fork writes a conversation of its own.
  (should-not (ghostherd--args-conversation 'claude '("-r" "abc-1" "--fork-session")))
  (should-not (ghostherd--args-conversation 'claude '("-r" "--evil")))
  (should-not (ghostherd--args-conversation 'claude '("--continue")))
  (should-not (ghostherd--args-conversation 'shell '("-r" "x"))))

(ert-deftest ghostherd-test-transcript-from-hooks-then-from-args ()
  "Hooks name the transcript; without them, the conversation an agent was
started on finds it; a fresh agent without hooks has none."
  (ghostherd-tests--with-herd ((ghostherd--herd-links (make-hash-table :test 'equal)))
    (ghostherd-tests--with-transcripts
      (let* ((resumed (ghostherd-tests--session :name "claude-r" :kind 'claude :args '("-r" "abc-1")))
             (fresh (ghostherd-tests--session :name "claude-f" :kind 'claude :args nil))
             (hooked (ghostherd-tests--session :name "agy-h" :kind 'agy :args '("--conversation" "old")))
             (file (ghostherd-tests--touch (concat root "claude/-x-proj/abc-1.jsonl")))
             (linked (ghostherd-tests--touch (concat root "elsewhere/t.jsonl"))))
        (ghostherd-tests--touch (concat root "agy/x/old.jsonl"))
        (should (equal (ghostherd-session-transcript resumed) file))
        (should-not (ghostherd-session-transcript fresh))
        (ghostherd--herd-take-links
         `((:session "agy-h" :cli "agy" :conversation "new" :transcript ,linked)))
        (should (equal (ghostherd-session-transcript hooked) linked))
        ;; Hooks that name the conversation but not its file: found by id,
        ;; the new one, not the one its arguments started it on.
        (let ((agy-file (ghostherd-tests--touch (concat root "agy/x/new.jsonl"))))
          (ghostherd--herd-take-links '((:session "agy-h" :cli "agy" :conversation "new")))
          (should (equal (ghostherd-session-transcript hooked) agy-file)))))))

(ert-deftest ghostherd-test-transcript-missing-is-not-sought-every-tick ()
  "The room asks every 2s; a transcript not there is looked for every 30."
  (ghostherd-tests--with-herd ((ghostherd--herd-links (make-hash-table :test 'equal)))
    (ghostherd-tests--with-transcripts
      (let* ((s (ghostherd-tests--session :name "claude-r" :kind 'claude :args '("-r" "late-1")))
             (globs 0)
             (inside nil)
             (orig (symbol-function 'file-expand-wildcards)))
        ;; It calls itself for a wildcard in a directory: count the outer.
        (cl-letf (((symbol-function 'file-expand-wildcards)
                   (lambda (&rest a)
                     (if inside
                         (apply orig a)
                       (cl-incf globs)
                       (setq inside t)
                       (unwind-protect (apply orig a) (setq inside nil))))))
          (should-not (ghostherd-session-transcript s))
          (ghostherd-tests--touch (concat root "claude/p/late-1.jsonl"))
          (should-not (ghostherd-session-transcript s))
          (should (= globs 1))
          (setcdr (gethash '(claude . "late-1") ghostherd--transcripts) 0)
          (should (ghostherd-session-transcript s))
          (should (= globs 2))
          ;; Found, it is not looked for again.
          (ghostherd-session-transcript s)
          (should (= globs 2)))))))

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

(ert-deftest ghostherd-test-tmux-not-asked-is-not-gone ()
  "tmux says nothing runs only when there is no server -- a socket left
behind, or none -- or no tmux.  Asked and not answering, it is unknown:
read as an empty herd, every session was gone, and the sweep dropped
the whole herd, its panes still running, without a word."
  (dolist (case '(((0 . "gh-abc-rev\t0\t\tclaude\n") (("gh-abc-rev" nil . "claude")))
                  ((1 . "no server running on /private/tmp/tmux-501/ghostherd\n") nil)
                  ((1 . "error connecting to /private/tmp/tmux-501/ghostherd (No such file or directory)\n") nil)
                  ((127 . "Searching for program: No such file or directory, tmux") nil)
                  ((1 . "error connecting to /private/tmp/tmux-501/ghostherd (Resource temporarily unavailable)\n") unknown)
                  ((1 . "lost server\n") unknown)
                  ((1 . "") unknown)))
    (cl-letf (((symbol-function 'ghostherd-tmux--call) (lambda (_args) (car case))))
      (should (equal (ghostherd-tmux--read-snapshot) (cadr case)))))
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (let ((s (ghostherd-tests--tmux-session :name "rev" :kind 'claude)))
        ;; Not answering: there, as far as anyone knows -- and not dead.
        (ghostherd-tests--with-tmux
            '(("list-panes" . (1 . "error connecting to /x (Resource temporarily unavailable)")))
          (should (ghostherd--session-live-p s))
          (should-not (ghostherd--host-exited-p s))
          (ghostherd--ensure-sessions))
        (should (eq (ghostherd-get "rev") s))
        (should-not ghostherd--log)
        ;; No server: gone, and the log says so.
        (ghostherd-tests--with-tmux '(("list-panes" . (1 . "no server running on /x")))
          (ghostherd--ensure-sessions))
        (should-not (ghostherd-get "rev"))
        (should (equal (mapcar #'ghostherd-log-entry-text ghostherd--log)
                       '("dropped from the herd: its host is gone")))))))

(ert-deftest ghostherd-test-a-readopted-agent-takes-back-its-view ()
  "An agent dropped and re-adopted in the same Emacs is still shown in
its buffer, attached: it takes that buffer back -- on restore, and on
being opened -- rather than attaching again in it, which ghostel
refuses.  A buffer only named for it, or one whose process is gone, is
not taken."
  (ghostherd-tests--with-herd ()
    (let* ((proc (make-process :name "gh-view" :command '("sleep" "30") :noquery t))
           (view (get-buffer-create "*ghostherd:rev*")))
      (unwind-protect
          (progn
            (with-current-buffer view
              (setq-local ghostherd-session-id "rev")
              (setq-local ghostel--process proc))
            ;; Restored: its buffer is the one still showing it.
            (let ((ghostherd-known-backends '(fake))
                  (ghostherd-tests--fake-recipes '((:host-id "gh-abc-rev" :name "rev" :kind claude))))
              (cl-letf (((symbol-function 'ghostherd--sidebar-refresh) #'ignore)
                        ((symbol-function 'ghostherd--ensure-poll-timer) #'ignore))
                (ghostherd-restore)))
            (should (eq (ghostherd-session-buffer (ghostherd-get "rev")) view))
            ;; Opened with no buffer of its own: that one, not a second attach.
            (let ((s (ghostherd-get "rev")))
              (setf (ghostherd-session-backend s) 'tmux
                    (ghostherd-session-buffer s) nil)
              (cl-letf (((symbol-function 'ghostherd-tmux--attach) (lambda (_) (error "Attached again"))))
                (should (eq (ghostherd-backend-view 'tmux s) view)))
              (should (eq (ghostherd-session-buffer s) view)))
            ;; Not its own, or not running: not taken.
            (with-current-buffer view (setq-local ghostherd-session-id "other"))
            (should-not (ghostherd--orphan-view "rev"))
            (with-current-buffer view (setq-local ghostherd-session-id "rev"))
            (delete-process proc)
            (should-not (ghostherd--orphan-view "rev"))
            (should-not (ghostherd--orphan-view "nobody")))
        (when (process-live-p proc) (delete-process proc))
        (kill-buffer view)))))

(ert-deftest ghostherd-test-an-empty-herd-is-looked-for-again ()
  "The poll looks for agents to re-adopt while the herd is empty -- not on
every tick -- so a herd emptied by a moment its host could not be
reached comes back by itself, to the herd page too."
  (ghostherd-tests--with-herd ()
    (let ((ghostherd--restore-tried-at 0) (restored 0))
      (cl-letf (((symbol-function 'ghostherd-restore) (lambda (&rest _) (cl-incf restored))))
        (ghostherd-poll-all)
        (ghostherd-poll-all)
        (should (= restored 1))
        (setq ghostherd--restore-tried-at (- (float-time) ghostherd-restore-every 1))
        (ghostherd-poll-all)
        (should (= restored 2))
        ;; A herd with someone in it is not looked for.
        (ghostherd-tests--session :name "a" :backend 'fake)
        (setq ghostherd--restore-tried-at 0)
        (let ((ghostherd-tests--fake-live t))
          (cl-letf (((symbol-function 'ghostherd-poll-session) #'ignore))
            (ghostherd-poll-all)))
        (should (= restored 2))))))

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
                                       :host-id "gh-abc-b"))
          (c (ghostherd-tests--session :name "c" :backend 'tmux
                                       :host-id "gh-abc-c"))
          (ghostherd-tmux--last-words nil))
      (ghostherd-tests--with-tmux
          '(("list-panes" . (0 . "gh-abc-a\t0\t\tclaude\ngh-abc-b\t1\t1\t\ngh-abc-c\t1\t\t\n")))
        (should (ghostherd--host-live-p a))
        (should (ghostherd--host-live-p b))
        (should-not (ghostherd--host-exited-p a))
        ;; `remain-on-exit' keeps a dead pane capturable: still live, but
        ;; the agent behind it is gone -- with its status, when tmux has
        ;; one; killed by a signal, it has none.
        (should (equal (ghostherd--host-exited-p b) "exited 1"))
        (should (equal (ghostherd--host-exited-p c) "process exited"))
        (should (equal (ghostherd--host-title a) "claude")))
      (should (= (cl-count "list-panes" ghostherd-tests--tmux-calls
                           :key #'car :test #'equal)
                 1)))))

(ert-deftest ghostherd-test-tmux-unknown-host-is-not-live ()
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :backend 'tmux
                                       :host-id "gh-abc-gone")))
      (ghostherd-tests--with-tmux '(("list-panes" . (0 . "gh-abc-other\t0\t\t\n")))
        (should-not (ghostherd--host-live-p s))
        (should (equal (car (ghostherd--detect-state s)) 'dead))))))

(ert-deftest ghostherd-test-tmux-starts-from-a-directory-that-is-there ()
  "A timer runs in whatever buffer is current.  One whose directory was
deleted -- a dired buffer on an unpacked tarball, the folder then
removed -- made `call-process' refuse to start tmux, that read as no
tmux, and the whole herd was dropped, every agent still running."
  (let ((gone (file-name-as-directory (make-temp-file "ghostherd-gone" t)))
        (ghostherd-tmux-executable "true")
        (ghostherd-tmux-config "/dev/null")
        (ghostherd-tmux--fetch nil)
        (ghostherd-tmux--status-cache nil)
        fetched styled)
    (delete-directory gone)
    (ghostherd-tests--with-herd ()
      (let ((s (ghostherd-tests--session :name "a" :backend 'tmux
                                         :host-id "gh-abc-a"))
            (default-directory gone))
        (should (equal (ghostherd-tmux--call '("list-panes")) '(0 . "")))
        ;; The poll's fetch and a styled capture start tmux too.
        (should (ghostherd-backend-screens 'tmux (list s) 10
                                           (lambda (_) (setq fetched t))))
        (ghostherd-backend-styled-screen 'tmux s (lambda (out) (setq styled out)))
        (with-timeout (5 (ert-fail "tmux never answered"))
          (while (not (and fetched styled))
            (accept-process-output nil 0.05)))
        (should (equal styled ""))))))

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
                         ";" "if-shell" "-F" "-t" "=gh-a:" "#{pane_dead}"
                         "capture-pane -p -J -S -50 -t =gh-a:" "capture-pane -p -t =gh-a:"
                         ";" "display-message" "-p" "@@m@@ gh-b"
                         ";" "if-shell" "-F" "-t" "=gh-b:" "#{pane_dead}"
                         "capture-pane -p -J -S -50 -t =gh-b:" "capture-pane -p -t =gh-b:")))))

(ert-deftest ghostherd-test-screens-marker-is-not-guessable ()
  "The text being delimited is whatever an agent chose to print, and an
agent can print anything -- including a fixed \"unlikely\" delimiter."
  (should-not (equal (ghostherd-tmux--marker) (ghostherd-tmux--marker))))

(ert-deftest ghostherd-test-parse-screens-splits-status-from-panes ()
  (pcase-let ((`(,status . ,screens)
               (ghostherd-tmux--parse-screens
                (concat "gh-a\t0\t\ttitle a\ngh-b\t0\t\ttitle b\n"
                        "@@m@@ gh-a\nAAA\nAAA2\n"
                        "@@m@@ gh-b\nBBB\n")
                "@@m@@")))
    (should (equal status "gh-a\t0\t\ttitle a\ngh-b\t0\t\ttitle b"))
    (should (equal (alist-get "gh-a" screens nil nil #'equal) "AAA\nAAA2"))
    (should (equal (alist-get "gh-b" screens nil nil #'equal) "BBB"))))

(ert-deftest ghostherd-test-parse-screens-tolerates-an-abandoned-list ()
  "tmux abandons the rest of a command list at the first failure, which a
pane killed from outside is enough to cause.  What came back is used; the
pane whose marker printed with nothing after it is *not* reported as an
empty screen, because an empty screen would read as `idle'."
  (pcase-let ((`(,_status . ,screens)
               (ghostherd-tmux--parse-screens
                (concat "gh-a\t0\t\tt\n"
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
        (insert "gh-abc-rev\t0\t\tagy\n@@m@@ gh-abc-rev\nThinking…\n"))
      (ghostherd-tmux--fetch-done buffer "@@m@@" '(("gh-abc-rev" . "rev")) 40
                                  (lambda (screens) (setq got screens)))
      (should (equal got '(("rev" . "Thinking…"))))
      (should (equal (ghostherd-tmux--parse-snapshot "gh-abc-rev\t0\t\tagy")
                     (cdr ghostherd-tmux--status-cache)))
      ;; Fresh by definition: it was stamped just now, so no reader on this
      ;; tick pays for a second list-panes.
      (should (assoc "gh-abc-rev" (ghostherd-tmux--snapshot))))))

(ert-deftest ghostherd-test-status-says-how-a-pane-ended ()
  "Running, nil; exited, its status; killed by a signal, t -- tmux has no
status to give.  The title, last, may hold the separator."
  (should (equal (ghostherd-tmux--parse-snapshot
                  "gh-a\t0\t\tclaude\ngh-b\t1\t1\ta\ttitle\ngh-c\t1\t\t\n")
                 '(("gh-a" nil . "claude") ("gh-b" 1 . "a\ttitle") ("gh-c" t . "")))))

(ert-deftest ghostherd-test-last-words-are-the-last-line-that-says-something ()
  "What a CLI said on its way out: blank lines, a box's edges and the
host's own line about the exit say nothing."
  (should (equal (ghostherd--last-words
                  (concat "first\nNo conversation found to continue\n\n  ╰────╯  \n\n"
                          "Pane is dead (status 1, Wed Oct 14 13:10:32 2026)\n")
                  ghostherd-tmux--dead-line)
                 "No conversation found to continue"))
  (should (equal (ghostherd--last-words "bye\n\n[Process exited]\n"
                                        ghostherd--ghostel-exit-line)
                 "bye"))
  (should-not (ghostherd--last-words "\n  \n─────\n"))
  (should-not (ghostherd--last-words nil))
  (let ((long (ghostherd--last-words (make-string 400 ?x))))
    (should (= (string-width long) ghostherd--last-words-width))
    (should (string-suffix-p "…" long))))

(ert-deftest ghostherd-test-exit-reason-says-status-and-words ()
  (should (equal (ghostherd--exit-reason 1 "No conversation found to continue")
                 "exited 1: No conversation found to continue"))
  (should (equal (ghostherd--exit-reason 0 nil) "exited 0"))
  (should (equal (ghostherd--exit-reason t "Killed") "process exited: Killed"))
  (should (equal (ghostherd--exit-reason nil nil) "process exited")))

(ert-deftest ghostherd-test-a-dead-pane-says-why ()
  "`claude --continue' in a folder with no conversation said so and
exited 1, the line one above the top of its screen; the herd said only
\"process exited\" (2026-10-14).  The tick that finds a pane dead reads
what its CLI said last -- past the screen the rules are given -- and
the reason and the log carry it."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (let ((dead (ghostherd-tests--session :name "be" :kind 'claude :state 'starting
                                            :backend 'tmux :host-id "gh-abc-be"))
            (live (ghostherd-tests--session :name "ops" :kind 'claude :state 'idle
                                            :backend 'tmux :host-id "gh-abc-ops"))
            (ghostherd-tmux--last-words '(("gh-abc-gone" . "from a pane since killed")))
            (buffer (generate-new-buffer " *fetch-test*"))
            got)
        (with-current-buffer buffer
          (insert "gh-abc-be\t1\t1\t\ngh-abc-ops\t0\t\tclaude\n"
                  "@@m@@ gh-abc-be\nNo conversation found to continue\n"
                  (make-string 50 ?\n)
                  "Pane is dead (status 1, Wed Oct 14 13:10:32 2026)\n"
                  "@@m@@ gh-abc-ops\nthe agent at work\n> \n"))
        (ghostherd-tests--with-tmux nil
          (ghostherd-tmux--fetch-done buffer "@@m@@"
                                      '(("gh-abc-be" . "be") ("gh-abc-ops" . "ops")) 40
                                      (lambda (screens) (setq got screens)))
          (should-not (string-match-p "No conversation" (alist-get "be" got nil nil #'equal)))
          (should (equal ghostherd-tmux--last-words
                         '(("gh-abc-be" . "No conversation found to continue"))))
          (should (eq (ghostherd-poll-session dead (alist-get "be" got nil nil #'equal)) 'dead))
          (should (equal (ghostherd-session-state-reason dead)
                         "exited 1: No conversation found to continue"))
          (should (string-match-p "No conversation found to continue"
                                  (ghostherd-log-entry-text (car ghostherd--log))))
          ;; A live pane's last line is not taken for last words.
          (should-not (ghostherd--host-exited-p live)))))))

(ert-deftest ghostherd-test-ghostel-dead-agent-says-why ()
  "The same on ghostel: the buffer's last words, under its own line about
the exit.  Its status only from a process of its own: the native PTY's
handle is a pipe, whose status says nothing about the child."
  (ghostherd-tests--with-herd ()
    (let* ((s (ghostherd-tests--session :name "g" :kind 'claude :backend 'ghostel))
           (buffer (ghostherd-session-buffer s))
           (real (make-process :name "ghostherd-exit-test" :noquery t
                               :command '("sh" "-c" "exit 2")))
           (pipe (make-pipe-process :name "ghostherd-exit-pipe" :noquery t)))
      (with-timeout (5 (ert-fail "the process never exited"))
        (while (process-live-p real) (accept-process-output nil 0.05)))
      (delete-process pipe)
      (with-current-buffer buffer
        (insert "No conversation found to continue\n\n[Process exited]\n")
        (setq-local ghostel--process real))
      (should (equal (ghostherd--host-exited-p s)
                     "exited 2: No conversation found to continue"))
      (with-current-buffer buffer (setq-local ghostel--process pipe))
      (should (equal (ghostherd--host-exited-p s)
                     "process exited: No conversation found to continue")))))

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

(ert-deftest ghostherd-test-tmux-dead-pane-round-trip ()
  "A real pane that says something, scrolls it off its screen and exits
3: the fetch reaches back for it, beside a pane still running -- and
after one killed from outside, which no longer costs the panes after it
their answer."
  (skip-unless (executable-find "tmux"))
  (ghostherd-tests--with-herd ()
    (let* ((ghostherd-tmux-socket "ghostherd-ert-exit")
           (ghostherd-tmux--status-cache nil)
           (ghostherd-tmux--last-words nil)
           (ghostherd-tmux--fetch nil)
           (sessions nil)
           (screens 'unset))
      (unwind-protect
          (progn
            (push (ghostherd-spawn
                   'shell :name "exits" :backend 'tmux
                   :project temporary-file-directory
                   :directory temporary-file-directory
                   :command "sh"
                   :args (list "-c" "echo No conversation found to continue; i=0; \
while [ $i -lt 80 ]; do echo; i=$((i+1)); done; exit 3")
                   :display nil)
                  sessions)
            (push (ghostherd-spawn
                   'shell :name "runs" :backend 'tmux
                   :project temporary-file-directory
                   :directory temporary-file-directory
                   :command "sh" :args (list "-c" "echo still here; cat")
                   :display nil)
                  sessions)
            ;; First in the list, and gone by the time it is asked about.
            (push (ghostherd-spawn
                   'shell :name "killed" :backend 'tmux
                   :project temporary-file-directory
                   :directory temporary-file-directory
                   :command "cat" :args nil :display nil)
                  sessions)
            (sleep-for 0.5)
            (call-process "tmux" nil nil nil "-L" "ghostherd-ert-exit" "kill-session" "-t"
                          (ghostherd-tmux--target (ghostherd-session-host-id (car sessions))))
            (should (ghostherd-backend-screens
                     'tmux sessions 40 (lambda (s) (setq screens s))))
            (with-timeout (10 (error "batched fetch never came back"))
              (while (eq screens 'unset)
                (accept-process-output nil 0.05)))
            (should (equal (sort (mapcar #'car screens) #'string<) '("exits" "runs")))
            (should (string-match-p "still here" (alist-get "runs" screens nil nil #'equal)))
            (should (equal (ghostherd--host-exited-p (ghostherd-get "exits"))
                           "exited 3: No conversation found to continue"))
            (should-not (ghostherd--host-exited-p (ghostherd-get "runs"))))
        (dolist (session sessions)
          (ignore-errors (ghostherd--host-kill session)))
        (when (timerp ghostherd--poll-timer)
          (cancel-timer ghostherd--poll-timer)
          (setq ghostherd--poll-timer nil))
        (call-process "tmux" nil nil nil "-L" "ghostherd-ert-exit"
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
    (cl-letf (((symbol-function 'ghostherd-memory--token) (lambda () "tok")))
      (should (equal (ghostherd-memory--rpc-url)
                     "http://127.0.0.1:49160/jsonrpc/tok"))
      (should (equal (ghostherd-memory--health-url)
                     "http://127.0.0.1:49160/health"))
      (should (equal (ghostherd-memory--rpc-url 49153)
                     "http://127.0.0.1:49153/jsonrpc/tok")))))

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
  "Ours means our service *on our token*: a sidecar on another token,
or from before tokens, would refuse every call."
  (cl-letf (((symbol-function 'ghostherd-memory--token) (lambda () "abc")))
    (should (ghostherd-memory--health-body-ours-p
             "{\"status\":\"ok\",\"service\":\"ghostherd-memory\",\"token_id\":\"ba7816bf8f01\"}"))
    (should (eq (ghostherd-memory--health-kind
                 "{\"status\":\"ok\",\"service\":\"ghostherd-memory\",\"token_id\":\"000000000000\"}")
                'stale))
    (should (eq (ghostherd-memory--health-kind
                 "{\"status\":\"ok\",\"service\":\"ghostherd-memory\"}")
                'stale))
    (should-not (ghostherd-memory--health-body-ours-p
                 "{\"status\":\"ok\"}"))
    (should-not (ghostherd-memory--health-kind "not json")))
  (cl-letf (((symbol-function 'ghostherd-memory--token) (lambda () nil)))
    (should (eq (ghostherd-memory--health-kind
                 "{\"service\":\"ghostherd-memory\",\"token_id\":\"ba7816bf8f01\"}")
                'stale))))

(ert-deftest ghostherd-test-memory-token-from-the-data-directory ()
  "Read from rpc.token, which the sidecar writes; fingerprinted the way
its /health does (sha256(\"abc\") starts ba7816bf8f01)."
  (let* ((dir (make-temp-file "gh-mem-" t))
         (ghostherd-memory-data-directory dir))
    (unwind-protect
        (progn
          (should-not (ghostherd-memory--token))
          (should-error (ghostherd-memory--rpc-url 49152) :type 'user-error)
          (let ((ghostherd-memory--port 49152))
            (should-not (ghostherd-memory-rpc-url)))
          (write-region "abc\n" nil (expand-file-name "rpc.token" dir))
          (should (equal (ghostherd-memory--token) "abc"))
          (should (equal (ghostherd-memory--token-id "abc") "ba7816bf8f01"))
          (let ((ghostherd-memory--port 49152)
                (ghostherd-memory-host "127.0.0.1"))
            (should (equal (ghostherd-memory-rpc-url)
                           "http://127.0.0.1:49152/jsonrpc/abc"))))
      (delete-directory dir t))))

(ert-deftest ghostherd-test-memory-rpc-locator-is-private ()
  "rpc.url carries the token: 0600, even over a file that was 0644."
  (let* ((home (make-temp-file "gh-home-" t))
         (user-emacs-directory (file-name-as-directory home))
         (file (expand-file-name "ghostherd-mail/rpc.url" home)))
    (unwind-protect
        (cl-letf (((symbol-function 'ghostherd-memory-rpc-url)
                   (lambda () "http://127.0.0.1:49152/jsonrpc/abc")))
          (make-directory (file-name-directory file) t)
          (write-region "old" nil file)
          (set-file-modes file #o644)
          (ghostherd-memory--write-rpc-locator)
          (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string))
                         "http://127.0.0.1:49152/jsonrpc/abc"))
          (should (= (file-modes file) #o600)))
      (delete-directory home t))))

(ert-deftest ghostherd-test-memory-start-refuses-a-foreign-stale-sidecar ()
  "Walking past it would start a second sidecar on the same Qdrant lock."
  (let ((ghostherd-memory-port 49152)
        (ghostherd-memory-port-tries 2)
        (ghostherd-memory--process nil)
        (ghostherd-memory--port nil)
        spawned)
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/usr/bin/uv"))
              ((symbol-function 'ghostherd-memory-healthy-p) #'ignore)
              ((symbol-function 'ghostherd-memory--find-running) #'ignore)
              ((symbol-function 'ghostherd-memory--server-directory) (lambda () "/tmp"))
              ((symbol-function 'ghostherd-memory--listening-p) (lambda (p) (= p 49152)))
              ((symbol-function 'ghostherd-memory--health-kind-at)
               (lambda (&optional p) (and (eql p 49152) 'stale)))
              ((symbol-function 'ghostherd-memory--spawn)
               (lambda (&rest _) (setq spawned t))))
      (let ((err (should-error (ghostherd-memory-start) :type 'user-error)))
        (should (string-match-p "port 49152" (cadr err)))
        (should (string-match-p "lsof -ti tcp:49152" (cadr err))))
      (should-not spawned))))

(ert-deftest ghostherd-test-memory-start-restarts-its-own-stale-sidecar ()
  (let ((ghostherd-memory-port 49152)
        (ghostherd-memory--port 49152)
        stopped messages)
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/usr/bin/uv"))
              ((symbol-function 'ghostherd-memory--process-live-p) (lambda () (not stopped)))
              ((symbol-function 'ghostherd-memory--health-kind-at)
               (lambda (&optional _) (if stopped 'ours 'stale)))
              ((symbol-function 'ghostherd-memory-stop) (lambda () (setq stopped t)))
              ((symbol-function 'sleep-for) #'ignore)
              ((symbol-function 'ghostherd-memory-healthy-p) (lambda () stopped))
              ((symbol-function 'ghostherd-memory--write-rpc-locator) #'ignore)
              ((symbol-function 'ghostherd-memory--token) (lambda () "s3cret"))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (should (ghostherd-memory-start))
      (should stopped)
      ;; Messages name the port, never the URL: it carries the token.
      (should messages)
      (should-not (cl-some (lambda (m) (string-search "s3cret" m)) messages)))))

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

(ert-deftest ghostherd-test-memory-request-body-is-what-json-encode-writes ()
  "Written in C where it can be, by `json-encode' where it cannot; the
same JSON either way, in UTF-8 bytes."
  (let ((ghostherd-memory--request-id 0)
        (encoded nil)
        (real-encode (symbol-function 'json-encode)))
    (cl-letf (((symbol-function 'json-encode)
               (lambda (o) (setq encoded t) (funcall real-encode o))))
      (dolist (case `((nil (:sessions [(:name "rd" :screen "你好 ✓\n" :notes nil)] :ack_ids [] :replies []))
                      (nil (:id "a1" :dead :json-false :done t :n 2.5 :agents [(:name "x" :path "/p")]))
                      (nil nil)
                      (t (:agents ("claude" "grok")))
                      (t (:kind claude))
                      (t (:screen ,(string-to-multibyte "bad \377 byte")))))
        (setq encoded nil)
        (let* ((params (cadr case))
               (body (ghostherd-memory--request-body "herd_tick" params))
               (read (lambda (s) (json-parse-string (decode-coding-string s 'utf-8)
                                                    :object-type 'alist :null-object :null
                                                    :false-object :false))))
          (should (eq encoded (car case)))
          (should-not (multibyte-string-p body))
          (setq ghostherd-memory--request-id 0)
          (should (equal (funcall read body)
                         (funcall read (encode-coding-string
                                        (funcall real-encode
                                                 (ghostherd-memory--build-request "herd_tick" params))
                                        'utf-8))))
          (setq ghostherd-memory--request-id 0))))))

(ert-deftest ghostherd-test-memory-response-reads-as-json-read-did ()
  "Objects plists with keyword keys, arrays lists, null and false nil."
  (let ((body "{\"jsonrpc\":\"2.0\",\"id\":3,\"result\":{\"pending\":[{\"id\":1,\"to\":\"rd\",\"submit\":false}],\"asks\":[],\"links\":{},\"n\":1.5,\"ok\":true,\"none\":null}}"))
    (should (equal (ghostherd-memory--parse-response body)
                   '(:pending ((:id 1 :to "rd" :submit nil)) :asks nil :links nil :n 1.5 :ok t :none nil))))
  (should-error (ghostherd-memory--parse-response ""))
  (should-error (ghostherd-memory--parse-response "{\"result\":")))

(defmacro ghostherd-tests--with-sidecar-http (&rest body)
  "BODY with url.el faked: `answer' lands a reply on the last request.
Each request's buffer is in `bufs'; `timers' holds the deadlines set,
as (SECS . FN), and `later' what was put off until next tick."
  (declare (indent 0))
  `(let ((bufs nil) (timers nil) (later nil) (landing nil) (refuse nil))
     (cl-letf (((symbol-function 'ghostherd-memory--rpc-url) (lambda () "http://127.0.0.1:1/rpc"))
               ((symbol-function 'url-retrieve)
                (lambda (_url cb &rest _)
                  (when refuse (signal 'wrong-type-argument (list 'stringp nil)))
                  (let ((b (generate-new-buffer " *gh-sidecar-test*")))
                    (push b bufs)
                    (setq landing cb)
                    b)))
               ((symbol-function 'run-with-timer)
                (lambda (secs _repeat fn &rest args)
                  (push (cons secs (lambda () (apply fn args))) timers)
                  (car timers)))
               ((symbol-function 'cancel-timer)
                (lambda (timer) (setq timers (delq timer timers))))
               ((symbol-function 'run-at-time)
                (lambda (_secs _repeat fn &rest args)
                  (push (lambda () (apply fn args)) later))))
       (cl-flet ((answer (status &optional body)
                   (with-current-buffer (car bufs)
                     (insert "HTTP/1.1 200 OK\r\n\r\n" (or body ""))
                     (funcall landing status))))
         (unwind-protect (progn ,@body)
           (mapc #'kill-buffer (seq-filter #'buffer-live-p bufs)))))))

(ert-deftest ghostherd-test-memory-async-answer-leaves-no-buffer ()
  "Every answer's buffer goes, read or failed.  Kept, they piled up in
the tens of thousands, and each garbage collection walked them all."
  (ghostherd-tests--with-sidecar-http
    (let (got failed)
      (ghostherd-memory-request-async
       "herd_tick" (lambda (r) (push r got)) nil (lambda (e) (push e failed)))
      (answer nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"ok\":true}}")
      (should (equal got '((:ok t))))
      (should-not (buffer-live-p (car bufs)))
      (should-not timers)
      (ghostherd-memory-request-async
       "herd_tick" (lambda (r) (push r got)) nil (lambda (e) (push e failed)))
      (answer '(:error (error connection-failed "refused")))
      (should (equal failed '("(error connection-failed refused)")))
      (should-not (buffer-live-p (car bufs)))
      (should (= (length got) 1)))))

(ert-deftest ghostherd-test-memory-async-unanswered-is-given-up-once ()
  "url.el can drop a request; the herd tick would then wait forever."
  (ghostherd-tests--with-sidecar-http
    (let (got failed)
      (ghostherd-memory-request-async
       "herd_tick" (lambda (r) (push r got)) nil (lambda (e) (push e failed)))
      (should (equal (mapcar #'car timers) (list ghostherd-memory-request-timeout)))
      (funcall (cdar timers))
      (should (equal failed (list (format "herd_tick: no answer in %ss"
                                          ghostherd-memory-request-timeout))))
      (should-not (buffer-live-p (car bufs)))
      ;; Too late: it was given up.
      (let ((b (generate-new-buffer " *gh-sidecar-test*")))
        (push b bufs)
        (answer nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"ok\":true}}"))
      (funcall (cdar timers))
      (should-not got)
      (should (= (length failed) 1)))))

(ert-deftest ghostherd-test-memory-async-import-is-waited-on ()
  "An import answers when it is done; twenty minutes is not a failure."
  (ghostherd-tests--with-sidecar-http
    (ghostherd-memory-request-async "memory_import" #'ignore nil #'ignore)
    (should-not timers)))

(ert-deftest ghostherd-test-memory-async-refused-at-once-fails-later ()
  "url.el can fail the call itself; the caller still hears, once,
and only after the call returns, as from any answer."
  (ghostherd-tests--with-sidecar-http
    (setq refuse t)
    (let (failed)
      (should-not (ghostherd-memory-request-async
                   "herd_tick" #'ignore nil (lambda (e) (push e failed))))
      (should-not failed)
      (should-not timers)
      (mapc #'funcall later)
      (should (equal failed '("Wrong type argument: stringp, nil"))))))

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

;;; Account usage

(ert-deftest ghostherd-test-usage-remaining-from-used ()
  (should (= (ghostherd-usage--remaining 89.0) 11.0))
  (should (= (ghostherd-usage--remaining 0) 100.0))
  (should (= (ghostherd-usage--remaining 100) 0.0)))

(ert-deftest ghostherd-test-usage-parse-grok-config ()
  (let* ((raw (json-parse-string
               "{\"creditUsagePercent\":89.0,\"currentPeriod\":{\"type\":\"USAGE_PERIOD_TYPE_WEEKLY\",\"end\":\"2026-09-20T14:57:40Z\"}}"
               :object-type 'alist :null-object nil))
         (wins (ghostherd-usage--parse-grok-config raw)))
    (should (= (length wins) 1))
    (should (equal (plist-get (car wins) :label) "wk"))
    (should (= (plist-get (car wins) :remaining) 11.0))))

(ert-deftest ghostherd-test-usage-grok-new-week-is-nothing-used ()
  "Billing leaves a zero out, as protobuf's JSON does: a week just begun
has no creditUsagePercent, and is nothing used, not no numbers."
  (let* ((raw (json-parse-string
               "{\"currentPeriod\":{\"type\":\"USAGE_PERIOD_TYPE_WEEKLY\",\"start\":\"2026-10-11T14:57:40.825457+00:00\",\"end\":\"2026-10-18T14:57:40.825457+00:00\"},\"onDemandCap\":{\"val\":0},\"isUnifiedBillingUser\":true}"
               :object-type 'alist :null-object nil))
         (wins (ghostherd-usage--parse-grok-config raw)))
    (should (equal (plist-get (car wins) :label) "wk"))
    (should (= (plist-get (car wins) :remaining) 100.0))
    (should (= (plist-get (car wins) :resets-at)
               (float-time (encode-time (iso8601-parse "2026-10-18T14:57:40.825457+00:00")))))
    ;; Nor a period: nothing to read, not a guess.
    (should-not (ghostherd-usage--parse-grok-config '((isUnifiedBillingUser . t))))))

(ert-deftest ghostherd-test-usage-grok-log-as-of-when-written ()
  "A billing line in grok's log counts from when grok wrote it, and only
over numbers older than it."
  (let* ((line (lambda (ts used)
                 (format "{\"ts\":\"%s\",\"msg\":\"billing: fetched credits config\",\"ctx\":{\"config\":{\"creditUsagePercent\":%s,\"currentPeriod\":{\"type\":\"USAGE_PERIOD_TYPE_WEEKLY\",\"end\":\"2026-10-18T14:57:40Z\"}}}}"
                         ts used)))
         (tail (concat (funcall line "2026-10-11T15:18:44.171Z" 12)
                       "\n{\"ts\":\"2026-10-11T15:19:00Z\",\"msg\":\"other\"}\n"))
         (ghostherd-usage--cache nil))
    (cl-letf (((symbol-function 'ghostherd-usage--file-tail) (lambda (&rest _) tail)))
      (should (ghostherd-usage--grok-from-log))
      (let ((pl (alist-get 'grok ghostherd-usage--cache)))
        (should (= (plist-get pl :fetched)
                   (float-time (encode-time (iso8601-parse "2026-10-11T15:18:44.171Z")))))
        (should (= (plist-get (car (plist-get pl :windows)) :remaining) 88.0)))
      ;; The API has said something since: the older line stands down.
      (ghostherd-usage--put 'grok (list (ghostherd-usage--window "wk" 70.0)))
      (ghostherd-usage--grok-from-log)
      (should (= (plist-get (car (plist-get (alist-get 'grok ghostherd-usage--cache) :windows))
                            :remaining)
                 70.0)))))

(ert-deftest ghostherd-test-usage-grok-fetch-says-why-it-failed ()
  "A fetch that fails keeps the numbers and says why: a card left as it
was reads as current."
  (let ((ghostherd-usage--cache
         (list (cons 'grok (list :windows (list (ghostherd-usage--window "wk" 7.0))
                                 :fetched 100.0 :error nil))))
        (reply nil))
    (cl-letf (((symbol-function 'ghostherd-usage--grok-auth-entry)
               (lambda () `((key . "k") (user_id . "u")
                            (expires_at . ,(format-time-string "%FT%T%z" (+ (float-time) 3600))))))
              ((symbol-function 'ghostherd-usage--http-json)
               (lambda (_url _headers ok-fn &rest _) (apply ok-fn reply)))
              ((symbol-function 'ghostherd-usage--done) #'ignore))
      (setq reply '(nil 503))
      (ghostherd-usage--fetch-grok)
      (let ((pl (alist-get 'grok ghostherd-usage--cache)))
        (should (equal (plist-get pl :error) "http 503"))
        (should (= (plist-get pl :fetched) 100.0))
        (should (= (plist-get (car (plist-get pl :windows)) :remaining) 7.0)))
      (setq reply '(((config (onDemandCap (val . 0))))))
      (ghostherd-usage--fetch-grok)
      (should (equal (plist-get (alist-get 'grok ghostherd-usage--cache) :error) "billing unreadable"))
      ;; The new week, as the API sends it: nothing used, the error gone.
      (setq reply '(((config (currentPeriod (type . "USAGE_PERIOD_TYPE_WEEKLY")
                                            (end . "2026-10-18T14:57:40Z"))))))
      (ghostherd-usage--fetch-grok)
      (let ((pl (alist-get 'grok ghostherd-usage--cache)))
        (should-not (plist-get pl :error))
        (should (= (plist-get (car (plist-get pl :windows)) :remaining) 100.0))))))

(ert-deftest ghostherd-test-usage-parse-claude ()
  (let* ((raw (json-parse-string
               "{\"five_hour\":{\"utilization\":38.0,\"resets_at\":\"2026-09-18T12:00:00Z\"},\"seven_day\":{\"utilization\":12.0}}"
               :object-type 'alist :null-object nil))
         (wins (ghostherd-usage--parse-claude raw)))
    (should (equal (mapcar (lambda (w) (plist-get w :label)) wins)
                   '("5h" "7d")))
    (should (= (plist-get (nth 0 wins) :remaining) 62.0))
    (should (= (plist-get (nth 1 wins) :remaining) 88.0))))

(ert-deftest ghostherd-test-usage-parse-agy-groups-names-each-group ()
  "Two groups each with a `wk' read as two identical rows unless named."
  (let* ((raw (json-parse-string
               "{\"groups\":[{\"displayName\":\"Gemini Models\",\"buckets\":[{\"window\":\"weekly\",\"remainingFraction\":0.993},{\"window\":\"5h\",\"remainingFraction\":1}]},{\"displayName\":\"Third-Party Models\",\"buckets\":[{\"window\":\"weekly\",\"remainingFraction\":1},{\"window\":\"5h\",\"remainingFraction\":1}]}]}"
               :object-type 'alist :array-type 'list :null-object nil))
         (wins (ghostherd-usage--parse-agy-groups raw)))
    (should (equal (mapcar (lambda (w) (plist-get w :label)) wins)
                   '("gem-wk" "gem-5h" "3p-wk" "3p-5h")))))

(ert-deftest ghostherd-test-herd-page-usage-is-rounded ()
  "100 - 99.3 in floating point is 0.7000000000000028, and the page
printed every digit of it across the next column."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let ((ghostherd-usage--cache '((agy :windows ((:label "gem-wk" :remaining 99.3)) :fetched 1.0))))
    (let ((w (aref (plist-get (aref (ghostherd-memory-page--usage) 0) :windows) 0)))
      (should (equal (plist-get w :used) 0.7)))))

(ert-deftest ghostherd-test-usage-parse-agy-groups ()
  (let* ((raw (json-parse-string
               "{\"groups\":[{\"displayName\":\"Gemini Models\",\"buckets\":[{\"window\":\"5h\",\"remainingFraction\":0.8},{\"window\":\"weekly\",\"remainingFraction\":0.55}]}]}"
               :object-type 'alist :array-type 'list :null-object nil))
         (wins (ghostherd-usage--parse-agy-groups raw)))
    (should (equal (mapcar (lambda (w) (plist-get w :label)) wins)
                   '("5h" "wk")))
    (should (= (plist-get (nth 0 wins) :remaining) 80.0))
    (should (= (plist-get (nth 1 wins) :remaining) 55.0))))

(ert-deftest ghostherd-test-usage-parse-agy-usage-screen ()
  (let* ((text "GEMINI MODELS
Weekly Limit Remaining
98.49%]
98% remaining · Refreshes in 117h 48m
Five Hour Limit Remaining
96.49%]
96% remaining · Refreshes in 2h 20m
CLAUDE AND GPT MODELS
Weekly Limit Remaining
Quota available
Five Hour Limit Remaining
Quota available")
         (wins (ghostherd-usage--parse-agy-usage-screen text)))
    (should (equal (mapcar (lambda (w) (plist-get w :label)) wins)
                   '("gem-wk" "gem-5h" "3p-wk" "3p-5h")))
    (should (= (plist-get (nth 0 wins) :remaining) 98))
    (should (= (plist-get (nth 1 wins) :remaining) 96))
    (should (= (plist-get (nth 2 wins) :remaining) 100))
    (should (= (plist-get (nth 3 wins) :remaining) 100))))

(ert-deftest ghostherd-test-usage-used-from-remaining ()
  (should (= (ghostherd-usage--used 78.0) 22.0))
  (should (= (ghostherd-usage--used 0) 100.0))
  (should (= (ghostherd-usage--used 100) 0.0)))

(ert-deftest ghostherd-test-usage-bar-and-mode-line-safe ()
  (should (string-match-p "█" (ghostherd-usage--bar 80 5)))
  (should (string-match-p "░" (ghostherd-usage--bar 80 5)))
  (should (equal (ghostherd-usage--mode-line-safe "78% 5h") "78%% 5h")))

(ert-deftest ghostherd-test-usage-bar-fills-with-what-is-spent ()
  "Filled cells are the used share, like the provider panels."
  (should (equal (substring-no-properties (ghostherd-usage--bar 100 4)) "░░░░"))
  (should (equal (substring-no-properties (ghostherd-usage--bar 0 4)) "████"))
  (should (equal (substring-no-properties (ghostherd-usage--bar 50 4)) "██░░")))

(ert-deftest ghostherd-test-usage-window-bar-prints-used ()
  "78% left reads as 22, the number claude.ai shows."
  (let ((w (ghostherd-usage--window "5h" 78.0)))
    (should (string-match-p "22 5h"
                            (substring-no-properties
                             (ghostherd-usage--window-bar w nil))))
    (should-not (string-match-p "78"
                                (substring-no-properties
                                 (ghostherd-usage--window-bar w nil))))))

(ert-deftest ghostherd-test-usage-help-spells-out-both-sides ()
  "The bare integer is ambiguous; the tooltip is not."
  (let* ((wins (list (ghostherd-usage--window
                      "5h" 78.0 (+ (float-time) (* 3600 1.05)))))
         (help (ghostherd-usage--help 'claude wins)))
    (should (string-match-p "22% used" help))
    (should (string-match-p "78% left" help))
    (should (string-match-p "resets in 1h0[23]m" help))))

(ert-deftest ghostherd-test-usage-eta ()
  (should (equal (ghostherd-usage--eta (+ (float-time) 125)) "2m"))
  (should (equal (ghostherd-usage--eta (+ (float-time) 3725)) "1h02m"))
  (should-not (ghostherd-usage--eta (- (float-time) 60)))
  (should-not (ghostherd-usage--eta nil)))

(ert-deftest ghostherd-test-usage-line-in-footer ()
  (let ((ghostherd-usage--cache
         '((claude . (:windows ((:label "5h" :remaining 62.0)) :error nil))
           (grok . (:windows ((:label "wk" :remaining 11.0)) :error nil)))))
    (let ((line (substring-no-properties (ghostherd-usage-line 120))))
      (should (string-match-p "claude" line))
      ;; 62% left of the 5h window is 38% used.
      (should (string-match-p "38" line))
      (should (string-match-p "█\\|░" line))
      (should-not (string-match-p "%" line))
      (should (string-match-p "grok" line)))
    (ghostherd-tests--with-herd ()
      (let ((ghostherd-sidebar-width 80)
            (ghostherd--sidebar-target-width 80))
        (should (string-match-p "claude"
                                (substring-no-properties
                                 (ghostherd--sidebar-footer))))))))

(ert-deftest ghostherd-test-usage-401-does-not-prompt ()
  "A 401 from our own hosts must reach our callback, not the minibuffer.
t is url.el\'s \"already tried, give up\" answer; nil would make it
retry and never activate the callback."
  (let ((called nil))
    (cl-letf (((symbol-function 'url-http-handle-authentication)
               (lambda (&rest _) (setq called t) 'orig)))
      ;; Our request: answered without consulting url.el.
      (let ((url-current-object (url-generic-parse-url
                                 "https://daily-cloudcode-pa.googleapis.com/v1internal:x")))
        (should (eq (ghostherd-usage--no-auth-prompt
                     (symbol-function 'url-http-handle-authentication) nil)
                    t))
        (should-not called))
      ;; Somebody else\'s request: untouched.
      (let ((url-current-object (url-generic-parse-url "https://example.com/")))
        (should (eq (ghostherd-usage--no-auth-prompt
                     (symbol-function 'url-http-handle-authentication) nil)
                    'orig))
        (should called)))))

(ert-deftest ghostherd-test-usage-http-deadline-fires-callback-once ()
  "A response that never lands must still finish the request.
Without this the batch counter stays pinned and gates every
later refresh."
  (let ((got nil) (deadline nil) (buf nil))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (&rest _)
                 (setq buf (generate-new-buffer " *gh-usage-test*"))))
              ((symbol-function 'run-with-timer)
               (lambda (_secs _repeat fn &rest args)
                 (setq deadline (lambda () (apply fn args)))
                 nil)))
      (ghostherd-usage--http "https://api.anthropic.com/api/oauth/usage" nil
                             (lambda (code body) (push (list code body) got)))
      (should-not got)
      (funcall deadline)
      (funcall deadline)
      (should (equal got '((0 "")))))
    (when (buffer-live-p buf) (kill-buffer buf))))

(ert-deftest ghostherd-test-usage-agy-failure-is-visible ()
  "agy must never render as a bare dash when the fetch failed.
No cache entry reads as \"not wired up\"; `agy ?\' carries the reason."
  (let ((ghostherd-usage--cache nil))
    (cl-letf (((symbol-function 'ghostherd-usage--agy-probe) (lambda () nil)))
      (ghostherd-usage--agy-fail "refresh 401"))
    (let ((pl (alist-get 'agy ghostherd-usage--cache)))
      (should pl)
      (should (equal (plist-get pl :error) "refresh 401")))
    (let ((s (ghostherd-usage--entry-string 'agy nil)))
      (should (string-match-p "agy \\?" (substring-no-properties s)))
      (should (equal (get-text-property 0 'help-echo s) "refresh 401")))))

(defmacro ghostherd-tests--with-agy-token (blob &rest body)
  "BODY with agy's saved token read as BLOB, and every request recorded."
  (declare (indent 1))
  `(let ((ghostherd-usage--cache nil) (requests nil) (done 0))
     (cl-letf (((symbol-function 'ghostherd-usage--agy-from-screens) #'ignore)
               ((symbol-function 'ghostherd-usage--agy-token-blob) (lambda () ,blob))
               ((symbol-function 'ghostherd-usage--http-json)
                (lambda (url &rest _) (push url requests)))
               ((symbol-function 'ghostherd-usage--done) (lambda () (cl-incf done))))
       ,@body)))

(ert-deftest ghostherd-test-usage-agy-expired-token-is-said-not-refreshed ()
  "agy refreshes its token in memory only; refreshing it ourselves needs a
client secret Google no longer takes.  An expired token is reported,
with when, and nothing is sent."
  (ghostherd-tests--with-agy-token
      `((access_token . "a") (refresh_token . "r")
        (expiry . ,(format-time-string "%FT%T%z" (- (float-time) 3600))))
    (let ((ghostherd-usage-agy-probe nil))
      (ghostherd-usage--fetch-agy))
    (should-not requests)
    (should (= done 1))
    ;; An hour ago is yesterday in the hour after midnight: the day then shows.
    (should (string-match-p (if (equal (format-time-string "%F" (- (float-time) 3600))
                                       (format-time-string "%F"))
                                "\\`agy's saved token expired [0-9][0-9]:[0-9][0-9]\\'"
                              "\\`agy's saved token expired [0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]\\'")
                            (plist-get (alist-get 'agy ghostherd-usage--cache) :error)))))

(ert-deftest ghostherd-test-usage-agy-fresh-token-asks-the-api ()
  (ghostherd-tests--with-agy-token
      `((access_token . "a") (expiry . ,(format-time-string "%FT%T%z" (+ (float-time) 3600))))
    (ghostherd-usage--fetch-agy)
    (should (equal requests
                   '("https://daily-cloudcode-pa.googleapis.com/v1internal:retrieveUserQuotaSummary")))))

(ert-deftest ghostherd-test-usage-agy-probe-can-be-switched-off ()
  "The probe types into your session: off means it never does."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--session :name "agy-a" :kind 'agy :state 'idle)
    (let (typed (ghostherd-usage--agy-probe-at 0))
      (cl-letf (((symbol-function 'ghostherd--host-send-text)
                 (lambda (_s text &rest _) (push text typed)))
                ((symbol-function 'ghostherd--session-live-p) (lambda (_) t))
                ((symbol-function 'run-with-timer) #'ignore))
        (let ((ghostherd-usage-agy-probe nil))
          (should-not (ghostherd-usage--agy-probe)))
        (should-not typed)
        (let ((ghostherd-usage-agy-probe t))
          (should (ghostherd-usage--agy-probe)))
        (should (equal typed '("/usage")))))))

(ert-deftest ghostherd-test-usage-failure-keeps-last-good-numbers ()
  "A failed poll reads as stale numbers, not as no numbers."
  (let ((ghostherd-usage--cache nil))
    (ghostherd-usage--put 'agy (list (ghostherd-usage--window "gem-5h" 94.0)))
    (let ((fetched (plist-get (alist-get 'agy ghostherd-usage--cache) :fetched)))
      (cl-letf (((symbol-function 'ghostherd-usage--agy-probe) (lambda () nil)))
        (ghostherd-usage--agy-fail "refresh 401"))
      (let ((pl (alist-get 'agy ghostherd-usage--cache)))
        (should (= (length (plist-get pl :windows)) 1))
        (should (equal (plist-get pl :error) "refresh 401"))
        ;; Stale means stale: the age must not be reset by the failure.
        (should (= (plist-get pl :fetched) fetched))))
    (let ((s (ghostherd-usage--entry-string 'agy nil)))
      (should (string-match-p "6 gem-5h" (substring-no-properties s)))
      (should (string-match-p "stale: refresh 401"
                              (get-text-property 0 'help-echo s))))))

(ert-deftest ghostherd-test-usage-stale-batch-unblocks-refresh ()
  "A batch older than every deadline is a dropped callback, not work."
  (let ((fetched 0))
    (cl-letf (((symbol-function 'ghostherd-usage--grok-from-log) #'ignore)
              ((symbol-function 'ghostherd-usage--redraw) #'ignore)
              ((symbol-function 'ghostherd-usage--fetch-claude)
               (lambda () (cl-incf fetched)))
              ((symbol-function 'ghostherd-usage--fetch-grok)
               (lambda () (cl-incf fetched)))
              ((symbol-function 'ghostherd-usage--fetch-agy)
               (lambda () (cl-incf fetched))))
      (let ((noninteractive nil)
            (ghostherd-usage-timeout 15)
            (ghostherd-usage--inflight 1)
            (ghostherd-usage--inflight-at (- (float-time) 1000)))
        (ghostherd-usage-refresh)
        (should (= fetched 3))
        (should (= ghostherd-usage--inflight 3)))
      ;; A batch still inside its deadline keeps gating.
      (setq fetched 0)
      (let ((noninteractive nil)
            (ghostherd-usage-timeout 15)
            (ghostherd-usage--inflight 1)
            (ghostherd-usage--inflight-at (float-time)))
        (ghostherd-usage-refresh)
        (should (= fetched 0))))))

(ert-deftest ghostherd-test-poll-tick-does-not-refresh-usage ()
  "Usage HTTP must not ride the 1.5s poll (same reason as herd mail)."
  (let ((usage nil))
    (cl-letf (((symbol-function 'ghostherd-usage-refresh)
               (lambda (&rest _) (setq usage t)))
              ((symbol-function 'ghostherd-poll-all) #'ignore)
              ((symbol-function 'ghostherd--sidebar-on-screen-p)
               (lambda () t))
              ((symbol-function 'ghostherd--sidebar-refresh) #'ignore))
      (ghostherd--poll-tick)
      (should-not usage))))


;;; Memory page (pure: no xwidget, no sidecar)

(defmacro ghostherd-test--with-page (&rest body)
  "Run BODY with sidecar requests and page calls recorded.
`requests' holds (METHOD PARAMS) oldest first; REPLY, a function of
METHOD and PARAMS, answers them.  `calls' holds (FN . OBJ) oldest first."
  (declare (indent 0))
  `(progn
     (skip-unless (require 'ghostherd-memory-page nil t))
     (let ((requests nil) (calls nil) (reply (lambda (_m _p) nil)) (fail nil) (watched 0)
           (ghostherd-memory-page--importing nil)
           (ghostherd-memory-page--progress-shown nil)
           (ghostherd-memory-page--progress-inflight nil)
           (ghostherd-memory-page--progress-timer nil))
       (cl-letf (((symbol-function 'ghostherd-memory-request-async)
                  (lambda (method cb &optional params err-cb)
                    (setq requests (append requests (list (list method params))))
                    (if fail (funcall err-cb fail) (funcall cb (funcall reply method params)))))
                 ((symbol-function 'ghostherd-memory-page--js)
                  (lambda (fn obj) (setq calls (append calls (list (cons fn obj))))))
                 ;; The tick is called by hand; no timer outlives a test.
                 ((symbol-function 'ghostherd-memory-page--watch-import)
                  (lambda () (setq watched (1+ watched)))))
         ,@body))))

(defun ghostherd-test--call (calls fn)
  (cdr (car (last (seq-filter (lambda (c) (equal (car c) fn)) calls)))))

(ert-deftest ghostherd-test-memory-page-search ()
  "Empty asks nothing; a query goes with its filters, its seq comes back."
  (ghostherd-test--with-page
    (setq reply (lambda (_m _p) '(:hits ((:text "a" :chunk_index 3) (:text "b" :chunk_index 9)))))
    (ghostherd-memory-page--handle '((op . "search") (seq . 7) (q . "  ") (agent . "") (project . "")))
    (should-not requests)
    (should (equal (ghostherd-test--call calls "renderHits") '(:seq 7 :q "" :hits [])))
    (ghostherd-memory-page--handle '((op . "search") (seq . 8) (q . " retry ") (agent . "claude")
                                     (project . "") (limit . 500)))
    (should (equal requests '(("memory_search" (:query "retry" :limit 120 :agent "claude")))))
    (let ((hits (ghostherd-test--call calls "renderHits")))
      (should (equal (plist-get hits :seq) 8))
      (should (vectorp (plist-get hits :hits)))
      (should (= (length (plist-get hits :hits)) 2)))))

(ert-deftest ghostherd-test-memory-page-search-failure-names-its-seq ()
  (ghostherd-test--with-page
    (setq fail "connection refused")
    (ghostherd-memory-page--handle '((op . "search") (seq . 4) (q . "x")))
    (should (equal (ghostherd-test--call calls "searchFailed")
                   '(:seq 4 :error "connection refused")))))

(ert-deftest ghostherd-test-memory-page-open-source ()
  "The page places the window; Emacs clamps it and hands focus back."
  (ghostherd-test--with-page
    (setq reply (lambda (_m _p) '(:total 900 :chunks ((:chunk_index 40) (:chunk_index 41)))))
    (ghostherd-memory-page--handle '((op . "open-source") (source_path . "/s.jsonl") (offset . 40)
                                     (limit . 5000) (focus . 80) (mode . "open")))
    (should (equal requests '(("memory_chunks" (:source_path "/s.jsonl" :offset 40 :limit 400)))))
    (let ((p (ghostherd-test--call calls "renderSource")))
      (should (equal (plist-get p :focus) 80))
      (should (equal (plist-get p :mode) "open"))
      (should (equal (plist-get p :total) 900))
      (should (vectorp (plist-get p :chunks))))
    ;; JSON null reads as :null, which json-encode refuses: it must not
    ;; reach the reply.
    (ghostherd-memory-page--handle '((op . "open-source") (source_path . "/s.jsonl") (focus . :null)))
    (should (json-encode (ghostherd-test--call calls "renderSource")))
    (should-not (plist-get (ghostherd-test--call calls "renderSource") :focus))))

(ert-deftest ghostherd-test-memory-page-context-is-arrays ()
  "json.el reads a list of plists as one alist: the sessions must
reach the page as an array of objects."
  (ghostherd-test--with-page
    (setq reply (lambda (_m _p) '(:total 2 :sources ((:source_path "/a" :agent "claude")
                                                     (:source_path "/b" :agent "grok")))))
    (let ((ghostherd-memory-page--project "/p")
          (ghostherd-memory-page--pending '(:view "sessions" :query nil :project nil)))
      (ghostherd-memory-page--handle '((op . "ready")))
      (should-not ghostherd-memory-page--pending))
    (let* ((ctx (ghostherd-test--call calls "setContext"))
           (back (json-parse-string (json-encode ctx) :object-type 'alist :array-type 'list)))
      (should (equal (plist-get ctx :view) "sessions"))
      (should (equal (plist-get ctx :current_project) "/p"))
      (should (equal (mapcar (lambda (s) (alist-get 'source_path s)) (alist-get 'sources back))
                     '("/a" "/b"))))))

(ert-deftest ghostherd-test-memory-page-links-and-copy ()
  (ghostherd-test--with-page
    (let (browsed copied)
      (cl-letf (((symbol-function 'browse-url) (lambda (u &rest _) (setq browsed u)))
                ((symbol-function 'xwapp-copy) (lambda (s) (setq copied s))))
        (ghostherd-memory-page--handle '((op . "open-browser") (url . "file:///etc/passwd")))
        (ghostherd-memory-page--handle '((op . "open-browser") (url . "javascript:alert(1)")))
        (should-not browsed)
        (ghostherd-memory-page--handle '((op . "open-browser") (url . "https://example.com/x")))
        (should (equal browsed "https://example.com/x"))
        (ghostherd-memory-page--handle '((op . "copy") (text . "a message")))
        (should (equal copied "a message"))
        (should (equal (ghostherd-test--call calls "flash") "Copied"))))))

(ert-deftest ghostherd-test-memory-page-errors-reach-the-page ()
  (ghostherd-test--with-page
    (ghostherd-memory-page--handle '((op . "open-source") (source_path . "")))
    (should (string-match-p "No source" (ghostherd-test--call calls "showError")))))


(ert-deftest ghostherd-test-memory-page-import-progress ()
  "Progress while the sidecar imports; the end of a background import
is announced and lists the sessions again."
  (ghostherd-test--with-page
    (cl-letf (((symbol-function 'xwapp-live-p) (lambda (_) t)))
      (setq reply (lambda (m _p)
                    (if (equal m "memory_status")
                        '(:import (:running t :files 3 :imported 223 :agent "claude"
                                   :current "/Users/x/.claude/projects/p/a0c6b62b-e1d2.jsonl"))
                      '(:sources nil :total 0))))
      (ghostherd-memory-page--progress-tick)
      (should (equal (ghostherd-test--call calls "importProgress")
                     '(:files 3 :imported 223 :agent "claude" :current "a0c6b62b-e1d2.jsonl")))
      (should ghostherd-memory-page--progress-shown)
      ;; It ends.
      (setq reply (lambda (m _p) (if (equal m "memory_status") '(:import nil) '(:sources nil))))
      (setq calls nil)
      (ghostherd-memory-page--progress-tick)
      (should (assoc "importProgress" calls))
      (should-not (ghostherd-test--call calls "importProgress"))
      (should (equal (ghostherd-test--call calls "flash") "Import finished"))
      (should (assoc "setContext" calls))
      (should-not ghostherd-memory-page--progress-shown)
      ;; Nothing running and nothing shown: quiet.
      (setq calls nil)
      (ghostherd-memory-page--progress-tick)
      (should-not calls))))

(ert-deftest ghostherd-test-memory-page-import-waits-for-the-sidecar ()
  "An import just asked for may not have begun at the first read; the
watch must not end before the answer comes."
  (ghostherd-test--with-page
    (cl-letf (((symbol-function 'xwapp-live-p) (lambda (_) t))
              ((symbol-function 'ghostherd-memory-page--unwatch)
               (lambda () (push "unwatch" calls))))
      (setq reply (lambda (_m _p) '(:import nil)))
      (let ((ghostherd-memory-page--importing t))
        (ghostherd-memory-page--progress-tick))
      (should-not calls))))

(ert-deftest ghostherd-test-memory-page-import-flow ()
  (ghostherd-test--with-page
    (setq reply (lambda (m _p)
                  (if (equal m "memory_import") '(:imported 3 :sessions 1) '(:sources nil :total 0))))
    (ghostherd-memory-page--handle '((op . "import")))
    (should (equal (mapcar #'car requests) '("memory_import" "memory_list")))
    (should (equal (plist-get (cdr (assoc "importProgress" calls)) :current) "starting"))
    (should-not (ghostherd-test--call calls "importProgress"))
    (should (equal (ghostherd-test--call calls "flash") "Imported 3 new chunks from 1 sessions"))
    (should-not ghostherd-memory-page--importing)
    (should (>= watched 2))
    ;; A background import already running.
    (setq reply (lambda (m _p) (if (equal m "memory_import") '(:busy t) '(:sources nil))))
    (ghostherd-memory-page--handle '((op . "import")))
    (should (equal (ghostherd-test--call calls "flash") "A background import is already running"))
    ;; A failure.
    (setq fail "sidecar gone")
    (ghostherd-memory-page--handle '((op . "import")))
    (should (string-match-p "Import: sidecar gone" (ghostherd-test--call calls "showError")))
    (should-not ghostherd-memory-page--importing)))


(ert-deftest ghostherd-test-memory-page-log-entry ()
  "A state entry comes apart into old, new and reason; others pass as text."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let* ((t0 (encode-time '(0 14 2 8 10 2026)))
         (blocked (ghostherd-memory-page--log-entry
                   (ghostherd-log-entry--create
                    :time t0 :session "reviewer" :kind 'state
                    :text "working → blocked  Bash(rm -rf build/) — proceed?\n  1. Yes"
                    :screen "the screen")))
         (plain (ghostherd-memory-page--log-entry
                 (ghostherd-log-entry--create :time t0 :session "a" :kind 'state :text "starting → idle")))
         (input (ghostherd-memory-page--log-entry
                 (ghostherd-log-entry--create :time t0 :session "a" :kind 'input :text "← review the diff"))))
    (should (equal (plist-get blocked :t) (* 1000 (float-time t0))))
    (should (equal (plist-get blocked :old) "working"))
    (should (equal (plist-get blocked :new) "blocked"))
    (should (equal (plist-get blocked :reason) "Bash(rm -rf build/) — proceed?\n  1. Yes"))
    (should (equal (plist-get blocked :screen) "the screen"))
    (should (equal (plist-get plain :new) "idle"))
    (should (equal (plist-get plain :reason) ""))
    (should-not (plist-member plain :screen))
    (should (equal (plist-get input :kind) "input"))
    (should-not (plist-member input :new))))

(ert-deftest ghostherd-test-memory-page-log-flow ()
  "The log is sent oldest first, read from the file if need be; new
entries reach an open page only."
  (ghostherd-test--with-page
    (let ((ghostherd--log nil) (ghostherd--log-loaded nil) (loaded nil))
      (cl-letf (((symbol-function 'ghostherd-log-load)
                 (lambda ()
                   (setq loaded t ghostherd--log-loaded t
                         ghostherd--log
                         (list (ghostherd-log-entry--create :session "b" :kind 'state :text "idle → working")
                               (ghostherd-log-entry--create :session "a" :kind 'life :text "spawned claude on tmux"))))))
        (ghostherd-memory-page--handle '((op . "log")))
        (should loaded)
        (let ((entries (plist-get (ghostherd-test--call calls "renderLog") :entries)))
          (should (vectorp entries))
          (should (equal (mapcar (lambda (e) (plist-get e :session)) entries) '("a" "b")))))
      (setq calls nil)
      (let ((e (ghostherd-log-entry--create :session "c" :kind 'state :text "working → done")))
        (cl-letf (((symbol-function 'xwapp-session) (lambda (_) nil)))
          (ghostherd-memory-page--on-log e))
        (should-not calls)
        (cl-letf (((symbol-function 'xwapp-session) (lambda (_) t)))
          (ghostherd-memory-page--on-log e))
        (should (equal (plist-get (ghostherd-test--call calls "logAdd") :new) "done"))))))

(ert-deftest ghostherd-test-memory-page-log-needs-no-sidecar ()
  "Asked for the log, the page shows it before the sessions answer."
  (ghostherd-test--with-page
    (setq reply (lambda (_m _p) '(:sources nil :total 0)))
    (let ((ghostherd-memory-page--pending '(:view "log")))
      (ghostherd-memory-page--handle '((op . "ready")))
      (should-not ghostherd-memory-page--pending))
    (should (equal (car (car calls)) "showView"))
    (should (equal (cdr (car calls)) "log"))
    (should (assoc "setContext" calls))))

(ert-deftest ghostherd-test-log-functions-hear-each-entry ()
  "Listeners get each entry; a broken one costs the herd nothing."
  (let ((ghostherd--log nil) (ghostherd-log-file nil) (heard nil)
        (ghostherd-log-functions nil))
    (add-hook 'ghostherd-log-functions (lambda (e) (push (ghostherd-log-entry-text e) heard)))
    (add-hook 'ghostherd-log-functions (lambda (_) (error "Listener bug")))
    (cl-letf (((symbol-function 'message) #'ignore))
      (ghostherd--log-add "a" 'state "idle → working"))
    (should (equal heard '("idle → working")))
    (should (equal (ghostherd-log-entry-text (car ghostherd--log)) "idle → working"))))


;;; Resuming a past conversation

(ert-deftest ghostherd-test-resume-args ()
  (should (equal (ghostherd--resume-args 'claude "a0c6b62b-e1d2") '("-r" "a0c6b62b-e1d2")))
  (should (equal (ghostherd--resume-args 'claude "a0c6" t) '("-r" "a0c6" "--fork-session")))
  (should (equal (ghostherd--resume-args 'grok "g1" t) '("-r" "g1" "--fork-session")))
  (should (equal (ghostherd--resume-args 'agy "c9") '("--conversation" "c9")))
  (should-not (ghostherd--resume-args 'agy "c9" t))
  (should-not (ghostherd--resume-args 'shell "x"))
  ;; An id comes from an index of files: one that reads as a flag is refused.
  (should-not (ghostherd--resume-args 'claude "-rf"))
  (should-not (ghostherd--resume-args 'claude "a b"))
  (should-not (ghostherd--resume-args 'claude nil)))

(ert-deftest ghostherd-test-resume-spawns-in-the-directory ()
  (let ((ghostherd--sessions (make-hash-table :test #'equal))
        (dir (make-temp-file "gh-proj-" t))
        spawned)
    (unwind-protect
        (cl-letf (((symbol-function 'ghostherd-spawn)
                   (lambda (kind &rest plist) (setq spawned (cons kind plist))))
                  ((symbol-function 'ghostherd-project-defaults)
                   (lambda (&rest _) '(claude "--permission-mode" "acceptEdits")))
                  ((symbol-function 'ghostherd--project-root) (lambda (&rest _) nil)))
          (ghostherd-resume 'claude "a0c6b62b-e1d2-4371" dir :title "Sentry triage")
          (should (eq (car spawned) 'claude))
          (should (equal (plist-get (cdr spawned) :args)
                         '("--permission-mode" "acceptEdits" "-r" "a0c6b62b-e1d2-4371")))
          (should (equal (plist-get (cdr spawned) :directory) (file-name-as-directory dir)))
          (should (equal (plist-get (cdr spawned) :name)
                         (format "claude-%s" (file-name-nondirectory dir))))
          (should (equal (plist-get (cdr spawned) :notes) "resumed a0c6b62b: Sentry triage"))
          (ghostherd-resume 'claude "a0c6b62b" dir :fork t)
          (should (member "--fork-session" (plist-get (cdr spawned) :args)))
          (should (string-prefix-p "forked from" (plist-get (cdr spawned) :notes)))
          (should-error (ghostherd-resume 'agy "c9" dir :fork t) :type 'user-error)
          (should-error (ghostherd-resume 'claude "a0c6" (expand-file-name "gone" dir)) :type 'user-error))
      (delete-directory dir t))))

(ert-deftest ghostherd-test-resumed-by-finds-the-agent-on-it ()
  (let ((ghostherd--sessions (make-hash-table :test #'equal)))
    (puthash "claude-x" (ghostherd-session--create :id "claude-x" :name "claude-x" :kind 'claude
                                                   :args '("-r" "id-1"))
             ghostherd--sessions)
    ;; A fork of id-1 is a conversation of its own.
    (puthash "claude-x-2" (ghostherd-session--create :id "claude-x-2" :name "claude-x-2" :kind 'claude
                                                     :args '("-r" "id-1" "--fork-session"))
             ghostherd--sessions)
    (puthash "claude-y" (ghostherd-session--create :id "claude-y" :name "claude-y" :kind 'claude
                                                   :args '("-r" "id-3" "--fork-session"))
             ghostherd--sessions)
    (cl-letf (((symbol-function 'ghostherd--session-live-p) (lambda (_) t)))
      (should (equal (ghostherd-session-name (ghostherd--resumed-by "id-1")) "claude-x"))
      (should-not (ghostherd--resumed-by "id-3"))
      (should-not (ghostherd--resumed-by "id-2")))
    (cl-letf (((symbol-function 'ghostherd--session-live-p) (lambda (_) nil)))
      (should-not (ghostherd--resumed-by "id-1")))))

(ert-deftest ghostherd-test-memory-page-resume-info ()
  "Resumable only when the CLI can, the file and the directory are
there, and it is a conversation; otherwise the page is told why."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let* ((root (make-temp-file "gh-resume-" t))
         (proj (expand-file-name "proj" root))
         (file (expand-file-name "a0c6b62b-e1d2.jsonl" root))
         (ghostherd-memory-page--resumable (make-hash-table :test #'equal))
         (chunk (lambda (&rest kv)
                  (append kv (list :agent "claude" :session_id "inside-id" :project proj
                                   :source_kind "transcript" :title "T")))))
    (unwind-protect
        (cl-letf (((symbol-function 'ghostherd--resumed-by) (lambda (_) nil)))
          (make-directory proj)
          (write-region "{}" nil file)
          (let ((ok (ghostherd-memory-page--resume-info file (list (funcall chunk)))))
            (should (eq (plist-get ok :ok) t))
            ;; claude resumes by the file's name, not a sessionId inside it.
            (should (equal (plist-get ok :session) "a0c6b62b"))
            (should (eq (plist-get ok :fork) t))
            (should (numberp (plist-get ok :recent)))
            (should (equal (gethash file ghostherd-memory-page--resumable)
                           (list 'claude "a0c6b62b-e1d2" proj "T"))))
          (set-file-times file (time-subtract nil 3600))
          (should-not (plist-get (ghostherd-memory-page--resume-info file (list (funcall chunk))) :recent))
          (cl-flet ((why (path chunks) (plist-get (ghostherd-memory-page--resume-info path chunks) :why)))
            (should (string-match-p "Notes" (why file (list (append '(:source_kind "memory") (funcall chunk))))))
            (should (string-match-p "cleanupPeriodDays" (why (expand-file-name "gone.jsonl" root) (list (funcall chunk)))))
            (should (string-match-p "directory .* is gone"
                                    (why file (list (append (list :project (expand-file-name "nope" root)) (funcall chunk))))))
            (should (string-match-p "cannot resume" (why file (list (append '(:agent "shell") (funcall chunk))))))
            (should-not (gethash (expand-file-name "gone.jsonl" root) ghostherd-memory-page--resumable))))
      (delete-directory root t))))

(ert-deftest ghostherd-test-memory-page-resume-uses-what-emacs-stored ()
  "The page names a source; the kind, id and directory are Emacs's."
  (ghostherd-test--with-page
    (let ((ghostherd-memory-page--resumable (make-hash-table :test #'equal))
          resumed visited (running nil))
      (puthash "/s.jsonl" (list 'claude "id-1" "/proj" "T") ghostherd-memory-page--resumable)
      (cl-letf (((symbol-function 'ghostherd-resume)
                 (lambda (kind id dir &rest kw)
                   (setq resumed (list kind id dir (plist-get kw :fork)))
                   (ghostherd-session--create :name "claude-proj")))
                ((symbol-function 'ghostherd--resumed-by) (lambda (_) running))
                ((symbol-function 'ghostherd-visit) (lambda (s) (setq visited s))))
        (ghostherd-memory-page--handle '((op . "resume") (source_path . "/s.jsonl") (fork . :false)
                                         (session_id . "-rf") (directory . "/etc")))
        (should (equal resumed '(claude "id-1" "/proj" nil)))
        (should (equal (ghostherd-test--call calls "flash") "Resumed as claude-proj"))
        (should (assoc "setResume" calls))
        (ghostherd-memory-page--handle '((op . "resume") (source_path . "/s.jsonl") (fork . t)))
        (should (equal (nth 3 resumed) t))
        ;; An agent already on it: go there, never a second writer.
        (setq resumed nil running (ghostherd-session--create :name "claude-proj"))
        (ghostherd-memory-page--handle '((op . "resume") (source_path . "/s.jsonl") (fork . :false)))
        (should-not resumed)
        (should visited)
        ;; A source Emacs never found resumable.
        (ghostherd-memory-page--handle '((op . "resume") (source_path . "/other.jsonl")))
        (should (string-match-p "cannot be resumed" (ghostherd-test--call calls "showError")))))))


;;; The agents tab

(defmacro ghostherd-test--with-herd (&rest body)
  "BODY with a fresh herd of one blocked claude agent and page calls recorded."
  (declare (indent 0))
  `(ghostherd-test--with-page
     (let ((ghostherd--sessions (make-hash-table :test #'equal))
           (ghostherd-usage--cache
            '((claude :windows ((:label "5h" :remaining 38 :resets-at 1791460000.0)
                                (:label "7d" :remaining 100 :resets-at nil))
                      :fetched 1791450000.0 :error nil)
              (agy :windows nil :fetched 1791450000.0 :error "token expired")))
           (ghostherd-memory-page--herd-timer nil))
       (puthash "claude-api"
                (ghostherd-session--create :id "claude-api" :name "claude-api" :kind 'claude
                                           :state 'blocked :state-reason "Bash(x) — proceed?"
                                           :project "/p/" :backend 'tmux :notes "api work")
                ghostherd--sessions)
       ,@body)))

(defvar ghostherd-test--prs-app)
(defvar ghostherd-test--closed-app)

(ert-deftest ghostherd-test-herd-says-what-opens-on-the-phone ()
  "The page is told which of pr-view and clickup-view your other devices
can open, by Emacs's address for them that holds no secret: loaded,
able to open by themselves, with somewhere to be reached from.  Not
so, a chip there goes to the forge or ClickUp."
  (ghostherd-test--with-herd
    (let ((ghostherd-test--prs-app (xwapp-create :buffer-name " *t-prs*" :index "/x/prs/ui/index.html"
                                                 :reopen #'ignore))
          (ghostherd-test--closed-app (xwapp-create :buffer-name " *t-closed*" :index "/x/c/ui/index.html"))
          (xwapp-remote-hosts '("box.tail0.ts.net")))
      (should (eq (ghostherd-memory-page--opens-elsewhere 'ghostherd-test--prs-app) t))
      (should (eq (ghostherd-memory-page--opens-elsewhere 'ghostherd-test--closed-app) :json-false))
      (should (eq (ghostherd-memory-page--opens-elsewhere 'ghostherd-test--never-loaded) :json-false))
      (let ((xwapp-remote-hosts nil))
        (should (eq (ghostherd-memory-page--opens-elsewhere 'ghostherd-test--prs-app) :json-false)))
      (cl-letf (((symbol-function 'ghostherd-memory-page--opens-elsewhere)
                 (lambda (var) (if (eq var 'pr-view--app) t :json-false))))
        (ghostherd-memory-page--send-herd)
        (should (equal (plist-get (ghostherd-test--call calls "renderHerd") :open)
                       '(:pr t :task :json-false))))
      (remhash ghostherd-test--prs-app xwapp--served)
      (remhash ghostherd-test--closed-app xwapp--served))))

(ert-deftest ghostherd-test-herd-page-snapshot ()
  (ghostherd-test--with-herd
    (ghostherd-memory-page--send-herd)
    (let* ((p (ghostherd-test--call calls "renderHerd"))
           (a (aref (plist-get p :agents) 0))
           (claude (seq-find (lambda (u) (equal (plist-get u :kind) "claude")) (plist-get p :usage)))
           (agy (seq-find (lambda (u) (equal (plist-get u :kind) "agy")) (plist-get p :usage))))
      (should (equal (plist-get a :state) "blocked"))
      (should (equal (plist-get a :project) "/p/"))
      (should (eq (plist-get a :detached) t))
      (should (equal (mapcar (lambda (w) (plist-get w :used)) (plist-get claude :windows)) '(62.0 0.0)))
      (should (equal (plist-get (aref (plist-get claude :windows) 0) :resets) 1791460000000.0))
      (should (equal (plist-get agy :error) "token expired"))
      (should (member "claude" (append (plist-get p :kinds) nil)))
      (should-not (member "shell" (append (plist-get p :kinds) nil)))
      (should (json-encode p)))))

(ert-deftest ghostherd-test-herd-page-sends-asks ()
  (ghostherd-test--with-herd
    (let ((ghostherd--herd-asks
           '((:id "a1" :from "claude-api" :to "agy-a" :status "answered" :auto t
                  :error nil :head "fetch the docs" :reply_head "screen line"
                  :created_ms 1791450000000.0 :answered_ms 1791450060000.0
                  :parent "a0" :run "a0" :round 2 :of 4))))
      (ghostherd-memory-page--send-herd)
      (let ((k (aref (plist-get (ghostherd-test--call calls "renderHerd") :asks) 0)))
        (should (equal (plist-get k :id) "a1"))
        (should (equal (plist-get k :status) "answered"))
        (should (eq (plist-get k :auto) t))
        (should (equal (plist-get k :replyHead) "screen line"))
        (should (equal (plist-get k :answered) 1791450060000.0))
        (should (equal (list (plist-get k :parent) (plist-get k :run) (plist-get k :round) (plist-get k :of))
                       '("a0" "a0" 2 4)))
        (should (json-encode k))))))

(ert-deftest ghostherd-test-herd-page-shows-what-each-said ()
  (ghostherd-test--with-herd
    (let ((ghostherd--herd-links (make-hash-table :test 'equal)))
      (ghostherd--herd-take-links
       '((:session "claude-api" :cli "claude" :conversation "c-1"
                   :last_head "Tests pass." :last_ms 1791450000000.0)))
      (ghostherd-memory-page--send-herd)
      (let ((a (aref (plist-get (ghostherd-test--call calls "renderHerd") :agents) 0)))
        (should (equal (plist-get a :said) "Tests pass."))
        (should (equal (plist-get a :saidAt) 1791450000000.0))
        (should (equal (plist-get a :conversation) "c-1"))))))

(ert-deftest ghostherd-test-herd-page-hooks ()
  "The page gets each hook with an index, and opens only by that index."
  (ghostherd-test--with-herd
    (let* ((file (make-temp-file "gh-settings" nil ".json" "{\n  \"hooks\": {\n    \"Stop\": [\n      \"cmd\"\n    ]\n  }\n}\n"))
           (ghostherd-memory-page--hooks
            `(:ghostherd (:claude (:have ("Stop") :want ("SessionStart" "Stop")))
              :hooks ((:file ,file :line 4 :readers ("claude" "grok") :scope "global"
                             :source "claude settings.json" :event "Stop" :matcher ""
                             :command "cmd" :ours :json-false :enabled t)))))
      (unwind-protect
          (progn
            (ghostherd-memory-page--send-herd)
            (let* ((hooks (plist-get (ghostherd-test--call calls "renderHerd") :hooks))
                   (h (aref (plist-get hooks :list) 0)))
              (should (equal (plist-get h :i) 0))
              (should (equal (plist-get h :name) (file-name-nondirectory file)))
              (should (equal (plist-get h :readers) ["claude" "grok"]))
              (should (equal (plist-get (plist-get (plist-get hooks :status) :claude) :have) ["Stop"]))
              (should (json-encode hooks)))
            (save-window-excursion
              (ghostherd-memory-page--hook-open '((i . 0)))
              (should (equal (buffer-file-name) file))
              (should (= (line-number-at-pos) 4))
              (kill-buffer))
            (should-error (ghostherd-memory-page--hook-open '((i . 7))) :type 'user-error)
            (should-error (ghostherd-memory-page--hook-open `((i . ,file))) :type 'user-error))
        (delete-file file)))))

(ert-deftest ghostherd-test-herd-page-hooks-nothing-installed ()
  "Nothing installed is an empty list, and an empty list must reach the
page as [] -- json.el writes nil as null, and the page stopped drawing."
  (ghostherd-test--with-herd
    (let ((ghostherd-memory-page--hooks
           (json-parse-string
            "{\"ghostherd\":{\"claude\":{\"have\":[],\"want\":[\"Stop\"]}},\"hooks\":[],\"files\":[]}"
            :object-type 'plist :array-type 'list :null-object nil :false-object :json-false)))
      (ghostherd-memory-page--send-herd)
      (let ((json (json-encode (plist-get (ghostherd-test--call calls "renderHerd") :hooks))))
        (should (string-search "\"have\":[]" json))
        (should (string-search "\"want\":[\"Stop\"]" json))
        (should (string-search "\"list\":[]" json))))))

(ert-deftest ghostherd-test-herd-page-hooks-refresh-asks-for-each-project ()
  "The inventory runs in the background, with every herd project's own."
  (ghostherd-test--with-herd
    (let* ((dir (make-temp-file "gh-client" t))
           (client (expand-file-name "herd" dir))
           (ghostherd-memory-page--hooks nil)
           (ghostherd-memory-page--hooks-proc nil))
      (with-temp-file client
        (insert "#!/bin/sh\nprintf '%s\\n' \"$*\" > \"$(dirname \"$0\")/args\"\n"
                "echo '{\"ghostherd\":{},\"hooks\":[{\"file\":\"/f\",\"line\":1,\"readers\":[\"agy\"],\"ours\":false,\"enabled\":true}],\"files\":[]}'\n"))
      (set-file-modes client #o755)
      (cl-letf (((symbol-function 'ghostherd-herd-client) (lambda () client))
                ((symbol-function 'xwapp-session) (lambda (&rest _) t)))
        (ghostherd-memory-page--hooks-refresh)
        (let ((end (+ (float-time) 10)))
          (while (and (not ghostherd-memory-page--hooks) (< (float-time) end))
            (accept-process-output nil 0.05))))
      (should (equal (plist-get (car (plist-get ghostherd-memory-page--hooks :hooks)) :file) "/f"))
      (should (equal (with-temp-buffer (insert-file-contents (expand-file-name "args" dir))
                                       (string-trim (buffer-string)))
                     "hooks list --json --project /p/"))
      (should (ghostherd-test--call calls "renderHerd"))
      (delete-directory dir t))))

(defmacro ghostherd-test--with-room (&rest body)
  "BODY in the room of /p/, claude-api's transcript at `transcript'."
  (declare (indent 0))
  `(ghostherd-test--with-herd
     (let* ((file (make-temp-file "gh-room" nil ".jsonl" "{}\n"))
            (transcript file)
            (ghostherd-memory-page--room "/p/")
            (ghostherd-memory-page--room-screens nil)
            (ghostherd-memory-page--room-timer nil)
            (ghostherd-memory-page--room-sent (make-hash-table :test 'equal))
            (ghostherd-memory-page--room-reading (make-hash-table :test 'equal))
            (ghostherd-memory-page--room-chat nil)
            (ghostherd-memory-page--chat-sent nil)
            (ghostherd-memory-page--chat-reading nil)
            (ghostherd-memory-page--room-limits (make-hash-table :test 'equal))
            (ghostherd-memory-page--chat-limit nil))
       (cl-letf (((symbol-function 'ghostherd-session-transcript) (lambda (_s) transcript))
                 ((symbol-function 'ghostherd--host-capture) (lambda (&rest _) "the screen"))
                 ((symbol-function 'xwapp-session) (lambda (&rest _) t)))
         (unwind-protect (progn ,@body)
           (ghostherd-memory-page--room-close)
           (delete-file file))))))

(ert-deftest ghostherd-test-room-reads-a-conversation-once-per-change ()
  "The sidecar reads a transcript when it changed, not every 2s; the page
gets each entry's known fields, lists as arrays."
  (ghostherd-test--with-room
    (setq reply (lambda (_m _p)
                  '(:cli "claude" :earlier t
                    :entries ((:role "user" :kind "prompt" :text "hi" :ts "2026-10-09T01:00:00Z" :secret "x")
                              (:role "tools" :tools ((:name "Bash" :hint "ls")))
                              (:role "herd" :kind "ask" :to "qa" :ask "26910956" :text "round 2" :waiting t)))))
    (ghostherd-memory-page--room-tick)
    (should (equal requests `(("herd_conversation" (:path ,file)))))
    (let* ((p (ghostherd-test--call calls "setConversation"))
           (e (aref (plist-get p :entries) 0))
           (tools (plist-get (aref (plist-get p :entries) 1) :tools)))
      (should (equal (plist-get p :name) "claude-api"))
      (should (eq (plist-get p :earlier) t))
      (should (equal (plist-get e :text) "hi"))
      (should-not (plist-member e :secret))
      (should (equal (plist-get (aref tools 0) :hint) "ls"))
      (let ((ask (aref (plist-get p :entries) 2)))
        (should (equal (list (plist-get ask :ask) (plist-get ask :waiting) (plist-get ask :failed))
                       '("26910956" t :json-false))))
      (should (string-match-p "\"entries\":\\[{.*\"tools\":\\[{\"name\":\"Bash\"" (json-encode p))))
    (should-not (ghostherd-test--call calls "setScreen"))
    (ghostherd-memory-page--room-tick)
    (should (= (length requests) 1))
    (write-region "more\n" nil file t 'silent)
    (ghostherd-memory-page--room-tick)
    (should (= (length requests) 2))))

(ert-deftest ghostherd-test-room-shows-a-sub-agents-transcript ()
  "Open a sub-agent card: its own transcript is read with
`herd_sub_conversation' and pushed as setSubConversation, re-read only when
it changes; closing it forgets it."
  (ghostherd-test--with-room
    (setq reply (lambda (_m _p)
                  '(:earlier :json-false
                    :entries ((:role "user" :kind "prompt" :text "Review x" :ts "t")
                              (:role "assistant" :text "looking" :ts "t")))))
    (let ((subf (ghostherd-memory-page--sub-file file "a51b15851f7c7ae94")))
      (make-directory (file-name-directory subf) t)
      (write-region "{}\n" nil subf nil 'silent)
      (ghostherd-memory-page--handle
       `((op . "sub-watch") (name . "claude-api") (path . ,file) (sub . "a51b15851f7c7ae94")))
      (should (equal (car (last requests))
                     `("herd_sub_conversation" (:path ,file :sub "a51b15851f7c7ae94"))))
      (let ((p (ghostherd-test--call calls "setSubConversation")))
        (should (equal (plist-get p :name) "claude-api"))
        (should (equal (plist-get p :sub) "a51b15851f7c7ae94"))
        (should (equal (plist-get (aref (plist-get p :entries) 1) :text) "looking")))
      ;; Not re-read until the sub transcript itself changes.
      (let ((n (length requests)))
        (ghostherd-memory-page--sub-read)
        (should (= (length requests) n))
        (write-region "more\n" nil subf t 'silent)
        (ghostherd-memory-page--sub-read)
        (should (= (length requests) (1+ n))))
      (ghostherd-memory-page--handle '((op . "sub-unwatch")))
      (should-not ghostherd-memory-page--sub)
      (let ((n (length requests)))
        (ghostherd-memory-page--sub-read)
        (should (= (length requests) n))))))

(ert-deftest ghostherd-test-room-load-earlier-reads-more-and-keeps-it ()
  "A column's `load earlier' asks for more; Emacs reads at that limit, and
-- the limit being part of the signature -- keeps it across polls instead
of shrinking back to the default."
  (ghostherd-test--with-room
    (setq reply (lambda (_m _p)
                  '(:cli "claude" :earlier t :limit 120
                    :entries ((:role "assistant" :text "hi" :ts "t")))))
    (ghostherd-memory-page--room-tick)
    (should (equal (car (last requests)) `("herd_conversation" (:path ,file))))
    (should (eql (plist-get (ghostherd-test--call calls "setConversation") :limit) 120))
    (ghostherd-memory-page--handle
     '((op . "room-watch") (project . "/p/") (screens . [])
       (limits . [((name . "claude-api") (limit . 120))]) (chatLimit . 0)))
    (should (equal (car (last requests)) `("herd_conversation" (:path ,file :limit 120))))
    (let ((n (length requests)))
      (ghostherd-memory-page--room-tick)
      (should (= (length requests) n)))))

(ert-deftest ghostherd-test-chat-load-earlier-asks-for-more ()
  "The chat's `load earlier' grows its one limit, passed on to herd_chat."
  (ghostherd-test--with-room
    (setq reply (lambda (_m _p) '(:earlier :json-false :entries [] :missing [])))
    (ghostherd-memory-page--handle '((op . "room-watch") (project . "/p/") (screens . []) (view . "chat")))
    (should (equal (caar (last requests)) "herd_chat"))
    (should-not (plist-member (cadar (last requests)) :limit))
    (ghostherd-memory-page--handle
     '((op . "room-watch") (project . "/p/") (screens . []) (view . "chat") (chatLimit . 400)))
    (should (eql (plist-get (cadar (last requests)) :limit) 400))))

(ert-deftest ghostherd-test-room-as-one-chat ()
  "Shown as a chat, the room asks the sidecar for the project's timeline
when any transcript changed -- once, not per agent -- naming the agents
it knows none for; the page gets each item's known fields.  Back to
columns, every column is sent afresh: they were not kept up meanwhile."
  (ghostherd-test--with-room
    (puthash "claude-web" (ghostherd-session--create :id "claude-web" :name "claude-web" :kind 'claude
                                                     :state 'idle :project "/p/" :backend 'tmux)
             ghostherd--sessions)
    (cl-letf (((symbol-function 'ghostherd-session-transcript)
               (lambda (s) (and (equal (ghostherd-session-name s) "claude-api") transcript))))
      (setq reply (lambda (m _p)
                    (when (equal m "herd_chat")
                      '(:earlier t :missing ("gone")
                        :entries ((:who "user" :to "claude-api" :kind "prompt" :text "hi"
                                   :ts "2026-10-09T01:00:00Z" :agent "claude-api" :secret "x"))))))
      ;; In columns first, then the chat.
      (ghostherd-memory-page--handle '((op . "room-watch") (project . "/p/") (screens . [])))
      (should (ghostherd-test--call calls "setConversation"))
      (setq calls nil requests nil)
      (ghostherd-memory-page--handle '((op . "room-watch") (project . "/p/") (screens . []) (view . "chat")))
      (should (equal (mapcar #'car requests) '("herd_chat")))
      (should (equal (plist-get (cadr (car requests)) :agents) (vector (list :name "claude-api" :path file))))
      (let* ((p (ghostherd-test--call calls "setChat"))
             (e (aref (plist-get p :entries) 0)))
        (should (equal (list (plist-get p :project) (plist-get p :absent) (plist-get p :missing) (plist-get p :earlier))
                       '("/p/" ["claude-web"] ["gone"] t)))
        (should (equal (list (plist-get e :who) (plist-get e :to) (plist-get e :text) (plist-get e :failed))
                       '("user" "claude-api" "hi" :json-false)))
        (should-not (plist-member e :secret))
        (should (string-match-p "\"entries\":\\[{" (json-encode p))))
      (should-not (ghostherd-test--call calls "setConversation"))
      (ghostherd-memory-page--room-tick)
      (should (= (length requests) 1))
      (write-region "more\n" nil file t 'silent)
      (ghostherd-memory-page--room-tick)
      (should (= (length requests) 2))
      ;; Back to columns: each column again, though its transcript is as it was.
      (setq calls nil requests nil)
      (ghostherd-memory-page--handle '((op . "room-watch") (project . "/p/") (screens . []) (view . "columns")))
      (should (equal (mapcar #'car requests) '("herd_conversation")))
      (should (ghostherd-test--call calls "setConversation"))
      ;; And with the transcript as it was, too.
      (ghostherd-memory-page--handle '((op . "room-watch") (project . "/p/") (screens . []) (view . "chat")))
      (setq calls nil)
      (ghostherd-memory-page--handle '((op . "room-watch") (project . "/p/") (screens . []) (view . "columns")))
      (should (ghostherd-test--call calls "setConversation"))
      ;; A sidecar older than the chat says how to fix it.
      (setq calls nil fail "JSON-RPC -32601: Method not found: herd_chat")
      (ghostherd-memory-page--handle '((op . "room-watch") (project . "/p/") (screens . []) (view . "chat")))
      (should (string-match-p "older than the room" (plist-get (ghostherd-test--call calls "setChat") :error))))))

(ert-deftest ghostherd-test-room-without-a-transcript-shows-the-screen ()
  (ghostherd-test--with-room
    (setq transcript nil)
    (ghostherd-memory-page--room-tick)
    (ghostherd-memory-page--room-tick)
    (should-not requests)
    (should (equal (mapcar #'cdr (seq-filter (lambda (c) (equal (car c) "setConversation")) calls))
                   '((:name "claude-api" :entries [] :none t))))
    (should (equal (length (seq-filter (lambda (c) (equal (car c) "setScreen")) calls)) 2))
    (should (equal (plist-get (ghostherd-test--call calls "setScreen") :text) "the screen"))))

(ert-deftest ghostherd-test-room-screens-and-failures ()
  "A column the page shows by its screen gets the screen; a sidecar that
cannot read says why, and is not asked again until the file changes."
  (ghostherd-test--with-room
    (setq ghostherd-memory-page--room-screens '("claude-api")
          fail "connection refused")
    (ghostherd-memory-page--room-tick)
    (should (equal (plist-get (ghostherd-test--call calls "setConversation") :error) "connection refused"))
    (should (ghostherd-test--call calls "setScreen"))
    (ghostherd-memory-page--room-tick)
    (should (= (length requests) 1))
    ;; A sidecar from before the room: say what to do about it.
    (setq fail "ghostherd-memory JSON-RPC -32601: Method not found: herd_conversation")
    (write-region "more\n" nil file t 'silent)
    (ghostherd-memory-page--room-tick)
    (should (string-match-p "ghostherd-memory-stop" (plist-get (ghostherd-test--call calls "setConversation") :error)))))

(ert-deftest ghostherd-test-room-drops-a-late-answer ()
  "The room was left while the sidecar read: its answer goes nowhere."
  (ghostherd-test--with-room
    (let (pending)
      (cl-letf (((symbol-function 'ghostherd-memory-request-async)
                 (lambda (_m cb &rest _) (setq pending cb))))
        (ghostherd-memory-page--room-tick)
        (ghostherd-memory-page--room-tick)
        (ghostherd-memory-page--room-close)
        (funcall pending '(:entries ((:role "user" :text "late"))))
        (should-not (ghostherd-test--call calls "setConversation"))
        (should (= (hash-table-count ghostherd-memory-page--room-reading) 0))))))

(ert-deftest ghostherd-test-room-intents ()
  "Opening watches the room; the same room again only changes its screens;
closing, or leaving the tab, stops; a page gone stops it too."
  (ghostherd-test--with-room
    (setq ghostherd-memory-page--room nil)
    (ghostherd-memory-page--handle '((op . "room-watch") (project . "/p/") (screens . [])))
    (should (equal ghostherd-memory-page--room "/p/"))
    (should (timerp ghostherd-memory-page--room-timer))
    (should (= (length requests) 1))
    (ghostherd-memory-page--handle '((op . "room-watch") (project . "/p/") (screens . ["claude-api" 3])))
    (should (equal ghostherd-memory-page--room-screens '("claude-api")))
    (should (= (length requests) 1))
    (ghostherd-memory-page--handle '((op . "room-close")))
    (should-not (or ghostherd-memory-page--room ghostherd-memory-page--room-timer))
    (ghostherd-memory-page--handle '((op . "room-watch") (project . "/p/")))
    (ghostherd-memory-page--handle '((op . "herd-unwatch")))
    (should-not ghostherd-memory-page--room-timer)
    (ghostherd-memory-page--handle '((op . "room-watch") (project . "")))
    (should (ghostherd-test--call calls "showError"))
    (should-not ghostherd-memory-page--room)
    (setq ghostherd-memory-page--room "/p/")
    (cl-letf (((symbol-function 'xwapp-session) (lambda (&rest _) nil)))
      (ghostherd-memory-page--room-tick))
    (should-not ghostherd-memory-page--room)))

(ert-deftest ghostherd-test-herd-page-operations ()
  "Each operation reaches the herd function with the session, then the
herd is sent again; bad input is refused before anything is done."
  (ghostherd-test--with-herd
    (let (did)
      (cl-letf (((symbol-function 'ghostherd-prompt) (lambda (s text &rest _) (push (list 'prompt (ghostherd-session-name s) text) did)))
                ((symbol-function 'ghostherd-interrupt) (lambda (s) (push (list 'esc (ghostherd-session-name s)) did)))
                ((symbol-function 'ghostherd-abort) (lambda (s) (push (list 'abort (ghostherd-session-name s)) did)))
                ((symbol-function 'ghostherd-answer) (lambda (s n) (push (list 'answer (ghostherd-session-name s) n) did)))
                ((symbol-function 'ghostherd-respawn) (lambda (s c) (push (list 'respawn (ghostherd-session-name s) c) did)))
                ((symbol-function 'ghostherd-kill) (lambda (s k) (push (list 'kill (ghostherd-session-name s) k) did)))
                ((symbol-function 'ghostherd-set-notes) (lambda (s n) (push (list 'notes (ghostherd-session-name s) n) did)))
                ((symbol-function 'ghostherd-visit) (lambda (s) (push (list 'visit (ghostherd-session-name s)) did)))
                ((symbol-function 'ghostherd--host-capture) (lambda (_s &rest _) "SCREEN")))
        (dolist (intent '(((op . "agent-prompt") (name . "claude-api") (text . "run the tests"))
                          ((op . "agent-interrupt") (name . "claude-api"))
                          ((op . "agent-abort") (name . "claude-api"))
                          ((op . "agent-answer") (name . "claude-api") (n . 2))
                          ((op . "agent-respawn") (name . "claude-api") (continue . t))
                          ((op . "agent-notes") (name . "claude-api") (text . "reviewer"))
                          ((op . "agent-visit") (name . "claude-api"))
                          ((op . "agent-kill") (name . "claude-api"))))
          (ghostherd-memory-page--handle intent))
        (should (equal (reverse did)
                       '((prompt "claude-api" "run the tests") (esc "claude-api") (abort "claude-api")
                         (answer "claude-api" 2) (respawn "claude-api" t) (notes "claude-api" "reviewer")
                         (visit "claude-api") (kill "claude-api" t))))
        (should (equal (ghostherd-test--call calls "flash") "Killed claude-api"))
        (should (>= (length (seq-filter (lambda (c) (equal (car c) "renderHerd")) calls)) 8))
        (ghostherd-memory-page--handle '((op . "agent-screen") (name . "claude-api")))
        (should (equal (ghostherd-test--call calls "setScreen") '(:name "claude-api" :text "SCREEN")))
        ;; Refused, and nothing done.
        (setq did nil)
        (dolist (bad '(((op . "agent-prompt") (name . "claude-api") (text . "   "))
                       ((op . "agent-answer") (name . "claude-api") (n . 12))
                       ((op . "agent-answer") (name . "claude-api") (n . "2"))
                       ((op . "agent-kill") (name . "nobody"))))
          (setq calls nil)
          (ghostherd-memory-page--handle bad)
          (should (ghostherd-test--call calls "showError")))
        (should-not did)))))

(ert-deftest ghostherd-test-herd-page-new-agent ()
  (ghostherd-test--with-herd
    (let ((dir (make-temp-file "gh-new-" t)) spawned)
      (unwind-protect
          (cl-letf (((symbol-function 'ghostherd-spawn)
                     (lambda (kind &rest plist)
                       (setq spawned (cons kind plist))
                       (ghostherd-session--create :name (plist-get plist :name))))
                    ((symbol-function 'ghostherd-project-defaults) (lambda (&rest _) nil))
                    ((symbol-function 'ghostherd--project-root) (lambda (&rest _) nil)))
            (ghostherd-memory-page--handle `((op . "agent-new") (project . ,dir) (kind . "grok") (name . "")))
            (should (eq (car spawned) 'grok))
            (should (equal (plist-get (cdr spawned) :name) (format "grok-%s" (file-name-nondirectory dir))))
            (should (null (plist-get (cdr spawned) :display)))
            (should (plist-member (cdr spawned) :display))
            (should (string-prefix-p "Started grok-" (ghostherd-test--call calls "flash")))
            (setq spawned nil)
            (dolist (bad `(((op . "agent-new") (project . ,dir) (kind . "shell"))
                           ((op . "agent-new") (project . ,dir) (kind . "rm"))
                           ((op . "agent-new") (project . "/nope/here") (kind . "claude"))
                           ((op . "agent-new") (project . ,dir) (kind . "claude") (name . "-t evil"))
                           ((op . "agent-new") (project . ,dir) (kind . "claude") (name . "a;b"))))
              (setq calls nil)
              (ghostherd-memory-page--handle bad)
              (should (ghostherd-test--call calls "showError")))
            (should-not spawned))
        (delete-directory dir t)))))

(ert-deftest ghostherd-test-herd-page-watch ()
  "The herd is sent while the tab is on screen, at once on a change."
  (ghostherd-test--with-herd
    (unwind-protect
        (progn
          (ghostherd-memory-page--handle '((op . "herd-watch")))
          (should (timerp ghostherd-memory-page--herd-timer))
          (setq calls nil)
          (ghostherd-memory-page--on-herd-change nil)
          (should (assoc "renderHerd" calls))
          (ghostherd-memory-page--handle '((op . "herd-unwatch")))
          (should-not ghostherd-memory-page--herd-timer)
          (setq calls nil)
          (ghostherd-memory-page--on-herd-change nil)
          (should-not calls)
          ;; No page any more: the next tick stops the timer.
          (ghostherd-memory-page--handle '((op . "herd-watch")))
          (cl-letf (((symbol-function 'xwapp-session) (lambda (_) nil)))
            (ghostherd-memory-page--herd-tick))
          (should-not ghostherd-memory-page--herd-timer))
      (ghostherd-memory-page--herd-unwatch))))

(ert-deftest ghostherd-test-herd-page-opens-without-the-sidecar ()
  (ghostherd-test--with-page
    (setq reply (lambda (_m _p) '(:sources nil)))
    (let ((ghostherd-memory-page--pending '(:view "agents")))
      (ghostherd-memory-page--handle '((op . "ready"))))
    (should (equal (car calls) '("showView" . "agents")))))


;;; Background import

(defmacro ghostherd-test--with-auto (&rest body)
  "Run BODY with a temp data directory and sidecar requests recorded.
`methods' lists the methods asked, oldest first; POINTS is what
memory_status reports; FAIL, when a string, fails every request."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "gh-auto-" t))
          (ghostherd-memory-data-directory dir)
          (ghostherd-memory-auto-import-interval 86400)
          (ghostherd-memory--port 49152)
          (ghostherd-memory--auto-inflight nil)
          (ghostherd-memory--auto-failing nil)
          (methods nil) (points 5) (fail nil) (said nil))
     (unwind-protect
         (cl-letf (((symbol-function 'ghostherd-memory-request-async)
                    (lambda (method cb &optional _params err-cb)
                      (setq methods (append methods (list method)))
                      (cond (fail (funcall err-cb fail))
                            ((equal method "memory_status") (funcall cb (list :points points)))
                            (t (funcall cb '(:imported 12 :sessions 3))))))
                   ((symbol-function 'message)
                    (lambda (fmt &rest args) (push (apply #'format fmt args) said))))
           ,@body)
       (delete-directory dir t))))

(ert-deftest ghostherd-test-memory-auto-import-when-due ()
  "Once a day, stamped on disk so restarts do not repeat it."
  (ghostherd-test--with-auto
    (should (ghostherd-memory--auto-due-p))
    (ghostherd-memory--auto-import-maybe)
    (should (equal methods '("memory_status" "memory_import")))
    (should-not ghostherd-memory--auto-inflight)
    (should (string-match-p "added 12 chunks from 3 sessions" (car said)))
    (should-not (ghostherd-memory--auto-due-p))
    (should (ghostherd-memory--auto-due-p (time-add (current-time) 86401)))
    (setq methods nil)
    (ghostherd-memory--auto-import-maybe)
    (should-not methods)))

(ert-deftest ghostherd-test-memory-auto-import-never-the-first ()
  "An empty index waits for a deliberate first import: that one
downloads a model and embeds everything."
  (ghostherd-test--with-auto
    (setq points 0)
    (ghostherd-memory--auto-import-maybe)
    (should (equal methods '("memory_status")))
    (should-not (file-exists-p (ghostherd-memory--auto-stamp)))
    (should-not ghostherd-memory--auto-inflight)))

(ert-deftest ghostherd-test-memory-auto-import-retries-in-an-hour ()
  "A missed day is how transcripts are lost, so a failure is not a day."
  (ghostherd-test--with-auto
    (setq fail "connection refused")
    (ghostherd-memory--auto-import-maybe)
    (should-not ghostherd-memory--auto-inflight)
    (should (string-match-p "background import failed" (car said)))
    (should-not (ghostherd-memory--auto-due-p))
    (should-not (ghostherd-memory--auto-due-p (time-add (current-time) 3500)))
    (should (ghostherd-memory--auto-due-p (time-add (current-time) 3700)))
    ;; Said once: a sidecar that stays down is not news every 5 minutes.
    (set-file-times (ghostherd-memory--auto-stamp) (time-subtract (current-time) 90000))
    (ghostherd-memory--auto-import-maybe)
    (should (= (length said) 1))
    ;; A short interval retries within it, not an hour later.
    (let ((ghostherd-memory-auto-import-interval 300))
      (set-file-times (ghostherd-memory--auto-stamp) (time-subtract (current-time) 90000))
      (ghostherd-memory--auto-import-maybe)
      (should-not (ghostherd-memory--auto-due-p (time-add (current-time) 250)))
      (should (ghostherd-memory--auto-due-p (time-add (current-time) 310))))
    ;; A success clears it, so the next failure is reported again.
    (setq fail nil)
    (set-file-times (ghostherd-memory--auto-stamp) (time-subtract (current-time) 90000))
    (ghostherd-memory--auto-import-maybe)
    (should-not ghostherd-memory--auto-failing)))

(ert-deftest ghostherd-test-memory-auto-import-holds-back ()
  "Not while one runs, not without a sidecar, not when switched off."
  (ghostherd-test--with-auto
    (let ((ghostherd-memory--auto-inflight t))
      (ghostherd-memory--auto-import-maybe))
    (let ((ghostherd-memory--port nil))
      (ghostherd-memory--auto-import-maybe))
    (let ((ghostherd-memory-auto-import-interval nil))
      (ghostherd-memory--auto-import-maybe)
      (should-not (ghostherd-memory--auto-due-p)))
    (should-not methods)))

(ert-deftest ghostherd-test-memory-auto-import-timer ()
  (let ((ghostherd-memory--auto-timer nil)
        (ghostherd-memory-auto-import-interval 86400))
    (unwind-protect
        (progn
          (ghostherd-memory-auto-import-start)
          (should (timerp ghostherd-memory--auto-timer))
          (should (eq (timer--function ghostherd-memory--auto-timer)
                      #'ghostherd-memory--auto-import-maybe))
          (ghostherd-memory-auto-import-stop)
          (should-not ghostherd-memory--auto-timer)
          (let ((ghostherd-memory-auto-import-interval nil))
            (ghostherd-memory-auto-import-start)
            (should-not ghostherd-memory--auto-timer)))
      (ghostherd-memory-auto-import-stop))))

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
        (should (equal (mapcar #'car ghostherd-tests--tmux-calls)
                       '("set-buffer" "paste-buffer" "sleep" "send-keys")))
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

(defconst ghostherd-test--rule (make-string 60 ?─))

(defun ghostherd-test--claude-screen (box &optional above)
  "A claude screen whose input box holds BOX, with ABOVE over it.
As claude draws it: its ❯ is followed by a no-break space, which a
test written with an ordinary one once hid."
  (concat (or above "  The tests pass.\n\n✻ Crunched for 3m 42s\n\n")
          ghostherd-test--rule "\n"
          (replace-regexp-in-string "\\`❯ " "❯\u00a0" box) "\n"
          ghostherd-test--rule
          "\n  ⏵⏵ auto mode on (shift+tab to cycle)"))

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
      (should (string-match-p "not submitted" (ghostherd-log-entry-text (car ghostherd--log)))))))

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

;;; A project's lead

(defmacro ghostherd-tests--with-leads (&rest body)
  "BODY with roles, unheard notes and work told of its own, nothing to say
what a project works on, `ghostherd-spawn' making
a test session in state `starting', and timers asked for in `later'."
  (declare (indent 0))
  `(ghostherd-tests--with-herd ((ghostherd--roles (make-hash-table :test 'equal))
                                (ghostherd--lead-unheard (make-hash-table :test 'equal))
                                (ghostherd--lead-work-told (make-hash-table :test 'equal))
                                (ghostherd-project-work-functions nil))
     (let ((spawned nil) (later nil))
       (cl-letf (((symbol-function 'ghostherd-spawn)
                  (lambda (kind &rest pl)
                    (push (cons kind pl) spawned)
                    (let ((s (ghostherd-tests--session :name (plist-get pl :name) :kind kind
                                                       :project (plist-get pl :project) :state 'starting)))
                      (when (plist-get pl :role)
                        (puthash (plist-get pl :name) (plist-get pl :role) ghostherd--roles))
                      s)))
                 ((symbol-function 'run-at-time)
                  (lambda (_time _repeat f &rest args) (push (cons f args) later))))
         ,@body))))

(ert-deftest ghostherd-test-a-project-is-one-spelling ()
  "Spawned here an agent's project is absolute; restored, it is under ~.
The page and the sidecar compare them as strings, so both go as one."
  (should (equal (ghostherd--project-name (expand-file-name "~/tmp/room-trial")) "~/tmp/room-trial/"))
  (should (equal (ghostherd--project-name "~/tmp/room-trial/") "~/tmp/room-trial/"))
  (should-not (ghostherd--project-name ""))
  (should-not (ghostherd--project-name nil)))

(ert-deftest ghostherd-test-a-lead-stays-a-lead ()
  "The role goes onto the host with the recipe, and comes back with a
restore, a respawn's recipe and the herd's snapshot."
  (ghostherd-tests--with-herd ((ghostherd--roles (make-hash-table :test 'equal)))
    (ghostherd-tests--with-tmux '(("has-session" . (1 . "")))
      (ghostherd-backend-spawn 'tmux (list :name "lead-p" :kind 'claude :command "claude"
                                           :directory "/tmp/" :project "/tmp/" :role "lead")))
    (should (equal (alist-get "@ghostherd-role" (ghostherd-tests--tmux-options) nil nil #'equal) "lead"))
    (let ((ghostherd-known-backends '(fake))
          (ghostherd-tests--fake-recipes
           '((:host-id "gh-1" :name "lead-p" :kind claude :project "~/p/" :role "lead")
             (:host-id "gh-2" :name "rd" :kind claude :project "~/p/"))))
      (should (= (ghostherd-restore) 2))
      ;; One spawned here carries the absolute name: the same project.
      (ghostherd-tests--session :name "qa" :kind 'claude :project (expand-file-name "~/p"))
      (let ((lead (ghostherd-get "lead-p")))
        (should (equal (ghostherd-session-role lead) "lead"))
        (should-not (ghostherd-session-role (ghostherd-get "rd")))
        (should (equal (plist-get (ghostherd-session-recipe lead) :role) "lead"))
        (should (eq (ghostherd-project-lead (expand-file-name "~/p")) lead))
        (should (equal (sort (mapcar (lambda (r) (list (plist-get r :name) (plist-get r :role) (plist-get r :project)))
                                     (ghostherd--herd-snapshot))
                             (lambda (a b) (string< (car a) (car b))))
                       '(("lead-p" "lead" "~/p/") ("qa" "" "~/p/") ("rd" "" "~/p/"))))))))

(ert-deftest ghostherd-test-a-lead-starts-with-its-role ()
  "claude takes the role as an appended system prompt, grok as rules;
agy takes none, and gets it with its first message."
  (ghostherd-tests--with-leads
    (let ((ghostherd-lead-kind 'claude))
      (ghostherd-start-lead "/tmp/room-trial/")
      (let ((pl (cdar spawned)))
        (should (eq (caar spawned) 'claude))
        (should (equal (list (plist-get pl :name) (plist-get pl :role) (plist-get pl :display))
                       '("lead-room-trial" "lead" nil)))
        (should (equal (last (plist-get pl :args) 2)
                       (list "--append-system-prompt" ghostherd--lead-role))))
      (should (equal (ghostherd--lead-args 'grok) (list "--rules" ghostherd--lead-role)))
      (should-not (ghostherd--lead-args 'agy))
      ;; The first run's lead reported in English to a user writing Chinese.
      (should (string-match-p "in the\nlanguage they write to you in" ghostherd--lead-role))
      ;; A second lead elsewhere does not take the first one's name.
      (ghostherd-start-lead "/other/room-trial/")
      (should (equal (plist-get (cdar spawned) :name) "lead-room-trial-2")))))

(ert-deftest ghostherd-test-a-respawned-lead-gets-the-role-as-it-is-now ()
  "A lead started before its role changed is respawned with the new one,
fresh or resumed; an agent that is not a lead keeps its args."
  (ghostherd-tests--with-herd ((ghostherd--herd-links (make-hash-table :test 'equal))
                               (ghostherd--roles (make-hash-table :test 'equal)))
    (puthash "lead-p" "lead" ghostherd--roles)
    (let ((lead (ghostherd-tests--session :name "lead-p" :kind 'claude
                                          :args '("--model" "opus" "--append-system-prompt" "old role")))
          (rd (ghostherd-tests--session :name "rd" :kind 'claude
                                        :args '("--append-system-prompt" "be terse")))
          spawned)
      (cl-letf (((symbol-function 'ghostherd-spawn)
                 (lambda (_kind &rest plist) (setq spawned plist) nil)))
        (ghostherd-respawn lead)
        (should (equal (plist-get spawned :args)
                       (list "--model" "opus" "--append-system-prompt" ghostherd--lead-role)))
        (ghostherd-respawn (ghostherd-tests--session :name "lead-p" :kind 'claude
                                                     :args '("--append-system-prompt" "old role"))
                           t)
        (should (equal (plist-get spawned :args)
                       (list "--continue" "--append-system-prompt" ghostherd--lead-role)))
        (ghostherd-respawn rd)
        (should (equal (plist-get spawned :args) '("--append-system-prompt" "be terse")))))
    (should (equal (ghostherd--with-lead-role 'grok '("--rules" "old" "-x"))
                   (list "-x" "--rules" ghostherd--lead-role)))
    (should (equal (ghostherd--with-lead-role 'agy '("-x")) '("-x")))))

(ert-deftest ghostherd-test-the-page-sees-one-project-and-tells-the-lead ()
  "However its agents spell their project, the page and the room see one;
and what the page sends an agent of a project with a lead is kept for
the lead."
  (ghostherd-test--with-herd
    (let ((ghostherd--roles (make-hash-table :test 'equal))
          (ghostherd--lead-unheard (make-hash-table :test 'equal))
          (ghostherd-memory-page--room "~/p/"))
      (ghostherd-tests--session :name "lead-p" :kind 'claude :project "~/p/" :state 'idle)
      (puthash "lead-p" "lead" ghostherd--roles)
      (ghostherd-tests--session :name "qa" :kind 'claude :project (expand-file-name "~/p") :state 'idle)
      (ghostherd-memory-page--send-herd)
      (should (equal (seq-keep (lambda (a) (and (member (plist-get a :name) '("lead-p" "qa"))
                                                 (plist-get a :project)))
                               (plist-get (ghostherd-test--call calls "renderHerd") :agents))
                     '("~/p/" "~/p/")))
      (should (equal (sort (mapcar #'ghostherd-session-name (ghostherd-memory-page--room-sessions)) #'string<)
                     '("lead-p" "qa")))
      (cl-letf (((symbol-function 'ghostherd-prompt) #'ignore))
        (ghostherd-memory-page--handle '((op . "agent-prompt") (name . "qa") (text . "run it"))))
      (should (equal (gethash "~/p/" ghostherd--lead-unheard) '(("qa" . "run it")))))))

(ert-deftest ghostherd-test-the-first-word-to-a-lead-starts-it ()
  "Saying something to a project's lead starts one when it has none; the
words reach it once it is at its prompt -- after its role, on a CLI that
took none.  The next go straight to it, after what the user said to the
team directly meanwhile, and that only once."
  (ghostherd-tests--with-leads
    (let ((ghostherd-lead-kind 'agy) (said nil))
      (ghostherd-tests--session :name "rd" :kind 'claude :project "~/p/" :state 'idle)
      (cl-letf (((symbol-function 'ghostherd-prompt)
                 (lambda (s text &rest _) (push (cons (ghostherd-session-name s) text) said))))
        ;; Before there is a lead, there is no one to tell.
        (ghostherd-tell (ghostherd-get "rd") "before any lead")
        (should (= (hash-table-count ghostherd--lead-unheard) 0))
        (setq said nil)
        (let ((lead (ghostherd-lead-say "~/p/" "plan the login page")))
          (should (equal (ghostherd-session-name lead) "lead-p"))
          (should-not said)
          (setf (ghostherd-session-state lead) 'idle)
          (let ((next (pop later))) (apply (car next) (cdr next)))
          (should (equal (car said) (cons "lead-p" (concat ghostherd--lead-role "\n---\n\nplan the login page"))))
          (ghostherd-tell (ghostherd-get "rd") "use the old API\nfor now")
          (should (equal (car said) '("rd" . "use the old API\nfor now")))
          (should (eq (ghostherd-lead-say (expand-file-name "~/p") "status?") lead))
          (should (equal (cdar said)
                         "[Since you last heard from me, I wrote to the team directly:\n- to rd: use the old API for now]\n\nstatus?"))
          (ghostherd-tell lead "and now?")
          (should (equal (cdar said) "and now?"))
          (should (= (length spawned) 1)))))))

(ert-deftest ghostherd-test-a-lead-is-told-what-its-project-works-on ()
  "A lead's first message opens with what its project works on, and the
rule to change none of it; the next ones only when that changed --
after what the user said to the team, and after a role it took with
its first message.  Its agents are not told, nor another project's lead."
  (ghostherd-tests--with-leads
    (let* ((work (list (list "~/p/" "branch x in ~/p/" "its pull request #3, open: Login -- https://h/3")
                       (list "~/q/" "branch main in ~/q/")))
           (ghostherd-project-work-functions (list (lambda (p) (cdr (assoc p work)))))
           (said nil)
           (lead (ghostherd-tests--session :name "lead-p" :kind 'claude :project "~/p/" :state 'idle))
           (rd (ghostherd-tests--session :name "rd" :kind 'claude :project "~/p/" :state 'idle))
           (other (ghostherd-tests--session :name "lead-q" :kind 'agy :project "~/q/" :state 'idle))
           (rule "Read them for the work; change none of them -- no ticket's status, fields or comments, nothing on the pull request -- unless I ask you to:\n"))
      (puthash "lead-p" "lead" ghostherd--roles)
      (puthash "lead-q" "lead" ghostherd--roles)
      (cl-letf (((symbol-function 'ghostherd-prompt)
                 (lambda (s text &rest _) (push (cons (ghostherd-session-name s) text) said))))
        (ghostherd-tell lead "plan it")
        (should (equal (cdar said)
                       (concat "[What this project works on, as the herd sees it.  " rule
                               "- branch x in ~/p/\n- its pull request #3, open: Login -- https://h/3]\n\nplan it")))
        (ghostherd-tell lead "status?")
        (should (equal (cdar said) "status?"))
        (ghostherd-tell rd "go")
        (should (equal (car said) '("rd" . "go")))
        (setcdr (assoc "~/p/" work) (list "branch x in ~/p/" "its pull request #3, merged: Login -- https://h/3"))
        (ghostherd-tell lead "now?")
        (should (equal (cdar said)
                       (concat "[Since you last heard from me, I wrote to the team directly:\n- to rd: go]\n\n"
                               "[What this project works on has changed since you were told.  Now:\n"
                               "- branch x in ~/p/\n- its pull request #3, merged: Login -- https://h/3]\n\nnow?")))
        ;; Nothing said of it: nothing told, and not taken for a change.
        (setcdr (assoc "~/p/" work) nil)
        (ghostherd-tell lead "and?")
        (should (equal (cdar said) "and?"))
        ;; A role it takes with its first message comes before it all.
        (puthash "~/q/" (list (cons "ops" "hold the deploy")) ghostherd--lead-unheard)
        (ghostherd-tell other "start" "ROLE\n---\n\n")
        (should (equal (car said)
                       (cons "lead-q" (concat "ROLE\n---\n\n"
                                              "[Since you last heard from me, I wrote to the team directly:\n- to ops: hold the deploy]\n\n"
                                              "[What this project works on, as the herd sees it.  "
                                              rule "- branch main in ~/q/]\n\nstart"))))
        ;; And the role says so as well, for every message after.
        (should (string-match-p "change none of them -- no ticket's status, fields or\ncomments, nothing on the pull request -- unless the user asks you to"
                                ghostherd--lead-role))))))

(ert-deftest ghostherd-test-a-new-lead-looks-before-it-is-ready ()
  "What its project works on is asked as the lead starts, so a look has
had the time it took to come to its prompt."
  (ghostherd-tests--with-leads
    (let* ((asked 0)
           (ghostherd-lead-kind 'claude)
           (ghostherd-project-work-functions (list (lambda (_p) (cl-incf asked) (list "branch x in ~/p/"))))
           (said nil))
      (cl-letf (((symbol-function 'ghostherd-prompt)
                 (lambda (_s text &rest _) (push text said))))
        (let ((lead (ghostherd-lead-say "~/p/" "plan it")))
          (should (= asked 1))
          (should-not said)
          (setf (ghostherd-session-state lead) 'idle)
          (let ((next (pop later))) (apply (car next) (cdr next)))
          (should (= asked 2))
          (should (string-suffix-p "- branch x in ~/p/]\n\nplan it" (car said))))))))

(ert-deftest ghostherd-test-the-page-tells-a-lead-what-its-project-works-on ()
  "From the page's last look: the project's branch, its pull request and
the tickets that names; an agent's checkout where it is on another; the
open pull requests and tickets its agents' conversations name, each
once and only so many.  Nothing of another project's."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (ghostherd-tests--with-herd ((ghostherd-memory-page--checkouts (make-hash-table :test 'equal))
                               (ghostherd-memory-page--prs (make-hash-table :test 'equal))
                               (ghostherd-memory-page--task-briefs (make-hash-table :test 'equal))
                               (ghostherd-memory-page--links (make-hash-table :test 'equal))
                               (ghostherd-memory-page--pr-briefs (make-hash-table :test 'equal))
                               (ghostherd-memory-page--remotes (make-hash-table :test 'equal)))
    (let ((ghostherd-memory-page--work-at (float-time))
          (now (float-time))
          (brief (lambda (name status type)
                   (list :at (float-time) :brief `((name . ,name) (status (name . ,status) (type . ,type))
                                                    (url . ,(concat "https://c/t/" (downcase name))))))))
      (ghostherd-tests--session :name "lead-p" :kind 'claude :project "~/p/")
      (ghostherd-tests--session :name "rd" :kind 'claude :project "~/p/")
      (ghostherd-tests--session :name "x" :kind 'claude :project "~/q/")
      (puthash "~/p/" '("~/p/" . "feat/login") ghostherd-memory-page--checkouts)
      (puthash '(agent . "lead-p") '("~/p/" . "feat/login") ghostherd-memory-page--checkouts)
      (puthash '(agent . "rd") '("~/wt/fix/" . "fix/typo") ghostherd-memory-page--checkouts)
      (puthash "~/q/" '("~/q/" . "main") ghostherd-memory-page--checkouts)
      (puthash '(agent . "x") '("~/q/" . "main") ghostherd-memory-page--checkouts)
      (puthash '("~/p/" . "feat/login")
               (list :at now :pr '((id . 12) (title . "Login\n  page") (state . "OPEN") (draft . t)
                                   (url . "https://h/12") (tasks "86a" "86b")))
               ghostherd-memory-page--prs)
      (puthash '("~/wt/fix/" . "fix/typo") (list :at now :pr '((error . "no pull request")))
               ghostherd-memory-page--prs)
      (puthash '("~/q/" . "main") (list :at now :pr '((id . 1) (title . "Q") (state . "OPEN") (url . "https://h/q1")))
               ghostherd-memory-page--prs)
      (puthash "86a" (funcall brief "Login" "in progress" "custom") ghostherd-memory-page--task-briefs)
      (puthash "86c" (funcall brief "Typo" "to do" "open") ghostherd-memory-page--task-briefs)
      (puthash "86d" (funcall brief "Old" "complete" "closed") ghostherd-memory-page--task-briefs)
      ;; rd's conversation: the project's PR again, one open, one merged;
      ;; a ticket the PR names, one open, one done, one not looked up.
      (puthash "~/wt/fix/" (cons now '(:kind github :owner "O" :repo "R")) ghostherd-memory-page--remotes)
      (dolist (pr '((12 "OPEN" "https://h/12") (20 "OPEN" "https://h/20") (21 "MERGED" "https://h/21")
                    (2 "OPEN" "https://w/b/2" "w" "b")))
        (puthash (list "github" (or (nth 3 pr) "o") (or (nth 4 pr) "r") (car pr))
                 (list :at now :pr `((title . ,(format "PR %d" (car pr))) (state . ,(nth 1 pr)) (url . ,(nth 2 pr))))
                 ghostherd-memory-page--pr-briefs))
      (puthash "rd" (list :sig "s" :links
                          (list :prs (append
                                      (mapcar (lambda (id) (list :host "github" :owner "o" :repo "r" :id id :last 1))
                                              '(12 20 21))
                                      ;; Another repository's, which it works on too.
                                      '((:host "github" :owner "w" :repo "b" :id 2 :last 1)))
                                :tasks (mapcar (lambda (id) (list :id id :last 1)) '("86a" "86c" "86d" "86e"))))
               ghostherd-memory-page--links)
      (should (equal (ghostherd-memory-page--lead-work "~/p/")
                     '("branch feat/login in ~/p/"
                       "its pull request #12, open, draft: Login page -- https://h/12"
                       "a ticket its pull request names: ClickUp task 86a, in progress: Login -- https://c/t/login"
                       "a ticket its pull request names: ClickUp task 86b"
                       "rd works on branch fix/typo in ~/wt/fix/"
                       "named in rd's conversation: pull request #20, open: PR 20 -- https://h/20"
                       "named in rd's conversation: pull request w/b#2, open: PR 2 -- https://w/b/2"
                       "named in rd's conversation: ClickUp task 86c, to do: Typo -- https://c/t/typo")))
      (let ((ghostherd-memory-page--lead-named 1))
        (should (equal (last (ghostherd-memory-page--lead-work "~/p/"))
                       '("named in rd's conversation: pull request #20, open: PR 20 -- https://h/20"))))
      (let ((ghostherd-memory-page--lead-named 0))
        (should (equal (last (ghostherd-memory-page--lead-work "~/p/"))
                       '("rd works on branch fix/typo in ~/wt/fix/"))))
      (should (memq #'ghostherd-memory-page--lead-work ghostherd-project-work-functions))
      (should-not (ghostherd-memory-page--lead-work "~/r/"))
      ;; A look a minute old: another is started, and the last one told.
      (let ((looked 0))
        (cl-letf (((symbol-function 'ghostherd-memory-page--work-look) (lambda () (cl-incf looked))))
          (ghostherd-memory-page--lead-work "~/p/")
          (should (= looked 0))
          (let ((ghostherd-memory-page--work-at (- (float-time) 61)))
            (should (equal (car (ghostherd-memory-page--lead-work "~/p/")) "branch feat/login in ~/p/"))
            (should (= looked 1))))))))

(ert-deftest ghostherd-test-waiting-for-a-prompt-gives-up ()
  "A session that never comes to its prompt is logged, its message not
sent; one gone is let be."
  (ghostherd-tests--with-leads
    (ghostherd-tests--with-log
      (let ((s (ghostherd-tests--session :name "a" :state 'starting)) (ran 0))
        (ghostherd--when-ready s (lambda () (cl-incf ran)) 1)
        (let ((next (pop later))) (apply (car next) (cdr next)))
        (should (= ran 0))
        (should-not later)
        (should (string-match-p "never came to its prompt" (ghostherd-log-entry-text (car ghostherd--log))))
        (remhash "a" ghostherd--sessions)
        (setf (ghostherd-session-state s) 'idle)
        (ghostherd--when-ready s (lambda () (cl-incf ran)))
        (should (= ran 0))))))

(ert-deftest ghostherd-test-the-page-starts-a-lead ()
  "The page's word to a project with no lead yet starts one and says so;
with one, it goes to it.  An image for the lead not yet started is kept
for it.  The page knows the lead by its role."
  (ghostherd-test--with-herd
    (let ((dir (file-name-as-directory (make-temp-file "gh-proj" t))) (asked nil) (lead nil))
      (unwind-protect
          (cl-letf (((symbol-function 'ghostherd-lead-say)
                     (lambda (project text) (push (list project text) asked)
                       (setq lead (or lead (ghostherd-tests--session :name "lead-x" :project project))))))
            (ghostherd-memory-page--handle `((op . "lead-say") (project . ,dir) (text . "plan it")))
            (should (equal asked (list (list dir "plan it"))))
            (should (string-match-p "Started lead-x" (ghostherd-test--call calls "flash")))
            (puthash "lead-x" "lead" ghostherd--roles)
            (cl-letf (((symbol-function 'ghostherd-project-lead) (lambda (_p) lead)))
              (ghostherd-memory-page--handle `((op . "lead-say") (project . ,dir) (text . "again"))))
            (should (equal (ghostherd-test--call calls "flash") "Sent to lead-x"))
            (ghostherd-memory-page--handle '((op . "lead-say") (project . "/no/such/dir") (text . "x")))
            (should (string-match-p "No project" (ghostherd-test--call calls "showError")))
            (let ((a (seq-find (lambda (x) (equal (plist-get x :name) "lead-x"))
                               (append (plist-get (progn (ghostherd-memory-page--send-herd)
                                                         (ghostherd-test--call calls "renderHerd"))
                                                  :agents)
                                       nil))))
              (should (equal (plist-get a :role) "lead")))
            (ghostherd-test--with-clipboard ghostherd-test--png-sh
              (cl-letf (((symbol-function 'ghostherd-memory-page--thumb) #'ignore))
                (ghostherd-memory-page--handle '((op . "agent-image") (name . "@lead")))))
            (should (equal (plist-get (ghostherd-test--call calls "attachImage") :name) "@lead")))
        (remhash "lead-x" ghostherd--roles)
        (clrhash ghostherd--lead-unheard)
        (delete-directory dir t)))))

;;; What a project works on

(defmacro ghostherd-test--with-checkouts (&rest body)
  "BODY with `repo', a git repository on feat/x, and `wt', a worktree of
it on feat/y; the work caches its own, and `asked' recording what
pr-view, clickup-view and the sidecar are asked, which answer as `prs',
`briefs', `by-id', `remote' and `said' say.  An agent's transcript is
as `transcripts' says.  `counted' holds the checkouts whose changes
pr-view counted, as `stats' says them."
  (declare (indent 0))
  `(let* ((_ (skip-unless (require 'ghostherd-memory-page nil t)))
          ;; Its true name: git answers with it, and /var is /private/var.
          (base (file-name-as-directory (file-truename (make-temp-file "gh-work" t))))
          (repo (concat base "repo/"))
          (wt (concat base "wt/"))
          (git (lambda (dir &rest args)
                 (let ((default-directory dir))
                   (apply #'call-process "git" nil nil nil
                          "-c" "user.email=t@t" "-c" "user.name=t" args))))
          (asked nil)
          (prs nil)
          (briefs nil)
          (by-id nil)
          (remote nil)
          (said nil)
          (transcripts nil)
          (counted nil)
          (stats nil)
          (ghostherd-memory-page--changes (make-hash-table :test 'equal))
          (ghostherd-memory-page--work-at 0)
          (ghostherd-memory-page--look nil)
          (ghostherd-memory-page--chores nil)
          (ghostherd-memory-page--chore-timer nil)
          (ghostherd-memory-page--checkouts (make-hash-table :test 'equal))
          (ghostherd-memory-page--prs (make-hash-table :test 'equal))
          (ghostherd-memory-page--task-briefs (make-hash-table :test 'equal))
          (ghostherd-memory-page--links (make-hash-table :test 'equal))
          (ghostherd-memory-page--pr-briefs (make-hash-table :test 'equal))
          (ghostherd-memory-page--remotes (make-hash-table :test 'equal)))
     (make-directory repo t)
     (funcall git repo "init" "-q" "-b" "main")
     (funcall git repo "commit" "-q" "--allow-empty" "-m" "start")
     (funcall git repo "checkout" "-q" "-b" "feat/x")
     (funcall git repo "worktree" "add" "-q" "-b" "feat/y" wt)
     (unwind-protect
         (cl-letf (((symbol-function 'pr-view-branch-pr)
                    (lambda (dir cb) (push (list 'pr dir) asked)
                      (funcall cb (cdr (assoc (abbreviate-file-name dir) prs)))))
                   ((symbol-function 'clickup-view-task-brief)
                    (lambda (id cb) (push (list 'task id) asked)
                      (funcall cb (or (cdr (assoc id briefs)) `((id . ,id) (error . "no such task"))))))
                   ((symbol-function 'pr-view-remote) (lambda (_dir) remote))
                   ((symbol-function 'pr-view-local-stat)
                    (lambda (dir) (push dir counted) (cdr (assoc dir stats))))
                   ((symbol-function 'pr-view-repo-pr-brief)
                    (lambda (remote id cb) (push (list 'brief remote id) asked)
                      (funcall cb (or (cdr (assoc id by-id)) '((error . "no such pull request"))))))
                   ((symbol-function 'ghostherd-session-transcript)
                    (lambda (s) (cdr (assoc (ghostherd-session-name (ghostherd-get s)) transcripts))))
                   ((symbol-function 'ghostherd-memory-request-async)
                    (lambda (method cb &optional params _err)
                      (push (list 'links method (plist-get params :path)) asked)
                      (funcall cb (cdr (assoc (plist-get params :path) said))))))
           ,@body)
       (when (timerp ghostherd-memory-page--chore-timer)
         (cancel-timer ghostherd-memory-page--chore-timer))
       (delete-directory base t))))

(defun ghostherd-test--look ()
  "Look at the herd's work, and do every chore it gives, as a pause in
your typing would."
  (ghostherd-memory-page--work-look)
  (while ghostherd-memory-page--chores
    (ghostherd-memory-page--chore-next)))

(ert-deftest ghostherd-test-a-checkout-is-its-root-and-branch ()
  "Its root and branch, a worktree's own; not a repository, or one with
no commit yet, is none."
  (ghostherd-test--with-checkouts
    (should (equal (ghostherd-memory-page--checkout (concat repo "")) (cons (abbreviate-file-name repo) "feat/x")))
    (should (equal (ghostherd-memory-page--checkout wt) (cons (abbreviate-file-name wt) "feat/y")))
    (should-not (ghostherd-memory-page--checkout base))
    (let ((fresh (concat base "fresh/")))
      (make-directory fresh)
      (funcall git fresh "init" "-q")
      (should-not (ghostherd-memory-page--checkout fresh)))
    (should-not (ghostherd-memory-page--checkout "/no/such/dir"))))

(ert-deftest ghostherd-test-the-herd-shows-what-each-project-works-on ()
  "Each project's branch, its PR and the tasks the PR cites; an agent's
only where it is not its project's -- in a worktree.  Asked once while
the answer is fresh; a PR not looked up says so and asks no tasks."
  (ghostherd-test--with-herd
    (ghostherd-test--with-checkouts
      (setq prs `((,(abbreviate-file-name repo)
                   (id . 87) (title . "Sentry fix") (state . "OPEN") (draft . nil) (url . "u87")
                   (tasks . ("86abc1def" "86xyz9876")))
                  (,(abbreviate-file-name wt) (error . "HTTP 500")))
            briefs '(("86abc1def" (id . "86abc1def") (name . "Sentry AI fix")
                      (status . ((name . "in progress") (color . "#4194f6"))) (url . "cu1"))))
      (clrhash ghostherd--sessions)
      (ghostherd-tests--session :name "rd" :kind 'claude :project repo :state 'idle)
      (ghostherd-tests--session :name "qa" :kind 'claude :project repo :state 'idle)
      (cl-letf (((symbol-function 'ghostherd-session-directory)
                 (lambda (s) (if (equal (ghostherd-session-name s) "qa") wt repo)))
                ((symbol-function 'ghostherd--project-name)
                 (lambda (p) (and p (abbreviate-file-name (file-name-as-directory (expand-file-name p)))))))
        (ghostherd-test--look)
        (should (equal (sort (mapcar #'cadr (seq-filter (lambda (a) (eq (car a) 'pr)) asked)) #'string<)
                       (sort (list (abbreviate-file-name repo) (abbreviate-file-name wt)) #'string<)))
        (should (equal (mapcar #'cadr (seq-filter (lambda (a) (eq (car a) 'task)) asked))
                       '("86xyz9876" "86abc1def")))
        (ghostherd-memory-page--send-herd)
        (let* ((p (ghostherd-test--call calls "renderHerd"))
               (w (aref (plist-get p :work) 0))
               (agents (append (plist-get p :agents) nil))
               (of (lambda (n) (plist-get (seq-find (lambda (a) (equal (plist-get a :name) n)) agents) :work))))
          (should (equal (list (plist-get w :project) (plist-get w :branch)) (list (abbreviate-file-name repo) "feat/x")))
          (should (equal (plist-get (plist-get w :pr) :id) 87))
          (should (equal (mapcar (lambda (tk) (list (plist-get tk :id) (plist-get tk :name) (plist-get tk :color) (plist-get tk :error)))
                                 (append (plist-get w :tasks) nil))
                         '(("86abc1def" "Sentry AI fix" "#4194f6" nil) ("86xyz9876" nil nil "no such task"))))
          (should (eq (funcall of "rd") :json-false))
          (let ((qa (funcall of "qa")))
            (should (equal (plist-get qa :branch) "feat/y"))
            (should (equal (plist-get qa :error) "HTTP 500"))
            (should (eq (plist-get qa :pr) :json-false))))
        ;; Fresh: not asked again.  Stale: asked again.
        (setq asked nil)
        (ghostherd-test--look)
        (should-not asked)
        (maphash (lambda (k v) (puthash k (plist-put v :at 0) ghostherd-memory-page--prs)) ghostherd-memory-page--prs)
        (ghostherd-test--look)
        (should (= (length (seq-filter (lambda (a) (eq (car a) 'pr)) asked)) 2))))))

(ert-deftest ghostherd-test-chores-wait-for-a-pause-in-typing ()
  "Done once you stop typing, a moment's worth at a time, each once
however often asked; nothing left, nothing keeps looking."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let ((ghostherd-memory-page--chores nil)
        (ghostherd-memory-page--chore-timer nil)
        (idle nil) (clock 100.0) (done nil)
        (real-float-time (symbol-function 'float-time)))
    (cl-letf (((symbol-function 'current-idle-time)
               (lambda () (and idle (seconds-to-time idle))))
              ((symbol-function 'float-time)
               (lambda (&optional time) (if time (funcall real-float-time time) clock))))
      (unwind-protect
          (let ((chore (lambda (n) (push n done) (cl-incf clock 0.015))))
            (dolist (n '(1 2 1 3)) (ghostherd-memory-page--chore chore n))
            (should (timerp ghostherd-memory-page--chore-timer))
            (should (= (length ghostherd-memory-page--chores) 3))
            ;; Typing, or a command running: nothing.
            (dolist (i (list nil 0.1 (- ghostherd-memory-page--chore-idle 0.01)))
              (setq idle i)
              (ghostherd-memory-page--chores-work))
            (should-not done)
            ;; A pause: as many as fit, the first always.
            (setq idle ghostherd-memory-page--chore-idle)
            (ghostherd-memory-page--chores-work)
            (should (equal done '(2 1)))
            (should (timerp ghostherd-memory-page--chore-timer))
            (ghostherd-memory-page--chores-work)
            (should (equal done '(3 2 1)))
            (should-not ghostherd-memory-page--chore-timer))
        (when (timerp ghostherd-memory-page--chore-timer)
          (cancel-timer ghostherd-memory-page--chore-timer))))))

(ert-deftest ghostherd-test-a-look-is-made-in-pauses ()
  "A look runs nothing at once; the page shows the last one until it is
done; another look meanwhile adds nothing; git is asked once a
directory, the agents and their project being in one."
  (ghostherd-test--with-herd
    (ghostherd-test--with-checkouts
      (clrhash ghostherd--sessions)
      (ghostherd-tests--session :name "rd" :kind 'claude :project repo :state 'idle)
      (ghostherd-tests--session :name "qa" :kind 'claude :project repo :state 'idle)
      (puthash "~/old/" '("~/old/" . "main") ghostherd-memory-page--checkouts)
      (let ((real (symbol-function 'ghostherd-memory-page--checkout))
            (gits nil))
        (cl-letf (((symbol-function 'ghostherd-session-directory) (lambda (_s) repo))
                  ((symbol-function 'ghostherd--project-name)
                   (lambda (p) (and p (abbreviate-file-name (file-name-as-directory (expand-file-name p))))))
                  ((symbol-function 'ghostherd-memory-page--checkout)
                   (lambda (dir) (push dir gits) (funcall real dir))))
          (ghostherd-memory-page--work-look)
          (let ((chores (copy-sequence ghostherd-memory-page--chores)))
            (ghostherd-memory-page--work-look)
            (should (equal ghostherd-memory-page--chores chores)))
          (should-not gits)
          (should-not asked)
          (should (equal (hash-table-keys ghostherd-memory-page--checkouts) '("~/old/")))
          (while ghostherd-memory-page--chores
            (ghostherd-memory-page--chore-next))
          (should (= (length gits) 1))
          (should-not (gethash "~/old/" ghostherd-memory-page--checkouts))
          (should (equal (gethash (abbreviate-file-name repo) ghostherd-memory-page--checkouts)
                         (cons (abbreviate-file-name repo) "feat/x")))
          (should (equal (mapcar #'car asked) '(pr))))))))

(ert-deftest ghostherd-test-an-agent-shows-what-its-conversation-names ()
  "The PRs its conversation names -- its project's own, marked so, and
another repository's, by that repository -- and its tasks, each looked
up; read again only when the transcript changed, looked up again when
stale -- a merged one much later.  Each opens in pr-view: its own by its
checkout, another's by its repository."
  (ghostherd-test--with-herd
    (ghostherd-test--with-checkouts
      (let ((path (concat base "rd.jsonl"))
            (root (abbreviate-file-name repo)))
        (with-temp-file path (insert "{}\n"))
        (setq transcripts `(("rd" . ,path))
              remote '(:kind bitbucket :owner "Team" :repo "Deploy")
              said `((,path :prs ((:host "bitbucket" :owner "team" :repo "deploy" :id 371 :count 2 :last "t3")
                                  (:host "github" :owner "team" :repo "deploy" :id 5 :count 1 :last "t2")
                                  (:host "bitbucket" :owner "other" :repo "deploy" :id 6 :count 1 :last "t2")
                                  (:host "bitbucket" :owner "team" :repo "deploy" :id 360 :count 1 :last "t1")
                                  ;; Not a pull request's names: not asked, not shown.
                                  (:host "gitlab" :owner "team" :repo "deploy" :id 7 :count 1 :last "t0")
                                  (:host "github" :owner "../x" :repo "deploy" :id 8 :count 1 :last "t0")
                                  (:host "github" :owner "team" :repo "deploy" :id "9" :count 1 :last "t0"))
                            :tasks ((:id "86abc1def" :count 1 :last "t3") (:id "86done001" :count 1 :last "t0"))))
              by-id '((371 (id . 371) (title . "Retry alerts") (state . "OPEN") (draft . t) (url . "b371"))
                      (360 (id . 360) (title . "Sentry fix") (state . "MERGED") (draft . nil) (url . "b360")))
              briefs '(("86abc1def" (id . "86abc1def") (name . "Sentry AI fix")
                        (status . ((name . "in progress") (color . "#4194f6") (type . "custom"))) (url . "cu1"))
                       ("86done001" (id . "86done001") (name . "Old one")
                        (status . ((name . "complete") (color . "#008844") (type . "closed"))) (url . "cu2"))))
        (clrhash ghostherd--sessions)
        (ghostherd-tests--session :name "rd" :kind 'claude :project repo :state 'idle)
        (ghostherd-tests--session :name "qa" :kind 'claude :project repo :state 'idle)
        (cl-letf (((symbol-function 'ghostherd-session-directory) (lambda (_s) repo))
                  ((symbol-function 'ghostherd--project-name)
                   (lambda (p) (and p (abbreviate-file-name (file-name-as-directory (expand-file-name p)))))))
          (ghostherd-test--look)
          (should (equal (seq-filter (lambda (a) (eq (car a) 'links)) asked) `((links "herd_links" ,path))))
          (should (equal (sort (mapcar #'cdr (seq-filter (lambda (a) (eq (car a) 'brief)) asked))
                               (lambda (x y) (< (cadr x) (cadr y))))
                         '(((:kind github :owner "team" :repo "deploy") 5)
                           ((:kind bitbucket :owner "other" :repo "deploy") 6)
                           ((:kind bitbucket :owner "team" :repo "deploy") 360)
                           ((:kind bitbucket :owner "team" :repo "deploy") 371))))
          (should (equal (sort (mapcar #'cadr (seq-filter (lambda (a) (eq (car a) 'task)) asked)) #'string<)
                         '("86abc1def" "86done001")))
          (ghostherd-memory-page--send-herd)
          (let* ((agents (append (plist-get (ghostherd-test--call calls "renderHerd") :agents) nil))
                 (of (lambda (n) (plist-get (seq-find (lambda (a) (equal (plist-get a :name) n)) agents) :links)))
                 (rd (funcall of "rd")))
            (should (eq (funcall of "qa") :json-false))
            (should (equal (plist-get rd :root) root))
            (should (equal (mapcar (lambda (pr) (list (plist-get pr :id) (plist-get pr :own) (plist-get pr :state)
                                                      (plist-get pr :draft) (plist-get pr :title) (plist-get pr :url)
                                                      (plist-get pr :last) (plist-get pr :error)))
                                   (append (plist-get rd :prs) nil))
                           '((371 t "OPEN" t "Retry alerts" "b371" "t3" nil)
                             (5 :json-false nil :json-false nil nil "t2" "no such pull request")
                             (6 :json-false nil :json-false nil nil "t2" "no such pull request")
                             (360 t "MERGED" :json-false "Sentry fix" "b360" "t1" nil))))
            (should (equal (mapcar (lambda (pr) (list (plist-get pr :host) (plist-get pr :owner) (plist-get pr :repo)))
                                   (append (plist-get rd :prs) nil))
                           '(("bitbucket" "team" "deploy") ("github" "team" "deploy")
                             ("bitbucket" "other" "deploy") ("bitbucket" "team" "deploy"))))
            (should (equal (mapcar (lambda (tk) (list (plist-get tk :id) (plist-get tk :done) (plist-get tk :last)))
                                   (append (plist-get rd :tasks) nil))
                           '(("86abc1def" :json-false "t3") ("86done001" t "t0")))))
          ;; The same transcript: not read again; its answers fresh: not asked.
          (setq asked nil)
          (ghostherd-test--look)
          (should-not (seq-filter (lambda (a) (memq (car a) '(links brief task))) asked))
          ;; Stale: an open PR is asked again, a merged one not yet.
          (maphash (lambda (k v) (puthash k (plist-put v :at (- (float-time) 600)) ghostherd-memory-page--pr-briefs))
                   ghostherd-memory-page--pr-briefs)
          (ghostherd-test--look)
          (should (equal (sort (mapcar #'caddr (seq-filter (lambda (a) (eq (car a) 'brief)) asked)) #'<) '(5 6 371)))
          ;; It grew: read again.
          (setq asked nil)
          (with-temp-buffer (insert "{}\n") (append-to-file (point-min) (point-max) path))
          (ghostherd-test--look)
          (should (equal (seq-filter (lambda (a) (eq (car a) 'links)) asked) `((links "herd_links" ,path))))
          ;; A PR it names opens in pr-view: its own by its checkout,
          ;; another's by its repository -- one it named, and no other.
          (let (opened)
            (cl-letf (((symbol-function 'pr-view-open-pr) (lambda (dir id) (push (list dir id) opened)))
                      ((symbol-function 'pr-view-open-remote-pr) (lambda (r id) (push (list r id) opened))))
              (clrhash ghostherd-memory-page--prs)
              (ghostherd-memory-page--handle `((op . "open-pr") (root . ,root) (id . 360)))
              (ghostherd-memory-page--handle '((op . "open-pr") (repo . "github/team/deploy") (id . 5)))
              (dolist (bad '(((op . "open-pr") (repo . "github/team/deploy") (id . 6))
                             ((op . "open-pr") (repo . "github/evil/x") (id . 5))
                             ((op . "open-pr") (repo . "github/team/deploy/x") (id . 5))
                             ((op . "open-pr") (root . "/etc/") (id . 360))))
                (ghostherd-memory-page--handle bad))
              (should (equal (reverse opened)
                             `((,root 360) ((:kind github :owner "team" :repo "deploy") 5)))))))))))

(ert-deftest ghostherd-test-the-page-counts-what-a-checkout-has-not-committed ()
  "Each checkout the herd is in is counted once a look, an agent's
worktree too; the work line says it -- nothing when it is clean -- and
the page is told only when it changed.  An agent that ends a turn has
its checkouts counted again, while the page is open."
  (ghostherd-test--with-herd
    (ghostherd-test--with-checkouts
      (let ((root (abbreviate-file-name repo))
            (tree (abbreviate-file-name wt)))
        (clrhash ghostherd--sessions)
        (ghostherd-tests--session :name "rd" :kind 'claude :project repo :state 'working)
        (ghostherd-tests--session :name "qa" :kind 'claude :project repo :state 'idle)
        (setq stats `((,root :root ,root :files 3 :added 12 :deleted 4 :untracked 1 :untracked-added 5)
                      (,tree :root ,tree :files 0 :added 0 :deleted 0 :untracked 0 :untracked-added 0)))
        (cl-letf (((symbol-function 'ghostherd-session-directory)
                   (lambda (s) (if (equal (ghostherd-session-name s) "qa") wt repo)))
                  ((symbol-function 'ghostherd--project-name)
                   (lambda (p) (and p (abbreviate-file-name (file-name-as-directory (expand-file-name p)))))))
          (ghostherd-test--look)
          (should (equal (sort (copy-sequence counted) #'string<) (sort (list root tree) #'string<)))
          (ghostherd-memory-page--send-herd)
          (let* ((herd (ghostherd-test--call calls "renderHerd"))
                 (work (seq-find (lambda (w) (equal (plist-get w :project) root))
                                 (append (plist-get herd :work) nil)))
                 (qa (seq-find (lambda (a) (equal (plist-get a :name) "qa"))
                               (append (plist-get herd :agents) nil))))
            (should (equal (plist-get work :changes)
                           '(:files 3 :added 12 :deleted 4 :untracked 1 :untrackedAdded 5)))
            ;; Clean: nothing to say.
            (should (equal (plist-get (plist-get qa :work) :root) tree))
            (should (eq (plist-get (plist-get qa :work) :changes) :json-false)))
          ;; Counted the same: the page is not told again; changed, it is.
          (let ((told 0))
            (cl-letf (((symbol-function 'ghostherd-memory-page--work-changed) (lambda () (cl-incf told))))
              (ghostherd-memory-page--changes-count root)
              (should (= told 0))
              (setf (alist-get root stats nil nil #'equal) (list :root root :files 4 :added 13 :deleted 4))
              (ghostherd-memory-page--changes-count root)
              (should (= told 1))))
          ;; rd ends a turn: its checkout is counted again -- with the page open.
          (setq counted nil ghostherd-memory-page--chores nil)
          (cl-letf (((symbol-function 'xwapp-live-p) (lambda (_app) nil)))
            (ghostherd--set-state (ghostherd-get "rd") 'idle))
          (should-not ghostherd-memory-page--chores)
          (setf (ghostherd-session-state (ghostherd-get "rd")) 'working)
          (cl-letf (((symbol-function 'xwapp-live-p) (lambda (_app) t)))
            (ghostherd--set-state (ghostherd-get "rd") 'idle)
            (ghostherd--set-state (ghostherd-get "qa") 'done))
          (while ghostherd-memory-page--chores (ghostherd-memory-page--chore-next))
          (should (equal counted (list root)))
          ;; So does one that ends it to wait on its sub-agents.
          (setq counted nil)
          (setf (ghostherd-session-state (ghostherd-get "rd")) 'working)
          (cl-letf (((symbol-function 'xwapp-live-p) (lambda (_app) t)))
            (ghostherd--set-state (ghostherd-get "rd") 'waiting))
          (while ghostherd-memory-page--chores (ghostherd-memory-page--chore-next))
          (should (equal counted (list root))))))))

(ert-deftest ghostherd-test-the-page-opens-a-checkouts-changes ()
  "What a checkout the herd is in has not committed opens in pr-view, and
pr-view's page may show it; another directory is neither."
  (ghostherd-test--with-herd
    (let ((ghostherd-memory-page--checkouts (make-hash-table :test 'equal)) opened)
      (puthash "~/p/" '("~/p/" . "feat/x") ghostherd-memory-page--checkouts)
      (cl-letf (((symbol-function 'pr-view-changes) (lambda (dir) (push dir opened)))
                ((symbol-function 'ghostherd-memory-page--checkout) (lambda (_dir) nil)))
        (ghostherd-memory-page--handle '((op . "open-changes") (root . "~/p/")))
        (should (equal opened '("~/p/")))
        (should (memq #'ghostherd-memory-page--herd-checkout-p pr-view-local-allowed-functions))
        (should (ghostherd-memory-page--herd-checkout-p "~/p/"))
        (setq calls nil)
        (dolist (bad '("/etc/" "~/p" 7 nil))
          (should-not (ghostherd-memory-page--herd-checkout-p bad))
          (ghostherd-memory-page--handle `((op . "open-changes") (root . ,bad))))
        (should (equal opened '("~/p/")))
        (should (= (length (seq-filter (lambda (c) (equal (car c) "showError")) calls)) 4)))
      ;; Before a look -- Emacs just restarted -- an agent's own checkout.
      (clrhash ghostherd-memory-page--checkouts)
      (cl-letf (((symbol-function 'ghostherd-memory-page--checkout)
                 (lambda (dir) (and (equal dir "/p/") '("~/q/" . "main")))))
        (should (ghostherd-memory-page--herd-checkout-p "~/q/"))
        (should-not (ghostherd-memory-page--herd-checkout-p "~/r/"))))))

(ert-deftest ghostherd-test-a-review-goes-to-an-agent-of-its-checkout ()
  "A review of what a checkout has not committed may go to its project's
lead first, then each agent working in it -- not one in another
worktree, nor a shell, nor a dead agent -- and to a lead started for it
when the project has none, last.  Sent, it is said to the one picked.
A checkout the herd is not in has nobody."
  (ghostherd-test--with-herd
    (ghostherd-test--with-checkouts
      (let* ((root (abbreviate-file-name repo))
             (tree (abbreviate-file-name wt))
             (ghostherd--roles (make-hash-table :test 'equal))
             (dirs `(("rd" . ,repo) ("qa" . ,wt) ("sh" . ,repo) ("old" . ,repo) ("lead-repo" . ,repo)))
             (to (lambda (dir) (mapcar (lambda (r) (list (plist-get r :id) (plist-get r :label)))
                                       (ghostherd-memory-page--review-recipients dir))))
             (send (lambda (dir id text)
                     (funcall (plist-get (seq-find (lambda (r) (equal (plist-get r :id) id))
                                                   (ghostherd-memory-page--review-recipients dir))
                                         :send)
                              text)))
             told started)
        (clrhash ghostherd--sessions)
        (ghostherd-tests--session :name "rd" :kind 'claude :project repo :state 'working)
        (ghostherd-tests--session :name "qa" :kind 'grok :project repo :state 'idle)
        (ghostherd-tests--session :name "sh" :kind 'shell :project repo :state 'idle)
        (ghostherd-tests--session :name "old" :kind 'agy :project repo :state 'dead)
        (cl-letf (((symbol-function 'ghostherd-session-directory)
                   (lambda (s) (cdr (assoc (ghostherd-session-name s) dirs))))
                  ((symbol-function 'ghostherd-tell)
                   (lambda (s text &rest _) (push (list (ghostherd-session-name s) text) told)))
                  ((symbol-function 'ghostherd-lead-say)
                   (lambda (project text)
                     (push (list project text) started)
                     (let ((lead (ghostherd-tests--session :name "lead-repo" :kind 'claude :project repo
                                                           :state 'idle)))
                       (puthash "lead-repo" "lead" ghostherd--roles)
                       lead))))
          (should (memq #'ghostherd-memory-page--review-recipients pr-view-local-recipients-functions))
          ;; No lead yet: whoever works there, then a lead to be started.
          (should (equal (funcall to root) `(("rd" "rd · claude") (,(concat "@lead " root) "new lead · claude"))))
          (should (equal (funcall to tree) `(("qa" "qa · grok") (,(concat "@lead " root) "new lead · claude"))))
          (should (equal (funcall send root "rd" "Comments…") "Sent to rd"))
          (should (equal told '(("rd" "Comments…"))))
          (should (equal (funcall send tree (concat "@lead " root) "More…")
                         "Started lead-repo; it gets the comments once it is ready"))
          (should (equal started `((,root "More…"))))
          ;; With a lead: the lead first, in its worktrees too.
          (should (equal (funcall to root) '(("lead-repo" "lead-repo · claude") ("rd" "rd · claude"))))
          (should (equal (funcall to tree) '(("lead-repo" "lead-repo · claude") ("qa" "qa · grok"))))
          (funcall send tree "lead-repo" "Again…")
          (should (equal (car told) '("lead-repo" "Again…")))
          ;; Nobody for a checkout the herd is not in.
          (should-not (ghostherd-memory-page--review-recipients "/etc/"))
          (should-not (ghostherd-memory-page--review-recipients (abbreviate-file-name base))))))))

(ert-deftest ghostherd-test-a-review-from-pr-views-page-reaches-the-agent ()
  "Comments written on a herd checkout's diff in pr-view's page reach the
agent picked as one prompt, each at its file and line."
  (skip-unless (require 'pr-view nil t))
  (ghostherd-test--with-herd
    (ghostherd-test--with-checkouts
      (let ((root (abbreviate-file-name repo)) told js)
        (clrhash ghostherd--sessions)
        (ghostherd-tests--session :name "rd" :kind 'claude :project repo :state 'idle)
        (cl-letf (((symbol-function 'ghostherd-session-directory) (lambda (_s) repo))
                  ((symbol-function 'ghostherd-tell)
                   (lambda (s text &rest _) (push (list (ghostherd-session-name s) text) told)))
                  ((symbol-function 'pr-view--js) (lambda (fn arg) (push (cons fn arg) js))))
          (pr-view--handle-intent
           `((op . "local-review") (root . ,root) (to . "rd")
             (comments . (((path . "src/pay.py") (side . "new") (line . 3) (code . "return charge(total)")
                           (text . "Retry it."))))))
          (should (equal (caar js) "reviewSent"))
          (should (equal (alist-get 'said (cdar js)) "Sent to rd"))
          (should (equal (mapcar #'car told) '("rd")))
          (should (equal (cadr (car told))
                         (concat "Comments on what is not committed in " root " (branch feat/x), "
                                 "each on a line of its diff against HEAD:\n"
                                 "\n1. src/pay.py:3\n   > return charge(total)\n   Retry it.\n"))))))))

(ert-deftest ghostherd-test-sidebar-shows-a-checkouts-changes ()
  "d on a row: the status of the checkout its agent works in -- a
worktree, say -- in magit, or vc-dir without it."
  (ghostherd-tests--with-herd ()
    (let* ((dir (file-name-as-directory (make-temp-file "gh-changes" t)))
           (wt (concat dir "wt/"))
           (s (ghostherd-tests--session :name "rd" :kind 'claude :project dir))
           shown)
      (make-directory wt)
      (unwind-protect
          (cl-letf (((symbol-function 'ghostherd--sidebar-session-at-point) (lambda () s))
                    ((symbol-function 'ghostherd--sidebar-leave-overlay) #'ignore)
                    ((symbol-function 'ghostherd-session-directory) (lambda (_s) wt))
                    ((symbol-function 'magit-status-setup-buffer) (lambda (d) (push (list 'magit d) shown)))
                    ((symbol-function 'vc-dir) (lambda (d) (push (list 'vc d) shown))))
            (should (eq (keymap-lookup ghostherd-sidebar-mode-map "d") #'ghostherd-sidebar-changes))
            (cl-letf (((symbol-function 'require) (lambda (f &rest _) (eq f 'magit))))
              (ghostherd-sidebar-changes))
            (cl-letf (((symbol-function 'require) (lambda (&rest _) nil)))
              (ghostherd-sidebar-changes))
            (should (equal (reverse shown) `((magit ,wt) (vc ,wt))))
            (delete-directory wt)
            (should-error (ghostherd-sidebar-changes) :type 'user-error))
        (delete-directory dir t)))))

(ert-deftest ghostherd-test-the-page-opens-a-pr-or-a-task ()
  "A PR of a checkout the herd is in opens in pr-view, a task in
clickup-view; anything else the page names is refused."
  (ghostherd-test--with-herd
    (let ((ghostherd-memory-page--prs (make-hash-table :test 'equal)) opened)
      (puthash '("~/p/" . "feat/x") '(:at 0 :pr nil) ghostherd-memory-page--prs)
      (cl-letf (((symbol-function 'pr-view-open-pr) (lambda (dir id) (push (list 'pr dir id) opened)))
                ((symbol-function 'clickup-view-task) (lambda (id) (push (list 'task id) opened))))
        (ghostherd-memory-page--handle '((op . "open-pr") (root . "~/p/") (id . 87)))
        (ghostherd-memory-page--handle '((op . "open-task") (id . "86abc1def")))
        (should (equal (reverse opened) '((pr "~/p/" 87) (task "86abc1def"))))
        (setq calls nil)
        (dolist (bad '(((op . "open-pr") (root . "/etc/") (id . 1))
                       ((op . "open-pr") (root . "~/p/") (id . "87; rm"))
                       ((op . "open-task") (id . "../x"))))
          (ghostherd-memory-page--handle bad))
        (should (= (length opened) 2))
        (should (= (length (seq-filter (lambda (c) (equal (car c) "showError")) calls)) 3))))))

;;; Images pasted into a prompt

(defconst ghostherd-test--png "\x89PNG\r\n\x1a\n\0\303\251\377 \n rest"
  "A PNG's signature, then bytes a text coding would change: UTF-8 to
decode, a lone newline to turn into CRLF.")
(defconst ghostherd-test--png-sh "printf '\\211PNG\\r\\n\\032\\n\\000\\303\\251\\377 \\n rest'"
  "A clipboard command that prints `ghostherd-test--png'.")

(defmacro ghostherd-test--with-clipboard (script &rest body)
  "BODY with the clipboard's image printed by SCRIPT, images saved under a temp dir."
  (declare (indent 1))
  `(let* ((root (make-temp-file "gh-images" t))
          (ghostherd-image-directory (expand-file-name "images/" root))
          (ghostherd-image-keep-days 30)
          (ghostherd-clipboard-image-commands (list (list "sh" "-c" ,script))))
     (unwind-protect (progn ,@body)
       (delete-directory root t))))

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

(ert-deftest ghostherd-test-page-pastes-an-image ()
  "The page asks for the clipboard's image for an agent; Emacs saves it
and attaches it, file and preview -- or says why not."
  (ghostherd-test--with-herd
    (ghostherd-test--with-clipboard ghostherd-test--png-sh
      (cl-letf (((symbol-function 'ghostherd-memory-page--thumb) (lambda (f) (concat "thumb of " f))))
        (ghostherd-memory-page--handle '((op . "agent-image") (name . "claude-api"))))
      (let ((p (ghostherd-test--call calls "attachImage")))
        (should (equal (plist-get p :name) "claude-api"))
        (should (file-exists-p (plist-get p :path)))
        (should (equal (plist-get p :thumb) (concat "thumb of " (plist-get p :path))))
        (should (equal (file-name-directory (plist-get p :path))
                       (expand-file-name ghostherd-image-directory)))))
    (setq calls nil)
    (ghostherd-test--with-clipboard "printf text"
      (ghostherd-memory-page--handle '((op . "agent-image") (name . "claude-api"))))
    (should-not (ghostherd-test--call calls "attachImage"))
    (should (equal (ghostherd-test--call calls "showError") "No image on the clipboard"))
    (ghostherd-memory-page--handle '((op . "agent-image") (name . "nobody")))
    (should (string-match-p "No agent named nobody" (ghostherd-test--call calls "showError")))))

(defconst ghostherd-test--jpeg "\xff\xd8\xff\xe0\0\x10JFIF\0\303\251\377 \n rest"
  "A JPEG's signature, then bytes a text coding would change.")

(ert-deftest ghostherd-test-page-uploads-an-image ()
  "From the page on another device the image comes in the intent, base64:
saved byte for byte as a .jpg or .png named for the agent, readable only
by you, and attached -- or refused, saying why, nothing saved."
  (ghostherd-test--with-herd
    (ghostherd-test--with-clipboard "exit 1"
      (cl-letf (((symbol-function 'ghostherd-memory-page--thumb) (lambda (f) (concat "thumb of " f))))
        (pcase-dolist (`(,bytes . ,ext) `((,ghostherd-test--jpeg . "jpg") (,ghostherd-test--png . "png")))
          (setq calls nil)
          (ghostherd-memory-page--handle `((op . "agent-image-upload") (name . "claude-api")
                                           (data . ,(base64-encode-string bytes t))))
          (let* ((p (ghostherd-test--call calls "attachImage"))
                 (file (plist-get p :path)))
            (should (equal (plist-get p :name) "claude-api"))
            (should (string-match-p (format "\\`claude-api-[0-9-]+\\.%s\\'" ext) (file-name-nondirectory file)))
            (should (equal (file-name-directory file) (expand-file-name ghostherd-image-directory)))
            (should (equal (with-temp-buffer
                             (set-buffer-multibyte nil)
                             (insert-file-contents-literally file)
                             (buffer-string))
                           bytes))
            (should (= (file-modes file) #o600))
            (should (equal (plist-get p :thumb) (concat "thumb of " file)))))
        ;; For a lead not started yet, as the clipboard's.
        (setq calls nil)
        (ghostherd-memory-page--handle `((op . "agent-image-upload") (name . "@lead")
                                         (data . ,(base64-encode-string ghostherd-test--jpeg t))))
        (should (equal (plist-get (ghostherd-test--call calls "attachImage") :name) "@lead"))
        (let ((saved (directory-files ghostherd-image-directory)))
          (pcase-dolist (`(,intent . ,why)
                         `((((name . "claude-api") (data . ,(base64-encode-string "just text" t))) . "Not a PNG or JPEG image")
                           (((name . "claude-api")) . "No image came")
                           (((name . "claude-api") (data . "%%% not base64")) . "No image came")
                           (((name . "nobody") (data . ,(base64-encode-string ghostherd-test--jpeg t))) . "No agent named nobody")))
            (setq calls nil)
            (ghostherd-memory-page--handle (cons '(op . "agent-image-upload") intent))
            (should-not (ghostherd-test--call calls "attachImage"))
            (should (string-match-p why (ghostherd-test--call calls "showError"))))
          (should (equal (directory-files ghostherd-image-directory) saved)))))))

(defconst ghostherd-test--pixel
  (base64-decode-string
   "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII=")
  "A real PNG: one pixel.")

(ert-deftest ghostherd-test-an-attached-image-has-a-preview ()
  "A preview is a PNG data URL: scaled by sips, or without it the file
itself when small.  Nothing that is not a PNG becomes one."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let* ((dir (make-temp-file "gh-thumb" t))
         (png (expand-file-name "a.png" dir))
         (junk (expand-file-name "b.png" dir))
         (big (expand-file-name "c.png" dir))
         (decode (lambda (url) (and url (string-prefix-p "data:image/png;base64," url)
                                    (base64-decode-string (substring url 22))))))
    (unwind-protect
        (let ((coding-system-for-write 'binary))
          (write-region ghostherd-test--pixel nil png nil 'silent)
          (write-region "not an image" nil junk nil 'silent)
          (write-region (concat ghostherd-test--pixel (make-string (* 300 1024) ?x)) nil big nil 'silent)
          (when (executable-find "sips")
            (should (string-prefix-p "\x89PNG" (funcall decode (ghostherd-memory-page--thumb png))))
            (should-not (ghostherd-memory-page--thumb junk)))
          (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
            (should (equal (funcall decode (ghostherd-memory-page--thumb png)) ghostherd-test--pixel))
            (should-not (ghostherd-memory-page--thumb junk))
            (should-not (ghostherd-memory-page--thumb big))))
      (delete-directory dir t))))

(ert-deftest ghostherd-test-cmd-v-pastes-into-the-page ()
  "Cmd-V never becomes a paste in the web view: the page asks, or Emacs
gets the key, and Emacs hands the page the clipboard's text, or nothing
-- the page then asks for an image."
  (ghostherd-test--with-page
    (dolist (clip '("some text" nil))
      (setq calls nil)
      (cl-letf (((symbol-function 'gui-get-selection) (lambda (&rest _) clip)))
        (ghostherd-memory-page--handle '((op . "paste"))))
      (should (equal (ghostherd-test--call calls "paste") (list :text (or clip ""))))))
  (should (eq (lookup-key ghostherd-memory-page-keys-mode-map (kbd "s-v")) #'ghostherd-memory-page-paste))
  (let ((ghostherd-memory-page--project nil)
        (ghostherd-memory-page--pending nil))
    (cl-letf (((symbol-function 'ghostherd-memory-ensure) #'ignore)
              ((symbol-function 'xwapp-open) (lambda (app) (get-buffer-create (xwapp-buffer-name app)))))
      (unwind-protect
          (progn
            (ghostherd-memory-page--open "agents")
            (with-current-buffer "*herd*"
              (should ghostherd-memory-page-keys-mode)
              (should (eq (key-binding (kbd "s-v")) #'ghostherd-memory-page-paste))))
        (kill-buffer "*herd*")))))

(ert-deftest ghostherd-test-a-browser-tab-reopens-the-page ()
  "A tab that comes back to the herd page -- after a restart, say -- finds
it made ready as the command would, but for what to show: the tab knows
that.  A sidecar that does not start is only said."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (should (eq (xwapp-reopen ghostherd-memory-page--app) 'ghostherd-memory-page-reopen))
  (let ((ghostherd-memory-page--project nil)
        (ghostherd-memory-page--pending '(:view "search")))
    (cl-letf (((symbol-function 'ghostherd-memory-ensure) (lambda () (error "No uv")))
              ((symbol-function 'ghostherd-memory-page--current-project) (lambda () "/p")))
      (ghostherd-memory-page-reopen))
    (should-not ghostherd-memory-page--pending)
    (should (equal ghostherd-memory-page--project "/p"))))

;;; Pushes to your phone

(defmacro ghostherd-test--pushing (&rest body)
  "BODY with pushes on: the bot set up, you away from the desk and from
the page; rooms linked as \"room:PROJECT\"; `idle' the seconds since you
touched this machine."
  (declare (indent 0))
  `(ghostherd-tests--with-herd
       ((ghostherd--herd-pushes nil)
        (ghostherd--working-since (make-hash-table :test 'equal))
        (ghostherd--roles (make-hash-table :test 'equal))
        (ghostherd-push '(blocked report ask-failed))
        (ghostherd-push-away-after 120)
        (ghostherd-present-functions nil)
        (ghostherd-watched-functions nil)
        (ghostherd-room-url-function (lambda (p) (concat "room:" p)))
        (process-environment (append '("GHOSTHERD_TELEGRAM_TOKEN=t" "GHOSTHERD_TELEGRAM_CHAT_IDS=1")
                                     process-environment))
        (idle 600))
     (cl-letf (((symbol-function 'ghostherd--idle-seconds) (lambda () idle))
               ((symbol-function 'ghostherd--maybe-notify-state) #'ignore))
       ,@body)))

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
                              :reason "Allow npm test?"))))
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

;;; A menu prompt, read off the screen

(defconst ghostherd-test--agy-menu
  "  Hi! How can I help you today?

────────────────────────────────────────────────────────────
> 解釋一下Respawn 的 fresh 跟 continue 差在哪裡

──────────────────────────────────────────────────────────────────────────────────
Requesting permission for:
   grep -rn -C 3 \"respawn\" local/ghostherd/
Run this command?
> 1. Yes, run command
  2. Yes, and always allow in this conversation for commands that start with
'grep'
  3. Yes, and always allow for commands that start with 'grep' (Persist to
settings.json)
  4. No, cancel
  ↑/↓ Navigate · tab Amend · ctrl+g edit/expand command
esc to cancel                         Gemini 3.8 Flash · high · AI: Out of credits
"
  "agy on a command it asks to run.")

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

(defconst ghostherd-test--claude-permission
  "● I'll run the tests.

╭──────────────────────────────────────────────────────────────╮
│ Bash command                                                 │
│                                                              │
│   npm test -- --runInBand                                    │
│   Run the test suite                                         │
│                                                              │
│ Do you want to proceed?                                      │
│ ❯ 1. Yes                                                     │
│   2. Yes, and don't ask again for npm test commands in /x    │
│   3. No, and tell Claude what to do differently (esc)        │
╰──────────────────────────────────────────────────────────────╯
"
  "claude's permission prompt, in its box.")

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

;;; Answering a menu

(defun ghostherd-test--agy-menu-on (on)
  "agy's menu with its highlight on choice ON, or on none."
  (replace-regexp-in-string
   "^[> ] \\([1-4]\\)\\. "
   (lambda (m) (format "%s %s. " (if (eql (string-to-number (match-string 1 m)) on) ">" " ")
                       (match-string 1 m)))
   ghostherd-test--agy-menu t t))

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
or idle as the rest says.  The menu itself, the box gone, is blocked --
and a kind not known to do so keeps reading blocked as before."
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
    (should-not replaces)
    (should (eq (car (ghostherd--match-rules ghostherd-test--claude-wrote-permission grok replaces)) 'blocked))))

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

;;; Relay: an heir takes an agent's name and its work

(defmacro ghostherd-tests--relaying (&rest body)
  "BODY in a clean herd that can relay.  Sidecar requests land in
`requests' as (METHOD PARAMS); the brief is \"BRIEF\".  `spawned' holds
the heir's (KIND . PLIST); the heir is registered idle on the fake
backend.  `told' holds (FN SESSION TEXT ROLE) for what it was told.
`ready' is the heir's wait for its prompt, (FN FAIL), when `hold' is
set; otherwise it is told at once."
  (declare (indent 0))
  `(ghostherd-tests--with-herd
       ((ghostherd--asks-open (make-hash-table :test 'equal))
        (ghostherd--asks-settled (make-hash-table :test 'equal))
        (ghostherd--relay-held (make-hash-table :test 'equal))
        (ghostherd--relaying (make-hash-table :test 'equal))
        (ghostherd--relay-offered (make-hash-table :test 'equal))
        (ghostherd--lineage (make-hash-table :test 'equal))
        (ghostherd--roles (make-hash-table :test 'equal))
        (ghostherd--herd-links (make-hash-table :test 'equal))
        (ghostherd--lead-work-told (make-hash-table :test 'equal))
        (ghostherd--lead-unheard (make-hash-table :test 'equal))
        (ghostherd--herd-asks nil)
        (ghostherd--herd-pushes nil)
        (ghostherd-usage--cache nil)
        (ghostherd-relay-kinds '(claude grok agy))
        (ghostherd--log nil)
        (requests nil) (spawned nil) (told nil) (ready nil) (hold nil) (brief-error nil))
     (cl-letf (((symbol-function 'ghostherd-herd-client) (lambda () "/p/bin/herd"))
               ((symbol-function 'ghostherd-memory-rpc-url) (lambda () "http://127.0.0.1:1/jsonrpc"))
               ((symbol-function 'ghostherd-memory-request-async)
                (lambda (method cb &optional params err)
                  (setq requests (append requests (list (list method params))))
                  (when (equal method "herd_relay_brief")
                    (if brief-error (funcall err brief-error) (funcall cb '(:text "BRIEF"))))))
               ((symbol-function 'ghostherd--host-kill) #'ignore)
               ((symbol-function 'ghostherd-project-defaults) (lambda (&optional _d) (cons nil nil)))
               ((symbol-function 'ghostherd-spawn)
                (lambda (kind &rest plist)
                  (setq spawned (cons kind plist))
                  (let ((heir (ghostherd-tests--session :name (plist-get plist :name) :kind kind
                                                        :state 'idle :backend 'fake
                                                        :project (plist-get plist :project)
                                                        :notes (plist-get plist :notes)
                                                        :args (plist-get plist :args))))
                    (if (plist-get plist :role)
                        (puthash (plist-get plist :name) (plist-get plist :role) ghostherd--roles)
                      (remhash (plist-get plist :name) ghostherd--roles))
                    (if (plist-get plist :lineage)
                        (puthash (plist-get plist :name) (plist-get plist :lineage) ghostherd--lineage)
                      (remhash (plist-get plist :name) ghostherd--lineage))
                    heir)))
               ((symbol-function 'ghostherd--when-ready)
                (lambda (_s fn &optional _tries fail)
                  (if hold (setq ready (list fn fail)) (funcall fn))))
               ((symbol-function 'ghostherd-prompt)
                (lambda (s text &rest _) (push (list 'prompt s text nil) told)))
               ((symbol-function 'ghostherd-tell)
                (lambda (s text &optional role) (push (list 'tell s text role) told))))
       ,@body)))

(defun ghostherd-tests--used-up (kind &optional resets fetched label)
  "Put KIND's usage in the cache as used up, until RESETS (seconds from
now, an hour by default), read FETCHED seconds ago."
  (setf (alist-get kind ghostherd-usage--cache)
        (list :windows (list (list :label (or label "5h") :remaining 0
                                   :resets-at (+ (float-time) (or resets 3600))))
              :fetched (- (float-time) (or fetched 0)))))

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
          (should (eq fn 'tell))
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

(defvar ghostherd-tests--fake-spawned nil
  "The plist the `fake' backend was last asked to spawn.")

(cl-defmethod ghostherd-backend-spawn ((_backend (eql fake)) plist)
  (setq ghostherd-tests--fake-spawned plist)
  (list :host-id (concat "gh-" (plist-get plist :name)) :buffer nil :started t))

(ert-deftest ghostherd-test-relay-spawn-keeps-the-line ()
  "An heir's line goes to the host with its recipe, and comes back in it;
an agent spawned without one is the first of its name."
  (ghostherd-tests--with-herd ((ghostherd--lineage (make-hash-table :test 'equal))
                               (ghostherd--roles (make-hash-table :test 'equal))
                               (ghostherd-tests--fake-spawned nil))
    (cl-letf (((symbol-function 'ghostherd--install-hooks) #'ignore)
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

(ert-deftest ghostherd-test-when-ready-says-when-it-will-not-be ()
  (ghostherd-tests--with-herd ((ghostherd--log nil))
    (let ((s (ghostherd-tests--session :name "rd" :kind 'grok :state 'starting))
          said)
      (ghostherd--when-ready s (lambda () (push 'told said)) 0 (lambda () (push 'never said)))
      (should (equal said '(never)))
      (remhash "rd" ghostherd--sessions)
      (ghostherd--when-ready s (lambda () (push 'told said)) 5 (lambda () (push 'gone said)))
      (should (equal said '(gone never)))
      (puthash "rd" s ghostherd--sessions)
      (setf (ghostherd-session-state s) 'idle)
      (ghostherd--when-ready s (lambda () (push 'told said)) 5 (lambda () (push 'never said)))
      (should (equal said '(told gone never))))))

(ert-deftest ghostherd-test-relay-from-telegram ()
  (ghostherd-tests--relaying
    (ghostherd-tests--session :name "rd" :kind 'claude :state 'idle :backend 'fake)
    (should (equal (ghostherd--herd-run-command '(:id "c1" :op "relay" :session "rd" :args (:kind "grok")))
                   '(:id "c1" :text "relaying rd to grok")))
    (should (eq (ghostherd-session-kind (ghostherd-get "rd")) 'grok))
    (should (equal (plist-get (ghostherd--herd-run-command '(:id "c2" :op "relay" :session "rd" :args (:kind "rm -rf")))
                              :text)
                   "Relay to what: rm -rf"))))

(ert-deftest ghostherd-test-relay-on-the-page ()
  "The page shows an heir's place in its line and where its work would
go; its Relay names the CLI, which must be one an agent runs on."
  (ghostherd-tests--relaying
    (skip-unless (require 'ghostherd-memory-page nil t))
    (let ((rd (ghostherd-tests--session :name "rd" :kind 'grok :state 'idle :backend 'fake))
          relayed)
      (puthash "rd" '(((kind . "claude") (conversation . "") (ended . "x") (reason . ""))) ghostherd--lineage)
      (ghostherd-tests--used-up 'grok)
      (let ((a (ghostherd-memory-page--agent rd)))
        (should (equal (plist-get a :generation) 2))
        (should (equal (plist-get a :line) ["claude"]))
        (should (equal (plist-get a :relayTo) "claude"))
        (should (string-match-p "its grok usage" (plist-get a :usedUp))))
      (cl-letf (((symbol-function 'ghostherd-relay) (lambda (s kind &rest _) (setq relayed (list s kind))))
                ((symbol-function 'ghostherd-memory-page--js) #'ignore)
                ((symbol-function 'ghostherd-memory-page--send-herd) #'ignore))
        (ghostherd-memory-page--agent-act "agent-relay" '((name . "rd") (kind . "agy")))
        (should (equal relayed (list rd 'agy)))
        (should-error (ghostherd-memory-page--agent-act "agent-relay" '((name . "rd") (kind . "shell")))
                      :type 'user-error)))))

(ert-deftest ghostherd-test-page-meters-each-agent-once-per-change ()
  "Each agent's meter, read by the sidecar from its transcript when that
changed -- not on every look -- and sent field by field.  None for an
agent whose transcript is not known, or that has not replied yet."
  (ghostherd-test--with-herd
    (let* ((file (make-temp-file "gh-meter" nil ".jsonl" "{}\n"))
           (ghostherd-memory-page--meters (make-hash-table :test 'equal))
           (ghostherd-memory-page--metering (make-hash-table :test 'equal))
           (ghostherd-memory-page--herd-soon nil)
           (paths `(("claude-api" . ,file))))
      (puthash "agy-x" (ghostherd-session--create :id "agy-x" :name "agy-x" :kind 'agy :state 'idle
                                                  :project "/p/" :backend 'tmux)
               ghostherd--sessions)
      (cl-letf (((symbol-function 'ghostherd-session-transcript)
                 (lambda (s) (cdr (assoc (ghostherd-session-name s) paths)))))
        (unwind-protect
            (let ((meter (lambda (name) (plist-get (ghostherd-memory-page--agent (ghostherd-get name)) :meter))))
              (setq reply (lambda (_m _p) '(:model "Opus 5.5" :effort "xhigh" :tokens 833282 :window 1000000
                                            :pending :json-false :compacted :json-false :ts "x" :secret "y")))
              (ghostherd-memory-page--meter-look)
              (ghostherd-memory-page--meter-look)
              (should (equal requests `(("herd_meter" (:path ,file)))))
              (should (equal (funcall meter "claude-api")
                             '(:model "Opus 5.5" :effort "xhigh" :tokens 833282 :window 1000000
                               :pending :json-false :compacted :json-false)))
              (should (eq (funcall meter "agy-x") :json-false))
              ;; Changed: asked again.  Compacted since its last reply: no size.
              (with-temp-file file (insert "{}\n{}\n"))
              (setq reply (lambda (_m _p) '(:model "Opus 5.5" :effort "xhigh" :tokens nil :window 1000000
                                            :pending :json-false :compacted t)))
              (ghostherd-memory-page--meter-look)
              (should (= (length requests) 2))
              (should (equal (plist-get (funcall meter "claude-api") :tokens) nil))
              (should (eq (plist-get (funcall meter "claude-api") :compacted) t))
              ;; A new conversation, not replied to yet: no meter.
              (with-temp-file file (insert "{}\n"))
              (setq reply (lambda (_m _p) nil))
              (ghostherd-memory-page--meter-look)
              (should (eq (funcall meter "claude-api") :json-false)))
          (delete-file file))))))

(ert-deftest ghostherd-test-meter-signature-follows-grok-signals ()
  "A grok meter depends on its `signals.json' as much as its transcript:
its tokens live there, written a beat after the conversation, so the
signature must move when that file does -- or the meter binds stale and
is never asked again while the agent idles.  A claude meter does not read
the file, so its signature ignores it."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let* ((ghostherd--sessions (make-hash-table :test #'equal))
         (dir (make-temp-file "gh-metersig" t))
         (tx (expand-file-name "chat_history.jsonl" dir))
         (sig-file (expand-file-name "signals.json" dir))
         (grok (ghostherd-tests--session :name "gk" :kind 'grok :backend 'fake))
         (cl (ghostherd-tests--session :name "cl" :kind 'claude :backend 'fake)))
    (unwind-protect
        (progn
          (write-region "{}\n" nil tx nil 'silent)
          (write-region "{\"contextTokensUsed\":10}\n" nil sig-file nil 'silent)
          (let ((g1 (ghostherd-memory-page--meter-sig grok tx))
                (c1 (ghostherd-memory-page--meter-sig cl tx)))
            ;; signals.json changes; the transcript does not.
            (write-region "{\"contextTokensUsed\":123456}\n" nil sig-file nil 'silent)
            (let ((g2 (ghostherd-memory-page--meter-sig grok tx))
                  (c2 (ghostherd-memory-page--meter-sig cl tx)))
              (should-not (equal g1 g2))       ; grok notices
              (should (equal c1 c2))           ; claude does not
              ;; grok's signature still carries the transcript's, so a change
              ;; there is caught too.
              (should (equal (car g2) (ghostherd-memory-page--room-signature tx))))))
      (dolist (s (list grok cl))
        (when (buffer-live-p (ghostherd-session-buffer s))
          (kill-buffer (ghostherd-session-buffer s))))
      (delete-directory dir t))))

(ert-deftest ghostherd-test-page-reads-sub-agents-when-they-change ()
  "Each agent's sub-agents still running, read by the sidecar when its
transcript changed or a file its last answer named -- not on every
look -- and sent field by field.  None for a dead agent: they stopped
with it."
  (ghostherd-test--with-herd
    (let* ((file (make-temp-file "gh-subs" nil ".jsonl" "{}\n"))
           (folder (make-temp-file "gh-subs" t))
           (running (expand-file-name "agent-a1.jsonl" folder))
           (ghostherd-memory-page--subs (make-hash-table :test 'equal))
           (ghostherd-memory-page--subs-reading (make-hash-table :test 'equal))
           (ghostherd-memory-page--herd-soon nil))
      (write-region "{}\n" nil running nil 'silent)
      (puthash "agy-x" (ghostherd-session--create :id "agy-x" :name "agy-x" :kind 'agy :state 'idle
                                                  :project "/p/" :backend 'tmux)
               ghostherd--sessions)
      (cl-letf (((symbol-function 'ghostherd-session-transcript)
                 (lambda (s) (and (equal (ghostherd-session-name s) "claude-api") file))))
        (unwind-protect
            (let ((subs (lambda (name) (plist-get (ghostherd-memory-page--agent (ghostherd-get name)) :subs)))
                  (asked (lambda () (seq-count (lambda (r) (equal (car r) "herd_subagents")) requests))))
              (setq reply (lambda (_m _p)
                            `(:subs ((:id "a1" :who "general-purpose" :title "Build R3" :started "2026-10-14T08:37:12Z"
                                      :tools 61 :tokens 195958 :doing "Bash · build" :secret "x")
                                     "not a sub-agent")
                              :watch (,folder ,running))))
              (ghostherd-memory-page--subs-look)
              (should (equal requests `(("herd_subagents" (:path ,file)))))
              (should (equal (funcall subs "claude-api")
                             [(:id "a1" :who "general-purpose" :title "Build R3" :started "2026-10-14T08:37:12Z"
                               :tools 61 :tokens 195958 :doing "Bash · build")]))
              (should (equal (funcall subs "agy-x") []))
              ;; What it now watches is new: read once more, then not again.
              (ghostherd-memory-page--subs-look)
              (ghostherd-memory-page--subs-look)
              (should (= (funcall asked) 2))
              ;; A running one wrote: asked again.  Its transcript is the same.
              (write-region "{}\n" nil running t 'silent)
              (set-file-times running (time-add nil 5))
              (setq reply (lambda (_m _p) `(:subs nil :watch (,folder))))
              (ghostherd-memory-page--subs-look)
              (should (= (funcall asked) 3))
              (should (equal (funcall subs "claude-api") []))
              ;; Dead: none, before the next look too, and not asked.
              (setq reply (lambda (_m _p) `(:subs ((:id "a2" :tools 1)) :watch (,folder))))
              (write-region "{}\n" nil file t 'silent)
              (ghostherd-memory-page--subs-look)
              (should (= (length (funcall subs "claude-api")) 1))
              (setf (ghostherd-session-state (ghostherd-get "claude-api")) 'dead)
              (should (equal (funcall subs "claude-api") []))
              (ghostherd-memory-page--subs-look)
              (should (= (funcall asked) 4))
              (should (equal (funcall subs "claude-api") [])))
          (delete-file file)
          (delete-directory folder t))))))

(ert-deftest ghostherd-test-room-sends-a-sub-agents-report ()
  "A sub-agent's report in a column, and the line that started it: what
the page needs of each, field by field."
  (ghostherd-test--with-room
    (setq reply (lambda (_m _p)
                  '(:cli "claude"
                    :entries ((:role "tools" :tools ((:name "Agent" :hint "Build R3" :sub "a1" :state "completed")
                                                     (:name "Bash" :hint "ls")))
                              (:role "sub" :kind "report" :sub "a1" :who "general-purpose" :title "Build R3"
                               :text "## built" :status "completed" :tokens 195958 :uses 61 :ms 931357
                               :ts "2026-10-14T08:52:44Z" :secret "x")
                              (:role "sub" :kind "report" :sub "a2" :text "partial" :tokens "many")))))
    (ghostherd-memory-page--room-tick)
    (let* ((entries (plist-get (ghostherd-test--call calls "setConversation") :entries))
           (tools (plist-get (aref entries 0) :tools))
           (e (aref entries 1)))
      (should (equal (seq-map (lambda (k) (plist-get (aref tools 0) k)) '(:name :hint :sub :state))
                     '("Agent" "Build R3" "a1" "completed")))
      (should (equal (seq-map (lambda (k) (plist-get (aref tools 1) k)) '(:sub :state)) '(nil nil)))
      (should (equal (seq-map (lambda (k) (plist-get e k))
                              '(:role :sub :who :title :text :status :tokens :uses :ms :ts))
                     '("sub" "a1" "general-purpose" "Build R3" "## built" "completed" 195958 61 931357
                       "2026-10-14T08:52:44Z")))
      (should-not (plist-member e :secret))
      (should (equal (seq-map (lambda (k) (plist-get (aref entries 2) k)) '(:status :tokens :uses :ms))
                     '(nil nil nil nil))))))

(ert-deftest ghostherd-test-page-switches-a-model ()
  "The page is told whose model it can switch, and to what; its Switch
goes to `ghostherd-switch-model', and the page hears how it went."
  (ghostherd-test--with-herd
    (ghostherd-memory-page--send-herd)
    (let ((herd (ghostherd-test--call calls "renderHerd")))
      (should (equal (plist-get (plist-get herd :pickers) :claude)
                     '(:models ["Default" "Opus" "Fable" "Sonnet" "Haiku"]
                       :efforts ["low" "medium" "high" "xhigh" "max"] :scope "session")))
      (should (equal (plist-get (plist-get herd :pickers) :grok)
                     '(:models ["grok-4.7" "grok-4.7-build-fast" "grok-4.6" "grok-4.5"]
                       :efforts ["low" "medium" "high" "xhigh"] :scope "default")))
      (should (equal (plist-get (plist-get herd :pickers) :agy)
                     '(:models ["Gemini 4 Argon" "Gemini 3.8 Flash" "Claude Opus 5.5"
                                "Claude Sonnet 5.5" "GPT-OSS 120B"]
                       :efforts ["low" "medium" "high"] :scope "default")))
      (should (eq (plist-get (aref (plist-get herd :agents) 0) :switching) :json-false)))
    (let (asked (said "Set model to Sonnet 5.5 for this session only"))
      (cl-letf (((symbol-function 'ghostherd-switch-model)
                 (lambda (s model effort done)
                   (setq asked (list (ghostherd-session-name s) model effort))
                   (funcall done (not (equal model "Fable")) (if (equal model "Fable") "its picker has no Fable" said)))))
        (ghostherd-memory-page--agent-act "agent-model" '((name . "claude-api") (model . "Sonnet") (effort . "")))
        (should (equal asked '("claude-api" "Sonnet" nil)))
        (should (member (cons "flash" said) calls))
        (should (member '("flash" . "Switching claude-api to Sonnet…") calls))
        (ghostherd-memory-page--agent-act "agent-model" '((name . "claude-api") (model . "Fable") (effort . "max")))
        (should (equal asked '("claude-api" "Fable" "max")))
        (should (member '("flash" . "claude-api: its picker has no Fable") calls))
        (should-error (ghostherd-memory-page--agent-act "agent-model" '((name . "claude-api")))
                      :type 'user-error)))))

(provide 'ghostherd-tests)
;;; ghostherd-tests.el ends here

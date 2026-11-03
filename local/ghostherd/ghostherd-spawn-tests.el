;;; ghostherd-spawn-tests.el --- Tests for ghostherd-spawn.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Part of ghostherd's tests; ghostherd-tests.el runs them all.  This
;; file alone:
;;
;;   emacs --batch --init-directory=~/.emacs.d/emacs-home/ \
;;         -L . -L ../xwapp -l ghostherd-spawn-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ghostherd-test-helpers)

(declare-function ghostherd-memory-page--handle "ghostherd-memory-page" (intent))
(declare-function ghostherd-memory-page--herd "ghostherd-memory-page" ())
(declare-function ghostherd-memory-page--herd-tick "ghostherd-memory-page" ())
(declare-function ghostherd-memory-page--herd-unwatch "ghostherd-memory-page" ())
(declare-function ghostherd-memory-page--meter "ghostherd-memory-page" (s))
(declare-function ghostherd-memory-page--on-herd-change "ghostherd-memory-page" (_entry))
(declare-function ghostherd-memory-page--resume-info "ghostherd-memory-page" (path chunks))
(declare-function ghostherd-memory-page--send-herd "ghostherd-memory-page" ())
(defvar ghostherd-memory-page--herd-soon)

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
                   "model not switched: its picker has no Fable"))
    (should (ghostherd-tests--issue-p "model not switched"))))

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
   ;; agy 1.4.0 ignores the herd's bracketed paste as a search filter, so a
   ;; pasted name changes nothing: the rows are walked by arrow instead.
   ((and (not submit) (ghostherd-tests--agy-get :open)) nil)))
(cl-defmethod ghostherd-backend-send-keys ((_backend (eql agy-picker)) _session keys)
  (ghostherd-tests--agy-set :sent (append (ghostherd-tests--agy-get :sent) keys))
  (dolist (key keys)
    (when (ghostherd-tests--agy-get :open)
      (let* ((rows (mapcar #'car (ghostherd-tests--agy-rows)))
             (cursor (ghostherd-tests--agy-get :cursor))
             (ri (cl-position cursor rows :test #'equal))
             (at (assoc cursor (ghostherd-tests--agy-get :at)))
             (efforts (and cursor (ghostherd-tests--agy-efforts cursor)))
             (i (and at (cl-position (cdr at) efforts :test #'equal))))
        (pcase key
          ;; The real picker's list wraps top to bottom.
          ("down" (when ri (ghostherd-tests--agy-set :cursor (nth (mod (1+ ri) (length rows)) rows))))
          ("up" (when ri (ghostherd-tests--agy-set :cursor (nth (mod (1- ri) (length rows)) rows))))
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

(defconst ghostherd-tests--agy-real-screen
  "Switch Model

  Search:
          ────────────────────

  Gemini 4 Argon
> Gemini 3.8 Flash      (current)
  Claude Opus 5.5
  Claude Sonnet 5.5
  GPT-OSS 120B (Medium)

  Effort  ◂        ●━━━━━━━━━━━━━━●━━━━━━━━━━━━━━◉        ▸
                  low          medium          high
            Deepest reasoning for complex problems — slower but strongest

Keyboard: ↑/↓ Navigate  ←/→ Effort  enter Select  esc Go Back

                                                                            Gemini 3.8 Flash · high · AI: Out of credits
"
  "agy 1.4.0's real \"Switch Model\" picker, captured from a scratch agy:
the cursor on the current model, the full list, an effort slider on high.")

(ert-deftest ghostherd-test-agy-picker-reads-the-real-14-screen ()
  "The reader on agy 1.4.0's actual picker: every row, the `>' cursor on
the current model, the slider's steps and its ◉ on high."
  (let ((p (ghostherd--agy-picker ghostherd-tests--agy-real-screen)))
    (should (equal (plist-get p :rows)
                   '("Gemini 4 Argon" "Gemini 3.8 Flash" "Claude Opus 5.5"
                     "Claude Sonnet 5.5" "GPT-OSS 120B (Medium)")))
    (should (equal (plist-get p :cursor) "Gemini 3.8 Flash"))
    (should (equal (plist-get p :efforts) '("low" "medium" "high")))
    (should (equal (plist-get p :effort) "high"))))

(ert-deftest ghostherd-test-agy-said-is-the-last ()
  (should (equal (ghostherd--agy-said
                  "x\n> /model\n  ⎿  Model set to Claude Opus 5.5 (High)\n>\n")
                 "Model set to Claude Opus 5.5 (High)"))
  (should (equal (ghostherd--agy-said "  ⎿  Exited /model command\n") "Exited /model command"))
  (should-not (ghostherd--agy-said "nothing happened here\n")))

(ert-deftest ghostherd-test-agy-switches-by-arrowing-and-slider ()
  "`/model', ↓/↑ from the current model to the chosen one, → to the
effort, Enter; said to have become the default.  Not the search box: the
herd's paste is not a filter there."
  (ghostherd-tests--with-agy
    ;; Current is Gemini 3.8 Flash (row 2); Claude Opus 5.5 is row 3 -- one down.
    (should (equal (ghostherd-tests--switch s "Claude Opus 5.5" "high")
                   '(t . "Model set to Claude Opus 5.5 (High) (now its default)")))
    (should (equal (ghostherd-tests--agy-get :sent)
                   '((:submit "/model") "down" "right" "return")))
    (should (equal (list (ghostherd-tests--agy-get :model) (ghostherd-tests--agy-get :effort))
                   '("Claude Opus 5.5" "high")))
    (should (equal (ghostherd-log-entry-text (car ghostherd--log))
                   "Model set to Claude Opus 5.5 (High) (now its default)"))))

(ert-deftest ghostherd-test-agy-takes-the-effort-a-model-has ()
  "An effort a model does not offer leaves it where it is; no effort
asked, it is switched at the model's own.  The cursor walks up as well
as down, from wherever the current model sits."
  (ghostherd-tests--with-agy
    ;; Gemini 3.8 Flash (row 2) to GPT-OSS 120B (row 5) is shorter up and
    ;; round -- row 2 → 1 → 5 -- than three down; it offers only medium, so
    ;; high is left alone.
    (should (equal (ghostherd-tests--switch s "GPT-OSS 120B" "high")
                   '(t . "Model set to GPT-OSS 120B (Medium) (now its default)")))
    (should (equal (ghostherd-tests--agy-get :sent)
                   '((:submit "/model") "up" "up" "return")))
    (ghostherd-tests--agy-set :sent nil)
    ;; Now current is GPT-OSS 120B (row 5); Gemini 4 Argon (row 1) is one down
    ;; round the wrap.
    (should (car (ghostherd-tests--switch s "Gemini 4 Argon")))
    (should (equal (ghostherd-tests--agy-get :sent) '((:submit "/model") "down" "return")))
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
  (skip-unless (require 'ghostherd-memory-page nil t))
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
the model and effort come off the screen.  Merged into one meter, each
with a window for a bar: grok's from the sidecar, agy's the uniform 1.0M
its `/context' reports, so agy is no longer the number alone."
  (skip-unless (require 'ghostherd-memory-page nil t))
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
      ;; agy's transcript has no window; the page fills the 1.0M /context shows.
      (puthash "ag" (cons "s2" '(:model "" :effort "" :tokens 189225 :window nil
                                 :pending :json-false :compacted :json-false))
               ghostherd-memory-page--meters)
      (should (equal (ghostherd-memory-page--meter a)
                     '(:model "Gemini 3.8 Flash" :effort "medium" :tokens 189225 :window 1000000
                       :pending :json-false :compacted :json-false))))))

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

;;; Resuming a past conversation

(ert-deftest ghostherd-test-resume-args ()
  ;; The conversation existing is checked separately (see the gone-p tests);
  ;; here the flag/format contract, with it assumed present.
  (cl-letf (((symbol-function 'ghostherd--conversation-gone-p) (lambda (&rest _) nil)))
    (should (equal (ghostherd--resume-args 'claude "a0c6b62b-e1d2") '("-r" "a0c6b62b-e1d2")))
    (should (equal (ghostherd--resume-args 'claude "a0c6" t) '("-r" "a0c6" "--fork-session")))
    (should (equal (ghostherd--resume-args 'grok "g1" t) '("-r" "g1" "--fork-session")))
    (should (equal (ghostherd--resume-args 'agy "c9") '("--conversation" "c9")))
    (should-not (ghostherd--resume-args 'agy "c9" t))
    (should-not (ghostherd--resume-args 'shell "x"))
    ;; An id comes from an index of files: one that reads as a flag is refused.
    (should-not (ghostherd--resume-args 'claude "-rf"))
    (should-not (ghostherd--resume-args 'claude "a b"))
    (should-not (ghostherd--resume-args 'claude nil))))

(ert-deftest ghostherd-test-a-gone-conversation-is-seen ()
  "A claude or agy conversation missing from where its CLI keeps it reads as
gone; grok's live on its server, so a local file is not a sure signal and it
is not checked; a malformed id is not probed."
  (let* ((dir (file-name-as-directory (make-temp-file "gh-conv-" t)))
         (pattern (concat dir "p/%s.jsonl")))
    (unwind-protect
        (cl-letf (((symbol-function 'ghostherd--spec)
                   (lambda (_k) (list :transcript pattern :resume-args '("-r" "%s")))))
          (should (ghostherd--conversation-gone-p 'claude "aaa111"))   ; no file -> gone
          (make-directory (concat dir "p"))
          (write-region "" nil (format pattern "aaa111"))
          (should-not (ghostherd--conversation-gone-p 'claude "aaa111")) ; there -> not
          (should (ghostherd--conversation-gone-p 'agy "bbb222"))        ; agy checkable too
          (should-not (ghostherd--conversation-gone-p 'grok "ccc333"))   ; remote -> unchecked
          (should-not (ghostherd--conversation-gone-p 'claude "../etc"))) ; malformed -> unprobed
      (delete-directory dir t))))

(ert-deftest ghostherd-test-resume-args-drops-a-gone-conversation ()
  "A conversation no longer on disk is not handed to the resume flag -- the
caller starts fresh rather than the CLI exiting on an id it cannot open."
  (cl-letf (((symbol-function 'ghostherd--conversation-gone-p)
             (lambda (kind _id) (eq kind 'claude))))   ; claude's gone, grok's not checked
    (should-not (ghostherd--resume-args 'claude "x1"))
    (should (equal (ghostherd--resume-args 'grok "g1") '("-r" "g1")))))

(ert-deftest ghostherd-test-started-on-resume-is-detected ()
  (ghostherd-tests--with-herd ()
    (let ((r (ghostherd-tests--session :name "c-r" :kind 'claude :args '("--model" "o" "-r" "id9")))
          (f (ghostherd-tests--session :name "c-f" :kind 'claude :args '("--model" "o"))))
      (should (ghostherd--started-on-resume-p r))
      (should-not (ghostherd--started-on-resume-p f)))))

(ert-deftest ghostherd-test-a-failed-resume-restarts-fresh-once ()
  "An agent that dies just after a resumed start -- the CLI could not open its
conversation (claude \"No conversation found with session ID\", grok a remote
404) -- is started once more, FRESH (no resume flag), so a pruned conversation
never wedges it.  A death long after the start is a crash, not a resume
failure; and a fresh start carries no resume flag, so this never loops."
  (ghostherd-tests--with-herd ((ghostherd-notify-on-exit nil))
    (let (spawned)
      (cl-letf (((symbol-function 'run-at-time) (lambda (_t _r fn &rest _) (funcall fn)))
                ((symbol-function 'ghostherd-kill) (lambda (&rest _) nil))
                ((symbol-function 'ghostherd-spawn)
                 (lambda (_kind &rest plist) (push (plist-get plist :args) spawned) nil)))
        ;; grok, resumed, dies at once with its remote 404: respawned fresh.
        (let ((s (ghostherd-tests--session :name "grok-dp" :kind 'grok :state 'working
                                           :args '("--model" "x" "-r" "gone-id"))))
          (setf (ghostherd-session-started-at s) (current-time))
          (ghostherd--set-state
           s 'dead "Error: Failed to restore session from remote: session get failed: 404 Not Found")
          (should (equal (car spawned) '("--model" "x")))   ; -r gone-id stripped
          (should-not (member "-r" (car spawned))))
        ;; A death long after the start is a crash, not a resume failure.
        (setq spawned nil)
        (let ((s (ghostherd-tests--session :name "c-late" :kind 'claude :state 'working
                                           :args '("-r" "id2"))))
          (setf (ghostherd-session-started-at s) (time-subtract (current-time) (seconds-to-time 120)))
          (ghostherd--set-state s 'dead "exited")
          (should-not spawned))
        ;; A fresh start (no resume flag) that dies at once is not retried.
        (setq spawned nil)
        (let ((s (ghostherd-tests--session :name "c-fresh" :kind 'claude :state 'working
                                           :args '("--model" "x"))))
          (setf (ghostherd-session-started-at s) (current-time))
          (ghostherd--set-state s 'dead "No conversation found with session ID: id3")
          (should-not spawned))))))

(ert-deftest ghostherd-test-primary-drops-a-gone-kept-conversation ()
  "Primary's kept conversation gone (claude's, verifiable): the start drops
the kept id, logs why, and comes up fresh rather than exiting on it."
  (ghostherd-tests--with-primary
    (cl-letf (((symbol-function 'ghostherd--conversation-gone-p) (lambda (&rest _) t)))
      (make-directory (file-name-directory (ghostherd--primary-state-file)) t)
      (with-temp-file (ghostherd--primary-state-file)
        (prin1 '(:kind claude :conversation "c-gone") (current-buffer)))
      (ghostherd-primary-start)
      (should-not (file-exists-p (ghostherd--primary-state-file)))   ; kept id cleared
      (should-not (member "-r" (plist-get ghostherd-tests--fake-spawned :args))) ; fresh
      (should (seq-find (lambda (e) (string-match-p "kept conversation c-gone is gone"
                                                    (ghostherd-log-entry-text e)))
                        ghostherd--log))
      (should (ghostherd-tests--issue-p "kept conversation c-gone is gone")))))

(ert-deftest ghostherd-test-resume-spawns-in-the-directory ()
  (let ((ghostherd--sessions (make-hash-table :test #'equal))
        (dir (make-temp-file "gh-proj-" t))
        spawned)
    (unwind-protect
        (cl-letf (((symbol-function 'ghostherd-spawn)
                   (lambda (kind &rest plist) (setq spawned (cons kind plist))))
                  ((symbol-function 'ghostherd--conversation-gone-p) (lambda (&rest _) nil))
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
        (cl-letf (((symbol-function 'ghostherd--resumed-by) (lambda (_) nil))
                  ;; resume-info's own file-exists-p check (below) is what
                  ;; this covers; the global glob is checked in the gone-p tests.
                  ((symbol-function 'ghostherd--conversation-gone-p) (lambda (&rest _) nil)))
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

;;; A new agent in any project, maybe on a past conversation (ask 28f170ff)

(defvar ghostherd-tests--tx nil
  "Where a `ghostherd-tests--with-places' test's transcripts are.")

(defun ghostherd-tests--tx (name)
  "Transcript NAME's path in the test's `ghostherd-tests--tx', there."
  (let ((f (expand-file-name name ghostherd-tests--tx)))
    (unless (file-exists-p f)
      (make-directory (file-name-directory f) t)
      (write-region "" nil f nil 'silent))
    f))

(defmacro ghostherd-tests--with-places (&rest body)
  "BODY with `base' a temp dir of projects a/ (with a/sub/), b/ to f/, wt/
-- a worktree of a/, as `ghostherd--project-root' (stubbed) resolves
it -- and primary/ primary's; `dir' a function naming one; no sidecar,
and nothing kept of the places yet.  Set `ghostherd--new-sources' for
what the sidecar would list."
  (declare (indent 0) (debug t))
  `(let* ((base (file-name-as-directory (file-truename (make-temp-file "gh-places" t))))
          (dir (lambda (n) (file-name-as-directory (expand-file-name n base))))
          (ghostherd--new-sources nil)
          (ghostherd--new-roots (make-hash-table :test 'equal))
          (ghostherd--new-waiting nil)
          (ghostherd-tests--tx (expand-file-name "tx/" base))
          (ghostherd-primary-directory (expand-file-name "primary/" base)))
     (dolist (d '("a/sub/" "b/" "c/" "d/" "e/" "f/" "wt/" "primary/"))
       (make-directory (funcall dir d) t))
     (unwind-protect
         (cl-letf (((symbol-function 'ghostherd--project-root)
                    (lambda (&optional d)
                      (let ((d (file-name-as-directory (expand-file-name (or d default-directory)))))
                        (cond ((string-prefix-p (funcall dir "wt/") d) (funcall dir "a/"))
                              ((string-match (concat "\\`" (regexp-quote base) "\\([^/]+/\\)") d)
                               (funcall dir (match-string 1 d)))))))
                   ((symbol-function 'ghostherd--new-sidecar-p) #'ignore)
                   ((symbol-function 'project-known-project-roots) (lambda () nil)))
           ,@body)
       (delete-directory base t))))

(defun ghostherd-tests--source (path agent id project ago &optional title)
  "An index row as the sidecar lists it: a transcript written AGO seconds
ago, at PATH -- a relative one made there (`ghostherd-tests--tx'), an
absolute one as it is: gone, unless it is there."
  (list :source_path (if (file-name-absolute-p path) path (ghostherd-tests--tx path))
        :agent agent :session_id id :project project :kind "transcript"
        :mtime (- (float-time) ago) :title (or title "")))

(ert-deftest ghostherd-test-new-projects-ranked-and-named-once ()
  "Where a new agent can go: the current project, then those with agents
(the newest at work first), then project.el's in its order, then where
past conversations ran (the newest first) -- each once, however its
directory is written or reached (a worktree is its main checkout's), a
project in two of these ranked by the first.  A gone directory, a dead
agent's project and primary's directory are not offered."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-places
      (let ((ghostherd--new-sources
             (cons (float-time)
                   (list (ghostherd-tests--source "/x/1" "claude" "1" (funcall dir "wt/") 100)
                         (ghostherd-tests--source "/x/2" "claude" "2" (directory-file-name (funcall dir "d")) 50)
                         (ghostherd-tests--source "/x/3" "grok" "3" (funcall dir "f/") 9000)
                         (ghostherd-tests--source "/x/4" "grok" "4" (funcall dir "e/") 3 )
                         (ghostherd-tests--source "/x/5" "claude" "5" "/nowhere/gone/" 1)
                         (ghostherd-tests--source "/x/6" "claude" "6" (funcall dir "primary/") 1)))))
        (ghostherd-tests--session :name "b-1" :project (funcall dir "b/") :state 'idle
                                  :last-active (time-subtract nil 30))
        (ghostherd-tests--session :name "lead-b" :project (funcall dir "b/") :state 'working
                                  :last-active (time-subtract nil 5))
        (puthash "lead-b" "lead" ghostherd--roles)
        (ghostherd-tests--session :name "c-1" :project (funcall dir "c/") :state 'dead)
        (ghostherd-tests--session :name "primary" :project (funcall dir "primary/") :state 'idle)
        (cl-letf (((symbol-function 'project-known-project-roots)
                   (lambda () (list (funcall dir "d/") (funcall dir "c/") "/nowhere/else/"))))
          (let* ((places (ghostherd-new-projects (funcall dir "a/sub/")))
                 (by (lambda (n) (seq-find (lambda (p) (equal (plist-get p :root) (funcall dir n))) places))))
            (should (equal (mapcar (lambda (p) (plist-get p :root)) places)
                           (mapcar dir '("a/" "b/" "d/" "c/" "e/" "f/"))))
            (should (equal (mapcar (lambda (p) (plist-get p :name)) places)
                           (mapcar (lambda (n) (ghostherd--project-name (funcall dir n)))
                                   '("a/" "b/" "d/" "c/" "e/" "f/"))))
            ;; The current project, with its worktree's conversation.
            (should (plist-get (funcall by "a/") :here))
            (should (= (plist-get (funcall by "a/") :conversations) 1))
            (should (equal (ghostherd-new-project-note (funcall by "a/"))
                           "this project · last conversation 1m ago"))
            (should (equal (ghostherd-new-project-note (funcall by "b/")) "2 agents · lead"))
            ;; d: known, and its conversation's "…/d" is the same project.
            (should (plist-get (funcall by "d/") :known))
            (should (= (plist-get (funcall by "d/") :conversations) 1))
            ;; c's only agent is dead: offered for project.el, with none.
            (should (= (plist-get (funcall by "c/") :agents) 0))
            (should (equal (ghostherd-new-project-note (funcall by "c/")) ""))
            ;; Primary's directory only when it is where you are.
            (should (equal (plist-get (car (ghostherd-new-projects (funcall dir "primary/"))) :root)
                           (funcall dir "primary/")))))))))

(ert-deftest ghostherd-test-new-reads-past-conversations-when-asked-and-keeps-them ()
  "The past conversations are asked of the sidecar when a picker opens --
transcripts only, with their titles -- and kept a minute; a sidecar that
does not answer costs them, not the picker, and is not asked again until
then."
  (let ((ghostherd--new-sources nil)
        (ghostherd--new-roots (make-hash-table :test 'equal))
        (asked 0))
    (cl-letf (((symbol-function 'ghostherd--new-sidecar-p) (lambda () t))
              ((symbol-function 'ghostherd-memory-request)
               (lambda (method params &optional _timeout)
                 (cl-incf asked)
                 (should (equal method "memory_list"))
                 (should (eq (plist-get params :titles) t))
                 (list :sources (list (list :kind "transcript" :source_path "t")
                                      (list :kind "memory" :source_path "m"))))))
      (should (equal (mapcar (lambda (s) (plist-get s :source_path)) (ghostherd--new-read-sources))
                     '("t")))
      (ghostherd--new-read-sources)
      (should (= asked 1))
      (puthash "/x/" "/x/" ghostherd--new-roots)
      (setcar ghostherd--new-sources (- (float-time) (1+ ghostherd-new-keep-seconds)))
      (ghostherd--new-read-sources)
      (should (= asked 2))
      ;; Read again, the directories are resolved again.
      (should (= (hash-table-count ghostherd--new-roots) 0)))
    (setq ghostherd--new-sources nil)
    (cl-letf (((symbol-function 'ghostherd--new-sidecar-p) (lambda () t))
              ((symbol-function 'ghostherd-memory-request)
               (lambda (&rest _) (cl-incf asked) (error "No answer"))))
      (should-not (ghostherd--new-read-sources))
      (should-not (ghostherd--new-read-sources))
      (should (= asked 3)))))

(ert-deftest ghostherd-test-new-asks-the-project-first-and-takes-its-defaults ()
  "`ghostherd-new' asks the project first, the current one the default;
the kind and args are the CHOSEN project's -- from a buffer in A, an
agent for B is B's kind with B's args -- and a typed subdirectory is
where it runs, its project its root.  With no past conversation there,
no conversation is asked."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-places
      (with-temp-file (expand-file-name ".dir-locals.el" (funcall dir "a/"))
        (insert "((nil . ((ghostherd-project-kind . claude) (ghostherd-project-args . (\"--from-a\")))))"))
      (with-temp-file (expand-file-name ".dir-locals.el" (funcall dir "b/"))
        (insert "((nil . ((ghostherd-project-kind . grok) (ghostherd-project-args . (\"--from-b\")))))"))
      (let ((default-directory (funcall dir "a/"))
            (answer nil) prompts spawned)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (prompt _table &optional _p _r _i _h def &rest _)
                     (push (cons prompt def) prompts)
                     (if (string-prefix-p "Project" prompt) answer def)))
                  ((symbol-function 'read-string)
                   (lambda (prompt &optional initial &rest _)
                     (push (cons prompt initial) prompts)
                     (or initial "")))
                  ((symbol-function 'y-or-n-p) #'ignore)
                  ((symbol-function 'ghostherd-spawn)
                   (lambda (kind &rest plist) (setq spawned (cons kind plist)) nil)))
          (setq answer (ghostherd--project-name (funcall dir "b/")))
          (call-interactively #'ghostherd-new)
          (should (equal (car (last prompts))
                         (cons "Project (or type a directory): "
                               (ghostherd--project-name (funcall dir "a/")))))
          (should (equal (assoc "Agent kind: " prompts) '("Agent kind: " . "grok")))
          (should (eq (car spawned) 'grok))
          (should (equal (plist-get (cdr spawned) :args) '("--from-b")))
          (should (equal (plist-get (cdr spawned) :directory) (funcall dir "b/")))
          (should (equal (plist-get (cdr spawned) :project) (funcall dir "b/")))
          (should-not (seq-find (lambda (p) (string-prefix-p "Conversation" (car p))) prompts))
          ;; A subdirectory typed in: it runs there, in its project.
          (setq answer (funcall dir "a/sub/") prompts nil)
          (call-interactively #'ghostherd-new)
          (should (eq (car spawned) 'claude))
          (should (equal (plist-get (cdr spawned) :directory) (funcall dir "a/sub/")))
          (should (equal (plist-get (cdr spawned) :project) (funcall dir "a/")))
          ;; A directory that is not there is said, not started in.
          (setq answer "/nowhere/at/all/" spawned nil)
          (should-error (call-interactively #'ghostherd-new) :type 'user-error)
          (should-not spawned))))))

(ert-deftest ghostherd-test-new-resumes-a-past-conversation-or-starts-fresh ()
  "After the project and kind: New, the default, starts fresh; or one of
the project's past conversations -- its worktrees' included, newest
first, a gone one left out, another project's not -- is picked up again
in its OWN directory, or forked where its CLI can."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-places
      (let ((ghostherd--new-sources
             (cons (float-time)
                   (list (ghostherd-tests--source "c1.jsonl" "claude" "in-c1" (funcall dir "wt/") 3600 "In the worktree")
                         (ghostherd-tests--source "c2.jsonl" "claude" "in-c2" (funcall dir "a/") 60)
                         (ghostherd-tests--source "g1/chat_history.jsonl" "grok" "g1" (funcall dir "a/sub/") 7200 "grok one")
                         (ghostherd-tests--source "gone.jsonl" "claude" "gone" (funcall dir "a/") 1 "gone")
                         ;; grok's id is not checked: its chat history gone, it is.
                         (ghostherd-tests--source "/x/g2/chat_history.jsonl" "grok" "g2" (funcall dir "a/") 2 "grok gone")
                         (ghostherd-tests--source "b1.jsonl" "claude" "b1" (funcall dir "b/") 1 "B's"))))
            (default-directory (funcall dir "a/"))
            (pick nil) (fork nil) prompts spawned)
        (cl-letf (((symbol-function 'ghostherd--conversation-gone-p)
                   (lambda (_kind id) (equal id "gone")))
                  ((symbol-function 'completing-read)
                   (lambda (prompt table &optional _p _r _i _h def &rest _)
                     (push (cons prompt (all-completions "" table)) prompts)
                     (if (and pick (string-prefix-p "Conversation" prompt)) pick def)))
                  ((symbol-function 'read-string) (lambda (_p &optional initial &rest _) (or initial "")))
                  ((symbol-function 'y-or-n-p) (lambda (&rest _) fork))
                  ((symbol-function 'ghostherd-spawn)
                   (lambda (kind &rest plist) (setq spawned (cons kind plist)) nil)))
          (let ((convs (ghostherd-new-conversations (funcall dir "a/"))))
            ;; claude resumes by its transcript's name; gone ones -- pruned
            ;; by claude, or whose transcript went -- and B's are not here.
            (should (equal (mapcar (lambda (c) (plist-get c :id)) convs) '("c2" "c1" "g1")))
            (should (equal (plist-get (nth 1 convs) :directory) (funcall dir "wt/")))
            (should (equal (mapcar (lambda (c) (plist-get c :fork)) convs) '(t t t)))
            (should (equal (ghostherd-new-conversation-note (nth 1 convs) (funcall dir "a/"))
                           (format "claude · 1h ago · %s" (ghostherd--abbreviate (funcall dir "wt/")))))
            (should (string-prefix-p "(untitled) c2" (ghostherd-new-conversation-label (nth 0 convs))))
            ;; New is the default and first: RET starts fresh.
            (call-interactively #'ghostherd-new)
            (let ((offered (cdr (assoc "Conversation (New, or one to pick up again): " prompts))))
              (should (equal (car offered) "New"))
              (should (= (length offered) 4)))
            (should (eq (car spawned) 'claude))
            (should-not (member "-r" (plist-get (cdr spawned) :args)))
            ;; One picked: resumed in its own directory, the worktree's, in a/.
            (setq pick (ghostherd-new-conversation-label (nth 1 convs)) spawned nil)
            (call-interactively #'ghostherd-new)
            (should (eq (car spawned) 'claude))
            (should (equal (plist-get (cdr spawned) :directory) (funcall dir "wt/")))
            (should (equal (plist-get (cdr spawned) :project) (funcall dir "a/")))
            (should (equal (last (plist-get (cdr spawned) :args) 2) '("-r" "c1")))
            (should (equal (plist-get (cdr spawned) :notes) "resumed c1: In the worktree"))
            (should-not (plist-get (cdr spawned) :worktree))
            ;; Or forked: on its own CLI -- grok, whatever kind was answered.
            (setq pick (ghostherd-new-conversation-label (nth 2 convs)) fork t spawned nil)
            (call-interactively #'ghostherd-new)
            (should (eq (car spawned) 'grok))
            (should (equal (plist-get (cdr spawned) :directory) (funcall dir "a/sub/")))
            (should (member "--fork-session" (plist-get (cdr spawned) :args)))))))))

(ert-deftest ghostherd-test-new-shows-the-agent-already-on-a-conversation ()
  "A past conversation an agent already runs is shown, never resumed a
second time -- two writers on one conversation; forking it is still a
new agent, on a conversation of its own.  The same from the page's form."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-places
      (let ((ghostherd--new-sources
             (cons (float-time)
                   (list (ghostherd-tests--source "c1.jsonl" "claude" "c1" (funcall dir "a/") 60 "Busy"))))
            (default-directory (funcall dir "a/"))
            (fork nil) visited spawned)
        (ghostherd-tests--session :name "claude-a" :kind 'claude :project (funcall dir "a/")
                                  :args '("-r" "c1") :state 'working)
        (cl-letf (((symbol-function 'ghostherd--conversation-gone-p) #'ignore)
                  ((symbol-function 'ghostherd--session-live-p) (lambda (_) t))
                  ((symbol-function 'completing-read)
                   (lambda (prompt table &optional _p _r _i _h def &rest _)
                     (if (string-prefix-p "Conversation" prompt) (cadr (all-completions "" table)) def)))
                  ((symbol-function 'y-or-n-p) (lambda (&rest _) fork))
                  ((symbol-function 'ghostherd-visit) (lambda (s) (setq visited s)))
                  ((symbol-function 'ghostherd-spawn)
                   (lambda (kind &rest plist) (setq spawned (cons kind plist)) nil)))
          (let ((conv (car (ghostherd-new-conversations (funcall dir "a/")))))
            (should (equal (ghostherd-session-name (plist-get conv :running)) "claude-a"))
            (should (string-match-p "running in claude-a"
                                    (ghostherd-new-conversation-note conv (funcall dir "a/")))))
          (call-interactively #'ghostherd-new)
          (should (equal (ghostherd-session-name visited) "claude-a"))
          (should-not spawned)
          (setq fork t visited nil)
          (call-interactively #'ghostherd-new)
          (should-not visited)
          (should (member "--fork-session" (plist-get (cdr spawned) :args)))
          ;; The page's form: Resume shows it too.
          (when (require 'ghostherd-memory-page nil t)
            (let ((ghostherd-memory-page--resumable (make-hash-table :test #'equal))
                  calls)
              (cl-letf (((symbol-function 'ghostherd-memory-page--js)
                         (lambda (fn obj) (setq calls (append calls (list (cons fn obj)))))))
                (setq visited nil spawned nil)
                (ghostherd-memory-page--handle `((op . "new-conversations") (project . ,(funcall dir "a/"))))
                (should (equal (plist-get (aref (plist-get (ghostherd-test--call calls "setNewConversations")
                                                           :conversations)
                                                0)
                                          :running)
                               "claude-a"))
                (ghostherd-memory-page--handle `((op . "resume") (source_path . ,(ghostherd-tests--tx "c1.jsonl")) (quiet . t)))
                (should (equal (ghostherd-session-name visited) "claude-a"))
                (should-not spawned)
                (should (equal (ghostherd-test--call calls "flash") "claude-a is on it already"))))))))))

(ert-deftest ghostherd-test-herd-page-new-agent-anywhere ()
  "The page's + New agent: the places, sent when the form asks -- never
with the herd -- the current project first; a typed directory's
project, its kind and its past conversations; an agent started in a
project with no agents yet, not shown; a conversation resumed from the
form, not shown either, in its own directory."
  (ghostherd-test--with-herd
    (ghostherd-tests--with-places
      (with-temp-file (expand-file-name ".dir-locals.el" (funcall dir "a/"))
        (insert "((nil . ((ghostherd-project-kind . grok))))"))
      (let ((ghostherd-memory-page--project (directory-file-name (funcall dir "a/")))
            (ghostherd-memory-page--resumable (make-hash-table :test #'equal))
            (ghostherd--new-sources
             (cons (float-time)
                   (list (ghostherd-tests--source "c1.jsonl" "claude" "c1" (funcall dir "wt/") 60 "Here"))))
            spawned)
        (cl-letf (((symbol-function 'ghostherd--conversation-gone-p) #'ignore)
                  ((symbol-function 'project-known-project-roots) (lambda () (list (funcall dir "b/"))))
                  ((symbol-function 'ghostherd-project-defaults)
                   (let ((real (symbol-function 'ghostherd-project-defaults)))
                     (lambda (&optional d) (funcall real d))))
                  ((symbol-function 'ghostherd-spawn)
                   (lambda (kind &rest plist)
                     (setq spawned (cons kind plist))
                     (ghostherd-session--create :name (plist-get plist :name)))))
          (ghostherd-memory-page--handle '((op . "new-places")))
          (let ((places (append (plist-get (ghostherd-test--call calls "setNewPlaces") :places) nil)))
            (should (equal (mapcar (lambda (p) (plist-get p :project)) places)
                           (list (ghostherd--project-name (funcall dir "a/")) "/p/"
                                 (ghostherd--project-name (funcall dir "b/")))))
            (should (eq (plist-get (car places) :here) t))
            (should (equal (plist-get (cadr places) :note) "1 agent")))
          ;; None of it rides the herd.
          (should-not (plist-member (ghostherd-memory-page--herd) :places))
          ;; A subdirectory typed in: its project, its kind, its conversations.
          (ghostherd-memory-page--handle `((op . "new-conversations") (project . ,(funcall dir "a/sub/"))))
          (let ((r (ghostherd-test--call calls "setNewConversations")))
            (should (equal (plist-get r :project) (funcall dir "a/sub/")))
            (should (equal (plist-get r :root) (ghostherd--project-name (funcall dir "a/"))))
            (should (equal (plist-get r :kind) "grok"))
            (let ((c (aref (plist-get r :conversations) 0)))
              (should (equal (plist-get c :path) (ghostherd-tests--tx "c1.jsonl")))
              (should (equal (plist-get c :title) "Here"))
              (should (equal (plist-get c :running) ""))
              (should (eq (plist-get c :fork) t))))
          (should (gethash (ghostherd-tests--tx "c1.jsonl") ghostherd-memory-page--resumable))
          (ghostherd-memory-page--handle '((op . "new-conversations") (project . "/nowhere/at/all/")))
          (should (equal (plist-get (ghostherd-test--call calls "setNewConversations") :why)
                         "No directory /nowhere/at/all/"))
          ;; b has no agent: started there all the same, and not shown.
          (ghostherd-memory-page--handle
           `((op . "agent-new") (project . ,(ghostherd--project-name (funcall dir "b/"))) (kind . "claude") (name . "")))
          (should (eq (car spawned) 'claude))
          (should (equal (plist-get (cdr spawned) :project) (funcall dir "b/")))
          (should (equal (plist-get (cdr spawned) :name) "claude-b"))
          (should (and (plist-member (cdr spawned) :display) (null (plist-get (cdr spawned) :display))))
          ;; Resumed from the form: in the worktree, of project a, not shown.
          (setq spawned nil calls nil)
          (ghostherd-memory-page--handle `((op . "resume") (source_path . ,(ghostherd-tests--tx "c1.jsonl")) (quiet . t)))
          (should (equal (plist-get (cdr spawned) :directory) (funcall dir "wt/")))
          (should (equal (plist-get (cdr spawned) :project) (funcall dir "a/")))
          (should (and (plist-member (cdr spawned) :display) (null (plist-get (cdr spawned) :display))))
          (should (string-prefix-p "Resumed as claude-" (ghostherd-test--call calls "flash")))
          ;; And the herd sent, for it to show on the page.
          (should (seq-find (lambda (c) (member (car c) '("renderHerd" "renderHerdAgain"))) calls)))))))

(ert-deftest ghostherd-test-worktree-main-checkout-asks-git-only-when-it-must ()
  "Resolving a directory to its project asks git only of a linked
worktree (or submodule), whose .git is a file: not of an ordinary
checkout's top, nor of a directory in no checkout -- a picker resolves
dozens of them."
  (ghostherd-tests--with-repo
    (let ((wt (concat base "wt/"))
          (plain (concat base "plain/"))
          (asked 0)
          (real (symbol-function 'call-process)))
      (make-directory plain t)
      (funcall git repo "worktree" "add" "-q" "-b" "feat/z" (directory-file-name wt))
      (cl-letf (((symbol-function 'call-process)
                 (lambda (&rest args) (cl-incf asked) (apply real args))))
        (should-not (ghostherd--worktree-main-checkout repo))
        (should-not (ghostherd--worktree-main-checkout plain))
        (should (= asked 0))
        (should (equal (directory-file-name (file-truename (ghostherd--worktree-main-checkout wt)))
                       (directory-file-name (file-truename repo))))
        (should (> asked 0))))))

;;; Review round 1 of the above (ask 8638958c)

(ert-deftest ghostherd-test-project-defaults-ask-nothing ()
  "The page reads a typed directory's defaults from a request: a project
whose dir-locals hold a risky value -- an `eval' -- must not raise the
prompt about it, which would hold the page out of sight; its safe kind
and args are read all the same."
  (ghostherd-tests--with-dir-locals
      "((nil . ((eval . (message \"risky\"))
                (ghostherd-project-kind . grok)
                (ghostherd-project-args . (\"--effort\" \"high\")))))"
    (let ((enable-local-variables t))
      (cl-letf (((symbol-function 'hack-local-variables-confirm)
                 (lambda (&rest _) (error "Asked about local variables"))))
        (should (equal (ghostherd-project-defaults dir) '(grok "--effort" "high")))))))

(ert-deftest ghostherd-test-new-looks-at-no-remote-directory ()
  "A remote directory -- a known root, or one typed in the page's box,
asked again at each pause -- is never looked at: no connection for a
picker."
  (ghostherd-test--with-page
    (let ((ghostherd--new-roots (make-hash-table :test 'equal))
          (ghostherd--new-sources (cons (float-time) nil)))
      (cl-letf (((symbol-function 'file-directory-p)
                 (lambda (f) (if (file-remote-p f) (error "Connected to %s" f) t)))
                ((symbol-function 'ghostherd--project-root) #'ignore))
        (should-not (ghostherd--new-root "/ssh:box:/srv/app/"))
        (ghostherd-memory-page--handle '((op . "new-conversations") (project . "/ssh:box:/srv/app/")))
        (let ((r (ghostherd-test--call calls "setNewConversations")))
          (should (equal (plist-get r :project) "/ssh:box:/srv/app/"))
          (should (string-match-p "remote" (plist-get r :why))))
        (should-not (ghostherd-test--call calls "showError"))
        (should-not requests)))))

(ert-deftest ghostherd-test-new-reads-once-for-callers-together ()
  "The page's form asks for its places and its project's conversations
together: while one read of the past conversations is in flight, the
other waits for it -- one read, one resolving of the directories -- and
both are answered by it, or by its failing."
  (let ((ghostherd--new-sources nil)
        (ghostherd--new-roots (make-hash-table :test 'equal))
        (ghostherd--new-waiting nil)
        (pending nil) (answered nil))
    (cl-letf (((symbol-function 'ghostherd--new-sidecar-p) (lambda () t))
              ((symbol-function 'ghostherd-memory-request-async)
               (lambda (method cb &optional _params err-cb)
                 (should (equal method "memory_list"))
                 (push (cons cb err-cb) pending))))
      (ghostherd--new-read-sources-async (lambda () (push 'places answered)))
      (ghostherd--new-read-sources-async (lambda () (push 'conversations answered)))
      (should (= (length pending) 1))
      (should-not answered)
      (funcall (car (car pending)) (list :sources (list (list :kind "transcript" :source_path "t"))))
      (should (equal (reverse answered) '(places conversations)))
      (should (equal (mapcar (lambda (s) (plist-get s :source_path)) (cdr ghostherd--new-sources)) '("t")))
      (should-not ghostherd--new-waiting)
      ;; Kept: asked again, answered at once.
      (ghostherd--new-read-sources-async (lambda () (push 'again answered)))
      (should (= (length pending) 1))
      (should (eq (car answered) 'again))
      ;; A read that fails answers its waiters too, with none.
      (setq ghostherd--new-sources nil pending nil answered nil)
      (ghostherd--new-read-sources-async (lambda () (push 'a answered)))
      (ghostherd--new-read-sources-async (lambda () (push 'b answered)))
      (funcall (cdr (car pending)) "no answer")
      (should (equal (reverse answered) '(a b)))
      (should-not ghostherd--new-waiting)
      (should (ghostherd--new-fresh-p)))))

(ert-deftest ghostherd-test-a-lead-is-a-whole-projects ()
  "Writing to a project's lead from a subdirectory or a worktree of it --
a room opened on what was typed -- reaches that project's lead, and
never starts a second lead on part of the project."
  (ghostherd-tests--with-leads
    (let ((told nil))
      (cl-letf (((symbol-function 'ghostherd--project-root)
                 (lambda (&optional d)
                   (and d (string-prefix-p (expand-file-name "~/p/") (file-name-as-directory (expand-file-name d)))
                        (expand-file-name "~/p/"))))
                ((symbol-function 'ghostherd-tell) (lambda (s text &rest _) (push (cons (ghostherd-session-name s) text) told))))
        (let ((lead (ghostherd-lead-say "~/p/web" "plan the login page")))
          (should (equal (ghostherd-session-name lead) "lead-p"))
          (should (equal (ghostherd--project-name (ghostherd-session-project lead)) "~/p/")))
        (puthash "lead-p" "lead" ghostherd--roles)
        (setq spawned nil)
        (should (equal (ghostherd-session-name (ghostherd-lead-say "~/p/web/src" "status?")) "lead-p"))
        (should-not spawned)
        (should (equal (car told) '("lead-p" . "status?")))))))

(ert-deftest ghostherd-test-new-says-whose-worktree-a-conversation-is-in ()
  "A past conversation in an agent's own worktree says so: resumed, it
would be a second agent in that agent's checkout."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-places
      (let ((ghostherd--worktrees (make-hash-table :test 'equal))
            (ghostherd--new-sources
             (cons (float-time)
                   (list (ghostherd-tests--source "w.jsonl" "claude" "w1" (funcall dir "wt/") 60 "Its worktree")
                         (ghostherd-tests--source "m.jsonl" "claude" "m1" (funcall dir "a/") 90 "Main")))))
        (ghostherd-tests--session :name "claude-wt" :kind 'claude :project (funcall dir "a/") :state 'working)
        (puthash "claude-wt" (list :path (funcall dir "wt/") :branch "wt" :base "main") ghostherd--worktrees)
        (cl-letf (((symbol-function 'ghostherd--conversation-gone-p) #'ignore))
          (let ((convs (ghostherd-new-conversations (funcall dir "a/"))))
            (should (equal (ghostherd-session-name (plist-get (car convs) :occupied)) "claude-wt"))
            (should (string-match-p "claude-wt works in that directory"
                                    (ghostherd-new-conversation-note (car convs) (funcall dir "a/"))))
            (should-not (plist-get (cadr convs) :occupied))))))))

(ert-deftest ghostherd-test-herd-page-watch ()
  "The herd is sent while the tab is on screen, at once on a change --
once for the changes that come together."
  (ghostherd-test--with-herd
    (unwind-protect
        (progn
          (ghostherd-memory-page--handle '((op . "herd-watch")))
          (should (timerp ghostherd-memory-page--herd-timer))
          (setq calls nil)
          (ghostherd-memory-page--on-herd-change nil)
          (ghostherd-memory-page--on-herd-change nil)
          (should (timerp ghostherd-memory-page--herd-soon))
          (timer-event-handler ghostherd-memory-page--herd-soon)
          (should (equal (mapcar #'car calls) '("renderHerd")))
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
      (when (timerp ghostherd-memory-page--herd-soon)
        (cancel-timer ghostherd-memory-page--herd-soon)
        (setq ghostherd-memory-page--herd-soon nil))
      (ghostherd-memory-page--herd-unwatch))))

(ert-deftest ghostherd-test-herd-page-is-sent-what-changed ()
  "The same herd is not sent twice: the page is told to draw again the
one it has.  A changed one is sent; and the whole of it to a page that
starts watching, which has none."
  (ghostherd-test--with-herd
    (unwind-protect
        (progn
          (ghostherd-memory-page--handle '((op . "herd-watch")))
          (setq calls nil)
          (ghostherd-memory-page--send-herd)
          (ghostherd-memory-page--send-herd)
          (should (equal (mapcar #'car calls) '("renderHerd" "renderHerdAgain")))
          (should-not (cdr (assoc "renderHerdAgain" calls)))
          (setf (ghostherd-session-state (gethash "claude-api" ghostherd--sessions)) 'working)
          (setq calls nil)
          (ghostherd-memory-page--send-herd)
          (should (equal (plist-get (aref (plist-get (ghostherd-test--call calls "renderHerd") :agents) 0) :state)
                         "working"))
          (ghostherd-memory-page--handle '((op . "herd-watch")))
          (setq calls nil)
          (ghostherd-memory-page--send-herd)
          (should (equal (mapcar #'car calls) '("renderHerd"))))
      (ghostherd-memory-page--herd-unwatch))))

(ert-deftest ghostherd-test-herd-page-opens-without-the-sidecar ()
  (ghostherd-test--with-page
    (setq reply (lambda (_m _p) '(:sources nil)))
    (let ((ghostherd-memory-page--pending '(:view "agents")))
      (ghostherd-memory-page--handle '((op . "ready"))))
    (should (equal (car calls) '("showView" . "agents")))))

(provide 'ghostherd-spawn-tests)
;;; ghostherd-spawn-tests.el ends here

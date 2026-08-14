;;; ghostherd-tests.el --- Tests for ghostherd  -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for ghostherd's pure helpers -- the parts that need no PTY.
;; The interactive spawn/kill paths still need the manual plan in
;; docs/testing.org; driving real ghostel terminals headlessly is what
;; Phase 1 deferred.
;;
;; Run:
;;
;;   emacs --batch -L . -l ghostherd.el -l ghostherd-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'ghostherd)

;;; Fixtures

;; ghostherd only `defvar's these so it can compile without ghostel loaded,
;; which leaves them unbound; `buffer-local-value' on an unbound symbol
;; errors, so give them defaults the way a loaded ghostel would.
(defvar-local ghostel--title nil)
(defvar-local ghostel--process nil)
(defvar ghostel-progress-function nil)

(defmacro ghostherd-tests--with-herd (bindings &rest body)
  "Run BODY with an empty session registry.
BINDINGS are extra `let' bindings evaluated inside the clean registry."
  (declare (indent 1) (debug t))
  `(let ((ghostherd--sessions (make-hash-table :test 'equal))
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

(provide 'ghostherd-tests)
;;; ghostherd-tests.el ends here

;;; ghostherd-tests.el --- Tests for ghostherd  -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for ghostherd's pure helpers -- the parts that need no PTY.
;; The interactive spawn/kill paths still need the manual plan in readme.org;
;; driving real ghostel terminals headlessly is what Phase 1 deferred.
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

;; NOTE: an older test asserted the candidate string itself contained the
;; kind and project.  That was true while everything was crammed into one
;; string; it is superseded by `ghostherd-test-candidate-is-just-the-name'
;; and `ghostherd-test-annotation-carries-detail'.

(provide 'ghostherd-tests)
;;; ghostherd-tests.el ends here

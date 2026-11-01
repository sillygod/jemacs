;;; ghostherd-sidebar-tests.el --- Tests for ghostherd-sidebar.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Part of ghostherd's tests; ghostherd-tests.el runs them all.  This
;; file alone:
;;
;;   emacs --batch --init-directory=~/.emacs.d/emacs-home/ \
;;         -L . -L ../xwapp -l ghostherd-sidebar-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ghostherd-test-helpers)

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

(provide 'ghostherd-sidebar-tests)
;;; ghostherd-sidebar-tests.el ends here

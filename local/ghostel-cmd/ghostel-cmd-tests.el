;;; ghostel-cmd-tests.el --- Tests for ghostel-cmd  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;   emacs --batch --init-directory=~/.emacs.d/emacs-home/ \
;;         -L ~/.emacs.d/local/ghostel-cmd \
;;         -l ghostel-cmd.el -l ghostel-cmd-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ghostel-cmd)

(defmacro ghostel-cmd-tests--with-store (&rest body)
  (declare (indent 0))
  `(let* ((dir (make-temp-file "ghostel-cmd-" t))
          (ghostel-cmd-save-file (expand-file-name "cmds.eld" dir))
          (ghostel-cmd--cache nil)
          (ghostel-cmd--cache-stamp nil)
          (ghostel-cmd--root (file-name-as-directory dir))
          (ghostel-cmd--caller-file (expand-file-name "foo.py" dir))
          (ghostel-cmd--caller-line 12)
          (default-directory dir))
     (unwind-protect
         (progn ,@body)
       (ignore-errors (delete-directory dir t)))))

(defmacro ghostel-cmd-tests--with-overlay (cmds &rest body)
  "Run BODY in the overlay buffer listing CMDS, shown in the selected window.
Keys go through the real keymaps with `execute-kbd-macro'."
  (declare (indent 1))
  `(ghostel-cmd-tests--with-store
     (let ((ghostel-cmd--query "")
           (ghostel-cmd--filtering nil)
           (ghostel-cmd--fill nil)
           (ghostel-cmd--help-visible nil))
       (ghostel-cmd--set-commands ,cmds ghostel-cmd--root)
       (let ((buf (ghostel-cmd--prepare-buffer)))
         (unwind-protect
             (progn
               (set-window-buffer (selected-window) buf)
               (with-current-buffer buf ,@body))
           (kill-buffer buf))))))

(ert-deftest ghostel-cmd-test-extract-vars-order-and-defaults ()
  (let ((ghostel-cmd--caller-file nil)
        (ghostel-cmd--caller-line nil))
    (should (equal (ghostel-cmd--extract-variables
                    "pytest {{file}} -k {{expr:all}}" "/tmp/proj/")
                   '(("file" . nil)
                     ("expr" . "all"))))))

(ert-deftest ghostel-cmd-test-builtin-file-and-root ()
  (ghostel-cmd-tests--with-store
    (should (equal (ghostel-cmd--builtin-var "root" ghostel-cmd--root)
                   (directory-file-name ghostel-cmd--root)))
    (should (equal (ghostel-cmd--builtin-var "file" ghostel-cmd--root)
                   "foo.py"))
    (should (equal (ghostel-cmd--builtin-var "line" ghostel-cmd--root)
                   "12"))
    (should (equal (ghostel-cmd--builtin-var "project-name" ghostel-cmd--root)
                   (file-name-nondirectory
                    (directory-file-name ghostel-cmd--root))))))

(ert-deftest ghostel-cmd-test-apply-replaces-all-occurrences ()
  (should (equal (ghostel-cmd--apply
                  "echo {{name:x}} && {{name}}"
                  '(("name" . "hi")))
                 "echo hi && hi")))

(ert-deftest ghostel-cmd-test-preview-uses-current-input ()
  (should (equal (ghostel-cmd--preview
                  "pytest {{file}} -k {{expr}}"
                  "/tmp/"
                  '(("file" . "a.py"))
                  "expr" "foo")
                 "pytest a.py -k foo")))

(ert-deftest ghostel-cmd-test-persist-roundtrip ()
  (ghostel-cmd-tests--with-store
    (ghostel-cmd--set-commands
     (list (list :name "test" :cmd "pytest {{file}}" :desc "run"))
     ghostel-cmd--root)
    (setq ghostel-cmd--cache nil
          ghostel-cmd--cache-stamp nil)
    (let ((got (car (ghostel-cmd--get-commands ghostel-cmd--root))))
      (should (equal (plist-get got :name) "test"))
      (should (equal (plist-get got :cmd) "pytest {{file}}")))))

;; An outside edit (`o', another Emacs) stamps the file 10s and 20s on,
;; so its mtime differs from ours even on a coarse clock.
(defun ghostel-cmd-tests--write-outside (form seconds)
  (with-temp-file ghostel-cmd-save-file
    (prin1 form (current-buffer)))
  (set-file-times ghostel-cmd-save-file (time-add (current-time) seconds)))

(defun ghostel-cmd-tests--names (root)
  (mapcar (lambda (c) (plist-get c :name)) (ghostel-cmd--get-commands root)))

(ert-deftest ghostel-cmd-test-outside-edit-is-read-and-kept ()
  (ghostel-cmd-tests--with-store
    (let ((root ghostel-cmd--root))
      (ghostel-cmd--set-commands (list (list :name "a" :cmd "echo a")) root)
      (should (equal (ghostel-cmd-tests--names root) '("a")))
      (ghostel-cmd-tests--write-outside
       `((,root (:name "a" :cmd "echo a") (:name "b" :cmd "echo b"))
         ("/other/" (:name "o" :cmd "echo o")))
       10)
      (should (equal (ghostel-cmd-tests--names root) '("a" "b")))
      ;; Edited again after our last read: our next save builds on it.
      (ghostel-cmd-tests--write-outside
       `((,root (:name "a" :cmd "echo a") (:name "c" :cmd "echo c"))
         ("/other/" (:name "o" :cmd "echo o")))
       20)
      (ghostel-cmd--update-commands
       (lambda (cmds) (append cmds (list (list :name "d" :cmd "echo d"))))
       root)
      (setq ghostel-cmd--cache nil
            ghostel-cmd--cache-stamp nil)
      (should (equal (ghostel-cmd-tests--names root) '("a" "c" "d")))
      (should (equal (ghostel-cmd-tests--names "/other/") '("o"))))))

(ert-deftest ghostel-cmd-test-unreadable-file-is-not-written-over ()
  (ghostel-cmd-tests--with-store
    (let ((root ghostel-cmd--root)
          (broken "((\"/x/\" (:name")
          (inhibit-message t))
      (ghostel-cmd--set-commands (list (list :name "a" :cmd "echo a")) root)
      (with-temp-file ghostel-cmd-save-file
        (insert broken))
      (set-file-times ghostel-cmd-save-file (time-add (current-time) 10))
      (should (equal (ghostel-cmd-tests--names root) '("a")))
      (should-error (ghostel-cmd--set-commands nil root) :type 'user-error)
      (should (equal (with-temp-buffer
                       (insert-file-contents ghostel-cmd-save-file)
                       (buffer-string))
                     broken)))))

(ert-deftest ghostel-cmd-test-query-flex-and ()
  (let ((rec (list :name "pytest" :cmd "pytest {{file}}" :desc "unit")))
    (should (ghostel-cmd--query-matches-p rec ""))
    (should (ghostel-cmd--query-matches-p rec "pyt"))
    (should (ghostel-cmd--query-matches-p rec "pt file"))
    (should-not (ghostel-cmd--query-matches-p rec "deploy"))))

(ert-deftest ghostel-cmd-test-filter-types-every-printable-char ()
  "n and p go into the query like any other char; C-n / C-p move."
  (ghostel-cmd-tests--with-overlay
      (list (list :name "npm" :cmd "npm test")
            (list :name "python" :cmd "python -m pytest"))
    (ghostel-cmd--set-filtering t)
    (execute-kbd-macro "npm")
    (should (equal ghostel-cmd--query "npm"))
    (ghostel-cmd-filter-clear)
    (execute-kbd-macro (kbd "C-n"))
    (should (equal (tabulated-list-get-id) "python"))
    (execute-kbd-macro (kbd "C-p"))
    (should (equal (tabulated-list-get-id) "npm"))
    (execute-kbd-macro "python")
    (should (equal ghostel-cmd--query "python"))))

(ert-deftest ghostel-cmd-test-fill-commits-then-finishes ()
  (let ((ghostel-cmd--fill
         (list :record (list :name "t" :cmd "a {{x}} {{y}}")
               :vars '(("x" . "1") ("y" . "2"))
               :index 0
               :values nil
               :input "aa"
               :submit nil)))
    (should (ghostel-cmd--fill-commit-current))
    (should (equal (alist-get "x" (plist-get ghostel-cmd--fill :values)
                              nil nil #'equal)
                   "aa"))
    (should (= (plist-get ghostel-cmd--fill :index) 1))
    (plist-put ghostel-cmd--fill :input "")
    (should-not (ghostel-cmd--fill-commit-current))
    (should (equal (alist-get "y" (plist-get ghostel-cmd--fill :values)
                              nil nil #'equal)
                   "2"))))

(ert-deftest ghostel-cmd-test-help-text-lists-overlay-keys ()
  (let ((text (ghostel-cmd--help-text)))
    (should (string-match-p "RET" text))
    (should (string-match-p "Paste" text))
    (should (string-match-p "Live-narrow" text))))

(ert-deftest ghostel-cmd-test-display-reuses-other-window ()
  (let ((file-buf (get-buffer-create " *ghostel-cmd-file*"))
        (term-buf (get-buffer-create " *ghostel-cmd-term*")))
    (unwind-protect
        (progn
          (delete-other-windows)
          (set-window-buffer (selected-window) file-buf)
          (let ((other (split-window-right)))
            (set-window-buffer other term-buf)
            (select-window (get-buffer-window file-buf))
            (ghostel-cmd--display-terminal term-buf)
            (should (eq (window-buffer (selected-window)) term-buf))
            (should (eq (window-buffer (get-buffer-window file-buf))
                        file-buf))))
      (when (buffer-live-p file-buf) (kill-buffer file-buf))
      (when (buffer-live-p term-buf) (kill-buffer term-buf))
      (delete-other-windows))))

(ert-deftest ghostel-cmd-test-display-splits-instead-of-stealing ()
  (let ((file-buf (get-buffer-create " *ghostel-cmd-file2*"))
        (term-buf (get-buffer-create " *ghostel-cmd-term2*")))
    (unwind-protect
        (progn
          (delete-other-windows)
          (set-window-buffer (selected-window) file-buf)
          (ghostel-cmd--display-terminal term-buf)
          (should (eq (window-buffer (get-buffer-window file-buf))
                      file-buf))
          (should (get-buffer-window term-buf))
          (should-not (eq (get-buffer-window file-buf)
                          (get-buffer-window term-buf))))
      (when (buffer-live-p file-buf) (kill-buffer file-buf))
      (when (buffer-live-p term-buf) (kill-buffer term-buf))
      (delete-other-windows))))

(ert-deftest ghostel-cmd-test-alias-run-is-sidebar ()
  (should (eq (indirect-function 'ghostel-project-cmd-run)
              (indirect-function 'ghostel-cmd-sidebar))))

(provide 'ghostel-cmd-tests)
;;; ghostel-cmd-tests.el ends here

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
          (ghostel-cmd--root (file-name-as-directory dir))
          (ghostel-cmd--caller-file (expand-file-name "foo.py" dir))
          (ghostel-cmd--caller-line 12)
          (default-directory dir))
     (unwind-protect
         (progn ,@body)
       (ignore-errors (delete-directory dir t)))))

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
    (setq ghostel-cmd--cache nil)
    (let ((got (car (ghostel-cmd--get-commands ghostel-cmd--root))))
      (should (equal (plist-get got :name) "test"))
      (should (equal (plist-get got :cmd) "pytest {{file}}")))))

(ert-deftest ghostel-cmd-test-query-flex-and ()
  (let ((rec (list :name "pytest" :cmd "pytest {{file}}" :desc "unit")))
    (should (ghostel-cmd--query-matches-p rec ""))
    (should (ghostel-cmd--query-matches-p rec "pyt"))
    (should (ghostel-cmd--query-matches-p rec "pt file"))
    (should-not (ghostel-cmd--query-matches-p rec "deploy"))))

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

(ert-deftest ghostel-cmd-test-alias-run-is-sidebar ()
  (should (eq (indirect-function 'ghostel-project-cmd-run)
              (indirect-function 'ghostel-cmd-sidebar))))

(provide 'ghostel-cmd-tests)
;;; ghostel-cmd-tests.el ends here

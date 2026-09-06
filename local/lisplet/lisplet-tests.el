;;; lisplet-tests.el --- Tests for lisplet  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;   emacs --batch --init-directory=~/.emacs.d/emacs-home/ \
;;         -L ~/.emacs.d/local/lisplet \
;;         -l lisplet.el -l lisplet-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'lisplet)

(defmacro lisplet-tests--with-elisp (text point-at &rest body)
  "Insert TEXT in a temp elisp buffer, move to POINT-AT, run BODY.
POINT-AT is `open' (before first `('), `close' (after first `)'),
or an integer buffer position."
  (declare (indent 2))
  `(with-temp-buffer
     (emacs-lisp-mode)
     (insert ,text)
     (goto-char (point-min))
     (pcase ,point-at
       ('open (search-forward "(") (forward-char -1))
       ('close (search-forward ")") (forward-char 0))
       ((pred integerp) (goto-char ,point-at)))
     ,@body))

(ert-deftest lisplet-test-special-at-open-and-close ()
  (lisplet-tests--with-elisp "(foo)" 'open
    (should (lisplet--at-open-p))
    (should-not (lisplet--at-close-p))
    (should (lisplet--special-p)))
  (lisplet-tests--with-elisp "(foo)" 'close
    (should (lisplet--at-close-p))
    (should-not (lisplet--at-open-p))
    (should (lisplet--special-p)))
  (lisplet-tests--with-elisp "(foo)" 3
    (should-not (lisplet--special-p))))

(ert-deftest lisplet-test-not-special-in-string ()
  (lisplet-tests--with-elisp "\"(foo)\"" 2
    (should-not (lisplet--special-p))))

(ert-deftest lisplet-test-different-jumps ()
  (lisplet-tests--with-elisp "(foo bar)" 'open
    (lisplet-different)
    (should (lisplet--at-close-p))
    (should (= (point) (point-max)))
    (lisplet-different)
    (should (lisplet--at-open-p))
    (should (= (point) (point-min)))))

(ert-deftest lisplet-test-kill-whole-multiline-list ()
  (lisplet-tests--with-elisp "(foo\n bar)" 'open
    (lisplet-kill)
    (should (equal (string-trim (buffer-string)) ""))))

(ert-deftest lisplet-test-kill-sexps-on-same-line ()
  (lisplet-tests--with-elisp "(foo) (bar)" 'open
    (lisplet-kill)
    (should (equal (string-trim (buffer-string)) ""))))

(ert-deftest lisplet-test-kill-from-close-rest-of-line ()
  (lisplet-tests--with-elisp "(foo) (bar)" 'close
    (lisplet-kill)
    (should (equal (string-trim (buffer-string)) "(foo)"))))

(ert-deftest lisplet-test-maybe-self-inserts-when-not-special ()
  (lisplet-tests--with-elisp "(foo)" 3
    (let ((cmd (lisplet--insert-or-call #'lisplet-kill)))
      (setq last-command-event ?d)
      (funcall cmd 1)
      (should (equal (buffer-string) "(fdoo)")))))

(ert-deftest lisplet-test-eval-from-open ()
  (lisplet-tests--with-elisp "(+ 10 32)" 'open
    (let (got)
      (cl-letf (((symbol-function 'eval-last-sexp)
                 (lambda (_)
                   (setq got (buffer-substring-no-properties
                              (save-excursion (backward-sexp) (point))
                              (point)))
                   42)))
        (lisplet-eval)
        (should (equal got "(+ 10 32)"))))))

(ert-deftest lisplet-test-indent-pretty-prints ()
  (lisplet-tests--with-elisp "(foo\nbar)" 'open
    (lisplet-indent)
    (should (string-match-p "(foo" (buffer-string)))
    (should (string-match-p "bar)" (buffer-string)))))

(ert-deftest lisplet-test-maybe-kills-when-special ()
  (lisplet-tests--with-elisp "(foo)" 'open
    (let ((cmd (lisplet--insert-or-call #'lisplet-kill)))
      (funcall cmd 1)
      (should (equal (string-trim (buffer-string)) "")))))

(provide 'lisplet-tests)
;;; lisplet-tests.el ends here

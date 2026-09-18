;;; jwebkit-tests.el --- Tests for jwebkit  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;   emacs --batch --init-directory=~/.emacs.d/emacs-home/ \
;;         -L ~/.emacs.d/local/jwebkit \
;;         -l jwebkit.el -l jwebkit-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'json)
(require 'jwebkit)

(ert-deftest jwebkit-test-find-js-encodes-query ()
  (let ((js (jwebkit--find-js "foo\"bar" nil t)))
    (should (string-match-p "window\\.find" js))
    (should (string-match-p "foo\\\\\"bar" js))
    (should (string-match-p ",false,true)" js)))
  (should (string-match-p ",true,false)" (jwebkit--find-js "x" t)))
  (should (string-match-p ",false,true)" (jwebkit--find-js "x" nil t))))

(ert-deftest jwebkit-test-parse-labels-json-string ()
  (should (equal (jwebkit--parse-labels "[\"a\",\"s\",\"d\"]")
                 '("a" "s" "d"))))

(ert-deftest jwebkit-test-parse-labels-vector ()
  (should (equal (jwebkit--parse-labels ["a" "s"]) '("a" "s"))))

(ert-deftest jwebkit-test-parse-json-objects ()
  (let ((rows (jwebkit--parse-json
               "[{\"text\":\"Home\",\"href\":\"https://ex.com\"}]")))
    (should (equal (alist-get 'text (car rows)) "Home"))
    (should (equal (alist-get 'href (car rows)) "https://ex.com"))))

(ert-deftest jwebkit-test-ace-js-no-title-bridge ()
  (should-not (string-match-p "document\\.title" jwebkit--ace-js))
  (should-not (string-match-p "document\\.title" jwebkit--collect-links-js))
  (should (string-match-p "data-jw-ace" jwebkit--ace-js))
  (should (string-match-p "querySelectorAll" jwebkit--ace-js)))

(ert-deftest jwebkit-test-ace-inject-calls-with-keys ()
  (let ((js (let ((jwebkit-ace-keys "asdf"))
              (jwebkit--ace-inject-js))))
    (should (string-suffix-p "(\"asdf\");" js))
    (should (string-match-p "i%k" js))))

(ert-deftest jwebkit-test-ace-js-is-valid-enough ()
  (should (string-match-p "window\\.__jwAce" jwebkit--ace-js))
  (should (string-match-p "function lab" jwebkit--ace-js)))

(provide 'jwebkit-tests)
;;; jwebkit-tests.el ends here

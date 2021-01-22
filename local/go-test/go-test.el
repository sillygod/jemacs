;;; go-test.el --- summary -*- lexical-binding: t -*-

;; Author: Jing
;; Maintainer: Jing
;; Version: 0.0.1
;; Keywords: golang

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; Code:

(defgroup go-test nil
  "gotest integration"
  :group 'golang
  :prefix "go-test-")

(defvar go-test-verbose nil
  "Control verbosity of `go test` output.")

(defvar go-test-command "go test"
  "Go test command.  Default is `go test`.")

(defvar go-use-test-args ""
  "Additional arguements to be used for `go test`.")

(defvar go-test-buffer-name "*gotest*")

(defun go-run-tests (args)
  "Run golang's test.
According to the situation, ARGS will be function name,
package name or sth else."
  (interactive)
  (compilation-start (concat go-test-command " " (when go-test-verbose "-v ") args " " go-use-test-args)
                     nil
                     (lambda (n) go-test-buffer-name) nil))


;;;###autoload
(defun go-run-test-current-function ()
  "Run the test which current the cursor on.
example: go test -timeout 30s -run ^TestNslookup$ gop/jcert/pkg/domain"
  (interactive)
  (if (string-match "_test\\.go" buffer-file-name)
      (save-excursion
        (move-end-of-line nil)
        (re-search-backward
         "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?\\([[:alnum:]]+\\))[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)")
        (let ((test-func-name (match-string-no-properties 2)))
          (if (not (equal test-func-name nil))
              ;; (match-string-no-properties 2) => HandlerProvisionTestSuite
              (progn
                ;; start to search where the func be called.
                ;; It will be something like the following.
                ;; the search strategy is
                ;; func Testxxxx(xx) {
                ;;   suite.Run(t, new(HandlerProvisionTestSuite))
                ;; }
                (goto-char (point-max))
                (re-search-backward
                 (concat "[\s]?suite.Run(.*" test-func-name ")"))

                (re-search-backward
                 "^func[ ]+\\(Test[[:alnum:]_]+\\)(.*)")

                (message (concat "-run='" (match-string-no-properties 1) "' -testify.m='" test-func-name "'"))
                (go-run-tests
                 (concat "-run='" (match-string-no-properties 1) "' -testify.m='" test-func-name "'")))

            (go-run-tests
             (concat "-run='" (match-string-no-properties 3) "$'")))))

    (message "Must be in a _test.go file to run tests.")))


;;;###autoload
(defun go-run-packages-tests ()
  "Run the package's test files."
  (interactive)
  (go-run-tests ""))


;;;###autoload
(defun go-packages-gopkgs ()
    "Return a list of all go packages."
    (sort (process-lines "gopkgs") #'string<))



(provide 'go-test)

;;; go-test.el ends here

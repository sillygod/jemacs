;;; go-test-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "go-test" "go-test.el" (0 0 0 0))
;;; Generated autoloads from go-test.el

(autoload 'go-run-test-current-function "go-test" "\
Run the test which current the cursor on.
example: go test -timeout 30s -run ^TestNslookup$ gop/jcert/pkg/domain" t nil)

(autoload 'go-run-packages-tests "go-test" "\
Run the package's test files." t nil)

(autoload 'go-packages-gopkgs "go-test" "\
Return a list of all go packages." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "go-test" '("go-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; go-test-autoloads.el ends here

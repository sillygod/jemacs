;;; org-secrets-tests.el --- Tests for org-secrets  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Unit tests for parse / encode / filter / write.  The overlay and
;; pinentry paths are interactive.
;;
;; Run:
;;
;;   emacs --batch --init-directory=~/.emacs.d/emacs-home/ \
;;         -L ~/.emacs.d/local/org-secrets \
;;         -l org-secrets.el -l org-secrets-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'org-secrets)
(require 'cl-lib)

(defmacro org-secrets-tests--with-org (content &rest body)
  "Insert CONTENT into a temp org buffer and run BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (let ((org-element-use-cache nil)
           (org-startup-folded t)
           (org-startup-with-inline-images nil)
           (org-id-track-globally nil)
           (org-id-locations-file (make-temp-file "org-secrets-id")))
       (delay-mode-hooks (org-mode))
       (unwind-protect
           (progn ,@body)
         (ignore-errors (delete-file org-id-locations-file))))))

(defun org-secrets-tests--parse (content)
  "Parse CONTENT as a vault and return entries."
  (org-secrets-tests--with-org content
    (org-secrets--parse-buffer "/tmp/vault.org")))

(defconst org-secrets-tests--hello-b64
  (base64-encode-string (encode-coding-string "hello" 'utf-8) t))


;;; Encode / decode

(ert-deftest org-secrets-test-looks-like-base64 ()
  (should (org-secrets--looks-like-base64-p "aGVsbG8="))
  (should-not (org-secrets--looks-like-base64-p "short"))
  (should-not (org-secrets--looks-like-base64-p "not base64!!"))
  (should-not (org-secrets--looks-like-base64-p "")))

(ert-deftest org-secrets-test-decode-base64 ()
  (should (equal (org-secrets--decode-secret "aGVsbG8=" "base64") "hello"))
  (should (equal (org-secrets--decode-secret "aGVsbG8=") "hello")))

(ert-deftest org-secrets-test-decode-plain ()
  (should (equal (org-secrets--decode-secret "not-b64" "plain") "not-b64"))
  (should (equal (org-secrets--decode-secret "not-b64") "not-b64")))

(ert-deftest org-secrets-test-encode-roundtrip ()
  (let ((plain "päss/word:1!"))
    (should (equal (org-secrets--decode-secret
                    (org-secrets--encode-secret plain)
                    "base64")
                   plain))))


;;; Parse

(ert-deftest org-secrets-test-parse-full-entry ()
  (let* ((entries (org-secrets-tests--parse
                   (format "* GitHub :dev:\n:PROPERTIES:\n:DOMAIN: github.com\n:USER: jing\n:URL: https://github.com/login\n:SECRET: %s\n:ENCODING: base64\n:ID: abc-1\n:END:\nnotes here\n" org-secrets-tests--hello-b64)))
         (e (car entries)))
    (should (= 1 (length entries)))
    (should (equal (org-secrets-entry-title e) "GitHub"))
    (should (equal (org-secrets-entry-domain e) "github.com"))
    (should (equal (org-secrets-entry-user e) "jing"))
    (should (equal (org-secrets-entry-url e) "https://github.com/login"))
    (should (equal (org-secrets-entry-secret e) "hello"))
    (should (equal (org-secrets-entry-org-id e) "abc-1"))
    (should (member "dev" (org-secrets-entry-tags e)))
    (should (equal (org-secrets-entry-notes e) "notes here"))))

(ert-deftest org-secrets-test-parse-skips-category-parent ()
  (let ((entries (org-secrets-tests--parse
                  (concat "* Banks\n"
                          "** Example\n:PROPERTIES:\n:DOMAIN: example.com\n:SECRET: not-b64\n:END:\n"))))
    (should (= 1 (length entries)))
    (should (equal (org-secrets-entry-title (car entries)) "Example"))))

(ert-deftest org-secrets-test-parse-plain-secret-without-encoding ()
  (let ((entries (org-secrets-tests--parse
                  "* X\n:PROPERTIES:\n:DOMAIN: x.test\n:SECRET: not-b64\n:END:\n")))
    (should (equal (org-secrets-entry-secret (car entries)) "not-b64"))))

(ert-deftest org-secrets-test-notes-do-not-swallow-next-heading ()
  "A drawer-only entry must not leak the following heading into notes.
This is the Richart Life / Richart net bank case: no body, next
heading immediately (or after blank lines that FULL meta-skip
walks over)."
  (let ((entries (org-secrets-tests--parse
                  (concat "* Richart Life\n"
                          ":PROPERTIES:\n:DOMAIN: none\n:SECRET: aaa\n:END:\n"
                          "* Richart net bank\n"
                          ":PROPERTIES:\n:DOMAIN: none\n:SECRET: bbb\n:END:\n"))))
    (should (= 2 (length entries)))
    (should (equal (org-secrets-entry-title (nth 0 entries)) "Richart Life"))
    (should (equal (org-secrets-entry-title (nth 1 entries)) "Richart net bank"))
    (should-not (org-secrets-entry-notes (nth 0 entries)))
    (should-not (org-secrets-entry-notes (nth 1 entries)))))

(ert-deftest org-secrets-test-notes-ignore-blank-lines-before-sibling ()
  (let ((entries (org-secrets-tests--parse
                  (concat "* A\n:PROPERTIES:\n:SECRET: aaa\n:END:\n\n\n"
                          "* B\n:PROPERTIES:\n:SECRET: bbb\n:END:\n"))))
    (should-not (org-secrets-entry-notes (nth 0 entries)))
    (should-not (string-match-p "^\\*" (or (org-secrets-entry-notes (nth 0 entries)) "")))))

(ert-deftest org-secrets-test-notes-keep-body-not-sibling ()
  (let ((entries (org-secrets-tests--parse
                  (concat "* A\n:PROPERTIES:\n:SECRET: aaa\n:END:\n"
                          "hello notes\n\n"
                          "* B\n:PROPERTIES:\n:SECRET: bbb\n:END:\n"))))
    (should (equal (org-secrets-entry-notes (nth 0 entries)) "hello notes"))
    (should-not (string-match-p "B" (org-secrets-entry-notes (nth 0 entries))))))

(ert-deftest org-secrets-test-parse-legacy-base64-without-encoding ()
  (let ((entries (org-secrets-tests--parse
                  (format "* X\n:PROPERTIES:\n:DOMAIN: x.test\n:SECRET: %s\n:END:\n"
                          org-secrets-tests--hello-b64))))
    (should (equal (org-secrets-entry-secret (car entries)) "hello"))))


;;; Filter / preview / candidates

(ert-deftest org-secrets-test-haystack-excludes-secret ()
  (let ((e (org-secrets-entry-create
            :title "GitHub" :domain "github.com" :user "jing"
            :file "/tmp/work.org.gpg"
            :secret "s3cret" :notes "2FA on")))
    (let ((hay (org-secrets--entry-haystack e)))
      (should (string-match-p "GitHub" hay))
      (should (string-match-p "2FA" hay))
      (should (string-match-p "work.org.gpg" hay))
      (should-not (string-match-p "s3cret" hay)))))

(ert-deftest org-secrets-test-query-and-tokens ()
  (let ((e (org-secrets-entry-create
            :title "GitHub" :domain "github.com" :user "jing" :notes "work")))
    (should (org-secrets--query-matches-p e ""))
    (should (org-secrets--query-matches-p e "gthb"))
    (should (org-secrets--query-matches-p e "git jing"))
    (should-not (org-secrets--query-matches-p e "git missing"))))

(ert-deftest org-secrets-test-return-event-p ()
  (should (org-secrets--return-event-p 'return))
  (should (org-secrets--return-event-p 'kp-enter))
  (should (org-secrets--return-event-p ?\r))
  (should (org-secrets--return-event-p ?\n))
  (should-not (org-secrets--return-event-p ?g))
  (should-not (org-secrets--return-event-p ?\C-g)))

(ert-deftest org-secrets-test-help-text-lists-bindings ()
  (let ((text (org-secrets--sidebar-help-text)))
    (should (string-match-p "Copy password" text))
    (should (string-match-p "Live-narrow" text))
    (should (string-match-p "j, k" text))
    (should (string-match-p "RET" text))
    (should (string-match-p "Switch vault" text))
    (should-not (string-match-p "C-n" text))))

(ert-deftest org-secrets-test-preview-hides-secret ()
  (let* ((org-secrets--sidebar-target-width 80)
         (e (org-secrets-entry-create
             :title "GitHub" :domain "github.com" :user "jing"
             :file "/tmp/work.org.gpg"
             :secret "s3cret" :notes "hello notes"))
         (text (org-secrets--preview-body e)))
    (should-not (string-match-p "s3cret" text))
    (should (string-match-p "hello notes" text))
    (should (string-match-p "secret   set" text))
    (should (string-match-p "github.com" text))
    (should (string-match-p "vault    work.org.gpg" text))))

(ert-deftest org-secrets-test-candidate-alist-disambiguates ()
  (let* ((a (org-secrets-entry-create :uid "1" :org-id "id-a"
                                      :title "Same" :domain "a.test" :user "u"))
         (b (org-secrets-entry-create :uid "2" :org-id "id-b"
                                      :title "Same" :domain "a.test" :user "u"))
         (alist (org-secrets--candidate-alist (list a b))))
    (should (= 2 (length alist)))
    (should-not (equal (caar alist) (caadr alist)))
    (should (eq (cdr (assoc (caar alist) alist)) a))))


;;; Password generator

(ert-deftest org-secrets-test-random-password-length-and-charset ()
  (let* ((syms "!@#")
         (pw (org-secrets-random-password 24 syms))
         (ok (concat org-secrets--alnum syms)))
    (should (= 24 (length pw)))
    (should (cl-every (lambda (ch) (cl-find ch ok)) pw))))


;;; Vault files

(ert-deftest org-secrets-test-vault-files-primary-then-extra ()
  (let ((org-secrets-file "/tmp/learning.org.gpg")
        (org-secrets-extra-files '("/tmp/work.org.gpg" "/tmp/learning.org.gpg"))
        (org-secrets-vault-filter nil))
    (should (equal (org-secrets-vault-files)
                   '("/tmp/learning.org.gpg" "/tmp/work.org.gpg")))))

(ert-deftest org-secrets-test-vault-filter-restricts-active-files ()
  (let ((org-secrets-file "/tmp/learning.org.gpg")
        (org-secrets-extra-files '("/tmp/work.org.gpg"))
        (org-secrets-vault-filter "/tmp/work.org.gpg"))
    (should (equal (org-secrets--active-files)
                   (list (expand-file-name "/tmp/work.org.gpg"))))
    (should (equal (org-secrets--write-file) "/tmp/work.org.gpg"))))

(ert-deftest org-secrets-test-scope-label ()
  (let ((org-secrets-file "/tmp/learning.org.gpg")
        (org-secrets-extra-files nil)
        (org-secrets-vault-filter nil))
    (should (equal (org-secrets--scope-label) "learning.org.gpg")))
  (let ((org-secrets-file "/tmp/learning.org.gpg")
        (org-secrets-extra-files '("/tmp/work.org.gpg"))
        (org-secrets-vault-filter nil))
    (should (equal (org-secrets--scope-label) "all")))
  (let ((org-secrets-file "/tmp/learning.org.gpg")
        (org-secrets-extra-files '("/tmp/work.org.gpg"))
        (org-secrets-vault-filter "/tmp/work.org.gpg"))
    (should (equal (org-secrets--scope-label) "work.org.gpg"))))

(ert-deftest org-secrets-test-vault-choice-alist ()
  (let* ((a (expand-file-name "/tmp/a/learning.org.gpg"))
         (b (expand-file-name "/tmp/b/work.org.gpg"))
         (org-secrets-file a)
         (org-secrets-extra-files (list b)))
    (should (equal (org-secrets--vault-choice-alist)
                   (list (cons "all" nil)
                         (cons "learning.org.gpg" a)
                         (cons "work.org.gpg" b)))))
  (let* ((a (expand-file-name "/tmp/a/vault.org.gpg"))
         (b (expand-file-name "/tmp/b/vault.org.gpg"))
         (org-secrets-file a)
         (org-secrets-extra-files (list b))
         (alist (org-secrets--vault-choice-alist)))
    (should (equal (car alist) (cons "all" nil)))
    (should (= 3 (length alist)))
    (should-not (equal (car (nth 1 alist)) (car (nth 2 alist))))))

(ert-deftest org-secrets-test-footer-shows-scope ()
  (let ((org-secrets-file "/tmp/learning.org.gpg")
        (org-secrets-extra-files nil)
        (org-secrets-vault-filter nil)
        (org-secrets--sidebar-query "")
        (org-secrets--sidebar-filtering nil)
        (org-secrets--sidebar-help-visible nil))
    (let ((text (org-secrets--sidebar-footer)))
      (should (string-match-p "learning.org.gpg" text))
      (should (string-match-p "s vault" text)))))

(ert-deftest org-secrets-test-maybe-forget-respects-custom ()
  (let ((org-secrets--cache '(("/tmp/vault.org" now)))
        (org-secrets-forget-on-close t))
    (org-secrets--maybe-forget)
    (should-not org-secrets--cache))
  (let ((org-secrets--cache '(("/tmp/vault.org" now)))
        (org-secrets-forget-on-close nil))
    (org-secrets--maybe-forget)
    (should org-secrets--cache)))


;;; Write + parse roundtrip

(ert-deftest org-secrets-test-append-and-parse-roundtrip ()
  (let* ((dir (make-temp-file "org-secrets-vault" t))
         (file (expand-file-name "vault.org" dir))
         (org-id-track-globally nil)
         (org-id-locations-file (expand-file-name "ids" dir))
         (org-secrets--cache nil))
    (unwind-protect
        (progn
          (write-region "" nil file)
          (org-secrets--append-entry
           file
           (list :title "Roundtrip"
                 :domain "rt.test"
                 :user "jing"
                 :url "https://rt.test/login"
                 :secret "s3cret"
                 :notes "keep me"
                 :tags '("dev" "test")))
          (let* ((entries (org-secrets--parse-file file))
                 (e (car entries)))
            (should (= 1 (length entries)))
            (should (equal (org-secrets-entry-title e) "Roundtrip"))
            (should (equal (org-secrets-entry-domain e) "rt.test"))
            (should (equal (org-secrets-entry-user e) "jing"))
            (should (equal (org-secrets-entry-url e) "https://rt.test/login"))
            (should (equal (org-secrets-entry-secret e) "s3cret"))
            (should (equal (org-secrets-entry-notes e) "keep me"))
            (should (member "dev" (org-secrets-entry-tags e)))
            (should (org-secrets-entry-org-id e))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest org-secrets-test-copy-password-uses-kill-ring ()
  (let ((kill-ring nil)
        (e (org-secrets-entry-create :title "X" :secret "s3cret")))
    (org-secrets-copy-password e)
    (should (equal (car kill-ring) "s3cret"))))

(ert-deftest org-secrets-test-copy-login ()
  (let ((kill-ring nil)
        (e (org-secrets-entry-create :title "X" :user "jing" :secret "s3cret")))
    (org-secrets-copy-login e)
    (should (equal (car kill-ring) "jing\ts3cret"))))

(provide 'org-secrets-tests)
;;; org-secrets-tests.el ends here

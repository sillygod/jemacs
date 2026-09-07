;;; org-shelf-tests.el --- Tests for org-shelf  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;   emacs --batch --init-directory=~/.emacs.d/emacs-home/ \
;;         -L ~/.emacs.d/local/org-shelf \
;;         -l org-shelf.el -l org-shelf-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-id)
(require 'org-shelf)

(defmacro org-shelf-tests--with-dir (&rest body)
  "Run BODY in a temp notes directory with a clean index."
  (declare (indent 0))
  `(let* ((org-shelf-tests--root (make-temp-file "org-shelf-" t))
          (org-shelf-directory org-shelf-tests--root)
          (org-shelf-cache-file (expand-file-name "cache.eld"
                                                 org-shelf-tests--root))
          (org-shelf-dailies-directory "journal/")
          (org-id-track-globally nil)
          (org-id-locations-file
           (expand-file-name "ids" org-shelf-tests--root)))
     (unwind-protect
         (progn
           (org-shelf--reset-index)
           ,@body)
       (org-shelf--reset-index)
       (ignore-errors
         (delete-directory org-shelf-tests--root t)))))

(defun org-shelf-tests--write (relative content)
  "Write CONTENT to RELATIVE under `org-shelf-directory'.  Return path."
  (let ((file (expand-file-name relative org-shelf-directory)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert content))
    file))

(defun org-shelf-tests--note (id title &optional tags body)
  "Return a typical file-level note string."
  (concat ":PROPERTIES:\n:ID:       " id "\n:END:\n"
          "#+title: " title "\n"
          (if tags (concat "#+filetags: " tags "\n") "")
          (or body "")))

(defmacro org-shelf-tests--with-org (content &rest body)
  "Insert CONTENT into a temp org buffer and run BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (let ((org-element-use-cache nil)
           (org-startup-folded t)
           (org-startup-with-inline-images nil)
           (org-id-track-globally nil))
       (delay-mode-hooks (org-mode))
       ,@body)))


;;; Parse

(ert-deftest org-shelf-test-parse-file-node ()
  (org-shelf-tests--with-org
      (org-shelf-tests--note "AAA-1" "Alpha" ":book:")
    (let ((nodes (org-shelf--parse-buffer "/tmp/alpha.org")))
      (should (= 1 (length nodes)))
      (let ((n (car nodes)))
        (should (equal (org-shelf-node-id n) "AAA-1"))
        (should (equal (org-shelf-node-title n) "Alpha"))
        (should (equal (org-shelf-node-tags n) '("book")))
        (should (eq (org-shelf-node-level n) 0))
        (should (eq (org-shelf-node-point n) 1))))))

(ert-deftest org-shelf-test-parse-heading-node ()
  (org-shelf-tests--with-org
      (concat (org-shelf-tests--note "FILE-1" "Host" ":ai:")
              "\n* set the credentials\n"
              "  :PROPERTIES:\n"
              "  :ID:       HEAD-1\n"
              "  :END:\n")
    (let ((nodes (org-shelf--parse-buffer "/tmp/host.org")))
      (should (= 2 (length nodes)))
      (let ((h (nth 1 nodes)))
        (should (equal (org-shelf-node-id h) "HEAD-1"))
        (should (equal (org-shelf-node-title h) "set the credentials"))
        (should (equal (org-shelf-node-tags h) '("ai")))
        (should (eq (org-shelf-node-level h) 1))))))

(ert-deftest org-shelf-test-parse-title-case-insensitive ()
  (org-shelf-tests--with-org
      ":PROPERTIES:\n:ID: X\n:END:\n#+TITLE: Caps\n#+FILETAGS: :t:\n"
    (let ((n (car (org-shelf--parse-buffer "/tmp/c.org"))))
      (should (equal (org-shelf-node-title n) "Caps"))
      (should (equal (org-shelf-node-tags n) '("t"))))))

(ert-deftest org-shelf-test-heading-title-strips-todo-and-tags ()
  (should (equal (org-shelf--heading-title "TODO [#A] Hello world :tag:x:")
                 "Hello world"))
  (should (equal (org-shelf--heading-tags "Hello world :tag:x:")
                 '("tag" "x"))))


;;; Slug / link

(ert-deftest org-shelf-test-slug-ascii-and-cjk ()
  (should (equal (org-shelf--slug "Hello World") "hello_world"))
  (should (equal (org-shelf--slug "用心於不交易") "用心於不交易"))
  (should (equal (org-shelf--slug "  foo__bar  ") "foo_bar")))

(ert-deftest org-shelf-test-link-format ()
  (let ((n (org-shelf-node--create :id "ID-9" :title "T")))
    (should (equal (org-shelf--link n) "[[id:ID-9][T]]"))))


;;; Index / refresh / cache

(ert-deftest org-shelf-test-refresh-indexes-dir ()
  (org-shelf-tests--with-dir
    (org-shelf-tests--write "a.org" (org-shelf-tests--note "A" "Alpha" ":book:"))
    (org-shelf-tests--write "b.org" (org-shelf-tests--note "B" "Beta"))
    (org-shelf-refresh)
    (should (= 2 (length (org-shelf--nodes))))
    (should (equal (org-shelf-node-title (org-shelf--node-by-id "A")) "Alpha"))
    (should (equal (org-shelf-node-tags (org-shelf--node-by-id "A")) '("book")))))

(ert-deftest org-shelf-test-cache-roundtrip ()
  (org-shelf-tests--with-dir
    (org-shelf-tests--write "a.org" (org-shelf-tests--note "A" "Alpha"))
    (org-shelf-refresh)
    (should (file-exists-p org-shelf-cache-file))
    (org-shelf--reset-index)
    (should (org-shelf--load-cache))
    (should (equal (org-shelf-node-title (org-shelf--node-by-id "A")) "Alpha"))))

(ert-deftest org-shelf-test-sync-drops-deleted ()
  (org-shelf-tests--with-dir
    (let ((f (org-shelf-tests--write "gone.org"
                                    (org-shelf-tests--note "G" "Gone"))))
      (org-shelf-refresh)
      (should (org-shelf--node-by-id "G"))
      (delete-file f)
      (org-shelf-refresh t)
      (should-not (org-shelf--node-by-id "G")))))

(ert-deftest org-shelf-test-after-save-reindexes ()
  (org-shelf-tests--with-dir
    (let ((file (org-shelf-tests--write
                 "live.org" (org-shelf-tests--note "L" "Live"))))
      (org-shelf-refresh)
      (find-file file)
      (goto-char (point-min))
      (search-forward "Live")
      (replace-match "Lived")
      (save-buffer)
      (org-shelf--after-save)
      (kill-buffer)
      (should (equal (org-shelf-node-title (org-shelf--node-by-id "L"))
                     "Lived")))))


;;; Completion

(ert-deftest org-shelf-test-token-bounds-ascii-and-cjk ()
  (with-temp-buffer
    (insert "see Emacs here")
    (goto-char 8) ;; inside Emacs
    (should (equal (buffer-substring (car (org-shelf--token-bounds))
                                     (cdr (org-shelf--token-bounds)))
                   "Emacs")))
  (with-temp-buffer
    (insert "見 用心於不交易。")
    (goto-char (1- (point-max)))
    (should (equal (buffer-substring (car (org-shelf--token-bounds))
                                     (cdr (org-shelf--token-bounds)))
                   "用心於不交易"))))

(ert-deftest org-shelf-test-complete-everywhere-wraps-id-link ()
  (org-shelf-tests--with-dir
    (org-shelf-tests--write "a.org" (org-shelf-tests--note "A" "Alpha" ":book:"))
    (org-shelf-refresh)
    (org-shelf-tests--with-org "see Al"
      (goto-char (point-max))
      (let ((capf (org-shelf-complete-everywhere)))
        (should capf)
        (pcase-let ((`(,beg ,end ,table . ,plist) capf))
          (should (eq (plist-get plist :exclusive) 'no))
          (should (member "Alpha" (all-completions "Al" table)))
          (delete-region beg end)
          (insert "Alpha")
          (funcall (plist-get plist :exit-function) "Alpha" 'finished)
          (should (equal (buffer-string) "see [[id:A][Alpha]]")))))))

(ert-deftest org-shelf-test-complete-everywhere-skips-src-and-links ()
  (org-shelf-tests--with-dir
    (org-shelf-tests--write "a.org" (org-shelf-tests--note "A" "Alpha"))
    (org-shelf-refresh)
    (org-shelf-tests--with-org "#+begin_src emacs-lisp\nAlpha\n#+end_src\n"
      (goto-char (point-min))
      (search-forward "Alpha")
      (forward-char -1)
      (should-not (org-shelf-complete-everywhere)))
    (org-shelf-tests--with-org "see [[id:A][Alpha]]\n"
      (goto-char (point-min))
      (search-forward "Alpha")
      (forward-char -1)
      (should-not (org-shelf-complete-everywhere)))
    (org-shelf-tests--with-org "#+title: Alpha\n"
      (goto-char (point-min))
      (search-forward "Alpha")
      (should-not (org-shelf-complete-everywhere)))))

(ert-deftest org-shelf-test-complete-link-at-point ()
  (org-shelf-tests--with-dir
    (org-shelf-tests--write "a.org" (org-shelf-tests--note "A" "Alpha"))
    (org-shelf-refresh)
    (org-shelf-tests--with-org "[[Al]]"
      (goto-char 4)
      (let ((capf (org-shelf-complete-link-at-point)))
        (should capf)
        (pcase-let ((`(,beg ,end ,_table . ,plist) capf))
          (delete-region beg end)
          (insert "Alpha")
          (funcall (plist-get plist :exit-function) "Alpha" 'finished)
          (should (equal (buffer-string) "[[id:A][Alpha]]")))))))

(ert-deftest org-shelf-test-complete-incomplete-bracket ()
  (org-shelf-tests--with-dir
    (org-shelf-tests--write "a.org" (org-shelf-tests--note "A" "Alpha"))
    (org-shelf-refresh)
    (org-shelf-tests--with-org "[[Al"
      (goto-char (point-max))
      (should-not (org-shelf-complete-everywhere))
      (let ((capf (org-shelf-complete-link-at-point)))
        (should capf)
        (pcase-let ((`(,beg ,end ,_table . ,plist) capf))
          (delete-region beg end)
          (insert "Alpha")
          (funcall (plist-get plist :exit-function) "Alpha" 'finished)
          (should (equal (buffer-string) "[[id:A][Alpha]]")))))))

(ert-deftest org-shelf-test-wrap-exit-ignores-non-final-status ()
  (org-shelf-tests--with-dir
    (org-shelf-tests--write "a.org" (org-shelf-tests--note "A" "Alpha"))
    (org-shelf-refresh)
    (with-temp-buffer
      (insert "Alpha")
      (org-shelf--wrap-exit "Alpha" 'exact)
      (should (equal (buffer-string) "Alpha")))))


;;; New / insert / tags / dailies

(ert-deftest org-shelf-test-new-writes-header ()
  (org-shelf-tests--with-dir
    (org-shelf--reset-index)
    (setq org-shelf--indexed-p t)
    (let ((node (org-shelf-new "A Book" 'book)))
      (should (org-shelf-node-id node))
      (should (file-exists-p (org-shelf-node-file node)))
      (with-temp-buffer
        (insert-file-contents (org-shelf-node-file node))
        (let ((s (buffer-string)))
          (should (string-match-p ":ID:" s))
          (should (string-match-p "#\\+title: A Book" s))
          (should (string-match-p "#\\+filetags: :book:" s))
          (should (string-match-p "author:" s)))))))

(ert-deftest org-shelf-test-insert-link ()
  (org-shelf-tests--with-dir
    (org-shelf-tests--write "a.org" (org-shelf-tests--note "A" "Alpha"))
    (org-shelf-refresh)
    (cl-letf (((symbol-function 'org-shelf--read-node)
               (lambda (&rest _) (org-shelf--node-by-id "A"))))
      (org-shelf-tests--with-org ""
        (org-shelf-insert)
        (should (equal (buffer-string) "[[id:A][Alpha]]"))))))

(ert-deftest org-shelf-test-tag-add-filetags ()
  (org-shelf-tests--with-dir
    (let ((file (org-shelf-tests--write
                 "a.org" (org-shelf-tests--note "A" "Alpha" ":book:"))))
      (org-shelf-refresh)
      (find-file file)
      (goto-char (point-min))
      (org-shelf-tag-add '("course"))
      (save-buffer)
      (kill-buffer)
      (with-temp-buffer
        (insert-file-contents file)
        (should (string-match-p "#\\+filetags: :book:course:"
                                (buffer-string))))
      (should (member "course"
                      (org-shelf-node-tags (org-shelf--node-by-id "A")))))))

(ert-deftest org-shelf-test-dailies-creates-journal ()
  (org-shelf-tests--with-dir
    (setq org-shelf--indexed-p t)
    (let* ((time (encode-time 0 0 12 6 9 2026))
           (file (org-shelf--dailies-ensure time)))
      (should (file-exists-p file))
      (should (string-suffix-p "journal/2026-09-06.org" file))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((s (buffer-string)))
          (should (string-match-p "#\\+title: 2026-09-06" s))
          (should (string-match-p "#\\+filetags: :daily:" s)))))))

(ert-deftest org-shelf-test-dailies-capture-inserts-heading ()
  (org-shelf-tests--with-dir
    (setq org-shelf--indexed-p t)
    (let ((time (encode-time 0 15 8 6 9 2026)))
      (org-shelf-dailies-capture-today time)
      (should (string-match-p "^\\* 08:15 "
                              (buffer-string)))
      (kill-buffer))))


;;; Backlinks

(ert-deftest org-shelf-test-backlinks-finds-id-refs ()
  (org-shelf-tests--with-dir
    (org-shelf-tests--write "src.org" (org-shelf-tests--note "S" "Source"))
    (org-shelf-tests--write
     "dst.org"
     (concat (org-shelf-tests--note "D" "Dest")
             "\nsee [[id:S][Source]]\n"))
    (org-shelf-refresh)
    (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
      (let ((hits (org-shelf--rg-id "S" org-shelf-directory)))
        (should (= 1 (length hits)))
        (should (string-suffix-p "dst.org" (nth 0 (car hits))))
        (should (string-match-p "id:S" (nth 2 (car hits))))))))

(provide 'org-shelf-tests)
;;; org-shelf-tests.el ends here

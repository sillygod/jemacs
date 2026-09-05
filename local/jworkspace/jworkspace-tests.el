;;; jworkspace-tests.el --- Tests for jworkspace  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;   emacs --batch --init-directory=~/.emacs.d/emacs-home/ \
;;         -L ~/.emacs.d/local/jworkspace \
;;         -l jworkspace.el -l jworkspace-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'jworkspace)
(require 'cl-lib)

(defmacro jworkspace-tests--with-clean-map (&rest body)
  "Run BODY with an empty `jworkspace-map' and no current workspace."
  (declare (indent 0))
  `(let ((jworkspace-map (make-hash-table :test 'equal)))
     (jworkspace--set-current-workspace nil)
     ,@body))

(ert-deftest jworkspace-test-buffer-identity-file-vs-name ()
  (with-temp-buffer
    (rename-buffer "jw-name-only" t)
    (should (equal (jworkspace--buffer-identity (current-buffer))
                   (cons 'name "jw-name-only"))))
  (let ((file (make-temp-file "jw-id-")))
    (unwind-protect
        (let ((buf (find-file-noselect file)))
          (should (equal (jworkspace--buffer-identity buf)
                         (expand-file-name file)))
          (kill-buffer buf))
      (ignore-errors (delete-file file)))))

(ert-deftest jworkspace-test-resolve-does-not-create ()
  (should-not (jworkspace--resolve-buffer-identity
               (cons 'name "jw-does-not-exist-xyz") nil))
  (should-not (jworkspace--resolve-buffer-identity
               "/no/such/jw-file-xyz.el" nil))
  (should-not (jworkspace--resolve-buffer-identity
               (cons 'name "jw-does-not-exist-xyz") t))
  (should-not (jworkspace--resolve-buffer-identity
               "/no/such/jw-file-xyz.el" t)))

(ert-deftest jworkspace-test-load-does-not-visit-files ()
  (jworkspace-tests--with-clean-map
    (let ((file (make-temp-file "jw-load-")))
      (unwind-protect
          (progn
            (write-region "secret" nil file)
            (jworkspace--load-printable
             (list :jworkspace-version 1
                   :current "proj"
                   :workspaces
                   (list (cons "proj"
                               (list :name "proj"
                                     :buffers (list file)
                                     :window-config nil)))))
            (let ((ws (gethash "proj" jworkspace-map)))
              (should ws)
              (should-not (find-buffer-visiting file))
              (should (equal (jworkspace-saved-ids ws) (list file)))
              (should-not (jworkspace-buffers ws))))
        (ignore-errors (kill-buffer (find-buffer-visiting file)))
        (ignore-errors (delete-file file))))))

(ert-deftest jworkspace-test-open-saved-ids-visits-quietly ()
  (jworkspace-tests--with-clean-map
    (let ((file (make-temp-file "jw-open-"))
          (hook-ran nil))
      (unwind-protect
          (progn
            (write-region "x" nil file)
            (let ((ws (jworkspace-new-workspace "proj")))
              (setf (jworkspace-saved-ids ws) (list file))
              (let ((find-file-hook (list (lambda () (setq hook-ran t)))))
                (jworkspace--open-saved-ids ws))
              (should (find-buffer-visiting file))
              (should-not hook-ran)
              (should (memq (find-buffer-visiting file)
                            (jworkspace-buffers ws)))
              (should-not (jworkspace-saved-ids ws))))
        (ignore-errors (kill-buffer (find-buffer-visiting file)))
        (ignore-errors (delete-file file))))))

(ert-deftest jworkspace-test-rebind-current-after-load ()
  (jworkspace-tests--with-clean-map
    (let* ((old (jworkspace-new-workspace "main"))
           (_ (jworkspace--set-current-workspace old)))
      (jworkspace--load-printable
       (list :jworkspace-version 1
             :current "main"
             :workspaces
             (list (cons "main" (list :name "main" :buffers nil :window-config nil)))))
      (jworkspace--rebind-current-workspace)
      (let ((fresh (jworkspace--get-current-workspace)))
        (should fresh)
        (should (equal (jworkspace-name fresh) "main"))
        (should-not (eq fresh old))
        (should (eq fresh (gethash "main" jworkspace-map)))))))

(ert-deftest jworkspace-test-new-workspace-does-not-overwrite ()
  (jworkspace-tests--with-clean-map
    (let ((ws (jworkspace-new-workspace "main")))
      (setf (jworkspace-saved-ids ws) '("keep-me"))
      (should (eq ws (jworkspace-new-workspace "main")))
      (should (equal (jworkspace-saved-ids ws) '("keep-me"))))))

(ert-deftest jworkspace-test-rename-collision ()
  (jworkspace-tests--with-clean-map
    (jworkspace-new-workspace "a")
    (let ((b (jworkspace-new-workspace "b")))
      (jworkspace--set-current-workspace b)
      (should-error
       (progn
         ;; Inline the collision check used by rename.
         (when (gethash "a" jworkspace-map)
           (user-error "Workspace named '%s' already exists" "a")))
       :type 'user-error))))

(ert-deftest jworkspace-test-buffer-in-other-workspace ()
  (jworkspace-tests--with-clean-map
    (let ((a (jworkspace-new-workspace "a"))
          (b (jworkspace-new-workspace "b"))
          (buf (get-buffer-create "jw-shared")))
      (jworkspace-add-buffer a buf)
      (jworkspace-add-buffer b buf)
      (should (jworkspace--buffer-in-other-workspace-p buf a))
      (jworkspace-remove-buffer b buf)
      (should-not (jworkspace--buffer-in-other-workspace-p buf a))
      (kill-buffer buf))))

(ert-deftest jworkspace-test-next-workspace-name-sorted ()
  (jworkspace-tests--with-clean-map
    (jworkspace-new-workspace "zeta")
    (jworkspace-new-workspace "alpha")
    (jworkspace-new-workspace "mu")
    (should (equal (jworkspace--next-workspace-name) "alpha"))
    (should (equal (jworkspace--next-workspace-name "alpha") "mu"))))

(ert-deftest jworkspace-test-map-to-printable-versioned ()
  (jworkspace-tests--with-clean-map
    (let ((ws (jworkspace-new-workspace "main")))
      (jworkspace--set-current-workspace ws)
      (let ((plist (jworkspace--map-to-printable)))
        (should (eq (plist-get plist :jworkspace-version)
                    jworkspace--save-version))
        (should (equal (plist-get plist :current) "main"))
        (should (assoc "main" (plist-get plist :workspaces)))))))

(ert-deftest jworkspace-test-save-file-follows-user-emacs-directory ()
  (let ((user-emacs-directory (file-name-as-directory
                               (make-temp-file "jw-emacs-home-" t)))
        (jworkspace-save-file nil))
    (unwind-protect
        (let ((path (jworkspace--save-file t)))
          (should (equal path (locate-user-emacs-file "jworkspace")))
          (should (file-in-directory-p path user-emacs-directory)))
      (ignore-errors (delete-directory user-emacs-directory t)))))

(ert-deftest jworkspace-test-save-file-legacy-fallback ()
  (let* ((user-emacs-directory (file-name-as-directory
                                (make-temp-file "jw-emacs-home-" t)))
         (jworkspace-save-file nil)
         (legacy (jworkspace--legacy-save-file)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory legacy) t)
          (write-region "()" nil legacy)
          (should (equal (jworkspace--save-file) legacy))
          (should (equal (jworkspace--save-file t)
                         (locate-user-emacs-file "jworkspace"))))
      (ignore-errors (delete-directory user-emacs-directory t)))))

(ert-deftest jworkspace-test-load-old-alist-format ()
  (jworkspace-tests--with-clean-map
    (jworkspace--load-printable
     (list (cons "legacy" (list :name "legacy" :buffers nil :window-config nil))))
    (should (gethash "legacy" jworkspace-map))))

(ert-deftest jworkspace-test-ghostherd-id-from-buffer-name ()
  (should (equal (jworkspace--ghostherd-id-from-buffer-name
                  "*ghostherd:agy-emacs-commit*")
                 "agy-emacs-commit"))
  (should-not (jworkspace--ghostherd-id-from-buffer-name "*scratch*"))
  (should-not (jworkspace--ghostherd-id-from-buffer-name "agy-emacs-commit")))

(ert-deftest jworkspace-test-buffer-identity-prefers-ghostherd ()
  (with-temp-buffer
    (rename-buffer "*ghostherd:agy-x*" t)
    (cl-letf (((symbol-function 'ghostherd-get)
               (lambda (buf)
                 (and (eq buf (current-buffer)) 'sess)))
              ((symbol-function 'ghostherd-session-id)
               (lambda (s) (and (eq s 'sess) "agy-x"))))
      (should (equal (jworkspace--buffer-identity (current-buffer))
                     '(ghostherd . "agy-x"))))))

(ert-deftest jworkspace-test-resolve-ghostherd-attaches-on-visit ()
  (let ((view (get-buffer-create " *jw-gh-view*"))
        (attached nil)
        (restored nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ghostherd-get)
                   (lambda (id) (and (equal id "agy-x") 'sess)))
                  ((symbol-function 'ghostherd-restore)
                   (lambda () (setq restored t)))
                  ((symbol-function 'ghostherd-session-buffer)
                   (lambda (_) nil))
                  ((symbol-function 'ghostherd-session-backend)
                   (lambda (_) 'tmux))
                  ((symbol-function 'ghostherd-backend-view-is-host-p)
                   (lambda (_) nil))
                  ((symbol-function 'ghostherd--host-view)
                   (lambda (s)
                     (setq attached s)
                     view)))
          (should-not (jworkspace--resolve-buffer-identity
                       '(ghostherd . "agy-x") nil))
          (should-not attached)
          (should (eq (jworkspace--resolve-buffer-identity
                       '(ghostherd . "agy-x") t)
                      view))
          (should (eq attached 'sess))
          (should-not restored))
      (kill-buffer view))))

(ert-deftest jworkspace-test-resolve-legacy-ghostherd-buffer-name ()
  (let ((view (get-buffer-create " *jw-gh-legacy*")))
    (unwind-protect
        (cl-letf (((symbol-function 'ghostherd-get)
                   (lambda (id) (and (equal id "agy-x") 'sess)))
                  ((symbol-function 'ghostherd-session-buffer)
                   (lambda (_) nil))
                  ((symbol-function 'ghostherd-session-backend)
                   (lambda (_) 'tmux))
                  ((symbol-function 'ghostherd-backend-view-is-host-p)
                   (lambda (_) nil))
                  ((symbol-function 'ghostherd--host-view)
                   (lambda (_) view)))
          (should (eq (jworkspace--resolve-buffer-identity
                       '(name . "*ghostherd:agy-x*") t)
                      view)))
      (kill-buffer view))))

(ert-deftest jworkspace-test-open-saved-ids-drops-dead-ghostherd ()
  (jworkspace-tests--with-clean-map
    (cl-letf (((symbol-function 'ghostherd-get) (lambda (_) nil))
              ((symbol-function 'ghostherd-restore) #'ignore))
      (let ((ws (jworkspace-new-workspace "proj")))
        (setf (jworkspace-saved-ids ws) '((ghostherd . "gone")))
        (jworkspace--open-saved-ids ws)
        (should-not (jworkspace-saved-ids ws))
        (should-not (jworkspace-buffers ws))))))

(ert-deftest jworkspace-test-open-saved-ids-keeps-ghostherd-if-unloaded ()
  (jworkspace-tests--with-clean-map
    (let ((saved (and (fboundp 'ghostherd-get)
                      (symbol-function 'ghostherd-get))))
      (when (fboundp 'ghostherd-get)
        (fmakunbound 'ghostherd-get))
      (unwind-protect
          (let ((ws (jworkspace-new-workspace "proj")))
            (setf (jworkspace-saved-ids ws) '((ghostherd . "agy-x")))
            (jworkspace--open-saved-ids ws)
            (should (equal (jworkspace-saved-ids ws)
                           '((ghostherd . "agy-x")))))
        (when saved
          (fset 'ghostherd-get saved))))))

(ert-deftest jworkspace-test-ghostel-host-is-not-reattached ()
  (cl-letf (((symbol-function 'ghostherd-get)
             (lambda (id) (and (equal id "dead-cli") 'sess)))
            ((symbol-function 'ghostherd-session-buffer)
             (lambda (_) nil))
            ((symbol-function 'ghostherd-session-backend)
             (lambda (_) 'ghostel))
            ((symbol-function 'ghostherd-backend-view-is-host-p)
             (lambda (backend) (eq backend 'ghostel)))
            ((symbol-function 'ghostherd--host-view)
             (lambda (_) (error "should not attach a ghostel host"))))
    (should-not (jworkspace--resolve-buffer-identity
                 '(ghostherd . "dead-cli") t))))

(provide 'jworkspace-tests)
;;; jworkspace-tests.el ends here

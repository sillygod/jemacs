;;; e2e-guard.el --- an e2e Emacs keeps off the user's own state  -*- lexical-binding: t; -*-

;; Loaded first by each e2e's Emacs (herd-room.el, apps.el ...).  The run
;; gives it a HOME of its own (appslib's `start'), so the herd's state --
;; ~/.ghostherd, its log and memory under `user-emacs-directory', the
;; CLIs' transcripts -- is the run's, not yours.  This records what would
;; still reach yours: a file name handler on your own home's
;; ~/.ghostherd, ~/.emacs.d (its emacs-home above all), ~/.claude,
;; ~/.grok and ~/.gemini (E2E_REAL_HOME), which notes each file looked
;; at, read or written there -- not the checkout under test, which may be
;; under them (a worktree in ~/.ghostherd/worktrees, or ~/.emacs.d
;; itself, less its emacs-home), nor the directories above it, which
;; finding its own true name passes -- in the run's real-touched.txt, and
;; does what was asked.  The run fails when that file holds any
;; (appslib's `stop').  Only Emacs's own file operations: a process it
;; runs (git, tmux) is not seen.  Nor are .dir-locals.el looked for:
;; none from outside the run shapes it.

(defvar e2e-guard-file
  (and (getenv "E2E_DIR") (expand-file-name "real-touched.txt" (getenv "E2E_DIR")))
  "Where what reached the user's own state is noted.")

(defvar e2e-guard-allowed
  (let* ((local (getenv "E2E_LOCAL"))
         (root (and local (file-name-directory (directory-file-name (expand-file-name local))))))
    (delq nil (list root (and root (file-truename root)))))
  "Under these, the code under test: not the user's state.")

(defvar e2e-guard-regexp
  (when-let* ((real (getenv "E2E_REAL_HOME"))
              ((not (string-empty-p real))))
    (concat "\\`" (regexp-quote (directory-file-name (expand-file-name real)))
            "/\\(?:\\.ghostherd\\|\\.emacs\\.d\\|\\.claude\\|\\.grok\\|\\.gemini\\)\\(?:/\\|\\'\\)"))
  "Your own state's paths, or nil when the run did not say your home.")

(defconst e2e-guard-ops
  '(insert-file-contents insert-file-contents-literally write-region file-exists-p file-readable-p
    file-writable-p file-executable-p file-directory-p file-regular-p file-symlink-p file-attributes
    directory-files directory-files-and-attributes file-name-all-completions make-directory
    make-directory-internal delete-file delete-directory rename-file copy-file make-symbolic-link
    add-name-to-file set-file-modes set-file-times file-modes file-newer-than-file-p insert-directory
    file-notify-add-watch lock-file file-locked-p)
  "The operations that reach a file: noted.  Not those that only make a name.")

;; The handler calls nothing that is autoloaded: loading it looks
;; through `load-path', and the checkout under test is on it, under one
;; of the paths handled -- the handler again, without end.

(defun e2e-guard--names (operation args)
  "The file names OPERATION, called with ARGS, reaches."
  (cond ((eq operation 'write-region) (list (nth 2 args)))
        ((memq operation '(rename-file copy-file make-symbolic-link add-name-to-file file-newer-than-file-p))
         (list (nth 0 args) (nth 1 args)))
        (t (list (car args)))))

(defvar e2e-guard-state
  (when-let* ((real (getenv "E2E_REAL_HOME"))
              ((not (string-empty-p real))))
    (file-name-as-directory (expand-file-name ".emacs.d/emacs-home" real)))
  "Your Emacs's own state: never the checkout's, though under it.")

(defun e2e-guard--allowed-p (file)
  "Non-nil when FILE is under the checkout under test -- not its
emacs-home -- or a directory above it."
  (let ((dirs e2e-guard-allowed) (dir (file-name-as-directory file)) found)
    (while (and dirs (not found))
      (setq found (or (string-prefix-p dir (car dirs))
                      (and (string-prefix-p (car dirs) file)
                           (not (and e2e-guard-state (string-prefix-p e2e-guard-state dir)))))
            dirs (cdr dirs)))
    found))

(defun e2e-guard-handler (operation &rest args)
  "Note OPERATION on a file of the user's own state; then do it."
  (when (and e2e-guard-file (memq operation e2e-guard-ops))
    (dolist (name (e2e-guard--names operation args))
      (when (and (stringp name)
                 (let ((full (expand-file-name name)))
                   (and (string-match-p e2e-guard-regexp full)
                        (not (e2e-guard--allowed-p full)))))
        (let ((inhibit-file-name-handlers (cons #'e2e-guard-handler inhibit-file-name-handlers))
              (inhibit-file-name-operation 'write-region))
          (write-region (format "%s %s\n" operation (expand-file-name name)) nil e2e-guard-file t 'silent)))))
  (let ((inhibit-file-name-handlers (cons #'e2e-guard-handler
                                          (and (eq inhibit-file-name-operation operation)
                                               inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))

(when e2e-guard-regexp
  (push (cons e2e-guard-regexp #'e2e-guard-handler) file-name-handler-alist))

(setq enable-dir-local-variables nil)

(provide 'e2e-guard)
;;; e2e-guard.el ends here

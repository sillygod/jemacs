;; (defvar-local projectile-tasks '((name1 . "ls")
;;                                  (name2 . "ls | wc -l")))

;; (defun my/projectile-run-tasks (&rest _)
;;   (interactive)
;;   (ivy-read "run tasks: "
;;             projectile-tasks
;;             :sort nil
;;             :require-match t
;;             :action (lambda (cand)
;;                       (let* ((cmd (cdr cand))
;;                              (default-directory (projectile-project-root)))
;;                         (async-shell-command cmd)))))

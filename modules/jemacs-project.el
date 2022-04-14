;;; mycraft --- Summary  -*- lexical-binding: t; -*-
;;; Copyright (C) 2020 mycraft maintainers
;;; Author: Jing
;;; package --- mycraft
;;; Commentary:

;;; Code:

(use-package perspective
  :diminish persp-mode
  :commands (persp-switch)
  :custom
  (persp-modestring-short t)
  :config
  (persp-mode))

(use-package projectile
  :defer 1
  :custom ((projectile-completion-system 'ivy))
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (add-to-list 'projectile-project-root-files-bottom-up "pyproject.toml")
  (projectile-mode +1))

(use-package counsel-projectile
  :after projectile
  :defer 1
  :config (counsel-projectile-mode))

(use-package vterm
  :defer t
  :custom
  (vterm-always-compile-module t)
  :init
  (setq vterm-timer-delay 0.01)
  (with-eval-after-load 'evil
    (evil-set-initial-state 'vterm-mode 'emacs))
  :config
  (define-key vterm-mode-map (kbd "<escape>") 'vterm-send-escape)
  (define-key vterm-mode-map (kbd "C-c C-z") 'vterm-send-C-z))

(use-package vterm-toggle
  :defer t)

(defun comment-or-uncomment-lines (&optional arg)
  (interactive "p")
  (evilnc-comment-or-uncomment-lines arg))

(defun counsel-jump-in-buffer ()
  "Jump in buffer with `counsel-imenu' or `counsel-org-goto' if in 'org-mode'."
  (interactive)
  (call-interactively
   (cond
    ((eq major-mode 'org-mode) 'counsel-org-goto)
    (t 'counsel-imenu))))

(defun project-run-vterm (&optional arg)
  "Invoke `vterm' in the project's root.

Switch to the project specific term buffer if it already exists.
Use a prefix argument ARG to indicate creation of a new process instead."
  (interactive "P")
  (let* ((project (projectile-ensure-project (projectile-project-root)))
         (buffer (projectile-generate-process-name "vterm" arg)))
    (unless (buffer-live-p (get-buffer buffer))
      (unless (require 'vterm nil 'noerror)
        (error "Package 'vterm' is not available"))
      (projectile-with-default-dir project
        (vterm-other-window buffer)))
    (pop-to-buffer buffer)))

(defun new-terminal ()
  "New a terminal in project root or the current directory."
  (interactive)
  (if (projectile-project-p)
      (project-run-vterm)
    (vterm-other-window)))

(defun avy-jump-url ()
  "Use avy to go to url in the buffer."
  (interactive)
  (avy-jump "https?://"))

(defun my-counsel-projectile-rg (&optional options)
  "Search the current project with rg and search under certarn directory
     if it's not in a project.

     OPTIONS, if non-nil, is a string containing additional options to
     be passed to rg. It is read from the minibuffer if the function
     is called with a prefix argument."
  (interactive)
  ;; change this to read a directory path
  (let* ((search-directory (if (projectile-project-p)
                               (projectile-project-root)
                             (read-directory-name "Start from directory: ")))
         (ivy--actions-list (copy-sequence ivy--actions-list))
         (ignored
          (mapconcat (lambda (i)
                       (concat "--glob !" (shell-quote-argument i)))
                     (append
                      (projectile--globally-ignored-file-suffixes-glob)
                      (projectile-ignored-files-rel)
                      (projectile-ignored-directories-rel))
                     " "))
         (counsel-rg-base-command
          (let ((counsel-ag-command counsel-rg-base-command))
            (counsel--format-ag-command ignored "%s")))
         (initial-input (cond
                         ((use-region-p) (buffer-substring (region-beginning) (region-end)))
                         ((and (boundp 'ahs-current-overlay)
                               (not (eq ahs-current-overlay nil))) (buffer-substring (overlay-start ahs-current-overlay)
                               (overlay-end ahs-current-overlay)))
                         (t nil))))

    (when (region-active-p)
      (deactivate-mark))

    (ivy-add-actions
     'counsel-rg
     counsel-projectile-rg-extra-actions)

    (when (= (prefix-numeric-value current-prefix-arg) 4)
      (setq current-prefix-arg '(16)))

    (counsel-rg initial-input
                search-directory
                options
                (projectile-prepend-project-name
                 (concat (car (if (listp counsel-rg-base-command)
                                  counsel-rg-base-command
                                (split-string counsel-rg-base-command)))
                         ": ")))))

(provide 'jemacs-project)
;;; jemacs-project.el ends here

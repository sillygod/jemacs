;;; mycraft --- Summary  -*- lexical-binding: t; -*-
;;; Copyright (C) 2020 mycraft maintainers
;;; Author: Jing
;;; package --- mycraft
;;; Commentary:

;;; Code:

(use-package git-messenger
  :defer t
  :custom
  (git-messenger:show-detail t)
  (git-messenger:use-magit-popup t))

(use-package magit
  :defer 2
  :custom
  ((magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
   (magit-repository-directories
    '(("~/Desktop/data_platform" . 1)
      ("~/Desktop/cdp-cache" . 1)
      ("~/Desktop/ansible-playground" . 1)
      ("~/Desktop/go-playground" . 1)
      ("~/Desktop/spacemacs-private" . 1)))))

(use-package forge
  :defer 2
  :after magit)

(defun get-pull-request-uri ()
  (interactive)
  (save-excursion
    (with-current-buffer (magit-process-buffer t)
      (goto-char (point-max))
      (re-search-backward "^remote:\s*\\(https?://.*\\)+?$")
      ;; it seems that match-string-no-properties will get nothing if
      ;; you switch buffer after re-search-backward
      (let ((uri (match-string-no-properties 1)))
        (message "copy pull request: %s to clipboard" uri)
        (kill-new uri)))))

(use-package ediff
  :defer t
  :after winner
  :hook
  (ediff-quit . winner-undo)
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally))

(use-package diff-hl
  :defer 1
  :init
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode))

(provide 'jemacs-version-control)
;;; jemacs-version-contorl.el ends here

;;; mycraft --- Summary  -*- lexical-binding: t; -*-
;;; Copyright (C) 2020 mycraft maintainers
;;; Author: Jing
;;; package --- mycraft
;;; Commentary:

;;; Code:

(use-package git-messenger
  :defer t
  :init
  (setq git-messenger:show-detail t)
  (setq git-messenger:use-magit-popup t))

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

(use-package ediff
  :defer t
  :after winner
  :hook
  (ediff-quit . winner-undo)
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally))

(provide 'jemacs-version-control)
;;; jemacs-version-contorl.el ends here

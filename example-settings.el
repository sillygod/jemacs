;;; mycraft --- Summary  -*- lexical-binding: t; -*-
;;; Copyright (C) 2020 mycraft maintainers
;;; Author: Jing
;;; package --- mycraft
;;; Commentary:

;;; Code:


(require 'jemacs-theme)
(require 'jemacs-unarrange)
(require 'jemacs-unarrange)
(require 'jemacs-python)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(with-eval-after-load 'evil
  (evil-define-key 'emacs 'global (kbd "M-b") 'buffer-operate/body)
  (evil-define-key 'emacs 'global (kbd "M-w") 'window-operate/body))

(use-package ielm
  :defer t
  :after evil
  :init
  (evil-define-key 'insert interior-emacs-lisp-mode-map (kbd "M-RET") #'ielm-return)
  (evil-define-key 'insert interior-emacs-lisp-mode-map (kbd "RET") #'electric-newline-and-maybe-indent)
  (evil-define-key 'insert inferior-emacs-lisp-mode-map (kbd "<up>") #'previous-line)
  (evil-define-key 'insert inferior-emacs-lisp-mode-map (kbd "<down>") #'next-line)
  (evil-define-key 'insert inferior-emacs-lisp-mode-map (kbd "M-<up>") #'comint-previous-input))

(provide 'settings)
;;; settings.el ends here

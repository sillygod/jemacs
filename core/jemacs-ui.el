;;; mycraft --- Summary  -*- lexical-binding: t; -*-
;;; Copyright (C) 2020 mycraft maintainers
;;; Author: Jing
;;; package --- mycraft
;;; Commentary:

;;; Code:


(setq frame-title-format "") ;; to disable show buffer name in the title bar
(setq scroll-conservatively 101) ;; to prevent recenter when cursor moves out of screen
(setq scroll-preserve-screen-position t)
(setq auto-window-vscroll nil)
(setq warning-minimum-level :error) ;; to supress the pop-up window of warning message

(setq help-window-select t)

(setq ring-bell-function 'ignore)

(when (eq (window-system) 'ns)
  (setq mac-command-modifier 'meta)
  ;; force to set command key to meta. In other emacs varaint like emacs-plus, the key is defined to =super=
  (setq frame-resize-pixelwise t)
  ;; make sure full maximized frame. It will not occupied the full screen in cocoa version.
  (setq ns-use-proxy-icon nil) ;; disable show icon in the title bar
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

(with-eval-after-load 'goto-addr
  (set-face-attribute 'link nil :foreground "#3f7c8f"))

(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'text-mode-hook 'hl-line-mode)

(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(windmove-do-window-select
                   winum-select-window-by-number))
  (advice-add command :after #'pulse-line))

(provide 'jemacs-ui)
;;; jemacs-ui.el ends here

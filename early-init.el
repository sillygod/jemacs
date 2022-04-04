;;; mycraft --- Summary  -*- lexical-binding: t; -*-
;;; Copyright (C) 2020 mycraft maintainers
;;; Author: Jing
;;; package --- mycraft
;;; Commentary:

;;; Code:

;; (toggle-debug-on-error) temporarily for debug usage

(setq gc-cons-threshold 64000000)
;;(add-hook 'after-init-hook #'(lambda ()
;; restore after startup
;;                               (setq gc-cons-threshold 800000)))

(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

(setq package-enable-at-startup nil)
(setq load-prefer-newer noninteractive)

(add-to-list 'default-frame-alist '(vertical-scroll-bars))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(left-fringe . 5))
(add-to-list 'default-frame-alist '(right-fringe . 5))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; (add-to-list 'default-frame-alist '(background-color . "#292b2e")) ;; this will be overwrite by doom-themes
(add-to-list 'default-frame-alist '(font . "Source Code Pro-15"))

(setq default-font-size 140)
(set-face-attribute 'fixed-pitch nil :font "Source Code Pro" :height default-font-size)
(set-face-attribute 'variable-pitch nil :font "Source Code Pro" :height default-font-size :weight 'regular)

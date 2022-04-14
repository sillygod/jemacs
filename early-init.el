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

(defvar home-dir (file-name-directory load-file-name))
(setq user-emacs-directory (expand-file-name "emacs-home" home-dir))
(unless (file-exists-p user-emacs-directory)
  (mkdir user-emacs-directory t))

(when (fboundp 'startup-redirect-eln-cache)
  (setcar native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
  (setq native-compile-target-directory (expand-file-name "eln-cache/" user-emacs-directory))
  (startup-redirect-eln-cache (expand-file-name "eln-cache/" user-emacs-directory)))

(defvar jemacs-config-file (expand-file-name "config.el" home-dir))
(defvar jemacs-settings-file (expand-file-name "settings.el" home-dir))
(when (file-exists-p jemacs-config-file)
  (load jemacs-config-file nil 'nomessage))

(add-to-list 'default-frame-alist '(vertical-scroll-bars))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(left-fringe . 5))
(add-to-list 'default-frame-alist '(right-fringe . 5))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; (add-to-list 'default-frame-alist '(background-color . "#292b2e")) ;; this will be overwrite by doom-themes
(add-to-list 'default-frame-alist '(font . "Source Code Pro-15"))

(defvar default-font-size 140)
(set-face-attribute 'fixed-pitch nil :font "Source Code Pro" :height default-font-size)
(set-face-attribute 'variable-pitch nil :font "Source Code Pro" :height default-font-size :weight 'regular)

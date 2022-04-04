;;; mycraft --- Summary  -*- lexical-binding: t; -*-
;;; Copyright (C) 2020 mycraft maintainers
;;; Author: Jing
;;; package --- mycraft
;;; Commentary:

;;; Code:

(defun do-something (process signal)
  (when (memq (process-status process) '(exit signal))
    (let* ((buf (process-buffer process))
           (content (with-current-buffer buf
                      (buffer-substring-no-properties (point-min) (point-max))))
           (path-from-shell (replace-regexp-in-string
                             "[ \t\n]*" ""
                             content)))

      (setenv "PATH" path-from-shell)
      (setq exec-path (split-string path-from-shell path-separator)))

    (shell-command-sentinel process signal)))


(add-hook 'emacs-startup-hook
          #'(lambda ()

              (let* ((display-buffer-alist '(("*my async shell command*" display-buffer-no-window)))
                     (output-buffer (generate-new-buffer "*my async shell command*"))
                     (proc (progn
                             (async-shell-command "$SHELL --login -c 'echo $PATH'" output-buffer)
                             (get-buffer-process output-buffer))))
                (if (process-live-p proc)
                    (set-process-sentinel proc #'do-something)
                  (message "No process running.")))))

(setq user-emacs-directory "~/.mycraft.d")
(defvar jemacs-config-file (expand-file-name "config.el" user-emacs-directory))
(defvar jemacs-settings-file (expand-file-name "settings.el" user-emacs-directory))

(when (file-exists-p jemacs-config-file)
  (load jemacs-config-file nil 'nomessage))

(setq package-user-dir (concat user-emacs-directory "/" "elpa"))
(setq mc/list-file (concat user-emacs-directory "/" "mc-lists.el"))

(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "core/" user-emacs-directory))

(require 'ui)
(require 'package)
(require 'core)

(when (file-exists-p jemacs-settings-file)
  (load jemacs-settings-file nil 'nomessage))

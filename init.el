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

(setq package-user-dir (concat user-emacs-directory "/" "elpa"))
(setq mc/list-file (concat user-emacs-directory "/" "mc-lists.el"))
(setq auto-save-list-file-prefix
      (concat
       (expand-file-name "auto-save-list" user-emacs-directory)
       "/.saves-"))

(add-to-list 'load-path (expand-file-name "modules/" home-dir))
(add-to-list 'load-path (expand-file-name "core/" home-dir))

(require 'jemacs-ui)
(require 'jemacs-package)
(require 'jemacs-core)

(when (file-exists-p jemacs-settings-file)
  (load jemacs-settings-file nil 'nomessage))

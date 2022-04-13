;;; mycraft --- Summary  -*- lexical-binding: t; -*-
;;; Copyright (C) 2020 mycraft maintainers
;;; Author: Jing
;;; package --- mycraft
;;; Commentary:

;;; Code:

(use-package doom-themes
  :init
  (load-theme 'doom-one t)
  (doom-themes-org-config)
  (set-face-attribute 'default nil :background "#292b2e")

  (with-eval-after-load 'org
    ;; change some ui
    (set-face-attribute 'org-link nil :font "Sarasa Mono SC" :height default-font-size :foreground "#3f7c8f")
    (set-face-attribute 'org-level-2 nil :foreground "#6cd4ac")
    (set-face-attribute 'org-level-3 nil :foreground "#219e57")
    (set-face-attribute 'org-table nil :font "Sarasa Mono SC")
    (set-face-attribute 'org-agenda-date nil :foreground "#41918b")
    (set-face-attribute 'org-agenda-date-today nil :foreground "#118844")
    (set-face-attribute 'org-agenda-date-weekend nil :foreground "#cc3333")))

(use-package doom-modeline
  :config
  ;; (setq persp-show-modestring nil) this will disable showing the persp name in the modeline
  (doom-modeline-mode 1)
  :custom
  (all-the-icons-scale-factor 1.1)
  (doom-modeline-height 12)
  (doom-modeline-persp-name nil))

(use-package all-the-icons
  :defer 0)

(provide 'jemacs-theme)
;;; jemacs-theme.el ends here

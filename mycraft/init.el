(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Initialize package sources
(setq package-user-dir "~/.mycraft.d/elpa")

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))


;; where the packages will be installed?
;; It seems the following variable can decide where the
;; packages to be installed.

;; prevent prompting in minibuffer, just quit the command
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t)


(use-package doom-themes
  :init (load-theme 'doom-gruvbox))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (progn
          (setq which-key-idle-delay 0.1)
          (which-key-mode 1))
  :diminish which-key-mode
  )

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))


(use-package swiper
  :ensure t
  :bind (("C-s" . swiper))
  )

;; what does ensure do?
;; The :ensure keyword causes the package(s) to be installed automatically if not already present on your system
;; this setting will globally enable ensure (setq use-package-always-ensure t)

(use-package ivy
  :ensure t
  :diminish
  :bind (:map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1)

  (setq ivy-initial-inputs-alist nil))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))


(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8d7684de9abb5a770fbfd72a14506d6b4add9a7d30942c6285f020d41d76e0fa" default))
 '(package-selected-packages '(ivy doom-modeline use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

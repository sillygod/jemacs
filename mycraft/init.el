;;; package --- mycraft
;;; Summary:
;;; Commentary:

;;; Code:


;; https://github.com/syl20bnr/spacemacs/blob/c7a103a772d808101d7635ec10f292ab9202d9ee/layers/%2Bdistributions/spacemacs-base/config.el

;;  goto-address-prog-mode
(fset 'yes-or-no-p 'y-or-n-p) ;; to simplify the yes or no input
(setq inhibit-startup-message t)
(setq column-number-mode t)
(setq make-backup-files nil)

;; this can make cursor in the help window at first when poping up a help window
(setq help-window-select t)
(setf epa-pinentry-mode 'loopback)
(setq epa-file-encrypt-to '("sillygod"))

;; https://stackoverflow.com/questions/6411121/how-to-make-emacs-use-my-bashrc-file
;; in order to make the shell to load source file
(setq shell-command-switch "-ic")


;; minimize some ui interface
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 5)

(toggle-frame-maximized)

;; https://github.com/tonsky/FiraCode
;; optional font
;; TODO: need a font customization

;; The following variable can decide where the
;; packages to be installed.
(setq package-user-dir "~/.mycraft.d/elpa")


;; Initialize package sources
;; Note: sometimes you may encouter an expired key when
;; downloading package. You need to fresh it.
;; There are many ways to do it. One of them is call list-package
;; Or delete the entire folder =elpa= make the emacs to redownload
;; all packages.
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))



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

;; enable link in comments can be click
(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; -- UI related

(add-hook 'prog-mode-hook 'hl-line-mode)

(use-package doom-themes
  :init (load-theme 'doom-one t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-parentheses
  :hook (prog-mode . highlight-parentheses-mode))

(use-package hl-todo
  :defer t
  :hook
  (text-mode . hl-todo-mode)
  (prog-mode . hl-todo-mode))

;; need some customization here.
(use-package diff-hl
  :config
  (global-diff-hl-mode)
  :defer t)

(use-package perspective
  :config
  (persp-mode))


(use-package which-key
  :diminish which-key-mode
  :init
  (setq which-key-idle-delay 0.05)
  (which-key-mode 1))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; ----------------------------------------------------------------


(use-package nginx-mode
  :defer t)

(use-package docker
  :defer t)

(use-package docker-tramp
  :defer t)

(use-package dockerfile-mode
  :defer t)

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper))
  )

;; TODO: search why there should append a suffix ='= for the mod
(use-package jinja2-mode
  :defer t
  :mode ("\\.j2\\'" . jinja2-mode))

(use-package js2-mode
  :after (rainbow-delimiters)
  :defer t
  :config
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (js2-minor-mode))

;; some customize functions
(defvar go-run-command "go run")
(defvar go-run-args ""
  "Additional arguments to by supplied to `go run` during runtime.")


(defun go-run-main ()
  (interactive)
  (shell-command
   (format (concat go-run-command " %s %s")
           (shell-quote-argument (or (file-remote-p (buffer-file-name (buffer-base-buffer)) 'localname)
                                     (buffer-file-name (buffer-base-buffer))))
           go-run-args)))


(defun kill-this-buffer (&optional arg)
  "Kill the current buffer.
ARG is an universal arg which will kill the window as well.
If the universal prefix argument is used then kill also the window."
  (interactive "P")
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (if (equal '(4) arg)
        (kill-buffer-and-window)
      (kill-buffer))))


(defun evil-smart-doc-lookup ()
  "Run documentation lookup command specific to the major mode.
Use command bound to `SPC m h h` if defined, otherwise fall back
to `evil-lookup'"
  (interactive)
  ;; (let ((binding (key-binding (kbd (concat "SPC" " mhh")))))
  (when (fboundp 'lsp-describe-thing-at-point)
    (lsp-describe-thing-at-point)
    (eivl-lookup)))

  ;; (let ((binding (global-key-binding (kbd (concat "SPC" " mhh")))))
  ;;   (print (key-binding (kbd (concat "SPC" " mhh"))))
  ;;   (if (commandp binding)
  ;; 	(call-interactively binding)
  ;;     (evil-lookup))))


(defun comment-or-uncomment-lines (&optional arg)
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line nil))
    (evilnc-comment-or-uncomment-lines arg)))


(defun counsel-jump-in-buffer ()
  "Jump in buffer with `counsel-imenu' or `counsel-org-goto' if in 'org-mode'."
  (interactive)
  (call-interactively
   (cond
    ((eq major-mode 'org-mode) 'counsel-org-goto)
    (t 'counsel-imenu))))

(defun avy-jump-url ()
  "Use avy to go to url in the buffer."
  (interactive)
  (avy-jump "https?://"))

(defun load-yasnippet ()
  "Ensure yasnippet is enbled."
  (unless yas-global-mode (yas-global-mode 1))
  (yas-minor-mode 1))

(defun ivy-yas ()
  "Lazy load ivy-yasnippet."
  (interactive)
  (load-yasnippet)
  (require 'ivy-yasnippet)
  (call-interactively 'ivy-yasnippet))

(defun toggle-flycheck-error-list ()
  "Toggle flycheck's error list window.
If the error list is visible, hide it.  Otherwise, show it."
  (interactive)
  (-if-let (window (flycheck-get-error-list-window))
      (quit-window nil window)
    (flycheck-list-errors)))


(defun toggle-maximize-buffer ()
  "Maximize buffer."
  (interactive)
  (save-excursion
    (if (and (= 1 (length (window-list)))
	     (assoc ?_ register-alist))
	(jump-to-register ?_)
      (progn
	(window-configuration-to-register ?_)
	(delete-other-windows)))))

(use-package yasnippet
  :config
  (yas-global-mode 1)
  (yas-minor-mode 1)
  (add-to-list 'yas-snippet-dirs "/Users/jing/Desktop/spacemacs-private/snippets"))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package ivy-yasnippet
  :after yaanippet)

(use-package winum
  :config
  (winum-mode))

;; TODO: check what does remap do
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package general
  :after (evil dired expand-region lsp-mode go-mode)
  :config
  ;; keysmaps override is to make general-define-key to be global scope
  ;; No need to set this one (evil-make-overriding-map dired-mode-map 'normal)

  (defconst leader-key "SPC")
  (defconst major-mode-leader-key "SPC m")

  (message "DEBUG: !! general init")

  (general-create-definer my-leader-keys
    :states '(normal visual motion emacs)
    :keymaps 'override
    :prefix leader-key)

  (general-create-definer my-local-leader-def
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix major-mode-leader-key)

  (general-create-definer my-local-leader-def-alias
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix ",")

  ;; TODO: create an alias =,= -> =SPC m=
  ;; (evil-define-key 'normal lsp-mode-map (kbd "SPC m") lsp-command-map)
  ;; maybe I can extract the key-map, rearrange it and assign

  (define-key evil-normal-state-map (kbd "K") 'evil-smart-doc-lookup)
  (evil-define-key 'normal go-mode-map (kbd "K") 'evil-smart-doc-lookup)

  ;; unbind some keybinding in the package 'evil-org
  (with-eval-after-load 'evil-org

    (evil-define-key 'motion org-agenda-mode-map (kbd "sc") nil)
    (evil-define-key 'motion org-agenda-mode-map (kbd "sr") nil)
    (evil-define-key 'motion org-agenda-mode-map (kbd "se") nil)
    (evil-define-key 'motion org-agenda-mode-map (kbd "st") nil)
    (evil-define-key 'motion org-agenda-mode-map (kbd "s^") nil)
    (evil-define-key 'motion org-agenda-mode-map (kbd "ss") nil)

    (evil-define-key 'motion org-agenda-mode-map (kbd "s") 'org-save-all-org-buffers))

  (with-eval-after-load 'org
    ;; define key open-thing-at-point with enter
    (evil-define-key 'normal org-mode-map (kbd "<return>") 'org-open-at-point)
    (evil-define-key 'normal prog-mode-map (kbd "<return>") 'org-open-at-point))


  ;; this is how spacemacs change the keybinding when edit org source code block
  ;; (with-eval-after-load 'org-src
  ;;   (spacemacs/set-leader-keys-for-minor-mode 'org-src-mode
  ;;     dotspacemacs-major-mode-leader-key 'org-edit-src-exit
  ;;     "c" 'org-edit-src-exit
  ;;     "a" 'org-edit-src-abort
  ;;     "k" 'org-edit-src-abort))


  (evil-define-key 'visual 'global
    (kbd "g y") 'copy-region-and-base64-decode
    (kbd "g e") 'copy-region-and-urlencode)



  (defun my-define-major-keys (&rest ARGS)
    ;; mapcar two definer with those key definitions)
   )

  ;; keybinding for go-mode

  (my-local-leader-def
    :states '(normal visual emacs)
    :keymaps 'go-mode-map
    "" '(:keymap lsp-command-map)
    "x" '(:ignore t :which-key "execute")
    "xx" '(go-run-main :which-key "go run")
    "e" '(gomacro-run :which-key "gomacro"))

  ;; TODO: change the prefix of +lsp

  ;; the following one doesn't work
  ;; (general-create-definer alias-local-def
  ;;   :states '(normal visual emacs)
  ;;   :keymaps 'override
  ;;   :wrapping my-local-leader-def
  ;;   :prefix ",")

  (my-local-leader-def
    :states '(normal visual emacs)
    :keymaps 'emacs-lisp-mode-map
    "" '(:ignore t :which-key "major mode")
    "e" '(:ignore t :which-key "eval")
    "eb" '(eval-buffer :which-key "eval buffer")
    "er" '(eval-region :which-key "eval region"))

  (my-local-leader-def
    :states '(normal visual emacs)
    :keymaps 'org-mode-map
    "" '(:ignore t :which-key "major mode")
    "a" 'org-agenda
    "," 'org-ctrl-c-ctrl-c
    "'" 'org-edit-special
    "s" '(:ignore t :which-key "schedule")
    "ss" '(org-schedule :which-key "org-schedule")
    "sd" '(org-deadline :which-key "org-deadline"))

  ;; (my-leader-keys
  ;;   "m" '(:ignore t :which-key "major mode")
  ;;   "my" 'org-store-link)

  (my-leader-keys
    "SPC" 'counsel-M-x
    "/" 'counsel-projectile-rg
    "v" 'er/expand-region
    "?" 'counsel-descbinds)

  ;; which-key-replacement-alist
  ;; change the content of the above variable
  (my-leader-keys
    "1" 'winum-select-window-1
    "2" '(winum-select-window-2 :which-key t)
    "3" '(winum-select-window-3 :which-key t)
    "4" '(winum-select-window-4 :which-key t)
    "5" '(winum-select-window-5 :which-key t)
    "6" '(winum-select-window-6 :which-key t)
    "7" '(winum-select-window-7 :which-key t)
    "8" '(winum-select-window-8 :which-key t)
    "9" '(winum-select-window-9 :which-key t))

  ;; need to find a way to add which-key hints
  ;; for the following window selection
  (push '(("\\(.*\\)1" . "winum-select-window-1") .
          ("\\11..9" . "select window 1..9"))
        which-key-replacement-alist)



  (my-leader-keys
    "j" '(:ignore t :which-key "jump")
    "jw" '(avy-goto-char-2 :which-key "avy goto ch2")
    "ju" '(avy-jump-url :which-key "goto url")
    "jl" '(avy-goto-line :which-key "goto line")
    "ji" '(counsel-jump-in-buffer :which-key "imenu"))

  (my-leader-keys
    "r" '(:ignore t :which-key "resume/register")
    "rl" '(ivy-resume :which-key "ivy-resume"))

  (my-leader-keys
    "b" '(:ignore t :which-key "buffer")
    "bb" '(counsel-projectile-switch-to-buffer :which-key "project-list-buffer")
    "bd" '(kill-this-buffer :which-key "kill-buffer")
    "bB" '(counsel-switch-buffer :which-key "list-buffer")
    "bn" '(next-buffer :which-key "next-buffer")
    "bp" '(previous-buffer :which-key "previous-buffer"))

  (my-leader-keys
    "c" '(:ignore t :which-key "comment/compile")
    "cl" '(comment-or-uncomment-lines :which-key "comment or uncomment"))

  (my-leader-keys
    "e" '(:ignore t :which-key "errors")
    "el" '(toggle-flycheck-error-list :which-key "comment or uncomment"))

  (my-leader-keys
    "i" '(:ignore t :which-key "insert")
    "is" '(ivy-yas :which-key "snippets"))

  (my-leader-keys
    "p" '(:ignore t :which-key "project")
    "pp" '(counsel-projectile-switch-project :which-key "switch project")
    "pf" '(counsel-projectile-find-file :which-key "find-file"))

  (my-leader-keys
    "s" '(:ignore t :which-key "search")
    "ss" '(swiper :which-key "swiper"))

  (my-leader-keys
    "g" '(:ignore t :which-key "git")
    "gs" '(magit-status :which-key "magit status"))


  (my-leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "ts" '(hydra-text-scale/body :which-key "scale text"))

  (my-leader-keys
    "w" '(:ignore t :which-key "windows")
    "wm" '(toggle-maximize-buffer :which-key "window maximized")
    "wM" '(toggle-frame-maximized :which-key "frame maximized")
    "wd" '(delete-window :which-key "delete window")
    "wh" '(evil-window-left :which-key "go to window left")
    "wl" '(evil-window-right :which-key "go to window right")
    "wk" '(evil-window-up :which-key "go to window up")
    "wj" '(evil-window-down :which-key "go to window down")
    "w/" '(evil-window-vsplit :which-key "split vertically")
    "w-" '(evil-window-split :which-key "split horizontally"))

  (my-leader-keys
    "f" '(:ignore t :which-key "files")
    "ff" '(counsel-find-file :which-key "find file")))


(use-package hydra)

;; what's the difference between hydra and transient
(defhydra hydra-text-scale (:timeout 8)
  "scale text"
  ("j" text-scale-increase "+")
  ("k" text-scale-decrease "-")
  ("0" ((lambda (inc) (text-scale-adjust inc)) 0) "reset")
  ("<escape>" nil "finished" :exit t))

;; https://github.com/emacs-evil/evil-collection
;; optional

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ;; Use visual line motions even outside of visual-line-mode buffers

  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :commands evilnc-comment-operator
  :init
  (define-key evil-normal-state-map "gc" 'evilnc-comment-operator))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package all-the-icons)

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
  (setq ivy-more-chars-alist '((t . 2))) ;; set the char limit when searching with ivy
  (setq ivy-initial-inputs-alist nil))

;; check-paren check whether there are lacks of the parentheses' pairs
(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq counsel-find-file-at-point t))


(use-package ivy-rich
  :init
  (ivy-rich-mode 1))


(use-package projectile
  :ensure t
  :custom ((projectile-completion-system 'ivy))
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package counsel-projectile
  :config (counsel-projectile-mode))


(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-magit
  :after magit)

;; the following is org's setup
;; we can check the org's version by the command =org-version=
(use-package org
  :ensure org-plus-contrib
  :pin org)

(use-package ox-reveal
  :after org)

(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

(defun org-mode-visual-fill ()
  (setq visual-fill-column-width 100)
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
  (global-visual-line-mode 1)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . org-mode-visual-fill))

;; Failed to install vterm: https://melpa.org/packages/vterm-20200926.1215.tar: Not found
;; package-refresh-contents
(use-package vterm)

(use-package avy
  :config
  (setq avy-background t))

;; company-mode setup

(use-package company
  :config
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.02)
  (define-key company-active-map (kbd "RET") 'company-complete-selection)
  (global-company-mode))


(use-package expand-region)

;; lsp configuation

(use-package go-mode
  :config
  (progn
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook 'gofmt-before-save)))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package json-mode)

;; TODO: Do I need to config the company backend?
(use-package yaml-mode
  :after (lsp-mode flycheck)
  :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
	 ("Procfile\\'" . yaml-mode))
  :init
  (add-hook 'yaml-mode-hook #'lsp)
  :config
  (when (listp flycheck-global-modes)
    (add-to-list 'flycheck-global-modes 'yaml-mode)))

(use-package gomacro-mode
  :hook (go-mode . gomacro-mode))

(use-package company-tabnine
  :config
  (with-eval-after-load 'company
    (add-to-list 'company-backends #'company-tabnine)
    (setq company-show-numbers t)
    (setq company-idle-delay 0.1)))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "SPC m") ;; this will only affect the display info of whichkey.
  :hook
  (go-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  (python-mode . lsp)
  (js-mode . lsp))

(use-package lsp-python-ms
  :after
  (lsp-mode)
  :init
  (setq lsp-python-ms-auto-install-server t))


(use-package lsp-ui
  :after flycheck
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-enable nil))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package dap-mode)

;; -----------------------------

;; by default, you need to press M-RET to add a
;; auto-numbering list
;; this will has some agenda mode binding..
;; TODO: find a way to remove?
(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
	    (lambda ()
	      (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (dot . t)
     (sql . t)
     (python . t)))

  ;; set org table's font
  ;; (set-face-font 'org-table " ")
  ;; I use the visual-column instead
  ;; (add-hook 'org-mode-hook 'toggle-word-wrap)

  ;; TODO: why my config will not auto add timestamp after making a toto item complete
  ;; customize this variable to read all lisp file under a certain directory

  (setq org-log-done 'time)
  (setq org-startup-truncated nil)
  (setq org-startup-with-inline-images t)
  (setq org-startup-with-inline-images t)
  (setq-default org-default-notes-file
		"~/Dropbox/myorgs/todo.org")

  (setq org-download-screenshot-method "screencapture -i %s")
  (setq-default org-download-image-dir "./img")
  (setq org-journal-dir "~/Dropbox/myorgs/journal/")
  (setq org-journal-file-type 'weekly)
  (setq org-journal-file-format "%Y-%m-%W.org")

  ;; (setq org-agenda-files (file-expand-wildcards "~/Dropbox/myorgs/*.org"))
  ;; In order to find the org files recursively
  (setq org-agenda-files (directory-files-recursively "~/Dropbox/myorgs/" "\\.org$"))

  ;; to config the org refile
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)

  ;; to allow creating a new heading when performing the org refile
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  ;; cool! some functions need to be enable
  ;; like <s press tab to complete org structure
  (setq org-modules '(ol-w3m
		      ol-bbdb
		      ol-bibtex
		      ol-docview
		      ol-gnus
		      ol-info
		      ol-irc
		      ol-mhe
		      ol-rmail
		      ol-eww
		      org-habit
		      ol-git-link
		      org-protocol
		      org-tempo))

  ;; customize the bullet symbol
  (custom-set-variables '(org-bullets-bullet-list (quote ("❐" "○" "﹅" "▶"))))
  (setq org-superstar-headline-bullets-list (quote ("❐" "○" "✎" "⚈")))
  (setq org-hide-leading-stars t)

  ;; to customize the org-capture template and clear the template before
  ;; we add the template in the list.
  (setq org-capture-templates nil)

  (setq org-todo-keywords
	'((sequence "TODO" "IN PROGRESS" "|" "DONE" "PRESERVE")))

  (setq org-todo-keyword-faces
	'(("TODO" . "#dc752f")
	  ("NO_NEWS" . "#cdb7b5")
	  ("ABANDON" . "#f2241f")
	  ("OFFERGET" . "#4f97d7")))


  ;; in order to group the templates we need to add the key-description
  ;; pair first or it will not work
  (add-to-list 'org-capture-templates '("i" "Inbox"))
  (add-to-list 'org-capture-templates
	       '("im" "Misc Inbox" entry
		 (file+headline "~/Dropbox/myorgs/inbox.org" "Misc")
		 "** %^{title} %?\n %(current-kill 0)\n\n"))

  (add-to-list 'org-capture-templates '("b" "Bookmarks"))
  (add-to-list 'org-capture-templates
	       '("bb" "Blogs bookmarks" entry
		 (file+headline "~/Dropbox/myorgs/bookmarks.org" "Blogs")
		 "** %^{title} %?\n %(current-kill 0)\n\n"))
  (add-to-list 'org-capture-templates
	       '("bs" "Speeches bookmarks" checkitem
		 (file+headline "~/Dropbox/myorgs/bookmarks.org" "Speeches")
		 "- [ ] [[%(current-kill 0)][%^{link description}]]\n"))

  (add-to-list 'org-capture-templates '("t" "Todos"))
  (add-to-list 'org-capture-templates
	       '("td" "a one day todo" entry
		 (file+headline "~/Dropbox/myorgs/todo.org" "一天內可以解決的事項")
		 "** TODO %^{title} %?\n SCHEDULED: %^t\n%? "))
  (add-to-list 'org-capture-templates
	       '("tw" "a week todo" entry
		 (file+headline "~/Dropbox/myorgs/todo.org" "一週內可以解決的事項")
		 "** TODO %^{title} %?\n SCHEDULED: %t\n"))
  (add-to-list 'org-capture-templates
	       '("tl" "a longterm todo" entry
		 (file+headline "~/Dropbox/myorgs/todo.org" "長期計畫")
		 "** TODO %^{title} %?\n SCHEDULED: %t\n")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8d7684de9abb5a770fbfd72a14506d6b4add9a7d30942c6285f020d41d76e0fa" default))
 '(package-selected-packages
   '(evil-org ivy-yasnippet org-plus-contrib evil-magit magit projectile hydra general ivy doom-modeline use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

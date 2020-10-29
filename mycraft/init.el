;;; mycraft --- Summary  -*- lexical-binding: t; -*-
;;; Copyright (C) 2020 mycraft maintainers
;;; Author: Jing
;;; package --- mycraft
;;; Commentary:

;;; Code:

;; I've put the variable in the beginning of the file
;; so it's no need to wirte (setq lexical-binding t)

;; https://github.com/syl20bnr/spacemacs/blob/c7a103a772d808101d7635ec10f292ab9202d9ee/layers/%2Bdistributions/spacemacs-base/config.el
;; tips for optimization https://github.com/nilcons/emacs-use-package-fast
;; (setq debug-on-error t) ;; temporarily for debug usage
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))

;; https://github.com/tonsky/FiraCode
;; optional font
;; TODO: search what does fixed-pitch mean?
(defvar default-font-size 140)
(set-face-attribute 'default nil :font "Source Code Pro" :height default-font-size)
(set-face-attribute 'default nil :background "#292b2e")
(set-face-attribute 'fixed-pitch nil :font "Source Code Pro" :height default-font-size)
(set-face-attribute 'variable-pitch nil :font "Source Code Pro" :height 140 :weight 'regular)


(fset 'yes-or-no-p 'y-or-n-p) ;; to simplify the yes or no input
(setq inhibit-startup-message t)
(setq inhibit-compacting-font-caches t) ;; for all-the-icon slow issue
(setq column-number-mode t)
(setq make-backup-files nil)
(setq-default indent-tabs-mode nil)
(setq xwidget-webkit-enable-plugins t)

;; NOTE: wow if you use setq here, it will not works
;; to research why
(setq-default tab-width 4)

(setq frame-title-format "") ;; to disable show buffer name in the title bar
;; (force-mode-line-update) to update the frame title

(setq scroll-conservatively 101) ;; to prevent recenter when cursor moves out of screen

(setq dired-dwim-target t)
;; make dired auto guess the path to rename
;; ex. open two buffer with dired mode

(setq ffap-machine-p-known 'reject)
;; NOTE: avoid ffap-guesser freeze when find-file-thing-at-point with something like url


(defun system-is-mac! ()
  (eq system-type 'darwin))

(defun system-is-linux! ()
  (eq system-type 'gnu/linux))

(defun system-is-windows ()
  (eq system-type 'windows-nt))

(when (string= system-type "darwin")
  "In macos, ls doesn't support --dired option"
  (setq dired-use-ls-dired nil))

;; this can make cursor in the help window at first when poping up a help window
(setq help-window-select t)
(setq epg-pinentry-mode 'loopback)
(setq epa-file-encrypt-to '("sillygod"))

;; https://stackoverflow.com/questions/6411121/how-to-make-emacs-use-my-bashrc-file
;; in order to make the shell to load source file
;; this will cause a side effect to slow down projectile-project-file
;; projectile-dir-files-alien
;; issue: https://github.com/syl20bnr/spacemacs/issues/4207
;; (setq shell-file-name "/bin/bash")
;; (setq shell-command-switch "-ic")
(setq shell-command-switch "-c")

;; activate natural title bar when the gui system is based on cocoa
(when (eq (window-system) 'ns)
  (setq mac-command-modifier 'meta)
  ;; force to set command key to meta. In other emacs varaint like emacs-plus, the key is defined to =super=
  (setq frame-resize-pixelwise t)
  ;; make sure full maximized frame. It will not occupied the full screen in cocoa version.
  (setq ns-use-proxy-icon nil) ;; disable show icon in the title bar
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;; minimize some ui interface
;; (scroll-bar-mode -1)
(add-to-list 'default-frame-alist '(vertical-scroll-bars))
;; (menu-bar-mode -1)
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
;; (tool-bar-mode -1)
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
;; (set-fringe-mode 5)
(add-to-list 'default-frame-alist '(left-fringe . 5))
(add-to-list 'default-frame-alist '(right-fringe . 5))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; disable word-wrap
(setq word-wrap nil)
;; (toggle-word-wrap 0)

(global-auto-revert-mode t)

;; The following variable can decide where the
;; packages to be installed.
(defconst my-home-dir "~/.mycraft.d")
(setq user-emacs-directory my-home-dir)
(setq package-user-dir (concat my-home-dir "/" "elpa"))
(setq mc/list-file (concat my-home-dir "/" "mc-lists.el"))

;; Initialize package sources
;; Note: sometimes you may encouter an expired key when
;; downloading package. You need to fresh it.
;; There are many ways to do it. One of them is call list-package
;; Or delete the entire folder =elpa= make the emacs to redownload
;; all packages.
(require 'subr-x)
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

(push (expand-file-name "~/Desktop/spacemacs-private/myemacs/local") load-path)

;; enable link in comments can be click and hightlight it
(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(add-hook 'prog-mode-hook '(lambda () (setq indent-tabs-mode nil)))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'dired-mode-hook 'auto-revert-mode)


;; TODO refactor this auto switch input method function into another file.
;; need to install the package im-select
(defcustom im-exec "/usr/local/bin/im-select"
  "The im executable binary path."
  :type 'string)

(defvar default-im "com.apple.keylayout.ABC"
  "Default English input method.")

(defun im-use-eng ()
  "Switch to english input method."
  (interactive)
  (cond ((string= system-type "darwin")
         (call-process-shell-command (concat im-exec " " default-im)))))

(defun im-remember ()
  "Remember the input method being used in insert mode."
  (interactive)
  (unless (boundp 'prev-im)
    (setq prev-im (substring (shell-command-to-string im-exec) 0 -1)))
  (cond ((string= system-type "darwin")
         (setq prev-im (substring (shell-command-to-string im-exec) 0 -1)))))

(defun im-use-prev ()
  "Change the input method to the previous one we remembered."
  (interactive)
  (unless (boundp 'prev-im)
    (setq prev-im (substring (shell-command-to-string im-exec) 0 -1)))
  (cond ((string= system-type "darwin")
         (if prev-im
             (call-process-shell-command (concat im-exec " " prev-im))
           (call-process-shell-command (concat im-exec " " default-im))))))

;; ---


;; By default, you will not go back to the original window layout when you
;; exit the ediff mode
(use-package winner
  :init
  (add-hook 'ediff-quit-hook 'winner-undo)
  :commands (winner-undo))

;; profiling
(use-package esup
  :defer t
  :init
  (setq esup-depth 0)
  ;; To use MELPA Stable use ":pin mepla-stable",
  :pin melpa)

;; -- UI related

(with-eval-after-load 'goto-addr
  (set-face-attribute 'link nil :foreground "#3f7c8f"))

;; (set-face-attribute 'hl-line nil :foreground "#72ba89")
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'text-mode-hook 'hl-line-mode)
;; jinja2 is the child of text-mode
;; (with-eval-after-load 'jinja2-mode
;;   (add-hook 'jinja2-mode-hook 'hl-line-mode))


(use-package rainbow-mode
  :defer t)

(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config)

  (with-eval-after-load 'org
    ;; change some ui
    (set-face-attribute 'org-link nil :foreground "#3f7c8f")
    (set-face-attribute 'org-level-2 nil :foreground "#6cd4ac")
    (set-face-attribute 'org-level-3 nil :foreground "#219e57")
    (set-face-attribute 'org-agenda-date nil :foreground "#41918b")
    (set-face-attribute 'org-agenda-date-today nil :foreground "#118844")
    (set-face-attribute 'org-agenda-date-weekend nil :foreground "#cc3333")))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-parentheses
  :hook (prog-mode . highlight-parentheses-mode))

(use-package devdocs
  :defer t
  :commands (devdocs-search)
  :load-path "~/Desktop/spacemacs-private/myemacs/local/devdocs")

(use-package hl-todo
  :defer t
  :hook
  ;; (text-mode . hl-todo-mode) text-mode is the parent of org-mode
  (prog-mode . hl-todo-mode))

;; need some customization here.
(use-package diff-hl
  :defer 1
  :init
  ;; (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  ;; I've check this. It seems setting post-refresh-hook is enough
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode))

(use-package perspective
  :diminish persp-mode
  :defer t
  :commands (persp-switch)
  :config
  (persp-mode))


(use-package which-key
  :diminish which-key-mode
  :init
  (setq which-key-idle-delay 0.05)
  (which-key-mode 1))

(use-package doom-modeline
  :init
  ;; (setq persp-show-modestring nil) this will disable showing the persp name in the modeline
  (doom-modeline-mode 1)
  (setq all-the-icons-scale-factor 1.1)
  :custom
  (doom-modeline-height 12)
  (doom-modeline-persp-name nil))

;; ----------------------------------------------------------------

;; dictionary packages
;; there two package are not usable right now.
(use-package define-word
  :defer t)

(use-package powerthesaurus
  :defer t)

;; read boooks
(use-package nov
  :defer t
  :mode ("\\.epub\\'" . nov-mode))

(use-package uuidgen
  :defer t)

(use-package nginx-mode
  :defer t)

(use-package docker
  :defer t)

(use-package docker-tramp
  :defer t)

(use-package dockerfile-mode
  :defer t)

(use-package emmet-mode
  :defer t
  :hook
  (html-mode . emmet-mode)
  (web-mode . emmet-mode))

(use-package web-mode
  :defer t
  :mode
  (("\\.html\\'" . web-mode)))


;; TODO: search why there should append a suffix ='= for the mod
;; the :config will be run after trigger autoload function
;; change the tab behavior of jinja2 mode by =indent-line-function
(use-package jinja2-mode
  :defer t
  :init
  (add-hook 'jinja2-mode-hook
            '(lambda ()
               (set (make-local-variable 'indent-line-function) 'insert-tab)))
  :mode ("\\.j2\\'" . jinja2-mode))

(use-package js2-mode
  :after (rainbow-delimiters)
  :defer t
  :config
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (js2-minor-mode))

;; some customize functions
(defvar go-test-command "go test")

(defvar go-run-command "go run")
(defvar go-run-args ""
  "Additional arguments to by supplied to `go run` during runtime.")

(defvar python-run-command "python")
(defvar python-run-args "")

;; TODO: rewrite this
(defun spacemacs/show-hide-helm-or-ivy-prompt-msg (msg sec)
  "Show a MSG at the helm or ivy prompt for SEC.
With Helm, remember the path, then restore it after SEC.
With Ivy, the path isn't editable, just remove the MSG after SEC."
  (run-at-time
   0 nil
   #'(lambda (msg sec)
       (let* ((prev-prompt-contents
               (buffer-substring (line-beginning-position)
                                 (line-end-position)))
              (prev-prompt-contents-p
               (not (string= prev-prompt-contents "")))
              (helmp (fboundp 'helm-mode)))
         (when prev-prompt-contents-p
           (delete-region (line-beginning-position)
                          (line-end-position)))
         (insert (propertize msg 'face 'warning))
         ;; stop checking for candidates
         ;; and update the helm prompt
         (when helmp (helm-suspend-update t))
         (sit-for sec)
         (delete-region (line-beginning-position)
                        (line-end-position))
         (when prev-prompt-contents-p
           (insert prev-prompt-contents)
           ;; start checking for candidates
           ;; and update the helm prompt
           (when helmp (helm-suspend-update nil)))))
   msg sec))

(defun google-search-action (x)
  "Search for X.
force to make new session without using the original session."
  (xwidget-webkit-browse-url
   (concat
    (nth 2 (assoc counsel-search-engine counsel-search-engines-alist))
    (url-hexify-string x)) t))


(defun google-search ()
  "Counsel-search with xwidget open url."
  (interactive)
  (require 'request)
  (require 'json)
  (let ((counsel-search-engine 'google))
    (ivy-read "search: "
              #'counsel-search-function
              :action #'google-search-action
              :dynamic-collection t
              :caller 'google-search)))

(defun open-with-xwidget-action (x)
  (xwidget-webkit-browse-url
   (url-encode-url (concat
                    "file://"
                    (expand-file-name x ivy--directory))) t))


;; this is based on the counsel.el
(with-eval-after-load 'counsel
  (defun open-with-xwidget (&optional initial-input)
    "Open file with xwidget browse url."
    (interactive)
    (counsel--find-file-1 "Find file: "
                          initial-input
                          #'open-with-xwidget-action
                          'open-with-xwidget))

  ;; how to customize the tab behavior
  ;; add the open-with-xwidget in the alt-done alist
  (ivy-configure 'open-with-xwidget
    :parent 'read-file-name-internal
    :occur #'counsel-find-file-occur))


;; TODO rewrite this
(defun rename-current-buffer-file (&optional arg)
  "Rename the current buffer and the file it is visiting.
If the buffer isn't visiting a file, ask if it should
be saved to a file, or just renamed.

If called without a prefix argument, the prompt is
initialized with the current directory instead of filename."
  (interactive "P")
  (let* ((old-short-name (buffer-name))
         (old-filename (buffer-file-name)))
    (if (and old-filename (file-exists-p old-filename))
        ;; the buffer is visiting a file
        (let* ((old-dir (file-name-directory old-filename))
               (new-name (read-file-name "New name: " (if arg old-dir old-filename)))
               (new-dir (file-name-directory new-name))
               (new-short-name (file-name-nondirectory new-name))
               (file-moved-p (not (string-equal new-dir old-dir)))
               (file-renamed-p (not (string-equal new-short-name old-short-name))))
          (cond ((get-buffer new-name)
                 (error "A buffer named '%s' already exists!" new-name))
                ((string-equal new-name old-filename)
                 (spacemacs/show-hide-helm-or-ivy-prompt-msg
                  "Rename failed! Same new and old name" 1.5)
                 (rename-current-buffer-file))
                (t
                 (let ((old-directory (file-name-directory new-name)))
                   (when (and (not (file-exists-p old-directory))
                              (yes-or-no-p
                               (format "Create directory '%s'?" old-directory)))
                     (make-directory old-directory t)))
                 (rename-file old-filename new-name 1)
                 (rename-buffer new-name)
                 (set-visited-file-name new-name)
                 (set-buffer-modified-p nil)
                 (when (fboundp 'recentf-add-file)
                   (recentf-add-file new-name)
                   (recentf-remove-if-non-kept old-filename))
                 (when (and (configuration-layer/package-used-p 'projectile)
                            (projectile-project-p))
                   (call-interactively #'projectile-invalidate-cache))
                 (message (cond ((and file-moved-p file-renamed-p)
                                 (concat "File Moved & Renamed\n"
                                         "From: " old-filename "\n"
                                         "To:   " new-name))
                                (file-moved-p
                                 (concat "File Moved\n"
                                         "From: " old-filename "\n"
                                         "To:   " new-name))
                                (file-renamed-p
                                 (concat "File Renamed\n"
                                         "From: " old-short-name "\n"
                                         "To:   " new-short-name)))))))
      ;; the buffer is not visiting a file
      (let ((key))
        (while (not (memq key '(?s ?r)))
          (setq key (read-key (propertize
                               (format
                                (concat "Buffer '%s' is not visiting a file: "
                                        "[s]ave to file or [r]ename buffer?")
                                old-short-name)
                               'face 'minibuffer-prompt)))
          (cond ((eq key ?s)            ; save to file
                 ;; this allows for saving a new empty (unmodified) buffer
                 (unless (buffer-modified-p) (set-buffer-modified-p t))
                 (save-buffer))
                ((eq key ?r)            ; rename buffer
                 (let ((new-buffer-name (read-string "New buffer name: ")))
                   (while (get-buffer new-buffer-name)
                     ;; ask to rename again, if the new buffer name exists
                     (if (yes-or-no-p
                          (format (concat "A buffer named '%s' already exists: "
                                          "Rename again?")
                                  new-buffer-name))
                         (setq new-buffer-name (read-string "New buffer name: "))
                       (keyboard-quit)))
                   (rename-buffer new-buffer-name)
                   (message (concat "Buffer Renamed\n"
                                    "From: " old-short-name "\n"
                                    "To:   " new-buffer-name))))
                ;; ?\a = C-g, ?\e = Esc and C-[
                ((memq key '(?\a ?\e)) (keyboard-quit))))))))

(defun lsp-keybinding ()
  "Return the keybinding for lsp functions."
  (list "=" "format" nil
        "==" "lsp-format-buffer" 'lsp-format-buffer
        "=r" "lsp-format-region" 'lsp-format-region

        "a" "code actions" nil
        "aa" "lsp-execute-code-action" 'lsp-execute-code-action
        "al" "lsp-avy-lens" 'lsp-avy-lens
        "ah" "lsp-document-highlight" 'lsp-document-highlight

        "F" "folder" nil
        "Fa" "lsp-workspace-folders-add" 'lsp-workspace-folders-add
        "Fr" "lsp-workspace-folders-remove " 'lsp-workspace-folders-remove
        "Fb" "lsp-workspace-blacklist-remove" 'lsp-workspace-blacklist-remove

        "g" "goto" nil
        "gg" "lsp-find-definition" 'lsp-find-definition
        "gr" "lsp-find-references" 'lsp-find-references
        "gi" "lsp-find-implementation" 'lsp-find-implementation
        "gt" "lsp-find-type-definition" 'lsp-find-type-definition
        "gd" "lsp-find-declaration" 'lsp-find-declaration
        "ga" "xref-find-apropos" 'xref-find-apropos

        "G" "peek" nil
        "Gg" "lsp-ui-peek-find-definitions" 'lsp-ui-peek-find-definitions
        "Gr" "lsp-ui-peek-find-references" 'lsp-ui-peek-find-references
        "Gi" "lsp-ui-peek-find-implementatio" 'lsp-ui-peek-find-implementation
        "Gs" "lsp-ui-peek-find-workspace-symbol" 'lsp-ui-peek-find-workspace-symbol


        "h" "help" nil
        "hh" "lsp-describe-thing-at-point" 'lsp-describe-thing-at-point
        "hs" "lsp-signature-activate" 'lsp-signature-activate
        "hg" "lsp-ui-doc-glance" 'lsp-ui-doc-glance

        "r" "refactor" nil
        "rr" "lsp-rename" 'lsp-rename
        "ro" "lsp-organize-imports" 'lsp-organize-imports


        "T" "toggle" nil
        "Tl" "lsp-lens-mode" 'lsp-lens-mode
        "TL" "lsp-toggle-trace-io" 'lsp-toggle-trace-io
        "Th" "lsp-toggle-symbol-highlight" 'lsp-toggle-symbol-highlight
        "TS" "lsp-ui-sideline-mode" 'lsp-ui-sideline-mode
        "Td" "lsp-ui-doc-mode" 'lsp-ui-doc-mode
        "Ts" "lsp-toggle-signature-auto-activate" 'lsp-toggle-signature-auto-activate))

(defun switch-to-minibuffer-window ()
  "Switch to minibuffer window (if active)."
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))


(defun org-file-show-headings (org-file)
  (interactive)
  (find-file (expand-file-name org-file))
  (counsel-org-goto)
  (org-overview)
  (org-reveal)
  (org-show-subtree)
  (forward-line))

(defun buffer-untabify ()
  "Transfer all tab to spaces."
  (interactive)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end)))

(defun new-empty-buffer ()
  "Create a new buffer called: untitled."
  (interactive)
  (let ((newbuf (generate-new-buffer "untitled")))

    ;; Prompt to save on `save-some-buffers' with positive PRED
    (with-current-buffer newbuf
      (setq-local buffer-offer-save t))
    ;; pass non-nil force-same-window to prevent `switch-to-buffer' from
    ;; displaying buffer in another window
    (switch-to-buffer newbuf nil 'force-same-window)))

(defun rotate-windows-forward (count)
  "Rotate each window forwards.
A negative prefix argument rotates each window backwards.
Dedicated (locked) windows are left untouched."
  (interactive "p")
  (let* ((non-dedicated-windows (cl-remove-if 'window-dedicated-p (window-list)))
         (states (mapcar #'window-state-get non-dedicated-windows))
         (num-windows (length non-dedicated-windows))
         (step (+ num-windows count)))
    (if (< num-windows 2)
        (error "You can't rotate a single window!")
      (dotimes (i num-windows)
        (window-state-put
         (elt states i)
         (elt non-dedicated-windows (% (+ step i) num-windows)))))))


;; Borrow project search function from the projectile
(defun my-counsel-projectile-rg (&optional options)
  "Search the current project with rg and search under certarn directory
if it's not in a project.

OPTIONS, if non-nil, is a string containing additional options to
be passed to rg. It is read from the minibuffer if the function
is called with a prefix argument."
  (interactive)
  ;; change this to read a directory path
  (let* ((search-directory (if (projectile-project-p)
                               (projectile-project-root)
                             (read-directory-name "Start from directory: ")))
         (ivy--actions-list (copy-sequence ivy--actions-list))
         (ignored
          (mapconcat (lambda (i)
                       (concat "--glob !" (shell-quote-argument i)))
                     (append
                      (projectile--globally-ignored-file-suffixes-glob)
                      (projectile-ignored-files-rel)
                      (projectile-ignored-directories-rel))
                     " "))
         (counsel-rg-base-command
          (let ((counsel-ag-command counsel-rg-base-command))
            (counsel--format-ag-command ignored "%s"))))
    (ivy-add-actions
     'counsel-rg
     counsel-projectile-rg-extra-actions)
    (counsel-rg (eval counsel-projectile-rg-initial-input)
                search-directory
                options
                (projectile-prepend-project-name
                 (concat (car (if (listp counsel-rg-base-command)
                                  counsel-rg-base-command
                                (split-string counsel-rg-base-command)))
                         ": ")))))

(defun my-find-dotfile ()
  "Edit the `dotfile', in the current window."
  (interactive)
  (find-file-existing "~/Desktop/spacemacs-private/mycraft/init.el"))


(defun hey-god (question)
  "Reduce distraction when you search the answer for the question.
Powered by the howdoi"
  (interactive "sAsk the god, you'll get it: ")
  (let ((buffer-name "*God's reply*")
        (exectuable-name "howdoi"))
    (with-output-to-temp-buffer buffer-name
      (shell-command (concat exectuable-name " " question)
                     buffer-name
                     "*Messages*")
      (pop-to-buffer buffer-name))))

(defun copy-region-and-base64-decode (start end)
  (interactive "r")
  (let ((x (base64-decode-string
            (decode-coding-string
             (buffer-substring start end) 'utf-8))))
    (kill-new x)))

(defun my-encode-region-base64 (start end)
  (interactive "r")
  (let ((content (buffer-substring-no-properties start end)))
    (when (use-region-p)
      (delete-region start end)
      (insert (base64-encode-string (encode-coding-string content 'utf-8))))))

(defun my-decode-region-base64 (start end)
  (interactive "r")
  (let ((content (buffer-substring-no-properties start end)))
    (when (use-region-p)
      (delete-region start end)
      (insert (base64-decode-string (decode-coding-string content 'utf-8))))))

(defun copy-region-and-urlencode (start end)
  (interactive "r")
  (let ((x (url-hexify-string
            (buffer-substring start end))))
    (kill-new x)))

(defun now ()
  "Get the current time, In the future this will show a temp buffer with unix format, human readable and the weather info."
  (interactive)
  (message "now: %s \ntimestamp: %s" (format-time-string "%Y-%m-%d %H:%m:%S %z") (format-time-string "%s")))


(defun my-shrink-window (delta)
  "Shrink-window."
  (interactive "p")
  (shrink-window delta))

(defun my-shrink-window-horizontally (delta)
  "Shrink-window."
  (interactive "p")
  (shrink-window delta t))


(defun my-enlarge-window (delta)
  (interactive "p")
  (enlarge-window delta))

(defun my-enlarge-window-horizontally (delta)
  (interactive "p")
  (enlarge-window delta t))


(defun my-emmet-expand ()
  (interactive)
  (unless (if (bound-and-true-p yas-minor-mode)
              (call-interactively 'emmet-expand-yas)
            (call-interactively 'emmet-expand-line))
    (indent-for-tab-command)))


(defun python-run-main ()
  (interactive)
  (shell-command
   (format (concat python-run-command " %s %s")
           (shell-quote-argument (or (file-remote-p (buffer-file-name (buffer-base-buffer)) 'localname)
                                     (buffer-file-name (buffer-base-buffer))))
           python-run-args)))

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

(defun define-leader-key-global (&rest MAPS)
  (let ((get-props (lambda () (list
                               my-leader-def-prop
                               my-leader-def-emacs-state-prop))))
    (dolist (prop (funcall get-props))
      (apply 'general-define-key
             :states (plist-get prop ':states)
             :prefix (symbol-value (plist-get prop ':key))
             :keymaps 'override
             MAPS))))

(defun define-leader-key-map-for (mode-map &rest MAPS)
  "Define the leader key map for the specify mode."
  ;; TODO: to understand the difference between '(1) (list 1)
  (let ((get-props (lambda () (list
                               my-local-leader-def-emacs-state-prop
                               my-local-leader-def-prop
                               my-local-leader-def-alias-prop))))

    ;; TODO: use evil-define-key instead. I don't know why
    ;; it will cause overwrite key binding on other mode
    ;; when binding with lsp-command-map (maybe, it is not a
    ;; normal keymap)
    ;; (which-key-add-major-mode-key-based-replacements mode key desc)

    ;; key desc binding

    (dolist (prop (funcall get-props))

      (cl-loop
       for (key desc binding)
       on MAPS by #'cdddr
       do

       (let ((mode (intern (string-remove-suffix "-map" (symbol-name mode-map))))
             (shortcut-key (concat (symbol-value (plist-get prop ':key)) key))
             (shortcut (kbd (concat (symbol-value (plist-get prop ':key)) key)))
             (sts (plist-get prop ':states)))

         (if (not (equal binding 'lsp-command-map))
             (apply 'general-define-key
                    :states sts
                    :prefix (symbol-value (plist-get prop ':key))
                    :keymaps mode-map
                    (if (equal binding nil)
                        (list key (list :ignore t :which-key desc))
                      (list key (list binding :which-key desc))))

           ;; (define-key python-mode-map (kbd "SPC m") lsp-command-map)
           ;; NOTE: what the fuck evil-define-key can't used symbol of mode-map ...
           ;; (evil-define-key 'normal python-mode-map (kbd "SPC m") lsp-command-map)
           ;; (evil-define-key 'normal go-mode-map (kbd "SPC m") lsp-command-map)

           ;; if using the lexcial binding, we need to add a wrap to
           ;; bind the varaibles
           ((lambda (sts kmap keybinding func)
              (message "evil this fucking thing!!")
              (print kmap)
              (print sts)
              (print keybinding)
              (print func)
              (evil-define-key sts kmap keybinding func))
            sts mode-map
            (kbd (symbol-value (plist-get prop ':key)))
            binding)))))))


(defun copy-file-path ()
  "Copy and show the file path of the current buffer."
  (interactive)
  (if-let (file-path (get-file-path))
      (progn
        (kill-new file-path)
        (message "%s" file-path))
    (message "WARNING: Current buffer is not attached to a file!")))


(defun get-file-path ()
  "Retrieve the file path of the current buffer.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (buffer-file-name))
    (file-truename file-path)))


(defun evil-smart-doc-lookup ()
  "Run documentation lookup command specific to the major mode.
Use command bound to `SPC m h h` if defined, otherwise fall back
to `evil-lookup'"
  (interactive)
  ;; (let ((binding (key-binding (kbd (concat "SPC" " mhh")))))
  (when (fboundp 'lsp-describe-thing-at-point)
    (lsp-describe-thing-at-point)
    (evil-lookup)))

;; (let ((binding (global-key-binding (kbd (concat "SPC" " mhh")))))
;;   (print (key-binding (kbd (concat "SPC" " mhh"))))
;;   (if (commandp binding)
;;      (call-interactively binding)
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


(defun project-run-vterm (&optional arg)
  "Invoke `vterm' in the project's root.

Switch to the project specific term buffer if it already exists.
Use a prefix argument ARG to indicate creation of a new process instead."
  (interactive "P")
  (let* ((project (projectile-ensure-project (projectile-project-root)))
         (buffer (projectile-generate-process-name "vterm" arg)))
    (unless (buffer-live-p (get-buffer buffer))
      (unless (require 'vterm nil 'noerror)
        (error "Package 'vterm' is not available"))
      (projectile-with-default-dir project
        (vterm-other-window buffer)))))

(defun new-terminal ()
  "New a terminal in project root or the current directory."
  (interactive)
  (if (projectile-project-p)
      (project-run-vterm)
    (vterm-other-window)))

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


;; Decide to use this package to auto balance the parens
;; NOTE: we should put this in the :init
;; if we put this in the :config, it will perform add these hook after
;; lazy-loading. That means we will not get it auto turn on when we enter one of the following program mode
;; :init before trigger
;; :config after trigger
(use-package smartparens
  :commands (smartparens-mode)
  :init
  (require 'smartparens-config)
  (add-hook 'js-mode-hook #'smartparens-mode)
  (add-hook 'go-mode-hook #'smartparens-mode)
  (add-hook 'html-mode-hook #'smartparens-mode)
  (add-hook 'python-mode-hook #'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode))

;; TODO: find a way to replace the hardcode path
(use-package yasnippet
  :defer 1
  :config
  (add-to-list 'yas-snippet-dirs "/Users/jing/Desktop/spacemacs-private/snippets")
  (yas-global-mode 1)
  (yas-minor-mode 1)
  ;; (yas-reload-all) to rebuild the snippets, This will be trigger when enable yas-xx-mode
  )

(use-package yasnippet-snippets
  :defer t
  :after yasnippet)

(use-package ivy-yasnippet
  :defer t
  :after yaanippet)

(use-package winum
  :config
  (winum-mode))

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
  :init
  (defconst leader-key "SPC")
  (defconst major-mode-leader-key "SPC m")
  (defconst major-mode-leader-key-shortcut ",")
  (defconst emacs-state-leader-key "M-m")
  (defconst emacs-state-major-mode-leader-key "M-m m")

  (setq my-leader-def-prop
        '(:key leader-key :states (normal visual motion)))

  (setq my-leader-def-emacs-state-prop
        '(:key emacs-state-leader-key :state (emacs)))

  ;; below are for major mode
  (setq my-local-leader-def-prop
        '(:key major-mode-leader-key :states (normal visual motion)))

  (setq my-local-leader-def-alias-prop
        '(:key major-mode-leader-key-shortcut :states (normal visual motion)))

  (setq my-local-leader-def-emacs-state-prop
        '(:key emacs-state-major-mode-leader-key :states (emacs)))
  ;; NOTE: '() the element inside will be symbol

  :after (evil)
  :config

  ;; NOTE: keysmaps override is to make general-define-key to be global scope
  ;; No need to set this one (evil-make-overriding-map dired-mode-map 'normal)
  (message "DEBUG: !! general init")

  (with-eval-after-load 'emmet-mode
    (evil-define-key 'insert emmet-mode-keymap (kbd "TAB") 'my-emmet-expand))

  ;; unbind some keybinding in the package 'evil-org
  (with-eval-after-load 'evil-org
    ;;  org-agenda-redo
    ;;  make org agenda enter the motion state
    ;;  I don't the original state
    (evil-set-initial-state 'org-agenda-mode 'motion)
    ;; TODO: research about this evilified-state-evilify-map

    (evil-define-key 'motion org-agenda-mode-map
      (kbd "j") 'org-agenda-next-line
      "t" 'org-agenda-todo
      "I" 'org-agenda-clock-in ; Original binding
      "O" 'org-agenda-clock-out ; Original binding
      (kbd "<return>") 'org-agenda-goto
      (kbd "k") 'org-agenda-previous-line
      (kbd "s") 'org-save-all-org-buffers))

  (with-eval-after-load 'org

    ;; define key open-thing-at-point with enter
    (evil-define-key 'normal org-mode-map (kbd "<return>") 'org-open-at-point)
    (evil-define-key 'normal prog-mode-map (kbd "<return>") 'org-open-at-point))


  ;; add shortcuts for org src edit mode
  (with-eval-after-load 'org-src
    (evil-define-key 'normal org-src-mode-map
      (kbd ", ,") 'org-edit-src-exit
      (kbd ", k") 'org-edit-src-abort))

  (with-eval-after-load 'with-editor
    (evil-define-key 'normal with-editor-mode-map
      (kbd ", ,") 'with-editor-finish
      (kbd ", k") 'with-editor-cancel))

  (evil-define-key 'visual 'global
    (kbd "g y") 'copy-region-and-base64-decode
    (kbd "g e") 'copy-region-and-urlencode)

  ;; TODO: maybe I can define my own hydra?
  (evil-define-key 'normal 'evil-motion-state-map
    (kbd "*") 'swiper-thing-at-point)

  ;; keybinding for go-mode
  (with-eval-after-load 'lsp-mode

    (with-eval-after-load 'go-mode

      (apply 'define-leader-key-map-for 'go-mode-map
             (lsp-keybinding))

      (define-leader-key-map-for 'go-mode-map
        "" "major mode" nil
        "x" "execute" nil
        "xx" "go run" 'go-run-main
        "d" "debug" 'dap-hydra
        "e" "gomacro" 'gomacro-run)


      (evil-define-key 'normal go-mode-map (kbd "K") 'evil-smart-doc-lookup))

    (with-eval-after-load 'python
      (apply 'define-leader-key-map-for 'python-mode-map
             (lsp-keybinding))

      (apply 'define-leader-key-map-for
             (list 'python-mode-map
                   "" "major mode" 'nil
                   "x" "execute" nil
                   "xx" "python run" 'python-run-main
                   "d" "debug" 'dap-hydra)))
    )

  (with-eval-after-load 'elisp-mode
    (define-leader-key-map-for 'emacs-lisp-mode-map
      "" "major mode" nil
      "e" "eval" nil
      "ef" "eval defun" 'eval-defun
      "eb" "eval buffer" 'eval-buffer
      "er" "eval region" 'eval-region ))

  (with-eval-after-load 'org
    (define-leader-key-map-for 'org-mode-map
      "" "major mode" nil

      "a" "org-agenda" 'org-agenda
      "," "org-ctrl-c-ctrl-c" 'org-ctrl-c-ctrl-c
      "'" "org-edit-special" 'org-edit-special

      "i" "insert" nil
      "il" "insert link" 'org-insert-link

      "e" "export" nil
      "ee" "org-export-dispatch" 'org-export-dispatch

      "n" "narrow" nil
      "ns" "narrow subtree" 'org-narrow-to-subtree
      "nN" "widen" 'widen

      "s" "schedule" nil
      "ss" "org-schedule" 'org-schedule
      "sd" "org-deadline" 'org-deadline
      "st" "org-time-stamp" 'org-time-stamp

      "d" "org-download" nil
      "dc" "from clipboard" 'org-download-clipboard
      "ds" "from screenshot" 'org-download-screenshot

      "t" "toggles" nil
      "tl" "link display" 'org-toggle-link-display
      "ti" "inline image" 'org-toggle-inline-images

      "j" "journals" nil
      "jn" "new entry" 'org-journal-new-entry))


  (define-leader-key-global
    "SPC" 'counsel-M-x
    "/" 'my-counsel-projectile-rg
    "v" 'er/expand-region
    "u" 'universal-argument
    "'" 'new-terminal
    "?" 'counsel-descbinds)

  ;; which-key-replacement-alist
  ;; change the content of the above variable
  (define-leader-key-global
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

  (define-leader-key-global
    "j" '(:ignore t :which-key "jump")
    "jw" '(avy-goto-char-2 :which-key "avy goto ch2")
    "ju" '(avy-jump-url :which-key "goto url")
    "jl" '(avy-goto-line :which-key "goto line")
    "ji" '(counsel-jump-in-buffer :which-key "imenu")
    "j(" '(check-parens :which-key "check-parens"))

  (define-leader-key-global
    "r" '(:ignore t :which-key "resume/register")
    "rk" '(counsel-yank-pop :which-key "kill ring")
    "re" '(counsel-evil-registers :which-key "evil register")
    "rl" '(ivy-resume :which-key "ivy-resume"))

  (define-leader-key-global
    "a" '(:ignore t :which-key "applications")

    "al" '(:ignore t :which-key "lookup/dictionary")
    "ald" '(define-word :which-key "lookup definition")
    "alg" '(google-search :which-key "google search")
    "alx" '(open-with-xwidget :which-key "open with xwidget")

    "ao" '(:ignore t :which-key "org")
    "aog" '(:ignore t :which-key "goto")
    "aoge" '((lambda () (interactive) (org-file-show-headings "~/Dropbox/myorgs/english/english_practice.org")) :which-key "english note")
    "aogb" '((lambda () (interactive) (org-file-show-headings "~/Dropbox/myorgs/books/books.org")) :which-key "book note")
    "aogt" '((lambda () (interactive) (org-file-show-headings "~/Dropbox/myorgs/todo.org")) :which-key "todo note"))

  (define-leader-key-global
    "b" '(:ignore t :which-key "buffer")
    "bb" '(counsel-projectile-switch-to-buffer :which-key "project-list-buffer")
    "bd" '(kill-this-buffer :which-key "kill-buffer")
    "bB" '(counsel-switch-buffer :which-key "list-buffer")
    "bi" '(ibuffer :which-key "ibuffer")
    "bn" '(next-buffer :which-key "next-buffer")
    "bp" '(previous-buffer :which-key "previous-buffer")
    "bN" '(new-empty-buffer :which-key "new empty buffer"))

  (define-leader-key-global
    "c" '(:ignore t :which-key "comment/compile")
    "cl" '(comment-or-uncomment-lines :which-key "comment or uncomment"))

  (define-leader-key-global
    "e" '(:ignore t :which-key "errors")
    "el" '(toggle-flycheck-error-list :which-key "flycheck error list"))


  (define-leader-key-global
    "i" '(:ignore t :which-key "insert")
    "is" '(ivy-yas :which-key "snippets"))

  (define-leader-key-global
    "l" '(:ignore t :which-key "layout")
    "ll" '(persp-switch :which-key "switch layout"))

  (define-leader-key-global
    "n" '(:ignore t :which-key "narrow")
    "nf" '(narrow-to-defun :which-key "narrow to defun")
    "nw" '(widen :which-key "widen"))

  (define-leader-key-global
    "p" '(:ignore t :which-key "project")
    "pp" '(counsel-projectile-switch-project :which-key "switch project")
    "pf" '(counsel-projectile-find-file :which-key "find-file"))

  (define-leader-key-global
    "s" '(:ignore t :which-key "search")
    "sc" '(evil-ex-nohighlight :which-key "clear highlight")
    "ss" '(swiper :which-key "swiper")
    "sS" '(swiper-all :which-key "swiper-all"))

  (define-leader-key-global
    "g" '(:ignore t :which-key "git")
    "gi" '(magit-init :which-key "gagit init")
    "gb" '(:ignore t :which-key "blame")
    "gbl" '(git-messenger:popup-message  :which-key "this line")
    "gbb" '(magit-blame-addition  :which-key "this buffer")
    "gs" '(magit-status :which-key "magit status"))


  (define-leader-key-global
    "t"  '(:ignore t :which-key "toggles")
    "tm" '(hydra-mode-toggle/body :which-key "toggle mode")
    "ts" '(hydra-text-scale/body :which-key "scale text"))

  (define-leader-key-global
    "w" '(:ignore t :which-key "windows")
    "wf" '(toggle-frame-fullscreen :which-key "toggle fullscreen")
    "wm" '(toggle-maximize-buffer :which-key "window maximized")
    "wM" '(toggle-frame-maximized :which-key "frame maximized")
    "wd" '(delete-window :which-key "delete window")
    "wh" '(evil-window-left :which-key "go to window left")
    "wl" '(evil-window-right :which-key "go to window right")
    "wk" '(evil-window-up :which-key "go to window up")
    "wr" '(rotate-windows-forward :which-key "rotate window")
    "wj" '(evil-window-down :which-key "go to window down")
    "wL" '(evil-window-move-far-right :which-key "move window to right side")
    "wH" '(evil-window-move-far-left :which-key "move window to left side")
    "wJ" '(evil-window-move-very-bottom :which-key "move window to bottom side")
    "wK" '(evil-window-move-very-top :which-key "move window to top side")

    "wg" '(switch-to-minibuffer-window :which-key "go to minibuffer")

    "w/" '(evil-window-vsplit :which-key "split vertically")
    "w-" '(evil-window-split :which-key "split horizontally")

    "w[" '(my-shrink-window-horizontally :which-key "shrink h")
    "w]" '(my-enlarge-window-horizontally :which-key "enlarge h")
    "w{" '(my-shrink-window :which-key: "shrink v")
    "w}" '(my-enlarge-window :which-key: "enlarge v")

    "wF" '(make-frame :which-key "make frame")
    "wo" '(other-frame :which-key "other frame")
    "w." '(window-operate/body :which-key "window transient"))

  (define-leader-key-global
    "x" '(:ignore t :which-key "texts")
    "xc" '(count-words-region :which-key "count-words-region")

    "xb" '(:ignore t :which-key "base64")
    "xbe" '(my-encode-region-base64 :which-key "base64-encode-region")
    "xbd" '(my-decode-region-base64 :which-key "base64-decode-region"))

  (define-leader-key-global
    "f" '(:ignore t :which-key "files")
    "fe" '(:ignore t :which-key "emacs")
    "fed" '(my-find-dotfile :which-key "open config dotfile")
    "fy" '(copy-file-path :which-key "copy file path")
    "fs" '(save-buffer :which-key "save file")
    "ff" '(counsel-find-file :which-key "find file")))


(use-package hydra
  :defer t)

(defhydra window-operate ()
  "window operation"
  ("[" my-shrink-window-horizontally "shrink (h)")
  ("]" my-enlarge-window-horizontally "enlarge (h)")
  ("{" my-shrink-window "shrink (v)")
  ("}" my-enlarge-window "enlarge (v)")
  ("=" balance-windows "balance"))

(defhydra hydra-text-scale (:timeout 8)
  "scale text"
  ("j" text-scale-increase "+")
  ("k" text-scale-decrease "-")
  ("0" ((lambda (inc) (text-scale-adjust inc)) 0) "reset")
  ("<escape>" nil "finished" :exit t))

(defhydra hydra-mode-toggle ()
  "toggle mode"
  ("r" rainbow-mode "rainbow mode")
  ("w" whitespace-mode "whitespace-mode")
  ("t" counsel-load-theme "theme")
  ("v" visual-line-mode "visual line mode")
  ("f" flyspell-mode "check spell"))

;; https://github.com/emacs-evil/evil-collection
;; optional

(use-package evil
  :defer 1
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
  (evil-set-initial-state 'dashboard-mode 'normal)

  (add-hook 'evil-normal-state-entry-hook 'im-use-eng)
  (add-hook 'evil-insert-state-entry-hook 'im-use-prev)
  (add-hook 'evil-insert-state-exit-hook 'im-remember)
  (add-hook 'evil-emacs-state-entry-hook 'im-use-eng))


;; create arbitrary fold not like other package auto detect the program language
(use-package vimish-fold
  :after evil
  :hook (prog-mode . vimish-fold-mode))

(use-package evil-vimish-fold
  :after vimish-fold
  :hook (prog-mode . evil-vimish-fold-mode))

;; make =%= to be able to jump to and back the tag
(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package evil-collection
  :after evil
  :config
  (delete 'wgrep evil-collection-mode-list)
  (delete 'vterm evil-collection-mode-list)
  (delete 'lispy evil-collection-mode-list)
  (delete 'ivy evil-collection-mode-list)
  ;; this will bind a global esc key for minibuffer-keyboard-quit so I remove it.
  (setq evil-collection-company-use-tng nil)
  (evil-collection-init))

;; NOTE: it will save the command behavior applied on the multiple cursor
;; to a file named .mc-lists.el. By default, it's path is =~/.emacs.d/.mc-lists.el=
;; I customize the storing path already.
;; Research how evil-mc customize the multiple-cursor
(use-package multiple-cursors
  :init
  (global-set-key (kbd "C-S-a") 'mc/edit-lines)
  (global-set-key (kbd "C-S-<down-mouse-1>") 'mc/add-cursor-on-click)
  (global-set-key (kbd "<C-S-right>") 'mc/mark-next-like-this)
  (global-set-key (kbd "<C-S-left>") 'mc/mark-previous-like-this)
  :commands
  (mc/edit-lines
   mc/mark-all-like-this
   mc/add-cursor-on-click
   mc/mark-next-like-this
   mc/mark-previous-like-this))

(use-package evil-nerd-commenter
  :after evil
  :commands evilnc-comment-operator
  :init
  (define-key evil-normal-state-map "gc" 'evilnc-comment-operator))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;; M-x all-the-icons-install-fonts
(use-package all-the-icons)

;; ENHANCE: adjust the pop ui
(use-package git-messenger
  :defer t
  :init
  (setq git-messenger:show-detail t)
  (setq git-messenger:use-magit-popup t))

(use-package slime
  :defer t
  :init
  (setq inferior-lisp-program "sbcl"))

;; TODO: maybe I neeed the better go to definition function like the spacemacs's implementation
(use-package elisp-slime-nav
  :defer t
  :init
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'elisp-slime-nav-mode)))

(use-package org-ql
  :defer t)

;; The :ensure keyword causes the package(s) to be installed automatically if not already present on your system
;; this setting will globally enable ensure (setq use-package-always-ensure t)

(use-package wgrep
  :after evil
  :commands
  (wgrep-finish-edit
   wgrep-finish-edit
   wgrep-abort-changes
   wgrep-abort-changes)
  :init
  (evil-define-key 'normal wgrep-mode-map (kbd "<escape>") 'wgrep-exit)
  (evil-define-key 'normal wgrep-mode-map (kbd ", ,") 'wgrep-finish-edit)
  (evil-define-key 'normal wgrep-mode-map (kbd ", k") 'wgrep-abort-changes))

(use-package ivy
  :ensure t
  :diminish
  :bind (:map ivy-minibuffer-map
              ("TAB" . ivy-alt-done)
              ("C-l" . ivy-alt-done)
              ("C-j" . ivy-next-line)
              ("C-k" . ivy-previous-line)
              ("C-u" . ivy-backward-kill-word)
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
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (setq ivy-use-selectable-prompt t)
  ;; NOTE: make the candidate you typed selectable
  ;; This is useful when you call =counsel-find-file=. Ex. You can choose the bar.yml when there is a candidate named barfar.yml

  ;; (setq ivy-dynamic-exhibit-delay-ms 250)
  (setq ivy-initial-inputs-alist nil))

;; After swiper, counsel search, ivy-occur (C-c C-o) to get the candidate in another buffer
;; Then we can enter edit mode by ivy-wgrep-change-to-wgrep-mode (C-x C-q)
;; use multiple-cursor may be helpful.
;; Finally, Ctrl-c Ctrl-c to commit change
;; there are some key binding in the swiper-map
(use-package swiper
  :bind (("C-s" . swiper))
  )

;; check-paren checks whether there are lacks of the parentheses' pairs
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-w" . 'ivy-backward-kill-word)
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq counsel-find-file-at-point t))

;; counsel-search will use the package request with this function
(use-package request
  :defer t)

(use-package ivy-rich
  :after (ivy)
  :init
  (ivy-rich-mode 1))


(use-package projectile
  :defer 1
  :custom ((projectile-completion-system 'ivy))
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package counsel-projectile
  :after projectile
  :defer 1
  :config (counsel-projectile-mode))

(use-package magit
  :defer 2
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-magit
  :defer 2
  :after magit)

;; the following is org's setup
;; we can check the org's version by the command =org-version=
(use-package org
  :defer t
  :ensure org-plus-contrib
  :pin org)


(use-package restclient
  :defer t)

(use-package ob-restclient
  :defer t
  :after (org restclient)
  :init (add-to-list 'org-babel-load-languages '(restclient . t)))

(use-package org-download
  :commands
  (org-download-screenshot
   org-download-clipboard)
  :defer t)

(use-package org-journal
  :defer t)

(use-package ox-reveal
  :after org)

(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

(defun org-mode-visual-fill ()
  "A beautiful word wrap effect."
  (setq visual-fill-column-width 150)
  ;; TODO: research implement a hook to dynamic change the visual-fill-column-with
  ;; maybe, I can remove this package?
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
  (global-visual-line-mode 1)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . org-mode-visual-fill))

;; Failed to install vterm: https://melpa.org/packages/vterm-20200926.1215.tar: Not found
;; package-refresh-contents
(use-package vterm
  :defer t
  :init
  (setq vterm-always-compile-module t)
  :config
  (define-key vterm-mode-map (kbd "<escape>") 'vterm-send-escape)
  (add-hook 'vterm-mode-hook (lambda ()
                               (evil-emacs-state)
                               (vterm-send-string "source ~/.bash_profile")
                               (vterm-send-return))))

(use-package avy
  :defer t
  :config
  (setq avy-background t))

;; company-mode setup
;; "<return>" is the Return key while emacs runs in a graphical user interface.
;; "RET" is the Return key while emacs runs in a terminal. ...
;; But the problem is, by binding (kbd "RET") , you are also binding (kbd "C-m")

(use-package company
  :config
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.1)
  ;; In evil-collection, it adjust the key binding for the company-mode
  ;; NOTE: Furthermore, it also disable the pre-select behavior when
  ;; showing the completion candidates.
  (define-key company-active-map (kbd "<return>") 'company-complete-selection)
  (global-company-mode 1))

(use-package expand-region
  :defer t)

;; lsp configuation

(use-package rust-mode
  :defer t
  :mode "\\.rs\\'"
  :init (setq rust-format-on-save t))

(use-package cargo
  :defer t)

(use-package go-mode
  :defer 2
  :config
  (progn
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook 'gofmt-before-save)))

(use-package protobuf-mode
  :defer t)

(use-package systemd
  ;; ISSUE: Company backend ’t’ could not be initialized
  :defer t)

(use-package lispy
  :init
  (setq lispy-key-theme '(special c-digits))
  :hook ((common-lisp-mode . lispy-mode)
         (emacs-lisp-mode . lispy-mode)
         (scheme-mode . lispy-mode)))

(use-package flycheck
  :commands (flycheck-mode)
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (add-hook 'text-mode-hook 'flycheck-mode)
  (setq flycheck-highlighting-mode 'lines)
  (setq flycheck-indication-mode 'nil))

(use-package json-mode
  :defer t)

(use-package yaml-mode
  :defer t
  :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
         ("Procfile\\'" . yaml-mode))
  :init
  (add-hook 'yaml-mode-hook 'lsp)
  :config
  (with-eval-after-load 'flycheck
    (when (listp flycheck-global-modes)
      (add-to-list 'flycheck-global-modes 'yaml-mode))))

(use-package gomacro-mode
  :hook (go-mode . gomacro-mode))

;; NOTE: temporarily disable this because it consumes lots of cpu
;; (use-package company-tabnine
;;   :config
;;   (with-eval-after-load 'company
;;     (add-to-list 'company-backends #'company-tabnine)
;;     (setq company-tabnine-always-trigger nil)
;;     (setq company-show-numbers t)
;;     (setq company-idle-delay 0.1)))

;; set up python dev tools

(with-eval-after-load 'python
  (setq python-shell-interpreter "ipython"))

(use-package python-pytest
  :defer t
  :custom
  (python-pytest-confirm t))

(use-package poetry
  :defer t)

(use-package pyvenv
  :commands (pyvenv-mode)
  :init
  (add-hook 'python-mode-hook #'pyvenv-mode))

(use-package pyimport
  :defer t)

;; ---

(use-package lsp-mode
  :init
  (setq lsp-completion-provider :capf) ;; the official recommends use this
  ;; run =company-diag= to check what the company-backen is being used.
  ;; (setq lsp-keymap-prefix "SPC m") ;; this will only affect the display info of whichkey.
  :commands
  (lsp)
  :hook
  (go-mode . lsp)
  ;; (lsp-mode . (lambda () (lsp-headerline-breadcrumb-mode)))
  ;; add breadcrumb to hint current position
  (python-mode . lsp)
  (rust-mode . lsp)
  (js-mode . lsp)
  :config
  ;; turn off lens mode
  (setq lsp-lens-enable nil))

(use-package lsp-python-ms
  :after
  (lsp-mode)
  :init
  (setq lsp-python-ms-auto-install-server t))


;; disable lsp-ui
;; (use-package lsp-ui
;;   :after flycheck
;;   :commands lsp-ui-mode
;;   :config
;;   (setq lsp-ui-doc-enable nil)
;;   (setq lsp-ui-sideline-enable nil))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package dap-mode
  :defer t
  :config
  ;; pip install "ptvsd>=4.2"
  (require 'dap-python)
  (require 'dap-go)
  ;; dap-go-setup
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))
  )

;; -----------------------------

;; by default, you need to press M-RET to add a auto-numbering list
;; this will has some agenda mode binding..
(use-package evil-org
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  ;; disable org agenda key binding
  ;; (require 'evil-org-agenda)
  ;; (evil-org-agenda-set-keys)
  )

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

  ;; Set faces for heading levels
  (dolist (face '((org-document-title . 1.2)
                  (org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Source Code Pro" :weight 'regular :height (cdr face)))

  ;; NOTE:
  ;; (setq org-format-latex-options
  ;;        (list :foreground 'default
  ;;              :background 'default
  ;;              :scale 1.5
  ;;              :html-foreground "Black"
  ;;              :html-background "Transparent"
  ;;              :html-scale 1.0
  ;;              :matchers '("begin" "$1" "$" "$$" "\\(" "\\[")))

  ;; this will make org-shift to auto add timestamp after making a toto item complete
  (setq org-log-done 'time)
  (setq org-startup-truncated nil)
  (setq org-image-actual-width nil)
  (setq org-agenda-use-tag-inheritance nil)

  (setq org-startup-folded t)
  ;; (setq org-ellipsis " ▾")
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
          ("IN PROGRESS" . "#33eecc")
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

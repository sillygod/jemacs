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

(defconst my-home-dir "~/.mycraft.d")
(setq user-emacs-directory my-home-dir)

(setq package-user-dir (concat my-home-dir "/" "elpa"))
(setq mc/list-file (concat my-home-dir "/" "mc-lists.el"))

(setq frame-title-format "") ;; to disable show buffer name in the title bar
;; (force-mode-line-update) to update the frame title
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

(add-to-list 'default-frame-alist '(vertical-scroll-bars))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(left-fringe . 5))
(add-to-list 'default-frame-alist '(right-fringe . 5))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; (add-to-list 'default-frame-alist '(background-color . "#292b2e")) ;; this will be overwrite by doom-themes
(add-to-list 'default-frame-alist '(font . "Source Code Pro-15"))

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

(setq default-font-size 140)
(set-face-attribute 'fixed-pitch nil :font "Source Code Pro" :height default-font-size)
(set-face-attribute 'variable-pitch nil :font "Source Code Pro" :height default-font-size :weight 'regular)

(fset 'yes-or-no-p 'y-or-n-p) ;; to simplify the yes or no input

(setq inhibit-startup-message t)
(setq inhibit-compacting-font-caches t) ;; for all-the-icon slow issue

(setq column-number-mode t)

(setq make-backup-files nil)
(setq-default indent-tabs-mode nil)
(setq xwidget-webkit-enable-plugins t) ;; what does this impact?

(setq-default tab-width 4)

(when (string= system-type "darwin")
  "In macos, ls doesn't support --dired option"
  (setq dired-use-ls-dired nil))

(setq dired-dwim-target t)

(setq wdired-allow-to-change-permissions t)

(setq delete-by-moving-to-trash t)

(setq ffap-machine-p-known 'reject)

(setq epg-pinentry-mode 'loopback)
(setq epa-file-encrypt-to '("sillygod"))

(setq shell-command-switch "-c")

(setq word-wrap nil)

(global-auto-revert-mode t)

(global-visual-line-mode 1)

(defun system-is-mac! ()
  (eq system-type 'darwin))

(defun system-is-linux! ()
  (eq system-type 'gnu/linux))

(defun system-is-windows ()
  (eq system-type 'windows-nt))

(defmacro measure-time (&rest body)
  `(let ((time (current-time)))
     ,@body
     (message "%.06f s" (float-time (time-since time)))))


;; TODO: find a way to handle this better
(add-hook 'org-babel-pre-tangle-hook #'(lambda ()
                                         (setq-default evil-normal-state-entry-hook nil)
                                         (setq-default evil-insert-state-entry-hook nil)
                                         (setq-default evil-insert-state-exit-hook nil)
                                         (setq-default evil-emacs-state-entry-hook nil)))


(add-hook 'org-babel-post-tangle-hook #'(lambda ()
                                          (add-hook 'evil-normal-state-entry-hook 'im-use-eng)
                                          (add-hook 'evil-insert-state-entry-hook 'im-use-prev)
                                          (add-hook 'evil-insert-state-exit-hook 'im-remember)
                                          (add-hook 'evil-emacs-state-entry-hook 'im-use-eng)))

(defun measure-org-babel-tangle ()
  "A simple wrap to measure org-babel-tangle."
  (interactive)
  (when (fboundp 'profiler-stop)
    (profiler-stop))
  (profiler-start 'cpu+mem)
  (setq temp emacs-lisp-mode-hook)
  (setq-default emacs-lisp-mode-hook nil)
  (measure-time (org-babel-tangle))
  (setq-default emacs-lisp-mode-hook temp)
  (profiler-report))

(defcustom im-exec "/usr/local/bin/im-select"
  "The im executable binary path."
  :type 'string)

(defvar default-im "com.apple.keylayout.ABC"
  "Default English input method.")

(defvar prev-im ""
  "previous input method.")

(defvar current-im ""
  "Current input method.")

(defun im-use-eng ()
  "Switch to english input method."
  (interactive)
  (cond ((and (string= system-type "darwin")
              (not (string= current-im default-im)))
         (call-process-shell-command (concat im-exec " " default-im))
         (setq current-im default-im))))

(defun im-remember ()
  "Remember the input method being used in insert mode."
  (interactive)
  (cond ((string= system-type "darwin")
         (setq prev-im (substring (shell-command-to-string im-exec) 0 -1))
         (setq current-im prev-im))))

(defun im-use-prev ()
  "Change the input method to the previous one we remembered."
  (interactive)
  (cond ((string= system-type "darwin")
         (if prev-im
             (progn
               (call-process-shell-command (concat im-exec " " prev-im))
               (setq current-im prev-im))
           (progn
             (call-process-shell-command (concat im-exec " " default-im))
             (setq current-im default-im))))))

;; NOTE: borrow from spacemacs
(defun show-hide-helm-or-ivy-prompt-msg (msg sec)
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
               (not (string= prev-prompt-contents ""))))
         (when prev-prompt-contents-p
           (delete-region (line-beginning-position)
                          (line-end-position)))
         (insert (propertize msg 'face 'warning))
         ;; stop checking for candidates
         ;; and update the helm prompt
         (sit-for sec)
         (delete-region (line-beginning-position)
                        (line-end-position))
         (when prev-prompt-contents-p
           (insert prev-prompt-contents)
           ;; start checking for candidates
           ;; and update the helm prompt
           )))
   msg sec))

;; NOTE: borrow from spacemacs
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
                 (show-hide-helm-or-ivy-prompt-msg
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
                 (when (and (featurep 'projectile)
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

(defun docker-container-vterm (container &optional read-shell)
  "Open `shell' in CONTAINER.  When READ-SHELL is not nil, ask the user for it."
  (interactive (list
                (docker-container-read-name)
                current-prefix-arg))
  (let* ((shell-file-name (docker-container--read-shell read-shell))
         (container-address (format "docker:%s:/" container))
         (file-prefix (let ((prefix (file-remote-p default-directory)))
                        (if prefix
                            (format "%s|" (s-chop-suffix ":" prefix))
                          "/")))
         (default-directory (format "%s%s" file-prefix container-address)))
    (vterm-toggle-cd)))

;; (vterm-other-window (buffer-name (docker-generate-new-buffer "vterm" default-directory)))

(defun restart-emacs-procedure ()
  (call-process "bash"
                nil
                nil
                nil
                "-c"
                (concat
                 (elt command-line-args 0)
                 " -Q --load /Users/jing/Desktop/spacemacs-private/mycraft/init.el &")))


(defun restart-emacs ()
  "Kill the original instance and start a new emacs instance.
However, have no idea how to get the original instance' starting command args
sys.args?"
  (interactive)
  (add-to-list 'kill-emacs-hook #'restart-emacs-procedure)
  (print kill-emacs-hook)
  (save-buffers-kill-emacs))

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

(defvar pair-list nil) ;; a property list

(defun iterate-org-level (&optional input)
  (interactive)
  ;; we need to escape the space in the property
  ;; ex. (setq a '(:abc\ cde 1))
  (require 'epa-file)
  (require 'org-element)
  (with-temp-buffer
    (epa-file-insert-file-contents "~/Dropbox/myorgs/management/learning.org.gpg")
    (setq pair-list nil)
    (cl-loop for i from 0
             for ele in (org-element-parse-buffer 'headline)
             when (and (> i 0) (not (equal ele nil)))
             do (let* ((prop (plist-get ele 'headline))
                       (domain (plist-get prop :DOMAIN))
                       (title (plist-get prop :title))
                       (pass (plist-get prop :SECRET)))

                  (setq pair-list (plist-put pair-list (intern (message ":%s--%s" title domain)) pass))))
    (cl-loop for i from 0 for ele in pair-list
             when (cl-evenp i) collect ele)))


(defun get-se-action (x)
  (kill-new
   (base64-decode-string
    (decode-coding-string
     (plist-get pair-list (intern x)) 'utf-8)))
  (message "success"))

(defun get-secret ()
  (interactive)
  (ivy-read "choose: " (iterate-org-level)
            :action #'get-se-action
            :caller 'get-secret))

(defun org-insert-toc ()
  "Insert table of content for org mode."
  (interactive)
  (beginning-of-line)
  (insert "*" " " ":TOC:")
  (backward-char 5)
  (evil-insert-state))

(defun create-journal-to (dest)
  "~/Dropbox/myorgs/stock/journal"
  (let ((org-journal-dir dest))
    (call-interactively 'org-journal-new-entry)))

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

(defun now ()
  "Get the current time, In the future this will show a temp buffer
with unix format, human readable and the weather info."
  (interactive)
  (message "now: %s \ntimestamp: %s" (format-time-string "%Y-%m-%d %H:%m:%S %z") (format-time-string "%s")))

(defun evil-smart-doc-lookup ()
  "Run documentation lookup command specific to the major mode.
Use command bound to `SPC m h h` if defined, otherwise fall back
to `evil-lookup'"
  (interactive)
  (let ((binding (key-binding (kbd (concat "SPC" " mhh")))))

    (if (commandp binding)
        (call-interactively binding)
      (evil-lookup))))

(defun org-mode-visual-fill ()
  "A beautiful word wrap effect."
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust))

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
  "Define the leader key map for the specify mode.
key desc binding."
  (let ((get-props (lambda () (list
                               my-local-leader-def-emacs-state-prop
                               my-local-leader-def-prop
                               my-local-leader-def-alias-prop))))


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

(defun switch-to-minibuffer-window ()
  "Switch to minibuffer window (if active)."
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

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

(defun get-pull-request-uri ()
  (interactive)
  (save-excursion
    (with-current-buffer (magit-process-buffer t)
      (goto-char (point-max))
      (re-search-backward "^remote:\s*\\(https?://.*\\)+?$")
      ;; it seems that match-string-no-properties will get nothing if
      ;; you switch buffer after re-search-backward
      (let ((uri (match-string-no-properties 1)))
        (message "copy pull request: %s to clipboard" uri)
        (kill-new uri)))))

(defun my-emmet-expand ()
  (interactive)
  (unless (if (bound-and-true-p yas-minor-mode)
              (call-interactively 'emmet-expand-yas)
            (call-interactively 'emmet-expand-line))
    (indent-for-tab-command)))

(defun toggle-flycheck-error-list ()
  "Toggle flycheck's error list window.
If the error list is visible, hide it.  Otherwise, show it."
  (interactive)
  (-if-let (window (flycheck-get-error-list-window))
      (quit-window nil window)
    (flycheck-list-errors)))

(defun comment-or-uncomment-lines (&optional arg)
  (interactive "p")
  (evilnc-comment-or-uncomment-lines arg))

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
        (vterm-other-window buffer)))
    (pop-to-buffer buffer)))

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

(defun send-text-and-move-to-projectile-vterm ()
  (interactive)
  (when (region-active-p)
    ;; get the mark content
    (let ((content (buffer-substring (region-beginning) (region-end))))
      (new-terminal)
      (deactivate-mark)
      (vterm-send-string content))))

(defun vterm-perform-last-command ()
  (interactive)
  (new-terminal)
  (vterm-send-up)
  (vterm-send-return))

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
            (counsel--format-ag-command ignored "%s")))
         (initial-input (cond
                         ((use-region-p) (buffer-substring (region-beginning) (region-end)))
                         ((and (boundp 'ahs-current-overlay)
                               (not (eq ahs-current-overlay nil))) (buffer-substring (overlay-start ahs-current-overlay)
                               (overlay-end ahs-current-overlay)))
                         (t nil))))

    (when (region-active-p)
      (deactivate-mark))

    (ivy-add-actions
     'counsel-rg
     counsel-projectile-rg-extra-actions)

    (when (= (prefix-numeric-value current-prefix-arg) 4)
      (setq current-prefix-arg '(16)))

    (counsel-rg initial-input
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

(defun my-yaml-indent-line ()
  "Indent the current line.
The first time this command is used, the line will be indented to the
maximum sensible indentation.  Each immediately subsequent usage will
back-dent the line by `yaml-indent-offset' spaces.  On reaching column
0, it will cycle back to the maximum sensible indentation."
  (interactive "*")
  (let ((ci (current-indentation))
        (cc (current-column))
        (need (yaml-compute-indentation)))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      (if (and (equal last-command this-command) (/= ci 0))
          (indent-to (* (/ (- ci 1) yaml-indent-offset) yaml-indent-offset))
        (indent-to need)))))

(defvar python-run-command "python")
(defvar python-run-args "")

(defun workon-virtual-env-and-lsp ()
  (interactive)
  (poetry-venv-workon)
  (lsp-restart-workspace))

;; TODO: implement this one
(defun my-run-python ()
  "Use vterm to run python shell instead.
     Furthermore, using ipython instead if it's installed."
  (interactive)

  ;; create a vterm buffer with python shell
  ;; maybe, I can reference from the python-inferior-mode

  (if (featurep 'poetry)
      (vterm-send-string (poetry-virtualenv-path))
    (vterm-send-string "python"))
  (vterm-send-return))

(defun python-run-main ()
  (interactive)
  (shell-command
   (format (concat python-run-command " %s %s")
           (shell-quote-argument (or (file-remote-p (buffer-file-name (buffer-base-buffer)) 'localname)
                                     (buffer-file-name (buffer-base-buffer))))
           python-run-args)))

(defvar go-test-command "go test")

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


;; gopkgs
;; go-outline
;; gotests
;; gomodifytags
;; impl
;; goplay
;; dlv
;; staticcheck
;; gopls
;; write a helper for upgrade these tools
;; reinstall all the tools found in the golang's bin directory?

(defun org-roam-dailies-capture-and-goto-today (&optional goto)
  (interactive)
  (org-roam-dailies--capture (current-time) goto)
  (org-roam-dailies-capture-today t))

(defun reload-dir-locals ()
  (interactive)
  (hack-dir-local-variables-non-file-buffer))

(defun my-refresh-org-roam-db-cache ()
  (interactive)
  (reload-dir-locals)
  (org-roam-db-clear-all)
  (org-roam-db-sync))

(defun my-org-roam-ui-open ()
  (interactive)
  (let ((org-roam-ui-browser-function (if (equal current-prefix-arg '(4))
                                          #'browse-url
                                        #'xwidget-webkit-browse-url)))
    (orui-open)))

(setq straight-check-for-modifications nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Always use straight to install on systems other than Linux
(setq straight-use-package-by-default (not (eq system-type 'gnu/linux)))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;; Load the helper package for commands like `straight-x-clean-unused-repos'
(require 'straight-x)

(add-hook 'prog-mode-hook 'goto-address-prog-mode)

(add-hook 'prog-mode-hook #'(lambda () (setq indent-tabs-mode nil)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'dired-mode-hook 'auto-revert-mode)

(use-package winner
  :init
  (add-hook 'ediff-quit-hook 'winner-undo)
  :commands (winner-undo))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  (setq all-the-icons-dired-monochrome nil)
  (set-face-attribute 'all-the-icons-dired-dir-face nil :foreground "#FF8822"))

(use-package esup
  :defer t
  :init
  (setq esup-depth 0))

(use-package svg-lib
  :defer 1
  :straight (svg-lib :type git :host github :files ("*.el") :repo "rougier/svg-lib"))

(use-package diminish :defer t)

(use-package command-log-mode
  :commands command-log-mode)

(use-package rainbow-mode
  :defer t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-parentheses
  :hook (prog-mode . highlight-parentheses-mode))

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
  (setq all-the-icons-scale-factor 1.1)
  :custom
  (doom-modeline-height 12)
  (doom-modeline-persp-name nil))

(use-package all-the-icons
  :defer 0)

(use-package polymode
  :defer t)

(use-package poly-ansible
  :defer t)

(use-package devdocs
  :defer t
  :straight (
             :local-repo "~/Desktop/spacemacs-private/local/devdocs"))

(use-package counsel-jq-yq
  :defer 1
  :straight (
             :local-repo "~/Desktop/spacemacs-private/local/counsel-jq-yq"
             )
  ;; :load-path "~/Desktop/spacemacs-private/local/counsel-jq-yq"
  ;; :config
  ;; (package-generate-autoloads "counsel-jq-yq" "~/Desktop/spacemacs-private/local/counsel-jq-yq")
  ;; (load-library "counsel-jq-yq-autoloads"))
  )

(use-package gotests
  :defer 1
  :straight (
             :local-repo "~/Desktop/spacemacs-private/local/gotests"
             )
  ;; the file with suffix -test will be treated as tests files by default
  ;; which will function as package files.
  ;; :load-path "~/Desktop/spacemacs-private/local/go-test"
  ;; :config
  ;; (package-generate-autoloads "go-test" "~/Desktop/spacemacs-private/local/go-test")
  ;; (load-library "go-test-autoloads")
  )

(use-package hl-todo
  :defer t
  :hook
  ;; (text-mode . hl-todo-mode) text-mode is the parent of org-mode
  (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":")
  (setq hl-todo-keyword-faces
        `(
          ("TODO" warning bold)
          ("FIXME" error bold)
          ("HACK" font-lock-constant-face bold)
          ("NOTE" success bold)
          ("BUG" error bold)
          ("DEPRECATED" font-lock-doc-face bold))))

(use-package diff-hl
  :defer 1
  :init
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode))

(use-package so-long
  :defer 1
  :config
  (global-so-long-mode 1))

(use-package rime
  :defer 1
  :straight (rime :type git
                  :host github
                  :repo "DogLooksGood/emacs-rime"
                  :files ("*.el" "Makefile" "lib.c"))
  :custom
  (rime-librime-root (expand-file-name "librime/dist" user-emacs-directory))
  (rime-user-data-dir "/Users/jing/Library/Rime/")
  (rime-inline-ascii-trigger 'shift-l)
  (default-input-method "rime")
  (rime-show-candidate 'posframe)

  :config
  (setq rime-translate-keybindings
        '("C-f" "C-b" "C-n" "C-p" "C-g" "<left>" "<return>" "TAB" "<tab>" "<right>" "<up>" "<down>" "<prior>" "<next>" "<delete>"))
  (define-key rime-mode-map (kbd "C-'") 'rime-inline-ascii))

(use-package perspective
  :diminish persp-mode
  :commands (persp-switch)
  :custom
  (persp-modestring-short t)
  :config
  (persp-mode))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.05)
  (which-key-mode 1))

(use-package define-word
  :defer t)

(use-package powerthesaurus
  :defer t)

(use-package alert
  :commands alert
  :config
  (if (system-is-mac!)
      (setq alert-default-style 'osx-notifier)))

(use-package nov
  :defer t
  :mode ("\\.epub\\'" . nov-mode))

(use-package uuidgen
  :defer t)

(use-package docker
  :defer t)

(use-package docker-tramp
  :defer t)

(use-package dockerfile-mode
  :defer t)

(use-package nginx-mode
  :defer t)

(use-package jsonnet-mode
  :defer t)

(use-package conf-mode
  :defer t
  :mode ("poetry\\.lock" . conf-toml-mode))

(use-package jinja2-mode
  :defer t
  :init
  (add-hook 'jinja2-mode-hook
            #'(lambda ()
                (set (make-local-variable 'indent-line-function) 'insert-tab)))
  :mode ("\\.j2\\'" . jinja2-mode))

(use-package make-mode
  :defer t
  :init
  (add-hook 'makefile-mode-hook
            #'(lambda ()
                (setq-local indent-tabs-mode t))))

(use-package racket-mode
  :defer t)

(use-package smartparens
  :defer 0
  :commands (smartparens-mode)
  :config
  (require 'smartparens-config)
  (add-hook 'js-mode-hook #'smartparens-mode)
  (add-hook 'go-mode-hook #'smartparens-mode)
  (add-hook 'html-mode-hook #'smartparens-mode)
  (add-hook 'python-mode-hook #'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode))

(use-package yasnippet
  :defer 1
  :config
  (add-to-list 'yas-snippet-dirs "/Users/jing/Desktop/spacemacs-private/snippets")
  ;; (yas-global-mode 1)
  (yas-minor-mode 1))

(use-package yasnippet-snippets
  :defer t
  :after yasnippet)

(use-package ivy-yasnippet
  :defer t
  :after yaanippet)

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package expand-region
  :commands
  (er--expand-region-1)
  :defer t)

(use-package winum
  :defer 0
  :config
  (winum-mode))

(use-package systemd
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

(use-package js2-mode
  :after (rainbow-delimiters)
  :defer t
  :config
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (js2-minor-mode))

(use-package flycheck
  :commands (flycheck-mode)
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (add-hook 'text-mode-hook 'flycheck-mode)
  (setq flycheck-highlighting-mode 'lines)
  (setq flycheck-indication-mode '())
  :config
  (add-hook 'org-src-mode-hook #'(lambda ()
                                  (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))))

(use-package json-mode
  :defer t)

(use-package yaml-mode
  :defer t
  :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
         ("Procfile\\'" . yaml-mode))
  :init
  (add-hook 'yaml-mode-hook 'lsp)
  (add-hook 'yaml-mode-hook #'(lambda ()
                                (set (make-local-variable 'tab-width) 2)
                                (set (make-local-variable 'evil-shift-width) 2)
                                (set (make-local-variable 'indent-line-function) 'my-yaml-indent-line)))
  :config
  ;; (with-eval-after-load 'evil
  ;;   (evil-define-key 'normal yaml-mode-map (kbd "=") 'yaml-indent-line))
  (with-eval-after-load 'flycheck
    (when (listp flycheck-global-modes)
      (add-to-list 'flycheck-global-modes 'yaml-mode))))

(use-package json-snatcher
  :defer t)

(use-package cmake-mode
  :defer t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode)))

(use-package lua-mode
  :mode ("\\.lua\\'" . lua-mode)
  :defer t)

(use-package slime
  :defer t
  :init
  (setq inferior-lisp-program "sbcl"))

(use-package elisp-slime-nav
  :defer t
  :init
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'elisp-slime-nav-mode)))

(use-package lispy
  :init
  (setq lispy-key-theme '(special c-digits))
  :hook ((common-lisp-mode . lispy-mode)
         (emacs-lisp-mode . lispy-mode)
         (scheme-mode . lispy-mode))
  :config
  (with-eval-after-load 'evil-matchit
    (lispy-define-key lispy-mode-map (kbd "%") 'lispy-different)
    (lispy-define-key lispy-mode-map (kbd "d") 'lispy-kill)))

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

(use-package gomacro-mode
  :hook (go-mode . gomacro-mode))

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
  :defer t
  :init
  (add-hook 'before-save-hook 'pyimport-remove-unused))

(use-package cython-mode
  :defer t)

(use-package dumb-jump
  :init
  (setq dumb-jump-selector 'ivy)
  :defer t)

(use-package lsp-mode
  :init
  (setq lsp-completion-provider :capf) ;; the official recommends use this
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-signature-render-documentation nil)
  (setq read-process-output-max (* 1024 1024))
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  :commands
  (lsp)
  :hook
  (go-mode . lsp)
  (python-mode . lsp)
  (lua-mode . lsp)
  (rust-mode . lsp)
  (js-mode . lsp)
  (c-mode . lsp)
  (c++-mode . lsp)
  :config
  ;; turn off lens mode
  (setq lsp-lens-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-snippet nil)
  (setq lsp-enable-imenu nil)
  (setq lsp-enable-links nil))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package dap-mode
  :defer t
  :config
  ;; pip install "ptvsd>=4.2"
  (require 'dap-python)
  (require 'dap-go)
  ;; dap-go-setup
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra))))

(use-package lsp-pyright
  :defer t
  :custom ((lsp-pyright-multi-root nil))
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

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
  ;; (setq ivy-dynamic-exhibit-delay-ms 250)
  (setq ivy-initial-inputs-alist nil)
  (with-eval-after-load 'evil
    (define-key ivy-occur-grep-mode-map (kbd "w") nil)
    (evil-define-key 'normal ivy-occur-grep-mode-map
      (kbd "i")
      #'(lambda ()
          (interactive)
          (ivy-wgrep-change-to-wgrep-mode)
          (evil-insert-state)))))

(use-package ivy-rich
  :after (ivy)
  :init
  (ivy-rich-mode 1))

(use-package swiper
  :bind (("C-s" . swiper)))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-w" . 'ivy-backward-kill-word)
         :map ivy-minibuffer-map
         ("C-w" . 'ivy-backward-kill-word)
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq counsel-find-file-at-point t))

;; counsel-search will use the package request with this function
(use-package request
  :defer t)

(use-package projectile
  :defer 1
  :custom ((projectile-completion-system 'ivy))
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (add-to-list 'projectile-project-root-files-bottom-up "pyproject.toml")
  (projectile-mode +1))

(use-package counsel-projectile
  :after projectile
  :defer 1
  :config (counsel-projectile-mode))

(use-package avy
  :defer t
  :config
  (setq avy-background t))

(use-package vterm
  :defer t
  :init
  (setq vterm-always-compile-module t)
  (setq vterm-timer-delay 0.01)
  (with-eval-after-load 'evil
    (evil-set-initial-state 'vterm-mode 'emacs))
  :config
  (define-key vterm-mode-map (kbd "<escape>") 'vterm-send-escape))

(use-package vterm-toggle
  :defer t)

(use-package ediff
  :defer t
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally))

(use-package evil
  :defer 0
  :init
  (setq evil-want-keybinding nil) ;; this will cause some evil keybinding
  ;; of other modes not working when it's set to true
  :config
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-define-key 'normal prog-mode-map (kbd "C-j") 'evil-scroll-line-down)
  (evil-define-key 'normal prog-mode-map (kbd "C-k") 'evil-scroll-line-up)
  (evil-define-key 'normal prog-mode-map (kbd "g h") 'flycheck-display-error-at-point)
  (evil-define-key 'normal prog-mode-map (kbd "U") 'undo-redo)


  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)

  (add-hook 'evil-normal-state-entry-hook 'im-use-eng)
  (add-hook 'evil-insert-state-entry-hook 'im-use-prev)
  (add-hook 'evil-insert-state-exit-hook 'im-remember)
  (add-hook 'evil-emacs-state-entry-hook 'im-use-eng))

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
  (delete 'view evil-collection-mode-list)
  ;; this will bind a global esc key for minibuffer-keyboard-quit so I remove it.
  (setq evil-collection-company-use-tng nil)
  (add-hook 'evil-collection-setup-hook #'(lambda (_mode mode-keymaps &rest _rest)
                                            (when (eq _mode 'docker)
                                              (evil-define-key 'normal 'docker-container-mode-map (kbd "b") 'docker-container-vterm))))
  (evil-collection-init))

(use-package evil-nerd-commenter
  :after evil
  :commands evilnc-comment-operator
  :init
  (define-key evil-normal-state-map "gc" 'evilnc-comment-operator))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

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

(use-package hideshow
  :commands
  (hs-toggle-hiding
   hs-hide-block
   hs-hide-level
   hs-show-all
   hs-hide-all))

(use-package vimish-fold
  :after evil
  :hook (prog-mode . vimish-fold-mode))

(use-package evil-vimish-fold
  :after vimish-fold
  :hook (prog-mode . evil-vimish-fold-mode))

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

(use-package iedit
  :commands
  (iedit-restrict-region)
  :config
  (define-key iedit-occurrence-keymap-default
    (kbd "<escape>") #'(lambda () (interactive) (iedit-mode -1))))

(use-package git-messenger
  :defer t
  :init
  (setq git-messenger:show-detail t)
  (setq git-messenger:use-magit-popup t))

(use-package magit
  :defer 2
  :custom
  ((magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
   (magit-repository-directories
    '(("~/Desktop/data_platform" . 1)
      ("~/Desktop/cdp-cache" . 1)
      ("~/Desktop/ansible-playground" . 1)
      ("~/Desktop/go-playground" . 1)
      ("~/Desktop/spacemacs-private" . 1)))))

(use-package forge
  :defer 2
  :after magit)

(use-package auto-highlight-symbol
  :commands
  (ahs-forward
   ahs-unhighlight
   ahs-unhighlight-all
   ahs-change-range
   ahs-change-range-internal
   ahs-dropdown-list-p
   ash-backward)
  :config
  (setq ahs-case-fold-search nil)
  (add-to-list 'ahs-plugin-bod-modes 'python-mode))

(use-package general
  :after (which-key evil)
  :config
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

  (with-eval-after-load 'org-capture
    (evil-define-key 'normal org-capture-mode-map
      (kbd ", ,") 'org-capture-finalize
      (kbd ", k") 'org-capture-kill
      (kbd ", w") 'org-capture-refile))

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


  ;; keybinding for racket-mode
  (with-eval-after-load 'racket-mode
    (define-leader-key-map-for 'racket-mode
      "" "major mode" nil
      "x" "execute" nil
      "xx" "racket run" 'racket-run))

  ;; lsp keybindings for some major modes
  (with-eval-after-load 'lsp-mode

    ;; keybinding for go-mode
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

    ;; keybinding for python-mode
    (with-eval-after-load 'python
      (apply 'define-leader-key-map-for 'python-mode-map
             (lsp-keybinding))

      (apply 'define-leader-key-map-for
             (list 'python-mode-map
                   "" "major mode" 'nil

                   "t" "tests" 'python-pytest-dispatch
                   "x" "execute" nil
                   "xx" "python run" 'python-run-main
                   "v" "workon env" 'workon-virtual-env-and-lsp
                   "d" "debug" 'dap-hydra))
      (evil-define-key 'normal python-mode-map (kbd "K") 'evil-smart-doc-lookup))

    (with-eval-after-load 'json-mode
      (define-leader-key-map-for 'json-mode-map
        "" "major mode" nil
        "l"  "lookup" nil
        "ll" "snatch path" 'jsons-print-path
        "lj" "jq" 'counsel-jq))


    (with-eval-after-load 'yaml-mode
      (define-leader-key-map-for 'yaml-mode-map
        "" "major mode" nil
        "l" "lookup" nil
        "ly" "yq" 'counsel-yq))

    ;; keybinding fro c, c++ mode
    (with-eval-after-load 'cc-mode
      (apply 'define-leader-key-map-for 'c-mode-map (lsp-keybinding))
      (apply 'define-leader-key-map-for 'c++-mode-map (lsp-keybinding))))

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

      "b" "babel" nil
      "bt" "tangle" 'org-babel-tangle

      "i" "insert" nil
      "il" "insert link" 'org-insert-link
      "it" "insert toc" 'org-insert-toc

      "e" "export" nil
      "ee" "org-export-dispatch" 'org-export-dispatch

      "n" "narrow" nil
      "ns" "narrow subtree" 'org-narrow-to-subtree
      "nN" "widen" 'widen

      "r" "org roam hydra" 'hydra-org-roam/body

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
      "jn" "new entry" 'org-journal-new-entry
      "js" "new stock entry" #'(lambda () (interactive) (create-journal-to "~/Dropbox/myorgs/stock/journal"))))


  (define-leader-key-global
    "SPC" 'counsel-M-x
    "/" 'my-counsel-projectile-rg
    "v" 'er/expand-region
    "u" 'universal-argument
    "'" 'new-terminal
    "TAB" 'vterm-perform-last-command
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
    "jw" '(avy-goto-char-timer :which-key "avy goto words")
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

    "ad" '(docker t :which-key "docker")

    "al" '(:ignore t :which-key "lookup/dictionary")
    "ald" '(define-word :which-key "lookup definition")
    "alg" '(google-search :which-key "google search")
    "alx" '(open-with-xwidget :which-key "open with xwidget")

    "ao" '(:ignore t :which-key "org")
    "aor" '(hydra-org-roam/body :which-key "org-roam-hydra")
    "aog" '(:ignore t :which-key "goto")
    "aogj" '((lambda () (interactive) (counsel-find-file (expand-file-name "~/Dropbox/myorgs/journal"))) :which-key "journal note")
    "aogt" '((lambda () (interactive) (org-file-show-headings "~/Dropbox/myorgs/life_books_courses_programming/todo.org")) :which-key "todo note"))

  (define-leader-key-global
    "b" '(:ignore t :which-key "buffer")
    "bb" '(counsel-projectile-switch-to-buffer :which-key "project-list-buffer")
    "bd" '(kill-this-buffer :which-key "kill-buffer")
    "bB" '(counsel-switch-buffer :which-key "list-buffer")
    "bi" '(ibuffer :which-key "ibuffer")
    "bn" '(next-buffer :which-key "next-buffer")
    "bp" '(previous-buffer :which-key "previous-buffer")
    "bN" '(new-empty-buffer :which-key "new empty buffer")
    "b." '(buffer-operate/body :which-key "buffer transient"))

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
    "ll" '(persp-switch :which-key "switch layout")
    "lr" '(persp-rename :which-key "rename layout")
    "ld" '(persp-kill :which-key "delete layout")
    "lb" '(persp-switch-to-buffer* :which-key "persp buffer list"))

  (define-leader-key-global
    "n" '(:ignore t :which-key "narrow")
    "nf" '(narrow-to-defun :which-key "narrow to defun")
    "nw" '(widen :which-key "widen"))

  (define-leader-key-global
    "p" '(:ignore t :which-key "project")
    "pp" '((lambda () (interactive) (counsel-projectile-switch-project 2)) :which-key "switch project")
    "pf" '(counsel-projectile-find-file :which-key "find-file"))

  (define-leader-key-global
    "s" '(:ignore t :which-key "search")
    "sc" '((lambda () (interactive) (evil-ex-nohighlight)(ahs-clear)) :which-key "clear highlight")
    "ss" '(swiper :which-key "swiper")
    "sS" '(swiper-all :which-key "swiper-all"))

  (define-leader-key-global
    "g" '(:ignore t :which-key "git")
    "gi" '(magit-init :which-key "gagit init")
    "gb" '(:ignore t :which-key "blame")
    "gl" '(magit-list-repositories :which-key "magit list repos")
    "gbl" '(git-messenger:popup-message  :which-key "this line")
    "gbb" '(magit-blame-addition  :which-key "this buffer")
    "gs" '(magit-status :which-key "magit status"))

  (define-leader-key-global
    "k" '(:ignore t :which-key "kmacro")
    "ks" '(kmacro-start-macro-or-insert-counter :which-key "start macro/insert counter")
    "ke" '(kmacro-end-or-call-macro :which-key "end or run record")
    "kv" '(kmacro-view-macro-repeat :which-key "view last macro")
    "kn" '(kmacro-name-last-macro :which-key "name the last kmacro"))

  (define-leader-key-global
    "q" '(:ignore t :which-key "quit")
    "qq" '(save-buffers-kill-emacs :which-key "quit with saving buffer")
    "qr" '(restart-emacs :which-key "restart"))

  (define-leader-key-global
    "t"  '(:ignore t :which-key "toggles")
    "tm" '(hydra-mode-toggle/body :which-key "toggle mode")
    "ti" '(toggle-input-method :which-key "toggle input method")
    "tv" '(visual-fill-column-mode :which-key "visual fill column mode")
    "ts" '(hydra-text-scale/body :which-key "scale text"))

  (define-leader-key-global
    "w" '(:ignore t :which-key "windows")
    "wf" '(toggle-frame-fullscreen :which-key "toggle fullscreen")
    "ww" '(other-window :which-key "other-window")
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

    "w=" '(balance-windows :which-key "balance")
    "w[" '(my-shrink-window-horizontally :which-key "shrink h")
    "w]" '(my-enlarge-window-horizontally :which-key "enlarge h")
    "w{" '(my-shrink-window :which-key: "shrink v")
    "w}" '(my-enlarge-window :which-key: "enlarge v")

    "wF" '(make-frame :which-key "make frame")
    "wD" '(delete-frame :which-key "delete frame")
    "wo" '(other-frame :which-key "other frame")
    "w." '(window-operate/body :which-key "window transient"))

  (define-leader-key-global
    "x" '(:ignore t :which-key "texts")
    "xc" '(count-words-region :which-key "count-words-region")

    "xb" '(:ignore t :which-key "base64")
    "xbe" '(my-encode-region-base64 :which-key "base64-encode-region")
    "xbd" '(my-decode-region-base64 :which-key "base64-decode-region")

    "xs" '(send-text-and-move-to-projectile-vterm :which-key "send content to and focus on vterm"))

  (define-leader-key-global
    "f" '(:ignore t :which-key "files")
    "fe" '(:ignore t :which-key "emacs")
    "fed" '(my-find-dotfile :which-key "open config dotfile")
    "fy" '(copy-file-path :which-key "copy file path")
    "fd" '(dired-jump :which-key "dired")
    "fs" '(save-buffer :which-key "save file")
    "fr" '(rename-current-buffer-file :which-key "rename file")
    "ff" '(counsel-find-file :which-key "find file"))

  (message "DEBUG: !! complete general setting"))

(use-package hydra
  :defer t)

(defhydra window-operate ()
  "
Window management :)
^Resize^                ^select^                         ^Move^          ^Action^
^───────────────^       ^────────^                       ^────────^      ^────────^
[_[_] : shrink h        [_h_]: left                      [_H_]: left       [_/_]: split vertically
[_]_] : enlarge h       [_l_]: right                     [_L_]: right      [_-_]: split horizontally
[_{_] : shrink v        [_k_]: up                        [_K_]: up         [_d_]: delete window
[_}_] : enlarge v       [_j_]: down                      [_J_]: down
[_=_] : balance         [_1_.._9_]: window 1..9
[_m_] : window maximize
"
  ("[" my-shrink-window-horizontally nil)
  ("]" my-enlarge-window-horizontally nil)
  ("{" my-shrink-window nil)
  ("}" my-enlarge-window nil)
  ("=" balance-windows nil)
  ("d" delete-window nil)
  ("m" toggle-maximize-buffer nil)
  ("h" evil-window-left nil)
  ("l" evil-window-right nil)
  ("k" evil-window-up nil)
  ("j" evil-window-down nil)
  ("r" rotate-windows-forward nil)
  ("L" evil-window-move-far-right nil)
  ("H" evil-window-move-far-left nil)
  ("J" evil-window-move-very-bottom nil)
  ("K" evil-window-move-very-top nil)
  ("/" evil-window-vsplit nil)
  ("-" evil-window-split nil)

  ("1" winum-select-window-1 nil)
  ("2" winum-select-window-2 nil)
  ("3" winum-select-window-3 nil)
  ("4" winum-select-window-4 nil)
  ("5" winum-select-window-5 nil)
  ("6" winum-select-window-6 nil)
  ("7" winum-select-window-7 nil)
  ("8" winum-select-window-8 nil)
  ("9" winum-select-window-9 nil))

(defhydra hydra-org-roam ()
  "Launcher for `org-roam'."
  ("c" org-id-get-create "create node")
  ("i" org-roam-node-insert "insert")
  ("f" org-roam-node-find "find file")
  ("d" org-roam-dailies-goto-today "dailies")
  ("l" org-roam-buffer-toggle "back link buffer")
  ("g" my-org-roam-ui-open "graph")
  ("r" my-refresh-org-roam-db-cache "db refresh")
  ("t" org-roam-tag-add "add tag"))


(defhydra buffer-operate ()
  "
buffer management :)
^Move^                         ^action^
^────────^                     ^───────^
[_n_] : next buffer            [_d_] : delete
[_p_] : prev buffer
[_b_] : project buffers
[_B_] : buffers list
[_o_] : other window
"

  ("n" next-buffer nil)
  ("p" previous-buffer nil)
  ("b" counsel-projectile-switch-to-buffer nil)
  ("B" counsel-switch-buffer nil)
  ("o" other-window nil)
  ("d" kill-this-buffer nil))

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

(defhydra hydra-table-mode ()
  "table-mode edit shortcut"
  ("+" table-insert-row-column "insert new row/column")
  ("*" table-span-cell "merge cell")
  (":" table-justify "align cell")
  ("-" table-split-cell-vertically "cell split horizontally")
  ("/" table-split-cell-horizontally "cell split vertically")
  ("<" table-narrow-cell "narrow cell")
  (">" table-widen-cell "widen cell")
  ("{" table-shorten-cell "shorten cell")
  ("}" table-heighten-cell "heighten cell"))

(setq ahs-default-range 'ahs-range-whole-buffer)

(defun my-ahs-highlight-p ()
  "Ruturn Non-nil if symbols can be highlighted."
  (interactive)
  (let* ((beg (if (region-active-p) (region-beginning) (overlay-start ahs-current-overlay)))
         (end (if (region-active-p) (region-end) (overlay-end ahs-current-overlay)))
         (face (get-text-property beg 'face))
         (symbol (buffer-substring beg end)))

    (ahs-unhighlight t)
    (when (and symbol
               (not (ahs-dropdown-list-p))
               ;; (not (ahs-face-p (ahs-add-overlay-face beg face) 'ahs-inhibit-face-list))
               ;; disable skip highlight for some font-face
               (not (ahs-symbol-p ahs-exclude symbol t))
               (ahs-symbol-p ahs-include symbol))
      (list symbol beg end))))

(defun my-ahs-search-symbol (symbol search-range)
  "Search `SYMBOL' in `SEARCH-RANGE'."
  (save-excursion
    (let ((case-fold-search ahs-case-fold-search)
          ;; (regexp (concat "\\_<\\(" (regexp-quote symbol) "\\)\\_>" ))
          (regexp (regexp-quote symbol))
          (beg (car search-range))
          (end (cdr search-range)))
      (goto-char end)
      (while (re-search-backward regexp beg t)
        (let* ((symbol-beg (match-beginning 0))
               (symbol-end (match-end 0))
               (tprop (text-properties-at symbol-beg))
               (face (cadr (memq 'face tprop)))
               (fontified (cadr (memq 'fontified tprop))))
          (unless (or face fontified)
            (setq ahs-need-fontify t))
          (push (list symbol-beg
                      symbol-end
                      face fontified) ahs-search-work))))))


(defun my-ahs-light-up (current)
  "Light up symbols."
  (cl-loop for symbol in ahs-search-work

           for beg = (nth 0 symbol)
           for end = (nth 1 symbol)
           for face = (or (nth 2 symbol)
                          (get-text-property beg 'face))
           for face = (ahs-add-overlay-face beg face)

           do (let ((overlay (make-overlay beg end nil nil t)))
                (overlay-put overlay 'ahs-symbol t)
                (overlay-put overlay 'window (selected-window))
                (overlay-put overlay 'face
                             (if (ahs-face-p face 'ahs-definition-face-list)
                                 (if current ahs-definition-face
                                   ahs-definition-face-unfocused)
                               (if current ahs-face ahs-face-unfocused)))
                (push overlay ahs-overlay-list))))

(advice-add 'ahs-light-up :override #'my-ahs-light-up)
(advice-add 'ahs-highlight-p :override #'my-ahs-highlight-p)
(advice-add 'ahs-search-symbol :override #'my-ahs-search-symbol)

(defun expand-and-highlight-region ()
  (interactive)
  (er--expand-region-1)
  (highlight-region))

(defun contract-and-highlight-region ()
  (interactive)
  (call-interactively 'er/contract-region)
  (highlight-region))


(defun highlight-region ()
  (interactive)
  (let ((hh (my-ahs-highlight-p)))
    (unless ahs-current-range
      (ahs-change-range-internal ahs-default-range))
    (when hh
      (ahs-highlight (nth 0 hh)
                     (nth 1 hh)
                     (nth 2 hh)))))


(defun evil-surround-region-utils (operation)
  ;; TODO: implement this one
  (interactive (evil-surround-interactive-setup))
  ;; (cond
  ;;  ((eq operation 'change)
  ;;   (call-interactively 'evil-surround-change))
  ;;  ((eq operation 'delete)
  ;;   (call-interactively 'evil-surround-delete))
  ;;  (t
  ;;   (evil-surround-setup-surround-line-operators)
  ;;   (evil-surround-call-with-repeat 'evil-surround-region))))

  (if (region-active-p)
      (evil-surround-setup-surround-line-operators)
    (evil-surround-call-with-repeat 'evil-surround-region)))


(defhydra mark-operation ()
  "\nSwift knife %s(propertize (format \" %s \" (ahs-current-plugin-prop 'name)) 'face  (ahs-current-plugin-prop 'face))

^match^                   ^Search^                       ^edit^                        ^operation^
^^^─────────────────────────────────────────────────────────────────────────────────────────────────────────
[_v_]: expand             [_s_]: swiper                  [_e_]: iedit                  [_t_]: send to vterm
[_-_]: contract           [_/_]: counsel-projectile-rg   [_h_]: highlight
[_r_]: range              ^ ^                            [_c_]: change surround
[_n_]: next
[_N_]: prev
[_<escape>_]: quit
"

  ("<escape>" (lambda ()(interactive) (ahs-unhighlight-all t)) nil :exit t)
  ("v" expand-and-highlight-region nil)
  ("-" contract-and-highlight-region nil)
  ;; counsel-projectile-rg-initial-input
  ("s" swiper-thing-at-point nil)
  ("c" evil-surround-region nil)
  ("/" my-counsel-projectile-rg nil)
  ("e" my-iedit-mode nil :exit t)
  ("h" highlight-region nil)
  ("r" my-change-range nil)
  ("t" send-text-and-move-to-projectile-vterm nil :exit t)
  ("n" my-ahs-forward nil)
  ("N" my-ahs-backward nil))

(defun my-iedit-mode ()
  (interactive)
  (ahs-unhighlight-all)
  (call-interactively 'iedit-mode)
  (iedit-restrict-region
   (ahs-current-plugin-prop 'start)
   (ahs-current-plugin-prop 'end)))

(defun my-change-range ()
  (interactive)
  (setq range (ahs-runnable-plugins t))
  (ahs-change-range-internal range)
  (if ahs-current-overlay
      (highlight-region))
  (iedit-restrict-region
   (ahs-current-plugin-prop 'start)
   (ahs-current-plugin-prop 'end)))

(defun my-ahs-forward ()
  (interactive)
  (when (region-active-p)
    (deactivate-mark))
  (ahs-forward))

(defun my-ahs-backward ()
  (interactive)
  (when (region-active-p)
    (deactivate-mark))
  (ahs-backward))

(with-eval-after-load 'auto-highlight-symbol
  (add-to-list 'ahs-unhighlight-allowed-commands 'mark-operation/my-change-range)
  (add-to-list 'ahs-unhighlight-allowed-commands 'mark-operation/my-ahs-backward)
  (add-to-list 'ahs-unhighlight-allowed-commands 'mark-operation/my-counsel-projectile-rg)
  (add-to-list 'ahs-unhighlight-allowed-commands 'mark-operation/my-ahs-forward))

(defun wrap-mark-operation ()
  (interactive)
  (unless (region-active-p)
    (er--expand-region-1))
  (highlight-region)
  (mark-operation/body))

(with-eval-after-load 'evil
  (evil-define-key '(normal motion) 'evil-motion-state-map
    (kbd "*") 'wrap-mark-operation))

(use-package company
  :defer 0
  :config
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.1)
  (setq company-format-margin-function 'company-vscode-dark-icons-margin)
  ;; In evil-collection, it adjust the key binding for the company-mode
  ;; NOTE: Furthermore, it also disable the pre-select behavior when
  ;; showing the completion candidates.
  (define-key company-active-map (kbd "<return>") 'company-complete-selection)
  (global-company-mode 1))

(use-package org-ql
  :defer t)

(use-package org-drill
  :defer t)

(use-package org
  :defer t
  :init
  ;; cool! some functions need to be enable
  ;; like <s press tab to complete org structure (org-tempo)
  ;; these variables can be found in the source code of org.el
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


  (setq-default safe-local-variable-values
                '((org-reveal-ignore-speaker-notes . (lambda (x) t))
                  (org-confirm-babel-evaluate . (lambda (x) t))
                  (eval . (progn
                            (when
                                (and (not (equal buffer-file-name nil))
                                     (equal
                                      (string-match
                                       (regexp-quote org-roam-directory)
                                       (regexp-quote buffer-file-name))
                                      nil))
                              (setq org-roam-directory (locate-dominating-file default-directory ".dir-locals.el"))
                              (setq org-roam-db-location (concat org-roam-directory "org-roam.db")))))
                  (org-export-babel-evaluate . (lambda (x) t))))
  :config
  (keymap-unset org-mode-map "C-'" t)
  (setq org-export-backends '(ascii html icalendar latex odt md))

  ;; this will make org-shift to auto add timestamp after making a toto item complete
  (setq org-log-done 'time)
  (setq org-startup-truncated nil)
  (setq org-image-actual-width nil)
  (setq org-src-window-setup 'current-window) ;; org-edit-src without prompting window
  (setq org-agenda-use-tag-inheritance nil)

  (setq org-startup-folded t)
  ;; (setq org-ellipsis " ▾")
  (setq org-startup-with-inline-images t)
  (setq-default org-default-notes-file
                "~/Dropbox/myorgs/todo.org")


  :ensure org-contrib)

(use-package org-tree-slide
  :defer t
  :custom
  (org-image-actual-iwth nil))

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

(defun presentation-setup ()
  ;; Scale the text.  The next line is for basic scaling:
  (with-eval-after-load 'face-remap
    (setq text-scale-mode-amount 3)
    (text-scale-mode 1)))

(defun presentation-end ()
  ;; Show the mode line again
  (setq text-scale-mode-amount 0)
  (text-scale-mode 0))


(use-package org-tree-slide
  :defer t
  :hook ((org-tree-slide-play . presentation-setup)
         (org-tree-slide-stop . presentation-end))
  :custom
  (org-tree-slide-slide-in-effect t)
  (org-tree-slide-header t)
  (org-tree-slide-breadcrumbs " > ")
  (org-image-actual-iwth nil))

(use-package visual-fill-column
  :init
  (setq visual-fill-column-width 150)
  (setq visual-fill-column-center-text t)
  :hook (org-mode . org-mode-visual-fill))

(use-package htmlize
  :defer t)

(use-package org-roam
  :straight
  (:host github :repo "org-roam/org-roam" :files (:defaults "extensions/*"))
  :after org
  :custom
  (org-roam-directory "/Users/jing/Dropbox/myorgs/life_books_courses_programming")
  (org-roam-db-location (concat org-roam-directory "org-roam.db"))
  :init
  (setq org-roam-v2-ack t)
  :config
  (setq org-roam-node-display-template (concat
                                        "${title:100}" (propertize "${tags:30}" 'face 'org-tag)))
  (setq org-roam-dailies-capture-templates
        `(("d" "default" entry
           "* %<%H:%M> %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+filetags: :daily:"))))
  (setq org-roam-capture-templates
        `(("d" "default" plain "%?" :target
           (file+head "${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)))
  (setq org-roam-dailies-directory "journal/")
  (org-roam-db-autosync-enable))


(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start nil))

(with-eval-after-load 'counsel
  (defun org-roam-todo ()
    "An ad-hoc agenda for `org-roam'."
    (interactive)
    (let* ((regex "^\\* TODO")
           (b (get-buffer (concat "*ivy-occur counsel-rg \"" regex "\"*"))))
      (if b
          (progn
            (switch-to-buffer b)
            (ivy-occur-revert-buffer))
        (setq unread-command-events (listify-key-sequence (kbd "C-c C-o M->")))
        (counsel-rg regex org-roam-directory "--sort modified")))))


(defun ora-org-roam-find-file-action (x)
  (if (consp x)
      (let ((file-path (plist-get (cdr x) :path)))
        (org-roam--find-file file-path))
    (let* ((title-with-tags x)
           (org-roam-capture--info
            `((title . ,title-with-tags)
              (slug . ,(funcall org-roam-title-to-slug-function title-with-tags))))
           (org-roam-capture--context 'title))
      (setq org-roam-capture-additional-template-props (list :finalize 'find-file))
      (org-roam-capture--capture))))

(defun ora-org-roam-find-file ()
  (interactive)
  (unless org-roam-mode (org-roam-mode))
  (ivy-read "File: " (org-roam--get-title-path-completions)
            :action #'ora-org-roam-find-file-action
            :caller 'ora-org-roam-find-file))

(use-package ob-async
  :defer t)

(use-package toc-org
  :defer t
  :init
  (add-hook 'org-mode-hook 'toc-org-mode)
  (add-hook 'markdown-mode-hook 'toc-org-mode)
  :commands (toc-org-insert-toc))

(use-package evil-org
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme))))

(use-package restclient
  :defer t)

(use-package ob-restclient
  :defer t
  :after (org restclient)
  :init (add-to-list 'org-babel-load-languages '(restclient . t)))

(with-eval-after-load 'org
  (defcustom org-html-tableel-org "no"
    "Export table.el cells as org code if set to \"t\" or \"yes\".
This is the default and can be changed per section with export option:
#+OPTIONS: HTML_TABLEEL_ORG: t"
    :type '(choice (const "no") (const "yes"))
    :group 'org-html)

  (eval-after-load 'ox-html
    '(eval ;;< Avoid eager macro expansion before ox-html is loaded.
      '(cl-pushnew
        (list
         :html-tableel-org
         "HTML_TABLEEL_ORG" ;; keyword
         "HTML_TABLEEL_ORG" ;; option for #+OPTIONS: line
         org-html-tableel-org ;; default value for the property
         t ;; handling of multiple keywords for the same property. (Replace old value with new one.)
         )
        (org-export-backend-options (org-export-get-backend 'html)))))

  (defvar org-element-all-elements) ;; defined in "org-element"
  (defun table-generate-orghtml-cell-contents (dest-buffer language cell info)
    "Generate and insert source cell contents of a CELL into DEST-BUFFER.
LANGUAGE must be 'orghtml."
    (cl-assert (eq language 'html) nil
               "Table cells with org content only working with html export")
    (let* ((cell-contents (extract-rectangle (car cell) (cdr cell)))
           (string (with-temp-buffer
                     (table--insert-rectangle cell-contents)
                     (table--remove-cell-properties (point-min) (point-max))
                     (goto-char (point-min))
                     (buffer-substring (point-min) (point-max)))))
      (with-current-buffer dest-buffer
        (let ((beg (point)))
          (insert (org-export-string-as string 'html t info))
          (indent-rigidly beg (point) 6)))))

  (defun my-org-html-table--table.el-table (table _info)
    "Format table.el tables into HTML.
INFO is a plist used as a communication channel."
    (when (eq (org-element-property :type table) 'table.el)
      (require 'table)
      (let ((outbuf (with-current-buffer
                        (get-buffer-create "*org-export-table*")
                      (erase-buffer) (current-buffer))))
        (with-temp-buffer
          (insert (org-element-property :value table))
          (goto-char 1)
          (re-search-forward "^[ \t]*|[^|]" nil t)
          (table-recognize-region (point-min) (point-max) 1)
          (table-generate-source 'html outbuf))
        (with-current-buffer outbuf
          (prog1 (org-trim (buffer-string))
            (kill-buffer))))))

  (defun org-orghtml-table--table.el-table (fun table info)
    "Format table.el TABLE into HTML.
This is an advice for `org-html-table--table.el-table' as FUN.
INFO is a plist used as a communication channel."
    (if (assoc-string (plist-get info :html-tableel-org) '("t" "yes"))
        (cl-letf (((symbol-function 'table--generate-source-cell-contents)
                   (lambda (dest-buffer language cell)
                     (table-generate-orghtml-cell-contents dest-buffer language cell info))))
          (funcall fun table info))
      (funcall fun table info)))

  (advice-add 'org-html-table--table.el-table :override #'my-org-html-table--table.el-table)
  (advice-add #'my-org-html-table--table.el-table :around #'org-orghtml-table--table.el-table))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (dot . t)
     (sql . t)
     (python . t)))

  (add-to-list 'org-structure-template-alist '("sel" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sb" . "src bash"))
  (add-to-list 'org-structure-template-alist '("sp" . "src python"))


  ;; to produce font-face for org quote block
  (setq org-fontify-quote-and-verse-blocks t)

  (setq org-hide-emphasis-markers t)
  ;; hide the empasis syntax ex. *hi* -> bold hi

  (setq org-adapt-indentation t) ;; use indent-line-function to
  ;; see the indentation function used by org-mode
  ;; check the doc of org-indent-line, know how indent works

  (set-face-attribute 'org-block nil :background "#202021")
  (set-face-attribute 'org-quote nil :background "#202021")


  ;; set org table's font
  ;; (set-face-font 'org-table " ")
  ;; I use the visual-column instead
  ;; (add-hook 'org-mode-hook 'toggle-word-wrap)

  ;; Set faces for heading levels
  (dolist (face '((org-document-title . 1.5)
                  (org-level-1 . 1.3)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.15)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
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


  (setq org-download-screenshot-method "screencapture -i %s")
  (setq-default org-download-image-dir "./img")
  (setq org-download-image-org-width 500)

  (setq org-journal-dir "~/Dropbox/myorgs/journal/")
  (setq org-journal-file-type 'weekly)
  (setq org-journal-file-format "%Y-%m-%W.org")

  (setq org-agenda-files (split-string (shell-command-to-string "find ~/Dropbox/myorgs -type f | grep '.*.org$' | grep -E -v 'presentation/|journal/'") "\n" t))

  ;; to config the org refile
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)

  ;; to allow creating a new heading when performing the org refile
  (setq org-refile-allow-creating-parent-nodes 'confirm)


  ;; customize the bullet symbol
  (custom-set-variables '(org-bullets-bullet-list '("❐" "○" "﹅" "▶")))
  (setq org-superstar-headline-bullets-list '("❐" "○" "✎" "⚈"))
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

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(with-eval-after-load 'evil
  (evil-define-key 'emacs 'global (kbd "M-b") 'buffer-operate/body)
  (evil-define-key 'emacs 'global (kbd "M-w") 'window-operate/body))


(use-package table
  :defer 0
  :config
  (unless table-cell-map
    (table--make-cell-map))
  (define-key table-cell-map (kbd "<return>") '*table--cell-newline-and-indent)
  (with-eval-after-load 'evil
    (evil-define-key 'normal table-cell-map (kbd "x") '*table--cell-delete-char)
    (evil-define-key 'normal table-cell-map (kbd "p") '*table--cell-yank)
    (evil-define-key 'normal table-cell-map (kbd "d") '*table--cell-delete-region)
    (evil-define-key 'normal table-cell-map (kbd ".") 'hydra-table-mode/body)))

(use-package ielm
  :defer t
  :after evil
  :init
  (evil-define-key 'insert interior-emacs-lisp-mode-map (kbd "M-RET") #'ielm-return)
  (evil-define-key 'insert interior-emacs-lisp-mode-map (kbd "RET") #'electric-newline-and-maybe-indent)
  (evil-define-key 'insert inferior-emacs-lisp-mode-map (kbd "<up>") #'previous-line)
  (evil-define-key 'insert inferior-emacs-lisp-mode-map (kbd "<down>") #'next-line)
  (evil-define-key 'insert inferior-emacs-lisp-mode-map (kbd "M-<up>") #'comint-previous-input))

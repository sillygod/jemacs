;;; mycraft --- Summary  -*- lexical-binding: t; -*-
;;; Copyright (C) 2020 mycraft maintainers
;;; Author: Jing
;;; package --- mycraft
;;; Commentary:

;;; Code:

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

(add-hook 'prog-mode-hook 'goto-address-prog-mode)

(add-hook 'prog-mode-hook #'(lambda () (setq indent-tabs-mode nil)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'dired-mode-hook 'auto-revert-mode)

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

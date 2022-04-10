;;; mycraft --- Summary  -*- lexical-binding: t; -*-
;;; Copyright (C) 2020 mycraft maintainers
;;; Author: Jing
;;; package --- mycraft
;;; Commentary:

;;; Code:

(use-package org-ql
  :defer t)

(use-package org-drill
  :defer t)

(use-package org
  :defer t
  :init

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
  :custom
  ;; cool! some functions need to be enable
  ;; like <s press tab to complete org structure (org-tempo)
  ;; these variables can be found in the source code of org.el
  (org-modules '(ol-w3m
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


  ;; to produce font-face for org quote block
  (org-fontify-quote-and-verse-blocks t)
  ;; hide the empasis syntax ex. *hi* -> bold hi
  (org-hide-emphasis-markers t)
  ;; use indent-line-function to
  ;; see the indentation function used by org-mode
  ;; check the doc of org-indent-line, know how indent works
  (org-adapt-indentation t)
  ;; this will make org-shift to auto add timestamp after making a toto item complete
  (org-log-done 'time)
  (org-startup-truncated nil)
  (org-export-backends '(ascii html icalendar latex odt md))

  (org-image-actual-width nil)
  ;; org-edit-src without prompting window
  (org-src-window-setup 'current-window)
  (org-startup-folded t)
  (org-startup-with-inline-images t)

  (org-agenda-use-tag-inheritance nil)

  :config
  (keymap-unset org-mode-map "C-'" t)
  ;; (setq org-ellipsis " ▾")
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
  :custom
  (org-download-screenshot-method "screencapture -i %s")
  (org-download-image-org-width 500)
  (org-download-image-dir "./img")
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

(provide 'jemacs-note-taking)
;;; jemacs-note-taking.el ends here

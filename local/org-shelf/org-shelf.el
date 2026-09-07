;;; org-shelf.el --- Org notes without sqlite -*- lexical-binding: t; -*-

;; Author: Jing
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: outlines, convenience, org
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Lightweight stand-in for the org-roam workflow used here: file-level
;; (and heading-level) nodes identified by org PROPERTIES :ID:, titled
;; with #+title, tagged with #+filetags.  No sqlite, no roam: links, no
;; graph UI.
;;
;; Notes keep this header:
;;
;;   :PROPERTIES:
;;   :ID:       ...
;;   :END:
;;   #+title: ...
;;   #+filetags: :tag:
;;
;; `org-shelf-complete-everywhere' is a capf: while typing in org
;; buffers, matching node titles wrap into [[id:UUID][title]].
;;
;; Main entry points:
;;   M-x org-shelf-find
;;   M-x org-shelf-insert
;;   M-x org-shelf-new
;;   M-x org-shelf-dailies-goto-today
;;   M-x org-shelf-backlinks
;;   M-x org-shelf-tag-add
;;   M-x org-shelf-refresh

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-id)
(require 'pcase)
(require 'seq)
(require 'subr-x)

(declare-function consult--read "consult")
(declare-function org-back-to-heading-or-point-min "org")
(declare-function org-before-first-heading-p "org")
(declare-function org-fold-show-context "org-fold")
(declare-function org-get-tags "org")
(declare-function org-in-src-block-p "org")
(declare-function org-set-tags "org")


;;; Customization

(defgroup org-shelf nil
  "Org notes indexed in memory, without sqlite."
  :group 'org
  :prefix "org-shelf-")

(defcustom org-shelf-directory nil
  "Root directory of notes.
Nil means `jemacs-org-roam-directory', then `jemacs-org-root', then ~/org/."
  :type '(choice (const :tag "Derive from jemacs org roots" nil)
                 (directory :tag "Notes directory"))
  :group 'org-shelf)

;; Old .dir-locals.el files setq `org-roam-directory' / `org-roam-db-location'.
(defvaralias 'org-roam-directory 'org-shelf-directory)
(defvar org-roam-db-location nil
  "Ignored compatibility stub; org-shelf does not use sqlite.")

(defcustom org-shelf-cache-file 'auto
  "Where to persist the node index.
`auto' means `locate-user-emacs-file' of org-shelf.eld.  Nil disables
the cache.  A string is an explicit path."
  :type '(choice (const :tag "Under user-emacs-directory" auto)
                 (const :tag "Do not persist" nil)
                 (file :tag "File"))
  :group 'org-shelf)

(defcustom org-shelf-completion-everywhere t
  "When non-nil, complete node titles while typing in Org buffers."
  :type 'boolean
  :group 'org-shelf)

(defcustom org-shelf-dailies-directory "journal/"
  "Directory of daily notes, relative to `org-shelf-directory'."
  :type 'string
  :group 'org-shelf)

(defcustom org-shelf-templates
  '((default :filetags nil :body "")
    (book :filetags ("book")
          :body "\nauthor:\n\n* The book in 3 sentences or 3 points\n* Impresisions\n* How the Book Changed Me\n* Favorite Quotes\n* Summary + Notes for each sections\n")
    (course :filetags ("course")
            :body "\nlink:\n\n* Impressions\n* Summary + Notes for each section\n"))
  "Alist of (KIND :filetags TAGS :body STRING) for `org-shelf-new'."
  :type '(alist :key-type symbol :value-type plist)
  :group 'org-shelf)

(defvar org-shelf-history nil
  "Minibuffer history of org-shelf node selections.")


;;; Node

(cl-defstruct (org-shelf-node
               (:constructor org-shelf-node--create)
               (:copier nil))
  id title tags file point level)


;;; Index

(defvar org-shelf--by-id (make-hash-table :test 'equal)
  "Map node id to `org-shelf-node'.")

(defvar org-shelf--by-file (make-hash-table :test 'equal)
  "Map absolute file name to list of `org-shelf-node'.")

(defvar org-shelf--mtime (make-hash-table :test 'equal)
  "Map absolute file name to last-indexed modification time.")

(defvar org-shelf--indexed-p nil)
(defvar org-shelf--title-candidates nil)

(defun org-shelf--directory ()
  "Return the absolute notes directory."
  (expand-file-name
   (or org-shelf-directory
       (bound-and-true-p jemacs-org-roam-directory)
       (bound-and-true-p jemacs-org-root)
       "~/org/")))

(defun org-shelf--cache-file ()
  "Return the cache path, or nil to skip persistence."
  (pcase org-shelf-cache-file
    ('auto (locate-user-emacs-file "org-shelf.eld"))
    ((pred stringp) org-shelf-cache-file)
    (_ nil)))

(defun org-shelf--org-files (dir)
  "Return absolute paths of org files under DIR."
  (when (file-directory-p dir)
    (seq-filter
     (lambda (f)
       (let ((base (file-name-nondirectory f)))
         (and (not (string-prefix-p "." base))
              (not (string-prefix-p "#" base)))))
     (directory-files-recursively dir "\\.org\\'" nil nil t))))

(defun org-shelf--reset-index ()
  "Clear in-memory tables."
  (clrhash org-shelf--by-id)
  (clrhash org-shelf--by-file)
  (clrhash org-shelf--mtime)
  (setq org-shelf--indexed-p nil
        org-shelf--title-candidates nil))

(defun org-shelf--nodes ()
  "Return every indexed node."
  (let (all)
    (maphash (lambda (_file nodes)
               (setq all (nconc (copy-sequence nodes) all)))
             org-shelf--by-file)
    all))

(defun org-shelf--split-tags (s)
  "Split a filetags or heading-tag STRING into a list."
  (when (and s (not (string-blank-p s)))
    (split-string s ":" t "[ \t]+")))

(defun org-shelf--heading-tags (raw)
  "Return tags trailing RAW heading text."
  (when (string-match "[ \t]\\(:[[:alnum:]_@#%:]+\\)[ \t]*$" raw)
    (org-shelf--split-tags (match-string 1 raw))))

(defun org-shelf--heading-title (raw)
  "Strip TODO, priority, cookies and tags from RAW heading text."
  (setq raw (replace-regexp-in-string "[ \t]+$" "" raw))
  (setq raw (replace-regexp-in-string "[ \t]+:[[:alnum:]_@#%:]+[ \t]*$" "" raw))
  (setq raw (replace-regexp-in-string "[ \t]+\\[[0-9%./]+\\]$" "" raw))
  (setq raw (replace-regexp-in-string
             "\\`\\(?:COMMENT[ \t]+\\)?\\(?:TODO\\|DONE\\|IN PROGRESS\\|PRESERVE\\)[ \t]+"
             "" raw))
  (setq raw (replace-regexp-in-string "\\`\\[#[A-Z0-9]\\][ \t]+" "" raw))
  (string-trim raw))

(defun org-shelf--keyword (name end)
  "Return the value of keyword NAME before buffer position END."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
          (re (format "^#\\+%s:[ \t]*\\(.*\\)$" (regexp-quote name))))
      (when (re-search-forward re end t)
        (string-trim (match-string-no-properties 1))))))

(defun org-shelf--id-in-region (beg end)
  "Return the first :ID: value between BEG and END."
  (save-excursion
    (goto-char beg)
    (when (re-search-forward "^[ \t]*:ID:[ \t]+\\(\\S-+\\)" end t)
      (match-string-no-properties 1))))

(defun org-shelf--id-after-heading ()
  "Return ID in the PROPERTIES drawer on the following lines, or nil."
  (save-excursion
    (forward-line 1)
    (when (looking-at-p "[ \t]*:PROPERTIES:[ \t]*$")
      (let ((end (save-excursion
                   (and (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
                        (point)))))
        (when end
          (org-shelf--id-in-region (point) end))))))

(defun org-shelf--parse-buffer (file)
  "Parse the current buffer as FILE and return a list of nodes."
  (save-excursion
    (goto-char (point-min))
    (let* ((preamble-end (save-excursion
                           (if (re-search-forward "^\\*+ " nil t)
                               (match-beginning 0)
                             (point-max))))
           (file-id (org-shelf--id-in-region (point-min) preamble-end))
           (title (org-shelf--keyword "title" preamble-end))
           (filetags (org-shelf--split-tags
                      (org-shelf--keyword "filetags" preamble-end)))
           nodes)
      (when (or file-id title)
        (push (org-shelf-node--create
               :id file-id
               :title (or title (file-name-base file))
               :tags filetags
               :file file
               :point 1
               :level 0)
              nodes))
      (goto-char preamble-end)
      (while (re-search-forward "^\\(\\*+\\) \\(.*\\)$" nil t)
        (let ((level (length (match-string 1)))
              (raw (match-string-no-properties 2))
              (heading-point (match-beginning 0))
              (id (org-shelf--id-after-heading)))
          (when id
            (push (org-shelf-node--create
                   :id id
                   :title (org-shelf--heading-title raw)
                   :tags (append filetags (org-shelf--heading-tags raw))
                   :file file
                   :point heading-point
                   :level level)
                  nodes))))
      (nreverse nodes))))

(defun org-shelf--drop-file (file)
  "Remove FILE and its nodes from the index."
  (dolist (node (gethash file org-shelf--by-file))
    (when (org-shelf-node-id node)
      (remhash (org-shelf-node-id node) org-shelf--by-id)))
  (remhash file org-shelf--by-file)
  (remhash file org-shelf--mtime)
  (setq org-shelf--title-candidates nil))

(defun org-shelf--store-nodes (file mtime nodes)
  "Replace FILE's index with NODES at MTIME."
  (org-shelf--drop-file file)
  (puthash file nodes org-shelf--by-file)
  (puthash file mtime org-shelf--mtime)
  (dolist (node nodes)
    (when (org-shelf-node-id node)
      (puthash (org-shelf-node-id node) node org-shelf--by-id)))
  (setq org-shelf--title-candidates nil))

(defun org-shelf--index-buffer (file &optional mtime)
  "Index the current buffer as FILE."
  (org-shelf--store-nodes
   (expand-file-name file)
   (or mtime
       (and (file-exists-p file)
            (file-attribute-modification-time (file-attributes file)))
       (current-time))
   (org-shelf--parse-buffer (expand-file-name file))))

(defun org-shelf--index-file (file)
  "Read FILE from disk and index it."
  (setq file (expand-file-name file))
  (if (not (file-readable-p file))
      (org-shelf--drop-file file)
    (with-temp-buffer
      (insert-file-contents file)
      (org-shelf--index-buffer
       file
       (file-attribute-modification-time (file-attributes file))))))

(defun org-shelf--node-to-plist (node)
  "Serialize NODE to a plist."
  (list :id (org-shelf-node-id node)
        :title (org-shelf-node-title node)
        :tags (org-shelf-node-tags node)
        :file (org-shelf-node-file node)
        :point (org-shelf-node-point node)
        :level (org-shelf-node-level node)))

(defun org-shelf--node-from-plist (plist)
  "Deserialize PLIST to a node."
  (org-shelf-node--create
   :id (plist-get plist :id)
   :title (plist-get plist :title)
   :tags (plist-get plist :tags)
   :file (plist-get plist :file)
   :point (plist-get plist :point)
   :level (plist-get plist :level)))

(defun org-shelf--cache-form ()
  "Return the cache Lisp object."
  (let (files)
    (maphash
     (lambda (file nodes)
       (push (list file
                   (gethash file org-shelf--mtime)
                   (mapcar #'org-shelf--node-to-plist nodes))
             files))
     org-shelf--by-file)
    (list :version 1
          :directory (org-shelf--directory)
          :files files)))

(defun org-shelf--write-cache ()
  "Persist the index when a cache file is configured."
  (when-let* ((file (org-shelf--cache-file)))
    (condition-case err
        (with-temp-file file
          (let ((print-length nil)
                (print-level nil)
                (print-circle nil))
            (prin1 (org-shelf--cache-form) (current-buffer))))
      (error (message "org-shelf: could not write cache: %s"
                      (error-message-string err))))))

(defun org-shelf--load-cache ()
  "Restore the index from disk.  Return non-nil on success."
  (when-let* ((file (org-shelf--cache-file)))
    (when (file-readable-p file)
      (condition-case nil
          (let ((form (with-temp-buffer
                        (insert-file-contents file)
                        (read (current-buffer)))))
            (when (and (eq (plist-get form :version) 1)
                       (equal (plist-get form :directory)
                              (org-shelf--directory)))
              (org-shelf--reset-index)
              (dolist (entry (plist-get form :files))
                (pcase-let ((`(,path ,mtime ,plists) entry))
                  (org-shelf--store-nodes
                   path mtime
                   (mapcar #'org-shelf--node-from-plist plists))))
              t))
        (error nil)))))

(defun org-shelf--sync-files ()
  "Reparse notes whose mtime changed; drop deleted files."
  (let* ((dir (org-shelf--directory))
         (files (org-shelf--org-files dir))
         (seen (make-hash-table :test 'equal)))
    (with-temp-buffer
      (dolist (file files)
        (puthash file t seen)
        (let ((mtime (file-attribute-modification-time
                      (file-attributes file))))
          (unless (equal mtime (gethash file org-shelf--mtime))
            (erase-buffer)
            (insert-file-contents file)
            (org-shelf--index-buffer file mtime)))))
    (maphash (lambda (file _)
               (unless (gethash file seen)
                 (org-shelf--drop-file file)))
             org-shelf--by-file)))

(defun org-shelf--ensure-index ()
  "Load cache and sync, once per session until `org-shelf-refresh'."
  (unless org-shelf--indexed-p
    (org-shelf--load-cache)
    (org-shelf--sync-files)
    (setq org-shelf--indexed-p t)
    (org-shelf--write-cache)))

;;;###autoload
(defun org-shelf-refresh (&optional force)
  "Rebuild the node index from disk.
With prefix FORCE, ignore the in-memory tables and cache."
  (interactive "P")
  (when force
    (org-shelf--reset-index))
  (unless force
    (org-shelf--load-cache))
  (org-shelf--sync-files)
  (setq org-shelf--indexed-p t)
  (org-shelf--write-cache)
  (when (called-interactively-p 'interactive)
    (message "org-shelf: %d nodes in %s"
             (length (org-shelf--nodes))
             (org-shelf--directory)))
  (org-shelf--nodes))

(defun org-shelf--after-save ()
  "Reindex the current file when it lives under the notes directory."
  (when-let* ((file (buffer-file-name)))
    (when (and (string-suffix-p ".org" file)
               org-shelf--indexed-p
               (file-in-directory-p file (org-shelf--directory)))
      (org-shelf--index-buffer file)
      (org-shelf--write-cache))))

(defun org-shelf--node-by-title (title)
  "Return the first node whose title equals TITLE (case-insensitive)."
  (let ((down (downcase title)))
    (seq-find (lambda (n)
                (and (org-shelf-node-title n)
                     (string= (downcase (org-shelf-node-title n)) down)))
              (org-shelf--nodes))))

(defun org-shelf--node-by-id (id)
  "Return the node with ID, or nil."
  (and id (gethash id org-shelf--by-id)))


;;; Links / visit

(defun org-shelf--link (node)
  "Return an org id-link string for NODE."
  (org-link-make-string
   (concat "id:" (org-shelf-node-id node))
   (org-shelf-node-title node)))

(defun org-shelf--slug (title)
  "Return a filename slug for TITLE."
  (downcase
   (string-trim
    (replace-regexp-in-string
     "_+" "_"
     (replace-regexp-in-string "[^[:alnum:][:digit:]]+" "_" title))
    "_+" "_+")))

(defun org-shelf--unique-file (dir slug)
  "Return an unused absolute path DIR/SLUG.org."
  (let ((n 0)
        file)
    (while (progn
             (setq file (expand-file-name
                         (if (zerop n)
                             (concat slug ".org")
                           (format "%s-%d.org" slug n))
                         dir))
             (or (file-exists-p file)
                 (gethash file org-shelf--by-file)))
      (cl-incf n))
    file))

(defun org-shelf-visit (node &optional other-window)
  "Visit NODE, optionally in OTHER-WINDOW."
  (let ((find (if other-window #'find-file-other-window #'find-file)))
    (funcall find (org-shelf-node-file node)))
  (org-with-wide-buffer
    (goto-char (point-min))
    (let ((id (org-shelf-node-id node)))
      (cond
       ((and id (re-search-forward
                 (format "^[ \t]*:ID:[ \t]+%s[ \t]*$" (regexp-quote id))
                 nil t))
        (org-back-to-heading-or-point-min t)
        (when (fboundp 'org-fold-show-context)
          (org-fold-show-context 'org-goto)))
       ((org-shelf-node-point node)
        (goto-char (org-shelf-node-point node)))))))

(defun org-shelf--ensure-id (node)
  "Return NODE's id, creating one in the file if needed."
  (or (and (org-shelf-node-id node)
           (not (string-empty-p (org-shelf-node-id node)))
           (org-shelf-node-id node))
      (let ((buf (find-file-noselect (org-shelf-node-file node))))
        (with-current-buffer buf
          (save-excursion
            (goto-char (or (org-shelf-node-point node) (point-min)))
            (let ((id (org-id-get-create)))
              (setf (org-shelf-node-id node) id)
              (puthash id node org-shelf--by-id)
              (org-id-add-location id (buffer-file-name))
              (when (buffer-modified-p)
                (let ((save-silently t)
                      (inhibit-message t))
                  (save-buffer)))
              id))))))


;;; Completion-at-point

(defconst org-shelf--bracket-re
  "\\[\\[\\(\\(?:id:\\|roam:\\)?\\)\\([^][\n]*\\)\\(\\]\\]\\)?"
  "Match a bracket link at point, with or without the closing `]]'.")

(defun org-shelf--in-keyword-p ()
  "Return non-nil if point is on an org keyword line."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[ \t]*#\\+")))

(defun org-shelf--bracket-bounds ()
  "Return (INNER-BEG INNER-END CLOSED) if point is inside a `[[' link.
CLOSED is non-nil when the closing `]]' is already present."
  (save-excursion
    (save-match-data
      (let ((pt (point)))
        (when (re-search-backward "\\[\\[" (line-beginning-position) t)
          (when (looking-at org-shelf--bracket-re)
            (let ((beg (match-beginning 2))
                  (end (match-end 2))
                  (closed (match-beginning 3)))
              (when (and (<= beg pt)
                         (<= pt (or (match-end 3) (line-end-position))))
                (list beg end closed)))))))))

(defun org-shelf--blocked-p ()
  "Return non-nil when node completion should not run at point."
  (or (org-in-src-block-p)
      (org-at-property-p)
      (org-shelf--in-keyword-p)
      (org-shelf--bracket-bounds)
      (save-match-data (org-in-regexp org-link-any-re))))

(defun org-shelf--token-bounds ()
  "Return (BEG . END) of the completion token at point, or nil.
Includes ASCII words and CJK runs so a Chinese title is one token."
  (let ((beg (point))
        (end (point)))
    (save-excursion
      (skip-chars-backward "[:alnum:]_-")
      (setq beg (point)))
    (save-excursion
      (skip-chars-forward "[:alnum:]_-")
      (setq end (point)))
    (and (> end beg) (cons beg end))))

(defun org-shelf--compute-title-candidates ()
  "Return propertized title strings for completion."
  (let ((seen (make-hash-table :test 'equal))
        out)
    (dolist (node (org-shelf--nodes) (nreverse out))
      (when-let* ((title (org-shelf-node-title node)))
        (when (gethash title seen)
          (setq title (format "%s (%s)"
                              title
                              (file-name-base (org-shelf-node-file node)))))
        (puthash title t seen)
        (push (propertize title 'org-shelf-node node) out)))))

(defun org-shelf--title-candidates ()
  "Return cached title candidates, computing them if needed."
  (org-shelf--ensure-index)
  (or org-shelf--title-candidates
      (setq org-shelf--title-candidates
            (org-shelf--compute-title-candidates))))

(defun org-shelf--annotate-title (s)
  "Annotation for completion candidate S."
  (when-let* ((node (or (get-text-property 0 'org-shelf-node s)
                       (org-shelf--node-by-title s))))
    (let ((tags (org-shelf-node-tags node))
          (base (file-name-base (org-shelf-node-file node))))
      (concat " " base
              (when tags
                (concat " " (propertize (org-make-tag-string tags)
                                        'face 'org-tag)))))))

(defun org-shelf--wrap-exit (str status)
  "Capf exit: replace completed STR with an id link when STATUS is final."
  (when (memq status '(finished sole))
    (when-let* ((node (or (get-text-property 0 'org-shelf-node str)
                         (org-shelf--node-by-title str))))
      (when (org-shelf-node-id node)
        (delete-char (- (length str)))
        (insert (org-shelf--link node))))))

;;;###autoload
(defun org-shelf-complete-everywhere ()
  "Complete the token at point as an org-shelf node title.
This is a `completion-at-point' function.  Unlike completing inside
`[[...]]', it runs in ordinary prose.  Inserts `[[id:UUID][title]]'.
`:exclusive no' lets Org and Cape capfs still run."
  (when (and org-shelf-completion-everywhere
             (derived-mode-p 'org-mode)
             (not (org-shelf--blocked-p)))
    (when-let* ((bounds (org-shelf--token-bounds)))
      (list (car bounds) (cdr bounds)
            (org-shelf--title-candidates)
            :annotation-function #'org-shelf--annotate-title
            :exit-function #'org-shelf--wrap-exit
            :exclusive 'no))))

;;;###autoload
(defun org-shelf-complete-link-at-point ()
  "Complete an org-shelf node inside a `[[' pair, closed or not."
  (when (and (derived-mode-p 'org-mode)
             (not (org-in-src-block-p)))
    (when-let* ((b (org-shelf--bracket-bounds)))
      (pcase-let ((`(,beg ,end ,closed) b))
        (list beg end
              (org-shelf--title-candidates)
              :annotation-function #'org-shelf--annotate-title
              :exit-function
              (lambda (str status)
                (when (memq status '(finished sole))
                  (when-let* ((node (or (get-text-property 0 'org-shelf-node str)
                                       (org-shelf--node-by-title str))))
                    (when (org-shelf-node-id node)
                      (delete-char (- (length str)))
                      (insert (concat "id:" (org-shelf-node-id node)
                                      "][" (org-shelf-node-title node)))
                      (if closed
                          (forward-char 2)
                        (insert "]]"))))))
              :exclusive 'no)))))


;;; Completing-read helpers

(defun org-shelf--format-candidate (node)
  "Display string for NODE in `completing-read'."
  (let* ((title (or (org-shelf-node-title node) ""))
         (tags (org-shelf-node-tags node))
         (pad (make-string (max 1 (- 72 (string-width title))) ?\s)))
    (propertize
     (concat title
             (when tags
               (concat pad (propertize (org-make-tag-string tags)
                                       'face 'org-tag))))
     'org-shelf-node node)))

(defun org-shelf--candidate-alist ()
  "Return alist of (DISPLAY . NODE), disambiguating duplicate titles."
  (let ((seen (make-hash-table :test 'equal))
        alist)
    (dolist (node (org-shelf--nodes) (nreverse alist))
      (let ((text (org-shelf--format-candidate node)))
        (when (gethash text seen)
          (setq text (propertize
                      (format "%s  %s"
                              text
                              (file-name-nondirectory (org-shelf-node-file node)))
                      'org-shelf-node node)))
        (puthash text t seen)
        (push (cons text node) alist)))))

(defun org-shelf--read-node (prompt &optional require-match initial)
  "Read a node with PROMPT.  Return a node, a new title string, or nil."
  (org-shelf--ensure-index)
  (let* ((alist (org-shelf--candidate-alist))
         (candidates (mapcar #'car alist))
         (choice
          (if (fboundp 'consult--read)
              (consult--read
               candidates
               :prompt prompt
               :sort nil
               :require-match require-match
               :history 'org-shelf-history
               :category 'org-shelf-node
               :initial initial)
            (completing-read prompt candidates nil require-match
                             initial 'org-shelf-history))))
    (or (and (stringp choice) (get-text-property 0 'org-shelf-node choice))
        (cdr (assoc choice alist))
        (and choice (not (string-blank-p choice)) choice))))


;;; Commands

(defun org-shelf--template (kind)
  "Return the template plist for KIND."
  (cdr (or (assq kind org-shelf-templates)
           (assq 'default org-shelf-templates))))

(defun org-shelf--write-new-file (file title kind)
  "Write a new note FILE with TITLE using template KIND.  Return the node."
  (let* ((tmpl (org-shelf--template kind))
         (tags (plist-get tmpl :filetags))
         (body (or (plist-get tmpl :body) ""))
         (id (org-id-new)))
    (with-temp-file file
      (insert ":PROPERTIES:\n:ID:       " id "\n:END:\n")
      (insert "#+title: " title "\n")
      (when tags
        (insert "#+filetags: :" (mapconcat #'identity tags ":") ":\n"))
      (insert body))
    (org-id-add-location id file)
    (org-shelf--index-file file)
    (org-shelf--node-by-id id)))

;;;###autoload
(defun org-shelf-new (&optional title kind)
  "Create a new note with TITLE and template KIND.
KIND is a key in `org-shelf-templates' (default, book, course).
Return the new node."
  (interactive
   (list (read-string "Title: ")
         (intern (completing-read "Template: "
                                  (mapcar #'symbol-name
                                          (mapcar #'car org-shelf-templates))
                                  nil t nil nil "default"))))
  (let* ((title (or title (read-string "Title: ")))
         (kind (or kind 'default))
         (slug (org-shelf--slug title))
         (slug (if (string-empty-p slug) "note" slug))
         (file (org-shelf--unique-file (org-shelf--directory) slug))
         (node (org-shelf--write-new-file file title kind)))
    (when (called-interactively-p 'interactive)
      (org-shelf-visit node))
    node))

;;;###autoload
(defun org-shelf-find (&optional other-window)
  "Find a node and visit it.
With prefix OTHER-WINDOW, visit in the other window.
A title that is not a node creates a default note."
  (interactive "P")
  (let ((node (org-shelf--read-node "Find node: ")))
    (cond
     ((org-shelf-node-p node)
      (org-shelf-visit node other-window))
     ((stringp node)
      (org-shelf-visit (org-shelf-new node 'default) other-window)))))

;;;###autoload
(defun org-shelf-insert ()
  "Insert an id link to a node at point.
The active region becomes the link description.  A title that is
not a node creates a default note first."
  (interactive)
  (let* ((beg (and (use-region-p) (region-beginning)))
         (end (and (use-region-p) (region-end)))
         (desc (and beg (buffer-substring-no-properties beg end)))
         (picked (org-shelf--read-node "Insert node: " nil desc))
         (node (cond
                ((org-shelf-node-p picked) picked)
                ((stringp picked) (org-shelf-new picked 'default)))))
    (unless node
      (user-error "No node"))
    (org-shelf--ensure-id node)
    (when beg
      (delete-region beg end))
    (insert (org-link-make-string
             (concat "id:" (org-shelf-node-id node))
             (or desc (org-shelf-node-title node))))))

;;;###autoload
(defun org-shelf-id-get-create ()
  "Create an org ID at point and reindex the file."
  (interactive)
  (call-interactively #'org-id-get-create)
  (when (buffer-file-name)
    (org-shelf--index-buffer (buffer-file-name))))

(defun org-shelf--all-tags ()
  "Return sorted unique tags from the index."
  (org-shelf--ensure-index)
  (let ((seen (make-hash-table :test 'equal)))
    (dolist (node (org-shelf--nodes))
      (dolist (tag (org-shelf-node-tags node))
        (puthash tag t seen)))
    (sort (hash-table-keys seen) #'string<)))

(defun org-shelf--add-filetags (tags)
  "Add TAGS to the current buffer's #+filetags keyword."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
          (end (save-excursion
                 (or (and (re-search-forward "^\\*+ " nil t)
                          (match-beginning 0))
                     (point-max)))))
      (goto-char (point-min))
      (if (re-search-forward "^#\\+filetags:[ \t]*\\(.*\\)$" end t)
          (let* ((beg (match-beginning 1))
                 (mend (match-end 1))
                 (current (org-shelf--split-tags (match-string 1)))
                 (merged (seq-uniq (append current tags)))
                 (value (concat ":" (mapconcat #'identity merged ":") ":")))
            (goto-char beg)
            (delete-region beg mend)
            (insert value))
        (goto-char (point-min))
        (let ((value (concat ":" (mapconcat #'identity tags ":") ":")))
          (if (re-search-forward "^#\\+title:.*$" end t)
              (progn (end-of-line) (insert "\n#+filetags: " value))
            (goto-char end)
            (insert "#+filetags: " value "\n")))))))

;;;###autoload
(defun org-shelf-tag-add (tags)
  "Add TAGS to the node at point (filetags before the first heading)."
  (interactive
   (list (completing-read-multiple "Tag: " (org-shelf--all-tags))))
  (unless (buffer-file-name)
    (user-error "Buffer is not visiting a file"))
  (save-excursion
    (if (org-before-first-heading-p)
        (org-shelf--add-filetags tags)
      (org-set-tags (seq-uniq (append tags (org-get-tags nil t))))))
  (org-shelf--index-buffer (buffer-file-name))
  tags)

(defun org-shelf-node-at-point ()
  "Return the indexed node at point, or nil."
  (org-shelf--ensure-index)
  (or (when-let* ((id (org-id-get)))
        (org-shelf--node-by-id id))
      (when-let* ((file (buffer-file-name)))
        (seq-find (lambda (n) (zerop (org-shelf-node-level n)))
                  (gethash (expand-file-name file) org-shelf--by-file)))))


;;; Dailies

(defun org-shelf--dailies-dir ()
  "Absolute dailies directory."
  (expand-file-name org-shelf-dailies-directory (org-shelf--directory)))

(defun org-shelf--dailies-file (time)
  "Absolute path of the daily note for TIME."
  (expand-file-name (concat (format-time-string "%Y-%m-%d" time) ".org")
                    (org-shelf--dailies-dir)))

(defun org-shelf--dailies-ensure (time)
  "Visit (creating if needed) the daily note for TIME.  Return the file."
  (let ((file (org-shelf--dailies-file time)))
    (unless (file-exists-p file)
      (make-directory (file-name-directory file) t)
      (org-shelf--write-new-file
       file
       (format-time-string "%Y-%m-%d" time)
       'default)
      ;; Daily notes always carry the daily filetag, even on the default
      ;; template.
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (unless (let ((case-fold-search t))
                  (re-search-forward "^#\\+filetags:" nil t))
          (goto-char (point-min))
          (when (re-search-forward "^#\\+title:.*$" nil t)
            (end-of-line)
            (insert "\n#+filetags: :daily:")
            (write-region (point-min) (point-max) file nil 'silent)
            (org-shelf--index-file file)))))
    file))

;;;###autoload
(defun org-shelf-dailies-goto-today (&optional time)
  "Visit today's daily note, creating it if needed.
TIME is for tests; interactively it is `current-time'."
  (interactive)
  (find-file (org-shelf--dailies-ensure (or time (current-time)))))

;;;###autoload
(defun org-shelf-dailies-capture-today (&optional time)
  "Visit today's daily note and insert a `* HH:MM' heading."
  (interactive)
  (let ((time (or time (current-time))))
    (find-file (org-shelf--dailies-ensure time))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "* " (format-time-string "%H:%M" time) " \n")
    (forward-char -1)))


;;; Backlinks

(defun org-shelf--rg-id (id dir)
  "Return (file line text) hits for `[[id:ID' under DIR."
  (let ((needle (concat "[[id:" id))
        hits)
    (if-let* ((rg (executable-find "rg")))
        (with-temp-buffer
          (when (eq 0 (call-process rg nil t nil
                                    "--no-heading" "-n" "-F" "--glob" "*.org"
                                    needle dir))
            (goto-char (point-min))
            (while (re-search-forward
                    "^\\(.*\\):\\([0-9]+\\):\\(.*\\)$" nil t)
              (push (list (match-string 1)
                          (string-to-number (match-string 2))
                          (match-string 3))
                    hits))))
      (dolist (file (org-shelf--org-files dir))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (search-forward needle nil t)
            (push (list file (line-number-at-pos) (buffer-substring
                                                   (line-beginning-position)
                                                   (line-end-position)))
                  hits)))))
    (nreverse hits)))

;;;###autoload
(defun org-shelf-backlinks ()
  "Show files that link to the node at point (or a chosen node)."
  (interactive)
  (org-shelf--ensure-index)
  (let* ((node (or (org-shelf-node-at-point)
                   (let ((picked (org-shelf--read-node "Backlinks for: " t)))
                     (and (org-shelf-node-p picked) picked)))))
    (unless (and node (org-shelf-node-id node))
      (user-error "No node with an id"))
    (let* ((id (org-shelf-node-id node))
           (hits (seq-filter
                  (lambda (hit)
                    (not (equal (expand-file-name (nth 0 hit))
                                (expand-file-name (org-shelf-node-file node)))))
                  (org-shelf--rg-id id (org-shelf--directory))))
           (buf (get-buffer-create "*org-shelf-backlinks*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Backlinks to *%s*  (%d)\n\n"
                          (org-shelf-node-title node)
                          (length hits)))
          (if (null hits)
              (insert "No backlinks.\n")
            (dolist (hit hits)
              (pcase-let ((`(,file ,line ,text) hit))
                (insert (format "- [[file:%s::%d][%s:%d]] %s\n"
                                file line
                                (file-name-nondirectory file) line
                                (string-trim text))))))
          (org-mode)
          (setq buffer-read-only t)
          (goto-char (point-min)))
        (pop-to-buffer buf)))))


;;; Mode

(defun org-shelf--on-org-mode ()
  "Register capf and save hooks in an Org buffer."
  (add-hook 'completion-at-point-functions
            #'org-shelf-complete-everywhere nil t)
  (add-hook 'completion-at-point-functions
            #'org-shelf-complete-link-at-point nil t)
  (add-hook 'after-save-hook #'org-shelf--after-save nil t))

;;;###autoload
(define-minor-mode org-shelf-mode
  "Index org notes under `org-shelf-directory' and complete them everywhere."
  :global t
  :group 'org-shelf
  :lighter " Shelf"
  (if org-shelf-mode
      (progn
        (add-hook 'org-mode-hook #'org-shelf--on-org-mode)
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (when (derived-mode-p 'org-mode)
              (org-shelf--on-org-mode))))
        (run-with-idle-timer 0.8 nil #'org-shelf--ensure-index))
    (remove-hook 'org-mode-hook #'org-shelf--on-org-mode)))

(provide 'org-shelf)
;;; org-shelf.el ends here

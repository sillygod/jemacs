;;; devdocs.el --- Quick doc search on devdocs.io -*- lexical-binding: t -*-

;;; Commentary:
;; To use, type M-x devdocs-search
;;; Code:

(require 'seq)
(require 'shr)
(require 'url-expand)
(require 'subr-x)

;; you can M-x customize-group and input "emacs" to see the all groups
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Customization-Groups.html#Customization-Groups
(defgroup devdocs nil
  "Search doc by devdocs."
  :group 'external)

;; Emacs' groups has the hierarchy concept so the above define a group named devdocs
;; and is a child of external group

(defcustom devdocs-data-dir (expand-file-name "devdocs" user-emacs-directory)
  "Directory to save the docs."
  :type 'directory)

(defcustom devdocs-separator " » "
  "String is used to format a documentation location, e.g. in header line."
  :type 'string)

(defvar devdocs-cdn-url "https://documents.devdocs.io"
  "Location of the DevDocs CDN.")

(defcustom devdocs-url "https://devdocs.io"
  "The url of devdocs.io ."
  :type 'string
  :group 'devdocs)

(defconst devdocs--data-format-version 1
  "Version number of the saved documentation data format.")

(defvar devdocs--cache (make-hash-table :test 'equal))

(defcustom devdocs-cache-timeout 600
  "Time for keeping the cache."
  :type 'number)

(defmacro devdocs--with-cache (&rest body)
  "Memoize the evaluation of BODY result."
  `(let* ((func (lambda () ,@body))
          (name ,'func)
          (data (gethash name devdocs--cache)))
     (if data
         (data)
       (progn
         (let ((value (funcall func)))
           (puthash name value devdocs--cache)
           (run-at-time devdocs-cache-timeout nil #'remhash name devdocs--cache)
           value)))))

(defun devdocs--doc-metadata (slug)
  "Return the metadata of an installed doc named SLUG."
  (let ((file (expand-file-name (concat slug "/metadata") devdocs-data-dir)))
    (unless (file-exists-p file)
      (user-error "Document `%s' is not installed" slug))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((metadata (read (current-buffer))))
        (unless (eq (car metadata)
                    devdocs--data-format-version)
          (user-error "Please run `devdoss-update-all'"))
        (cdr metadata)))))

(defun devdocs--installed-docs ()
  "Return a list of installed documents."
  (mapcar #'devdocs--doc-metadata
          (let ((default-directory devdocs-data-dir))
            (seq-filter #'file-directory-p
                        (when (file-directory-p devdocs-data-dir)
                          (directory-files "." nil "^[^.]"))))))

(defun devdocs--available-docs ()
  "Return a list of available documents.
This will download doc from `devdocs-url' if needed."
  (devdocs--with-cache
   (with-temp-buffer
     (url-insert-file-contents
      (format "%s/docs.json" devdocs-url))
     (json-read))))

(defun devdocs--doc-title (doc)
  "Title of DOC."
  (let-alist (if (stringp doc) (devdocs--doc-metadata doc) doc)
    (if (seq-empty-p .version) .name (concat .name " " .version))))

(defun devdocs--read-document (prompt &optional multiple available)
  "Query interactively for a devdocs doc.
PROMPT is passed to `completing-read'"
  (let ((cands (mapcar (lambda (it) (cons (alist-get 'slug it) it))
                       (if available
                           (devdocs--available-docs)
                         (or (devdocs--installed-docs)
                             (user-error "No documents in `%s'" devdocs-data-dir))))))
    (if multiple
        (delq nil (mapcar (lambda (s) (cdr (assoc s cands)))
                          (completing-read-multiple prompt cands)))
      (cdr (assoc (completing-read prompt cands nil t) cands)))))

;;;###autoload
(defun devdocs-delete (doc)
  "Delete devdocs' documentation.
DOC is a document metadata alist."
  (interactive (list (devdocs--read-document "Delete documentation: ")))
  (let ((dest (expand-file-name (alist-get 'slug doc) devdocs-data-dir)))
    (if (and (file-directory-p dest)
             (file-in-directory-p dest devdocs-data-dir))
        (delete-directory dest t)))
  (user-error "Document `%s' is not installed" (alist-get 'slug doc)))

;;;###autoload
(defun devdocs-install (doc)
  "Download and install devdos documentation.
DOC is a document metadata alist."
  (interactive (list (devdocs--read-document "Install documentation: " nil t)))
  (make-directory devdocs-data-dir t)
  (let* ((slug (alist-get 'slug doc))
         (mtime (alist-get 'mtime doc))
         (temp (make-temp-file "devdocs-" t))
         pages)
    (with-temp-buffer
      (url-insert-file-contents (format "%s/%s/db.json?%s" devdocs-cdn-url slug mtime))
      (dolist-with-progress-reporter
          (entry (let ((json-key-type 'string))
                   (json-read)))
          "Installing documentation..."
        (with-temp-file (expand-file-name
                         (url-hexify-string (format "%s.html" (car entry))) temp)
          (push (car entry) pages)
          (insert (cdr entry)))))
    (with-temp-buffer
      (mm-url-insert-file-contents (format "%s/%s/index.json?%s" devdocs-cdn-url slug mtime))
      (let ((index (json-read)))
        (push `(page . ,(vconcat (nreverse pages))) index)
        (with-temp-file (expand-file-name "index" temp)
          (prin1 index (current-buffer)))))
    (with-temp-file (expand-file-name "metadata" temp)
      (prin1 (cons devdocs--data-format-version doc) (current-buffer)))
    (let ((dest (expand-file-name slug devdocs-data-dir)))
      (when (and (file-directory-p dest)
                 (file-in-directory-p dest devdocs-data-dir))
        (delete-directory dest t))
      (rename-file (file-name-as-directory temp) dest))
    (message "Document `%s' installed" slug)))

;;;###autoload
(defun devdocs-update-all()
  "Reinstall all documents with a new version."
  (interactive)
  (when-let ((installed (when (file-directory-p  devdocs-data-dir)
                          (directory-files devdocs-data-dir nil "^[^.]")))
             (newer (seq-filter (lambda (doc)
                                  (let-alist doc
                                    (and (member .slug installed)
                                         (< (alist-get 'mtime
                                                       (ignore-errors (devdocs--doc-metadata .slug))
                                                       0) ;; update docs with an old data format
                                            .mtime))))
                                (devdocs--available-docs)))
             ((y-or-n-p (format "Update %s documents %s?"
                                (length newer)
                                (mapcar (lambda (d) (alist-get 'slug d)) newer)))))
    (dolist (doc newer)
      (devdocs-install doc))))

(defun devdocs--index (doc kind)
  "Return an index of document DOC, where KIND is `entries', `pages' or `types'."
  (if kind
      (alist-get kind (devdocs--with-cache (devdocs--index doc nil)))
    (let* ((docmeta (cons 'doc doc))
           (indexes (with-temp-buffer
                      (insert-file-contents (expand-file-name
                                             (concat (alist-get 'slug doc) "/index")
                                             devdocs-data-dir))
                      (read (current-buffer))))
           (entries (alist-get 'entries indexes)))
      (prog1 indexes
        (seq-do-indexed (lambda (entry i) (aset entries i (cons docmeta entry)))
                        entries)))))

;; Documentation viewer

(defvar-local devdocs--stack nil
  "List of viewed entries.")

(defvar-local devdocs--forward-stack nil
  "List of viewed entries for `devdocs-go-forward'.")

(defvar devdocs-header-line
  '(:eval (let-alist (car devdocs--stack)
            (concat (devdocs--doc-title .doc)
                    (and .type devdocs-separator) .type
                    devdocs-separator (or .name .path)))))

(defvar-local devdocs--stack nil
  "List of viewed entries, set buffer-locally when in `devdocs-mode'.")

(defvar-local devdocs--forward-stack nil
  "List of viewed entries for `devdocs-go-forward'.")

;; special-mode is intended to view specially formatted data and usually
;; use  read-only buffer
(define-derived-mode devdocs-mode special-mode "devdocs"
  "Major mode for viewing devdocs documents."
  :interactive nil
  (setq-local
   browse-url-browser-function 'devdocs--browse-url
   buffer-undo-list t
   header-line-format devdocs-header-line
   revert-buffer-function 'devdocs--revert-buffer
   truncate-lines t))

;; TODO: url-generic-parse-url
(defun devdocs--path-file (path)
  "Return the non-fragment part of PATH."
  (substring path 0 (string-match "#" path)))

(defun devdocs--path-fragment (path)
  "Return the fragment part of PATH, or nil if absent."
  (when-let ((i (string-match "#" path)))
    (substring path (1+ i))))

(defun devdocs--revert-buffer (&rest _args)
  "Refresh DevDocs buffer."
  (devdocs--render (pop devdocs--stack)))

(defun devdocs--render (entry)
  "Render a devdocs documentation entry, returning a buffer.

ENTRY is an alist like those in the entry index of the document,
possibly with an additional ENTRY.fragment which overrides the
fragment part of ENTRY.path."
  (with-current-buffer (get-buffer-create "*devdocs*")
    (unless (eq major-mode 'devdocs-mode)
      (devdocs-mode))
    (let-alist entry
      (let ((inhibit-read-only t)
            (shr-external-rendering-functions `((pre . devdocs--shr-tag-pre)
                                                ,@shr-external-rendering-functions))
            (file (expand-file-name (format "%s/%s.html"
                                            \.doc.slug
                                            (url-hexify-string (devdocs--path-file \.path)))
                                    devdocs-data-dir)))
        (erase-buffer)
        (setq-local shr-target-id (or \.fragment (devdocs--path-fragment \.path)))
        (shr-insert-document
         (with-temp-buffer
           (insert-file-contents file)
           (libxml-parse-html-region (point-min) (point-max)))))
      (set-buffer-modified-p nil)
      (setq-local devdocs-current-docs (list \.doc.slug))
      (push entry devdocs--stack)
      (setq-local list-buffers-directory (format-mode-line devdocs-header-line nil nil (current-buffer)))
      (devdocs-goto-target))))

(defun devdocs-goto-target ()
  "Go to the original position in a devdocs buffer."
  (interactive)
  (goto-char (point-min))
  (when-let ((pred (if (fboundp 'shr--set-target-ids) #'member t))
             (match (text-property-search-forward 'shr-target-id shr-target-id pred)))
    (goto-char (prop-match-beginning match))))

(defun devdocs-go-back ()
  "Go to the previously displayed entry in devdocs buffer."
  (interactive)
  (unless (cadr devdocs--stack)
    (user-error "No previous eentry"))
  (push (pop devdocs--stack) devdocs--forward-stack)
  (devdocs--render (pop devdocs--stack)))

(defun devdocs-go-forward ()
  "Go to the next entry in the devdocs buffer."
  (interactive)
  (unless (car devdocs--forward-stack)
    (user-error "No next entry"))
  (devdocs--render (pop devdocs--forward-stack)))

(defun devdocs-next-entry (count)
  "Go forward COUNT entries in this documents.

Note taht this refers to the index order, which may not coincide with the order
of appearance in the text."
  (interactive "p")
  (let-alist (car devdocs--stack)
    (let* ((entries (devdocs--index \.doc 'entries))
            (pred (lambda (entry _) (string= (alist-get 'path entry) \.path)))
            (current (seq-position entries nil pred)))
         (unless current (user-error "No current entry"))
         (devdocs--render
          (or (ignore-error 'args-out-of-range (elt entries (+ count current)))
              (user-error "No %s entry" (if (< count 0) "previous" "next")))))))

(defun devdocs-previous-entry (count)
  "Go backward COUNT entries in this document."
  (interactive "p")
  (devdocs-next-entry (- count)))

(defun devdocs-goto-page (doc page)
  "Go to a given PAGE (a number or path) of DOC.
Interactively, read a page name with completion."
  (interactive (let-alist (car devdocs--stack)
                 (list .doc (completing-read "Go to page: "
                                             (append (devdocs--index .doc 'page) nil)
                                             nil t nil 'devdocs-history))))
  (let* ((path (cond ((stringp page) page)
                     ((numberp page) (elt (devdocs--index doc 'pages) page))))
         (entry (or (seq-find (lambda (entry) (string= (alist-get 'path entry) path))
                              (devdocs--index doc 'entries))
                    `((doc . ,doc) (path . ,path)))))
    (devdocs--render entry)))

(defun devdocs-first-page (doc)
  "Go to the first page of DOC."
  (interactive (list (alist-get 'doc (car devdocs--stack))))
  (devdocs-goto-page doc 0))

(defun devdocs-last-page (doc)
  "Go to last page of DOC."
  (interactive (list (alist-get 'doc (car devdocs--stack))))
  (devdocs-goto-page doc (1- (length (devdocs--index doc 'pages)))))

(defun devdocs-next-page (count)
  "Go forward COUNT pages in this document."
  (interactive "p")
  (let-alist (car devdocs--stack)
    (let* ((pages (devdocs--index .doc 'pages))
           (dest (+ count (seq-position pages (devdocs--path-file .path)))))
      (cond ((< dest 0) (user-error "No previous page"))
            ((<= (length pages) dest) (user-error "No next page")))
      (devdocs-goto-page .doc dest))))

(defun devdocs-previous-page (count)
  "Go backward COUNT entries in this document."
  (interactive "p")
  (devdocs-next-page (- count)))

(defun devdocs-copy-url ()
  "Copy the URL of the current DevDocs page to the kill ring."
  (interactive)
  (let-alist (or (car devdocs--stack)
                 (user-error "Not in a DevDcos buffer"))
    (let ((url (url-encode-url (format "%s/%s/%s" devdocs-site-url .doc.slug (if .fragment (concat (devdocs--path-file .path) "#" .fragment)
                                                                               .path)))))
      (kill-new url)
      (message "Copied %s" url))))

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Customization-Types.html#Customization-Types
(defcustom devdocs-alist
  '((c-mode . "c")
    (c++-mode . "c++")
    (css-mode . "css")
    (rust-mode . "rust")
    (go-mode . "go")
    (haskell-mode . "haskell")
    (js2-mode . "javascript")
    (python-mode . "python")
    (php-mode . "php"))
  "Major-mode maps to documentation on devdocs.io."
  :type '(repeat (cons (symbol :tag "major mode")
                       (string :tag "devdocs doc")))
  :group 'devdocs)

(defun devdocs--get-search-target ()
  "Get the search content from the region or 'thing-at-point'."
  (let ((document (cdr (assoc major-mode devdocs-alist)))
        (query (if (use-region-p)
                   (buffer-substring (region-beginning) (region-end))
                 (thing-at-point 'symbol))))
    (string-trim (string-join (list document query) " "))))

(defun devdocs-do-search (content)
  "Open the browser and go to the url"
  (browse-url
   (format "%s/#q=%s" devdocs-url (url-hexify-string content))))

;;;###autoload
(defun devdocs-search (&optional confirm)
  "Search the content on the devdocs"
  (interactive "P")
  (let ((content (funcall 'devdocs--get-search-target)))
    (when confirm
      (setq content (read-string "Searching devdocs: " content)))
    (devdocs-do-search content)))


(provide 'devdocs)
;;; devdocs.el ends here

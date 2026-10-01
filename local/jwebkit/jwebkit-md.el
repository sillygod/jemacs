;;; jwebkit-md.el --- Show Markdown in xwidget with GFM + Mermaid -*- lexical-binding: t; -*-

;; Author: Jing
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Preview Markdown in xwidget-webkit with full GitHub Flavored Markdown
;; (tables, strikethrough, task lists, autolinks) and ```mermaid fences.
;;
;; Same constraint as jwebkit-pdf: a file:// viewer cannot fetch local
;; libs or the document, so a loopback HTTP server inside Emacs serves
;; the viewer, pinned JS/CSS libs, and Markdown from one origin.
;; Documents are served by random token, never by path.
;;
;; Scope: `jwebkit-open-markdown'.  Local .md files opened with
;; `find-file' are left alone.  Does not use document.title.
;;
;;   gr   reload the current markdown view (re-reads file / buffer)

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'url-util)
(require 'xwidget)

(defcustom jwebkit-md-marked-version "11.2.0"
  "marked.js release to install (UMD build; GFM)."
  :type 'string
  :group 'jwebkit)

(defcustom jwebkit-md-mermaid-version "10.9.3"
  "mermaid.js release to install.
10.x ships a classic UMD `mermaid.min.js' that older system WebKit can
load as a plain script; 11+ prefers ESM modules."
  :type 'string
  :group 'jwebkit)

(defcustom jwebkit-md-css-version "5.8.1"
  "github-markdown-css release to install."
  :type 'string
  :group 'jwebkit)

(defcustom jwebkit-md-directory
  (expand-file-name "jwebkit/md-libs" user-emacs-directory)
  "Where pinned Markdown viewer libs are stored, one directory per pin."
  :type 'directory
  :group 'jwebkit)

(defcustom jwebkit-md-port nil
  "Preferred port for the local Markdown server.
Nil lets the OS pick a free one.  A port that is already taken falls
back to an OS-picked one rather than failing."
  :type '(choice (const :tag "Any free port" nil) integer)
  :group 'jwebkit)

(defvar jwebkit-md--server nil
  "The listening server process, or nil.")

(defvar jwebkit-md--docs (make-hash-table :test #'equal)
  "Token -> plist (:file FILE) or (:buffer BUFFER) or (:string STRING :name NAME).
The viewer asks for /md/TOKEN.  Tokens rather than paths, so the server
cannot be talked into reading an arbitrary local file.")


;;; Install

(defun jwebkit-md--release ()
  "Directory name for the pinned lib set."
  (format "marked-%s_mermaid-%s_css-%s"
          jwebkit-md-marked-version
          jwebkit-md-mermaid-version
          jwebkit-md-css-version))

(defun jwebkit-md--root ()
  (file-name-as-directory
   (expand-file-name (jwebkit-md--release) jwebkit-md-directory)))

(defun jwebkit-md--lib-files ()
  "Alist of (LOCAL-NAME . URL) for the pinned libs."
  (list
   (cons "marked.min.js"
         (format "https://cdn.jsdelivr.net/npm/marked@%s/marked.min.js"
                 jwebkit-md-marked-version))
   (cons "mermaid.min.js"
         (format "https://cdn.jsdelivr.net/npm/mermaid@%s/dist/mermaid.min.js"
                 jwebkit-md-mermaid-version))
   (cons "github-markdown.css"
         (format "https://cdn.jsdelivr.net/npm/github-markdown-css@%s/github-markdown.css"
                 jwebkit-md-css-version))))

(defun jwebkit-md--installed-p ()
  (let ((root (jwebkit-md--root)))
    (cl-every (lambda (pair)
                (file-regular-p (expand-file-name (car pair) root)))
              (jwebkit-md--lib-files))))

(defun jwebkit-md-install ()
  "Download pinned marked, mermaid and CSS into `jwebkit-md-directory'."
  (interactive)
  (let ((root (jwebkit-md--root)))
    (make-directory root t)
    (dolist (pair (jwebkit-md--lib-files))
      (let* ((name (car pair))
             (url (cdr pair))
             (dest (expand-file-name name root))
             (tmp (make-temp-file "jwebkit-md-" nil (concat "-" name))))
        (unwind-protect
            (progn
              (message "jwebkit-md: downloading %s…" name)
              (url-copy-file url tmp t)
              (copy-file tmp dest t))
          (ignore-errors (delete-file tmp)))))
    (message "jwebkit-md: libs installed in %s" root)))

(defun jwebkit-md--ensure-installed ()
  (unless (jwebkit-md--installed-p)
    (if (y-or-n-p (format "Download Markdown libs (%s) into %s? "
                          (jwebkit-md--release) jwebkit-md-directory))
        (jwebkit-md-install)
      (user-error "jwebkit-md: libs not installed"))))


;;; Paths

(defun jwebkit-md--library-file ()
  "Absolute path of this library's source file, or nil.
straight.el symlinks the .el into its build directory and does not
copy `md-viewer/' with it.  `locate-library' returns that symlink, so
follow it.  A byte-compiled hit is paired with the .el beside it,
which may itself be the symlink."
  (let ((found (or load-file-name (locate-library "jwebkit-md"))))
    (when found
      (file-truename
       (if (string-match-p "\\.elc\\'" found)
           (concat (file-name-sans-extension found) ".el")
         found)))))

(defun jwebkit-md--package-root ()
  "Directory containing this file and the shipped md-viewer/."
  (file-name-as-directory
   (expand-file-name
    (or (let ((lib (jwebkit-md--library-file)))
          (and lib (file-name-directory lib)))
        default-directory))))

(defun jwebkit-md--viewer-root ()
  (file-name-as-directory
   (expand-file-name "md-viewer" (jwebkit-md--package-root))))


;;; Server

(defun jwebkit-md--listen (service)
  (make-network-process
   :name "jwebkit-md"
   :server t
   :host 'local
   :family 'ipv4
   :service service
   :coding 'binary
   :noquery t
   :filter #'jwebkit-md--filter))

(defun jwebkit-md--port ()
  "Port of the running server, starting it first if needed.
Binding is the availability check: probing a port and binding it later
leaves a window for someone else to take it."
  (unless (process-live-p jwebkit-md--server)
    (setq jwebkit-md--server
          (or (and jwebkit-md-port
                   (condition-case err
                       (jwebkit-md--listen jwebkit-md-port)
                     (file-error
                      (message "jwebkit-md: port %d unavailable (%s); using a free port"
                               jwebkit-md-port (error-message-string err))
                      nil)))
              (jwebkit-md--listen t))))
  (process-contact jwebkit-md--server :service))

(defun jwebkit-md-stop ()
  "Stop the local Markdown server."
  (interactive)
  (when (process-live-p jwebkit-md--server)
    (delete-process jwebkit-md--server))
  (setq jwebkit-md--server nil))

(defun jwebkit-md--origin ()
  (format "http://127.0.0.1:%d" (jwebkit-md--port)))

(defun jwebkit-md--request-target (request)
  "(METHOD . PATH) from the request line of REQUEST, query dropped."
  (when (string-match "\\`\\([A-Z]+\\) \\([^ ?#]*\\)[^ ]* HTTP/" request)
    (cons (match-string 1 request)
          (url-unhex-string (match-string 2 request)))))

(defun jwebkit-md--filter (proc chunk)
  (let ((acc (concat (or (process-get proc 'jwebkit-req) "") chunk)))
    (if (not (string-search "\r\n\r\n" acc))
        (process-put proc 'jwebkit-req acc)
      (process-put proc 'jwebkit-req nil)
      (let ((target (jwebkit-md--request-target acc)))
        (jwebkit-md--send
         proc
         (if (member (car target) '("GET" "HEAD"))
             (jwebkit-md--route (cdr target))
           '(405 "text/plain" "method not allowed"))
         (equal (car target) "HEAD"))))))

(defconst jwebkit-md--mime-types
  '(("html" . "text/html; charset=utf-8")
    ("js" . "text/javascript; charset=utf-8")
    ("css" . "text/css; charset=utf-8")
    ("md" . "text/markdown; charset=utf-8")
    ("svg" . "image/svg+xml")
    ("png" . "image/png")
    ("map" . "application/json"))
  "Content types by extension.  Anything else is octet-stream.")

(defun jwebkit-md--mime-type (file)
  (or (cdr (assoc (downcase (or (file-name-extension file) ""))
                  jwebkit-md--mime-types))
      "application/octet-stream"))

(defun jwebkit-md--read-bytes (file)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun jwebkit-md--safe-file (root rel)
  "File under ROOT for REL, or nil if REL escapes it."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (file (expand-file-name rel root)))
    (and (string-prefix-p root file)
         (file-regular-p file)
         file)))

(defun jwebkit-md--doc-body (doc)
  "UTF-8 Markdown string for DOC plist, or nil if unavailable."
  (cond
   ((plist-get doc :file)
    (let ((file (plist-get doc :file)))
      (when (file-readable-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (buffer-string)))))
   ((plist-get doc :buffer)
    (let ((buf (plist-get doc :buffer)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (buffer-substring-no-properties (point-min) (point-max))))))
   ((plist-get doc :string)
    (plist-get doc :string))))

(defun jwebkit-md--route (path)
  "Response for PATH.
Either (STATUS CONTENT-TYPE BODY) or (STATUS CONTENT-TYPE :file FILE)."
  (cond
   ((string-match "\\`/md/viewer/\\(.*\\)\\'" path)
    (let ((file (jwebkit-md--safe-file (jwebkit-md--viewer-root)
                                       (match-string 1 path))))
      (if file
          (list 200 (jwebkit-md--mime-type file) :file file)
        '(404 "text/plain" "not found"))))
   ((string-match "\\`/md/libs/\\(.*\\)\\'" path)
    (let ((file (jwebkit-md--safe-file (jwebkit-md--root)
                                       (match-string 1 path))))
      (if file
          (list 200 (jwebkit-md--mime-type file) :file file)
        '(404 "text/plain" "not found"))))
   ((string-match "\\`/md/\\([^/]+\\)\\'" path)
    (let* ((token (match-string 1 path))
           (doc (gethash token jwebkit-md--docs))
           (body (and doc (jwebkit-md--doc-body doc))))
      (if body
          (list 200 "text/markdown; charset=utf-8" body)
        '(404 "text/plain" "unknown document"))))
   (t '(404 "text/plain" "not found"))))

(defun jwebkit-md--send (proc response head-only)
  (pcase-let* ((`(,status ,type . ,rest) response)
               (body (if (eq (car rest) :file)
                         (jwebkit-md--read-bytes (cadr rest))
                       (encode-coding-string (car rest) 'utf-8))))
    (process-send-string
     proc
     (concat (format "HTTP/1.1 %d %s\r\n" status
                     (pcase status (200 "OK") (404 "Not Found") (_ "Error")))
             (format "Content-Type: %s\r\n" type)
             (format "Content-Length: %d\r\n" (string-bytes body))
             "Cache-Control: no-store\r\n"
             "Connection: close\r\n\r\n"
             (unless head-only body)))
    (process-send-eof proc)))


;;; Viewer glue

(defun jwebkit-md--token ()
  (secure-hash 'sha256 (format "%s%s%s" (random t) (float-time) (emacs-pid))))

(defun jwebkit-md--register (plist)
  "Serve PLIST under a fresh token and return the token."
  (let ((token (jwebkit-md--token)))
    (puthash token plist jwebkit-md--docs)
    token))

(defun jwebkit-md--viewer-url (token)
  (concat (jwebkit-md--origin) "/md/viewer/viewer.html?src="
          (url-hexify-string (format "/md/%s" token))))

(defun jwebkit-md-viewer-p (xwidget)
  "Non-nil if XWIDGET is showing the local Markdown viewer."
  (and (process-live-p jwebkit-md--server)
       (string-prefix-p (concat (jwebkit-md--origin) "/md/viewer/")
                        (or (xwidget-webkit-uri xwidget) ""))))

(defun jwebkit-md--show (plist &optional xwidget new-session)
  "Show Markdown described by PLIST in XWIDGET or a (NEW-SESSION) buffer."
  (jwebkit-md--ensure-installed)
  (let ((url (jwebkit-md--viewer-url (jwebkit-md--register plist))))
    (if xwidget
        (xwidget-webkit-goto-uri xwidget url)
      (xwidget-webkit-browse-url url new-session))))

(defun jwebkit-md--markdown-buffer-p (&optional buffer)
  "Non-nil if BUFFER looks like a Markdown buffer."
  (with-current-buffer (or buffer (current-buffer))
    (or (derived-mode-p 'markdown-mode 'gfm-mode)
        (and buffer-file-name
             (string-match-p "\\.\\(md\\|markdown\\|mdown\\|mkd\\)\\'"
                             buffer-file-name)))))

(defun jwebkit-md--read-source ()
  "Read a Markdown file name, defaulting to the current Markdown buffer's file."
  (let* ((buf-file (and (jwebkit-md--markdown-buffer-p)
                        buffer-file-name))
         (in (read-file-name "Markdown file: "
                             (and buf-file (file-name-directory buf-file))
                             buf-file
                             t nil
                             (lambda (f)
                               (or (file-directory-p f)
                                   (string-match-p
                                    "\\.\\(md\\|markdown\\|mdown\\|mkd\\)\\'"
                                    f))))))
    (expand-file-name in)))


;;; Commands

(defun jwebkit-md--session ()
  (let ((xw (xwidget-webkit-current-session)))
    (unless (and xw (jwebkit-md-viewer-p xw))
      (user-error "Not a jwebkit markdown page"))
    xw))

;;;###autoload
(defun jwebkit-open-markdown (&optional source new-session)
  "Open SOURCE Markdown in xwidget with GFM + Mermaid.
SOURCE may be a file name, a live buffer, or nil.
Interactively: with a Markdown current buffer, preview that buffer
\(unsaved edits included); otherwise prompt for a file.
With NEW-SESSION (or prefix), use a new xwidget session."
  (interactive
   (list (if (jwebkit-md--markdown-buffer-p)
             (current-buffer)
           (jwebkit-md--read-source))
         current-prefix-arg))
  (cond
   ((bufferp source)
    (unless (buffer-live-p source)
      (user-error "Buffer is dead"))
    (jwebkit-md--show (list :buffer source) nil new-session))
   ((and (stringp source) (file-readable-p source))
    (jwebkit-md--show (list :file (expand-file-name source)) nil new-session))
   ((null source)
    (if (jwebkit-md--markdown-buffer-p)
        (jwebkit-md--show (list :buffer (current-buffer)) nil new-session)
      (user-error "Not a Markdown buffer; pass a file")))
   (t (user-error "No such file: %s" source))))

(defun jwebkit-md-reload ()
  "Reload the current Markdown view (re-fetches file or buffer text)."
  (interactive)
  (let ((xw (jwebkit-md--session)))
    ;; `xwidget-webkit-reload' takes no args (current session).
    (if (fboundp 'xwidget-webkit-reload)
        (xwidget-webkit-reload)
      (xwidget-webkit-execute-script xw "location.reload();"))))

(defun jwebkit-md-enable ()
  "Prepare Markdown preview (no auto-takeover; use `jwebkit-open-markdown')."
  ;; Symmetry with `jwebkit-pdf-enable'; nothing to advise yet.
  nil)


(provide 'jwebkit-md)
;;; jwebkit-md.el ends here

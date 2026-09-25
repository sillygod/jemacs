;;; jwebkit-pdf.el --- Show PDFs in xwidget with pdf.js -*- lexical-binding: t; -*-

;; Author: Jing
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; WebKit's built-in PDF view is a plugin with no DOM, so none of
;; jwebkit's JS keys (/ f Y) work in it.  pdf.js draws a text layer as
;; DOM, which brings them back, plus an outline and page jumps.
;;
;; The viewer has to fetch the PDF, and a file:// viewer can fetch
;; neither file:// (WKWebView leaves allowFileAccessFromFileURLs off)
;; nor https:// (CORS).  So a loopback HTTP server inside Emacs serves
;; the pdf.js dist and the PDF from one origin.  Remote PDFs are
;; downloaded by Emacs first.
;;
;; Scope: PDFs met inside xwidget, and `jwebkit-open-pdf'.  Local .pdf
;; files opened with `find-file' are left alone.
;;
;;   gp   go to page
;;   gO   toggle the outline
;;   / n N use pdf.js's own find (see `jwebkit-find-start')

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'url-util)
(require 'xwidget)

(defvar url-http-end-of-headers)
(defvar jwebkit-pdf--shim-js)

(defcustom jwebkit-pdf-version "6.3.289"
  "pdf.js release to install; see github.com/mozilla/pdf.js/releases."
  :type 'string
  :group 'jwebkit)

(defcustom jwebkit-pdf-directory
  (expand-file-name "jwebkit/pdfjs" user-emacs-directory)
  "Where pdf.js releases are unpacked, one directory per version."
  :type 'directory
  :group 'jwebkit)

(defcustom jwebkit-pdf-port nil
  "Preferred port for the local pdf.js server.
Nil lets the OS pick a free one.  A port that is already taken falls
back to an OS-picked one rather than failing."
  :type '(choice (const :tag "Any free port" nil) integer)
  :group 'jwebkit)

(defcustom jwebkit-pdf-auto t
  "Non-nil to reopen PDFs met in xwidget with pdf.js."
  :type 'boolean
  :group 'jwebkit)

(defvar jwebkit-pdf--server nil
  "The listening server process, or nil.")

(defvar jwebkit-pdf--docs (make-hash-table :test #'equal)
  "Token -> plist (:file FILE :temp BOOL :source URL-OR-FILE).
The viewer asks for /pdf/TOKEN/NAME.  Tokens rather than paths, so the
server cannot be talked into reading an arbitrary local file.")


;;; Install

(defun jwebkit-pdf--root ()
  (file-name-as-directory
   (expand-file-name jwebkit-pdf-version jwebkit-pdf-directory)))

(defun jwebkit-pdf--installed-p ()
  (file-exists-p (expand-file-name "web/viewer.html" (jwebkit-pdf--root))))

(defun jwebkit-pdf-install ()
  "Download and unpack pdf.js `jwebkit-pdf-version'."
  (interactive)
  (unless (executable-find "unzip")
    (user-error "jwebkit-pdf: `unzip' not found"))
  (let* ((url (format "https://github.com/mozilla/pdf.js/releases/download/v%s/pdfjs-%s-dist.zip"
                      jwebkit-pdf-version jwebkit-pdf-version))
         (zip (make-temp-file "pdfjs-" nil ".zip"))
         (root (jwebkit-pdf--root)))
    (unwind-protect
        (progn
          (message "jwebkit-pdf: downloading pdf.js %s…" jwebkit-pdf-version)
          (url-copy-file url zip t)
          (make-directory root t)
          (unless (zerop (call-process "unzip" nil nil nil "-q" "-o" zip "-d" root))
            (error "jwebkit-pdf: unzip failed for %s" zip))
          (message "jwebkit-pdf: pdf.js installed in %s" root))
      (delete-file zip))))

(defun jwebkit-pdf--ensure-installed ()
  (unless (jwebkit-pdf--installed-p)
    (if (y-or-n-p (format "Download pdf.js %s (~6 MB) into %s? "
                          jwebkit-pdf-version jwebkit-pdf-directory))
        (jwebkit-pdf-install)
      (user-error "jwebkit-pdf: pdf.js not installed"))))


;;; Server

(defun jwebkit-pdf--listen (service)
  (make-network-process
   :name "jwebkit-pdf"
   :server t
   :host 'local
   :family 'ipv4
   :service service
   :coding 'binary
   :noquery t
   :filter #'jwebkit-pdf--filter))

(defun jwebkit-pdf--port ()
  "Port of the running server, starting it first if needed.
Binding is the availability check: probing a port and binding it later
leaves a window for someone else to take it."
  (unless (process-live-p jwebkit-pdf--server)
    (setq jwebkit-pdf--server
          (or (and jwebkit-pdf-port
                   (condition-case err
                       (jwebkit-pdf--listen jwebkit-pdf-port)
                     (file-error
                      (message "jwebkit-pdf: port %d unavailable (%s); using a free port"
                               jwebkit-pdf-port (error-message-string err))
                      nil)))
              (jwebkit-pdf--listen t))))
  (process-contact jwebkit-pdf--server :service))

(defun jwebkit-pdf-stop ()
  "Stop the local pdf.js server."
  (interactive)
  (when (process-live-p jwebkit-pdf--server)
    (delete-process jwebkit-pdf--server))
  (setq jwebkit-pdf--server nil))

(defun jwebkit-pdf--origin ()
  (format "http://127.0.0.1:%d" (jwebkit-pdf--port)))

(defun jwebkit-pdf--request-target (request)
  "(METHOD . PATH) from the request line of REQUEST, query dropped."
  (when (string-match "\\`\\([A-Z]+\\) \\([^ ?#]*\\)[^ ]* HTTP/" request)
    (cons (match-string 1 request)
          (url-unhex-string (match-string 2 request)))))

(defun jwebkit-pdf--filter (proc chunk)
  (let ((acc (concat (or (process-get proc 'jwebkit-req) "") chunk)))
    (if (not (string-search "\r\n\r\n" acc))
        (process-put proc 'jwebkit-req acc)
      (process-put proc 'jwebkit-req nil)
      (let ((target (jwebkit-pdf--request-target acc)))
        (jwebkit-pdf--send
         proc
         (if (member (car target) '("GET" "HEAD"))
             (jwebkit-pdf--route (cdr target))
           '(405 "text/plain" "method not allowed"))
         (equal (car target) "HEAD"))))))

(defconst jwebkit-pdf--mime-types
  '(("html" . "text/html; charset=utf-8")
    ("mjs" . "text/javascript")
    ("js" . "text/javascript")
    ("css" . "text/css")
    ("json" . "application/json")
    ("map" . "application/json")
    ("ftl" . "text/plain; charset=utf-8")
    ("svg" . "image/svg+xml")
    ("png" . "image/png")
    ("gif" . "image/gif")
    ("wasm" . "application/wasm")
    ("pdf" . "application/pdf")
    ("ttf" . "font/ttf"))
  "Content types by extension.  Anything else is octet-stream.")

(defun jwebkit-pdf--mime-type (file)
  (or (cdr (assoc (downcase (or (file-name-extension file) ""))
                  jwebkit-pdf--mime-types))
      "application/octet-stream"))

(defun jwebkit-pdf--read-bytes (file)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun jwebkit-pdf--static-file (rel)
  "File under the pdf.js root for REL, or nil if REL escapes it."
  (let* ((root (jwebkit-pdf--root))
         (file (expand-file-name rel root)))
    (and (string-prefix-p root file)
         (file-regular-p file)
         file)))

(defun jwebkit-pdf--route (path)
  "Response for PATH.
Either (STATUS CONTENT-TYPE BODY) or (STATUS CONTENT-TYPE :file FILE)."
  (cond
   ((equal path "/jwebkit/shim.js")
    (list 200 "text/javascript" jwebkit-pdf--shim-js))
   ((string-match "\\`/pdf/\\([^/]+\\)/" path)
    (let ((doc (gethash (match-string 1 path) jwebkit-pdf--docs)))
      (if (and doc (file-readable-p (plist-get doc :file)))
          (list 200 "application/pdf" :file (plist-get doc :file))
        '(404 "text/plain" "unknown document"))))
   ((string-prefix-p "/pdfjs/" path)
    (let ((file (jwebkit-pdf--static-file (substring path 7))))
      (cond
       ((null file) '(404 "text/plain" "not found"))
       ((equal (file-name-nondirectory file) "viewer.html")
        (list 200 (jwebkit-pdf--mime-type file)
              (jwebkit-pdf--inject-shim (jwebkit-pdf--read-bytes file))))
       (t (list 200 (jwebkit-pdf--mime-type file) :file file)))))
   (t '(404 "text/plain" "not found"))))

(defun jwebkit-pdf--send (proc response head-only)
  (pcase-let* ((`(,status ,type . ,rest) response)
               (body (if (eq (car rest) :file)
                         (jwebkit-pdf--read-bytes (cadr rest))
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

(defconst jwebkit-pdf--shim-js
  "(function(){
  if (window.jwebkitPdf) return;
  function app(){ return window.PDFViewerApplication; }
  function box(){ return document.getElementById('viewerContainer'); }
  // xwidget scrolls with window.scrollBy / scrollTo, but pdf.js scrolls
  // #viewerContainer and leaves the window still.
  window.scrollBy = function(x, y){
    var c = box(); if (!c) return;
    if (typeof x === 'object') c.scrollBy(x); else c.scrollBy(x || 0, y || 0);
  };
  window.scrollTo = function(x, y){
    var c = box(); if (!c) return;
    if (typeof x === 'object') { c.scrollTo(x); return; }
    // xwidget's G is scrollTo(pageXOffset, body.scrollHeight); here the
    // body is one viewport tall, so read that as the end.
    if (y > 0 && document.body && y >= document.body.scrollHeight) y = c.scrollHeight;
    c.scrollTo(x || 0, y || 0);
  };
  window.jwebkitPdf = {
    find: function(q, again, prev){
      var a = app(); if (!a || !a.eventBus) return false;
      a.eventBus.dispatch('find', {source: window, type: again ? 'again' : '',
        query: q, caseSensitive: false, entireWord: false, highlightAll: true,
        findPrevious: !!prev, matchDiacritics: false});
      return true;
    },
    clear: function(){
      var a = app(); if (a && a.eventBus) a.eventBus.dispatch('findbarclose', {source: window});
    },
    page: function(n){
      var a = app(); if (!a || !a.pdfDocument) return 0;
      a.page = n; return a.page;
    },
    outline: function(){
      var v = app() && app().viewsManager; if (!v) return false;
      if (v.isOpen) { v.switchView(0); return true; }
      v.switchView(2, true);
      // No outline in the document: pdf.js refuses the view, so show
      // thumbnails rather than nothing.
      if (!v.isOpen) v.switchView(1, true);
      return true;
    }
  };
})();"
  "Loaded into viewer.html ahead of pdf.js.  See `jwebkit-pdf--inject-shim'.")

(defun jwebkit-pdf--inject-shim (html)
  "HTML with the jwebkit shim loaded before `</head>'."
  (replace-regexp-in-string
   "</head>" "<script src=\"/jwebkit/shim.js\"></script>\n</head>"
   html t t))

(defun jwebkit-pdf--token ()
  (secure-hash 'sha256 (format "%s%s%s" (random t) (float-time) (emacs-pid))))

(defun jwebkit-pdf--register (file source &optional temp)
  "Serve FILE under a fresh token and return the token.
SOURCE is where it came from (URL or file); TEMP means delete it on exit."
  (let ((token (jwebkit-pdf--token)))
    (puthash token (list :file file :temp temp :source source) jwebkit-pdf--docs)
    token))

(defun jwebkit-pdf--viewer-url (token name)
  (concat (jwebkit-pdf--origin) "/pdfjs/web/viewer.html?file="
          (url-hexify-string (format "/pdf/%s/%s" token (url-hexify-string name)))))

(defun jwebkit-pdf-viewer-p (xwidget)
  "Non-nil if XWIDGET is showing the local pdf.js viewer."
  (and (process-live-p jwebkit-pdf--server)
       (string-prefix-p (concat (jwebkit-pdf--origin) "/pdfjs/")
                        (or (xwidget-webkit-uri xwidget) ""))))

(defun jwebkit-pdf-source (xwidget)
  "Where the PDF shown in XWIDGET came from (URL or file), or nil."
  (when-let* (((jwebkit-pdf-viewer-p xwidget))
              (uri (xwidget-webkit-uri xwidget))
              ((string-match "[?&]file=\\([^&#]+\\)" uri))
              (file (url-unhex-string (match-string 1 uri)))
              ((string-match "\\`/pdf/\\([^/]+\\)/" file)))
    (plist-get (gethash (match-string 1 file) jwebkit-pdf--docs) :source)))

(defun jwebkit-pdf--name (source)
  (let ((name (file-name-nondirectory
               (car (split-string (url-unhex-string source) "[?#]")))))
    (if (string-empty-p name) "document.pdf" name)))

(defun jwebkit-pdf--show (file source &optional temp xwidget new-session)
  "Show FILE in pdf.js, in XWIDGET or a (NEW-SESSION) xwidget buffer."
  (jwebkit-pdf--ensure-installed)
  (let ((url (jwebkit-pdf--viewer-url
              (jwebkit-pdf--register file source temp)
              (jwebkit-pdf--name source))))
    (if xwidget
        (xwidget-webkit-goto-uri xwidget url)
      (xwidget-webkit-browse-url url new-session))))


;;; Remote PDFs

(defun jwebkit-pdf--pdf-bytes-p (start)
  "Non-nil if the current buffer holds a PDF from START."
  (string= (buffer-substring-no-properties
            start (min (point-max) (+ start 5)))
           "%PDF-"))

(defun jwebkit-pdf--fetch (url callback)
  "Download URL to a temp file, then call CALLBACK with it or nil.
Nil when the download fails or the body is not a PDF -- a login page,
typically, since Emacs does not share WebKit's cookies."
  (url-retrieve
   url
   (lambda (status)
     (let ((buf (current-buffer))
           (file nil))
       (unwind-protect
           (when (and (not (plist-get status :error))
                      (boundp 'url-http-end-of-headers)
                      url-http-end-of-headers)
             (set-buffer-multibyte nil)
             (let ((start (1+ url-http-end-of-headers)))
               (when (jwebkit-pdf--pdf-bytes-p start)
                 (setq file (make-temp-file "jwebkit-" nil ".pdf"))
                 (let ((coding-system-for-write 'binary))
                   (write-region start (point-max) file nil 'silent)))))
         (kill-buffer buf))
       (funcall callback file)))
   nil t t))

(defun jwebkit-pdf--open-url (url &optional xwidget new-session)
  (if (string-prefix-p "file://" url)
      (let ((file (url-unhex-string (url-filename (url-generic-parse-url url)))))
        (jwebkit-pdf--show file file nil xwidget new-session))
    (message "jwebkit-pdf: fetching %s…" url)
    (jwebkit-pdf--fetch
     url
     (lambda (file)
       (if file
           (jwebkit-pdf--show file url t xwidget new-session)
         (message "jwebkit-pdf: could not fetch a PDF from %s (login required?)"
                  url))))))

(defun jwebkit-pdf--cleanup ()
  (maphash (lambda (_ doc)
             (when (plist-get doc :temp)
               (ignore-errors (delete-file (plist-get doc :file)))))
           jwebkit-pdf--docs)
  (clrhash jwebkit-pdf--docs))

(add-hook 'kill-emacs-hook #'jwebkit-pdf--cleanup)


;;; Taking over WebKit's PDF view

(defun jwebkit-pdf--pdf-url-p (uri)
  (string-match-p "\\.pdf\\'" (car (split-string uri "[?#]"))))

(defun jwebkit-pdf--maybe-take-over (xwidget)
  "Reopen XWIDGET's page with pdf.js if WebKit is showing a PDF.
NS xwidget has no hook on the response, so this runs after the load:
WebKit's view shows for a moment first."
  (let ((uri (or (xwidget-webkit-uri xwidget) "")))
    (unless (or (not (string-match-p "\\`\\(https?\\|file\\)://" uri))
                (jwebkit-pdf-viewer-p xwidget))
      (xwidget-webkit-execute-script
       xwidget "document.contentType"
       (lambda (type)
         (when (or (equal type "application/pdf")
                   ;; The plugin document may not answer; the fetch checks
                   ;; the bytes anyway.
                   (and (not (stringp type)) (jwebkit-pdf--pdf-url-p uri)))
           (jwebkit-pdf--open-url uri xwidget)))))))

(defun jwebkit-pdf--callback-advice (orig xwidget type)
  "Around `xwidget-webkit-callback': route PDFs to pdf.js.
With plugins off, WebKit may hand a PDF over as a download instead of
showing it; that becomes a viewer too rather than a save prompt."
  (if (and jwebkit-pdf-auto
           (eq type 'download-callback)
           (equal (nth 4 last-input-event) "application/pdf"))
      (jwebkit-pdf--open-url (nth 3 last-input-event) xwidget)
    (funcall orig xwidget type)
    (when (and jwebkit-pdf-auto
               (eq type 'load-changed)
               (equal (nth 3 last-input-event) "load-finished")
               (buffer-live-p (xwidget-buffer xwidget)))
      (jwebkit-pdf--maybe-take-over xwidget))))

(defun jwebkit-pdf-enable ()
  "Route PDFs met in xwidget to pdf.js."
  (advice-add 'xwidget-webkit-callback :around #'jwebkit-pdf--callback-advice))


;;; Commands

(defun jwebkit-pdf--session ()
  (let ((xw (xwidget-webkit-current-session)))
    (unless (and xw (jwebkit-pdf-viewer-p xw))
      (user-error "Not a pdf.js page"))
    xw))

(defun jwebkit-pdf--read-source ()
  "Read a PDF file name, or a URL typed into the same prompt."
  (let ((in (read-file-name "PDF file or URL: " nil nil nil nil
                            (lambda (f) (or (file-directory-p f)
                                            (string-suffix-p ".pdf" f t))))))
    ;; A URL typed after the default directory arrives still behind it.
    (if (string-match "\\(https?://.*\\)\\'" in)
        (match-string 1 in)
      (expand-file-name in))))

;;;###autoload
(defun jwebkit-open-pdf (source &optional new-session)
  "Open SOURCE, a PDF file or URL, with pdf.js in xwidget.
With NEW-SESSION (or prefix), use a new xwidget session."
  (interactive (list (jwebkit-pdf--read-source) current-prefix-arg))
  (if (string-match-p "\\`\\(https?\\|file\\)://" source)
      (jwebkit-pdf--open-url source nil new-session)
    (unless (file-readable-p source)
      (user-error "No such file: %s" source))
    (jwebkit-pdf--show source source nil nil new-session)))

(defun jwebkit-pdf--find-js (query again reverse)
  (format "window.jwebkitPdf && window.jwebkitPdf.find(%s,%s,%s);"
          (json-encode query)
          (if again "true" "false")
          (if reverse "true" "false")))

(defun jwebkit-pdf-goto-page (page)
  "Go to PAGE of the PDF."
  (interactive (list (read-number "Page: ")))
  (xwidget-webkit-execute-script
   (jwebkit-pdf--session)
   (format "window.jwebkitPdf && window.jwebkitPdf.page(%d);" page)))

(defun jwebkit-pdf-outline ()
  "Toggle the PDF outline sidebar (thumbnails if there is no outline)."
  (interactive)
  (xwidget-webkit-execute-script
   (jwebkit-pdf--session)
   "window.jwebkitPdf && window.jwebkitPdf.outline();"))

(provide 'jwebkit-pdf)
;;; jwebkit-pdf.el ends here

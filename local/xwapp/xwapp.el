;;; xwapp.el --- Elisp-backed web pages in an xwidget -*- lexical-binding: t; -*-

;; Author: Jing
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, hypermedia
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; The plumbing under pr-view and clickup-view: a local HTML page in
;; xwidget-webkit renders, Elisp owns HTTP and secrets.
;;
;; Page → Emacs: the page sets document.title to PREFIX + JSON and a
;; timer polls the title.  xwidget-webkit gives Emacs no message channel
;; it can listen on, so the title is the channel.
;; Emacs → page: `xwapp-js' calls NAMESPACE[fn](json).
;;
;; The same page can be shown in a web browser instead (`xwapp-browse',
;; or `xwapp-browser' for every page): Emacs serves it on 127.0.0.1,
;; and the page talks to Emacs over HTTP.  See "In a web browser".
;;
;; One `xwapp' struct per app names its buffer, page and intent prefix.
;;
;; The HTTP layer is url.el with the fixes pr-view paid for: redirects
;; keep Authorization, Accept is not sent twice, bodies go out as UTF-8
;; bytes, and JSON false/null both read as nil.

;;; Code:

(require 'browse-url)
(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url)

(declare-function xwidget-webkit-goto-uri "xwidget")
(declare-function xwidget-webkit-execute-script "xwidget")
(declare-function xwidget-webkit-title "xwidget")
(declare-function xwidget-webkit-current-session "xwidget")
(declare-function xwidget-webkit-last-session "xwidget")
(declare-function xwidget-webkit--create-new-session-buffer "xwidget")
(declare-function get-buffer-xwidgets "xwidget")
(declare-function xwidget-live-p "xwidget")
(declare-function xwidget-webkit-reload "xwidget")
(defvar xwidget-webkit-buffer-name-format)

(defgroup xwapp nil
  "Elisp-backed web pages."
  :group 'tools
  :prefix "xwapp-")

(defcustom xwapp-browser nil
  "Non-nil: `xwapp-open' shows pages in the web browser, not in Emacs."
  :type 'boolean)

(defcustom xwapp-browse-function #'browse-url-default-browser
  "Function that shows a URL in the web browser.
One that runs scripts: the page is a program, not a document."
  :type 'function)


;;; HTTP

(defun xwapp-absolutize (loc base)
  "Turn possibly-relative LOC into an absolute URL using BASE."
  (cond
   ((and loc (string-match-p "\\`https?://" loc)) loc)
   ((and loc (string-prefix-p "//" loc))
    (concat "https:" loc))
   ((and loc (string-prefix-p "/" loc))
    (let ((u (url-generic-parse-url base)))
      (format "%s://%s%s" (or (url-type u) "https") (url-host u) loc)))
   (t loc)))

(defun xwapp-utf8-bytes (s)
  "UTF-8 unibyte bytes of S.  url.el errors on multibyte request bodies."
  (let ((out (encode-coding-string s 'utf-8)))
    (if (multibyte-string-p out)
        (encode-coding-string out 'iso-latin-1)
      out)))

(defun xwapp-scrub-error (msg)
  "Strip secrets and huge url.el dumps from MSG.
Masks any Authorization value, Bearer or not: ClickUp sends its
`pk_' token bare."
  (setq msg (replace-regexp-in-string
             "\\(Authorization: \\(?:Bearer \\)?\\)[^ \n]+" "\\1***" msg t))
  (if (string-match-p "Multibyte text in HTTP request" msg)
      "HTTP encoding error (non-ASCII in request). Retry after reload."
    msg))

(defun xwapp-extra-headers (headers &optional json-body)
  "HEADERS for `url-request-extra-headers'.
Drop Accept — `url-http-create-request' always emits Accept from
`url-mime-accept-string', and a second Accept can make GitHub 406."
  (let ((h (assoc-delete-all "Accept" (copy-alist headers))))
    (when json-body
      (push '("Content-Type" . "application/json; charset=utf-8") h))
    h))

(defun xwapp-http (url headers callback &optional method json-body hops)
  "Request URL with HEADERS.  CALLBACK is (lambda (body status-code err)).
METHOD defaults to GET.  JSON-BODY is an Elisp object json-encoded as the body.
3xx redirects are followed with Authorization kept (url.el would drop it)."
  (let ((url-request-method (or method "GET"))
        (url-request-extra-headers (xwapp-extra-headers headers json-body))
        (url-request-data
         (when json-body
           (xwapp-utf8-bytes (json-encode json-body))))
        (url-mime-accept-string (or (cdr (assoc "Accept" headers))
                                    "application/json"))
        (url-max-redirections 0)
        (url-show-status nil)
        (hops (or hops 0)))
    (url-retrieve
     url
     (lambda (status)
       (let ((err (plist-get status :error))
             (buf (current-buffer))
             code body location)
         (unwind-protect
             (progn
               (goto-char (point-min))
               (setq code (and (re-search-forward "^HTTP/[^ ]+ \\([0-9]+\\)" nil t)
                               (string-to-number (match-string 1))))
               (goto-char (point-min))
               (when (re-search-forward "^[Ll]ocation:[ \t]*\\(.*\\)$" nil t)
                 (setq location (string-trim (match-string 1))))
               (goto-char (point-min))
               (when (re-search-forward "\n\n" nil t)
                 (setq body (decode-coding-string
                             (buffer-substring-no-properties (point) (point-max))
                             'utf-8)))
               (if (and location
                        (memq code '(301 302 303 307 308))
                        (< hops 5))
                   (xwapp-http (xwapp-absolutize location url)
                               headers callback method json-body
                               (1+ hops))
                 (funcall callback body code err)))
           (when (buffer-live-p buf)
             (kill-buffer buf))))))))

(defun xwapp-parse-json (body)
  "Parse BODY as JSON into alists and lists; blank BODY reads as {}.
JSON false and null both become nil, so test them with the raw
string or a field that cannot be false."
  (json-parse-string (if (and body (not (string-empty-p (string-trim body))))
                         body "{}")
                     :object-type 'alist
                     :array-type 'list
                     :null-object nil
                     :false-object nil))

(defun xwapp-json-callback (url ok-fn fail-fn)
  "An `xwapp-http' callback for URL that hands parsed JSON to OK-FN.
A failed request calls FAIL-FN with (CODE ERR BODY URL).  So does a
response that arrives but cannot be handled -- unparsable JSON, or
OK-FN signalling -- with ERR the condition and BODY nil: a bug in
OK-FN then shows in the page instead of dying in a url.el timer.
url.el never reports an error alongside a 2xx, so a 2xx CODE with
ERR means exactly this case."
  (lambda (body code err)
    (if (or (and code (>= code 400)) err)
        (funcall fail-fn code err body url)
      (condition-case e
          (funcall ok-fn (xwapp-parse-json body))
        (error (funcall fail-fn code e nil url))))))

(defun xwapp-http-json (url headers ok-fn fail-fn &optional method json-body)
  "Request URL and hand the parsed JSON reply to OK-FN.
FAIL-FN is called as `xwapp-json-callback' describes."
  (xwapp-http url headers (xwapp-json-callback url ok-fn fail-fn)
              method json-body))


;;; Page bridge

(cl-defstruct (xwapp (:constructor xwapp-create)
                     (:copier nil))
  "One page-backed app: its buffer, page and intent channel."
  (buffer-name nil :documentation "Name of the app's xwidget buffer.")
  (index nil :documentation "Absolute path of the page's index.html.")
  (prefixes nil :documentation "Title prefixes that carry an intent.")
  (namespace nil :documentation "Global JS object `xwapp-js' calls into.")
  (idle-title nil :documentation "Title put back once an intent is read.")
  (handler nil :documentation "Function called with each intent alist.")
  (on-kill nil :documentation "Optional function run when the buffer dies.")
  (poll-interval 0.2 :documentation "Seconds between title polls.")
  (timer nil :documentation "The running poll timer.")
  (last-title nil :documentation "Last intent title handled."))

(defvar-local xwapp--xw nil
  "The xwidget this app buffer was created with.")

(defvar-local xwapp--app nil
  "The app whose page this buffer shows.")

(defun xwapp-buffer (app)
  "APP's live buffer, or nil."
  (get-buffer (xwapp-buffer-name app)))

(defun xwapp-session (app)
  "The live xwidget in APP's buffer, or nil.
Never another buffer's: with two apps open, the global \"last
session\" may be the other app's page."
  (when-let* ((buf (xwapp-buffer app)))
    (with-current-buffer buf
      (or (and xwapp--xw (xwidget-live-p xwapp--xw) xwapp--xw)
          (car (ignore-errors (get-buffer-xwidgets buf)))))))

(defun xwapp-js (app fn obj)
  "Call NAMESPACE[FN] with JSON-encoded OBJ in APP's page."
  (let ((ns (xwapp-namespace app)))
    (if (xwapp--get app :page)
        (xwapp--call app (format "{\"ns\":%s,\"fn\":%s,\"arg\":%s}"
                                 (json-encode ns) (json-encode fn) (json-encode obj)))
      (when-let* ((xw (xwapp-session app)))
        (xwidget-webkit-execute-script
         xw
         (format "window.%s && %s[%s](%s);"
                 ns ns (json-encode fn) (json-encode obj)))))))

(defun xwapp-live-p (app)
  "Non-nil while APP's page is open: in Emacs, or in a browser tab."
  (or (and (xwapp--get app :page) t)
      (xwapp-session app)))

(defun xwapp-seen-p (app)
  "Non-nil when you could be looking at APP's page.
In a browser: its tab in front, in a window that has focus.  In Emacs:
its buffer in a window of a visible frame that has focus -- an
iconified frame or one on another desktop is not looked at.  Focus the
window system cannot report counts as focus."
  (if (xwapp--get app :page)
      (xwapp--get app :seen)
    (when-let* ((buf (xwapp-buffer app))
                (win (get-buffer-window buf 'visible)))
      (not (null (frame-focus-state (window-frame win)))))))

(defun xwapp-reload (app)
  "Load APP's page again where it is, so edited ui/ files take effect."
  (if (xwapp--get app :page)
      (xwapp--call app "{\"xwapp\":\"reload\"}")
    (when-let* ((buf (xwapp-buffer app)))
      (with-current-buffer buf (xwidget-webkit-reload)))))

(defun xwapp-parse-intent (title prefixes)
  "The intent TITLE carries after one of PREFIXES, as an alist, or nil."
  (when (stringp title)
    (seq-some (lambda (prefix)
                (and (string-prefix-p prefix title)
                     (ignore-errors
                       (json-parse-string (substring title (length prefix))
                                          :object-type 'alist
                                          :array-type 'list))))
              prefixes)))

(defun xwapp--poll (app)
  "Read one intent from APP's page title and dispatch it."
  (when-let* ((xw (xwapp-session app)))
    (let* ((title (ignore-errors (xwidget-webkit-title xw)))
           (intent (xwapp-parse-intent title (xwapp-prefixes app))))
      (when (and intent (not (equal title (xwapp-last-title app))))
        (setf (xwapp-last-title app) title)
        (xwidget-webkit-execute-script
         xw (format "document.title = %s;" (json-encode (xwapp-idle-title app))))
        (funcall (xwapp-handler app) intent)))))

(defun xwapp-start-poll (app)
  "Start polling APP's page title for intents."
  (xwapp-stop-poll app)
  (setf (xwapp-timer app)
        (run-at-time 0.4 (xwapp-poll-interval app) #'xwapp--poll app)))

(defun xwapp-stop-poll (app)
  "Stop polling APP's page title."
  (when (timerp (xwapp-timer app))
    (cancel-timer (xwapp-timer app))
    (setf (xwapp-timer app) nil)))

(defun xwapp--on-kill (app)
  (xwapp-stop-poll app)
  (setf (xwapp-last-title app) nil)
  ;; A page that has moved to a browser tab is not closed.
  (when (and (xwapp-on-kill app) (not (xwapp--get app :page)))
    (funcall (xwapp-on-kill app))))

(defun xwapp-open (app)
  "Show APP's page on a fresh load: in Emacs, or in the web browser
when `xwapp-browser' is non-nil.
In Emacs the xwidget is created on first use and reused after that, and
a page open in a browser tab is let go: an app has one page at a time."
  (if xwapp-browser
      (xwapp-browse app)
    (unless (featurep 'xwidget-internal)
      (user-error "xwidget-webkit required. Rebuild Emacs with xwidgets"))
    (require 'xwidget)
    (let* ((index (xwapp-index app))
           (url (concat "file://" index))
           (name (xwapp-buffer-name app)))
      (unless (file-readable-p index)
        (user-error "Missing UI at %s" index))
      (xwapp--let-go app)
      (setf (xwapp-last-title app) nil)
      (if-let* ((buf (xwapp-buffer app)))
          (progn
            (pop-to-buffer buf)
            (with-current-buffer buf (setq-local xwapp--app app))
            (when-let* ((xw (xwapp-session app)))
              (xwidget-webkit-goto-uri xw url)))
        (let ((buf (xwidget-webkit--create-new-session-buffer url)))
          (switch-to-buffer buf)
          (setq-local xwidget-webkit-buffer-name-format name)
          (rename-buffer name t)
          (setq-local xwapp--app app)
          (setq-local xwapp--xw (or (xwidget-webkit-current-session)
                                    (xwidget-webkit-last-session)))
          (add-hook 'kill-buffer-hook (lambda () (xwapp--on-kill app)) nil t)
          (xwidget-webkit-goto-uri xwapp--xw url)))
      (xwapp-start-poll app))))


;;; In a web browser
;;
;; The same page, served by Emacs on 127.0.0.1.  There Emacs reads no
;; title and runs no script, so the page asks:
;;
;;   POST hello    the page has loaded.  It becomes the app's page, and
;;                 the one before -- a tab, or the xwidget -- is let go:
;;                 an app has one page, as it has one state in Emacs.
;;   GET next      Emacs's calls since the last, as a JSON array.  Held
;;                 until there are some, so one is always waiting.
;;   POST intent   one intent; the page sends them one at a time, so
;;                 they arrive in order.
;;
;; Emacs writes only into a request the page has made.  A stream it
;; pushed into would block Emacs the moment a tab stopped reading it;
;; a page that stops asking is only let go.
;;
;; Every URL starts with the app's secret, which only Emacs and the
;; page know: another site open in the browser can neither read the
;; page nor send it an intent.

(defconst xwapp--hold 25
  "Seconds a `next' is held before it is answered empty.")

(defconst xwapp--gone-after 10
  "Seconds a browser page may go without asking before it is let go.")

(defconst xwapp--max-request (* 8 1024 1024)
  "Bytes past which a request is refused.")

(defconst xwapp--reasons
  '((200 . "OK") (204 . "No Content") (400 . "Bad Request")
    (404 . "Not Found") (410 . "Gone") (413 . "Content Too Large")
    (500 . "Internal Server Error")))

(defvar xwapp--server nil
  "The process listening for browsers, or nil.")

(defvar xwapp--served (make-hash-table :test 'eq)
  "App -> plist of its life in a browser:
:secret   the first segment of its URLs
:page     the id of the page that said hello last, or nil
:waiting  that page's `next', held, or nil
:outbox   calls not yet sent to it, newest first
:seen     non-nil when it last said you could be looking at it
:hold :flush :gone   its timers.")

(defun xwapp--get (app key)
  (plist-get (gethash app xwapp--served) key))

(defun xwapp--put (app key value)
  (puthash app (plist-put (gethash app xwapp--served) key value) xwapp--served)
  value)

(defun xwapp--timer (app key secs fn)
  "Run FN with APP in SECS, as APP's timer KEY: one set before is dropped."
  (xwapp--untimer app key)
  (xwapp--put app key (run-at-time secs nil fn app)))

(defun xwapp--untimer (app key)
  (when (timerp (xwapp--get app key))
    (cancel-timer (xwapp--get app key)))
  (xwapp--put app key nil))

(defun xwapp--secret (app)
  "APP's secret: 16 random bytes in hex, made on first use."
  (or (xwapp--get app :secret)
      (xwapp--put app :secret
                  (with-temp-buffer
                    (set-buffer-multibyte nil)
                    (insert-file-contents-literally "/dev/urandom" nil nil 16)
                    (mapconcat (lambda (b) (format "%02x" b)) (buffer-string) "")))))

(defun xwapp--app-of (secret)
  "The app whose secret is SECRET, or nil."
  (catch 'found
    (maphash (lambda (app plist)
               (when (equal secret (plist-get plist :secret))
                 (throw 'found app)))
             xwapp--served)
    nil))

(defun xwapp--root (app)
  "The directory with APP's package and xwapp in it.
The page reaches the kit as ../../xwapp/ui, there and here alike."
  (expand-file-name "../../" (file-name-directory (xwapp-index app))))

(defun xwapp--port ()
  "The port pages are served on, listening first if need be."
  (unless (process-live-p xwapp--server)
    (setq xwapp--server
          (make-network-process :name "xwapp" :server t :noquery t
                                :host "127.0.0.1" :service t :family 'ipv4
                                :coding 'binary
                                :filter #'xwapp--filter
                                :sentinel #'xwapp--sentinel)))
  (process-contact xwapp--server :service))

(defun xwapp-url (app)
  "The address of APP's page in the web browser.  It holds APP's secret."
  (format "http://127.0.0.1:%d/%s/%s" (xwapp--port) (xwapp--secret app)
          (file-relative-name (xwapp-index app) (xwapp--root app))))

(defun xwapp-browse (app)
  "Show APP's page in the web browser, on a fresh load.
Interactively, the page in this buffer, which moves there: the tab
takes it once loaded, and this buffer is killed."
  (interactive (list (or xwapp--app (user-error "No xwapp page in this buffer"))))
  (unless (file-readable-p (xwapp-index app))
    (user-error "Missing UI at %s" (xwapp-index app)))
  ;; A tab closed a moment ago is not to close the app on the new one.
  (xwapp--untimer app :gone)
  (funcall xwapp-browse-function (xwapp-url app)))

;; Serving

(defun xwapp--filter (proc bytes)
  "Gather the request on PROC from BYTES as they come, then answer it."
  (unless (process-get proc 'xwapp-taken)
    (set-process-query-on-exit-flag proc nil)
    (let ((in (concat (process-get proc 'xwapp-in) bytes)))
      (process-put proc 'xwapp-in in)
      (if (> (length in) xwapp--max-request)
          (xwapp--respond proc 413)
        (when-let* ((request (xwapp--request in)))
          (process-put proc 'xwapp-taken t)
          (process-put proc 'xwapp-in nil)
          (condition-case err
              (xwapp--answer proc request)
            (error
             (message "xwapp: %s" (error-message-string err))
             (xwapp--respond proc 500))))))))

(defun xwapp--request (in)
  "The request IN holds, as (METHOD TARGET HEADERS BODY), once all in."
  (when-let* ((end (string-search "\r\n\r\n" in)))
    (let* ((lines (split-string (substring in 0 end) "\r\n"))
           (headers (delq nil (mapcar (lambda (line)
                                        (when (string-match "\\`\\([^:]+\\):[ \t]*\\(.*?\\)[ \t]*\\'" line)
                                          (cons (downcase (match-string 1 line))
                                                (match-string 2 line))))
                                      (cdr lines))))
           (size (string-to-number (or (cdr (assoc "content-length" headers)) "0")))
           (body (substring in (+ end 4))))
      (when (>= (length body) size)
        (pcase-let ((`(,method ,target) (split-string (car lines) " ")))
          (list method target headers (substring body 0 size)))))))

(defun xwapp--from-here-p (headers)
  "Non-nil when HEADERS name this server as the host, and as the origin
when there is one: a name rebound to 127.0.0.1 gets nothing, nor does
a page of another site that has the secret."
  (let* ((port (process-contact xwapp--server :service))
         (here (list (format "127.0.0.1:%d" port) (format "localhost:%d" port)))
         (origin (cdr (assoc "origin" headers))))
    (and (member (cdr (assoc "host" headers)) here)
         (or (null origin)
             (member origin (mapcar (lambda (h) (concat "http://" h)) here))))))

(defun xwapp--answer (proc request)
  "Answer REQUEST, which came on PROC."
  (pcase-let* ((`(,method ,target ,headers ,body) request)
               (`(,path ,query) (split-string (or target "") "?"))
               (`(,secret . ,rest) (split-string path "/" t))
               (app (and secret (xwapp--app-of secret)))
               (page (cadr (assoc "page" (url-parse-query-string (or query ""))))))
    (cond
     ((not (and app (xwapp--from-here-p headers))) (xwapp--respond proc 404))
     ((and (equal method "POST") (equal rest '("hello")) page)
      (xwapp--hello app proc page))
     ((and (equal method "GET") (equal rest '("next")))
      (xwapp--next app proc page))
     ((and (equal method "POST") (equal rest '("intent")))
      (xwapp--intent app proc page body))
     ((equal method "GET") (xwapp--file app proc rest))
     (t (xwapp--respond proc 404)))))

(defun xwapp--respond (proc code &optional type body)
  "Answer PROC with CODE, and BODY (bytes) of TYPE, then close it."
  (ignore-errors
    (process-send-string
     proc
     (concat (format "HTTP/1.1 %d %s\r\n" code (alist-get code xwapp--reasons ""))
             (if type (format "Content-Type: %s\r\n" type) "")
             (format "Content-Length: %d\r\n" (length body))
             "Cache-Control: no-store\r\n"
             "Referrer-Policy: no-referrer\r\n"
             "X-Content-Type-Options: nosniff\r\n"
             "Connection: close\r\n\r\n"
             body)))
  (ignore-errors (delete-process proc)))

(defun xwapp--file (app proc parts)
  "Answer PROC with the file PARTS name: of APP's page, or of xwapp's kit."
  (let* ((root (xwapp--root app))
         (file (expand-file-name (string-join parts "/") root)))
    (if (and parts
             (not (seq-some (lambda (p) (string-prefix-p "." p)) parts))
             (seq-some (lambda (dir) (string-prefix-p dir file))
                       (list (file-name-directory (xwapp-index app))
                             (expand-file-name "xwapp/ui/" root)))
             (file-regular-p file))
        (xwapp--respond proc 200
                        (pcase (file-name-extension file)
                          ("html" "text/html; charset=utf-8")
                          ("js" "text/javascript; charset=utf-8")
                          ("css" "text/css; charset=utf-8")
                          ("svg" "image/svg+xml")
                          ("png" "image/png")
                          (_ "application/octet-stream"))
                        (with-temp-buffer
                          (set-buffer-multibyte nil)
                          (insert-file-contents-literally file)
                          (buffer-string)))
      (xwapp--respond proc 404))))

;; The page's life

(defun xwapp--current-p (app page)
  "Non-nil when PAGE is APP's page: the one that said hello last."
  (and page (equal page (xwapp--get app :page))))

(defun xwapp--hello (app proc page)
  "PAGE has loaded in a browser tab: it is APP's page now."
  (xwapp--let-go app)
  (xwapp--put app :page page)
  (xwapp--timer app :gone xwapp--gone-after #'xwapp--gone)
  ;; Moved, not closed: with :page set, the buffer's death runs no on-kill.
  (when-let* ((buf (xwapp-buffer app)))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buf)))
  (xwapp--respond proc 204))

(defun xwapp--next (app proc page)
  "Hold PROC, PAGE's request for calls, until there are some."
  (if (not (xwapp--current-p app page))
      (xwapp--respond proc 410)
    (xwapp--untimer app :gone)
    (when-let* ((old (xwapp--get app :waiting)))
      (xwapp--put app :waiting nil)
      (xwapp--respond old 200 "application/json" "[]"))
    (process-put proc 'xwapp-app app)
    (xwapp--put app :waiting proc)
    (if (xwapp--get app :outbox)
        (xwapp--timer app :flush 0 #'xwapp--flush)
      (xwapp--timer app :hold xwapp--hold #'xwapp--flush))))

(defun xwapp--call (app json)
  "Send JSON, one call, to APP's browser page."
  (xwapp--put app :outbox (cons json (xwapp--get app :outbox)))
  ;; On the next turn, so the calls of one command go out together.
  (when (and (xwapp--get app :waiting) (not (xwapp--get app :flush)))
    (xwapp--timer app :flush 0 #'xwapp--flush)))

(defun xwapp--flush (app)
  "Answer APP's held `next' with the calls gathered since the last."
  (xwapp--untimer app :hold)
  (xwapp--untimer app :flush)
  (when-let* ((proc (xwapp--get app :waiting)))
    (let ((calls (reverse (xwapp--get app :outbox))))
      (xwapp--put app :waiting nil)
      (xwapp--put app :outbox nil)
      (xwapp--timer app :gone xwapp--gone-after #'xwapp--gone)
      (xwapp--respond proc 200 "application/json"
                      (encode-coding-string (concat "[" (string-join calls ",") "]")
                                            'utf-8)))))

(defun xwapp--intent (app proc page body)
  "Take PAGE's intent BODY: xwapp's own now, the app's on the next turn."
  (let ((intent (ignore-errors
                  (json-parse-string (decode-coding-string body 'utf-8)
                                     :object-type 'alist :array-type 'list))))
    (cond
     ((not (xwapp--current-p app page)) (xwapp--respond proc 410))
     ((not (consp intent)) (xwapp--respond proc 400))
     (t
      (xwapp--respond proc 204)
      (if (equal (alist-get 'op intent) "xwapp-seen")
          (xwapp--put app :seen (eq (alist-get 'seen intent) t))
        ;; As the title's intents are: from a timer, not this filter.
        (run-at-time 0 nil (xwapp-handler app) intent))))))

(defun xwapp--sentinel (proc _event)
  "A held `next' whose tab hung up: the page may be gone."
  (when-let* ((app (process-get proc 'xwapp-app)))
    (when (and (not (process-live-p proc))
               (eq proc (xwapp--get app :waiting)))
      (xwapp--put app :waiting nil)
      (xwapp--untimer app :hold)
      (xwapp--untimer app :flush)
      (xwapp--timer app :gone xwapp--gone-after #'xwapp--gone))))

(defun xwapp--let-go (app)
  "Let APP's browser page go: told so if it is waiting, forgotten anyway."
  (dolist (key '(:hold :flush :gone))
    (xwapp--untimer app key))
  (let ((proc (xwapp--get app :waiting)))
    ;; Before answering: the sentinel must not take the hang-up for a
    ;; page that went.
    (dolist (key '(:waiting :page :outbox :seen))
      (xwapp--put app key nil))
    (when proc
      (xwapp--respond proc 410))))

(defun xwapp--gone (app)
  "APP's browser page has stopped asking: it was closed."
  (xwapp--let-go app)
  (when (and (xwapp-on-kill app) (not (xwapp-session app)))
    (funcall (xwapp-on-kill app))))


;;; Misc

(defun xwapp-copy (text)
  "Put TEXT on the kill ring and the system clipboard."
  (kill-new text)
  (when (fboundp 'gui-set-selection)
    (gui-set-selection 'CLIPBOARD text)
    (ignore-errors (gui-set-selection 'PRIMARY text))))

(provide 'xwapp)
;;; xwapp.el ends here

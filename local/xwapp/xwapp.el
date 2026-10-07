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
;; One `xwapp' struct per app names its buffer, page and intent prefix.
;;
;; The HTTP layer is url.el with the fixes pr-view paid for: redirects
;; keep Authorization, Accept is not sent twice, bodies go out as UTF-8
;; bytes, and JSON false/null both read as nil.

;;; Code:

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
(defvar xwidget-webkit-buffer-name-format)


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
  (when-let* ((xw (xwapp-session app)))
    (let ((ns (xwapp-namespace app)))
      (xwidget-webkit-execute-script
       xw
       (format "window.%s && %s[%s](%s);"
               ns ns (json-encode fn) (json-encode obj))))))

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
  (when (xwapp-on-kill app)
    (funcall (xwapp-on-kill app))))

(defun xwapp-open (app)
  "Show APP's buffer on a fresh load of its page and start polling.
The xwidget is created on first use and reused after that."
  (unless (featurep 'xwidget-internal)
    (user-error "xwidget-webkit required. Rebuild Emacs with xwidgets"))
  (require 'xwidget)
  (let* ((index (xwapp-index app))
         (url (concat "file://" index))
         (name (xwapp-buffer-name app)))
    (unless (file-readable-p index)
      (user-error "Missing UI at %s" index))
    (setf (xwapp-last-title app) nil)
    (if-let* ((buf (xwapp-buffer app)))
        (progn
          (pop-to-buffer buf)
          (when-let* ((xw (xwapp-session app)))
            (xwidget-webkit-goto-uri xw url)))
      (let ((buf (xwidget-webkit--create-new-session-buffer url)))
        (switch-to-buffer buf)
        (setq-local xwidget-webkit-buffer-name-format name)
        (rename-buffer name t)
        (setq-local xwapp--xw (or (xwidget-webkit-current-session)
                                  (xwidget-webkit-last-session)))
        (add-hook 'kill-buffer-hook (lambda () (xwapp--on-kill app)) nil t)
        (xwidget-webkit-goto-uri xwapp--xw url)))
    (xwapp-start-poll app)))


;;; Misc

(defun xwapp-copy (text)
  "Put TEXT on the kill ring and the system clipboard."
  (kill-new text)
  (when (fboundp 'gui-set-selection)
    (gui-set-selection 'CLIPBOARD text)
    (ignore-errors (gui-set-selection 'PRIMARY text))))

(provide 'xwapp)
;;; xwapp.el ends here

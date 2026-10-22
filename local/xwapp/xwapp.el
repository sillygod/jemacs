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

(defcustom xwapp-port nil
  "Port the pages are served on to a web browser, or nil for any free one.
A proxy that brings them to your other devices needs it fixed: see
`xwapp-remote-hosts'."
  :type '(choice (const :tag "Any free port" nil) natnum))

(defcustom xwapp-remote-hosts nil
  "Names under which your other devices reach the pages, through a proxy
on this machine: this machine's name on your tailnet, say, with
`tailscale serve' passing it on to `xwapp-port'.  A request under one is
answered only for one of `xwapp-remote-users'; nil serves this machine
alone.  While there are any, an app's secret is kept over restarts, so
an address you saved on a phone keeps working: `xwapp-copy-remote-url'
gives it, `xwapp-renew-secret' ends it."
  :type '(repeat string))

(defcustom xwapp-remote-users nil
  "Tailscale logins answered under `xwapp-remote-hosts' -- yours.
`tailscale serve' names the user of the device a request comes from in
Tailscale-User-Login, having taken off any such header the device sent;
a request with none, or another's, is not answered."
  :type '(repeat string))

(defcustom xwapp-remote-scheme "https"
  "How your other devices reach the pages under `xwapp-remote-hosts':
\"https\" when the proxy has a certificate (`tailscale serve --https'),
\"http\" when it has none (`--http').  Only the addresses it gives say so."
  :type '(choice (const "https") (const "http")))

(defcustom xwapp-remote-socket (locate-user-emacs-file "xwapp/remote.sock")
  "A Unix socket the proxy for `xwapp-remote-hosts' forwards to, in place
of the loopback port the browser uses.

What a request came over is the one thing a header cannot forge.  The
browser speaks TCP to 127.0.0.1, so a program that reaches loopback can
dress a request as the proxy's -- its tailnet Host, its
Tailscale-User-Login -- and be answered as one of your other devices,
secret and all (seen 2026-10-13).  So the proxy is pointed at this
socket instead, and only a request that arrives on it is treated as
having come through the proxy: a page for your other devices, or a
secret through `/open/'.  The loopback port serves this machine alone.

The socket and the directory holding it are made yours alone, so another
account cannot open it; a sandbox that reaches loopback but not this
path cannot either.  One that is also you, with the run of your files,
is not kept out -- that is past what a transport can tell.  Point the
proxy here with `xwapp-remote-serve-command'."
  :type 'string)


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

;; Its layout is compiled into every package that uses it -- the
;; constructor and the accessors inline -- and straight rebuilds a
;; package only when its own files change.  A slot added here would
;; shift every slot after it under a pr-view.elc built before, so what
;; an app gains goes into `xwapp--served' instead, as :reopen did.
(cl-defstruct (xwapp (:constructor xwapp--make)
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

(defun xwapp-create (&rest slots)
  "A new app, of SLOTS: the keywords of `xwapp''s slots, and :reopen.

:reopen is a function that does what the app's command does before
showing its page, for a browser tab that comes back to an app that is
not open: one left open while Emacs restarted, or let go while the
browser had it frozen.  The tab then loads again, on its own.  Without
one, such a tab says to reload it, or to open it from Emacs.

While the page of an app with :reopen is in a browser tab, its address
outlives Emacs (`xwapp-state-file'), so the tab can find it after a
restart.  Name an autoloaded function there: once Emacs is up,
`xwapp-resume' loads the app through it, to listen for the tab."
  (let ((app (apply #'xwapp--make
                     (cl-loop for (key value) on slots by #'cddr
                              unless (eq key :reopen) append (list key value))))
        (reopen (plist-get slots :reopen)))
    (when reopen
      (xwapp--put app :reopen reopen)
      (xwapp--restore app))
    app))

(defun xwapp-reopen (app)
  "APP's :reopen, or nil; see `xwapp-create'."
  (xwapp--get app :reopen))

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

;; Keeping a crashed WebKit from wedging Emacs.
;;
;; The page runs in an xwidget-webkit view, driven over a WebKit IPC
;; channel: Emacs reads its title each poll and pushes updates with
;; `xwidget-webkit-execute-script'.  WebKit can abort that channel (the
;; Networking helper faults with EXC_GUARD on `didReceiveInvalidMessage');
;; it happened 13 times one evening, escalating into a 100%-CPU freeze.
;; After the abort the web process is gone but still answers
;; `xwidget-live-p', so a poll that keeps reading and scripting it is the
;; inferred spin: Emacs on a broken channel every interval.
;;
;; Defensive, not a proven cure (the exact message WebKit rejected was not
;; reproduced -- in a scratch GUI Emacs an external helper kill recovered
;; cleanly and an 8MB script was tolerated): (1) a breaker stops the poll
;; once title reads start failing, so a channel that does stop answering is
;; not driven in a loop; (2) every script is scrubbed to clean UTF-8, the
;; one invalid-message class we can rule out cheaply; (3) a generous size
;; cap backstops a runaway payload.  None of the three changes the happy
;; path -- scrub copies nothing for clean text, the cap never fires on a
;; normal push, the breaker acts only after repeated failures.

(defcustom xwapp-max-script-bytes (* 4 1024 1024)
  "Largest script xwapp sends a page in one call, in bytes.
A larger update is dropped.  A backstop against a runaway payload, set
well above any normal push (8MB was tolerated in a scratch test, so this
never fires on real updates); it is not the confirmed crash trigger."
  :type 'integer
  :group 'xwapp)

(defcustom xwapp-poll-fail-max 5
  "Consecutive failed title reads before xwapp stops driving a page.
A crashed WebKit process still answers `xwidget-live-p', so without this
the poll keeps calling into the broken IPC channel every interval.  A
fresh `xwapp-open' clears it."
  :type 'integer
  :group 'xwapp)

(defun xwapp--scrub (s)
  "S with any char that is not a valid Unicode scalar made U+FFFD.
A raw eight-bit byte (> #x10FFFF in Emacs) or a lone surrogate encodes to
invalid UTF-8, which the WebKit IPC guard can abort on.  Returns S itself
when it is already clean, so the common path copies nothing."
  (if (and (multibyte-string-p s)
           (seq-some (lambda (c) (or (> c #x10FFFF) (<= #xD800 c #xDFFF))) s))
      (concat (mapcar (lambda (c) (if (or (> c #x10FFFF) (<= #xD800 c #xDFFF)) #xFFFD c)) s))
    s))

(defun xwapp--script-safe (script)
  "SCRIPT fit to hand WebKit -- scrubbed to clean UTF-8 and within
`xwapp-max-script-bytes' -- or nil, with a message, when too big to send."
  (let* ((clean (xwapp--scrub script))
         (bytes (string-bytes clean)))
    (if (> bytes xwapp-max-script-bytes)
        (progn
          (message "xwapp: dropped a %d-byte page update over the %d-byte cap"
                   bytes xwapp-max-script-bytes)
          nil)
      clean)))

(defun xwapp--poll-failed (app)
  "Count a failed drive of APP's page; past `xwapp-poll-fail-max', stop.
A crashed WebKit web process looks live but cannot be read or scripted;
polling it every interval is what spins Emacs on the broken IPC channel,
so stop the timer and mark the app wedged, for a reopen to recover."
  (let ((n (1+ (or (xwapp--get app :poll-fails) 0))))
    (xwapp--put app :poll-fails n)
    (when (>= n xwapp-poll-fail-max)
      (xwapp--put app :wedged t)
      (xwapp-stop-poll app)
      (message "xwapp: stopped driving %s -- its page will not answer (crashed WebKit?); reopen to recover"
               (xwapp-buffer-name app)))))

(defun xwapp--exec (app xw script)
  "Run SCRIPT in XW for APP, scrubbed and size-capped.
Does nothing once APP is wedged.  A call that signals counts toward the
breaker, so a page that starts rejecting scripts stops being driven."
  (unless (xwapp--get app :wedged)
    (when-let* ((safe (xwapp--script-safe script)))
      (condition-case nil
          (xwidget-webkit-execute-script xw safe)
        (error (xwapp--poll-failed app))))))

(defun xwapp-js (app fn obj)
  "Call NAMESPACE[FN] with JSON-encoded OBJ in APP's page."
  (let ((ns (xwapp-namespace app)))
    (if (xwapp--get app :page)
        (xwapp--call app (format "{\"ns\":%s,\"fn\":%s,\"arg\":%s}"
                                 (json-encode ns) (json-encode fn) (json-encode obj)))
      (when-let* ((xw (xwapp-session app)))
        (xwapp--exec
         app xw
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
  "Read one intent from APP's page title and dispatch it.
A title that cannot be read -- a crashed WebKit process still looks live
-- counts toward the breaker; a run of them stops the poll rather than
spin Emacs reading and scripting a broken IPC channel every interval."
  (when-let* ((xw (xwapp-session app)))
    (let ((title (condition-case nil
                     (prog1 (xwidget-webkit-title xw)
                       (xwapp--put app :poll-fails 0))
                   (error (xwapp--poll-failed app) :failed))))
      (unless (eq title :failed)
        (let ((intent (xwapp-parse-intent title (xwapp-prefixes app))))
          (when (and intent (not (equal title (xwapp-last-title app))))
            (setf (xwapp-last-title app) title)
            (xwapp--exec app xw (format "document.title = %s;"
                                        (json-encode (xwapp-idle-title app))))
            (xwapp--dispatch app intent)))))))

(defun xwapp-start-poll (app)
  "Start polling APP's page title for intents."
  (xwapp-stop-poll app)
  (xwapp--put app :poll-fails 0)
  (xwapp--put app :wedged nil)
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

(defvar xwapp--from-browser nil
  "Non-nil while an intent from a page in a browser tab is handled.")

(defun xwapp-open (app)
  "Show APP's page on a fresh load: in Emacs, or in the web browser
when `xwapp-browser' is non-nil -- or when a page in the browser asked
for it: a page opens where the page you clicked in is.
In Emacs the xwidget is created on first use and reused after that, and
a page open in a browser tab is let go: an app has one page at a time."
  (if (or xwapp-browser xwapp--from-browser)
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
      (xwapp--save)
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
;;
;; A page that is not the app's is answered 410, with why:
;;
;;   taken    another page is the app's, in a tab or in Emacs.  The tab
;;            says so; reloading it takes the app back.
;;   free     no page is, and the app can open again (`xwapp-create's
;;            :reopen).  The tab loads again on its own, and comes back
;;            with a soft hello, which takes nothing another page has.
;;   closed   no page is, and the app cannot open again: reloading the
;;            tab opens it, or Emacs does.
;;
;; While a tab is the page of an app that can open again, the app's
;; secret and the port are kept in `xwapp-state-file'.  After a
;; restart, Emacs listens there again (`xwapp-resume'), so the tab,
;; which asks again while Emacs does not answer, finds it free.

(defconst xwapp--hold 25
  "Seconds a `next' is held before it is answered empty.")

(defconst xwapp--gone-after 10
  "Seconds a browser page may go without asking before it is let go.")

(defconst xwapp--max-request (* 8 1024 1024)
  "Bytes past which a request is refused.")

(defconst xwapp--reasons
  '((200 . "OK") (204 . "No Content") (302 . "Found") (400 . "Bad Request")
    (404 . "Not Found") (410 . "Gone") (413 . "Content Too Large")
    (500 . "Internal Server Error")))

(defconst xwapp--come-back-within 60
  "Seconds a restored address waits for its tab before it is not kept.")

;;;###autoload
(defvar xwapp-state-file (locate-user-emacs-file "xwapp-browser.eld")
  "Where the address of a page in a browser tab is kept while it is there,
for the tab to find Emacs again after a restart.  It holds the page's
secret: Emacs makes it readable by you alone.")

(defvar xwapp--keep (not noninteractive)
  "Non-nil when addresses outlive Emacs.  A batch Emacs has no tabs.")

(defvar xwapp--server nil
  "The process listening on loopback for browsers, or nil.")

(defvar xwapp--unix-server nil
  "The process listening on `xwapp-remote-socket' for the proxy, or nil.
A request on it came through the proxy; one on `xwapp--server' did not.")

(defvar xwapp--served (make-hash-table :test 'eq)
  "App -> plist of its life in a browser:
:reopen   what `xwapp-create' was given as :reopen
:secret   the first segment of its URLs
:page     the id of the page that said hello last, or nil
:waiting  that page's `next', held, or nil
:outbox   calls not yet sent to it, newest first
:seen     non-nil when it last said you could be looking at it
:asked    non-nil from `xwapp-browse' until a tab says hello: that tab
          is the one Emacs opened, and the app is ready for it
:restored non-nil from a restart until its tab comes back, or does not
:hold :flush :gone :forget   its timers.")

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

(defun xwapp--listen (port)
  "Listen for browsers on loopback PORT, t for any.
A request here is `local': the browser's, or any program that reaches
loopback -- never trusted as the proxy's, whatever its headers say."
  (make-network-process :name "xwapp" :server t :noquery t
                        :host "127.0.0.1" :service port :family 'ipv4
                        :coding 'binary
                        :filter (lambda (proc bytes) (xwapp--filter proc bytes 'local))
                        :sentinel #'xwapp--sentinel))

(defun xwapp--listen-unix ()
  "Listen on `xwapp-remote-socket' for the proxy; return the process.
The socket and its directory are made yours alone, so reaching it is
what a request from the proxy can do and a forger on loopback cannot: a
request here is `proxy'.  A stale socket from a past run is replaced."
  (let* ((sock (expand-file-name xwapp-remote-socket))
         (dir (file-name-directory sock)))
    (make-directory dir t)
    (set-file-modes dir #o700)
    (when (file-exists-p sock) (delete-file sock))
    (with-file-modes #o600
      (make-network-process :name "xwapp-unix" :server t :noquery t
                            :family 'local :service sock
                            :coding 'binary
                            :filter (lambda (proc bytes) (xwapp--filter proc bytes 'proxy))
                            :sentinel #'xwapp--sentinel))))

(defun xwapp--port ()
  "The port pages are served on, listening first if need be: on the one
kept in `xwapp-state-file' while it is free, which a tab from before a
restart still asks.  With `xwapp-remote-hosts', the proxy's socket is
listened on too."
  (unless (process-live-p xwapp--server)
    (let ((kept (plist-get (xwapp--state) :port)))
      (setq xwapp--server
            (cond (xwapp-port
                   (condition-case err
                       (xwapp--listen xwapp-port)
                     (error (user-error "xwapp: cannot listen on port %d (`xwapp-port'): %s"
                                        xwapp-port (error-message-string err)))))
                  ((and kept (ignore-errors (xwapp--listen kept))))
                  (t (xwapp--listen t))))))
  (when (and xwapp-remote-hosts (not (process-live-p xwapp--unix-server)))
    (condition-case err
        (setq xwapp--unix-server (xwapp--listen-unix))
      (error (message "xwapp: cannot listen on %s (`xwapp-remote-socket'): %s"
                      xwapp-remote-socket (error-message-string err)))))
  (process-contact xwapp--server :service))

(defun xwapp--page (app)
  "APP's page under the root it is served from: <pkg>/ui/index.html."
  (file-relative-name (xwapp-index app) (xwapp--root app)))

(defun xwapp--path (app)
  "The path of APP's page in a URL, its secret first."
  (format "/%s/%s" (xwapp--secret app) (xwapp--page app)))

(defun xwapp-url (app)
  "The address of APP's page in the web browser.  It holds APP's secret."
  (format "http://127.0.0.1:%d%s" (xwapp--port) (xwapp--path app)))

(defun xwapp--remote-host (app host)
  "HOST, or the first of `xwapp-remote-hosts', for APP's page from your
other devices -- its address kept over restarts from now on."
  (let ((host (or host (car xwapp-remote-hosts)
                  (user-error "No `xwapp-remote-hosts': pages are served to this machine alone"))))
    (unless (xwapp-reopen app)
      (user-error "This page cannot open again by itself, so it cannot be reached from elsewhere"))
    (xwapp--secret app)
    (xwapp--port)
    (xwapp--save)
    host))

(defun xwapp-remote-url (app &optional host)
  "The address of APP's page from your other devices, under HOST -- the
first of `xwapp-remote-hosts' by default.  It holds APP's secret, kept
over restarts from now on."
  (format "%s://%s%s" xwapp-remote-scheme (xwapp--remote-host app host) (xwapp--path app)))

(defun xwapp-remote-open-url (app &optional fragment host)
  "An address of APP's page from your other devices that does not hold
its secret: for a message that passes through others' hands, a chat's
server keeping what it carries.  Asked there by one of
`xwapp-remote-users', it sends the browser on to the page under its
secret, FRAGMENT kept -- the page's own place, say."
  (format "%s://%s/open/%s%s" xwapp-remote-scheme (xwapp--remote-host app host)
          (car (split-string (xwapp--page app) "/"))
          (if fragment (concat "#" fragment) "")))

(defun xwapp--read-app (prompt)
  "An app that can open again, read with PROMPT by its buffer name."
  (let (apps)
    (maphash (lambda (app _) (when (xwapp-reopen app) (push (cons (xwapp-buffer-name app) app) apps)))
             xwapp--served)
    (unless apps (user-error "No page that can open again is loaded"))
    (cdr (assoc (completing-read prompt apps nil t nil nil (caar apps)) apps))))

;;;###autoload
(defun xwapp-copy-remote-url (app)
  "Copy the address of APP's page from your other devices.
It holds the page's secret, so it is not shown.
Opened there, the page is that device's -- the one in Emacs or in a tab
here lets go of it, as for any other tab."
  (interactive (list (xwapp--read-app "Address of the page: ")))
  (xwapp-copy (xwapp-remote-url app))
  (message "Copied the address of %s for your other devices; it holds the page's secret"
           (string-trim (xwapp-buffer-name app))))

(defun xwapp-remote-serve-command ()
  "The `tailscale serve' command that points the proxy at
`xwapp-remote-socket', reproducing `xwapp-remote-scheme'.
It replaces the `/' mount of the serve config, so the pages reach your
tailnet over the socket, not the loopback port a local program can
dress up as the proxy."
  (format "tailscale serve --bg %sunix:%s"
          (if (equal xwapp-remote-scheme "http") "--http=80 " "")
          (expand-file-name xwapp-remote-socket)))

;;;###autoload
(defun xwapp-remote-serve-setup ()
  "Copy the `tailscale serve' command that points the proxy at this
machine's socket, for you to run.  Reconfiguring the proxy is yours to
do: this only hands you the exact command."
  (interactive)
  (unless xwapp-remote-hosts
    (user-error "No `xwapp-remote-hosts': nothing reaches this machine from elsewhere"))
  (let ((cmd (xwapp-remote-serve-command)))
    (xwapp-copy cmd)
    (message "Copied; run it to point the proxy at the socket:\n%s" cmd)))

;;;###autoload
(defun xwapp-renew-secret (app)
  "Give APP a new secret: the addresses given out for it stop working,
and a page open in a tab says to open it again."
  (interactive (list (xwapp--read-app "New secret for the page: ")))
  (xwapp--let-go app)
  (xwapp--put app :secret nil)
  (xwapp--put app :restored nil)
  (xwapp--secret app)
  (xwapp--save)
  (message "%s has a new secret; copy its address again" (string-trim (xwapp-buffer-name app))))

;; The page's own button for it: an element with data-xw-phone, shown
;; when Emacs answers the page's hello that it can be reached so, and
;; pressed, the address as a QR code for the phone's camera.

(defun xwapp--kit (app call)
  "Send CALL, an alist, to the page kit of APP's page."
  (let ((json (json-encode call)))
    (if (xwapp--get app :page)
        (xwapp--call app json)
      (when-let* ((xw (xwapp-session app)))
        (xwidget-webkit-execute-script xw (format "window.XW && XW.kit(%s);" json))))))

(defun xwapp--greet (app)
  "Tell APP's page, which has just loaded, that your other devices can
reach it -- when they can.  Its button is hidden until then."
  (when (and xwapp-remote-hosts (xwapp-reopen app))
    (xwapp--kit app '((xwapp . "phone")))))

(defun xwapp--show-qr (app)
  "Show APP's address from your other devices in its page, as a QR code."
  (xwapp--kit app (condition-case err
                      `((xwapp . "qr") (url . ,(xwapp-remote-url app))
                        (host . ,(car xwapp-remote-hosts)))
                    (user-error `((xwapp . "qr") (error . ,(error-message-string err)))))))

(defun xwapp--dispatch (app intent)
  "Hand INTENT, from APP's page, to the kit when it is the kit's, else to APP."
  (pcase (alist-get 'op intent)
    ("xwapp-hello" (xwapp--greet app))
    ("xwapp-qr" (xwapp--show-qr app))
    (_ (funcall (xwapp-handler app) intent))))

;; Over a restart

(defun xwapp--state ()
  "What `xwapp-state-file' holds: (:port PORT :pages ((NAME SECRET
REOPEN) ...)), each page an app's buffer name, secret and :reopen.
Nil when there is none, or nothing sound in it."
  (when (and xwapp--keep (file-readable-p xwapp-state-file))
    (let ((state (ignore-errors
                   (with-temp-buffer
                     (insert-file-contents xwapp-state-file)
                     (read (current-buffer))))))
      (when (and (plistp state) (natnump (plist-get state :port)))
        (list :port (plist-get state :port)
              :pages (seq-filter
                      (lambda (page)
                        (pcase page
                          (`(,(pred stringp) ,(and (pred stringp) secret) ,(pred symbolp))
                           (string-match-p "\\`[0-9a-f]\\{32\\}\\'" secret))))
                      (plist-get state :pages)))))))

(defvar xwapp--state-ours nil
  "Non-nil once this Emacs has kept or restored `xwapp-state-file':
another Emacs's is not its to remove.")

(defun xwapp--save ()
  "Keep in `xwapp-state-file' the address of each page in a browser tab
whose app can open again, or remove it when there is none."
  (when xwapp--keep
    (let (pages)
      (maphash (lambda (app plist)
                 (when (and (xwapp-reopen app) (plist-get plist :secret)
                            (or (plist-get plist :page) (plist-get plist :restored)
                                ;; An address on another device: kept for it.
                                xwapp-remote-hosts))
                   (push (list (xwapp-buffer-name app) (plist-get plist :secret)
                               (and (symbolp (xwapp-reopen app)) (xwapp-reopen app)))
                         pages)))
               xwapp--served)
      (condition-case err
          (cond
           ((and pages (process-live-p xwapp--server))
            (with-file-modes #o600
              (with-temp-file xwapp-state-file
                (prin1 (list :port (process-contact xwapp--server :service)
                             :pages (sort pages (lambda (a b) (string< (car a) (car b)))))
                       (current-buffer))))
            (setq xwapp--state-ours t))
           (xwapp--state-ours
            (when (file-exists-p xwapp-state-file)
              (delete-file xwapp-state-file))
            (setq xwapp--state-ours nil)))
        (error (message "xwapp: %s" (error-message-string err)))))))

(defun xwapp--restore (app)
  "Give APP the address its page had in a browser tab as Emacs last
exited, and listen there, so that tab can come back.  Not when another
Emacs has the port: the tab is that one's."
  (when-let* ((state (xwapp--state))
              (page (assoc (xwapp-buffer-name app) (plist-get state :pages)))
              (port (with-demoted-errors "xwapp: %S" (xwapp--port))))
    (when (and (eql port (plist-get state :port))
               (not (xwapp--get app :secret)))
      (setq xwapp--state-ours t)
      (xwapp--put app :secret (nth 1 page))
      (xwapp--put app :restored t)
      (xwapp--timer app :forget xwapp--come-back-within #'xwapp--forget))))

(defun xwapp--forget (app)
  "APP's tab has not come back since the restart: its address is not kept."
  (xwapp--put app :forget nil)
  (xwapp--put app :restored nil)
  (xwapp--save))

;;;###autoload
(defun xwapp-resume ()
  "Load the apps whose pages were in browser tabs as Emacs last exited.
Each listens again where its tab asks, and the tab comes back.  With
`xwapp-remote-hosts', listen in any case: your other devices come
through a proxy, which cannot ask Emacs to begin.  Run once Emacs is
up, when there is a `xwapp-state-file' or remote hosts."
  (interactive)
  (dolist (page (plist-get (xwapp--state) :pages))
    (let ((reopen (nth 2 page)))
      (when (and reopen (autoloadp (symbol-function reopen)))
        (with-demoted-errors "xwapp: %S"
          (autoload-do-load (symbol-function reopen) reopen)))))
  (when xwapp-remote-hosts
    (with-demoted-errors "xwapp: %S" (xwapp--port))))

;;;###autoload
(add-hook 'emacs-startup-hook
          (lambda () (when (or (file-exists-p xwapp-state-file) (bound-and-true-p xwapp-remote-hosts))
                       (xwapp-resume))))

(defun xwapp-browse (app)
  "Show APP's page in the web browser, on a fresh load.
Interactively, the page in this buffer, which moves there: the tab
takes it once loaded, and this buffer is killed."
  (interactive (list (or xwapp--app (user-error "No xwapp page in this buffer"))))
  (unless (file-readable-p (xwapp-index app))
    (user-error "Missing UI at %s" (xwapp-index app)))
  ;; A tab closed a moment ago is not to close the app on the new one,
  ;; which has a while to load; one that never does closes it.
  (xwapp--timer app :gone (* 3 xwapp--gone-after) #'xwapp--gone)
  (xwapp--put app :asked t)
  (funcall xwapp-browse-function (xwapp-url app)))

;; Serving

(defun xwapp--filter (proc bytes transport)
  "Gather the request on PROC from BYTES as they come, then answer it.
TRANSPORT is where it arrived -- `local' on loopback, `proxy' on the
socket -- which, not any header, says whether it came through the proxy."
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
              (xwapp--answer proc request transport)
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

(defun xwapp--from-here-p (headers transport)
  "Whether REQUEST's HEADERS and TRANSPORT make it one to answer.

`here' for the browser on this machine: a request on loopback (TRANSPORT
`local') naming this server as its host, and as its origin when there is
one -- a name rebound to 127.0.0.1 gets nothing, nor does a page of
another site that has the secret.

`elsewhere' for your other devices: a request through the proxy
(TRANSPORT `proxy', the one transport a header cannot forge), for one of
`xwapp-remote-users'.  `tailscale serve' rewrites the Host to
\"localhost\" on the way to the socket, so the Host is no proof and no
filter here -- the transport is.  The login serve stamps is believed
only because it arrived this way; the same header on loopback is some
program's own word, and gets nothing.  The page's own calls carry an
Origin, which must name one of `xwapp-remote-hosts' (the page's own, as
serve leaves Origin alone): a cross-site page on the device is turned
away."
  (let* ((port (process-contact xwapp--server :service))
         (here (list (format "127.0.0.1:%d" port) (format "localhost:%d" port)))
         (host (downcase (or (cdr (assoc "host" headers)) "")))
         (origin (cdr (assoc "origin" headers))))
    (cond
     ((and (eq transport 'local) (member host here))
      (and (or (null origin)
               (member origin (mapcar (lambda (h) (concat "http://" h)) here)))
           'here))
     ((and (eq transport 'proxy) xwapp-remote-hosts
           (member (cdr (assoc "tailscale-user-login" headers)) xwapp-remote-users))
      (and (or (null origin)
               (let ((o (downcase origin)))
                 (seq-some (lambda (h)
                             (or (equal o (concat "https://" (downcase h)))
                                 (equal o (concat "http://" (downcase h)))))
                           xwapp-remote-hosts)))
           'elsewhere)))))

(defun xwapp--open (proc parts headers transport)
  "Send PROC on to the page PARTS name, (PKG), under its secret: a
request from your other devices for `xwapp-remote-open-url'.  Not from
this machine, where any page in the browser could ask."
  (let ((app (and (eq (xwapp--from-here-p headers transport) 'elsewhere)
                  (= (length parts) 1)
                  (catch 'found
                    (maphash (lambda (app _)
                               (when (and (xwapp-reopen app)
                                          (string-prefix-p (concat (car parts) "/") (xwapp--page app)))
                                 (throw 'found app)))
                             xwapp--served)
                    nil))))
    (if (not app)
        (xwapp--respond proc 404)
      (xwapp--respond proc 302 nil nil `(("Location" . ,(xwapp--path app))))
      (xwapp--save))))

(defun xwapp--answer (proc request transport)
  "Answer REQUEST, which came on PROC over TRANSPORT (see `xwapp--filter')."
  (pcase-let* ((`(,method ,target ,headers ,body) request)
               (`(,path ,query) (split-string (or target "") "?"))
               (`(,secret . ,rest) (split-string path "/" t))
               (app (and secret (xwapp--app-of secret)))
               (args (url-parse-query-string (or query "")))
               (page (cadr (assoc "page" args))))
    (cond
     ((and (equal secret "open") (equal method "GET")) (xwapp--open proc rest headers transport))
     ((not (and app (xwapp--from-here-p headers transport))) (xwapp--respond proc 404))
     ((and (equal method "POST") (equal rest '("hello")) page)
      (xwapp--hello app proc page (equal (cadr (assoc "soft" args)) "1")))
     ((and (equal method "GET") (equal rest '("next")))
      (xwapp--next app proc page))
     ((and (equal method "POST") (equal rest '("intent")))
      (xwapp--intent app proc page body))
     ((equal method "GET") (xwapp--file app proc rest))
     (t (xwapp--respond proc 404)))))

(defun xwapp--respond (proc code &optional type body headers)
  "Answer PROC with CODE, and BODY (bytes) of TYPE, then close it.
HEADERS, an alist, go along.  An error with nothing to say says its
name: answered empty, a browser takes it for a file to download."
  (when (and (>= code 400) (null type))
    (setq type "text/plain; charset=utf-8"
          body (alist-get code xwapp--reasons "")))
  (ignore-errors
    (process-send-string
     proc
     (concat (format "HTTP/1.1 %d %s\r\n" code (alist-get code xwapp--reasons ""))
             (if type (format "Content-Type: %s\r\n" type) "")
             (mapconcat (lambda (h) (format "%s: %s\r\n" (car h) (cdr h))) headers "")
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

(defun xwapp--why (app)
  "Why a page is not APP's: \"taken\", \"free\" or \"closed\".
See \"In a web browser\" above."
  (cond ((xwapp-live-p app) "taken")
        ((xwapp-reopen app) "free")
        (t "closed")))

(defun xwapp--refuse (proc why)
  "Answer PROC that its page is not the app's, and WHY."
  (xwapp--respond proc 410 "application/json" (format "{\"why\":\"%s\"}" why)))

(defun xwapp--hello (app proc page soft)
  "PAGE has loaded in a browser tab: it is APP's page now.
A SOFT hello is a page coming back on its own: it takes APP only when
APP is free."
  (let ((why (xwapp--why app))
        (asked (xwapp--get app :asked)))
    (if (and soft (not (equal why "free")))
        (xwapp--refuse proc why)
      (xwapp--put app :asked nil)
      (xwapp--let-go app)
      (xwapp--put app :page page)
      (xwapp--greet app)
      (xwapp--timer app :gone xwapp--gone-after #'xwapp--gone)
      ;; Moved, not closed: with :page set, the buffer's death runs no on-kill.
      (when-let* ((buf (xwapp-buffer app)))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buf)))
      (xwapp--respond proc 204)
      (xwapp--save)
      ;; Open nowhere until now, and not by Emacs: made ready as its
      ;; command would have.
      (when (and (equal why "free") (not asked))
        (run-at-time 0 nil (xwapp-reopen app))))))

(defun xwapp--next (app proc page)
  "Hold PROC, PAGE's request for calls, until there are some."
  (if (not (xwapp--current-p app page))
      (xwapp--refuse proc (xwapp--why app))
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
  "Send JSON, one call, to APP's browser page.
Scrubbed and size-capped like the xwidget path: an oversized or unclean
payload is dropped rather than queued."
  (when-let* ((safe (xwapp--script-safe json)))
    (xwapp--put app :outbox (cons safe (xwapp--get app :outbox)))
    ;; On the next turn, so the calls of one command go out together.
    (when (and (xwapp--get app :waiting) (not (xwapp--get app :flush)))
      (xwapp--timer app :flush 0 #'xwapp--flush))))

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
     ((not (xwapp--current-p app page)) (xwapp--refuse proc (xwapp--why app)))
     ((not (consp intent)) (xwapp--respond proc 400))
     (t
      (xwapp--respond proc 204)
      (if (equal (alist-get 'op intent) "xwapp-seen")
          (xwapp--put app :seen (eq (alist-get 'seen intent) t))
        ;; As the title's intents are: from a timer, not this filter.
        (run-at-time 0 nil #'xwapp--handle app intent))))))

(defun xwapp--handle (app intent)
  "Hand APP INTENT, from its page in a browser tab: a page it opens
opens there too."
  (let ((xwapp--from-browser t))
    (xwapp--dispatch app intent)))

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
  "Let APP's browser page go: told so if it is waiting, forgotten anyway.
Only another page, in a tab or in Emacs, takes it from one that waits."
  (dolist (key '(:hold :flush :gone :forget))
    (xwapp--untimer app key))
  (let ((proc (xwapp--get app :waiting)))
    ;; Before answering: the sentinel must not take the hang-up for a
    ;; page that went.
    (dolist (key '(:waiting :page :outbox :seen :restored))
      (xwapp--put app key nil))
    (when proc
      (xwapp--refuse proc "taken"))))

(defun xwapp--gone (app)
  "APP's browser page has stopped asking: it was closed.  What Emacs
made ready for a tab it opened is undone with it."
  (xwapp--let-go app)
  (xwapp--put app :asked nil)
  (xwapp--save)
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

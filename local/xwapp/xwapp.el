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

(defconst xwapp--surrogate-re (format "[%c-%c]" #xD800 #xDFFF)
  "A lone surrogate.")
(defconst xwapp--past-unicode-re (format "[^%c-%c]" 0 #x10FFFF)
  "A char past Unicode: a raw byte, or one of Emacs's own beyond it.")

(defun xwapp--scrub (s)
  "S with any char that is not a valid Unicode scalar made U+FFFD.
A raw eight-bit byte (> #x10FFFF in Emacs) or a lone surrogate encodes to
invalid UTF-8, which the WebKit IPC guard can abort on.  Returns S itself
when it is already clean, so the common path copies nothing.  Whether it
is, two regexps ask, in C: every page update is scrubbed, and a Lisp
test of each char of a herd's cost milliseconds an update -- over ten
when not compiled."
  (if (and (multibyte-string-p s)
           (let ((case-fold-search nil))
             (or (string-match-p xwapp--surrogate-re s)
                 (string-match-p xwapp--past-unicode-re s))))
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

(defun xwapp-js (app fn obj &optional later)
  "Call NAMESPACE[FN] with JSON-encoded OBJ in APP's page.
LATER non-nil: in a browser tab, the call can wait behind every other --
see \"Calls that can wait\"; a symbol or string there names it for
`xwapp-js-forget'.  In Emacs's own page every call runs at once."
  (let ((ns (xwapp-namespace app)))
    (if (xwapp--get app :page)
        (xwapp--call app (format "{\"ns\":%s,\"fn\":%s,\"arg\":%s}"
                                 (json-encode ns) (json-encode fn) (json-encode obj))
                     later)
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

(defun xwapp--read-intent (json)
  "The intent JSON holds, as an alist, or nil."
  (ignore-errors
    (json-parse-string json :object-type 'alist :array-type 'list)))

(defun xwapp-parse-intent (title prefixes)
  "The intent TITLE carries after one of PREFIXES, as an alist, or nil."
  (when (stringp title)
    (seq-some (lambda (prefix)
                (and (string-prefix-p prefix title)
                     (xwapp--read-intent (substring title (length prefix)))))
              prefixes)))

(defun xwapp--prefixed-p (title prefixes)
  "Non-nil when TITLE starts with one of PREFIXES: an intent, read or not."
  (and (stringp title)
       (seq-some (lambda (prefix) (string-prefix-p prefix title)) prefixes)))

;; A long intent comes in parts.
;;
;; WebKit hands Emacs no more than the first 1000 UTF-16 units of a
;; page's title (measured 2026-10-30 on macOS 15: WKWebView's own title
;; is cut there, while the page's document.title keeps all of it), and
;; an emoji cut in half leaves Emacs an empty title.  A prompt of a few
;; hundred words is past it.  So the kit sends a title that would be
;; longer as parts, each a title of its own,
;;
;;   PREFIX{"xwpart":N,"i":K,"n":COUNT,"s":"..."}
;;
;; the S of parts 0 to COUNT-1, strung together, being the intent's
;; JSON, and N the intent's own n.  Emacs keeps the parts by N, in
;; whatever order they come, and acts on the intent once it has them
;; all.  One that stops coming for `xwapp--part-wait' seconds is
;; dropped, and said to be.  While parts are coming the title is read
;; every `xwapp--part-poll' seconds, not every poll interval: a part
;; goes only once the last is read, and 20,000 CJK characters are 30.

(defconst xwapp--part-wait 10
  "Seconds without a part after which an intent sent in parts is dropped.
The page waits as long (WAIT in ui/xwapp.js) for a title to be read
before it gives it up for another intent.")

(defconst xwapp--part-poll 0.02
  "Seconds between title reads while an intent's parts are coming in.")

(defconst xwapp--part-max 100000
  "Most parts an intent comes in.  The page's longest, 2M characters of
plain text, is some 3,000; escapes can make one several times that.")

(defun xwapp--say-dropped (app what)
  "Say in the echo area that APP's page sent WHAT, which was dropped.
Never what it said: that may be anything you wrote."
  (message "xwapp: %s's page sent %s; it was dropped" (xwapp-buffer-name app) what))

(defun xwapp--part (app intent)
  "INTENT, as APP's page meant it: itself, or when it is a part of a
longer one, that one once its every part is in -- nil until then."
  (if (not (assq 'xwpart intent))
      intent
    (let-alist intent
      (let* ((parts (xwapp--get app :parts))
             (entry (assoc .xwpart parts)))
        (cond
         ((not (and (natnump .n) (<= 1 .n xwapp--part-max) (natnump .i) (< .i .n)
                    (stringp .s) (or (null entry) (= .n (plist-get (cdr entry) :count)))))
          (when entry
            (xwapp--put app :parts (delq entry parts)))
          (xwapp--say-dropped app "a long message in parts that do not fit together")
          nil)
         (t
          (unless entry
            (setq entry (list .xwpart :count .n :got 0 :pieces (make-vector .n nil)))
            (xwapp--put app :parts (cons entry parts)))
          (let ((pieces (plist-get (cdr entry) :pieces)))
            (unless (aref pieces .i)
              (aset pieces .i .s)
              (plist-put (cdr entry) :got (1+ (plist-get (cdr entry) :got))))
            (if (< (plist-get (cdr entry) :got) .n)
                (progn (xwapp--timer app :part-wait xwapp--part-wait #'xwapp--parts-expire)
                       nil)
              (xwapp--put app :parts (delq entry (xwapp--get app :parts)))
              (unless (xwapp--get app :parts)
                (xwapp--untimer app :part-wait))
              (or (xwapp--read-intent (mapconcat #'identity pieces ""))
                  (progn (xwapp--say-dropped app "a long message Emacs could not read")
                         nil))))))))))

(defun xwapp--parts-expire (app)
  "Drop the intents APP's page has stopped sending the parts of, saying so."
  (dolist (entry (xwapp--get app :parts))
    (let ((plist (cdr entry)))
      (xwapp--say-dropped
       app (format "a long message in %d parts, %d of which never came"
                   (plist-get plist :count)
                   (- (plist-get plist :count) (plist-get plist :got))))))
  (xwapp--parts-clear app))

(defun xwapp--parts-clear (app)
  "Forget the parts APP's page has sent so far: a new page sends its own."
  (xwapp--put app :parts nil)
  (xwapp--untimer app :part-wait)
  (xwapp--untimer app :part-poll))

;; A title read is answered.
;;
;; Emacs used to clear a title it had read by setting the page's own
;; back, but a script run in the page lands later than it is sent, and
;; Emacs may have read the title late, from WebKit's copy of it: by
;; then the page may have given that title up (see WAIT in
;; ui/xwapp.js) and set the next, which the clear wiped out unread.
;; So Emacs now says which title it read, by its intent N and part I
;; -- the title's own words may differ in WebKit's copy -- and the kit
;; clears the title only while it is still that one:
;;
;;   if (!(window.XW && XW.kit && XW.kit({"xwapp":"read","n":N,"i":I,
;;          "whole":true,"idle":IDLE}))) document.title = IDLE;
;;
;; "unread" in place of "read" when Emacs could make no intent of it,
;; or dropped the intent it was a part of: the kit gives that intent
;; up.  WHOLE when Emacs has the whole intent: one the page had given
;; up, Emacs read after all, and the page takes its loss back.  A page
;; with no kit, or a kit from before, answers nothing, and the title
;; is cleared as it always was.

(defun xwapp--title-id (title prefixes intent)
  "Which of the page kit's titles TITLE is, as (N . I): part I of the
intent numbered N, an intent in a title of its own being its part 0.
From INTENT, what TITLE was read as, or, when it could not be read,
from TITLE's start, which WebKit keeps.  Nil for a title the kit did
not set."
  (if intent
      (let* ((part (assq 'xwpart intent))
             (n (if part (cdr part) (alist-get 'n intent)))
             (i (if part (alist-get 'i intent) 0)))
        (and (integerp n) (integerp i) (cons n i)))
    (when-let* ((prefix (seq-find (lambda (p) (string-prefix-p p title)) prefixes))
                (json (substring title (length prefix))))
      (cond
       ((string-match "\\`{\"xwpart\":\\([0-9]+\\),\"i\":\\([0-9]+\\)," json)
        (cons (string-to-number (match-string 1 json))
              (string-to-number (match-string 2 json))))
       ((string-match "\\`{\"op\":\"\\(?:[^\"\\\\]\\|\\\\.\\)*\",\"n\":\\([0-9]+\\)[,}]" json)
        (cons (string-to-number (match-string 1 json)) 0))))))

(defun xwapp--title-answer (app what id whole)
  "The script that answers APP's page for a title Emacs has read: WHAT
is \"read\", or \"unread\" when the intent it carries is lost; ID is
the title's (N . I), if it has one; WHOLE, whether Emacs has the whole
intent now.  The kit clears the title only while it is that one."
  (let ((idle (xwapp-idle-title app)))
    (format "if (!(window.XW && XW.kit && XW.kit(%s))) document.title = %s;"
            (json-encode `((xwapp . ,what)
                           ,@(when id `((n . ,(car id)) (i . ,(cdr id))))
                           ,@(when whole '((whole . t)))
                           (idle . ,idle)))
            (json-encode idle))))

(defun xwapp--poll (app)
  "Read one intent from APP's page title and dispatch it.
A title that cannot be read -- a crashed WebKit process still looks live
-- counts toward the breaker; a run of them stops the poll rather than
spin Emacs reading and scripting a broken IPC channel every interval.
A title read is answered, so the page sends the next: see
`xwapp--title-answer'.  An intent that cannot be parsed is said to be,
once, and the page told it is lost, so it goes on to the next and
gives back what was lost; so is one sent in parts that Emacs dropped."
  (when-let* ((xw (xwapp-session app)))
    (let ((title (condition-case nil
                     (prog1 (xwidget-webkit-title xw)
                       (xwapp--put app :poll-fails 0))
                   (error (xwapp--poll-failed app) :failed))))
      (when (and (not (eq title :failed))
                 (xwapp--prefixed-p title (xwapp-prefixes app))
                 (not (equal title (xwapp-last-title app))))
        (setf (xwapp-last-title app) title)
        (let* ((intent (xwapp-parse-intent title (xwapp-prefixes app)))
               (id (xwapp--title-id title (xwapp-prefixes app) intent))
               (whole (and intent (xwapp--part app intent)))
               ;; A part whose intent is in neither WHOLE nor the parts
               ;; kept was dropped: the rest of it would come for nothing.
               (lost (or (not intent)
                         (and (not whole)
                              (not (assoc (alist-get 'xwpart intent)
                                          (xwapp--get app :parts)))))))
          (xwapp--exec app xw (xwapp--title-answer app (if lost "unread" "read") id whole))
          (unless intent
            (xwapp--say-dropped
             app (format "a %d-character message Emacs could not read" (length title))))
          (when whole
            (xwapp--dispatch app whole))))
      (if (and (xwapp--get app :parts) (not (xwapp--get app :wedged)))
          (xwapp--timer app :part-poll xwapp--part-poll #'xwapp--poll)
        (xwapp--untimer app :part-poll)))))

(defun xwapp-start-poll (app)
  "Start polling APP's page title for intents."
  (xwapp-stop-poll app)
  (xwapp--put app :poll-fails 0)
  (xwapp--put app :wedged nil)
  (setf (xwapp-timer app)
        (run-at-time 0.4 (xwapp-poll-interval app) #'xwapp--poll app)))

(defun xwapp-stop-poll (app)
  "Stop polling APP's page title."
  (xwapp--untimer app :part-poll)
  (when (timerp (xwapp-timer app))
    (cancel-timer (xwapp-timer app))
    (setf (xwapp-timer app) nil)))

(defun xwapp--on-kill (app)
  (xwapp-stop-poll app)
  (xwapp--parts-clear app)
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
      (xwapp--parts-clear app)
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
;;   GET .files/N  a file the app serves by name (`xwapp-serve-files');
;;                 .SET/N one of another set it serves.
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
;;            says so, with a button that takes the app back (a
;;            hello, not soft), as reloading it does.
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
  "Bytes past which a request is refused.
Above the page's longest intent (INTENT_MAX in ui/xwapp.js): 2M UTF-16
units of JSON, 6 MB at most as UTF-8.")

(defconst xwapp--reasons
  '((200 . "OK") (204 . "No Content") (302 . "Found") (304 . "Not Modified") (400 . "Bad Request")
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
:sending  that page's `next' while its answer is still going out, from
          a thread (`xwapp--flush'), or nil
:outbox   calls not yet sent to it, newest first
:later    calls that can wait, (NAME . CALL) each, oldest first: see
          \"Calls that can wait\"
:when-sent  functions to call once the calls in :outbox now are out
:peer     the tailnet address its page last came from, through the proxy
:seen    non-nil when it last said you could be looking at it
:asked    non-nil from `xwapp-browse' until a tab says hello: that tab
          is the one Emacs opened, and the app is ready for it
:restored non-nil from a restart until its tab comes back, or does not
:sets     the files it serves besides its page: (SET . HOW) each, as
          `xwapp-serve-files' was told
:hold :flush :gone :forget :unhold   its timers.
And of its page in Emacs:
:parts    the long intents it is sending in parts, (N :count C :got G
          :pieces V) each: see `xwapp--part'
:part-wait :part-poll   their timers.")

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
  "Send CALL, an alist, to the page kit of APP's page.
Scrubbed and size-capped as any script is (`xwapp--exec'): a call may
carry text from elsewhere -- the clipboard's."
  (let ((json (json-encode call)))
    (if (xwapp--get app :page)
        (xwapp--call app json)
      (when-let* ((xw (xwapp-session app)))
        (xwapp--exec app xw (format "window.XW && XW.kit(%s);" json))))))

(defun xwapp--greet (app)
  "Tell APP's page, which has just loaded, that your other devices can
reach it -- when they can.  Its button is hidden until then.  And, in
the xwidget, where the files it is served are (`xwapp-serve-files'):
a page of file: reaches them as files."
  (when (and xwapp-remote-hosts (xwapp-reopen app))
    (xwapp--kit app '((xwapp . "phone"))))
  (xwapp--tell-files app))

(defun xwapp--show-qr (app)
  "Show APP's address from your other devices in its page, as a QR code."
  (xwapp--kit app (condition-case err
                      `((xwapp . "qr") (url . ,(xwapp-remote-url app))
                        (host . ,(car xwapp-remote-hosts)))
                    (user-error `((xwapp . "qr") (error . ,(error-message-string err)))))))

;; ⌘V in the xwidget.
;;
;; In WebKit under Emacs, ⌘V never becomes a paste: the paste is the
;; menu's to send, and Emacs's menu does not send it to the web view, so
;; the page sees the keydown and nothing after -- not for text either.
;; So the kit, on that key in a field of the page's, asks Emacs for the
;; clipboard (the intent xwapp-paste), and puts the text in at the caret
;; as typing would; with none -- an image on the clipboard, say -- the
;; page is told, and may take what else there is.  In a browser ⌘V is a
;; paste of its own, from that device's clipboard: Emacs's is not asked.

(defun xwapp--clipboard-text ()
  "The text on the clipboard, or \"\" when it holds none.  A char no
Unicode text has is U+FFFD, as in any script (`xwapp--scrub'): JSON
would write a raw byte out as its octal escape, text of its own."
  (let ((text (ignore-errors (gui-get-selection 'CLIPBOARD 'STRING))))
    (if (stringp text) (xwapp--scrub (substring-no-properties text)) "")))

(defun xwapp--paste (app)
  "Answer APP's page, which asks on ⌘V in a field of its own, with the
clipboard's text -- \"\" when it holds none.  Only to the page in the
xwidget: the kit asks there alone, on the key you pressed."
  (unless (xwapp--get app :page)
    (xwapp--kit app `((xwapp . "paste") (text . ,(xwapp--clipboard-text))))))

(defun xwapp--dispatch (app intent)
  "Hand INTENT, from APP's page, to the kit when it is the kit's, else to APP."
  (pcase (alist-get 'op intent)
    ("xwapp-hello" (xwapp--greet app))
    ("xwapp-qr" (xwapp--show-qr app))
    ("xwapp-paste" (xwapp--paste app))
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

;; What each request cost.
;;
;; A phone's page is "sometimes" slow, and a slow moment is gone by the
;; time anyone looks.  So its timings are kept, to be read after: the
;; page times its side (ui/xwapp.js, "How long it took") and, once a
;; load or a wake is over, sends it (the kit's intent xwapp-timing); and
;; Emacs times its side of every request as it serves it, here, for the
;; two to be joined (\"How long the page took\", below):
;;
;;   t0 t1   its first bytes read, and the whole of it
;;   tf      a `next's: when Emacs had calls for it -- held until then
;;   t2 t3   its answer begun, and its last byte in the socket
;;   th tw   an intent's: its handler begun, and done
;;
;; A few `float-time's a request, nothing between them: a page left
;; idle costs its held `next', timed when answered, every 25 s.  The
;; latest `xwapp--reqs-max' are kept, a ring.  A path is kept without
;; its first segment -- the secret's -- and with any run of 32 hex
;; digits, which a secret is, made "…".

(defcustom xwapp-timing t
  "Non-nil: time what a page in a browser asks of Emacs, and keep the
page's own record of each load and wake, to read after: `xwapp-timings'.
See \"How long the page took\"."
  :type 'boolean)

(defcustom xwapp-timing-slow 1.5
  "Seconds from which a page's load, or its wake, is slow: its record
kept among the slow ones, the device's path on the tailnet looked up,
and `xwapp-timing-functions' told so.  An intent answered this late is
recorded too."
  :type 'number)

(defcustom xwapp-timing-file (locate-user-emacs-file "xwapp/timings.eld")
  "Where the pages' timing records are kept over restarts, or nil for in
memory only.  Timings alone: no page's secret, nothing a page showed."
  :type '(choice (const :tag "Memory only" nil) file))

(defcustom xwapp-tailscale-program
  (or (executable-find "tailscale") "/Applications/Tailscale.app/Contents/MacOS/Tailscale")
  "The tailscale command, asked how a slow page's device was reached --
straight or relayed (`tailscale status --json'), and run to find it a
direct path (`tailscale ping', \"Kicking the path\"), or nil not to."
  :type '(choice (const :tag "Do not ask" nil) file))

(defcustom xwapp-tailscale-hold nil
  "Seconds at most the calls that can wait wait for a device's path to
turn direct while Emacs pings it (\"Kicking the path\"), or nil for them
not to wait -- as by default.  Measured 2026-11-05: on cellular, direct
was no faster than the relay (9 KB/s against 10-20); on the home
network direct is the LAN, but what is under way when the path turns
direct goes on over it -- what was sent over the relay meanwhile is
ground gained, not lost.  Set it should the timings show a transfer
that stays slow after its path turned direct.  What you wait for never
waits."
  :type '(choice (const :tag "Do not wait" nil) number))

(defconst xwapp--kick-gone 30
  "Seconds a device goes unheard before its next request is taken for
an open or a wake: its page, at work, asks at least every
`xwapp--hold' seconds.")

(defvar xwapp--kick-every 30
  "Seconds at least between two kicks of a device's path.")

(defvar xwapp--kick-for 15
  "Seconds a kick may take: its `tailscale ping' is stopped then.")

(defvar xwapp--peers (make-hash-table :test 'equal)
  "A device's tailnet address -> what Emacs knows of its path, a plist:
:seen when a request came from it last, through the proxy; :kick its
latest kick, a plist -- :at when begun, :first (PATH . SECONDS) its
first pong, :direct seconds until a pong came direct, :end seconds until
over, :how `direct', `relay' or `none' (no pong) -- and :kicks the five
latest, the latest first; :proc its `tailscale ping' while under way;
:paths its path as seen, ((TIME . PATH) ...) the latest first, PATH
\"direct\", \"relay hkg\", \"peer-relay\".")

;; A request timed is a plist, put to in place (`plist-put' on one that
;; has :via from the first): :app its app, :via its transport, :method,
;; :path, :n and :page the kit's number and page, :wake, :op an intent's,
;; :fwd the address the proxy says it came from, :gc `gc-elapsed' then,
;; the times above, :code :bytes :gzip :aside its answer's.  Not a
;; struct: bin/reload (ghostherd's) defines functions again, not structs,
;; and a struct's setters missing would fail every answer.

(defvar xwapp--reqs-max 256
  "Requests timed and kept: the latest.")

(defvar xwapp--reqs (make-vector xwapp--reqs-max nil)
  "The latest requests served, timed (\"What each request cost\"), a ring.")

(defvar xwapp--reqs-at 0
  "How many requests `xwapp--reqs' has been given.")

(defun xwapp--reqs-note (req)
  "Keep REQ in `xwapp--reqs', over the oldest."
  (unless (= (length xwapp--reqs) xwapp--reqs-max)
    (setq xwapp--reqs (make-vector xwapp--reqs-max nil) xwapp--reqs-at 0))
  (aset xwapp--reqs (% xwapp--reqs-at xwapp--reqs-max) req)
  (setq xwapp--reqs-at (1+ xwapp--reqs-at)))

(defun xwapp--reqs-list ()
  "The requests kept, the latest first."
  (let ((len (length xwapp--reqs)) out)
    (dotimes (i (min xwapp--reqs-at len))
      (push (aref xwapp--reqs (% (- xwapp--reqs-at 1 i) len)) out))
    (nreverse out)))

(defun xwapp--unhex32 (s)
  "S with every run of 32 hex digits or more -- a secret is one -- made
\"…\"; at most 120 characters.  As requests are kept."
  (let ((case-fold-search t))
    (truncate-string-to-width
     (replace-regexp-in-string "[0-9a-f]\\{32,\\}" "…" s t t) 120)))

(defun xwapp--req-begin (proc request transport)
  "Time REQUEST, which PROC has whole now, over TRANSPORT: a plist on
PROC, and in the ring (\"What each request cost\")."
  (pcase-let* ((`(,method ,target ,headers ,_body) request)
               (`(,path ,query) (split-string (or target "") "?"))
               (parts (split-string path "/" t))
               (args (ignore-errors (url-parse-query-string (or query ""))))
               (arg (lambda (name re)
                      (let ((v (cadr (assoc name args))))
                        (and (stringp v) (let ((case-fold-search nil)) (string-match-p re v)) v))))
               (now (float-time))
               (req (list
                     :via transport :method method
                     ;; The secret's segment dropped; /open/ has none.
                     :path (xwapp--unhex32
                            (string-join (if (equal (car parts) "open") parts (cdr parts)) "/"))
                     :n (when-let* ((n (funcall arg "n" "\\`[0-9]\\{1,9\\}\\'"))) (string-to-number n))
                     :page (funcall arg "page" "\\`[0-9a-f]\\{16\\}\\'")
                     :wake (equal (cadr (assoc "wake" args)) "1")
                     :fwd (cdr (assoc "x-forwarded-for" headers))
                     :gc gc-elapsed
                     :t0 (or (process-get proc 'xwapp-t0) now) :t1 now)))
    (process-put proc 'xwapp-req req)
    (xwapp--reqs-note req)))

;; Serving

(defun xwapp--filter (proc bytes transport)
  "Gather the request on PROC from BYTES as they come, then answer it.
TRANSPORT is where it arrived -- `local' on loopback, `proxy' on the
socket -- which, not any header, says whether it came through the proxy."
  (unless (process-get proc 'xwapp-taken)
    (set-process-query-on-exit-flag proc nil)
    (unless (process-get proc 'xwapp-t0)
      (process-put proc 'xwapp-t0 (float-time)))
    (let ((in (concat (process-get proc 'xwapp-in) bytes)))
      (process-put proc 'xwapp-in in)
      (if (> (length in) xwapp--max-request)
          (xwapp--respond proc 413)
        (when-let* ((request (xwapp--request in)))
          (process-put proc 'xwapp-taken t)
          (process-put proc 'xwapp-in nil)
          (when xwapp-timing
            (xwapp--req-begin proc request transport))
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
    (when-let* ((req (process-get proc 'xwapp-req)))
      (plist-put req :app app))
    (cond
     ((and (equal secret "open") (equal method "GET")) (xwapp--open proc rest headers transport))
     ((not (and app (xwapp--from-here-p headers transport))) (xwapp--respond proc 404))
     ;; Answered below; its device seen first -- its path kicked, when it
     ;; opens or wakes the page (\"Kicking the path\").
     ((progn (when (eq transport 'proxy)
               (xwapp--peer-came app (cdr (assoc "x-forwarded-for" headers))
                                 (or (equal rest '("hello"))
                                     (and (equal rest '("next")) (equal (cadr (assoc "wake" args)) "1")))))
             nil))
     ((and (equal method "POST") (equal rest '("hello")) page)
      (xwapp--hello app proc page (equal (cadr (assoc "soft" args)) "1")))
     ((and (equal method "GET") (equal rest '("next")))
      (xwapp--next app proc page (and (eq transport 'proxy) (xwapp--takes-gzip-p headers))
                   (equal (cadr (assoc "wake" args)) "1")))
     ((and (equal method "POST") (equal rest '("intent")))
      (xwapp--intent app proc page body))
     ((and (equal method "GET") (string-prefix-p "." (or (car rest) "")))
      (xwapp--served-file app proc (cdr rest) (substring (car rest) 1)))
     ((equal method "GET") (xwapp--file app proc rest headers))
     (t (xwapp--respond proc 404)))))

;; A big answer goes from a thread.
;;
;; `process-send-string' returns only once the last byte is in the
;; socket, and a phone on a slow link reads slowly: an 8 MB image to a
;; reader of 2 MB/s held Emacs 5.4 seconds, a key pressed meanwhile
;; waiting it out.  Sent from a thread, which waits on the socket with
;; Emacs free, the key was taken in 4 ms.
;;
;; Nor need the reader be slow.  The proxy reaches Emacs over a unix
;; socket, which holds 8 KB on macOS (net.local.stream.sendspace), and
;; Emacs, finding it full, sleeps 20 ms before it tries again
;; (send_process, src/process.c) -- a key unread all the while.  So Emacs
;; sends the proxy some 340 KB a second at most, whoever reads: the
;; herd's app.js, 90 KB gzip'd, held it 0.26 s; its Inbox, 44 KB, 0.13
;; s.  Hence `xwapp--send-aside' is what that socket takes at once: an
;; answer past it -- a file of the page's, an app's, a `next' -- goes from
;; a thread of its own, and one within it is in the socket before
;; `process-send-string' returns.  The thread does not send faster; it
;; lets Emacs do the rest meanwhile.  Safe for the long-poll:
;;
;;   - In order.  Every request is a connection of its own, answered
;;     once; the page asks its next `next' only once it has read the
;;     last whole, so two answers to it are never sent at once.
;;   - Not taken for a page gone.  The page cannot ask again while it
;;     is still reading, so the ten seconds it has to (`xwapp--gone')
;;     start once the answer is out, not when it starts going.
;;   - Not forever.  A reader that stops reading for good -- the phone
;;     slept mid-answer, and the proxy holds its side open -- would keep
;;     the thread waiting until TCP gives up, minutes on, the page
;;     counted as there.  After `xwapp--send-patience' its connection is
;;     closed: the thread's send fails, and the page is gone unless it
;;     asks again.
;;   - A connection closed meanwhile, from either end, fails the send in
;;     the thread, which is caught: the thread ends, and the page has its
;;     ten seconds to ask again as after any answer.
;;
;; And safe for Emacs.  Its main thread waits in select() on the sockets
;; of the processes it owns -- every connection here -- and a socket
;; closed under it by another thread makes select fail, which Emacs
;; takes for a bug and aborts on (wait_reading_process_output: EBADF).
;; The thread used to close the connection once it had sent: Emacs died
;; at once in a test (2026-10-31).  So the thread never closes one: it
;; shuts its sending side (`process-send-eof'), the reader hangs up, and
;; the main thread, told so (`xwapp--sentinel'), deletes the process.
;; And while it waits on the socket the thread runs no timers, which it
;; otherwise would -- any of Emacs's, measured 27 of 40 -- and one that
;; closed a process would do the same: it binds the timer lists to nil,
;; its own binding alone.  The connection left open past
;; `xwapp--send-patience' is closed from the main thread.

(defconst xwapp--send-aside (* 8 1024)
  "Bytes past which an answer is sent from a thread of its own: what the
proxy's socket takes at once.  See \"A big answer goes from a thread\".")

(defvar xwapp--send-patience 60
  "Seconds an answer sent from a thread may take before its connection
is closed: the reader has stopped reading.")

(defun xwapp--respond (proc code &optional type body headers done)
  "Answer PROC with CODE, and BODY (bytes) of TYPE, then close it.
HEADERS, an alist, go along; a Cache-Control among them replaces
no-store.  An error with nothing to say says its name: answered empty,
a browser takes it for a file to download.  A 304 has no body, nor
says one's length.  A response past `xwapp--send-aside' goes from a
thread of its own.  DONE, a function, is called once the response is
out, or has failed to go -- in the main thread, from a timer, for one
sent from a thread."
  (when (and (>= code 400) (null type))
    (setq type "text/plain; charset=utf-8"
          body (alist-get code xwapp--reasons "")))
  (let* ((response
         (concat (format "HTTP/1.1 %d %s\r\n" code (alist-get code xwapp--reasons ""))
                 (if type (format "Content-Type: %s\r\n" type) "")
                 (mapconcat (lambda (h) (format "%s: %s\r\n" (car h) (cdr h))) headers "")
                 (if (= code 304) "" (format "Content-Length: %d\r\n" (length body)))
                 (if (assoc "Cache-Control" headers) "" "Cache-Control: no-store\r\n")
                 "Referrer-Policy: no-referrer\r\n"
                 "X-Content-Type-Options: nosniff\r\n"
                 "Connection: close\r\n\r\n"
                 (if (= code 304) "" body)))
        (req (process-get proc 'xwapp-req))
        (aside (and (> (length response) xwapp--send-aside) (fboundp 'make-thread))))
    (when req
      (plist-put req :t2 (float-time))
      (plist-put req :code code)
      (plist-put req :bytes (length response))
      (plist-put req :gzip (and (assoc "Content-Encoding" headers) t))
      (plist-put req :aside aside))
    (if aside
        (progn
          (run-at-time xwapp--send-patience nil #'xwapp--close proc)
          (make-thread (lambda ()
                         (xwapp--send-from-thread proc response)
                         (when req (plist-put req :t3 (float-time)))
                         ;; In the main thread: a thread sees none of its
                         ;; bindings, nor touches what it waits on.
                         (when done (run-at-time 0 nil #'xwapp--done done)))
                       "xwapp-send"))
      (xwapp--send proc response)
      (when req (plist-put req :t3 (float-time)))
      (xwapp--done done))))

(defun xwapp--send (proc response)
  "Send PROC RESPONSE whole, then close it.  In the main thread."
  (ignore-errors (process-send-string proc response))
  (ignore-errors (delete-process proc)))

(defun xwapp--send-from-thread (proc response)
  "Send PROC RESPONSE whole, from a thread of its own, then shut the
sending side: the reader hangs up, and the main thread deletes PROC.
See \"A big answer goes from a thread\" for why it closes nothing, and
runs no timer while it waits."
  (let ((timer-list nil)
        (timer-idle-list nil))
    (ignore-errors (process-send-string proc response)))
  (ignore-errors (process-send-eof proc)))

(defun xwapp--close (proc)
  "Delete PROC, a connection, if it is still there: from the main thread
alone, which closing a socket from another may abort."
  (if (eq (current-thread) main-thread)
      (ignore-errors (delete-process proc))
    (run-at-time 0.1 nil #'xwapp--close proc)))

(defun xwapp--done (done)
  "Call DONE, if a function, saying what it signals rather than raising
it: what it signals is no fault of the answer's."
  (when done
    (condition-case err (funcall done)
      (error (message "xwapp: %s" (error-message-string err))))))

(defun xwapp--type (file)
  "The Content-Type to serve FILE as, by its extension."
  (pcase (downcase (or (file-name-extension file) ""))
    ("html" "text/html; charset=utf-8")
    ("js" "text/javascript; charset=utf-8")
    ("css" "text/css; charset=utf-8")
    ("svg" "image/svg+xml")
    ("png" "image/png")
    ((or "jpg" "jpeg") "image/jpeg")
    ("gif" "image/gif")
    ("webp" "image/webp")
    (_ "application/octet-stream")))

(defun xwapp--bytes (file)
  "FILE's contents, as bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

;; The page's own files are asked for again on every load -- a phone's
;; tab coming back after Emacs let it go reloads -- and the herd's are
;; some 475 KB, over the tailnet.  So each goes with a validator, its
;; ETag, and `no-cache': the browser keeps it, and asks each load
;; whether it is still the one, which a 304 answers with no body.  A
;; ui/ file edited has another ETag, and loads at once.  The ETag is
;; made of the file's path, size and time, as the file system has them
;; for the file itself: in straight's build the file is a symlink into
;; the repository, whose own time never changes.

(defun xwapp--stamp (file)
  "FILE's path, size and modification time, as one string, or nil:
those of the file it is a symlink to, if it is one."
  (let ((true (file-truename file)))
    (when-let* ((attrs (file-attributes true)))
      (format "%s\0%d\0%s" true (file-attribute-size attrs)
              (format-time-string "%s%N" (file-attribute-modification-time attrs))))))

(defun xwapp--etag (file)
  "FILE's validator, from its path, size and modification time, or nil."
  (when-let* ((stamp (xwapp--stamp file)))
    (format "\"%s\"" (substring (secure-hash 'sha1 stamp) 0 20))))

(defun xwapp--fresh-p (etag headers)
  "Non-nil when HEADERS' If-None-Match names ETAG, or any (*)."
  (when-let* ((etag)
              (match (cdr (assoc "if-none-match" headers))))
    (seq-some (lambda (tag)
                (let ((tag (string-trim tag)))
                  (or (equal tag "*")
                      (equal (string-remove-prefix "W/" tag) etag))))
              (split-string match ","))))

;; The page's own files go gzip'd.
;;
;; Text compresses some fourfold -- the herd's app.js from 314 KB to 88
;; KB -- and the phone's link is the slow part.  So a file of the page's
;; or the kit's, text of 1 KB or more, goes gzip'd to a browser that
;; takes it (Accept-Encoding), with Vary: Accept-Encoding.
;;
;; - Its own ETag.  A strong ETag says "these bytes", and a gzip'd body
;;   is other bytes than the file's: the two must not share one, or a
;;   cache could take one for the other.  So the gzip'd one is the
;;   file's with -gz in it, strong too -- exact, so a 304 can be: the
;;   browser asks with the ETag of what it has, and is told 304 when that
;;   is what it would be sent now.  (A weak ETag shared by both would do,
;;   but says less than is so.)
;; - Once per version of a file, not per request: the gzip'd bytes are
;;   kept by the file's ETag, `xwapp--gzip-keep' bytes of them at most,
;;   the least lately used going first.
;; - Off the main thread.  Emacs has zlib to inflate, not to deflate, so
;;   it is gzip, as a process of its own: the request waits for it,
;;   Emacs does not.  Measured on app.js (2026-10-31, an M-series Mac):
;;   `call-process' 8.4 ms, Emacs blocked throughout; `make-process' 12.8
;;   ms to the answer (at most 24.8 in 20), Emacs busy 1.4 ms of them.
;;   gzip -6: -9 saves 0.4% more for more time.  If gzip fails, or takes
;;   past `xwapp--gzip-wait', the file goes plain, under its own ETag.
;;   gzip reads a file, not a pipe from Emacs: writing to a pipe, Emacs
;;   waits as it does on the proxy's socket (\"A big answer goes from a
;;   thread\") -- 72 ms for 210 KB, 190 for 540 -- while a file of them is
;;   written in 1 or 2 ms (2026-11-03).
;;
;; And the page's calls, its `next', go gzip'd through the proxy.  They
;; carry what the page shows -- the herd's Inbox 44 KB, its sessions 389
;; KB -- over the socket that holds Emacs to some 340 KB a second, then
;; the phone's link; gzip'd, they are six to nine times smaller.  So a
;; `next' of `xwapp--gzip-next-min' or more goes gzip'd when it was asked
;; through the proxy by a browser that takes it.  Not on loopback, where
;; 389 KB take 2 ms and gzip would add 3 to 12; nor a smaller one, which
;; the socket takes at once.
;;
;; Nothing that holds a secret goes gzip'd.  Compressed, a body is as
;; long as what in it does not repeat: one that holds a secret beside
;; text someone else can put in it -- an agent's, a pull request's, a
;; task's -- tells a watcher on the path, who sees the lengths alone,
;; whether a guess at the secret matched, a guess at a time (BREACH).
;; xwapp's secrets are its pages': an address opens its page as you.
;; Emacs puts one in a body for the QR code (`xwapp--show-qr'), and any
;; text that quotes an address carries one too -- a note you pasted, an
;; agent's reply that names it.  So the rule is no list of who sends
;; what: everything gzip'd here, a `next' or a file, is gzip'd by
;; `xwapp--deflate' alone, which looks in the bytes for every secret
;; xwapp keeps (`xwapp--holds-secret-p') and gives them back plain when
;; one is there.  The length of what goes plain says nothing of what is
;; in it but its own length.

(defvar xwapp--gzip-program "gzip"
  "The gzip that compresses the page's files, or nil to send them plain.")

(defconst xwapp--gzip-min 1024
  "Bytes below which a file goes plain: gzip would gain little.")

(defconst xwapp--gzip-next-min (* 8 1024)
  "Bytes from which the page's calls go gzip'd through the proxy.
Fewer, the proxy's socket takes at once (`xwapp--send-aside'), and gzip
would add more time than it saves.")

(defvar xwapp--gzip-keep (* 4 1024 1024)
  "Bytes of gzip'd files kept, at most.  All the pages' files, gzip'd,
are some 300 KB: this holds a dozen versions of them.")

(defvar xwapp--gzip-wait 3
  "Seconds a file may take to gzip before it goes plain.")

(defvar xwapp--gzipped (make-hash-table :test 'equal)
  "A file's ETag -> its gzip'd bytes.")

(defvar xwapp--gzipped-order nil
  "The ETags in `xwapp--gzipped', the most lately used first.")

(defvar xwapp--gzipping (make-hash-table :test 'equal)
  "A file's ETag -> the functions waiting for it to be gzip'd.")

(defun xwapp--gzip-type-p (type size)
  "Non-nil when a file of TYPE and SIZE goes gzip'd to who takes it."
  (and xwapp--gzip-program
       (>= size xwapp--gzip-min)
       (string-match-p "\\`\\(?:text/\\|image/svg\\+xml\\)" type)))

(defun xwapp--takes-gzip-p (headers)
  "Non-nil when HEADERS' Accept-Encoding takes gzip: named, or *, and
not with q=0."
  (when-let* ((accept (cdr (assoc "accept-encoding" headers))))
    (let (gzip any)
      (dolist (part (split-string accept "," t "[ \t]+"))
        (let* ((bits (split-string part ";" t "[ \t]+"))
               (q (if-let* ((p (seq-find (lambda (b) (string-prefix-p "q=" b)) (cdr bits))))
                      (string-to-number (substring p 2))
                    1)))
          (pcase (downcase (car bits))
            ((or "gzip" "x-gzip") (setq gzip q))
            ("*" (setq any q)))))
      (> (or gzip any 0) 0))))

(defun xwapp--gz-etag (etag)
  "The ETag of a file's gzip'd bytes, the file's being ETAG."
  (concat (substring etag 0 -1) "-gz\""))

(defun xwapp--gzip-kept (etag)
  "The gzip'd bytes kept for the file of ETAG, or nil; now the latest used."
  (when-let* ((bytes (gethash etag xwapp--gzipped)))
    (setq xwapp--gzipped-order (cons etag (delete etag xwapp--gzipped-order)))
    bytes))

(defun xwapp--gzip-keep (etag bytes)
  "Keep BYTES as the gzip'd file of ETAG, letting the least lately used
go past `xwapp--gzip-keep'."
  (puthash etag bytes xwapp--gzipped)
  (setq xwapp--gzipped-order (cons etag (delete etag xwapp--gzipped-order)))
  (let ((total 0) (kept nil))
    (dolist (tag xwapp--gzipped-order)
      (let ((size (length (gethash tag xwapp--gzipped))))
        (if (and kept (> (+ total size) xwapp--gzip-keep))
            (remhash tag xwapp--gzipped)
          (setq total (+ total size))
          (push tag kept))))
    (setq xwapp--gzipped-order (nreverse kept))))

(defun xwapp--gzip (file etag then)
  "Call THEN with FILE gzip'd, its ETag being ETAG -- or with nil, when
it could not be.  At once when kept; else when gzip, a process of its
own, is done: one for all who ask for it meanwhile."
  (if-let* ((bytes (xwapp--gzip-kept etag)))
      (funcall then bytes)
    (let ((waiting (gethash etag xwapp--gzipping)))
      (puthash etag (cons then waiting) xwapp--gzipping)
      (unless waiting
        (xwapp--gzip-start file etag)))))

(defun xwapp--gzip-start (file etag)
  "Gzip FILE, of ETAG; hand the bytes to who waits for them."
  (xwapp--deflate
   (condition-case nil (xwapp--bytes (file-truename file)) (error nil))
   (lambda (bytes)
     (let ((waiting (gethash etag xwapp--gzipping)))
       (remhash etag xwapp--gzipping)
       ;; Kept only when still of this version of the file.
       (when (and bytes (equal (xwapp--etag file) etag))
         (xwapp--gzip-keep etag bytes))
       (dolist (then (nreverse waiting))
         (xwapp--done (lambda () (funcall then bytes))))))))

(defun xwapp--secrets ()
  "Every secret xwapp keeps: of each page it serves, and each kept over
a restart (`xwapp-state-file') for an app not loaded yet."
  (let (secrets)
    (maphash (lambda (_app plist)
               (when-let* ((secret (plist-get plist :secret))) (push secret secrets)))
             xwapp--served)
    (dolist (page (plist-get (xwapp--state) :pages))
      (push (nth 1 page) secrets))
    (delete-dups secrets)))

(defun xwapp--holds-secret-p (bytes)
  "Non-nil when BYTES hold a secret xwapp keeps (`xwapp--secrets'), in
either case.  Some 1 ms for 540 KB."
  (let ((case-fold-search t))
    (seq-some (lambda (secret) (string-match-p (regexp-quote secret) bytes))
              (xwapp--secrets))))

(defun xwapp--deflate (bytes then)
  "Call THEN with BYTES gzip'd -- or with nil, for them to go plain: when
they hold a secret (`xwapp--holds-secret-p'), and when gzip is not
there, fails, or takes past `xwapp--gzip-wait'.  The one way anything
goes gzip'd here: see \"The page's own files go gzip'd\".  At once when
plain; else when gzip, a process of its own, is done."
  (let ((file (and bytes xwapp--gzip-program (not (xwapp--holds-secret-p bytes))
                   (condition-case nil
                       (with-file-modes #o600
                         (let ((coding-system-for-write 'no-conversion))
                           (make-temp-file "xwapp-gz" nil nil bytes)))
                     (error nil)))))
    (if (not file)
        (funcall then nil)
      (xwapp--gzip-run file (lambda (gz)
                              (ignore-errors (delete-file file))
                              (funcall then gz))))))

(defun xwapp--gzip-run (file then)
  "Gzip FILE in a process; call THEN with the bytes, or nil when gzip is
not there, fails, or takes past `xwapp--gzip-wait'."
  (xwapp-run "xwapp-gzip" (list xwapp--gzip-program "-c" "-n" "-6" "--" file)
              xwapp--gzip-wait then))

(defun xwapp-run (name command wait then)
  "Run COMMAND, a process called NAME; call THEN with what it wrote, as
bytes -- or with nil when it is not there, fails, or takes past WAIT
seconds.  Emacs waits for it not."
  (let* ((chunks nil)
         ;; Its own, not one Emacs makes for a buffer: that one outlives
         ;; a process killed.
         (err (make-pipe-process :name (concat name " stderr") :noquery t :coding 'binary
                                 :filter #'ignore :sentinel #'ignore))
         (proc
          (condition-case nil
              (make-process
               :name name :noquery t :connection-type 'pipe :coding 'binary
               :command command
               :stderr err
               :filter (lambda (_p s) (push s chunks))
               :sentinel
               (lambda (p _e)
                 (unless (process-live-p p)
                   (when-let* ((timer (process-get p 'xwapp-timer))) (cancel-timer timer))
                   (process-put p 'xwapp-timer nil)
                   (ignore-errors (delete-process err))
                   (unless (process-get p 'xwapp-done)
                     (process-put p 'xwapp-done t)
                     (funcall then (and (eq (process-status p) 'exit) (zerop (process-exit-status p))
                                        (apply #'concat (nreverse chunks))))))))
            (error nil))))
    (if (not proc)
        (progn (ignore-errors (delete-process err))
               (funcall then nil))
      (process-put proc 'xwapp-timer
                   (run-at-time wait nil
                                (lambda () (when (process-live-p proc) (delete-process proc))))))))

(defun xwapp--file (app proc parts &optional headers)
  "Answer PROC with the file PARTS name: of APP's page, or of xwapp's kit.
A browser that has it already, as its If-None-Match among HEADERS says,
is told so (304) rather than sent it again.  Gzip'd to one that takes
it: see \"The page's own files go gzip'd\"."
  (let* ((root (xwapp--root app))
         (file (expand-file-name (string-join parts "/") root)))
    (if (and parts
             (not (seq-some (lambda (p) (string-prefix-p "." p)) parts))
             (seq-some (lambda (dir) (string-prefix-p dir file))
                       (list (file-name-directory (xwapp-index app))
                             (expand-file-name "xwapp/ui/" root)))
             (file-regular-p file))
        (let* ((etag (xwapp--etag file))
               (type (xwapp--type file))
               (size (file-attribute-size (file-attributes (file-truename file))))
               (varies (and etag size (xwapp--gzip-type-p type size)))
               (gzip (and varies (xwapp--takes-gzip-p headers)))
               (keep (lambda (tag)
                       `(("Cache-Control" . "no-cache")
                         ,@(when tag `(("ETag" . ,tag)))
                         ,@(when varies '(("Vary" . "Accept-Encoding"))))))
               (tag (if gzip (xwapp--gz-etag etag) etag)))
          (cond
           ((xwapp--fresh-p tag headers)
            (xwapp--respond proc 304 nil nil (funcall keep tag)))
           (gzip
            (xwapp--gzip file etag
                         (lambda (bytes)
                           (if bytes
                               (xwapp--respond proc 200 type bytes
                                               (cons '("Content-Encoding" . "gzip") (funcall keep tag)))
                             (xwapp--respond proc 200 type (xwapp--bytes file) (funcall keep etag))))))
           (t
            (xwapp--respond proc 200 type (xwapp--bytes file) (funcall keep etag)))))
      (xwapp--respond proc 404))))

;; Files an app serves besides its page: images an agent named, say, or
;; documents to read.

(defconst xwapp-sandbox-policy
  (concat "sandbox; default-src 'none'; "
          "style-src 'unsafe-inline' https://fonts.googleapis.com; "
          "font-src https://fonts.gstatic.com; img-src data:; "
          "base-uri 'none'; form-action 'none'; frame-ancestors 'none'")
  "The Content-Security-Policy a document served :sandbox goes with.
It comes from the page's own origin, under its secret, and the page can
act for Emacs -- so a document must never run, nor reach the page.  No
script of its runs (`sandbox' without allow-scripts), and its origin is
none (without allow-same-origin): it is not the page's, so even a
script that did run could ask the page's endpoints nothing as it.  It
loads only what a document made to stand alone needs -- its own inline
styles, Google's fonts, data: images -- and nothing from Emacs.  No
form posts, no frame holds it.")

(defun xwapp-serve-files (app dir regexp &rest how)
  "Serve APP's page the files in DIR whose names match REGEXP whole.
DIR is a directory, a list of them, or a function of no arguments giving
either, asked each time; a name is looked for in each in turn, and the
first that has it serves it.  A name is one segment, and only a regular
file there -- not a symlink -- is served.  In a browser at .files/NAME
under the page's secret, as the page itself is, to whoever the page is
served to; in the xwidget as a file: URL, its directory told to the
page on its hello.  The page has both as XW.file(NAME).  Anyone the
page is served to can ask for any name: make the names ones nobody
guesses, or serve only what they may all read.

HOW, keywords, makes a set of files served apart from those:
  :as SET       its name.  The page asks for NAME at .SET/NAME, and has
                it as XW.file(NAME, SET) -- in a browser alone: in the
                xwidget it is not a file: URL, and `xwapp-show-file'
                shows it.
  :sandbox t    its files are documents, to be read and never run: each
                goes with `xwapp-sandbox-policy'.
  :max-bytes N  a file past N bytes is not served.
  :exact t      NAME must be the file's own, to the letter: a file
                system that ignores case (macOS's) would find it under
                another.  Each request lists the directory -- not for a
                big one, nor for names of one case, which need it not.
  :fresh t      its files change under the same name (documents you
                edit): the browser asks each time rather than keep one.

Said again the same, it does nothing; anew, a page already open in the
xwidget is told at once where the files of .files are."
  (let ((set (or (plist-get how :as) "files"))
        (spec (list :dir dir :regexp regexp
                    :sandbox (and (plist-get how :sandbox) t)
                    :max-bytes (plist-get how :max-bytes)
                    :exact (and (plist-get how :exact) t)
                    :fresh (and (plist-get how :fresh) t)))
        (sets (xwapp--get app :sets)))
    (unless (let ((case-fold-search nil)) (string-match-p "\\`[a-z][a-z0-9-]*\\'" set))
      (error "xwapp: a set's name is a word in lower case, not %S" set))
    (unless (equal (cdr (assoc set sets)) spec)
      (xwapp--put app :sets (cons (cons set spec) (assoc-delete-all set (copy-sequence sets))))
      (when (equal set "files")
        (xwapp--tell-files app)))))

(defun xwapp--set (app &optional set)
  "How APP serves the files of SET, of .files by default, or nil."
  (cdr (assoc (or set "files") (xwapp--get app :sets))))

(defun xwapp--tell-files (app)
  "Tell APP's page in the xwidget, a page of file:, where the files it is
served are (`xwapp-serve-files'): it reaches them as files.  A page in
a browser asks for them by name, and is told nothing."
  (when-let* (((not (xwapp--get app :page)))
              (dir (xwapp--files-dir app)))
    (xwapp--kit app `((xwapp . "files")
                      (base . ,(concat "file://" (mapconcat #'url-hexify-string
                                                            (split-string dir "/") "/")))))))

(defun xwapp--files-dirs (app &optional set)
  "The directories APP serves the files of SET from (`xwapp-serve-files')."
  (when-let* ((spec (xwapp--set app set))
              (dir (plist-get spec :dir))
              (dirs (if (functionp dir) (funcall dir) dir)))
    (mapcar (lambda (d) (file-name-as-directory (expand-file-name d)))
            (seq-filter #'stringp (ensure-list dirs)))))

(defun xwapp--files-dir (app &optional set)
  "The first directory APP serves the files of SET from, or nil:
its one, for .files (`xwapp--tell-files')."
  (car (xwapp--files-dirs app set)))

(defun xwapp--file-of (app name &optional set)
  "The file APP serves as NAME in SET, .files by default
\(`xwapp-serve-files'), or nil."
  (when-let* ((spec (xwapp--set app set))
              ((stringp name))
              ((let ((case-fold-search nil))
                 (string-match-p (concat "\\`\\(?:" (plist-get spec :regexp) "\\)\\'") name)))
              ((not (or (string-search "/" name) (string-search "\0" name)
                        (member name '("." ".."))))))
    (seq-some
     (lambda (dir)
       (let ((file (expand-file-name name dir))
             (max (plist-get spec :max-bytes)))
         (and (equal (file-name-directory file) dir)
              (not (file-symlink-p file))
              (file-regular-p file)
              (or (not max)
                  (<= (or (file-attribute-size (file-attributes file)) (1+ max)) max))
              ;; By its own name, not one the file system takes for it.
              (or (not (plist-get spec :exact))
                  (directory-files dir nil (concat "\\`" (regexp-quote name) "\\'") t 1))
              file)))
     (xwapp--files-dirs app set))))

(defun xwapp--served-file (app proc parts &optional set)
  "Answer PROC with the file PARTS name among those APP serves in SET.
Its name never changes what it holds, so the browser keeps it -- unless
the set is :fresh."
  (if-let* (((= (length parts) 1))
            (spec (xwapp--set app set))
            (file (xwapp--file-of app (car parts) set)))
      (xwapp--respond proc 200 (xwapp--type file) (xwapp--bytes file)
                      (append
                       (unless (plist-get spec :fresh)
                         '(("Cache-Control" . "private, max-age=604800, immutable")))
                       (when (plist-get spec :sandbox)
                         `(("Content-Security-Policy" . ,xwapp-sandbox-policy)))))
    (xwapp--respond proc 404)))

(defun xwapp-file-url (app name &optional set)
  "The address of the file APP serves as NAME in SET, for this machine's
browser (`xwapp-serve-files').  It holds APP's secret."
  (format "http://127.0.0.1:%d/%s/.%s/%s" (xwapp--port) (xwapp--secret app)
          (or set "files") (url-hexify-string name)))

(defvar xwapp--shown (make-hash-table :test 'equal)
  "Address -> the buffer `xwapp-show-file' shows it in.")

(defun xwapp-show-file (app name &optional set)
  "Show the file APP serves as NAME in SET (`xwapp-serve-files') in a view
of its own, from Emacs as a browser gets it: over loopback, under the
page's secret, so with its set's headers -- a :sandbox document runs
nothing here either.  Not as a file: URL, which would run a document's
scripts with the run of your files.  In the web browser when
`xwapp-browser', or when a page there asked; else in an xwidget of its
own, the one it is in already if there is one."
  (unless (xwapp--file-of app name set)
    (user-error "xwapp: %s is not served" name))
  (let ((url (xwapp-file-url app name set)))
    (if (or xwapp-browser xwapp--from-browser)
        (funcall xwapp-browse-function url)
      (unless (featurep 'xwidget-internal)
        (user-error "xwidget-webkit required. Rebuild Emacs with xwidgets"))
      (require 'xwidget)
      (let* ((buf (gethash url xwapp--shown))
             (xw (and (buffer-live-p buf)
                      (car (ignore-errors (get-buffer-xwidgets buf))))))
        (if (and xw (xwidget-live-p xw))
            (progn (pop-to-buffer buf)
                   ;; Afresh, and at it: a link in it may have led elsewhere.
                   (xwidget-webkit-goto-uri xw url))
          (setq buf (xwidget-webkit--create-new-session-buffer url))
          (pop-to-buffer buf)
          ;; Named for the file until its title comes.
          (rename-buffer (format "*%s*" name) t)
          (xwidget-webkit-goto-uri (xwidget-webkit-last-session) url)
          (puthash url buf xwapp--shown))))))

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

;; A page that comes back where it is.
;;
;; A phone's tab asleep past `xwapp--gone-after' is let go, and wakes to
;; a 410 free.  It used to load again, which costs the herd page some
;; 475 KB over the tailnet, a white flash, and where it was scrolled to
;; and what it had open.  A page that can (XW.bridge's `back') now says
;; a soft hello where it is instead, and asks again for what it shows.
;; Only while it is made of the files Emacs serves now: one loaded
;; before a ui/ file changed -- Emacs restarted on new code, say -- loads
;; again.  So Emacs names the files in a fingerprint, given in answer to
;; the page's hello (the header Xwapp-Ui) and with a 410 free (its
;; "ui"): the page comes back in place only when the two are one.

(defconst xwapp--ui-max 200
  "Most files `xwapp--ui' looks at: a page's directory and the kit's
have a few dozen.")

(defun xwapp--ui (app)
  "What APP's page is made of, as a short fingerprint: each file of its
directory and of the kit's, by path, size and time.  A page loaded
before one of them changed is not made of these.  Nil when there are
none, or more than `xwapp--ui-max' -- then it is not a page's directory."
  (let ((stamps nil) (left xwapp--ui-max))
    (cl-labels ((walk (dir)
                  (dolist (file (ignore-errors
                                  (directory-files dir t directory-files-no-dot-files-regexp t (1+ left))))
                    (when (< (cl-decf left) 0) (throw 'many nil))
                    (if (file-directory-p file)
                        (walk file)
                      (when-let* ((stamp (xwapp--stamp file))) (push stamp stamps))))))
      (catch 'many
        (dolist (dir (list (file-name-directory (xwapp-index app))
                           (expand-file-name "xwapp/ui/" (xwapp--root app))))
          (walk dir))
        (and stamps
             (substring (secure-hash 'sha1 (string-join (sort stamps #'string<) "\n")) 0 16))))))

(defun xwapp--refuse (proc why &optional app)
  "Answer PROC that its page is not the app's, and WHY.  A page APP has
let go, and can open again (free), is told what APP's page is made of
now, to come back where it is if it is made of that."
  (xwapp--respond proc 410 "application/json"
                  (json-encode `((why . ,why)
                                 ,@(when-let* (((equal why "free"))
                                               (app)
                                               (ui (xwapp--ui app)))
                                     `((ui . ,ui)))))))

(defun xwapp--hello (app proc page soft)
  "PAGE has loaded in a browser tab: it is APP's page now.
A SOFT hello is a page coming back on its own: it takes APP only when
APP is free.  It is told what it is made of (`xwapp--ui'), to know
itself when told again."
  (let ((why (xwapp--why app))
        (asked (xwapp--get app :asked)))
    (if (and soft (not (equal why "free")))
        (xwapp--refuse proc why app)
      (xwapp--put app :asked nil)
      (xwapp--let-go app)
      (xwapp--put app :page page)
      (xwapp--greet app)
      (xwapp--timer app :gone xwapp--gone-after #'xwapp--gone)
      ;; Moved, not closed: with :page set, the buffer's death runs no on-kill.
      (when-let* ((buf (xwapp-buffer app)))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buf)))
      (xwapp--respond proc 204 nil nil
                      (append (when-let* ((ui (xwapp--ui app))) `(("Xwapp-Ui" . ,ui)))
                              ;; It takes the page's timings, and when they are slow.
                              (when xwapp-timing
                                `(("Xwapp-Timing" . ,(number-to-string
                                                      (round (* 1000 xwapp-timing-slow))))))))
      (xwapp--save)
      ;; Open nowhere until now, and not by Emacs: made ready as its
      ;; command would have.
      (when (and (equal why "free") (not asked))
        (run-at-time 0 nil (xwapp-reopen app))))))

(defun xwapp--next (app proc page &optional gzip wake)
  "Hold PROC, PAGE's request for calls, until there are some.
GZIP non-nil: it came through the proxy from a browser that takes gzip,
and is answered so when big (\"The page's own files go gzip'd\").
WAKE non-nil: the page has just woken, and is answered at once, calls
or none -- it learns it reaches Emacs again, and when (ui/xwapp.js, \"How
long it took\")."
  (if (not (xwapp--current-p app page))
      (xwapp--refuse proc (xwapp--why app) app)
    (xwapp--untimer app :gone)
    (when-let* ((old (xwapp--get app :waiting)))
      (xwapp--put app :waiting nil)
      (xwapp--respond old 200 "application/json" "[]"))
    (process-put proc 'xwapp-app app)
    (process-put proc 'xwapp-gzip gzip)
    (xwapp--put app :waiting proc)
    (if (or (xwapp--get app :outbox) wake
            (and (xwapp--get app :later) (not (xwapp--holding-p app))))
        (xwapp--timer app :flush 0 #'xwapp--flush)
      (xwapp--timer app :hold xwapp--hold #'xwapp--flush))))

;; Calls that can wait.
;;
;; A `next' answer goes whole before the page has any of it, and while
;; it goes nothing else reaches the page: over a phone's slow link -- 10
;; to 20 KB a second, relayed -- 150 KB of conversations took 25 s, and a
;; tap's answer waited behind them.  So an app can say a call can wait
;; (`xwapp-js' LATER): it goes after every other call, and a `next'
;; carries such calls only up to `xwapp--later-budget' bytes with the
;; rest -- one at least, so they always move.  The page asks again the
;; moment it has an answer, and the next part goes then.  What the app
;; needs out before it sends more, it waits on with `xwapp-when-sent'.
;; And while Emacs finds the page's device a direct path, they may wait
;; for it, a few seconds at most, when `xwapp-tailscale-hold' says so
;; ("Kicking the path").

(defconst xwapp--later-budget (* 48 1024)
  "Bytes of calls a `next' carries at most, when some can wait: about a
second at a phone's slow link, gzip'd.")

(defun xwapp--call (app json &optional later)
  "Send JSON, one call, to APP's browser page.
Scrubbed and size-capped like the xwidget path: an oversized or unclean
payload is dropped rather than queued.  LATER non-nil: it can wait
\(\"Calls that can wait\"), named LATER."
  (when-let* ((safe (xwapp--script-safe json)))
    (if later
        (xwapp--put app :later (append (xwapp--get app :later) (list (cons later safe))))
      (xwapp--put app :outbox (cons safe (xwapp--get app :outbox))))
    ;; On the next turn, so the calls of one command go out together.
    (when (and (xwapp--get app :waiting) (not (xwapp--get app :flush))
               (not (and later (xwapp--holding-p app))))
      (xwapp--timer app :flush 0 #'xwapp--flush))))

(defun xwapp-js-forget (app name)
  "Drop the calls that can wait named NAME (`xwapp-js' LATER) not yet
sent to APP's page: what they say is of no use to it any more."
  (xwapp--put app :later (seq-remove (lambda (c) (equal (car c) name)) (xwapp--get app :later))))

(defun xwapp-when-sent (app fn)
  "Call FN once the calls made of APP's page so far are out -- not those
that can wait.  In Emacs's own page each ran as it was made: at once.
In a browser tab, once the `next' answer that carries the last of them
has gone, or failed to go.  Not when the page is let go first."
  (cond ((not (xwapp--get app :page)) (funcall fn))
        ((xwapp--get app :outbox)
         (xwapp--put app :when-sent (append (xwapp--get app :when-sent) (list fn))))
        ((process-live-p (xwapp--get app :sending))
         (let ((proc (xwapp--get app :sending)))
           (process-put proc 'xwapp-when-sent (append (process-get proc 'xwapp-when-sent) (list fn)))))
        (t (funcall fn))))

(defun xwapp--flush (app)
  "Answer APP's held `next' with the calls gathered since the last, and
of those that can wait as many as `xwapp--later-budget' leaves room for."
  (xwapp--untimer app :hold)
  (xwapp--untimer app :flush)
  (when-let* ((proc (xwapp--get app :waiting)))
    (let* ((calls (reverse (xwapp--get app :outbox)))
           (size (apply #'+ (mapcar #'string-bytes calls)))
           (later (xwapp--get app :later))
           (held (and later (xwapp--holding-p app)))
           (took nil))
      (while (and later (not held)
                  (or (null took) (<= (+ size (string-bytes (cdar later))) xwapp--later-budget)))
        (setq size (+ size (string-bytes (cdar later))))
        (push (cdr (pop later)) took))
      (setq calls (append calls (nreverse took)))
      (when-let* ((req (process-get proc 'xwapp-req)))
        (plist-put req :tf (float-time)))
      (process-put proc 'xwapp-when-sent (xwapp--get app :when-sent))
      (xwapp--put app :when-sent nil)
      (xwapp--put app :waiting nil)
      (xwapp--put app :outbox nil)
      (xwapp--put app :later later)
      (xwapp--put app :sending proc)
      (xwapp--timer app :gone xwapp--gone-after #'xwapp--gone)
      (let ((body (encode-coding-string (concat "[" (string-join calls ",") "]") 'utf-8))
            (sent (lambda () (xwapp--sent app proc))))
        (if (and (process-get proc 'xwapp-gzip) (>= (length body) xwapp--gzip-next-min))
            (xwapp--deflate body
                            (lambda (gz)
                              (if gz
                                  (xwapp--respond proc 200 "application/json" gz
                                                  '(("Content-Encoding" . "gzip") ("Vary" . "Accept-Encoding"))
                                                  sent)
                                (xwapp--respond proc 200 "application/json" body nil sent))))
          (xwapp--respond proc 200 "application/json" body nil sent))))))

(defun xwapp--sent (app proc)
  "PROC, the answer to APP's page's `next', is out, or failed to go: the
page has `xwapp--gone-after' from now to ask again -- unless it has, or
is APP's page no more.  What waited for it to be out is called."
  (let ((fns (process-get proc 'xwapp-when-sent)))
    (process-put proc 'xwapp-when-sent nil)
    (when (xwapp--get app :page)
      (dolist (fn fns) (xwapp--done fn))))
  (when (eq (xwapp--get app :sending) proc)
    (xwapp--put app :sending nil)
    (when (and (xwapp--get app :page) (not (xwapp--get app :waiting)))
      (xwapp--timer app :gone xwapp--gone-after #'xwapp--gone))))

(defun xwapp--intent (app proc page body)
  "Take PAGE's intent BODY: xwapp's own now, the app's on the next turn."
  (let ((intent (ignore-errors
                  (json-parse-string (decode-coding-string body 'utf-8)
                                     :object-type 'alist :array-type 'list))))
    (cond
     ((not (xwapp--current-p app page)) (xwapp--refuse proc (xwapp--why app) app))
     ((not (consp intent)) (xwapp--respond proc 400))
     (t
      (xwapp--respond proc 204)
      (let ((op (alist-get 'op intent))
            (req (process-get proc 'xwapp-req)))
        (when req
          (plist-put req :op
                (if (and (stringp op) (let ((case-fold-search nil))
                                        (string-match-p "\\`[a-z][a-z0-9-]\\{0,39\\}\\'" op)))
                    op "?")))
        (pcase op
          ("xwapp-seen" (xwapp--put app :seen (eq (alist-get 'seen intent) t)))
          ;; The kit's own: the page's timings, joined with Emacs's.
          ("xwapp-timing"
           (when xwapp-timing
             (run-at-time 0 nil #'xwapp--timing-take app intent req)))
          ;; As the title's intents are: from a timer, not this filter.
          (_ (run-at-time 0 nil #'xwapp--handle app intent req))))))))

(defun xwapp--handle (app intent &optional req)
  "Hand APP INTENT, from its page in a browser tab: a page it opens
opens there too.  REQ, the intent's request as timed, is told when its
handler began and ended."
  (let ((xwapp--from-browser t)
        (start (float-time)))
    (unwind-protect (xwapp--dispatch app intent)
      (when req
        (plist-put req :th start)
        (plist-put req :tw (float-time))))))

(defun xwapp--sentinel (proc _event)
  "A connection whose reader hung up: deleted, in the main thread, which
waits on it -- one a thread sent a big answer on, say.  A held `next':
the page may be gone."
  (unless (or (process-live-p proc) (eq proc xwapp--server) (eq proc xwapp--unix-server))
    (xwapp--close proc))
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
  ;; Not :unhold: a kick for the page's device may have just begun,
  ;; its page saying hello -- let go now, the hello's own.
  (dolist (key '(:hold :flush :gone :forget))
    (xwapp--untimer app key))
  (let ((proc (xwapp--get app :waiting)))
    ;; Before answering: the sentinel must not take the hang-up for a
    ;; page that went.  An answer still going out to it goes on, to a
    ;; page that is not the app's.
    (dolist (key '(:waiting :page :outbox :later :when-sent :seen :restored :sending))
      (xwapp--put app key nil))
    (when proc
      (xwapp--refuse proc "taken"))))

(defun xwapp--gone (app)
  "APP's browser page has stopped asking: it was closed.  What Emacs
made ready for a tab it opened is undone with it.  Not while an answer
to it is still going out: it cannot ask again before it has that, and
`xwapp--sent' gives it its time once it has."
  (if (process-live-p (xwapp--get app :sending))
      (xwapp--put app :gone nil)
    (xwapp--gone-now app)))

(defun xwapp--gone-now (app)
  "APP's browser page was closed: see `xwapp--gone'."
  (xwapp--let-go app)
  (xwapp--put app :asked nil)
  (xwapp--save)
  (when (and (xwapp-on-kill app) (not (xwapp-session app)))
    (funcall (xwapp-on-kill app))))


;;; How long the page took
;;
;; A moment, to the page: its load (an open, or a reload), from the
;; tap to the page drawn -- what the app calls ready (XW.ready), or else
;; Emacs's first calls; a wake, from the page shown again to Emacs's
;; first answer to a `next' (asked with wake=1, so answered at once);
;; and an intent answered past `xwapp-timing-slow'.  And the app's own:
;; one it begins (XW.begin(NAME)) and says is over (XW.ready(NAME)) --
;; the herd page's "room", from a tap on a room to its conversation
;; drawn -- of kind NAME, a word.  Once one is over the page sends its
;; timings, the kit's intent xwapp-timing:
;;
;;   {k: "open"|"reload"|"wake"|"intent"|the app's, at: its start (ms, the page's
;;    clock), d: how long, end: what ended it, m: {mark: ms...},
;;    r: [{p: path, n: the kit's number, s: start, q: until sent --
;;         connecting, dns tcp tls --, w: until the first byte back,
;;         dl: the rest, b: bytes, z: gzip'd, c: from its cache}...],
;;    omit: requests left out, why/away: what woke it, after how long}
;;
;; Timings only: no request's content, its path without the secret.
;; Emacs joins each request with its own (\"What each request cost\"):
;; the kit's by the page and its number, a file by its path and time.
;;
;; Then each request's wait, w, comes apart.  Emacs knows its own part
;; to the millisecond -- Emacs preparing (a `next' held while Emacs had
;; nothing to say: the app at work, its sidecar) and Emacs answering
;; (reading a file, gzip, the calls' JSON) -- and the rest is the
;; network both ways, and Emacs busy: the request in Emacs's socket while
;; its main thread was elsewhere, read late.  Emacs cannot see that: its
;; filter only runs late.  The page's clock can, once the two clocks are
;; lined up -- as NTP does, by the request whose round trip, less
;; Emacs's part, is the shortest.  The way there past the way back is
;; then Emacs busy, the network taken to take as long each way.  Off by
;; at most that shortest round trip; on one machine, as in the tests,
;; exact.  What follows the first byte -- the download -- is Emacs
;; answering when Emacs was the one slow to send it (its socket to the
;; proxy: \"A big answer goes from a thread\"), else the network.
;;
;; And the moment comes apart into what it waited on, each instant
;; given to the most telling of the requests under way then -- Emacs
;; busy, Emacs answering, the network, Emacs preparing, in that order --
;; and to the page when none was: its own work, its scripts and drawing.
;;
;; A slow one from another device -- through the proxy -- is said with
;; how tailscale reaches that device now: straight, or relayed and
;; where through (`xwapp-tailscale-program', asked at most every
;; `xwapp--tailscale-fresh' seconds, a process Emacs does not wait on).
;; Kept: the slow ones and some others, to compare -- in memory and in
;; `xwapp-timing-file' -- and handed to `xwapp-timing-functions'.  Nothing
;; kept holds a secret: every string is looked through for each secret
;; xwapp keeps, plain or %-escaped, and any 32 hex digits, before.

(defvar xwapp-timing-functions nil
  "Functions called as (APP RECORD) with each page timing kept.
RECORD is a plist: see `xwapp-timings' and \"How long the page took\";
:slow non-nil when it is.")

(defvar xwapp--timing-save (not noninteractive)
  "Non-nil when records are written to `xwapp-timing-file'.  A batch
Emacs, a test's, writes none of yours.")

(defvar xwapp--timing-keep-slow 100
  "Slow records kept, the latest.")

(defvar xwapp--timing-keep-normal 20
  "Records not slow kept, the latest: to compare a slow one with.")

(defconst xwapp--timing-max-reqs 40
  "Most requests a record lists; the page sends no more either.")

(defvar xwapp--timings nil
  "The pages' timing records, the latest first.")

(defvar xwapp--timings-loaded nil
  "Non-nil once `xwapp-timing-file' has been read.")

(defvar xwapp--timing-id 0
  "The last id given a record.")

(defconst xwapp--timing-names
  '((net . "network") (busy . "Emacs busy") (emacs . "Emacs answering")
    (prep . "Emacs preparing") (queue . "queued in the page") (page . "page"))
  "What a moment waited on, by name: see \"How long the page took\".")

(defun xwapp--timing-sec (v)
  "V, milliseconds from the page, in seconds; nil unless a sound number."
  (and (numberp v) (<= 0 v 86400000) (/ v 1000.0)))

(defun xwapp--timing-word (v)
  "V, a word from the page -- a mark's name, a kind -- when it is one."
  (let ((s (if (and v (symbolp v)) (symbol-name v) v)))
    (and (stringp s)
         (let ((case-fold-search nil)) (string-match-p "\\`[a-z][a-z0-9-]\\{0,23\\}\\'" s))
         s)))

(defun xwapp--timing-page-req (r)
  "R, a request as the page timed it, as a plist of seconds; or nil."
  (let ((p (alist-get 'p r))
        (s (xwapp--timing-sec (alist-get 's r))))
    (when (and (stringp p) s)
      (let ((ms (lambda (k) (or (xwapp--timing-sec (alist-get k r)) 0))))
        (list :p (xwapp--unhex32 p)
              :n (let ((n (alist-get 'n r))) (and (natnump n) n))
              :s s :q (funcall ms 'q) :w (funcall ms 'w) :dl (funcall ms 'dl)
              :dns (funcall ms 'dns) :tcp (funcall ms 'tcp) :tls (funcall ms 'tls)
              :b (let ((b (alist-get 'b r))) (and (natnump b) b))
              :z (and (memq (alist-get 'z r) '(1 t)) t)
              :c (and (memq (alist-get 'c r) '(1 t)) t))))))

(defun xwapp--timing-join (app req reqs base)
  "REQS, the page's, each with Emacs's own of it as :e where Emacs has
it: a request of APP's as timed.  REQ is the record's own request -- whose
page and transport the others share; BASE, the moment's start."
  (let ((mine (seq-filter (lambda (x) (and x (eq (plist-get x :app) app) (plist-get x :t1)))
                          (xwapp--reqs-list)))
        (page (and req (plist-get req :page)))
        (via (and req (plist-get req :via)))
        (taken nil))
    (mapcar
     (lambda (r)
       (let* ((p (plist-get r :p))
              (sent (+ base (plist-get r :s) (plist-get r :q)))
              (e (if (member p '("hello" "next" "intent"))
                     (and page (plist-get r :n)
                          (seq-find (lambda (x) (and (equal (plist-get x :page) page)
                                                     (eql (plist-get x :n) (plist-get r :n))
                                                     (equal (plist-get x :path) p)
                                                     (not (memq x taken))))
                                    mine))
                   ;; A file: the one of that path nearest in time.
                   (let (best)
                     (dolist (x mine best)
                       (let ((off (abs (- (plist-get x :t0) sent))))
                         (when (and (equal (plist-get x :path) p) (eq (plist-get x :via) via)
                                    (not (memq x taken)) (< off 30)
                                    (or (not best) (< off (abs (- (plist-get best :t0) sent)))))
                           (setq best x))))))))
         (if (not e) r
           (push e taken)
           (append r (list :e e)))))
     reqs)))

(defun xwapp--timing-offset (reqs base)
  "Emacs's clock less the page's, and the round trip it is found by:
(OFFSET . ROUND-TRIP), from the request of REQS whose round trip, less
Emacs's part, is the shortest -- or nil when none can tell."
  (let (best)
    (dolist (r reqs best)
      (when-let* ((e (plist-get r :e))
                  (t0 (plist-get e :t0))
                  (t2 (plist-get e :t2))
                  ((> (plist-get r :w) 0)))
        (let* ((sent (+ base (plist-get r :s) (plist-get r :q)))
               (back (+ sent (plist-get r :w)))
               (trip (- (plist-get r :w) (- t2 t0))))
          (when (and (>= trip 0) (or (not best) (< trip (cdr best))))
            (setq best (cons (/ (+ (- t0 sent) (- t2 back)) 2) trip))))))))

(defvar xwapp--timing-clocks nil
  "Each device's clock lined up with Emacs's, the best of late: an alist
\(KEY OFFSET ROUND-TRIP WHEN), KEY its transport and tailnet address.")

(defconst xwapp--timing-clock-keep 1800
  "Seconds a device's clock, lined up, is taken to stay so.")

(defun xwapp--timing-clock (key own)
  "The offset to line up a moment of the device KEY by: OWN, its own,
from `xwapp--timing-offset', or the device's of late when that was found
by a shorter round trip -- as NTP keeps the best of its samples.  A
moment whose every request waited on Emacs has no round trip of its own
short enough to tell: a wake while Emacs was busy.  OWN, when better,
is kept for the next.  (OFFSET . ROUND-TRIP . KEPT), KEPT non-nil when
the device's of late."
  (let* ((kept (assoc key xwapp--timing-clocks))
         (fresh (and kept (< (- (float-time) (nth 3 kept)) xwapp--timing-clock-keep))))
    (if (and fresh (or (not own) (< (nth 2 kept) (cdr own))))
        (list (nth 1 kept) (nth 2 kept) t)
      (when own
        (setf (alist-get key xwapp--timing-clocks nil nil #'equal)
              (list (car own) (cdr own) (float-time))))
      (and own (list (car own) (cdr own) nil)))))

(defun xwapp--timing-split (r base offset)
  "Request R's wait apart, a plist of seconds: :conn :net :busy :prep
:emacs and :dl, the download, with :dlcat what it waited on.  BASE is
the moment's start; OFFSET, from `xwapp--timing-offset'."
  (let* ((e (plist-get r :e))
         (w (plist-get r :w))
         (dl (plist-get r :dl))
         (out (list :conn (plist-get r :q) :dl dl)))
    (if (not (and e offset (plist-get e :t2)))
        ;; Not Emacs's to tell: the network, or the browser's own copy.
        (let ((cached (and (plist-get r :c) (not e))))
          (append out (list :net (if cached 0 w) :busy 0 :prep 0 :emacs 0
                            :dlcat (if cached 'page 'net))))
      (let* ((t0 (plist-get e :t0))
             (t2 (plist-get e :t2))
             (tf (plist-get e :tf))
             (inside (max 0 (- t2 t0)))
             (prep (if tf (max 0 (min inside (- tf t0))) 0))
             (sent (+ base (plist-get r :s) (plist-get r :q)))
             (there (- t0 (car offset) sent))
             (back (- (+ sent w) (- t2 (car offset))))
             (busy (max 0 (min (- there (max back 0)) (- w inside))))
             (send (and (plist-get e :t3) (max 0 (- (plist-get e :t3) t2))))
             ;; Emacs's part as long as the page's wait at most: the page's
             ;; clock had it whole.
             (scale (if (> inside w 0) (/ w inside) 1)))
        (append out (list :net (max 0 (- w (* scale inside) busy)) :busy busy
                          :prep (* scale prep) :emacs (* scale (- inside prep))
                          :send send
                          :dlcat (if (and send (> dl 0.02) (>= send (* 0.8 dl))) 'emacs 'net)))))))

(defun xwapp--timing-sum (reqs d &optional queued)
  "How D seconds went, of REQS laid out by their :ph (`xwapp--timing-
split'): an alist of what waited on -> seconds.  Each instant goes to
the most telling request under way -- busy, emacs, net, prep -- and to
the page when none is.  QUEUED, seconds from the start an intent waited
its turn in the page, goes to queue where no request is under way."
  (let ((rank '((busy . 4) (emacs . 3) (net . 2) (prep . 1) (queue . 0.5)))
        (spans (and queued (> queued 0) (list (list 0 queued 'queue))))
        (sum (list (cons 'net 0.0) (cons 'busy 0.0) (cons 'emacs 0.0) (cons 'prep 0.0)
                   (cons 'queue 0.0) (cons 'page 0.0))))
    (dolist (r reqs)
      (let ((at (plist-get r :s))
            (ph (plist-get r :ph)))
        (dolist (piece `((net . ,(plist-get ph :conn)) (net . ,(/ (plist-get ph :net) 2))
                         (busy . ,(plist-get ph :busy)) (prep . ,(plist-get ph :prep))
                         (emacs . ,(plist-get ph :emacs)) (net . ,(/ (plist-get ph :net) 2))
                         (,(plist-get ph :dlcat) . ,(plist-get ph :dl))))
          (let ((len (or (cdr piece) 0)))
            (when (and (> len 0) (assq (car piece) rank))
              (push (list at (+ at len) (car piece)) spans))
            (setq at (+ at len))))))
    (let ((points (sort (delete-dups
                         (append (list 0.0 (float d))
                                 (mapcan (lambda (s) (list (float (max 0 (min d (car s))))
                                                           (float (max 0 (min d (cadr s))))))
                                         spans)))
                        #'<)))
      (while (cdr points)
        (let* ((a (car points)) (b (cadr points)) (mid (/ (+ a b) 2)) (cat 'page))
          (dolist (s spans)
            (when (and (<= (car s) mid) (< mid (cadr s))
                       (> (alist-get (nth 2 s) rank) (alist-get cat rank 0)))
              (setq cat (nth 2 s))))
          (setf (alist-get cat sum) (+ (alist-get cat sum) (- b a))))
        (setq points (cdr points))))
    sum))

(defun xwapp--timing-round (x)
  "X, seconds, to the millisecond."
  (and (numberp x) (/ (round (* x 1000)) 1000.0)))

(defun xwapp--timing-handlers (app page from to)
  "The handlers of APP's PAGE's intents that ran between FROM and TO,
seconds of Emacs's clock: ((OP . SECONDS) ...), the longest first, those
of 20 ms or more."
  (let (out)
    (dolist (x (xwapp--reqs-list))
      (when (and x (eq (plist-get x :app) app) (equal (plist-get x :page) page)
                 (plist-get x :th) (plist-get x :tw)
                 (<= from (plist-get x :th) to)
                 (>= (- (plist-get x :tw) (plist-get x :th)) 0.02))
        (push (cons (or (plist-get x :op) "?")
                    (xwapp--timing-round (- (plist-get x :tw) (plist-get x :th))))
              out)))
    (seq-take (sort out (lambda (a b) (> (cdr a) (cdr b)))) 6)))

(defun xwapp--timing-record (app intent req)
  "The record of APP's page's timings INTENT, joined with Emacs's; REQ,
the intent's own request.  Nil when INTENT is no sound one."
  (let* ((kind (xwapp--timing-word (alist-get 'k intent)))
         (at (alist-get 'at intent))
         (d (xwapp--timing-sec (alist-get 'd intent))))
    ;; The kit's kinds, or an app's moment: a word, as any mark's name.
    (when (and kind (numberp at) (> at 0) d)
      (let* ((base (/ at 1000.0))
             (raw (let ((r (alist-get 'r intent))) (and (proper-list-p r) r)))
             (page-reqs (delq nil (mapcar #'xwapp--timing-page-req
                                          (seq-take raw xwapp--timing-max-reqs))))
             (reqs (xwapp--timing-join app req page-reqs base))
             (clock (xwapp--timing-clock (list (and req (plist-get req :via)) (and req (plist-get req :fwd)))
                                         (xwapp--timing-offset reqs base)))
             (offset (and clock (cons (nth 0 clock) (nth 1 clock))))
             (reqs (mapcar (lambda (r) (append r (list :ph (xwapp--timing-split r base offset)))) reqs))
             (marks (let (out)
                      (dolist (m (let ((m (alist-get 'm intent))) (and (consp m) m)))
                        (when-let* ((name (xwapp--timing-word (car-safe m)))
                                    (t1 (xwapp--timing-sec (cdr-safe m))))
                          (push (cons name t1) out)))
                      (sort out (lambda (a b) (< (cdr a) (cdr b))))))
             (sum (xwapp--timing-sum reqs d (and (equal kind "intent") (alist-get "sent" marks nil nil #'equal))))
             (es (delq nil (mapcar (lambda (r) (plist-get r :e)) reqs)))
             (gcs (delq nil (mapcar (lambda (e) (plist-get e :gc)) es)))
             (skew (if offset (car offset) 0)))
        (append
         (list :id (setq xwapp--timing-id (1+ xwapp--timing-id))
              :app (string-trim (xwapp-buffer-name app))
              :kind kind
              :at (xwapp--timing-round base)
              :d (xwapp--timing-round d)
              :slow (>= d xwapp-timing-slow)
              :via (and req (plist-get req :via))
              :end (xwapp--timing-word (alist-get 'end intent))
              :why (xwapp--timing-word (alist-get 'why intent))
              :away (xwapp--timing-round (xwapp--timing-sec (alist-get 'away intent)))
              :sum (mapcar (lambda (c) (cons (car c) (xwapp--timing-round (cdr c)))) sum)
              :marks (mapcar (lambda (m) (cons (car m) (xwapp--timing-round (cdr m)))) marks)
              :offset (and offset (xwapp--timing-round (car offset)))
              :rtt (and offset (xwapp--timing-round (cdr offset)))
              :clock (and clock (if (nth 2 clock) "kept" "own"))
              :gc (and gcs (xwapp--timing-round (- (apply #'max gcs) (apply #'min gcs))))
              :handlers (and req (xwapp--timing-handlers app (plist-get req :page)
                                                         (+ base skew -1) (+ base skew d 1)))
              :omit (let ((o (alist-get 'omit intent)))
                      (+ (if (natnump o) o 0) (max 0 (- (length raw) (length page-reqs)))))
              :reqs (mapcar #'xwapp--timing-req-out reqs))
         (xwapp--timing-path-of (and req (plist-get req :fwd)) (+ base skew) d))))))

(defun xwapp--timing-path-of (addr start d)
  "What a moment of D seconds from START (Emacs's clock) knows of the
path to ADDR, its device (\"Kicking the path\"): :path0 at its start,
:path1 at its end, as last seen before each, and :kick, the first kick
it fell in or followed closely -- :at seconds from the moment's start,
:first its first pong's path, :direct and :end seconds from its own
start, :how.  Nothing when ADDR is none; never ADDR itself."
  (when (xwapp--tailnet-p addr)
    (let* ((kick (car (last (seq-filter (lambda (k) (<= (- start xwapp--kick-every) (plist-get k :at) (+ start d)))
                                        (xwapp--peer-get addr :kicks)))))
           (at (and kick (plist-get kick :at)))
           (round #'xwapp--timing-round))
      (append
       (let ((p (xwapp--peer-path-at addr (+ start 0.5)))) (and p (list :path0 p)))
       (let ((p (xwapp--peer-path-at addr (+ start d 0.5)))) (and p (list :path1 p)))
       (when at
         (list :kick (list :at (funcall round (- at start))
                           :first (car (plist-get kick :first))
                           :direct (funcall round (plist-get kick :direct))
                           :end (funcall round (plist-get kick :end))
                           :how (symbol-name (or (plist-get kick :how) 'running)))))))))

(defun xwapp--timing-req-out (r)
  "Request R as a record keeps it: numbers and its path alone."
  (let ((e (plist-get r :e))
        (ph (plist-get r :ph))
        (round #'xwapp--timing-round))
    (append (list :p (plist-get r :p))
            (when (plist-get r :n) (list :n (plist-get r :n)))
            (list :s (funcall round (plist-get r :s)))
            (cl-loop for k in '(:conn :net :busy :prep :emacs :dl :send)
                     for v = (plist-get ph k)
                     when (and v (> v 0)) append (list k (funcall round v)))
            (cl-loop for k in '(:dns :tcp :tls)
                     for v = (plist-get r k)
                     when (and v (> v 0)) append (list k (funcall round v)))
            (when (plist-get r :b) (list :b (plist-get r :b)))
            (when (plist-get r :z) (list :z t))
            ;; Served by Emacs, it was not the browser's own copy.
            (when (and (plist-get r :c) (not e)) (list :c t))
            (when e
              (append (list :code (plist-get e :code))
                      (when (plist-get e :op) (list :op (plist-get e :op)))
                      (when (plist-get e :aside) (list :aside t))
                      (when (plist-get e :th)
                        (list :late (funcall round (- (plist-get e :th) (or (plist-get e :t2) (plist-get e :t1))))
                              :work (funcall round (- (plist-get e :tw) (plist-get e :th)))))))
            (unless e (list :unmatched t)))))

;; No secret kept

(defun xwapp--timing-clean (x secrets)
  "X, a record or a part of one, with no string that holds one of
SECRETS, plain or %-escaped, or any 32 hex digits: those made \"…\"."
  (cond
   ((stringp x)
    (let* ((plain (xwapp--unhex32 x))
           (seen (downcase (or (ignore-errors (url-unhex-string plain)) plain))))
      (if (or (string-match-p "[0-9a-f]\\{32\\}" seen)
              (seq-some (lambda (s) (string-search (downcase s) seen)) secrets))
          "…"
        plain)))
   ((consp x) (cons (xwapp--timing-clean (car x) secrets)
                    (xwapp--timing-clean (cdr x) secrets)))
   (t x)))

(defun xwapp--timing-safe (record)
  "RECORD cleaned of every secret xwapp keeps (`xwapp--timing-clean'),
or nil if, cleaned, it still held one -- which it cannot."
  (let* ((secrets (xwapp--secrets))
         (clean (xwapp--timing-clean record secrets)))
    (unless (let ((print-length nil) (print-level nil))
              (xwapp--holds-secret-p (prin1-to-string clean)))
      clean)))

;; The device's path

(defconst xwapp--tailscale-fresh 30
  "Seconds what tailscale said is good for: asked once in that time.")

(defvar xwapp--tailscale-said nil
  "(WHEN . STATUS) of tailscale's last answer: STATUS its parsed JSON, or nil.")

(defvar xwapp--tailscale-waiting nil
  "Functions waiting for the tailscale look under way, if there is one.")

(defun xwapp--tailscale-run (then)
  "Ask tailscale its status; call THEN with it parsed, or nil.  A
process of its own, Emacs not waiting for it: 5 s at most."
  (if (not xwapp-tailscale-program)
      (funcall then nil)
    (xwapp-run "xwapp-tailscale" (list xwapp-tailscale-program "status" "--json") 5
                (lambda (out)
                  (funcall then
                           (and out
                                (ignore-errors
                                  (json-parse-string (decode-coding-string out 'utf-8)
                                                     :object-type 'alist :array-type 'list
                                                     :null-object nil :false-object nil))))))))

(defun xwapp--tailscale-status (then)
  "Call THEN with tailscale's status: what it last said, within
`xwapp--tailscale-fresh' seconds, or what it says now."
  (if (and xwapp--tailscale-said
           (< (- (float-time) (car xwapp--tailscale-said)) xwapp--tailscale-fresh))
      (funcall then (cdr xwapp--tailscale-said))
    (push then xwapp--tailscale-waiting)
    (when (null (cdr xwapp--tailscale-waiting))
      (xwapp--tailscale-run
       (lambda (status)
         (setq xwapp--tailscale-said (cons (float-time) status))
         (let ((waiting (nreverse xwapp--tailscale-waiting)))
           (setq xwapp--tailscale-waiting nil)
           (dolist (fn waiting)
             (xwapp--done (lambda () (funcall fn status))))))))))

(defun xwapp--tailscale-private-p (addr)
  "Non-nil when ADDR, \"IP:PORT\", is on a private network: a LAN."
  (let ((ip (replace-regexp-in-string ":[0-9]+\\'" "" (or addr ""))))
    (string-match-p (rx bos (or "10." "192.168." "169.254." "[fe80:" "[fd" "[fc"
                                (seq "172." (or (seq "1" (any "6-9")) (seq "2" digit) (seq "3" (any "01"))) ".")))
                    ip)))

(defun xwapp--tailscale-peer (status from)
  "The peer of STATUS a request FROM, a tailnet address, came from -- or
the one active peer when FROM is not known -- as (:name :os :path
:relay :lan): :path \"direct\", \"peer-relay\" or \"relay\", through
:relay's DERP; :lan non-nil when straight on a local network.  Nil when
none is."
  (let* ((peers (mapcar #'cdr (alist-get 'Peer status)))
         (peer (if from
                   (seq-find (lambda (p) (member from (alist-get 'TailscaleIPs p))) peers)
                 (let ((active (seq-filter (lambda (p) (alist-get 'Active p)) peers)))
                   (and (= (length active) 1) (car active))))))
    (when peer
      (let ((addr (alist-get 'CurAddr peer))
            (relay (alist-get 'Relay peer))
            (peer-relay (alist-get 'PeerRelay peer))
            (dns (alist-get 'DNSName peer)))
        (list :name (let ((n (or (and (stringp dns) (car (split-string dns "\\." t)))
                                 (alist-get 'HostName peer))))
                      (and (stringp n) (xwapp--timing-word (downcase n))))
              :os (xwapp--timing-word (downcase (format "%s" (or (alist-get 'OS peer) ""))))
              :path (cond ((and (stringp addr) (not (string-empty-p addr))) "direct")
                          ((and (stringp peer-relay) (not (string-empty-p peer-relay))) "peer-relay")
                          ((and (stringp relay) (not (string-empty-p relay))) "relay")
                          (t "unknown"))
              :relay (and (stringp relay) (xwapp--timing-word relay))
              :lan (and (stringp addr) (xwapp--tailscale-private-p addr) t))))))

;; Kicking the path
;;
;; A phone that wakes reaches this machine through tailscale's relay --
;; its DERP server, Hong Kong from here, 10-20 KB a second -- until
;; tailscale finds it a direct path, and tailscale was slow to look: 33 s
;; after one wake, nearly 2 minutes after another, and then only once
;; `tailscale ping' was run, which got it direct in 7 s (tailscaled's
;; log, 2026-11-05).  So the page's first request after an open or a wake
;; -- its device unheard for `xwapp--kick-gone' seconds, its hello, its
;; `next' asked with wake=1 -- has Emacs run `tailscale ping' to the
;; device until its path is direct: in the background, Emacs waiting not;
;; `xwapp--kick-for' seconds at most; one at a time, and once in
;; `xwapp--kick-every' seconds, a device.  Its pongs say the path as they
;; come (\"via DERP(hkg)\", or an address: direct).
;;
;; While one is under way, not direct yet, the page's calls that can wait
;; may wait for it (\"Calls that can wait\"), `xwapp-tailscale-hold'
;; seconds at most -- off, as measured: on cellular direct was no faster
;; than the relay (9 KB/s against 10-20), and at home, where direct is the
;; LAN (19 ms against the relay's 1.5 s), what is under way when the path
;; turns goes on over it.  What the page is waiting for -- its files, a
;; tap's answer, a room's newest -- would never wait.
;;
;; Each device's kicks and the paths seen, kept here a while, go with a
;; timing of a moment they fell in: the path at its start and at its end,
;; when kicked and how soon direct.  No address is kept with them.

(defun xwapp--tailnet-p (addr)
  "Non-nil when ADDR is a tailnet address -- 100.64.0.0/10, or
fd7a:115c:a1e0::/48 -- and nothing else: it goes on a command line."
  (and (stringp addr)
       (let ((case-fold-search nil))
         (or (and (string-match "\\`100\\.\\([0-9]\\{1,3\\}\\)\\.[0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}\\'" addr)
                  (<= 64 (string-to-number (match-string 1 addr)) 127))
             (string-match-p "\\`fd7a:115c:a1e0:[0-9a-f:]*\\'" addr)))
       t))

(defun xwapp--peer-get (addr key)
  (plist-get (gethash addr xwapp--peers) key))

(defun xwapp--peer-put (addr key value)
  (puthash addr (plist-put (gethash addr xwapp--peers) key value) xwapp--peers)
  value)

(defun xwapp--peer-saw-path (addr path &optional at)
  "Note ADDR's path was PATH AT (now): kept, the 20 latest."
  (when (and (xwapp--tailnet-p addr) (stringp path))
    (xwapp--peer-put addr :paths (seq-take (cons (cons (or at (float-time)) path)
                                                 (xwapp--peer-get addr :paths))
                                           20))))

(defun xwapp--peer-path-at (addr time)
  "ADDR's path at TIME, as last seen before it -- within a minute -- or nil."
  (cdr (seq-find (lambda (p) (and (<= (car p) time) (< (- time (car p)) 60)))
                 (and addr (xwapp--peer-get addr :paths)))))

(defun xwapp--peer-came (app addr wakes)
  "A request of APP's page through the proxy, from ADDR: the page's
device now.  The first after `xwapp--kick-gone' seconds unheard, or
WAKES -- a hello, a `next' asked with wake=1 -- kicks its path."
  (when (xwapp--tailnet-p addr)
    (xwapp--put app :peer addr)
    (let ((seen (xwapp--peer-get addr :seen))
          (now (float-time)))
      (xwapp--peer-put addr :seen now)
      (when (or wakes (not seen) (> (- now seen) xwapp--kick-gone))
        (xwapp--kick addr)))))

(defconst xwapp--pong
  "\\`pong from .* via \\(DERP(\\([^)]*\\))\\|peer-relay\\S-*\\|\\S-+\\) in [0-9.]+m?s"
  "A line of `tailscale ping' that came back, and its path.")

(defun xwapp--kick (addr)
  "Ping ADDR, a tailnet address, until its path is direct: in the
background, once at a time, not again within `xwapp--kick-every'
seconds, `xwapp--kick-for' at most.  Nothing without tailscale."
  (let ((last (xwapp--peer-get addr :kick))
        (now (float-time)))
    (when (and xwapp-tailscale-program
               (not (process-live-p (xwapp--peer-get addr :proc)))
               (not (and last (< (- now (plist-get last :at)) xwapp--kick-every))))
      (let* ((kick (list :at now))
             (left "")
             (proc (condition-case nil
                       (make-process
                        :name "xwapp-tailscale-ping" :noquery t :connection-type 'pipe
                        :coding 'utf-8
                        :command (list xwapp-tailscale-program "ping" "--until-direct"
                                       "--c" "15" "--timeout" "2s" addr)
                        :filter (lambda (_p s) (setq left (xwapp--kick-heard addr kick (concat left s))))
                        :sentinel (lambda (p _e)
                                    (unless (process-live-p p) (xwapp--kick-over addr kick p))))
                     (error nil))))
        (when proc
          (process-put proc 'xwapp-timer
                       (run-at-time xwapp--kick-for nil
                                    (lambda () (when (process-live-p proc) (delete-process proc)))))
          (xwapp--peer-put addr :kick kick)
          (xwapp--peer-put addr :kicks (seq-take (cons kick (xwapp--peer-get addr :kicks)) 5))
          (xwapp--peer-put addr :proc proc)
          (xwapp--hold-for addr))))))

(defun xwapp--kick-heard (addr kick text)
  "TEXT, what ADDR's `tailscale ping' said so far: each whole line a
pong noted in KICK -- the first as ADDR's path when it began, one direct
as its path from then.  What is left: a line not whole yet."
  (let ((start 0))
    (while (string-match "\n" text start)
      (let ((line (substring text start (match-beginning 0))))
        (setq start (match-end 0))
        (when (string-match xwapp--pong line)
          (let ((path (cond ((match-beginning 2)
                             (concat "relay " (or (xwapp--timing-word (downcase (match-string 2 line))) "?")))
                            ((string-prefix-p "peer-relay" (match-string 1 line)) "peer-relay")
                            (t "direct")))
                (took (- (float-time) (plist-get kick :at))))
            (unless (plist-get kick :first)
              (plist-put kick :first (cons path took))
              (xwapp--peer-saw-path addr path (plist-get kick :at)))
            (when (and (equal path "direct") (not (plist-get kick :direct)))
              (plist-put kick :direct took)
              (xwapp--peer-saw-path addr "direct")
              (xwapp--unhold-for addr))))))
    (substring text start)))

(defun xwapp--kick-over (addr kick proc)
  "ADDR's kick KICK, its `tailscale ping' PROC, is over: how it went is
noted, what tailscale said of paths before is old, and what waited for
it goes."
  (when-let* ((timer (process-get proc 'xwapp-timer))) (cancel-timer timer))
  (plist-put kick :end (- (float-time) (plist-get kick :at)))
  (plist-put kick :how (cond ((plist-get kick :direct) 'direct)
                             ((plist-get kick :first) 'relay)
                             (t 'none)))
  (when (eq (plist-get kick :how) 'relay)
    (xwapp--peer-saw-path addr (car (plist-get kick :first))))
  (when (eq (xwapp--peer-get addr :proc) proc)
    (xwapp--peer-put addr :proc nil))
  (setq xwapp--tailscale-said nil)
  (xwapp--unhold-for addr))

(defun xwapp--holding-p (app)
  "Non-nil while APP's calls that can wait wait for its device's path:
a kick under way, not direct yet, begun within `xwapp-tailscale-hold'."
  (when-let* ((hold xwapp-tailscale-hold)
              (addr (xwapp--get app :peer))
              (kick (xwapp--peer-get addr :kick))
              ((process-live-p (xwapp--peer-get addr :proc))))
    (and (not (plist-get kick :direct))
         (< (- (float-time) (plist-get kick :at)) hold))))

(defun xwapp--hold-for (addr)
  "A kick of ADDR has begun: the apps whose page is there are told when
their calls that can wait may go, at the latest."
  (when xwapp-tailscale-hold
    (maphash (lambda (app _)
               (when (equal (xwapp--get app :peer) addr)
                 (xwapp--timer app :unhold xwapp-tailscale-hold #'xwapp--unhold)))
             xwapp--served)))

(defun xwapp--unhold-for (addr)
  (maphash (lambda (app _)
             (when (equal (xwapp--get app :peer) addr) (xwapp--unhold app)))
           xwapp--served))

(defun xwapp--unhold (app)
  "APP's calls that can wait may go: to its `next', if one waits."
  (xwapp--untimer app :unhold)
  (when (and (xwapp--get app :waiting) (xwapp--get app :later) (not (xwapp--holding-p app))
             (not (xwapp--get app :flush)))
    (xwapp--timer app :flush 0 #'xwapp--flush)))

;; Taking one, keeping it

(defun xwapp--timing-take (app intent req)
  "Take APP's page's timings INTENT, which came as REQ: a record of
them, kept -- a slow one from another device once tailscale has said how
it reaches it."
  (xwapp--timings-load)
  (when-let* ((record (xwapp--timing-record app intent req))
              ;; Sent again, when its first sending was cut after Emacs
              ;; had it: kept once.
              ((not (seq-find (lambda (r) (and (equal (plist-get r :app) (plist-get record :app))
                                               (equal (plist-get r :kind) (plist-get record :kind))
                                               (equal (plist-get r :at) (plist-get record :at))))
                              xwapp--timings))))
    (if (and (plist-get record :slow) (eq (plist-get record :via) 'proxy) xwapp-tailscale-program)
        (let ((from (and req (plist-get req :fwd))))
          (xwapp--tailscale-status
           (lambda (status)
             (let ((peer (and status (xwapp--tailscale-peer status from))))
               (xwapp--peer-saw-path from (xwapp--timing-path peer))
               (xwapp--timing-keep app (append record
                                               (and peer (not (plist-get record :path1))
                                                    (list :path1 (xwapp--timing-path peer)))
                                               (list :peer peer)))))))
      (xwapp--timing-keep app record))))

(defun xwapp--timing-keep (app record)
  "Keep RECORD of APP's page, its secrets taken out; tell
`xwapp-timing-functions'."
  (when-let* ((record (xwapp--timing-safe record)))
    (xwapp--timings-load)
    (push record xwapp--timings)
    (let ((slow 0) (other 0))
      (setq xwapp--timings
            (seq-filter (lambda (r) (if (plist-get r :slow)
                                        (<= (setq slow (1+ slow)) xwapp--timing-keep-slow)
                                      (<= (setq other (1+ other)) xwapp--timing-keep-normal)))
                        xwapp--timings)))
    (xwapp--timings-save)
    (run-hook-wrapped 'xwapp-timing-functions
                      (lambda (fn a r)
                        (with-demoted-errors "xwapp: timing listener: %S"
                          (funcall fn a r))
                        nil)
                      app record)
    record))

(defun xwapp--timings-load ()
  "Read `xwapp-timing-file' into `xwapp--timings', once."
  (unless xwapp--timings-loaded
    (setq xwapp--timings-loaded t)
    (when-let* (((and xwapp--timing-save xwapp-timing-file (file-readable-p xwapp-timing-file)))
                (kept (ignore-errors
                        (with-temp-buffer
                          (insert-file-contents xwapp-timing-file)
                          (read (current-buffer))))))
      (when (proper-list-p kept)
        (setq xwapp--timings (append xwapp--timings
                                     (seq-filter (lambda (r) (and (plistp r) (plist-get r :kind))) kept))
              xwapp--timing-id (apply #'max xwapp--timing-id
                                      (mapcar (lambda (r) (let ((id (plist-get r :id))) (if (natnump id) id 0)))
                                              xwapp--timings)))))))

(defun xwapp--timings-save ()
  "Write `xwapp--timings' to `xwapp-timing-file', yours alone."
  (when (and xwapp--timing-save xwapp-timing-file)
    (condition-case err
        (let ((dir (file-name-directory (expand-file-name xwapp-timing-file))))
          (unless (file-directory-p dir)
            (make-directory dir t)
            (set-file-modes dir #o700))
          (with-file-modes #o600
            (let ((print-length nil) (print-level nil) (print-escape-newlines t)
                  (coding-system-for-write 'utf-8))
              (write-region (concat (prin1-to-string xwapp--timings) "\n") nil
                            xwapp-timing-file nil 'silent))))
      (error (message "xwapp: %s" (error-message-string err))))))

;; Saying one

(defun xwapp--timing-path (peer)
  "PEER's path, as `xwapp--tailscale-peer' says it, in words: \"direct\",
\"relay hkg\", \"peer-relay\" -- or nil."
  (pcase (plist-get peer :path)
    ("relay" (concat "relay " (or (plist-get peer :relay) "?")))
    ((and p (pred stringp) (guard (not (equal p "unknown")))) p)))

(defun xwapp--timing-kick-said (kick)
  "KICK, a record's, in words: \"kicked: direct in 6.8 s\"."
  (pcase (plist-get kick :how)
    ("direct" (format "kicked: direct in %.1f s" (or (plist-get kick :direct) 0)))
    ("relay" (format "kicked: no direct path in %.0f s" (or (plist-get kick :end) 0)))
    ("none" "kicked: no answer")
    (_ "kicked: under way")))

(defun xwapp-timing-summary (record)
  "RECORD in a line: what it was, how long, and what that went on --
\"open 3.4 s: network 2.1 s · Emacs busy 0.6 s · page 0.5 s · relay hkg\"."
  (let* ((parts (seq-filter (lambda (c) (>= (cdr c) 0.05))
                            (sort (copy-sequence (plist-get record :sum))
                                  (lambda (a b) (> (cdr a) (cdr b))))))
         (peer (plist-get record :peer))
         (p0 (plist-get record :path0))
         (p1 (or (plist-get record :path1) (xwapp--timing-path peer)))
         (kick (plist-get record :kick)))
    (concat (format "%s %.1f s" (plist-get record :kind) (plist-get record :d))
            (if parts
                (concat ": " (mapconcat (lambda (c) (format "%s %.1f s" (alist-get (car c) xwapp--timing-names)
                                                           (cdr c)))
                                        parts " · "))
              "")
            ;; The path at its start, when it was another at its end.
            (if (and p0 p1 (not (equal p0 p1)))
                (format " · %s → %s%s" p0 p1
                        (if (and (equal p1 "direct") (plist-get peer :lan)) ", LAN" ""))
              (pcase (plist-get peer :path)
                ("relay" (format " · relay %s" (or (plist-get peer :relay) "?")))
                ("peer-relay" " · peer relay")
                ("direct" (if (plist-get peer :lan) " · direct, LAN" " · direct"))
                (_ (if p1 (concat " · " p1) ""))))
            (if kick (concat " · " (xwapp--timing-kick-said kick)) ""))))

(defun xwapp-timing-summary-phases (line)
  "What LINE, as `xwapp-timing-summary' writes one, says its moment
waited on: ((KEY NAME SECONDS) ...), in its order -- KEY one of
`xwapp--timing-names'.  Read here, beside what writes it."
  (let ((re (concat "\\(" (regexp-opt (mapcar #'cdr xwapp--timing-names))
                    "\\) \\([0-9]+\\(?:\\.[0-9]+\\)?\\) s\\_>"))
        (case-fold-search nil)
        (start 0)
        out)
    (while (string-match re line start)
      (push (list (car (rassoc (match-string 1 line) xwapp--timing-names))
                  (match-string 1 line)
                  (string-to-number (match-string 2 line)))
            out)
      (setq start (match-end 0)))
    (nreverse out)))

(defun xwapp--timing-short (p)
  "Request path P, short: its file's name, or the kit's call."
  (if (string-match-p "/" p) (file-name-nondirectory p) p))

(defun xwapp--timing-lines (head items)
  "HEAD then ITEMS, joined by \" · \", in lines of 40 columns at most."
  (let ((lines nil) (line head) (sep (if (string-empty-p head) "" " ")))
    (dolist (item items)
      (let ((more (concat line sep item)))
        (setq sep " · ")
        (if (and (> (string-width more) 40) (not (string-empty-p line)))
            (setq lines (cons line lines) line (concat "  " item))
          (setq line more))))
    (string-join (nreverse (cons line lines)) "\n")))

(defun xwapp-timing-describe (record)
  "RECORD, its phases, in lines of 44 columns at most: a phone's."
  (let* ((s (lambda (x) (if (numberp x) (format "%.2f" x) "-")))
         (peer (plist-get record :peer))
         (marks (plist-get record :marks))
         (handlers (plist-get record :handlers)))
    (string-join
     (delq nil
           (append
            (list
             (concat (format "%s %.2f s%s" (plist-get record :kind) (plist-get record :d)
                             (if (plist-get record :slow) ", slow" ""))
                     (if (plist-get record :away) (format ", away %.0f s" (plist-get record :away)) "")
                     (if peer (format " · %s %s%s" (or (plist-get peer :name) "?") (plist-get peer :path)
                                      (if (equal (plist-get peer :path) "relay")
                                          (concat " " (or (plist-get peer :relay) "?")) ""))
                       ""))
             (let ((p0 (plist-get record :path0))
                   (p1 (plist-get record :path1))
                   (kick (plist-get record :kick)))
               (and (or p0 p1 kick)
                    (xwapp--timing-lines
                     "path:" (delq nil
                                   (list (and (or p0 p1) (format "%s → %s" (or p0 "?") (or p1 "?")))
                                         (and kick (format "%s, %s s in"
                                                           (xwapp--timing-kick-said kick)
                                                           (funcall s (plist-get kick :at)))))))))
             (xwapp--timing-lines
              "" (mapcar (lambda (c) (format "%s %s" (alist-get (car c) xwapp--timing-names) (funcall s (cdr c))))
                         (seq-filter (lambda (c) (>= (cdr c) 0.005)) (plist-get record :sum))))
             (and marks (xwapp--timing-lines
                         "at:" (mapcar (lambda (m) (format "%s %s" (car m) (funcall s (cdr m)))) marks)))
             (xwapp--timing-lines
              "" (delq nil
                       (list (format "ended by %s" (or (plist-get record :end) "?"))
                             (and (plist-get record :rtt) (format "round trip %s" (funcall s (plist-get record :rtt))))
                             (and (plist-get record :offset)
                                  (format "clock %+.3f%s" (plist-get record :offset)
                                          (if (equal (plist-get record :clock) "kept") " (kept)" "")))
                             (and (plist-get record :gc) (> (plist-get record :gc) 0)
                                  (format "GC %s" (funcall s (plist-get record :gc)))))))
             (and handlers (xwapp--timing-lines
                            "Emacs handlers:" (mapcar (lambda (h) (format "%s %s" (car h) (funcall s (cdr h))))
                                                      handlers)))
             "start  net busy prep emacs  dl   KB")
            (mapcar
             (lambda (r)
               (format "%5s %4s %4s %4s %5s %4s %4s %s%s"
                       (funcall s (plist-get r :s))
                       (funcall s (+ (or (plist-get r :conn) 0) (or (plist-get r :net) 0)))
                       (funcall s (or (plist-get r :busy) 0))
                       (funcall s (or (plist-get r :prep) 0))
                       (funcall s (or (plist-get r :emacs) 0))
                       (funcall s (or (plist-get r :dl) 0))
                       (if (plist-get r :b) (format "%.0f" (/ (plist-get r :b) 1024.0)) "")
                       (concat (xwapp--timing-short (plist-get r :p))
                               (if (plist-get r :n) (format "#%d" (plist-get r :n)) "")
                               (if (plist-get r :op) (concat " " (plist-get r :op)) ""))
                       (concat (if (plist-get r :c) " cached" "")
                               (if (plist-get r :unmatched) " ?" ""))))
             (plist-get record :reqs))
            (list (and (natnump (plist-get record :omit)) (> (plist-get record :omit) 0)
                       (format "%d more not slow" (plist-get record :omit))))))
     "\n")))

;;;###autoload
(defun xwapp-timings ()
  "Show the pages' timing records, the latest first: where each slow
load or wake of a page in a browser spent its time."
  (interactive)
  (xwapp--timings-load)
  (with-current-buffer (get-buffer-create "*xwapp timings*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (if xwapp--timings
                  (mapconcat (lambda (r)
                               (concat (format-time-string "%F %T  " (plist-get r :at))
                                       (plist-get r :app) "\n"
                                       (xwapp-timing-describe r) "\n"))
                             xwapp--timings "\n")
                "No timings yet: a page in a browser sends them once loaded, or woken.\n"))
      (goto-char (point-min))
      (special-mode))
    (pop-to-buffer (current-buffer))))


;;; Misc

(defun xwapp-copy (text)
  "Put TEXT on the kill ring and the system clipboard."
  (kill-new text)
  (when (fboundp 'gui-set-selection)
    (gui-set-selection 'CLIPBOARD text)
    (ignore-errors (gui-set-selection 'PRIMARY text))))

(provide 'xwapp)
;;; xwapp.el ends here

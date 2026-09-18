;;; ghostherd-usage.el --- Account-level remaining quota -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Remaining quota for claude / grok / agy, shown on the sidebar footer.
;; Account-wide, not per session.  HTTP is async and never on the 1.5s
;; poll timer (that timer must not compete with typing).
;;
;; claude  GET api.anthropic.com/api/oauth/usage
;; grok    last `billing: fetched credits config` in ~/.grok/logs, then
;;         live cli-chat-proxy billing if the token is still fresh
;; agy     `/usage` TUI on a live session, else Cloud Code quota API
;;
;; Tokens are read, never written back, never printed.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'url-util)
(require 'iso8601)

(defgroup ghostherd-usage nil
  "Account remaining-quota readout for ghostherd."
  :group 'ghostherd
  :prefix "ghostherd-usage-")

(defcustom ghostherd-usage-interval 90
  "Seconds between usage refreshes while the sidebar is visible."
  :type 'number
  :group 'ghostherd-usage)

(defvar ghostherd-usage--cache nil
  "Alist of (KIND . PLIST).  PLIST keys: :windows :fetched :error.
Each window is (:label STRING :remaining NUMBER :resets-at NUMBER-or-nil).")

(defvar ghostherd-usage--inflight nil)
(defvar ghostherd-usage--timer nil)
(defvar ghostherd-usage--agy-client nil
  "Cached (CLIENT-ID . CLIENT-SECRET) extracted from the agy binary.")

(defvar ghostherd-usage--agy-probe-at 0
  "When we last submitted `/usage` to an idle agy session.")


;;; Pure helpers

(defun ghostherd-usage--round1 (n)
  (/ (float (round (* n 10.0))) 10.0))

(defun ghostherd-usage--remaining (used)
  "USED percent → remaining percent, or nil."
  (when (numberp used)
    (ghostherd-usage--round1 (max 0.0 (min 100.0 (- 100.0 used))))))

(defun ghostherd-usage--pct-n (n)
  (max 0 (min 100 (truncate (+ n 0.5)))))

(defun ghostherd-usage--face (remaining)
  (cond ((< remaining 8) 'error)
        ((< remaining 20) 'warning)
        (t 'success)))

(defun ghostherd-usage--bar (remaining &optional width)
  "HP bar for REMAINING percent.  No `%' (mode-line would eat it)."
  (let* ((width (or width 6))
         (n (max 0 (min width
                        (truncate (+ (/ (* remaining width) 100.0) 0.5)))))
         (s (concat (make-string n ?█) (make-string (- width n) ?░))))
    (propertize s 'face (ghostherd-usage--face remaining))))

(defun ghostherd-usage--parse-time (s)
  (cond
   ((null s) nil)
   ((numberp s) (float s))
   ((and (stringp s) (not (string-empty-p s)))
    (ignore-errors
      (float-time (encode-time (iso8601-parse s)))))
   (t nil)))

(defun ghostherd-usage--window (label remaining &optional resets)
  (and (numberp remaining)
       (list :label label :remaining remaining :resets-at resets)))

(defun ghostherd-usage--alist (obj key)
  (or (alist-get key obj)
      (and (stringp key) (alist-get (intern key) obj))))

(defun ghostherd-usage--json (s)
  (json-parse-string s
                     :object-type 'alist
                     :array-type 'list
                     :null-object nil
                     :false-object nil))


;;; Parsers (no I/O)

(defun ghostherd-usage--parse-grok-config (cfg)
  "Parse grok billing `config' object.  creditUsagePercent is *used*."
  (let* ((used (ghostherd-usage--alist cfg 'creditUsagePercent))
         (period (ghostherd-usage--alist cfg 'currentPeriod))
         (type (format "%s" (or (ghostherd-usage--alist period 'type) "")))
         (label (if (string-match-p "WEEKLY" type) "wk" "mo"))
         (end (ghostherd-usage--parse-time
               (or (ghostherd-usage--alist period 'end)
                   (ghostherd-usage--alist cfg 'billingPeriodEnd))))
         (rem (ghostherd-usage--remaining used)))
    (and rem (list (ghostherd-usage--window label rem end)))))

(defun ghostherd-usage--parse-claude (raw)
  (delq nil
        (list
         (let ((w (ghostherd-usage--alist raw 'five_hour)))
           (ghostherd-usage--window
            "5h"
            (ghostherd-usage--remaining (ghostherd-usage--alist w 'utilization))
            (ghostherd-usage--parse-time (ghostherd-usage--alist w 'resets_at))))
         (let ((w (ghostherd-usage--alist raw 'seven_day)))
           (ghostherd-usage--window
            "7d"
            (ghostherd-usage--remaining (ghostherd-usage--alist w 'utilization))
            (ghostherd-usage--parse-time (ghostherd-usage--alist w 'resets_at)))))))

(defun ghostherd-usage--parse-agy-groups (raw)
  "Parse retrieveUserQuotaSummary.  remainingFraction is remaining 0–1."
  (let* ((groups (or (ghostherd-usage--alist raw 'groups)
                     (ghostherd-usage--alist
                      (ghostherd-usage--alist raw 'response) 'groups)))
         (out nil))
    (dolist (g groups)
      (let ((gname (or (ghostherd-usage--alist g 'displayName) "")))
        (dolist (b (or (ghostherd-usage--alist g 'buckets) '()))
          (let* ((frac (ghostherd-usage--alist b 'remainingFraction))
                 (win (format "%s" (or (ghostherd-usage--alist b 'window)
                                       (ghostherd-usage--alist b 'bucketId)
                                       "")))
                 (label
                  (cond
                   ((string-match-p "weekly\\|WEEKLY" win) "wk")
                   ((string-match-p "5h\\|five" win) "5h")
                   ((string-match-p "Gemini" gname) "gem")
                   (t "lim")))
                 (rem (and (numberp frac)
                           (ghostherd-usage--round1 (* 100.0 frac)))))
            (when rem
              (push (ghostherd-usage--window
                     label rem
                     (ghostherd-usage--parse-time
                      (ghostherd-usage--alist b 'resetTime)))
                    out))))))
    (nreverse out)))

(defun ghostherd-usage--parse-agy-usage-screen (text)
  "Parse the `/usage` TUI.  Printed percents are remaining, not used."
  (when (and text (string-match-p "Limit Remaining\\|Quota available" text))
    (let ((vals nil)
          (pos 0)
          (labels '("gem-wk" "gem-5h" "3p-wk" "3p-5h")))
      (while (string-match
              "\\([0-9]+\\(?:\\.[0-9]+\\)?\\)% remaining\\|Quota available"
              text pos)
        (push (if (match-beginning 1)
                  (string-to-number (match-string 1 text))
                100.0)
              vals)
        (setq pos (match-end 0)))
      (setq vals (nreverse vals))
      (cl-mapcar #'ghostherd-usage--window
                 labels
                 vals))))

(defun ghostherd-usage--put (kind windows &optional err)
  (setq ghostherd-usage--cache
        (cons (cons kind
                    (list :windows windows
                          :fetched (float-time)
                          :error err))
              (assq-delete-all kind ghostherd-usage--cache))))


;;; Format
;;
;; Mode-line treats `%' as a construct (`78% 5h' became `785h').  Bars
;; plus a bare integer avoid that; `ghostherd-usage--mode-line-safe'
;; still doubles any leftover percent signs.

(defun ghostherd-usage--mode-line-safe (s)
  (replace-regexp-in-string "%" "%%" (or s "")))

(defun ghostherd-usage--help (kind wins)
  (mapconcat
   (lambda (w)
     (format "%s %s left"
             (plist-get w :label)
             (format "%d%%" (ghostherd-usage--pct-n (plist-get w :remaining)))))
   wins
   "  "))

(defun ghostherd-usage--window-bar (w compact)
  (let* ((n (ghostherd-usage--pct-n (plist-get w :remaining)))
         (bar (ghostherd-usage--bar (plist-get w :remaining) (if compact 4 6)))
         (s (if compact
                (format "%s%d" bar n)
              (format "%s%d %s" bar n (plist-get w :label)))))
    (propertize s 'face (ghostherd-usage--face (plist-get w :remaining)))))

(defun ghostherd-usage--tightest (wins)
  (car (seq-sort-by (lambda (w) (plist-get w :remaining)) #'< wins)))

(defun ghostherd-usage--entry-string (kind compact)
  (let* ((pl (alist-get kind ghostherd-usage--cache))
         (name (symbol-name kind))
         (wins (and pl (plist-get pl :windows)))
         (err (and pl (plist-get pl :error))))
    (cond
     ((null pl) (propertize (format "%s —" name) 'face 'shadow))
     ((and err (not wins))
      (propertize (format "%s ?" name) 'face 'shadow
                  'help-echo err))
     (t
      (let* ((show (if (or compact (> (length wins) 2))
                       (list (ghostherd-usage--tightest wins))
                     wins))
             (body (mapconcat (lambda (w) (ghostherd-usage--window-bar w compact))
                              show " "))
             (s (concat name " " body)))
        (propertize s 'help-echo (ghostherd-usage--help kind wins)))))))

(defun ghostherd-usage-line (&optional width)
  "Footer fragment: remaining quota per kind as HP bars."
  (when ghostherd-usage--cache
    (let* ((width (or width 80))
           (wide (mapconcat (lambda (k) (ghostherd-usage--entry-string k nil))
                            '(claude grok agy) "  "))
           (narrow (mapconcat (lambda (k) (ghostherd-usage--entry-string k t))
                              '(claude grok agy) " "))
           (s (if (< (length (substring-no-properties wide))
                     (max 24 (- width 32)))
                  wide
                narrow)))
      (ghostherd-usage--mode-line-safe s))))


;;; I/O — credentials (never printed)

(defun ghostherd-usage--json-file (path)
  (when (file-readable-p path)
    (ignore-errors
      (ghostherd-usage--json
       (with-temp-buffer
         (insert-file-contents path)
         (buffer-string))))))

(defun ghostherd-usage--keychain-json (service)
  (when (eq system-type 'darwin)
    (with-temp-buffer
      (when (eq 0 (call-process "security" nil t nil
                                "find-generic-password"
                                "-s" service
                                "-a" (or (user-login-name) "")
                                "-w"))
        (ignore-errors (ghostherd-usage--json (string-trim (buffer-string))))))))

(defun ghostherd-usage--claude-oauth ()
  (let* ((file (ghostherd-usage--json-file
                (expand-file-name ".credentials.json"
                                  (expand-file-name ".claude" "~"))))
         (raw (or file
                  (ghostherd-usage--keychain-json "Claude Code-credentials"))))
    (ghostherd-usage--alist raw 'claudeAiOauth)))

(defun ghostherd-usage--grok-auth-entry ()
  (let ((raw (ghostherd-usage--json-file
              (expand-file-name "auth.json" (expand-file-name ".grok" "~")))))
    (cdr (car-safe raw))))

(defun ghostherd-usage--agy-token-blob ()
  (let ((raw (ghostherd-usage--json-file
              (expand-file-name "antigravity-oauth-token"
                                (expand-file-name "antigravity-cli"
                                                  (expand-file-name ".gemini" "~"))))))
    (or (ghostherd-usage--alist raw 'token) raw)))

(defun ghostherd-usage--expired-p (expiry)
  (let ((ts (cond
             ((numberp expiry)
              (if (> expiry 1e12) (/ expiry 1000.0) (float expiry)))
             ((stringp expiry) (ghostherd-usage--parse-time expiry))
             (t nil))))
    (and ts (< ts (+ (float-time) 60)))))


;;; HTTP

(defun ghostherd-usage--http (url headers callback &optional method json-body form)
  "CALLBACK is (lambda (code body)).  BODY is a string.  Never logs headers."
  (let ((url-request-method (or method "GET"))
        (url-request-extra-headers
         (let ((h (copy-alist headers)))
           (when json-body
             (push '("Content-Type" . "application/json") h))
           h))
        (url-request-data
         (cond
          (form (encode-coding-string form 'utf-8))
          (json-body (encode-coding-string (json-encode json-body) 'utf-8))))
        (url-show-status nil)
        (url-mime-accept-string "application/json"))
    ;; 401 must not pop a username prompt (Google/Anthropic).
    (cl-letf (((symbol-function 'url-http-handle-authentication)
               (lambda (&rest _) nil)))
    (url-retrieve
     url
     (lambda (status callback)
       (let ((err (plist-get status :error))
             (buf (current-buffer))
             code body)
         (unwind-protect
             (progn
               (goto-char (point-min))
               (setq code (and (re-search-forward "^HTTP/[^ ]+ \\([0-9]+\\)" nil t)
                               (string-to-number (match-string 1))))
               (goto-char (point-min))
               (when (re-search-forward "\n\n" nil t)
                 (setq body (decode-coding-string
                             (buffer-substring-no-properties (point) (point-max))
                             'utf-8)))
               (funcall callback (or code (and err 0)) (or body "")))
           (when (buffer-live-p buf)
             (kill-buffer buf)))))
     (list callback)
     t t))))

(defun ghostherd-usage--http-json (url headers ok-fn &optional method json-body form)
  (ghostherd-usage--http
   url headers
   (lambda (code body)
     (if (and code (< code 400) body (not (string-empty-p (string-trim body))))
         (condition-case _
             (funcall ok-fn (ghostherd-usage--json body))
           (error (funcall ok-fn nil)))
       (funcall ok-fn nil code)))
   method json-body form))


;;; Fetchers

(defun ghostherd-usage--file-tail (file nbytes)
  (when (file-readable-p file)
    (let* ((size (file-attribute-size (file-attributes file)))
           (start (max 0 (- size nbytes))))
      (with-temp-buffer
        (insert-file-contents file nil start size)
        (buffer-string)))))

(defun ghostherd-usage--grok-from-log ()
  "Fill grok cache from the last billing line in unified.jsonl."
  (let* ((file (expand-file-name "logs/unified.jsonl"
                                 (expand-file-name ".grok" "~")))
         (tail (ghostherd-usage--file-tail file (* 256 1024)))
         (hit nil))
    (when tail
      (dolist (line (nreverse (split-string tail "\n" t)))
        (when (and (not hit) (string-match-p "fetched credits config" line))
          (setq hit (ignore-errors (ghostherd-usage--json line))))))
    (when-let* ((cfg (ghostherd-usage--alist
                      (ghostherd-usage--alist hit 'ctx) 'config))
                (wins (ghostherd-usage--parse-grok-config cfg)))
      (ghostherd-usage--put 'grok wins)
      t)))

(defun ghostherd-usage--done ()
  (when ghostherd-usage--inflight
    (setq ghostherd-usage--inflight (1- ghostherd-usage--inflight))
    (when (<= ghostherd-usage--inflight 0)
      (setq ghostherd-usage--inflight nil)))
  (ghostherd-usage--redraw))

(defun ghostherd-usage--fetch-claude ()
  (let ((oauth (ghostherd-usage--claude-oauth)))
    (if (not oauth)
        (progn (ghostherd-usage--put 'claude nil "no credentials")
               (ghostherd-usage--done))
      (let ((token (ghostherd-usage--alist oauth 'accessToken))
            (refresh (ghostherd-usage--alist oauth 'refreshToken)))
        (cl-labels
            ((apply-usage
              (raw)
              (let ((wins (and raw (ghostherd-usage--parse-claude raw))))
                (ghostherd-usage--put 'claude wins (and (not wins) "empty"))
                (ghostherd-usage--done)))
             (get-usage
              (tok)
              (ghostherd-usage--http-json
               "https://api.anthropic.com/api/oauth/usage"
               `(("Authorization" . ,(concat "Bearer " tok))
                 ("anthropic-beta" . "oauth-2025-04-20")
                 ("User-Agent" . "claude-cli/1.0"))
               (lambda (raw &optional _code)
                 (if raw (apply-usage raw)
                   (ghostherd-usage--put 'claude nil "http")
                   (ghostherd-usage--done))))))
          (if (and (ghostherd-usage--expired-p
                    (ghostherd-usage--alist oauth 'expiresAt))
                   refresh)
              (ghostherd-usage--http-json
               "https://platform.claude.com/v1/oauth/token"
               '(("Content-Type" . "application/json")
                 ("anthropic-beta" . "oauth-2025-04-20"))
               (lambda (raw &optional _code)
                 (let ((tok (or (ghostherd-usage--alist raw 'access_token) token)))
                   (get-usage tok)))
               "POST"
               `((grant_type . "refresh_token")
                 (client_id . "9d1c250a-e61b-44d9-88ed-5944d1962f5e")
                 (refresh_token . ,refresh)))
            (get-usage token)))))))

(defun ghostherd-usage--fetch-grok ()
  (let ((entry (ghostherd-usage--grok-auth-entry)))
    (if (or (not entry)
            (ghostherd-usage--expired-p (ghostherd-usage--alist entry 'expires_at)))
        (progn
          (unless (alist-get 'grok ghostherd-usage--cache)
            (ghostherd-usage--put 'grok nil "expired"))
          (ghostherd-usage--done))
      (let ((tok (ghostherd-usage--alist entry 'key))
            (uid (or (ghostherd-usage--alist entry 'user_id) "")))
        (ghostherd-usage--http-json
         "https://cli-chat-proxy.grok.com/v1/billing?format=credits"
         `(("Authorization" . ,(concat "Bearer " tok))
           ("X-XAI-Token-Auth" . "xai-grok-cli")
           ("x-userid" . ,uid))
         (lambda (raw &optional _code)
           (let* ((cfg (or (ghostherd-usage--alist raw 'config) raw))
                  (wins (and cfg (ghostherd-usage--parse-grok-config cfg))))
             (when wins (ghostherd-usage--put 'grok wins))
             (ghostherd-usage--done))))))))

(defun ghostherd-usage--agy-client ()
  (or ghostherd-usage--agy-client
      (when-let* ((bin (executable-find "agy"))
                  (py (or (executable-find "python3") (executable-find "python"))))
        (with-temp-buffer
          (when (eq 0 (call-process
                       py nil t nil "-c"
                       "import re,sys
p=open(sys.argv[1],'rb').read()
ids=re.findall(rb'(\\d{10,}-[a-z0-9]+\\.apps\\.googleusercontent\\.com)', p)
secs=re.findall(rb'GOCSPX-[A-Za-z0-9_-]{20,}', p)
cid=next((i.decode() for i in ids if i.startswith(b'1071')), ids[0].decode() if ids else '')
sec=secs[0].decode() if secs else ''
print(cid+'\\t'+sec)"
                       bin))
            (let* ((line (string-trim (buffer-string)))
                   (parts (split-string line "\t")))
              (when (and (= (length parts) 2)
                         (not (string-empty-p (nth 0 parts)))
                         (not (string-empty-p (nth 1 parts))))
                (setq ghostherd-usage--agy-client
                      (cons (nth 0 parts) (nth 1 parts))))))))))

(defun ghostherd-usage--agy-refresh (refresh cb)
  (let ((pair (ghostherd-usage--agy-client)))
    (if (not pair)
        (funcall cb nil)
      (ghostherd-usage--http-json
       "https://oauth2.googleapis.com/token"
       '(("Content-Type" . "application/x-www-form-urlencoded"))
       (lambda (raw &optional _code)
         (funcall cb (ghostherd-usage--alist raw 'access_token)))
       "POST" nil
       (concat "grant_type=refresh_token"
               "&refresh_token=" (url-hexify-string refresh)
               "&client_id=" (url-hexify-string (car pair))
               "&client_secret=" (url-hexify-string (cdr pair)))))))

(defun ghostherd-usage--agy-from-screens ()
  "Read a live agy pane if the `/usage` panel is already up."
  (when (and (boundp 'ghostherd--sessions)
             (fboundp 'ghostherd-session-kind))
    (cl-loop for s in (hash-table-values ghostherd--sessions)
             when (eq (ghostherd-session-kind s) 'agy)
             do (let* ((text (or (and (boundp 'ghostherd--screens)
                                      (gethash (ghostherd-session-id s)
                                               ghostherd--screens))
                                 (ignore-errors (ghostherd--host-capture s))))
                       (wins (and text
                                  (ghostherd-usage--parse-agy-usage-screen text))))
                  (when wins (cl-return wins))))))

(defun ghostherd-usage--agy-probe ()
  "Type `/usage` into one idle agy session, then Esc after it paints.
The slash command is how the CLI itself shows remaining quota; the
OAuth API is a fallback and is often expired."
  (when (and (fboundp 'ghostherd--host-send-text)
             (boundp 'ghostherd--sessions)
             (> (- (float-time) ghostherd-usage--agy-probe-at) 60))
    (when-let* ((s (seq-find
                    (lambda (x)
                      (and (eq (ghostherd-session-kind x) 'agy)
                           (memq (ghostherd-session-state x) '(idle done))
                           (ghostherd--session-live-p x)))
                    (hash-table-values ghostherd--sessions))))
      (setq ghostherd-usage--agy-probe-at (float-time))
      (ignore-errors (ghostherd--host-send-text s "/usage" t))
      (run-with-timer
       1.4 nil
       (lambda (id)
         (when-let* ((sess (and (fboundp 'ghostherd-get) (ghostherd-get id)))
                     (text (ignore-errors (ghostherd--host-capture sess)))
                     (wins (ghostherd-usage--parse-agy-usage-screen text)))
           (ghostherd-usage--put 'agy wins)
           (ghostherd-usage--redraw)
           (ignore-errors (ghostherd-send-keys sess "esc"))))
       (ghostherd-session-id s))
      t)))

(defun ghostherd-usage--agy-quota (token)
  (ghostherd-usage--http-json
   "https://daily-cloudcode-pa.googleapis.com/v1internal:retrieveUserQuotaSummary"
   `(("Authorization" . ,(concat "Bearer " token))
     ("User-Agent" . "antigravity/darwin/arm64")
     ("Content-Type" . "application/json"))
   (lambda (raw &optional _code)
     (let ((wins (and raw (ghostherd-usage--parse-agy-groups raw))))
       (cond
        (wins (ghostherd-usage--put 'agy wins))
        ((ghostherd-usage--agy-probe))
        (t (ghostherd-usage--put 'agy nil "empty")))
       (ghostherd-usage--done)))
   "POST" (make-hash-table)))

(defun ghostherd-usage--fetch-agy ()
  (if-let* ((wins (ghostherd-usage--agy-from-screens)))
      (progn (ghostherd-usage--put 'agy wins)
             (ghostherd-usage--done))
    (let ((blob (ghostherd-usage--agy-token-blob)))
      (if (not blob)
          (progn
            (unless (ghostherd-usage--agy-probe)
              (ghostherd-usage--put 'agy nil "no credentials"))
            (ghostherd-usage--done))
        (let ((tok (ghostherd-usage--alist blob 'access_token))
              (refresh (ghostherd-usage--alist blob 'refresh_token))
              (expiry (ghostherd-usage--alist blob 'expiry)))
          (if (and tok (not (ghostherd-usage--expired-p expiry)))
              (ghostherd-usage--agy-quota tok)
            (ghostherd-usage--agy-refresh
             refresh
             (lambda (fresh)
               (if fresh
                   (ghostherd-usage--agy-quota fresh)
                 (unless (ghostherd-usage--agy-probe)
                   (ghostherd-usage--put 'agy nil "expired"))
                 (ghostherd-usage--done))))))))))


;;; Timer / public

(defun ghostherd-usage--redraw ()
  (when-let* ((buf (get-buffer "*ghostherd*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (force-mode-line-update t))
      (when (and (fboundp 'ghostherd--sidebar-posframe-showing-p)
                 (ghostherd--sidebar-posframe-showing-p)
                 (fboundp 'posframe-refresh))
        (posframe-refresh buf)))))

(defun ghostherd-usage--ensure-timer ()
  (unless (or noninteractive (timerp ghostherd-usage--timer))
    (setq ghostherd-usage--timer
          (run-with-timer ghostherd-usage-interval
                          ghostherd-usage-interval
                          #'ghostherd-usage--tick))))

(defun ghostherd-usage--tick ()
  (when (and (fboundp 'ghostherd--sidebar-on-screen-p)
             (ghostherd--sidebar-on-screen-p))
    (ghostherd-usage-refresh)))

;;;###autoload
(defun ghostherd-usage-refresh (&optional force)
  "Refresh cached remaining quota.  Prefix FORCE ignores in-flight."
  (interactive "P")
  (ghostherd-usage--grok-from-log)
  (ghostherd-usage--redraw)
  (when (and (or force (not noninteractive))
             (or force (not ghostherd-usage--inflight)))
    (setq ghostherd-usage--inflight 3)
    (ghostherd-usage--fetch-claude)
    (ghostherd-usage--fetch-grok)
    (ghostherd-usage--fetch-agy)))

(provide 'ghostherd-usage)
;;; ghostherd-usage.el ends here

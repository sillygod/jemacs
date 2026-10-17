;;; ghostherd-usage.el --- Account-level remaining quota -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Account usage for claude / grok / agy, shown on the sidebar footer.
;; Account-wide, not per session.  HTTP is async and never on the 1.5s
;; poll timer (that timer must not compete with typing).
;;
;; Windows are stored as *remaining* percent (that is what agy reports)
;; but rendered as *used*, so the footer reads the same way round as the
;; claude.ai and agy usage panels: a bar that fills as you spend.
;;
;; claude  GET api.anthropic.com/api/oauth/usage
;; grok    last `billing: fetched credits config` in ~/.grok/logs, then
;;         live cli-chat-proxy billing if the token is still fresh
;; agy     Cloud Code quota API with the token agy saved, while it is
;;         fresh; after that the `/usage' panel of an idle agy session
;;
;; agy saves its token once and refreshes it only in memory, so the
;; saved one is good for an hour.  Refreshing it ourselves would take
;; agy's OAuth client secret, which is not in the binary any more:
;; Google refuses both client ids found there with the one secret
;; found there (`invalid_client').  So an expired token is reported as
;; such, and the panel is read instead.
;;
;; Tokens are read, never written back, never printed.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'url-parse)
(require 'url-util)
(require 'iso8601)

(defgroup ghostherd-usage nil
  "Account usage readout for ghostherd."
  :group 'ghostherd
  :prefix "ghostherd-usage-")

(defcustom ghostherd-usage-interval 90
  "Seconds between usage refreshes while the sidebar is visible."
  :type 'number
  :group 'ghostherd-usage)

(defcustom ghostherd-usage-timeout 15
  "Seconds before a usage request is given up on.
A request whose callback never runs used to leave
`ghostherd-usage--inflight' pinned above zero, which gates every
later refresh; the deadline makes that unreachable."
  :type 'number
  :group 'ghostherd-usage)

(defvar ghostherd-usage--cache nil
  "Alist of (KIND . PLIST).  PLIST keys: :windows :fetched :error.
Each window is (:label STRING :remaining NUMBER :resets-at NUMBER-or-nil).")

(defvar ghostherd-usage--inflight nil)
(defvar ghostherd-usage--inflight-at nil
  "`float-time' when the current in-flight batch started.")
(defvar ghostherd-usage--timer nil)

(defvar ghostherd-usage--agy-probe-at 0
  "When we last submitted `/usage` to an idle agy session.")


;;; Pure helpers

(defun ghostherd-usage--round1 (n)
  (/ (float (round (* n 10.0))) 10.0))

(defun ghostherd-usage--remaining (used)
  "USED percent → remaining percent, or nil."
  (when (numberp used)
    (ghostherd-usage--round1 (max 0.0 (min 100.0 (- 100.0 used))))))

(defun ghostherd-usage--used (remaining)
  "REMAINING percent -> used percent."
  (when (numberp remaining)
    (ghostherd-usage--round1 (max 0.0 (min 100.0 (- 100.0 remaining))))))

(defun ghostherd-usage--pct-n (n)
  (max 0 (min 100 (truncate (+ n 0.5)))))

(defun ghostherd-usage--face (remaining)
  (cond ((< remaining 8) 'error)
        ((< remaining 20) 'warning)
        (t 'success)))

(defun ghostherd-usage--bar (remaining &optional width)
  "Bar for a window with REMAINING percent left.
Filled cells are the *used* share, to match the provider panels;
the colour still tracks what is left.  No `%' in the output (the
mode-line would eat it)."
  (let* ((width (or width 6))
         (used (ghostherd-usage--used remaining))
         (n (max 0 (min width
                        (truncate (+ (/ (* used width) 100.0) 0.5)))))
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
  "Parse grok billing `config' object.  creditUsagePercent is *used*.
The billing API leaves out a field that is zero, as protobuf's JSON
does: a period with no creditUsagePercent is one nothing is used of
yet, as every new week starts."
  (let* ((period (ghostherd-usage--alist cfg 'currentPeriod))
         (used (or (ghostherd-usage--alist cfg 'creditUsagePercent)
                   (and period 0)))
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

(defun ghostherd-usage--agy-group-prefix (name)
  "Short name for agy quota group NAME, as the `/usage' panel's labels go.
Gemini 4 Argon is its own group, kept apart from the other Gemini models
so it is a row of its own rather than a second `gem' that collides."
  (let ((n (downcase name)))
    (cond ((or (string-match-p "argon" n)
               (string-match-p "gemini[^a-z0-9]*4\\b" n)) "g4")
          ((string-match-p "gemini" n) "gem")
          ((string-match-p "third\\|party\\|claude\\|anthropic\\|openai\\|gpt" n) "3p")
          (t (let ((word (car (split-string n "[^a-z0-9]+" t))))
               (if word (substring word 0 (min 4 (length word))) "grp"))))))

(defun ghostherd-usage--parse-agy-groups (raw)
  "Parse retrieveUserQuotaSummary.  remainingFraction is remaining 0–1.
With more than one group each label names its group (`gem-wk',
`3p-5h'), as the `/usage' panel's do: two groups' `wk' side by side
cannot be told apart."
  (let* ((groups (or (ghostherd-usage--alist raw 'groups)
                     (ghostherd-usage--alist
                      (ghostherd-usage--alist raw 'response) 'groups)))
         (several (cdr groups))
         (out nil))
    (dolist (g groups)
      (let ((gname (or (ghostherd-usage--alist g 'displayName) "")))
        (dolist (b (or (ghostherd-usage--alist g 'buckets) '()))
          (let* ((frac (ghostherd-usage--alist b 'remainingFraction))
                 (win (format "%s" (or (ghostherd-usage--alist b 'window)
                                       (ghostherd-usage--alist b 'bucketId)
                                       "")))
                 (span
                  (cond
                   ((string-match-p "weekly\\|WEEKLY" win) "wk")
                   ((string-match-p "5h\\|five" win) "5h")
                   ((and (not several) (string-match-p "Gemini" gname)) "gem")
                   (t "lim")))
                 (label (if several
                            (concat (ghostherd-usage--agy-group-prefix gname) "-" span)
                          span))
                 (rem (and (numberp frac)
                           (ghostherd-usage--round1 (* 100.0 frac)))))
            (when rem
              (push (ghostherd-usage--window
                     label rem
                     (ghostherd-usage--parse-time
                      (ghostherd-usage--alist b 'resetTime)))
                    out))))))
    (nreverse out)))

(defconst ghostherd-usage--agy-header-re
  "\\`[A-Z][A-Z0-9 &/-]*[A-Z0-9]\\'"
  "A `/usage' group header: an all-caps line like \"GEMINI 4 ARGON\".")

(defun ghostherd-usage--agy-screen-remaining (lines)
  "The remaining percent of the window whose header LINES follow -- a
bar's `N.NN%', or 100 for a `Quota available' with no bar -- read before
the next window or group.  Nil when none is found."
  (catch 'done
    (dolist (l lines)
      (let ((s (string-trim l)))
        (cond
         ;; The next window, or the next group: this window had no number.
         ((string-match-p "Limit\\b" s) (throw 'done nil))
         ((and (string-match-p ghostherd-usage--agy-header-re s)
               (not (string-match-p "QUOTA\\|REMAINING" s)))
          (throw 'done nil))
         ((string-match "\\([0-9]+\\(?:\\.[0-9]+\\)?\\)%" s)
          (throw 'done (string-to-number (match-string 1 s))))
         ((string-match-p "Quota available" s) (throw 'done 100.0)))))
    nil))

(defun ghostherd-usage--parse-agy-usage-screen (text)
  "Parse the `/usage' panel into one window per group's limit, labelled
from the group it sits under -- `gem-wk', `3p-5h', `g4-wk' -- so a group
the panel adds (Gemini 4 Argon) is a row of its own, not dropped or stuck
onto the wrong one.  Printed percents are remaining, not used."
  (when (and text (string-match-p "Limit Remaining\\|Limit\\b\\|Quota available" text))
    (let ((lines (split-string text "\n"))
          (prefix nil) (out nil))
      (while lines
        (let ((s (string-trim (car lines))))
          (cond
           ;; A group header names the rows under it until the next header.
           ((and (string-match-p ghostherd-usage--agy-header-re s)
                 (not (string-match-p "LIMIT\\|REMAINING\\|QUOTA" s)))
            (setq prefix (ghostherd-usage--agy-group-prefix s)))
           ;; A window: its span, then its remaining on a line soon after.
           ((and prefix (string-match-p "\\(Weekly\\|Five[ -]?Hour\\|5[ -]?Hour\\) Limit" s))
            (let ((span (if (string-match-p "Weekly" s) "wk" "5h"))
                  (rem (ghostherd-usage--agy-screen-remaining (cdr lines))))
              (when rem
                (push (ghostherd-usage--window (concat prefix "-" span) rem nil) out))))))
        (setq lines (cdr lines)))
      (nreverse out))))

(defun ghostherd-usage--put (kind windows &optional err fetched)
  "Record KIND's WINDOWS, ERR, and FETCHED, when they were read (now)."
  (setq ghostherd-usage--cache
        (cons (cons kind
                    (list :windows windows
                          :fetched (or fetched (float-time))
                          :error err))
              (assq-delete-all kind ghostherd-usage--cache))))

(defun ghostherd-usage--put-error (kind reason)
  "Record REASON for KIND, keeping the last good windows if there are any.
One failed poll should read as stale numbers, not as no numbers:
agy in particular answers from a `/usage' probe that only runs
once a minute, and dropping its windows on the next expired-token
refresh would blank a perfectly good readout."
  (let* ((old (alist-get kind ghostherd-usage--cache))
         (wins (plist-get old :windows)))
    (setq ghostherd-usage--cache
          (cons (cons kind
                      (list :windows wins
                            :fetched (if wins
                                         (plist-get old :fetched)
                                       (float-time))
                            :error reason))
                (assq-delete-all kind ghostherd-usage--cache)))))


;;; Format
;;
;; Mode-line treats `%' as a construct (`78% 5h' became `785h').  Bars
;; plus a bare integer avoid that; `ghostherd-usage--mode-line-safe'
;; still doubles any leftover percent signs.

(defun ghostherd-usage--mode-line-safe (s)
  (replace-regexp-in-string "%" "%%" (or s "")))

(defun ghostherd-usage--eta (resets-at)
  "Human `1h03m' until RESETS-AT, or nil if it is not ahead of now."
  (when (and (numberp resets-at) (> resets-at (float-time)))
    (let* ((secs (- resets-at (float-time)))
           (h (floor secs 3600))
           (m (floor (mod secs 3600) 60)))
      (if (> h 0) (format "%dh%02dm" h m) (format "%dm" m)))))

(defun ghostherd-usage--help (_kind wins)
  (mapconcat
   (lambda (w)
     (let* ((rem (plist-get w :remaining))
            (eta (ghostherd-usage--eta (plist-get w :resets-at))))
       (concat (format "%s %d%% used, %d%% left"
                       (plist-get w :label)
                       (ghostherd-usage--pct-n (ghostherd-usage--used rem))
                       (ghostherd-usage--pct-n rem))
               (and eta (format ", resets in %s" eta)))))
   wins
   "  "))

(defun ghostherd-usage--window-bar (w compact)
  "Render window W as `<bar><used> <label>'.
The number is the used percent, like the provider panels print."
  (let* ((n (ghostherd-usage--pct-n
             (ghostherd-usage--used (plist-get w :remaining))))
         (bar (ghostherd-usage--bar (plist-get w :remaining) (if compact 4 6)))
         (s (if compact
                (format "%s%d" bar n)
              (format "%s%d %s" bar n (plist-get w :label)))))
    (propertize s 'face (ghostherd-usage--face (plist-get w :remaining)))))

(defun ghostherd-usage--tightest (wins)
  (car (seq-sort-by (lambda (w) (plist-get w :remaining)) #'< wins)))

(defconst ghostherd-usage-trusted-for 7200
  "Seconds a reading of usage is believed when it says some is used up.")

(defun ghostherd-usage-used-up (kind &optional model)
  "The window of KIND's usage that is used up, or nil.
A window as the cache keeps it: :label, :remaining, :resets-at.  Less
than 1% left counts.  Not one that has reset since it was read, nor
any from a reading older than `ghostherd-usage-trusted-for'.

With MODEL, and a KIND whose quota is split by model group (agy): only a
window of the group MODEL belongs to counts -- so an agent on a group
that has quota is not called used up because a different group is spent."
  (let* ((pl (alist-get kind ghostherd-usage--cache))
         (fetched (plist-get pl :fetched))
         (now (float-time))
         (group (and model (eq kind 'agy) (ghostherd-usage--agy-group-prefix model))))
    (when (and (numberp fetched) (< (- now fetched) ghostherd-usage-trusted-for))
      (car (seq-sort-by
            (lambda (w) (or (plist-get w :resets-at) most-positive-fixnum))
            #'>
            (seq-filter (lambda (w)
                          (let ((left (plist-get w :remaining))
                                (resets (plist-get w :resets-at))
                                (label (plist-get w :label)))
                            (and (numberp left) (< left 1)
                                 (not (and (numberp resets) (<= resets now)))
                                 (or (not group)
                                     (and (stringp label)
                                          (string-prefix-p (concat group "-") label))))))
                        (plist-get pl :windows)))))))

(defun ghostherd-usage-used-up-text (kind &optional model)
  "KIND's usage used up, in words -- \"its claude usage (5h) is used up,
until 15:00\" -- or nil when it is not.  MODEL narrows it to that model's
group, for a KIND whose quota is split by model (agy)."
  (when-let* ((w (ghostherd-usage-used-up kind model)))
    (let ((label (plist-get w :label))
          (resets (plist-get w :resets-at)))
      (format "its %s usage%s is used up%s" kind
              (if (and (stringp label) (not (string-empty-p label))) (format " (%s)" label) "")
              (cond ((not (numberp resets)) "")
                    ((< (- resets (float-time)) (* 20 3600))
                     (format-time-string ", until %H:%M" resets))
                    (t (format-time-string ", until %a %H:%M" resets)))))))

(defun ghostherd-usage--agy-session-model (session)
  "The model NAME agy SESSION's screen shows now, or nil -- read from the
screen the state poll already captured, as the meter does."
  (when (and (fboundp 'ghostherd--agy-model-line)
             (boundp 'ghostherd--screens)
             (fboundp 'ghostherd-session-id))
    (car (ghostherd--agy-model-line
          (gethash (ghostherd-session-id session) ghostherd--screens)))))

(defconst ghostherd-usage--agy-limited-re
  "·[[:blank:]]*AI:[[:blank:]]*Out of credits\\b"
  "agy's footer note that it has no AI credits -- its status line, the one
carrying its model and effort, ends `… · AI: Out of credits'.")

(defun ghostherd-usage--agy-screen-limited (screen)
  "agy is out of AI credits, read from SCREEN's footer -- the status line
that carries its model and effort, ending in `· AI: Out of credits' -- or
nil.  Only the last lines are looked at, where that status line is drawn,
so the same words quoted in the conversation above never trigger it."
  (when (and (stringp screen) (fboundp 'ghostherd--agy-model-line))
    ;; agy 1.4.0 draws the status line last, under its input box (────/>/────),
    ;; so the last three lines hold it and nothing from the conversation.  If
    ;; a later agy ever drew it higher, this would miss the live catch and
    ;; fall back to the usage poll -- never a false positive.
    (let ((foot (last (split-string (string-trim-right screen) "\n") 3)))
      (cl-loop for l in (reverse foot)
               when (and (ghostherd--agy-model-line l)
                         (string-match-p ghostherd-usage--agy-limited-re l))
               return "out of AI credits"))))

(defun ghostherd-usage--screen-limited (session)
  "The CLI's own words, off SESSION's live screen, that it is out of credits
or rate limited right now -- or nil.  Read from the status line the CLI
draws, not the conversation, and from the live screen each time, so it
clears on its own when the line does and does not trigger on a quote.
Only agy has a confirmed status string so far; claude and grok wait on a
real sample of theirs."
  (when (and (boundp 'ghostherd--screens) (fboundp 'ghostherd-session-id))
    (let ((screen (gethash (ghostherd-session-id session) ghostherd--screens)))
      (pcase (ghostherd-session-kind session)
        ('agy (ghostherd-usage--agy-screen-limited screen))
        (_ nil)))))

(defun ghostherd-usage--session-has-quota (session)
  "Non-nil when a fresh usage reading covers SESSION's own model group
\(agy) or its CLI and shows quota left there.

agy's footer `· AI: Out of credits' is its paid Google AI credits -- the
overage past the model quota -- not the quota itself: a fresh session
with the quota full shows it and its requests still go through.  So that
line must not, on its own, mark a session used up when the poll can see
the model's group still has quota.  Nil when no fresh reading covers the
group (none yet, too old to trust, or a different group): then the live
footer catch is the only signal left."
  (let* ((kind (ghostherd-session-kind session))
         (pl (alist-get kind ghostherd-usage--cache))
         (fetched (plist-get pl :fetched))
         (now (float-time))
         (model (and (eq kind 'agy) (ghostherd-usage--agy-session-model session)))
         (group (and model (ghostherd-usage--agy-group-prefix model))))
    (when (and (numberp fetched) (< (- now fetched) ghostherd-usage-trusted-for)
               (or (not (eq kind 'agy)) group))
      (let ((wins (seq-filter
                   (lambda (w)
                     (or (not group)
                         (let ((label (plist-get w :label)))
                           (and (stringp label)
                                (string-prefix-p (concat group "-") label)))))
                   (plist-get pl :windows))))
        (and wins
             (seq-every-p (lambda (w)
                            (let ((left (plist-get w :remaining))
                                  (resets (plist-get w :resets-at)))
                              (and (numberp left)
                                   (or (>= left 1)
                                       (and (numberp resets) (<= resets now))))))
                          wins))))))

(defun ghostherd-usage-session-used-up (session)
  "The window of SESSION's usage that is used up, by its own model's group
when its CLI splits quota by model (agy) -- so an agent on a group that
has quota is not called used up because a different group is spent.  Nil
when an agy's model cannot be read: better no relay than a wrong one.

The usage poll is the authority.  When the poll says this agent's own
group is spent, that is the catch.  When it says the group still has
quota, it is not used up -- even if the CLI's footer shows it is out of
paid credits, which is agy's overage, separate from the model quota.
Only when the poll has no reading to go on does the CLI's own status line
stand in, without waiting for the next poll (c9a18d0): that reads as a
window with no label, carrying the CLI's words as :reason."
  (let* ((kind (ghostherd-session-kind session))
         (poll (if (eq kind 'agy)
                   (when-let* ((model (ghostherd-usage--agy-session-model session)))
                     (ghostherd-usage-used-up kind model))
                 (ghostherd-usage-used-up kind))))
    (cond
     (poll poll)
     ((ghostherd-usage--session-has-quota session) nil)
     ((when-let* ((reason (ghostherd-usage--screen-limited session)))
        (list :label "" :remaining 0 :resets-at nil :reason reason))))))

(defun ghostherd-usage-session-used-up-text (session)
  "SESSION's usage used up, in words, by its own model's group for agy, or
the CLI's own words when its status line says so live and the poll has no
reading to contradict it; nil when not, or when an agy's model cannot be
read.  Mirrors `ghostherd-usage-session-used-up'."
  (let* ((kind (ghostherd-session-kind session))
         (poll (if (eq kind 'agy)
                   (when-let* ((model (ghostherd-usage--agy-session-model session)))
                     (ghostherd-usage-used-up-text kind model))
                 (ghostherd-usage-used-up-text kind))))
    (cond
     (poll poll)
     ((ghostherd-usage--session-has-quota session) nil)
     ((when-let* ((reason (ghostherd-usage--screen-limited session)))
        (format "its %s is %s" kind reason))))))

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
        (propertize s 'help-echo
                    (concat (ghostherd-usage--help kind wins)
                            (and err (format "  (stale: %s)" err)))))))))

(defun ghostherd-usage-line (&optional width)
  "Footer fragment: per-kind usage bars, filled by what is spent."
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

(defun ghostherd-usage--expiry-time (expiry)
  "EXPIRY -- epoch seconds or milliseconds, or ISO text -- as epoch seconds."
  (cond
   ((numberp expiry)
    (if (> expiry 1e12) (/ expiry 1000.0) (float expiry)))
   ((stringp expiry) (ghostherd-usage--parse-time expiry))
   (t nil)))

(defun ghostherd-usage--expired-p (expiry)
  (let ((ts (ghostherd-usage--expiry-time expiry)))
    (and ts (< ts (+ (float-time) 60)))))


;;; HTTP

(defconst ghostherd-usage--hosts
  '("api.anthropic.com" "platform.claude.com" "cli-chat-proxy.grok.com"
    "daily-cloudcode-pa.googleapis.com")
  "Hosts `ghostherd-usage--http' talks to.")

(defvar-local ghostherd-usage--own-request nil
  "Non-nil in a url buffer opened by `ghostherd-usage--http'.")

(defun ghostherd-usage--own-request-p ()
  "Non-nil if the current url buffer is one of ours."
  (or ghostherd-usage--own-request
      (and (boundp 'url-current-object)
           url-current-object
           (member (url-host url-current-object) ghostherd-usage--hosts)
           t)))

(defun ghostherd-usage--no-auth-prompt (orig &rest args)
  "Let a 401 reach our own callback instead of asking for a password.
url.el only skips the minibuffer prompt when the request already
carried an `Authorization' header -- which the token refreshes do
not -- and that prompt runs inside a process filter, where the
quit it raises kills the callback: the fetch then never finishes
and `ghostherd-usage--inflight' stays pinned.  t is url.el's own
\"already tried, give up\" answer, so our callback sees the 401;
nil would make it retry and never activate the callback at all.

A `cl-letf' around `url-retrieve' cannot do this -- it is long
unwound by the time the response lands."
  (if (ghostherd-usage--own-request-p) t (apply orig args)))

(advice-add 'url-http-handle-authentication :around
            #'ghostherd-usage--no-auth-prompt)

(defun ghostherd-usage--http (url headers callback &optional method json-body form)
  "CALLBACK is (lambda (code body)).  BODY is a string.  Never logs headers.
CALLBACK runs exactly once: with the response, or with code 0
after `ghostherd-usage-timeout' if none arrives."
  (let* ((fired nil)
         (buf nil)
         (fire
          (lambda (code body)
            (unless fired
              (setq fired t)
              (condition-case err
                  (funcall callback code body)
                (error (message "ghostherd-usage: callback failed: %S" err))))))
         (url-request-method (or method "GET"))
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
    (setq buf
          (url-retrieve
           url
           (lambda (_status)
             ;; A connection-level failure leaves no status line, so it
             ;; arrives as code 0 -- same shape as the deadline.
             (let ((this (current-buffer))
                   code body)
               (unwind-protect
                   (ignore-errors
                     (goto-char (point-min))
                     (setq code (and (re-search-forward
                                      "^HTTP/[^ ]+ \\([0-9]+\\)" nil t)
                                     (string-to-number (match-string 1))))
                     (goto-char (point-min))
                     (when (re-search-forward "\n\n" nil t)
                       (setq body (decode-coding-string
                                   (buffer-substring-no-properties
                                    (point) (point-max))
                                   'utf-8))))
                 (when (buffer-live-p this)
                   (kill-buffer this)))
               ;; After the unwind, so it fires even when parsing blew
               ;; up, and outside the dead buffer.
               (funcall fire (or code 0) (or body ""))))
           nil t t))
    (when (buffer-live-p buf)
      (with-current-buffer buf (setq ghostherd-usage--own-request t)))
    (run-with-timer
     (max 1 ghostherd-usage-timeout) nil
     (lambda ()
       (unless fired
         (when (buffer-live-p buf)
           (when-let* ((proc (get-buffer-process buf)))
             (ignore-errors (delete-process proc)))
           (kill-buffer buf))
         (funcall fire 0 ""))))
    buf))

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
  "Fill grok cache from the last billing line in unified.jsonl.
As of when grok wrote it, and only when that is newer than what the
cache has: a line an hour old is an hour old, and must not stand
over what the billing API said a minute ago."
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
                (wins (ghostherd-usage--parse-grok-config cfg))
                (at (or (ghostherd-usage--parse-time (ghostherd-usage--alist hit 'ts))
                        (float-time))))
      (when (> at (or (plist-get (alist-get 'grok ghostherd-usage--cache) :fetched) 0))
        (ghostherd-usage--put 'grok wins nil at))
      t)))

(defun ghostherd-usage--done ()
  (when ghostherd-usage--inflight
    (setq ghostherd-usage--inflight (1- ghostherd-usage--inflight))
    (when (<= ghostherd-usage--inflight 0)
      (setq ghostherd-usage--inflight nil
            ghostherd-usage--inflight-at nil)))
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
               (lambda (raw &optional code)
                 (if raw (apply-usage raw)
                   (ghostherd-usage--put-error
                    'claude (format "http %s" (or code "?")))
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
         (lambda (raw &optional code)
           ;; One that fails says why, the numbers kept: a card left
           ;; as it was reads as current.
           (let* ((cfg (or (ghostherd-usage--alist raw 'config) raw))
                  (wins (and cfg (ghostherd-usage--parse-grok-config cfg))))
             (cond (wins (ghostherd-usage--put 'grok wins))
                   (raw (ghostherd-usage--put-error 'grok "billing unreadable"))
                   (t (ghostherd-usage--put-error 'grok (format "http %s" (or code "?")))))
             (ghostherd-usage--done))))))))

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

(defcustom ghostherd-usage-agy-probe t
  "Non-nil to read agy's quota by typing `/usage' into an idle agy session.
Once the token agy saved has expired -- an hour after agy wrote it --
the `/usage' panel is the only reading there is.  It types into your
session, and Escapes out after it paints, so it is yours to switch
off: agy's numbers then stay as last read, marked stale, until agy
saves a fresh token."
  :type 'boolean
  :group 'ghostherd-usage)

(defun ghostherd-usage--agy-probe ()
  "Type `/usage` into one idle agy session, then Esc after it paints.
The slash command is how the CLI itself shows remaining quota, and
the only reading left once agy's saved token has expired."
  (when (and ghostherd-usage-agy-probe
             (fboundp 'ghostherd--host-send-text)
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

(defun ghostherd-usage--agy-fail (reason)
  "Record REASON against agy, kicking the `/usage' probe if it can run.
Always leaves a cache entry.  With none, the footer renders a
bare `agy —', which reads as \"not wired up\" and hides the real
answer; `agy ?' carries REASON in its help-echo instead."
  (ghostherd-usage--put-error 'agy
                              (if (ghostherd-usage--agy-probe)
                                  (concat reason "; asking /usage")
                                reason)))

(defun ghostherd-usage--agy-quota (token)
  (ghostherd-usage--http-json
   "https://daily-cloudcode-pa.googleapis.com/v1internal:retrieveUserQuotaSummary"
   `(("Authorization" . ,(concat "Bearer " token))
     ("User-Agent" . "antigravity/darwin/arm64")
     ("Content-Type" . "application/json"))
   (lambda (raw &optional code)
     (let ((wins (and raw (ghostherd-usage--parse-agy-groups raw))))
       (if wins
           (ghostherd-usage--put 'agy wins)
         (ghostherd-usage--agy-fail (format "quota %s" (or code "empty"))))
       (ghostherd-usage--done)))
   "POST" (make-hash-table)))

(defun ghostherd-usage--agy-expired (expiry)
  "Why agy cannot be read: its saved token ran out at EXPIRY."
  (let ((ts (ghostherd-usage--expiry-time expiry)))
    (if (not ts)
        "agy's saved token has expired"
      (format "agy's saved token expired %s"
              (format-time-string
               (if (equal (format-time-string "%F" ts) (format-time-string "%F"))
                   "%H:%M"
                 "%m-%d %H:%M")
               ts)))))

(defun ghostherd-usage--fetch-agy ()
  "Read agy's quota: a `/usage' panel on screen, else the API while
agy's saved token is fresh, else say it has expired (and ask an idle
session for the panel, if `ghostherd-usage-agy-probe' allows)."
  (if-let* ((wins (ghostherd-usage--agy-from-screens)))
      (progn (ghostherd-usage--put 'agy wins)
             (ghostherd-usage--done))
    (let* ((blob (ghostherd-usage--agy-token-blob))
           (tok (and blob (ghostherd-usage--alist blob 'access_token)))
           (expiry (and blob (ghostherd-usage--alist blob 'expiry))))
      (cond
       ((not tok)
        (ghostherd-usage--agy-fail "no agy token saved")
        (ghostherd-usage--done))
       ((ghostherd-usage--expired-p expiry)
        (ghostherd-usage--agy-fail (ghostherd-usage--agy-expired expiry))
        (ghostherd-usage--done))
       (t (ghostherd-usage--agy-quota tok))))))


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
  ;; A batch that outlived every request's deadline can only be a
  ;; dropped callback; it must not gate refreshes for ever.
  (when (and ghostherd-usage--inflight
             (or (null ghostherd-usage--inflight-at)
                 (> (- (float-time) ghostherd-usage--inflight-at)
                    (* 3 (max 1 ghostherd-usage-timeout)))))
    (setq ghostherd-usage--inflight nil
          ghostherd-usage--inflight-at nil))
  (when (and (or force (not noninteractive))
             (or force (not ghostherd-usage--inflight)))
    (setq ghostherd-usage--inflight 3
          ghostherd-usage--inflight-at (float-time))
    (ghostherd-usage--fetch-claude)
    (ghostherd-usage--fetch-grok)
    (ghostherd-usage--fetch-agy)))

(provide 'ghostherd-usage)
;;; ghostherd-usage.el ends here

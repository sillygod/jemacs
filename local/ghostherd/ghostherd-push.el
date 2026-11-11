;;; ghostherd-push.el --- Pushes to your phone, and the herd tick -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of ghostherd (see ghostherd.el).  Pushes to your phone through
;; the sidecar's Telegram bot, and the herd tick that carries them with
;; the snapshot, mail and asks.

;;; Code:

(require 'ghostherd-core)
(require 'ghostherd-screen)
(require 'ghostherd-spawn)
(require 'ghostherd-mail)
(require 'ghostherd-hooks)
(require 'ghostherd-primary)

;;; Pushes to your phone
;;
;; The herd page shows everything -- while you look at it.  Telegram is
;; for when you do not: the sidecar's bot pushes what wants you, an agent
;; blocked, a lead's report at the end of its turn, an ask that failed.
;; Emacs decides when one is worth it -- you are looking at neither the
;; agent nor the herd page, and have left this machine -- and the bot
;; words it: it has the lead's last message, and the buttons to answer.

(defcustom ghostherd-push '(blocked report ask-failed used-up lead-started guard)
  "What the herd pushes to Telegram while you are away: an agent
`blocked', a lead's `report' at the end of its turn, an `ask-failed',
an agent stopped with its CLI's usage `used-up' -- with a button that
relays it -- a lead the herd started in a project for a handoff
from another, `lead-started', and a call a guarded agent's `guard'
refused, waiting for your approval -- told only: it is approved on the
herd page.  Nil pushes nothing.  The sidecar's bot sends them to the
chats of GHOSTHERD_TELEGRAM_CHAT_IDS; a reply to one goes to that
project's lead."
  :type '(set (const blocked) (const report) (const ask-failed) (const used-up)
              (const lead-started) (const guard))
  :group 'ghostherd)

(defcustom ghostherd-push-images t
  "Non-nil: a lead's report pushed to Telegram brings the images its words
name, after its text -- one as a photo, more as an album, one too big for
a photo as a file; four at most.  They are the copies the herd keeps of
what an agent's reply names (`ghostherd-reply-images-directory'), and go to
Telegram's servers as the text does: nil keeps them to the herd page."
  :type 'boolean
  :group 'ghostherd)

(defcustom ghostherd-push-reports t
  "Non-nil: a lead's report pushed to Telegram brings the HTML pages its
words name -- a report an agent wrote as one -- as files after its text,
two at most.  They are the copies the herd keeps (beside the images, in
`ghostherd-reply-images-directory'), and go to Telegram's servers as the
text does -- the file itself, never a link: the herd page's address
holds its secret.  Nil keeps them to the herd page, where a message that
names one has Open under it."
  :type 'boolean
  :group 'ghostherd)

(defcustom ghostherd-push-away-after 120
  "Seconds without a key or the mouse on this Mac after which you count
as away from it, which is when a push goes out.  Nil: whenever you look
at neither the agent nor the herd page, at the desk too.  Where the idle
time cannot be read (no ioreg), you count as away."
  :type '(choice (const :tag "Whenever not looking" nil) natnum)
  :group 'ghostherd)

(defvar ghostherd-present-functions nil
  "Functions of no arguments saying whether the herd page is in front of
you now.  The first non-nil answer holds pushes back.")

(defvar ghostherd-room-url-function nil
  "Function of a project giving the address of its room from your phone
-- of the herd page itself, for nil -- for a push or the bot to link
to; nil, or a function giving nil, links none.")

(defvar ghostherd--room-urls (make-hash-table :test 'equal)
  "(FUNCTION PROJECT HOSTS SCHEME) -> what FUNCTION said is PROJECT's
address from your phone.  Asked once: asking keeps xwapp's address
over restarts, which writes a file, and the herd's tick asks every
second.")

(defun ghostherd--room-url (&optional project)
  "The address of PROJECT's room from your phone -- of the herd page, for
nil -- as `ghostherd-room-url-function' gives it; nil when it gives none."
  (when ghostherd-room-url-function
    (let ((key (list ghostherd-room-url-function project
                     (bound-and-true-p xwapp-remote-hosts)
                     (bound-and-true-p xwapp-remote-scheme))))
      (or (gethash key ghostherd--room-urls)
          (puthash key (ignore-errors (funcall ghostherd-room-url-function project))
                   ghostherd--room-urls)))))

(defun ghostherd--board-url (kind name fields)
  "The address, from your phone, of what a push of KIND about agent NAME
\(FIELDS its plist) is about on the herd page's Board -- the page's home,
`ghostherd--room-url' of nil -- or nil: what waits on you, by its item
\(`#board/at/ID', the Inbox's id: a menu, a guarded call, a failed ask,
a relay); else the agent's (`#board/agent/NAME': its report, a lead the
herd started)."
  (when-let* ((home (ghostherd--room-url))
              ((string-suffix-p "#board" home)))
    (let ((id (pcase kind
                ('blocked (concat "blocked:" name))
                ('guard (and (plist-get fields :request) (format "guard:%s" (plist-get fields :request))))
                ('ask-failed (and (plist-get fields :ask) (format "ask-failed:%s" (plist-get fields :ask))))
                ('used-up (concat "relay:" name)))))
      (concat home (if id (concat "/at/" (url-hexify-string id)) (concat "/agent/" (url-hexify-string name)))))))

(defvar ghostherd--herd-pushes nil
  "Pushes not yet handed to the sidecar, newest first.")

(defvar ghostherd--working-since (make-hash-table :test 'equal)
  "Session id -> when it last began working: a lead's report is what it
said after that.")

(defun ghostherd--idle-seconds ()
  "Seconds since a key or the mouse was last touched on this Mac, or nil."
  (when (executable-find "ioreg")
    (with-temp-buffer
      (when (and (eq 0 (ignore-errors
                         (call-process "ioreg" nil t nil "-c" "IOHIDSystem" "-d" "4" "-k" "HIDIdleTime")))
                 (re-search-backward "\"HIDIdleTime\" = \\([0-9]+\\)" nil t))
        (/ (string-to-number (match-string 1)) 1e9)))))

(defun ghostherd--away-p ()
  "Non-nil when you count as away from this machine; see
`ghostherd-push-away-after'."
  (or (null ghostherd-push-away-after)
      (let ((idle (ghostherd--idle-seconds)))
        (or (null idle) (>= idle ghostherd-push-away-after)))))

(defun ghostherd--push (kind session &rest fields)
  "Hand the sidecar a push of KIND about SESSION, with FIELDS (a plist),
when it is worth one: see `ghostherd-push'."
  (when (and (memq kind ghostherd-push)
             (getenv "GHOSTHERD_TELEGRAM_TOKEN")
             (getenv "GHOSTHERD_TELEGRAM_CHAT_IDS")
             (not (ghostherd--watched-p session))
             (not (run-hook-with-args-until-success 'ghostherd-present-functions))
             (ghostherd--away-p))
    (let ((project (ghostherd--project-name (ghostherd-session-project session))))
      (push (append (list :kind (symbol-name kind)
                          :session (ghostherd-session-name session)
                          :project (or project "")
                          :url (or (and project (ghostherd--room-url project)) "")
                          :board (or (ghostherd--board-url kind (ghostherd-session-name session) fields) "")
                          :images (if ghostherd-push-images t :json-false)
                          :reports (if ghostherd-push-reports t :json-false))
                    fields)
            ghostherd--herd-pushes))))

(defun ghostherd--push-state (session _old new)
  "Push SESSION's turning NEW: blocked, or a lead done with its turn."
  (pcase new
    ('working (puthash (ghostherd-session-id session) (float-time) ghostherd--working-since))
    ('blocked (ghostherd--push 'blocked session :reason (or (ghostherd-session-state-reason session) "")))
    ('done (when (member (ghostherd-session-role session) '("lead" "primary"))
             (ghostherd--push 'report session
                              :since (gethash (ghostherd-session-id session) ghostherd--working-since 0))))))

(add-hook 'ghostherd-state-change-hook #'ghostherd--push-state)

;;;###autoload
(defun ghostherd-push-test ()
  "Push a message to your phone now, away or not: to see that the herd
reaches you there."
  (interactive)
  (unless (and (getenv "GHOSTHERD_TELEGRAM_TOKEN") (getenv "GHOSTHERD_TELEGRAM_CHAT_IDS"))
    (user-error "No Telegram bot: GHOSTHERD_TELEGRAM_TOKEN and GHOSTHERD_TELEGRAM_CHAT_IDS"))
  (push (list :kind "test" :session "ghostherd" :project "" :url "") ghostherd--herd-pushes)
  (message "A test push goes to Telegram with the herd's next tick"))

(defun ghostherd--herd-tick-async ()
  "Push the snapshot and pull mail plus Telegram commands.  Never blocks."
  (when (and (not ghostherd--herd-tick-inflight)
             (fboundp 'ghostherd-memory-rpc-url)
             (ghostherd-memory-rpc-url)
             (fboundp 'ghostherd-memory-request-async))
    (let ((acks (or ghostherd--herd-ack-ids []))
          (replies ghostherd--herd-replies)
          (pushes ghostherd--herd-pushes)
          (told ghostherd--guard-told-ids))
      (setq ghostherd--herd-tick-inflight t
            ghostherd--herd-replies nil
            ghostherd--herd-pushes nil
            ghostherd--guard-told-ids nil)
      (ghostherd-memory-request-async
       "herd_tick"
       (lambda (result)
         (setq ghostherd--herd-tick-inflight nil
               ghostherd--herd-ack-ids
               (seq-difference ghostherd--herd-ack-ids
                               (append acks nil)))
         (ghostherd--herd-deliver result)
         (ghostherd--herd-take-asks (plist-get result :asks))
         (ghostherd--herd-take-links (plist-get result :links))
         (ghostherd--herd-take-reports (plist-get result :reports))
         (ghostherd--herd-take-guard (plist-get result :guard))
         (ghostherd--herd-take-lint (plist-get result :lint))
         (ghostherd--handoff-take-slots (plist-get result :handoff_slots))
         (ghostherd--herd-run-commands result))
       (list :sessions (vconcat (ghostherd--herd-snapshot))
             :page_url (or (ghostherd--room-url) "")
             ;; Whether this Emacs has restored its herd yet: before it, an
             ;; empty snapshot is a herd not yet picked up, and the sidecar
             ;; must not fail an open ask for a taker missing from it.
             :restored (if ghostherd--restored t :json-false)
             :ack_ids (vconcat (append acks nil))
             :replies (vconcat (or replies []))
             :pushes (vconcat (reverse pushes))
             ;; How long a guarded agent's refused call waits for the user.
             :guard_ttl ghostherd-guard-request-ttl
             ;; The guard's requests told of since: not again.
             :guard_told (vconcat (reverse told))
             ;; The user's caps on the leads the herd starts for handoffs:
             ;; said here alone, where no agent's token reaches.
             :handoff_caps (ghostherd--handoff-caps))
       (lambda (_err)
         (setq ghostherd--herd-tick-inflight nil
               ghostherd--herd-replies
               (append replies ghostherd--herd-replies)
               ghostherd--herd-pushes
               (append ghostherd--herd-pushes pushes)
               ghostherd--guard-told-ids
               (append ghostherd--guard-told-ids told)))))))

(provide 'ghostherd-push)

;; Local Variables:
;; generated-autoload-load-name: "ghostherd"
;; End:

;;; ghostherd-push.el ends here

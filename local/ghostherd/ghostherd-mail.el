;;; ghostherd-mail.el --- The sidecar's herd mail, asks, and how a turn began -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of ghostherd (see ghostherd.el).  The sidecar's herd mail --
;; agents POST to it, Emacs delivers into their terminals -- asks, mail
;; that wants an answer, and how a turn began: the user's own prompt,
;; or another agent's ask.

;;; Code:

(require 'ghostherd-core)
(require 'ghostherd-worktree)
(require 'ghostherd-log)
(require 'ghostherd-screen)
(require 'ghostherd-comm)

(declare-function ghostherd--herd-tick-async "ghostherd" ())
(declare-function ghostherd--hooked-p "ghostherd-hooks" (session))
(declare-function ghostherd--push "ghostherd" (kind session &rest fields))
(declare-function ghostherd--role-take "ghostherd-lead" (session))
(declare-function ghostherd--room-url "ghostherd" (&optional project))
(declare-function ghostherd--session-guarded-p "ghostherd-hooks" (session))

;;; Sidecar herd mail (agents POST /jsonrpc; Emacs delivers to PTYs)

(defvar ghostherd--herd-ack-ids nil
  "Mail ids delivered this cycle, acked on the next `herd_tick'.")

(defvar ghostherd--herd-replies nil
  "Command replies to send on the next `herd_tick'.")

(defvar ghostherd--herd-tick-inflight nil
  "Non-nil while an async `herd_tick' is in flight.")

(defcustom ghostherd-herd-tick-idle 0.8
  "Idle seconds before a sidecar `herd_tick', and seconds between them.

The poll timer runs while you type, so putting HTTP on it made
keystrokes compete with `url-retrieve'.  A tick waits until Emacs has
been idle this long, and keeps coming while it stays idle.  An idle
timer does not: it fires once per spell of idleness, so an Emacs left
alone stopped delivering the herd's mail until someone touched it ---
and two agents asking each other in an empty room stopped with it."
  :type 'number
  :group 'ghostherd)

(defvar ghostherd--herd-tick-timer nil
  "Timer that drives `ghostherd--herd-tick-async' while Emacs is idle.")

(defun ghostherd--herd-tick-when-idle ()
  "Tick the sidecar, unless you are typing."
  (let ((idle (current-idle-time)))
    (when (and idle (>= (float-time idle) ghostherd-herd-tick-idle))
      (ghostherd--herd-tick-async))))

(defun ghostherd--ensure-herd-tick-timer ()
  "Start the idle sidecar tick if `ghostherd-mode' is on."
  (unless (timerp ghostherd--herd-tick-timer)
    (setq ghostherd--herd-tick-timer
          (run-at-time ghostherd-herd-tick-idle ghostherd-herd-tick-idle
                       #'ghostherd--herd-tick-when-idle))))

(defun ghostherd--stop-herd-tick-timer ()
  (when (timerp ghostherd--herd-tick-timer)
    (cancel-timer ghostherd--herd-tick-timer)
    (setq ghostherd--herd-tick-timer nil)))

(defconst ghostherd--herd-protocol "\
Inter-agent mail on this Emacs's ghostherd sidecar.

Environment:
  GHOSTHERD_SESSION  this agent's name
  GHOSTHERD_RPC      POST JSON-RPC 2.0 here; the URL carries a token,
                     so use the variable and do not print its value
  rpc.url            same URL, in this directory, if the env is stale
                     (a 401 means it is: the sidecar now wants a token)

curl -sS \"$GHOSTHERD_RPC\" -H 'Content-Type: application/json' \\
  -d '{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"herd_list\",\"params\":{}}'

{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"herd_message\",\"params\":{
   \"from\":\"<GHOSTHERD_SESSION>\",
   \"to\":\"grok-dev\",
   \"body\":\"Findings:\\n- ...\",
   \"handoff\":true}}

Emacs pastes into the target PTY.  If that agent is working, the
sidecar holds the message until it is idle.  Overlay m does not
use this queue.  Long bodies: method herd_inbox, params session=<name>.

Asks want an answer back.  $GHOSTHERD_HERD is a client for them:

  \"$GHOSTHERD_HERD\" ask agy 'Fetch X and sum it up' --wait
  \"$GHOSTHERD_HERD\" reply <id> 'the answer'

Text given as `-', or left out, is read from stdin.

`to' is a name, or a kind (claude, grok, agy) for that kind's agent
in your project.  --wait prints the answer when it comes; without it
the answer comes back to you as a message once you are idle.  An
agent answering an ask may hand parts of it on, three asks deep at
most, but never to one waiting on it; a kind never picks a project's
lead.  And it may ask any one agent only so many times for it: what
the asker gave as --rounds, 5 when it gave none -- one nobody asked,
until it finishes a turn with no ask open.  Methods: herd_ask {to,
body, from, wait, rounds}, herd_reply {id, body, from}, herd_await {id,
timeout}, herd_cancel {id}, herd_asks.

  \"$GHOSTHERD_HERD\" handoff unnoiac 'what unnoiac must change, and why' --wait

A handoff is an ask of another project's lead, PROJECT by path, name or
store slug; the herd frames it, and starts a lead there when it has
none.  A lead hands off, or an agent in a project with no lead; one
under a lead is refused and tells its lead.  Method: herd_handoff
{project, body, from, wait, rounds}.

`primary' is the herd-wide lead: the user's entry point for what crosses
projects.  It hands work off as above; no agent asks it -- the herd
refuses -- and it asks no project's agents.

Hooks: `\"$GHOSTHERD_HERD\" hook [EVENT]' reads a CLI hook's JSON on
stdin and posts herd_report {session, state, reason, cli,
conversation, transcript, last}.  `herd hooks install' wires it in.
")

;;;###autoload
(defun ghostherd-install-hooks (&optional uninstall)
  "Wire `herd hook' into claude, grok and agy, after showing the change.
claude and grok share ~/.claude/settings.json; agy reads
~/.gemini/config/hooks.json.  The commands do nothing in a CLI the
herd did not start, so every session of each CLI can carry them.
With prefix UNINSTALL, take them out again."
  (interactive "P")
  (let* ((client (or (and (fboundp 'ghostherd-herd-client) (ghostherd-herd-client))
                     (user-error "No bin/herd next to ghostherd")))
         (action (if uninstall "uninstall" "install"))
         (run (lambda (&rest args)
                (with-temp-buffer
                  (cons (apply #'call-process client nil t nil "hooks" action args)
                        (string-trim (buffer-string)))))))
    (pcase-let ((`(,status . ,plan) (funcall run "--dry-run")))
      (unless (eq status 0)
        (user-error "herd hooks %s: %s" action plan))
      (with-help-window "*ghostherd hooks*"
        (princ plan))
      (when (yes-or-no-p (format "%s ghostherd's hooks as shown? " (capitalize action)))
        (pcase-let ((`(,status . ,out) (funcall run)))
          (message "%s%s" out (if (eq status 0) "" "  (failed)")))))))

(defun ghostherd--herd-write-protocol ()
  "Refresh PROTOCOL.md next to rpc.url."
  (let ((dir (expand-file-name "ghostherd-mail" user-emacs-directory)))
    (make-directory dir t)
    (let ((coding-system-for-write 'utf-8))
      (with-temp-file (expand-file-name "PROTOCOL.md" dir)
        (insert ghostherd--herd-protocol)))))

(defun ghostherd--herd-snapshot ()
  "Session list pushed to the sidecar each poll.
Includes the last captured pane so Telegram `screen' can reply
from sqlite instead of waiting for another Emacs round trip, and the
address of each one's room from your phone, for the bot to link to."
  (mapcar
   (lambda (s)
     (let* ((blocked (eq (ghostherd-session-state s) 'blocked))
            (err (and blocked (ghostherd--cli-error s)))
            (url (and blocked (ghostherd--login-url s)))
            (signed-in (and blocked (ghostherd--login-done s)))
            (continue-at (and blocked (ghostherd--sleep-continue-at s)))
            (words (and blocked (ghostherd--continue-words s)))
            (wstat (and (ghostherd-session-worktree s) (ghostherd--worktree-stat s))))
      (list :name (ghostherd-session-name s)
           :kind (symbol-name (or (ghostherd-session-kind s) 'shell))
           :state (symbol-name (or (ghostherd-session-state s) 'idle))
           :notes (or (ghostherd-session-notes s) "")
           :project (or (ghostherd--project-name (ghostherd-session-project s)) "")
           :room (or (when-let* ((p (ghostherd--project-name (ghostherd-session-project s))))
                       (ghostherd--room-url p))
                     "")
           :role (or (ghostherd-session-role s) "")
           :reason (or (ghostherd-session-state-reason s) "")
           :since (let ((t0 (or (ghostherd-session-last-active s)
                                (ghostherd-session-started-at s))))
                    (if t0 (* 1000 (float-time t0)) 0))
           :prompt (ghostherd--prompt-json (ghostherd-session-prompt s))
           ;; A turn killed by an expired login: the card offers a one-click
           ;; /login.  Only while blocked on it, so the button is there
           ;; exactly when it would work -- not once /login is under way,
           ;; where it would be typed as the code or eat the last Return.
           :login (if (and (eq (cdr err) 'login) (not url) (not signed-in)) t :json-false)
           ;; /login's last step, signed in: the card offers the Return it
           ;; waits on (`ghostherd-login-continue').
           :loginContinue (if signed-in t :json-false)
           ;; Signed in again, its turn still stopped at the expired login:
           ;; the card offers Continue (`ghostherd-carry-on') -- once the
           ;; sign-in's last Return is pressed.
           :carryOn (if (and (eq (cdr err) 'carry-on) (not signed-in)) t :json-false)
           ;; A turn killed by the Mac sleeping or a dropped connection: the
           ;; herd continues it itself.  `continueAt' is when (float ms), for
           ;; the card's "continuing after sleep in Ns" countdown and its
           ;; Cancel; `continueStopped' is t once it has stopped retrying.
           ;; `continueAfter' / `continueAgain' are the cause's words for
           ;; each (`ghostherd--continue-causes').
           :continueAt (if continue-at (* 1000 continue-at) 0)
           :continueStopped (if (ghostherd--sleep-continue-stopped-p s) t :json-false)
           :continueAfter (or (car words) "")
           :continueAgain (or (cdr words) "")
           ;; The /login URL to open/copy from the card, while it waits on
           ;; the code.
           :url (or url "")
           ;; Where it works and the branch it is on, so the lead can find
           ;; and read its work.  A worktree agent is in its own directory
           ;; on its own branch (from the recipe, no per-poll git); the
           ;; others share the project's, so their branch is left blank.
           :directory (let ((d (or (plist-get (ghostherd-session-worktree s) :path)
                                   (ghostherd-session-project s))))
                        (if d (ghostherd--abbreviate d) ""))
           :branch (or (plist-get (ghostherd-session-worktree s) :branch) "")
           ;; That directory is gone -- a worktree removed outside the herd:
           ;; `herd list' says so, for its lead to stop it.
           :directoryMissing (if (ghostherd-session-directory-gone s) t :json-false)
           ;; Who started it through `herd start', and where its worktree
           ;; branch stands -- for `herd list' and the card's "started by".
           :startedBy (or (ghostherd-session-started-by s) "")
           :ahead (if wstat (plist-get wstat :ahead) 0)
           :merged (if (and wstat (plist-get wstat :merged)) t :json-false)
           ;; The process it lives under -- its tmux pane's, its ghostel
           ;; shell -- so the herd takes a hook's report as its own only
           ;; from inside it, not from a CLI a server it started runs.
           :hostPid (or (ghostherd--host-pid s) 0)
           ;; Behind the herd's guard: what it asks of others is marked so.
           :guarded (if (ghostherd--session-guarded-p s) t :json-false)
           :screen (or (gethash (ghostherd-session-id s) ghostherd--screens)
                       ""))))
   (hash-table-values ghostherd--sessions)))

(defun ghostherd--herd-deliver-one (msg)
  "Paste one sidecar mail MSG through `ghostherd-message' / `handoff'.
Mail is not the user's prompt -- an agent's message, the herd's note --
so the turn it starts is marked `mail' (`ghostherd--turn-from-ask'),
and its end is no report to the user.  Save what the sidecar says
otherwise (its `turn'): \"user\", the user's own words (a prompt from
Telegram), is the user's turn as any prompt is; \"carry\", the answer to
an ask the taker made or the user's verdict on a call its guard
refused, carries on the work it was on, and its turn is whoever's the
last one was."
  (let* ((from (or (plist-get msg :from) "unknown"))
         (to (plist-get msg :to))
         (body (or (plist-get msg :body) ""))
         (handoff (plist-get msg :handoff))
         (ask (plist-get msg :ask))
         (turn (plist-get msg :turn))
         (submit (if (plist-member msg :submit)
                     (plist-get msg :submit)
                   t)))
    ;; Before the paste, which moves the taker to `working' at once (see
    ;; `ghostherd--ask-deliver'); none when it is not submitted -- the
    ;; Return pressed on it later is someone's own -- nor from a sidecar
    ;; that does not say yet (no `turn'; until it is restarted).
    (when (and (not ask) (or handoff submit) (plist-member msg :turn)
               (not (equal turn "user")))
      (ghostherd--turn-from-ask to (if (equal turn "carry")
                                       (or (ghostherd--turn-origin-of to) 'mail)
                                     'mail)))
    (cond
     (ask (ghostherd--ask-deliver ask from to body (plist-get msg :rounds)
                                  (eq (plist-get msg :guarded) t)))
     (handoff (ghostherd-handoff to body from))
     (t (ghostherd-message from to body :submit submit)))))

(defun ghostherd--herd-held-p (msg)
  "Non-nil when MSG's taker cannot take a paste now: starting, on a menu,
or with no input box at its foot.  A menu -- a trust prompt, a
permission prompt -- takes no paste, and reads the Return pressed after
it as its answer.  The sidecar holds mail by the state Emacs last
ticked; this is for a state that changed since, and for a dialog no
rule reads: the box missing is enough (`ghostherd--off-prompt-p')."
  (when-let* (((not (plist-get msg :handoff)))
              (to (ghostherd-get (plist-get msg :to))))
    (or (memq (ghostherd-session-state to) '(starting blocked))
        (ghostherd--off-prompt-p to))))

(defun ghostherd--herd-deliver (result)
  "Deliver pending mail from a `herd_tick' RESULT plist.
Mail `ghostherd--herd-held-p' holds is not acknowledged: it comes again
on a later tick."
  (dolist (msg (plist-get result :pending))
    (let ((id (plist-get msg :id)))
      (unless (ghostherd--herd-held-p msg)
        (condition-case err
            (ghostherd--herd-deliver-one msg)
          (error
           (message "ghostherd herd mail %s: %s"
                    id (error-message-string err))))
        (when id
          (push id ghostherd--herd-ack-ids))))))


;;; Asks: mail that wants an answer
;;
;; The sidecar keeps the ask; Emacs pastes it, with the line that
;; answers it, and closes it when the taker stops without answering.
;; A taker's own `herd reply' happens before it goes idle -- it is a
;; command it runs during its turn -- so by the time a settled state
;; reaches Emacs a real answer is already in, and the sidecar keeps it.

(defcustom ghostherd-ask-screen-lines 60
  "Screen lines sent back as the answer when a taker forgets to answer."
  :type 'integer
  :group 'ghostherd)

(defvar ghostherd--asks-open (make-hash-table :test 'equal)
  "Asks delivered and not yet closed: ask id -> taker's session id.")

(defvar ghostherd--asks-settled (make-hash-table :test 'equal)
  "Ask ids this Emacs has closed, so a stale tick does not reopen them.")

(defvar ghostherd--relay-held (make-hash-table :test 'equal)
  "Ask id -> the name whose heir answers it, until the heir is told:
see `ghostherd-relay'.  Not open meanwhile: an heir sitting at its first
prompt has not stopped answering it.")

(defvar ghostherd--herd-asks nil
  "Open and recent asks from the last `herd_tick', newest first.")

(defvar ghostherd--herd-asks-taken nil
  "Non-nil once a `herd_tick' has given this Emacs the herd's asks: an
ask new after that is new to the herd, not to this Emacs.")

(defun ghostherd--ask-head (text)
  "First non-blank line of TEXT, shortened for the log."
  (let ((line (or (seq-find (lambda (l) (not (string-blank-p l)))
                            (split-string (or text "") "\n"))
                  "")))
    (truncate-string-to-width (string-trim line) 80 nil nil "…")))

(defun ghostherd--ask-text (id body &optional rounds lead)
  "BODY as the taker sees ask ID: the work, then how to answer it.
The answer line is spelled out in full, the client by its absolute
name, so a taker needs no skill installed and no PATH of ours.
ROUNDS, when the sidecar says, is how many times the taker may ask
any one agent for it.  Last, what to do with work another project
needs: a LEAD hands it to that project's lead; any other taker puts it
in its answer, for whoever asked to take on -- neither edits that
project's checkout."
  (format "[ask %s -- whoever asked is waiting for your answer]\n%s\n\n\
When you are done, send your answer with:\n\
%s reply %s <<'HERD'\n\
<your answer>\n\
HERD\n\
Answer even if you could not do it, and say why.  You may ask other\n\
agents for parts of it, but not whoever is waiting on you%s.\n\
%s"
          id body
          (shell-quote-argument (or (and (fboundp 'ghostherd-herd-client)
                                         (ghostherd-herd-client))
                                    "herd"))
          id
          (if (natnump rounds)
              (format ",\nand any one of them %d time%s at most" rounds (if (= rounds 1) "" "s"))
            "")
          (if lead
              "A change another project needs is not yours to make in its checkout:\n\
hand it to that project's lead (\"$GHOSTHERD_HERD\" handoff PROJECT)."
            "A change another project needs is not yours to make in its checkout:\n\
say it in your answer -- what, where, why -- and whoever asked takes it on.")))

;;; How a turn began: the user's own prompt, or another agent's ask.
;;
;; A worker's turn that the user started -- their prompt from the card,
;; the room or chat composer, Telegram, or typed straight into the
;; terminal -- ends as an Inbox item: its report to the user (a Question
;; when it ends by asking, else a Report).  A turn that an ask from
;; another agent started stays out: whoever asked reports.  So does one
;; the herd's other mail started -- an agent's message, the herd's note
;; that an ask was cancelled -- save the user's own words it carries
;; (`ghostherd--herd-deliver-one').  The herd knows a turn is from its
;; own delivery (`ghostherd--ask-deliver', `ghostherd--herd-deliver-one');
;; every other turn start is the user's.  Leads and primary are
;; unaffected -- their turn end is surfaced by role, as before.

(defvar ghostherd--turn-origin (make-hash-table :test 'equal)
  "Session id -> how its current or last turn began: `ask' (the herd
delivered it another agent's ask), `mail' (the herd's other mail) or
`user' (the user's own prompt).  Read to decide whether a worker's turn
end is its report to the user.")

(defvar ghostherd--turn-ask-pending (make-hash-table :test 'equal)
  "Session id -> the origin the turn the herd's delivery starts takes
(`ask', `mail', or a turn's origin carried on), until that turn is
settled; (submitted . ORIGIN) once its CLI has the prompt.  The next
turn start is marked so.")

(defun ghostherd--turn-from-ask (session &optional origin)
  "Mark that SESSION's next turn begins from an ask the herd delivered --
or, with ORIGIN, from other mail it delivered, marked ORIGIN."
  (when-let* ((s (ghostherd-get session)))
    (puthash (ghostherd-session-id s) (or origin 'ask) ghostherd--turn-ask-pending)))

(defun ghostherd--turn-pending-origin (pending)
  "The origin PENDING, a `ghostherd--turn-ask-pending' value, gives its
turn (t, from before the table held one, is an ask's)."
  (cond ((consp pending) (cdr pending))
        ((memq pending '(t submitted)) 'ask)
        (t pending)))

(defun ghostherd--turn-origin-of (session)
  "How SESSION's current or last turn began: `ask', `user', or nil."
  (when-let* ((s (ghostherd-get session)))
    (gethash (ghostherd-session-id s) ghostherd--turn-origin)))

(defun ghostherd--turn-origin-on-state (session old new)
  "Settle SESSION's turn origin as a turn starts (OLD resting, NEW busy).
A turn the herd started by delivering an ask (`ghostherd--turn-ask-pending')
is `ask', one its other mail started `mail' (or the origin it carries
on); any other turn start -- a user prompt through the herd, or one
typed straight into the terminal -- is `user'.  A mid-turn move into a
busy state (OLD already busy) is not a turn start and is left alone, so
an ask turn that blocks to ask is not re-marked.

The ask's flag is spent only once its prompt is known to have reached
the CLI (`ghostherd--turn-submitted').  The herd moves a taker to
`working' the moment it sends; when the Return was lost, that was no
turn, and the real one starts later -- once Return is pressed again,
by the herd or by hand -- and is still the ask's."
  (when (and (memq new '(working blocked waiting))
             (memq old '(idle done dead nil)))
    (let* ((id (ghostherd-session-id session))
           (pending (gethash id ghostherd--turn-ask-pending)))
      (if pending
          (progn (puthash id (ghostherd--turn-pending-origin pending) ghostherd--turn-origin)
                 (when (or (consp pending) (eq pending 'submitted)
                           (not (ghostherd--submit-pending-p session)))
                   (remhash id ghostherd--turn-ask-pending)))
        (puthash id 'user ghostherd--turn-origin)))))

(defun ghostherd--turn-submitted (session)
  "SESSION's CLI took the prompt the herd sent it: an ask's flag is spent
on the turn it starts -- the one under way, else the next to start."
  (let* ((id (ghostherd-session-id session))
         (pending (gethash id ghostherd--turn-ask-pending))
         (origin (and pending (ghostherd--turn-pending-origin pending))))
    (when pending
      (if (memq (ghostherd-session-state session) '(working blocked waiting))
          (progn (remhash id ghostherd--turn-ask-pending)
                 (puthash id origin ghostherd--turn-origin))
        (puthash id (cons 'submitted origin) ghostherd--turn-ask-pending)))))

(add-hook 'ghostherd-state-change-hook #'ghostherd--turn-origin-on-state)

(defun ghostherd--turn-origin-forget (session)
  "Drop SESSION's turn-origin record when it is gone."
  (let ((id (ghostherd-session-id session)))
    (remhash id ghostherd--turn-origin)
    (remhash id ghostherd--turn-ask-pending)))

(add-hook 'ghostherd-session-removed-hook #'ghostherd--turn-origin-forget)

(defun ghostherd--ask-deliver (id from to body &optional rounds guarded)
  "Paste ask ID from FROM into TO, and watch for TO to stop.
ROUNDS is how many times TO may ask any one agent for it.  GUARDED: FROM
runs behind the herd's guard -- BODY says so to TO, and the log does.  A lead owed
its role -- started for a handoff on a CLI that takes none as it starts
-- reads it after the ask: before it, the ask would not read as one in
the room's chat (server/conversation.py), which stops at its answer
line."
  (let* ((session (or (ghostherd-get to)
                      (user-error "Unknown target session")))
         (role (ghostherd--role-take session)))
    ;; BEFORE the message: sending it moves the taker to `working'
    ;; synchronously (`ghostherd-send'), which runs the turn-origin hook --
    ;; set after, the hook would see no pending ask and mark the turn the
    ;; user's.  The turn this ask starts is from an ask, not the user: its
    ;; end is not the taker's report to the user (whoever asked reports).
    (ghostherd--turn-from-ask session)
    (ghostherd-message from session
                       (concat (ghostherd--ask-text id body rounds
                                                    (equal (ghostherd-session-role session) "lead"))
                               (and role (concat "\n\n---\n\nYour role in this herd, given with \
your first message as your CLI takes none as it starts:\n\n" role)))
                       :submit t)
    (puthash id (ghostherd-session-id session) ghostherd--asks-open)
    (ghostherd--log-add session 'ask
                        (format "ask %s from %s%s: %s" id from
                                (if guarded " (a guarded agent)" "")
                                (ghostherd--ask-head
                                 (if guarded
                                     (replace-regexp-in-string "\\`\\[From a guarded agent:[^]]*\\]\\s-*" "" body)
                                   body))))))

(defun ghostherd--ask-settle (id session dead &optional error)
  "Tell the sidecar the taker of ask ID stopped, or died when DEAD.
With ERROR, it never got the ask, and that says why.  The sidecar
ignores this for an ask already answered, and leaves it open while the
taker waits on asks of its own: it ended its turn to wait, not to stop.
Such an ask is watched again, for the next time it stops."
  (remhash id ghostherd--asks-open)
  (puthash id t ghostherd--asks-settled)
  (let ((screen (and (not dead) (not error)
                     (ignore-errors
                       (ghostherd--host-capture session ghostherd-ask-screen-lines)))))
    (when (fboundp 'ghostherd-memory-request-async)
      (ghostherd-memory-request-async
       "herd_settle"
       (lambda (r)
         (when (equal (plist-get r :status) "delivered")
           (remhash id ghostherd--asks-settled)
           (puthash id (ghostherd-session-id session) ghostherd--asks-open)
           ;; Watched again: `too_soon' is the sidecar holding it open until
           ;; the taker has really worked on it after delivery -- the next
           ;; stop, once it has, closes it.
           (ghostherd--log-add session 'ask
                               (cond
                                ((plist-get r :too_soon)
                                 (format "ask %s left open: %s was only just handed it"
                                         id (ghostherd-session-name session)))
                                ;; Its screen read idle, but its hook has not
                                ;; said the turn ended (nor its transcript):
                                ;; a view scrolled back, say.  Its turn's end
                                ;; closes it.
                                ((plist-get r :turn_on)
                                 (format "ask %s left open: %s's turn has not ended by its hook or transcript"
                                         id (ghostherd-session-name session)))
                                ;; The sidecar holds a failure until this Emacs
                                ;; has picked its herd up, then is asked again.
                                ((plist-get r :unrestored)
                                 (format "ask %s left open: the herd is not restored yet" id))
                                (t
                                 (format "ask %s left open: %s waits on %s"
                                         id (ghostherd-session-name session)
                                         (mapconcat #'identity (plist-get r :waiting_on) ", ")))))))
       (append (list :id id
                     :screen (string-trim-right (or screen ""))
                     :dead (if dead t :json-false))
               (and error (list :error error)))
       (lambda (err) (message "ghostherd: closing ask %s: %s" id err))))))

(defun ghostherd--ask-in (session text)
  "The open ask SESSION was handed in TEXT, or nil."
  (seq-find (lambda (id) (string-search (format "[ask %s " id) text))
            (ghostherd--asks-of session)))

(defun ghostherd--asks-of (session)
  "Ids of the open asks SESSION is answering."
  (let ((sid (ghostherd-session-id session))
        ids)
    (maphash (lambda (id taker) (when (equal taker sid) (push id ids)))
             ghostherd--asks-open)
    ids))

(defun ghostherd--ask-unsent-p (session)
  "Non-nil when an ask still sits unsent in SESSION's input box."
  (when-let* ((screen (ignore-errors (ghostherd--host-capture session 20))))
    (ghostherd--unsent-p screen (substring ghostherd-message-template 0
                                          (string-search "%s" ghostherd-message-template)))))

(defun ghostherd--ask-not-taken (session)
  "The open ask in the text the herd last sent SESSION, while that text
is not known taken: no turn begun on it yet, of an agent whose hooks
say when one does -- the submit check waits for it, and ends by
confirming the text or failing the ask (`ghostherd--confirm-submit')."
  (when-let* ((sub (ghostherd--submit-of session))
              ((eq (plist-get sub :state) 'pending))
              ((ghostherd--hooked-p session)))
    (ghostherd--ask-in session (plist-get sub :text))))

(defun ghostherd--ask-on-state (session _old new)
  "A taker that stopped closes its open ask -- unless it never started.
Delivery marks the taker `working' itself; when the Return was lost the
ask is still in its input box, and the taker goes idle without having
read it.  Its screen then answers nothing, so the ask stays open.  So
too while the text that carried it is not known taken: the submit check
settles that, failing the ask if no turn ever began on it."
  (when (memq new '(idle done dead))
    (let ((ids (ghostherd--asks-of session)))
      (when-let* (((not (eq new 'dead)))
                  (id (ghostherd--ask-not-taken session))
                  ((member id ids)))
        (ghostherd--log-add session 'ask
                            (format "ask %s left open: %s has begun no turn on it yet"
                                    id (ghostherd-session-name session)))
        (setq ids (delete id ids)))
      (when (and ids (not (eq new 'dead)) (ghostherd--ask-unsent-p session))
        (ghostherd--log-add session 'ask
                            (format "ask %s is still in its input box: left open"
                                    (string-join ids ", ")))
        (setq ids nil))
      (dolist (id ids)
        (ghostherd--ask-settle id session (eq new 'dead))))))

(defun ghostherd--ask-on-removed (session)
  "A taker killed outright fails its open ask."
  (dolist (id (ghostherd--asks-of session))
    (ghostherd--ask-settle id session t)))

(add-hook 'ghostherd-state-change-hook #'ghostherd--ask-on-state)
(add-hook 'ghostherd-session-removed-hook #'ghostherd--ask-on-removed)

(defun ghostherd--ask-outcome (ask)
  "Log text for ASK, a `herd_tick' plist that has just closed."
  (let ((id (plist-get ask :id))
        (from (plist-get ask :from)))
    (pcase (plist-get ask :status)
      ("answered" (if (plist-get ask :auto)
                      (format "stopped without answering ask %s; %s got %s"
                              id from
                              (if (equal (plist-get ask :auto_source) "message")
                                  "its last message"
                                "the screen"))
                    (format "answered ask %s for %s: %s" id from
                            (or (plist-get ask :reply_head) ""))))
      ("failed" (format "ask %s from %s failed: %s" id from
                        (or (plist-get ask :error) "")))
      (status (format "ask %s from %s %s" id from status)))))

(defun ghostherd--herd-take-asks (asks)
  "Keep ASKS from a `herd_tick': log what closed, adopt what is ours.
Adopting is for asks delivered before this Emacs started: watched
from here on, and closed now if the taker is already sitting idle."
  (let ((before (make-hash-table :test 'equal)))
    (dolist (a ghostherd--herd-asks)
      (puthash (plist-get a :id) (plist-get a :status) before))
    (dolist (a asks)
      (let* ((id (plist-get a :id))
             (status (plist-get a :status))
             (was (gethash id before)))
        (when (and was (not (equal was status))
                   (member status '("answered" "failed" "cancelled")))
          (remhash id ghostherd--asks-open)
          ;; A handoff that failed before it had a taker: its asker's.
          (ghostherd--log-add (if (equal (plist-get a :to) "") (plist-get a :from) (plist-get a :to))
                              'ask (ghostherd--ask-outcome a)))
        ;; Failed since the last tick -- or before it, when the ask is new
        ;; to a herd that has seen asks already, not at Emacs's start.
        (when-let* (((equal status "failed"))
                    ((not (equal was "failed")))
                    ((or was ghostherd--herd-asks-taken))
                    (asker (ghostherd-get (plist-get a :from))))
          (ghostherd--push 'ask-failed asker :ask id
                           :to (let ((to (or (plist-get a :to) "")))
                                 (if (and (equal to "") (plist-get a :project))
                                       (format "%s's lead" (file-name-nondirectory
                                                            (directory-file-name (plist-get a :project))))
                                     to))
                           :error (or (plist-get a :error) "") :head (or (plist-get a :head) "")))
        (when-let* (((equal status "delivered"))
                    ((not (gethash id ghostherd--asks-open)))
                    ((not (gethash id ghostherd--asks-settled)))
                    ;; Its taker's heir is not told it yet: at its first
                    ;; prompt, it has not stopped answering it.
                    ((not (gethash id ghostherd--relay-held)))
                    (session (ghostherd-get (plist-get a :to))))
          (puthash id (ghostherd-session-id session) ghostherd--asks-open)
          (when (memq (ghostherd-session-state session) '(idle done dead))
            (ghostherd--ask-settle id session
                                   (eq (ghostherd-session-state session) 'dead))))))
    (setq ghostherd--herd-asks asks
          ghostherd--herd-asks-taken t)))

(provide 'ghostherd-mail)

;; Local Variables:
;; generated-autoload-load-name: "ghostherd"
;; End:

;;; ghostherd-mail.el ends here

;;; ghostherd-hooks.el --- What agents say through CLI hooks; the agy guard -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of ghostherd (see ghostherd.el).  What agents say about
;; themselves through their CLI's hooks -- links, transcripts,
;; conversations -- and the guard of an agy agent, its PreToolUse hook.

;;; Code:

(require 'ghostherd-core)
(require 'ghostherd-log)
(require 'ghostherd-spawn)
(require 'ghostherd-sidebar)
(require 'ghostherd-mail)

(declare-function ghostherd--primary-remember "ghostherd-primary" ())
(declare-function ghostherd--push "ghostherd-push" (kind session &rest fields))
(declare-function ghostherd--relay-kinds "ghostherd-primary" ())
(declare-function ghostherd-relay "ghostherd-primary" (session kind &optional reason))
(declare-function ghostherd-relay-default-kind "ghostherd-primary" (session))

;;; What agents say about themselves, through their CLI's hooks
;;
;; `herd hook' runs in claude's, grok's and agy's lifecycle hooks and
;; posts to the sidecar: the state the agent is in, the conversation it
;; is on, and the answer that ended its turn.  States come here on the
;; tick and go through `ghostherd-report', so they keep its TTL and its
;; place in the authority order.  The rest stays in the sidecar, and a
;; copy of it rides each tick.

(defvar ghostherd--herd-links (make-hash-table :test 'equal)
  "Session name -> what its hooks last said, from the last `herd_tick'.
A plist of :cli, :conversation, :transcript, :last_head (first line of
its last answer) and :last_ms.  Keyed by name because that is what the
agent's environment carries.")

(defun ghostherd--herd-take-reports (reports)
  "Apply REPORTS from a `herd_tick': each agent's own word on its state."
  (dolist (r reports)
    (let ((session (ghostherd-get (plist-get r :session)))
          (state (intern-soft (or (plist-get r :state) "")))
          (reason (plist-get r :reason)))
      (when (and session (or (eq state 'auto) (memq state ghostherd-report-states)))
        (ghostherd-report session state
                          (and reason (not (string-empty-p reason)) reason))
        ;; Its turn's end, by its hook, while its screen already reads it
        ;; stopped: no change of state follows to settle its ask on, and
        ;; the sidecar left that ask open for exactly this.
        (when (and (memq state '(done idle auto))
                   (memq (ghostherd-session-state session) '(idle done)))
          (ghostherd--ask-on-state session nil (ghostherd-session-state session)))))))

(defun ghostherd--herd-take-links (links)
  "Keep LINKS from a `herd_tick', replacing the last set."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (l links)
      (puthash (plist-get l :session) l table))
    (setq ghostherd--herd-links table))
  (ignore-errors (ghostherd--primary-remember)))

(defun ghostherd--link (session)
  "What SESSION's hooks last said, or nil.
Not what another CLI's said under its name: the agent it took over
from (`ghostherd-relay'), whose conversation is not its own."
  (when-let* ((s (ghostherd-get session))
              (link (gethash (ghostherd-session-name s) ghostherd--herd-links)))
    (let ((cli (plist-get link :cli)))
      (and (or (not (stringp cli)) (string-empty-p cli)
               (equal cli (symbol-name (or (ghostherd-session-kind s) 'shell))))
           link))))

(defun ghostherd--turn-began-since (session at)
  "Non-nil when SESSION's hooks said a turn began after AT, a float time:
the sidecar keeps when the last one did (its link's `:turn_ms'), which
no later report folds away."
  (let ((ms (plist-get (ghostherd--link session) :turn_ms)))
    (and (numberp ms) (numberp at) (> ms (* 1000 at)))))

(defun ghostherd--hooked-p (session)
  "Non-nil when SESSION's own CLI's hooks report to the herd: they named
it.  A turn it begins is then said, not only drawn."
  (let ((cli (plist-get (ghostherd--link session) :cli)))
    (and (stringp cli) (not (string-empty-p cli)))))

(defun ghostherd-session-conversation (session)
  "The conversation SESSION is on, as its own hooks named it, or nil."
  (let ((id (plist-get (ghostherd--link session) :conversation)))
    (and (stringp id) (not (string-empty-p id)) id)))

(defun ghostherd--args-conversation (kind args)
  "The conversation ARGS start KIND on, by its resume flag, or nil.
Nil for a fork: that writes a conversation of its own, whose id only
its hooks can name."
  (let* ((spec (ignore-errors (ghostherd--spec kind)))
         (flag (car (plist-get spec :resume-args)))
         (id (and flag (cadr (member flag args)))))
    (and (stringp id)
         (string-match-p ghostherd--session-id-re id)
         (not (seq-some (lambda (f) (member f args)) (plist-get spec :fork-args)))
         id)))

(defconst ghostherd--transcript-retry 30
  "Seconds before looking again for a transcript that was not there.")

(defvar ghostherd--transcripts (make-hash-table :test 'equal)
  "(KIND . ID) -> (PATH-or-nil . WHEN): where a conversation was found.")

(defun ghostherd-session-transcript (session)
  "The transcript file SESSION writes, or nil when it is not known.
Its hooks name it.  Failing that, the conversation it was started on
does, for a kind whose spec says where (`:transcript').  A fresh agent
without hooks has no name for its conversation yet, so none."
  (when-let* ((s (ghostherd-get session)))
    (let ((linked (plist-get (ghostherd--link s) :transcript)))
      (if (and (stringp linked) (not (string-empty-p linked)) (file-readable-p linked))
          linked
        (let* ((kind (ghostherd-session-kind s))
               (id (or (ghostherd-session-conversation s)
                       (ghostherd--args-conversation kind (ghostherd-session-args s))))
               (place (plist-get (ignore-errors (ghostherd--spec kind)) :transcript))
               (key (cons kind id))
               (seen (gethash key ghostherd--transcripts)))
          (when (and id place (string-match-p ghostherd--session-id-re id))
            (if (and seen (if (car seen)
                              (file-exists-p (car seen))
                            (< (- (float-time) (cdr seen)) ghostherd--transcript-retry)))
                (car seen)
              (let ((found (car (sort (file-expand-wildcards
                                       (expand-file-name (format place id)) t)
                                      #'file-newer-than-file-p))))
                (puthash key (cons found (float-time)) ghostherd--transcripts)
                found))))))))

(defun ghostherd--without-resume (kind args)
  "ARGS less any flag that picks a conversation for KIND.
A respawn onto a known conversation adds its own; the recipe's
`--continue', or the `-r' of the conversation it was first started
on, would contradict it."
  (let* ((spec (ignore-errors (ghostherd--spec kind)))
         (resume (car (plist-get spec :resume-args)))
         (drop (append (plist-get spec :continue-args) (plist-get spec :fork-args)))
         out)
    (while args
      (let ((a (pop args)))
        (cond ((and resume (equal a resume)) (pop args))
              ((member a drop))
              (t (push a out)))))
    (nreverse out)))

(defun ghostherd--explain-text (session)
  "One-screen explanation of SESSION state, for Telegram."
  (setq session (ghostherd-get session))
  (format "%s %s\n%s"
          (ghostherd--state-glyph (ghostherd-session-state session))
          (ghostherd-session-state session)
          (or (ghostherd-session-state-reason session) "—")))

(defun ghostherd--relay-request (by target kind reason)
  "Relay agent TARGET to KIND at the request of agent BY, with REASON, as
`herd relay' asks over the wire.  The guardrails a page button or Telegram
tap do not need: BY must be the lead of TARGET's own project, may not relay
itself, and neither it nor TARGET may already be in a relay -- which also
stops a relay from looping back on the one that asked for it.  KIND empty
takes the herd's default for TARGET.  Returns what to tell BY: the relay
started, or, clearly, why it was refused.

A finer \"BY is waiting on an ask from TARGET\" guard is left to the ask
machinery: a relay hands TARGET's open asks to its heir, so BY still gets
its answer rather than losing it."
  (let* ((by-s (ghostherd-get by))
         (tgt (ghostherd-get target)))
    (cond
     ((not by-s) (format "no such agent: %s" by))
     ((not tgt) (format "no such agent: %s" target))
     ((eq by-s tgt) "an agent cannot relay itself")
     ((not (equal (ghostherd-session-role by-s) "lead"))
      (format "only a project's lead may relay; %s is not a lead" by))
     ((let ((p (ghostherd--project-name (ghostherd-session-project by-s))))
        ;; A real, shared project -- not two project-less sessions reading
        ;; equal because both are nil.
        (not (and p (equal p (ghostherd--project-name (ghostherd-session-project tgt))))))
      (format "%s may only relay agents of its own project, not %s" by target))
     ((gethash (ghostherd-session-id by-s) ghostherd--relaying)
      (format "%s is being relayed itself; wait for that to finish" by))
     ((gethash (ghostherd-session-id tgt) ghostherd--relaying)
      (format "%s is already being relayed" target))
     (t
      (let ((k (if (and (stringp kind) (not (string-empty-p kind)))
                   (intern kind)
                 (ghostherd-relay-default-kind tgt))))
        (if (not (memq k (ghostherd--relay-kinds)))
            (format "relay to what: %s is not a CLI the herd relays to"
                    (if (and (stringp kind) (not (string-empty-p kind))) kind k))
          (condition-case err
              (progn
                (ghostherd-relay tgt k (and (stringp reason) (not (string-empty-p reason)) reason))
                (format "relaying %s to %s" target k))
            (error (error-message-string err)))))))))


;;; The guard of an agy agent
;;
;; A herd agy agent runs with --dangerously-skip-permissions, and its guard
;; -- `herd guard', bin/herd_guard.py -- is the only gate: a PreToolUse hook
;; (from the hooks.json in `--add-dir <its guard dir>') that answers allow
;; or deny.  agy 1.4.5's own `allow' does not skip its prompt without that
;; flag, and with it `ask' does not prompt (ask 134ac578), so what wants the
;; user is a denial they approve once, for that exact call, on the agent's
;; card (ask 75362f01).  The agent also starts with secret-looking variables
;; gone from its environment (`herd scrub-exec'): what is not there cannot
;; leak.  The guard directory -- its policy, its approvals -- is one per
;; agent, under `ghostherd-guard-root', and the guard refuses the agent
;; every look at it.  agy runs its tools in its first workspace, which is
;; that --add-dir, not the agent's own: the guard sends each command it
;; allows to where it checked it (`cd WORKSPACE && ...'), refuses relative
;; file paths, and the agent's first message names its workspace.

(defcustom ghostherd-agy-guard 'auto
  "How a herd agy agent's tool calls are checked.
`auto': its workspace's reads and writes, read-only commands, the herd's
and the project's own commands run; secrets, the world outside the
workspace and what the guard does not know are denied with a reason --
those the user may approve once on the agent's card.  `read-only':
reads in the workspace and read-only commands only (jnotes' rules).
nil: no guard, agy prompting for each command as it does.  A project
sets its own in .dir-locals.el.  Takes effect when an agy agent starts
or is respawned."
  :type '(choice (const :tag "Auto (guarded)" auto)
                 (const :tag "Read-only (guarded)" read-only)
                 (const :tag "Off: agy prompts" nil))
  :group 'ghostherd)
(put 'ghostherd-agy-guard 'safe-local-variable (lambda (v) (memq v '(nil read-only auto))))

(defcustom ghostherd-guard-allow-commands nil
  "A project's own commands its guarded agents run without asking, as
prefixes: (\"npm test\" \"pytest\").  Set it per project in .dir-locals.el.
Their files are checked all the same; not in `read-only'.  They run code
the agent can change (its tests, package scripts), which the guard never
sees: allow only what you would let it run unwatched."
  :type '(repeat string)
  :group 'ghostherd)
(put 'ghostherd-guard-allow-commands 'safe-local-variable
     (lambda (v) (and (listp v) (seq-every-p #'stringp v))))

(defcustom ghostherd-guard-request-ttl 3600
  "Seconds a call a guarded agent was refused waits for the user before it
expires: by then the turn that wanted it is long over.  An agent's open
requests also clear when it is respawned or relaunched."
  :type 'natnum
  :group 'ghostherd)

(defcustom ghostherd-guard-root "~/.ghostherd/guard/"
  "Where each guarded agent's guard directory goes: <root>/<agent>/.
The sidecar writes the user's approvals there, and only there
(GHOSTHERD_GUARD_ROOT)."
  :type 'directory
  :group 'ghostherd)

(defcustom ghostherd-agent-env-allow
  '(;; Where it is, who it is, its terminal and language.
    "PATH" "HOME" "USER" "LOGNAME" "SHELL" "PWD" "TMPDIR" "TZ"
    "TERM" "TERM_*" "TERMINFO" "TERMINFO_DIRS" "COLORTERM" "CLICOLOR"
    "LANG" "LANGUAGE" "LC_*" "__CF_USER_TEXT_ENCODING"
    "EDITOR" "VISUAL" "PAGER" "XDG_*"
    ;; Who git says made a commit.
    "GIT_AUTHOR_NAME" "GIT_AUTHOR_EMAIL" "GIT_COMMITTER_NAME" "GIT_COMMITTER_EMAIL"
    ;; The way out: a proxy (one with a password in it is dropped all the
    ;; same), and ssh-agent, which signs and gives no key away -- and every
    ;; use of it (git push, ssh) asks the user first.
    "HTTP_PROXY" "HTTPS_PROXY" "NO_PROXY" "ALL_PROXY" "http_proxy" "https_proxy" "no_proxy" "all_proxy"
    "SSH_AUTH_SOCK"
    ;; Switches that keep what it runs from phoning home: onnxruntime 1.29
    ;; posts telemetry to Microsoft unless this is set when it loads.
    "ORT_DISABLE_TELEMETRY"
    ;; The herd's own: GHOSTHERD_RPC holds the agents' token, not the herd's.
    "GHOSTHERD_RPC" "GHOSTHERD_RPC_FILE" "GHOSTHERD_SESSION" "GHOSTHERD_HERD"
    "GHOSTHERD_SOCKET" "GHOSTHERD_BACKEND")
  "The variables a guarded agent starts with -- every other one is left
behind: an allowlist, so DATABASE_URL, SENTRY_DSN, a *_PAT or a webhook
URL never reach it, whatever their names.  Names or globs (LC_*).  agy
itself needs none of the rest: it logs in from ~/.gemini (measured on
1.4.5).  A value with a credential in a URL (http://user:pass@proxy) is
dropped even when its name is allowed.  Add names with
`ghostherd-agent-env-allow-extra'."
  :type '(repeat string)
  :group 'ghostherd)

(defcustom ghostherd-agent-env-allow-extra nil
  "More variables a guarded agent starts with, on top of
`ghostherd-agent-env-allow': yours, or a project's in its .dir-locals.el
-- which Emacs asks you about first, since a project could name a secret
here."
  :type '(repeat string)
  :group 'ghostherd)

(make-obsolete-variable 'ghostherd-agent-env-scrub 'ghostherd-agent-env-allow "2026-10-22")
(make-obsolete-variable 'ghostherd-agent-env-keep 'ghostherd-agent-env-allow-extra "2026-10-22")

(defconst ghostherd--agy-guard-note "\
A herd guard checks each of your tool calls.  Your workspace is %s and your
commands run there.  %s is the herd's guard, listed among your workspaces:
it is not yours -- leave it alone.  Reading and changing files in your
workspace and read-only commands run; anything else is denied, with a
reason saying what to do instead.  A denial that needs the user says so:
they can approve that exact call once on your card, and the herd tells you
when -- then run exactly that call again.  Do not look for another way
around a denial, and never read secrets (.env, keys, tokens, the
environment) without that approval.

"
  "What a guarded agy agent is told first, before its memory opening: a
format string, its workspace and its guard directory.")

(defun ghostherd--dir-local (directory var default)
  "VAR as DIRECTORY's .dir-locals.el sets it, or DEFAULT when it does not --
nil set there counts.  Read as `ghostherd-project-defaults' reads them."
  (let ((directory (file-name-as-directory (expand-file-name directory))))
    (or (ignore-errors
          (with-temp-buffer
            (setq default-directory directory)
            (setq-local buffer-file-name (expand-file-name ".ghostherd" directory))
            (hack-dir-local-variables)
            (when-let* ((cell (assq var file-local-variables-alist)))
              (list (cdr cell)))))
        (list default))))

(defun ghostherd--agy-guard-mode (directory)
  "The guard mode an agy agent in DIRECTORY runs under: auto, read-only or nil."
  (let ((mode (car (ghostherd--dir-local directory 'ghostherd-agy-guard ghostherd-agy-guard))))
    (and (memq mode '(auto read-only)) mode)))

(defun ghostherd--guard-dir (name)
  "Agent NAME's guard directory."
  (file-name-as-directory (expand-file-name name (expand-file-name ghostherd-guard-root))))

(defun ghostherd--agy-guard-setup (name mode directory project)
  "Write agent NAME's guard: its policy -- MODE, its workspace DIRECTORY,
PROJECT to read too, the project's own commands, the herd's secrets -- and
the hooks.json agy loads from it.  Return the guard directory."
  (let* ((dir (ghostherd--guard-dir name))
         (herd (and (fboundp 'ghostherd-herd-client) (ghostherd-herd-client)))
         (cmd (concat (shell-quote-argument (or herd "herd")) " guard " (shell-quote-argument dir)
                      " || echo '{\"decision\":\"deny\",\"reason\":\"The herd guard did not answer; tell the user.\"}'")))
    (make-directory (expand-file-name ".agents" dir) t)
    ;; Rebuilt for a new process: the old one's allowances go with it.
    (ignore-errors (delete-file (expand-file-name "approved.json" dir)))
    (set-file-modes dir #o700)
    (let ((coding-system-for-write 'utf-8)
          (json-encoding-pretty-print t))
      (with-temp-file (expand-file-name "policy.json" dir)
        (insert (json-encode
                 (list :agent name
                       :mode (symbol-name mode)
                       :workspace (expand-file-name directory)
                       :read_roots (vconcat (and project (list (expand-file-name project))))
                       :allow_commands (vconcat (car (ghostherd--dir-local
                                                      directory 'ghostherd-guard-allow-commands
                                                      ghostherd-guard-allow-commands)))
                       :herd (vconcat (delq nil (list herd (and herd (expand-file-name
                                                                      "ghostherd" (file-name-directory herd))))))
                       :guard_root (expand-file-name ghostherd-guard-root)
                       :secret_dirs (vconcat (and (fboundp 'ghostherd-memory--data-directory)
                                                  (list (ghostherd-memory--data-directory))))
                       :secret_files (vconcat (and (fboundp 'ghostherd-memory--mail-directory)
                                                   (list (expand-file-name "rpc.url" (ghostherd-memory--mail-directory)))))
                       ;; The herd's own token: never an agent's, even approved.
                       :never_files (vconcat (and (fboundp 'ghostherd-memory--token-file)
                                                  (list (ghostherd-memory--token-file))))))))
      (with-temp-file (expand-file-name ".agents/hooks.json" dir)
        (insert (json-encode
                 `((ghostherd-guard
                    . ((PreToolUse
                        . [((matcher . "*")
                            (hooks . [((type . "command") (command . ,cmd) (timeout . 15))]))]))))))))
    dir))

(defun ghostherd--with-agy-guard (args dir)
  "ARGS with the guard's flags for DIR -- --dangerously-skip-permissions
and --add-dir DIR -- or, DIR nil, without them.  Any earlier start's pair
(an --add-dir under `ghostherd-guard-root', the flag before it) goes
first, so a respawn takes the guard as it is now."
  (let ((root (file-name-as-directory (expand-file-name ghostherd-guard-root)))
        (in (copy-sequence args)) out)
    (while in
      (let ((a (pop in)))
        (if (and (equal a "--dangerously-skip-permissions")
                 (equal (car in) "--add-dir")
                 (stringp (cadr in))
                 (string-prefix-p root (file-name-as-directory (expand-file-name (cadr in)))))
            (setq in (cddr in))
          (push a out))))
    (append (nreverse out)
            (and dir (list "--dangerously-skip-permissions" "--add-dir"
                           (directory-file-name dir))))))

(defun ghostherd--session-guarded-p (session)
  "Non-nil when SESSION runs behind the herd's guard: an agy started with
its guard's --dangerously-skip-permissions --add-dir pair."
  (let ((root (file-name-as-directory (expand-file-name ghostherd-guard-root))))
    (and (eq (ghostherd-session-kind session) 'agy)
         (cl-loop for (flag add dir) on (ghostherd-session-args session)
                  thereis (and (equal flag "--dangerously-skip-permissions")
                               (equal add "--add-dir")
                               (stringp dir)
                               (string-prefix-p root (file-name-as-directory (expand-file-name dir))))))))

(defun ghostherd--scrub-wrapper (&optional directory)
  "The words run in front of a guarded agent's CLI: `herd scrub-exec',
keeping only `ghostherd-agent-env-allow' and the extra names -- yours, or
DIRECTORY's project's -- in its environment."
  (when-let* (((fboundp 'ghostherd-herd-client))
              (herd (ghostherd-herd-client)))
    (append (list herd "scrub-exec")
            (mapcan (lambda (name) (list "--allow" name))
                    (append ghostherd-agent-env-allow
                            (if directory
                                (car (ghostherd--dir-local directory 'ghostherd-agent-env-allow-extra
                                                           ghostherd-agent-env-allow-extra))
                              ghostherd-agent-env-allow-extra)))
            (list "--"))))

(defvar ghostherd--guard-requests nil
  "The guard's requests from the last `herd_tick', newest first.")

(defvar ghostherd--guard-seen (make-hash-table :test 'equal)
  "Guard request id -> the status the log last told of it.")

;; A request waiting for the user stays `working' to the agent's state --
;; the guard refused the call and the agent goes on -- so no `blocked'
;; tells you of it.  It is told on its own, once: a desktop notification,
;; and a push while you are away.  The sidecar keeps that it was told
;; (`guard_told' in the next tick), so a restarted Emacs, or sidecar, does
;; not tell of it again.  An Emacs gone before that tick tells again
;; after its restart: twice, rather than never.

(defvar ghostherd--guard-told (make-hash-table :test 'equal)
  "Guard request id -> t: told of by this Emacs.")

(defvar ghostherd--guard-told-ids nil
  "Guard requests told of, for the sidecar in the next tick.")

(defun ghostherd--guard-tell (r)
  "Tell the user guard request R waits for them: a desktop notification,
and a push while away -- each when it is on."
  (let* ((name (or (plist-get r :session) "?"))
         (tool (or (plist-get r :tool) ""))
         (summary (or (plist-get r :summary) ""))
         (call (truncate-string-to-width
                (replace-regexp-in-string
                 "[\n\t ]+" " " (if (or (equal tool "") (string-prefix-p (concat tool ":") summary))
                                     summary
                                   (concat tool ": " summary)))
                120 nil nil "…")))
    (when ghostherd-notify-on-guard
      (ghostherd--notify (format "ghostherd · %s needs your approval" name)
                         (format "%s -- approve it or keep it denied on the herd page" call)))
    (when-let* ((session (ghostherd-get name)))
      (ghostherd--push 'guard session :request (plist-get r :id) :tool tool :summary summary))))

(defun ghostherd--herd-take-guard (requests)
  "Keep REQUESTS from a `herd_tick', and log each refusal, approval and
use once.  A new one waiting for the user is told of, once."
  (dolist (r requests)
    (let ((id (plist-get r :id))
          (status (plist-get r :status)))
      (when (and (equal status "pending")
                 (not (eq (plist-get r :told) t))
                 (not (gethash id ghostherd--guard-told)))
        (puthash id t ghostherd--guard-told)
        (push id ghostherd--guard-told-ids)
        (with-demoted-errors "ghostherd: telling of a guard request: %S"
          (ghostherd--guard-tell r)))
      (unless (equal (gethash id ghostherd--guard-seen) status)
        (puthash id status ghostherd--guard-seen)
        (ghostherd--log-add
         (or (plist-get r :session) "?") 'guard
         (pcase status
           ((or "pending" "denied")
            (format "guard %s %s: %s -- %s" (if (equal status "pending") "needs approval" "denied")
                    id (plist-get r :summary) (plist-get r :rule)))
           ("approved" (format "guard %s approved once: %s" id (plist-get r :summary)))
           ("used" (format "guard %s ran, approved: %s" id (plist-get r :summary)))
           ("declined" (format "guard %s kept denied by the user: %s" id (plist-get r :summary)))
           ("expired" (format "guard %s expired, never answered: %s" id (plist-get r :summary)))
           (_ (format "guard %s %s: %s" id status (plist-get r :summary))))))))
  (setq ghostherd--guard-requests requests))

(defvar ghostherd--lint-seen (make-hash-table :test 'equal)
  "The checks' denials the log has told of, by id.")

(defun ghostherd--herd-take-lint (denials)
  "Log each of DENIALS from a `herd_tick' once: a call one of the herd's
checks denied (bin/herd_lint.py) -- the fact, and the command, so a check
that denies what it should not shows.  The page counts them per check."
  (dolist (d (reverse denials))          ; the tick's are newest first
    (let ((id (plist-get d :id)))
      (unless (gethash id ghostherd--lint-seen)
        (puthash id t ghostherd--lint-seen)
        (ghostherd--log-add
         (or (plist-get d :session) "?") 'lint
         (format "check %s denied (%s): %s" (plist-get d :fact) (or (plist-get d :cli) "?")
                 (truncate-string-to-width
                  (replace-regexp-in-string "[\n\t ]+" " " (or (plist-get d :command) ""))
                  160 nil nil "…")))))))

(defun ghostherd--guard-clear (name)
  "Tell the sidecar agent NAME is a new process: its open guard requests expire."
  (when (and (fboundp 'ghostherd-memory-rpc-url) (ghostherd-memory-rpc-url)
             (fboundp 'ghostherd-memory-request-async))
    (ghostherd-memory-request-async "herd_guard_clear" #'ignore (list :session name) #'ignore)))

(defun ghostherd-guard-reject (id)
  "Keep the guard's request ID denied: it closes, and the agent is told not
to try that call again."
  (interactive (list (completing-read "Keep denied: "
                                      (mapcar (lambda (r) (plist-get r :id))
                                              (seq-filter (lambda (r) (equal (plist-get r :status) "pending"))
                                                          ghostherd--guard-requests)))))
  (ghostherd-memory-request-async
   "herd_guard_reject" (lambda (_r) (message "ghostherd: kept %s denied" id))
   (list :id id)
   (lambda (err) (message "ghostherd: keeping %s denied: %s" id err))))

(defun ghostherd-guard-approve (id)
  "Approve the guard's request ID once: the agent may run that exact call
again, this once (the sidecar writes it into the agent's guard, and tells
it)."
  (interactive (list (completing-read "Approve guard request: "
                                      (mapcar (lambda (r) (plist-get r :id))
                                              (seq-filter (lambda (r) (equal (plist-get r :status) "pending"))
                                                          ghostherd--guard-requests)))))
  (ghostherd-memory-request-async
   "herd_guard_approve" (lambda (_r) (message "ghostherd: approved %s once" id))
   (list :id id)
   (lambda (err) (message "ghostherd: approving %s: %s" id err))))

(provide 'ghostherd-hooks)

;; Local Variables:
;; generated-autoload-load-name: "ghostherd"
;; End:

;;; ghostherd-hooks.el ends here

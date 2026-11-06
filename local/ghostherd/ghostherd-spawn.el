;;; ghostherd-spawn.el --- Spawn, restore, kill and rename agents; switch a model -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of ghostherd (see ghostherd.el).  Starting, restoring, killing
;; and renaming agents; where a new agent goes -- any project, maybe on
;; a past conversation -- and resuming one; switching an agent's model
;; as its CLI lets you.

;;; Code:

(require 'ghostherd-core)
(require 'ghostherd-worktree)
(require 'ghostherd-log)
(require 'ghostherd-screen)

(defvar ghostel-query-before-killing)
(declare-function ghostherd--age-string "ghostherd-sidebar" (time))
(declare-function ghostherd--agy-guard-mode "ghostherd-hooks" (directory))
(declare-function ghostherd--agy-guard-setup "ghostherd-hooks" (name mode directory project))
(declare-function ghostherd--args-with-memory "ghostherd-lead" (project args &optional agent))
(declare-function ghostherd--format-candidate "ghostherd-sidebar" (session))
(declare-function ghostherd--guard-clear "ghostherd-hooks" (name))
(declare-function ghostherd--guard-dir "ghostherd-hooks" (name))
(declare-function ghostherd--install-hooks "ghostherd" ())
(declare-function ghostherd--lead-args "ghostherd-lead" (kind))
(declare-function ghostherd--link "ghostherd-hooks" (session))
(declare-function ghostherd--memory-opening "ghostherd-lead" (project &optional agent))
(declare-function ghostherd--primary-forget "ghostherd-primary" ())
(declare-function ghostherd--register-eval-cmds "ghostherd" ())
(declare-function ghostherd--scrub-wrapper "ghostherd-hooks" (&optional directory))
(declare-function ghostherd--session-completion-table "ghostherd-sidebar" (candidates))
(declare-function ghostherd--sidebar-leave-overlay "ghostherd-sidebar" ())
(declare-function ghostherd--sidebar-refresh "ghostherd-sidebar" ())
(declare-function ghostherd--sidebar-session-at-point "ghostherd-sidebar" ())
(declare-function ghostherd--state-glyph "ghostherd-sidebar" (state))
(declare-function ghostherd--when-ready "ghostherd-lead" (session fn &optional tries fail))
(declare-function ghostherd--with-agy-guard "ghostherd-hooks" (args dir))
(declare-function ghostherd--with-lead-role "ghostherd-lead" (kind args))
(declare-function ghostherd--without-resume "ghostherd-hooks" (kind args))
(declare-function ghostherd-primary-start "ghostherd-primary" (&optional display))
(declare-function ghostherd-prompt "ghostherd-comm" (session prompt &optional wait timeout))
(declare-function ghostherd-session-conversation "ghostherd-hooks" (session))

;;; Spawn / kill / rename

(defun ghostherd-spawn (kind &rest plist)
  "Spawn an agent of KIND and return the `ghostherd-session'.

PLIST keys:
  :name      session name (unique)
  :project   project root directory
  :command   override executable
  :args      override argument list
  :notes     free-form note / role description
  :role      its role in the herd: \"lead\" for a project's lead
  :lineage   the agents that held its name before it (`ghostherd-relay')
  :directory working directory (defaults to project or `default-directory')
  :worktree  path of its own git worktree, when it has one; :branch and
             :base then say the branch and the ref it came off
  :branch    its worktree's branch (with :worktree)
  :base      the ref its worktree branched from (with :worktree)
  :backend   host to start it on (defaults to `ghostherd-backend')
  :display   when non-nil (default t), pop to the buffer"
  (ghostherd--install-hooks)
  (ghostherd--register-eval-cmds)
  (let* ((spec (ghostherd--spec kind))
         (name (or (plist-get plist :name)
                   (ghostherd--unique-name (symbol-name kind))))
         (project (or (plist-get plist :project)
                      (ghostherd--project-root)))
         (directory (expand-file-name
                     (or (plist-get plist :directory)
                         project
                         default-directory)))
         ;; Never where the directory is not: tmux's `new-session -c'
         ;; starts the agent in $HOME instead, without a word, and the
         ;; CLI there asks to trust the whole home directory.  Every path
         ;; that starts an agent comes through here; the ones that kill
         ;; one first (respawn, relay) look before they do.
         (_ (when (ghostherd--directory-missing-p directory)
              (user-error "%s's directory %s is not there: not starting it (it would start in your home directory)"
                          name (abbreviate-file-name directory))))
         (command (or (plist-get plist :command)
                      (plist-get spec :command)))
         (args (let ((base (if (plist-member plist :args)
                               (plist-get plist :args)
                             (plist-get spec :args))))
                 ;; grok reads the shared-memory opening (rules + index) from
                 ;; --rules at launch; agy, which has no such flag, is told it
                 ;; as a first message below; claude reads memory through its
                 ;; linked dir.  Here for every grok/agy, whatever spawned it.
                 (if (eq kind 'grok)
                     (ghostherd--args-with-memory project base name)
                   base)))
         ;; A guarded agy: its guard written, and agy started with the flag
         ;; and the guard's --add-dir -- or, the guard off, without them,
         ;; whatever an earlier start of it had.
         (guard (and (eq kind 'agy) command
                     (ghostherd--agy-guard-mode directory)))
         (args (if (eq kind 'agy)
                   (ghostherd--with-agy-guard
                    args (and guard (ghostherd--agy-guard-setup
                                     (or (plist-get plist :name) name) guard directory project)))
                 args))
         (notes (plist-get plist :notes))
         (role (plist-get plist :role))
         (lineage (plist-get plist :lineage))
         (worktree (plist-get plist :worktree))
         (branch (plist-get plist :branch))
         (base (plist-get plist :base))
         (started-by (plist-get plist :started-by))
         (handoff-by (plist-get plist :handoff-by))
         (backend (or (plist-get plist :backend) ghostherd-backend))
         (display (if (plist-member plist :display)
                      (plist-get plist :display)
                    t))
         (started (ghostherd-backend-spawn
                   backend
                   (list :name name :kind kind :command command :args args
                         :directory directory :project project :notes notes
                         :role role :lineage lineage
                         :worktree worktree :branch branch :base base
                         :started-by started-by
                         :handoff-by handoff-by
                         ;; Run in front of the CLI, never kept in its recipe.
                         :wrapper (and guard (ghostherd--scrub-wrapper directory)))))
         session)
    (setq session
          (ghostherd-session--create
           :id name
           :name name
           :kind kind
           :backend backend
           :host-id (plist-get started :host-id)
           :buffer (plist-get started :buffer)
           :project (and project (expand-file-name project))
           :command command
           :args args
           :state (if (plist-get started :started) 'starting 'idle)
           :state-reason (if (plist-get started :started) "spawned" "shell")
           :notes notes
           :last-active (current-time)))
    (puthash name session ghostherd--sessions)
    (if role
        (puthash name role ghostherd--roles)
      (remhash name ghostherd--roles))
    (if lineage
        (puthash name lineage ghostherd--lineage)
      (remhash name ghostherd--lineage))
    (if worktree
        (puthash name (list :path worktree :branch branch :base base)
                 ghostherd--worktrees)
      (remhash name ghostherd--worktrees))
    (if started-by
        (puthash name started-by ghostherd--started-by)
      (remhash name ghostherd--started-by))
    ;; A lead the herd started for a handoff: its caps count it.
    (if handoff-by
        (puthash name handoff-by ghostherd--handoff-leads)
      (remhash name ghostherd--handoff-leads))
    (ghostherd--log-add session 'life
                        (if started-by
                            (format "started by %s (%s on %s)" started-by kind backend)
                          (format "spawned %s on %s" kind backend)))
    ;; A new process under this name: the guard requests its old one left
    ;; open mean nothing to it.
    (ghostherd--guard-clear name)
    (ghostherd--ensure-poll-timer)
    (run-hook-with-args 'ghostherd-session-created-hook session)
    ;; agy has no --rules flag: tell it the shared-memory opening as its first
    ;; message once it is ready.  Only on a real launch -- a restored agy kept
    ;; the opening it was first told, and an adopted shell gets none.
    (when (and (eq kind 'agy) (plist-get started :started))
      (ghostherd--when-ready
       session (lambda () (ghostherd-prompt session (concat (and guard (format ghostherd--agy-guard-note
                                                                              directory (ghostherd--guard-dir name)))
                                                            (ghostherd--memory-opening project name))))))
    (when display
      (ghostherd-visit session))
    (ghostherd--sidebar-refresh)
    session))

(declare-function ghostel--adjust-size "ghostel" (window &optional force))

(defun ghostherd--sync-view-size (buffer)
  "Resize BUFFER's terminal to the window now showing it.

ghostel sizes a terminal when it is created and thereafter from
`window-size-change-functions', which fire when a window is *resized*.
Displaying a buffer in an existing window is not a resize, so a view
created before it was displayed -- which is every tmux attach, since the
client has to exist before there is anything to show -- keeps whatever
size it was born with.  The agent then draws into a screen a fraction of
the window you are looking at, which reads as not being able to scroll
rather than as a pane that is 63x23 inside a window twice that.

Forced, because the terminal's own idea of its size is exactly what is
stale here."
  (when-let* ((window (get-buffer-window buffer t)))
    (when (fboundp 'ghostel--adjust-size)
      (ignore-errors (ghostel--adjust-size window t)))))

(defun ghostherd-visit (session)
  "Display SESSION, attaching a view to its host when there is none.

On the ghostel backend there is always exactly one buffer and this just
pops to it.  On tmux the buffer is a client that may not exist yet, so
visiting is what runs `tmux attach' -- the only moment in the whole
design where a second terminal emulator is in the picture."
  (setq session (ghostherd-get session))
  (unless session
    (user-error "No such session"))
  ;; The overlay's window is dedicated and its frame unsplittable;
  ;; popping the agent from there has nowhere to put it.
  (ghostherd--sidebar-leave-overlay)
  (let ((buffer (ghostherd--host-view session)))
    (setf (ghostherd-session-buffer session) buffer
          (ghostherd-session-seen session) t)
    (pop-to-buffer buffer)
    (ghostherd--sync-view-size buffer)
    buffer))

;;;###autoload
(defvar ghostherd--restored nil
  "Non-nil once this Emacs has restored its herd (or found nothing to).
Until then an empty or partial session snapshot is a herd not yet picked
up after a restart, not agents that died, so the sidecar must not fail an
open ask on it -- `ghostherd--herd-tick' sends this as `restored'.")

(defvar ghostherd--restore-timer nil
  "The pending deferred-restore retry timer, or nil.")

(defun ghostherd-restore ()
  "Register agents the host still has but this Emacs does not.

Only reachable on a backend whose agents outlive Emacs, so this is
where a herd comes back after a restart.  Sessions come back detached
-- alive, with no buffer -- and the first poll fills in their state
from the screen.

Emacs-side bookkeeping does not survive and is not meant to: a manual
state override, whether a session has been seen, the last OSC progress
report.  Notes do, because they were written onto the host.

Every known backend is asked, not just `ghostherd-backend'.  Changing
which host new agents start on should not hide the ones already
running, and a session keeps the backend it was born with anyway."
  (interactive)
  (let ((restored 0))
    (dolist (entry (mapcan (lambda (backend)
                             (mapcar (lambda (recipe) (cons backend recipe))
                                     (ghostherd-backend-list backend)))
                           ghostherd-known-backends))
      (let* ((backend (car entry))
             (recipe (cdr entry))
             (name (plist-get recipe :name)))
        (unless (ghostherd-get name)
          (puthash name
                   (ghostherd-session--create
                    :id name
                    :name name
                    :kind (or (plist-get recipe :kind) 'shell)
                    :backend backend
                    :host-id (plist-get recipe :host-id)
                    ;; None after a restart; one still showing it when
                    ;; this Emacs dropped it and takes it back.
                    :buffer (ghostherd--orphan-view name)
                    :project (plist-get recipe :project)
                    :command (plist-get recipe :command)
                    :args (plist-get recipe :args)
                    :notes (plist-get recipe :notes)
                    :state 'idle
                    :state-reason "restored"
                    ;; Seen: a herd that was already there when Emacs
                    ;; started has not just finished anything.
                    :seen t
                    :last-active (current-time))
                   ghostherd--sessions)
          (if-let* ((role (plist-get recipe :role)))
              (puthash name role ghostherd--roles)
            (remhash name ghostherd--roles))
          (if-let* ((lineage (plist-get recipe :lineage)))
              (puthash name lineage ghostherd--lineage)
            (remhash name ghostherd--lineage))
          (if-let* ((worktree (plist-get recipe :worktree)))
              (puthash name (list :path worktree
                                  :branch (plist-get recipe :branch)
                                  :base (plist-get recipe :base))
                       ghostherd--worktrees)
            (remhash name ghostherd--worktrees))
          (if-let* ((started-by (plist-get recipe :started-by)))
              (puthash name started-by ghostherd--started-by)
            (remhash name ghostherd--started-by))
          ;; Still the herd's, started for a handoff: its caps count it.
          (if-let* ((handoff-by (plist-get recipe :handoff-by)))
              (puthash name handoff-by ghostherd--handoff-leads)
            (remhash name ghostherd--handoff-leads))
          (ghostherd--log-add name 'life
                              (format "restored from %s, detached" backend))
          (setq restored (1+ restored)))))
    (when (> restored 0)
      ;; The next poll looks whether their directories are there, not a
      ;; minute on: one restored with its worktree gone is marked at once.
      (setq ghostherd--dir-checked-at 0)
      (ghostherd--ensure-poll-timer)
      (ghostherd--sidebar-refresh))
    (setq ghostherd--restored t)
    restored))

(defun ghostherd--restore-now (&optional attempt)
  "Attempt `ghostherd-restore'; on a load race, schedule a retry.
`restored' stays nil until a try succeeds, so the sidecar holds open asks
rather than failing them for takers whose live hosts this Emacs has not
picked up yet."
  (let ((attempt (or attempt 1)))
    (setq ghostherd--restore-timer nil)
    (condition-case err
        (ghostherd-restore)                 ; sets `ghostherd--restored' on success
      (error
       (if (< attempt 6)
           (setq ghostherd--restore-timer
                 (run-at-time (* attempt 0.3) nil #'ghostherd--restore-now (1+ attempt)))
         (message "ghostherd: automatic restore kept failing (%s); run M-x ghostherd-restore"
                  (error-message-string err)))))))

(defun ghostherd--restore-deferred ()
  "Schedule the herd restore for after load and init settle.
`ghostherd-mode' calls this instead of `ghostherd-restore' directly: a
zero-delay timer runs it once the current load or command returns, so the
whole file and its state (e.g. `ghostherd--worktrees') are bound first --
a synchronous restore during mode enable could race them void from a
stale build or an unfinished load.  Idempotent: `ghostherd-restore'
registers only agents not already known, so the automatic restore and any
later manual one do not clash."
  (when (timerp ghostherd--restore-timer)
    (cancel-timer ghostherd--restore-timer))
  (setq ghostherd--restore-timer (run-at-time 0 nil #'ghostherd--restore-now 1)))

(defun ghostherd-session-recipe (session)
  "Return the plist `ghostherd-spawn' needs to recreate SESSION.

This is the whole of what a respawn can carry over.  The conversation
is not in it and never can be: it lives in the CLI's own store, which
is what `:continue-args' reaches.

Whether the *agent* survives depends on the backend.  On ghostel it
cannot -- the agent is an Emacs child.  On tmux it does, and this same
recipe is written onto the host so `ghostherd-restore' can rebuild the
session around a process that never stopped."
  (let ((wt (ghostherd-session-worktree session)))
    (list :name (ghostherd-session-name session)
          :project (ghostherd-session-project session)
          ;; A worktree agent respawns in its worktree, not the main
          ;; checkout: its heir takes over the same branch, not a copy.
          :directory (or (plist-get wt :path) (ghostherd-session-project session))
          :backend (ghostherd-session-backend session)
          :command (ghostherd-session-command session)
          :args (ghostherd-session-args session)
          :notes (ghostherd-session-notes session)
          :role (ghostherd-session-role session)
          :lineage (ghostherd-session-lineage session)
          :worktree (plist-get wt :path)
          :branch (plist-get wt :branch)
          :base (plist-get wt :base)
          :started-by (ghostherd-session-started-by session)
          :handoff-by (gethash (ghostherd-session-id session) ghostherd--handoff-leads))))

;;;###autoload
(defun ghostherd-respawn (session &optional continue)
  "Relaunch SESSION from its recipe, replacing any existing buffer.

With CONTINUE non-nil, append the CLI's `:continue-args' so it resumes
its previous conversation rather than starting cold.  Without it, it
starts cold: the recipe's flags that pick a conversation are left off.  Interactively that
is the default when the agent kind has such a flag, since the usual
reason to respawn is that the agent died or wedged and you want it back
where it was.

Called interactively, acts on the sidebar row at point when there is
one, and otherwise prompts."
  (interactive
   (let* ((session (or (and (derived-mode-p 'ghostherd-sidebar-mode)
                            (ghostherd--sidebar-session-at-point))
                       (ghostherd--read-session "Respawn agent: ")))
          (spec (ignore-errors
                  (ghostherd--spec (ghostherd-session-kind session))))
          (exact (ghostherd--resume-args (ghostherd-session-kind session)
                                         (ghostherd-session-conversation session)))
          (continuable (or exact (plist-get spec :continue-args))))
     (list session
           (and continuable
                (y-or-n-p (format "Resume %s (%s)? "
                                  (if exact "its conversation" "the previous conversation")
                                  (string-join continuable " ")))))))
  (setq session (ghostherd-get session))
  (unless session
    (user-error "No such session"))
  ;; Before it is killed: refused after, it would be gone from the herd too.
  (ghostherd--refuse-gone session "respawning")
  (let* ((kind (ghostherd-session-kind session))
         (spec (ghostherd--spec kind))
         (recipe (ghostherd-session-recipe session))
         ;; The conversation its hooks named, exactly, rather than
         ;; `--continue' -- the most recent one in the directory, which
         ;; with two agents of a kind in one project may be the other's.
         (exact (and continue
                     (ghostherd--resume-args kind (ghostherd-session-conversation session))))
         ;; The recipe has the flags it was last started with -- the
         ;; `--continue' of a continuing respawn, the `-r' of a resume.
         ;; Fresh is without them, or it goes on where it was.
         (base (ghostherd--without-resume kind (plist-get recipe :args)))
         (args (cond (exact (append base exact))
                     (continue (append base (plist-get spec :continue-args)))
                     (t base))))
    (when (and continue (not exact) (null (plist-get spec :continue-args)))
      (user-error "%s has no resume flag" kind))
    ;; The recipe holds the role as it was when the lead started; it
    ;; goes back with the role as it is now.
    (when (equal (plist-get recipe :role) "lead")
      (setq args (ghostherd--with-lead-role kind args)))
    ;; Free both the buffer name and the registry id before respawning:
    ;; `ghostherd-spawn' refuses to clobber an existing buffer.
    (ghostherd-kill session t)
    (let ((heir (apply #'ghostherd-spawn kind (plist-put (copy-sequence recipe) :args args))))
      ;; A lead on a CLI that takes no role flag (agy) carries its role in
      ;; the first message, not its args, so a fresh respawn -- a new, cold
      ;; conversation -- would come up without it.  Re-send it once it is
      ;; ready.  A continuing respawn resumes the conversation that already
      ;; has it, and a relay carries its own handoff with the role, so
      ;; neither reaches here: only a cold respawn of such a lead re-sends.
      (when (and heir
                 (equal (plist-get recipe :role) "lead")
                 (not (ghostherd--lead-args kind))
                 (not exact) (not continue))
        (ghostherd--when-ready heir
                               (lambda () (ghostherd-prompt heir ghostherd--lead-role))))
      heir)))

;;; Where a new agent goes: any project, maybe on a past conversation
;;
;; `ghostherd-new' and the page's + New agent offer the same places
;; (`ghostherd-new-projects'): the project you are in, the projects the
;; herd has agents in, the ones project.el knows, and where past
;; conversations ran -- the sidecar's index of them, the Sessions tab's.
;; Each is resolved to its project (a worktree to its main checkout) and
;; named once.  A project's past conversations, its worktrees' included,
;; can be picked up again instead of starting fresh
;; (`ghostherd-new-conversations').  All of it is read when a picker
;; opens and kept a minute: never on the herd's tick, which is where a
;; git look per directory would cost.

(defcustom ghostherd-new-keep-seconds 60
  "How long the places a new agent can go are kept once read.
They are read when `ghostherd-new' or the page's + New agent opens: the
sidecar's past conversations, and each one's directory resolved to its
project, which can take a git look."
  :type 'integer
  :group 'ghostherd)

(defconst ghostherd--new-wait 4
  "Seconds `ghostherd-new' waits for the sidecar's past conversations.
Without them it offers the other places.")

(defvar ghostherd--new-sources nil
  "(TIME . SOURCES): the past conversations as the sidecar listed them
at TIME, transcripts only, each a plist of its index row.")

(defvar ghostherd--new-roots (make-hash-table :test 'equal)
  "Directory -> the project it belongs to (`ghostherd--new-root'), or
`none' for one that is gone.  Read again with the past conversations.")

(defvar ghostherd-new-project-history nil
  "Minibuffer history of `ghostherd-new''s project prompt.")

(defun ghostherd--new-fresh-p ()
  "Non-nil while the kept past conversations are new enough to offer."
  (and ghostherd--new-sources
       (< (- (float-time) (car ghostherd--new-sources)) ghostherd-new-keep-seconds)))

(defun ghostherd--new-keep (result)
  "Keep the past conversations in RESULT, a `memory_list' answer or nil;
the directories are resolved afresh from now on."
  (clrhash ghostherd--new-roots)
  (setq ghostherd--new-sources
        (cons (float-time)
              (seq-filter (lambda (s) (equal (plist-get s :kind) "transcript"))
                          (plist-get result :sources)))))

(defconst ghostherd--new-list '(:limit 5000 :titles t)
  "What the past conversations are asked of the sidecar with: every one,
with its title (`memory_list').")

(defun ghostherd--new-sidecar-p ()
  "Non-nil when there is a sidecar to ask for past conversations."
  (and (fboundp 'ghostherd-memory-rpc-url) (ghostherd-memory-rpc-url) t))

(defun ghostherd--new-read-sources ()
  "The past conversations: the kept ones, or read now and kept.
Read synchronously, for `ghostherd-new', which waits for its prompt
anyway; none when the sidecar is not there or does not answer in
`ghostherd--new-wait' -- then none until they are read again."
  (unless (ghostherd--new-fresh-p)
    (ghostherd--new-keep
     (and (ghostherd--new-sidecar-p)
          (condition-case nil
              (ghostherd-memory-request "memory_list" ghostherd--new-list ghostherd--new-wait)
            (error nil)))))
  (cdr ghostherd--new-sources))

(defvar ghostherd--new-waiting nil
  "What waits for the past conversations being read, newest first: nil
when no read is in flight.")

(defun ghostherd--new-read-sources-async (then)
  "Call THEN once the past conversations are kept: at once when they are,
else when the sidecar answers -- or fails, with none kept.  One read at
a time: a call while one is in flight waits for it, so the form's places
and its project's conversations, asked together, cost one read and one
resolving of the directories.  The request gives up in
`ghostherd-memory-request-timeout', so the waiting never wedges."
  (cond
   ((ghostherd--new-fresh-p) (funcall then))
   ((not (ghostherd--new-sidecar-p)) (ghostherd--new-keep nil) (funcall then))
   (ghostherd--new-waiting (push then ghostherd--new-waiting))
   (t (setq ghostherd--new-waiting (list then))
      (let ((done (lambda (result)
                    (ghostherd--new-keep result)
                    (let ((waiting (reverse ghostherd--new-waiting)))
                      (setq ghostherd--new-waiting nil)
                      (mapc #'funcall waiting)))))
        (ghostherd-memory-request-async
         "memory_list" done ghostherd--new-list
         (lambda (_err) (funcall done nil)))))))

(defun ghostherd--new-root (dir)
  "The project DIR belongs to, absolute, as a directory: its root by
`ghostherd--project-root' (a worktree's main checkout), or DIR itself
when it is in none.  Nil when DIR is gone, and for a remote one: looking
would open a connection for a picker.  Kept with the past conversations,
so a directory is looked at once while they are."
  (when (and (stringp dir) (not (string-empty-p dir)) (not (file-remote-p dir)))
    (let* ((dir (file-name-as-directory (expand-file-name dir)))
           (root (or (gethash dir ghostherd--new-roots)
                     (puthash dir
                              (if (file-directory-p dir)
                                  (file-name-as-directory
                                   (expand-file-name (or (ghostherd--project-root dir) dir)))
                                'none)
                              ghostherd--new-roots))))
      (and (stringp root) root))))

(defun ghostherd--ago (time)
  "How long ago TIME, a float time, was: 40s, 5m, 3h, 2d."
  (let ((s (max 0 (floor (- (float-time) time)))))
    (cond ((< s 60) (format "%ds" s))
          ((< s 3600) (format "%dm" (/ s 60)))
          ((< s 86400) (format "%dh" (/ s 3600)))
          (t (format "%dd" (/ s 86400))))))

(defun ghostherd--source-time (source)
  "When SOURCE, an index row, was last written, as a float time, or nil."
  (let ((m (plist-get source :mtime)))
    (and (numberp m) (float m))))

(defun ghostherd-new-projects (&optional here)
  "Where a new agent can go, best first, each once (by
`ghostherd--project-name'): HERE, a directory -- the current project --
then the projects the herd has agents in, the newest at work first;
those `project-known-project-roots' knows, in its order; and where past
conversations ran, the newest first.  A project in more than one of
these is ranked by the first.  Each is a plist: :root (absolute) and
:name, :here, :agents and :lead (the herd's, now), :known, :last (its
newest past conversation, a float time) and :conversations (how many).
Primary's directory is not offered unless it is HERE: primary works
there alone.  Reads the past conversations if they are not kept."
  (let ((by (make-hash-table :test 'equal))
        (primary (and (fboundp 'ghostherd--primary-dir)
                      (ghostherd--project-name (ghostherd--primary-dir))))
        (n 0))
    (cl-flet ((place (root rank order)
                (let* ((name (ghostherd--project-name root))
                       (p (gethash name by)))
                  (when (or (= rank 0) (not (equal name primary)))
                    (unless p
                      (setq p (puthash name (list :root root :name name :rank rank :order order
                                                  :here nil :agents 0 :lead nil :known nil
                                                  :last nil :conversations 0)
                                       by)))
                    (when (< rank (plist-get p :rank))
                      (plist-put p :rank rank)
                      (plist-put p :order order))
                    p))))
      (when-let* ((root (and here (ghostherd--new-root here))))
        (plist-put (place root 0 0) :here t))
      (dolist (s (ghostherd-sessions))
        (when-let* (((not (eq (ghostherd-session-state s) 'dead)))
                    (project (ghostherd-session-project s))
                    (p (place (file-name-as-directory (expand-file-name project)) 1
                              (- (float-time (or (ghostherd-session-last-active s) 0))))))
          (plist-put p :agents (1+ (plist-get p :agents)))
          (when (equal (ghostherd-session-role s) "lead")
            (plist-put p :lead t))))
      (dolist (dir (and (fboundp 'project-known-project-roots) (project-known-project-roots)))
        (when-let* ((root (ghostherd--new-root dir))
                    (p (place root 2 (cl-incf n))))
          (plist-put p :known t)))
      (dolist (s (ghostherd--new-read-sources))
        (when-let* ((root (ghostherd--new-root (plist-get s :project)))
                    (time (or (ghostherd--source-time s) 0))
                    (p (place root 3 (- time))))
          (plist-put p :conversations (1+ (plist-get p :conversations)))
          (when (> time (or (plist-get p :last) 0))
            (plist-put p :last time)))))
    (sort (hash-table-values by)
          (lambda (a b)
            (or (< (plist-get a :rank) (plist-get b :rank))
                (and (= (plist-get a :rank) (plist-get b :rank))
                     (< (plist-get a :order) (plist-get b :order))))))))

(defun ghostherd-new-project-note (place)
  "What is said of PLACE, a `ghostherd-new-projects' entry, beside it:
\"this project · 2 agents · lead · last conversation 3d ago\"."
  (let ((n (plist-get place :agents)))
    (string-join
     (delq nil (list (and (plist-get place :here) "this project")
                     (and (> n 0) (format "%d agent%s" n (if (= n 1) "" "s")))
                     (and (plist-get place :lead) "lead")
                     (and (plist-get place :last)
                          (format "last conversation %s ago" (ghostherd--ago (plist-get place :last))))))
     " · ")))

(defun ghostherd--conversation-id (agent path session-id)
  "The id AGENT's CLI resumes the conversation at PATH by."
  (if (equal agent "claude")
      ;; `claude -r ID' opens ID.jsonl, and a transcript that was itself
      ;; resumed can carry another sessionId inside it.
      (file-name-base path)
    session-id))

(defun ghostherd-new-conversations (root)
  "ROOT's past conversations that can be picked up again, newest first:
those in it and in its worktrees -- whatever directory resolves to ROOT
-- whose CLI resumes by id and still has them (`ghostherd--resume-args',
which leaves out one claude or agy pruned) and whose transcript is still
there (the Sessions tab's test: grok's chat history goes with its
session, and grok's id is not checked).  Each is a plist: :kind, :id,
:directory (its own: a resume runs there), :title, :time, :path (its
transcript), :fork (whether its CLI can branch off it), :running (the
agent on it now, `ghostherd--resumed-by', or nil) and :occupied (an
agent whose own worktree its directory is, or nil: a resume there puts
a second agent in that agent's checkout).  Reads the past conversations
if they are not kept."
  (let* ((name (ghostherd--project-name root))
         (out nil))
    (dolist (s (ghostherd--new-read-sources))
      (let* ((agent (plist-get s :agent))
             (kind (and (stringp agent) (intern-soft agent)))
             (path (plist-get s :source_path))
             (dir (plist-get s :project))
             (id (and kind (stringp path)
                      (ghostherd--conversation-id agent path (plist-get s :session_id))))
             (at (ghostherd--new-root dir)))
        (when (and at (equal (ghostherd--project-name at) name)
                   (assq kind ghostherd-agent-specs)
                   (stringp path) (file-exists-p path)
                   (ghostherd--resume-args kind id))
          (push (list :kind kind :id id
                      :directory (file-name-as-directory (expand-file-name dir))
                      :title (let ((title (plist-get s :title)))
                               (if (stringp title) (string-trim title) ""))
                      :time (ghostherd--source-time s)
                      :path path
                      :fork (and (plist-get (ghostherd--spec kind) :fork-args) t)
                      :running (ghostherd--resumed-by id)
                      :occupied (ghostherd--worktree-owner dir))
                out))))
    (sort out (lambda (a b) (> (or (plist-get a :time) 0) (or (plist-get b :time) 0))))))

(defun ghostherd--worktree-owner (dir)
  "The live agent whose own git worktree DIR is, or nil."
  (let ((name (ghostherd--project-name dir)))
    (seq-find (lambda (s)
                (and (not (eq (ghostherd-session-state s) 'dead))
                     (when-let* ((path (plist-get (ghostherd-session-worktree s) :path)))
                       (equal (ghostherd--project-name path) name))))
              (ghostherd-sessions))))

(defun ghostherd--completion-table (candidates annotate)
  "A completion table of CANDIDATES in their order, ANNOTATE (a function
of one) saying what each is beside it."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (display-sort-function . identity)
                   (cycle-sort-function . identity)
                   (annotation-function . ,annotate))
      (complete-with-action action candidates string pred))))

(defun ghostherd--read-new-directory (here)
  "Ask where a new agent goes: a project offered by
`ghostherd-new-projects', or a directory typed in -- a subdirectory is
taken as it is, its project being its root's.  HERE, the current
project, is the default; with none, the first offered.  Return the
directory, absolute."
  (let* ((places (ghostherd-new-projects here))
         (names (mapcar (lambda (p) (plist-get p :name)) places))
         (notes (mapcar (lambda (p) (cons (plist-get p :name) (ghostherd-new-project-note p)))
                        places))
         (choice (completing-read
                  "Project (or type a directory): "
                  (ghostherd--completion-table
                   names (lambda (c)
                           (let ((note (cdr (assoc c notes))))
                             (and note (not (string-empty-p note)) (concat "  " note)))))
                  nil nil nil 'ghostherd-new-project-history
                  (and here (ghostherd--project-name here))))
         (place (seq-find (lambda (p) (equal (plist-get p :name) choice)) places)))
    (cond
     (place (plist-get place :root))
     ((string-empty-p choice)
      (or (plist-get (car places) :root) (user-error "No project to start an agent in")))
     (t (let ((dir (expand-file-name (substitute-in-file-name choice))))
          (unless (file-directory-p dir)
            (user-error "No directory %s" choice))
          (file-name-as-directory dir))))))

(defun ghostherd-new-conversation-label (conv)
  "CONV, a `ghostherd-new-conversations' entry, as a candidate: its title
and its id's start, which tells two of one title apart."
  (let ((id (plist-get conv :id)))
    (concat (if (string-empty-p (plist-get conv :title)) "(untitled)" (plist-get conv :title))
            " " (propertize (substring id 0 (min 8 (length id))) 'face 'shadow))))

(defun ghostherd-new-conversation-note (conv root)
  "What is said of CONV beside it: its kind, its age, the agent on it,
the agent whose worktree it is in, and its directory when that is not
ROOT's -- a worktree's, say."
  (let ((running (plist-get conv :running))
        (occupied (plist-get conv :occupied))
        (dir (plist-get conv :directory)))
    (string-join
     (delq nil (list (symbol-name (plist-get conv :kind))
                     (and (plist-get conv :time) (concat (ghostherd--ago (plist-get conv :time)) " ago"))
                     (and running (format "running in %s" (ghostherd-session-name running)))
                     (and occupied (not (eq occupied running))
                          (format "%s works in that directory" (ghostherd-session-name occupied)))
                     (and (not (equal (ghostherd--project-name dir) (ghostherd--project-name root)))
                          (ghostherd--abbreviate dir))))
     " · ")))

(defun ghostherd--read-new-conversation (root)
  "Ask whether the new agent starts fresh or picks up one of ROOT's past
conversations (`ghostherd-new-conversations').  New is the default, so
RET goes on as before; nil for it, and at once when there is none."
  (when-let* ((convs (ghostherd-new-conversations root)))
    (let* ((new "New")
           (table (cons (cons new nil)
                        (mapcar (lambda (c) (cons (ghostherd-new-conversation-label c) c)) convs)))
           (choice (completing-read
                    "Conversation (New, or one to pick up again): "
                    (ghostherd--completion-table
                     (mapcar #'car table)
                     (lambda (c)
                       (let ((conv (cdr (assoc c table))))
                         (concat "  " (if conv (ghostherd-new-conversation-note conv root)
                                        "a fresh conversation")))))
                    nil t nil nil new)))
      (cdr (assoc choice table)))))

(defun ghostherd-new-resume (conv &optional fork display)
  "Pick CONV, a `ghostherd-new-conversations' entry, up again in a new
agent -- or with FORK, branch a new conversation off it.  It runs in the
conversation's own directory: a CLI's resume by id is scoped to where it
ran.  An agent already on it is shown instead, never a second writer on
one conversation (`ghostherd--resumed-by').  DISPLAY nil starts it
without showing it.  Return the agent."
  (let ((running (and (not fork) (ghostherd--resumed-by (plist-get conv :id)))))
    (if running
        (progn (ghostherd-visit running)
               (message "%s is on that conversation already" (ghostherd-session-name running))
               running)
      (ghostherd-resume (plist-get conv :kind) (plist-get conv :id) (plist-get conv :directory)
                        :fork fork :title (plist-get conv :title) :display display))))

;;;###autoload
(defun ghostherd-new (&optional kind)
  "Start a new agent in any project, or on one of its past conversations.
Asks the project first: the current one by default, so in a project RET
goes on as it always did; any other the herd, project.el or a past
conversation knows is offered with what it has (`ghostherd-new-projects'),
or type a directory.  Then the kind, preselected by that project's
`ghostherd-project-defaults'.  Then, when the project has past
conversations, New -- the default -- or one of them, any CLI's, newest
first, to pick up again in its own directory (`ghostherd-new-resume'),
or fork where its CLI can.  A new agent asks its name, args and notes,
and whether it gets its own git worktree.
With KIND, skip the kind prompt."
  (interactive)
  (let* ((directory (ghostherd--read-new-directory (ghostherd--project-root)))
         (project (ghostherd--project-root directory))
         ;; From the chosen project, not the buffer's: an agent started
         ;; for B from a buffer in A takes B's kind and args.
         (defaults (ghostherd-project-defaults directory))
         (kind (or kind
                   (intern
                    (completing-read
                     "Agent kind: "
                     (mapcar #'symbol-name (ghostherd--agent-kinds))
                     nil t nil nil
                     (symbol-name (or (car defaults) ghostherd-default-kind))))))
         (conv (ghostherd--read-new-conversation (or project directory))))
    (if conv
        (let ((running (ghostherd--resumed-by (plist-get conv :id))))
          (ghostherd-new-resume
           conv
           (and (plist-get conv :fork)
                (y-or-n-p (if running
                              (format "%s is on it already.  Fork a new conversation off it (no: show %s)? "
                                      (ghostherd-session-name running) (ghostherd-session-name running))
                            "Fork a new conversation off it (no: resume it)? ")))
           t))
      (ghostherd--new-fresh kind directory project defaults))))

(defun ghostherd--new-fresh (kind directory project defaults)
  "The rest of `ghostherd-new' for a fresh KIND agent in DIRECTORY, of
PROJECT (nil outside any), DEFAULTS its project's: its name, args, notes
and whether it gets its own worktree."
  (let* ((spec (ghostherd--spec kind))
         (default-name (ghostherd--unique-name (symbol-name kind)))
         (name (read-string "Session name: " default-name))
         (final-name (if (string-empty-p name) default-name name))
         (arg-string (read-string
                      (format "Extra args for %s (optional): "
                              (or (plist-get spec :command) "shell"))
                      (when-let* ((a (or (cdr defaults)
                                         (plist-get spec :args))))
                        (string-join a " "))))
         (args (when (and arg-string (not (string-empty-p arg-string)))
                 (split-string-and-unquote arg-string)))
         (notes (read-string "Role / notes (optional): "))
         ;; Opt in to the agent's own git worktree, off the project's HEAD,
         ;; on a branch named for the agent.  Done last, after the prompts,
         ;; so aborting one leaves no orphan worktree behind.
         (wt (and project
                  (y-or-n-p (format "Own git worktree off %s? "
                                    (ghostherd--abbreviate project)))
                  (ghostherd--worktree-create
                   project final-name (read-string "Branch: " final-name) kind))))
    (when wt
      (ghostherd--worktree-deps-setup project (plist-get wt :path) final-name))
    (ghostherd-spawn
     kind
     :name final-name
     ;; A directory in no project is its own, as a resume's is -- not
     ;; the project of the buffer this was started from.
     :project (or project directory)
     :directory (or (plist-get wt :path) directory)
     :args args
     :notes (and (not (string-empty-p notes)) notes)
     :worktree (plist-get wt :path)
     :branch (plist-get wt :branch)
     :base (plist-get wt :base))))

(defconst ghostherd--session-id-re "\\`[A-Za-z0-9][A-Za-z0-9_.-]*\\'"
  "What a CLI's conversation id looks like: a UUID, or a plain token.
Checked before it becomes an argument: it arrives from an index of
files, and an id starting with \"-\" would read as a flag.")

(defconst ghostherd--resume-checkable '(claude agy)
  "Kinds whose conversation can be checked to still exist before resuming.
claude keeps each in ~/.claude/projects, agy in ~/.gemini/.../brain (its
`:transcript'), so a missing file means the conversation is gone and
`-r'/`--conversation' would make the CLI exit (claude: \"No conversation
found\"; agy warns and starts fresh on its own).  grok's sessions live on
its server -- a bad id there is a 404, not a local file -- so it is not
checked here; the fresh-start backstop (`ghostherd--resume-backstop')
covers it.")

(defun ghostherd--conversation-gone-p (kind id)
  "Non-nil when KIND's conversation ID can be checked for sure and is no
longer on disk where its CLI keeps it (`:transcript').  nil when it is
there, or when KIND's store cannot be checked (grok, or no pattern)."
  (and (memq kind ghostherd--resume-checkable)
       (stringp id) (string-match-p ghostherd--session-id-re id)
       (when-let* ((place (plist-get (ghostherd--spec kind) :transcript)))
         (not (file-expand-wildcards (expand-file-name (format place id)) t)))))

(defun ghostherd--resume-args (kind id &optional fork)
  "Arguments that make KIND resume conversation ID, or nil if it cannot.
With FORK, branch a new conversation off it; nil if KIND cannot fork.
Nil, too, when ID's conversation is verifiably gone -- so a pruned one is
not handed to the CLI's resume flag, which would make it exit without
starting (`ghostherd--conversation-gone-p'); the caller starts fresh."
  (let* ((spec (ghostherd--spec kind))
         (resume (plist-get spec :resume-args))
         (forking (plist-get spec :fork-args)))
    (when (and resume (stringp id) (string-match-p ghostherd--session-id-re id)
               (or (not fork) forking)
               (not (ghostherd--conversation-gone-p kind id)))
      (append (mapcar (lambda (a) (format a id)) resume)
              (and fork forking)))))

;; The backstop: the pre-check verifies claude's and agy's conversations
;; from a local file, but grok's live on its server (a bad id is a 404, not
;; a missing file), and a claude id whose file is there could still be
;; refused.  So when an agent started on a resume dies at once -- the CLI
;; exiting on a conversation it could not open -- start it fresh, once, with
;; the kept id dropped.  A fresh start carries no resume flag, so it can
;; never loop.
(defconst ghostherd--resume-fail-window 25
  "Seconds after a resumed start within which a death is taken as a resume
failure, not a later crash: a CLI that cannot open its conversation exits
in a second or two, while a resume that worked is long up by now.")

(defun ghostherd--started-on-resume-p (session)
  "Non-nil when SESSION was started with a resume flag among its args."
  (when-let* ((kind (ghostherd-session-kind session))
              (flag (car (plist-get (ignore-errors (ghostherd--spec kind)) :resume-args))))
    (and (member flag (ghostherd-session-args session)) t)))

(defun ghostherd--respawn-fresh (session)
  "Respawn SESSION cold -- its args stripped of any resume flag.  A lead on
a CLI that takes no role flag re-reads its role, as a cold respawn does."
  (ghostherd--refuse-gone session "respawning")
  (let* ((kind (ghostherd-session-kind session))
         (recipe (ghostherd-session-recipe session))
         (role (plist-get recipe :role))
         (args (ghostherd--without-resume kind (plist-get recipe :args))))
    (ghostherd-kill session t)
    (let ((heir (apply #'ghostherd-spawn kind (plist-put (copy-sequence recipe) :args args))))
      (when (and heir (equal role "lead") (not (ghostherd--lead-args kind)))
        (ghostherd--when-ready heir (lambda () (ghostherd-prompt heir ghostherd--lead-role))))
      heir)))

(defun ghostherd--resume-backstop (session _old new)
  "On SESSION dying just after a resumed start, start it fresh once and drop
the kept conversation.  For `ghostherd-state-change-hook'."
  (when (and (eq new 'dead)
             (ghostherd--started-on-resume-p session)
             (let ((at (ghostherd-session-started-at session)))
               (and at (< (- (float-time) (float-time at)) ghostherd--resume-fail-window))))
    (let ((name (ghostherd-session-name session))
          (primary (equal (ghostherd-session-role session) "primary")))
      (ghostherd--log-add
       session 'life
       (format "resume failed (%s); starting %s fresh, the kept conversation dropped"
               (or (ghostherd-session-state-reason session) "it exited at once") name))
      ;; After this poll, not inside the state change, so killing and
      ;; respawning do not run while the herd is mid-sweep.
      (run-at-time
       0 nil
       (lambda ()
         (if primary
             (progn (ghostherd--primary-forget)
                    (when-let* ((s (ghostherd-get name))) (ghostherd-kill s t))
                    (ghostherd-primary-start))
           (when-let* ((s (ghostherd-get name)))
             ;; Its directory gone, it stays as it is, dead and marked:
             ;; started fresh it would come up in the home directory.
             (if (ghostherd--mark-gone s)
                 (ghostherd--log-add s 'life "not started fresh: its directory is gone")
               (ghostherd--respawn-fresh s)))))))))

(add-hook 'ghostherd-state-change-hook #'ghostherd--resume-backstop)

(defun ghostherd--resumed-by (id)
  "The live herd agent that was started on conversation ID, or nil.
Found by its arguments, which the recipe keeps across restarts.  An
agent forked from ID is on a conversation of its own and does not
count.  One that reached ID some other way is not found:
`ghostherd-memory-page' warns about a transcript written to recently."
  (let ((live (seq-filter #'ghostherd--session-live-p
                         (hash-table-values ghostherd--sessions))))
    ;; An agent's hooks name the conversation it is on, however it got
    ;; there; its arguments only say where it started.
    (or (seq-find (lambda (s) (equal (ghostherd-session-conversation s) id)) live)
        (seq-find (lambda (s)
                    (let ((args (ghostherd-session-args s))
                          (fork (ignore-errors
                                  (plist-get (ghostherd--spec (ghostherd-session-kind s))
                                             :fork-args))))
                      (and (member id args)
                           (not (seq-some (lambda (f) (member f args)) fork))
                           (not (ghostherd-session-conversation s)))))
                  live))))

(cl-defun ghostherd-resume (kind id directory &key fork title (display t))
  "Spawn a KIND agent in DIRECTORY on conversation ID, and show it --
unless DISPLAY is nil.  With FORK, branch a new conversation off it.
TITLE goes in the notes.  The agent is named KIND-<project>, made
unique, and takes the project's own default args before the resume
ones."
  (when (ghostherd--conversation-gone-p kind id)
    (user-error "%s's conversation %s no longer exists" kind id))
  (let* ((args (or (ghostherd--resume-args kind id fork)
                   (user-error "%s cannot %s a conversation by id" kind
                               (if fork "fork" "resume"))))
         (directory (if (and (stringp directory) (file-directory-p directory))
                        (file-name-as-directory (expand-file-name directory))
                      (user-error "No directory %s to resume in" directory)))
         (defaults (ghostherd-project-defaults directory))
         (base (or (and (eq (car defaults) kind) (cdr defaults))
                   (plist-get (ghostherd--spec kind) :args)))
         (leaf (file-name-nondirectory (directory-file-name directory)))
         (short (substring id 0 (min 8 (length id)))))
    (ghostherd-spawn
     kind
     :name (ghostherd--unique-name (format "%s-%s" kind leaf))
     :project (or (ghostherd--project-root directory) directory)
     :directory directory
     :args (append base args)
     :notes (format "%s %s%s" (if fork "forked from" "resumed") short
                    (if (and title (not (string-empty-p title))) (concat ": " title) ""))
     :display display)))

;;;###autoload
(defun ghostherd-new-pair ()
  "Spawn two agents side by side for the current project.
Prompts for left/right kinds and names (defaults: implementer + reviewer)."
  (interactive)
  (let* ((project (or (ghostherd--project-root)
                      (user-error "Not in a project")))
         (left-kind (intern
                     (completing-read
                      "Left agent kind: "
                      (mapcar #'symbol-name (ghostherd--agent-kinds))
                      nil t nil nil "claude")))
         (right-kind (intern
                      (completing-read
                       "Right agent kind: "
                       (mapcar #'symbol-name (ghostherd--agent-kinds))
                       nil t nil nil "claude")))
         (left-name (read-string "Left name: " "implementer"))
         (right-name (read-string "Right name: " "reviewer"))
         (left (ghostherd-spawn left-kind
                                :name left-name
                                :project project
                                :display nil))
         (right (ghostherd-spawn right-kind
                                 :name right-name
                                 :project project
                                 :display nil)))
    ;; Two Emacs windows, each showing one agent.  On tmux that is two
    ;; attach clients; it is emphatically not a tmux split -- pane layout
    ;; is something Emacs already does better than a herd manager should
    ;; reimplement.
    (ghostherd--sidebar-leave-overlay)
    (delete-other-windows)
    (switch-to-buffer (ghostherd--host-view left))
    (split-window-right)
    (other-window 1)
    (switch-to-buffer (ghostherd--host-view right))
    (ghostherd--sync-view-size (ghostherd-session-buffer left))
    (ghostherd--sync-view-size (ghostherd-session-buffer right))
    (message "Spawned %s (%s) | %s (%s)"
             left-name left-kind right-name right-kind)
    (list left right)))

(defun ghostherd--maybe-restore ()
  "Re-adopt hosted agents when the registry has nothing in it.

Cheap insurance against a transient blind spot.  `ghostherd--ensure-sessions'
runs from the mode-line and prunes anything whose host it cannot reach,
so one moment of a host being unreachable would otherwise drop the herd
from the UI until the mode was toggled -- even though every agent is
still running."
  (when (zerop (hash-table-count ghostherd--sessions))
    (ignore-errors (ghostherd-restore))))

(defun ghostherd--read-session (&optional prompt predicate)
  "Read a session via completing-read using PROMPT and optional PREDICATE."
  (ghostherd--maybe-restore)
  (ghostherd--ensure-sessions)
  (let* ((sessions (cl-remove-if-not
                    (or predicate #'identity)
                    (ghostherd-sessions)))
         (candidates (mapcar #'ghostherd--format-candidate sessions)))
    (unless candidates
      (user-error "No ghostherd sessions"))
    (let ((choice (completing-read
                   (or prompt "Session: ")
                   (ghostherd--session-completion-table candidates)
                   nil t)))
      (or (ghostherd-get choice)
          (user-error "Unknown session")))))

;;;###autoload
;;;###autoload
(defun ghostherd-explain (&optional session)
  "Explain how SESSION's state was decided.
Shows the scraped tail, every `:screen-rules' pattern that matched it,
and which one won.  Rule tuning is guesswork without this: a false
`blocked' looks identical to a real one until you can see that some
pattern matched text the agent merely printed.

Interactively, uses the sidebar row at point when there is one, and
otherwise prompts."
  (interactive)
  (let* ((session (or session
                      (and (derived-mode-p 'ghostherd-sidebar-mode)
                           (ghostherd--sidebar-session-at-point))
                      (ghostherd--read-session "Explain agent: ")))
         (kind (ghostherd-session-kind session))
         (spec (ignore-errors (ghostherd--spec kind)))
         (rules (plist-get spec :screen-rules))
         (live (ghostherd--session-live-p session))
         (tail (and live (ghostherd--host-capture session)))
         (box (plist-get spec :prompt-replaces-input))
         (spinner (plist-get spec :spinner))
         (hits (and tail rules (ghostherd--match-all-rules tail rules box spinner)))
         (winner (and tail rules (ghostherd--match-rules tail rules box spinner))))
    (ghostherd--sidebar-leave-overlay)
    (with-help-window "*ghostherd explain*"
      (with-current-buffer standard-output
        (insert (format "%s  (%s)\n\n" (ghostherd-session-name session) kind))
        (insert (format "  state    %s %s\n"
                        (ghostherd--state-glyph (ghostherd-session-state session))
                        (ghostherd-session-state session)))
        (insert (format "  reason   %s\n"
                        (or (ghostherd-session-state-reason session) "—")))
        (when (ghostherd-session-manual-state session)
          (insert (format "  manual   %s  (overrides detection until cleared)\n"
                          (ghostherd-session-manual-state session))))
        ;; Which authority decided this is the first thing to know, and a
        ;; report is the one that leaves no trace on the screen below.
        (when-let* ((entry (gethash (ghostherd-session-id session)
                                    ghostherd--reports)))
          (pcase-let ((`(,reported ,why ,at) entry))
            (insert (format "  reports  %s%s  (%s ago, trusted %s)\n"
                            reported
                            (if why (format "  %s" why) "")
                            (ghostherd--age-string at)
                            (if (eq reported 'waiting)
                                "until replaced"
                              (format "for %ss" ghostherd-report-ttl))))
            (unless (ghostherd--fresh-report session)
              (insert "           expired — detection has it back\n"))))
        (when-let* ((link (ghostherd--link session)))
          (insert (format "  talks    %s  (%s, by its hooks)\n"
                          (let ((c (plist-get link :conversation)))
                            (if (and c (not (string-empty-p c))) c "—"))
                          (plist-get link :cli)))
          (let ((said (plist-get link :last_head)))
            (when (and said (not (string-empty-p said)))
              (insert (format "  said     %s\n" said)))))
        (insert (format "  host     %s  (%s)\n"
                        (or (ghostherd-session-host-id session) "—")
                        (ghostherd-session-backend session)))
        (insert (format "  view     %s\n\n"
                        (let ((buffer (ghostherd-session-buffer session)))
                          (cond ((buffer-live-p buffer) (buffer-name buffer))
                                (live "detached — nobody is attached")
                                (t "—")))))

        (cond
         ((not live)
          (insert "Host is gone; no detection runs.\n"))
         ((null rules)
          (insert "No :screen-rules for this kind — state is forced to `working'.\n"))
         ((null hits)
          (insert "No rule matched. Falling back to `idle'.\n"))
         (t
          (insert "Matches, in precedence order (blocked > working > idle):\n\n")
          (pcase-dolist (`(,state . ,pattern) hits)
            (insert (format "  %-8s %-3s %s\n"
                            state
                            (if (equal (cons state pattern) winner) "->" "")
                            pattern)))
          (insert "\nOnly the first line wins.  A rule listed under a\n")
          (insert "higher-precedence state shadows every rule below it.\n")))
        (when (and box tail rules (ghostherd--input-at-foot-p tail)
                   (cl-some (lambda (p) (ghostherd--rule-matches-p p tail)) (alist-get 'blocked rules)))
          (insert "\nIts input box is at the foot, which a menu would take the place of:\n")
          (insert "what `blocked' matched there is the conversation's, not a prompt.\n"))

        (when tail
          (insert (format "\nScraped tail (last %d lines):\n"
                          ghostherd-screen-tail-lines))
          (insert (make-string 60 ?-) "\n")
          (insert tail)
          (unless (bolp) (insert "\n"))
          (insert (make-string 60 ?-) "\n"))))))

(defun ghostherd-switch ()
  "Switch to a ghostherd session (consult/completing-read)."
  (interactive)
  (ghostherd-visit (ghostherd--read-session "Switch to agent: ")))

;;;###autoload
(defun ghostherd-next-blocked ()
  "Jump to the next blocked (or done/unseen) session."
  (interactive)
  (ghostherd-poll-all)
  (let* ((priority '(blocked done))
         (sessions (ghostherd-sessions))
         (targets (cl-remove-if-not
                   (lambda (s)
                     (or (memq (ghostherd-session-state s) priority)
                         (not (ghostherd-session-seen s))))
                   sessions)))
    (unless targets
      (user-error "No blocked/done agents"))
    (let ((session (car targets)))
      (ghostherd-visit session)
      (message "Jumped to %s (%s)"
               (ghostherd-session-name session)
               (ghostherd-session-state session)))))

;;;###autoload
(defun ghostherd-kill (session &optional kill-buffer)
  "Remove SESSION from the herd.
When KILL-BUFFER is non-nil (the interactive default), also kill its buffer."
  (interactive
   (list (ghostherd--read-session "Kill session: ") t))
  (setq session (ghostherd-get session))
  (unless session
    (user-error "No such session"))
  (let ((buf (ghostherd-session-buffer session))
        (id (ghostherd-session-id session)))
    (remhash id ghostherd--sessions)
    (remhash id ghostherd--input-at)
    (remhash id ghostherd--idle-since)
    (remhash id ghostherd--reports)
    (remhash id ghostherd--screens)
    (remhash id ghostherd--panes)
    (remhash id ghostherd--feet)
    (remhash id ghostherd--queues)
    (remhash id ghostherd--started-by)
    (remhash id ghostherd--handoff-leads)
    (remhash id ghostherd--dir-gone)
    (ghostherd--log-add session 'life
                        (if kill-buffer "killed" "released from the herd"))
    (run-hook-with-args 'ghostherd-session-removed-hook session)
    (when kill-buffer
      ;; The host first: on tmux the buffer is only a client, so killing
      ;; it would detach and leave the agent running headless with
      ;; nothing in the herd pointing at it.
      (ghostherd--host-kill session)
      (when (buffer-live-p buf)
        (let ((kill-buffer-query-functions nil)
              (ghostel-query-before-killing nil))
          (ignore-errors (kill-buffer buf))))
      ;; A true kill, not a release: the agent is gone, so clean up its
      ;; worktree (only if merged and clean -- see the function).
      (ignore-errors (ghostherd--worktree-cleanup session)))
    (ghostherd--stop-poll-timer)
    (ghostherd--sidebar-refresh)
    (message "Killed session %s" (ghostherd-session-name session))))

(defun ghostherd-stop-gone (session)
  "Stop SESSION, whose directory is gone, as `herd stop' does: it leaves
the herd and its host is killed; its branch, if any, is left as it is.
The Inbox's and the card's Stop.  Refused when its directory is back --
what offered it was out of date.  Returns what to say."
  (setq session (or (ghostherd-get session) (user-error "No such session")))
  (let ((name (ghostherd-session-name session))
        (gone (ghostherd--mark-gone session t)))
    (unless gone
      (ghostherd--unmark-gone session)
      (user-error "%s's directory is back: not stopping it" name))
    (ghostherd--log-add session 'life "stopped by the user: its directory was gone")
    (ghostherd-kill session t)
    (format "Stopped %s; its %s was already gone" name
            (if (plist-get gone :worktree) "worktree" "directory"))))

;;;###autoload
;;;###autoload
(defun ghostherd-set-notes (session notes)
  "Set SESSION's NOTES -- the free-form role description.

Kept out of the session name on purpose: the name is an identifier that
buffer names and `ghostherd-get' depend on, so it cannot carry \"reviews
the auth branch, do not let it push\".  Notes show up in the switcher
annotation, which is where you are choosing between agents."
  (interactive
   (let ((s (ghostherd--read-session "Note for agent: ")))
     (list s (read-string "Note: " (ghostherd-session-notes s)))))
  (setq session (ghostherd-get session))
  (unless session
    (user-error "No such session"))
  (let ((notes (and notes (string-trim notes))))
    (setf (ghostherd-session-notes session)
          (and notes (not (string-empty-p notes)) notes)))
  (ghostherd--sidebar-refresh)
  (ghostherd-session-notes session))

(defun ghostherd-rename (session new-name)
  "Rename SESSION to NEW-NAME."
  (interactive
   (let* ((s (ghostherd--read-session "Rename session: "))
          (n (read-string "New name: " (ghostherd-session-name s))))
     (list s n)))
  (setq session (ghostherd-get session))
  (when (ghostherd-get new-name)
    (user-error "Name already in use: %s" new-name))
  (let ((old-id (ghostherd-session-id session)))
    (remhash old-id ghostherd--sessions)
    (setf (ghostherd-session-id session) new-name
          (ghostherd-session-name session) new-name)
    (puthash new-name session ghostherd--sessions)
    ;; Everything else keyed by id has to move with it.  A rename used to
    ;; drop the input grace and the idle-settle timestamp on the floor,
    ;; which was invisible; dropping a report is not, since it would put a
    ;; blocked row back under the rules mid-prompt.
    (dolist (table (list ghostherd--input-at ghostherd--idle-since
                         ghostherd--reports ghostherd--screens
                         ghostherd--dir-gone))
      (when-let* ((value (gethash old-id table)))
        (remhash old-id table)
        (puthash new-name value table)))
    ;; The host has its own name for the session -- a tmux session name,
    ;; the buffer name -- and it is the backend's job to keep it in step.
    (ghostherd--host-rename session new-name)
    (ghostherd--sidebar-refresh)
    (message "Renamed to %s" new-name)))


;;; Its model, switched as that CLI lets you
;;
;; Done in the running agent, the way you would, each step waiting for the
;; screen to show it; never by restarting it with `--model', which would
;; lose what the session set by hand -- its mode, its shells.  Each CLI is
;; different, so `:model-picker' says how (`:style'):
;;
;; claude -- its `/model NAME' saves NAME as every new session's default;
;; only its picker switches one session alone (`s' on a row).  So the herd
;; drives the picker: `/model', down to the row, ←/→ to the effort, `s'.
;;
;; grok -- `/model NAME EFFORT' takes them as words and shows them in its
;; input box's border; the herd types the command and reads the border.
;;
;; agy -- `/model' opens a picker, cursor on the current model; the herd
;; walks ↑/↓ to NAME, slides ←/→ to the effort, and Enters.  (It has a
;; search box, but ignores the herd's bracketed paste as a filter.)
;;
;; grok and agy have no session-only switch, so for them it becomes the
;; CLI's default (`:scope default'); claude stays this session alone.
;; Anything a step does not see, the herd does not press past: it backs
;; out and says why.

(defcustom ghostherd-model-switch-wait 6
  "Seconds each step of a model switch waits for the picker to show it."
  :type 'number
  :group 'ghostherd)

(defvar ghostherd--switching (make-hash-table :test 'equal)
  "Session id -> t while its model is being switched.")

(defconst ghostherd--picker-row-re
  "\\`[[:blank:]]*\\(❯\\)?[[:blank:]]*\\(?:[↑↓][[:blank:]]*\\)?\\([0-9]+\\)\\.[[:blank:]]+\\(.+?\\)\\(?:[[:blank:]]\\{2,\\}.*\\)?\\'"
  "A row of claude's model picker: ❯ on the one it is on, its number, its
name -- the description after two spaces or more is not part of it.")

(defconst ghostherd--picker-effort-re
  "\\([[:alpha:]]+\\) effort\\(?: (default)\\)? ←/→"
  "The picker's effort line, under its rows: \"◉ xHigh effort ←/→ to adjust\".")

(defun ghostherd--model-picker (text)
  "claude's model picker on screen TEXT: (:rows ROWS :cursor N :effort
LEVEL), or nil when it does not show one.  ROWS is ((N . NAME) ...) as
it numbers them, the newest of a family first, a name without its tick;
N the row it is on; LEVEL the effort that row would run at, in lower
case, or nil for a model that has none.  Only what is under its title is
read: the conversation above can hold a numbered list too."
  (let* ((lines (split-string (or text "") "\n"))
         (from (cl-position-if (lambda (l) (string-match-p "\\`[[:blank:]]*Select model" l)) lines
                               :from-end t))
         rows cursor effort)
    (when from
      (cl-loop for line in (nthcdr (1+ from) lines)
               until (string-match-p "s to use this session only" line)
               do (cond
                   ((string-match ghostherd--picker-row-re line)
                    (let ((n (string-to-number (match-string 2 line))))
                      (push (cons n (string-trim (replace-regexp-in-string "[✔✓]" "" (match-string 3 line))))
                            rows)
                      (when (match-beginning 1) (setq cursor n))))
                   ((string-match ghostherd--picker-effort-re line)
                    (setq effort (downcase (match-string 1 line)))))
               finally return (and rows cursor
                                   (list :rows (nreverse rows) :cursor cursor :effort effort))))))

(defun ghostherd--picker-row (rows name)
  "The number of the first of ROWS whose name begins with NAME, a word:
the newest of that family.  Nil when none does."
  (let ((case-fold-search t))
    (car (seq-find (lambda (row) (string-match-p (concat "\\`" (regexp-quote name) "\\b") (cdr row)))
                   rows))))

(defun ghostherd--model-said (text)
  "What claude said its last `/model' did, on screen TEXT, or nil: the
line under the command, \"Set model to Sonnet 5.5 for this session only
with xhigh effort\"."
  (let ((lines (split-string (or text "") "\n")) said)
    (while lines
      (when (string-match-p "\\`❯[[:blank:]]*/model[[:blank:]]*\\'" (car lines))
        (setq said (seq-some (lambda (l)
                               (and (string-match "\\(\\(?:Set\\|Kept\\) model .*\\)" l)
                                    (string-trim (match-string 1 l))))
                             (seq-take (cdr lines) 3))))
      (setq lines (cdr lines)))
    said))

(defun ghostherd--grok-model-line (text)
  "grok's model and effort, read from the line in its input box's border
-- \"... Grok 4.6 (medium) · always-approve ...\" -> (\"Grok 4.6\" . \"medium\")
-- the last one on screen, which is the live one; nil when there is none."
  (let ((lines (split-string (or text "") "\n")) (case-fold-search nil) found)
    (dolist (l lines found)
      (when (string-match
             "\\([[:upper:]][[:alnum:].[:blank:]-]*?\\)[[:blank:]]+(\\(low\\|medium\\|high\\|xhigh\\))"
             l)
        (setq found (cons (string-trim (match-string 1 l)) (match-string 2 l)))))))

(defun ghostherd--agy-model-line (text)
  "agy's model and effort, read from its footer status line -- \"… Claude
Opus 5.5 · high\", or \"Gemini 3.8 Flash · high · AI: …\" -> (\"Claude Opus
5.5\" . \"high\") -- the last one on screen; nil when there is none."
  (let ((lines (split-string (or text "") "\n")) (case-fold-search nil) found)
    (dolist (l lines found)
      (when (string-match
             "\\([[:upper:]][[:alnum:].[:blank:]-]*?\\)[[:blank:]]+·[[:blank:]]+\\(low\\|medium\\|high\\|xhigh\\|max\\)\\(?:[[:blank:]]+·\\|[[:blank:]]*\\'\\)"
             l)
        (setq found (cons (string-trim (match-string 1 l)) (match-string 2 l)))))))

(defun ghostherd--agy-picker (text)
  "agy's \"Switch Model\" picker on screen TEXT: (:rows (NAME...) :cursor
NAME :efforts (WORD...) :effort WORD), or nil when it shows none.  ROWS
are the names now visible -- the list is windowed and searched, so not all
-- CURSOR the one marked `>'; EFFORTS the steps its slider offers for that
row, lowest first, EFFORT the one its `◉' sits on."
  (let* ((lines (split-string (or text "") "\n"))
         (top (cl-position-if (lambda (l) (string-match-p "\\`[[:blank:]]*Switch Model[[:blank:]]*\\'" l))
                              lines :from-end t)))
    (when (and top (seq-some (lambda (l) (string-match-p "Navigate" l)) (nthcdr top lines)))
      (let* ((rest (nthcdr (1+ top) lines))
             (eff (cl-position-if (lambda (l) (and (string-match-p "Effort" l)
                                                   (string-match-p "[●◉○]" l)))
                                  rest))
             rows cursor efforts effort)
        ;; The model rows, between the search box and the effort slider.
        (dolist (l (if eff (seq-take rest eff) rest))
          (when (and (string-match "\\`[[:blank:]]*\\(>\\)?[[:blank:]]*\\([[:alpha:]][^\n]*?\\)[[:blank:]]*\\'" l)
                     (not (string-prefix-p "Search" (match-string 2 l))))
            (let ((nm (replace-regexp-in-string "[[:blank:]]+(current)\\'" ""
                                                (string-trim (match-string 2 l)))))
              (push nm rows)
              (when (match-beginning 1) (setq cursor nm)))))
        ;; The effort slider's stops -- ● done, ◉ here, ○ to go -- and the
        ;; words under it name them.
        (when eff
          (let* ((stops (seq-filter (lambda (c) (memq c '(?● ?◉ ?○)))
                                    (append (nth eff rest) nil)))
                 (here (cl-position ?◉ stops))
                 (labels (seq-find (lambda (l) (string-match-p "\\b\\(low\\|medium\\|high\\)\\b" l))
                                   (nthcdr (1+ eff) rest))))
            (when labels
              (setq efforts (seq-filter (lambda (w) (member w '("low" "medium" "high")))
                                        (split-string labels))))
            (when (and here efforts (< here (length efforts)))
              (setq effort (nth here efforts)))))
        (and rows (list :rows (nreverse rows) :cursor cursor :efforts efforts :effort effort))))))

(defun ghostherd--agy-said (text)
  "What agy said its last `/model' did: \"Model set to Claude Opus 5.5
(High)\", or \"Exited /model command\" when it was cancelled -- the last
such line, nil when none."
  (let ((lines (split-string (or text "") "\n")) said)
    (dolist (l lines said)
      (when (string-match "\\(Model set to .*\\|Exited /model command\\)" l)
        (setq said (string-trim (match-string 1 l)))))))

(defun ghostherd--await-screen (session pred then fail &optional wait)
  "Call THEN with what PRED makes of SESSION's screen once it is non-nil,
looking five times a second; FAIL after WAIT seconds, by default
`ghostherd-model-switch-wait', or as soon as SESSION is gone."
  (let ((until (+ (float-time) (or wait ghostherd-model-switch-wait)))
        (id (ghostherd-session-id session)))
    (cl-labels ((look ()
                  (let* ((here (eq (gethash id ghostherd--sessions) session))
                         (got (and here (funcall pred (ignore-errors (ghostherd--host-capture session 80))))))
                    (cond (got (funcall then got))
                          ((or (not here) (> (float-time) until)) (funcall fail))
                          (t (run-at-time 0.2 nil #'look))))))
      (look))))

(defun ghostherd--model-switch-via-picker (session efforts model effort finish)
  "Drive claude's picker: `/model', down to MODEL, ←/→ to EFFORT, `s'.
EFFORTS is the kind's effort steps; call FINISH with OK and what it said."
  (cl-labels
      ((abandon (why)
         (when (ghostherd--model-picker (ignore-errors (ghostherd--host-capture session 80)))
           (ignore-errors (ghostherd--host-send-keys session '("esc"))))
         (funcall finish nil why))
       (press (key n)
         (when (> n 0) (ghostherd--host-send-keys session (make-list n key))))
       (confirm (&rest _)
         (press "s" 1)
         (ghostherd--await-screen
          session
          (lambda (text) (and (not (ghostherd--model-picker text)) (ghostherd--model-said text)))
          (lambda (said) (if (string-prefix-p "Set " said) (funcall finish t said) (funcall finish nil said)))
          (lambda () (abandon "it did not say it switched"))))
       (to-effort (p)
         (let* ((have (cl-position (plist-get p :effort) efforts :test #'equal))
                (want (and effort (cl-position effort efforts :test #'equal)))
                (by (and have want (- want have))))
           (if (memq by '(nil 0))
               (confirm)
             (press (if (> by 0) "right" "left") (abs by))
             ;; A model whose top is lower stops there; what it set is
             ;; what it says it set.
             (ghostherd--await-screen
              session
              (lambda (text) (equal (plist-get (ghostherd--model-picker text) :effort) effort))
              #'confirm #'confirm (min 1.5 ghostherd-model-switch-wait)))))
       (to-row (p)
         (let ((row (ghostherd--picker-row (plist-get p :rows) model))
               (at (plist-get p :cursor)))
           (if (not row)
               (abandon (format "its picker has no %s" model))
             (press (if (> row at) "down" "up") (abs (- row at)))
             (ghostherd--await-screen
              session
              (lambda (text) (let ((q (ghostherd--model-picker text)))
                               (and q (eql (plist-get q :cursor) row) q)))
              #'to-effort
              (lambda () (abandon (format "it did not reach %s in its picker" model))))))))
    (ghostherd--host-send-text session "/model" t)
    (ghostherd--await-screen
     session #'ghostherd--model-picker #'to-row
     (lambda () (abandon "its model picker did not open")))))

(defun ghostherd--model-switch-via-command (session picker model effort finish)
  "Switch by typing grok's `/model MODEL EFFORT', then reading the model
line it shows (`:labels' names it) to see it took.  Writes grok's default."
  (let ((want (or (cdr (assoc model (plist-get picker :labels))) model)))
    (ghostherd--host-send-text
     session (concat "/model " model (and effort (concat " " effort))) t)
    (ghostherd--await-screen
     session
     (lambda (text) (let ((cur (ghostherd--grok-model-line text)))
                      (and cur (equal (car cur) want)
                           (or (null effort) (equal (cdr cur) effort))
                           cur)))
     (lambda (cur) (funcall finish t (format "Set model to %s with %s effort (now its default)"
                                             (car cur) (cdr cur))))
     (lambda () (funcall finish nil (format "it did not switch to %s" want))))))

(defun ghostherd--model-switch-via-search (session model effort finish)
  "Drive agy's model picker: `/model', ↑/↓ to MODEL from the current one,
←/→ to EFFORT, Enter; read \"Model set to ...\" to see it took.  The
cursor starts on the current model and the effort steps are the ones the
chosen row's slider offers.  Writes agy's default.

The picker has a search box, but the herd sends text as one bracketed
paste, which the box does not take as a filter, so the rows are walked by
arrow instead -- which is what its footer says to do."
  (cl-labels
      ((abandon (why)
         (when (ghostherd--agy-picker (ignore-errors (ghostherd--host-capture session 80)))
           ;; One Escape clears a search, another backs out of the picker.
           (ignore-errors (ghostherd--host-send-keys session '("esc" "esc"))))
         (funcall finish nil why))
       (press (key n)
         (when (> n 0) (ghostherd--host-send-keys session (make-list n key))))
       (select ()
         (ghostherd--host-send-keys session '("return"))
         (ghostherd--await-screen
          session
          (lambda (text) (and (not (ghostherd--agy-picker text)) (ghostherd--agy-said text)))
          (lambda (said) (if (string-prefix-p "Model set to" said)
                             (funcall finish t (concat said " (now its default)"))
                           (funcall finish nil said)))
          (lambda () (abandon "it did not say it switched"))))
       (to-effort (p)
         (let* ((offered (plist-get p :efforts))
                (have (cl-position (plist-get p :effort) offered :test #'equal))
                (want (and effort (cl-position effort offered :test #'equal)))
                (by (and have want (- want have))))
           (if (memq by '(nil 0))
               (select)
             (press (if (> by 0) "right" "left") (abs by))
             (ghostherd--await-screen
              session
              (lambda (text) (equal (plist-get (ghostherd--agy-picker text) :effort) effort))
              (lambda (_) (select)) #'select (min 1.5 ghostherd-model-switch-wait)))))
       (to-row (p)
         ;; agy 1.4.0's picker is arrow-driven -- the footer says so: ↑/↓
         ;; the rows, ←/→ the effort, Enter to select -- with the cursor on
         ;; the current model, and the list wraps top to bottom.  Walk to
         ;; MODEL one row at a time, the shorter way round, re-reading after
         ;; each step: a key the just-opened picker drops leaves the cursor
         ;; put, so the next round presses again rather than losing the move.
         ;; (Typing the name into its search box did filter, but only from a
         ;; burst of keystrokes; the herd sends text as one bracketed paste,
         ;; which the box does not take, so the list never pared and Enter
         ;; kept the model it opened on.)
         (let ((want (cl-position-if (lambda (nm) (string-prefix-p model nm))
                                     (plist-get p :rows))))
           (if (not want)
               (abandon (format "its picker has no %s" model))
             (cl-labels
                 ((go (q budget)
                    (let* ((rows (plist-get q :rows))
                           (nm (plist-get q :cursor))
                           (at (and nm (cl-position nm rows :test #'equal)))
                           (n (length rows)))
                      (cond
                       ((and nm (string-prefix-p model nm)) (to-effort q))
                       ((or (not at) (<= budget 0))
                        (abandon (format "it did not reach %s in its picker" model)))
                       (t
                        (let* ((down (mod (- want at) n))
                               (dir (if (<= down (- n down)) "down" "up"))
                               (next (nth (mod (+ at (if (equal dir "down") 1 -1)) n) rows)))
                          (press dir 1)
                          (ghostherd--await-screen
                           session
                           (lambda (text) (let ((q2 (ghostherd--agy-picker text)))
                                            (and q2 (equal (plist-get q2 :cursor) next) q2)))
                           (lambda (q2) (go q2 (1- budget)))
                           ;; The step may be slow, or lost to a picker not yet
                           ;; ready; either way re-read and let `go' decide again.
                           (lambda ()
                             (ghostherd--await-screen
                              session #'ghostherd--agy-picker
                              (lambda (q2) (go q2 (1- budget)))
                              (lambda () (abandon (format "it did not reach %s in its picker" model)))))
                           (min 1.0 ghostherd-model-switch-wait))))))))
               (go p (* 3 (length (plist-get p :rows)))))))))
    (ghostherd--host-send-text session "/model" t)
    (ghostherd--await-screen
     session #'ghostherd--agy-picker #'to-row
     (lambda () (funcall finish nil "its model picker did not open")))))

(defun ghostherd-switch-model (session model &optional effort done)
  "Switch SESSION to MODEL, at EFFORT when given, as that CLI allows.
MODEL is one of the names its kind's `:model-picker' lists, EFFORT one of
its levels.  Done in the running agent, as you would; not while it works,
or with words of yours in its input box.  For claude this is the session
alone; grok and agy have no such switch, so for them it becomes the CLI's
default.  DONE, when given, is called with OK and what it said it did --
or why it was not."
  (interactive
   (let* ((s (ghostherd--read-session "Switch the model of: "))
          (picker (plist-get (ignore-errors (ghostherd--spec (ghostherd-session-kind s))) :model-picker)))
     (unless picker
       (user-error "The herd does not know how to switch %s's model" (ghostherd-session-kind s)))
     (list s
           (completing-read "Model: " (plist-get picker :models) nil t)
           (let ((e (completing-read "Effort (empty: as it is): "
                                     (plist-get picker :efforts) nil t)))
             (and (not (string-empty-p e)) e)))))
  (setq session (ghostherd-get session))
  (unless session
    (user-error "No such session"))
  (let* ((name (ghostherd-session-name session))
         (id (ghostherd-session-id session))
         (picker (plist-get (ignore-errors (ghostherd--spec (ghostherd-session-kind session)))
                            :model-picker))
         (efforts (plist-get picker :efforts))
         (input (plist-get (gethash id ghostherd--feet) :input)))
    (cond
     ((null picker)
      (user-error "The herd does not know how to switch %s's model" (ghostherd-session-kind session)))
     ((not (member model (plist-get picker :models)))
      (user-error "No model %s for %s: one of %s" model name
                  (string-join (plist-get picker :models) ", ")))
     ((and effort (not (member effort efforts)))
      (user-error "No effort %s: one of %s" effort (string-join efforts ", ")))
     ((gethash id ghostherd--switching)
      (user-error "%s is switching its model already" name))
     ((not (memq (ghostherd-session-state session) '(idle done waiting)))
      (user-error "%s is %s: switch its model once it is idle" name (ghostherd-session-state session)))
     ((and input (not (equal input (ghostherd-session-suggestion session))))
      (user-error "%s has words in its input box: send them or clear them first" name)))
    (puthash id t ghostherd--switching)
    (cl-labels ((finish (ok what)
                  (remhash id ghostherd--switching)
                  (ghostherd--log-add session 'life (if ok what (concat "model not switched: " what)))
                  (when done (funcall done ok what))))
      (condition-case err
          (pcase (plist-get picker :style)
            ('command (ghostherd--model-switch-via-command session picker model effort #'finish))
            ('search (ghostherd--model-switch-via-search session model effort #'finish))
            (_ (ghostherd--model-switch-via-picker session efforts model effort #'finish)))
        (error (finish nil (error-message-string err)))))))

;;;###autoload
(defun ghostherd-mark-state (session state)
  "Manually set SESSION state (sticky until cleared).
STATE is one of working, blocked, idle, done, dead, or auto (clear)."
  (interactive
   (list (ghostherd--read-session "Session: ")
         (intern (completing-read
                  "State: "
                  '("auto" "working" "blocked" "idle" "done" "dead")
                  nil t))))
  (setq session (ghostherd-get session))
  (if (eq state 'auto)
      (progn
        (setf (ghostherd-session-manual-state session) nil)
        (ghostherd-poll-session session)
        (message "Auto state for %s" (ghostherd-session-name session)))
    (setf (ghostherd-session-manual-state session) state)
    (ghostherd--set-state session state "manual")
    (message "%s → %s (manual)" (ghostherd-session-name session) state)))

(provide 'ghostherd-spawn)

;; Local Variables:
;; generated-autoload-load-name: "ghostherd"
;; End:

;;; ghostherd-spawn.el ends here

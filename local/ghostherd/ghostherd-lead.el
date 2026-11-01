;;; ghostherd-lead.el --- A project's lead: its role, what it starts, handoffs -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of ghostherd (see ghostherd.el).  A project's lead: its role
;; and the memory it opens with, what you tell it, the agents it
;; starts and stops through the herd, and handoffs -- work for another
;; project's lead.

;;; Code:

(require 'ghostherd-core)
(require 'ghostherd-worktree)
(require 'ghostherd-log)
(require 'ghostherd-screen)
(require 'ghostherd-spawn)
(require 'ghostherd-comm)
(require 'ghostherd-mail)

(declare-function ghostherd--primary-dir-p "ghostherd" (dir))
(declare-function ghostherd--push "ghostherd" (kind session &rest fields))
(declare-function ghostherd-primary-say "ghostherd" (text))

;;; A project's lead
;;
;; One agent per project plans the work and hands it out: you talk to
;; it in the project's chat, it asks the others, and what it says at
;; the end of a turn is its report.  It is an ordinary agent with a
;; role -- its own CLI, transcript and column -- not a bot of the
;; sidecar's.  It starts the first time you write to it.

(defcustom ghostherd-lead-kind 'claude
  "The CLI a project's lead runs on.
Its model is the CLI's to change: tell a claude lead `/model sonnet'."
  :type 'symbol
  :group 'ghostherd)

(defconst ghostherd--memory-rules "\
The herd keeps shared memory for this project -- curated facts every
agent of the project sees, whatever its CLI, and the user's own
preferences across projects.  It is the source of truth: use it, not
your own memory.  Read it with \"$GHOSTHERD_HERD\" recall (no argument
prints the index; a query prints the matching facts in full).  Add a
fact with \"$GHOSTHERD_HERD\" remember [--type project|feedback|reference],
update one with remember --name NAME (when remember says your text
restates a fact, it tells you what to add), and drop one with
\"$GHOSTHERD_HERD\" forget NAME.  What this project's past sessions
said: \"$GHOSTHERD_HERD\" search WORDS."
  "Shared-memory rules, given to a grok or agy agent as it starts.")

(defun ghostherd--memory-link-claude (session)
  "Point a claude SESSION's memory dir at its project's shared store.
Asks the sidecar (memory_link_claude), which resolves a worktree to its
main checkout so worktrees share the main store, and never clobbers a real
memory dir with facts.  A no-op for other CLIs, or when the sidecar is not
up yet (the migration covers the existing dirs; a later spawn covers this)."
  (when (and (eq (ghostherd-session-kind session) 'claude)
             (fboundp 'ghostherd-memory-rpc-url) (ghostherd-memory-rpc-url)
             (fboundp 'ghostherd-memory-request-async))
    (let* ((project (ghostherd-session-project session))
           (wt (ghostherd-session-worktree session))
           (dir (or (and wt (plist-get wt :path)) project)))
      (when (and project dir)
        (ghostherd-memory-request-async
         "memory_link_claude" #'ignore
         (list :cwd (expand-file-name dir) :project (expand-file-name project))
         #'ignore)))))

(add-hook 'ghostherd-session-created-hook #'ghostherd--memory-link-claude)

(defcustom ghostherd-memory-opening-max 4000
  "Most characters of the memory index in a grok/agy agent's opening.
The index is every fact that is not a rule, one line each, then a line
naming the facts that did not fit -- whole lines only, so none is cut
and none dropped silently.  The rules (the user's facts and the feedback
ones) come before it in full, outside this cap: they say how to work.
See the sidecar's `shared_memory.opening'."
  :type 'integer
  :group 'ghostherd)

(defcustom ghostherd-memory-opening-timeout 3
  "Seconds to wait for the sidecar to compose a grok/agy opening.
Past it, or with no sidecar, the opening is the index lines read from
MEMORY.md, capped the same way."
  :type 'number
  :group 'ghostherd)

(defcustom ghostherd-memory-dir "~/.ghostherd/memory"
  "The shared-memory store root -- the same as the sidecar's
GHOSTHERD_SHARED_MEMORY_DIR.  One directory per project (its main
checkout's slug) plus `user'; each holds the fact files and MEMORY.md."
  :type 'directory
  :group 'ghostherd)

(defconst ghostherd--memory-mark "### ghostherd shared memory — do not edit ###"
  "Marks the memory block inside grok's --rules, so a re-launch replaces it.")

(defun ghostherd--memory-slug (path)
  "PATH as a store key -- the same scheme as server/shared_memory.py:
`/' and `.' become `-' in the absolute, un-slashed path."
  (replace-regexp-in-string "[/.]" "-" (directory-file-name (expand-file-name path))))

(defun ghostherd--memory-index (project)
  "PROJECT store's MEMORY.md as the agent should see it: the section and
fact lines (one per fact, the user facts and the project's), or nil when
there is no store yet.  Read straight from the file -- the herd keeps it
current -- so it needs no sidecar."
  (let ((f (expand-file-name (concat (ghostherd--memory-slug project) "/MEMORY.md")
                             ghostherd-memory-dir)))
    (when (file-readable-p f)
      (with-temp-buffer
        (insert-file-contents f)
        (let (lines)
          (goto-char (point-min))
          (while (not (eobp))
            (let ((l (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))))
              (when (string-match-p "\\`\\(- \\|## \\)" l)
                (push l lines)))
            (forward-line 1))
          (and lines (string-join (nreverse lines) "\n")))))))

(defun ghostherd--memory-opening-composed (project &optional agent)
  "PROJECT's opening as the sidecar composes it (`memory_opening'): the
rules in full, then the other facts within `ghostherd-memory-opening-max',
then the names of what did not fit.  The text, \"\" for an empty store,
or nil when the sidecar is not there to ask or does not answer in
`ghostherd-memory-opening-timeout'.  AGENT, the one starting, is who the
sidecar counts the rules as reaching."
  (when (and (fboundp 'ghostherd-memory-rpc-url) (ghostherd-memory-rpc-url)
             (fboundp 'ghostherd-memory-request))
    (condition-case nil
        (let ((r (ghostherd-memory-request
                  "memory_opening"
                  (append (list :project (expand-file-name project)
                                :max ghostherd-memory-opening-max)
                          (and agent (list :agent agent)))
                  ghostherd-memory-opening-timeout)))
          (and (stringp (plist-get r :text)) (plist-get r :text)))
      (error nil))))

(defun ghostherd--memory-opening-lines (index max)
  "INDEX's lines that fit in MAX characters, whole, and a note of how many
did not -- the opening without the sidecar, which alone can read the
rules out of the fact files."
  (let* ((lines (split-string index "\n"))
         (facts (seq-count (lambda (l) (string-prefix-p "- " l)) lines))
         ;; Room for the note, when not every line fits.
         (room (if (<= (length index) max) max (- max 80)))
         (kept nil) (used 0))
    (catch 'full
      (dolist (l lines)
        (when (> (+ used (length l) 1) room)
          (throw 'full nil))
        (push l kept)
        (setq used (+ used (length l) 1))))
    (let ((left (- facts (seq-count (lambda (l) (string-prefix-p "- " l)) kept))))
      (concat (string-join (nreverse kept) "\n")
              (if (> left 0)
                  (format "\n%d more, not listed — \"$GHOSTHERD_HERD\" recall <topic> for any" left)
                "")))))

(defun ghostherd--memory-opening (project &optional agent)
  "The shared-memory opening for a grok or agy agent in PROJECT -- AGENT,
when it is named.
The sidecar composes it (`ghostherd--memory-opening-composed'): the
project's rules in full, then every other fact one line each within
`ghostherd-memory-opening-max', then the names of what did not fit.
Without the sidecar, the index lines from MEMORY.md, whole, within the
same cap, and how many were left out.  Before either, the rule to recall
for the full text and anything newer; just that rule when the store is
empty or absent."
  (let ((composed (ghostherd--memory-opening-composed project agent))
        (head "\n\nThe project's memory as you start (\"$GHOSTHERD_HERD\" recall \
<topic> gives the full text of any fact, and anything newer).  "))
    (cond
     ((and composed (not (string-empty-p (string-trim composed))))
      (concat ghostherd--memory-rules head (string-trim-right composed)))
     (composed ghostherd--memory-rules)
     (t
      (let ((index (ghostherd--memory-index project)))
        (if (not (and index (not (string-empty-p index))))
            ghostherd--memory-rules
          (concat ghostherd--memory-rules head "One line per fact:\n\n"
                  (ghostherd--memory-opening-lines index ghostherd-memory-opening-max))))))))

(defun ghostherd--args-with-memory (project args &optional agent)
  "ARGS with the current memory opening folded into grok's --rules --
AGENT's, when it is named.
A previous memory block (from a re-launch, marked by `ghostherd--memory-mark')
is replaced, so there is always exactly one, with the current index; any role
text already in --rules is kept before it."
  (let ((opening (ghostherd--memory-opening project agent))
        (rules nil) (out nil))
    (while args
      (let ((a (pop args)))
        (if (and (equal a "--rules") args)
            (setq rules (pop args))
          (push a out))))
    (when (and rules (string-match (regexp-quote ghostherd--memory-mark) rules))
      (setq rules (string-trim-right (substring rules 0 (match-beginning 0)))))
    (let ((block (concat ghostherd--memory-mark "\n" opening)))
      (append (nreverse out)
              (list "--rules" (if (and rules (not (string-empty-p rules)))
                                  (concat rules "\n\n" block)
                                block))))))

(defconst ghostherd--lead-role "\
You are the lead of this project's herd of agents: you plan the work,
hand it out to the other agents, and report to the user.

The user talks to you in the project's chat.  What you say at the end of
a turn is what they read there: say what you decided, who is doing what,
what came back, and what you need from them -- briefly, and in the
language they write to you in.  When you need the user to decide
something, end your turn with the question itself, so it reaches their
Inbox as a question to answer.

Your team: \"$GHOSTHERD_HERD\" list shows every agent with its kind,
state, notes and project, and the directory and branch it works in;
yours are the ones in this project.  You see what an agent does only
through what it answers you.

Some agents work in their own git worktree -- their own directory on
their own branch off the project, so two can write code at once without
colliding.  Name an agent's path and branch in the brief you give it, so
it knows where to work; and read another agent's work by its branch
rather than its files -- \"git diff BASE...BRANCH\", where BASE is the
branch it came off (usually the project's main) and BRANCH is the one
\"$GHOSTHERD_HERD\" list shows for it.  Do not edit in another agent's
worktree.  A worktree's dependencies are already there -- node_modules
a copy-on-write clone of the main checkout's, a uv project's venv its
own -- so tell its agent to use \"npm install\", not \"npm ci\", which
deletes the clone and installs a full copy.

Hand work out as an ask, run in the background so you can go on:

  \"$GHOSTHERD_HERD\" ask NAME --wait <<'HERD'
  the work, and what you want back
  HERD

You are told when it finishes; its output is the answer.  Ask several
agents at once when the work allows.

Hand out outcomes, not steps: \"build it and get it through qa, four
rounds at most\" to rd lets rd and qa go back and forth without you in
the middle, and you get the result.  Give the rounds to the herd as
well -- ask rd with --rounds 4 -- and it holds rd to them: past four
asks of qa it refuses the fifth, and rd answers you with where it
stands.  Without --rounds it allows five.  An agent you asked may ask
others; nobody may ask you back while you wait on them -- the herd
refuses it.

Do small things yourself: read a file to plan, answer from what you
know.  Do not do an agent's work for it.

When the team has nobody for a job, start one yourself:

  \"$GHOSTHERD_HERD\" start KIND [--worktree [BRANCH] | --no-worktree] [--name NAME] [--notes TEXT]

KIND is claude, grok or agy.  It prints the agent's name (and its
worktree), which you then ask.  It reuses an idle agent you started whose
branch is merged and worktree clean -- the same agent, with its context,
fresh off the main branch -- before starting a new one; pass --new to
force a fresh one.  A worktree is the default once another agent works in
the checkout, so two do not collide; --no-worktree runs it in the main
checkout.  The herd caps how many you may have up (it lists them when you
hit the cap) and refuses a used-up kind, naming one with quota.

Queue rather than start when the work touches the same files as a busy
agent's (its files are in \"$GHOSTHERD_HERD\" list), is small, waits on
that agent's result, or needs its context: send it as an ask that waits
its turn behind the one it has.  Start or reuse an agent when the area is
independent and the work is substantial enough to run on its own.  Several
agents of one kind draw on one quota, so do not start more than the work
needs.

Stop an agent you started when the user says so or its work is done:

  \"$GHOSTHERD_HERD\" stop NAME

It refuses one still owing an ask; a merged, clean worktree is removed, an
unmerged one kept.  Keep an agent while more of its work is likely rather
than stopping and restarting it from scratch.  Either way -- started,
reused or stopped -- tell the user, briefly, in the chat.

When an ask fails, or comes back as the herd's answer rather than the
agent's, say what happened and decide: ask again, ask someone else, or
ask the user.

When the work needs a change in another project -- an API it serves, a
config it owns, a bug on its side -- do not edit its checkout, and do
not have your agents do it: you do not know its rules, and its own
agents may be at work there.  Hand it to that project's lead:

  \"$GHOSTHERD_HERD\" handoff PROJECT --wait <<'HERD'
  the brief
  HERD

PROJECT is its path or its name (unnoiac).  Brief it for a lead who
knows nothing of your work: what you found; the evidence -- files and
lines, logs, tickets, commits; what is needed of that project; how it
will know it is done; and who waits on it.  It is an ask: run it in the
background, and its answer comes back to you.  A project with no lead
gets one, started for it.  Your agents do not hand off -- the herd
refuses them -- they tell you in their answers, and you decide.

A handoff may come to you from another project: the herd says so where
it begins.  Take it in as your own work -- plan it here, by this
project's rules, with your agents -- and answer with the result: what
was done, where (branch, commit, pull request), and what is left for
the sender.  It cannot be asked while it waits on you: put a question
in your answer, and it can hand off again.

When an agent is stuck for good -- its CLI out of credits or rate
limited, or wedged with no way on -- hand its work to a fresh agent on
another CLI:

  \"$GHOSTHERD_HERD\" relay NAME [KIND] --reason \"why\"

It reads where the work stood and starts a new agent of KIND that takes
the name and carries on.  Leave KIND off to let the herd pick a CLI with
usage left; name one (claude, grok, agy) to choose.  You may relay only
your own project's agents, not yourself.

The herd keeps shared memory for this project -- curated facts every
agent sees, whatever its CLI, and the user's preferences across
projects.  Read it with \"$GHOSTHERD_HERD\" recall (no argument prints
the index; a query prints the matching facts in full), add a fact with
\"$GHOSTHERD_HERD\" remember, drop one with \"$GHOSTHERD_HERD\" forget.
It is the source of truth: use it, not notes kept elsewhere.  Record what
the team learns -- a decision, a gotcha, how a thing is done -- so the
next agent starts with it.  If you are claude this is your own memory
dir: keep writing it as you do, and the herd shares it with the project.

What the project works on -- its branch, its pull request, the ClickUp
tickets they and the team's conversations name -- opens the user's
first message to you, and their next one whenever it changes.  Read the
tickets and the pull request for the work, with whatever tools you have
for them; change none of them -- no ticket's status, fields or
comments, nothing on the pull request -- unless the user asks you to.

The user may also write to an agent directly.  When they have since you
last heard from them, their next message to you starts with a note
saying so: take it into account rather than undoing it.
"
  "The lead's role: given to it as it starts, or with its first message.")

(defun ghostherd--lead-args (kind)
  "Arguments that give a KIND agent the lead's role as it starts.
nil for a CLI that takes none: its first message carries the role.
The text itself rather than a file: claude records the system prompt
with the conversation's first request and resumes with that record,
so a file read afresh would buy nothing, and its -file flag is not one
it lists."
  (pcase kind
    ('claude (list "--append-system-prompt" ghostherd--lead-role))
    ('grok (list "--rules" ghostherd--lead-role))))

(defun ghostherd--with-lead-role (kind args)
  "ARGS with the lead's role as it is now, for a KIND agent.
Whatever role ARGS gave it -- the text after the flag that takes one
-- is dropped, and the current one appended."
  (if-let* ((role (ghostherd--lead-args kind)))
      (let (out)
        (while args
          (let ((a (pop args)))
            (if (equal a (car role)) (pop args) (push a out))))
        (append (nreverse out) role))
    args))

(defun ghostherd-project-lead (project)
  "PROJECT's lead, or nil."
  (let ((key (ghostherd--project-name project)))
    (seq-find (lambda (s)
                (and (equal (ghostherd-session-role s) "lead")
                     (equal (ghostherd--project-name (ghostherd-session-project s)) key)))
              (ghostherd-sessions))))

(defvar ghostherd--role-owed (make-hash-table :test 'equal)
  "Session id -> the role text owed to an agent -- a lead, primary --
started on a CLI that takes none as it starts, until its first message:
that carries the role, whether it is the user's (`ghostherd-tell') or
an ask (`ghostherd--ask-deliver').")

(defvar ghostherd--role-spent (make-hash-table :test 'equal)
  "Session id -> the owed role its last message took, given back if its
CLI never had that message (`ghostherd--role-give-back').")

(defun ghostherd--role-take (session)
  "The role SESSION is owed, taken; or nil."
  (let ((id (ghostherd-session-id session)))
    (prog1 (when-let* ((role (gethash id ghostherd--role-owed)))
             (puthash id role ghostherd--role-spent))
      (remhash id ghostherd--role-owed))))

(defun ghostherd--role-give-back (session text)
  "SESSION is owed again the role TEXT carried: its CLI never had TEXT
(`ghostherd--submit-lost'), so the message sent instead still owes it."
  (let ((id (ghostherd-session-id session)))
    (when-let* ((role (gethash id ghostherd--role-spent))
                ((string-search role text))
                ((not (gethash id ghostherd--role-owed))))
      (puthash id role ghostherd--role-owed)
      (remhash id ghostherd--role-spent))))

(defun ghostherd-start-lead (project &optional kind notes)
  "Start PROJECT's lead, a KIND agent (`ghostherd-lead-kind' by default).
NOTES, when given, say more than that it plans the work.  A CLI that
takes no role as it starts is owed it, with its first message."
  (let* ((kind (or kind ghostherd-lead-kind))
         (dir (file-name-as-directory (expand-file-name project)))
         (lead (ghostherd-spawn
                kind
                :name (ghostherd--unique-name
                       (format "lead-%s" (file-name-nondirectory (directory-file-name dir))))
                :project project
                :directory dir
                :args (append (plist-get (ghostherd--spec kind) :args)
                              (ghostherd--lead-args kind))
                :notes (or notes "lead: plans the work, hands it out")
                :role "lead"
                :display nil)))
    (unless (ghostherd--lead-args kind)
      (puthash (ghostherd-session-id lead) ghostherd--lead-role ghostherd--role-owed))
    lead))

(defvar ghostherd--lead-unheard (make-hash-table :test 'equal)
  "Project -> what the user said to its agents directly since its lead
last heard from them, newest first, as (NAME . TEXT).")

(defun ghostherd--lead-unheard-note (project)
  "What PROJECT's lead has not heard, as the note its next message opens
with; or the empty string."
  (if-let* ((said (reverse (gethash project ghostherd--lead-unheard))))
      (concat "[Since you last heard from me, I wrote to the team directly:\n"
              (mapconcat (lambda (c)
                           (format "- to %s: %s" (car c)
                                   (truncate-string-to-width
                                    (replace-regexp-in-string "[\n\t ]+" " " (string-trim (cdr c)))
                                    300 nil nil "…")))
                         said "\n")
              "]\n\n")
    ""))

(defvar ghostherd-project-work-functions nil
  "Functions called with a project, each saying what it works on -- its
branch, its pull request, the tickets they name -- as lines, or nil.
Its lead is told them.  The herd page's look at the herd's checkouts
answers it.")

(defvar ghostherd--lead-work-told (make-hash-table :test 'equal)
  "Lead session id -> the lines it was last told its project works on.")

(defun ghostherd--project-work (project)
  "What PROJECT works on, as lines; nil when nothing says."
  (seq-mapcat (lambda (f) (with-demoted-errors "ghostherd: %S" (funcall f project)))
              ghostherd-project-work-functions))

(defun ghostherd--lead-work-note (lead project)
  "What LEAD has not been told its PROJECT works on, as the note its next
message opens with; or the empty string.  Told, it is not told again
until it changes."
  (let ((lines (ghostherd--project-work project))
        (had (gethash (ghostherd-session-id lead) ghostherd--lead-work-told 'none)))
    (if (or (null lines) (equal lines had))
        ""
      (puthash (ghostherd-session-id lead) lines ghostherd--lead-work-told)
      (concat (if (eq had 'none)
                  "[What this project works on, as the herd sees it.  Read them for the work; \
change none of them -- no ticket's status, fields or comments, nothing on the pull request -- \
unless I ask you to:\n"
                "[What this project works on has changed since you were told.  Now:\n")
              (mapconcat (lambda (l) (concat "- " l)) lines "\n")
              "]\n\n"))))

(defun ghostherd-tell (session text &optional role by-herd)
  "Prompt SESSION with TEXT from the user -- or, BY-HERD, the herd's own.
The herd log records TEXT, not the notes put before it.
A project's lead hears first what the user said to its agents directly
since it last heard from them, and what the project works on when that
is news to it -- after ROLE, the role of a lead that took none as it
started: the one it is owed (`ghostherd--role-owed') when ROLE is nil.
Any other agent owed its role -- primary, on such a CLI -- reads it
first too.  Said to another agent of a project that has a lead, TEXT is
kept for that."
  (let* ((project (ghostherd--project-name (ghostherd-session-project session)))
         (lead (and project (ghostherd-project-lead project)))
         (ghostherd--input-said (if by-herd (concat "[from the herd] " text) text)))
    ;; A herd-made prompt (a relay, say) starts a turn that is not the user's,
    ;; so its end is not an Inbox item.  Marked BEFORE the prompt, which moves
    ;; the session to `working' synchronously and runs the origin hook.  The
    ;; user's own prompt (BY-HERD nil) is left to the hook, which marks it
    ;; `user'.
    (when by-herd (ghostherd--turn-from-ask session))
    (if (eq lead session)
        (progn
          (ghostherd-prompt session (concat (or role
                                                (when-let* ((owed (ghostherd--role-take session)))
                                                  (concat owed "\n---\n\n")))
                                            (ghostherd--lead-unheard-note project)
                                            (ghostherd--lead-work-note session project)
                                            text))
          (remhash project ghostherd--lead-unheard))
      (ghostherd-prompt session (concat (when-let* ((owed (ghostherd--role-take session)))
                                          (concat owed "\n---\n\n"))
                                        text))
      (when lead
        (push (cons (ghostherd-session-name session) text)
              (gethash project ghostherd--lead-unheard))))))

(defun ghostherd--when-ready (session fn &optional tries fail)
  "Call FN once SESSION is at its prompt, looking every second.
At its prompt: at rest, its input box drawn at its foot -- not a trust
prompt reading idle (`ghostherd--off-prompt-p').  Two minutes at most,
TRIES counting down; a session that never gets there is logged, and one
gone is let be.  Either way FAIL, when given, is called instead."
  (let ((tries (or tries 120)))
    (cond
     ((not (eq (gethash (ghostherd-session-id session) ghostherd--sessions) session))
      (when fail (funcall fail)))
     ((and (memq (ghostherd-session-state session) '(idle done waiting))
           (not (ghostherd--off-prompt-p session)))
      (funcall fn))
     ((<= tries 0)
      (ghostherd--log-add session 'life "never came to its prompt: the message for it was not sent")
      (when fail (funcall fail)))
     (t (run-at-time 1 nil #'ghostherd--when-ready session fn (1- tries) fail)))))

(defun ghostherd--lead-project (project)
  "The project whose lead PROJECT reaches: the one it is in, by
`ghostherd--project-root' -- a subdirectory's or a worktree's -- or
PROJECT itself, in none or primary's."
  (if (ghostherd--primary-dir-p project)
      project
    (or (ghostherd--project-root (expand-file-name project)) project)))

(defun ghostherd-lead-say (project text)
  "Say TEXT to PROJECT's lead, starting one when it has none.
A lead just started hears TEXT once it is ready -- after its role, on a
CLI that took none as it started.  Return the lead.  Primary's directory
has no lead but primary, who is told instead (`ghostherd-primary-say'):
a reply to its report on the phone, its room's chat.
PROJECT is taken as the project it is in (`ghostherd--project-root'): a
subdirectory or a worktree of one reaches that project's lead, and never
starts a second lead on part of it."
  (setq project (ghostherd--lead-project project))
  (cond
   ((ghostherd--primary-dir-p project) (ghostherd-primary-say text))
   ((ghostherd-project-lead project)
    (let ((lead (ghostherd-project-lead project)))
      (ghostherd-tell lead text)
      lead))
   (t
    (let ((lead (ghostherd-start-lead project)))
      ;; Asked now, so that whatever looks has looked by the time the
      ;; lead is ready to be told.
      (ghostherd--project-work project)
      (ghostherd--when-ready lead (lambda () (ghostherd-tell lead text)))
      lead))))


;;; A lead starts and stops agents through the herd
;;
;; `herd start' / `herd stop' let a lead run agents without reaching into
;; Emacs internals: the sidecar queues the request, Emacs acts here, guarded
;; by who asks.  Only a lead, for its own project, may start; only the agents
;; a lead started may be stopped or reused by it.  Reuse before spawning: an
;; idle agent the lead started, same kind, whose branch is merged and
;; worktree clean, is fast-forwarded and handed back with its context rather
;; than started anew.  The herd caps how many a lead has up and refuses a
;; used-up kind.  `started-by' is kept on the host with the recipe, so a
;; lead still owns its agents after a restart.

(defcustom ghostherd-lead-may-start t
  "Whether a lead may start and stop agents through `herd start' / `herd stop'.
Nil refuses every such request, and the lead is told to ask the user."
  :type 'boolean
  :group 'ghostherd)

(defcustom ghostherd-lead-start-max 2
  "How many agents one lead may have alive at once in a project, through
`herd start'.  Past it the herd refuses and lists the ones already up, so a
lead reuses or stops one instead of piling them on.  Reuse and stop are not
capped; neither are agents the user started by hand."
  :type 'integer
  :group 'ghostherd)

(defun ghostherd--session-project-name (session)
  "The project name SESSION belongs to, or nil."
  (ghostherd--project-name (ghostherd-session-project session)))

(defun ghostherd--lead-started-alive (lead project)
  "The live sessions LEAD started in PROJECT (by project name)."
  (seq-filter (lambda (s)
                (and (equal (ghostherd-session-started-by s) lead)
                     (equal (ghostherd--session-project-name s) project)
                     (ghostherd--session-live-p s)))
              (ghostherd-sessions)))

(defun ghostherd--worktree-ff (path base)
  "Fast-forward the git worktree at PATH onto BASE; non-nil on success."
  (and (file-directory-p (expand-file-name path))
       (let ((default-directory (file-name-as-directory (expand-file-name path))))
         (eq 0 (ignore-errors
                 (call-process "git" nil nil nil "merge" "--ff-only" base))))))

(defun ghostherd--lead-reuse-candidate (lead kind project)
  "An idle agent LEAD started in PROJECT, same KIND, its branch merged into
its base and its worktree clean -- to refresh rather than spawn anew; or nil."
  (seq-find
   (lambda (s)
     (let ((wt (ghostherd-session-worktree s)))
       (and (eq (ghostherd-session-kind s) kind)
            (eq (ghostherd-session-state s) 'idle)
            wt (plist-get wt :path) (plist-get wt :branch) (plist-get wt :base)
            (ghostherd--worktree-clean-p (plist-get wt :path))
            (ghostherd--worktree-merged-p project (plist-get wt :branch)
                                           (plist-get wt :base)))))
   (ghostherd--lead-started-alive lead project)))

(defun ghostherd--project-main-busy-p (project)
  "Non-nil when some live agent already works in PROJECT's main checkout --
no worktree of its own -- so a second agent wants its own worktree."
  (seq-some (lambda (s)
              (and (equal (ghostherd--session-project-name s) project)
                   (ghostherd--session-live-p s)
                   (not (ghostherd-session-worktree s))))
            (ghostherd-sessions)))

(defun ghostherd--agent-owes-ask-p (name)
  "Non-nil when agent NAME is the taker of an ask still open or queued,
by the asks the last herd tick gave this Emacs."
  (seq-some (lambda (a)
              (and (equal (plist-get a :to) name)
                   (member (plist-get a :status) '("queued" "delivered"))))
            ghostherd--herd-asks))

(defun ghostherd-lead-start (by kind-str &rest opts)
  "Start, or reuse, an agent for lead BY, as `herd start' asks over the wire.
KIND-STR is the CLI kind.  OPTS: :worktree (\"yes\"/\"no\"/\"auto\"), :branch,
:name, :notes, :new.  Returns the line to tell BY -- the agent started or was
reused (with its name and worktree), or, plainly, why it was refused."
  (let* ((by-s (ghostherd-get by))
         (kind (and (stringp kind-str) (not (string-empty-p kind-str))
                    (intern kind-str)))
         (wmode (or (plist-get opts :worktree) "auto"))
         (branch (let ((b (plist-get opts :branch)))
                   (and (stringp b) (not (string-empty-p b)) b)))
         (want-name (let ((n (plist-get opts :name)))
                      (and (stringp n) (not (string-empty-p n)) n)))
         (notes (plist-get opts :notes))
         (new (eq (plist-get opts :new) t)))
    (cond
     ((not ghostherd-lead-may-start)
      "the herd is set not to start agents (ghostherd-lead-may-start is off); ask the user")
     ((not by-s) (format "no such agent: %s" by))
     ((not (equal (ghostherd-session-role by-s) "lead"))
      (format "only a project's lead may start agents; %s is not a lead" by))
     ((not (and kind (memq kind (ghostherd--agent-kinds))
                (plist-get (ghostherd--spec kind) :command)))
      (format "no agent kind %s" kind-str))
     (t
      (let ((project (ghostherd--session-project-name by-s))
            (dir (ghostherd-session-project by-s)))
        (cond
         ((not (and project dir (file-directory-p (expand-file-name dir))))
          (format "%s has no project to start an agent in" by))
         ;; Reuse before spawning.
         ((and (not new)
               (when-let* ((r (ghostherd--lead-reuse-candidate by kind project))
                           (wt (ghostherd-session-worktree r)))
                 (ghostherd--worktree-ff (plist-get wt :path) (plist-get wt :base))
                 (ghostherd--log-add r 'life
                                     (format "reused by %s: worktree fast-forwarded to %s"
                                             by (plist-get wt :base)))
                 (format "reused %s (%s on %s, fresh off %s); it keeps its context"
                         (ghostherd-session-name r)
                         (ghostherd--abbreviate (plist-get wt :path))
                         (plist-get wt :branch) (plist-get wt :base)))))
         ;; A used-up kind: name one that is not.
         ((ghostherd-usage-used-up kind)
          (let ((alt (seq-find (lambda (k) (and (not (eq k kind))
                                                (not (ghostherd-usage-used-up k))
                                                (plist-get (ghostherd--spec k) :command)))
                               (ghostherd--agent-kinds))))
            (format "%s is used up or out of quota%s" kind-str
                    (if alt (format "; start a %s instead" alt)
                      ", and so is every kind the herd knows"))))
         ;; The per-lead cap.
         ((let ((alive (ghostherd--lead-started-alive by project)))
            (when (>= (length alive) (max 0 ghostherd-lead-start-max))
              (format "%s already has %d agent%s up in %s (max %d): %s -- reuse or stop one"
                      by (length alive) (if (= (length alive) 1) "" "s") project
                      ghostherd-lead-start-max
                      (mapconcat #'ghostherd-session-name alive ", ")))))
         (t
          (condition-case err
              (let* ((want-wt (pcase wmode
                                ("yes" t) ("no" nil)
                                (_ (or branch (ghostherd--project-main-busy-p project)))))
                     (final (ghostherd--unique-name
                             (or want-name
                                 (format "%s-%s" kind
                                         (file-name-nondirectory (directory-file-name dir))))))
                     (defaults (ghostherd-project-defaults dir))
                     (args (or (and (eq (car defaults) kind) (cdr defaults))
                               (plist-get (ghostherd--spec kind) :args)))
                     ;; The shared-memory opening (rules + index) is added for
                     ;; every grok/agy by `ghostherd-spawn', so it rides every
                     ;; path, not only this one.
                     (spawn (lambda (directory wt)
                              (ghostherd-spawn kind :name final :project dir
                                               :directory directory :args args :notes notes
                                               :started-by by :display nil
                                               :worktree (and wt (plist-get wt :path))
                                               :branch (and wt (plist-get wt :branch))
                                               :base (and wt (plist-get wt :base))))))
                (if (not want-wt)
                    (progn
                      (funcall spawn dir nil)
                      (format "started %s (%s, in the main checkout)" final kind))
                  (let ((wt (ghostherd--worktree-create dir final branch kind)))
                    (ghostherd--worktree-deps-setup dir (plist-get wt :path) final)
                    (funcall spawn (plist-get wt :path) wt)
                    (format "started %s (%s, worktree %s on %s off %s)"
                            final kind (ghostherd--abbreviate (plist-get wt :path))
                            (plist-get wt :branch) (plist-get wt :base)))))
            (error (format "could not start a %s: %s" kind-str (error-message-string err)))))))))))

(defun ghostherd-lead-stop (by name)
  "Stop agent NAME for lead BY, as `herd stop' asks over the wire.  Only an
agent BY started, and not one holding an open or queued ask.  Removes a
merged, clean worktree; keeps an unmerged one.  Returns the line to tell BY."
  (let ((s (ghostherd-get name)))
    (cond
     ((not ghostherd-lead-may-start)
      "the herd is set not to stop agents (ghostherd-lead-may-start is off); ask the user")
     ((not s) (format "no such agent: %s" name))
     ((not (equal (ghostherd-session-started-by s) by))
      (format "%s did not start %s through the herd, so it may not stop it" by name))
     ((ghostherd--agent-owes-ask-p name)
      (format "%s holds an open or queued ask; let it answer, or cancel it, first" name))
     (t
      (let* ((wt (ghostherd-session-worktree s))
             ;; Looked at now, not as the last poll saw it.
             (gone (ghostherd--mark-gone s t))
             (branch (and gone (ghostherd--gone-branch-now s)))
             (will-remove (and wt (plist-get wt :path) (not gone)
                               (ghostherd--worktree-clean-p (plist-get wt :path))
                               (ghostherd--worktree-merged-p (ghostherd-session-project s)
                                                             (plist-get wt :branch)
                                                             (plist-get wt :base)))))
        (ghostherd-kill s t)
        (cond
         ;; Removed outside the herd: nothing of it to keep or remove, and
         ;; the branch, whatever it holds, is left as it is.
         (gone
          (format "stopped %s; its %s %s was already gone%s"
                  name (if (plist-get gone :worktree) "worktree" "directory")
                  (abbreviate-file-name (plist-get gone :path))
                  (if branch
                      (concat " (" (ghostherd--gone-branch-words
                                    (plist-get wt :branch) (plist-get wt :base)
                                    (car branch) (cdr branch))
                              (if (eq (car branch) 'ahead) "; the branch is kept" "")
                              ")")
                    "")))
         ((not wt) (format "stopped %s" name))
         (will-remove
          (format "stopped %s; its worktree was merged and clean, so it was removed" name))
         (t (format "stopped %s; its worktree was not merged or not clean, so it was kept (M-x ghostherd-worktree-forget)"
                    name))))))))


;;; Handoffs: work for another project's lead
;;
;; `herd handoff PROJECT' is an ask of another project's lead: what an
;; agent found needs a change there, and that project's own lead plans
;; it, by that project's rules, rather than the finder editing a checkout
;; it does not know.  The sidecar keeps the ask, judges who may send it
;; and frames the brief; when PROJECT has no lead it asks Emacs to start
;; one (a `handoff' command), and the ask waits for its name.  Starting a
;; lead nobody asked for spends quota in a project the user may not be
;; looking at, so it is a setting, capped, only in a git checkout, and
;; pushed to the user's phone.

(defcustom ghostherd-handoff-may-start-lead t
  "Whether a handoff to a project with no lead starts one there.
`herd handoff PROJECT' goes to PROJECT's lead; with none, the herd starts
one -- a `ghostherd-lead-kind' agent, or the project's own default kind
from its .dir-locals.el -- and hands it the work.  Nil refuses such a
handoff, and the sender is told to ask the user."
  :type 'boolean
  :group 'ghostherd)

(defcustom ghostherd-handoff-lead-max 3
  "How many leads the herd may have up at once that it started for
handoffs.  Past it a handoff to a project with no lead is refused, naming
the ones up, so the user decides.  Leads the user started do not count,
nor ones started before Emacs restarted."
  :type 'integer
  :group 'ghostherd)

(defvar ghostherd--handoff-leads (make-hash-table :test 'equal)
  "Session id -> the agent it was started for: the leads the herd started
for a handoff, counted against `ghostherd-handoff-lead-max'.  Not kept
over a restart: after one, they are leads like any other.")

(defun ghostherd--handoff-leads-up ()
  "The leads the herd started for handoffs that are still up: not dead."
  (seq-filter (lambda (s)
                (and (gethash (ghostherd-session-id s) ghostherd--handoff-leads)
                     (not (eq (ghostherd-session-state s) 'dead))
                     (ghostherd--session-live-p s)))
              (ghostherd-sessions)))

(defun ghostherd--handoff-lead-for (session)
  "The agent SESSION's lead was started for, when the herd started it for
a handoff and it is still up; else nil.  The Inbox shows an Update for
it, kept until read."
  (when-let* ((s (ghostherd-get session))
              (by (gethash (ghostherd-session-id s) ghostherd--handoff-leads)))
    (and (not (eq (ghostherd-session-state s) 'dead))
         (ghostherd--session-live-p s)
         by)))

(defun ghostherd-handoff-lead (by project &optional from-project head)
  "PROJECT's lead, for a handoff from BY, as the sidecar asks over the
wire; one started when it has none.  FROM-PROJECT is BY's project and HEAD
the brief's first line, for the push.  Returns (NAME . TEXT): the lead's
name and what was done, or nil and why it was refused."
  (let* ((dir (and (stringp project) (not (string-empty-p project))
                   (file-name-as-directory (expand-file-name project))))
         (root (and dir (file-directory-p dir)
                    (or (ghostherd--project-root dir) dir)))
         (name (and root (ghostherd--project-name root)))
         (lead (and name (ghostherd-project-lead name)))
         (from (and (stringp from-project) (not (string-empty-p from-project))
                    (ghostherd--project-name from-project)))
         (leaf (lambda (p) (file-name-nondirectory (directory-file-name p))))
         (by-s (and (stringp by) (ghostherd-get by))))
    (cond
     ((not root) (cons nil (format "no directory %s: no project to hand off to"
                                   (if dir (ghostherd--abbreviate dir) project))))
     ((and from (equal from name))
      (cons nil (format "%s is %s's own project" (funcall leaf name) by)))
     ((and lead (eq (ghostherd-session-state lead) 'dead))
      (cons nil (format "%s, %s's lead, is dead; ask the user to start it again"
                        (ghostherd-session-name lead) (funcall leaf name))))
     ((and lead by-s (eq lead by-s))
      (cons nil (format "%s is %s's lead itself" by (funcall leaf name))))
     (lead (cons (ghostherd-session-name lead)
                 (format "handed to %s, %s's lead" (ghostherd-session-name lead)
                         (funcall leaf name))))
     ((not ghostherd-handoff-may-start-lead)
      (cons nil (format "%s has no lead, and the herd is set not to start one for a \
handoff (ghostherd-handoff-may-start-lead is off); ask the user" (funcall leaf name))))
     ((not (ghostherd--git-lines root "rev-parse" "--show-toplevel"))
      (cons nil (format "%s is not a git checkout, and has no lead: the herd starts a \
lead only in a git checkout; ask the user" (ghostherd--abbreviate root))))
     ((let ((up (ghostherd--handoff-leads-up)))
        (when (>= (length up) (max 0 ghostherd-handoff-lead-max))
          (cons nil (format "%s has no lead, and the herd has %d up that it started for \
handoffs (max %d): %s -- ask the user to start one there, or to stop one of those"
                            (funcall leaf name) (length up) ghostherd-handoff-lead-max
                            (mapconcat #'ghostherd-session-name up ", "))))))
     (t
      (let* ((kind (or (car (ghostherd-project-defaults root)) ghostherd-lead-kind))
             ;; Primary hands off the user's requests, from a place of its own.
             (who (cond ((member by '(nil "" "user")) "the user")
                        ((and by-s (equal (ghostherd-session-role by-s) "primary"))
                         (format "%s, for the user" by))
                        (t by)))
             (from (and from (not (ghostherd--primary-dir-p from)) from))
             (for (if from (format "%s in %s" who (funcall leaf from)) who)))
        (if (not (and (memq kind (ghostherd--agent-kinds))
                      (plist-get (ghostherd--spec kind) :command)))
            (cons nil (format "no agent kind %s to start %s's lead on" kind (funcall leaf name)))
          (condition-case err
              (let ((new (ghostherd-start-lead
                          root kind
                          (format "lead: plans the work, hands it out; started for a handoff from %s"
                                  for))))
                (puthash (ghostherd-session-id new) (or by "user") ghostherd--handoff-leads)
                ;; Asked now, as `ghostherd-lead-say' does, so that what the
                ;; project works on has been looked at by the time it is told.
                (ghostherd--project-work name)
                (ghostherd--log-add new 'life (format "started for a handoff from %s" for))
                (message "ghostherd: started %s for a handoff from %s"
                         (ghostherd-session-name new) for)
                (ghostherd--push 'lead-started new :by (or by "user") :from (or from "")
                                 :head (or head ""))
                (cons (ghostherd-session-name new)
                      (format "started %s (%s) in %s for a handoff from %s"
                              (ghostherd-session-name new) kind
                              (ghostherd--abbreviate root) for)))
            (error (cons nil (format "could not start %s's lead: %s" (funcall leaf name)
                                     (error-message-string err)))))))))))

(provide 'ghostherd-lead)

;; Local Variables:
;; generated-autoload-load-name: "ghostherd"
;; End:

;;; ghostherd-lead.el ends here

;;; ghostherd-core.el --- Settings, the session model and shared helpers -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of ghostherd (see ghostherd.el).  What every other part builds
;; on: the settings, the session model's hooks, the helpers for
;; projects, roles and lineage, the registry's sessions, and desktop
;; notifications.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'project)
(require 'tabulated-list)
(require 'ghostherd-backend)
(require 'ghostherd-tmux)
(require 'ghostherd-memory)
(require 'ghostherd-usage)

(declare-function alert "alert" (message &rest kwargs))
(declare-function ghostherd--log-add "ghostherd-log" (session kind text &optional screen why pinned note))
(declare-function ghostherd--worktree-cleanup "ghostherd-worktree" (session))

;;; Customization

(defconst ghostherd--rule-permission-prompt
  "\\([^[:alnum:]_]\\|^\\)permission\\([^[:alnum:]_-]\\|$\\)"
  "A `blocked' pattern for a permission *prompt*.

The old substring `permission' matched `permissions' in a JSON allow
list and `permission-mode' in grok's idle banner -- both of which sit
on screen while the agent is parked at a prompt.  Blocked outranks
idle, so the row stayed ⚠.  Character classes rather than `\\\\<' `\\\\>'
because those consult the current buffer's syntax table, the same
class of ambient that `case-fold-search' used to be.")

(defconst ghostherd--rule-status-line "^[ \t│┃┆┊▌▐▏▕]*"
  "Prefix making a rule match a TUI *status line*, not the transcript.

These agents scroll, so the tail holds everything they have ever
printed, and an agent that writes *about* its own spinner puts the
words on screen without being busy.  That is not hypothetical: agy
summarising a commit to these very rules quoted `Generating\=',
`esc to cancel\=' and a braille range into its own transcript, and sat
at `working\=' for as long as the summary stayed on screen.

The discriminator is position.  A TUI draws its status line at the
start of a line, indented at most by whitespace or a box edge; prose
quoting the same words has them mid-line, behind a bullet or a
quotation mark.  Anchoring costs nothing a real status line has, which
is why the `idle\=' rules have always been anchored -- the same scroll
made an unanchored `> \=' match the transcript.")

(defcustom ghostherd-agent-specs
  `((claude
     :command "claude"
     :args nil
     :description "Claude Code"
     :process-names ("claude")
     :continue-args ("--continue")
     :resume-args ("-r" "%s")
     :fork-args ("--fork-session")
     :transcript "~/.claude/projects/*/%s.jsonl"
     :prompt-replaces-input t
     :shell-mode t
     ;; "✽ Bloviating… (37s · ↓ 3.3k tokens)": its verb picked at
     ;; random, its glyph one of these; past tense once the turn is over
     ;; ("✻ Baked for 1s").  A narrow pane cuts `esc to interrupt' off
     ;; its footer, and this is what is left -- so this must catch every
     ;; spinner, the fixed ones too: "✽ Compacting conversation… (22s)" at
     ;; an auto-compaction is working, not idle, and its label is two words,
     ;; so allow more than one before the ellipsis.  The time in parens is
     ;; optional: in the first frame of a turn claude draws "· Hyperspacing…"
     ;; before the elapsed-time counter appears (ask aed900f7, 2026-10-18),
     ;; and that first frame is working too.  The ellipsis still anchors it,
     ;; so its past tense ("✻ Baked for 1s", no "…") stays idle.
     :spinner "\\`[·✢✳✶✻✽*][[:blank:]]+[[:alpha:]]+\\(?:[[:blank:]]+[[:alpha:]]+\\)*…\\(?:[[:blank:]]+(\\(?:[0-9]+[hms]\\)\\)?"
     ;; Scrolled back through its conversation, claude draws "Jump to
     ;; bottom (click) ↓" -- "3 new messages (click) ↓" once more came in;
     ;; "(KEY) ↓", " ↓" or ": fn+↓ to scroll" for the key -- over the last
     ;; line of the view, above its input box.  The view is history then:
     ;; the spinner is out of it (ask 18f57d20 was closed on such a view
     ;; of an agent 23 minutes into its turn).
     :scrolled "\\(?:Jump to bottom\\|[0-9]+ new messages?\\)\\(?:\\(?: ([^)\n]*)\\)? ↓\\|: .* to scroll\\)"
     :model-picker (:models ("Default" "Opus" "Fable" "Sonnet" "Haiku")
                    :efforts ("low" "medium" "high" "xhigh" "max"))
     :screen-rules
     ((blocked . ("Do you want to proceed"
                  "Do you want to make this edit"
                  "Allow this action"
                  "Bash command"
                  ,ghostherd--rule-permission-prompt
                  "Yes, and don't ask again"
                  "❯ 1\\. Yes"
                  ;; AskUserQuestion's numbered menu, by its footer: it is
                  ;; parked waiting for a pick (`ghostherd--prompt' reads
                  ;; the options; the herd page offers them).  Distinct
                  ;; from the model picker's "Enter to set as default".
                  "Tab/Arrow keys to navigate"
                  "Enter to select"
                  ;; The trust-folder prompt claude draws on startup in a
                  ;; directory it has not seen: "Quick safety check: Is this
                  ;; a project you created or one you trust?" over a marked
                  ;; [❯] menu (No, exit / Yes, I trust this folder) with no
                  ;; input box -- the agent is parked, not idle.  Matching
                  ;; the question first makes it the reason the card shows;
                  ;; `ghostherd--prompt-marker' reads the two choices, which
                  ;; carry no numbers.  `:prompt-replaces-input' keeps the
                  ;; same words quoted in the chat (box drawn) from tripping.
                  "Quick safety check"
                  "Yes, I trust this folder"))
      (working . ("esc to interrupt"
                  "Baking…"
                  "Baking..."
                  "Thinking"
                  "⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"))
      ;; An *empty* prompt.  A scrolling agent keeps every line you ever
      ;; typed, so "^> " matches the transcript rather than the cursor --
      ;; see the note under :screen-rules below.  Blank, not space:
      ;; claude follows its ❯ with a no-break space.
      (idle . ("^❯[[:blank:]]*$"
               "^>[[:blank:]]*$"))))
    (grok
     :command "grok"
     :args nil
     :description "Grok Build TUI"
     :process-names ("grok")
     :continue-args ("--continue")
     :resume-args ("-r" "%s")
     :fork-args ("--fork-session")
     :transcript "~/.grok/sessions/*/%s/chat_history.jsonl"
     :shell-mode t
     ;; grok's menus take its input box's place too: its question card
     ;; ("Tab:next answer  │  Esc:scrollback") was drawn with no box under
     ;; it, and its docs say the four blocking cards -- question, MCP form,
     ;; permission prompt, cancel-turn panel -- share one contract and take
     ;; the keyboard.  With the box at the foot, then, its `blocked' words
     ;; are the conversation's: a review that says "Approve" (ask cf9873be).
     :prompt-replaces-input t
     ;; "⠴ Sleep 30s then print probe-done… 0.3s   24s ⇣27.3k [stop]":
     ;; braille, then its [stop] button.  What it said ends at the
     ;; scrollbar instead.
     :spinner "\\`[[:blank:]]*[⠁-⣿][[:blank:]].*[[:blank:]]\\[stop\\][[:blank:]]*\\'"
     ;; grok's `/model' takes the model and effort as words -- `/model
     ;; grok-4.6 high' -- and shows them in its input box's border, so the
     ;; herd types the command rather than driving a picker.  Switching
     ;; writes grok's default (`:scope default'), not this session alone.
     :model-picker (:style command :scope default
                    :models ("grok-4.7" "grok-4.7-build-fast" "grok-4.6" "grok-4.5")
                    :efforts ("low" "medium" "high" "xhigh")
                    :labels (("grok-4.7" . "Grok 4.7") ("grok-4.7-build-fast" . "Grok 4.7 Fast")
                             ("grok-4.6" . "Grok 4.6") ("grok-4.5" . "Grok 4.5")))
     :screen-rules
     ((blocked . ("Do you want to proceed"
                  "Allow this"
                  "Approve"
                  ,ghostherd--rule-permission-prompt
                  "\\[y/N\\]"
                  "\\(y/n\\)"
                  ;; The folder-trust prompt grok draws on its first start
                  ;; in a directory not yet trusted -- each new worktree:
                  ;; the question over "Yes, proceed  y" / "No, quit  n",
                  ;; and no input box.  Read as idle, it was handed an ask:
                  ;; the paste went nowhere and the Return after it said
                  ;; "Yes, proceed" (ask c644aebc).  The question, its last
                  ;; choice under it, and no input box from there to the
                  ;; foot: an agent quoting it has its box under the quote.
                  "^[[:blank:]]*Do you trust the contents of this directory\\?[[:blank:]]*\\(?:\n.*\\)\\{1,12\\}\n[[:blank:]]*No, quit[[:blank:]]+n[[:blank:]]*\\(?:\n[^│╭╰❯›\n]*\\)*\\'"))
      (working . ("Working"
                  "Thinking"
                  "Running"
                  "esc to interrupt"
                  "ctrl\\+c to interrupt"
                  ;; grok's footer hint, present while a turn runs --
                  ;; but not while it waits on a task of its own, when
                  ;; there is nothing to cancel.  Its spinner, over its
                  ;; task list and a three-line box, is out of
                  ;; `ghostherd-status-lines': that is its `:spinner'.
                  "Ctrl\\+c:cancel"
                  "⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"))
      (idle . ("^›[[:blank:]]*$"
               "^❯[[:blank:]]*$"
               "^>[[:blank:]]*$"))))
    (agy
     :command "agy"
     :args nil
     :description "agy CLI"
     :process-names ("agy")
     :continue-args ("--continue")
     :resume-args ("--conversation" "%s")
     :transcript "~/.gemini/antigravity-cli/brain/%s/.system_generated/logs/transcript.jsonl"
     :prompt-replaces-input t
     :shell-mode t
     ;; A panel paged over the conversation -- `/usage', which the herd
     ;; itself types to read agy's quota -- ends "↑/↓ Scroll · pgup/pgdown
     ;; Page · ctrl+end Bottom · ctrl+home Top · esc Close", no input box
     ;; under it: its view is the panel, not the agent's state.
     :scrolled "ctrl\\+end Bottom · ctrl\\+home Top"
     ;; agy's `/model' opens a \"Switch Model\" picker with a list and an
     ;; effort slider, the cursor on the current model.  The herd walks
     ;; ↑/↓ to the name, slides ←/→ to the effort, and Enters -- its search
     ;; box ignores the herd's bracketed paste, so the rows are not pared.
     ;; Switching writes agy's default (`:scope default'), not this session
     ;; alone; each model offers its own efforts.
     :model-picker (:style search :scope default
                    :models ("Gemini 4 Argon" "Gemini 3.8 Flash" "Claude Opus 5.5"
                             "Claude Sonnet 5.5" "GPT-OSS 120B")
                    :efforts ("low" "medium" "high"))
     :screen-rules
     ((blocked . ("Do you want to proceed"
                  "Allow this"
                  ,ghostherd--rule-permission-prompt
                  "Approve"
                  "\\[y/N\\]"
                  "\\(y/n\\)"))
      ;; Anchored -- see `ghostherd--rule-status-line'.  agy is the kind
      ;; that talks about its own interface, so it is the kind where a
      ;; substring match reads the transcript instead of the state.
      (working . (,(concat ghostherd--rule-status-line "Working")
                  ,(concat ghostherd--rule-status-line "Thinking")
                  ,(concat ghostherd--rule-status-line "Generating")
                  ,(concat ghostherd--rule-status-line "Running")
                  ;; agy's own wording; the other two say "interrupt".
                  ,(concat ghostherd--rule-status-line "esc to cancel")
                  ,(concat ghostherd--rule-status-line "esc to interrupt")
                  ;; Any braille pattern, rather than the ten frames the
                  ;; other two enumerate: agy spins the 8-dot family, and
                  ;; a list of frames is a list of ways to miss one.
                  ,(concat ghostherd--rule-status-line "[⠁-⣿]")))
      (idle . ("^❯[[:blank:]]*$"
               "^>[[:blank:]]*$"
               "^›[[:blank:]]*$"))))
    (shell
     :command nil
     :args nil
     :description "Plain ghostel shell"
     :process-names nil
     :screen-rules nil))
  "Registry of known agent kinds.

Each entry is (KIND . PLIST) with keys:
:command        executable name or path (nil = shell only)
:args           default argument list (strings)
:continue-args  flag that resumes the CLI's previous conversation, used by
                `ghostherd-respawn'.  nil when the CLI has no such flag.
:resume-args    arguments that resume one conversation by id, %s the id;
                used by `ghostherd-resume'.  nil when the CLI cannot.
:fork-args      added to :resume-args to branch a new conversation off
                that one instead of continuing it.  nil when it cannot.
:transcript     where conversation %s is written, a wildcard pattern;
                the herd page's room reads it.  nil when unknown.
:description    human label
:process-names  process names used for detection (future)
:screen-rules   alist of (STATE . REGEXP-LIST) for tail matching
:prompt-replaces-input  non-nil when a menu the CLI waits on takes its
                input box's place: with the box at the foot, a `blocked'
                pattern matched the conversation, not a prompt -- an
                agent that wrote \"permission\" is not asking for it.
                Seen of claude and agy, and of grok's question card.
:shell-mode     non-nil when the CLI has a shell mode: `!' typed on its empty
                prompt makes the box a command line, `!' in the prompt
                sign's place, and what is submitted there runs in its shell
                asking no permission.  The user's own `!' command, sent
                from the herd page or Emacs, runs there
                (`ghostherd-user-shell-command'); claude, grok and agy.
:scrolled       a regexp for what the CLI draws over its view while you
                have it scrolled back -- asked of the few lines just above
                its input box (`ghostherd--scrolled-back-p').  The screen
                is then history, not the agent's state, and is not read
                for one.  nil when the kind has no such view.
:spinner        a regexp for the line the CLI draws above its input box
                while it works, asked of the lines from the box up to the
                first that is not indented (`ghostherd--spinner-line').
                `working' wherever it sits, however far a todo list or a
                task list under it pushes it up.  nil when the kind has
                none.
:model-picker   how `ghostherd-switch-model' changes this CLI's model, a
                plist, nil when the herd does not know the CLI's.  Always
                :models NAMES and :efforts LEVELS -- NAMES one of which to
                pick, LEVELS its effort's steps lowest first -- and:
                :style  how it is driven.  nil or `picker' drives claude's
                        numbered picker (down to the row, ←/→ the effort,
                        `s').  `command' types the CLI's `/model NAME
                        EFFORT' and reads the model line it then shows
                        (grok).  `search' opens the picker, types NAME into
                        its search to pare it to one, slides ←/→ to the
                        effort and Enters (agy).
                :scope  `session' (claude's `s', this session alone) or
                        `default' (grok and agy have no session-only
                        switch; it becomes the CLI's default).  Shown in
                        what the switch reports.
                :labels for `command': (NAME . \"Display\")..., the name as
                        the model line shows it, to read back the switch.

A kind's `working' rules must cover *every* indicator that CLI shows
while busy, because its input box is on screen the whole time and the
`idle' rule therefore always matches too.  Precedence is all that keeps
them apart: miss one working indicator and the agent reads `idle' mid
task.  agy cost three of them -- it labels the spinner `Generating',
says `esc to cancel' rather than `esc to interrupt', and spins the
8-dot braille family.

Cover them *anchored*, though: see `ghostherd--rule-status-line'.  The
same scroll that makes the idle rules demand an empty line makes an
unanchored working rule match an agent merely writing about a spinner.

An `idle' rule must match an *empty* prompt: ^> *$ rather than ^> with
anything allowed after it.  These agents scroll rather than repaint, so
every line you have ever typed stays on screen behind its prompt glyph,
and a rule that matches those reports `idle' from the moment you first
press Return, forever.  Tightening costs nothing: a prompt the rules
fail to recognise falls through to the idle default anyway.

Three rules were dropped for the same reason -- claude's separator line,
grok's startup banner and grok's permission-mode banner are part of the
frame, not of the state.  The first two are on screen while the agent
works, so they said `idle' throughout.  The third was worse: grok prints
its mode in the border of the input box, so `always-approve' -- the mode
in which it will *not* ask -- read as `blocked', which outranks
`working', for as long as that mode was on.

Common optional CLI flags (not enabled by default — set via :args
or the ARGUMENTS prompt in `ghostherd-new'):

claude:
  --permission-mode plan|acceptEdits|bypassPermissions|default|dontAsk
  --dangerously-skip-permissions
  --model sonnet|opus|<full-name>
  --continue / --resume
  --system-prompt PROMPT
  -p / --print  (headless; usually not for interactive herd)

grok:
  --permission-mode default|acceptEdits|auto|dontAsk|bypassPermissions|plan
  --always-approve
  -m / --model MODEL
  --continue / --resume
  --cwd DIR
  --system-prompt-override PROMPT
  --worktree [NAME]

agy:
  --mode accept-edits|plan
  --dangerously-skip-permissions
  --model MODEL
  --continue
  --prompt-interactive / -i PROMPT
  --effort low|medium|high
  --sandbox"
  :type '(alist :key-type symbol :value-type plist)
  :group 'ghostherd)

(defcustom ghostherd-default-kind 'claude
  "Default agent kind for `ghostherd-new'."
  :type 'symbol
  :group 'ghostherd)

(defcustom ghostherd-poll-interval 1.5
  "Seconds between automatic state polls for live sessions."
  :type 'number
  :group 'ghostherd)

(defcustom ghostherd-use-osc-progress t
  "Treat ConEmu OSC 9;4 progress reports as evidence that an agent is working.
Screen scraping cannot tell a silent agent from a finished one; a CLI
that emits progress tells us directly.  Takes effect when
`ghostherd-mode' is next enabled."
  :type 'boolean
  :group 'ghostherd)

(defcustom ghostherd-progress-ttl 5
  "Seconds an OSC 9;4 progress report counts as evidence of `working'.
Reports stop arriving the moment an agent blocks on a prompt, so this
only needs to outlast the gap between two updates of a live task."
  :type 'number
  :group 'ghostherd)

(defvar ghostherd--saved-progress-function nil
  "Value of `ghostel-progress-function' displaced by `ghostherd--on-progress'.")

(defcustom ghostherd-notify-on-blocked t
  "Notify when a session transitions into `blocked'."
  :type 'boolean
  :group 'ghostherd)

(defcustom ghostherd-notify-on-exit t
  "Notify when an agent process exits (via `ghostel-exit-functions')."
  :type 'boolean
  :group 'ghostherd)

(defcustom ghostherd-notify-on-done t
  "Notify when a session transitions into `done' (idle after work, unseen)."
  :type 'boolean
  :group 'ghostherd)

(defcustom ghostherd-notify-on-guard t
  "Notify when a guarded agent's guard refuses a call you may approve: a
new request waits on the herd page.  Once a request, over restarts."
  :type 'boolean
  :group 'ghostherd)

(defcustom ghostherd-sidebar-side 'left
  "Side window side for `ghostherd-sidebar' when not using posframe."
  :type '(choice (const left) (const right))
  :group 'ghostherd)

(defcustom ghostherd-sidebar-width 36
  "Width of the ghostherd side window.

Used only when the session list is shown as a side window.  The
posframe overlay sizes itself from the parent frame (see
`ghostherd-sidebar-posframe-width-ratio'), so a 36-column dashboard
does not constrain a centred panel."
  :type 'integer
  :group 'ghostherd)

(defcustom ghostherd-sidebar-use-posframe t
  "Show the session list in a posframe overlay when possible.

When non-nil, `ghostherd-sidebar' uses posframe if the library can
be loaded and the display can host a child frame.  Otherwise it
falls back to a side window -- the original display, and the one
that still works on a tty or in batch.

Nil forces the side window even when posframe is available."
  :type 'boolean
  :group 'ghostherd)

(defcustom ghostherd-sidebar-show-preview t
  "Show a snapshot of the session at point under the session list.

The snapshot is `capture-pane' / buffer text, not an attached view.
`v' in the list toggles this."
  :type 'boolean
  :group 'ghostherd)

(defcustom ghostherd-sidebar-preview-lines 16
  "Lines of captured screen shown in the overlay preview."
  :type 'integer
  :group 'ghostherd)

(defcustom ghostherd-sidebar-posframe-width 72
  "Minimum character width of the session-list posframe overlay.

The overlay grows with the parent frame (see
`ghostherd-sidebar-posframe-width-ratio'); this is the floor, so a
narrow Emacs still gets Kind and Project.  Nil for the ratio
makes this a fixed width instead."
  :type 'integer
  :group 'ghostherd)

(defcustom ghostherd-sidebar-posframe-width-ratio 0.6
  "Fraction of the parent frame's width used by the overlay.

Nil means a fixed `ghostherd-sidebar-posframe-width' instead of
scaling.  The result is clamped between that width and the parent
minus a small margin."
  :type '(choice (const :tag "Fixed width" nil)
                 (number :tag "Fraction of frame"))
  :group 'ghostherd)

(defcustom ghostherd-sidebar-show-title nil
  "Offer the agent's OSC 2 terminal title as a sidebar column.

The column still has to earn its place: columns are fitted to the
sidebar's width (see `ghostherd--sidebar-column-specs'), and Title ranks
above Project, so in a narrow sidebar enabling this trades the project
path for the title.  Nothing overflows either way."
  :type 'boolean
  :group 'ghostherd)

(defcustom ghostherd-detached-glyph "▪"
  "Marker appended to the sidebar's state glyph when nothing is attached.

Rides in the spare character of the two-wide glyph column rather than
taking a column of its own: at the default sidebar width there is no
room for another column, so a `View' one would be fitted away exactly
where it is needed.

Marks the *detached* rows rather than the attached ones, which keeps it
silent on the ghostel backend -- there a registered session always has
its buffer, so nothing is ever marked.

Was a shadowed middle dot, on the theory that a detached agent is the
normal case on the tmux backend and should not shout.  It undershot:
one dim pixel next to a glyph is not a signal, and information nobody
notices is information that is not there.  Something with ink, in the
row's own colour."
  :type 'string
  :group 'ghostherd)

(defcustom ghostherd-message-template
  "[ghostherd message from %s → %s]\n%s\n"
  "Template for inter-agent messages.
Filled with FROM name, TO name, and BODY."
  :type 'string
  :group 'ghostherd)

(defcustom ghostherd-submit-on-message t
  "When non-nil, inter-agent messages are submitted with RET after paste."
  :type 'boolean
  :group 'ghostherd)


;;; Session model

;; `ghostherd-session', the registry and `ghostherd-get' live in
;; ghostherd-backend.el: the implementors need them, and they are the one
;; thing both layers share.

(defvar ghostherd--counter 0
  "Monotonic counter for generated session names.")

(defvar ghostherd--poll-timer nil
  "Idle timer that polls session states.")

;; How late the poll ran.
;;
;; Emacs's main thread held elsewhere -- a long command, a GC, a process
;; waited on -- runs no timer meanwhile, and the poll's tick, due every
;; `ghostherd-poll-interval', runs late by as long.  So each tick says
;; how late it ran, and one late by `ghostherd--held-min' or more is
;; kept, the latest `ghostherd--held-max': the herd page's slow timings
;; ask whether Emacs was held then (`ghostherd-memory-page--held'), which
;; the page's own clock can only infer.  A subtraction a tick, no timer
;; of its own.  It sees a hold of a tick's length or more, not less.

(defvar ghostherd--held nil
  "The poll's late ticks, the latest first: (WHEN . LATE) each, seconds:
Emacs's main thread held from WHEN - LATE to WHEN.")

(defconst ghostherd--held-min 0.25
  "Seconds late from which a tick of the poll is kept.")

(defconst ghostherd--held-max 32
  "Late ticks kept, the latest.")

(defun ghostherd--note-held ()
  "Note how late the poll's tick runs -- when its timer runs it."
  (when (and (timerp ghostherd--poll-timer)
             (eq timer-event-last ghostherd--poll-timer)
             (numberp (timer--repeat-delay ghostherd--poll-timer)))
    (let* ((now (float-time))
           ;; Its timer is set for the next tick already: this one was
           ;; due a repeat before that.
           (late (- now (- (float-time (timer--time ghostherd--poll-timer))
                           (timer--repeat-delay ghostherd--poll-timer)))))
      (when (>= late ghostherd--held-min)
        (push (cons now late) ghostherd--held)
        (when (> (length ghostherd--held) ghostherd--held-max)
          (setq ghostherd--held (seq-take ghostherd--held ghostherd--held-max)))))))

(defvar ghostherd-state-change-hook nil
  "Hook run after a session state changes.
Each function is called with (SESSION OLD-STATE NEW-STATE).")

(defvar ghostherd-session-created-hook nil
  "Hook run after a session is registered.  Arg: SESSION.")

(defvar ghostherd-session-removed-hook nil
  "Hook run after a session is removed.  Arg: SESSION.")


;;; Helpers

(defun ghostherd--agent-kinds ()
  "Return the list of known agent kind symbols."
  (mapcar #'car ghostherd-agent-specs))

(defvar ghostherd-project-kind nil
  "Default agent kind for a project, set from its =.dir-locals.el=.

  ((nil . ((ghostherd-project-kind . claude))))")
(put 'ghostherd-project-kind 'safe-local-variable #'symbolp)

(defvar ghostherd-project-args nil
  "Default extra CLI arguments for a project, from its =.dir-locals.el=.

  ((nil . ((ghostherd-project-args . (\"--permission-mode\" \"acceptEdits\")))))")
(put 'ghostherd-project-args 'safe-local-variable
     (lambda (value) (and (listp value) (seq-every-p #'stringp value))))

(defun ghostherd-project-defaults (&optional directory)
  "Return (KIND . ARGS) from DIRECTORY's directory-local variables.

Read from the directory rather than from the current buffer on purpose:
`ghostherd-new' is often invoked from the sidebar or another non-file
buffer, which never picked up the project's dir-locals at all.

A malformed .dir-locals.el costs you the defaults, not the ability to
start an agent: `hack-dir-local-variables' warns and yields nothing
rather than signalling.  The `ignore-errors' is for the filesystem
around it, not for that case.

Only safe values are read, and nothing is asked: the page asks this of
each directory typed in its New agent form, from a request, where a
prompt about a project's risky local variables -- an `eval' -- would
hold the page until answered in Emacs, out of sight.  Both variables
read here are safe for their own shapes."
  (let ((directory (file-name-as-directory
                    (expand-file-name (or directory default-directory)))))
    (or (ignore-errors
          (with-temp-buffer
            (setq default-directory directory)
            ;; The lookup keys off `buffer-file-name', not
            ;; `default-directory'; the latter just keeps this temp buffer
            ;; coherent.  `hack-dir-local-variables' collects into
            ;; `file-local-variables-alist' without applying anything.
            (setq-local buffer-file-name
                        (expand-file-name ".ghostherd" directory))
            (let ((enable-local-variables :safe))
              (hack-dir-local-variables))
            (cons (alist-get 'ghostherd-project-kind file-local-variables-alist)
                  (alist-get 'ghostherd-project-args file-local-variables-alist))))
        (cons nil nil))))

(defun ghostherd--spec (kind)
  "Return the plist for KIND from `ghostherd-agent-specs'."
  (or (alist-get kind ghostherd-agent-specs)
      (user-error "Unknown agent kind: %s" kind)))

(defcustom ghostherd-worktree-root "~/.ghostherd/worktrees"
  "Directory an agent's own git worktree is created under.
A worktree goes in <root>/<project>/<name>, outside the project's own
tree, so grep, find and project search in the main checkout never meet a
second copy of the code.  See `ghostherd-new'."
  :type 'directory
  :group 'ghostherd)

(defun ghostherd--git-config (gitdir key)
  "Git config KEY read against git dir GITDIR, trimmed; nil when unset."
  (with-temp-buffer
    (when (eq 0 (ignore-errors
                  (call-process "git" nil '(t nil) nil
                                "--git-dir" (expand-file-name gitdir) "config" key)))
      (let ((s (string-trim (buffer-string))))
        (and (not (string-empty-p s)) s)))))

(defun ghostherd--git-lines (dir &rest args)
  "Run git ARGS in DIR; its trimmed output, or nil when it failed or said
nothing."
  (when (and (stringp dir) (file-directory-p (expand-file-name dir)))
    (with-temp-buffer
      (let ((default-directory (file-name-as-directory (expand-file-name dir))))
        (and (eq 0 (ignore-errors (apply #'call-process "git" nil '(t nil) nil args)))
             (let ((s (string-trim (buffer-string))))
               (and (not (string-empty-p s)) s)))))))

(defun ghostherd--worktree-project (common)
  "The project directory a linked git worktree whose common git dir is
COMMON belongs to -- see `ghostherd--worktree-main-checkout'."
  (let ((common (directory-file-name (expand-file-name common))) wt)
    (cond
     ;; A bare repo has no main checkout: its worktrees group under it.
     ((equal (ghostherd--git-config common "core.bare") "true")
      (file-name-as-directory common))
     ;; A submodule's git dir names its own working tree (`core.worktree',
     ;; relative to the git dir): that is its project, not the superproject's
     ;; .git/modules it is folded under.
     ((setq wt (ghostherd--git-config common "core.worktree"))
      (file-name-as-directory (expand-file-name wt (file-name-as-directory common))))
     ;; An ordinary repo: the main checkout is the common dir's parent.
     (t (let ((main (file-name-directory common)))
          (and main (file-name-as-directory (expand-file-name main))))))))

(defun ghostherd--worktree-main-checkout (dir)
  "If DIR is a linked git worktree, the project directory it belongs to;
else nil.  A linked worktree's own git dir sits under its repository's
common dir, while an ordinary checkout's two are the same (then nil, and
the caller takes DIR's own project).  Which project the common dir names
depends on the repository: a bare repo's worktrees group under the bare
repo itself (it has no main checkout); a submodule is its own project, at
its own working tree, not folded into the superproject's .git/modules; an
ordinary repo's worktrees belong to its main checkout, the common dir's
parent (`ghostherd--worktree-project').
Git is not asked when it need not be: of a directory in no checkout, or
the top of an ordinary one -- whose .git is a directory, where a linked
worktree's or a submodule's is a file."
  (when (and (stringp dir) (file-directory-p (expand-file-name dir))
             (locate-dominating-file (expand-file-name dir) ".git")
             (not (file-directory-p (expand-file-name ".git" dir))))
    (let ((default-directory (file-name-as-directory (expand-file-name dir))))
      (with-temp-buffer
        (when (eq 0 (ignore-errors
                      (call-process "git" nil '(t nil) nil "rev-parse"
                                    "--path-format=absolute"
                                    "--git-common-dir" "--git-dir")))
          (pcase (split-string (buffer-string) "\n" t)
            (`(,common ,gitdir)
             (and (not (equal (file-name-as-directory common)
                              (file-name-as-directory gitdir)))
                  (ghostherd--worktree-project common)))))))))

(defun ghostherd--project-root (&optional dir)
  "Return project root for DIR, or nil.
An agent working in one of a project's git worktrees belongs to that
project, not to a new one: a linked worktree resolves to its main
checkout (`ghostherd--worktree-main-checkout'), so it keeps its own
directory and branch while sharing the project's room and lead."
  (if-let* ((proj (project-current nil dir)))
      (let ((root (project-root proj)))
        (or (ghostherd--worktree-main-checkout root) root))
    (ghostherd--worktree-main-checkout (or dir default-directory))))

(defun ghostherd--abbreviate (path)
  "Abbreviate PATH for display."
  (if path (abbreviate-file-name path) "~"))

(defvar ghostherd--project-names (make-hash-table :test 'equal)
  "Absolute path -> `ghostherd--project-name' of it.")
(defvar ghostherd--project-names-by nil
  "The `directory-abbrev-alist' the kept names were written by.")

(defun ghostherd--project-name (path)
  "PATH as a project is always written: a directory, under ~ when it is.
An agent spawned here carries the absolute name and a restored one the
abbreviated name; the page and the sidecar compare them as strings.
Asked of a few paths, for every agent, several times a second: kept,
for an absolute PATH, while `directory-abbrev-alist' is the same."
  (and path (not (string-empty-p path))
       (if (not (file-name-absolute-p path))
           (abbreviate-file-name (file-name-as-directory (expand-file-name path)))
         (unless (eq ghostherd--project-names-by directory-abbrev-alist)
           (clrhash ghostherd--project-names)
           (setq ghostherd--project-names-by directory-abbrev-alist))
         (with-memoization (gethash path ghostherd--project-names)
           (abbreviate-file-name (file-name-as-directory (expand-file-name path)))))))

(defvar ghostherd--roles (make-hash-table :test 'equal)
  "Session id -> its role in the herd: \"lead\" for a project's lead.
A table rather than a slot, for the reason `ghostherd--input-at' gives;
written onto the host with the recipe, so a lead is still one after a
restart.")

(defvar ghostherd--handoff-leads (make-hash-table :test 'equal)
  "Session id -> the agent it was started for: the leads the herd started
for a handoff, which its caps count (ghostherd-lead.el).  Written onto
the host with the recipe (`:handoff-by'), so they are still the herd's
after a restart.")

(defun ghostherd-session-role (session)
  "SESSION's role in the herd, or nil."
  (gethash (ghostherd-session-id session) ghostherd--roles))

(defvar ghostherd--lineage (make-hash-table :test 'equal)
  "Session id -> the agents that held its name before it, oldest first.
Each an alist of `kind', `conversation', `ended' and `reason', all
strings: see `ghostherd-relay'.  Written onto the host with the recipe,
as the role is.")

(defun ghostherd-session-lineage (session)
  "The agents that held SESSION's name before it, oldest first."
  (gethash (ghostherd-session-id session) ghostherd--lineage))

(defun ghostherd-session-generation (session)
  "SESSION's place in the line of its name: 1 for the first to hold it."
  (1+ (length (ghostherd-session-lineage session))))


(defun ghostherd--roman (n)
  "N in Roman numerals, for the line of a name; past 39, the number."
  (if (and (integerp n) (< 0 n 40))
      (concat (make-string (/ n 10) ?X)
              (aref ["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"] (% n 10)))
    (format "%s" n)))

(defun ghostherd--line-text (session)
  "SESSION as the line of its name says it, the CLIs that held the
name, oldest first: \"rd II, the CLIs of its name: claude → grok\"."
  (format "%s %s, the CLIs of its name: %s"
          (ghostherd-session-name session)
          (ghostherd--roman (ghostherd-session-generation session))
          (mapconcat #'identity
                     (append (mapcar (lambda (a) (or (alist-get 'kind a) "?"))
                                     (ghostherd-session-lineage session))
                             (list (symbol-name (ghostherd-session-kind session))))
                     " → ")))

(defun ghostherd--session-title (session)
  "Return SESSION's terminal title, or nil when it says nothing new.
Which OSC 2 stream that comes from is the backend's business: a ghostel
session has it as a buffer-local, a detached tmux session has it as
`#{pane_title}'."
  (ghostherd--host-title session))

(defun ghostherd--unique-name (base)
  "Return a unique session name derived from BASE."
  (let ((name base)
        (n 1))
    (while (ghostherd-get name)
      (setq n (1+ n)
            name (format "%s-%d" base n)))
    name))

(defun ghostherd--session-live-p (session)
  "Return non-nil if SESSION's host is still there.

This used to mean \"the buffer exists\", which was the same statement
while every agent was an Emacs child.  It is not the same on a backend
where the buffer is a client: a tmux session nobody is attached to has
no buffer at all and is very much alive."
  (ghostherd--host-live-p session))

(defun ghostherd-sessions (&optional project)
  "Return all sessions, optionally filtered to PROJECT root."
  (let ((all (hash-table-values ghostherd--sessions)))
    (if (not project)
        (cl-sort all #'string< :key #'ghostherd-session-name)
      (let ((root (expand-file-name project)))
        (cl-sort
         (cl-remove-if-not
          (lambda (s)
            (when-let* ((p (ghostherd-session-project s)))
              (file-equal-p p root)))
          all)
         #'string< :key #'ghostherd-session-name)))))

(defun ghostherd--ensure-sessions ()
  "Drop sessions whose host is gone.

An agent that exited is not gone: the ghostel backend keeps its buffer
(`ghostel-kill-buffer-on-exit' nil) and the tmux backend keeps its pane
(`remain-on-exit on') precisely so the row can sit there saying `dead'
with its last screen intact.  What this prunes is the session whose
host no longer exists to ask."
  (maphash
   (lambda (id session)
     ;; One session whose host cannot be asked must not take the sweep
     ;; down with it, and must not be read as gone either: pruning on a
     ;; failed question would delete the herd rather than report it.
     ;; Keep it, and let the poll surface the error.
     (unless (condition-case nil
                 (ghostherd--session-live-p session)
               (error t))
       (setf (ghostherd-session-state session) 'dead
             (ghostherd-session-state-reason session) "host gone")
       (remhash id ghostherd--sessions)
       ;; Said: a herd that empties itself without a line leaves nothing
       ;; to go on.
       (ghostherd--log-add session 'life "dropped from the herd: its host is gone")
       (run-hook-with-args 'ghostherd-session-removed-hook session)
       ;; Its host is gone for good, so clean up its worktree if the branch
       ;; is merged and the tree is clean; an unclean one is kept.
       (ignore-errors (ghostherd--worktree-cleanup session))))
   ghostherd--sessions))


;;; Notifications

(defun ghostherd--notify (title body)
  "Show TITLE/BODY via alert when available, else `message'."
  (cond
   ((and (fboundp 'alert) (require 'alert nil t))
    (alert body :title title :category 'ghostherd))
   (t
    (message "%s: %s" title body))))

(defun ghostherd--maybe-notify-state (session old new)
  "Notify about SESSION state transition OLD → NEW when configured."
  (let ((name (ghostherd-session-name session))
        (kind (symbol-name (ghostherd-session-kind session))))
    (pcase new
      ('blocked
       (when (and ghostherd-notify-on-blocked (not (eq old 'blocked)))
         (ghostherd--notify
          (format "ghostherd · %s blocked" name)
          (format "%s needs attention%s"
                  kind
                  (if-let* ((r (ghostherd-session-state-reason session)))
                      (format " (%s)" r)
                    "")))))
      ('done
       (when (and ghostherd-notify-on-done (not (eq old 'done)))
         (ghostherd--notify
          (format "ghostherd · %s done" name)
          (format "%s finished and is ready for review" kind))))
      ('dead
       (when (and ghostherd-notify-on-exit (not (eq old 'dead)))
         (ghostherd--notify
          (format "ghostherd · %s exited" name)
          (or (ghostherd-session-state-reason session)
              "process exited")))))))

(provide 'ghostherd-core)

;; Local Variables:
;; generated-autoload-load-name: "ghostherd"
;; End:

;;; ghostherd-core.el ends here

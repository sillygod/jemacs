;;; ghostherd.el --- Manage AI agent CLIs in ghostel terminals -*- lexical-binding: t; -*-

;; Author: Jing
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ghostel "0.32") (xwapp "0.1.0"))
;; Keywords: tools, terminals, ai
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ghostherd is a herdr-inspired control plane for coding agents that
;; run inside ghostel terminals.  Each agent is a named session bound
;; to a real PTY buffer.  Sessions are scoped to Emacs (no background
;; daemon).
;;
;; Features:
;; - Spawn claude / grok / agy (or a plain shell) per project
;; - Multiple concurrent agents (e.g. implementer + reviewer)
;; - Session list (posframe overlay, side window fallback) + consult switcher
;; - Inter-agent messaging (prompt, send, read, wait, message, sidecar herd_*)
;; - Notifications on blocked / process exit (via alert when available)
;; - Shared transcript memory (Python sidecar: import + search)
;;
;; Main entry points:
;;   M-x ghostherd-new
;;   M-x ghostherd-switch
;;   M-x ghostherd-sidebar
;;   M-x ghostherd-next-blocked
;;   M-x ghostherd-message

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'project)
(require 'tabulated-list)
;; The host slot, the session struct and the ghostel implementor.  Loaded
;; first because everything below is written against the slot rather than
;; against ghostel directly.
(require 'ghostherd-backend)
(require 'ghostherd-tmux)
(require 'ghostherd-memory)
(require 'ghostherd-usage)

(declare-function magit-status-setup-buffer "magit-status" (&optional directory))
(declare-function alert "alert" (message &rest kwargs))
(declare-function posframe-workable-p "posframe")
(declare-function posframe-show "posframe")
(declare-function posframe-hide "posframe")
(declare-function posframe-refresh "posframe")
(declare-function posframe-poshandler-frame-center "posframe")

;; The ghostel I/O shims, the `ghostherd' group and the session struct
;; now live in ghostherd-backend.el.  These few stay because `defvar'
;; without a value declares a variable special only within one file, and
;; the hook plumbing below is still ghostherd's own.
(defvar ghostel-progress-function)
(defvar ghostel-query-before-killing)
(defvar ghostel-eval-cmds)
(defvar ghostel-exit-functions)
(defvar ghostel-command-start-functions)
(defvar ghostel-command-finish-functions)
(defvar ghostel--input-mode)
(declare-function ghostel-copy-mode "ghostel")
(declare-function ghostel-readonly-exit "ghostel")
(declare-function evil-define-key* "evil")
(declare-function evil-make-intercept-map "evil")


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


;;; Git worktrees
;;
;; An agent can work in its own git worktree, so two agents write code in
;; one project at once without stepping on each other.  The worktree is a
;; real `git worktree' outside the project tree (`ghostherd-worktree-root'),
;; on its own branch; the agent's directory is the worktree while its
;; project stays the main checkout (`ghostherd--project-root'), so it shares
;; the project's room and lead.  What branch and base a worktree is on is
;; kept beside the role and lineage -- a table, not a slot -- and on the
;; host with the recipe, so it survives a restart.

(defvar ghostherd--worktrees (make-hash-table :test 'equal)
  "Session id -> a plist of its git worktree: :path, :branch, :base.
A table rather than a slot, as the role and lineage are; written onto the
host with the recipe, so a worktree agent is still one after a restart.
Nil for an agent not in a herd-made worktree.")

(defun ghostherd-session-worktree (session)
  "SESSION's git worktree as (:path :branch :base), or nil for none."
  (gethash (ghostherd-session-id session) ghostherd--worktrees))

(defvar ghostherd--started-by (make-hash-table :test 'equal)
  "Session id -> the lead that started it through `herd start', or nothing.
A table rather than a slot, as the role and worktree are; written onto the
host with the recipe, so a lead still owns the agents it started after a
restart.  `herd stop' and the reuse in `herd start' act only on these.")

(defun ghostherd-session-started-by (session)
  "The lead that started SESSION through the herd, or nil."
  (gethash (ghostherd-session-id session) ghostherd--started-by))

(defun ghostherd--worktree-path (project name)
  "Where agent NAME's worktree of PROJECT goes, under `ghostherd-worktree-root'."
  (expand-file-name
   name
   (expand-file-name
    (file-name-nondirectory (directory-file-name (expand-file-name project)))
    (expand-file-name ghostherd-worktree-root))))

(defun ghostherd--worktree-base (project)
  "The ref PROJECT's worktrees branch from: the main checkout's current
branch, or its HEAD commit when it is detached; nil when it is no git
checkout with a commit."
  (let ((default-directory (file-name-as-directory (expand-file-name project))))
    (with-temp-buffer
      (if (eq 0 (ignore-errors
                  (call-process "git" nil '(t nil) nil
                                "symbolic-ref" "--short" "-q" "HEAD")))
          (let ((b (string-trim (buffer-string))))
            (and (not (string-empty-p b)) b))
        (erase-buffer)
        (and (eq 0 (ignore-errors
                     (call-process "git" nil '(t nil) nil "rev-parse" "-q" "HEAD")))
             (let ((h (string-trim (buffer-string))))
               (and (not (string-empty-p h)) h)))))))

;;; Trust for the worktrees the herd creates (ask 2befbbd6)
;;
;; Each CLI asks, the first time it starts in a folder, whether to trust
;; it, and an agent parked on that prompt takes no work until the user
;; answers.  A worktree the herd has just created off a project its CLI
;; already trusts, by the CLI's own record, is the same repository: the
;; herd records it trusted in that CLI's store, so the agent starts at its
;; prompt.  Nothing else is trusted for the user -- a project the CLI does
;; not trust, a directory typed in, a conversation's own, the main
;; checkout.  bin/herd_trust.py reads and writes each store as its CLI
;; does, one entry at a time; what it added is dropped with the worktree.

(defcustom ghostherd-pretrust t
  "Non-nil: a worktree the herd creates is recorded trusted in its agent's
CLI's store when that CLI trusts the project's main checkout, so the
agent does not start on a trust prompt.  See `ghostherd-trust-stores'."
  :type 'boolean
  :group 'ghostherd)

(defcustom ghostherd-trust-stores
  `((grok . ,(expand-file-name "trusted_folders.toml" (or (getenv "GROK_HOME") "~/.grok")))
    (agy . "~/.gemini/antigravity-cli/settings.json"))
  "Where each CLI keeps the folders it trusts, as (KIND . FILE).
grok: a [folders.\"PATH\"] table of `trusted' and `decided_at'.  agy: its
settings' \"trustedWorkspaces\" list.  claude needs no entry: it counts
a linked worktree trusted when its main checkout is."
  :type '(alist :key-type symbol :value-type file)
  :group 'ghostherd)

(defcustom ghostherd-pretrust-file (locate-user-emacs-file "ghostherd-pretrust.eld")
  "Where the herd keeps the trust it recorded -- (:path :kind :store
:name) each -- to drop it when it removes the worktree."
  :type 'file
  :group 'ghostherd)

(defun ghostherd--trust-run (op kind store path)
  "Run bin/herd_trust.py OP for KIND's STORE and PATH: (STATUS . OUTPUT)."
  (let ((script (and (fboundp 'ghostherd-herd-client) (ghostherd-herd-client)
                     (expand-file-name "herd_trust.py"
                                       (file-name-directory (ghostherd-herd-client))))))
    (if (not (and script (file-executable-p script)))
        (cons -1 "bin/herd_trust.py not found")
      (with-temp-buffer
        (let ((status (ignore-errors
                        (call-process script nil '(t t) nil op (symbol-name kind)
                                      (expand-file-name store) path))))
          (cons (or status -1) (string-trim (buffer-string))))))))

(defun ghostherd--pretrusted ()
  "The trust the herd recorded, read from `ghostherd-pretrust-file'."
  (and ghostherd-pretrust-file (file-exists-p ghostherd-pretrust-file)
       (ignore-errors
         (with-temp-buffer
           (insert-file-contents ghostherd-pretrust-file)
           (read (current-buffer))))))

(defun ghostherd--pretrusted-save (entries)
  "Keep ENTRIES as the trust the herd recorded."
  (when ghostherd-pretrust-file
    (make-directory (file-name-directory (expand-file-name ghostherd-pretrust-file)) t)
    (with-temp-file ghostherd-pretrust-file
      (let ((print-length nil) (print-level nil))
        (prin1 entries (current-buffer))))))

(defun ghostherd--worktree-pretrust (kind project path name)
  "Record worktree PATH, just created off PROJECT for agent NAME, trusted
in KIND's store when KIND trusts PROJECT's main checkout by that store.
Logged under NAME; non-nil when recorded."
  (when-let* ((ghostherd-pretrust)
              (store (alist-get kind ghostherd-trust-stores))
              (root (file-name-as-directory (expand-file-name ghostherd-worktree-root)))
              (path (directory-file-name (file-truename path)))
              ((string-prefix-p (file-name-as-directory (file-truename root))
                                (file-name-as-directory path)))
              (main (directory-file-name (file-truename project)))
              ((eq 0 (car (ghostherd--trust-run "trusts" kind store main)))))
    (let ((r (ghostherd--trust-run "add" kind store path)))
      (cond
       ((not (eq 0 (car r)))
        (ghostherd--log-add name 'life (format "not trusted for %s: %s" kind (cdr r)))
        nil)
       ((not (equal (cdr r) "added")) nil)
       ;; Unrecorded, it would never be dropped: take it back.
       ((not (ignore-errors
               (ghostherd--pretrusted-save
                (cons (list :path path :kind kind :store store :name name)
                      (ghostherd--pretrusted)))
               t))
        (ghostherd--trust-run "remove" kind store path)
        (ghostherd--log-add name 'life (format "not trusted for %s: %s cannot be written"
                                               kind ghostherd-pretrust-file))
        nil)
       (t
        (ghostherd--log-add name 'life
                            (format "trusted %s for %s, as it trusts %s"
                                    (abbreviate-file-name path) kind
                                    (abbreviate-file-name main)))
        t)))))

(defun ghostherd--worktree-untrust (path)
  "Drop the trust the herd recorded for worktree PATH, now removed."
  (let* ((path (directory-file-name (expand-file-name path)))
         (all (ghostherd--pretrusted))
         (mine (seq-filter (lambda (e) (member (plist-get e :path)
                                               (list path (ignore-errors
                                                            (directory-file-name (file-truename path))))))
                           all)))
    (dolist (e mine)
      (let ((r (ghostherd--trust-run "remove" (plist-get e :kind) (plist-get e :store)
                                     (plist-get e :path))))
        (ghostherd--log-add (or (plist-get e :name) path) 'life
                            (if (eq 0 (car r))
                                (format "dropped the trust it gave %s in %s"
                                        (plist-get e :kind) (abbreviate-file-name path))
                              (format "could not drop the trust it gave %s in %s: %s"
                                      (plist-get e :kind) (abbreviate-file-name path) (cdr r))))))
    (when mine
      (ghostherd--pretrusted-save (seq-difference all mine)))))

(defun ghostherd--worktree-create (project name &optional branch kind)
  "Create a git worktree of PROJECT for agent NAME; return its plist.
The worktree goes under `ghostherd-worktree-root' on a new BRANCH (NAME by
default) off PROJECT's current HEAD.  Returns (:path :branch :base).
For an agent of KIND, it is recorded trusted where KIND trusts PROJECT
(`ghostherd--worktree-pretrust').  Signals a `user-error' when git
cannot, so the caller may fall back to a plain directory."
  (let* ((project (file-name-as-directory (expand-file-name project)))
         (branch (or branch name))
         (path (directory-file-name (ghostherd--worktree-path project name)))
         (base (ghostherd--worktree-base project))
         (default-directory project))
    (unless base
      (user-error "Not a git checkout with a commit: %s" (abbreviate-file-name project)))
    (when (file-exists-p path)
      (user-error "Worktree path already exists: %s" (abbreviate-file-name path)))
    (make-directory (file-name-directory path) t)
    (with-temp-buffer
      (unless (eq 0 (call-process "git" nil '(t t) nil
                                  "worktree" "add" "-b" branch path base))
        (user-error "git worktree add failed: %s" (string-trim (buffer-string)))))
    (when kind
      (ghostherd--worktree-pretrust kind project path name))
    (list :path (file-name-as-directory path) :branch branch :base base)))

(defun ghostherd--worktree-clean-p (path)
  "Non-nil when git worktree PATH has no uncommitted or untracked change.
The dependencies the herd carried in do not count against this: it marks
them ignored in the worktree (`ghostherd--worktree-exclude'), so git does
not list them here, in an agent's own `git status', or in the page's count."
  (and (file-directory-p (expand-file-name path))
       (let ((default-directory (file-name-as-directory (expand-file-name path))))
         (with-temp-buffer
           (and (eq 0 (ignore-errors
                        (call-process "git" nil '(t nil) nil "status" "--porcelain")))
                (= 0 (buffer-size)))))))

(defun ghostherd--worktree-merged-p (main branch base)
  "Non-nil when BRANCH is merged into BASE in the MAIN checkout -- its tip
an ancestor of BASE, so removing BRANCH would lose no commit."
  (and (stringp branch) (stringp base)
       (let ((default-directory (file-name-as-directory (expand-file-name main))))
         (eq 0 (ignore-errors
                 (call-process "git" nil nil nil
                               "merge-base" "--is-ancestor" branch base))))))

(defcustom ghostherd-worktree-stat-ttl 20
  "Seconds a worktree's commits-ahead and merged state are cached for the
snapshot `herd list' reads, so git is not run every poll.  A worktree's
branch moves rarely; this bounds the cost to one check every so often."
  :type 'number
  :group 'ghostherd)

(defvar ghostherd--worktree-stat (make-hash-table :test 'equal)
  "Session id -> (AT AHEAD MERGED): a worktree's commits ahead of its base
and whether it is merged, cached for `ghostherd-worktree-stat-ttl' seconds.")

(defun ghostherd--worktree-stat (session)
  "(:ahead N :merged BOOL) for SESSION's worktree, cached; nil with no
worktree.  AHEAD is its own commits past its base; MERGED is t once the
base contains them -- the work is in, the worktree removable."
  (when-let* ((wt (ghostherd-session-worktree session))
              (path (plist-get wt :path))
              (branch (plist-get wt :branch))
              (base (plist-get wt :base))
              (main (ghostherd-session-project session)))
    (let* ((id (ghostherd-session-id session))
           (c (gethash id ghostherd--worktree-stat)))
      (if (and c (< (- (float-time) (nth 0 c)) ghostherd-worktree-stat-ttl))
          (list :ahead (nth 1 c) :merged (nth 2 c))
        (let* ((ahead (or (and (file-directory-p (expand-file-name main))
                               (let ((default-directory
                                      (file-name-as-directory (expand-file-name main))))
                                 (with-temp-buffer
                                   (and (eq 0 (ignore-errors
                                                (call-process "git" nil '(t nil) nil "rev-list"
                                                              "--count" (concat base ".." branch))))
                                        (string-to-number (string-trim (buffer-string)))))))
                          0))
               (merged (ghostherd--worktree-merged-p main branch base)))
          (puthash id (list (float-time) ahead merged) ghostherd--worktree-stat)
          (list :ahead ahead :merged merged))))))

;;; An agent whose directory is gone
;;
;; A worktree removed outside the herd -- `git worktree remove' by hand
;; once its branch merged, an `rm -rf' -- leaves its agent where it was,
;; and the herd went on as if the directory were there: `herd list' named
;; it, `herd stop' said the worktree "was not merged or not clean, so it
;; was kept", and a respawn started the agent in the home directory --
;; tmux's `new-session -c' falls back to $HOME, without a word, when the
;; directory is missing, and there claude asked to trust all of ~.  So
;; the poll looks, once a minute, whether each agent's directory is still
;; there.  One that is gone is marked -- its sidebar row, its card, `herd
;; list' -- the Inbox tells the user where its branch stands, and nothing
;; starts it there again.  It is never stopped for the user: whether its
;; work is safe to leave is theirs to say.

(defcustom ghostherd-directory-check-every 60
  "Seconds between the poll's looks at whether each agent's directory is
still there: a stat per agent.  One found gone has its branch read again,
by git in the background, at each look; one marked gone is looked at
again whenever it is shown, so it is unmarked as soon as it is back."
  :type 'number
  :group 'ghostherd)

(defcustom ghostherd-gone-glyph "✗"
  "Marker in the sidebar's glyph column for an agent whose directory is
gone.  It takes the spare character the detached marker rides in."
  :type 'string
  :group 'ghostherd)

(defvar ghostherd--dir-gone (make-hash-table :test 'equal)
  "Session id -> a plist of its directory that is gone: :path, :since
\(float time) and :worktree; for a worktree, once git has said where its
branch stands, :branch, :base, :state (`merged', `deleted', `ahead' or
`unknown') and :ahead, its commits not on base.")

(defvar ghostherd--dir-gone-reading (make-hash-table :test 'equal)
  "Session id -> the git process reading where its gone worktree's branch
stands, while it does.")

(defvar ghostherd--dir-checked-at 0
  "When the poll last looked at every agent's directory (float time).")

(defun ghostherd--recorded-directory (session)
  "Where SESSION works by its recipe -- its worktree, else its project --
which is where a respawn would start it; nil when it has neither."
  (or (plist-get (ghostherd-session-worktree session) :path)
      (ghostherd-session-project session)))

(defun ghostherd--directory-missing-p (dir)
  "Non-nil when DIR, a local directory, is not there; nil for no DIR.
No file-name handlers: an agent's directory is local (tmux starts it
there), and their regexps would cost more than the stat."
  (and (stringp dir) (not (string-empty-p dir))
       (let ((file-name-handler-alist nil))
         (not (file-directory-p (expand-file-name dir))))))

(defun ghostherd--mark-gone (session &optional no-read)
  "Mark SESSION's directory gone, when it is and is not marked yet; then,
unless NO-READ, read where its branch stands.  Its entry, or nil when it
is there."
  (let* ((id (ghostherd-session-id session))
         (dir (ghostherd--recorded-directory session))
         (was (gethash id ghostherd--dir-gone)))
    (when (ghostherd--directory-missing-p dir)
      (unless (and was (equal (plist-get was :path) dir))
        (let ((worktree (and (plist-get (ghostherd-session-worktree session) :path) t)))
          (puthash id (list :path dir :since (float-time) :worktree worktree)
                   ghostherd--dir-gone)
          (ghostherd--log-add session 'life
                              (format "its %s %s is gone"
                                      (if worktree "worktree" "directory")
                                      (abbreviate-file-name dir)))))
      (unless no-read
        (ghostherd--gone-branch-read session))
      (gethash id ghostherd--dir-gone))))

(defun ghostherd--unmark-gone (session)
  "Unmark SESSION, whose directory is back."
  (when (gethash (ghostherd-session-id session) ghostherd--dir-gone)
    (remhash (ghostherd-session-id session) ghostherd--dir-gone)
    (ghostherd--log-add session 'life "its directory is back")))

(defun ghostherd-session-directory-gone (session)
  "SESSION's gone directory as `ghostherd--dir-gone' has it, or nil.
Looked at again here -- one stat, and only for an agent marked gone -- so
one whose directory is back is unmarked at once, not at the next look."
  (when-let* ((gone (gethash (ghostherd-session-id session) ghostherd--dir-gone)))
    (if (ghostherd--directory-missing-p (plist-get gone :path))
        gone
      (ghostherd--unmark-gone session)
      nil)))

(defun ghostherd--check-directories (&optional now)
  "Mark each agent whose directory is gone and unmark each whose is back;
at most once every `ghostherd-directory-check-every' seconds, unless NOW.
From the poll.  Returns non-nil when a mark changed."
  (when (or now (>= (- (float-time) ghostherd--dir-checked-at)
                    ghostherd-directory-check-every))
    (setq ghostherd--dir-checked-at (float-time))
    (let ((before (hash-table-count ghostherd--dir-gone))
          (paths (let (l) (maphash (lambda (id g) (push (cons id (plist-get g :path)) l))
                                   ghostherd--dir-gone)
                      l))
          changed)
      (dolist (s (hash-table-values ghostherd--sessions))
        (if (ghostherd--mark-gone s)
            (unless (equal (cdr (assoc (ghostherd-session-id s) paths))
                           (ghostherd--recorded-directory s))
              (setq changed t))
          (when (gethash (ghostherd-session-id s) ghostherd--dir-gone)
            (ghostherd--unmark-gone s)
            (setq changed t))))
      ;; An agent that left the herd takes its mark with it.
      (dolist (id (mapcar #'car paths))
        (unless (gethash id ghostherd--sessions)
          (remhash id ghostherd--dir-gone)))
      (when (or changed (/= before (hash-table-count ghostherd--dir-gone)))
        (ghostherd--sidebar-refresh)
        t))))

(defconst ghostherd--gone-branch-script
  "cd \"$1\" 2>/dev/null && git rev-parse --git-dir >/dev/null 2>&1 || { echo unknown; exit 0; }
git rev-parse -q --verify \"refs/heads/$2\" >/dev/null 2>&1 || { echo deleted; exit 0; }
git rev-list --count \"$3..refs/heads/$2\" 2>/dev/null || echo unknown"
  "Where a gone worktree's branch stands, read in its main checkout $1:
`deleted' when branch $2 is no more; else how many of its commits are not
on base $3, 0 when it is merged; `unknown' when git cannot say -- the
main checkout gone as well, or the base.")

(defun ghostherd--gone-branch-parse (out)
  "(STATE . AHEAD) from `ghostherd--gone-branch-script''s OUT: STATE
`merged', `deleted', `ahead' or `unknown', AHEAD the commits not on base."
  (let ((s (car (last (split-string (or out "") "\n" t "[ \t\r]+")))))
    (cond ((equal s "deleted") '(deleted . 0))
          ((and s (string-match-p "\\`[0-9]+\\'" s))
           (let ((n (string-to-number s)))
             (if (zerop n) '(merged . 0) (cons 'ahead n))))
          (t '(unknown . 0)))))

(defun ghostherd--gone-branch-args (session)
  "The script's arguments (MAIN BRANCH BASE) for SESSION's worktree, or
nil when it has none to read."
  (let* ((wt (ghostherd-session-worktree session))
         (branch (plist-get wt :branch))
         (base (plist-get wt :base))
         (main (ghostherd-session-project session)))
    (and (plist-get wt :path) (stringp branch) (stringp base) (stringp main)
         (list (expand-file-name main) branch base))))

(defun ghostherd--gone-branch-put (session args out)
  "Put where SESSION's branch stands, from the script's OUT for ARGS, into
its gone entry, when it still has one."
  (let ((id (ghostherd-session-id session)))
    (when-let* ((gone (gethash id ghostherd--dir-gone)))
      (pcase-let ((`(,state . ,ahead) (ghostherd--gone-branch-parse out)))
        (puthash id (append (list :branch (nth 1 args) :base (nth 2 args)
                                  :state state :ahead ahead)
                            (list :path (plist-get gone :path) :since (plist-get gone :since)
                                  :worktree (plist-get gone :worktree)))
                 ghostherd--dir-gone)))))

(defun ghostherd--gone-branch-read (session)
  "Read where SESSION's gone worktree's branch stands into its entry, in
the background: git takes milliseconds a call, and this runs from the
poll.  One read at a time per agent; nothing for one with no worktree."
  (let ((id (ghostherd-session-id session))
        (args (ghostherd--gone-branch-args session)))
    (when (and args (not (process-live-p (gethash id ghostherd--dir-gone-reading))))
      (let ((buf (generate-new-buffer " *ghostherd-gone*"))
            ;; The buffer the timer runs in may itself be in a directory
            ;; that is gone, and a process cannot start there.
            (default-directory "/"))
        (condition-case err
            (puthash
             id
             (make-process
              :name "ghostherd-gone" :buffer buf :noquery t :connection-type 'pipe
              :command (append (list "sh" "-c" ghostherd--gone-branch-script "sh") args)
              :sentinel
              (lambda (proc _event)
                (unless (process-live-p proc)
                  ;; Its own entry only: an agent stopped and started again
                  ;; under the name may be reading by now.
                  (when (eq (gethash id ghostherd--dir-gone-reading) proc)
                    (remhash id ghostherd--dir-gone-reading))
                  (let ((out (and (buffer-live-p buf)
                                  (with-current-buffer buf (buffer-string)))))
                    (when (buffer-live-p buf) (kill-buffer buf))
                    (when (eq (ghostherd-get id) session)
                      (ghostherd--gone-branch-put session args out))))))
             ghostherd--dir-gone-reading)
          (error
           (when (buffer-live-p buf) (kill-buffer buf))
           (message "ghostherd: reading %s's branch: %s" id (error-message-string err))))))))

(defun ghostherd--gone-branch-now (session)
  "Where SESSION's gone worktree's branch stands, read now: (STATE . AHEAD)
as `ghostherd--gone-branch-parse' gives it, or nil with no worktree.  For
a command, not the poll: it waits on git."
  (when-let* ((args (ghostherd--gone-branch-args session)))
    (with-temp-buffer
      (let ((default-directory "/"))
        (apply #'call-process "sh" nil '(t nil) nil "-c" ghostherd--gone-branch-script "sh" args))
      (let ((out (buffer-string)))
        (ghostherd--gone-branch-put session args out)
        (ghostherd--gone-branch-parse out)))))

(defun ghostherd--gone-branch-words (branch base state ahead)
  "Where BRANCH stands against BASE, by STATE and AHEAD, in a few words."
  (pcase state
    ('merged (format "branch %s is merged into %s" branch base))
    ('deleted (format "branch %s is gone too" branch))
    ('ahead (format "branch %s has %d commit%s not on %s" branch ahead
                    (if (= ahead 1) "" "s") base))
    (_ (format "whether branch %s is merged into %s could not be read" branch base))))

(defun ghostherd--refuse-gone (session doing)
  "Signal a `user-error' when SESSION's directory is gone, rather than
DOING it (\"respawning\", \"relaying\"): it would start in the home
directory.  SESSION is marked gone first, so the Inbox says so."
  (when (ghostherd--mark-gone session)
    (let ((gone (gethash (ghostherd-session-id session) ghostherd--dir-gone)))
      (user-error "%s's %s %s is gone: not %s it (it would start in your home directory); stop it instead"
                  (ghostherd-session-name session)
                  (if (plist-get gone :worktree) "worktree" "directory")
                  (abbreviate-file-name (plist-get gone :path))
                  doing))))

(defun ghostherd--worktree-remove (main path &optional force branch)
  "Remove git worktree PATH of the MAIN checkout; return t on success.
With FORCE, discard the worktree's uncommitted changes.  With BRANCH, also
delete it with `git branch -d', which refuses an unmerged branch -- so a
branch's committed work is never lost here.  The trust the herd recorded
for it goes with it (`ghostherd--worktree-untrust')."
  (let ((default-directory (file-name-as-directory (expand-file-name main))))
    (with-temp-buffer
      (when (eq 0 (apply #'call-process "git" nil '(t t) nil
                         `("worktree" "remove" ,@(and force '("--force"))
                           ,(directory-file-name (expand-file-name path)))))
        (when branch
          (call-process "git" nil nil nil "branch" "-d" branch))
        (ghostherd--worktree-untrust path)
        t))))

(defun ghostherd--worktree-cleanup (session)
  "When a gone SESSION had its own git worktree, remove it and its branch if
the branch is merged into its base and the worktree is clean; otherwise
keep both -- the user removes an unmerged one by hand with
`ghostherd-worktree-forget'.  Never removes unmerged or uncommitted work,
and never merges.  Safe to call for an agent with no worktree."
  (when-let* ((wt (ghostherd-session-worktree session))
              (path (plist-get wt :path))
              (main (ghostherd-session-project session))
              ((file-directory-p (expand-file-name path))))
    (let ((branch (plist-get wt :branch))
          (base (plist-get wt :base)))
      (if (and (ghostherd--worktree-clean-p path)
               (ghostherd--worktree-merged-p main branch base)
               (ghostherd--worktree-remove main path nil branch))
          (ghostherd--log-add session 'life
                              (format "worktree merged and clean: removed %s and branch %s"
                                      (abbreviate-file-name path) branch))
        (ghostherd--log-add session 'life
                            (format "worktree kept, not merged/clean: %s on %s -- M-x ghostherd-worktree-forget"
                                    (abbreviate-file-name path) branch))
        (message "ghostherd: kept %s's worktree %s (branch %s): not merged or not clean"
                 (ghostherd-session-name session) (abbreviate-file-name path) branch)))))

(defun ghostherd--worktree-list (project)
  "Linked git worktrees of the MAIN checkout PROJECT, as (PATH . BRANCH);
the main checkout itself is left out."
  (let ((main (file-truename (directory-file-name (expand-file-name project))))
        (default-directory (file-name-as-directory (expand-file-name project)))
        cur res)
    (with-temp-buffer
      (when (eq 0 (ignore-errors
                    (call-process "git" nil '(t nil) nil
                                  "worktree" "list" "--porcelain")))
        (dolist (line (split-string (buffer-string) "\n"))
          (cond
           ((string-prefix-p "worktree " line)
            (setq cur (list :path (substring line (length "worktree ")))))
           ((string-prefix-p "branch " line)
            (setq cur (plist-put cur :branch
                                 (replace-regexp-in-string
                                  "\\`refs/heads/" "" (substring line (length "branch "))))))
           ((string-empty-p line)
            (when (and cur (not (equal (file-truename (directory-file-name (plist-get cur :path))) main)))
              (push (cons (file-name-as-directory (plist-get cur :path)) (plist-get cur :branch)) res))
            (setq cur nil))))
        (when (and cur (not (equal (file-truename (directory-file-name (plist-get cur :path))) main)))
          (push (cons (file-name-as-directory (plist-get cur :path)) (plist-get cur :branch)) res))))
    (nreverse res)))

(defun ghostherd-worktree-forget (path)
  "Remove the idle git worktree at PATH, after confirming; its branch is
kept, so committed work is never lost.  Interactively, offers the
worktrees of the project at point or the current directory that no live
agent is in.  Uncommitted or untracked changes are discarded only after a
second confirmation."
  (interactive
   (let* ((project (or (ghostherd--project-root) default-directory))
          (busy (delq nil (mapcar (lambda (s)
                                    (when-let* ((p (plist-get (ghostherd-session-worktree s) :path)))
                                      (file-truename (directory-file-name p))))
                                  (ghostherd-sessions))))
          (choices (seq-remove (lambda (pb)
                                 (member (file-truename (directory-file-name (car pb))) busy))
                               (ghostherd--worktree-list project))))
     (unless choices
       (user-error "No idle worktree to remove under %s" (ghostherd--abbreviate project)))
     (let ((alist (mapcar (lambda (pb)
                            (cons (format "%s  [%s]" (abbreviate-file-name (car pb)) (or (cdr pb) "?"))
                                  (car pb)))
                          choices)))
       (list (cdr (assoc (completing-read "Remove idle worktree: " alist nil t) alist))))))
  (let* ((path (file-name-as-directory (expand-file-name path)))
         (main (ghostherd--worktree-main-checkout path)))
    (unless main
      (user-error "Not a linked git worktree: %s" (abbreviate-file-name path)))
    (unless (yes-or-no-p (format "Remove worktree %s?  Its branch and commits are kept. "
                                 (abbreviate-file-name path)))
      (user-error "Not removed"))
    (let ((clean (ghostherd--worktree-clean-p path)))
      (if (and (not clean)
               (not (yes-or-no-p
                     (format "%s has uncommitted or untracked changes; discard them? "
                             (abbreviate-file-name path)))))
          (user-error "Not removed")
        (if (ghostherd--worktree-remove main path (not clean))
            (message "ghostherd: removed worktree %s (branch kept)" (abbreviate-file-name path))
          (user-error "git could not remove %s" (abbreviate-file-name path)))))))

;; A worktree is a fresh checkout: its code is there, but a project's
;; uninstalled dependencies -- a Python venv, node_modules -- are not.
;; Carrying them in makes a worktree usable at once.  A uv project's venv
;; is the worktree's own, synced from its own lock: a symlink to the main
;; checkout's would let a `uv sync' or `uv run' in the worktree rewrite the
;; main checkout's venv to the branch's lock.  uv clones from its cache
;; (copy-on-write on APFS), so that costs little disk and, warm, well under
;; a second.  Any other venv holds absolute paths, so it is symlinked to the
;; main checkout's rather than copied; a node_modules is cloned
;; copy-on-write (APFS `cp -c'), so the worktree may diverge it cheaply.
;; Build caches are simply left off the list.  A clone that cannot be made
;; is skipped and said so on the card -- never a full copy, since the disk
;; is not big.  All of it runs in the background.

(defcustom ghostherd-worktree-deps
  '((".venv" . uv) ("venv" . uv) ("node_modules" . clone))
  "Dependencies carried into a new worktree, each (PATH . METHOD).
PATH is relative to the checkout root; a bare name (no slash) is found by
name among the main checkout's ignored directories at any depth, so
`.venv' also finds server/.venv.  METHOD is one of:
 - `uv' -- the worktree's own venv when the venv's directory in the
   worktree has a `uv.lock' and `uv' is on PATH: `uv sync --frozen' there,
   from the worktree's own lock.  uv clones from its cache (copy-on-write
   on APFS, its default link mode on macOS), so it is cheap, and a `uv
   sync' in the worktree then never touches the main checkout's venv, as
   it would through a symlink.  With no `uv.lock' there or no `uv', a
   symlink, as `symlink'.  A sync that fails is skipped and said so on the
   card, its partial venv removed -- never a symlink in its place;
 - `symlink' -- a link to the main checkout's, for a thing that holds
   absolute paths and breaks when copied, like a venv uv does not manage;
 - `clone' -- an APFS copy-on-write copy (cp -c) the worktree may then
   diverge, like node_modules;
 - `venv' -- a per-worktree virtualenv made at PATH as named (never
   found), by `uv sync --frozen' when PATH's directory has a `uv.lock' --
   for a venv the main checkout does not have.  Skipped, and said so on the
   card, when there is no `uv.lock' there or no `uv' on PATH -- never a
   lock-less install.
A uv/symlink/clone PATH with no source in the main checkout is skipped; a
clone that cannot be made is skipped and said so, never copied in full.
Build caches are left off this list.  Override per project with a
`ghostherd-project-worktree-deps' dir-local; your own value of this, or
that dir-local, wins over the default."
  :type '(alist :key-type string
                :value-type (choice (const uv) (const symlink) (const clone) (const venv)))
  :group 'ghostherd)

(defvar ghostherd-project-worktree-deps 'unset
  "Per-project override of `ghostherd-worktree-deps', from .dir-locals.el.

  ((nil . ((ghostherd-project-worktree-deps . ((\".venv\" . venv))))))")
(put 'ghostherd-project-worktree-deps 'safe-local-variable
     (lambda (v) (and (listp v)
                      (seq-every-p (lambda (e) (and (consp e) (stringp (car e))
                                                    (memq (cdr e) '(uv symlink clone venv))))
                                   v))))

(defvar ghostherd--worktree-deps-status (make-hash-table :test 'equal)
  "Session id -> alist of its worktree dependencies' outcome: PATH ->
(:status STATUS :how HOW :reason REASON), STATUS `done' or `skipped', HOW
for a done one what it is: \"uv\" (its own venv), \"symlink\" or
\"clone\".")

(defun ghostherd-session-worktree-deps (session)
  "The outcome of SESSION's worktree dependencies (PATH -> plist), or nil."
  (gethash (ghostherd-session-id session) ghostherd--worktree-deps-status))

(defun ghostherd--worktree-deps-for (dir)
  "The dependency specs for a worktree of the project at DIR: its
`ghostherd-project-worktree-deps' dir-local when set, else
`ghostherd-worktree-deps'."
  (let ((local (ignore-errors
                 (with-temp-buffer
                   (setq default-directory (file-name-as-directory (expand-file-name dir)))
                   (setq-local buffer-file-name (expand-file-name ".ghostherd" default-directory))
                   (hack-dir-local-variables)
                   (assq 'ghostherd-project-worktree-deps file-local-variables-alist)))))
    (if local (cdr local) ghostherd-worktree-deps)))

(defun ghostherd--worktree-ignored-dirs (main)
  "The MAIN checkout's ignored directories, relative and without a trailing
slash.  `git ls-files --directory' lists an ignored directory once and does
not descend into it, so nothing inside one (a vendored repo, a build dir)
is picked."
  (let ((default-directory (file-name-as-directory (expand-file-name main))))
    (with-temp-buffer
      (when (eq 0 (ignore-errors
                    (call-process "git" nil '(t nil) nil "ls-files" "-o" "-i"
                                  "--exclude-standard" "--directory")))
        (mapcar #'directory-file-name (split-string (buffer-string) "\n" t))))))

(defun ghostherd--worktree-deps-resolve (main specs)
  "Turn SPECS into concrete (RELPATH . METHOD) for the MAIN checkout.
A spec whose path has a slash is explicit and kept as is.  A bare name is
matched by directory name against the main checkout's ignored directories
at any depth, so a project with its deps in subdirectories -- backend/.venv,
frontend/node_modules -- gets them without naming each path."
  (let ((ignored (ghostherd--worktree-ignored-dirs main))
        out)
    (dolist (spec specs)
      (let ((name (car spec)) (method (cdr spec)))
        (cond
         ;; A venv is created, not found, so its path is taken as named
         ;; (relative to the worktree) -- not matched against existing dirs.
         ((eq method 'venv)
          (push (cons (directory-file-name name) method) out))
         ((string-match-p "/" name)
          (push (cons (directory-file-name name) method) out))
         (t (dolist (d ignored)
              (when (equal (file-name-nondirectory d) name)
                (push (cons d method) out)))))))
    (seq-uniq (nreverse out))))

(defun ghostherd--worktree-exclude (worktree relpaths)
  "Make git ignore RELPATHS inside WORKTREE, at the source: anchored
patterns (`/path') appended to the repo's info/exclude, which git keeps
shared across a repo's worktrees.  Anchored, so in the main checkout each
matches only the real dependency already at that path -- a venv or
node_modules, ignorable anyway -- and nowhere else.  So an agent's own
`git status', a `git add', and the page's uncommitted count never see a
carried-in dependency.  Idempotent; `extensions.worktreeConfig' untouched."
  (when relpaths
    (let ((default-directory (file-name-as-directory (expand-file-name worktree))))
      (when-let* ((raw (ignore-errors
                         (with-temp-buffer
                           (when (eq 0 (call-process "git" nil '(t nil) nil
                                                     "rev-parse" "--git-path" "info/exclude"))
                             (string-trim (buffer-string))))))
                  ((not (string-empty-p raw)))
                  (exclude (expand-file-name raw)))
        (let* ((content (and (file-exists-p exclude)
                             (with-temp-buffer (insert-file-contents exclude) (buffer-string))))
               (have (and content (split-string content "\n")))
               (want (seq-uniq (mapcar (lambda (p)
                                         (concat "/" (directory-file-name (string-remove-prefix "/" p))))
                                       relpaths)))
               (new (seq-remove (lambda (p) (member p have)) want)))
          (when new
            (make-directory (file-name-directory exclude) t)
            (let ((lead (if (and content (not (string-empty-p content))
                                 (not (string-suffix-p "\n" content)))
                            "\n" "")))
              (write-region (concat lead (mapconcat #'identity new "\n") "\n")
                            nil exclude t 'silent))))))))

(defun ghostherd--worktree-deps-script (specs main worktree)
  "A /bin/sh script setting SPECS up into WORKTREE from MAIN, printing one
status line per spec -- `done PATH HOW' or `skip PATH REASON' -- and never
copying a dependency in full."
  (mapconcat
   (lambda (spec)
     (let* ((name (car spec))
            (src (shell-quote-argument (directory-file-name (expand-file-name name main))))
            (dstf (directory-file-name (expand-file-name name worktree)))
            (dst (shell-quote-argument dstf))
            (par (shell-quote-argument (directory-file-name (file-name-directory dstf))))
            (tag (shell-quote-argument name))
            (link (format "elif mkdir -p %s 2>/dev/null && ln -s %s %s 2>/dev/null; then echo done %s symlink; else echo skip %s link-failed; fi"
                          par src dst tag tag)))
       (pcase (cdr spec)
         ('symlink
          (format "if [ ! -e %s ]; then echo skip %s no-source; elif [ -e %s ]; then echo skip %s exists; %s"
                  src tag dst tag link))
         ('clone
          (format "if [ ! -e %s ]; then echo skip %s no-source; elif [ -e %s ]; then echo skip %s exists; elif mkdir -p %s 2>/dev/null && cp -c -R %s %s 2>/dev/null; then echo done %s clone; else echo skip %s no-copy-on-write; fi"
                  src tag dst tag par src dst tag tag))
         ;; The worktree's own venv, from its own lock, when its directory
         ;; has one and there is a uv: `uv sync --frozen' there, uv cloning
         ;; from its cache.  Else the symlink, as before.  A failed sync
         ;; is said so, its partial venv gone -- not a symlink, which would
         ;; bring back a worktree's `uv sync' writing the main checkout's.
         ('uv
          (format "if [ ! -e %s ]; then echo skip %s no-source; elif [ -e %s ] || [ -L %s ]; then echo skip %s exists; elif [ -f %s/uv.lock ] && command -v uv >/dev/null 2>&1; then if ( cd %s && UV_PROJECT_ENVIRONMENT=%s uv sync --frozen >/dev/null 2>&1 ); then echo done %s uv; else rm -rf %s; echo skip %s uv-sync-failed; fi; %s"
                  src tag dst dst tag par par dst tag dst tag link))
         ;; A fresh virtualenv from the worktree's own lock: `uv sync
         ;; --frozen' in the venv's directory (where uv.lock sits), uv
         ;; cloning from its cache.  Skipped, not faked, when there is
         ;; no uv.lock there or no uv -- never a lock-less install.
         ('venv
          (format "if [ -e %s ]; then echo skip %s exists; elif ! command -v uv >/dev/null 2>&1; then echo skip %s no-uv; elif [ ! -f %s/uv.lock ]; then echo skip %s no-uv.lock; elif ( cd %s && UV_PROJECT_ENVIRONMENT=%s uv sync --frozen >/dev/null 2>&1 ); then echo done %s uv; else echo skip %s uv-sync-failed; fi"
                  dst tag tag par tag par dst tag tag))
         (_ (format "echo skip %s unknown-method" tag)))))
   specs
   "\n"))

(defun ghostherd--worktree-deps-record (id output)
  "Store the dependency OUTPUT (status lines) for session ID."
  (let (alist)
    (dolist (line (split-string (or output "") "\n" t))
      (pcase (split-string (string-trim line) " ")
        (`("done" ,name . ,how)
         (push (cons name (list :status 'done :how (car how))) alist))
        (`("skip" ,name . ,reason)
         (push (cons name (list :status 'skipped :reason (string-join reason " "))) alist))))
    (when alist
      (puthash id (nreverse alist) ghostherd--worktree-deps-status))))

(defun ghostherd--worktree-deps-setup (main worktree id &optional specs)
  "Carry the project's dependencies into WORKTREE from the MAIN checkout,
for the agent with session ID, in the background.  SPECS default to the
project's (`ghostherd--worktree-deps-for').  Records each one's outcome and
refreshes the page; never blocks Emacs, and never copies in full."
  (when-let* ((specs (or specs (ghostherd--worktree-deps-for main)))
              (main (file-name-as-directory (expand-file-name main)))
              (worktree (file-name-as-directory (expand-file-name worktree)))
              ;; Bare names found among the project's ignored dirs at any
              ;; depth; explicit paths kept.  Resolving against the main
              ;; checkout is read-only.
              (resolved (ghostherd--worktree-deps-resolve main specs)))
    ;; Mark them ignored in the worktree before they appear, so no agent's
    ;; status or add, and no page count, ever sees a carried-in dependency.
    (ignore-errors (ghostherd--worktree-exclude worktree (mapcar #'car resolved)))
    (let ((buf (generate-new-buffer " *ghostherd-deps*"))
          (script (ghostherd--worktree-deps-script resolved main worktree)))
      (condition-case err
          (make-process
           :name "ghostherd-deps" :buffer buf :noquery t
           :command (list "sh" "-c" script)
           :sentinel
           (lambda (proc _e)
             (unless (process-live-p proc)
               (when (buffer-live-p (process-buffer proc))
                 (with-current-buffer (process-buffer proc)
                   (ghostherd--worktree-deps-record id (buffer-string)))
                 (kill-buffer (process-buffer proc)))
               (when (fboundp 'ghostherd-memory-page--work-changed)
                 (ignore-errors (ghostherd-memory-page--work-changed))))))
        (error (when (buffer-live-p buf) (kill-buffer buf))
               (message "ghostherd: worktree deps for %s: %s" id (error-message-string err)))))))

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


;;; Herd log
;;
;; Notifications are the wrong medium for a herd that runs while you are
;; elsewhere: they arrive once, in the order they happened, and are gone.
;; The log is what you read when you come back -- and, because it keeps
;; the screen behind each `blocked', it is also how `:screen-rules' get
;; tuned without anyone having to be present at the moment a rule fired.

(defcustom ghostherd-log-days 7
  "Days of herd-log lines kept.  A line older than this is dropped on the
next trim -- unless it is pinned, which nothing drops.  0 keeps them by
`ghostherd-log-max' alone.

Retention is by age, not count: the question the log answers -- \"what was
grok doing the day before yesterday afternoon\" -- is one about time, and
a count cap turns a busy afternoon into an hour."
  :type 'number
  :group 'ghostherd)

(defcustom ghostherd-log-max 10000
  "An absolute ceiling on herd-log lines, pinned ones aside.

The meaning changed: it was the log's one retention rule, keeping the
newest N; now `ghostherd-log-days' keeps lines by age and this is only a
safety ceiling so a flapping agent cannot grow the log without bound
inside the window.  Reach it and the oldest unpinned lines go even if
they are younger than the day cap.  0 lifts the ceiling."
  :type 'integer
  :group 'ghostherd)

(defcustom ghostherd-log-screens 200
  "How many captured screens the log keeps, pinned ones aside.

The point of keeping them: rule tuning needs the screen a rule fired
against, and =ghostherd-explain= only offers that while the agent is
still sitting on the prompt.  Storing it means the data accumulates
whether or not you were watching.

Captures come from transitions into `blocked', the `capture screen'
button, and the auto-capture of a suspicious screen; a pinned one is kept
whatever this says, and does not count against it.  0 disables keeping
screens for the unpinned ones."
  :type 'integer
  :group 'ghostherd)

(cl-defstruct (ghostherd-log-entry (:constructor ghostherd-log-entry--create)
                                   (:copier nil))
  (time (current-time))
  session
  kind
  text
  screen
  ;; Why a screen was kept -- a symbol: `blocked', `manual' (the button),
  ;; or an auto-capture trigger (`limit', `menu', `mismatch', `compaction').
  ;; nil on an ordinary line.
  why
  ;; Non-nil: exempt from every retention rule, kept until the user says
  ;; otherwise.  The `capture screen' button pins; the Log tab un-pins.
  pinned
  ;; A short note the user may attach when capturing, or nil.
  note
  ;; A number unique within the log, so the page and the CLI can name one
  ;; entry to pin, un-pin or delete.
  id)

(defvar ghostherd--log-seq 0
  "The last id handed to a log entry.  Bumped past what the file holds on
load, so ids stay unique across an Emacs restart.")

(defun ghostherd--log-next-id ()
  "Return the next unique log-entry id."
  (setq ghostherd--log-seq (1+ ghostherd--log-seq)))

(defcustom ghostherd-log-file (locate-user-emacs-file "ghostherd-log.eld")
  "File the herd log is appended to, or nil to keep it in memory only.

On by default, because the restart this is for is the one nobody
planned.  Phase 3 made agents outlive Emacs -- that is what the tmux
backend is *for* -- and the log describes itself as \"what you read when
you get back\", where the most ordinary way of getting back is starting
Emacs again.  A log that only survives while Emacs does answers every
absence except that one.

The screens matter more than the lines here.  `ghostherd-explain' can
only say why an agent is in a state while it is still in it, so a rule
that fired at 02:00 is unarguable by morning unless the screen behind it
was kept -- and \"by morning\" is exactly the span an Emacs restart
tends to fall inside."
  :type '(choice (const :tag "Memory only" nil) file)
  :group 'ghostherd)

(defvar ghostherd--log nil
  "Herd log entries, newest first.")

(defvar ghostherd-log-functions nil
  "Functions called with each new herd log entry, as it is recorded.
The herd page uses it to show transitions as they happen.")

(defvar ghostherd--log-loaded nil
  "Non-nil once `ghostherd-log-file' has been read this session.")

(defvar ghostherd--log-writable t
  "Non-nil while appending to `ghostherd-log-file' is still worth trying.

Cleared after one failure, because the write happens per state
transition: a read-only directory would otherwise produce an error
message every time an agent changed state.")

(defvar ghostherd--log-disk-lines 0
  "Lines written to `ghostherd-log-file' since it was last compacted.")

(defvar ghostherd--log-disk-screens 0
  "Captured screens written to `ghostherd-log-file' since it was last compacted.")

(defun ghostherd--log-form (entry)
  "Return ENTRY as a plist ready to be printed on one line.

A plist rather than the struct itself, deliberately.  Printing
`#s(ghostherd-log-entry ...)' reads back only into the struct as it was
*then*: add a slot and every line written before today comes back with
its fields shifted along, which is the same trap as \"Reloading over a
live herd\" in readme.org and just as remote from its cause.  A plist
tolerates a key that did not exist yet and one that no longer does."
  (append (list :time (ghostherd-log-entry-time entry)
                :session (ghostherd-log-entry-session entry)
                :kind (ghostherd-log-entry-kind entry)
                :text (ghostherd-log-entry-text entry))
          (when-let* ((id (ghostherd-log-entry-id entry)))
            (list :id id))
          (when-let* ((screen (ghostherd-log-entry-screen entry)))
            (list :screen screen))
          (when-let* ((why (ghostherd-log-entry-why entry)))
            (list :why why))
          (when (ghostherd-log-entry-pinned entry)
            (list :pinned t))
          (when-let* ((note (ghostherd-log-entry-note entry)))
            (list :note note))))

(defun ghostherd--log-entry-from-form (form)
  "Rebuild a log entry from FORM, or nil when FORM is not one."
  (when (and (consp form) (plist-member form :time))
    (ghostherd-log-entry--create
     :time (plist-get form :time)
     :session (plist-get form :session)
     :kind (plist-get form :kind)
     :text (plist-get form :text)
     :screen (plist-get form :screen)
     :why (plist-get form :why)
     :pinned (plist-get form :pinned)
     :note (plist-get form :note)
     :id (plist-get form :id))))

(defun ghostherd--log-print (entries)
  "Return ENTRIES, oldest first, as one printed form per line.

One line per entry is what makes appending cheap and a crash survivable:
a half-written last line is the only damage possible, and the loader
stops there.  Three print settings are load-bearing --
`print-escape-newlines' because a captured screen is full of them and
would otherwise span lines, and `print-length' / `print-level' because a
user who set them would get \"...\" written into their own log."
  (let ((print-escape-newlines t)
        (print-length nil)
        (print-level nil))
    (mapconcat (lambda (entry)
                 (concat (prin1-to-string (ghostherd--log-form entry)) "\n"))
               entries "")))

(defun ghostherd--log-write (text append)
  "Write TEXT to `ghostherd-log-file', appending when APPEND is non-nil."
  (when (and ghostherd-log-file ghostherd--log-writable)
    (condition-case error
        (let ((coding-system-for-write 'utf-8))
          ;; Explicit UTF-8, and not decorative: a screen is box drawing and
          ;; a reason can be a prompt in any language, and `write-region'
          ;; without one calls `select-safe-coding-system', which *prompts*
          ;; -- an error in batch, a wedged daemon otherwise.
          (write-region text nil ghostherd-log-file append 'silent))
      (error
       (setq ghostherd--log-writable nil)
       (display-warning
        'ghostherd
        (format "cannot write %s, keeping the herd log in memory only: %s"
                ghostherd-log-file (error-message-string error))
        :warning)))))

(defun ghostherd--log-rewrite ()
  "Rewrite `ghostherd-log-file' from the trimmed in-memory `ghostherd--log'."
  (setq ghostherd--log-disk-lines (length ghostherd--log)
        ghostherd--log-disk-screens (cl-count-if #'ghostherd-log-entry-screen
                                                 ghostherd--log))
  (ghostherd--log-write (ghostherd--log-print (reverse ghostherd--log)) nil))

(defun ghostherd-log-load ()
  "Read `ghostherd-log-file' into the herd log and compact the file.

Compaction happens here and whenever `ghostherd--log-add' has doubled the
entry or screen cap on disk.  A line that will not parse ends the read --
the expected corruption is a truncated tail from a crash, and what
follows one is not worth guessing at."
  (interactive)
  (setq ghostherd--log-loaded t)
  (when (and ghostherd-log-file (file-readable-p ghostherd-log-file))
    (let ((entries nil))
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8))
          (insert-file-contents ghostherd-log-file))
        (goto-char (point-min))
        (condition-case nil
            (while t
              (if-let* ((entry (ghostherd--log-entry-from-form (read (current-buffer)))))
                  (push entry entries)
                ;; A form that is not an entry: skip it rather than stop,
                ;; since two Emacsen appending to one file can interleave.
                nil))
          (error nil)))
      ;; `entries' is newest first already, which is how the log is kept.
      ;; Seed the id sequence past what the file holds, and give an id to
      ;; any line written before ids existed, oldest first so they rise
      ;; with time -- so the page and CLI can always name one entry.
      (setq ghostherd--log-seq
            (apply #'max 0 (delq nil (mapcar #'ghostherd-log-entry-id entries))))
      (dolist (entry (reverse entries))
        (unless (ghostherd-log-entry-id entry)
          (setf (ghostherd-log-entry-id entry) (ghostherd--log-next-id))))
      (setq ghostherd--log (ghostherd--log-trim entries))
      (ghostherd--log-rewrite)
      (let ((read (length ghostherd--log)))
        (when (> read 0)
          ;; Through `--log-add', so the seam is a line of the log like any
          ;; other: trimmed with the rest, and there again next time.
          (ghostherd--log-add "herd" 'life
                              (format "resumed %d entries from an earlier Emacs"
                                      read)))
        read))))

(defun ghostherd--log-trim (entries)
  "Return ENTRIES (newest first) within the log's retention.

Lines older than `ghostherd-log-days' go; `ghostherd-log-max' is an
absolute ceiling on the rest; and screens past `ghostherd-log-screens'
lose the screen but keep the line.  A pinned entry is exempt from all
three -- kept whole, and not counted against the ceiling or the screen
cap, so a pin is a promise the retention does not quietly break.

Entries and screens are bounded separately because they cost
differently: a one-line entry is nothing, and a 40x120 screen is the
actual memory, and now the actual disk."
  (let ((cutoff (and (> ghostherd-log-days 0)
                     (time-subtract (current-time)
                                    (seconds-to-time (* ghostherd-log-days 86400)))))
        (lines 0)
        (screens 0)
        (kept nil))
    (dolist (entry entries)
      (cond
       ((ghostherd-log-entry-pinned entry)
        (push entry kept))
       ((and cutoff (time-less-p (ghostherd-log-entry-time entry) cutoff))
        nil)
       ((and (> ghostherd-log-max 0) (>= lines ghostherd-log-max))
        nil)
       (t
        (setq lines (1+ lines))
        (when (ghostherd-log-entry-screen entry)
          (setq screens (1+ screens))
          (when (and (>= ghostherd-log-screens 0)
                     (> screens ghostherd-log-screens))
            (setf (ghostherd-log-entry-screen entry) nil)))
        (push entry kept))))
    (nreverse kept)))

;;;###autoload
(defun ghostherd-log-clear ()
  "Forget the herd log, on disk as well as in memory.

Quitting Emacs used to do this by accident; now that it does not,
something has to do it on purpose."
  (interactive)
  (when (or (not (called-interactively-p 'interactive))
            (yes-or-no-p "Forget the herd log, including the kept screens? "))
    (setq ghostherd--log nil
          ghostherd--log-disk-lines 0
          ghostherd--log-disk-screens 0)
    (when (and ghostherd-log-file (file-exists-p ghostherd-log-file))
      (ignore-errors (delete-file ghostherd-log-file)))
    (when-let* ((buf (get-buffer "*ghostherd-log*")))
      (ghostherd--log-render buf))
    (message "Herd log cleared")))

(defun ghostherd--log-add (session kind text &optional screen why pinned note)
  "Record TEXT about SESSION under KIND, optionally with a SCREEN.

WHY, when a screen is kept, says what captured it (a symbol: `blocked',
`manual', or an auto-capture trigger).  PINNED exempts the entry from
retention.  NOTE is a short note to attach.  Returns the entry's id."
  (let* ((id (ghostherd--log-next-id))
         (entry (ghostherd-log-entry--create
                 :session (if (ghostherd-session-p session)
                              (ghostherd-session-name session)
                            (format "%s" session))
                 :kind kind :text text :screen screen
                 :why why :pinned pinned :note note :id id)))
    (push entry ghostherd--log)
    (setq ghostherd--log (ghostherd--log-trim ghostherd--log)
          ghostherd--log-disk-lines (1+ ghostherd--log-disk-lines)
          ghostherd--log-disk-screens (+ ghostherd--log-disk-screens
                                         (if screen 1 0)))
    ;; Appended as it happens rather than saved on the way out: a
    ;; `kill-emacs-hook' does not run for the crash this is meant to
    ;; survive, and one line is cheap enough to write per transition.
    ;; Once disk lines or screens reach twice the ceiling, rewrite from the
    ;; trimmed in-memory log so a long-running Emacs does not grow the
    ;; file without bound.  With no ceiling, fall back to the screen cap
    ;; and a fixed line bound, so time-only retention still compacts.
    (if (or (>= ghostherd--log-disk-lines
                (* 2 (if (> ghostherd-log-max 0) ghostherd-log-max 10000)))
            (and (> ghostherd-log-screens 0)
                 (>= ghostherd--log-disk-screens (* 2 ghostherd-log-screens))))
        (ghostherd--log-rewrite)
      (ghostherd--log-write (ghostherd--log-print (list entry)) t))
    (when-let* ((buf (get-buffer "*ghostherd-log*")))
      (when (get-buffer-window buf t)
        (ghostherd--log-render buf)))
    ;; Each listener on its own, demoted: one listener's bug must neither
    ;; cost the herd its log nor keep the next listener from hearing.
    (run-hook-wrapped 'ghostherd-log-functions
                      (lambda (fn e)
                        (with-demoted-errors "ghostherd: log listener: %S"
                          (funcall fn e))
                        nil)
                      entry)
    id))

(defun ghostherd--log-transition (session old new reason)
  "Log SESSION moving from OLD to NEW because of REASON."
  (let ((screen
         ;; One extra capture, only on the transition worth arguing about.
         ;; It is not free -- on the tmux backend it is a subprocess -- but
         ;; an agent blocks once per prompt, not once per poll.
         (when (and (eq new 'blocked) (> ghostherd-log-screens 0))
           (ignore-errors (ghostherd--host-capture session)))))
    (ghostherd--log-add
     session 'state
     (format "%s → %s%s" (or old "?") new (if reason (format "  %s" reason) ""))
     screen (and screen 'blocked))))

(defun ghostherd--capture-text (name why note)
  "The log line for a screen capture of NAME, triggered by WHY, with NOTE."
  (let ((base (pcase why
                ('manual (format "%s: screen captured" name))
                ('limit (format "%s: a limit or quota message" name))
                ('menu (format "%s: a menu no rule knew" name))
                ('mismatch (format "%s: the screen and the hook disagreed" name))
                ('compaction (format "%s: a /compact in progress" name))
                (_ (format "%s: screen captured" name)))))
    (if (and note (not (string-empty-p (string-trim note))))
        (format "%s — %s" base (string-trim note))
      base)))

(defun ghostherd-capture-screen (session &optional note)
  "Save SESSION's current screen to the herd log as a pinned entry.

The manual capture, from the Log tab's button: pinned, so retention never
drops it, and tagged `manual'.  NOTE is a short note to attach.  Returns
the entry id; signals when there is no screen to read."
  (let* ((s (or (ghostherd-get session) (user-error "No such session")))
         (name (ghostherd-session-name s))
         (screen (or (ignore-errors (ghostherd--host-capture s))
                     (gethash (ghostherd-session-id s) ghostherd--screens))))
    (unless (and screen (not (string-empty-p (string-trim screen))))
      (user-error "No screen to capture for %s" name))
    (ghostherd--log-add s 'screen (ghostherd--capture-text name 'manual note)
                        screen 'manual t note)))

(defun ghostherd--log-entry-by-id (id)
  "The log entry whose id is ID, or nil."
  (and id (seq-find (lambda (e) (eql (ghostherd-log-entry-id e) id))
                    ghostherd--log)))

(defun ghostherd--log-refresh ()
  "Redraw the log buffer after the log changed in place."
  (when-let* ((buf (get-buffer "*ghostherd-log*")))
    (when (get-buffer-window buf t)
      (ghostherd--log-render buf))))

(defun ghostherd-log-set-pinned (id pinned)
  "Pin, or with PINNED nil un-pin, the log entry with ID.

A pin exempts it from retention; un-pinning returns it to the caps, but
not retroactively -- it ages out on the next trim, not at once, so an
un-pin-to-move does not lose it.  Persist, and return the entry or nil."
  (when-let* ((entry (ghostherd--log-entry-by-id id)))
    (setf (ghostherd-log-entry-pinned entry) (and pinned t))
    (ghostherd--log-rewrite)
    (ghostherd--log-refresh)
    entry))

(defun ghostherd-log-delete (id)
  "Delete the log entry with ID.  Persist; return non-nil if one went."
  (let ((before (length ghostherd--log)))
    (setq ghostherd--log
          (seq-remove (lambda (e) (eql (ghostherd-log-entry-id e) id))
                      ghostherd--log))
    (when (/= (length ghostherd--log) before)
      (ghostherd--log-rewrite)
      (ghostherd--log-refresh)
      t)))

(defcustom ghostherd-screens-cli-chars 1600
  "Characters of each screen `herd screens' prints before it truncates.
The CLI is a finding aid -- time, agent, reason and enough of the screen
to recognise it; the whole of one is read on the Log tab."
  :type 'integer
  :group 'ghostherd)

(defconst ghostherd-screens-cli-cap 20
  "The most screens `herd screens' prints, newest first.")

(defun ghostherd--log-screens-text (agent since why)
  "The saved screens `herd screens' prints: time, agent, reason, screen.
AGENT limits to one agent, SINCE (epoch seconds) to a time, WHY to a
trigger; each may be nil or empty.  Newest first."
  (let* ((agent (and (stringp agent) (not (string-empty-p agent)) agent))
         (why (and (stringp why) (not (string-empty-p why)) (intern why)))
         (since (and (numberp since) since))
         (out nil) (shown 0) (total 0))
    (dolist (e ghostherd--log)           ; newest first
      (when (and (ghostherd-log-entry-screen e)
                 (or (not agent) (equal (ghostherd-log-entry-session e) agent))
                 (or (not why) (eq (ghostherd-log-entry-why e) why))
                 (or (not since)
                     (>= (float-time (ghostherd-log-entry-time e)) since)))
        (setq total (1+ total))
        (when (< shown ghostherd-screens-cli-cap)
          (setq shown (1+ shown))
          (let* ((raw (string-trim-right (ghostherd-log-entry-screen e)))
                 (screen (if (> (length raw) ghostherd-screens-cli-chars)
                             (concat (substring raw 0 ghostherd-screens-cli-chars)
                                     "\n  …(truncated; open it on the Log tab)")
                           raw)))
            (push (format "%s  %s  [%s]%s\n%s"
                          (format-time-string "%Y-%m-%d %H:%M:%S"
                                              (ghostherd-log-entry-time e))
                          (ghostherd-log-entry-session e)
                          (or (ghostherd-log-entry-why e) "screen")
                          (if (ghostherd-log-entry-note e)
                              (concat "  — " (ghostherd-log-entry-note e)) "")
                          (replace-regexp-in-string "^" "    " screen))
                  out)))))
    (if (zerop total)
        "no saved screens match"
      (concat (format "%d screen%s%s:\n\n" total (if (= total 1) "" "s")
                      (if (> total shown) (format " (newest %d shown)" shown) ""))
              (string-join (nreverse out) "\n\n")))))


;;; Auto-capturing a suspicious screen
;;
;; The blocked-transition capture keeps the screens we already recognise.
;; These keep the ones we do not: a quota message, a menu no rule knew, a
;; screen that disagrees with the agent's own hook, a /compact in flight --
;; the very screens a rule has to be built from, and the ones never saved
;; before because no rule fired on them.  Each is tagged with why it was
;; kept.  False positives are cheap -- the user deletes them from the Log
;; tab -- but bounded: an identical screen is not kept twice, and a trigger
;; fires for an agent at most once per `ghostherd-auto-capture-throttle'.

(defcustom ghostherd-auto-capture t
  "Whether to auto-capture a suspicious screen into the log.
Off leaves only the blocked-transition and manual captures."
  :type 'boolean
  :group 'ghostherd)

(defcustom ghostherd-auto-capture-throttle 1800
  "Seconds before the same trigger may capture the same agent again.
Thirty minutes by default: a suspicious screen tends to sit unchanged, so
one sample of it is the sample, and the next is worth keeping only once
the situation has had time to become a new one."
  :type 'number
  :group 'ghostherd)

(defcustom ghostherd-auto-capture-foot 8
  "Lines at the screen's foot an auto-capture limit-word search looks at,
when no input box is drawn there (see `ghostherd--screen-limit-p').
A fresh CLI message -- a quota notice, say -- lands here, in the box's
place; the words it uses are all over our own conversations higher up,
so only the foot is searched."
  :type 'integer
  :group 'ghostherd)

(defcustom ghostherd-auto-capture-limit-re
  (concat "\\b\\(?:quota\\|credits?\\|rate[ -]?limit\\(?:ed\\|s\\)?"
          "\\|usage limit\\|out of \\(?:credit\\|tokens?\\|quota\\|messages?\\)"
          "\\|limit \\(?:reached\\|exceeded\\|hit\\)\\|too many requests"
          "\\|insufficient\\|429\\|upgrade to\\)\\b")
  "What, near a screen's foot, reads as a limit or quota message.
Tuned the way `:screen-rules' are; the Log tab's `limit'-tagged captures
are the samples to tune it against."
  :type 'regexp
  :group 'ghostherd)

(defcustom ghostherd-auto-capture-limit-ignore-re "AI: Out of credits"
  "What a line's limit words are read without, or nil.
agy's footer says `· AI: Out of credits' of its paid Google AI credits
-- the overage past the model quota, which the usage reader ignores for
the same reason -- not of a quota: an agent with quota left shows it
all day, and was kept as a limit every half hour."
  :type '(choice (const :tag "None" nil) regexp)
  :group 'ghostherd)

(defcustom ghostherd-auto-capture-mismatch-grace 8
  "Seconds a screen/hook mismatch must hold before it is kept.

The noisy trigger: as a turn ends, the agent's last `working' hook has
not expired yet while the screen already reads idle -- a disagreement
that is the hook lagging, not the screen misreading, and it lasts an
instant.  A real misread -- a compaction, a first frame with no timer, an
agent streaming with its spinner scrolled out of the captured tail --
holds, so waiting this out keeps those and drops the turn-end flicker."
  :type 'number
  :group 'ghostherd)

(defvar ghostherd--auto-capture-at (make-hash-table :test 'equal)
  "\"ID\\0WHY\" -> (TIME . SCREEN-HASH) of the last auto-capture, for the
throttle and the identical-screen dedupe.")

(defvar ghostherd--mismatch-since (make-hash-table :test 'equal)
  "Session id → when its screen/hook mismatch began, while one holds.")

(defvar ghostherd--screen-reads nil
  "Screen -> what has been read off it, as ((KEY . ANSWER) ...); see
`ghostherd--screen-read'.  Weak on the screen: one no session shows any
more goes with its reads.")
;; Made anew on each load: a reload may change what a read means.
(setq ghostherd--screen-reads (make-hash-table :test 'eq :weakness 'key))

(defun ghostherd--screen-read (screen key fn &rest args)
  "FN applied to SCREEN and ARGS, read once per SCREEN and KEY.

Most screens do not change between two polls -- an agent idle, or
waiting on the team, draws the same thing for minutes -- yet each was
read for its state by dozens of regexps every poll, and twice: for the
state, then for whether to keep it.  So what is read off a screen is
kept with it, and the poll hands an unchanged screen on as the very
string it had before (`ghostherd--same-screen'), for its reads to be
found again.  KEY names the read and the settings it depends on; with
ARGS, compared by `equal'."
  (if (not (stringp screen))
      (apply fn screen args)
    (let* ((key (cons key args))
           (reads (gethash screen ghostherd--screen-reads))
           (kept (assoc key reads)))
      (if kept
          (cdr kept)
        (let ((answer (apply fn screen args)))
          (puthash screen (cons (cons key answer) reads) ghostherd--screen-reads)
          answer)))))

(defvar ghostherd-status-lines)

(defun ghostherd--read-rules (screen rules &optional replaces-input spinner)
  "`ghostherd--match-rules' of SCREEN, read once per screen."
  (ghostherd--screen-read screen (list 'rules ghostherd-status-lines)
                          #'ghostherd--match-rules rules replaces-input spinner))

(defvar ghostherd-reason-context)
(defvar ghostherd-reason-width)

(defun ghostherd--read-reason (screen hit)
  "`ghostherd--rule-reason' of SCREEN and HIT, read once per screen."
  (ghostherd--screen-read screen (list 'reason ghostherd-status-lines ghostherd-reason-context
                                       ghostherd-reason-width)
                          #'ghostherd--rule-reason hit))

(defun ghostherd--screen-foot (screen n)
  "The last N non-blank lines of SCREEN."
  (let ((lines (seq-remove #'string-blank-p
                           (split-string (or screen "") "\n"))))
    (last lines (max 1 n))))

(defun ghostherd--screen-limit-p (screen &optional scrolled)
  "Non-nil when a limit or quota message sits at SCREEN's foot.
Read where the CLI speaks, not where the agents do.  With an input box
drawn, the box and what is under it: a notice by it, as claude's
\"Usage limit reached\" -- above it is the conversation, where a reply
on rate limits, a test's 429, or a spinner's \"↓ 429 tokens\" are all
words, not a notice.  With none, a card or a notice took its place
(grok's \"You hit your free usage limit\"): the last
`ghostherd-auto-capture-foot' lines.  A view scrolled back -- SCROLLED
is the kind's `:scrolled' -- is not read at all: agy's /usage panel,
which the herd itself opens to read its quota, is all \"quota\".  Nor
what `ghostherd-auto-capture-limit-ignore-re' matches."
  (and (not (ghostherd--scrolled-back-p screen scrolled))
       (let* ((lines (vconcat (split-string (or screen "") "\n")))
              (top (ghostherd--box-top lines))
              (case-fold-search t))
         (seq-some (lambda (l)
                     (string-match-p ghostherd-auto-capture-limit-re
                                     (if ghostherd-auto-capture-limit-ignore-re
                                         (replace-regexp-in-string
                                          ghostherd-auto-capture-limit-ignore-re "" l t t)
                                       l)))
                   (if top
                       (seq-remove #'string-blank-p (append (seq-drop lines top) nil))
                     (ghostherd--screen-foot screen ghostherd-auto-capture-foot))))))

(defun ghostherd--screen-compacting-p (screen)
  "Non-nil when SCREEN shows a conversation compaction in progress."
  (and screen (string-match-p "[Cc]ompacting conversation" screen)))

(defun ghostherd--screen-menu-p (screen hit &optional replaces-input)
  "Non-nil when SCREEN looks like a menu that no `blocked' rule matched.
Read as `ghostherd--detect-state' reads a menu: for a kind whose menus
take its input box's place -- REPLACES-INPUT, its
`:prompt-replaces-input' -- only with no input box at the foot, for a
numbered list in a reply above an idle box is not a menu; and never
the model picker, which the herd drives itself."
  (and (not (eq (car-safe hit) 'blocked))
       ;; A cheap marker check before the full menu parse, since this runs
       ;; every poll: ❯/› marks, or the confirm/pick footers.
       (string-match-p "[❯›]\\|Enter to \\(?:confirm\\|select\\)\\|Tab/Arrow keys"
                       screen)
       (not (and replaces-input
                 (ghostherd--screen-read screen (list 'box ghostherd-status-lines)
                                         #'ghostherd--input-at-foot-p)))
       (not (ghostherd--model-picker screen))
       (ghostherd--prompt screen)))

(defun ghostherd--screen-hook-mismatch-p (hit report)
  "Non-nil when the hook REPORT says busy and the screen read HIT does not
-- the misdetection class (compaction, a first frame with no timer)."
  (and report
       (memq (car report) '(working blocked))
       (not (memq (car-safe hit) '(working blocked)))))

(defun ghostherd--auto-capture-why (screen hit report &optional spec)
  "The trigger SCREEN fires for an auto-capture, or nil.
SPEC is its agent's kind's, for how its CLI draws a menu and a view
scrolled back.  Checked in order, most actionable first, so one screen
keeps one entry."
  (cond
   ((ghostherd--screen-read screen (list 'limit ghostherd-auto-capture-limit-re ghostherd-auto-capture-foot
                                         ghostherd-auto-capture-limit-ignore-re)
                            #'ghostherd--screen-limit-p (plist-get spec :scrolled))
    'limit)
   ((ghostherd--screen-compacting-p screen) 'compaction)
   ((ghostherd--screen-read screen (list 'menu ghostherd-status-lines) #'ghostherd--screen-menu-p
                            hit (plist-get spec :prompt-replaces-input))
    'menu)
   ((ghostherd--screen-hook-mismatch-p hit report) 'mismatch)))

(defun ghostherd--auto-capture-ok (id why screen)
  "Non-nil when WHY may capture SCREEN for agent ID now.
Not within `ghostherd-auto-capture-throttle' of the last such capture, and
not an identical screen to it."
  (let ((last (gethash (concat id "\0" (symbol-name why))
                       ghostherd--auto-capture-at)))
    (cond
     ((null last) t)
     ((< (float-time (time-subtract (current-time) (car last)))
         ghostherd-auto-capture-throttle)
      nil)
     ((eql (cdr last) (sxhash-equal screen)) nil)
     (t t))))

(defun ghostherd--auto-capture (session)
  "Capture SESSION's screen if it looks suspicious, tagged with why.
Reads the screen the poll just stored, so it adds no capture of its own."
  (when (and ghostherd-auto-capture
             (> ghostherd-log-screens 0)
             (not (eq (ghostherd-session-kind session) 'shell)))
    (let* ((id (ghostherd-session-id session))
           (screen (gethash id ghostherd--screens)))
      (when (and screen (not (string-blank-p screen)))
        (let* ((spec (ignore-errors (ghostherd--spec (ghostherd-session-kind session))))
               (rules (plist-get spec :screen-rules))
               (hit (and rules (ghostherd--read-rules
                                screen rules
                                (plist-get spec :prompt-replaces-input)
                                (plist-get spec :spinner))))
               (report (ghostherd--fresh-report session))
               (why (ghostherd--auto-capture-why screen hit report spec)))
          ;; A mismatch must hold a moment before it counts: a hook lagging
          ;; a turn's end disagrees with a correctly-idle screen for an
          ;; instant, which is noise.  A real misread persists.
          (if (eq why 'mismatch)
              (let ((since (or (gethash id ghostherd--mismatch-since)
                               (puthash id (current-time) ghostherd--mismatch-since))))
                (when (< (float-time (time-subtract (current-time) since))
                         ghostherd-auto-capture-mismatch-grace)
                  (setq why nil)))
            (remhash id ghostherd--mismatch-since))
          (when (and why (ghostherd--auto-capture-ok id why screen))
            (ghostherd--log-add
             session 'screen
             (ghostherd--capture-text (ghostherd-session-name session) why nil)
             screen why nil nil)
            (puthash (concat id "\0" (symbol-name why))
                     (cons (current-time) (sxhash-equal screen))
                     ghostherd--auto-capture-at)))))))


;;; What the agent says about itself
;;
;; The third state authority, after screen rules and the ghostel/OSC
;; hooks.  Rules scrape a TUI that is redrawn differently every release,
;; which is why tuning them is a permanent job; but the CLIs already know
;; when they stop for a question and when they finish, and most of them
;; can run a command at that moment.  A report is that command arriving:
;;
;;   ghostherd report self blocked "Bash(rm -rf build/) — proceed?"
;;
;; It is deliberately *not* `ghostherd-mark-state'.  A manual mark is
;; sticky, which is right for a human overriding detection and wrong for
;; a hook: one missed edge and the row is frozen at `blocked' forever.  A
;; report expires instead, the way an OSC progress report does -- the
;; pattern this is modelled on, down to the freshness check.

(defcustom ghostherd-report-ttl 120
  "Seconds an agent's own report about itself stays trusted.

Short, because expiry is nearly free when the report was *true*: a real
prompt is still on the screen, the rules find it, and the herd falls
back to exactly the behaviour it had before reports existed.  Expiry is
also the only cure when the report was *stale* -- a hook chain that
broke halfway through a session, or a CLI that fires nothing on the way
out of a prompt -- so a long TTL buys a lie and sells nothing.

Long enough, meanwhile, that a prompt nobody answers for a minute is
still described by the agent's own words rather than by whichever
regexp happened to match the box it was drawn in."
  :type 'number
  :group 'ghostherd)

(defconst ghostherd-report-states '(working blocked waiting idle done)
  "States an agent may claim for itself.

`dead' is not among them: liveness is the host's answer, and an agent
that could still report is by definition not dead.

`waiting' is the sidecar's word, not a hook's: a turn that ended with
work the agent started still out -- sub-agents running, asks of its own
open.  Its box is free, but it is not done: it wakes as that work comes
back.  It holds until the agent says otherwise or the sidecar sees
nothing left out, which no screen can tell.")

(defvar ghostherd--reports (make-hash-table :test 'equal)
  "Session id → (STATE REASON TIME), the last thing an agent said about itself.

A hash table rather than two more `ghostherd-session' slots, for the
reason under \"Reloading over a live herd\" in readme.org: struct slots
cannot be added under a running herd without shifting every field after
them.  This is Emacs-side, transient and not worth that risk -- and it
is the same shape as `ghostherd--input-at' and `ghostherd--idle-since',
which are cleared in the same places.")

(defvar ghostherd--screens (make-hash-table :test 'equal)
  "Session id → last captured screen.

Filled by the poll path (and any other capture) so the overlay can
preview without a second round trip, and without a new struct slot.")

;;;###autoload
(defun ghostherd-report (session state &optional reason)
  "Record that SESSION says it is in STATE, because REASON.

STATE is one of `ghostherd-report-states', or `auto' to withdraw the
report and go back to detection alone.

This only records; the poll path decides what it means, the same
division of labour as `ghostherd--on-progress'.  The poll it schedules
is a timer rather than a direct call because the shortest way here is
`ghostel_cmd', which is dispatched inside ghostel's VT parser -- where a
capture is a subprocess in the middle of drawing a terminal."
  (interactive
   (list (ghostherd--read-session "Session: ")
         (intern (completing-read "Reports itself: "
                                  (cons "auto" (mapcar #'symbol-name
                                                       ghostherd-report-states))
                                  nil t))))
  (setq session (ghostherd-get session))
  (unless session
    (user-error "No such session"))
  (unless (or (eq state 'auto) (memq state ghostherd-report-states))
    (user-error "Cannot report state %s (one of %s, or auto)"
                state ghostherd-report-states))
  (let ((id (ghostherd-session-id session)))
    (if (eq state 'auto)
        (progn (remhash id ghostherd--reports)
               (ghostherd--log-add session 'report "withdrew its own report"))
      (puthash id (list state reason (current-time)) ghostherd--reports)
      (ghostherd--log-add session 'report
                          (format "reports %s%s" state
                                  (if reason (format "  %s" reason) "")))
      ;; Its prompt-submit hook: the text the herd sent it was taken.
      (when (eq state 'working)
        (ghostherd--submit-reported session)))
    (run-with-timer 0 nil #'ghostherd-poll-session id)
    state))

(defun ghostherd--report-seen (session)
  "SESSION's own report of `done', now seen: `idle' while it is trusted.
Else, looked away from within `ghostherd-report-ttl' of the report, the
agent was `done' again -- seen no more, so held there after the report
expired, and announced a second time."
  (let ((id (ghostherd-session-id session)))
    (when-let* ((entry (gethash id ghostherd--reports)))
      (puthash id (cons 'idle (cdr entry)) ghostherd--reports))))

(defun ghostherd--fresh-report (session)
  "Return SESSION's own last report as (STATE . REASON) while it is trusted.
`waiting' is trusted until replaced: work out an hour is still out, and
the screen would call it idle."
  (when-let* ((entry (gethash (ghostherd-session-id session) ghostherd--reports)))
    (pcase-let ((`(,state ,reason ,at) entry))
      (when (or (eq state 'waiting)
                (< (float-time (time-subtract (current-time) at))
                   ghostherd-report-ttl))
        (cons state (or reason (format "reported %s" state)))))))


;;; State detection

(defun ghostherd--rule-matches-p (pattern text)
  "Return non-nil when screen-rule PATTERN matches TEXT.

Case-sensitively, and that is the whole reason this exists rather than
a bare `string-match-p'.  Rules used to match under whatever
`case-fold-search' happened to be when the poll timer fired -- the
buffer that was current, in other words, which no rule author chooses
or can even see.  The same pattern could therefore mean two different
things on two ticks.

nil rather than t because the patterns were written by reading an
agent's screen and copying what was on it: case-sensitive *is* the
authored intent, and it matches `ghostherd-output-matches', which has
always bound it this way.  It is also the safer direction under the
project's standing policy of preferring a missed `blocked' to a false
one -- folding made `Running' match the word \"running\" in ordinary
agent prose, and `permission' match an agent merely talking about
permissions.

A rule that genuinely wants both cases says so: \\=[Pp]ermission."
  (let ((case-fold-search nil))
    (string-match-p pattern text)))

(defcustom ghostherd-status-lines 6
  "Lines at the bottom of a capture that count as the agent's status area.

`working' rules are matched against these lines alone.  Everything
above them is scrollback: these agents scroll rather than repaint, so
the tail holds every line the agent has ever printed, and an agent that
writes *about* a spinner is not a spinner.  That is how a claude
session sat at `working' while idle -- it had printed an explanation
containing the words `esc to interrupt'.

Six is the tallest status area observed, not a margin on it:

  spinner        · Crafting… (18s · ↓ 996 tokens)
  banner                        ✔ Update installed · Restart
  box            ─────────────────────────────────────────
  prompt         ❯
  box            ─────────────────────────────────────────
  footer           ⏵⏵ auto mode on … · esc to interrupt · ←

Both directions cost something, which is why this is a setting.  Too
many lines reads scrollback as status, the bug above.  Too few misses a
spinner drawn above a tall box -- once survivable, since every kind
also prints a hint in the footer, one line from the bottom.  Not
always: claude cuts it off a narrow footer, and grok drops its
`Ctrl+c:cancel' while it waits on a task.  A kind's `:spinner' finds
the spinner itself, above the box, however far up.

Anchoring the patterns cannot replace this: claude prints its hint
mid-line, in the middle of that footer."
  :type 'integer
  :group 'ghostherd)

(defun ghostherd--status-region (text)
  "The bottom of TEXT: where a TUI draws live status, not scrollback.
Trailing blank lines are dropped first -- a capture is padded to the
pane height, and the padding would otherwise be the whole region."
  (let* ((lines (split-string (or text "") "\n"))
         (lines (nreverse (seq-drop-while #'string-blank-p (nreverse lines)))))
    (string-join (last lines (max 1 ghostherd-status-lines)) "\n")))

(defconst ghostherd--input-rule-re "^[ \t]*[╭╰┌└├]?[─━═]\\{8,\\}"
  "A horizontal rule of the kind the CLIs draw around their input box.")

(defconst ghostherd--menu-choice-re
  "\\`\\(?:[[:blank:]]*[│┃][[:blank:]]*\\)?[❯›>][[:blank:]]*[1-9]\\."
  "A menu's first choice: the prompt sign, maybe just inside a frame, then
its number -- not the input box, though it opens with the same sign.
Only at the sign: text in the box may hold \"> 1.\" (ask 50461410).")

(defvar ghostherd--box-prompt-re)

(defun ghostherd--input-at-foot-p (text)
  "Non-nil when TEXT's status area holds the input box: a rule, then a
line opening with the prompt sign, or with it just inside a frame as
grok draws it -- not a menu's first choice, which carries the sign
before its number.  Only there: higher up, a rule and a sign are an old
prompt in the conversation."
  (let ((lines (split-string (ghostherd--status-region text) "\n")))
    (cl-loop for (above line) on lines
             thereis (and line
                          (string-match-p ghostherd--input-rule-re above)
                          (string-match-p ghostherd--box-prompt-re line)
                          (not (string-match-p ghostherd--menu-choice-re line))))))

;;; What a screen says besides the state
;;
;; Two things claude shows only on its screen: work it has running in
;; the background -- "1 shell" in its footer, there from the turn's end
;; on -- and the prompt it suggests you send next, drawn faint in its
;; empty input box.  The poll reads them from the screen it has.  A
;; suggestion is told from text you typed into the box by that
;; faintness, which the poll's capture does not keep: the host is asked
;; once for the screen with its styles, when new text is in the box.

(defcustom ghostherd-foot-kinds '(claude)
  "Kinds whose screen's foot is read for the work they have running in
the background and the prompt they suggest you send next."
  :type '(repeat symbol)
  :group 'ghostherd)

(defconst ghostherd--background-re
  "\\`[0-9]+ \\(?:shell\\|agent\\|monitor\\|task\\|workflow\\)s?\\'"
  "A count of work running in the background, as a footer says it.")

(defconst ghostherd--sgr-re "\e\\[[0-9;:]*m"
  "A style escape, as a styled capture holds them.")

(defconst ghostherd--faint-input-re
  (concat "\\`[^❯›>]*[❯›>]\\(?:" ghostherd--sgr-re "\\|[[:blank:]]\\)*"
          "\e\\[\\(?:[0-9;:]*;\\)?2\\(?:;[0-9;:]*\\)?m")
  "An input box's first line whose text starts faint.")

(defvar ghostherd--feet (make-hash-table :test 'equal)
  "Session id -> what its screen's foot says besides its state:
\(:background WORDS :input TEXT :suggestion TEXT).")

(defun ghostherd--foot-at (lines)
  "Where LINES, a screen's, draw an input box at their foot: (TOP . BOTTOM),
TOP the box's first line -- the prompt sign's -- and BOTTOM its lower
rule, a footer of a line or three under it.  Nil when there is none."
  (let* ((lines (vconcat lines))
         (n (length lines))
         (end (1- n)))
    (while (and (>= end 0) (string-blank-p (aref lines end)))
      (setq end (1- end)))
    (cl-loop for bottom from (1- end) downto (max 0 (- end 3))
             when (string-match-p ghostherd--input-rule-re (aref lines bottom))
             return (cl-loop for top from (1- bottom) downto (max 1 (- bottom 8))
                             when (string-match-p ghostherd--input-rule-re (aref lines (1- top)))
                             return (and (string-match-p "\\`[❯›>]\\(?:[[:blank:]]\\|\\'\\)" (aref lines top))
                                         (not (string-match-p "\\`[❯›>][[:blank:]]*[1-9]\\." (aref lines top)))
                                         (cons top bottom))))))

(defun ghostherd--join-wrapped (a b)
  "A then B, a line wrapped between them: a space between words, none
between ideographs."
  (if (or (string-empty-p a) (string-empty-p b)
          (>= (aref a (1- (length a))) #x2e80) (>= (aref b 0) #x2e80))
      (concat a b)
    (concat a " " b)))

(defun ghostherd--foot (text)
  "What TEXT's foot says besides the state: (:input TEXT :background
WORDS) -- what its input box holds, nil when empty; the background work
its footer counts, as (\"1 shell\").  Nil when no input box is drawn there."
  (let* ((lines (vconcat (split-string (or text "") "\n")))
         (at (ghostherd--foot-at lines)))
    (when at
      (let* ((box (mapcar #'string-trim
                          (cons (replace-regexp-in-string "\\`[❯›>][[:blank:]]*" "" (aref lines (car at)))
                                (append (seq-subseq lines (1+ (car at)) (cdr at)) nil))))
             (input (string-trim (seq-reduce #'ghostherd--join-wrapped (cdr box) (car box))))
             (footer (append (seq-subseq lines (1+ (cdr at))) nil)))
        (list :input (and (not (string-empty-p input)) input)
              :background (seq-filter (lambda (part) (string-match-p ghostherd--background-re part))
                                      (mapcan (lambda (line) (mapcar #'string-trim (split-string line "·")))
                                              footer)))))))

(defun ghostherd--faint-input-p (styled)
  "Non-nil when STYLED, a screen with its styles as escapes, starts the
text of the input box at its foot faint: a suggestion, not typed."
  (let* ((lines (split-string (or styled "") "\n"))
         (at (ghostherd--foot-at
              (mapcar (lambda (l) (replace-regexp-in-string ghostherd--sgr-re "" l)) lines))))
    (and at (string-match-p ghostherd--faint-input-re (nth (car at) lines)) t)))

(defun ghostherd--read-foot (session text)
  "Keep what SESSION's screen TEXT says at its foot besides the state.
New text in its input box: the host is asked whether it is faint."
  (let ((id (ghostherd-session-id session)))
    (if (not (memq (ghostherd-session-kind session) ghostherd-foot-kinds))
        (remhash id ghostherd--feet)
      (let* ((foot (ghostherd--screen-read text 'foot #'ghostherd--foot))
             (input (plist-get foot :input))
             (had (gethash id ghostherd--feet)))
        (puthash id (list :background (plist-get foot :background)
                          :input input
                          :suggestion (and input (equal input (plist-get had :input))
                                           (plist-get had :suggestion)))
                 ghostherd--feet)
        (when (and input (not (equal input (plist-get had :input))))
          (ignore-errors
            (ghostherd-backend-styled-screen
             (ghostherd--backend-of session) session
             (lambda (styled)
               (let ((now (gethash id ghostherd--feet)))
                 (when (and now (equal (plist-get now :input) input)
                            (ghostherd--faint-input-p styled))
                   (puthash id (plist-put (copy-sequence now) :suggestion input)
                            ghostherd--feet)))))))))))

(defun ghostherd-session-background (session)
  "The work SESSION has running in the background, as its screen counts
it: (\"1 shell\"), or nil."
  (plist-get (gethash (ghostherd-session-id session) ghostherd--feet) :background))

(defun ghostherd-session-suggestion (session)
  "The prompt SESSION suggests you send next, or nil."
  (plist-get (gethash (ghostherd-session-id session) ghostherd--feet) :suggestion))

(defun ghostherd--match-rules (text rules &optional replaces-input spinner)
  "Return (STATE . REASON) for first matching rule in RULES against TEXT.
RULES is an alist of (STATE . REGEXP-LIST).  Prefer blocked over
working over idle (strict blocked detection, herdr-style).

`working' is asked of the status area only; see
`ghostherd-status-lines'.  `blocked' and `idle' still read the whole
tail: a permission box can be tall, and `idle' is the fallback rather
than positive evidence.  REPLACES-INPUT, a kind's
`:prompt-replaces-input': `blocked' only while no input box is drawn at
the foot -- with one there, what matched is the conversation's.
SPINNER, a kind's `:spinner': `working' too when it matches the line
above the box, wherever that sits."
  (cl-labels ((try (state haystack)
                (when-let* ((patterns (alist-get state rules)))
                  (cl-loop for pat in patterns
                           when (ghostherd--rule-matches-p pat haystack)
                           return (cons state pat)))))
    (or (and (not (and replaces-input (ghostherd--input-at-foot-p text)))
             (try 'blocked text))
        (try 'working (ghostherd--status-region text))
        (and (ghostherd--spinning-p text spinner) (cons 'working spinner))
        (try 'idle text))))

(defconst ghostherd--spinner-reach 30
  "Lines above its input box a spinner is looked for in, at most: claude
lists its todos under its spinner, grok its tasks.")

(defconst ghostherd--box-prompt-re
  "\\`\\(?:[[:blank:]]*[│┃][[:blank:]]*\\)?[❯›>]\\(?:[[:blank:]]\\|\\'\\)"
  "An input box's first line: the prompt sign at its start, as claude draws
it, or just inside a frame, as grok does.")

(defun ghostherd--box-top (lines)
  "Index in LINES of the top edge of the input box at their foot, or nil:
a rule, and under it the prompt sign -- not a menu's first choice, which
carries the sign before its number.  Looked for among the last 14 lines
that are not padding, room for a box of several lines and a footer."
  (let ((end (1- (length lines))))
    (while (and (>= end 0) (string-blank-p (aref lines end)))
      (setq end (1- end)))
    (cl-loop for i downfrom end to (max 1 (- end 14))
             for line = (aref lines i)
             when (and (string-match-p ghostherd--box-prompt-re line)
                       (not (string-match-p ghostherd--menu-choice-re line))
                       (string-match-p ghostherd--input-rule-re (aref lines (1- i))))
             return (1- i))))

(defun ghostherd--off-prompt-p (session)
  "Non-nil when SESSION's CLI draws no input box at the foot of the
screen the poll last took: a dialog, a menu, a panel, a screen not yet
drawn.  A paste goes into none of them, and the Return after it answers
whatever is there -- each CLI's trust prompt, drawn from the top of a
tall pane, reads idle in the tail the rules see (ask cf9873be).  Only
of a CLI the herd reads by rules, and a screen it has.  A tail with no
box is asked again of the whole pane: one drawn from its top has blank
rows under its box."
  (when-let* ((spec (ignore-errors (ghostherd--spec (ghostherd-session-kind session))))
              ((plist-get spec :screen-rules))
              (screen (gethash (ghostherd-session-id session) ghostherd--screens))
              ((not (ghostherd--box-top (vconcat (split-string screen "\n"))))))
    (let ((pane (ghostherd--pane session screen)))
      (not (and pane (ghostherd--box-top (vconcat (split-string pane "\n"))))))))

(defun ghostherd--pane (session tail)
  "SESSION's whole visible screen, TAIL its foot: the pane the batched
fetch kept with TAIL (`ghostherd--panes'), else read now -- on ghostel,
its terminal's rows, not the scrollback over them."
  (let ((kept (gethash (ghostherd-session-id session) ghostherd--panes)))
    (if (and kept (equal (car kept) tail))
        (cdr kept)
      (ignore-errors (ghostherd-backend-pane (ghostherd--backend-of session) session)))))

(defun ghostherd--whole-pane (session tail)
  "SESSION's whole visible pane, its blank rows dropped, when it shows
more than TAIL does; else nil.  For a dialog drawn from the top of a
pane taller than the tail: the rows under it are padding."
  (let* ((rows (lambda (s) (seq-remove #'string-blank-p (split-string (or s "") "\n"))))
         (all (funcall rows (ghostherd--pane session tail))))
    (when (> (length all) (length (funcall rows tail)))
      (string-join all "\n"))))

(defun ghostherd--spinner-line (text spinner)
  "The line above TEXT's input box that SPINNER, a kind's `:spinner',
matches; nil when none does, or no box is drawn at the foot.

Read upward from the box.  What sits under a spinner is indented --
claude's tip, its todo list, the right-aligned update banner; grok's
task list -- so the search ends at the first line that is not, and
`ghostherd--spinner-reach' lines up at most.  Idle, that line is the
first of what the agent said last, and its words are not evidence: a
spinner's shape is drawn by no sentence."
  (let* ((lines (vconcat (split-string (or text "") "\n")))
         (top (ghostherd--box-top lines)))
    (when (and top spinner)
      (catch 'found
        (let ((i (1- top)))
          (while (>= i (max 0 (- top ghostherd--spinner-reach)))
            (let ((line (aref lines i)))
              (unless (string-blank-p line)
                (when (ghostherd--rule-matches-p spinner line)
                  (throw 'found line))
                (unless (string-match-p "\\`[[:blank:]]" line)
                  (throw 'found nil))))
            (setq i (1- i))))))))

(defun ghostherd--spinning-p (text spinner)
  "Non-nil when SPINNER, a kind's `:spinner', matches the line TEXT draws
its spinner on."
  (and (ghostherd--spinner-line text spinner) t))

(defconst ghostherd--scrolled-reach 4
  "Lines above the input box the scrolled-back marker is looked for in:
the view's last line, under which only a banner may sit.")

(defun ghostherd--scrolled-back-p (text scrolled)
  "Non-nil when TEXT is a view scrolled back, by SCROLLED, a kind's
`:scrolled': it matches one of the lines just above the input box --
the foot of the view, where the CLI draws its way back to the bottom --
or, with no box drawn, one of the last lines.  Not higher up, where it
would be the conversation quoting it."
  (when scrolled
    (let* ((lines (vconcat (split-string (or text "") "\n")))
           (top (ghostherd--box-top lines))
           (end (or top
                    (let ((i (length lines)))
                      (while (and (> i 0) (string-blank-p (aref lines (1- i))))
                        (setq i (1- i)))
                      i))))
      (cl-loop for i from (max 0 (- end (if top ghostherd--scrolled-reach 8))) below end
               thereis (ghostherd--rule-matches-p scrolled (aref lines i))))))

(defun ghostherd--progress-fresh-p (session)
  "Return non-nil if SESSION reported OSC progress recently enough to trust.
`remove' and `pause' are excluded: they say the agent stopped reporting,
which is exactly when the screen rules should take back over."
  (and ghostherd-use-osc-progress
       (memq (ghostherd-session-progress-state session) '(set indeterminate))
       (when-let* ((at (ghostherd-session-progress-at session)))
         (< (float-time (time-subtract (current-time) at))
            ghostherd-progress-ttl))))

(defun ghostherd--progress-reason (session)
  "Describe SESSION's last progress report, for the state reason."
  (let ((percent (ghostherd-session-progress-percent session)))
    (if percent
        (format "osc progress %d%%" percent)
      "osc progress")))

(defun ghostherd--match-all-rules (text rules &optional replaces-input spinner)
  "Return every (STATE . PATTERN) in RULES matching TEXT.
`ghostherd--match-rules' stops at the first winner, which is what the
state machine wants but hides why a rule lost.  This reports the lot, in
precedence order, for `ghostherd-explain' -- against the same regions
the state machine used, or the explanation would not explain it; and so
no `blocked' where REPLACES-INPUT and the input box is at the foot, and
SPINNER among the `working' ones when its line matches."
  (let ((status (ghostherd--status-region text))
        (box (and replaces-input (ghostherd--input-at-foot-p text))))
    (cl-loop for state in (if box '(working idle) '(blocked working idle))
             append (cl-loop for pattern in (alist-get state rules)
                             when (ghostherd--rule-matches-p
                                   pattern
                                   (if (eq state 'working) status text))
                             collect (cons state pattern))
             when (and (eq state 'working) (ghostherd--spinning-p text spinner))
             collect (cons 'working spinner))))

(defcustom ghostherd-reason-width 90
  "Characters of screen text kept as a session's state reason."
  :type 'integer
  :group 'ghostherd)

(defconst ghostherd--screen-furniture
  "[│─╭╮╰╯┌┐└┘├┤┬┴┼┃━┏┓┗┛▌▐█▏▕]"
  "Box-drawing and gutter characters agent TUIs pad their frames with.")

(defun ghostherd--clean-screen-line (line)
  "Return LINE as one readable sentence, or nil if nothing is left.

A captured line arrives as the agent drew it: inside a box, padded to
the pane width, often behind a prompt glyph.  None of that survives a
trip through a desktop notification, so strip it here rather than at
each of the four places the reason is displayed."
  (let* ((s (replace-regexp-in-string ghostherd--screen-furniture " " line))
         (s (replace-regexp-in-string "\\`[ \t❯›»▸▪●○*•>-]+" "" s))
         (s (replace-regexp-in-string "[ \t]+" " " s))
         (s (string-trim s)))
    (unless (string-empty-p s)
      (truncate-string-to-width s ghostherd-reason-width nil nil t))))

(defcustom ghostherd-reason-context 3
  "Lines to look back for the subject of a prompt the rules matched.

The matched line is usually the *generic* half.  A claude permission
prompt reads

  Bash(rm -rf build/ && npm publish)

  Do you want to proceed?

and the pattern matches the second one, so \"needs attention (Do you
want to proceed?)\" says no more than \"needs attention\" did.  The line
that tells you what you are approving is above it, past however many
blank or box-only rows the agent drew.  Set to 0 to use the matched line
alone."
  :type 'integer
  :group 'ghostherd)

(defun ghostherd--saying-something-p (line)
  "Return non-nil when LINE carries words rather than decoration.

Agent CLIs draw logos and spinners out of block glyphs, and the first
run of the herd log caught the subject lookup picking one up: a state
reason came out as a row of half-blocks followed by the command that had
actually been typed.  Enumerating the glyphs is a losing game -- there
are hundreds and every CLI picks different ones -- so require the line
to contain some letters or digits instead."
  (>= (cl-count-if (lambda (c) (or (<= ?a c ?z) (<= ?A c ?Z) (<= ?0 c ?9)))
                   line)
      3))

(defun ghostherd--screen-lines (text pos)
  "Return the line of TEXT containing POS, and the lines before it."
  (let* ((start (1+ (or (cl-position ?\n text :end pos :from-end t) -1)))
         (end (or (cl-position ?\n text :start pos) (length text))))
    (cons (substring text start end)
          (nreverse (split-string (substring text 0 (max 0 (1- start))) "\n")))))

(defun ghostherd--matched-line (text pattern)
  "Return the cleaned prompt of TEXT that PATTERN matched, or nil.

The reason field used to hold the pattern itself, which is a debugging
artifact: it leaked into the notification, the switcher annotation and
the JSON listing, so \"agy needs attention\" was followed by a regexp
rather than by the question the agent had asked.

Returns the matched line, and where one is found within
`ghostherd-reason-context' rows above it, the nearest line that survives
cleaning -- joined subject-first, so truncation eats the generic half
rather than the specific one.

This is a display heuristic and is allowed to be one: at worst a
notification gains an irrelevant line.  It cannot affect *state*, which
is decided by the pattern alone."
  (let ((case-fold-search nil))
    (when-let* ((pos (string-match pattern text)))
      (pcase-let* ((`(,line . ,before) (ghostherd--screen-lines text pos))
                   (matched (ghostherd--clean-screen-line line)))
        ;; Context only augments a line that said something itself.  An
        ;; anchored prompt like "^> " cleans away to nothing, and dressing
        ;; it with whatever preceded it would attribute an unrelated line
        ;; to the reason.
        (when matched
          (let ((subject (cl-loop for candidate in (seq-take
                                                    before
                                                    ghostherd-reason-context)
                                  for clean = (ghostherd--clean-screen-line
                                               candidate)
                                  when (and clean
                                            (not (equal clean matched))
                                            (ghostherd--saying-something-p clean))
                                  return clean)))
            (if subject
                (ghostherd--clean-screen-line (concat subject " — " matched))
              matched)))))))

(defun ghostherd--rule-reason (text hit)
  "Return the human-readable reason for rule HIT against TEXT.
Falls back to the pattern when the matched line cleans up to nothing --
an anchored prompt like \"^> \" is all furniture.

Reads the same region the rule was matched against, so the reason
quotes the line that decided the state rather than an earlier
coincidence further up the scrollback."
  (let ((haystack (if (eq (car-safe hit) 'working)
                      (ghostherd--status-region text)
                    text)))
    (or (ghostherd--matched-line haystack (cdr hit))
        ;; A spinner, matched on its own line above the box.
        (and (eq (car-safe hit) 'working)
             (when-let* ((line (ghostherd--spinner-line text (cdr hit))))
               (ghostherd--matched-line line (cdr hit))))
        (cdr hit))))

;;; A menu prompt, read off the screen
;;
;; A blocked agent is mostly sitting on a menu: what it asks, then its
;; choices, numbered -- "> 1. Yes, run command", "❯ 2. Not yet".  The
;; rule that found it quotes one line; you answer by number, so the
;; numbers need their words.

(defconst ghostherd--prompt-option-re
  "^[[:blank:]│]*\\(?:\\(?3:[❯>›▶]\\)[[:blank:]]*\\)?\\(?1:[1-9]\\)\\.[[:blank:]]+\\(?2:.*[^[:blank:]│]\\)[[:blank:]│]*$"
  "A menu's option: its number, its first line, the highlight's mark.")

(defconst ghostherd--prompt-rule-re
  "\\`[[:blank:]]*[─━═╭╮╰╯┌┐└┘├┤▔▁▀▄-]\\{3,\\}[[:blank:]]*\\'"
  "A line that only draws: the edge of what a menu asks.  Includes the
block-rule (▔) claude draws the top of its /login box with, so the menu's
question stops there and does not swallow the turn above it.")

(defconst ghostherd--prompt-near 8
  "Lines from the screen's foot within which a menu's last option sits.
Further up, numbered lines are the conversation's, not a menu.")

(defconst ghostherd--prompt-foot-re "Enter to select\\|to navigate"
  "An AskUserQuestion's footer line.  Its choices are the numbered run just
above it, however far a preview box and its notes have pushed them up --
unlike a permission prompt, whose choices sit near the screen's foot, so
only this footer lets the choices be read from that far up.")

(defconst ghostherd--prompt-foot-patterns '("Enter to select" "Tab/Arrow keys to navigate")
  "The blocked rules that are an AskUserQuestion's footer, not its words.
When one of these found the menu, the reason is the question it asks, not
the footer or the `Chat about this' line above it.")

(defconst ghostherd--prompt-header-re "\\`[☐☑☒□▢▣◻◼]"
  "A line opening with a checkbox: an AskUserQuestion's header chip, above
the question and not part of it.")

(defconst ghostherd--prompt-corner-re "[┌└╭╰]"
  "A box's top- or bottom-left corner.")

(defconst ghostherd--prompt-tag-re "\\`[[:blank:]│]*([^()]*)[[:blank:]│]*\\'"
  "A line that is only a parenthetical tag, like `(Recommended)'.  An
AskUserQuestion wraps it under its choice when a preview box leaves no
room beside it, so it belongs to that choice's label.")

(defconst ghostherd--prompt-confirm-re "Enter to confirm"
  "Footer of a marker-highlighted confirm menu -- claude's trust-folder
prompt (\"Enter to confirm · Esc to cancel\").  Its choices carry no
numbers, only a [❯] on the highlighted one, so the numbered reader does
not see them; this footer anchors `ghostherd--prompt-marker' instead.")

(defconst ghostherd--prompt-mark-re
  "\\`[[:blank:]│]*[❯›▶][[:blank:]]+[^[:blank:]]"
  "A marker-menu's highlighted option: leading blanks, a mark, then text.
Requiring the text keeps a bare `❯ ' (an empty input box) out; leaving `>'
out keeps the box's own sign, which is `>' or `❯', from being a choice --
a trust menu marks with `❯' and always has words after it.")

(defconst ghostherd--prompt-mark-prefix-re
  "\\`[[:blank:]│]*[❯›▶][[:blank:]]+"
  "The lead of a marked option up to its text: blanks, the mark, blanks.")

(defun ghostherd--prompt-at-col-p (line col chars)
  "Non-nil if LINE's character at display column COL is one of CHARS.
Counts display width, so a box border lands on the same column whatever
mix of single- and double-width characters comes before it."
  (let ((i 0) (w 0) (len (length line)))
    (while (and (< i len) (< w col))
      (setq w (+ w (char-width (aref line i))) i (1+ i)))
    (and (= w col) (< i len) (memq (aref line i) chars))))

(defun ghostherd--prompt-strip-preview (lines)
  "Return LINES with any right-hand preview box cut away.
AskUserQuestion draws a preview box to the right of its choices, its
border on the same rows; left in, its │…│ bleeds into the choices and the
question.  A box whose left corner has real text before it is such a
preview (a permission prompt's box sits at the left edge, nothing before
it), so find that corner's column and drop each row from the box's left
edge rightward -- only rows whose character there draws the box's edge, so
the full-width question and the rule lines are left whole."
  (let ((col nil)
        (edge '(?│ ?┌ ?└ ?╭ ?╰)))
    (dolist (line lines)
      (when (string-match ghostherd--prompt-corner-re line)
        (let ((before (substring line 0 (match-beginning 0))))
          (when (string-match-p "[^[:blank:]│┌┐└┘╭╮╰╯─]" before)
            (let ((c (string-width before)))
              (setq col (if col (min col c) c)))))))
    (if (not col)
        lines
      (mapcar (lambda (line)
                (if (ghostherd--prompt-at-col-p line col edge)
                    (truncate-string-to-width line col)
                  line))
              lines))))

(defun ghostherd--prompt-label (lines i)
  "Option I of LINES, joined to the lines its CLI wrapped it onto.
A wrapped line starts left of the option's words, under a long line;
a choice's description is indented with its words, and a hint under a
short last choice is neither."
  (string-match ghostherd--prompt-option-re (aref lines i))
  (let ((label (match-string 2 (aref lines i)))
        (column (match-beginning 2))
        (prev (aref lines i)))
    (cl-loop for j from (1+ i) below (length lines)
             for line = (aref lines j)
             while (and (not (string-match-p ghostherd--prompt-option-re line))
                        (not (string-match-p ghostherd--prompt-rule-re line))
                        ;; A parenthetical tag the CLI wrapped onto its own
                        ;; line belongs to the label however short the choice
                        ;; is; otherwise only the continuation of a line long
                        ;; enough to have wrapped, left of the words.
                        (or (string-match-p ghostherd--prompt-tag-re line)
                            (and (> (length (string-trim-right prev "[[:blank:]│]+")) 50)
                                 (string-match "\\`\\([[:blank:]]*\\)[^[:blank:]]" line)
                                 (< (length (match-string 1 line)) column))))
             do (setq label (concat label " " (string-trim line)) prev line))
    (truncate-string-to-width label 100 nil nil "…")))

(defun ghostherd--prompt (screen)
  "The menu SCREEN ends on, as (:question TEXT :options ((N . LABEL) ...)
:on N), or nil.  A numbered menu first -- an AskUserQuestion or a
permission prompt -- then, failing that, a marker-highlighted confirm
menu with no numbers, like claude's trust-folder prompt."
  (let ((lines (vconcat (ghostherd--prompt-strip-preview
                         (split-string (string-trim-right (or screen "")) "\n")))))
    (or (ghostherd--prompt-numbered lines)
        (ghostherd--prompt-marker lines))))

(defun ghostherd--prompt-numbered (lines)
  "The numbered menu LINES end on, as `ghostherd--prompt' returns, or nil.
The options are the last run numbered from 1, each a few lines from the
next, the last near the foot; the question the lines above the first, up
to a line that only draws, six at most; on, the option marked
highlighted, nil when none is."
  (let* ((n (length lines))
         (number (lambda (i) (and (string-match ghostherd--prompt-option-re (aref lines i))
                                  (string-to-number (match-string 1 (aref lines i))))))
         ;; An AskUserQuestion's footer anchors the run: its choices are just
         ;; above it, however far the preview box and notes reach down.  With
         ;; no such footer, keep to the old window near the screen's foot, so
         ;; the conversation's own numbered lines are not mistaken for a menu.
         (foot (cl-loop for i downfrom (1- n) to 0
                        when (string-match-p ghostherd--prompt-foot-re (aref lines i))
                        return i))
         (from (or foot (1- n)))
         (stop (if foot 0 (max 0 (- n ghostherd--prompt-near))))
         (last (cl-loop for i downfrom from to stop
                        when (funcall number i) return i)))
    (when last
      (let ((at (list last)))
        ;; Back up the run: each option a few lines above the next.
        (while (and (car at) (> (funcall number (car at)) 1))
          (let ((want (1- (funcall number (car at)))))
            (push (cl-loop for j downfrom (1- (car at)) to (max 0 (- (car at) 5))
                           when (eql (funcall number j) want) return j)
                  at)))
        ;; Ended on 1, or on nothing: a gap too wide, a number missing.
        (when (and (car at) (cdr at))
          (let (question)
            (cl-loop for j downfrom (1- (car at)) to 0
                     for line = (string-trim (replace-regexp-in-string "\\`[[:blank:]]*│\\|│[[:blank:]]*\\'" "" (aref lines j)))
                     until (or (string-match-p ghostherd--prompt-rule-re line)
                               (string-match-p ghostherd--prompt-header-re line)
                               (>= (length question) 6))
                     unless (string-empty-p line) do (push line question))
            (list :question (string-join question "\n")
                  :options (mapcar (lambda (i) (cons (funcall number i) (ghostherd--prompt-label lines i)))
                                   at)
                  :on (cl-loop for i in at
                               when (and (string-match ghostherd--prompt-option-re (aref lines i))
                                         (match-beginning 3))
                               return (funcall number i)))))))))

(defun ghostherd--prompt-text-col (line)
  "The column LINE's words begin at, past a menu mark where it has one.
A marked option (\"❯ Yes\") and its plain siblings (\"  No\") share this
column, so it tells one menu row from the question or a drawing above."
  (if (string-match ghostherd--prompt-mark-prefix-re line)
      (match-end 0)
    (and (string-match "[^[:blank:]│]" line) (match-beginning 0))))

(defun ghostherd--prompt-marker (lines)
  "The marker-highlighted confirm menu LINES end on, as `ghostherd--prompt'
returns, or nil -- claude's trust-folder prompt, whose choices carry no
numbers.  Anchored on the \"Enter to confirm\" footer and a [❯]-marked
choice just above it, so an idle input box's own sign or the conversation
is never read as a menu.  The choices are the run of lines sharing the
marked one's text column; numbered from the top; :on the marked one.
:question is left empty: the blocked rule that found it already names the
prompt, and the card shows that reason."
  (let* ((n (length lines))
         (foot (cl-loop for i downfrom (1- n) to 0
                        when (string-match-p ghostherd--prompt-confirm-re (aref lines i))
                        return i))
         (mark (and foot
                    (cl-loop for i downfrom (1- foot) to (max 0 (- foot ghostherd--prompt-near))
                             when (string-match-p ghostherd--prompt-mark-re (aref lines i))
                             return i))))
    (when mark
      (let* ((col (ghostherd--prompt-text-col (aref lines mark)))
             (optp (lambda (i)
                     (and (>= i 0) (< i foot)
                          (let ((l (aref lines i)))
                            (and (not (string-blank-p l))
                                 (not (string-match-p ghostherd--prompt-rule-re l))
                                 (not (string-match-p ghostherd--prompt-header-re l))
                                 (eql (ghostherd--prompt-text-col l) col))))))
             (top mark) (bot mark))
        (while (funcall optp (1- top)) (setq top (1- top)))
        (while (funcall optp (1+ bot)) (setq bot (1+ bot)))
        ;; A real menu has at least two choices; one marked line alone is
        ;; some other `❯' (a lone bullet, a box) that happened to sit here.
        (when (> bot top)
          (let ((at (number-sequence top bot)))
            (list :question ""
                  :options (cl-loop for i in at for k from 1
                                    collect (cons k (ghostherd--prompt-marker-label
                                                     (aref lines i) col)))
                  :on (cl-loop for i in at for k from 1
                               when (string-match-p ghostherd--prompt-mark-re (aref lines i))
                               return k))))))))

(defun ghostherd--prompt-marker-label (line col)
  "The words of marker-menu option LINE, from its text column COL, trimmed."
  (string-trim (string-trim-right (substring line (min col (length line)))
                                  "[[:blank:]│]+")))

(defun ghostherd--prompt-reason (screen)
  "The question the AskUserQuestion on SCREEN asks, as one line for the
reason field, or nil when none reads.  Its footer (\"Enter to select …\")
and the \"Chat about this\" line above it are furniture; the card should
say what it actually asks."
  (when-let* ((prompt (ghostherd--prompt screen))
              (q (plist-get prompt :question)))
    (unless (string-empty-p q)
      (string-join (split-string q "\n" t) " "))))

(defun ghostherd-session-prompt (session)
  "What blocked SESSION's menu asks and offers, read off its last screen;
nil when it is not blocked, or sits on no menu with numbers."
  (when (eq (ghostherd-session-state session) 'blocked)
    (ghostherd--prompt (gethash (ghostherd-session-id session) ghostherd--screens))))

(defcustom ghostherd-cli-errors
  '((claude
     ("\\`\\(?:Login expired\\|Not logged in\\)" login "its login expired -- run /login")
     ("\\`API Error: Your computer went to sleep mid-response" continue
      "its turn was cut off: the computer slept" slept)
     ("\\`API Error: Connection lost mid-response" continue
      "its turn was cut off: the connection dropped" dropped)
     ("\\`API Error:" nil "its turn ended on an API error")))
  "Per-kind errors that end a turn without answering: (KIND (REGEXP ACTION
REASON [CAUSE])...).  REGEXP is matched against the error's own words, from
their start -- never against the conversation, where an agent may
quote or mention the same words (the herd once blocked an agent for
writing \"API Error:\" and \"Login expired\" in a reply).  Those words
come from the CLI's own record of the error where its transcript keeps
one (claude: see `ghostherd--claude-turn-end'), and only failing that
from the screen: the last message above the input box, when it is the
whole of what claude drew (`ghostherd--last-message').

A match means the agent's turn died and it is stuck -- not idle --
until acted on; it is read as blocked, so an ask it was answering is
held, not closed, and REASON says why.  An error the transcript marks
that no rule names still blocks, with its own words as the reason.

ACTION `login' offers a one-click /login on the card.  ACTION
`continue' lets the herd carry the turn on by itself after
`ghostherd-sleep-continue-delay', at most `ghostherd-sleep-continue-max'
times per stuck turn; CAUSE, a key of `ghostherd--continue-causes'
(`slept' when left out), picks the prompt it sends and the card's
words.  The errors safe to auto-continue are exactly those marked
`continue' here, ahead of the bare API error: the Mac sleeping and the
connection dropping mid-response, both a turn cut short with nothing
wrong in it.  Login expired and every other error stay blocked for you."
  :type '(alist :key-type symbol
                :value-type (repeat (choice (list regexp (choice (const nil) symbol) string)
                                            (list regexp (choice (const nil) symbol) string
                                                  symbol))))
  :group 'ghostherd)

;; What tells the CLI's own error from an agent's words about one.  The
;; screen cannot, by its words: an agent quotes "API Error: Connection
;; lost mid-response" in a reply, or prints "Login expired" in a test it
;; runs, and the same characters sit at the screen's foot.  claude's
;; transcript can: it records the error as an assistant entry of its own
;; -- `isApiErrorMessage' true, model "<synthetic>", an `error' code such
;; as `server_error' or `authentication_failed' -- while a reply quoting
;; it is the model's message with no such mark.  So the last entry of the
;; conversation decides: claude's error, the turn ended on it; anything
;; else, it did not, whatever the screen says.  Bookkeeping written after
;; the error (turn_duration, last-prompt, a /login's local_command) is
;; not the conversation; a prompt typed since is, and ends it.

(defconst ghostherd--transcript-tail-max (* 4 1024 1024)
  "Most bytes read back from a transcript's end to find its last turn.")

(defvar ghostherd--turn-ends (make-hash-table :test 'equal)
  "Transcript path -> (SIZE MTIME . VERDICT): what `ghostherd--claude-turn-end'
read there while the file had that size and mtime, so the poll reads an
unchanged transcript once, not every second.")

(defconst ghostherd--turn-ends-max 500
  "A last-guard ceiling on `ghostherd--turn-ends'.  The real bound is
`ghostherd--prune-turn-ends' dropping the transcripts no live agent
follows; this only trips if the followed set itself grows absurd, and
clears the cache -- each live agent re-reads its own on the next poll.")

(defun ghostherd--prune-turn-ends (&rest _)
  "Drop `ghostherd--turn-ends' entries for transcripts no live agent follows
any more -- a removed agent's, or an old conversation a live one has since
left.  A live agent's current transcript is kept, so nothing it needs is
dropped.  Cheap: runs when an agent leaves and every so many polls.  Takes
and ignores args, so it also serves as a `ghostherd-session-removed-hook'."
  (let ((keep (make-hash-table :test 'equal)))
    (dolist (s (ghostherd-sessions))
      (when-let* ((p (ignore-errors (ghostherd-session-transcript s))))
        (puthash p t keep)))
    (maphash (lambda (path _v)
               (unless (gethash path keep)
                 (remhash path ghostherd--turn-ends)))
             ghostherd--turn-ends)
    ;; A pathological herd (more followed transcripts than the ceiling):
    ;; clear and let the live agents re-read theirs -- never wedged, just
    ;; the one slower poll.
    (when (> (hash-table-count ghostherd--turn-ends) ghostherd--turn-ends-max)
      (clrhash ghostherd--turn-ends))))

(add-hook 'ghostherd-session-removed-hook #'ghostherd--prune-turn-ends)

(defconst ghostherd--turn-ends-prune-every 120
  "Poll ticks between a sweep of `ghostherd--turn-ends', for a live agent
that left an old conversation for a new one without any agent being removed.")

(defvar ghostherd--poll-count 0
  "Poll ticks since start, for occasional housekeeping.")

(defun ghostherd--claude-entry-text (message)
  "The words of transcript MESSAGE, its text parts joined, trimmed."
  (let ((content (plist-get message :content)))
    (string-trim
     (if (stringp content)
         content
       (mapconcat (lambda (part) (or (and (listp part) (plist-get part :text)) ""))
                  content "\n")))))

(defun ghostherd--claude-entry-verdict (line)
  "What claude transcript LINE says about the turn: (:error TEXT) for
claude's own error, :clean for any other entry of the conversation, nil
for one to read past -- bookkeeping, a sub-agent's, a local command."
  ;; Cheap first: a conversation entry names its type at the top level.
  (when (string-match-p "\"type\":\"\\(?:user\\|assistant\\)\"" line)
    (when-let* ((entry (ignore-errors
                         (json-parse-string line :object-type 'plist
                                            :null-object nil :false-object nil))))
      (let ((type (plist-get entry :type))
            (message (plist-get entry :message)))
        (cond
         ((plist-get entry :isSidechain) nil)
         ((equal type "assistant")
          (if (plist-get entry :isApiErrorMessage)
              (list :error (ghostherd--claude-entry-text message))
            :clean))
         ((equal type "user")
          (let ((content (plist-get message :content)))
            (unless (or (plist-get entry :isMeta)
                        (and (stringp content)
                             (string-match-p "\\`<\\(?:command-\\|local-command-\\|bash-\\)"
                                             content)))
              :clean))))))))

(defun ghostherd--claude-read-turn-end (path size)
  "Read claude transcript PATH, SIZE bytes long, back from its end for
`ghostherd--claude-turn-end'.  A chunk at a time, growing, since one
entry can be large; up to `ghostherd--transcript-tail-max'."
  (let ((chunk 65536)
        (coding-system-for-read 'utf-8))
    (catch 'done
      (while t
        (let* ((beg (max 0 (- size chunk)))
               (lines (with-temp-buffer
                        (insert-file-contents path nil beg size)
                        (split-string (buffer-string) "\n" t))))
          ;; A chunk that starts inside the file starts inside a line.
          (when (> beg 0) (pop lines))
          (dolist (line (nreverse lines))
            (when-let* ((verdict (ghostherd--claude-entry-verdict line)))
              (throw 'done verdict)))
          (when (or (= beg 0) (>= chunk ghostherd--transcript-tail-max))
            (throw 'done nil))
          (setq chunk (* chunk 4)))))))

(defun ghostherd--claude-turn-end (path)
  "What the last turn in claude transcript PATH ended on: (:error TEXT)
when the conversation's last entry is claude's own error (an assistant
entry marked `isApiErrorMessage'), :clean when it is any other, nil
when none can be read.  Read once per change of the file."
  (when-let* ((attrs (file-attributes path)))
    (let ((size (file-attribute-size attrs))
          (mtime (file-attribute-modification-time attrs))
          (seen (gethash path ghostherd--turn-ends)))
      (if (and seen (eql (car seen) size) (equal (cadr seen) mtime))
          (cddr seen)
        (let ((verdict (ignore-errors (ghostherd--claude-read-turn-end path size))))
          (puthash path (cons size (cons mtime verdict)) ghostherd--turn-ends)
          verdict)))))

(defun ghostherd--transcript-turn-end (session)
  "What SESSION's transcript says its last turn ended on, as
`ghostherd--claude-turn-end' does; nil when there is no transcript to
ask -- an agent without hooks yet, or a kind whose transcript keeps no
such mark (grok keeps none; agy's ERROR_MESSAGE entries have no rule)."
  (when (eq (ghostherd-session-kind session) 'claude)
    (when-let* ((path (ghostherd-session-transcript session)))
      (ghostherd--claude-turn-end path))))

(defconst ghostherd--turn-status-re "\\`[·✢✳✶✻✽*][[:blank:]]"
  "claude's spinner, or its past tense once the turn is over (\"✻ Cooked
for 22m 42s · done 10:42 PM\"), at the left edge under the last message.")

(defun ghostherd--last-message (screen)
  "The words of the last message drawn above SCREEN's input box, or nil.
claude starts a message with ⏺ (● where the font has none) at the left
edge and indents the rest; its own error is drawn as such a message,
the whole of it.  Read up from the box past blank lines, the turn's
status line and indented ones (the update banner, the message's wrapped
lines) to the first line at the left edge: a ⏺ there starts the last
message, and its first paragraph, joined, is returned.  Anything else
there -- a prompt typed since, the scrollback's top with the message's
start gone above it -- and the last thing on screen is not a message."
  (let* ((lines (vconcat (split-string (or screen "") "\n")))
         (top (ghostherd--box-top lines)))
    (when top
      (let ((i (1- top)))
        (while (and (>= i 0)
                    (let ((line (aref lines i)))
                      (or (string-blank-p line)
                          (string-match-p "\\`[[:blank:]]" line)
                          (string-match-p ghostherd--turn-status-re line))))
          (setq i (1- i)))
        (when (and (>= i 0) (string-match "\\`[⏺●][[:blank:]]+" (aref lines i)))
          (let ((words (list (substring (aref lines i) (match-end 0)))))
            (cl-loop for j from (1+ i) below top
                     for line = (aref lines j)
                     while (string-match-p "\\`[[:blank:]]+[^[:blank:]]" line)
                     do (push (string-trim line) words))
            (string-join (nreverse words) " ")))))))

(defun ghostherd--cli-error-match (session &optional screen)
  "The CLI error SESSION's last turn ended on, as (:reason :action
:cause), or nil.
SESSION's transcript decides where it can
(`ghostherd--transcript-turn-end'); the screen only when it cannot --
SCREEN when the caller has it, else the last one stored -- and then
only the last message above the input box counts."
  (when-let* ((rules (alist-get (ghostherd-session-kind session) ghostherd-cli-errors)))
    (cl-flet ((rule-for (text)
                (let ((case-fold-search t))
                  (cl-loop for (re action reason cause) in rules
                           when (string-match-p re text)
                           return (list :reason reason :action action :cause cause)))))
      (pcase (ghostherd--transcript-turn-end session)
        (`(:error ,text)
         (or (rule-for text)
             (list :reason (concat "its turn ended on an error: "
                                   (truncate-string-to-width
                                    (car (split-string text "\n")) 80 nil nil "…"))
                   :action nil :cause nil)))
        (:clean nil)
        (_ (when-let* ((screen (or screen (gethash (ghostherd-session-id session)
                                                   ghostherd--screens)))
                       (text (ghostherd--last-message screen)))
             (rule-for text)))))))

(defun ghostherd--cli-error (session &optional screen)
  "Return (REASON . ACTION) for the CLI error SESSION's turn ended on, or nil.
See `ghostherd--cli-error-match', which also gives the cause.  A turn
that ended on such an error has not answered: the agent is stuck, not
idle, until the user acts."
  (when-let* ((m (ghostherd--cli-error-match session screen)))
    (cons (plist-get m :reason) (plist-get m :action))))

(defun ghostherd--continue-cause (session &optional screen)
  "What cut SESSION's turn short, when the herd may carry it on itself:
a key of `ghostherd--continue-causes', or nil when the turn did not end
on a `continue' error."
  (when-let* ((m (ghostherd--cli-error-match session screen)))
    (and (eq (plist-get m :action) 'continue)
         (or (plist-get m :cause) 'slept))))


;;; Carry a turn on after the Mac slept, or the connection dropped
;;
;; A turn killed by "API Error: … Your computer went to sleep mid-response"
;; reads as `blocked' on a `continue' CLI error, so the ask it was
;; answering is held, not closed (see `ghostherd-cli-errors').  But the
;; error appears only once the machine is awake again -- so a poll seeing
;; it is the wake signal, and the herd can finish the thought itself: wait
;; a settle delay for the network to come back, then send a prompt telling
;; the agent its turn was cut short and to pick up where it left off.
;; The held ask is then answered by the continued turn as usual.  A turn
;; killed by "API Error: Connection lost mid-response" is the same thing
;; with another cause, and is carried on the same way, in its own words:
;; the prompt and the card say the connection dropped, not that the
;; computer slept (`ghostherd--continue-causes').
;;
;; Guard rails: only the errors `ghostherd-cli-errors' marks `continue',
;; at most `ghostherd-sleep-continue-max' times per stuck turn, and not at
;; all if by the end of the delay the user has stepped in, the error is
;; gone, or the session is.  A continued turn that dies again stays
;; blocked.

(defcustom ghostherd-sleep-continue-delay 45
  "Seconds to wait, after first seeing a turn cut short -- the Mac slept,
the connection dropped -- before continuing it.  The slept error only
shows once the Mac is awake, so this is the grace for the network to
come back before the prompt is sent; a dropped connection wants the
same."
  :type 'number
  :group 'ghostherd)

(defcustom ghostherd-sleep-continue-max 1
  "How many times the herd continues one turn cut short, whatever cut it:
the Mac sleeping, the connection dropping.
Once spent, a turn that dies the same way again stays blocked, and the
card says the herd stopped retrying.  The budget resets when the agent
reaches a settled state (idle/done/waiting) under its own steam -- a new
stuck turn is a fresh one."
  :type 'integer
  :group 'ghostherd)

(defcustom ghostherd-sleep-continue-prompt
  "Your last turn was cut off because this computer went to sleep mid-response -- not because the work was finished. Pick up exactly where you left off and carry on to the end."
  "The prompt sent to an agent whose turn the Mac's sleep cut short."
  :type 'string
  :group 'ghostherd)

(defcustom ghostherd-dropped-continue-prompt
  "Your last turn was cut off because the connection to the API dropped mid-response -- not because the work was finished. Pick up exactly where you left off and carry on to the end."
  "The prompt sent to an agent whose turn a dropped connection cut short.
It says nothing of sleep: the computer did not sleep."
  :type 'string
  :group 'ghostherd)

(defconst ghostherd--continue-causes
  '((slept :prompt ghostherd-sleep-continue-prompt
           :cut "the computer's sleep"
           :after "after sleep"
           :again "the computer slept again")
    (dropped :prompt ghostherd-dropped-continue-prompt
             :cut "a dropped connection"
             :after "after a dropped connection"
             :again "the connection dropped again"))
  "How the herd speaks of each cause of a `continue' CLI error (see
`ghostherd-cli-errors'): the variable holding the prompt it sends the
agent, what cut the turn short in its log, and the card's words --
\"continuing AFTER in Ns\" while it counts down, \"stopped retrying --
AGAIN\" once it has stopped.")

(defvar ghostherd--sleep-continue (make-hash-table :test 'equal)
  "Session id -> the plist of its current cut-short episode, or nothing.
The plist is (:since FLOAT :attempts INT :cancelled BOOL): when the block
was first seen, how many continues have been sent, and whether the user
waved it off.  A hash rather than a session slot, for the reason
`ghostherd--input-at' gives.  Cleared when the agent settles.")

(defun ghostherd--sleep-continue-at (session)
  "When SESSION's cut-short turn will be continued, as a float time, or nil.
Non-nil only while a continue is actually pending -- blocked on a
`continue' error, budget left, not cancelled -- so the card shows a
countdown exactly then.  See `ghostherd--sleep-maybe-continue'."
  (when-let* ((ep (gethash (ghostherd-session-id session) ghostherd--sleep-continue)))
    (when (and (eq (ghostherd-session-state session) 'blocked)
               (not (plist-get ep :cancelled))
               (< (plist-get ep :attempts) ghostherd-sleep-continue-max)
               (ghostherd--continue-cause session))
      (+ (plist-get ep :since) ghostherd-sleep-continue-delay))))

(defun ghostherd--sleep-continue-stopped-p (session)
  "Non-nil when SESSION sits on a cut-short turn the herd has stopped
retrying.  Either the budget is spent or the user cancelled -- the card
then says so instead of counting down."
  (when-let* ((ep (gethash (ghostherd-session-id session) ghostherd--sleep-continue)))
    (and (eq (ghostherd-session-state session) 'blocked)
         (ghostherd--continue-cause session)
         (or (plist-get ep :cancelled)
             (>= (plist-get ep :attempts) ghostherd-sleep-continue-max)))))

(defun ghostherd--continue-words (session)
  "The card's words for SESSION's cut-short turn, as (AFTER . AGAIN) from
`ghostherd--continue-causes'; nil when it sits on no `continue' error."
  (when-let* ((says (alist-get (ghostherd--continue-cause session)
                               ghostherd--continue-causes)))
    (cons (plist-get says :after) (plist-get says :again))))

(defun ghostherd--sleep-continue-ok-p (session screen ep)
  "The fire-time guards for continuing SESSION's cut-short turn.
By the end of the delay: the error must still be the turn's end (read
off SCREEN only where no transcript says), nobody may have typed into
the agent since the wait began (EP's `:since'), and the session must be
live."
  (and (ghostherd--session-live-p session)
       (ghostherd--continue-cause session screen)
       (let ((at (gethash (ghostherd-session-id session) ghostherd--input-at)))
         (or (null at) (< at (plist-get ep :since))))))

(defun ghostherd--sleep-continue-fire (session ep cause)
  "Send SESSION the continue prompt for CAUSE, log it, and spend one of
EP's tries.  CAUSE is a key of `ghostherd--continue-causes'."
  (let ((n (1+ (plist-get ep :attempts)))
        (says (alist-get cause ghostherd--continue-causes)))
    (puthash (ghostherd-session-id session)
             (plist-put ep :attempts n)
             ghostherd--sleep-continue)
    (ghostherd--log-add
     session 'continue
     (format "↻ continuing a turn %s cut short (try %d of %d)"
             (plist-get says :cut) n ghostherd-sleep-continue-max))
    (ghostherd-send session (symbol-value (plist-get says :prompt)) t)))

(defun ghostherd--sleep-maybe-continue (session screen)
  "After a poll set SESSION's state, carry its turn on if it was cut short.
SCREEN is the screen that poll read.  A turn the Mac's sleep or a
dropped connection killed reads as `blocked' on a `continue' CLI error;
once `ghostherd-sleep-continue-delay' has passed and the guards hold,
send the continue prompt for its cause, at most
`ghostherd-sleep-continue-max' times per stuck turn."
  (let* ((id (ghostherd-session-id session))
         (state (ghostherd-session-state session))
         (cause (and (eq state 'blocked)
                     (ghostherd--continue-cause session screen))))
    (cond
     ;; A settled, non-stuck state ends the episode: the next sleep is a
     ;; fresh one with a full budget.  `working' does NOT end it -- sending
     ;; the continue prompt itself turns the agent working -- so a continued
     ;; turn that dies the same way keeps the spent budget and is not
     ;; retried, which is the rule.  Blocked on something else is left
     ;; alone: harmless, and clears when it finally settles.
     ((memq state '(idle done waiting dead))
      (remhash id ghostherd--sleep-continue))
     ((not cause) nil)
     (t
      (let ((ep (or (gethash id ghostherd--sleep-continue)
                    (puthash id (list :since (float-time) :attempts 0 :cancelled nil)
                             ghostherd--sleep-continue))))
        (when (and (not (plist-get ep :cancelled))
                   (< (plist-get ep :attempts) ghostherd-sleep-continue-max)
                   (>= (- (float-time) (plist-get ep :since))
                       ghostherd-sleep-continue-delay)
                   (ghostherd--sleep-continue-ok-p session screen ep))
          (ghostherd--sleep-continue-fire session ep cause)))))))

(defun ghostherd-sleep-continue-cancel (session)
  "Wave off the pending auto-continue for SESSION's cut-short turn.
The card's Cancel: it stays blocked, and the herd will not continue this
turn.  The next stuck turn is unaffected -- the episode clears when the
agent settles."
  (setq session (ghostherd-get session))
  (when session
    (let ((id (ghostherd-session-id session)))
      (puthash id (plist-put (or (gethash id ghostherd--sleep-continue)
                                 (list :since (float-time) :attempts 0))
                             :cancelled t)
               ghostherd--sleep-continue))))

(defcustom ghostherd-login-code-re
  "paste.*code\\|code here\\|authoriz.*code\\|enter the code\\|paste it here"
  "What claude's /login code-entry screen says near its field, after the
subscription option: a URL to open above, a place to paste the code.

UNCONFIRMED from a real capture -- a scratch /login shares the user's
credentials, too risky to run just to read the screen -- so this is from
the shape of the flow, not a sample.  Tune it, with `ghostherd-login-url-re',
once a real one is in the Log tab (the auto-capture will keep it)."
  :type 'regexp
  :group 'ghostherd)

(defcustom ghostherd-login-url-re
  "https?://[^ \t\n\r\"'<>|]+"
  "A URL on a screen, for the login flow's open/copy on the card."
  :type 'regexp
  :group 'ghostherd)

(defun ghostherd--login-url (session &optional screen)
  "The URL to open when SESSION sits on claude's /login code-entry screen
-- a URL above a field for the code -- or nil.  See `ghostherd-login-code-re'
for what is confirmed and what is not."
  (when (eq (ghostherd-session-kind session) 'claude)
    (when-let* ((screen (or screen (gethash (ghostherd-session-id session) ghostherd--screens))))
      (ghostherd--screen-read screen (list 'login ghostherd-login-code-re ghostherd-login-url-re)
                              #'ghostherd--login-url-on))))

(defun ghostherd--login-url-on (screen)
  "The URL of claude's /login code-entry SCREEN, or nil."
  (let ((foot (string-join (ghostherd--screen-foot screen 14) "\n"))
        (case-fold-search t))
    (when (and (string-match-p ghostherd-login-code-re foot)
               (string-match ghostherd-login-url-re foot))
      (match-string 0 foot))))

(defun ghostherd--prompt-json (prompt)
  "PROMPT for JSON: its options as (:n N :label LABEL); false for none."
  (if prompt
      (list :question (plist-get prompt :question)
            :options (vconcat (mapcar (lambda (o) (list :n (car o) :label (cdr o)))
                                      (plist-get prompt :options))))
    :json-false))

(defun ghostherd--same-screen (id screen)
  "SCREEN -- or session ID's last screen, the very string, when SCREEN is
the same text: what was read off it is found again (`ghostherd--screen-read')."
  (let ((last (gethash id ghostherd--screens)))
    (if (and (stringp screen) (stringp last) (not (eq last screen)) (string= last screen))
        last
      screen)))

(defun ghostherd--detect-state (session &optional screen)
  "Return (STATE . REASON) for SESSION from screen rules / buffer liveness.

SCREEN is that session's screen when the caller already has it -- the
poll path fetches the whole herd's screens in one go, and reading each
one again individually is the cost that made it worth batching.  Without
it, this captures for itself, which is what every interactive caller
does."
  (cond
   ((ghostherd-session-manual-state session)
    (cons (ghostherd-session-manual-state session) "manual"))
   ((not (ghostherd--session-live-p session))
    (cons 'dead "host gone"))
   (t
    (let* ((kind (ghostherd-session-kind session))
           (spec (ignore-errors (ghostherd--spec kind)))
           (rules (plist-get spec :screen-rules))
           (exited (ghostherd--host-exited-p session)))
      (cond
       (exited
        (cons 'dead exited))
       (t
        (let* ((tail (and rules (ghostherd--same-screen
                                 (ghostherd-session-id session)
                                 (or screen (ghostherd--host-capture session)))))
               ;; Scrolled back, the screen is history: none of it is read
               ;; for a state -- not a spinner gone from view, nor an old
               ;; prompt or error the view happens to show.
               (scrolled (and tail (ghostherd--screen-read tail 'scrolled #'ghostherd--scrolled-back-p
                                                           (plist-get spec :scrolled))))
               (hit (and tail (not scrolled)
                         (ghostherd--read-rules tail rules (plist-get spec :prompt-replaces-input)
                                                (plist-get spec :spinner))))
               (report (ghostherd--fresh-report session))
               (err (and tail (not scrolled) (ghostherd--cli-error session tail)))
               (login-url (and tail (not scrolled) (not err) (ghostherd--login-url session tail)))
               ;; A dialog drawn from the top of a pane taller than the tail
               ;; -- each CLI's trust prompt, in a window over 40 rows --
               ;; leaves the tail blank and no box in it: it read idle, and
               ;; took a paste's Return as its answer (ask cf9873be).  With
               ;; nothing read and no box at the foot, the whole pane is
               ;; read, its blank rows dropped.
               (pane (and tail (not scrolled) (not err) (not login-url)
                          (memq (car-safe hit) '(nil idle))
                          (plist-get spec :prompt-replaces-input)
                          (not (ghostherd--input-at-foot-p tail))
                          (ghostherd--whole-pane session tail)))
               (hit (or (and pane (ghostherd--read-rules pane rules t (plist-get spec :spinner)))
                        hit))
               (seen (or pane tail)))
          (when tail
            (puthash (ghostherd-session-id session) tail ghostherd--screens)
            (ghostherd--read-foot session tail))
          (cond
           ;; Scrolled back: what the agent last said of itself, if it is
           ;; still believed, or else the state it was in -- the view tells
           ;; nothing new until it is back at the bottom.
           (scrolled
            (or report
                (cons (ghostherd-session-state session)
                      "scrolled back: the screen shows history, not its state")))
           ;; Screen rules keep priority for `blocked': a stale progress
           ;; report must never mask an agent sitting on a prompt, and
           ;; neither may a stale report from the agent itself.
           ((eq (car-safe hit) 'blocked)
            (cons 'blocked (or (and (member (cdr hit) ghostherd--prompt-foot-patterns)
                                    (ghostherd--prompt-reason seen))
                               (ghostherd--read-reason seen hit))))
           ;; A menu that has replaced the input box, caught in general
           ;; rather than by a rule per menu: AskUserQuestion, the trust
           ;; prompt, the /login method picker, whatever the next release
           ;; draws next.  Only while the box at the foot is the menu's own
           ;; (`:prompt-replaces-input', and no input box under it), so a
           ;; numbered list quoted in the conversation never trips it;
           ;; `ghostherd--prompt' then reads its options for the card.
           ((and (plist-get spec :prompt-replaces-input)
                 seen
                 (not (ghostherd--screen-read seen (list 'box ghostherd-status-lines)
                                              #'ghostherd--input-at-foot-p))
                 ;; The model picker also replaces the input and is a numbered
                 ;; list, but the herd drives it, not the user -- it is not a
                 ;; block to answer from the card.
                 (not (ghostherd--model-picker seen))
                 (ghostherd--prompt seen))
            (cons 'blocked (or (ghostherd--prompt-reason seen) "waiting for a choice")))
           ;; A turn that ended on a CLI error -- login expired, the computer
           ;; slept mid-response -- has not answered and is not idle: read it
           ;; as blocked, so an ask it was answering is held rather than
           ;; closed with the error screen, and say plainly why.  Ahead of
           ;; the report, so a stale `working' hook cannot mask it.
           (err (cons 'blocked (car err)))
           ;; The /login code-entry screen: a URL to open and a field to
           ;; paste the code into.  Blocked on the user, with the URL carried
           ;; to the card so it can be opened or copied from there.
           (login-url (cons 'blocked "sign in: open the link, then paste the code"))
           ;; What the agent says about itself beats reading its screen,
           ;; which is a rendering of a drawing of the same fact -- with
           ;; one exception.  A claimed `blocked' loses to a screen that
           ;; positively says `working', because those patterns match a
           ;; spinner: an agent generating tokens is not sitting on a
           ;; prompt, whatever its last hook said.  So does `waiting': a
           ;; sub-agent back wakes it before its hook says so.  `idle' is
           ;; not positive evidence -- it is this function's fallback --
           ;; so it does not get the same veto.
           ((and report
                 (not (and (memq (car report) '(blocked waiting))
                           (eq (car-safe hit) 'working))))
            report)
           ;; A shell is not an agent, so nothing about it is detectable
           ;; -- but if something reported for it, that was believed above.
           ((eq kind 'shell)
            (cons 'idle "shell"))
           ;; Otherwise a live progress report beats scraping, which cannot
           ;; tell a working agent from a quiet one.
           ((ghostherd--progress-fresh-p session)
            (cons 'working (ghostherd--progress-reason session)))
           (hit (cons (car hit) (ghostherd--read-reason tail hit)))
           ((null rules)
            (cons 'working "no rules"))
           ;; Known agent, no match → idle fallback (herdr-style)
           (t (cons 'idle "no rule matched"))))))))))

(defcustom ghostherd-input-grace 6
  "Seconds after input during which a session is not believed to be idle.

The same reasoning as `ghostherd-handoff-grace', which had it first and
had it alone.  An agent does not start the instant you press Return: the
CLI has to wake up and print, and until it does the screen still shows
what it showed before -- so the poll one second later reads `idle',
promotes it to `done' because work was in flight, and notifies you that
an agent which has not yet read your prompt is ready for review.

`ghostherd-handoff' guarded its own watch against exactly this and the
poll path did not, which is why it only showed up when someone sent a
prompt by hand and then watched the log."
  :type 'number
  :group 'ghostherd)

(defvar ghostherd--input-at (make-hash-table :test 'equal)
  "Session id → when input was last sent, as a float time.

A hash table rather than a slot on `ghostherd-session': adding a slot
means every session object created before the next reload is read
through shifted accessors, and this package is developed by reloading
into a live herd.  The readme's troubleshooting section has the story.")

(defcustom ghostherd-idle-settle 4
  "Seconds a busy session must look idle before it is believed.

Agent CLIs draw their input prompt whether or not they are working, so
an empty prompt is not evidence of idleness -- what makes a screen read
as busy is a spinner or a status line, and those blink out between two
tool calls.  A poll landing in that gap sees a screen with nothing
working on it, calls it idle, promotes it to `done', and notifies.  The
next poll sees the spinner again.  Three seconds of flapping produced
four notifications in the log this was written from.

Only entering idle waits.  Leaving it is immediate, because that is a
positive signal: something appeared on the screen."
  :type 'number
  :group 'ghostherd)

(defvar ghostherd--idle-since (make-hash-table :test 'equal)
  "Session id → when it first looked idle, as a float time.")

(defun ghostherd--settle-idle (session state)
  "Return STATE, or SESSION's current state while idle has not stuck yet."
  (let ((id (ghostherd-session-id session))
        (was (ghostherd-session-state session)))
    (cond
     ((or (not (eq state 'idle))
          (not (memq was '(working blocked starting))))
      (remhash id ghostherd--idle-since)
      state)
     (t
      (let ((since (or (gethash id ghostherd--idle-since)
                       (puthash id (float-time) ghostherd--idle-since))))
        (if (>= (- (float-time) since) ghostherd-idle-settle)
            (progn (remhash id ghostherd--idle-since) state)
          was))))))

(defvar ghostherd-watched-functions nil
  "Functions called with a session, saying whether you are looking at it
somewhere other than its own buffer.  The first non-nil answer counts.
The herd page's room uses it: its columns and its chat show what an
agent said as well as the agent's buffer does.")

(defun ghostherd--buffer-watched-p (buffer)
  "Return non-nil when BUFFER is where you could be looking now.

Being displayed is not enough: `get-buffer-window\=' with ALL-FRAMES t
counts frames that are iconified or on another desktop, which are
precisely the moments a banner is for.  So: a window on a *visible*
frame, and that frame holding input focus.

`frame-focus-state\=' answers `unknown\=' where the window system cannot
say.  Unknown is treated as focused, on the grounds that a missing
notification is a smaller wrong than one you did not need."
  (when (buffer-live-p buffer)
    (when-let* ((window (get-buffer-window buffer 'visible)))
      (not (null (frame-focus-state (window-frame window)))))))

(defun ghostherd--watched-p (session)
  "Return non-nil when you could actually be looking at SESSION now.
`done' means finished while you were not looking, so this decides
whether a notification is worth firing, and when a `done' is seen: its
buffer in front of you (`ghostherd--buffer-watched-p'), or another place
that shows it (`ghostherd-watched-functions')."
  (or (when-let* ((buffer (ghostherd-session-buffer session)))
        (ghostherd--buffer-watched-p buffer))
      (run-hook-with-args-until-success 'ghostherd-watched-functions session)))

(defun ghostherd--waking-up-p (session)
  "Return non-nil while SESSION is too freshly prompted to be believed idle."
  (when-let* ((at (gethash (ghostherd-session-id session)
                           ghostherd--input-at)))
    (< (- (float-time) at) ghostherd-input-grace)))

(defun ghostherd--set-state (session new &optional reason)
  "Set SESSION state to NEW with optional REASON, run hooks/notify."
  (let ((old (ghostherd-session-state session)))
    (unless (eq old new)
      (setf (ghostherd-session-state session) new
            (ghostherd-session-state-reason session) reason
            (ghostherd-session-last-active session) (current-time))
      ;; Unseen while working/blocked/done so finish can promote to `done'
      ;; until the user visits the session.
      (when (memq new '(working blocked done))
        (setf (ghostherd-session-seen session) nil))
      (ghostherd--log-transition session old new reason)
      (ghostherd--maybe-notify-state session old new)
      (run-hook-with-args 'ghostherd-state-change-hook session old new)
      (when (get-buffer "*ghostherd*")
        (ghostherd--sidebar-refresh)))
    new))

(defun ghostherd-poll-session (session &optional screen)
  "Recompute and store state for SESSION.  Return new state.
SCREEN is SESSION's screen if the caller already fetched it; see
`ghostherd--detect-state'."
  (setq session (ghostherd-get session))
  (when session
    (let ((watched (ghostherd--watched-p session)))
      ;; A session you are looking at has been seen, by definition.  `done'
      ;; means "finished while you were not looking"; announcing it about an
      ;; agent on your screen is just noise, and it was most of the noise.
      (when watched
        (setf (ghostherd-session-seen session) t))
      (pcase-let ((`(,state . ,reason) (ghostherd--detect-state session screen)))
        ;; Freshly prompted agents are not idle, they are slow.  Before the
        ;; grace elapses the screen still shows whatever it showed when you
        ;; pressed Return, and believing it turns into a `done' notification
        ;; for work that has not started.  Nor still waiting: that was
        ;; said before the prompt, and its hook says `working' only once
        ;; the CLI has woken to it.
        ;;
        ;; Both this and the settle below only ever hold back an `idle',
        ;; and they exist because a *screen* is weak evidence.  The states
        ;; an agent actually reports about itself -- `working', `blocked',
        ;; `done' -- pass through untouched, which is the point: a hook
        ;; firing at the moment the CLI stops needs no hysteresis.
        (when (and (memq state '(idle waiting))
                   (not (ghostherd-session-manual-state session))
                   (ghostherd--waking-up-p session))
          (setq state 'working
                reason "input sent, not awake yet"))
        ;; ...and a busy agent between two tool calls is not idle either.
        (setq state (ghostherd--settle-idle session state))
        ;; Promote idle → done when work finishes.
        ;;
        ;; Whether you saw it finish is asked below, of `watched\=', which
        ;; is about this instant.  It used to be asked here of `seen\=',
        ;; which is not: `seen\=' latches the moment the session is on a
        ;; focused frame and only clears on the way back into `working\='.
        ;; So watching an agent start -- which is what sending it a prompt
        ;; looks like -- suppressed the banner for the whole run, however
        ;; long you were away by the time it ended.  No `done\=' was ever
        ;; logged on the herd this was written from.
        (when (and (eq state 'idle)
                   (memq (ghostherd-session-state session)
                         '(working blocked starting)))
          (setq state 'done
                reason (or reason "idle after work")))
        ;; Herdr-style: `done' stays visible until the user views the
        ;; session.  It needs saying, because nothing re-derives it: the
        ;; screen cannot show `done\=' -- `ghostherd--detect-state' has no
        ;; branch that returns it -- and the promotion above only fires
        ;; out of `working'.  Without this the ✓ lasted a single tick.
        (when (and (eq state 'idle)
                   (eq (ghostherd-session-state session) 'done)
                   (not (ghostherd-session-seen session)))
          (setq state 'done
                reason (or (ghostherd-session-state-reason session) reason)))
        ;; `done' means finished while you were not looking, whoever said
        ;; so -- the promotion, the hold above, or an agent reporting it
        ;; outright.  All three answer to this.
        ;;
        ;; Both of these overwrite REASON rather than defaulting it.
        ;; `ghostherd--detect-state' answers with a reason in every
        ;; branch it has, so an `or\=' here could never reach its own
        ;; string, and the log said "no rule matched" about a decision
        ;; the rules did not make -- which is the one line the manual
        ;; tells you to read when a banner does not arrive.
        (when (and (eq state 'done) watched)
          (setq state 'idle
                reason "done, but you are watching")
          (ghostherd--report-seen session))
        ;; Once viewed, demote done back to plain idle.
        (when (and (eq state 'idle)
                   (eq (ghostherd-session-state session) 'done)
                   (ghostherd-session-seen session))
          (setq reason "seen"))
        (ghostherd--set-state session state reason)
        ;; With the screen of this poll still in hand, keep it if it looks
        ;; suspicious -- throttled and deduped, so this is nearly always a
        ;; few regexps and no capture.
        (ignore-errors (ghostherd--auto-capture session))
        ;; ...and, with that same screen, carry a turn on by itself if the
        ;; Mac's sleep cut it short.  The state is set, so this reads it.
        (ignore-errors
          (ghostherd--sleep-maybe-continue
           session (gethash (ghostherd-session-id session) ghostherd--screens)))
        state))))

(defvar ghostherd-restore-every 30
  "Seconds between the poll's looks for agents to re-adopt, while the
herd is empty.")

(defvar ghostherd--restore-tried-at 0
  "When the poll last looked for agents to re-adopt.")

(defun ghostherd-poll-all ()
  "Poll every registered session, one round trip per host where possible.

A host that can dump the whole herd's screens in one go says so from
`ghostherd-backend-screens' and is then responsible for the tick; one
that cannot -- ghostel, where the screen is already an Emacs buffer --
gets read session by session, which costs nothing there.

Sessions are grouped by backend rather than swept in one list, because
\"one round trip\" is a claim only a single host can make, and a herd may
straddle two."
  (let ((groups (seq-group-by #'ghostherd-session-backend
                              (hash-table-values ghostherd--sessions))))
    (if (null groups)
        (progn
          (ghostherd--ensure-sessions)
          ;; An empty herd is looked for again now and then: one emptied
          ;; by a moment its host could not be reached came back only
          ;; when a session was picked or the sidebar opened, and the
          ;; herd page did neither.
          (when (> (- (float-time) ghostherd--restore-tried-at) ghostherd-restore-every)
            (setq ghostherd--restore-tried-at (float-time))
            (ghostherd--maybe-restore)))
      (pcase-dolist (`(,backend . ,members) groups)
        ;; The callback closes over MEMBERS and may arrive long after this
        ;; loop has moved on -- safe, because `dolist' binds afresh per
        ;; iteration under lexical binding, and each group's callback
        ;; therefore keeps its own sessions.
        (unless (ignore-errors
                  (ghostherd-backend-screens
                   backend members ghostherd-screen-tail-lines
                   (lambda (screens)
                     (ghostherd--poll-with-screens members screens))))
          (ghostherd--poll-with-screens members nil))))))

(defun ghostherd--poll-with-screens (sessions screens)
  "Recompute state for SESSIONS, reading SCREENS instead of the hosts.

SCREENS is an alist of session id → screen, and it is allowed to be
partial: a host may abandon a batch halfway, and a session it did not
answer for is read individually rather than skipped.

The sweep runs here rather than in the caller so that a batched host has
already stamped whatever it learned in the same round trip -- otherwise
asking whether each session is still alive would be the second
synchronous call this exists to remove."
  (ghostherd--ensure-sessions)
  (dolist (session sessions)
    (ignore-errors
      ;; The sweep may have deregistered it in between.
      (when (ghostherd-get (ghostherd-session-id session))
        (ghostherd-poll-session
         session (cdr (assoc (ghostherd-session-id session) screens)))))))

(defun ghostherd--ensure-poll-timer ()
  "Start the background poll timer if needed."
  (unless (timerp ghostherd--poll-timer)
    (setq ghostherd--poll-timer
          (run-with-timer ghostherd-poll-interval
                          ghostherd-poll-interval
                          #'ghostherd--poll-tick))))

(defun ghostherd--stop-poll-timer ()
  "Stop the background poll timer when no sessions remain."
  (when (and (timerp ghostherd--poll-timer)
             (= (hash-table-count ghostherd--sessions) 0))
    (cancel-timer ghostherd--poll-timer)
    (setq ghostherd--poll-timer nil)))


;;; Ghostel integration hooks

(defun ghostherd--on-ghostel-exit (buffer event)
  "React to the ghostel process in BUFFER exiting.  EVENT is the sentinel.

Whether that is a death depends on what the buffer was running.  On the
ghostel backend the buffer *is* the agent, so it is.  On a backend where
the buffer holds a client -- `tmux attach' -- the exit only means the
view went away, and marking the session dead would turn every closed
window into a crash: a notification would fire, a handoff would give up,
and the next poll would flip the state straight back."
  (when-let* ((session (ghostherd-get buffer)))
    (if (ghostherd-backend-view-is-host-p (ghostherd-session-backend session))
        (progn
          (ghostherd--set-state session 'dead (string-trim (or event "exited")))
          (ghostherd--stop-poll-timer))
      (setf (ghostherd-session-buffer session) nil)
      (ghostherd--sidebar-refresh))))

(defun ghostherd--on-command-start (buffer)
  "OSC 133 command-start hook: mark SESSION on BUFFER as working."
  (when-let* ((session (ghostherd-get buffer)))
    (unless (ghostherd-session-manual-state session)
      (ghostherd--set-state session 'working "command start"))))

(defun ghostherd--on-command-finish (buffer status)
  "OSC 133 command-finish hook for BUFFER with exit STATUS."
  (when-let* ((session (ghostherd-get buffer)))
    (unless (ghostherd-session-manual-state session)
      ;; Don't force idle for long-running TUI agents; let screen rules decide.
      (when (eq (ghostherd-session-kind session) 'shell)
        (ghostherd--set-state
         session 'idle
         (format "command finish (%s)" (or status "?")))))))

(defun ghostherd--on-progress (state percent)
  "Record a ConEmu OSC 9;4 progress report for the current buffer's session.
STATE and PERCENT are as documented for `ghostel-progress-function'.

This runs synchronously on ghostel's VT-parser callpath, where anything
slow stalls terminal output, so it only stores the report -- deciding
what it means is left to the poll path.  The previous
`ghostel-progress-function' is always called, since it is a single
global setting rather than a hook and is very likely someone's spinner."
  (when-let* ((session (ghostherd-get (current-buffer))))
    (setf (ghostherd-session-progress-state session) state
          (ghostherd-session-progress-percent session) percent
          (ghostherd-session-progress-at session) (current-time)))
  (when (functionp ghostherd--saved-progress-function)
    (funcall ghostherd--saved-progress-function state percent)))

(defun ghostherd--install-hooks ()
  "Install ghostel hooks used by ghostherd."
  (add-hook 'ghostel-exit-functions #'ghostherd--on-ghostel-exit)
  (add-hook 'ghostel-command-start-functions #'ghostherd--on-command-start)
  (add-hook 'ghostel-command-finish-functions #'ghostherd--on-command-finish)
  (advice-add 'ghostel-copy-mode :after #'ghostherd--copy-mode-sync-evil)
  (advice-add 'ghostel-readonly-exit :after #'ghostherd--copy-mode-sync-evil)
  (when (and ghostherd-use-osc-progress
             (boundp 'ghostel-progress-function)
             (not (eq ghostel-progress-function #'ghostherd--on-progress)))
    (setq ghostherd--saved-progress-function ghostel-progress-function
          ghostel-progress-function #'ghostherd--on-progress)))

(defun ghostherd--remove-hooks ()
  "Undo `ghostherd--install-hooks'.
Restoring `ghostel-progress-function' matters more than the rest: it is
a single global function, not a hook, so leaving ours in place would
keep someone else's spinner permanently displaced."
  (remove-hook 'ghostel-exit-functions #'ghostherd--on-ghostel-exit)
  (remove-hook 'ghostel-command-start-functions #'ghostherd--on-command-start)
  (remove-hook 'ghostel-command-finish-functions #'ghostherd--on-command-finish)
  (advice-remove 'ghostel-copy-mode #'ghostherd--copy-mode-sync-evil)
  (advice-remove 'ghostel-readonly-exit #'ghostherd--copy-mode-sync-evil)
  (when (and (boundp 'ghostel-progress-function)
             (eq ghostel-progress-function #'ghostherd--on-progress))
    (setq ghostel-progress-function ghostherd--saved-progress-function
          ghostherd--saved-progress-function nil)))

(defun ghostherd--register-eval-cmds ()
  "Whitelist ghostherd commands for `ghostel_cmd' from agent shells."
  (when (boundp 'ghostel-eval-cmds)
    (dolist (entry '(("ghostherd-list" ghostherd-cmd-list)
                     ("ghostherd-send" ghostherd-cmd-send)
                     ("ghostherd-prompt" ghostherd-cmd-prompt)
                     ("ghostherd-message" ghostherd-cmd-message)
                     ("ghostherd-read" ghostherd-cmd-read)
                     ("ghostherd-state" ghostherd-cmd-state)
                     ("ghostherd-report" ghostherd-cmd-report)
                     ("ghostherd-memory-search" ghostherd-cmd-memory-search)
                     ("ghostherd-memory-import" ghostherd-cmd-memory-import)
                     ("ghostherd-memory-list" ghostherd-cmd-memory-list)
                     ("ghostherd-memory-chunks" ghostherd-cmd-memory-chunks)))
      (unless (assoc (car entry) ghostel-eval-cmds)
        (add-to-list 'ghostel-eval-cmds entry)))))


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
          :started-by (ghostherd-session-started-by session))))

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
    (remhash id ghostherd--started-by)
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


;;; Inter-agent communication

(defcustom ghostherd-submit-check-after 1.5
  "Seconds after submitting text to make sure it left the input box, or nil.
The Return that submits a paste can be lost (see
`ghostherd-submit-delay'); the text then sits in the agent's input box
while the herd counts it as sent -- an ask delivered that nobody will
ever answer.  Seeing it still there, on an agent neither working nor
on a menu, ghostherd presses Return again, up to
`ghostherd-submit-retries' times, and logs it."
  :type '(choice (const :tag "Never look" nil) number)
  :group 'ghostherd)

(defcustom ghostherd-submit-retries 2
  "How many more times to press Return on text left in an input box."
  :type 'natnum
  :group 'ghostherd)

(defcustom ghostherd-submit-looks 8
  "How many more looks a submit gets while it is settled neither way:
its CLI still taking the paste in, or at work with the paste still in
its box.  `ghostherd-submit-check-after' apart, about 12 seconds."
  :type 'natnum
  :group 'ghostherd)

(defvar ghostherd--submits (make-hash-table :test 'equal)
  "Session id -> the last text the herd submitted to it, until it is known
to have left the input box: (:text TEXT :at TIME :state STATE :resting
R :looks N :clear N :retried R).  STATE is `pending'; `confirmed' once its
CLI's hook says a turn began, or the screen shows the box let it go;
`unsent' once Return was pressed on it again and again, or its looks
ran out with it still there -- that one shows on the agent's card and
in the Inbox; `lost' once its looks ran out with the box clear and no
turn begun: the CLI never had it.  RESTING: the agent was not at work
when it was sent, so a hook's word that a turn began is about this
text -- and for a prompt to a resting agent only that, or its screen at
work, confirms it: a box clear is no proof the text was ever in it.

A delivery is confirmed, not assumed: an image path in a paste makes
claude take it in as \"[Image #N]\" for a moment, and the Return the
herd pressed in that moment was lost (ask 2b76d37d sat unsent for 7
minutes).  And a dialog takes no paste: grok's folder-trust prompt let
an ask's text go by and took the Return after it as \"Yes, proceed\";
the input box grok drew next was clear (ask c644aebc).")

(defun ghostherd--submit-of (session)
  "The text the herd last submitted to SESSION, as `ghostherd--submits' has it."
  (gethash (ghostherd-session-id session) ghostherd--submits))

(defun ghostherd--submit-pending-p (session)
  "Non-nil while the text the herd submitted to SESSION is not known taken."
  (memq (plist-get (ghostherd--submit-of session) :state) '(pending unsent)))

(defun ghostherd--submit-confirmed (session why)
  "SESSION took the text the herd submitted; WHY says how that is known.
The ask it may hold now starts its turn (`ghostherd--turn-submitted').
Logged only when it had needed a Return again, or was given up on."
  (when-let* ((sub (ghostherd--submit-of session))
              ((ghostherd--submit-pending-p session)))
    (let ((late (or (plist-get sub :retried) (eq (plist-get sub :state) 'unsent))))
      (plist-put sub :state 'confirmed)
      (ghostherd--turn-submitted session)
      (when late
        (ghostherd--log-add session 'input (format "↵ submitted: %s" why))))))

(defun ghostherd--submit-unsent (session text screen)
  "Give up on TEXT, still in SESSION's input box on SCREEN: say so on its
card and in the Inbox, and fail the ask it holds."
  (ghostherd--log-add session 'input "← still in its input box: not submitted" screen)
  (when-let* ((sub (ghostherd--submit-of session))
              ((equal (plist-get sub :text) text)))
    (plist-put sub :state 'unsent))
  ;; An ask it was: fail it, so the asker hears now rather than waiting
  ;; on an agent that never read it.
  (when-let* ((id (ghostherd--ask-in session text)))
    (ghostherd--ask-settle
     id session nil
     (format "not submitted: it sat in %s's input box"
             (ghostherd-session-name session)))))

(defvar ghostherd--turn-ask-pending)

(defun ghostherd--submit-lost (session text screen)
  "Give up on TEXT, which SESSION's CLI never had: its box clear on SCREEN,
and no turn begun -- by its hook or its screen -- in all the looks it
got.  Fail the ask it held, saying so, rather than leave it to be
answered by a screen that never read it; the asker sends it again.
Nothing is left to press Return on, so the card shows nothing."
  (ghostherd--log-add session 'input "← never taken: no turn began on it" screen)
  (when-let* ((sub (ghostherd--submit-of session))
              ((equal (plist-get sub :text) text)))
    (plist-put sub :state 'lost))
  ;; A lead owed its role read none of it: the next message carries it.
  (ghostherd--role-give-back session text)
  (when-let* ((id (ghostherd--ask-in session text)))
    ;; Its turn will not come: the next one is not the ask's.
    (remhash (ghostherd-session-id session) ghostherd--turn-ask-pending)
    (ghostherd--ask-settle
     id session nil
     (format "not taken: %s's CLI started no turn on it -- send it again"
             (ghostherd-session-name session)))))

(defun ghostherd--submit-starts-turn-p (text)
  "Non-nil when TEXT, submitted, begins a turn: a prompt, not a CLI's own
slash command (`/model', `/usage'), which leaves its box clear and no
turn after it."
  (not (string-match-p "\\`[[:blank:]\n]*/" text)))

(defun ghostherd--submit-reported (session)
  "SESSION's CLI says a turn began (its prompt-submit hook): the text the
herd submitted while it rested reached it.  One sent to an agent at
work is left to the screen -- agy reports each step of a turn."
  (when-let* ((sub (ghostherd--submit-of session))
              ((plist-get sub :resting)))
    (ghostherd--submit-confirmed session "its CLI says its turn began")))

(defun ghostherd--submit-on-state (session _old _new)
  "An `unsent' text no longer in SESSION's input box -- you pressed Return,
or cleared it -- is no longer shown as unsent.  Read off the screen the
poll just took, so nothing is captured for it."
  (when-let* ((sub (ghostherd--submit-of session))
              ((eq (plist-get sub :state) 'unsent))
              (screen (gethash (ghostherd-session-id session) ghostherd--screens)))
    (unless (ghostherd--unsent-p screen (plist-get sub :text))
      (ghostherd--submit-confirmed session "its input box let it go"))))

(add-hook 'ghostherd-state-change-hook #'ghostherd--submit-on-state)

(defun ghostherd--submit-forget (session)
  "Drop SESSION's submit record when it is gone."
  (remhash (ghostherd-session-id session) ghostherd--submits))

(add-hook 'ghostherd-session-removed-hook #'ghostherd--submit-forget)

(defun ghostherd--unsent-head (session)
  "The first line of the text left unsent in SESSION's input box, or nil."
  (when-let* ((sub (ghostherd--submit-of session))
              ((eq (plist-get sub :state) 'unsent)))
    (car (split-string (plist-get sub :text) "\n" t "[ \t]+"))))

(defvar ghostherd--input-said nil
  "What the herd log records of the next input, when not its first line:
the user's own words, though the herd put a note before them
(`ghostherd-tell'), or the herd's words marked as its own.  The Threads
tab reads the log to say who set a thread going.")

(defun ghostherd-send (session text &optional submit)
  "Send TEXT to SESSION's terminal.
When SUBMIT is non-nil, also send RET (Enter)."
  (setq session (ghostherd-get session))
  (unless (ghostherd--session-live-p session)
    (user-error "Session host is not live"))
  (ghostherd--host-send-text session text submit)
  (puthash (ghostherd-session-id session) (float-time) ghostherd--input-at)
  ;; Logged before the state change, so the log reads in causal order:
  ;; what you sent, then what it did.
  (ghostherd--log-add session 'input
                      (format "← %s%s"
                              (ghostherd--clean-screen-line
                               (car (split-string (or ghostherd--input-said text) "\n" t)))
                              (if submit "" "  (not submitted)")))
  ;; Recorded before the state change, which settles an ask's turn
  ;; origin: until its CLI takes it, an ask's flag waits for the real
  ;; start (`ghostherd--turn-origin-on-state').
  (when (and submit ghostherd-submit-check-after)
    (puthash (ghostherd-session-id session)
             (list :text text :at (float-time) :state 'pending
                   :resting (not (memq (ghostherd-session-state session) '(working blocked)))
                   :looks ghostherd-submit-looks :clear 0 :retried nil)
             ghostherd--submits))
  (unless (ghostherd-session-manual-state session)
    (ghostherd--set-state session 'working "input sent"))
  (when (and submit ghostherd-submit-check-after)
    (run-at-time ghostherd-submit-check-after nil #'ghostherd--confirm-submit
                 session text ghostherd-submit-retries))
  text)

(defun ghostherd--input-box (screen)
  "The lines of SCREEN's input box, between its last two rules; or nil."
  (let* ((lines (split-string screen "\n"))
         (rules (cl-loop for l in lines for i from 0
                         when (string-match-p ghostherd--input-rule-re l) collect i)))
    (when (cdr rules)
      (cl-subseq lines (1+ (car (last rules 2))) (car (last rules))))))

(defconst ghostherd--paste-fold-re
  (concat "\\[\\(?:Pasted text #[0-9]+\\(?: \\+[0-9]+ lines\\| [0-9]+ chars\\)?"
          "\\|Image #[0-9]+\\(?::[^]\n]*\\)?"
          "\\|\\.\\.\\.Truncated text #[0-9]+ \\+[0-9]+ lines\\.\\.\\."
          "\\|✦ Team setup guide #[0-9]+\\(?: \\+[0-9]+ lines\\)?\\)\\]")
  "What a CLI shows in its input box in place of a paste it took in.
claude and agy fold a big paste (\"[Pasted text #2 +20 lines]\", agy's
\"[Pasted text #2 1234 chars]\"); claude and grok turn a picture's path
into \"[Image #1]\" (grok's \"[Image #1: name]\").  claude matches its own
the same way.  An image path in a paste comes first: the incident text
\"[Image #80][Pasted text #81 +62 lines]\" was a screenshot path, then
the rest of the message.")

(defconst ghostherd--image-path-re
  "\\.\\(?:png\\|jpe?g\\|gif\\|webp\\|bmp\\|heic\\)\\_>"
  "A picture's file name in pasted text, which claude and grok attach.")

(defconst ghostherd--pasting-re "^[[:blank:]]*Pasting…"
  "claude's footer while it takes a paste in.  The box is empty then, and
a Return pressed meanwhile is lost; what the box shows next is the
paste's fold.")

(defun ghostherd--folded-paste-p (in text)
  "Non-nil when IN, an input-box line, starts with the fold of a paste of
TEXT.  A picture's fold only when TEXT names a picture, so one you
pasted yourself is not taken for the herd's."
  (and (string-match-p (concat "\\`" ghostherd--paste-fold-re) in)
       (or (not (string-prefix-p "[Image #" in))
           (let ((case-fold-search t))
             (string-match-p ghostherd--image-path-re text)))))

(defun ghostherd--unsent-p (screen text)
  "Non-nil when SCREEN's input box still holds TEXT, pasted but not submitted.
Only TEXT counts -- a CLI's fold of it (`ghostherd--paste-fold-re'), or
its first words -- so a Return pressed for it never submits something
you were typing."
  (let* ((first (car (split-string text "\n" t "[ \t]+")))
         (head (and first (substring first 0 (min 20 (length first))))))
    (seq-some (lambda (line)
                ;; claude follows its ❯ with a no-break space.
                (and (string-match "^[[:blank:]│|]*[❯›>][[:blank:]]+\\(.*\\)" line)
                     (let ((in (match-string 1 line)))
                       (or (ghostherd--folded-paste-p in text)
                           (and head (string-search head in))))))
              (ghostherd--input-box screen))))

(defun ghostherd--pasting-p (screen)
  "Non-nil when SCREEN's footer, under its input box, shows its CLI still
taking a paste in.  Not the words up in the conversation."
  (let* ((lines (split-string screen "\n"))
         (rule (cl-loop for l in lines for i from 0
                        when (string-match-p ghostherd--input-rule-re l) maximize i))
         (case-fold-search nil))
    (and rule
         (seq-some (lambda (l) (string-match-p ghostherd--pasting-re l))
                   (nthcdr (1+ rule) lines)))))

(defun ghostherd--confirm-submit (session text tries)
  "Make sure TEXT, pasted into SESSION with Return, left its input box.
Its CLI's hook saying a turn began settles it (`ghostherd-report'), and
so does the screen: the turn at work, or the box clear on two looks
running -- but not for a prompt to a resting agent whose hooks report:
that waits for its turn to begin, and is `lost' when none does in all
its looks.  Still in the box of an agent neither working nor on a menu --
Return there is an answer -- it gets Return again, TRIES more times;
then it is `unsent', on the card and in the Inbox, and an ask in it
fails.  Still being taken in (claude's \"Pasting…\"), or in the box of
an agent at work, it is looked at again, `ghostherd-submit-looks' times
at most."
  (condition-case err
      (let ((sub (ghostherd--submit-of session)))
        (when (and (eq (gethash (ghostherd-session-id session) ghostherd--sessions) session)
                   (ghostherd--session-live-p session)
                   ;; Settled already, or a newer text took its place.
                   (or (null sub)
                       (and (eq (plist-get sub :state) 'pending)
                            (equal (plist-get sub :text) text))))
          (let* ((spec (ignore-errors (ghostherd--spec (ghostherd-session-kind session))))
                 (rules (plist-get spec :screen-rules))
                 (screen (and rules (ghostherd--host-capture session 20)))
                 (hit (and screen (ghostherd--match-rules screen rules (plist-get spec :prompt-replaces-input)
                                                          (plist-get spec :spinner))))
                 (busy (memq (car-safe hit) '(working blocked)))
                 (unsent (and screen (ghostherd--unsent-p screen text)))
                 (again (lambda (n)
                          (run-at-time ghostherd-submit-check-after nil #'ghostherd--confirm-submit
                                       session text n))))
            (cond
             ((null screen) nil)
             ((and unsent (not busy))
              (if (<= tries 0)
                  (ghostherd--submit-unsent session text screen)
                (ghostherd--host-send-keys session '("return"))
                (ghostherd--log-add session 'input "↵ again: it was still in the input box" screen)
                (when sub (plist-put sub :retried t))
                (funcall again (1- tries))))
             ;; Settled neither way: at work with it still in the box, or
             ;; its CLI still taking the paste in -- the box is empty then.
             ((or unsent (ghostherd--pasting-p screen))
              (cond ((and sub (> (plist-get sub :looks) 0))
                     (plist-put sub :looks (1- (plist-get sub :looks)))
                     (funcall again tries))
                    ((and sub unsent) (ghostherd--submit-unsent session text screen))))
             (busy (ghostherd--submit-confirmed session "its turn is under way"))
             ;; A turn its hooks said began after the send -- one so quick
             ;; its `working' and `done' reached the herd in one tick, which
             ;; carries the newest report only, so `working' never came
             ;; here.  Already over, its stop left the ask open while the
             ;; text was not known taken: that stop is read now.
             ((and sub (plist-get sub :resting)
                   (ghostherd--turn-began-since session (plist-get sub :at)))
              (ghostherd--submit-confirmed session "its hooks say its turn began")
              (when (memq (ghostherd-session-state session) '(idle done))
                (ghostherd--ask-on-state session nil (ghostherd-session-state session))))
             ;; Clear, and no turn begun.  For a prompt to an agent at rest
             ;; whose hooks report, that is no proof it was ever in the box:
             ;; a dialog takes no paste, and the box drawn after it is clear
             ;; (ask c644aebc).  Only the turn beginning confirms it -- its
             ;; hook, its screen at work; none in all its looks, the CLI
             ;; never had it.  An agent with no hook to say so keeps the
             ;; box's word, and a view scrolled back tells nothing.
             ((and sub (plist-get sub :resting)
                   (ghostherd--submit-starts-turn-p text)
                   (ghostherd--hooked-p session)
                   (not (ghostherd--scrolled-back-p screen (plist-get spec :scrolled))))
              (if (> (plist-get sub :looks) 0)
                  (progn (plist-put sub :looks (1- (plist-get sub :looks)))
                         (funcall again tries))
                (ghostherd--submit-lost session text screen)))
             ;; Clear: once more before it counts as taken, as a CLI may
             ;; clear its box a moment before it shows the paste's fold.
             ((and sub (< (plist-get sub :clear) 1))
              (plist-put sub :clear 1)
              (funcall again tries))
             (t (ghostherd--submit-confirmed session "its input box let it go"))))))
    (error (message "ghostherd: checking a submit: %s" (error-message-string err)))))

;;;###autoload
(defun ghostherd-submit-again (session)
  "Press Return for the message the herd left unsent in SESSION's input box.
Only while it is verifiably still there and SESSION is neither working
nor on a menu, so it never submits anything else; then it is watched
as a fresh submit."
  (interactive (list (ghostherd--read-session "Submit again: ")))
  (setq session (ghostherd-get session))
  (let* ((name (ghostherd-session-name session))
         (sub (or (and (eq (plist-get (ghostherd--submit-of session) :state) 'unsent)
                       (ghostherd--submit-of session))
                  (user-error "Nothing the herd sent sits unsent in %s's input box" name)))
         (text (plist-get sub :text))
         (spec (ignore-errors (ghostherd--spec (ghostherd-session-kind session))))
         (screen (ghostherd--host-capture session 20))
         (hit (and screen (plist-get spec :screen-rules)
                   (ghostherd--match-rules screen (plist-get spec :screen-rules)
                                           (plist-get spec :prompt-replaces-input)
                                           (plist-get spec :spinner)))))
    (cond
     ((not (ghostherd--unsent-p screen text))
      (ghostherd--submit-confirmed session "its input box let it go")
      (user-error "It is no longer in %s's input box" name))
     ((memq (car-safe hit) '(working blocked))
      (user-error "%s is at work or on a menu: Return there is an answer" name))
     (t
      (ghostherd--host-send-keys session '("return"))
      (ghostherd--log-add session 'input "↵ again, as asked: it was still in the input box" screen)
      (plist-put sub :state 'pending)
      (plist-put sub :looks ghostherd-submit-looks)
      (plist-put sub :clear 0)
      (plist-put sub :retried t)
      (when ghostherd-submit-check-after
        (run-at-time ghostherd-submit-check-after nil #'ghostherd--confirm-submit
                     session text ghostherd-submit-retries))
      name))))

;;;###autoload
(defun ghostherd-send-keys (session &rest keys)
  "Send control KEYS to SESSION -- Escape, C-c, arrows, Tab and so on.

`ghostherd-send' can only paste text and optionally press Return, which
covers talking *to* an agent but not operating its interface.  Escaping
a runaway generation, answering an arrow-driven permission menu or
sending EOF are keystrokes, not text: pasting the characters \"esc\"
just feeds the agent a word.

Unlike `ghostherd-send', this does not mark the session `working'.  The
usual reason to send a key is to make the agent *stop*, so claiming it
just started would be backwards; the next poll reports what actually
happened."
  (setq session (ghostherd-get session))
  (unless (ghostherd--session-live-p session)
    (user-error "Session host is not live"))
  (ghostherd--host-send-keys session keys)
  keys)

;;;###autoload
(defun ghostherd-interrupt (session)
  "Send Escape to SESSION -- what agent CLIs bind to \"esc to interrupt\"."
  (interactive (list (ghostherd--read-session "Interrupt agent: ")))
  (ghostherd-send-keys session "esc"))

;;;###autoload
(defun ghostherd-abort (session)
  "Send C-c to SESSION, for when Escape is not enough."
  (interactive (list (ghostherd--read-session "Abort agent: ")))
  (ghostherd-send-keys session "C-c"))

(defvar ghostherd--answer-look-every 0.2
  "Seconds between looks for an answer's highlight on its choice.")

(defvar ghostherd--answer-looks 5
  "Looks more for an answer's highlight before it is given up.")

;;;###autoload
(defun ghostherd-answer (session choice)
  "Answer SESSION's menu with CHOICE, 1-based: move there, then press
Return once the screen shows the highlight on it.

The moves count from the choice highlighted now, read off the screen.
Counting from the top, after enough Ups to get there, picks wrong on a
menu that wraps around -- agy's does: nine Ups from its first of four
choices land on its last, and Yes became \"No, cancel\".  Arrows rather
than the digit, which not every CLI takes; and Return only on the
screen's word, so moves that went astray leave the menu unanswered
rather than answered wrong.  Both outcomes are logged."
  (interactive
   (list (ghostherd--read-session "Answer agent: ")
         (read-number "Choice (1-based): " 1)))
  (setq session (ghostherd-get session))
  (unless (ghostherd--session-live-p session)
    (user-error "Session host is not live"))
  (let* ((name (ghostherd-session-name session))
         (menu (ghostherd--prompt (ghostherd--host-capture session)))
         (on (plist-get menu :on)))
    (cond ((not menu) (user-error "%s is on no menu with numbers" name))
          ((not (assq choice (plist-get menu :options)))
           (user-error "%s's menu has no choice %s" name choice))
          ((not on) (user-error "Which of %s's choices is highlighted does not show" name)))
    (when (/= choice on)
      (ghostherd--host-send-keys session (make-list (abs (- choice on)) (if (> choice on) "down" "up"))))
    (run-at-time ghostherd--answer-look-every nil #'ghostherd--confirm-answer
                 session choice ghostherd--answer-looks)
    choice))

(defun ghostherd--confirm-answer (session choice looks)
  "Press Return on SESSION's menu once its highlight is on CHOICE,
looking LOOKS more times while it is not."
  (condition-case err
      (when (and (eq (gethash (ghostherd-session-id session) ghostherd--sessions) session)
                 (ghostherd--session-live-p session))
        (let* ((screen (ghostherd--host-capture session))
               (menu (ghostherd--prompt screen))
               (on (plist-get menu :on)))
          (cond ((eql on choice)
                 (ghostherd--host-send-keys session '("return"))
                 (ghostherd--log-add session 'input
                                     (format "← %d. %s" choice
                                             (alist-get choice (plist-get menu :options)))))
                ((> looks 0)
                 (run-at-time ghostherd--answer-look-every nil #'ghostherd--confirm-answer
                              session choice (1- looks)))
                (t
                 (ghostherd--log-add session 'input
                                     (if on
                                         (format "%d not answered: the highlight stayed on %d" choice on)
                                       (format "%d not answered: the menu went" choice))
                                     screen)))))
    (error (message "ghostherd: answering: %s" (error-message-string err)))))

(defun ghostherd-prompt (session prompt &optional wait timeout)
  "Submit PROMPT to SESSION (text + Enter).
When WAIT is non-nil, poll until state is idle/done/blocked or TIMEOUT
seconds elapse (default 120)."
  (ghostherd-send session prompt t)
  (when wait
    (ghostherd-wait session '(idle done blocked) (or timeout 120)))
  session)

(defcustom ghostherd-image-directory (locate-user-emacs-file "ghostherd-images/")
  "Where an image pasted into an agent's prompt is saved.
The prompt carries the file's name, not the image: every CLI can read
a file, while only claude takes an image from the terminal.  The
agent's transcript keeps that name, so the file outlives the paste."
  :type 'directory
  :group 'ghostherd)

(defcustom ghostherd-image-keep-days 30
  "Days a pasted image is kept, or nil to keep them all.
Older ones go when the next image is saved."
  :type '(choice (const :tag "Keep them all" nil) natnum)
  :group 'ghostherd)

(defcustom ghostherd-clipboard-image-commands
  '(("pngpaste" "-")
    ("wl-paste" "--no-newline" "--type" "image/png")
    ("xclip" "-selection" "clipboard" "-t" "image/png" "-o"))
  "Commands that print the clipboard's image as PNG; the first installed is used."
  :type '(repeat (repeat string))
  :group 'ghostherd)

(defun ghostherd--prune-images (dir keep)
  "Remove pasted images in DIR older than `ghostherd-image-keep-days', but KEEP."
  (when ghostherd-image-keep-days
    (let ((before (- (float-time) (* ghostherd-image-keep-days 86400))))
      (dolist (f (directory-files dir t "\\.\\(png\\|jpg\\)\\'" t))
        (unless (equal f keep)
          (when-let* ((attrs (file-attributes f)))
            (when (< (float-time (file-attribute-modification-time attrs)) before)
              (ignore-errors (delete-file f)))))))))

(defun ghostherd-save-image (bytes &optional name)
  "Save BYTES, a PNG or a JPEG, as an image file, and return its name.
NAME, an agent's, begins the file's name.  The file is readable only by
you: a screenshot holds whatever was on the screen."
  (let* ((ext (cond ((string-prefix-p "\x89PNG\r\n\x1a\n" bytes) "png")
                    ((string-prefix-p "\xff\xd8\xff" bytes) "jpg")
                    (t (user-error "Not a PNG or JPEG image"))))
         (dir (file-name-as-directory (expand-file-name ghostherd-image-directory)))
         (stem (replace-regexp-in-string "[^[:alnum:]._-]+" "-" (or name "")))
         (file (expand-file-name
                (format "%s-%s.%s" (if (string-empty-p stem) "image" stem)
                        (format-time-string "%Y%m%d-%H%M%S-%3N") ext)
                dir)))
    (with-file-modes #o700 (make-directory dir t))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert bytes)
      (let ((coding-system-for-write 'binary))
        (with-file-modes #o600 (write-region nil nil file nil 'silent))))
    (ghostherd--prune-images dir file)
    file))

(defun ghostherd-save-clipboard-image (&optional name)
  "Save the clipboard's image as a PNG, and return the file's name.
NAME, an agent's, begins the file's name; see `ghostherd-save-image'."
  (let ((cmd (or (seq-find (lambda (c) (executable-find (car c)))
                           ghostherd-clipboard-image-commands)
                 (user-error "Pasting an image needs %s (brew install pngpaste)"
                             (mapconcat #'car ghostherd-clipboard-image-commands " or ")))))
    (ghostherd-save-image
     (with-temp-buffer
       (set-buffer-multibyte nil)
       (let ((coding-system-for-read 'binary))
         (unless (and (eq (apply #'call-process (car cmd) nil '(t nil) nil (cdr cmd)) 0)
                      (string-prefix-p "\x89PNG\r\n\x1a\n" (buffer-string)))
           (user-error "No image on the clipboard")))
       (buffer-string))
     name)))

(defun ghostherd-read (session &optional n-lines)
  "Return the last N-LINES of SESSION's screen."
  (setq session (ghostherd-get session))
  (unless (ghostherd--session-live-p session)
    (user-error "Session host is not live"))
  (ghostherd--host-capture session (or n-lines ghostherd-screen-tail-lines)))

(defcustom ghostherd-wait-poll-interval 0.4
  "Seconds between checks in `ghostherd-wait' and `ghostherd-wait-output'."
  :type 'number
  :group 'ghostherd)

(defun ghostherd-wait (session states &optional timeout)
  "Block until SESSION state is a member of STATES or TIMEOUT seconds.
STATES is a list of symbols.  Returns the state or signals on timeout.
TIMEOUT nil means wait indefinitely (not recommended interactively)."
  (setq session (ghostherd-get session))
  (let ((deadline (and timeout (+ (float-time) timeout)))
        state)
    (while (progn
             (setq state (ghostherd-poll-session session))
             (and (not (memq state states))
                  (or (null deadline)
                      (< (float-time) deadline))))
      (sit-for ghostherd-wait-poll-interval))
    (unless (memq state states)
      (user-error "Timeout waiting for %s to become %s (was %s)"
                  (ghostherd-session-name session) states state))
    state))

(defun ghostherd-output-matches (session regexp &optional lines)
  "Return the text in SESSION's last LINES matching REGEXP, or nil.

Matching is case-sensitive: a regexp written to catch a prompt should
mean what it says.  `:screen-rules' now agree -- see
`ghostherd--rule-matches-p'."
  (setq session (ghostherd-get session))
  (when (ghostherd--session-live-p session)
    (let ((case-fold-search nil)
          (tail (ghostherd--host-capture session lines)))
      (when (string-match regexp tail)
        (match-string 0 tail)))))

;;;###autoload
(defun ghostherd-wait-output (session regexp &optional timeout noerror lines)
  "Block until REGEXP appears in SESSION's recent output, or TIMEOUT.

`ghostherd-wait' watches the state machine, which only knows the five
states the screen rules can produce.  Plenty of things worth waiting for
have no state of their own -- a test summary line, a specific error, the
agent naming the file it just wrote.

Returns the matched text.  On timeout, signals unless NOERROR, in which
case it returns nil; scripts branching on \"did it happen\" should not
have to wrap every call in `condition-case'.

LINES overrides how much of the tail is searched, defaulting to
`ghostherd-screen-tail-lines'.  TIMEOUT nil waits indefinitely."
  (setq session (ghostherd-get session))
  (let ((deadline (and timeout (+ (float-time) timeout)))
        found)
    ;; Check before sleeping: output can arrive between whatever triggered
    ;; the wait and the wait itself, and a first poll one interval late
    ;; would miss it.
    (while (and (not (setq found (ghostherd-output-matches
                                  session regexp lines)))
                (ghostherd--session-live-p session)
                (or (null deadline) (< (float-time) deadline)))
      (sit-for ghostherd-wait-poll-interval))
    (cond
     (found)
     (noerror nil)
     (t (user-error "Timeout waiting for %s to output %s"
                    (ghostherd-session-name session) regexp)))))

(cl-defun ghostherd-message (from to body &key (submit ghostherd-submit-on-message))
  "Send an attributed message from session FROM to session TO.
BODY is the payload.  SUBMIT defaults to `ghostherd-submit-on-message'.
FROM may be a session, session name, or free-form label such as \"user\"."
  (setq to (ghostherd-get to))
  (unless to
    (user-error "Unknown target session"))
  (let* ((from-session (and from (ghostherd-get from)))
         (from-name (cond
                     (from-session (ghostherd-session-name from-session))
                     ((and from (stringp from)) from)
                     (t "user")))
         (to-name (ghostherd-session-name to))
         (payload (format ghostherd-message-template from-name to-name body)))
    (ghostherd-send to payload submit)
    (message "Message %s → %s" from-name to-name)
    payload))

;;;###autoload
(defcustom ghostherd-handoff-settled-states '(blocked idle done dead)
  "States that end a `ghostherd-handoff' watch."
  :type '(repeat symbol)
  :group 'ghostherd)

(defcustom ghostherd-handoff-grace 4
  "Seconds after a handoff during which a settled state is not believed.

An agent does not start working the instant it is handed something: the
CLI has to wake up and print, and until it does the screen rules see the
same idle prompt they saw before.  Without this the watch would report
completion immediately, every time."
  :type 'number
  :group 'ghostherd)

(defvar ghostherd--handoff-watches (make-hash-table :test 'equal)
  "Active handoff watch timers, keyed by session id.")

(defun ghostherd--handoff-cancel (id)
  "Cancel any handoff watch on session ID."
  (when-let* ((timer (gethash id ghostherd--handoff-watches)))
    (cancel-timer timer)
    (remhash id ghostherd--handoff-watches)))

(defun ghostherd--handoff-tick (id deadline grace callback)
  "One poll of the handoff watch on session ID."
  (let ((session (ghostherd-get id))
        (now (float-time)))
    (condition-case error
        (cond
         ((null session) (ghostherd--handoff-cancel id))
         ((< now grace) nil)
         ((memq (ghostherd-poll-session session)
                ghostherd-handoff-settled-states)
          (ghostherd--handoff-cancel id)
          (funcall callback session (ghostherd-session-state session)))
         ((and deadline (>= now deadline))
          (ghostherd--handoff-cancel id)
          (funcall callback session 'timeout)))
      ;; A broken callback must not leave the timer running forever.
      (error
       (ghostherd--handoff-cancel id)
       (message "ghostherd handoff watch failed: %s"
                (error-message-string error))))))

;;;###autoload
(defun ghostherd-handoff (to text &optional from timeout callback)
  "Hand TEXT to agent TO, then watch it settle -- without blocking Emacs.

This is the pipeline primitive: message an agent, wait, report.  It is
deliberately asynchronous.  `ghostherd-wait' blocks in a `sit-for' loop,
which would freeze you out of the very editor you are meant to keep
working in while the other agent runs -- and orchestrating without
having to sit and watch a pane is the whole point.

FROM attributes the message (a session, a name, or a label like
\"user\").  CALLBACK receives (SESSION STATE), where STATE is the
settled state or the symbol `timeout'; it defaults to a notification.
TIMEOUT defaults to 600 seconds, nil means no deadline.

Handing off to a session that is already being watched replaces the
watch rather than stacking a second timer on it."
  (setq to (ghostherd-get to))
  (unless to
    (user-error "Unknown target session"))
  (let* ((id (ghostherd-session-id to))
         (timeout (if (eq timeout 'none) nil (or timeout 600)))
         (callback (or callback #'ghostherd--handoff-notify))
         (now (float-time)))
    (if from
        (ghostherd-message from to text :submit t)
      (ghostherd-send to text t))
    (ghostherd--handoff-cancel id)
    (puthash id
             (run-with-timer
              ghostherd-wait-poll-interval ghostherd-wait-poll-interval
              #'ghostherd--handoff-tick
              id
              (and timeout (+ now timeout))
              (+ now ghostherd-handoff-grace)
              callback)
             ghostherd--handoff-watches)
    to))

(defun ghostherd--handoff-notify (session state)
  "Default `ghostherd-handoff' callback: notify about SESSION and STATE."
  (ghostherd--notify
   (format "ghostherd · %s %s" (ghostherd-session-name session) state)
   (if (eq state 'timeout)
       "handoff timed out"
     (or (ghostherd-session-state-reason session) "handoff settled"))))

;;;###autoload
(defun ghostherd-handoff-interactive (to text)
  "Hand TEXT to agent TO from the current session, and watch it settle."
  (interactive
   (let ((to (ghostherd--read-session "Hand off to agent: ")))
     (list to (read-string (format "Message for %s: "
                                   (ghostherd-session-name to))))))
  (ghostherd-handoff to text (or (ghostherd-get (current-buffer)) "user")))

(defun ghostherd-message-interactive (to body)
  "Interactively message agent TO with BODY (from user or current session)."
  (interactive
   (list (ghostherd--read-session "Message to: ")
         (read-string "Message: ")))
  (let ((from (ghostherd-get (current-buffer))))
    (ghostherd-message from to body :submit t)))

;;;###autoload
(defun ghostherd-prompt-interactive (session prompt)
  "Interactively prompt SESSION with PROMPT."
  (interactive
   (list (ghostherd--read-session "Prompt agent: ")
         (read-string "Prompt: ")))
  (ghostherd-prompt session prompt)
  (message "Prompted %s" (ghostherd-session-name (ghostherd-get session))))


;;; Commands callable from agent shells via ghostel_cmd

(defun ghostherd--json (object)
  "Encode OBJECT as JSON.
`json-serialize' is built in since Emacs 27 and this package requires
28.1, so there is no json.el fallback to keep."
  (json-serialize object))

(defun ghostherd-session-as-alist (session)
  "Return SESSION as an alist ready for JSON encoding."
  (list (cons 'name (ghostherd-session-name session))
        (cons 'kind (symbol-name (ghostherd-session-kind session)))
        (cons 'state (symbol-name (ghostherd-session-state session)))
        (cons 'reason (or (ghostherd-session-state-reason session) ""))
        (cons 'project (or (ghostherd-session-project session) ""))
        (cons 'notes (or (ghostherd-session-notes session) ""))
        (cons 'age (ghostherd--age-string
                    (or (ghostherd-session-last-active session)
                        (ghostherd-session-started-at session))))))

(defun ghostherd--cmd-caller ()
  "Return the session on whose behalf a `ghostel_cmd' is running, or nil.

`ghostel_cmd' is dispatched from the VT parser of the terminal that
asked, so the caller is identifiable with no environment at all: its
buffer is the current one.  `bin/ghostherd' cannot use this --
emacsclient does not carry the caller's environment into the form it
evaluates, and the buffer it evaluates in is nobody's terminal -- so
there the shell substitutes `GHOSTHERD_SESSION' itself.  Two paths, one
answer."
  (ghostherd-get (current-buffer)))

(defun ghostherd--cmd-target (name)
  "Return NAME, or the calling session when NAME is \"self\"."
  (if (equal name "self")
      (or (ghostherd--cmd-caller)
          (user-error "self: this is not an agent's own terminal"))
    name))

(defun ghostherd-cmd-list (&rest _)
  "Return herd sessions as JSON, for agent shells.

JSON rather than the tab/equals text this used to emit, because two of
the fields are free-form and the old format had no escaping: a note
containing a tab or a newline silently turned one record into several,
and `reason' holds a raw `:screen-rules' regexp, which can contain
anything at all."
  (ghostherd--ensure-sessions)
  (ghostherd--json
   (vconcat (mapcar #'ghostherd-session-as-alist (ghostherd-sessions)))))

(defun ghostherd-cmd-state (name &rest _)
  "Return the state of session NAME as JSON.  NAME may be \"self\"."
  (ghostherd--json
   (if-let* ((s (ghostherd-get (ghostherd--cmd-target name))))
       (ghostherd-session-as-alist s)
     (list (cons 'error "unknown session")
           (cons 'name name)))))

(defun ghostherd-cmd-send (name text &rest _)
  "Send TEXT to session NAME without submitting."
  (ghostherd-send (ghostherd--cmd-target name) text nil)
  (format "sent to %s" name))

(defun ghostherd-cmd-prompt (name text &rest _)
  "Prompt session NAME with TEXT (submitted)."
  (ghostherd-prompt (ghostherd--cmd-target name) text)
  (format "prompted %s" name))

(defun ghostherd-cmd-message (to text &optional from &rest _)
  "Message TO with TEXT, optional FROM name.

FROM defaults to the calling agent rather than to \"user\": an agent's
message attributed to the human is what identity exists to stop."
  (ghostherd-message (or (and from (ghostherd--cmd-target from))
                         (ghostherd--cmd-caller)
                         "user")
                     (ghostherd--cmd-target to) text :submit t)
  (format "message → %s" to))

(defun ghostherd-cmd-report (name state &optional reason &rest _)
  "Record that session NAME reports itself in STATE, because REASON.
NAME may be \"self\", which is how a CLI hook is expected to call this."
  (ghostherd-report (ghostherd--cmd-target name) (intern state) reason)
  (format "%s reports %s" name state))

(defun ghostherd-cmd-read (name &optional n &rest _)
  "Read last N lines from session NAME.  NAME may be \"self\"."
  (ghostherd-read (ghostherd--cmd-target name)
                  (and n (string-to-number n))))


;;; Display helpers

(defun ghostherd--state-glyph (state)
  "Return a short glyph for STATE."
  (pcase state
    ('blocked "⚠")
    ('working "⟳")
    ('waiting "◷")
    ('done    "✓")
    ('idle    "○")
    ('starting "…")
    ('dead    "✖")
    (_        "?")))

(defun ghostherd--state-face (state)
  "Return a face symbol for STATE."
  (pcase state
    ('blocked 'error)
    ('working 'warning)
    ('waiting 'font-lock-constant-face)
    ('done    'success)
    ('dead    'shadow)
    ('starting 'font-lock-comment-face)
    (_        'default)))

(defcustom ghostherd-annotation-note-width 32
  "Characters of a session's note shown in the switcher before eliding."
  :type 'integer
  :group 'ghostherd)

(defun ghostherd--format-candidate (session)
  "Return the completing-read candidate string for SESSION.
Just the name: detail belongs in the annotation, where it is visually
distinct and does not silently widen what your input matches against."
  (ghostherd-session-name session))

(defun ghostherd--session-annotation (session)
  "Return the annotation suffix describing SESSION."
  (let* ((note (ghostherd-session-notes session))
         (note (and note (string-trim note)))
         (note (and note (not (string-empty-p note))
                    (truncate-string-to-width
                     note ghostherd-annotation-note-width nil nil t)))
         (parts (delq nil
                      (list (symbol-name (ghostherd-session-kind session))
                            (symbol-name (ghostherd-session-state session))
                            ;; A view, not a state: the agent is running
                            ;; and nobody is attached.  Worth saying here
                            ;; because this is where you pick one, and
                            ;; choosing it is what attaches.
                            (unless (buffer-live-p
                                     (ghostherd-session-buffer session))
                              "detached")
                            (ghostherd--age-string
                             (or (ghostherd-session-last-active session)
                                 (ghostherd-session-started-at session)))
                            (ghostherd--abbreviate
                             (ghostherd-session-project session))
                            note))))
    (concat "  " (propertize (string-join parts "  ")
                             'face 'completions-annotations))))

(defun ghostherd--session-affixation (candidates)
  "Affix CANDIDATES with a state glyph and `ghostherd--session-annotation'."
  (mapcar
   (lambda (name)
     (if-let* ((session (ghostherd-get name)))
         (let ((state (ghostherd-session-state session)))
           (list name
                 (propertize (concat (ghostherd--state-glyph state) " ")
                             'face (ghostherd--state-face state))
                 (ghostherd--session-annotation session)))
       (list name "" "")))
   candidates))

(defun ghostherd--session-completion-table (candidates)
  "Return a completion table over CANDIDATES carrying ghostherd metadata.
A table rather than a plain list so the category is advertised, which is
what lets consult and marginalia treat these as sessions."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        `(metadata (category . ghostherd-session)
                   (affixation-function . ghostherd--session-affixation))
      (complete-with-action action candidates string predicate))))

(defun ghostherd--age-string (time)
  "Human-readable age for TIME."
  (if (null time)
      "-"
    (let* ((secs (max 0 (floor (float-time (time-subtract (current-time) time)))))
           (m (floor secs 60))
           (h (floor m 60)))
      (cond
       ((>= h 1) (format "%dh%02dm" h (mod m 60)))
       ((>= m 1) (format "%dm" m))
       (t (format "%ds" secs))))))


(defcustom ghostherd-scrollback-lines 3000
  "Lines of history `ghostherd-scrollback' asks the host for."
  :type 'integer
  :group 'ghostherd)

;;;###autoload
(defun ghostherd-scrollback (session)
  "Show SESSION's history in an ordinary Emacs buffer.

The tmux backend took something away without meaning to.  A ghostel
buffer accumulates everything the agent ever printed, so scrolling back
is just moving point; behind tmux the buffer holds only the visible
pane, and the history is in tmux -- where `prefix None' leaves no way in
from the keyboard.

Putting it back as a tmux key binding would make tmux a third
navigation layer, which is the one thing this backend refuses to be.
Pulling the history into a buffer instead costs nothing and lands it
somewhere every Emacs motion already works.

A full-screen TUI keeps no history at all -- the alternate screen is not
saved -- so for those this shows the current screen and the agent's own
keys are what scroll it."
  (interactive (list (ghostherd--read-session "Scrollback of agent: ")))
  (setq session (ghostherd-get session))
  (let ((text (ghostherd--host-scrollback session ghostherd-scrollback-lines))
        (name (ghostherd-session-name session)))
    (unless (and text (not (string-empty-p (string-trim text))))
      (user-error "No history kept for %s" name))
    (ghostherd--sidebar-leave-overlay)
    (with-current-buffer (get-buffer-create (format "*ghostherd history: %s*" name))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)
        (goto-char (point-max)))
      (special-mode)
      (pop-to-buffer (current-buffer)))))


(defcustom ghostherd-scroll-lines 3
  "Lines moved per scroll step in an attached agent view."
  :type 'integer
  :group 'ghostherd)

(defun ghostherd--scroll (lines)
  "Scroll the agent viewed in the current buffer back by LINES."
  (if (eq ghostel--input-mode 'copy)
      ;; The client is frozen; driving tmux would move a host this
      ;; buffer no longer shows.
      (if (> lines 0) (scroll-down lines) (scroll-up (- lines)))
    (let ((session (ghostherd-get (current-buffer))))
      (unless session
        (user-error "Not a ghostherd agent buffer"))
      (unless (ghostherd--host-scroll session lines)
        ;; A host that does not scroll its own view keeps its output in the
        ;; buffer, where Emacs has always been able to scroll it.
        (if (> lines 0) (scroll-down lines) (scroll-up (- lines)))))))

;;;###autoload
(defun ghostherd-scroll-up (&optional lines)
  "Scroll the agent view back into its history by LINES."
  (interactive "p")
  (ghostherd--scroll (* (or lines 1) ghostherd-scroll-lines)))

;;;###autoload
(defun ghostherd-scroll-down (&optional lines)
  "Scroll the agent view forward, back towards the live screen."
  (interactive "p")
  (ghostherd--scroll (- (* (or lines 1) ghostherd-scroll-lines))))

;;;###autoload
(defun ghostherd-scroll-page-up ()
  "Scroll the agent view back by roughly a screen."
  (interactive)
  (ghostherd--scroll (max 1 (- (window-body-height) 2))))

;;;###autoload
(defun ghostherd-scroll-page-down ()
  "Scroll the agent view forward by roughly a screen."
  (interactive)
  (ghostherd--scroll (- (max 1 (- (window-body-height) 2)))))

;;;###autoload
(defun ghostherd-scroll-bottom ()
  "Return the agent view to the live screen.

Scrolls forward far enough to hit the bottom, which is also what makes
tmux leave copy mode -- so this is \"put me back where I can type\"
rather than a mode command."
  (interactive)
  (let ((session (ghostherd-get (current-buffer))))
    (unless session
      (user-error "Not a ghostherd agent buffer"))
    (unless (ghostherd--host-scroll session (- (* 1000 ghostherd-scroll-lines)))
      (goto-char (point-max)))))

(defun ghostherd--copy-mode-sync-evil (&rest _)
  "Use Evil normal state in copy mode so search keys do not exit it.

`ghostel-readonly-fast-exit' leaves copy mode on any self-insert.
Evil emacs state makes `?' and `/' self-insert, so the first key
meant to search would drop copy mode -- which is how it looked
impossible to enter."
  (when (and (derived-mode-p 'ghostel-mode)
             (bound-and-true-p ghostherd-session-id)
             (bound-and-true-p evil-local-mode)
             (fboundp 'evil-normal-state)
             (fboundp 'evil-emacs-state))
    (if (eq ghostel--input-mode 'copy)
        (evil-normal-state)
      (evil-emacs-state))))

;;;###autoload
(defun ghostherd-copy-mode ()
  "Toggle ghostel copy mode in this agent view.

Bound to `C-z' so it wins against Evil's emacs-state map, which
would otherwise take `C-z' for `evil-exit-emacs-state' and leave
the terminal live."
  (interactive)
  (unless (derived-mode-p 'ghostel-mode)
    (user-error "Not a ghostel buffer"))
  (call-interactively #'ghostel-copy-mode))

(defvar-keymap ghostherd-terminal-mode-map
  :doc "Scrolling for an attached agent view, in the vocabulary Emacs uses."
  "<wheel-up>"     #'ghostherd-scroll-up
  "<wheel-down>"   #'ghostherd-scroll-down
  "<mouse-4>"      #'ghostherd-scroll-up
  "<mouse-5>"      #'ghostherd-scroll-down
  "<prior>"        #'ghostherd-scroll-page-up
  "<next>"         #'ghostherd-scroll-page-down
  "M-v"            #'ghostherd-scroll-page-up
  "C-M-v"          #'ghostherd-scroll-page-down
  "M->"            #'ghostherd-scroll-bottom
  "C-z"            #'ghostherd-copy-mode)

(defun ghostherd--bind-evil-copy-mode ()
  "Bind `C-z' in Evil emacs/normal so it is not `evil-exit-emacs-state'.

Must call `evil-define-key*', the function.  `evil-define-key' is
a macro; if this file is compiled without evil loaded the call is
left as a function and startup dies with `Invalid function'."
  (when (fboundp 'evil-define-key*)
    (evil-define-key* 'emacs ghostherd-terminal-mode-map
      (kbd "C-z") #'ghostherd-copy-mode)
    (evil-define-key* 'normal ghostherd-terminal-mode-map
      (kbd "C-z") #'ghostherd-copy-mode)))

(with-eval-after-load 'evil
  (ghostherd--bind-evil-copy-mode))

;; So reloading this file is enough; `ghostherd-mode' still adds/removes
;; the same pair around its own lifetime.
(advice-add 'ghostel-copy-mode :after #'ghostherd--copy-mode-sync-evil)
(advice-add 'ghostel-readonly-exit :after #'ghostherd--copy-mode-sync-evil)

;;;###autoload
(define-minor-mode ghostherd-terminal-mode
  "Make an attached agent view scroll like an ordinary buffer.

Only needed where the buffer is a *client*.  On the ghostel backend the
buffer holds everything the agent printed and Emacs has always scrolled
it; behind tmux it holds one screen, and the history is in the host --
so the wheel, PageUp/PageDown and \[ghostherd-scroll-page-up] drive the
host's own view instead, and the same buffer shows older output.

The mechanism is tmux copy mode, driven by command rather than by a
prefix key, so it never becomes something to learn or get stuck in:
scrolling back to the bottom leaves it automatically.  Ghostel copy
mode is separate: `C-z' (and ghostel's own `C-c C-t') freeze the
current screen to select text.  That is not history -- overlay `H'
is.

Evil users in normal state will want the commands bound there too --
`ghostherd-scroll-up\=', `ghostherd-scroll-down\=' and the two page
commands are the whole surface.

Bind them out of the terminal's own vocabulary, though.  This map is on
the agent view, so anything bound here is a key the agent never sees,
and only behind tmux -- the ghostel backend has no such mode, so the
same key keeps working there and the loss looks like a backend bug.
`C-M-v\=' pages down above for that reason: `C-v\=' is how Claude Code
pastes an image."
  :lighter " ⇅"
  :keymap ghostherd-terminal-mode-map
  (when (and ghostherd-terminal-mode
             (bound-and-true-p evil-local-mode)
             (fboundp 'evil-emacs-state)
             (not (eq evil-state 'emacs)))
    ;; The buffer is a PTY.  Normal state would swallow the agent's own
    ;; keys; emacs state is the one this map was written for.
    (evil-emacs-state)))


;;; Herd log buffer

(defun ghostherd--log-kind-face (kind)
  "Face for a log entry of KIND."
  (pcase kind
    ('state  'default)
    ('input  'font-lock-string-face)
    ('report 'font-lock-keyword-face)
    ('ask    'font-lock-type-face)
    ('life   'shadow)
    (_       'default)))

(defvar-keymap ghostherd-log-mode-map
  :doc "Keymap for `ghostherd-log-mode'."
  "RET" #'ghostherd-log-show-screen
  "g"   #'ghostherd-log-refresh
  "C"   #'ghostherd-log-clear
  "q"   #'quit-window)

(define-derived-mode ghostherd-log-mode special-mode "GhostHerd-Log"
  "Chronological log of what the herd did."
  (setq truncate-lines t))

(defun ghostherd--log-render (buffer)
  "Draw the herd log into BUFFER."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (at-end (eobp))
          (point-was (point)))
      (erase-buffer)
      ;; Oldest first: a log is read downwards, and the interesting end is
      ;; the one you have not seen.
      (dolist (entry (reverse ghostherd--log))
        (let ((start (point)))
          (insert (format "%s  %-14s %s\n"
                          (format-time-string "%H:%M:%S"
                                              (ghostherd-log-entry-time entry))
                          (ghostherd-log-entry-session entry)
                          (propertize (or (ghostherd-log-entry-text entry) "")
                                      'face (ghostherd--log-kind-face
                                             (ghostherd-log-entry-kind entry)))))
          (when (ghostherd-log-entry-screen entry)
            (put-text-property start (point) 'ghostherd-log-entry entry)
            (save-excursion
              (goto-char (1- (point)))
              (insert (propertize "  ⏎" 'face 'shadow))))))
      (when (= (point-min) (point-max))
        (insert "Nothing logged yet.\n"))
      (goto-char (if at-end (point-max) (min point-was (point-max)))))))

(defun ghostherd-log-refresh ()
  "Redraw the herd log."
  (interactive)
  (ghostherd--log-render (current-buffer)))

(defun ghostherd-log-show-screen ()
  "Show the screen captured with the log entry at point.

This is the reason the log keeps them.  `ghostherd-explain' answers \"why
is it in that state\" only while the agent is still in it; a rule that
fired at 02:00 is otherwise unarguable by morning."
  (interactive)
  (let ((entry (get-text-property (point) 'ghostherd-log-entry)))
    (unless entry
      (user-error "No screen kept for this entry"))
    (with-help-window "*ghostherd screen*"
      (with-current-buffer standard-output
        (insert (format "%s  %s  %s\n"
                        (format-time-string "%F %T"
                                            (ghostherd-log-entry-time entry))
                        (ghostherd-log-entry-session entry)
                        (ghostherd-log-entry-text entry)))
        (insert (make-string 60 ?-) "\n")
        (insert (ghostherd-log-entry-screen entry))
        (unless (bolp) (insert "\n"))
        (insert (make-string 60 ?-) "\n")))))

;;;###autoload
(defun ghostherd-log ()
  "Show what the herd has been doing.

Every state transition and every prompt sent, with a timestamp.  Lines
marked =⏎= kept the screen that produced them; =RET= shows it.  The log
spans Emacs restarts, so entries from an earlier session are above the
line that says so; `C' forgets the lot."
  (interactive)
  ;; Reachable without `ghostherd-mode' having been enabled -- so this is
  ;; the second place the file gets read, and the flag is why it is not
  ;; read twice.
  (unless ghostherd--log-loaded
    (ghostherd-log-load))
  (let ((buf (get-buffer-create "*ghostherd-log*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'ghostherd-log-mode)
        (ghostherd-log-mode))
      (ghostherd--log-render buf))
    (ghostherd--sidebar-leave-overlay)
    (pop-to-buffer buf)))


;;; Sidebar

(defvar ghostherd--sidebar-filter-project nil
  "When non-nil, sidebar only shows this project root.")

(defvar ghostherd--sidebar-query ""
  "Live-narrow string for the session list.  Empty means everything.")

(defvar ghostherd--sidebar-filtering nil
  "Non-nil while `/` has the session list consuming keys as a query.")

(defvar ghostherd--sidebar-match-count 0
  "Rows visible after the live query.")

(defvar ghostherd--sidebar-total-count 0
  "Rows after the project filter, before the live query.")

(defvar-local ghostherd--sidebar-preview-start nil
  "Marker where the preview pane begins in the session-list buffer.")

(defvar ghostherd--sidebar-preview-id nil
  "Session id currently shown in the preview pane.")

(defvar ghostherd--sidebar-preview-timer nil
  "Debounce timer for a fresh capture when the selected row changes.")

(defvar ghostherd--sidebar-help-visible nil
  "Non-nil when the overlay preview pane is showing the key legend.")

(defvar ghostherd--sidebar-picking nil
  "Frozen (DISPLAY ROOT COUNT) candidates while the project picker is open.
Frozen because a poll landing mid-pick would otherwise renumber
the rows under the key you are about to press.  Nil when closed.")

(defvar ghostherd--sidebar-pick-index 0
  "Selected row in the project picker.")

(defvar ghostherd--sidebar-target-width nil
  "Column budget that wins over the live window's width.

Bound around a posframe open or refit so columns are laid out for
the size the overlay *will* be, not the size the child frame still
is.  Without it a resize would paint into the old width and then
stretch the frame, truncating Project one step behind.")

(defvar ghostherd--sidebar-posframe-parent nil
  "Parent frame to restore when the posframe overlay is dismissed.")

(defvar ghostherd--sidebar-posframe-fitted-width nil
  "Last overlay width fitted to the parent frame.")

(defvar ghostherd--sidebar-posframe-refitting nil
  "Non-nil while the overlay is being resized, to ignore nested hooks.")

(defconst ghostherd-sidebar-help-commands
  '((ghostherd-sidebar-visit                 . "Visit agent buffer")
    (next-line                               . "Next row")
    (previous-line                           . "Previous row")
    (ghostherd-new                           . "New agent")
    (ghostherd-new-pair                      . "New pair (split windows)")
    (ghostherd-sidebar-kill                  . "Kill session")
    (ghostherd-sidebar-rename                . "Rename session")
    (ghostherd-respawn                       . "Respawn from recipe")
    (ghostherd-relay                         . "Relay: an heir on another CLI")
    (ghostherd-sidebar-changes               . "What its checkout has not committed")
    (ghostherd-sidebar-notes                 . "Edit note / role")
    (ghostherd-sidebar-interrupt             . "Interrupt (Escape)")
    (ghostherd-sidebar-abort                 . "Abort (C-c)")
    (ghostherd-sidebar-answer                . "Answer menu prompt")
    (ghostherd-sidebar-message               . "Message agent")
    (ghostherd-sidebar-prompt                . "Prompt agent")
    (ghostherd-sidebar-project-filter        . "Filter by project")
    (ghostherd-sidebar-filter                . "Live-narrow (flex)")
    (ghostherd-sidebar-toggle-preview        . "Toggle screen preview")
    (ghostherd-sidebar-refresh               . "Refresh")
    (ghostherd-usage-refresh                 . "Refresh account usage")
    (ghostherd-next-blocked                  . "Next blocked / done")
    (ghostherd-sidebar-mark-state            . "Mark state (manual / auto)")
    (ghostherd-explain                       . "Explain how state was decided")
    (ghostherd-log                           . "Herd log (what happened while you were away)")
    (ghostherd-scrollback                    . "Scrollback (history the pane no longer shows)")
    (ghostherd-sidebar-help                  . "This help")
    (ghostherd-sidebar-quit                  . "Quit"))
  "Commands listed by `ghostherd-sidebar-help', in display order.")

(defvar ghostherd-sidebar-mode-map)

(defun ghostherd--sidebar-help-maps ()
  "Keymaps whose bindings count as overlay keys.
Global leader maps are excluded: `?' is a legend for this panel, not
for `SPC a h'."
  (let ((maps (list ghostherd-sidebar-mode-map)))
    (when (fboundp 'evil-get-auxiliary-keymap)
      (dolist (state '(normal emacs motion))
        (when-let* ((aux (ignore-errors
                           (evil-get-auxiliary-keymap
                            ghostherd-sidebar-mode-map state t t))))
          (push aux maps))))
    maps))

(defun ghostherd--sidebar-help-keys (command)
  "Return up to two overlay key descriptions for COMMAND.
Looks only in `ghostherd-sidebar-mode-map' (and Evil's auxiliary
maps on it).  `where-is-internal' with a nil map would also report
`SPC a h n', which is how the old `*ghostherd help*' buffer listed
leader keys and then dismissed the overlay to show them."
  (seq-take
   (delete-dups
    (delq nil
          (mapcan
           (lambda (map)
             (mapcar
              (lambda (key)
                (unless (and (> (length key) 0)
                             (symbolp (aref key 0))
                             (string-suffix-p "-state"
                                              (symbol-name (aref key 0))))
                  (key-description key)))
              (where-is-internal command map nil nil t)))
           (ghostherd--sidebar-help-maps))))
   2))

(defun ghostherd--sidebar-help-text ()
  "Return the overlay key legend as a string."
  (let* ((rows (delq nil
                     (mapcar
                      (lambda (entry)
                        (when-let* ((keys (ghostherd--sidebar-help-keys
                                           (car entry))))
                          (cons (string-join keys ", ") (cdr entry))))
                      ghostherd-sidebar-help-commands)))
         (width (apply #'max 3 (mapcar (lambda (row) (length (car row))) rows)))
         (line-format (format "%%-%ds  %%s" width)))
    (concat
     (mapconcat (lambda (row)
                  (format line-format (car row) (cdr row)))
                rows "\n")
     "\n\n"
     (mapconcat (lambda (state)
                  (format "%s %s"
                          (ghostherd--state-glyph state) state))
                '(blocked working waiting done idle starting dead)
                "  ")
     (format "\n%s detached (nobody attached; RET attaches)"
             ghostherd-detached-glyph))))

(defun ghostherd-sidebar-help ()
  "Toggle the key legend in the overlay preview pane.
Does not dismiss the overlay -- that was the old `*ghostherd help*'
window, which had nowhere to go except by killing the child frame."
  (interactive)
  (setq ghostherd--sidebar-help-visible
        (not ghostherd--sidebar-help-visible))
  (ghostherd--sidebar-draw-preview)
  (when (ghostherd--sidebar-posframe-showing-p)
    (ghostherd--sidebar-show-posframe (current-buffer)))
  (force-mode-line-update t))

(defvar-keymap ghostherd-sidebar-mode-map
  :doc "Keymap for `ghostherd-sidebar-mode'."
  "?" #'ghostherd-sidebar-help
  "e" #'ghostherd-explain
  "R" #'ghostherd-respawn
  "T" #'ghostherd-relay
  "d" #'ghostherd-sidebar-changes
  "c" #'ghostherd-sidebar-notes
  "z" #'ghostherd-sidebar-interrupt
  "Z" #'ghostherd-sidebar-abort
  "a" #'ghostherd-sidebar-answer
  "n" #'next-line
  "p" #'previous-line
  "RET" #'ghostherd-sidebar-visit
  "o" #'ghostherd-sidebar-visit
  "N" #'ghostherd-new
  "P" #'ghostherd-new-pair
  "k" #'ghostherd-sidebar-kill
  "r" #'ghostherd-sidebar-rename
  "m" #'ghostherd-sidebar-message
  "i" #'ghostherd-sidebar-prompt
  "s" #'ghostherd-sidebar-project-filter
  "/" #'ghostherd-sidebar-filter
  "v" #'ghostherd-sidebar-toggle-preview
  "g" #'ghostherd-sidebar-refresh
  "U" #'ghostherd-usage-refresh
  "q" #'ghostherd-sidebar-quit
  "C-g" #'ghostherd-sidebar-quit
  "<escape>" #'ghostherd-sidebar-quit
  "." #'ghostherd-next-blocked
  "H" #'ghostherd-scrollback
  "L" #'ghostherd-log
  "M" #'ghostherd-sidebar-mark-state)

(define-derived-mode ghostherd-sidebar-mode tabulated-list-mode "GhostHerd"
  "Listing of ghostherd agent sessions."
  ;; Padding first: `ghostherd--sidebar-format' budgets against it.
  (setq tabulated-list-padding 1)
  (setq tabulated-list-format (ghostherd--sidebar-format))
  (setq tabulated-list-sort-key (cons "Name" nil))
  (setq truncate-lines t)
  (setq-local mode-line-format '(" " (:eval (ghostherd--sidebar-footer))))
  (hl-line-mode 1)
  (setq-local ghostherd--sidebar-preview-start nil)
  (add-hook 'tabulated-list-revert-hook #'ghostherd--sidebar-entries nil t)
  (add-hook 'post-command-hook #'ghostherd--sidebar-preview-on-command nil t)
  (tabulated-list-init-header)
  (when (fboundp 'evil-normalize-keymaps)
    (evil-normalize-keymaps)))

(defun ghostherd--sidebar-footer ()
  "Mode-line hint row for the session list.
Remaining account quota (claude / grok / agy) sits in front of the
hints when `ghostherd-usage' has a cache."
  (let* ((query ghostherd--sidebar-query)
         (querying (or ghostherd--sidebar-filtering
                       (and query (not (string-empty-p query)))))
         (counts (when querying
                   (format " %d/%d"
                           ghostherd--sidebar-match-count
                           ghostherd--sidebar-total-count)))
         (usage (and (fboundp 'ghostherd-usage-line)
                     (ghostherd-usage-line
                      (ghostherd--sidebar-available-width))))
         (usage (and usage (not (string-empty-p usage)) usage)))
    (concat
     (or usage "")
     (if usage "  ·  " "")
     "GhostHerd"
     (when ghostherd--sidebar-filter-project
       (format " [%s]"
               (file-name-nondirectory
                (directory-file-name ghostherd--sidebar-filter-project))))
     (when querying (concat "  /" query))
     (or counts "")
     (cond
      (ghostherd--sidebar-picking
       "   RET choose  n/p move  Esc cancel")
      (ghostherd--sidebar-filtering
       "   RET apply  n/p move  Esc clear")
      (ghostherd--sidebar-help-visible
       "   ? close help  Esc close")
      (t
       "   RET visit  / filter  v preview  ? keys  Esc close")))))

(defconst ghostherd--sidebar-column-specs
  '((glyph   "S"        2 mandatory)
    (name    "Name"    14 mandatory)
    (state   "State"    8)
    (age     "Age"      5)
    (kind    "Kind"     6)
    (title   "Title"   20)
    (project "Project" 20))
  "Candidate sidebar columns as (KEY HEADER WIDTH [MANDATORY]).

Listed most useful first: `ghostherd--sidebar-visible-columns' takes them
in this order for as long as they fit.  The glyph and the name are
mandatory -- without them there is no list.  `project' comes last because
it is the widest and says least: sessions are usually all in one project,
and the project filter exists for when they are not.")

(defun ghostherd--sidebar-available-width ()
  "Columns the session list actually has: a bound budget, else the window.

`ghostherd--sidebar-target-width' wins when set: the overlay is sized
from the parent frame *before* the child frame exists (or while it is
still the old size during a refit).  Otherwise the live window, so a
side window follows a manual drag.  Last, the configured width."
  (or ghostherd--sidebar-target-width
      (when-let* ((buf (get-buffer "*ghostherd*"))
                  (win (get-buffer-window buf t)))
        (window-body-width win))
      ghostherd-sidebar-width))

(defun ghostherd--sidebar-visible-columns ()
  "Return the column specs that fit the sidebar's available width.

The widths used to be a fixed vector totalling ~70 columns against a
default sidebar width of 36, so every row wrapped.  Fit them to the
window instead, dropping the least useful columns rather than silently
overflowing."
  ;; No floor on the budget: one used to be here, but glyph + name always
  ;; cost less than it, so the mandatory flag below could never fire and a
  ;; deliberately tiny sidebar overflowed anyway.  Let the budget go small
  ;; and let `mandatory' be what guarantees a usable list.
  (let ((budget (1- (ghostherd--sidebar-available-width)))
        (used 0)
        (kept nil))
    (dolist (spec ghostherd--sidebar-column-specs)
      (when (or (not (eq (car spec) 'title)) ghostherd-sidebar-show-title)
        ;; tabulated-list draws `tabulated-list-padding' leading columns and
        ;; a separating space after every column but the last.
        (let ((cost (+ (nth 2 spec) (if kept 1 tabulated-list-padding))))
          (when (or (nth 3 spec) (<= (+ used cost) budget))
            (setq used (+ used cost))
            (push spec kept)))))
    (ghostherd--sidebar-grow-columns (nreverse kept) budget used)))

(defun ghostherd--sidebar-grow-columns (columns budget used)
  "Stretch project/name/title so leftover width is not empty padding.

A wide overlay that still clips Project at 20 characters wasted the
space that made it wide.  Weights prefer the path, then the name."
  (let ((extra (max 0 (- budget used)))
        (weights '((project . 3) (name . 2) (title . 2))))
    (if (zerop extra)
        columns
      (let* ((keys (seq-filter (lambda (k) (assq k columns))
                               (mapcar #'car weights)))
             (total (apply #'+ (mapcar (lambda (k) (alist-get k weights))
                                       keys))))
        (if (zerop total)
            columns
          (let* ((shares
                  (mapcar (lambda (k)
                            (cons k (floor (* extra (/ (float (alist-get k weights))
                                                       total)))))
                          keys))
                 (rest (- extra (apply #'+ (mapcar #'cdr shares)))))
            (when (and (> rest 0) shares)
              (setcdr (car shares) (+ (cdr (car shares)) rest)))
            (mapcar (lambda (spec)
                      (let ((add (or (alist-get (car spec) shares) 0)))
                        (if (zerop add)
                            spec
                          (append (list (nth 0 spec)
                                        (nth 1 spec)
                                        (+ (nth 2 spec) add))
                                  (nthcdr 3 spec)))))
                    columns)))))))

(defun ghostherd--sidebar-format ()
  "Return `tabulated-list-format' for the columns that fit."
  (vconcat (mapcar (lambda (spec)
                     (list (nth 1 spec) (nth 2 spec) t))
                   (ghostherd--sidebar-visible-columns))))

(defun ghostherd--sidebar-sync-format ()
  "Re-fit `tabulated-list-format' when the available width changed.
Called before every rebuild so widening or narrowing the sidebar brings
columns back rather than needing the buffer recreated."
  (when (derived-mode-p 'ghostherd-sidebar-mode)
    (let ((fitted (ghostherd--sidebar-format)))
      (unless (equal fitted tabulated-list-format)
        (setq tabulated-list-format fitted)
        (tabulated-list-init-header)))))

(defun ghostherd--gone-label (gone)
  "The sidebar's word for GONE, a `ghostherd--dir-gone' entry."
  (if (plist-get gone :worktree) "wt gone" "dir gone"))

(defun ghostherd--sidebar-cell (session key state face)
  "Return SESSION's cell for column KEY, given its STATE and FACE."
  (pcase key
    ('glyph   (concat
               (propertize (ghostherd--state-glyph state) 'face face)
               ;; Its directory gone outranks how it is viewed: the mark
               ;; that says so has the spare character, as the state
               ;; column, when it fits, has the words.
               (if-let* ((gone (ghostherd-session-directory-gone session)))
                   (propertize ghostherd-gone-glyph 'face 'error
                               'help-echo (format "its %s %s is gone"
                                                  (if (plist-get gone :worktree) "worktree" "directory")
                                                  (abbreviate-file-name (plist-get gone :path))))
                 ;; Whether anyone is looking is a fact about the *view*,
                 ;; so it is a separate mark rather than a state glyph of
                 ;; its own -- an agent is working or blocked regardless.
                 ;; It belongs in the overview all the same: without it a
                 ;; herd running headless looks exactly like one you are
                 ;; watching.
                 (unless (buffer-live-p (ghostherd-session-buffer session))
                   (propertize ghostherd-detached-glyph 'face face)))))
    ('name    (let ((name (propertize (ghostherd-session-name session) 'face face))
                    (generation (ghostherd-session-generation session)))
                ;; An heir's place in the line of its name, as the herd
                ;; page shows it: rd II took rd's name and work.
                (if (< generation 2)
                    name
                  (propertize (concat name (propertize (concat " " (ghostherd--roman generation))
                                                       'face (list 'bold face)))
                              'help-echo (ghostherd--line-text session)))))
    ('kind    (symbol-name (ghostherd-session-kind session)))
    ('state   (if-let* ((gone (ghostherd-session-directory-gone session)))
                  (propertize (ghostherd--gone-label gone) 'face 'error
                              'help-echo (format "%s; its %s %s is gone" state
                                                 (if (plist-get gone :worktree) "worktree" "directory")
                                                 (abbreviate-file-name (plist-get gone :path))))
                (propertize (symbol-name state) 'face face)))
    ('project (ghostherd--abbreviate (ghostherd-session-project session)))
    ('title   (or (ghostherd--session-title session) ""))
    ('age     (ghostherd--age-string
               (or (ghostherd-session-last-active session)
                   (ghostherd-session-started-at session))))
    (_        "")))

(defun ghostherd--flex-regexp (query)
  "Return a subsequence regexp for QUERY.
Each character is quoted, so `emacs.d` does not become `emacs<any>d`."
  (mapconcat (lambda (ch) (regexp-quote (char-to-string ch)))
             (string-to-list query)
             ".*"))

(defun ghostherd--flex-match-p (query string)
  "Return non-nil if QUERY is a subsequence of STRING, case-insensitively."
  (let ((case-fold-search t))
    (and query string
         (not (string-empty-p query))
         (string-match-p (ghostherd--flex-regexp query) string))))

(defun ghostherd--sidebar-session-haystack (session)
  "Searchable text for SESSION: name, kind, state, project, notes, title."
  (mapconcat #'identity
             (delq nil
                   (list (ghostherd-session-name session)
                         (when-let* ((k (ghostherd-session-kind session)))
                           (symbol-name k))
                         (when-let* ((st (ghostherd-session-state session)))
                           (symbol-name st))
                         (ghostherd-session-project session)
                         (ghostherd--abbreviate
                          (ghostherd-session-project session))
                         (ghostherd-session-notes session)
                         (ignore-errors (ghostherd--session-title session))
                         ;; So `/gone' finds the agents whose directory is.
                         (and (ghostherd-session-directory-gone session)
                              "gone")))
             " "))

(defun ghostherd--sidebar-query-matches-p (session query)
  "Return non-nil if SESSION matches QUERY.

Empty QUERY matches everything.  Whitespace splits into tokens, each
of which must flex-match the haystack -- so `agy idle` is an AND, not
one long subsequence."
  (or (not query)
      (string-empty-p (string-trim query))
      (let ((hay (ghostherd--sidebar-session-haystack session))
            (case-fold-search t))
        (cl-every (lambda (token)
                    (string-match-p (ghostherd--flex-regexp token) hay))
                  (split-string query)))))

(defun ghostherd--sidebar-build-entries ()
  "Rebuild `tabulated-list-entries' from the sessions as they stand.
Pure rendering -- it does not poll, so unlike `ghostherd--sidebar-entries'
it is safe to call from inside the poll path without recursing."
  (ghostherd--sidebar-sync-format)
  (let* ((columns (ghostherd--sidebar-visible-columns))
         (sessions (if ghostherd--sidebar-filter-project
                       (ghostherd-sessions ghostherd--sidebar-filter-project)
                     (ghostherd-sessions)))
         (query ghostherd--sidebar-query)
         (matched (if (and query (not (string-empty-p (string-trim query))))
                      (cl-remove-if-not
                       (lambda (s) (ghostherd--sidebar-query-matches-p s query))
                       sessions)
                    sessions)))
    (setq ghostherd--sidebar-total-count (length sessions)
          ghostherd--sidebar-match-count (length matched)
          tabulated-list-entries
          (mapcar
           (lambda (s)
             (let* ((state (ghostherd-session-state s))
                    (face (ghostherd--state-face state)))
               ;; Cells are generated from the same column list as the
               ;; header, so the two cannot drift apart.
               (list (ghostherd-session-id s)
                     (vconcat
                      (mapcar (lambda (spec)
                                (ghostherd--sidebar-cell
                                 s (car spec) state face))
                              columns)))))
           matched))))

(defun ghostherd--sidebar-entries ()
  "Poll every session, then rebuild `tabulated-list-entries'.
Used by the interactive refresh and `tabulated-list-revert-hook'."
  (ghostherd-poll-all)
  (ghostherd--sidebar-build-entries))

(defun ghostherd--sidebar-erase-preview ()
  "Remove the preview pane, leaving only the tabulated-list rows."
  (when (and ghostherd--sidebar-preview-start
             (marker-position ghostherd--sidebar-preview-start))
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (delete-region ghostherd--sidebar-preview-start (point-max))))
  (setq ghostherd--sidebar-preview-start nil))

(defun ghostherd--sidebar-preview-body (session)
  "Return the snapshot text for SESSION, truncated to the pane."
  (let* ((raw (or (gethash (ghostherd-session-id session) ghostherd--screens)
                  (ignore-errors (ghostherd--host-capture session))
                  ""))
         (raw (if (and raw (not (string-empty-p (string-trim raw))))
                  raw
                "(no screen yet)"))
         (tail (ghostherd--string-tail raw ghostherd-sidebar-preview-lines))
         (width (max 20 (1- (ghostherd--sidebar-available-width)))))
    (when (and raw (not (string-empty-p raw))
               (not (equal raw "(no screen yet)")))
      (puthash (ghostherd-session-id session) raw ghostherd--screens))
    (concat
     ;; Above its screen, in words: what the row's mark means.
     (when-let* ((gone (ghostherd-session-directory-gone session)))
       (concat (propertize
                (truncate-string-to-width
                 (format "%s %s is gone: a respawn is refused; stop it instead"
                         (if (plist-get gone :worktree) "worktree" "directory")
                         (abbreviate-file-name (plist-get gone :path)))
                 width nil nil t)
                'face 'error)
               "\n"))
     (mapconcat (lambda (line)
                  (truncate-string-to-width line width nil nil t))
                (split-string tail "\n")
                "\n"))))

(defun ghostherd--sidebar-draw-preview ()
  "Rebuild the preview pane under the current row."
  (when (derived-mode-p 'ghostherd-sidebar-mode)
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t)
          (session (and (not ghostherd--sidebar-help-visible)
                        (not ghostherd--sidebar-picking)
                        ghostherd-sidebar-show-preview
                        (ghostherd--sidebar-session-at-point)))
          (width (max 10 (or (ignore-errors (window-body-width))
                             (ghostherd--sidebar-available-width)))))
      (save-excursion
        (ghostherd--sidebar-erase-preview)
        (when (or ghostherd-sidebar-show-preview
                  ghostherd--sidebar-help-visible
                  ghostherd--sidebar-picking)
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (setq ghostherd--sidebar-preview-start (point-marker))
          (insert (propertize (make-string width ?─) 'face 'shadow) "\n")
          (insert (cond
                   (ghostherd--sidebar-picking
                    (ghostherd--sidebar-pick-text))
                   (ghostherd--sidebar-help-visible
                    (ghostherd--sidebar-help-text))
                   (session
                    (ghostherd--sidebar-preview-body session))
                   (t (propertize "(no session)" 'face 'shadow))))
          (unless (bolp) (insert "\n"))))
      (restore-buffer-modified-p nil))))

(defun ghostherd--sidebar-print (&optional update)
  "Print the session table and, if enabled, the preview under it."
  (let ((id (tabulated-list-get-id)))
    (ghostherd--sidebar-erase-preview)
    (tabulated-list-print update)
    (when id
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (equal (tabulated-list-get-id) id)))
        (forward-line 1))
      (when (eobp) (goto-char (point-min))))
    (ghostherd--sidebar-draw-preview)
    (setq ghostherd--sidebar-preview-id (tabulated-list-get-id))))

(defun ghostherd--sidebar-confine-point ()
  "Keep point on a table row, not in the preview pane."
  (when (and ghostherd--sidebar-preview-start
             (marker-position ghostherd--sidebar-preview-start)
             (>= (point) ghostherd--sidebar-preview-start)
             (> ghostherd--sidebar-preview-start (point-min)))
    (goto-char (1- ghostherd--sidebar-preview-start))
    (beginning-of-line)))

(defun ghostherd--sidebar-fresh-capture (id)
  "Recapture ID's screen and redraw the preview if still selected."
  (when-let* ((session (ghostherd-get id))
              (buf (get-buffer "*ghostherd*")))
    (when (and (ghostherd--sidebar-on-screen-p)
               (equal id (with-current-buffer buf
                           (ghostherd--sidebar-confine-point)
                           (tabulated-list-get-id))))
      (when-let* ((screen (ignore-errors (ghostherd--host-capture session))))
        (puthash id screen ghostherd--screens)
        (when (equal id ghostherd--sidebar-preview-id)
          (with-current-buffer buf
            (ghostherd--sidebar-draw-preview)
            (when (and (ghostherd--sidebar-posframe-showing-p)
                       (fboundp 'posframe-refresh))
              (posframe-refresh buf))))))))

(defun ghostherd--sidebar-schedule-fresh-capture (id)
  "Debounce a recapture of ID so holding j/k does not fork per row."
  (when (timerp ghostherd--sidebar-preview-timer)
    (cancel-timer ghostherd--sidebar-preview-timer))
  (setq ghostherd--sidebar-preview-timer
        (run-with-timer 0.2 nil #'ghostherd--sidebar-fresh-capture id)))

(defun ghostherd--sidebar-preview-on-command ()
  "Follow point: draw the cached snapshot, then recapture shortly."
  (when (derived-mode-p 'ghostherd-sidebar-mode)
    (ghostherd--sidebar-confine-point)
    (when (and ghostherd-sidebar-show-preview
               (not ghostherd--sidebar-help-visible))
      (let ((id (tabulated-list-get-id)))
        (unless (equal id ghostherd--sidebar-preview-id)
          (setq ghostherd--sidebar-preview-id id)
          (ghostherd--sidebar-draw-preview)
          (when id
            (ghostherd--sidebar-schedule-fresh-capture id)))))))

(defun ghostherd-sidebar-toggle-preview ()
  "Toggle the snapshot pane under the session list."
  (interactive)
  (setq ghostherd-sidebar-show-preview
        (not ghostherd-sidebar-show-preview))
  (setq ghostherd--sidebar-preview-id nil)
  (ghostherd--sidebar-print t)
  (when (ghostherd--sidebar-posframe-showing-p)
    (ghostherd--sidebar-show-posframe (current-buffer)))
  (message "Preview %s" (if ghostherd-sidebar-show-preview "on" "off")))

(defun ghostherd--sidebar-refresh ()
  "Re-render the sidebar from current session state, without polling.
Rebuilding the entries is the point: `tabulated-list-print' alone
re-prints whatever `tabulated-list-entries' already held, so a state
change would redraw the same stale row it drew last time."
  (when-let* ((buf (get-buffer "*ghostherd*")))
    (with-current-buffer buf
      (when (derived-mode-p 'ghostherd-sidebar-mode)
        (ghostherd--sidebar-build-entries)
        (ghostherd--sidebar-print t)
        (when (and (ghostherd--sidebar-posframe-showing-p)
                   (fboundp 'posframe-refresh))
          (posframe-refresh buf))))))

(defun ghostherd--sidebar-on-screen-p ()
  "Return non-nil when the session list is actually visible.

An invisible posframe still has a window, so `get-buffer-window'
is not enough: Age would keep being rewritten after Esc."
  (when-let* ((buf (get-buffer "*ghostherd*")))
    (cl-some (lambda (win)
               (frame-visible-p (window-frame win)))
             (get-buffer-window-list buf nil t))))

(defun ghostherd--poll-tick ()
  "Timer callback: poll every session, then keep a visible sidebar current.
A state change already redraws via `ghostherd--set-state', but the Age
column advances with no state change at all, so refresh on each tick
while the sidebar is actually on screen.

Sidecar mail/Telegram is not on this timer.  `url-retrieve' on every
poll hitchs typing; that path is `ghostherd--herd-tick-timer'."
  (ghostherd-poll-all)
  ;; Housekeeping on a cheap cadence: drop the turn-end cache's dead
  ;; transcripts even when no agent was removed (a live one may have moved
  ;; to a new conversation).
  (when (zerop (% (cl-incf ghostherd--poll-count) ghostherd--turn-ends-prune-every))
    (ghostherd--prune-turn-ends))
  ;; Whose directory is gone: a stat each, once a minute.
  (with-demoted-errors "ghostherd: looking at the agents' directories: %S"
    (ghostherd--check-directories))
  (when (ghostherd--sidebar-on-screen-p)
    (ghostherd--sidebar-refresh)))


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
           ;; exactly when it would work.
           :login (if (eq (cdr err) 'login) t :json-false)
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
           :url (or (and (eq (ghostherd-session-state s) 'blocked)
                         (ghostherd--login-url s))
                    "")
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
  "Paste one sidecar mail MSG through `ghostherd-message' / `handoff'."
  (let* ((from (or (plist-get msg :from) "unknown"))
         (to (plist-get msg :to))
         (body (or (plist-get msg :body) ""))
         (handoff (plist-get msg :handoff))
         (ask (plist-get msg :ask))
         (submit (if (plist-member msg :submit)
                     (plist-get msg :submit)
                   t)))
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
;; another agent started stays out: whoever asked reports.  The herd
;; knows a turn is from an ask by its own delivery of it
;; (`ghostherd--ask-deliver'); every other turn start is the user's.
;; Leads and primary are unaffected -- their turn end is surfaced by
;; role, as before.

(defvar ghostherd--turn-origin (make-hash-table :test 'equal)
  "Session id -> how its current or last turn began: `ask' (the herd
delivered it another agent's ask) or `user' (the user's own prompt).
Read to decide whether a worker's turn end is its report to the user.")

(defvar ghostherd--turn-ask-pending (make-hash-table :test 'equal)
  "Session id -> t after the herd delivers an ask, until the turn it
starts is settled.  The next turn-start marks that turn `ask'.")

(defun ghostherd--turn-from-ask (session)
  "Mark that SESSION's next turn begins from an ask the herd delivered."
  (when-let* ((s (ghostherd-get session)))
    (puthash (ghostherd-session-id s) t ghostherd--turn-ask-pending)))

(defun ghostherd--turn-origin-of (session)
  "How SESSION's current or last turn began: `ask', `user', or nil."
  (when-let* ((s (ghostherd-get session)))
    (gethash (ghostherd-session-id s) ghostherd--turn-origin)))

(defun ghostherd--turn-origin-on-state (session old new)
  "Settle SESSION's turn origin as a turn starts (OLD resting, NEW busy).
A turn the herd started by delivering an ask (`ghostherd--turn-ask-pending')
is `ask'; any other turn start -- a user prompt through the herd, or one
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
          (progn (puthash id 'ask ghostherd--turn-origin)
                 (when (or (eq pending 'submitted)
                           (not (ghostherd--submit-pending-p session)))
                   (remhash id ghostherd--turn-ask-pending)))
        (puthash id 'user ghostherd--turn-origin)))))

(defun ghostherd--turn-submitted (session)
  "SESSION's CLI took the prompt the herd sent it: an ask's flag is spent
on the turn it starts -- the one under way, else the next to start."
  (let ((id (ghostherd-session-id session)))
    (when (gethash id ghostherd--turn-ask-pending)
      (if (memq (ghostherd-session-state session) '(working blocked waiting))
          (progn (remhash id ghostherd--turn-ask-pending)
                 (puthash id 'ask ghostherd--turn-origin))
        (puthash id 'submitted ghostherd--turn-ask-pending)))))

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

(defun ghostherd--herd-take-guard (requests)
  "Keep REQUESTS from a `herd_tick', and log each refusal, approval and
use once."
  (dolist (r requests)
    (let ((id (plist-get r :id))
          (status (plist-get r :status)))
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

;;; Primary: the herd-wide lead
;;
;; The user's one entry point for what crosses projects, or where they do
;; not know which project it belongs to: one agent, `primary', in the
;; herd's own directory outside every repo.  It routes work to the
;; projects' leads by handoff and keeps track of it; it never writes code
;; and decides nothing that is the user's.  Its directory is its project:
;; so it has a memory store of its own (with the user's, as every project
;; has), a room on the page, and the herd's rules hold for it as for any
;; agent -- it may hand off, as a project with no lead's agent may, and
;; may not ask another project's agents.  Started when first written to
;; -- the page's Primary room, Telegram, M-x ghostherd-primary -- and,
;; once stopped or gone with the host, resumed on its last conversation,
;; so it keeps its context.  Nothing of its own runs on a timer: it wakes
;; when spoken to, or when an answer comes back.

(defcustom ghostherd-primary-kind 'claude
  "The CLI primary, the herd-wide lead, runs on.
Its conversation is resumed on restart only on the same CLI: changing
this starts the next primary afresh."
  :type 'symbol
  :group 'ghostherd)

(defcustom ghostherd-primary-directory "~/.ghostherd/primary/"
  "Where primary works: the herd's own directory, outside every repo.
It is primary's project -- its memory store, its room on the page --
and holds no checkout: primary edits none."
  :type 'directory
  :group 'ghostherd)

(defconst ghostherd--primary-name "primary"
  "Primary's name in the herd, which agents and the herd's words use.")

(defconst ghostherd--primary-role "\
You are primary: the herd-wide lead.  The user brings you what crosses
projects, or what they do not know where it belongs, and asks you what
everyone is doing.  You route the work and keep track of it; you do not
do it.  You never write code and never edit a checkout -- not a line --
and you do not run any project's agents: each project has a lead, or
gets one, and its work goes to it.

What you see, across every project:

  \"$GHOSTHERD_HERD\" list                 every agent: kind, state, notes,
                                         project, branch, the ask it is on
  \"$GHOSTHERD_HERD\" asks                 asks open and lately closed
  \"$GHOSTHERD_HERD\" recall --project P [QUERY]
                                         what project P's memory says

What you do: hand the work to the project it belongs to.

  \"$GHOSTHERD_HERD\" handoff PROJECT --wait <<'HERD'
  the brief
  HERD

PROJECT is its path or its name -- the name of a project with agents, a
memory store, or that claude has run in; one the herd does not know is
refused, saying which it does: give the path then, or ask the user.  Run
it in the background: you are told when the answer comes, and you tell
the user.  A project with no lead gets one started for it.  Brief it for a lead who knows nothing of your
conversation: what the user wants and why, what is known (files,
tickets, links, commits), what done looks like, and that the user waits.
Work that spans projects is a handoff to each, with its part, and says
what each depends on.  Not sure where something belongs: look first
(list, recall); still unsure, ask the user rather than guess.

You do not ask any project's agents yourself -- the herd refuses it:
their lead plans their work.  And agents do not ask you; the user does.

What is the user's to decide, you recommend and leave to them: stopping
or pausing agents, approving a memory promotion, merging, anything that
spends quota for long.  Say what you would do and why.

\"What is everyone doing?\", \"what is stuck?\": answer from herd list and
herd asks, by project -- who is on what, what is blocked or waiting and
on whom, what failed, what came back.  Briefly.

At the end of each turn, report what went where, who is on it and what
to wait for -- or what came back, and what it means for the user.
Briefly, and in the language they write to you in.  When they are away
it reaches them on their phone.

Your memory is your own store: \"$GHOSTHERD_HERD\" remember what you learn
about routing -- which project owns what, which lead handles what.  A
project's memory is its lead's, the user's preferences the user's: read
them with recall, change none.

You wake only when the user writes to you or an answer comes back.  No
polling, no loops, nothing on a timer.
"
  "Primary's role: given to it as it starts, or with its first message.")

(defun ghostherd--primary-args (kind)
  "Arguments that give a KIND primary its role as it starts; nil for a
CLI that takes none, which is owed it with its first message."
  (pcase kind
    ('claude (list "--append-system-prompt" ghostherd--primary-role))
    ('grok (list "--rules" ghostherd--primary-role))))

(defun ghostherd--primary-dir ()
  "Primary's directory, absolute, with a trailing slash."
  (file-name-as-directory (expand-file-name ghostherd-primary-directory)))

(defun ghostherd--primary-dir-p (dir)
  "Non-nil when DIR is primary's directory, however written."
  (and (stringp dir) (not (string-empty-p dir))
       (equal (ghostherd--project-name dir) (ghostherd--project-name (ghostherd--primary-dir)))))

(defun ghostherd--primary-session ()
  "Primary, the herd-wide lead, as the herd has it -- up or not -- or nil."
  (seq-find (lambda (s) (equal (ghostherd-session-role s) "primary")) (ghostherd-sessions)))

(defun ghostherd--primary-up (&optional session)
  "SESSION -- primary by default -- when it is up: its host there, not dead."
  (let ((s (or session (ghostherd--primary-session))))
    (and s (ghostherd--session-live-p s) (not (eq (ghostherd-session-state s) 'dead)) s)))

(defun ghostherd--primary-state-file ()
  "Where primary's last conversation is kept: beside its directory, not in
it, so it is no file of primary's."
  (concat (directory-file-name (ghostherd--primary-dir)) ".eld"))

(defun ghostherd--primary-remembered ()
  "Primary's last conversation as (:kind KIND :conversation ID), or nil."
  (let ((file (ghostherd--primary-state-file)))
    (when (file-readable-p file)
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents file)
          (let ((v (read (current-buffer))))
            (and (plistp v) (symbolp (plist-get v :kind))
                 (stringp (plist-get v :conversation)) v)))))))

(defun ghostherd--primary-remember ()
  "Keep the conversation primary is on, as its hooks name it, for the
next start to resume: a restart of Emacs, or of the host, loses the
agent but not this."
  (when-let* ((s (ghostherd--primary-session))
              (id (ghostherd-session-conversation s)))
    (let ((now (list :kind (ghostherd-session-kind s) :conversation id)))
      (unless (equal now (ghostherd--primary-remembered))
        (make-directory (file-name-directory (ghostherd--primary-state-file)) t)
        (with-temp-file (ghostherd--primary-state-file)
          (prin1 now (current-buffer)))))))

(defun ghostherd--primary-forget ()
  "Drop the kept conversation, so the next start is fresh."
  (let ((file (ghostherd--primary-state-file)))
    (when (file-exists-p file)
      (ignore-errors (delete-file file)))))

(defun ghostherd-primary-start (&optional display)
  "Primary, up: the one already up, or one started.  A primary stopped or
gone with its host is resumed on its last conversation -- the one its
hooks named, else the one kept for it (`ghostherd--primary-remember') --
when that was on the same CLI as `ghostherd-primary-kind'; otherwise it
starts afresh.  With DISPLAY, show it."
  (if-let* ((up (ghostherd--primary-up)))
      (progn (when display (ghostherd-visit up)) up)
    (let* ((old (ghostherd--primary-session))
           (kind ghostherd-primary-kind)
           (kept (ghostherd--primary-remembered))
           (id (or (and old (eq (ghostherd-session-kind old) kind)
                        (ghostherd-session-conversation old))
                   (and (eq (plist-get kept :kind) kind) (plist-get kept :conversation))))
           ;; A kept conversation that is gone (claude/agy, verifiable) would
           ;; make the CLI exit on resume: drop it so the next start is fresh,
           ;; not wedged.  grok's remote ones fall to the backstop.
           (_ (when (and id (ghostherd--conversation-gone-p kind id))
                (ghostherd--log-add
                 ghostherd--primary-name 'life
                 (format "kept conversation %s is gone; starting fresh" id))
                (ghostherd--primary-forget)
                (setq id nil)))
           (resume (and id (ghostherd--resume-args kind id)))
           (dir (ghostherd--primary-dir)))
      (when old (ghostherd-kill old t))
      (when-let* ((other (ghostherd-get ghostherd--primary-name)))
        (user-error "An agent named %s is not the herd's primary; rename it first"
                    (ghostherd-session-name other)))
      (unless (and (memq kind (ghostherd--agent-kinds)) (plist-get (ghostherd--spec kind) :command))
        (user-error "No agent kind %s for primary (ghostherd-primary-kind)" kind))
      (make-directory dir t)
      (let ((new (ghostherd-spawn
                  kind
                  :name ghostherd--primary-name
                  :project dir
                  :directory dir
                  :args (append (plist-get (ghostherd--spec kind) :args)
                                (ghostherd--primary-args kind) resume)
                  :notes "primary: the herd-wide lead -- routes work to projects, tracks it, reports"
                  :role "primary"
                  :display display)))
        ;; Resumed, its conversation has the role; a fresh one on a CLI that
        ;; takes none reads it with its first message.
        (unless (or resume (ghostherd--primary-args kind))
          (puthash (ghostherd-session-id new) ghostherd--primary-role ghostherd--role-owed))
        (ghostherd--log-add new 'life (if resume
                                          (format "resumed on its conversation %s" id)
                                        "started, a fresh conversation"))
        new))))

(defun ghostherd-primary-say (text)
  "Say TEXT to primary, starting or resuming it when it is not up -- then
once it is ready.  Return primary."
  (if-let* ((up (ghostherd--primary-up)))
      (progn (ghostherd-tell up text) up)
    (let ((new (ghostherd-primary-start)))
      (ghostherd--when-ready new (lambda () (ghostherd-tell new text)))
      new)))

;;;###autoload
(defun ghostherd-primary (&optional text)
  "Show primary, the herd-wide lead, starting or resuming it if need be.
With a prefix argument, write TEXT to it as well."
  (interactive (list (and current-prefix-arg (read-string "To primary: "))))
  (let ((s (if (and (stringp text) (not (string-empty-p (string-trim text))))
               (ghostherd-primary-say text)
             (ghostherd-primary-start))))
    (ghostherd-visit s)
    s))

(defun ghostherd--herd-run-command (cmd)
  "Run one sidecar Telegram CMD plist.  Return (:id ID :text TEXT), and
for a handoff the lead it went to as :lead."
  (let* ((id (plist-get cmd :id))
         (op (format "%s" (or (plist-get cmd :op) "")))
         (name (plist-get cmd :session))
         (args (plist-get cmd :args))
         (text "ok")
         lead)
    (condition-case err
        (pcase op
          ("prompt"
           (ghostherd-prompt name (or (plist-get args :body) "") nil)
           (setq text "prompted"))
          ("answer"
           (let ((n (plist-get args :n)))
             (ghostherd-answer name (truncate (or n 1))))
           (setq text "answering"))
          ("interrupt"
           (ghostherd-interrupt name)
           (setq text "interrupted"))
          ("abort"
           (ghostherd-abort name)
           (setq text "aborted"))
          ("kill"
           (ghostherd-kill name t)
           (setq text "killed"))
          ("respawn"
           (ghostherd-respawn name)
           (setq text "respawned"))
          ("relay"
           (if-let* ((by (plist-get args :by)))
               ;; Asked over the wire (`herd relay'): guarded by who asks.
               (setq text (ghostherd--relay-request by name (plist-get args :kind)
                                                    (plist-get args :reason)))
             ;; A page button or Telegram tap: the user, already trusted.
             (let ((kind (plist-get args :kind)))
               (unless (and (stringp kind) (memq (intern kind) (ghostherd--relay-kinds)))
                 (error "Relay to what: %s" kind))
               (ghostherd-relay name (intern kind))
               (setq text (format "relaying %s to %s" name kind)))))
          ("screen"
           (setq text (or (ghostherd-read name 40) "(empty)"))
           (when (> (length text) 3500)
             (setq text (concat (substring text 0 3500) "\n…"))))
          ("screens"
           ;; `herd screens': the saved screens from the log, read-only.
           (setq text (ghostherd--log-screens-text
                       (plist-get args :agent) (plist-get args :since)
                       (plist-get args :why))))
          ("start"
           ;; `herd start': a lead starts or reuses an agent in its project.
           (setq text (apply #'ghostherd-lead-start
                             (plist-get args :by) (plist-get args :kind)
                             (list :worktree (plist-get args :worktree)
                                   :branch (plist-get args :branch)
                                   :name (plist-get args :name)
                                   :notes (plist-get args :notes)
                                   :new (plist-get args :new)))))
          ("stop"
           ;; `herd stop': a lead stops an agent it started.
           (setq text (ghostherd-lead-stop (plist-get args :by) name)))
          ("primary-say"
           ;; Telegram, free-form: for primary, started or resumed if need be.
           (let ((body (string-trim (or (plist-get args :body) "")))
                 (up (ghostherd--primary-up)))
             (when (string-empty-p body)
               (error "Nothing to send"))
             (ghostherd-primary-say body)
             (setq text (if up "sent to primary"
                          "started primary; it reads this once it is up"))))
          ("handoff"
           ;; `herd handoff' to a project the sidecar found no lead in.
           (pcase-let ((`(,name . ,said)
                        (ghostherd-handoff-lead (plist-get args :by) (plist-get args :project)
                                                (plist-get args :from_project)
                                                (plist-get args :head))))
             (setq lead name text said)))
          ("explain"
           (setq text (ghostherd--explain-text name)))
          ("lead-say"
           (let ((project (plist-get args :project))
                 (body (string-trim (or (plist-get args :body) ""))))
             (unless (and (stringp project) (file-directory-p (expand-file-name project)))
               (error "No project %s" project))
             (when (string-empty-p body)
               (error "Nothing to send"))
             (setq text (format "sent to %s" (ghostherd-session-name (ghostherd-lead-say project body))))))
          (_ (setq text (format "unknown op %s" op))))
      (error (setq text (error-message-string err))))
    (append (list :id id :text text) (and lead (list :lead lead)))))

(defun ghostherd--herd-run-commands (result)
  "Execute Telegram commands from RESULT and queue replies."
  (dolist (cmd (plist-get result :commands))
    (push (ghostherd--herd-run-command cmd) ghostherd--herd-replies)))

;;; Relay: an heir takes an agent's name and its work
;;
;; An agent stops -- its CLI's usage used up, wedged, or you would
;; rather another CLI went on -- and a new agent takes its name, its
;; role, its notes and its project: the second of the name, the third.
;; Its first message is where the work stood, which the sidecar reads
;; from the old one's transcript, the herd's asks and the checkout
;; (server/relay.py): the old one may not be able to say anything any
;; more.  The name is what the rest of the herd knows it by, so asks of
;; it, the asks it waits on and mail for it go on as they were; the
;; asks it was answering are its heir's to answer, not failed with it.

(defcustom ghostherd-relay-kinds '(claude grok agy)
  "CLIs an agent's work is relayed to, the herd's first choice first.
`ghostherd-relay' offers the first that is not the agent's own and has
usage left; any other kind with a command can still be picked."
  :type '(repeat symbol)
  :group 'ghostherd)

(defvar ghostherd--relaying (make-hash-table :test 'equal)
  "Session id -> t while its relay waits for the sidecar's brief.")

(defvar ghostherd--relay-offered (make-hash-table :test 'equal)
  "Session id -> the used-up window it was last offered a relay for.")

(defun ghostherd--relay-kinds ()
  "The kinds an agent's work can go to: `ghostherd-relay-kinds' first,
then any other that runs a command."
  (seq-filter (lambda (k) (plist-get (alist-get k ghostherd-agent-specs) :command))
              (seq-uniq (append ghostherd-relay-kinds (ghostherd--agent-kinds)))))

(defun ghostherd-relay-default-kind (session)
  "The CLI SESSION's work goes to unless said otherwise: the first of
`ghostherd-relay-kinds' other than its own with usage left, its own
when none has."
  (let ((own (ghostherd-session-kind session)))
    (or (seq-find (lambda (k) (not (or (eq k own) (ghostherd-usage-used-up k))))
                  (ghostherd--relay-kinds))
        own)))

(defun ghostherd--relay-args (kind recipe lead)
  "Arguments for an heir on KIND to the agent of RECIPE, its :kind
added; LEAD when it leads its project.  On the same CLI, the old one's
own, less what picked its conversation; on another, what the project
or KIND gives an agent, since one CLI's flags mean nothing to another."
  (let* ((dir (or (plist-get recipe :directory) (plist-get recipe :project) default-directory))
         (defaults (ghostherd-project-defaults dir))
         (args (if (eq kind (plist-get recipe :kind))
                   (ghostherd--without-resume kind (plist-get recipe :args))
                 (if (eq (car defaults) kind)
                     (cdr defaults)
                   (plist-get (ghostherd--spec kind) :args)))))
    (if lead (ghostherd--with-lead-role kind args) args)))

(defun ghostherd--relay-ancestor (session reason)
  "SESSION as the line of its name remembers it, gone for REASON."
  `((kind . ,(symbol-name (ghostherd-session-kind session)))
    (conversation . ,(or (ghostherd-session-conversation session) ""))
    (ended . ,(format-time-string "%FT%T%z"))
    (reason . ,(or reason ""))))

(defcustom ghostherd-worktree-brief-commits 20
  "Most of a worktree branch's own commits listed in a relay brief (git
log base..branch) and files in its diffstat; the rest are counted, not
listed.  So the branch state stays within the brief's budget."
  :type 'integer
  :group 'ghostherd)

(defun ghostherd--worktree-brief (session)
  "Where SESSION's worktree branch stands against its base, to add to a
relay brief: the base, commits ahead and behind, the branch's own commits
and a diffstat against the base (each capped by
`ghostherd-worktree-brief-commits'), and what is uncommitted.  Nil for an
agent not in a herd worktree.  No full diff -- the heir reads the branch
itself; this orients it to a branch that has diverged from base."
  (let* ((wt (ghostherd-session-worktree session))
         (dir (and wt (plist-get wt :path)))
         (branch (and wt (plist-get wt :branch)))
         (base (and wt (plist-get wt :base))))
    (when (and dir branch base (file-directory-p (expand-file-name dir)))
      (let* ((cap (max 1 ghostherd-worktree-brief-commits))
             (counts (ghostherd--git-lines dir "rev-list" "--left-right" "--count"
                                           (concat base "..." branch)))
             (ba (and counts (split-string counts)))
             (behind (or (nth 0 ba) "0"))
             (ahead (or (nth 1 ba) "0"))
             (log (ghostherd--git-lines dir "log" "--oneline" "--no-decorate"
                                        (format "-n%d" cap) (concat base ".." branch)))
             (more (let ((n (string-to-number ahead))) (and (> n cap) (- n cap))))
             (stat (ghostherd--git-lines dir "diff" (format "--stat-count=%d" cap) "--stat"
                                         (concat base "..." branch)))
             (dirty (ghostherd--git-lines dir "status" "--porcelain=v1")))
        (when counts
          (string-join
           (delq nil
                 (list
                  (format "\n\n--- Your worktree: branch %s, forked from %s ---" branch base)
                  (format "%s commit(s) ahead of %s, %s behind it." ahead base behind)
                  (and log (concat "Its own commits (git log " base ".." branch "):\n" log
                                   (and more (format "\n  … and %d more" more))))
                  (and stat (concat "Changed against " base " (git diff --stat):\n" stat))
                  (if dirty
                      (concat "Uncommitted in the worktree:\n"
                              (if (> (length dirty) 800)
                                  (concat (substring dirty 0 800) "\n  …")
                                dirty))
                    "Nothing uncommitted in the worktree.")))
           "\n"))))))

;;;###autoload
(defun ghostherd-relay (session kind &optional reason)
  "Hand SESSION's work to a new agent on KIND, which takes its name.
The heir has SESSION's role, notes and project, and is the next of the
name; its first message is where SESSION's work stood -- what it was
asked, its last steps and answer, the asks it owes and waits on, the
checkout -- which the sidecar reads from SESSION's transcript.  The
asks SESSION was answering are the heir's to answer.  SESSION is
killed; its conversation stays where its CLI keeps it.

REASON says why SESSION stops; by default its CLI's usage used up,
when the usage says so.  The heir goes on with the work when SESSION
was cut short -- working or blocked, or its CLI's usage used up -- and
otherwise says where the work stands and waits to be told.

Interactively, acts on the sidebar row at point when there is one, and
offers `ghostherd-relay-default-kind'.
Returns at once: the heir starts when the sidecar has the brief."
  (interactive
   (let* ((s (or (and (derived-mode-p 'ghostherd-sidebar-mode)
                      (ghostherd--sidebar-session-at-point))
                 (ghostherd--read-session "Relay agent: ")))
          (default (ghostherd-relay-default-kind s)))
     (list s (intern (completing-read
                      (format "Relay %s (%s) to: " (ghostherd-session-name s)
                              (ghostherd-session-kind s))
                      (mapcar #'symbol-name (ghostherd--relay-kinds))
                      nil t nil nil (symbol-name default))))))
  (setq session (or (ghostherd-get session) (user-error "No such session")))
  (let* ((id (ghostherd-session-id session))
         (name (ghostherd-session-name session))
         (old (ghostherd-session-kind session))
         (reason (or reason (ghostherd-usage-session-used-up-text session))))
    (unless (plist-get (alist-get kind ghostherd-agent-specs) :command)
      (user-error "%s runs no agent" kind))
    (unless (plist-get (alist-get old ghostherd-agent-specs) :command)
      (user-error "%s is a shell, not an agent" name))
    (unless (and (fboundp 'ghostherd-memory-rpc-url) (ghostherd-memory-rpc-url))
      (user-error "A relay needs the herd's sidecar, to read %s's conversation" name))
    (when (gethash id ghostherd--relaying)
      (user-error "%s is already being relayed" name))
    (ghostherd--refuse-gone session "relaying")
    (puthash id t ghostherd--relaying)
    (ghostherd--log-add session 'life (format "relaying to %s: reading where its work stands" kind))
    (ghostherd-memory-request-async
     "herd_relay_brief"
     (lambda (brief)
       (remhash id ghostherd--relaying)
       ;; Add where the worktree branch stands, so an heir to a worktree
       ;; agent is not left with only the transcript when the branch has
       ;; diverged from its base.
       (ghostherd--relay-hand session kind
                              (concat (plist-get brief :text)
                                      (or (ghostherd--worktree-brief session) ""))
                              reason))
     (list :name name
           :path (or (ghostherd-session-transcript session) "")
           :project (or (ghostherd-session-project session) "")
           :from (symbol-name old)
           :to (symbol-name kind)
           :generation (1+ (ghostherd-session-generation session))
           :reason (or reason "")
           :cut (if (or (memq (ghostherd-session-state session) '(working blocked))
                        (ghostherd-usage-session-used-up session))
                    t :json-false)
           :notes (or (ghostherd-session-notes session) "")
           :client (or (and (fboundp 'ghostherd-herd-client) (ghostherd-herd-client)) "herd"))
     (lambda (err)
       (remhash id ghostherd--relaying)
       (ghostherd--log-add session 'life (format "relay to %s failed: %s" kind err))
       (message "ghostherd: relaying %s: %s" name err)))
    nil))

(defun ghostherd--relay-hand (session kind text reason)
  "Kill SESSION and start its heir on KIND, told TEXT once it is ready.
REASON is why SESSION stopped.  The asks SESSION was answering are held
for the heir meanwhile.  Return the heir, or nil when SESSION left the
herd, or its directory went, while the brief was written."
  (let ((id (ghostherd-session-id session))
        (name (ghostherd-session-name session)))
    (cond
     ((not (eq (gethash id ghostherd--sessions) session))
      (message "ghostherd: %s left the herd before its heir could take over" name)
      nil)
     ;; Its directory went while the brief was written: it stays as it is.
     ((ghostherd--mark-gone session)
      (ghostherd--log-add session 'life (format "not relayed to %s: its directory is gone" kind))
      (message "ghostherd: %s's directory is gone: not relaying it; stop it instead" name)
      nil)
     (t
      (let* ((old (ghostherd-session-kind session))
             (recipe (plist-put (ghostherd-session-recipe session) :kind old))
             (lead (equal (plist-get recipe :role) "lead"))
             (held (ghostherd--asks-of session))
             (lineage (append (ghostherd-session-lineage session)
                              (list (ghostherd--relay-ancestor session reason))))
             (buffer (ghostherd-session-buffer session))
             (shown (and (buffer-live-p buffer) (get-buffer-window buffer t) t))
             (args (ghostherd--relay-args kind recipe lead))
             heir)
        ;; Its asks are its heir's, not failed with it.
        (dolist (ask held)
          (remhash ask ghostherd--asks-open)
          (puthash ask name ghostherd--relay-held))
        (ghostherd--handoff-cancel id)
        ;; What its hooks said, and what its lead was told, were its own.
        (remhash name ghostherd--herd-links)
        (remhash id ghostherd--lead-work-told)
        (ghostherd-memory-request-async "herd_unlink" #'ignore (list :session name) #'ignore)
        (ghostherd-kill session t)
        (setq heir (ghostherd-spawn kind
                                    :name name
                                    :project (plist-get recipe :project)
                                    :directory (or (plist-get recipe :directory)
                                                   (plist-get recipe :project))
                                    :backend (plist-get recipe :backend)
                                    :args args
                                    :notes (plist-get recipe :notes)
                                    :role (plist-get recipe :role)
                                    :lineage lineage
                                    :display shown))
        (ghostherd--log-add heir 'life
                            (format "%s %s, on %s: took over from %s%s"
                                    name (ghostherd--roman (ghostherd-session-generation heir))
                                    kind old (if reason (concat " -- " reason) "")))
        (ghostherd--when-ready heir
                               (lambda () (ghostherd--relay-tell heir text held lead))
                               nil
                               (lambda () (ghostherd--relay-release heir held)))
        heir)))))

(defun ghostherd--relay-tell (heir text held lead)
  "Tell HEIR its work, TEXT; then the asks HELD for it are its to answer.
LEAD when it leads its project: it hears what a lead hears first."
  (if lead
      (ghostherd-tell heir text (unless (ghostherd--lead-args (ghostherd-session-kind heir))
                                  (concat ghostherd--lead-role "\n---\n\n"))
                      t)
    (let ((ghostherd--input-said (concat "[from the herd] " text)))
      (ghostherd-prompt heir text)))
  (dolist (ask held)
    (remhash ask ghostherd--relay-held)
    (puthash ask (ghostherd-session-id heir) ghostherd--asks-open))
  (ghostherd--log-add heir 'life "told where the work stood"))

(defun ghostherd--relay-release (heir held)
  "The asks HELD for HEIR fail: it never came to be told them."
  (dolist (ask held)
    (remhash ask ghostherd--relay-held)
    (ghostherd--ask-settle ask heir nil
                           (format "%s's heir never came to its prompt" (ghostherd-session-name heir)))))

(defun ghostherd--relay-offer (session old new)
  "SESSION stopped working, from OLD to NEW, with its CLI's usage used
up: say so, and that a relay would carry its work on -- once for each
time its usage runs out.

When it settles and is no longer used up, the offer is forgotten.  The
card's relay button reads the live used-up each poll, so it clears on its
own; this is for the push memo, so a later, real used-up offers again
rather than being taken for the one already sent."
  (when (memq new '(idle blocked done))
    (let* ((id (ghostherd-session-id session))
           (kind (ghostherd-session-kind session))
           (window (ghostherd-usage-session-used-up session)))
      (if (not window)
          (remhash id ghostherd--relay-offered)
        (when-let* (((eq old 'working))
                    (to (ghostherd-relay-default-kind session))
                    ((not (eq to kind)))
                    (key (list kind (plist-get window :label) (plist-get window :resets-at)))
                    ((not (equal (gethash id ghostherd--relay-offered) key))))
          (puthash id key ghostherd--relay-offered)
          (let ((why (ghostherd-usage-session-used-up-text session)))
            (ghostherd--log-add session 'life (format "stopped: %s.  A relay hands its work to %s" why to))
            (message "ghostherd: %s stopped: %s -- M-x ghostherd-relay hands its work to %s"
                     (ghostherd-session-name session) why to)
            (ghostherd--push 'used-up session :from (symbol-name kind) :to (symbol-name to) :reason why)))))))

(add-hook 'ghostherd-state-change-hook #'ghostherd--relay-offer)

;;; Pushes to your phone
;;
;; The herd page shows everything -- while you look at it.  Telegram is
;; for when you do not: the sidecar's bot pushes what wants you, an agent
;; blocked, a lead's report at the end of its turn, an ask that failed.
;; Emacs decides when one is worth it -- you are looking at neither the
;; agent nor the herd page, and have left this machine -- and the bot
;; words it: it has the lead's last message, and the buttons to answer.

(defcustom ghostherd-push '(blocked report ask-failed used-up lead-started)
  "What the herd pushes to Telegram while you are away: an agent
`blocked', a lead's `report' at the end of its turn, an `ask-failed',
an agent stopped with its CLI's usage `used-up' -- with a button that
relays it -- and a lead the herd started in a project for a handoff
from another, `lead-started'.  Nil pushes nothing.  The sidecar's bot
sends them to the chats of GHOSTHERD_TELEGRAM_CHAT_IDS; a reply to one
goes to that project's lead."
  :type '(set (const blocked) (const report) (const ask-failed) (const used-up)
              (const lead-started))
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
          (pushes ghostherd--herd-pushes))
      (setq ghostherd--herd-tick-inflight t
            ghostherd--herd-replies nil
            ghostherd--herd-pushes nil)
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
             :guard_ttl ghostherd-guard-request-ttl)
       (lambda (_err)
         (setq ghostherd--herd-tick-inflight nil
               ghostherd--herd-replies
               (append replies ghostherd--herd-replies)
               ghostherd--herd-pushes
               (append ghostherd--herd-pushes pushes)))))))

(defun ghostherd-sidebar-refresh ()
  "Interactive sidebar refresh."
  (interactive)
  (ghostherd--sidebar-entries)
  (ghostherd--sidebar-print t)
  (when (fboundp 'ghostherd-usage-refresh)
    (ghostherd-usage-refresh))
  (when (and (ghostherd--sidebar-posframe-showing-p)
             (fboundp 'posframe-refresh))
    (posframe-refresh (current-buffer)))
  (message "ghostherd refreshed"))

(defun ghostherd--sidebar-session-at-point ()
  "Return the session for the sidebar row at point."
  (ghostherd-get (tabulated-list-get-id)))

(defun ghostherd-sidebar-visit ()
  "Visit the session at point, attaching a view when it is detached."
  (interactive)
  (when-let* ((s (ghostherd--sidebar-session-at-point)))
    (ghostherd-visit s)))

(defun ghostherd-sidebar-changes ()
  "Show what the checkout of the session at point has not committed: its
status in magit, or `vc-dir' without magit."
  (interactive)
  (when-let* ((s (ghostherd--sidebar-session-at-point)))
    (let ((dir (ghostherd-session-directory s)))
      (unless (and dir (file-directory-p (expand-file-name dir)))
        (user-error "%s has no directory here" (ghostherd-session-name s)))
      ;; The overlay's window is dedicated: magit has nowhere to go from it.
      (ghostherd--sidebar-leave-overlay)
      (if (require 'magit nil t)
          (magit-status-setup-buffer (expand-file-name dir))
        (vc-dir (expand-file-name dir))))))

(defun ghostherd-sidebar-kill ()
  "Kill the session at point."
  (interactive)
  (when-let* ((s (ghostherd--sidebar-session-at-point)))
    (when (y-or-n-p (format "Kill session %s? " (ghostherd-session-name s)))
      (ghostherd-kill s t))))

(defun ghostherd-sidebar-rename ()
  "Rename the session at point."
  (interactive)
  (when-let* ((s (ghostherd--sidebar-session-at-point)))
    (call-interactively
     (lambda ()
       (interactive)
       (ghostherd-rename
        s
        (read-string "New name: " (ghostherd-session-name s)))))))

(defun ghostherd-sidebar-notes ()
  "Edit the note on the session at point."
  (interactive)
  (when-let* ((s (ghostherd--sidebar-session-at-point)))
    (ghostherd-set-notes
     s (read-string "Note: " (ghostherd-session-notes s)))))

(defun ghostherd-sidebar-interrupt ()
  "Send Escape to the session at point."
  (interactive)
  (when-let* ((s (ghostherd--sidebar-session-at-point)))
    (ghostherd-interrupt s)))

(defun ghostherd-sidebar-abort ()
  "Send C-c to the session at point."
  (interactive)
  (when-let* ((s (ghostherd--sidebar-session-at-point)))
    (ghostherd-abort s)))

(defun ghostherd-sidebar-answer ()
  "Answer the menu prompt of the session at point."
  (interactive)
  (when-let* ((s (ghostherd--sidebar-session-at-point)))
    (ghostherd-answer s (read-number "Choice (1-based): " 1))))

(defun ghostherd--sidebar-caller-session ()
  "Session that opened the list, if the parent window is an agent view.

The overlay buffer is never a session -- `ghostherd-get' of it is nil,
and treating that as FROM used to be harmless only because
`ghostherd-message' falls back to \"user\".  The parent frame's window
is the one that had focus before the overlay, so a message sent from
an agent view is still attributed to that agent."
  (when (and ghostherd--sidebar-posframe-parent
             (frame-live-p ghostherd--sidebar-posframe-parent))
    (ghostherd-get (window-buffer
                    (frame-selected-window
                     ghostherd--sidebar-posframe-parent)))))

(defun ghostherd-sidebar-message ()
  "Message the session at point."
  (interactive)
  (when-let* ((s (ghostherd--sidebar-session-at-point))
              (body (read-string
                     (format "Message → %s: " (ghostherd-session-name s)))))
    ;; SUBMIT is a keyword.  A bare `t' is `Keyword argument t not one
    ;; of (:submit)' -- which is how overlay `m' failed while
    ;; `SPC a h m' worked, the interactive command having used :submit.
    (ghostherd-message (or (ghostherd--sidebar-caller-session) "user")
                       s body :submit t)))

(defun ghostherd-sidebar-prompt ()
  "Prompt the session at point."
  (interactive)
  (when-let* ((s (ghostherd--sidebar-session-at-point))
              (body (read-string
                     (format "Prompt %s: " (ghostherd-session-name s)))))
    (ghostherd-prompt s body)
    (message "Prompted %s" (ghostherd-session-name s))))

(defun ghostherd-sidebar-mark-state ()
  "Manually mark state for session at point."
  (interactive)
  (when-let* ((s (ghostherd--sidebar-session-at-point)))
    (ghostherd-mark-state
     s
     (intern (completing-read
              "State: "
              '("auto" "working" "blocked" "idle" "done" "dead")
              nil t)))))

(defconst ghostherd--sidebar-all-projects "All projects"
  "Candidate that clears the project filter.")

(defun ghostherd--sidebar-project-table ()
  "List of (DISPLAY ROOT COUNT) for the projects the herd has sessions in."
  (let ((seen nil))
    (dolist (s (ghostherd-sessions))
      (when-let* ((p (ghostherd-session-project s)))
        (let* ((root (file-name-as-directory (expand-file-name p)))
               (cell (assoc root seen)))
          (if cell
              (cl-incf (nth 1 cell))
            (push (list root 1) seen)))))
    (mapcar (lambda (c) (list (ghostherd--abbreviate (car c)) (car c) (nth 1 c)))
            (cl-sort seen #'string< :key #'car))))

(defun ghostherd--sidebar-caller-project ()
  "Project root of the buffer the list was opened over, or nil.

The overlay buffer's own `default-directory' is wherever the
posframe happened to be created, so `project-current' asked from
inside it answers for ghostherd's own directory no matter which
project you were actually looking at."
  (let ((buf (and ghostherd--sidebar-posframe-parent
                  (frame-live-p ghostherd--sidebar-posframe-parent)
                  (window-buffer (frame-selected-window
                                  ghostherd--sidebar-posframe-parent)))))
    (if (buffer-live-p buf)
        (with-current-buffer buf (ghostherd--project-root))
      (ghostherd--project-root))))

(defun ghostherd--sidebar-pick-candidates ()
  "Picker candidates: `All projects' first, then one row per project."
  (cons (list ghostherd--sidebar-all-projects nil
              (length (ghostherd-sessions)))
        (ghostherd--sidebar-project-table)))

(defun ghostherd--sidebar-pick-initial (candidates)
  "Index in CANDIDATES the picker should open on.
The row under point when nothing is filtered -- it is the one
thing on screen that says which project you mean -- and `All
projects' when something is, so `s RET' still just clears."
  (if ghostherd--sidebar-filter-project
      0
    (let* ((s (ghostherd--sidebar-session-at-point))
           (root (or (and s (ghostherd-session-project s))
                     (ghostherd--sidebar-caller-project))))
      (or (and root
               (seq-position candidates root
                             (lambda (c r)
                               (and (nth 1 c)
                                    (ignore-errors (file-equal-p (nth 1 c) r))))))
          0))))

(defun ghostherd--sidebar-pick-text ()
  "Render the project picker for the overlay pane."
  (let* ((cands ghostherd--sidebar-picking)
         (width (max 24 (1- (ghostherd--sidebar-available-width))))
         (namew (max 12 (min (apply #'max 0 (mapcar (lambda (c) (length (car c)))
                                                    cands))
                             (- width 20)))))
    (concat
     (propertize "Show project   RET choose · n/p move · 1-9 jump · Esc cancel"
                 'face 'shadow)
     "\n"
     (string-join
      (cl-loop
       for c in cands
       for i from 0
       collect
       (let* ((selected (= i ghostherd--sidebar-pick-index))
              (n (nth 2 c))
              (line (format "%s%2d  %s  %s"
                            (if selected "›" " ")
                            (1+ i)
                            (string-pad
                             (truncate-string-to-width (car c) namew nil nil t)
                             namew)
                            (if (nth 1 c)
                                (format "%d session%s" n (if (= n 1) "" "s"))
                              (format "%d in all" n)))))
         (if selected
             (propertize line 'face 'highlight)
           line)))
      "\n"))))

(defun ghostherd--sidebar-pick-redraw ()
  "Redraw the pane and refit the overlay around it."
  (when-let* ((buf (get-buffer "*ghostherd*")))
    (with-current-buffer buf
      (ghostherd--sidebar-draw-preview)
      (when (ghostherd--sidebar-posframe-showing-p)
        (ghostherd--sidebar-show-posframe buf))))
  (force-mode-line-update t))

(defun ghostherd--set-picking (candidates)
  "Open the picker on CANDIDATES, or close it when nil.
State and keymap only -- the caller redraws.  Refreshes Evil's map
alist for the same reason live-narrow does: the intercept map has
to outrank the overlay's normal-state letters."
  (setq ghostherd--sidebar-picking candidates)
  (when-let* ((buf (get-buffer "*ghostherd*")))
    (with-current-buffer buf
      (ghostherd-pick-mode (if candidates 1 -1))
      (when (fboundp 'evil-normalize-keymaps)
        (evil-normalize-keymaps)))))

(defun ghostherd--sidebar-apply-filter (root)
  "Show only sessions under ROOT, or all of them when ROOT is nil."
  (setq ghostherd--sidebar-filter-project root)
  (when-let* ((buf (get-buffer "*ghostherd*")))
    (with-current-buffer buf
      (when (derived-mode-p 'ghostherd-sidebar-mode)
        (ghostherd--sidebar-build-entries)
        (ghostherd--sidebar-print t)
        (when (ghostherd--sidebar-posframe-showing-p)
          (ghostherd--sidebar-show-posframe buf)))))
  (force-mode-line-update t)
  (message "Project filter: %s" (if root (ghostherd--abbreviate root) "off")))

(defun ghostherd-sidebar-project-filter ()
  "Choose which project the session list shows.

The picker draws in the pane `?' and `v' already use and takes its
keys there, like live-narrow.  It used to be a `completing-read':
in the posframe overlay that put the prompt at the bottom of the
parent frame, nowhere near the floating list you were reading, and
left the order to whatever completion UI happened to be installed.

`s' again closes it; RET applies the highlighted row."
  (interactive)
  (if ghostherd--sidebar-picking
      (ghostherd-sidebar-pick-cancel)
    (let ((cands (ghostherd--sidebar-pick-candidates)))
      (when (and (null (cdr cands)) (not ghostherd--sidebar-filter-project))
        (user-error "No sessions with a project"))
      (setq ghostherd--sidebar-pick-index (ghostherd--sidebar-pick-initial cands))
      (ghostherd--set-picking cands)
      (ghostherd--sidebar-pick-redraw))))

(defun ghostherd-sidebar-pick-cancel ()
  "Close the project picker, leaving the filter as it was."
  (interactive)
  (ghostherd--set-picking nil)
  (ghostherd--sidebar-pick-redraw))

(defun ghostherd-sidebar-pick-confirm ()
  "Apply the selected project and close the picker."
  (interactive)
  (let ((root (nth 1 (nth ghostherd--sidebar-pick-index
                          ghostherd--sidebar-picking))))
    (ghostherd--set-picking nil)
    (ghostherd--sidebar-apply-filter root)))

(defun ghostherd--sidebar-pick-move (delta)
  "Move the picker selection by DELTA, wrapping at both ends."
  (when ghostherd--sidebar-picking
    (setq ghostherd--sidebar-pick-index
          (mod (+ ghostherd--sidebar-pick-index delta)
               (length ghostherd--sidebar-picking)))
    (ghostherd--sidebar-pick-redraw)))

(defun ghostherd-sidebar-pick-next ()
  "Select the next project."
  (interactive)
  (ghostherd--sidebar-pick-move 1))

(defun ghostherd-sidebar-pick-previous ()
  "Select the previous project."
  (interactive)
  (ghostherd--sidebar-pick-move -1))

(defun ghostherd-sidebar-pick-digit ()
  "Apply the numbered project straight away."
  (interactive)
  (let ((n (- (event-basic-type last-command-event) ?0)))
    (if (and (>= n 1) (<= n (length ghostherd--sidebar-picking)))
        (progn (setq ghostherd--sidebar-pick-index (1- n))
               (ghostherd-sidebar-pick-confirm))
      (message "No project %d" n))))

(defun ghostherd-sidebar-pick-ignore ()
  "Swallow a key that means nothing to the picker.
Without this, `x' while picking would reach the overlay map and
kill the session under point."
  (interactive)
  nil)

(defvar-keymap ghostherd-sidebar-pick-map
  :doc "Keymap while the project picker owns the overlay pane.
Modal on purpose: every other key is swallowed rather than run as
an overlay command.  Esc, C-g, q and `s' all close it."
  "RET"         #'ghostherd-sidebar-pick-confirm
  "C-m"         #'ghostherd-sidebar-pick-confirm
  "<return>"    #'ghostherd-sidebar-pick-confirm
  "SPC"         #'ghostherd-sidebar-pick-confirm
  "n"           #'ghostherd-sidebar-pick-next
  "p"           #'ghostherd-sidebar-pick-previous
  "j"           #'ghostherd-sidebar-pick-next
  "k"           #'ghostherd-sidebar-pick-previous
  "C-n"         #'ghostherd-sidebar-pick-next
  "C-p"         #'ghostherd-sidebar-pick-previous
  "<down>"      #'ghostherd-sidebar-pick-next
  "<up>"        #'ghostherd-sidebar-pick-previous
  "s"           #'ghostherd-sidebar-pick-cancel
  "q"           #'ghostherd-sidebar-pick-cancel
  "C-g"         #'ghostherd-sidebar-pick-cancel
  "<escape>"    #'ghostherd-sidebar-pick-cancel)

(define-key ghostherd-sidebar-pick-map [t] #'ghostherd-sidebar-pick-ignore)
;; After `[t]': events the catch-all would otherwise swallow before
;; `function-key-map' translates them (see the filter map).
(define-key ghostherd-sidebar-pick-map [return] #'ghostherd-sidebar-pick-confirm)
(define-key ghostherd-sidebar-pick-map [kp-enter] #'ghostherd-sidebar-pick-confirm)
(define-key ghostherd-sidebar-pick-map [escape] #'ghostherd-sidebar-pick-cancel)
(dotimes (i 9)
  (define-key ghostherd-sidebar-pick-map (vector (+ ?1 i))
              #'ghostherd-sidebar-pick-digit))

(define-minor-mode ghostherd-pick-mode
  "Pick a project filter inside the ghostherd overlay.
A minor mode for the same reason `ghostherd-filter-mode' is one:
Evil's state maps sit in `emulation-mode-map-alists' and would
swallow RET before a transient map saw it."
  :lighter nil
  :keymap ghostherd-sidebar-pick-map)

(define-obsolete-function-alias 'ghostherd-sidebar-toggle-project-filter
  'ghostherd-sidebar-project-filter "2026-09-18")

(defun ghostherd--sidebar-set-query (query)
  "Set the live-narrow QUERY and redraw, keeping point on the same row."
  (setq ghostherd--sidebar-query (or query ""))
  (when-let* ((buf (get-buffer "*ghostherd*")))
    (with-current-buffer buf
      (when (derived-mode-p 'ghostherd-sidebar-mode)
        (ghostherd--sidebar-build-entries)
        (ghostherd--sidebar-print t)
        (when (and (ghostherd--sidebar-posframe-showing-p)
                   (fboundp 'posframe-refresh))
          (posframe-refresh buf)))))
  (force-mode-line-update t))

(defun ghostherd--return-event-p (ev)
  "Return non-nil if EV is Enter in any GUI/tty encoding.

A catch-all `[t]' on the filter map is looked up *before*
`function-key-map' translates `<return>' to RET.  The default
binding then runs instead of the RET command, and a printable-only
self-insert silently drops it -- which is why Enter appeared to
do nothing, or jumped straight to visit."
  (or (memq ev '(return kp-enter newline S-return))
      (and (characterp ev) (memq ev '(?\r ?\n)))
      (memq (event-basic-type ev) '(return kp-enter newline))))

(defun ghostherd-sidebar-filter-self-insert ()
  "Append `last-command-event' to the live query, or apply on Enter."
  (interactive)
  (let ((ev last-command-event))
    (cond
     ((ghostherd--return-event-p ev)
      (ghostherd-sidebar-filter-confirm))
     ((and (characterp ev) (>= ev 32) (not (eq ev 127)))
      (ghostherd--sidebar-set-query
       (concat ghostherd--sidebar-query (char-to-string ev)))))))

(defun ghostherd-sidebar-filter-backspace ()
  "Drop the last character of the live query."
  (interactive)
  (when (> (length ghostherd--sidebar-query) 0)
    (ghostherd--sidebar-set-query
     (substring ghostherd--sidebar-query 0 -1))))

(defun ghostherd-sidebar-filter-clear ()
  "Empty the live query without leaving filter-mode."
  (interactive)
  (ghostherd--sidebar-set-query ""))

(defun ghostherd--set-filtering (on)
  "Turn live-narrow on or off, including `ghostherd-filter-mode'.
Refresh Evil's map alist so the filter intercept map actually
outranks the overlay's normal-state letters."
  (setq ghostherd--sidebar-filtering (and on t))
  (when-let* ((buf (get-buffer "*ghostherd*")))
    (with-current-buffer buf
      (ghostherd-filter-mode (if ghostherd--sidebar-filtering 1 -1))
      (when (fboundp 'evil-normalize-keymaps)
        (evil-normalize-keymaps))))
  (force-mode-line-update t))

(defun ghostherd-sidebar-filter-confirm ()
  "Keep the current query and leave live-narrow.
Returns `j'/`k' and RET to the overlay: move, then visit."
  (interactive)
  (ghostherd--set-filtering nil))

(defvar-keymap ghostherd-sidebar-filter-map
  :doc "Keymap while the session list is live-narrowing.
Printable keys append to the query; `n'/`p' still move.  RET applies
the query and returns to overlay keys so `j'/`k' move and RET visits.
`/` itself is how you enter this map, so it is not a query character."
  "RET"         #'ghostherd-sidebar-filter-confirm
  "C-m"         #'ghostherd-sidebar-filter-confirm
  "<return>"    #'ghostherd-sidebar-filter-confirm
  "n"           #'next-line
  "p"           #'previous-line
  "C-n"         #'next-line
  "C-p"         #'previous-line
  "<down>"      #'next-line
  "<up>"        #'previous-line
  "DEL"         #'ghostherd-sidebar-filter-backspace
  "<backspace>" #'ghostherd-sidebar-filter-backspace
  "<delete>"    #'ghostherd-sidebar-filter-backspace
  "C-h"         #'ghostherd-sidebar-filter-backspace
  "C-u"         #'ghostherd-sidebar-filter-clear
  "C-g"         #'ghostherd-sidebar-quit
  "<escape>"    #'ghostherd-sidebar-quit)

(define-key ghostherd-sidebar-filter-map [t]
            #'ghostherd-sidebar-filter-self-insert)
;; After `[t]': specific events the catch-all would otherwise swallow
;; before function-key-map translation.
(define-key ghostherd-sidebar-filter-map [return]
            #'ghostherd-sidebar-filter-confirm)
(define-key ghostherd-sidebar-filter-map [kp-enter]
            #'ghostherd-sidebar-filter-confirm)
(define-key ghostherd-sidebar-filter-map [S-return]
            #'ghostherd-sidebar-filter-confirm)
(define-key ghostherd-sidebar-filter-map (kbd "C-j")
            #'ghostherd-sidebar-filter-confirm)

(defun ghostherd--bind-filter-query-keys ()
  "Bind printable keys on the filter map to self-insert.

Evil looks up a specific binding in the overlay's normal-state
map (`a' answer, `e' explain, …) before a `[t]' default on a
lower map is considered.  Explicit letters on the intercept map
make `/` then `a` search rather than run the overlay command.
`n'/`p' stay as motion so a match can be highlighted while typing."
  (let ((c 32))
    (while (< c 127)
      (unless (memq c '(?n ?p 127))
        (define-key ghostherd-sidebar-filter-map (vector c)
                    #'ghostherd-sidebar-filter-self-insert))
      (setq c (1+ c)))))

(ghostherd--bind-filter-query-keys)

(define-minor-mode ghostherd-filter-mode
  "Live-narrow the ghostherd overlay.

A minor mode rather than `set-transient-map': evil's state maps
sit in `emulation-mode-map-alists' and can swallow RET before a
terminal-local transient map sees it.  An intercept keymap on this
mode outranks evil, so Enter applies the filter."
  :lighter nil
  :keymap ghostherd-sidebar-filter-map)

(defun ghostherd-sidebar-filter ()
  "Start live-narrowing the session list.

Printable keys append to the query; `n'/`p' still move.  RET keeps
the query and returns to overlay keys (`j'/`k' move, RET visits).
Esc clears a non-empty query, and dismisses the list when the
query is already empty.  Matches name, kind, state, project,
notes and title as a flex subsequence; whitespace is AND."
  (interactive)
  (ghostherd--set-filtering t))

(defun ghostherd--posframe-available-p ()
  "Return non-nil when posframe can actually display a child frame.

`require' succeeding is not enough: batch, a tty, and a frame
without its own minibuffer all fail `posframe-workable-p', and
those are exactly the cases that must fall back."
  (and (require 'posframe nil t)
       (fboundp 'posframe-workable-p)
       (posframe-workable-p)))

(defun ghostherd--use-posframe-p ()
  "Return non-nil when the session list should open as a posframe."
  (and ghostherd-sidebar-use-posframe
       (ghostherd--posframe-available-p)))

(defun ghostherd--sidebar-posframe-frame ()
  "Return the session list's posframe, or nil."
  (when-let* ((buf (get-buffer "*ghostherd*")))
    (and (boundp 'posframe--frame)
         (buffer-local-value 'posframe--frame buf))))

(defun ghostherd--sidebar-posframe-showing-p ()
  "Return non-nil when the session list is a visible posframe."
  (when-let* ((frame (ghostherd--sidebar-posframe-frame)))
    (and (frame-live-p frame)
         (frame-visible-p frame))))

(defun ghostherd--sidebar-hide-posframe ()
  "Hide the overlay and restore input focus to its parent frame."
  (let ((parent ghostherd--sidebar-posframe-parent)
        (buf (get-buffer "*ghostherd*")))
    (setq ghostherd--sidebar-posframe-parent nil
          ghostherd--sidebar-posframe-fitted-width nil
          ghostherd--sidebar-query ""
          ghostherd--sidebar-help-visible nil
          ghostherd--sidebar-preview-id nil)
    (ghostherd--set-filtering nil)
    (ghostherd--set-picking nil)
    (when (timerp ghostherd--sidebar-preview-timer)
      (cancel-timer ghostherd--sidebar-preview-timer)
      (setq ghostherd--sidebar-preview-timer nil))
    (when (and buf (fboundp 'posframe-hide))
      (posframe-hide buf))
    (when (and parent (frame-live-p parent))
      (select-frame-set-input-focus parent))))

(defun ghostherd--sidebar-leave-overlay ()
  "Dismiss the posframe overlay so a subsequent display uses a real window.

The overlay's window is dedicated and its frame is unsplittable.
`pop-to-buffer' from there has nowhere to put an agent or a log
-- or would put it *in* the child frame.  `?' no longer goes
through this: the legend draws in the overlay's own preview pane.
The side-window display is left alone: it is a dashboard, and
visit has always kept it open."
  (when (ghostherd--sidebar-posframe-showing-p)
    (ghostherd--sidebar-hide-posframe)))

(defun ghostherd-sidebar-quit ()
  "Dismiss the session list, or the live query first.

A non-empty `/` query is cleared and filter-mode ends, so the next
key is a command again (`z', `k').  An empty query dismisses: the
posframe overlay hides, or the side window quits."
  (interactive)
  (cond
   (ghostherd--sidebar-picking
    (ghostherd-sidebar-pick-cancel))
   ((and ghostherd--sidebar-query
         (not (string-empty-p ghostherd--sidebar-query)))
    (ghostherd--sidebar-set-query "")
    (ghostherd--set-filtering nil))
   (ghostherd--sidebar-help-visible
    (setq ghostherd--sidebar-help-visible nil)
    (ghostherd--sidebar-draw-preview)
    (when (ghostherd--sidebar-posframe-showing-p)
      (ghostherd--sidebar-show-posframe (get-buffer "*ghostherd*")))
    (force-mode-line-update t))
   (t
    (setq ghostherd--sidebar-query "")
    (ghostherd--set-filtering nil)
    (if (ghostherd--sidebar-posframe-showing-p)
        (ghostherd--sidebar-hide-posframe)
      (quit-window)))))

(defun ghostherd--sidebar-close-side-windows (buf)
  "Delete any side window showing BUF, so the overlay does not share a frame."
  (dolist (win (get-buffer-window-list buf nil t))
    (when (and (window-parameter win 'window-side)
               (not (frame-parent (window-frame win))))
      (ignore-errors (delete-window win)))))

(defun ghostherd--sidebar-posframe-border-color ()
  "Border colour for the overlay, taken from the current theme."
  (or (face-foreground 'vertical-border nil t)
      (face-foreground 'mode-line-inactive nil t)
      "gray50"))

(defun ghostherd--sidebar-posframe-parent-frame ()
  "Frame the overlay should be sized against and return focus to."
  (or (and (frame-live-p ghostherd--sidebar-posframe-parent)
           ghostherd--sidebar-posframe-parent)
      (frame-parent (selected-frame))
      (selected-frame)))

(defun ghostherd--sidebar-posframe-char-width (&optional frame)
  "Character width of the overlay on FRAME.

Scales with `ghostherd-sidebar-posframe-width-ratio' so a full-screen
Emacs does not get a 72-column stamp in the middle.  The configured
width is the floor; the parent minus a margin is the ceiling."
  (let* ((frame (or frame (ghostherd--sidebar-posframe-parent-frame)))
         (avail (max 1 (frame-width frame)))
         (margin (min 12 (max 4 (/ avail 16))))
         (ceil (max 48 (- avail margin)))
         (floor (min ghostherd-sidebar-posframe-width ceil)))
    (if (null ghostherd-sidebar-posframe-width-ratio)
        floor
      (let ((wanted (round (* ghostherd-sidebar-posframe-width-ratio avail))))
        (max floor (min wanted ceil))))))

(defun ghostherd--sidebar-posframe-max-height (&optional frame)
  "Max overlay height in lines, as a fraction of FRAME."
  (let* ((frame (or frame (ghostherd--sidebar-posframe-parent-frame)))
         (avail (max 8 (frame-height frame))))
    (max 8 (min (- avail 4) (round (* 0.72 avail))))))

(defun ghostherd--sidebar-refit-posframe (width)
  "Rebuild the overlay's columns for WIDTH and show it again."
  (when-let* ((buf (get-buffer "*ghostherd*")))
    (let ((ghostherd--sidebar-posframe-refitting t)
          (ghostherd--sidebar-target-width width))
      (setq ghostherd--sidebar-posframe-fitted-width width)
      (with-current-buffer buf
        (when (derived-mode-p 'ghostherd-sidebar-mode)
          (ghostherd--sidebar-build-entries)
          (ghostherd--sidebar-print t)))
      (ghostherd--sidebar-show-posframe buf))))

(defun ghostherd--sidebar-posframe-on-parent-resize (frame)
  "Refit the overlay when its parent FRAME changes size.
Ignores the child frame itself, which also resizes as we show it."
  (when (and (not ghostherd--sidebar-posframe-refitting)
             ghostherd--sidebar-posframe-parent
             (eq frame ghostherd--sidebar-posframe-parent)
             (ghostherd--sidebar-posframe-showing-p))
    (let ((width (ghostherd--sidebar-posframe-char-width frame)))
      (unless (eql width ghostherd--sidebar-posframe-fitted-width)
        (ghostherd--sidebar-refit-posframe width)))))

(defun ghostherd--sidebar-show-posframe (buf)
  "Show BUF as a centred, focusable posframe overlay."
  (ghostherd--sidebar-close-side-windows buf)
  (let* ((parent (ghostherd--sidebar-posframe-parent-frame))
         (width (or ghostherd--sidebar-target-width
                    (ghostherd--sidebar-posframe-char-width parent)))
         (rows (with-current-buffer buf
                 (length tabulated-list-entries)))
         (preview (cond
                   (ghostherd--sidebar-help-visible
                    (max 8 (length (split-string
                                    (ghostherd--sidebar-help-text) "\n"))))
                   (ghostherd-sidebar-show-preview
                    (+ 2 ghostherd-sidebar-preview-lines))
                   (t 0))))
    (setq ghostherd--sidebar-posframe-parent parent
          ghostherd--sidebar-posframe-fitted-width width)
    (add-hook 'window-size-change-functions
              #'ghostherd--sidebar-posframe-on-parent-resize)
    ;; `posframe-show' takes the selected frame as parent.  Calling it
    ;; from inside the overlay would nest a child frame in itself.
    (with-selected-frame parent
      (posframe-show
       buf
       :poshandler #'posframe-poshandler-frame-center
       :position (point)
       :width width
       :min-width (min 48 width)
       :min-height (max 4 (+ 1 rows preview))
       :max-height (ghostherd--sidebar-posframe-max-height parent)
       :left-fringe 12
       :right-fringe 12
       :border-width 2
       :border-color (ghostherd--sidebar-posframe-border-color)
       :respect-header-line t
       :respect-mode-line t
       :lines-truncate t
       :cursor 'box
       :accept-focus t
       :window-point (with-current-buffer buf (point))))
    (when-let* ((frame (ghostherd--sidebar-posframe-frame)))
      (select-frame-set-input-focus frame)
      (select-window (frame-root-window frame)))))

(defun ghostherd--sidebar-show-side-window (buf)
  "Show BUF in a side window -- the display used when posframe cannot."
  (when (fboundp 'posframe-hide)
    (posframe-hide buf))
  (setq ghostherd--sidebar-posframe-parent nil)
  (pop-to-buffer
   buf
   `((display-buffer-in-side-window)
     (side . ,ghostherd-sidebar-side)
     (slot . 0)
     (window-width . ,ghostherd-sidebar-width)
     (preserve-size . (t . nil)))))

(defun ghostherd--sidebar-prepare-buffer ()
  "Return the session-list buffer, filled from the current registry."
  (let ((buf (get-buffer-create "*ghostherd*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'ghostherd-sidebar-mode)
        (ghostherd-sidebar-mode))
      (ghostherd--sidebar-entries)
      (ghostherd--sidebar-print t))
    buf))

;;;###autoload
(defun ghostherd-sidebar ()
  "Open the ghostherd session list.

Uses a posframe overlay when `ghostherd-sidebar-use-posframe' is
non-nil and posframe can display a child frame.  Otherwise opens
the original side window.  The buffer and keys are the same
either way; Esc or `q' dismisses."
  (interactive)
  (ghostherd--maybe-restore)
  (ghostherd--ensure-sessions)
  (setq ghostherd--sidebar-query ""
        ghostherd--sidebar-help-visible nil
        ghostherd--sidebar-preview-id nil)
  (ghostherd--set-filtering nil)
  (let* ((overlay (ghostherd--use-posframe-p))
         (ghostherd--sidebar-target-width
          (and overlay (ghostherd--sidebar-posframe-char-width)))
         (buf (ghostherd--sidebar-prepare-buffer)))
    (if overlay
        (ghostherd--sidebar-show-posframe buf)
      (ghostherd--sidebar-show-side-window buf))
    (when (fboundp 'ghostherd-usage-refresh)
      (ghostherd-usage-refresh)
      (ghostherd-usage--ensure-timer))
    buf))


;;; Global minor mode / modeline

(defvar ghostherd--mode-line-kept nil
  "(COUNTS . SEGMENT): the mode-line segment last made, and the counts it
was made of.")

(defun ghostherd--mode-line-segment ()
  "Mode-line segment summarizing herd attention.

Reads the registry and nothing else.  It used to sweep it first, which
was free while a host check was `buffer-live-p' and is not once a host
check can run a subprocess -- redisplay is no place for I/O, and no
place to discover that the I/O failed either.  The poll timer already
sweeps every `ghostherd-poll-interval'.

Asked at every redisplay of every mode line -- a terminal streaming an
agent's work redraws its own many times a second -- so it counts in one
pass and makes the segment again only when a count changed."
  (let ((blocked 0) (working 0) (waiting 0) (done 0) (total 0))
    (maphash (lambda (_id s)
               (cl-incf total)
               (pcase (ghostherd-session-state s)
                 ('blocked (cl-incf blocked))
                 ('working (cl-incf working))
                 ('waiting (cl-incf waiting))
                 ('done (cl-incf done))))
             ghostherd--sessions)
    (let ((counts (list total blocked working waiting done)))
      (unless (equal counts (car ghostherd--mode-line-kept))
        (setq ghostherd--mode-line-kept
              (cons counts (ghostherd--mode-line-make total blocked working waiting done))))
      ;; A copy, as it always was a fresh string: a mode line may add to it.
      (copy-sequence (cdr ghostherd--mode-line-kept)))))

(defun ghostherd--mode-line-make (total blocked working waiting done)
  "The mode-line segment for these counts of the herd's agents, or nil."
  (when (> total 0)
    (propertize
     (format " 🐄%d%s%s%s%s"
             total
             (if (> blocked 0) (format " ⚠%d" blocked) "")
             (if (> working 0) (format " ⟳%d" working) "")
             (if (> waiting 0) (format " ◷%d" waiting) "")
             (if (> done 0) (format " ✓%d" done) ""))
     'face (cond ((> blocked 0) 'error)
                 ((> done 0) 'success)
                 ((> working 0) 'warning)
                 (t 'shadow))
     'help-echo "ghostherd sessions (click for sidebar)"
     'mouse-face 'mode-line-highlight
     'local-map (define-keymap
                  "<mode-line> <mouse-1>" #'ghostherd-sidebar))))

;;;###autoload
(defun ghostherd--herd-page-for-phone ()
  "Load the herd page now, when your phone reaches it (`xwapp-remote-hosts'):
its address without the secret, a push's room and the bot's links all
want it loaded, and after a restart nothing here has opened it yet."
  (when (bound-and-true-p xwapp-remote-hosts)
    (with-demoted-errors "ghostherd: herd page: %S"
      (require 'ghostherd-memory-page))))

(define-minor-mode ghostherd-mode
  "Global minor mode for ghostherd modeline indicator and hooks."
  :global t
  :group 'ghostherd
  (if ghostherd-mode
      (progn
        (ghostherd--install-hooks)
        (ghostherd--register-eval-cmds)
        ;; The log first, so what the previous Emacs saw is above what this
        ;; one is about to do: restore writes its own lines.
        (unless ghostherd--log-loaded
          (ignore-errors (ghostherd-log-load)))
        ;; Enabling the mode is the restore hook: a herd left running by
        ;; a previous Emacs is picked up here.  Deferred to a zero-delay
        ;; timer so the whole file and its state are loaded first (a
        ;; synchronous restore here could race `ghostherd--worktrees' void),
        ;; and retried past a transient; idempotent.
        (ghostherd--restore-deferred)
        ;; Demoted, not ignored: a sidecar on an old token makes start
        ;; refuse, and without a word agents would just spawn with no
        ;; GHOSTHERD_RPC.
        (with-demoted-errors "ghostherd: memory sidecar: %S"
          (ghostherd-memory-start))
        (ghostherd--herd-page-for-phone)
        (ghostherd-memory-auto-import-start)
        (ignore-errors (ghostherd--herd-write-protocol))
        (ghostherd--ensure-poll-timer)
        (ghostherd--ensure-herd-tick-timer)
        (unless (member '(:eval (ghostherd--mode-line-segment)) mode-line-misc-info)
          (setq mode-line-misc-info
                (append mode-line-misc-info
                        '((:eval (ghostherd--mode-line-segment)))))))
    (ghostherd--remove-hooks)
    (when (timerp ghostherd--poll-timer)
      (cancel-timer ghostherd--poll-timer)
      (setq ghostherd--poll-timer nil))
    (ghostherd--stop-herd-tick-timer)
    (ghostherd-memory-auto-import-stop)
    (setq mode-line-misc-info
          (cl-remove '(:eval (ghostherd--mode-line-segment))
                     mode-line-misc-info
                     :test #'equal))))

(when (bound-and-true-p ghostherd-mode)
  (ghostherd--ensure-herd-tick-timer))


;;; Transient-ish quick menu (no transient dependency)

(defconst ghostherd-menu-choices
  '(("n" "new agent" ghostherd-new)
    ("2" "new pair (split)" ghostherd-new-pair)
    ("s" "switch" ghostherd-switch)
    ("b" "sidebar" ghostherd-sidebar)
    ("." "next blocked" ghostherd-next-blocked)
    ("m" "message agent" ghostherd-message-interactive)
    ("i" "prompt agent" ghostherd-prompt-interactive)
    ("k" "kill" ghostherd-kill)
    ("r" "rename" ghostherd-rename)
    ("M" "mark state" ghostherd-mark-state)
    ("l" "herd log" ghostherd-log)
    ("h" "scrollback" ghostherd-scrollback)
    ("/" "search memory" ghostherd-memory-search)
    ("v" "view memory" ghostherd-memory-view)
    ("I" "import memory" ghostherd-memory-import)
    ("g" "poll now" ghostherd-poll-all))
  "Key / label / command rows for `ghostherd-menu'.")

;;;###autoload
(defun ghostherd-menu ()
  "Simple command dispatcher for ghostherd."
  (interactive)
  (let* ((choices ghostherd-menu-choices)
         (key (char-to-string
               (read-char
                (concat "ghostherd: "
                        (mapconcat (lambda (c)
                                     (format "%s=%s" (nth 0 c) (nth 1 c)))
                                   choices "  ")))))
         (cmd (nth 2 (assoc key choices))))
    (if cmd
        (call-interactively cmd)
      (user-error "Unknown choice %s" key))))

(defun ghostherd--maybe-setup-evil-sidebar ()
  "Bind overlay keys in evil normal state so they are not shadowed.

Must call `evil-define-key*', the function.  Filter-mode uses an
intercept map so RET applies the query instead of visiting.  `j'/`k'
stay as motion.  `?' is evil-search unless it is on this map; the
full overlay alphabet has to live here, not only `/'.

`x' is kill and `gr' is refresh: `k' and `g' are evil motion/prefix."
  (when (fboundp 'evil-make-intercept-map)
    (evil-make-intercept-map ghostherd-sidebar-filter-map 'normal)
    (evil-make-intercept-map ghostherd-sidebar-pick-map 'normal))
  (when (fboundp 'evil-define-key*)
    (evil-define-key* 'normal ghostherd-sidebar-pick-map
      (kbd "RET") #'ghostherd-sidebar-pick-confirm
      (kbd "<return>") #'ghostherd-sidebar-pick-confirm
      (kbd "C-m") #'ghostherd-sidebar-pick-confirm
      (kbd "j") #'ghostherd-sidebar-pick-next
      (kbd "k") #'ghostherd-sidebar-pick-previous
      (kbd "n") #'ghostherd-sidebar-pick-next
      (kbd "p") #'ghostherd-sidebar-pick-previous
      (kbd "s") #'ghostherd-sidebar-pick-cancel
      (kbd "q") #'ghostherd-sidebar-pick-cancel
      (kbd "<escape>") #'ghostherd-sidebar-pick-cancel)
    (dotimes (i 9)
      (evil-define-key* 'normal ghostherd-sidebar-pick-map
        (kbd (number-to-string (1+ i))) #'ghostherd-sidebar-pick-digit))
    (evil-define-key* 'normal ghostherd-sidebar-filter-map
      (kbd "RET") #'ghostherd-sidebar-filter-confirm
      (kbd "<return>") #'ghostherd-sidebar-filter-confirm
      (kbd "C-m") #'ghostherd-sidebar-filter-confirm
      (kbd "<escape>") #'ghostherd-sidebar-quit)
    (evil-define-key* 'normal ghostherd-sidebar-mode-map
      (kbd "?") #'ghostherd-sidebar-help
      (kbd "e") #'ghostherd-explain
      (kbd "R") #'ghostherd-respawn
      (kbd "c") #'ghostherd-sidebar-notes
      (kbd "z") #'ghostherd-sidebar-interrupt
      (kbd "Z") #'ghostherd-sidebar-abort
      (kbd "a") #'ghostherd-sidebar-answer
      (kbd "RET") #'ghostherd-sidebar-visit
      (kbd "<return>") #'ghostherd-sidebar-visit
      (kbd "o") #'ghostherd-sidebar-visit
      (kbd "N") #'ghostherd-new
      (kbd "P") #'ghostherd-new-pair
      (kbd "x") #'ghostherd-sidebar-kill
      (kbd "r") #'ghostherd-sidebar-rename
      (kbd "m") #'ghostherd-sidebar-message
      (kbd "i") #'ghostherd-sidebar-prompt
      (kbd "s") #'ghostherd-sidebar-project-filter
      (kbd "/") #'ghostherd-sidebar-filter
      (kbd "v") #'ghostherd-sidebar-toggle-preview
      (kbd "gr") #'ghostherd-sidebar-refresh
      (kbd "U") #'ghostherd-usage-refresh
      (kbd ".") #'ghostherd-next-blocked
      (kbd "H") #'ghostherd-scrollback
      (kbd "L") #'ghostherd-log
      (kbd "M") #'ghostherd-sidebar-mark-state
      (kbd "q") #'ghostherd-sidebar-quit
      (kbd "<escape>") #'ghostherd-sidebar-quit)))

(with-eval-after-load 'evil
  (ghostherd--maybe-setup-evil-sidebar))

(provide 'ghostherd)
;;; ghostherd.el ends here

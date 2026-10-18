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
     ;; so allow more than one before the ellipsis.
     :spinner "\\`[·✢✳✶✻✽*][[:blank:]]+[[:alpha:]]+\\(?:[[:blank:]]+[[:alpha:]]+\\)*…[[:blank:]]+(\\(?:[0-9]+[hms]\\)"
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
                  "\\(y/n\\)"))
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
                Seen of claude and agy; grok's prompts not yet.
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
around it, not for that case."
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
            (hack-dir-local-variables)
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

(defun ghostherd--worktree-main-checkout (dir)
  "If DIR is a linked git worktree, its main checkout's directory; else nil.
A linked worktree's own git dir sits under the main repo's
`.git/worktrees/', while its common dir is that main `.git'; the main
checkout is the common dir's parent.  An ordinary checkout, where the two
are the same, gives nil, as does a directory that is no worktree."
  (when (and (stringp dir) (file-directory-p (expand-file-name dir)))
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
                  (let ((main (file-name-directory (directory-file-name common))))
                    (and main (file-name-as-directory (expand-file-name main))))))))))))

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

(defun ghostherd--project-name (path)
  "PATH as a project is always written: a directory, under ~ when it is.
An agent spawned here carries the absolute name and a restored one the
abbreviated name; the page and the sidecar compare them as strings."
  (and path (not (string-empty-p path))
       (abbreviate-file-name (file-name-as-directory (expand-file-name path)))))

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

(defun ghostherd--worktree-create (project name &optional branch)
  "Create a git worktree of PROJECT for agent NAME; return its plist.
The worktree goes under `ghostherd-worktree-root' on a new BRANCH (NAME by
default) off PROJECT's current HEAD.  Returns (:path :branch :base).
Signals a `user-error' when git cannot, so the caller may fall back to a
plain directory."
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

(defun ghostherd--worktree-remove (main path &optional force branch)
  "Remove git worktree PATH of the MAIN checkout; return t on success.
With FORCE, discard the worktree's uncommitted changes.  With BRANCH, also
delete it with `git branch -d', which refuses an unmerged branch -- so a
branch's committed work is never lost here."
  (let ((default-directory (file-name-as-directory (expand-file-name main))))
    (with-temp-buffer
      (when (eq 0 (apply #'call-process "git" nil '(t t) nil
                         `("worktree" "remove" ,@(and force '("--force"))
                           ,(directory-file-name (expand-file-name path)))))
        (when branch
          (call-process "git" nil nil nil "branch" "-d" branch))
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
;; Carrying them in makes a worktree usable at once.  A venv holds absolute
;; paths, so it is symlinked to the main checkout's rather than copied; a
;; node_modules is cloned copy-on-write (APFS `cp -c'), so the worktree may
;; diverge it cheaply.  Build caches are simply left off the list.  A clone
;; that cannot be made is skipped and said so on the card -- never a full
;; copy, since the disk is not big.  All of it runs in the background.

(defcustom ghostherd-worktree-deps
  '((".venv" . symlink) ("venv" . symlink) ("node_modules" . clone))
  "Dependencies carried into a new worktree, each (PATH . METHOD).
PATH is relative to the checkout root.  METHOD is `symlink' -- a link to
the main checkout's, for a thing that holds absolute paths and breaks when
copied, like a Python venv -- or `clone', an APFS copy-on-write copy (cp
-c) the worktree may then diverge, like node_modules.  A PATH with no
source in the main checkout is skipped; a clone that cannot be made is
skipped and said so on the card, never copied in full.  Build caches are
left off this list.  Override per project with a
`ghostherd-project-worktree-deps' dir-local."
  :type '(alist :key-type string :value-type (choice (const symlink) (const clone)))
  :group 'ghostherd)

(defvar ghostherd-project-worktree-deps 'unset
  "Per-project override of `ghostherd-worktree-deps', from .dir-locals.el.

  ((nil . ((ghostherd-project-worktree-deps . ((\"node_modules\" . clone))))))")
(put 'ghostherd-project-worktree-deps 'safe-local-variable
     (lambda (v) (and (listp v)
                      (seq-every-p (lambda (e) (and (consp e) (stringp (car e))
                                                    (memq (cdr e) '(symlink clone))))
                                   v))))

(defvar ghostherd--worktree-deps-status (make-hash-table :test 'equal)
  "Session id -> alist of its worktree dependencies' outcome: PATH ->
(:status STATUS :reason REASON), STATUS `done' or `skipped'.")

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
        (if (string-match-p "/" name)
            (push (cons (directory-file-name name) method) out)
          (dolist (d ignored)
            (when (equal (file-name-nondirectory d) name)
              (push (cons d method) out))))))
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
status line per spec -- `done PATH' or `skip PATH REASON' -- and never
copying a dependency in full."
  (mapconcat
   (lambda (spec)
     (let* ((name (car spec))
            (src (shell-quote-argument (directory-file-name (expand-file-name name main))))
            (dstf (directory-file-name (expand-file-name name worktree)))
            (dst (shell-quote-argument dstf))
            (par (shell-quote-argument (directory-file-name (file-name-directory dstf))))
            (tag (shell-quote-argument name)))
       (pcase (cdr spec)
         ('symlink
          (format "if [ ! -e %s ]; then echo skip %s no-source; elif [ -e %s ]; then echo skip %s exists; elif mkdir -p %s 2>/dev/null && ln -s %s %s 2>/dev/null; then echo done %s; else echo skip %s link-failed; fi"
                  src tag dst tag par src dst tag tag))
         ('clone
          (format "if [ ! -e %s ]; then echo skip %s no-source; elif [ -e %s ]; then echo skip %s exists; elif mkdir -p %s 2>/dev/null && cp -c -R %s %s 2>/dev/null; then echo done %s; else echo skip %s no-copy-on-write; fi"
                  src tag dst tag par src dst tag tag))
         (_ (format "echo skip %s unknown-method" tag)))))
   specs
   "\n"))

(defun ghostherd--worktree-deps-record (id output)
  "Store the dependency OUTPUT (status lines) for session ID."
  (let (alist)
    (dolist (line (split-string (or output "") "\n" t))
      (pcase (split-string (string-trim line) " ")
        (`("done" ,name . ,_) (push (cons name (list :status 'done)) alist))
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

(defcustom ghostherd-log-max 500
  "Entries kept in the herd log.  Oldest are dropped first."
  :type 'integer
  :group 'ghostherd)

(defcustom ghostherd-log-screens 20
  "How many captured screens the log keeps alongside `blocked' entries.

The point of keeping them: rule tuning needs the screen a rule fired
against, and =ghostherd-explain= only offers that while the agent is
still sitting on the prompt.  Storing it means the data accumulates
whether or not you were watching.

Only transitions into `blocked' are captured -- they are rare, and they
are the ones worth arguing about.  0 disables it."
  :type 'integer
  :group 'ghostherd)

(cl-defstruct (ghostherd-log-entry (:constructor ghostherd-log-entry--create)
                                   (:copier nil))
  (time (current-time))
  session
  kind
  text
  screen)

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
          (when-let* ((screen (ghostherd-log-entry-screen entry)))
            (list :screen screen))))

(defun ghostherd--log-entry-from-form (form)
  "Rebuild a log entry from FORM, or nil when FORM is not one."
  (when (and (consp form) (plist-member form :time))
    (ghostherd-log-entry--create
     :time (plist-get form :time)
     :session (plist-get form :session)
     :kind (plist-get form :kind)
     :text (plist-get form :text)
     :screen (plist-get form :screen))))

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
  "Return ENTRIES (newest first) within both log caps.

Entries and screens are capped separately because they cost differently:
500 one-line entries are nothing, and twenty 40x120 screens are the
actual memory -- and now the actual disk."
  (let ((kept (if (> (length entries) ghostherd-log-max)
                  (seq-take entries ghostherd-log-max)
                entries))
        (screens 0))
    (dolist (entry kept)
      (when (ghostherd-log-entry-screen entry)
        (setq screens (1+ screens))
        (when (> screens ghostherd-log-screens)
          (setf (ghostherd-log-entry-screen entry) nil))))
    kept))

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

(defun ghostherd--log-add (session kind text &optional screen)
  "Record TEXT about SESSION under KIND, optionally with a SCREEN."
  (let ((entry (ghostherd-log-entry--create
                :session (if (ghostherd-session-p session)
                             (ghostherd-session-name session)
                           (format "%s" session))
                :kind kind :text text :screen screen)))
    (push entry ghostherd--log)
    (setq ghostherd--log (ghostherd--log-trim ghostherd--log)
          ghostherd--log-disk-lines (1+ ghostherd--log-disk-lines)
          ghostherd--log-disk-screens (+ ghostherd--log-disk-screens
                                         (if screen 1 0)))
    ;; Appended as it happens rather than saved on the way out: a
    ;; `kill-emacs-hook' does not run for the crash this is meant to
    ;; survive, and one line is cheap enough to write per transition.
    ;; Once disk lines or screens reach twice the cap, rewrite from the
    ;; trimmed in-memory log so a long-running Emacs does not grow the
    ;; file without bound.
    (if (or (>= ghostherd--log-disk-lines (* 2 (max 1 ghostherd-log-max)))
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
    nil))

(defun ghostherd--log-transition (session old new reason)
  "Log SESSION moving from OLD to NEW because of REASON."
  (ghostherd--log-add
   session 'state
   (format "%s → %s%s" (or old "?") new (if reason (format "  %s" reason) ""))
   ;; One extra capture, only on the transition worth arguing about.  It
   ;; is not free -- on the tmux backend it is a subprocess -- but an
   ;; agent blocks once per prompt, not once per poll.
   (when (and (eq new 'blocked) (> ghostherd-log-screens 0))
     (ignore-errors (ghostherd--host-capture session)))))


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
                                  (if reason (format "  %s" reason) ""))))
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

(defun ghostherd--input-at-foot-p (text)
  "Non-nil when TEXT's status area holds the input box: a rule, then a
line opening with the prompt sign -- not a menu's first choice, which
carries the sign before its number.  Only there: higher up, a rule and
a sign are an old prompt in the conversation."
  (let ((lines (split-string (ghostherd--status-region text) "\n")))
    (cl-loop for (above line) on lines
             thereis (and line
                          (string-match-p ghostherd--input-rule-re above)
                          (string-match-p "\\`[❯›>]\\(?:[[:blank:]]\\|\\'\\)" line)
                          (not (string-match-p "\\`[❯›>][[:blank:]]*[1-9]\\." line))))))

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
      (let* ((foot (ghostherd--foot text))
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
                       (not (string-match-p "[❯›>][[:blank:]]*[1-9]\\." line))
                       (string-match-p ghostherd--input-rule-re (aref lines (1- i))))
             return (1- i))))

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
  "\\`[[:blank:]]*[─━═╭╮╰╯┌┐└┘├┤-]\\{3,\\}[[:blank:]]*\\'"
  "A line that only draws: the edge of what a menu asks.")

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

(defun ghostherd--prompt-json (prompt)
  "PROMPT for JSON: its options as (:n N :label LABEL); false for none."
  (if prompt
      (list :question (plist-get prompt :question)
            :options (vconcat (mapcar (lambda (o) (list :n (car o) :label (cdr o)))
                                      (plist-get prompt :options))))
    :json-false))

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
        (let* ((tail (and rules (or screen (ghostherd--host-capture session))))
               (hit (and tail (ghostherd--match-rules tail rules (plist-get spec :prompt-replaces-input)
                                                      (plist-get spec :spinner))))
               (report (ghostherd--fresh-report session)))
          (when tail
            (puthash (ghostherd-session-id session) tail ghostherd--screens)
            (ghostherd--read-foot session tail))
          (cond
           ;; Screen rules keep priority for `blocked': a stale progress
           ;; report must never mask an agent sitting on a prompt, and
           ;; neither may a stale report from the agent itself.
           ((eq (car-safe hit) 'blocked)
            (cons 'blocked (or (and (member (cdr hit) ghostherd--prompt-foot-patterns)
                                    (ghostherd--prompt-reason tail))
                               (ghostherd--rule-reason tail hit))))
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
           (hit (cons (car hit) (ghostherd--rule-reason tail hit)))
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
        (ghostherd--set-state session state reason)))))

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
         (command (or (plist-get plist :command)
                      (plist-get spec :command)))
         (args (if (plist-member plist :args)
                   (plist-get plist :args)
                 (plist-get spec :args)))
         (notes (plist-get plist :notes))
         (role (plist-get plist :role))
         (lineage (plist-get plist :lineage))
         (worktree (plist-get plist :worktree))
         (branch (plist-get plist :branch))
         (base (plist-get plist :base))
         (backend (or (plist-get plist :backend) ghostherd-backend))
         (display (if (plist-member plist :display)
                      (plist-get plist :display)
                    t))
         (started (ghostherd-backend-spawn
                   backend
                   (list :name name :kind kind :command command :args args
                         :directory directory :project project :notes notes
                         :role role :lineage lineage
                         :worktree worktree :branch branch :base base)))
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
    (ghostherd--log-add session 'life
                        (format "spawned %s on %s" kind backend))
    (ghostherd--ensure-poll-timer)
    (run-hook-with-args 'ghostherd-session-created-hook session)
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
          (ghostherd--log-add name 'life
                              (format "restored from %s, detached" backend))
          (setq restored (1+ restored)))))
    (when (> restored 0)
      (ghostherd--ensure-poll-timer)
      (ghostherd--sidebar-refresh))
    restored))

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
          :base (plist-get wt :base))))

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

;;;###autoload
(defun ghostherd-new (&optional kind)
  "Interactively create a new ghostherd agent session.
With prefix or KIND, skip the kind prompt."
  (interactive)
  (let* ((project (ghostherd--project-root))
         ;; Resolved before the kind prompt so a project can preselect it.
         ;; A later change of directory does not re-read: re-prompting for
         ;; kind after you already answered would be worse than stale.
         (defaults (ghostherd-project-defaults (or project default-directory)))
         (kind (or kind
                   (intern
                    (completing-read
                     "Agent kind: "
                     (mapcar #'symbol-name (ghostherd--agent-kinds))
                     nil t nil nil
                     (symbol-name (or (car defaults) ghostherd-default-kind))))))
         (spec (ghostherd--spec kind))
         (default-name (ghostherd--unique-name (symbol-name kind)))
         (name (read-string "Session name: " default-name))
         (directory (if project
                        (read-directory-name "Directory: " project nil t)
                      (read-directory-name "Directory: " default-directory nil t)))
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
         (wt-project (ghostherd--project-root directory))
         (wt (and wt-project
                  (y-or-n-p (format "Own git worktree off %s? "
                                    (ghostherd--abbreviate wt-project)))
                  (ghostherd--worktree-create
                   wt-project final-name (read-string "Branch: " final-name)))))
    (when wt
      (ghostherd--worktree-deps-setup wt-project (plist-get wt :path) final-name))
    (ghostherd-spawn
     kind
     :name final-name
     :project wt-project
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

(defun ghostherd--resume-args (kind id &optional fork)
  "Arguments that make KIND resume conversation ID, or nil if it cannot.
With FORK, branch a new conversation off it; nil if KIND cannot fork."
  (let* ((spec (ghostherd--spec kind))
         (resume (plist-get spec :resume-args))
         (forking (plist-get spec :fork-args)))
    (when (and resume (stringp id) (string-match-p ghostherd--session-id-re id)
               (or (not fork) forking))
      (append (mapcar (lambda (a) (format a id)) resume)
              (and fork forking)))))

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

(cl-defun ghostherd-resume (kind id directory &key fork title)
  "Spawn a KIND agent in DIRECTORY on conversation ID, and show it.
With FORK, branch a new conversation off it.  TITLE goes in the
notes.  The agent is named KIND-<project>, made unique, and takes the
project's own default args before the resume ones."
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
                    (if (and title (not (string-empty-p title))) (concat ": " title) "")))))

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
    (remhash id ghostherd--feet)
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
                         ghostherd--reports ghostherd--screens))
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
                               (car (split-string text "\n" t)))
                              (if submit "" "  (not submitted)")))
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

(defun ghostherd--unsent-p (screen text)
  "Non-nil when SCREEN's input box still holds TEXT, pasted but not submitted.
Only TEXT counts -- claude's fold of a big paste, or its first words --
so a Return pressed for it never submits something you were typing."
  (let* ((first (car (split-string text "\n" t "[ \t]+")))
         (head (and first (substring first 0 (min 20 (length first))))))
    (seq-some (lambda (line)
                ;; claude follows its ❯ with a no-break space.
                (and (string-match "^[[:blank:]│|]*[❯›>][[:blank:]]+\\(.*\\)" line)
                     (let ((in (match-string 1 line)))
                       (or (string-prefix-p "[Pasted text #" in)
                           (and head (string-search head in))))))
              (ghostherd--input-box screen))))

(defun ghostherd--confirm-submit (session text tries)
  "Press Return again while SESSION's input box still holds TEXT, TRIES more times.
Not on an agent that is working or on a menu: Return there is an answer."
  (condition-case err
      (when (and (eq (gethash (ghostherd-session-id session) ghostherd--sessions) session)
                 (ghostherd--session-live-p session))
        (let* ((spec (ignore-errors (ghostherd--spec (ghostherd-session-kind session))))
               (rules (plist-get spec :screen-rules))
               (screen (and rules (ghostherd--host-capture session 20)))
               (hit (and screen (ghostherd--match-rules screen rules (plist-get spec :prompt-replaces-input)
                                                        (plist-get spec :spinner)))))
          (when (and screen
                     (not (memq (car-safe hit) '(working blocked)))
                     (ghostherd--unsent-p screen text))
            (if (<= tries 0)
                (progn
                  (ghostherd--log-add session 'input "← still in its input box: not submitted" screen)
                  ;; An ask it was: fail it, so the asker hears now rather
                  ;; than waiting on an agent that never read it.
                  (when-let* ((id (ghostherd--ask-in session text)))
                    (ghostherd--ask-settle
                     id session nil
                     (format "not submitted: it sat in %s's input box"
                             (ghostherd-session-name session)))))
              (ghostherd--host-send-keys session '("return"))
              (ghostherd--log-add session 'input "↵ again: it was still in the input box" screen)
              (run-at-time ghostherd-submit-check-after nil #'ghostherd--confirm-submit
                           session text (1- tries))))))
    (error (message "ghostherd: checking a submit: %s" (error-message-string err)))))

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

(defun ghostherd--sidebar-cell (session key state face)
  "Return SESSION's cell for column KEY, given its STATE and FACE."
  (pcase key
    ('glyph   (concat
               (propertize (ghostherd--state-glyph state) 'face face)
               ;; Whether anyone is looking is a fact about the *view*,
               ;; so it is a separate mark rather than a state glyph of
               ;; its own -- an agent is working or blocked regardless.
               ;; It belongs in the overview all the same: without it a
               ;; herd running headless looks exactly like one you are
               ;; watching.
               (unless (buffer-live-p (ghostherd-session-buffer session))
                 (propertize ghostherd-detached-glyph 'face face))))
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
    ('state   (propertize (symbol-name state) 'face face))
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
                         (ignore-errors (ghostherd--session-title session))))
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
    (mapconcat (lambda (line)
                 (truncate-string-to-width line width nil nil t))
               (split-string tail "\n")
               "\n")))

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
           ;; Where it works and the branch it is on, so the lead can find
           ;; and read its work.  A worktree agent is in its own directory
           ;; on its own branch (from the recipe, no per-poll git); the
           ;; others share the project's, so their branch is left blank.
           :directory (let ((d (or (plist-get (ghostherd-session-worktree s) :path)
                                   (ghostherd-session-project s))))
                        (if d (ghostherd--abbreviate d) ""))
           :branch (or (plist-get (ghostherd-session-worktree s) :branch) "")
           :screen (or (gethash (ghostherd-session-id s) ghostherd--screens)
                       "")))
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
     (ask (ghostherd--ask-deliver ask from to body (plist-get msg :rounds)))
     (handoff (ghostherd-handoff to body from))
     (t (ghostherd-message from to body :submit submit)))))

(defun ghostherd--herd-deliver (result)
  "Deliver pending mail from a `herd_tick' RESULT plist."
  (dolist (msg (plist-get result :pending))
    (let ((id (plist-get msg :id)))
      (condition-case err
          (ghostherd--herd-deliver-one msg)
        (error
         (message "ghostherd herd mail %s: %s"
                  id (error-message-string err))))
      (when id
        (push id ghostherd--herd-ack-ids)))))


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

(defun ghostherd--ask-text (id body &optional rounds)
  "BODY as the taker sees ask ID: the work, then how to answer it.
The answer line is spelled out in full, the client by its absolute
name, so a taker needs no skill installed and no PATH of ours.
ROUNDS, when the sidecar says, is how many times the taker may ask
any one agent for it."
  (format "[ask %s -- whoever asked is waiting for your answer]\n%s\n\n\
When you are done, send your answer with:\n\
%s reply %s <<'HERD'\n\
<your answer>\n\
HERD\n\
Answer even if you could not do it, and say why.  You may ask other\n\
agents for parts of it, but not whoever is waiting on you%s."
          id body
          (shell-quote-argument (or (and (fboundp 'ghostherd-herd-client)
                                         (ghostherd-herd-client))
                                    "herd"))
          id
          (if (natnump rounds)
              (format ",\nand any one of them %d time%s at most" rounds (if (= rounds 1) "" "s"))
            "")))

(defun ghostherd--ask-deliver (id from to body &optional rounds)
  "Paste ask ID from FROM into TO, and watch for TO to stop.
ROUNDS is how many times TO may ask any one agent for it."
  (let ((session (or (ghostherd-get to)
                     (user-error "Unknown target session"))))
    (ghostherd-message from session (ghostherd--ask-text id body rounds) :submit t)
    (puthash id (ghostherd-session-id session) ghostherd--asks-open)
    (ghostherd--log-add session 'ask
                        (format "ask %s from %s: %s" id from
                                (ghostherd--ask-head body)))))

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
           (ghostherd--log-add session 'ask
                               (format "ask %s left open: %s waits on %s"
                                       id (ghostherd-session-name session)
                                       (mapconcat #'identity (plist-get r :waiting_on) ", ")))))
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

(defun ghostherd--ask-on-state (session _old new)
  "A taker that stopped closes its open ask -- unless it never started.
Delivery marks the taker `working' itself; when the Return was lost the
ask is still in its input box, and the taker goes idle without having
read it.  Its screen then answers nothing, so the ask stays open."
  (when (memq new '(idle done dead))
    (let ((ids (ghostherd--asks-of session)))
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
          (ghostherd--log-add (plist-get a :to) 'ask (ghostherd--ask-outcome a)))
        ;; Failed since the last tick -- or before it, when the ask is new
        ;; to a herd that has seen asks already, not at Emacs's start.
        (when-let* (((equal status "failed"))
                    ((not (equal was "failed")))
                    ((or was ghostherd--herd-asks-taken))
                    (asker (ghostherd-get (plist-get a :from))))
          (ghostherd--push 'ask-failed asker :ask id :to (or (plist-get a :to) "")
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

(defconst ghostherd--lead-role "\
You are the lead of this project's herd of agents: you plan the work,
hand it out to the other agents, and report to the user.

The user talks to you in the project's chat.  What you say at the end of
a turn is what they read there: say what you decided, who is doing what,
what came back, and what you need from them -- briefly, and in the
language they write to you in.

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
worktree.

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
know.  Do not do an agent's work for it.  When the team has nobody for
a job, say so and ask the user to start one.

When an ask fails, or comes back as the herd's answer rather than the
agent's, say what happened and decide: ask again, ask someone else, or
ask the user.

When an agent is stuck for good -- its CLI out of credits or rate
limited, or wedged with no way on -- hand its work to a fresh agent on
another CLI:

  \"$GHOSTHERD_HERD\" relay NAME [KIND] --reason \"why\"

It reads where the work stood and starts a new agent of KIND that takes
the name and carries on.  Leave KIND off to let the herd pick a CLI with
usage left; name one (claude, grok, agy) to choose.  You may relay only
your own project's agents, not yourself.

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

(defun ghostherd-start-lead (project &optional kind)
  "Start PROJECT's lead, a KIND agent (`ghostherd-lead-kind' by default)."
  (let* ((kind (or kind ghostherd-lead-kind))
         (dir (file-name-as-directory (expand-file-name project))))
    (ghostherd-spawn kind
                     :name (ghostherd--unique-name
                            (format "lead-%s" (file-name-nondirectory (directory-file-name dir))))
                     :project project
                     :directory dir
                     :args (append (plist-get (ghostherd--spec kind) :args)
                                   (ghostherd--lead-args kind))
                     :notes "lead: plans the work, hands it out"
                     :role "lead"
                     :display nil)))

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

(defun ghostherd-tell (session text &optional role)
  "Prompt SESSION with TEXT from the user.
A project's lead hears first what the user said to its agents directly
since it last heard from them, and what the project works on when that
is news to it -- after ROLE, the role of a lead that took none as it
started.  Said to another agent of a project that has a lead, TEXT is
kept for that."
  (let* ((project (ghostherd--project-name (ghostherd-session-project session)))
         (lead (and project (ghostherd-project-lead project))))
    (if (eq lead session)
        (progn
          (ghostherd-prompt session (concat role
                                            (ghostherd--lead-unheard-note project)
                                            (ghostherd--lead-work-note session project)
                                            text))
          (remhash project ghostherd--lead-unheard))
      (ghostherd-prompt session text)
      (when lead
        (push (cons (ghostherd-session-name session) text)
              (gethash project ghostherd--lead-unheard))))))

(defun ghostherd--when-ready (session fn &optional tries fail)
  "Call FN once SESSION is at its prompt, looking every second.
Two minutes at most, TRIES counting down; a session that never gets
there is logged, and one gone is let be.  Either way FAIL, when given,
is called instead."
  (let ((tries (or tries 120)))
    (cond
     ((not (eq (gethash (ghostherd-session-id session) ghostherd--sessions) session))
      (when fail (funcall fail)))
     ((memq (ghostherd-session-state session) '(idle done waiting))
      (funcall fn))
     ((<= tries 0)
      (ghostherd--log-add session 'life "never came to its prompt: the message for it was not sent")
      (when fail (funcall fail)))
     (t (run-at-time 1 nil #'ghostherd--when-ready session fn (1- tries) fail)))))

(defun ghostherd-lead-say (project text)
  "Say TEXT to PROJECT's lead, starting one when it has none.
A lead just started hears TEXT once it is ready -- after its role, on a
CLI that took none as it started.  Return the lead."
  (if-let* ((lead (ghostherd-project-lead project)))
      (progn (ghostherd-tell lead text) lead)
    (let* ((lead (ghostherd-start-lead project))
           (role (unless (ghostherd--lead-args (ghostherd-session-kind lead))
                   (concat ghostherd--lead-role "\n---\n\n"))))
      ;; Asked now, so that whatever looks has looked by the time the
      ;; lead is ready to be told.
      (ghostherd--project-work project)
      (ghostherd--when-ready lead (lambda () (ghostherd-tell lead text role)))
      lead)))

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
                          (and reason (not (string-empty-p reason)) reason))))))

(defun ghostherd--herd-take-links (links)
  "Keep LINKS from a `herd_tick', replacing the last set."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (l links)
      (puthash (plist-get l :session) l table))
    (setq ghostherd--herd-links table)))

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

(defun ghostherd--herd-run-command (cmd)
  "Run one sidecar Telegram CMD plist.  Return (:id ID :text TEXT)."
  (let* ((id (plist-get cmd :id))
         (op (format "%s" (or (plist-get cmd :op) "")))
         (name (plist-get cmd :session))
         (args (plist-get cmd :args))
         (text "ok"))
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
    (list :id id :text text)))

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
    (puthash id t ghostherd--relaying)
    (ghostherd--log-add session 'life (format "relaying to %s: reading where its work stands" kind))
    (ghostherd-memory-request-async
     "herd_relay_brief"
     (lambda (brief)
       (remhash id ghostherd--relaying)
       (ghostherd--relay-hand session kind (plist-get brief :text) reason))
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
herd while the brief was written."
  (let ((id (ghostherd-session-id session))
        (name (ghostherd-session-name session)))
    (if (not (eq (gethash id ghostherd--sessions) session))
        (progn (message "ghostherd: %s left the herd before its heir could take over" name)
               nil)
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
        heir))))

(defun ghostherd--relay-tell (heir text held lead)
  "Tell HEIR its work, TEXT; then the asks HELD for it are its to answer.
LEAD when it leads its project: it hears what a lead hears first."
  (if lead
      (ghostherd-tell heir text (unless (ghostherd--lead-args (ghostherd-session-kind heir))
                                  (concat ghostherd--lead-role "\n---\n\n")))
    (ghostherd-prompt heir text))
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

(defcustom ghostherd-push '(blocked report ask-failed used-up)
  "What the herd pushes to Telegram while you are away: an agent
`blocked', a lead's `report' at the end of its turn, an `ask-failed',
an agent stopped with its CLI's usage `used-up' -- with a button that
relays it.  Nil pushes nothing.  The sidecar's bot sends them to the
chats of GHOSTHERD_TELEGRAM_CHAT_IDS; a reply to one goes to that
project's lead."
  :type '(set (const blocked) (const report) (const ask-failed) (const used-up))
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
                          :url (or (and project (ghostherd--room-url project)) ""))
                    fields)
            ghostherd--herd-pushes))))

(defun ghostherd--push-state (session _old new)
  "Push SESSION's turning NEW: blocked, or a lead done with its turn."
  (pcase new
    ('working (puthash (ghostherd-session-id session) (float-time) ghostherd--working-since))
    ('blocked (ghostherd--push 'blocked session :reason (or (ghostherd-session-state-reason session) "")))
    ('done (when (equal (ghostherd-session-role session) "lead")
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
         (ghostherd--herd-run-commands result))
       (list :sessions (vconcat (ghostherd--herd-snapshot))
             :page_url (or (ghostherd--room-url) "")
             :ack_ids (vconcat (append acks nil))
             :replies (vconcat (or replies []))
             :pushes (vconcat (reverse pushes)))
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

(defun ghostherd--mode-line-segment ()
  "Mode-line segment summarizing herd attention.

Reads the registry and nothing else.  It used to sweep it first, which
was free while a host check was `buffer-live-p' and is not once a host
check can run a subprocess -- redisplay is no place for I/O, and no
place to discover that the I/O failed either.  The poll timer already
sweeps every `ghostherd-poll-interval'."
  (let* ((sessions (hash-table-values ghostherd--sessions))
         (blocked (cl-count-if (lambda (s) (eq (ghostherd-session-state s) 'blocked))
                               sessions))
         (working (cl-count-if (lambda (s) (eq (ghostherd-session-state s) 'working))
                               sessions))
         (waiting (cl-count-if (lambda (s) (eq (ghostherd-session-state s) 'waiting))
                               sessions))
         (done (cl-count-if (lambda (s) (eq (ghostherd-session-state s) 'done))
                            sessions))
         (total (length sessions)))
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
                    "<mode-line> <mouse-1>" #'ghostherd-sidebar)))))

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
        ;; a previous Emacs is picked up here, before anything asks for
        ;; a session list.
        (ignore-errors (ghostherd-restore))
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

;;; ghostherd-screen.el --- An agent's state, read off its screen; the poll -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of ghostherd (see ghostherd.el).  Reading an agent's state off
;; its screen: the status rules, what else a screen says, a menu
;; prompt, a CLI's own errors and turn ends; carrying a turn on after
;; the Mac slept or the connection dropped; and the poll that sets
;; each session's state.

;;; Code:

(require 'ghostherd-core)
(require 'ghostherd-log)

(declare-function ghostherd--maybe-restore "ghostherd" ())
(declare-function ghostherd--model-picker "ghostherd" (text))
(declare-function ghostherd--poll-tick "ghostherd" ())
(declare-function ghostherd--read-session "ghostherd" (&optional prompt predicate))
(declare-function ghostherd--sidebar-refresh "ghostherd" ())
(declare-function ghostherd-send "ghostherd" (session text &optional submit))
(declare-function ghostherd-session-transcript "ghostherd" (session))

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

ACTION `login' offers a one-click /login on the card; once a /login has
gone through after the error, the turn is still stuck but the action is
`carry-on', a one-click continue (`ghostherd-carry-on').  ACTION
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

(defun ghostherd--claude-login-ok-p (line)
  "Non-nil when claude transcript LINE is a /login that went through: its
output, \"Login successful\" (an interrupted one says \"Login
interrupted\"), as a local command writes it -- a system entry, or a
user one in the newer shape.  Only for a LINE that is not the
conversation (`ghostherd--claude-entry-verdict' nil): a reply quoting the
words is the model's."
  (and (string-search "<local-command-stdout>Login successful" line)
       (when-let* ((entry (ignore-errors
                            (json-parse-string line :object-type 'plist
                                               :null-object nil :false-object nil))))
         (let ((content (pcase (plist-get entry :type)
                          ("system" (plist-get entry :content))
                          ("user" (plist-get (plist-get entry :message) :content)))))
           (and (not (plist-get entry :isSidechain))
                (stringp content)
                (string-prefix-p "<local-command-stdout>Login successful" content))))))

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
                        (split-string (buffer-string) "\n" t)))
               (signed-in nil))
          ;; A chunk that starts inside the file starts inside a line.
          (when (> beg 0) (pop lines))
          (dolist (line (nreverse lines))
            (let ((verdict (ghostherd--claude-entry-verdict line)))
              (cond
               ;; A /login that went through after claude's error: the
               ;; turn still ended on the error, but it is signed in again.
               ((and (eq (car-safe verdict) :error) signed-in)
                (throw 'done (append verdict '(:signed-in t))))
               (verdict (throw 'done verdict))
               ((ghostherd--claude-login-ok-p line) (setq signed-in t)))))
          (when (or (= beg 0) (>= chunk ghostherd--transcript-tail-max))
            (throw 'done nil))
          (setq chunk (* chunk 4)))))))

(defun ghostherd--claude-turn-end (path)
  "What the last turn in claude transcript PATH ended on: (:error TEXT)
when the conversation's last entry is claude's own error (an assistant
entry marked `isApiErrorMessage') -- (:error TEXT :signed-in t) when a
/login went through after it (`ghostherd--claude-login-ok-p') -- :clean
when it is any other, nil when none can be read.  Read once per change
of the file."
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
        (`(:error ,text . ,more)
         (let ((m (or (rule-for text)
                      (list :reason (concat "its turn ended on an error: "
                                            (truncate-string-to-width
                                             (car (split-string text "\n")) 80 nil nil "…"))
                            :action nil :cause nil))))
           ;; Signed in again since the expired login: still stuck -- the
           ;; turn did not finish -- but /login is not what it needs now.
           ;; One click carries the turn on (`ghostherd-carry-on').
           (if (and (eq (plist-get m :action) 'login) (plist-get more :signed-in))
               (list :reason "signed in again: its turn stopped at the expired login"
                     :action 'carry-on :cause 'expired)
             m)))
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

(defcustom ghostherd-expired-continue-prompt
  "Your last turn was cut off because your login expired mid-response -- not because the work was finished. Pick up exactly where you left off and carry on to the end."
  "The prompt sent to an agent whose turn an expired login cut short,
once it is signed in again and you click Continue (`ghostherd-carry-on').
The same as the others but for its cause: it says nothing of sleep or
the connection."
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
             :again "the connection dropped again")
    ;; Carried on only by a click, once signed in again: no countdown.
    (expired :prompt ghostherd-expired-continue-prompt
             :cut "an expired login"))
  "How the herd speaks of each cause of a turn it carries on (see
`ghostherd-cli-errors'): the variable holding the prompt it sends the
agent, what cut the turn short in its log, and, for a `continue' CLI
error, the card's words -- \"continuing AFTER in Ns\" while it counts
down, \"stopped retrying -- AGAIN\" once it has stopped.")

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

(defun ghostherd--continue-send (session cause how)
  "Send SESSION the continue prompt for CAUSE, a key of
`ghostherd--continue-causes', and log it -- HOW after the cause.  The
one way the herd carries a stopped turn on, by its timer or your click:
the turn it starts answers an ask the agent holds."
  (let ((says (alist-get cause ghostherd--continue-causes)))
    (ghostherd--log-add
     session 'continue
     (format "↻ continuing a turn %s cut short%s" (plist-get says :cut) how))
    (ghostherd-send session (symbol-value (plist-get says :prompt)) t)))

(defun ghostherd--sleep-continue-fire (session ep cause)
  "Send SESSION the continue prompt for CAUSE, log it, and spend one of
EP's tries.  CAUSE is a key of `ghostherd--continue-causes'."
  (let ((n (1+ (plist-get ep :attempts))))
    (puthash (ghostherd-session-id session)
             (plist-put ep :attempts n)
             ghostherd--sleep-continue)
    (ghostherd--continue-send session cause
                              (format " (try %d of %d)" n ghostherd-sleep-continue-max))))

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

;;;###autoload
(defun ghostherd-carry-on (session)
  "Carry on SESSION's turn an expired login stopped, now that it is signed
in again: send it the continue prompt for that cause, as a turn the Mac's
sleep cut short is continued, so an ask it holds is answered by the
turn that goes on.  Only while that is still so -- its transcript ends
on the expired login with a /login gone through since -- and its input
box is on its live screen, for the prompt to land in."
  (interactive (list (ghostherd--read-session "Carry on the turn of: ")))
  (setq session (ghostherd-get session))
  (unless (ghostherd--session-live-p session)
    (user-error "Session host is not live"))
  (let* ((name (ghostherd-session-name session))
         (screen (ghostherd--host-capture session 20))
         (m (ghostherd--cli-error-match session screen)))
    (unless (eq (plist-get m :action) 'carry-on)
      (user-error "%s's turn is not one an expired login stopped, signed in again since" name))
    (unless (and screen (ghostherd--input-at-foot-p screen))
      (user-error "%s's input box is not on its screen" name))
    (ghostherd--continue-send session (plist-get m :cause) ", as asked")
    name))

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

(defcustom ghostherd-login-done-re
  "Login successful\\.[[:space:]]+Press[[:space:]]+Enter[[:space:]]+to[[:space:]]+continue"
  "What claude's /login says once the sign-in went through, under
\"Logged in as …\": the step that waits for one Return before it gives
the input box back.  From claude's own text (2.1.312: \"Login
successful. Press \", Enter in bold, \" to continue…\"); the space
between the words may be a wrap in a narrow pane.  Read only at the
screen's foot, and only with no input box there
(`ghostherd--login-done-on').  grok and agy have no such step: grok's
sign-in prints \"Signed in as …\", agy's \"Authentication successful!\",
and neither waits on a key."
  :type 'regexp
  :group 'ghostherd)

(defconst ghostherd--login-done-foot 4
  "Non-blank lines at a screen's foot read for claude's signed-in step:
its one line, wrapped in a narrow pane, and the \"Logged in as\" above.")

(defun ghostherd--login-done (session &optional screen)
  "Non-nil when SESSION sits on claude's /login signed-in step -- \"Login
successful. Press Enter to continue\" -- which waits for one Return.
SCREEN is the screen to read, else its last one stored."
  (when (eq (ghostherd-session-kind session) 'claude)
    (when-let* ((screen (or screen (gethash (ghostherd-session-id session) ghostherd--screens))))
      (ghostherd--screen-read screen (list 'login-done ghostherd-login-done-re ghostherd-status-lines)
                              #'ghostherd--login-done-on))))

(defun ghostherd--login-done-on (screen)
  "Non-nil when claude's signed-in step is at SCREEN's foot: its words in
the last few lines, and no input box drawn there.  The step takes the
box's place, so the same words quoted in the conversation, over a box,
do not count."
  (let ((foot (string-join (ghostherd--screen-foot screen ghostherd--login-done-foot) "\n"))
        (case-fold-search nil))
    (and (string-match-p ghostherd-login-done-re foot)
         (not (ghostherd--input-at-foot-p screen)))))

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
               ;; Not behind `err': the transcript keeps the expired login
               ;; as the turn's end through the whole sign-in (a /login run
               ;; is not the conversation), so it would hide this step.
               (login-done (and tail (not scrolled) (ghostherd--login-done session tail)))
               ;; A dialog drawn from the top of a pane taller than the tail
               ;; -- each CLI's trust prompt, in a window over 40 rows --
               ;; leaves the tail blank and no box in it: it read idle, and
               ;; took a paste's Return as its answer (ask cf9873be).  With
               ;; nothing read and no box at the foot, the whole pane is
               ;; read, its blank rows dropped.
               (pane (and tail (not scrolled) (not err) (not login-url) (not login-done)
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
           ;; /login's last step: signed in, and waiting for Return before
           ;; it gives the input box back.  Ahead of the CLI error, which
           ;; still names the expired login (see `login-done' above); the
           ;; card and the Inbox offer the Return
           ;; (`ghostherd-login-continue').
           (login-done (cons 'blocked "signed in: press Return to continue"))
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

(provide 'ghostherd-screen)

;; Local Variables:
;; generated-autoload-load-name: "ghostherd"
;; End:

;;; ghostherd-screen.el ends here

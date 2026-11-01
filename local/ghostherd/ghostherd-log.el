;;; ghostherd-log.el --- The herd log, its buffer, and what an agent says of itself -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of ghostherd (see ghostherd.el).  The herd log -- what the herd
;; did, kept on disk -- and its buffer; the screens it captures by
;; itself when one looks wrong; and what an agent says about itself
;; (`ghostherd-report').

;;; Code:

(require 'ghostherd-core)

(declare-function ghostherd--box-top "ghostherd" (lines))
(declare-function ghostherd--input-at-foot-p "ghostherd" (text))
(declare-function ghostherd--match-rules "ghostherd" (text rules &optional replaces-input spinner))
(declare-function ghostherd--model-picker "ghostherd" (text))
(declare-function ghostherd--prompt "ghostherd" (screen))
(declare-function ghostherd--read-session "ghostherd" (&optional prompt predicate))
(declare-function ghostherd--rule-reason "ghostherd" (text hit))
(declare-function ghostherd--scrolled-back-p "ghostherd" (text scrolled))
(declare-function ghostherd--sidebar-leave-overlay "ghostherd" ())
(declare-function ghostherd--submit-reported "ghostherd" (session))
(declare-function ghostherd-poll-session "ghostherd" (session &optional screen))

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

(provide 'ghostherd-log)

;; Local Variables:
;; generated-autoload-load-name: "ghostherd"
;; End:

;;; ghostherd-log.el ends here

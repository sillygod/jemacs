;;; ghostherd-comm.el --- Talking to an agent: send, submit, answer, handoff -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of ghostherd (see ghostherd.el).  Talking to an agent: sending,
;; and confirming that a submit really went in; answering a menu,
;; interrupting, waiting; images pasted into a prompt; handoffs; and
;; the commands an agent's shell calls through ghostel_cmd.

;;; Code:

(require 'ghostherd-core)
(require 'ghostherd-log)
(require 'ghostherd-screen)
(require 'ghostherd-spawn)

(declare-function ghostherd--age-string "ghostherd-sidebar" (time))
(declare-function ghostherd--ask-in "ghostherd-mail" (session text))
(declare-function ghostherd--ask-on-state "ghostherd-mail" (session _old new))
(declare-function ghostherd--ask-settle "ghostherd-mail" (id session dead &optional error))
(declare-function ghostherd--hooked-p "ghostherd-hooks" (session))
(declare-function ghostherd--role-give-back "ghostherd-lead" (session text))
(declare-function ghostherd--turn-began-since "ghostherd-hooks" (session at))
(declare-function ghostherd--turn-submitted "ghostherd-mail" (session))

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
  ;; Its turn will not come: the next one is not the ask's, nor the mail's.
  (remhash (ghostherd-session-id session) ghostherd--turn-ask-pending)
  (when-let* ((id (ghostherd--ask-in session text)))
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
(defun ghostherd-login-continue (session)
  "Press Return for SESSION's /login on its last step: signed in, \"Press
Enter to continue\".  Only while that step is verifiably still on its
screen -- read live now, not the last poll's -- so the Return never
lands in its input box or on a menu that has taken the step's place."
  (interactive (list (ghostherd--read-session "Continue the sign-in of: ")))
  (setq session (ghostherd-get session))
  (unless (ghostherd--session-live-p session)
    (user-error "Session host is not live"))
  (let ((name (ghostherd-session-name session))
        (screen (ghostherd--host-capture session 20)))
    (unless (and screen (ghostherd--login-done session screen))
      (user-error "%s is no longer on its sign-in's \"Press Enter to continue\"" name))
    (ghostherd--host-send-keys session '("return"))
    (ghostherd--log-add session 'input "↵ to finish its sign-in, as asked: it was signed in" screen)
    name))

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

(provide 'ghostherd-comm)

;; Local Variables:
;; generated-autoload-load-name: "ghostherd"
;; End:

;;; ghostherd-comm.el ends here

;;; ghostherd-test-helpers.el --- What ghostherd's test files share  -*- lexical-binding: t; -*-

;;; Commentary:

;; Required by every ghostherd-*-tests.el: the clean registry, the fake
;; backend, the stubbed tmux, and the fixtures more than one test file
;; uses.

;;; Code:

(require 'ert)
(require 'ghostherd)
;; For `server-name', which `ghostherd-agent-environment' reads and one
;; test binds -- ghostherd only `defvar's it, so without this a `let'
;; around it is lexical and the function still sees it unbound.
(require 'server)

;;; Fixtures

;; ghostherd only `defvar's these so it can compile without ghostel loaded,
;; which leaves them unbound; `buffer-local-value' on an unbound symbol
;; errors, so give them defaults the way a loaded ghostel would.
(defvar-local ghostel--title nil)
(defvar-local ghostel--process nil)
(defvar-local ghostel--input-mode nil)
(defvar ghostel-progress-function nil)
(defvar evil-local-mode nil)

(defvar ghostherd-tests--guard-root (file-name-as-directory (make-temp-file "gh-guard-" t))
  "Where a test's guarded agents' guards go: never the user's ~/.ghostherd/guard.")

(defvar ghostherd-tests--trust-home (file-name-as-directory (make-temp-file "gh-trust-" t))
  "Where a test's CLI trust stores go: never the user's ~/.grok or agy's.")

(defmacro ghostherd-tests--with-herd (bindings &rest body)
  "Run BODY with an empty session registry.
BINDINGS are extra `let' bindings evaluated inside the clean registry."
  (declare (indent 1) (debug t))
  `(let ((ghostherd--sessions (make-hash-table :test 'equal))
         ;; Keyed by session id and global, so without this a test that
         ;; sends input to "a" leaves the next test's "a" inside the
         ;; input grace -- which is exactly how this line got written.
         (ghostherd--input-at (make-hash-table :test 'equal))
         (ghostherd--idle-since (make-hash-table :test 'equal))
         ;; Keyed by id and global, like `ghostherd--input-at': without this
         ;; one test's slept episode for "c" survives into the next test's.
         (ghostherd--sleep-continue (make-hash-table :test 'equal))
         ;; Keyed by transcript path: what a test's transcript said stays
         ;; that test's.
         (ghostherd--turn-ends (make-hash-table :test 'equal))
         (ghostherd--reports (make-hash-table :test 'equal))
         (ghostherd--screens (make-hash-table :test 'equal))
         (ghostherd--panes (make-hash-table :test 'equal))
         ;; Keyed by session id: one test's text left unsent, or ask turn
         ;; pending, is not the next test's.
         (ghostherd--submits (make-hash-table :test 'equal))
         (ghostherd--turn-origin (make-hash-table :test 'equal))
         (ghostherd--turn-ask-pending (make-hash-table :test 'equal))
         ;; Keyed by session id: a lead one test starts on agy is not owed
         ;; its role in the next.
         (ghostherd--role-owed (make-hash-table :test 'equal))
         (ghostherd--role-spent (make-hash-table :test 'equal))
         (ghostherd--handoff-leads (make-hash-table :test 'equal))
         ;; An agy spawned by a test is not guarded unless the test says so,
         ;; and a guard it writes goes under a temp root -- never the user's.
         (ghostherd-agy-guard nil)
         (ghostherd-guard-root ghostherd-tests--guard-root)
         ;; A worktree's trust is recorded in stores under a temp home --
         ;; never the user's ~/.grok or agy's settings.
         (ghostherd-trust-stores
          `((grok . ,(concat ghostherd-tests--trust-home "grok/trusted_folders.toml"))
            (agy . ,(concat ghostherd-tests--trust-home "agy/settings.json"))))
         (ghostherd-pretrust-file (concat ghostherd-tests--trust-home "pretrust.eld"))
         (ghostherd--guard-requests nil)
         ;; The panes' processes the snapshot names: none known, and none
         ;; read (a real `list-panes') unless a test says so.
         (ghostherd-tmux--pane-pids (make-hash-table :test 'equal))
         (ghostherd-tmux--pane-pids-at 1.0e+INF)
         (ghostherd--guard-seen (make-hash-table :test 'equal))
         ;; The guard's requests one test told of are not the next's.
         (ghostherd--guard-told (make-hash-table :test 'equal))
         (ghostherd--guard-told-ids nil)
         ;; Every logged transition now appends to a file, and the default
         ;; is the user's real one.  Persistence gets tested deliberately,
         ;; with a temporary file, and nowhere else.
         (ghostherd-log-file nil)
         (ghostherd--log-writable t)
         (ghostherd--log-loaded t)
         (ghostherd--sidebar-query "")
         (ghostherd--sidebar-filtering nil)
         (ghostherd--sidebar-help-visible nil)
         ,@bindings)
     ,@body))

(defun ghostherd-tests--session (&rest args)
  "Register and return a session built from ARGS, with a linked buffer."
  (let* ((name (or (plist-get args :name) "agent"))
         (id (or (plist-get args :id) name))
         (buf (generate-new-buffer (format "*ghostherd:%s*" name)))
         (session (apply #'ghostherd-session--create
                         :id id :buffer buf
                         (append args (list :name name)))))
    (with-current-buffer buf
      (setq-local ghostherd-session-id id))
    (puthash id session ghostherd--sessions)
    session))

;;; A backend that answers with whatever a test wants

(defvar ghostherd-tests--fake-screen ""
  "Screen the `fake' backend reports.")

(defvar ghostherd-tests--fake-live t
  "What the `fake' backend says about liveness.")

(defvar ghostherd-tests--fake-recipes nil
  "Recipes the `fake' backend offers `ghostherd-restore'.")

(cl-defmethod ghostherd-backend-capture
  ((_backend (eql fake)) _session &optional _n)
  ghostherd-tests--fake-screen)

(cl-defmethod ghostherd-backend-live-p ((_backend (eql fake)) _session)
  ghostherd-tests--fake-live)

(cl-defmethod ghostherd-backend-list ((_backend (eql fake)))
  ghostherd-tests--fake-recipes)

(defmacro ghostherd-tests--recording-keys (&rest body)
  "Run BODY with ghostel's send functions recorded into `sent'.
`sent' collects (KEY-NAME . MODS) for keys and (:text . STRING) for text."
  (declare (indent 0) (debug t))
  `(let ((sent nil))
     (cl-letf (((symbol-function 'ghostel-send-key)
                (lambda (name &optional mods) (push (cons name mods) sent)))
               ((symbol-function 'ghostel-paste-string)
                (lambda (text) (push (cons :text text) sent)))
               ((symbol-function 'derived-mode-p) (lambda (&rest _) t)))
       ,@body
       (nreverse sent))))

(defvar ghostherd-tests--tmux-calls nil
  "Argument lists handed to tmux by the code under test.")

(defun ghostherd-tests--tmux-panes ()
  "Fake `list-panes' output showing every registered tmux session alive."
  (mapconcat (lambda (s)
               (format "%s\t0\t\t\n" (ghostherd-session-host-id s)))
             (cl-remove-if-not
              (lambda (s) (eq (ghostherd-session-backend s) 'tmux))
              (hash-table-values ghostherd--sessions))
             ""))

(defmacro ghostherd-tests--with-tmux (responses &rest body)
  "Run BODY with tmux stubbed, recording calls in `ghostherd-tests--tmux-calls'.

RESPONSES maps a tmux subcommand to the (STATUS . OUTPUT) it answers
with.  `list-panes' defaults to reporting every registered tmux session
alive, since almost everything checks liveness on the way past;
anything else unlisted succeeds silently.

The recording is not `let'-bound, so assertions after the form still
see the calls."
  (declare (indent 1) (debug t))
  `(progn
     (setq ghostherd-tests--tmux-calls nil)
     (let ((ghostherd-tmux--status-cache nil))
       (cl-letf (((symbol-function 'ghostherd-tmux--call)
                  (lambda (args)
                    (push args ghostherd-tests--tmux-calls)
                    (or (alist-get (car args) ,responses nil nil #'equal)
                        (if (equal (car args) "list-panes")
                            (cons 0 (ghostherd-tests--tmux-panes))
                          (cons 0 "")))))
                 ;; `--load-buffer' feeds tmux over stdin (call-process-region),
                 ;; not `--call'; record the text it would send as the paste.
                 ((symbol-function 'ghostherd-tmux--load-buffer)
                  (lambda (name text)
                    (push (list "load-buffer" "-b" name "-" text)
                          ghostherd-tests--tmux-calls))))
         ,@body))
     (setq ghostherd-tests--tmux-calls
           (nreverse ghostherd-tests--tmux-calls))))

(defun ghostherd-tests--tmux-call (subcommand)
  "Return the recorded tmux call for SUBCOMMAND, or nil."
  (cl-find subcommand ghostherd-tests--tmux-calls
           :key #'car :test #'equal))

(defun ghostherd-tests--tmux-options ()
  "Return the (OPTION . VALUE) pairs written by recorded `set-option' calls."
  (mapcar (lambda (call) (cons (nth 3 call) (nth 4 call)))
          (cl-remove-if-not (lambda (call) (equal (car call) "set-option"))
                            ghostherd-tests--tmux-calls)))

(defun ghostherd-tests--tmux-session (&rest args)
  "Register a tmux-backed session from ARGS."
  (apply #'ghostherd-tests--session
         :backend 'tmux :host-id "gh-abc-rev" args))


;;; Used by more than one test file

(defconst ghostherd-tests--permission-screen
  (concat "╭────────────────────────────────────────────╮\n"
          "│ Bash(rm -rf build/ && npm publish)         │\n"
          "│                                            │\n"
          "│ Do you want to proceed?                    │\n"
          "│ ❯ 1. Yes                                   │\n"
          "╰────────────────────────────────────────────╯\n")
  "A permission prompt as an agent actually draws one.")

(defconst ghostherd-tests--claude-login-menu
  (concat
   "⏺ Login expired · Please run /login\n\n"
   "✻ Churned for 2s · done 9:12 AM\n"
   "▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔\n"
   "   Login\n\n"
   "   Claude Code can be used with your Claude subscription or billed based on API usage\n"
   "   through your Console account.\n\n"
   "   Select login method:\n\n"
   "   ❯ 1. Claude account with subscription · Pro, Max, Team, or Enterprise\n"
   "     2. Anthropic Console account · API usage billing\n"
   "     3. 3rd-party platform · Amazon Bedrock, Microsoft Foundry, Google Vertex AI\n\n"
   "   Esc to cancel\n")
  "claude's /login method picker, as auto-capture caught it (ask a74e0a82):
a ❯-marked numbered list that replaced the input box, no rule per menu.")

(cl-defmethod ghostherd-backend-view ((_backend (eql fake)) session)
  (ghostherd-session-buffer session))

(defmacro ghostherd-tests--with-log (&rest body)
  "Run BODY with an empty herd log."
  (declare (indent 0) (debug t))
  `(let ((ghostherd--log nil)
         (ghostherd-log-days 7)
         (ghostherd-log-max 500)
         (ghostherd-log-screens 20)
         (ghostherd--log-seq 0)
         (ghostherd--auto-capture-at (make-hash-table :test 'equal))
         (ghostherd--mismatch-since (make-hash-table :test 'equal)))
     (cl-letf (((symbol-function 'ghostherd--notify) (lambda (&rest _) nil)))
       ,@body)))

(defun ghostherd-tests--issue-p (re)
  "Whether the newest log line matching RE is an issue for the Log tab.
With no line matching, the test fails: a `should-not' must not pass on
nothing.  Asked beside the tests that drive each writer, so a reworded
line that `ghostherd--log-issue-res' no longer matches fails there."
  (if-let* ((e (seq-find (lambda (e) (string-match-p re (or (ghostherd-log-entry-text e) "")))
                         ghostherd--log)))
      (and (ghostherd-log-issue-p e) t)
    (ert-fail (list "No log line matches" re))))

(defmacro ghostherd-tests--with-dir-locals (contents &rest body)
  "Run BODY with `dir' bound to a temp directory holding CONTENTS.
CONTENTS is written verbatim as .dir-locals.el, or nil for none."
  (declare (indent 1) (debug t))
  `(let ((dir (file-name-as-directory (make-temp-file "ghostherd-dl" t))))
     (unwind-protect
         (progn
           (when ,contents
             (with-temp-file (expand-file-name ".dir-locals.el" dir)
               (insert ,contents)))
           ,@body)
       (delete-directory dir t))))

(defun ghostherd-tests--parse-json (string)
  "Parse STRING as JSON into alists with symbol keys."
  (json-parse-string string :object-type 'alist :array-type 'list))

(defconst ghostherd-tests--bot-env
  '("GHOSTHERD_TELEGRAM_TOKEN=fake-token" "GHOSTHERD_TELEGRAM_CHAT_IDS=1" "GITHUB_TOKEN=fake-gh")
  "The Telegram bot's variables, fake, and a token agents work with.")

(defun ghostherd-tests--bot-free-p (env)
  "Non-nil when ENV, VAR=VALUE strings, has no Telegram bot variable."
  (not (cl-find-if (lambda (v) (string-prefix-p "GHOSTHERD_TELEGRAM_" v)) env)))

(defmacro ghostherd-tests--with-asks (&rest body)
  "BODY in a clean herd with ask state of its own.
Sidecar requests are recorded in `requests' as (METHOD PARAMS), oldest
first, and the client is at /p/bin/herd."
  (declare (indent 0))
  `(ghostherd-tests--with-herd
       ((ghostherd--asks-open (make-hash-table :test 'equal))
        (ghostherd--asks-settled (make-hash-table :test 'equal))
        (ghostherd--herd-asks nil)
        (ghostherd--log nil)
        (requests nil))
     (cl-letf (((symbol-function 'ghostherd-herd-client) (lambda () "/p/bin/herd"))
               ((symbol-function 'ghostherd-memory-request-async)
                (lambda (method _cb &optional params _err)
                  (setq requests (append requests (list (list method params)))))))
       ,@body)))

(defmacro ghostherd-test--with-page (&rest body)
  "Run BODY with sidecar requests and page calls recorded.
`requests' holds (METHOD PARAMS) oldest first; REPLY, a function of
METHOD and PARAMS, answers them.  `calls' holds (FN . OBJ) oldest first."
  (declare (indent 0))
  `(progn
     (skip-unless (require 'ghostherd-memory-page nil t))
     (let ((requests nil) (calls nil) (reply (lambda (_m _p) nil)) (fail nil) (watched 0)
           (ghostherd-memory-page--lazy nil)
           (ghostherd-memory-page--importing nil)
           (ghostherd-memory-page--progress-shown nil)
           (ghostherd-memory-page--progress-inflight nil)
           (ghostherd-memory-page--progress-timer nil))
       (cl-letf (((symbol-function 'ghostherd-memory-request-async)
                  (lambda (method cb &optional params err-cb)
                    (setq requests (append requests (list (list method params))))
                    (if fail (funcall err-cb fail) (funcall cb (funcall reply method params)))))
                 ((symbol-function 'ghostherd-memory-page--js)
                  (lambda (fn obj) (setq calls (append calls (list (cons fn obj))))))
                 ;; The tick is called by hand; no timer outlives a test.
                 ((symbol-function 'ghostherd-memory-page--watch-import)
                  (lambda () (setq watched (1+ watched)))))
         ,@body))))

(defun ghostherd-test--call (calls fn)
  (cdr (car (last (seq-filter (lambda (c) (equal (car c) fn)) calls)))))

(defmacro ghostherd-test--with-herd (&rest body)
  "BODY with a fresh herd of one blocked claude agent and page calls recorded."
  (declare (indent 0))
  `(ghostherd-test--with-page
     (let ((ghostherd--sessions (make-hash-table :test #'equal))
           (ghostherd-usage--cache
            '((claude :windows ((:label "5h" :remaining 38 :resets-at 1791460000.0)
                                (:label "7d" :remaining 100 :resets-at nil))
                      :fetched 1791450000.0 :error nil)
              (agy :windows nil :fetched 1791450000.0 :error "token expired")))
           (ghostherd-memory-page--herd-timer nil))
       (puthash "claude-api"
                (ghostherd-session--create :id "claude-api" :name "claude-api" :kind 'claude
                                           :state 'blocked :state-reason "Bash(x) — proceed?"
                                           :project "/p/" :backend 'tmux :notes "api work")
                ghostherd--sessions)
       ,@body)))

(defconst ghostherd-test--rule (make-string 60 ?─))

(defun ghostherd-test--claude-screen (box &optional above)
  "A claude screen whose input box holds BOX, with ABOVE over it.
As claude draws it: its ❯ is followed by a no-break space, which a
test written with an ordinary one once hid."
  (concat (or above "  The tests pass.\n\n✻ Crunched for 3m 42s\n\n")
          ghostherd-test--rule "\n"
          (replace-regexp-in-string "\\`❯ " "❯\u00a0" box) "\n"
          ghostherd-test--rule
          "\n  ⏵⏵ auto mode on (shift+tab to cycle)"))

(defconst ghostherd-test--grok-welcome-screen
  (concat "  master ~/.emacs.d\n\n\n"
          "                New worktree                                 ctrl+w\n"
          "                Resume session                               ctrl+r\n"
          "                Changelog\n"
          "                Quit                                         ctrl+q\n\n"
          "  Update: v1.0.70 available, press ctrl+u to restart\n\n"
          "  ╭" (make-string 76 ?─) "╮\n"
          "  │ ❯" (make-string 74 ?\s) "│\n"
          "  ╰" (make-string 40 ?─) " Grok 4.7 (xhigh) · always-approve ─╯\n\n"
          "                                                     Grok Build  1.0.67 [stable]")
  "The screen ask c644aebc was closed with: grok's welcome, its box empty.")

(defun ghostherd-test--grok-at-rest (said)
  "grok at its prompt, its last answer SAID over its input box."
  (replace-regexp-in-string "  Update: .*" (concat "     " said) ghostherd-test--grok-welcome-screen
                            t t))

(defconst ghostherd-test--claude-trust
  (concat "\n" (make-string 120 ?─) "\n Accessing workspace:\n\n /Users/u/.ghostherd/worktrees/app/qa\n\n"
          " Quick safety check: Is this a project you created or one you trust? (Like your own code, a well-known open source\n"
          " project, or work from your team). If not, take a moment to review what's in this folder first.\n\n"
          " Claude Code'll be able to read, edit, and execute files here.\n\n Security guide\n\n"
          " ❯ No, exit\n   Yes, I trust this folder\n\n Enter to confirm · Esc to cancel")
  "claude's trust prompt as it draws it, from the top of the pane.")

(defmacro ghostherd-tests--with-leads (&rest body)
  "BODY with roles, unheard notes and work told of its own, nothing to say
what a project works on, `ghostherd-spawn' making
a test session in state `starting', and timers asked for in `later'."
  (declare (indent 0))
  `(ghostherd-tests--with-herd ((ghostherd--roles (make-hash-table :test 'equal))
                                (ghostherd--lead-unheard (make-hash-table :test 'equal))
                                (ghostherd--lead-work-told (make-hash-table :test 'equal))
                                (ghostherd-project-work-functions nil))
     (let ((spawned nil) (later nil))
       (cl-letf (((symbol-function 'ghostherd-spawn)
                  (lambda (kind &rest pl)
                    (push (cons kind pl) spawned)
                    (let ((s (ghostherd-tests--session :name (plist-get pl :name) :kind kind
                                                       :project (plist-get pl :project) :state 'starting)))
                      (when (plist-get pl :role)
                        (puthash (plist-get pl :name) (plist-get pl :role) ghostherd--roles))
                      s)))
                 ((symbol-function 'run-at-time)
                  (lambda (_time _repeat f &rest args) (push (cons f args) later))))
         ,@body))))

(defconst ghostherd-test--png "\x89PNG\r\n\x1a\n\0\303\251\377 \n rest"
  "A PNG's signature, then bytes a text coding would change: UTF-8 to
decode, a lone newline to turn into CRLF.")

(defconst ghostherd-test--png-sh "printf '\\211PNG\\r\\n\\032\\n\\000\\303\\251\\377 \\n rest'"
  "A clipboard command that prints `ghostherd-test--png'.")

(defmacro ghostherd-test--with-clipboard (script &rest body)
  "BODY with the clipboard's image printed by SCRIPT, images saved under a temp dir."
  (declare (indent 1))
  `(let* ((root (make-temp-file "gh-images" t))
          (ghostherd-image-directory (expand-file-name "images/" root))
          (ghostherd-image-keep-days 30)
          (ghostherd-clipboard-image-commands (list (list "sh" "-c" ,script))))
     (unwind-protect (progn ,@body)
       (delete-directory root t))))

(defmacro ghostherd-test--pushing (&rest body)
  "BODY with pushes on: the bot set up, you away from the desk and from
the page; rooms linked as \"room:PROJECT\"; `idle' the seconds since you
touched this machine."
  (declare (indent 0))
  `(ghostherd-tests--with-herd
       ((ghostherd--herd-pushes nil)
        (ghostherd--working-since (make-hash-table :test 'equal))
        (ghostherd--roles (make-hash-table :test 'equal))
        (ghostherd-push '(blocked report ask-failed))
        (ghostherd-push-away-after 120)
        (ghostherd-present-functions nil)
        (ghostherd-watched-functions nil)
        (ghostherd-room-url-function (lambda (p) (concat "room:" p)))
        (process-environment (append '("GHOSTHERD_TELEGRAM_TOKEN=t" "GHOSTHERD_TELEGRAM_CHAT_IDS=1")
                                     process-environment))
        (idle 600))
     (cl-letf (((symbol-function 'ghostherd--idle-seconds) (lambda () idle))
               ((symbol-function 'ghostherd--maybe-notify-state) #'ignore))
       ,@body)))

(defconst ghostherd-test--agy-menu
  "  Hi! How can I help you today?

────────────────────────────────────────────────────────────
> 解釋一下Respawn 的 fresh 跟 continue 差在哪裡

──────────────────────────────────────────────────────────────────────────────────
Requesting permission for:
   grep -rn -C 3 \"respawn\" local/ghostherd/
Run this command?
> 1. Yes, run command
  2. Yes, and always allow in this conversation for commands that start with
'grep'
  3. Yes, and always allow for commands that start with 'grep' (Persist to
settings.json)
  4. No, cancel
  ↑/↓ Navigate · tab Amend · ctrl+g edit/expand command
esc to cancel                         Gemini 3.8 Flash · high · AI: Out of credits
"
  "agy on a command it asks to run.")

(defconst ghostherd-test--claude-permission
  "● I'll run the tests.

╭──────────────────────────────────────────────────────────────╮
│ Bash command                                                 │
│                                                              │
│   npm test -- --runInBand                                    │
│   Run the test suite                                         │
│                                                              │
│ Do you want to proceed?                                      │
│ ❯ 1. Yes                                                     │
│   2. Yes, and don't ask again for npm test commands in /x    │
│   3. No, and tell Claude what to do differently (esc)        │
╰──────────────────────────────────────────────────────────────╯
"
  "claude's permission prompt, in its box.")

(defconst ghostherd-test--claude-trust
  " ─────────────────────────────────────────────────────────────
 Accessing workspace:

 /tmp/gh-scratch/demo

 Quick safety check: Is this a project you created or one
 you trust? (Like your own code, a well-known open source
 project, or work from your team). If not, take a moment to
 review what's in this folder first.

 Claude Code'll be able to read, edit, and execute files
 here.

 Security guide

 ❯ No, exit
   Yes, I trust this folder

 Enter to confirm · Esc to cancel
"
  "claude's trust-folder prompt on startup in a directory it has not seen,
captured from a real scratch claude: a marked [❯] menu with no numbers and
no input box.  The agent is parked on it, not idle.")

(defun ghostherd-test--agy-menu-on (on)
  "agy's menu with its highlight on choice ON, or on none."
  (replace-regexp-in-string
   "^[> ] \\([1-4]\\)\\. "
   (lambda (m) (format "%s %s. " (if (eql (string-to-number (match-string 1 m)) on) ">" " ")
                       (match-string 1 m)))
   ghostherd-test--agy-menu t t))

(defmacro ghostherd-tests--relaying (&rest body)
  "BODY in a clean herd that can relay.  Sidecar requests land in
`requests' as (METHOD PARAMS); the brief is \"BRIEF\".  `spawned' holds
the heir's (KIND . PLIST); the heir is registered idle on the fake
backend.  `told' holds (FN SESSION TEXT ROLE) for what it was told, FN
`tell-by-herd' for what it was told as the herd's.
`ready' is the heir's wait for its prompt, (FN FAIL), when `hold' is
set; otherwise it is told at once."
  (declare (indent 0))
  `(ghostherd-tests--with-herd
       ((ghostherd--asks-open (make-hash-table :test 'equal))
        (ghostherd--asks-settled (make-hash-table :test 'equal))
        (ghostherd--relay-held (make-hash-table :test 'equal))
        (ghostherd--relaying (make-hash-table :test 'equal))
        (ghostherd--relay-offered (make-hash-table :test 'equal))
        (ghostherd--lineage (make-hash-table :test 'equal))
        (ghostherd--roles (make-hash-table :test 'equal))
        (ghostherd--herd-links (make-hash-table :test 'equal))
        (ghostherd--lead-work-told (make-hash-table :test 'equal))
        (ghostherd--lead-unheard (make-hash-table :test 'equal))
        (ghostherd--herd-asks nil)
        (ghostherd--herd-pushes nil)
        (ghostherd-usage--cache nil)
        (ghostherd-relay-kinds '(claude grok agy))
        (ghostherd--log nil)
        (requests nil) (spawned nil) (told nil) (ready nil) (hold nil) (brief-error nil))
     ;; Its paths are labels: nothing starts in them, so none is missing.
     (cl-letf (((symbol-function 'ghostherd--directory-missing-p) #'ignore)
               ((symbol-function 'ghostherd-herd-client) (lambda () "/p/bin/herd"))
               ((symbol-function 'ghostherd-memory-rpc-url) (lambda () "http://127.0.0.1:1/jsonrpc"))
               ((symbol-function 'ghostherd-memory-request-async)
                (lambda (method cb &optional params err)
                  (setq requests (append requests (list (list method params))))
                  (when (equal method "herd_relay_brief")
                    (if brief-error (funcall err brief-error) (funcall cb '(:text "BRIEF"))))))
               ((symbol-function 'ghostherd--host-kill) #'ignore)
               ((symbol-function 'ghostherd-project-defaults) (lambda (&optional _d) (cons nil nil)))
               ((symbol-function 'ghostherd-spawn)
                (lambda (kind &rest plist)
                  (setq spawned (cons kind plist))
                  (let ((heir (ghostherd-tests--session :name (plist-get plist :name) :kind kind
                                                        :state 'idle :backend 'fake
                                                        :project (plist-get plist :project)
                                                        :notes (plist-get plist :notes)
                                                        :args (plist-get plist :args))))
                    (if (plist-get plist :role)
                        (puthash (plist-get plist :name) (plist-get plist :role) ghostherd--roles)
                      (remhash (plist-get plist :name) ghostherd--roles))
                    (if (plist-get plist :lineage)
                        (puthash (plist-get plist :name) (plist-get plist :lineage) ghostherd--lineage)
                      (remhash (plist-get plist :name) ghostherd--lineage))
                    heir)))
               ((symbol-function 'ghostherd--when-ready)
                (lambda (_s fn &optional _tries fail)
                  (if hold (setq ready (list fn fail)) (funcall fn))))
               ((symbol-function 'ghostherd-prompt)
                (lambda (s text &rest _) (push (list 'prompt s text nil) told)))
               ((symbol-function 'ghostherd-tell)
                (lambda (s text &optional role by-herd)
                  (push (list (if by-herd 'tell-by-herd 'tell) s text role) told))))
       ,@body)))

(defun ghostherd-tests--used-up (kind &optional resets fetched label)
  "Put KIND's usage in the cache as used up, until RESETS (seconds from
now, an hour by default), read FETCHED seconds ago."
  (setf (alist-get kind ghostherd-usage--cache)
        (list :windows (list (list :label (or label "5h") :remaining 0
                                   :resets-at (+ (float-time) (or resets 3600))))
              :fetched (- (float-time) (or fetched 0)))))

(defvar ghostherd-tests--fake-spawned nil
  "The plist the `fake' backend was last asked to spawn.")

(cl-defmethod ghostherd-backend-spawn ((_backend (eql fake)) plist)
  (setq ghostherd-tests--fake-spawned plist)
  (list :host-id (concat "gh-" (plist-get plist :name)) :buffer nil :started t))

(defmacro ghostherd-tests--with-repo (&rest body)
  "BODY with `base' a temp dir, `repo' a git checkout on `main' in it with
one commit, and `git' a lambda (DIR &rest ARGS) running git in DIR."
  (declare (indent 0) (debug t))
  `(let* ((base (file-name-as-directory (file-truename (make-temp-file "gh-wt" t))))
          (repo (concat base "repo/"))
          (git (lambda (dir &rest args)
                 (let ((default-directory dir))
                   (apply #'call-process "git" nil nil nil
                          "-c" "user.email=t@t" "-c" "user.name=t" args)))))
     (unwind-protect
         (progn
           (make-directory repo t)
           (funcall git repo "init" "-q" "-b" "main")
           (funcall git repo "commit" "-q" "--allow-empty" "-m" "start")
           ,@body)
       (delete-directory base t))))

(defmacro ghostherd-tests--with-lead-start (&rest body)
  "BODY in a clean herd over a temp git `repo' (`with-repo'), with the
lead-start hashes and the fake backend bound, a lead `lead-p' in `repo',
and worktrees under the temp dir -- never the user's tmux or worktree root."
  (declare (indent 0) (debug t))
  `(ghostherd-tests--with-repo
     (ghostherd-tests--with-herd
         ((ghostherd--roles (make-hash-table :test 'equal))
          (ghostherd--worktrees (make-hash-table :test 'equal))
          (ghostherd--started-by (make-hash-table :test 'equal))
          (ghostherd--lineage (make-hash-table :test 'equal))
          (ghostherd--herd-asks nil)
          (ghostherd-tests--fake-spawned nil)
          (ghostherd-backend 'fake)
          (ghostherd-worktree-root (concat base "wts/"))
          (ghostherd-lead-may-start t)
          (ghostherd-lead-start-max 2))
       (cl-letf (((symbol-function 'ghostherd--install-hooks) #'ignore)
                 ((symbol-function 'ghostherd--register-eval-cmds) #'ignore)
                 ((symbol-function 'ghostherd--ensure-poll-timer) #'ignore)
                 ((symbol-function 'ghostherd--worktree-deps-setup) #'ignore)
                 ((symbol-function 'ghostherd--host-kill) #'ignore)
                 ((symbol-function 'ghostherd--sidebar-refresh) #'ignore)
                 ((symbol-function 'ghostherd-usage-used-up) (lambda (&rest _) nil)))
         (ghostherd-tests--session :name "lead-p" :kind 'claude :backend 'fake :project repo)
         (puthash "lead-p" "lead" ghostherd--roles)
         ,@body))))

(defmacro ghostherd-tests--with-handoff (&rest body)
  "BODY as in `ghostherd-tests--with-lead-start' -- `lead-p' leads `repo' --
with `other' a second git checkout and no lead in it, `plain' a directory
that is not one, the handoff tables its own, and pushes on as
`ghostherd-test--pushing' has them: you away, the bot set up."
  (declare (indent 0) (debug t))
  `(ghostherd-tests--with-lead-start
     (let* ((other (concat base "other/"))
            (plain (concat base "plain/"))
            (ghostherd--handoff-leads (make-hash-table :test 'equal))
            (ghostherd--role-owed (make-hash-table :test 'equal))
            (ghostherd-handoff-may-start-lead t)
            (ghostherd-handoff-lead-max 3)
            (ghostherd-lead-kind 'claude)
            (ghostherd-project-work-functions nil)
            (ghostherd--herd-pushes nil)
            (ghostherd-push '(blocked report ask-failed used-up lead-started))
            (ghostherd-push-away-after nil)
            (ghostherd-present-functions nil)
            (ghostherd-watched-functions nil)
            (ghostherd-room-url-function nil)
            (ghostherd--log nil)
            (process-environment (append '("GHOSTHERD_TELEGRAM_TOKEN=t" "GHOSTHERD_TELEGRAM_CHAT_IDS=1")
                                         process-environment)))
       (make-directory other t)
       (funcall git other "init" "-q" "-b" "main")
       (funcall git other "commit" "-q" "--allow-empty" "-m" "start")
       (make-directory plain t)
       (cl-letf (((symbol-function 'ghostherd--when-ready) #'ignore)
                 ((symbol-function 'message) #'ignore))
         ,@body))))

(defmacro ghostherd-tests--with-primary (&rest body)
  "BODY as in `ghostherd-tests--with-lead-start' -- `lead-p' leads `repo'
-- with primary's directory under the temp dir, its kind claude, the
herd's links its own, and `said' what was prompted, newest first."
  (declare (indent 0) (debug t))
  `(ghostherd-tests--with-lead-start
     (let ((ghostherd-primary-directory (concat base "herd/primary/"))
           (ghostherd-primary-kind 'claude)
           (ghostherd--herd-links (make-hash-table :test 'equal))
           (ghostherd--log nil)
           (said nil))
       (cl-letf (((symbol-function 'ghostherd-prompt)
                  (lambda (s text &rest _) (push (cons (ghostherd-session-name s) text) said)))
                 ;; The kept conversation is assumed to still exist; a test of
                 ;; the gone case overrides this with its own cl-letf.
                 ((symbol-function 'ghostherd--conversation-gone-p) (lambda (&rest _) nil))
                 ((symbol-function 'message) #'ignore))
         ,@body))))

(defun ghostherd-tests--inbox-kinds (items)
  (mapcar (lambda (it) (plist-get it :kind)) items))

(provide 'ghostherd-test-helpers)
;;; ghostherd-test-helpers.el ends here

;;; ghostherd-hooks-tests.el --- Tests for ghostherd-hooks.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Part of ghostherd's tests; ghostherd-tests.el runs them all.  This
;; file alone:
;;
;;   emacs --batch --init-directory=~/.emacs.d/emacs-home/ \
;;         -L . -L ../xwapp -l ghostherd-hooks-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ghostherd-test-helpers)

(declare-function ghostherd-memory-page--handle "ghostherd-memory-page" (intent))

;;; What agents say about themselves

(defconst ghostherd-tests--conv "c0ffee00-1111-2222-3333-444455556666")

(ert-deftest ghostherd-test-hook-reports-go-through-report ()
  "A hook's state keeps the report's TTL and authority: it is a report."
  (ghostherd-tests--with-herd ((ghostherd--log nil))
    (let ((s (ghostherd-tests--session :name "agy-a" :kind 'agy :state 'idle)))
      (cl-letf (((symbol-function 'run-with-timer) #'ignore))
        (ghostherd--herd-take-reports
         '((:session "agy-a" :state "blocked" :reason "Approve run_command?")
           (:session "agy-a" :state "sleeping" :reason "")
           (:session "nobody" :state "done" :reason ""))))
      (should (equal (seq-take (gethash "agy-a" ghostherd--reports) 2)
                     '(blocked "Approve run_command?")))
      (should (equal (ghostherd--fresh-report s) '(blocked . "Approve run_command?")))
      (cl-letf (((symbol-function 'run-with-timer) #'ignore))
        (ghostherd--herd-take-reports '((:session "agy-a" :state "auto" :reason ""))))
      (should-not (gethash "agy-a" ghostherd--reports)))))

(ert-deftest ghostherd-test-hook-links-name-the-conversation ()
  (ghostherd-tests--with-herd ((ghostherd--herd-links (make-hash-table :test 'equal)))
    (let ((s (ghostherd-tests--session :name "claude-a" :kind 'claude)))
      (ghostherd--herd-take-links
       `((:session "claude-a" :cli "claude" :conversation ,ghostherd-tests--conv
                   :transcript "/t.jsonl" :last_head "Done." :last_ms 1.0)))
      (should (equal (ghostherd-session-conversation s) ghostherd-tests--conv))
      (should (equal (plist-get (ghostherd--link s) :last_head) "Done."))
      (ghostherd--herd-take-links '((:session "claude-a" :cli "claude" :conversation "")))
      (should-not (ghostherd-session-conversation s)))))

(ert-deftest ghostherd-test-respawn-resends-agy-lead-role ()
  "A lead on a CLI that carries its role in the first message (agy) is sent
it again on a cold respawn -- a new conversation would otherwise come up
without it.  Not on a continuing respawn (that conversation has it), not
for a CLI that carries the role in its args (claude), not for a non-lead."
  (ghostherd-tests--with-herd ((ghostherd--roles (make-hash-table :test 'equal)))
    (let (prompted)
      (cl-letf (((symbol-function 'ghostherd-spawn)
                 (lambda (kind &rest plist)
                   (ghostherd-tests--session :name (plist-get plist :name)
                                             :kind kind :backend 'fake)))
                ((symbol-function 'ghostherd--when-ready)
                 (lambda (_s fn &rest _) (funcall fn)))
                ((symbol-function 'ghostherd-prompt)
                 (lambda (s text &rest _) (setq prompted (cons s text)))))
        (cl-flet ((lead (name kind)
                    (let ((s (ghostherd-tests--session :name name :kind kind :backend 'fake)))
                      (puthash (ghostherd-session-id s) "lead" ghostherd--roles)
                      s)))
          ;; agy lead, cold respawn: the role is re-sent as the first message.
          (setq prompted nil)
          (ghostherd-respawn (lead "la" 'agy) nil)
          (should (equal (cdr prompted) ghostherd--lead-role))
          ;; agy lead, continuing respawn: the conversation has it, not re-sent.
          (setq prompted nil)
          (ghostherd-respawn (lead "lb" 'agy) t)
          (should-not prompted)
          ;; claude lead carries the role in its args, not a message.
          (setq prompted nil)
          (ghostherd-respawn (lead "lc" 'claude) nil)
          (should-not prompted)
          ;; agy, not a lead: nothing to re-send.
          (setq prompted nil)
          (ghostherd-respawn (ghostherd-tests--session :name "wk" :kind 'agy :backend 'fake) nil)
          (should-not prompted))))))

(ert-deftest ghostherd-test-respawn-resumes-its-own-conversation ()
  "`--continue' takes the newest conversation in the directory -- with two
claudes in one project, possibly the other's.  The hooks know which."
  (ghostherd-tests--with-herd ((ghostherd--herd-links (make-hash-table :test 'equal)))
    (let ((s (ghostherd-tests--session :name "claude-a" :kind 'claude
                                       :args '("--model" "opus" "-r" "old-one" "--fork-session")))
          spawned)
      (ghostherd--herd-take-links
       `((:session "claude-a" :cli "claude" :conversation ,ghostherd-tests--conv)))
      (cl-letf (((symbol-function 'ghostherd-spawn)
                 (lambda (_kind &rest plist) (setq spawned plist) nil))
                ((symbol-function 'ghostherd--conversation-gone-p) (lambda (&rest _) nil)))
        (ghostherd-respawn s t))
      (should (equal (plist-get spawned :args)
                     (list "--model" "opus" "-r" ghostherd-tests--conv))))))

(ert-deftest ghostherd-test-respawn-falls-back-to-continue ()
  (ghostherd-tests--with-herd ((ghostherd--herd-links (make-hash-table :test 'equal)))
    (let ((s (ghostherd-tests--session :name "claude-a" :kind 'claude :args '("--model" "opus")))
          spawned)
      (cl-letf (((symbol-function 'ghostherd-spawn)
                 (lambda (_kind &rest plist) (setq spawned plist) nil)))
        (ghostherd-respawn s t))
      (should (equal (plist-get spawned :args) '("--model" "opus" "--continue"))))))

(ert-deftest ghostherd-test-resumed-by-trusts-the-hooks ()
  "An agent that got onto a conversation by itself is found by its hooks;
one whose hooks name another conversation is not found by stale args."
  (ghostherd-tests--with-herd ((ghostherd--herd-links (make-hash-table :test 'equal)))
    (ghostherd-tests--session :name "claude-a" :kind 'claude :args nil)
    (ghostherd-tests--session :name "claude-b" :kind 'claude :args '("-r" "id-1"))
    (ghostherd--herd-take-links
     '((:session "claude-a" :conversation "id-1")
       (:session "claude-b" :conversation "id-2")))
    (cl-letf (((symbol-function 'ghostherd--session-live-p) (lambda (_) t)))
      (should (equal (ghostherd-session-name (ghostherd--resumed-by "id-1")) "claude-a"))
      (should (equal (ghostherd-session-name (ghostherd--resumed-by "id-2")) "claude-b")))))

(ert-deftest ghostherd-test-explain-names-the-conversation ()
  (ghostherd-tests--with-herd ((ghostherd--herd-links (make-hash-table :test 'equal)))
    (let ((s (ghostherd-tests--session :name "agy-a" :kind 'agy :backend 'fake)))
      (setq ghostherd-tests--fake-screen "")
      (ghostherd--herd-take-links
       '((:session "agy-a" :cli "agy" :conversation "a-456" :last_head "Committed ee5fee6.")))
      (save-window-excursion (ghostherd-explain s))
      (with-current-buffer "*ghostherd explain*"
        (should (string-search "talks    a-456  (agy, by its hooks)" (buffer-string)))
        (should (string-search "said     Committed ee5fee6." (buffer-string))))
      ;; Waiting is trusted for as long as it is out, not for a TTL.
      (puthash "agy-a" (list 'waiting "waiting on qa" (time-subtract nil 600)) ghostherd--reports)
      (save-window-excursion (ghostherd-explain s))
      (with-current-buffer "*ghostherd explain*"
        (should (string-search "waiting on qa  (10m ago, trusted until replaced)" (buffer-string)))
        (should-not (string-search "expired" (buffer-string)))))))

(ert-deftest ghostherd-test-ask-outcome-says-whose-words ()
  (should (equal (ghostherd--ask-outcome
                  '(:id "a1" :from "claude-main" :status "answered" :auto t :auto_source "message"))
                 "stopped without answering ask a1; claude-main got its last message"))
  (should (equal (ghostherd--ask-outcome
                  '(:id "a1" :from "claude-main" :status "answered" :auto t :auto_source "screen"))
                 "stopped without answering ask a1; claude-main got the screen"))
  ;; An issue for the Log tab: answered for it, not by it.  An answer is not.
  (dolist (o (list (list :status "answered" :auto t :auto_source "screen" :issue t)
                   (list :status "answered" :reply_head "done: it failed: twice")
                   (list :status "failed" :error "agy-a is gone" :issue t)
                   (list :status "cancelled")))
    (should (eq (plist-get o :issue)
                (ghostherd-log-issue-p
                 (ghostherd-log-entry--create
                  :kind 'ask :text (ghostherd--ask-outcome (append (list :id "a1" :from "claude-main") o))))))))

(ert-deftest ghostherd-test-install-hooks-shows-then-asks ()
  "Nothing is written unless the plan shown is accepted."
  (let (calls)
    (cl-letf (((symbol-function 'ghostherd-herd-client) (lambda () "/p/bin/herd"))
              ((symbol-function 'call-process)
               (lambda (_prog _in _buf _disp &rest args)
                 (push args calls) (insert "plan") 0))
              ((symbol-function 'yes-or-no-p) (lambda (_) nil)))
      (save-window-excursion (ghostherd-install-hooks)))
    (should (equal calls '(("hooks" "install" "--dry-run"))))))

(defmacro ghostherd-tests--with-transcripts (&rest body)
  "BODY with each kind's transcripts at ROOT/KIND/*/ID.jsonl, ROOT temporary."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "gh-transcripts" t)))
          (ghostherd--transcripts (make-hash-table :test 'equal))
          (ghostherd-agent-specs
           (mapcar (lambda (e)
                     (let ((p (copy-sequence (cdr e))))
                       (cons (car e)
                             (if (plist-get p :transcript)
                                 (plist-put p :transcript
                                            (concat root (symbol-name (car e)) "/*/%s.jsonl"))
                               p))))
                   ghostherd-agent-specs)))
     (unwind-protect (progn ,@body)
       (delete-directory root t))))

(defun ghostherd-tests--touch (file)
  (make-directory (file-name-directory file) t)
  (write-region "" nil file nil 'silent)
  file)

(ert-deftest ghostherd-test-args-name-the-conversation ()
  (should (equal (ghostherd--args-conversation 'claude '("--model" "opus" "-r" "abc-1")) "abc-1"))
  (should (equal (ghostherd--args-conversation 'agy '("--conversation" "c1")) "c1"))
  ;; A fork writes a conversation of its own.
  (should-not (ghostherd--args-conversation 'claude '("-r" "abc-1" "--fork-session")))
  (should-not (ghostherd--args-conversation 'claude '("-r" "--evil")))
  (should-not (ghostherd--args-conversation 'claude '("--continue")))
  (should-not (ghostherd--args-conversation 'shell '("-r" "x"))))

(ert-deftest ghostherd-test-transcript-from-hooks-then-from-args ()
  "Hooks name the transcript; without them, the conversation an agent was
started on finds it; a fresh agent without hooks has none."
  (ghostherd-tests--with-herd ((ghostherd--herd-links (make-hash-table :test 'equal)))
    (ghostherd-tests--with-transcripts
      (let* ((resumed (ghostherd-tests--session :name "claude-r" :kind 'claude :args '("-r" "abc-1")))
             (fresh (ghostherd-tests--session :name "claude-f" :kind 'claude :args nil))
             (hooked (ghostherd-tests--session :name "agy-h" :kind 'agy :args '("--conversation" "old")))
             (file (ghostherd-tests--touch (concat root "claude/-x-proj/abc-1.jsonl")))
             (linked (ghostherd-tests--touch (concat root "elsewhere/t.jsonl"))))
        (ghostherd-tests--touch (concat root "agy/x/old.jsonl"))
        (should (equal (ghostherd-session-transcript resumed) file))
        (should-not (ghostherd-session-transcript fresh))
        (ghostherd--herd-take-links
         `((:session "agy-h" :cli "agy" :conversation "new" :transcript ,linked)))
        (should (equal (ghostherd-session-transcript hooked) linked))
        ;; Hooks that name the conversation but not its file: found by id,
        ;; the new one, not the one its arguments started it on.
        (let ((agy-file (ghostherd-tests--touch (concat root "agy/x/new.jsonl"))))
          (ghostherd--herd-take-links '((:session "agy-h" :cli "agy" :conversation "new")))
          (should (equal (ghostherd-session-transcript hooked) agy-file)))))))

(ert-deftest ghostherd-test-transcript-missing-is-not-sought-every-tick ()
  "The room asks every 2s; a transcript not there is looked for every 30."
  (ghostherd-tests--with-herd ((ghostherd--herd-links (make-hash-table :test 'equal)))
    (ghostherd-tests--with-transcripts
      (let* ((s (ghostherd-tests--session :name "claude-r" :kind 'claude :args '("-r" "late-1")))
             (globs 0)
             (inside nil)
             (orig (symbol-function 'file-expand-wildcards)))
        ;; It calls itself for a wildcard in a directory: count the outer.
        (cl-letf (((symbol-function 'file-expand-wildcards)
                   (lambda (&rest a)
                     (if inside
                         (apply orig a)
                       (cl-incf globs)
                       (setq inside t)
                       (unwind-protect (apply orig a) (setq inside nil))))))
          (should-not (ghostherd-session-transcript s))
          (ghostherd-tests--touch (concat root "claude/p/late-1.jsonl"))
          (should-not (ghostherd-session-transcript s))
          (should (= globs 1))
          (setcdr (gethash '(claude . "late-1") ghostherd--transcripts) 0)
          (should (ghostherd-session-transcript s))
          (should (= globs 2))
          ;; Found, it is not looked for again.
          (ghostherd-session-transcript s)
          (should (= globs 2)))))))

(ert-deftest ghostherd-test-cmd-self-is-the-calling-terminal ()
  "`ghostel_cmd' is dispatched from the asking terminal's VT parser, so
the caller is identifiable with no environment at all."
  (ghostherd-tests--with-herd ()
    (let ((session (ghostherd-tests--session :name "impl" :kind 'agy
                                             :state 'working)))
      (with-current-buffer (ghostherd-session-buffer session)
        (let ((parsed (ghostherd-tests--parse-json (ghostherd-cmd-state "self"))))
          (should (equal (alist-get 'name parsed) "impl")))))))

(ert-deftest ghostherd-test-cmd-self-outside-an-agent-is-an-error ()
  "Better than resolving to whatever buffer happened to be current."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--session :name "impl" :kind 'agy)
    (with-temp-buffer
      (should-error (ghostherd-cmd-state "self") :type 'user-error))))

(ert-deftest ghostherd-test-cmd-message-attributes-the-caller ()
  "FROM used to default to \"user\", which was a lie whenever the caller
was another agent -- and an agent had no way to say otherwise."
  (ghostherd-tests--with-herd ()
    (let ((impl (ghostherd-tests--session :name "impl" :kind 'agy))
          (rev (ghostherd-tests--session :name "rev" :kind 'agy))
          (sent nil))
      (ignore rev)
      (cl-letf (((symbol-function 'ghostherd-send)
                 (lambda (_session text &optional _submit) (setq sent text))))
        (with-current-buffer (ghostherd-session-buffer impl)
          (ghostherd-cmd-message "rev" "have a look"))
        (should (string-match-p "impl" sent))
        (should-not (string-match-p "user" sent))))))

;;; The guard of an agy agent (ask 75362f01)

(defmacro ghostherd-tests--with-guard (&rest body)
  "BODY in a clean herd with the fake backend, a temp `repo' (a project),
the guard on (auto) under a temp root, and the herd client at /p/bin/herd."
  (declare (indent 0) (debug t))
  `(ghostherd-tests--with-repo
     (ghostherd-tests--with-herd
         ((ghostherd--roles (make-hash-table :test 'equal))
          (ghostherd-tests--fake-spawned nil)
          (ghostherd-agy-guard 'auto)
          (ghostherd-guard-root (concat base "guard/"))
          (ghostherd-guard-allow-commands nil))
       (cl-letf (((symbol-function 'ghostherd--install-hooks) #'ignore)
                 ((symbol-function 'ghostherd--register-eval-cmds) #'ignore)
                 ((symbol-function 'ghostherd--ensure-poll-timer) #'ignore)
                 ((symbol-function 'ghostherd--sidebar-refresh) #'ignore)
                 ((symbol-function 'ghostherd--when-ready) #'ignore)
                 ((symbol-function 'ghostherd-herd-client) (lambda () "/p/bin/herd")))
         ,@body))))

(defun ghostherd-tests--json-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (json-parse-buffer :object-type 'alist :array-type 'list)))

(ert-deftest ghostherd-test-guard-mode-per-project ()
  "auto unless a project's .dir-locals.el says otherwise -- read-only, or
nil, which counts as said."
  (ghostherd-tests--with-guard
    (should (eq (ghostherd--agy-guard-mode repo) 'auto))
    (let ((ghostherd-agy-guard nil)) (should-not (ghostherd--agy-guard-mode repo)))
    (with-temp-file (concat repo ".dir-locals.el")
      (insert "((nil . ((ghostherd-agy-guard . read-only) (ghostherd-guard-allow-commands . (\"npm test\")))))\n"))
    (should (eq (ghostherd--agy-guard-mode repo) 'read-only))
    (with-temp-file (concat repo ".dir-locals.el")
      (insert "((nil . ((ghostherd-agy-guard . nil))))\n"))
    (should-not (ghostherd--agy-guard-mode repo))))

(ert-deftest ghostherd-test-a-guarded-agy-spawns-with-its-guard ()
  "The flag and the --add-dir of its own guard dir, which holds its policy
and the hooks.json agy loads; the env scrub runs in front of it; its first
words say a guard decides."
  (ghostherd-tests--with-guard
    (with-temp-file (concat repo ".dir-locals.el")
      (insert "((nil . ((ghostherd-guard-allow-commands . (\"npm test\" \"pytest\")))))\n"))
    (let* ((said nil)
           (s (cl-letf (((symbol-function 'ghostherd--when-ready) (lambda (_s f &rest _) (funcall f)))
                        ((symbol-function 'ghostherd-prompt) (lambda (_s text &rest _) (setq said text)))
                        ((symbol-function 'ghostherd--memory-opening) (lambda (_p &optional _a) "MEMORY")))
                (ghostherd-spawn 'agy :name "agy-p" :project repo :directory repo :backend 'fake :display nil)))
           (dir (ghostherd--guard-dir "agy-p"))
           (args (plist-get ghostherd-tests--fake-spawned :args)))
      (should s)
      (should (equal (last args 3) (list "--dangerously-skip-permissions" "--add-dir" (directory-file-name dir))))
      (should (equal (file-modes dir) #o700))
      (let ((pol (ghostherd-tests--json-file (concat dir "policy.json"))))
        (should (equal (alist-get 'agent pol) "agy-p"))
        (should (equal (alist-get 'mode pol) "auto"))
        (should (equal (alist-get 'workspace pol) (expand-file-name repo)))
        (should (equal (alist-get 'allow_commands pol) '("npm test" "pytest")))
        (should (equal (alist-get 'herd pol) '("/p/bin/herd" "/p/bin/ghostherd")))
        (should (equal (alist-get 'guard_root pol) (expand-file-name ghostherd-guard-root)))
        ;; The herd's own token: never the agent's, not even approved.
        (should (equal (alist-get 'never_files pol) (list (ghostherd-memory--token-file)))))
      ;; Its asks go out marked: it is guarded, as the snapshot says.
      (should (ghostherd--session-guarded-p s))
      (should (eq (plist-get (seq-find (lambda (r) (equal (plist-get r :name) "agy-p"))
                                       (ghostherd--herd-snapshot))
                             :guarded)
                  t))
      (let* ((hooks (ghostherd-tests--json-file (concat dir ".agents/hooks.json")))
             (group (car (alist-get 'PreToolUse (alist-get 'ghostherd-guard hooks))))
             (cmd (alist-get 'command (car (alist-get 'hooks group)))))
        (should (equal (alist-get 'matcher group) "*"))
        (should (string-prefix-p (concat "/p/bin/herd guard " (shell-quote-argument dir)) cmd))
        ;; No answer from the guard is a denial, never a silent allow.
        (should (string-match-p "|| echo '{\"decision\":\"deny\"" cmd)))
      (let ((wrapper (plist-get ghostherd-tests--fake-spawned :wrapper)))
        (should (equal (seq-take wrapper 2) '("/p/bin/herd" "scrub-exec")))
        (should (equal (car (last wrapper)) "--"))
        ;; An allowlist: only what is named goes through.
        (should (equal (cl-subseq wrapper 2 4) '("--allow" "PATH")))
        (should-not (member "--scrub" wrapper))
        (should (member "GHOSTHERD_RPC" wrapper)))
      (should (string-prefix-p (format ghostherd--agy-guard-note repo (ghostherd--guard-dir "agy-p")) said))
      (should (string-suffix-p "MEMORY" said)))))

(ert-deftest ghostherd-test-a-respawn-takes-the-guard-as-it-is-now ()
  "The guard's pair goes once, however often it is started; the guard off
takes it out -- but not a flag the user gave agy themselves."
  (ghostherd-tests--with-guard
    (let* ((dir (directory-file-name (ghostherd--guard-dir "a")))
           (once (ghostherd--with-agy-guard '("--model" "x") (ghostherd--guard-dir "a"))))
      (should (equal once (list "--model" "x" "--dangerously-skip-permissions" "--add-dir" dir)))
      (should (equal (ghostherd--with-agy-guard once (ghostherd--guard-dir "a")) once))
      (should (equal (ghostherd--with-agy-guard once nil) '("--model" "x")))
      (should (equal (ghostherd--with-agy-guard '("--dangerously-skip-permissions") nil)
                     '("--dangerously-skip-permissions"))))
    ;; Respawned from a recipe that had it, with the guard now off.
    (let ((ghostherd-agy-guard nil))
      (ghostherd-spawn 'agy :name "agy-q" :project repo :directory repo :backend 'fake :display nil
                       :args (list "--dangerously-skip-permissions" "--add-dir"
                                   (directory-file-name (ghostherd--guard-dir "agy-q"))))
      (should-not (member "--dangerously-skip-permissions" (plist-get ghostherd-tests--fake-spawned :args)))
      (should-not (plist-get ghostherd-tests--fake-spawned :wrapper)))))

(ert-deftest ghostherd-test-only-agy-is-guarded ()
  "claude and grok spawn as they did: no flag, no guard, no wrapper."
  (ghostherd-tests--with-guard
    (dolist (kind '(claude grok))
      (ghostherd-spawn kind :name (format "%s-p" kind) :project repo :directory repo :backend 'fake :display nil)
      (should-not (member "--dangerously-skip-permissions" (plist-get ghostherd-tests--fake-spawned :args)))
      (should-not (plist-get ghostherd-tests--fake-spawned :wrapper))
      (should-not (file-exists-p (ghostherd--guard-dir (format "%s-p" kind)))))))

(ert-deftest ghostherd-test-the-backends-run-the-wrapper-first ()
  "tmux execs the wrapper, which execs the CLI; ghostel types it first."
  (let ((got nil))
    (cl-letf (((symbol-function 'ghostherd-tmux--ensure-available) #'ignore)
              ((symbol-function 'ghostherd-tmux--unique-id) (lambda (&rest _) "gh-x"))
              ((symbol-function 'ghostherd-tmux--write-recipe) #'ignore)
              ((symbol-function 'ghostherd-tmux--forget) #'ignore)
              ((symbol-function 'ghostherd-agent-environment) (lambda (&rest _) nil))
              ((symbol-function 'ghostherd-tmux--new-session) (lambda (args _env) (setq got args))))
      (ghostherd-backend-spawn 'tmux (list :name "x" :command "agy" :args '("--add-dir" "/g")
                                           :directory "/tmp/" :wrapper '("/p/bin/herd" "scrub-exec" "--"))))
    (should (equal (last got 6) '("/p/bin/herd" "scrub-exec" "--" "agy" "--add-dir" "/g")))
    ;; ghostel types the launch line into its shell: the wrapper, quoted, first.
    (let ((buf (generate-new-buffer " *gh-wrapper-test*")) typed)
      (unwind-protect
          (cl-letf (((symbol-function 'ghostherd--require-ghostel) #'ignore)
                    ((symbol-function 'ghostherd-agent-environment) (lambda (&rest _) nil))
                    ((symbol-function 'ghostel) (lambda (&rest _) buf))
                    ((symbol-function 'ghostherd--launch-in-buffer) (lambda (_b launch) (setq typed launch))))
            (ghostherd-backend-spawn 'ghostel (list :name "gh-wrapper-test" :command "agy" :args '("--add-dir" "/g")
                                                    :directory "/tmp/"
                                                    :wrapper '("/p/bin/herd" "scrub-exec" "--scrub" "(^|_)KEY" "--"))))
        (kill-buffer buf))
      (should (equal typed "/p/bin/herd scrub-exec --scrub \\(\\^\\|_\\)KEY -- agy --add-dir /g")))))

(ert-deftest ghostherd-test-the-default-scrub-drops-secrets-and-keeps-the-herd ()
  "The default allowlist, run through the real `herd scrub-exec': only
what is named goes through -- whatever a secret's name, DATABASE_URL or a
webhook too -- with what the herd, the terminal and git need; a proxy
with a password in it is dropped.  A project adds names of its own."
  (let* ((herd (expand-file-name "bin/herd" (file-name-directory (locate-library "ghostherd"))))
         (dir (make-temp-file "gh-proj-" t))
         (ghostherd-herd-client-function nil)
         (ghostherd-agent-env-allow-extra '("MY_TOOL_HOME"))
         (process-environment
          (append '("GITHUB_TOKEN=s" "BITBUCKET_TOKEN=s" "CLICKUP_API_TOKEN=s" "GRAFANA_API_KEY=s"
                    "ANTHROPIC_API_KEY=s" "AWS_SECRET_ACCESS_KEY=s" "GHOSTHERD_TELEGRAM_TOKEN=s"
                    "DATABASE_URL=postgres://u:p@db/x" "SENTRY_DSN=https://k@sentry.io/1" "MONGO_URI=s"
                    "GITLAB_PAT=s" "DISCORD_WEBHOOK=https://discord.com/api/webhooks/1/x" "PLAIN=s"
                    "HTTPS_PROXY=http://proxy:3128" "HTTP_PROXY=http://me:pw@proxy:3128"
                    "GHOSTHERD_RPC=keep" "GHOSTHERD_SESSION=keep" "GHOSTHERD_HERD=keep" "SSH_AUTH_SOCK=keep"
                    "GIT_AUTHOR_NAME=keep" "LC_CTYPE=keep" "XDG_CONFIG_HOME=keep" "TERM_PROGRAM=keep"
                    "MY_TOOL_HOME=keep" "ORT_DISABLE_TELEMETRY=1")
                  process-environment)))
    (unwind-protect
        (cl-letf (((symbol-function 'ghostherd-herd-client) (lambda () herd)))
          (let* ((wrapper (ghostherd--scrub-wrapper dir))
                 (out (with-temp-buffer
                        (apply #'call-process (car wrapper) nil t nil
                               (append (cdr wrapper)
                                       (list "python3" "-c" "import os, json; print(json.dumps(sorted(os.environ)))")))
                        (json-parse-string (buffer-string) :array-type 'list))))
            (dolist (gone '("GITHUB_TOKEN" "BITBUCKET_TOKEN" "CLICKUP_API_TOKEN" "GRAFANA_API_KEY"
                            "ANTHROPIC_API_KEY" "AWS_SECRET_ACCESS_KEY" "GHOSTHERD_TELEGRAM_TOKEN"
                            "DATABASE_URL" "SENTRY_DSN" "MONGO_URI" "GITLAB_PAT" "DISCORD_WEBHOOK" "PLAIN"
                            "HTTP_PROXY"))
              (should-not (member gone out)))
            (dolist (kept '("GHOSTHERD_RPC" "GHOSTHERD_SESSION" "GHOSTHERD_HERD" "SSH_AUTH_SOCK" "GIT_AUTHOR_NAME"
                            "LC_CTYPE" "XDG_CONFIG_HOME" "TERM_PROGRAM" "HTTPS_PROXY" "MY_TOOL_HOME" "PATH" "HOME"
                            "ORT_DISABLE_TELEMETRY"))
              (should (member kept out)))))
      (delete-directory dir t))))

(ert-deftest ghostherd-test-the-guards-requests-are-logged-once ()
  "Each refusal, approval and use is a log line, once."
  (ghostherd-tests--with-herd ((ghostherd--log nil))
    (cl-letf (((symbol-function 'ghostherd--notify) #'ignore))
      (let ((r (lambda (status) (list (list :id "g1" :session "agy-p" :status status
                                           :summary "run_command: cat .env" :rule "secret-file")))))
        (ghostherd--herd-take-guard (funcall r "pending"))
        (ghostherd--herd-take-guard (funcall r "pending"))
        (ghostherd--herd-take-guard (funcall r "approved"))
        (ghostherd--herd-take-guard (funcall r "used"))
        (should (equal (mapcar #'ghostherd-log-entry-text (reverse ghostherd--log))
                       '("guard needs approval g1: run_command: cat .env -- secret-file"
                         "guard g1 approved once: run_command: cat .env"
                         "guard g1 ran, approved: run_command: cat .env")))
        (should (eq (ghostherd-log-entry-kind (car ghostherd--log)) 'guard))
        ;; Held for the user is an issue; the user's yes, and the run, are not.
        (should (ghostherd-tests--issue-p "needs approval"))
        (should-not (ghostherd-tests--issue-p "approved once"))
        (should-not (ghostherd-tests--issue-p "ran, approved"))))))

(ert-deftest ghostherd-test-the-cards-approve-once-reaches-the-herd ()
  "Approve once on the card asks the sidecar to approve that request, by
its id; anything not an id is refused before it goes anywhere."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let (asked shown)
    (cl-letf (((symbol-function 'ghostherd-memory-request-async)
               (lambda (method ok params &rest _) (push (list method params) asked) (funcall ok nil)))
              ((symbol-function 'ghostherd-memory-page--js) (lambda (&rest args) (push args shown))))
      (ghostherd-memory-page--handle '((op . "guard-approve") (id . "1a2b3c4d")))
      (should (equal asked '(("herd_guard_approve" (:id "1a2b3c4d")))))
      (should (equal (car shown) '("flash" "Approved once; the agent is told to run it again")))
      (dolist (bad '("../x" "" nil "1a2b3c4d; rm" "ZZZZ"))
        (ghostherd-memory-page--handle `((op . "guard-approve") (id . ,bad))))
      (should (= (length asked) 1)))))

(ert-deftest ghostherd-test-the-snapshot-names-each-agents-terminal ()
  "Each agent's host process rides the snapshot (`hostPid'), so the herd
takes a hook's report as its own only from inside it -- not from a CLI a
server it started in the background runs (claude-jnotes, 2026-10-22).
tmux: its pane's process, read for the whole herd in one `list-panes' and
kept, asked again for a pane it lacks at most every few seconds.  ghostel:
its terminal's shell.  0 when unknown."
  (ghostherd-tests--with-herd ()
    (let ((calls 0)
          (ghostherd-tmux--pane-pids (make-hash-table :test #'equal))
          (ghostherd-tmux--pane-pids-at 0)
          (panes "gh-1-a\t24671\ngh-1-b\t41426\n")
          (row (lambda (name) (seq-find (lambda (r) (equal (plist-get r :name) name))
                                        (ghostherd--herd-snapshot)))))
      (cl-letf (((symbol-function 'ghostherd-tmux--call)
                 (lambda (args)
                   (if (equal args '("list-panes" "-a" "-F" "#{session_name}\t#{pane_pid}"))
                       (progn (cl-incf calls) (cons 0 panes))
                     (cons 1 "")))))
        (let ((a (ghostherd-tests--session :name "a" :kind 'claude :backend 'tmux :host-id "gh-1-a"))
              (b (ghostherd-tests--session :name "b" :kind 'claude :backend 'tmux :host-id "gh-1-b"))
              (c (ghostherd-tests--session :name "c" :kind 'claude :backend 'tmux :host-id "gh-1-c")))
          (should (equal (list (ghostherd--host-pid a) (ghostherd--host-pid b)) '(24671 41426)))
          (should (= calls 1))
          ;; No pane for c yet: unknown, and not asked again at once.
          (should-not (ghostherd--host-pid c))
          (should (= calls 1))
          ;; Later, its pane is there.
          (setq panes (concat panes "gh-1-c\t50000\n")
                ghostherd-tmux--pane-pids-at 0)
          (should (= (ghostherd--host-pid c) 50000))
          (should (= (ghostherd--host-pid a) 24671))
          (should (= calls 2))
          (should (equal (mapcar (lambda (n) (plist-get (funcall row n) :hostPid)) '("a" "b" "c"))
                         '(24671 41426 50000))))
        ;; ghostel: the shell ghostel keeps the pid of, while its process lives.
        (let* ((proc (start-process "gh-host-test" nil "sleep" "30"))
               (g (ghostherd-tests--session :name "g" :kind 'claude :backend 'ghostel)))
          (unwind-protect
              (progn
                (with-current-buffer (ghostherd-session-buffer g)
                  (setq-local ghostel--pid 4242)
                  (setq-local ghostel--process proc))
                (should (= (ghostherd--host-pid g) 4242))
                (should (= (plist-get (funcall row "g") :hostPid) 4242))
                (delete-process proc)
                (should-not (ghostherd--host-pid g)))
            (when (process-live-p proc) (delete-process proc))))
        ;; A backend that cannot say: 0.
        (ghostherd-tests--session :name "f" :kind 'claude :backend 'fake)
        (should (= (plist-get (funcall row "f") :hostPid) 0))
        (should (= calls 2))))))

;;; Kept denied, expired, cleared (ask 6c009f2f)

(ert-deftest ghostherd-test-a-tick-says-how-long-a-guard-request-waits ()
  (ghostherd-tests--with-herd ((ghostherd-guard-request-ttl 1234) (ghostherd--herd-tick-inflight nil))
    (let (sent)
      (cl-letf (((symbol-function 'ghostherd-memory-rpc-url) (lambda () "http://127.0.0.1:1/jsonrpc/x"))
                ((symbol-function 'ghostherd-memory-request-async)
                 (lambda (method _ok params &rest _) (when (equal method "herd_tick") (setq sent params)))))
        (ghostherd--herd-tick-async)
        (should (eql (plist-get sent :guard_ttl) 1234))))))

(ert-deftest ghostherd-test-a-respawn-clears-its-guard-requests ()
  "A new process under a name: the sidecar is told its open guard requests
expire, and its rebuilt guard keeps none of the old allowances."
  (ghostherd-tests--with-guard
    (let ((asked nil)
          (dir (ghostherd--guard-dir "agy-p")))
      (make-directory dir t)
      (write-region "{\"approvals\": [{\"id\": \"old\"}]}" nil (concat dir "approved.json"))
      (cl-letf (((symbol-function 'ghostherd-memory-rpc-url) (lambda () "http://127.0.0.1:1/jsonrpc/x"))
                ((symbol-function 'ghostherd-memory-request-async)
                 (lambda (method _ok params &rest _) (push (cons method params) asked))))
        (ghostherd-spawn 'agy :name "agy-p" :project repo :directory repo :backend 'fake :display nil)
        (ghostherd-spawn 'claude :name "claude-p" :project repo :directory repo :backend 'fake :display nil))
      (should (member '("herd_guard_clear" :session "agy-p") asked))
      (should (member '("herd_guard_clear" :session "claude-p") asked))
      (should-not (file-exists-p (concat dir "approved.json")))
      (should (file-exists-p (concat dir "policy.json"))))))

(ert-deftest ghostherd-test-kept-denied-and-expired-are-logged ()
  (ghostherd-tests--with-herd ((ghostherd--log nil))
    (ghostherd--herd-take-guard (list (list :id "g1" :session "agy-p" :status "declined" :summary "run_command: cat .env")
                                      (list :id "g2" :session "agy-p" :status "expired" :summary "run_command: git -C x grep holy")))
    (should (equal (mapcar #'ghostherd-log-entry-text (reverse ghostherd--log))
                   '("guard g1 kept denied by the user: run_command: cat .env"
                     "guard g2 expired, never answered: run_command: git -C x grep holy")))
    (should (ghostherd-tests--issue-p "expired, never answered"))
    (should-not (ghostherd-tests--issue-p "kept denied by the user"))))

(ert-deftest ghostherd-test-the-cards-keep-denied-reaches-the-herd ()
  "Keep denied on the card (or the Inbox) asks the sidecar to reject that
request, by its id; anything not an id goes nowhere."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let (asked shown)
    (cl-letf (((symbol-function 'ghostherd-memory-request-async)
               (lambda (method ok params &rest _) (push (list method params) asked) (funcall ok nil)))
              ((symbol-function 'ghostherd-memory-page--js) (lambda (&rest args) (push args shown))))
      (ghostherd-memory-page--handle '((op . "guard-reject") (id . "1a2b3c4d")))
      (should (equal asked '(("herd_guard_reject" (:id "1a2b3c4d")))))
      (should (equal (car shown) '("flash" "Kept denied; the agent is told not to try it again")))
      (dolist (bad '("../x" "" nil "1a2b3c4d; rm"))
        (ghostherd-memory-page--handle `((op . "guard-reject") (id . ,bad))))
      (should (= (length asked) 1)))))

(provide 'ghostherd-hooks-tests)
;;; ghostherd-hooks-tests.el ends here

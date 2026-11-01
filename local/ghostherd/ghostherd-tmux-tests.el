;;; ghostherd-tmux-tests.el --- Tests for ghostherd-tmux.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Part of ghostherd's tests; ghostherd-tests.el runs them all.  This
;; file alone:
;;
;;   emacs --batch --init-directory=~/.emacs.d/emacs-home/ \
;;         -L . -L ../xwapp -l ghostherd-tmux-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ghostherd-test-helpers)

;;; tmux naming and targets

(ert-deftest ghostherd-test-tmux-name-has-no-target-punctuation ()
  "tmux parses a target as `session:window.pane', so a colon or a dot in
a session name is a bug rather than a style problem."
  (let ((name (ghostherd-tmux-session-name "/tmp/p/" "rev:iew.er 2")))
    (should-not (string-match-p "[:.]" name))
    (should (string-prefix-p "gh-" name))
    (should (string-suffix-p "-rev_iew_er_2" name))))

(ert-deftest ghostherd-test-tmux-name-separates-projects ()
  "Two agents called `reviewer' in two projects share one socket."
  (should-not (equal (ghostherd-tmux-session-name "/tmp/a/" "rev")
                     (ghostherd-tmux-session-name "/tmp/b/" "rev")))
  ;; ...and the same project must hash the same across Emacs restarts,
  ;; which is what restore matches on.
  (should (equal (ghostherd-tmux-session-name "/tmp/a/" "rev")
                 (ghostherd-tmux-session-name "/tmp/a" "rev"))))

(ert-deftest ghostherd-test-tmux-name-survives-an-empty-name ()
  "A name with nothing usable in it still has to produce a target."
  (should (string-suffix-p "-agent" (ghostherd-tmux-session-name nil "")))
  (should-not (string-match-p "[:.]" (ghostherd-tmux-session-name nil "..."))))

(ert-deftest ghostherd-test-tmux-target-is-exact ()
  "Without the `=' tmux prefix-matches, so `gh-abc-rev' silently drives
`gh-abc-reviewer'; without the trailing colon the commands that parse a
pane target resolve a bare name against the current session instead."
  (should (equal (ghostherd-tmux--target "gh-abc-rev") "=gh-abc-rev:"))
  (should-error (ghostherd-tmux--target "gh-abc-rev:iew")))

;;; tmux server config

(ert-deftest ghostherd-test-tmux-config-is-generated-and-absolute ()
  "Generated rather than shipped, because a lone .conf beside the source
is not on any package manager's default file list -- and the resulting
default `remain-on-exit off' fails a long way from its cause.

Absolute, because `locate-user-emacs-file' returns \"~/.emacs.d/...\",
Emacs expands a tilde in a file name and tmux does not: it would take
the config as missing and start with its own defaults, silently."
  (let* ((ghostherd-tmux-config nil)
         (ghostherd-tmux--generated-config nil)
         (user-emacs-directory (file-name-as-directory
                                (make-temp-file "ghostherd-cfg" t)))
         (file (ghostherd-tmux--config-file)))
    (unwind-protect
        (progn
          (should file)
          (should (file-name-absolute-p file))
          (should-not (string-prefix-p "~" file))
          (should (equal (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))
                         ghostherd-tmux-config-text))
          ;; and it is on every invocation, since only the process that
          ;; starts the server reads it
          (should (member "-f" (ghostherd-tmux--socket-args))))
      (delete-directory user-emacs-directory t))))

(ert-deftest ghostherd-test-tmux-config-declares-what-the-code-assumes ()
  "Three options the Elisp is written against.  If the config stops
setting them the failures are remote: sessions vanish when an agent
exits, the pane loses a row to a status line the moment a client
attaches, and Escape lags."
  (dolist (line '("set -g remain-on-exit on"
                  "set -g status off"
                  "set -g prefix None"))
    (should (string-match-p (regexp-quote line) ghostherd-tmux-config-text))))

;;; tmux command construction
;;
;; `ghostherd-tmux--call' is the single seam every tmux invocation passes
;; through, so stubbing it pins the actual command lines -- which is
;; where this backend's mistakes live.

(ert-deftest ghostherd-test-tmux-capture-takes-the-visible-pane ()
  "No -e (rules match text, not SGR), no -J (joining wrapped lines
invents rows the agent never drew), and no scrollback (an alt-screen
TUI does not keep its UI in history, so history is where you match a
permission prompt that scrolled away and is no longer real)."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--tmux-session :name "a")))
      (ghostherd-tests--with-tmux '(("capture-pane" . (0 . "hello\n")))
        (should (equal (ghostherd--host-capture s) "hello")))
      (let ((call (ghostherd-tests--tmux-call "capture-pane")))
        (should (equal call '("capture-pane" "-p" "-t" "=gh-abc-rev:")))
        (should-not (member "-e" call))
        (should-not (member "-J" call))
        (should-not (member "-S" call))))))

(ert-deftest ghostherd-test-tmux-paste-does-not-submit-each-line ()
  "paste-buffer turns every linefeed into a carriage return by default,
which submits each line of a multi-line prompt separately.  -r keeps
them as newlines; -p brackets the paste when the agent asked for it."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--tmux-session :name "a")))
      (ghostherd-tests--with-tmux nil
        (ghostherd--host-send-text s "alpha\nbeta" nil))
      ;; Loaded over stdin, not as a `set-buffer' argument, so a long or CJK
      ;; prompt does not overrun tmux's command-length cap.
      (should (equal (ghostherd-tests--tmux-call "load-buffer")
                     '("load-buffer" "-b" "ghostherd-paste" "-" "alpha\nbeta")))
      (should-not (ghostherd-tests--tmux-call "set-buffer"))
      (let ((paste (ghostherd-tests--tmux-call "paste-buffer")))
        (should (member "-r" paste))
        (should (member "-p" paste))
        (should (member "=gh-abc-rev:" paste)))
      (should-not (ghostherd-tests--tmux-call "send-keys")))))

(ert-deftest ghostherd-test-tmux-load-buffer-carries-a-huge-paste ()
  "A long prompt -- or a shorter CJK one, three bytes a character -- loads
over stdin, so it does not overrun tmux's ~16 KB command-length cap the
way `set-buffer -- TEXT' does: 120 KB of mixed CJK and ASCII with
newlines round-trips byte-identical through a real scratch tmux buffer.
The whole send path gets this: card prompt, chat composer, ask delivery
and relay brief all reach it through `ghostherd-backend-send-text'."
  (skip-unless (ghostherd-tmux-available-p))
  (let* ((ghostherd-tmux-socket (format "ghostherd-test-%d" (emacs-pid)))
         (ghostherd-tmux--status-cache nil)
         (line "你好，世界 — hello world, 123 测試 with a newline\n")
         (text (apply #'concat
                      (make-list (1+ (/ (* 120 1024) (string-bytes line))) line))))
    (should (> (string-bytes text) (* 120 1024)))
    (unwind-protect
        (progn
          (ghostherd-tmux--run "new-session" "-d" "-s" "t" "-x" "80" "-y" "24")
          (ghostherd-tmux--load-buffer "ghostherd-paste-test" text)
          (should (equal (ghostherd-tmux--run "show-buffer" "-b" "ghostherd-paste-test")
                         text)))
      (ignore-errors (ghostherd-tmux--run "kill-server")))))

(ert-deftest ghostherd-test-tmux-submit-presses-enter ()
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--tmux-session :name "a")))
      (ghostherd-tests--with-tmux nil
        (ghostherd--host-send-text s "hi" t))
      (should (equal (ghostherd-tests--tmux-call "send-keys")
                     '("send-keys" "-t" "=gh-abc-rev:" "Enter"))))))

(ert-deftest ghostherd-test-tmux-key-names ()
  "The friendly names are the contract; each backend translates them
into its own vocabulary, and anything unlisted passes through so tmux's
full key set stays reachable."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--tmux-session :name "a")))
      (ghostherd-tests--with-tmux nil
        (ghostherd--host-send-keys s '("esc" "C-c" "down" "return" "F5")))
      (should (equal (ghostherd-tests--tmux-call "send-keys")
                     '("send-keys" "-t" "=gh-abc-rev:"
                       "Escape" "C-c" "Down" "Enter" "F5"))))))

(ert-deftest ghostherd-test-tmux-send-keys-still-does-not-claim-working ()
  "Same contract on either host: sending Escape means stop."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--tmux-session :name "a" :state 'blocked)))
      (ghostherd-tests--with-tmux nil
        (ghostherd-send-keys s "esc"))
      (should (eq (ghostherd-session-state s) 'blocked)))))

(ert-deftest ghostherd-test-tmux-spawn-execs-directly ()
  "No shell, so no `ghostherd-spawn-delay' guesswork and no quoting:
tmux hands multiple trailing arguments to execvp as they are."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-tmux '(("has-session" . (1 . "")))
      (let ((spawned (ghostherd-backend-spawn
                      'tmux
                      (list :name "rev" :kind 'agy :command "agy"
                            :args '("--effort" "high" "a b")
                            :directory "/tmp/" :project "/tmp/"
                            :notes "n"))))
        (should (equal (plist-get spawned :host-id)
                       (ghostherd-tmux-session-name "/tmp/" "rev")))
        ;; Detached: spawning is not visiting.
        (should-not (plist-get spawned :buffer))))
    (let ((call (ghostherd-tests--tmux-call "new-session")))
      (should (member "-d" call))
      (should (equal (last call 4) '("agy" "--effort" "high" "a b"))))))

(ert-deftest ghostherd-test-tmux-spawn-injects-identity ()
  "`-e' before the command, and the command still last: everything after
it belongs to execvp."
  (ghostherd-tests--with-herd ()
    (let ((ghostherd-tmux--env-supported 'unknown))
      (ghostherd-tests--with-tmux '(("has-session" . (1 . "")))
        (ghostherd-backend-spawn
         'tmux (list :name "rev" :kind 'agy :command "agy" :args '("--effort")
                     :directory "/tmp/" :project "/tmp/"))))
    (let ((call (ghostherd-tests--tmux-call "new-session")))
      (should (member "GHOSTHERD_SESSION=rev" call))
      (should (member "GHOSTHERD_BACKEND=tmux" call))
      (should (equal (last call 2) '("agy" "--effort")))
      ;; -e has to come before the command, or tmux hands it to the agent.
      (should (< (cl-position "-e" call :test #'equal)
                 (cl-position "agy" call :test #'equal))))))

(ert-deftest ghostherd-test-tmux-spawn-retries-without-identity ()
  "`-e' is tmux 3.2+.  The version is not parsed -- `tmux -V' says things
like \"next-3.6\" -- so the spawn is its own probe: an old tmux gets one
failed call and then the herd it always had, minus identity."
  (ghostherd-tests--with-herd ()
    (let ((ghostherd-tmux--env-supported 'unknown)
          (calls nil))
      (cl-letf (((symbol-function 'ghostherd-tmux--call)
                 (lambda (args)
                   (push args calls)
                   (cond
                    ;; Failure means "no such session"; answering 0 here
                    ;; tells `--unique-id' every name is taken, and it
                    ;; looks for a free one forever.
                    ((equal (car args) "has-session") (cons 1 ""))
                    ((member "-e" args) (cons 1 "unknown option -- e"))
                    (t (cons 0 ""))))))
        (should (ghostherd-backend-spawn
                 'tmux (list :name "rev" :kind 'agy :command "agy"
                             :directory "/tmp/" :project "/tmp/")))
        ;; And the answer is remembered: the next spawn does not pay for it.
        (should (eq ghostherd-tmux--env-supported nil))
        (setq calls nil)
        (ghostherd-backend-spawn
         'tmux (list :name "rev2" :kind 'agy :command "agy"
                     :directory "/tmp/" :project "/tmp/"))
        (should-not (cl-find-if (lambda (args) (member "-e" args)) calls))))))

(ert-deftest ghostherd-test-tmux-spawn-failure-is-still-an-error ()
  "The retry must not swallow a spawn that failed for a real reason."
  (ghostherd-tests--with-herd ()
    (let ((ghostherd-tmux--env-supported 'unknown))
      (cl-letf (((symbol-function 'ghostherd-tmux--call)
                 (lambda (_args) (cons 1 "no space left on device"))))
        (should-error (ghostherd-backend-spawn
                       'tmux (list :name "rev" :kind 'agy :command "agy"
                                   :directory "/tmp/" :project "/tmp/"))
                      :type 'user-error)))))

(ert-deftest ghostherd-test-tmux-spawn-writes-the-recipe ()
  "The tmux session name is not big enough to hold a recipe, and a
sidecar file can be deleted while the process it describes is running."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-tmux '(("has-session" . (1 . "")))
      (ghostherd-backend-spawn
       'tmux (list :name "rev" :kind 'agy :command "agy"
                   :args '("--effort" "high") :directory "/tmp/"
                   :project "/tmp/" :notes "reviews auth")))
    (let ((written (ghostherd-tests--tmux-options)))
      (pcase-dolist (`(,option . ,value)
                     '(("@ghostherd-name"  . "rev")
                       ("@ghostherd-kind"  . "agy")
                       ("@ghostherd-notes" . "reviews auth")
                       ("@ghostherd-args"  . "[\"--effort\",\"high\"]")))
        (should (equal (alist-get option written nil nil #'equal) value)))
      ;; Every set-option must address the session exactly, or a name
      ;; that is a prefix of another quietly gets someone else's recipe.
      (let ((target (ghostherd-tmux--target
                     (ghostherd-tmux-session-name "/tmp/" "rev"))))
        (dolist (call ghostherd-tests--tmux-calls)
          (when (equal (car call) "set-option")
            (should (equal (nth 2 call) target))))))))

(ert-deftest ghostherd-test-tmux-client-runs-without-the-withheld ()
  "The client that starts the server hands it its own environment, and the
server hands that to every pane: no tmux client of ours has a withheld
name."
  (let* ((fake (make-temp-file "gh-fake-tmux"))
         (ghostherd-tmux-executable fake)
         (ghostherd-tmux-config "/dev/null")
         (ghostherd-agent-env-withhold '("GHOSTHERD_TELEGRAM_TOKEN" "GHOSTHERD_TELEGRAM_CHAT_IDS"))
         (process-environment (append ghostherd-tests--bot-env process-environment)))
    (unwind-protect
        (progn
          (with-temp-file fake (insert "#!/bin/sh\nenv\n"))
          (set-file-modes fake #o755)
          (let ((env (split-string (cdr (ghostherd-tmux--call '("new-session"))) "\n" t)))
            (should (ghostherd-tests--bot-free-p env))
            (should (member "GITHUB_TOKEN=fake-gh" env))))
      (delete-file fake))))

(ert-deftest ghostherd-test-tmux-spawn-unsets-the-withheld-in-the-server ()
  "A server keeps the environment it started with for life: one started
before the names were withheld still has them.  Each spawn unsets them
there first, in one call."
  (ghostherd-tests--with-herd ()
    (let ((ghostherd-agent-env-withhold '("GHOSTHERD_TELEGRAM_TOKEN" "GHOSTHERD_TELEGRAM_CHAT_IDS")))
      (ghostherd-tests--with-tmux '(("has-session" . (1 . "")))
        (ghostherd-backend-spawn
         'tmux (list :name "rev" :kind 'agy :command "agy"
                     :directory "/tmp/" :project "/tmp/")))))
  (should (equal (ghostherd-tests--tmux-call "set-environment")
                 '("set-environment" "-g" "-u" "GHOSTHERD_TELEGRAM_TOKEN" ";"
                   "set-environment" "-g" "-u" "GHOSTHERD_TELEGRAM_CHAT_IDS")))
  (should (< (cl-position "set-environment" ghostherd-tests--tmux-calls :key #'car :test #'equal)
             (cl-position "new-session" ghostherd-tests--tmux-calls :key #'car :test #'equal))))

(ert-deftest ghostherd-test-tmux-spawn-with-nothing-withheld-unsets-nothing ()
  (ghostherd-tests--with-herd ()
    (let ((ghostherd-agent-env-withhold nil))
      (ghostherd-tests--with-tmux '(("has-session" . (1 . "")))
        (ghostherd-backend-spawn
         'tmux (list :name "rev" :kind 'agy :command "agy"
                     :directory "/tmp/" :project "/tmp/")))))
  (should-not (ghostherd-tests--tmux-call "set-environment"))
  (should (ghostherd-tests--tmux-call "new-session")))

(ert-deftest ghostherd-test-tmux-agent-never-sees-the-withheld ()
  "Against a real tmux: a server started by a client that had the bot's
variables -- as the herd's was, from Emacs's environment -- spawns an
agent without them, a token agents work with still there, and keeps
them no more."
  (skip-unless (ghostherd-tmux-available-p))
  (ghostherd-tests--with-herd ()
    (let* ((ghostherd-tmux-socket (format "ghostherd-test-withhold-%d" (emacs-pid)))
           (ghostherd-tmux-config "/dev/null")
           (ghostherd-tmux--status-cache nil)
           (ghostherd-tmux--env-supported 'unknown)
           (ghostherd-agent-env-withhold '("GHOSTHERD_TELEGRAM_TOKEN" "GHOSTHERD_TELEGRAM_CHAT_IDS"))
           (process-environment (append ghostherd-tests--bot-env process-environment))
           (out (make-temp-file "gh-withhold-env"))
           (part (concat out ".part")))
      (unwind-protect
          (progn
            (should (eq 0 (call-process ghostherd-tmux-executable nil nil nil
                                        "-L" ghostherd-tmux-socket "-f" "/dev/null"
                                        "new-session" "-d" "-s" "before" "sleep" "60")))
            (should (string-match-p "^GHOSTHERD_TELEGRAM_TOKEN="
                                    (ghostherd-tmux--run "show-environment" "-g")))
            (ghostherd-backend-spawn
             'tmux (list :name "probe" :kind 'shell :command "sh"
                         :args (list "-c" (format "env > %s && mv %s %s; sleep 60"
                                                  (shell-quote-argument part)
                                                  (shell-quote-argument part)
                                                  (shell-quote-argument out)))
                         :directory temporary-file-directory
                         :project temporary-file-directory))
            (let ((deadline (+ (float-time) 5)))
              (while (and (zerop (file-attribute-size (file-attributes out)))
                          (< (float-time) deadline))
                (sleep-for 0.05)))
            (let ((env (with-temp-buffer
                         (insert-file-contents out)
                         (split-string (buffer-string) "\n" t))))
              (should (ghostherd-tests--bot-free-p env))
              (should (member "GITHUB_TOKEN=fake-gh" env))
              (should (member "GHOSTHERD_SESSION=probe" env)))
            (should-not (string-match-p "^GHOSTHERD_TELEGRAM_"
                                        (ghostherd-tmux--run "show-environment" "-g"))))
        (call-process ghostherd-tmux-executable nil nil nil
                      "-L" ghostherd-tmux-socket "kill-server")
        (ignore-errors (delete-file part))
        (delete-file out)))))

(ert-deftest ghostherd-test-tmux-args-round-trip-through-json ()
  "Arguments are a list of free text: spaces, quotes and backslashes all
occur, and none of them can go into one tmux option value untouched."
  (dolist (args '(nil ("--model" "x y") ("--system-prompt" "say \"hi\"\\n")))
    (should (equal (ghostherd-tmux--decode
                    :args (ghostherd-tmux--encode :args args))
                   args)))
  (should (equal (ghostherd-tmux--decode :kind (ghostherd-tmux--encode :kind 'agy))
                 'agy))
  ;; An unset option must not become the symbol nil or the empty string.
  (should-not (ghostherd-tmux--decode :notes nil))
  (should-not (ghostherd-tmux--decode :kind "")))

(ert-deftest ghostherd-test-tmux-list-only-claims-its-own ()
  "The socket is ghostherd's, but a stray session on it is not: only the
`gh-' prefix marks one this package made."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-tmux
        '(("list-sessions" . (0 . "gh-abc-rev\nscratch\ngh-abc-imp\n")))
      (cl-letf (((symbol-function 'ghostherd-tmux--read-recipe)
                 (lambda (id) (list :name (concat "n:" id)))))
        (let ((listed (ghostherd-backend-list 'tmux)))
          (should (equal (mapcar (lambda (r) (plist-get r :host-id)) listed)
                         '("gh-abc-rev" "gh-abc-imp"))))))))

(ert-deftest ghostherd-test-tmux-live-p-uses-one-snapshot ()
  "Liveness is asked far more often than it changes -- every poll and
every mode-line redisplay -- so one `list-panes' has to answer for the
whole herd rather than costing a subprocess per session."
  (ghostherd-tests--with-herd ()
    (let ((a (ghostherd-tests--session :name "a" :backend 'tmux
                                       :host-id "gh-abc-a"))
          (b (ghostherd-tests--session :name "b" :backend 'tmux
                                       :host-id "gh-abc-b"))
          (c (ghostherd-tests--session :name "c" :backend 'tmux
                                       :host-id "gh-abc-c"))
          (ghostherd-tmux--last-words nil))
      (ghostherd-tests--with-tmux
          '(("list-panes" . (0 . "gh-abc-a\t0\t\tclaude\ngh-abc-b\t1\t1\t\ngh-abc-c\t1\t\t\n")))
        (should (ghostherd--host-live-p a))
        (should (ghostherd--host-live-p b))
        (should-not (ghostherd--host-exited-p a))
        ;; `remain-on-exit' keeps a dead pane capturable: still live, but
        ;; the agent behind it is gone -- with its status, when tmux has
        ;; one; killed by a signal, it has none.
        (should (equal (ghostherd--host-exited-p b) "exited 1"))
        (should (equal (ghostherd--host-exited-p c) "process exited"))
        (should (equal (ghostherd--host-title a) "claude")))
      (should (= (cl-count "list-panes" ghostherd-tests--tmux-calls
                           :key #'car :test #'equal)
                 1)))))

(ert-deftest ghostherd-test-tmux-unknown-host-is-not-live ()
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :backend 'tmux
                                       :host-id "gh-abc-gone")))
      (ghostherd-tests--with-tmux '(("list-panes" . (0 . "gh-abc-other\t0\t\t\n")))
        (should-not (ghostherd--host-live-p s))
        (should (equal (car (ghostherd--detect-state s)) 'dead))))))

(ert-deftest ghostherd-test-tmux-starts-from-a-directory-that-is-there ()
  "A timer runs in whatever buffer is current.  One whose directory was
deleted -- a dired buffer on an unpacked tarball, the folder then
removed -- made `call-process' refuse to start tmux, that read as no
tmux, and the whole herd was dropped, every agent still running."
  (let ((gone (file-name-as-directory (make-temp-file "ghostherd-gone" t)))
        (ghostherd-tmux-executable "true")
        (ghostherd-tmux-config "/dev/null")
        (ghostherd-tmux--fetch nil)
        (ghostherd-tmux--status-cache nil)
        fetched styled)
    (delete-directory gone)
    (ghostherd-tests--with-herd ()
      (let ((s (ghostherd-tests--session :name "a" :backend 'tmux
                                         :host-id "gh-abc-a"))
            (default-directory gone))
        (should (equal (ghostherd-tmux--call '("list-panes")) '(0 . "")))
        ;; The poll's fetch and a styled capture start tmux too.
        (should (ghostherd-backend-screens 'tmux (list s) 10
                                           (lambda (_) (setq fetched t))))
        (ghostherd-backend-styled-screen 'tmux s (lambda (out) (setq styled out)))
        (with-timeout (5 (ert-fail "tmux never answered"))
          (while (not (and fetched styled))
            (accept-process-output nil 0.05)))
        (should (equal styled ""))))))

(ert-deftest ghostherd-test-tmux-missing-binary-is-a-dead-host ()
  "A machine without tmux must not break the ghostel backend, so a
missing binary reports a gone host rather than signalling."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :backend 'tmux
                                       :host-id "gh-abc-a"))
          (ghostherd-tmux--status-cache nil)
          (ghostherd-tmux-executable "ghostherd-no-such-tmux"))
      (should-not (ghostherd--host-live-p s))
      (should-not (ghostherd-tmux-available-p)))))

;;; One round trip for the whole herd

(ert-deftest ghostherd-test-screens-args-put-a-marker-before-each-pane ()
  "The status fields ride along in front, then one delimiter per pane --
so a single invocation answers liveness, titles and every screen."
  (let* ((marker "@@m@@")
         (args (ghostherd-tmux--screens-args '("gh-a" "gh-b") marker)))
    (should (equal (seq-take args 4)
                   (list "list-panes" "-a" "-F"
                         ghostherd-tmux--status-format)))
    (should (equal (nthcdr 4 args)
                   (list ";" "display-message" "-p" "@@m@@ gh-a"
                         ";" "if-shell" "-F" "-t" "=gh-a:" "#{pane_dead}"
                         "capture-pane -p -J -S -50 -t =gh-a:" "capture-pane -p -t =gh-a:"
                         ";" "display-message" "-p" "@@m@@ gh-b"
                         ";" "if-shell" "-F" "-t" "=gh-b:" "#{pane_dead}"
                         "capture-pane -p -J -S -50 -t =gh-b:" "capture-pane -p -t =gh-b:")))))

(ert-deftest ghostherd-test-screens-marker-is-not-guessable ()
  "The text being delimited is whatever an agent chose to print, and an
agent can print anything -- including a fixed \"unlikely\" delimiter."
  (should-not (equal (ghostherd-tmux--marker) (ghostherd-tmux--marker))))

(ert-deftest ghostherd-test-parse-screens-splits-status-from-panes ()
  (pcase-let ((`(,status . ,screens)
               (ghostherd-tmux--parse-screens
                (concat "gh-a\t0\t\ttitle a\ngh-b\t0\t\ttitle b\n"
                        "@@m@@ gh-a\nAAA\nAAA2\n"
                        "@@m@@ gh-b\nBBB\n")
                "@@m@@")))
    (should (equal status "gh-a\t0\t\ttitle a\ngh-b\t0\t\ttitle b"))
    (should (equal (alist-get "gh-a" screens nil nil #'equal) "AAA\nAAA2"))
    (should (equal (alist-get "gh-b" screens nil nil #'equal) "BBB"))))

(ert-deftest ghostherd-test-parse-screens-tolerates-an-abandoned-list ()
  "tmux abandons the rest of a command list at the first failure, which a
pane killed from outside is enough to cause.  What came back is used; the
pane whose marker printed with nothing after it is *not* reported as an
empty screen, because an empty screen would read as `idle'."
  (pcase-let ((`(,_status . ,screens)
               (ghostherd-tmux--parse-screens
                (concat "gh-a\t0\t\tt\n"
                        "@@m@@ gh-a\nAAA\n"
                        "@@m@@ gh-gone\n")
                "@@m@@")))
    (should (equal (mapcar #'car screens) '("gh-a")))))

(ert-deftest ghostherd-test-fetch-stamps-the-status-cache ()
  "Liveness came back in the same round trip, so asking for it again --
synchronously, on the timer -- is the call this whole path removes."
  (ghostherd-tests--with-herd ()
    (let ((ghostherd-tmux--status-cache nil)
          (ghostherd-tmux--fetch nil)
          (got 'unset)
          (buffer (generate-new-buffer " *fetch-test*")))
      (with-current-buffer buffer
        (insert "gh-abc-rev\t0\t\tagy\n@@m@@ gh-abc-rev\nThinking…\n"))
      (ghostherd-tmux--fetch-done buffer "@@m@@" '(("gh-abc-rev" . "rev")) 40
                                  (lambda (screens) (setq got screens)))
      (should (equal got '(("rev" . "Thinking…"))))
      (should (equal (ghostherd-tmux--parse-snapshot "gh-abc-rev\t0\t\tagy")
                     (cdr ghostherd-tmux--status-cache)))
      ;; Fresh by definition: it was stamped just now, so no reader on this
      ;; tick pays for a second list-panes.
      (should (assoc "gh-abc-rev" (ghostherd-tmux--snapshot))))))

(ert-deftest ghostherd-test-status-says-how-a-pane-ended ()
  "Running, nil; exited, its status; killed by a signal, t -- tmux has no
status to give.  The title, last, may hold the separator."
  (should (equal (ghostherd-tmux--parse-snapshot
                  "gh-a\t0\t\tclaude\ngh-b\t1\t1\ta\ttitle\ngh-c\t1\t\t\n")
                 '(("gh-a" nil . "claude") ("gh-b" 1 . "a\ttitle") ("gh-c" t . "")))))

(ert-deftest ghostherd-test-last-words-are-the-last-line-that-says-something ()
  "What a CLI said on its way out: blank lines, a box's edges and the
host's own line about the exit say nothing."
  (should (equal (ghostherd--last-words
                  (concat "first\nNo conversation found to continue\n\n  ╰────╯  \n\n"
                          "Pane is dead (status 1, Wed Oct 14 13:10:32 2026)\n")
                  ghostherd-tmux--dead-line)
                 "No conversation found to continue"))
  (should (equal (ghostherd--last-words "bye\n\n[Process exited]\n"
                                        ghostherd--ghostel-exit-line)
                 "bye"))
  (should-not (ghostherd--last-words "\n  \n─────\n"))
  (should-not (ghostherd--last-words nil))
  (let ((long (ghostherd--last-words (make-string 400 ?x))))
    (should (= (string-width long) ghostherd--last-words-width))
    (should (string-suffix-p "…" long))))

(ert-deftest ghostherd-test-exit-reason-says-status-and-words ()
  (should (equal (ghostherd--exit-reason 1 "No conversation found to continue")
                 "exited 1: No conversation found to continue"))
  (should (equal (ghostherd--exit-reason 0 nil) "exited 0"))
  (should (equal (ghostherd--exit-reason t "Killed") "process exited: Killed"))
  (should (equal (ghostherd--exit-reason nil nil) "process exited")))

(ert-deftest ghostherd-test-a-dead-pane-says-why ()
  "`claude --continue' in a folder with no conversation said so and
exited 1, the line one above the top of its screen; the herd said only
\"process exited\" (2026-10-14).  The tick that finds a pane dead reads
what its CLI said last -- past the screen the rules are given -- and
the reason and the log carry it."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (let ((dead (ghostherd-tests--session :name "be" :kind 'claude :state 'starting
                                            :backend 'tmux :host-id "gh-abc-be"))
            (live (ghostherd-tests--session :name "ops" :kind 'claude :state 'idle
                                            :backend 'tmux :host-id "gh-abc-ops"))
            (ghostherd-tmux--last-words '(("gh-abc-gone" . "from a pane since killed")))
            (buffer (generate-new-buffer " *fetch-test*"))
            got)
        (with-current-buffer buffer
          (insert "gh-abc-be\t1\t1\t\ngh-abc-ops\t0\t\tclaude\n"
                  "@@m@@ gh-abc-be\nNo conversation found to continue\n"
                  (make-string 50 ?\n)
                  "Pane is dead (status 1, Wed Oct 14 13:10:32 2026)\n"
                  "@@m@@ gh-abc-ops\nthe agent at work\n> \n"))
        (ghostherd-tests--with-tmux nil
          (ghostherd-tmux--fetch-done buffer "@@m@@"
                                      '(("gh-abc-be" . "be") ("gh-abc-ops" . "ops")) 40
                                      (lambda (screens) (setq got screens)))
          (should-not (string-match-p "No conversation" (alist-get "be" got nil nil #'equal)))
          (should (equal ghostherd-tmux--last-words
                         '(("gh-abc-be" . "No conversation found to continue"))))
          (should (eq (ghostherd-poll-session dead (alist-get "be" got nil nil #'equal)) 'dead))
          (should (equal (ghostherd-session-state-reason dead)
                         "exited 1: No conversation found to continue"))
          (should (string-match-p "No conversation found to continue"
                                  (ghostherd-log-entry-text (car ghostherd--log))))
          ;; A live pane's last line is not taken for last words.
          (should-not (ghostherd--host-exited-p live)))))))

(ert-deftest ghostherd-test-ghostel-dead-agent-says-why ()
  "The same on ghostel: the buffer's last words, under its own line about
the exit.  Its status only from a process of its own: the native PTY's
handle is a pipe, whose status says nothing about the child."
  (ghostherd-tests--with-herd ()
    (let* ((s (ghostherd-tests--session :name "g" :kind 'claude :backend 'ghostel))
           (buffer (ghostherd-session-buffer s))
           (real (make-process :name "ghostherd-exit-test" :noquery t
                               :command '("sh" "-c" "exit 2")))
           (pipe (make-pipe-process :name "ghostherd-exit-pipe" :noquery t)))
      (with-timeout (5 (ert-fail "the process never exited"))
        (while (process-live-p real) (accept-process-output nil 0.05)))
      (delete-process pipe)
      (with-current-buffer buffer
        (insert "No conversation found to continue\n\n[Process exited]\n")
        (setq-local ghostel--process real))
      (should (equal (ghostherd--host-exited-p s)
                     "exited 2: No conversation found to continue"))
      (with-current-buffer buffer (setq-local ghostel--process pipe))
      (should (equal (ghostherd--host-exited-p s)
                     "process exited: No conversation found to continue")))))

(ert-deftest ghostherd-test-fetch-does-not-stack ()
  "A second fetch against a slow server is how a pile-up starts."
  (ghostherd-tests--with-herd ()
    (let* ((s (ghostherd-tests--tmux-session :name "rev"))
           (started 0)
           (fake (start-process "ghostherd-fetch-stub" nil "sleep" "30")))
      (unwind-protect
          (let ((ghostherd-tmux--fetch fake)
                (ghostherd-tmux--fetch-started (float-time)))
            (cl-letf (((symbol-function 'make-process)
                       (lambda (&rest _) (setq started (1+ started)) fake)))
              (should (ghostherd-backend-screens 'tmux (list s) 40 #'ignore)))
            (should (= started 0)))
        (delete-process fake)))))

(ert-deftest ghostherd-test-poll-uses-the-screen-it-was-given ()
  "The whole point: the poll path reads each screen once, in a batch, and
detection must not go back to the host for what it already has."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy :state 'idle)))
      (cl-letf (((symbol-function 'ghostherd--host-capture)
                 (lambda (&rest _) (error "captured again"))))
        (should (eq (ghostherd-poll-session s "Thinking…\n") 'working))))))

(ert-deftest ghostherd-test-poll-all-falls-back-per-session ()
  "A host whose screen is already an Emacs buffer says nothing here, and
gets read session by session -- which costs nothing there."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy :state 'idle
                                       :backend 'fake))
          (ghostherd-tests--fake-screen "Thinking…\n"))
      (ghostherd-poll-all)
      (should (eq (ghostherd-session-state s) 'working)))))

(ert-deftest ghostherd-test-poll-all-groups-by-backend ()
  "One round trip is a claim only a single host can make, and a herd may
straddle two."
  (ghostherd-tests--with-herd ()
    (let ((tmux-session (ghostherd-tests--tmux-session :name "t" :kind 'agy
                                                       :state 'idle))
          (fake-session (ghostherd-tests--session :name "f" :kind 'agy
                                                  :state 'idle :backend 'fake))
          (ghostherd-tests--fake-screen "Thinking…\n")
          (asked nil))
      (cl-letf (((symbol-function 'ghostherd-backend-screens)
                 (lambda (backend sessions _n callback)
                   (push (cons backend (mapcar #'ghostherd-session-name sessions))
                         asked)
                   (when (eq backend 'tmux)
                     (funcall callback '(("t" . "Do you want to proceed\n")))
                     t))))
        ;; The liveness sweep still runs, and without the stub it would ask
        ;; the real socket about a session that is not there.
        (ghostherd-tests--with-tmux nil
          (ghostherd-poll-all)))
      (should (equal (alist-get 'tmux asked) '("t")))
      (should (equal (alist-get 'fake asked) '("f")))
      (should (eq (ghostherd-session-state tmux-session) 'blocked))
      (should (eq (ghostherd-session-state fake-session) 'working)))))

(ert-deftest ghostherd-test-late-callbacks-keep-their-own-group ()
  "A fetch answers after the loop that started it has moved on, so each
group's callback has to still be about *that* group's sessions."
  (ghostherd-tests--with-herd ()
    (let ((a (ghostherd-tests--session :name "a" :kind 'agy :state 'idle
                                       :backend 'one))
          (b (ghostherd-tests--session :name "b" :kind 'agy :state 'idle
                                       :backend 'two))
          (pending nil))
      (cl-letf (((symbol-function 'ghostherd-backend-screens)
                 (lambda (backend sessions _n callback)
                   ;; Hold the callback, the way a real fetch does.
                   (push (cons backend (lambda (screens)
                                         (ignore sessions)
                                         (funcall callback screens)))
                         pending)
                   t))
                ((symbol-function 'ghostherd--session-live-p) (lambda (_s) t)))
        (ghostherd-poll-all)
        (should (= (length pending) 2))
        ;; Deliver in the opposite order, and give both the same screen
        ;; text: if a callback had captured the wrong group, one of these
        ;; sessions would be left untouched.
        (funcall (cdr (assq 'one pending)) '(("a" . "Do you want to proceed\n")))
        (funcall (cdr (assq 'two pending)) '(("b" . "Thinking…\n")))
        (should (eq (ghostherd-session-state a) 'blocked))
        (should (eq (ghostherd-session-state b) 'working))))))

(ert-deftest ghostherd-test-tmux-batch-round-trip ()
  "Two real panes, one invocation, both screens back -- and stderr from a
pane that is not there kept out of them."
  (skip-unless (executable-find "tmux"))
  (ghostherd-tests--with-herd ()
    (let* ((ghostherd-tmux-socket "ghostherd-ert-batch")
           (ghostherd-tmux--status-cache nil)
           (ghostherd-tmux--fetch nil)
           (sessions nil)
           (screens 'unset))
      (unwind-protect
          (progn
            (dolist (name '("batch-a" "batch-b"))
              (push (ghostherd-spawn
                     'shell :name name :backend 'tmux
                     :project temporary-file-directory
                     :directory temporary-file-directory
                     :command "sh"
                     :args (list "-c" (format "echo screen-of-%s; cat" name))
                     :display nil)
                    sessions))
            (sleep-for 0.5)
            (should (ghostherd-backend-screens
                     'tmux sessions 40 (lambda (s) (setq screens s))))
            (with-timeout (10 (error "batched fetch never came back"))
              (while (eq screens 'unset)
                (accept-process-output nil 0.05)))
            (should (equal (sort (mapcar #'car screens) #'string<)
                           '("batch-a" "batch-b")))
            (should (string-match-p "screen-of-batch-a"
                                    (alist-get "batch-a" screens nil nil #'equal)))
            (should (string-match-p "screen-of-batch-b"
                                    (alist-get "batch-b" screens nil nil #'equal)))
            ;; Each pane is kept whole beside the tail the rules got, not
            ;; captured again for a screen they read nothing on.
            (dolist (name '("batch-a" "batch-b"))
              (let ((kept (gethash name ghostherd--panes)))
                (should (equal (car kept) (alist-get name screens nil nil #'equal)))
                (should (string-match-p (concat "screen-of-" name) (cdr kept)))))
            ;; And the same round trip answered liveness.
            (should (assoc (ghostherd-session-host-id (car sessions))
                           (cdr ghostherd-tmux--status-cache))))
        (dolist (session sessions)
          (ignore-errors (ghostherd--host-kill session)))
        (when (timerp ghostherd--poll-timer)
          (cancel-timer ghostherd--poll-timer)
          (setq ghostherd--poll-timer nil))
        (call-process "tmux" nil nil nil "-L" "ghostherd-ert-batch"
                      "kill-server")))))

(ert-deftest ghostherd-test-tmux-dead-pane-round-trip ()
  "A real pane that says something, scrolls it off its screen and exits
3: the fetch reaches back for it, beside a pane still running -- and
after one killed from outside, which no longer costs the panes after it
their answer."
  (skip-unless (executable-find "tmux"))
  (ghostherd-tests--with-herd ()
    (let* ((ghostherd-tmux-socket "ghostherd-ert-exit")
           (ghostherd-tmux--status-cache nil)
           (ghostherd-tmux--last-words nil)
           (ghostherd-tmux--fetch nil)
           (sessions nil)
           (screens 'unset))
      (unwind-protect
          (progn
            (push (ghostherd-spawn
                   'shell :name "exits" :backend 'tmux
                   :project temporary-file-directory
                   :directory temporary-file-directory
                   :command "sh"
                   :args (list "-c" "echo No conversation found to continue; i=0; \
while [ $i -lt 80 ]; do echo; i=$((i+1)); done; exit 3")
                   :display nil)
                  sessions)
            (push (ghostherd-spawn
                   'shell :name "runs" :backend 'tmux
                   :project temporary-file-directory
                   :directory temporary-file-directory
                   :command "sh" :args (list "-c" "echo still here; cat")
                   :display nil)
                  sessions)
            ;; First in the list, and gone by the time it is asked about.
            (push (ghostherd-spawn
                   'shell :name "killed" :backend 'tmux
                   :project temporary-file-directory
                   :directory temporary-file-directory
                   :command "cat" :args nil :display nil)
                  sessions)
            (sleep-for 0.5)
            (call-process "tmux" nil nil nil "-L" "ghostherd-ert-exit" "kill-session" "-t"
                          (ghostherd-tmux--target (ghostherd-session-host-id (car sessions))))
            (should (ghostherd-backend-screens
                     'tmux sessions 40 (lambda (s) (setq screens s))))
            (with-timeout (10 (error "batched fetch never came back"))
              (while (eq screens 'unset)
                (accept-process-output nil 0.05)))
            (should (equal (sort (mapcar #'car screens) #'string<) '("exits" "runs")))
            (should (string-match-p "still here" (alist-get "runs" screens nil nil #'equal)))
            (should (equal (ghostherd--host-exited-p (ghostherd-get "exits"))
                           "exited 3: No conversation found to continue"))
            (should-not (ghostherd--host-exited-p (ghostherd-get "runs"))))
        (dolist (session sessions)
          (ignore-errors (ghostherd--host-kill session)))
        (when (timerp ghostherd--poll-timer)
          (cancel-timer ghostherd--poll-timer)
          (setq ghostherd--poll-timer nil))
        (call-process "tmux" nil nil nil "-L" "ghostherd-ert-exit"
                      "kill-server")))))

;;; Optional: a real tmux round trip

(ert-deftest ghostherd-test-tmux-round-trip ()
  "Spawn, capture, send, list and kill against a real tmux.
Everything above stubs the seam; this is the one that would catch tmux
itself changing its mind about a flag."
  (skip-unless (executable-find "tmux"))
  (ghostherd-tests--with-herd ()
    (let* ((ghostherd-tmux-socket "ghostherd-ert")
           (ghostherd-tmux--status-cache nil)
           (session nil))
      (unwind-protect
          (progn
            (setq session (ghostherd-spawn
                           'shell
                           :name "ert-probe" :backend 'tmux
                           :project temporary-file-directory
                           :directory temporary-file-directory
                           :command "cat" :args nil :display nil))
            (should (ghostherd-session-host-id session))
            ;; Detached from birth: nothing was attached to it.
            (should-not (ghostherd-session-buffer session))
            (should (ghostherd--host-live-p session))
            (ghostherd--host-send-text session "ping-from-ert" t)
            (sleep-for 0.5)
            (ghostherd-tmux--forget)
            (should (string-match-p "ping-from-ert"
                                    (ghostherd--host-capture session)))
            ;; The recipe is on the host, so a fresh registry can rebuild it.
            (clrhash ghostherd--sessions)
            (let ((ghostherd-known-backends '(tmux)))
              (should (= (ghostherd-restore) 1)))
            (should (ghostherd-get "ert-probe")))
        (when session
          (ignore-errors (ghostherd--host-kill session)))
        (when (timerp ghostherd--poll-timer)
          (cancel-timer ghostherd--poll-timer)
          (setq ghostherd--poll-timer nil))
        (call-process "tmux" nil nil nil "-L" "ghostherd-ert" "kill-server")))))

;; NOTE: an older test asserted the candidate string itself contained the
;; kind and project.  That was true while everything was crammed into one
;; string; it is superseded by `ghostherd-test-candidate-is-just-the-name'
;; and `ghostherd-test-annotation-carries-detail'.

(provide 'ghostherd-tmux-tests)
;;; ghostherd-tmux-tests.el ends here

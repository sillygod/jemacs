;;; ghostherd-backend-tests.el --- Tests for ghostherd-backend.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Part of ghostherd's tests; ghostherd-tests.el runs them all.  This
;; file alone:
;;
;;   emacs --batch --init-directory=~/.emacs.d/emacs-home/ \
;;         -L . -L ../xwapp -l ghostherd-backend-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ghostherd-test-helpers)

;;; Knowing your own name

(ert-deftest ghostherd-test-agent-environment-carries-the-name ()
  "The one thing an agent cannot be told after it starts."
  (let ((env (ghostherd-agent-environment
              'tmux (list :name "reviewer" :kind 'agy))))
    (should (member "GHOSTHERD_SESSION=reviewer" env))
    (should (member "GHOSTHERD_BACKEND=tmux" env))))

(ert-deftest ghostherd-test-agent-environment-names-the-emacs-socket ()
  "`bin/ghostherd' is the only way back for a tmux-hosted agent, and it
should reach *this* Emacs without being configured."
  (let ((server-name "herd"))
    (should (member "GHOSTHERD_SOCKET=herd"
                    (ghostherd-agent-environment 'tmux '(:name "a"))))))

(ert-deftest ghostherd-test-agent-environment-advertises-rpc ()
  "An agent gets the agents' URL -- never the herd's own, which approves a
guard's refusals and edits memory."
  (cl-letf (((symbol-function 'ghostherd-memory-agent-rpc-url)
             (lambda () "http://127.0.0.1:49152/jsonrpc/agent"))
            ((symbol-function 'ghostherd-memory-rpc-url)
             (lambda () "http://127.0.0.1:49152/jsonrpc/admin")))
    (let ((env (ghostherd-agent-environment 'tmux '(:name "a"))))
      (should (member "GHOSTHERD_RPC=http://127.0.0.1:49152/jsonrpc/agent" env))
      (should-not (cl-find "admin" env :test #'string-match-p)))))

(ert-deftest ghostherd-test-agent-environment-omits-rpc-until-sidecar ()
  "No agents' URL -- no sidecar yet, or one from before agent tokens -- is
no GHOSTHERD_RPC at all: never the herd's own in its place."
  (cl-letf (((symbol-function 'ghostherd-memory-agent-rpc-url) (lambda () nil))
            ((symbol-function 'ghostherd-memory-rpc-url)
             (lambda () "http://127.0.0.1:49152/jsonrpc/admin")))
    (should-not
     (cl-find "GHOSTHERD_RPC="
              (ghostherd-agent-environment 'tmux '(:name "a"))
              :test (lambda (pre s) (string-prefix-p pre s))))))

;;; What an agent never has

(defun ghostherd-tests--env-lines ()
  "The environment a process started now has, one VAR=VALUE a line."
  (with-temp-buffer
    (call-process "env" nil t nil)
    (split-string (buffer-string) "\n" t)))

(ert-deftest ghostherd-test-agent-env-withhold-is-the-bots-alone ()
  "The default withholds the bot's two variables and nothing an agent
works with."
  (should (equal (sort (copy-sequence (eval (car (get 'ghostherd-agent-env-withhold
                                                       'standard-value))
                                            t))
                       #'string<)
                 '("GHOSTHERD_TELEGRAM_CHAT_IDS" "GHOSTHERD_TELEGRAM_TOKEN"))))

(ert-deftest ghostherd-test-agent-process-environment-unsets-the-withheld ()
  "A process started under it has none of the withheld names and the rest
as they were; a \"name\" with an = would set a variable, so it is not one.
Emacs's own `getenv' is untouched: it is bound around a spawn, never set."
  (let ((ghostherd-agent-env-withhold
         '("GHOSTHERD_TELEGRAM_TOKEN" "GHOSTHERD_TELEGRAM_CHAT_IDS" "PLANTED=x"))
        (process-environment (append ghostherd-tests--bot-env process-environment)))
    (let ((env (let ((process-environment (ghostherd-agent-process-environment)))
                 (ghostherd-tests--env-lines))))
      (should (ghostherd-tests--bot-free-p env))
      (should-not (member "PLANTED=x" env))
      (should (member "GITHUB_TOKEN=fake-gh" env)))
    (should (equal (getenv "GHOSTHERD_TELEGRAM_TOKEN") "fake-token"))))

(ert-deftest ghostherd-test-agent-environment-never-names-a-withheld-one ()
  "Identity is injected after the environment is cleaned -- tmux's `-e'
-- so a withheld name has to stay out of it too."
  (let ((server-name "herd")
        (ghostherd-agent-env-withhold '("GHOSTHERD_SOCKET")))
    (let ((env (ghostherd-agent-environment 'tmux '(:name "a"))))
      (should (member "GHOSTHERD_SESSION=a" env))
      (should-not (cl-find "GHOSTHERD_SOCKET" env :test #'string-prefix-p)))))

(ert-deftest ghostherd-test-ghostel-spawn-withholds-the-bot ()
  "The shell ghostel starts -- and so the agent typed into it -- has no
withheld name, and its identity and the tokens agents work with."
  (let ((buf (generate-new-buffer " *gh-withhold-test*"))
        (ghostherd-agent-env-withhold '("GHOSTHERD_TELEGRAM_TOKEN" "GHOSTHERD_TELEGRAM_CHAT_IDS"))
        (process-environment (append ghostherd-tests--bot-env process-environment))
        seen)
    (unwind-protect
        (cl-letf (((symbol-function 'ghostherd--require-ghostel) #'ignore)
                  ;; A real process, started where ghostel starts its shell.
                  ((symbol-function 'ghostel)
                   (lambda (&rest _) (setq seen (ghostherd-tests--env-lines)) buf))
                  ((symbol-function 'ghostherd--launch-in-buffer) #'ignore))
          (ghostherd-backend-spawn 'ghostel (list :name "gh-withhold-test" :command "claude"
                                                  :directory "/tmp/")))
      (kill-buffer buf))
    (should (ghostherd-tests--bot-free-p seen))
    (should (member "GITHUB_TOKEN=fake-gh" seen))
    (should (member "GHOSTHERD_SESSION=gh-withhold-test" seen))
    (should (equal (getenv "GHOSTHERD_TELEGRAM_TOKEN") "fake-token"))))

(ert-deftest ghostherd-test-the-agents-url-is-the-agents-token ()
  "Read from the sidecar's rpc.agent-token, beside its own rpc.token."
  (let* ((data (make-temp-file "gh-data-" t))
         (ghostherd-memory--port 49152))
    (unwind-protect
        (cl-letf (((symbol-function 'ghostherd-memory--data-directory) (lambda () data)))
          (should-not (ghostherd-memory-agent-rpc-url))           ; a sidecar from before
          (write-region "admintoken" nil (expand-file-name "rpc.token" data))
          (should-not (ghostherd-memory-agent-rpc-url))
          (write-region "agenttoken\n" nil (expand-file-name "rpc.agent-token" data))
          (should (equal (ghostherd-memory-agent-rpc-url) "http://127.0.0.1:49152/jsonrpc/agenttoken"))
          (should (string-suffix-p "/admintoken" (ghostherd-memory-rpc-url))))
      (delete-directory data t))))

;;; The backend slot
;;
;; The point of the slot is that everything above it goes through it, so
;; these tests pin the call sites rather than either implementor: a fake
;; backend returns a known screen, and breaking the wiring fails a test
;; without tmux or a PTY being anywhere near it.

(ert-deftest ghostherd-test-detect-reads-through-the-backend ()
  "Detection must take the screen from the backend, not from a buffer.
A session on a backend with no buffer at all still has to be detectable
-- that is the whole reason the slot exists."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy :backend 'fake))
          (ghostherd-tests--fake-screen "Do you want to proceed\n"))
      ;; the buffer says nothing; the backend says blocked
      (should (eq (car (ghostherd--detect-state s)) 'blocked)))))

(ert-deftest ghostherd-test-read-and-wait-read-through-the-backend ()
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :kind 'agy :backend 'fake))
          (ghostherd-tests--fake-screen "42 tests, 0 failures\n"))
      (should (equal (ghostherd-read s) "42 tests, 0 failures\n"))
      (should (equal (ghostherd-output-matches s "[0-9]+ failures")
                     "0 failures")))))

(ert-deftest ghostherd-test-live-p-is-the-host-not-the-view ()
  "A detached session -- alive, no buffer -- is live.  This is the
distinction the whole phase turns on."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :backend 'fake))
          (ghostherd-tests--fake-live t))
      (kill-buffer (ghostherd-session-buffer s))
      (setf (ghostherd-session-buffer s) nil)
      (should (ghostherd--session-live-p s))
      (let ((ghostherd-tests--fake-live nil))
        (should-not (ghostherd--session-live-p s))))))

(ert-deftest ghostherd-test-ghostel-session-live-p-unchanged ()
  "The default backend must still mean exactly what it did."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a")))
      (should (eq (ghostherd-session-backend s) 'ghostel))
      (should (ghostherd--session-live-p s))
      (kill-buffer (ghostherd-session-buffer s))
      (should-not (ghostherd--session-live-p s)))))

(ert-deftest ghostherd-test-string-tail-keeps-the-top-line ()
  "A capture of exactly N rows ends in a newline.  Splitting it naively
yields N+1 fields, and taking the last N then drops the *first* line --
which is how the first thing an agent printed goes missing from a
screen that is plainly still showing it."
  (let ((screen (mapconcat #'number-to-string (number-sequence 1 40) "\n")))
    (should (equal (ghostherd--string-tail (concat screen "\n") 40) screen))
    (should (equal (ghostherd--string-tail (concat screen "\n") 3) "38\n39\n40"))))

;;; Surviving a reload

(ert-deftest ghostherd-test-stale-session-names-itself ()
  "Adding a slot to `ghostherd-session' does not migrate the structs
already in the registry, so every field after it reads shifted and a
project path arrives where a backend symbol belongs.  Left alone that
surfaces as `cl-no-applicable-method', once per redisplay, naming
neither the cause nor the cure."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "old")))
      (setf (ghostherd-session-backend s) "/src/app/")
      (let ((message (cadr (should-error (ghostherd--host-live-p s)))))
        (should (string-match-p "clrhash ghostherd--sessions" message))
        (should (string-match-p "old" message))))))

(ert-deftest ghostherd-test-mode-line-asks-no-host ()
  "Redisplay is no place for I/O, and no place to find out that the I/O
failed: a segment that signals breaks the whole frame's redisplay, and
repeats for as long as the session is registered."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :state 'blocked)))
      ;; the worst case: a session whose host cannot even be identified
      (setf (ghostherd-session-backend s) "/src/app/")
      (let ((segment (substring-no-properties
                      (or (ghostherd--mode-line-segment) ""))))
        (should (string-match-p "1" segment))))))

(ert-deftest ghostherd-test-ensure-sessions-keeps-what-it-cannot-ask ()
  "Pruning on a failed question would delete the herd rather than
report it -- and a host is far more likely to be briefly unreachable
than actually gone."
  (ghostherd-tests--with-herd ()
    (let ((s (ghostherd-tests--session :name "a" :backend 'fake))
          (ghostherd-tests--fake-live t))
      (cl-letf (((symbol-function 'ghostherd--host-live-p)
                 (lambda (&rest _) (error "host unreachable"))))
        (ghostherd--ensure-sessions))
      (should (eq (ghostherd-get "a") s)))))

(ert-deftest ghostherd-test-tmux-not-asked-is-not-gone ()
  "tmux says nothing runs only when there is no server -- a socket left
behind, or none -- or no tmux.  Asked and not answering, it is unknown:
read as an empty herd, every session was gone, and the sweep dropped
the whole herd, its panes still running, without a word."
  (dolist (case '(((0 . "gh-abc-rev\t0\t\tclaude\n") (("gh-abc-rev" nil . "claude")))
                  ((1 . "no server running on /private/tmp/tmux-501/ghostherd\n") nil)
                  ((1 . "error connecting to /private/tmp/tmux-501/ghostherd (No such file or directory)\n") nil)
                  ((127 . "Searching for program: No such file or directory, tmux") nil)
                  ((1 . "error connecting to /private/tmp/tmux-501/ghostherd (Resource temporarily unavailable)\n") unknown)
                  ((1 . "lost server\n") unknown)
                  ((1 . "") unknown)))
    (cl-letf (((symbol-function 'ghostherd-tmux--call) (lambda (_args) (car case))))
      (should (equal (ghostherd-tmux--read-snapshot) (cadr case)))))
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-log
      (let ((s (ghostherd-tests--tmux-session :name "rev" :kind 'claude)))
        ;; Not answering: there, as far as anyone knows -- and not dead.
        (ghostherd-tests--with-tmux
            '(("list-panes" . (1 . "error connecting to /x (Resource temporarily unavailable)")))
          (should (ghostherd--session-live-p s))
          (should-not (ghostherd--host-exited-p s))
          (ghostherd--ensure-sessions))
        (should (eq (ghostherd-get "rev") s))
        (should-not ghostherd--log)
        ;; No server: gone, and the log says so.
        (ghostherd-tests--with-tmux '(("list-panes" . (1 . "no server running on /x")))
          (ghostherd--ensure-sessions))
        (should-not (ghostherd-get "rev"))
        (should (equal (mapcar #'ghostherd-log-entry-text ghostherd--log)
                       '("dropped from the herd: its host is gone")))))))

(ert-deftest ghostherd-test-a-readopted-agent-takes-back-its-view ()
  "An agent dropped and re-adopted in the same Emacs is still shown in
its buffer, attached: it takes that buffer back -- on restore, and on
being opened -- rather than attaching again in it, which ghostel
refuses.  A buffer only named for it, or one whose process is gone, is
not taken."
  (ghostherd-tests--with-herd ()
    (let* ((proc (make-process :name "gh-view" :command '("sleep" "30") :noquery t))
           (view (get-buffer-create "*ghostherd:rev*")))
      (unwind-protect
          (progn
            (with-current-buffer view
              (setq-local ghostherd-session-id "rev")
              (setq-local ghostel--process proc))
            ;; Restored: its buffer is the one still showing it.
            (let ((ghostherd-known-backends '(fake))
                  (ghostherd-tests--fake-recipes '((:host-id "gh-abc-rev" :name "rev" :kind claude))))
              (cl-letf (((symbol-function 'ghostherd--sidebar-refresh) #'ignore)
                        ((symbol-function 'ghostherd--ensure-poll-timer) #'ignore))
                (ghostherd-restore)))
            (should (eq (ghostherd-session-buffer (ghostherd-get "rev")) view))
            ;; Opened with no buffer of its own: that one, not a second attach.
            (let ((s (ghostherd-get "rev")))
              (setf (ghostherd-session-backend s) 'tmux
                    (ghostherd-session-buffer s) nil)
              (cl-letf (((symbol-function 'ghostherd-tmux--attach) (lambda (_) (error "Attached again"))))
                (should (eq (ghostherd-backend-view 'tmux s) view)))
              (should (eq (ghostherd-session-buffer s) view)))
            ;; Not its own, or not running: not taken.
            (with-current-buffer view (setq-local ghostherd-session-id "other"))
            (should-not (ghostherd--orphan-view "rev"))
            (with-current-buffer view (setq-local ghostherd-session-id "rev"))
            (delete-process proc)
            (should-not (ghostherd--orphan-view "rev"))
            (should-not (ghostherd--orphan-view "nobody")))
        (when (process-live-p proc) (delete-process proc))
        (kill-buffer view)))))

(ert-deftest ghostherd-test-restore-is-idempotent-and-marks-restored ()
  "A second restore registers nothing new, and restore marks the herd
restored so the sidecar may trust the session snapshot."
  (ghostherd-tests--with-herd ((ghostherd--restored nil)
                               (ghostherd--roles (make-hash-table :test 'equal))
                               (ghostherd--lineage (make-hash-table :test 'equal)))
    (let ((ghostherd-known-backends '(fake))
          (ghostherd-tests--fake-recipes '((:host-id "gh-x-a" :name "a" :kind agy)
                                           (:host-id "gh-x-b" :name "b" :kind claude))))
      (cl-letf (((symbol-function 'ghostherd--sidebar-refresh) #'ignore)
                ((symbol-function 'ghostherd--ensure-poll-timer) #'ignore)
                ((symbol-function 'ghostherd--orphan-view) #'ignore))
        (should-not ghostherd--restored)
        (should (= 2 (ghostherd-restore)))      ; both agents registered
        (should ghostherd--restored)            ; and the herd is marked restored
        (should (ghostherd-get "a"))
        (should (ghostherd-get "b"))
        (should (= 0 (ghostherd-restore)))))))  ; idempotent: nothing new the 2nd time

(ert-deftest ghostherd-test-an-empty-herd-is-looked-for-again ()
  "The poll looks for agents to re-adopt while the herd is empty -- not on
every tick -- so a herd emptied by a moment its host could not be
reached comes back by itself, to the herd page too."
  (ghostherd-tests--with-herd ()
    (let ((ghostherd--restore-tried-at 0) (restored 0))
      (cl-letf (((symbol-function 'ghostherd-restore) (lambda (&rest _) (cl-incf restored))))
        (ghostherd-poll-all)
        (ghostherd-poll-all)
        (should (= restored 1))
        (setq ghostherd--restore-tried-at (- (float-time) ghostherd-restore-every 1))
        (ghostherd-poll-all)
        (should (= restored 2))
        ;; A herd with someone in it is not looked for.
        (ghostherd-tests--session :name "a" :backend 'fake)
        (setq ghostherd--restore-tried-at 0)
        (let ((ghostherd-tests--fake-live t))
          (cl-letf (((symbol-function 'ghostherd-poll-session) #'ignore))
            (ghostherd-poll-all)))
        (should (= restored 2))))))

;;; Detach is not death

(ert-deftest ghostherd-test-view-exit-kills-only-on-ghostel ()
  "The easy bug.  On a backend where the buffer runs `tmux attach', the
process exiting means a client detached; marking the session dead would
turn every closed window into a crash notification, fire a handoff
callback, and be reverted by the next poll anyway."
  (ghostherd-tests--with-herd ()
    (cl-letf (((symbol-function 'ghostherd--notify) (lambda (&rest _) nil)))
      (let* ((ghostel (ghostherd-tests--session
                       :name "g" :kind 'agy :state 'working))
             (tmux (ghostherd-tests--session
                    :name "t" :kind 'agy :state 'working :backend 'tmux)))
        (ghostherd--on-ghostel-exit (ghostherd-session-buffer ghostel)
                                    "finished\n")
        (should (eq (ghostherd-session-state ghostel) 'dead))

        (ghostherd--on-ghostel-exit (ghostherd-session-buffer tmux)
                                    "finished\n")
        (should (eq (ghostherd-session-state tmux) 'working))
        (should-not (ghostherd-session-buffer tmux))))))

(ert-deftest ghostherd-test-view-is-host-p-declared-per-backend ()
  (should (ghostherd-backend-view-is-host-p 'ghostel))
  (should-not (ghostherd-backend-view-is-host-p 'tmux))
  ;; An unknown backend must default to the safe answer: a view that is
  ;; not known to own the agent cannot be allowed to declare it dead.
  (should-not (ghostherd-backend-view-is-host-p 'something-new)))

(provide 'ghostherd-backend-tests)
;;; ghostherd-backend-tests.el ends here

;;; ghostherd-lead-tests.el --- Tests for ghostherd-lead.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Part of ghostherd's tests; ghostherd-tests.el runs them all.  This
;; file alone:
;;
;;   emacs --batch --init-directory=~/.emacs.d/emacs-home/ \
;;         -L . -L ../xwapp -l ghostherd-lead-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ghostherd-test-helpers)

(declare-function ghostherd-memory-page--ask "ghostherd-memory-page" (a))
(declare-function ghostherd-memory-page--handle "ghostherd-memory-page" (intent))
(declare-function ghostherd-memory-page--lead-work "ghostherd-memory-page" (project))
(declare-function ghostherd-memory-page--memory-tick "ghostherd-memory-page" ())
(declare-function ghostherd-memory-page--room-sessions "ghostherd-memory-page" ())
(declare-function ghostherd-memory-page--send-herd "ghostherd-memory-page" ())

;;; A project's lead

(ert-deftest ghostherd-test-a-project-is-one-spelling ()
  "Spawned here an agent's project is absolute; restored, it is under ~.
The page and the sidecar compare them as strings, so both go as one."
  (should (equal (ghostherd--project-name (expand-file-name "~/tmp/room-trial")) "~/tmp/room-trial/"))
  (should (equal (ghostherd--project-name "~/tmp/room-trial/") "~/tmp/room-trial/"))
  (should-not (ghostherd--project-name ""))
  (should-not (ghostherd--project-name nil)))

(ert-deftest ghostherd-test-a-lead-stays-a-lead ()
  "The role goes onto the host with the recipe, and comes back with a
restore, a respawn's recipe and the herd's snapshot."
  (ghostherd-tests--with-herd ((ghostherd--roles (make-hash-table :test 'equal)))
    (ghostherd-tests--with-tmux '(("has-session" . (1 . "")))
      (ghostherd-backend-spawn 'tmux (list :name "lead-p" :kind 'claude :command "claude"
                                           :directory "/tmp/" :project "/tmp/" :role "lead")))
    (should (equal (alist-get "@ghostherd-role" (ghostherd-tests--tmux-options) nil nil #'equal) "lead"))
    (let ((ghostherd-known-backends '(fake))
          (ghostherd-tests--fake-recipes
           '((:host-id "gh-1" :name "lead-p" :kind claude :project "~/p/" :role "lead")
             (:host-id "gh-2" :name "rd" :kind claude :project "~/p/"))))
      (should (= (ghostherd-restore) 2))
      ;; One spawned here carries the absolute name: the same project.
      (ghostherd-tests--session :name "qa" :kind 'claude :project (expand-file-name "~/p"))
      (let ((lead (ghostherd-get "lead-p")))
        (should (equal (ghostherd-session-role lead) "lead"))
        (should-not (ghostherd-session-role (ghostherd-get "rd")))
        (should (equal (plist-get (ghostherd-session-recipe lead) :role) "lead"))
        (should (eq (ghostherd-project-lead (expand-file-name "~/p")) lead))
        (should (equal (sort (mapcar (lambda (r) (list (plist-get r :name) (plist-get r :role) (plist-get r :project)))
                                     (ghostherd--herd-snapshot))
                             (lambda (a b) (string< (car a) (car b))))
                       '(("lead-p" "lead" "~/p/") ("qa" "" "~/p/") ("rd" "" "~/p/"))))))))

(ert-deftest ghostherd-test-a-lead-starts-with-its-role ()
  "claude takes the role as an appended system prompt, grok as rules;
agy takes none, and gets it with its first message."
  (ghostherd-tests--with-leads
    (let ((ghostherd-lead-kind 'claude))
      (ghostherd-start-lead "/tmp/room-trial/")
      (let ((pl (cdar spawned)))
        (should (eq (caar spawned) 'claude))
        (should (equal (list (plist-get pl :name) (plist-get pl :role) (plist-get pl :display))
                       '("lead-room-trial" "lead" nil)))
        (should (equal (last (plist-get pl :args) 2)
                       (list "--append-system-prompt" ghostherd--lead-role))))
      (should (equal (ghostherd--lead-args 'grok) (list "--rules" ghostherd--lead-role)))
      (should-not (ghostherd--lead-args 'agy))
      ;; The first run's lead reported in English to a user writing Chinese.
      (should (string-match-p "in the\nlanguage they write to you in" ghostherd--lead-role))
      ;; rd-tabs ran npm ci in its worktree, trading the cloned
      ;; node_modules for a full copy: the lead briefs against it.
      (should (string-search "use \"npm install\", not \"npm ci\", which\ndeletes the clone"
                             ghostherd--lead-role))
      ;; A second lead elsewhere does not take the first one's name.
      (ghostherd-start-lead "/other/room-trial/")
      (should (equal (plist-get (cdar spawned) :name) "lead-room-trial-2")))))

(ert-deftest ghostherd-test-a-respawned-lead-gets-the-role-as-it-is-now ()
  "A lead started before its role changed is respawned with the new one,
fresh or resumed; an agent that is not a lead keeps its args."
  (ghostherd-tests--with-herd ((ghostherd--herd-links (make-hash-table :test 'equal))
                               (ghostherd--roles (make-hash-table :test 'equal)))
    (puthash "lead-p" "lead" ghostherd--roles)
    (let ((lead (ghostherd-tests--session :name "lead-p" :kind 'claude
                                          :args '("--model" "opus" "--append-system-prompt" "old role")))
          (rd (ghostherd-tests--session :name "rd" :kind 'claude
                                        :args '("--append-system-prompt" "be terse")))
          spawned)
      (cl-letf (((symbol-function 'ghostherd-spawn)
                 (lambda (_kind &rest plist) (setq spawned plist) nil)))
        (ghostherd-respawn lead)
        (should (equal (plist-get spawned :args)
                       (list "--model" "opus" "--append-system-prompt" ghostherd--lead-role)))
        (ghostherd-respawn (ghostherd-tests--session :name "lead-p" :kind 'claude
                                                     :args '("--append-system-prompt" "old role"))
                           t)
        (should (equal (plist-get spawned :args)
                       (list "--continue" "--append-system-prompt" ghostherd--lead-role)))
        (ghostherd-respawn rd)
        (should (equal (plist-get spawned :args) '("--append-system-prompt" "be terse")))))
    (should (equal (ghostherd--with-lead-role 'grok '("--rules" "old" "-x"))
                   (list "-x" "--rules" ghostherd--lead-role)))
    (should (equal (ghostherd--with-lead-role 'agy '("-x")) '("-x")))))

(ert-deftest ghostherd-test-the-page-sees-one-project-and-tells-the-lead ()
  "However its agents spell their project, the page and the room see one;
and what the page sends an agent of a project with a lead is kept for
the lead."
  (ghostherd-test--with-herd
    (let ((ghostherd--roles (make-hash-table :test 'equal))
          (ghostherd--lead-unheard (make-hash-table :test 'equal))
          (ghostherd-memory-page--room "~/p/"))
      (ghostherd-tests--session :name "lead-p" :kind 'claude :project "~/p/" :state 'idle)
      (puthash "lead-p" "lead" ghostherd--roles)
      (ghostherd-tests--session :name "qa" :kind 'claude :project (expand-file-name "~/p") :state 'idle)
      (ghostherd-memory-page--send-herd)
      (should (equal (seq-keep (lambda (a) (and (member (plist-get a :name) '("lead-p" "qa"))
                                                 (plist-get a :project)))
                               (plist-get (ghostherd-test--call calls "renderHerd") :agents))
                     '("~/p/" "~/p/")))
      (should (equal (sort (mapcar #'ghostherd-session-name (ghostherd-memory-page--room-sessions)) #'string<)
                     '("lead-p" "qa")))
      (cl-letf (((symbol-function 'ghostherd-prompt) #'ignore))
        (ghostherd-memory-page--handle '((op . "agent-prompt") (name . "qa") (text . "run it"))))
      (should (equal (gethash "~/p/" ghostherd--lead-unheard) '(("qa" . "run it")))))))

(ert-deftest ghostherd-test-the-first-word-to-a-lead-starts-it ()
  "Saying something to a project's lead starts one when it has none; the
words reach it once it is at its prompt -- after its role, on a CLI that
took none.  The next go straight to it, after what the user said to the
team directly meanwhile, and that only once."
  (ghostherd-tests--with-leads
    (let ((ghostherd-lead-kind 'agy) (said nil))
      (ghostherd-tests--session :name "rd" :kind 'claude :project "~/p/" :state 'idle)
      (cl-letf (((symbol-function 'ghostherd-prompt)
                 (lambda (s text &rest _) (push (cons (ghostherd-session-name s) text) said))))
        ;; Before there is a lead, there is no one to tell.
        (ghostherd-tell (ghostherd-get "rd") "before any lead")
        (should (= (hash-table-count ghostherd--lead-unheard) 0))
        (setq said nil)
        (let ((lead (ghostherd-lead-say "~/p/" "plan the login page")))
          (should (equal (ghostherd-session-name lead) "lead-p"))
          (should-not said)
          (setf (ghostherd-session-state lead) 'idle)
          (let ((next (pop later))) (apply (car next) (cdr next)))
          (should (equal (car said) (cons "lead-p" (concat ghostherd--lead-role "\n---\n\nplan the login page"))))
          (ghostherd-tell (ghostherd-get "rd") "use the old API\nfor now")
          (should (equal (car said) '("rd" . "use the old API\nfor now")))
          (should (eq (ghostherd-lead-say (expand-file-name "~/p") "status?") lead))
          (should (equal (cdar said)
                         "[Since you last heard from me, I wrote to the team directly:\n- to rd: use the old API for now]\n\nstatus?"))
          (ghostherd-tell lead "and now?")
          (should (equal (cdar said) "and now?"))
          (should (= (length spawned) 1)))))))

(ert-deftest ghostherd-test-a-lead-is-told-what-its-project-works-on ()
  "A lead's first message opens with what its project works on, and the
rule to change none of it; the next ones only when that changed --
after what the user said to the team, and after a role it took with
its first message.  Its agents are not told, nor another project's lead."
  (ghostherd-tests--with-leads
    (let* ((work (list (list "~/p/" "branch x in ~/p/" "its pull request #3, open: Login -- https://h/3")
                       (list "~/q/" "branch main in ~/q/")))
           (ghostherd-project-work-functions (list (lambda (p) (cdr (assoc p work)))))
           (said nil)
           (lead (ghostherd-tests--session :name "lead-p" :kind 'claude :project "~/p/" :state 'idle))
           (rd (ghostherd-tests--session :name "rd" :kind 'claude :project "~/p/" :state 'idle))
           (other (ghostherd-tests--session :name "lead-q" :kind 'agy :project "~/q/" :state 'idle))
           (rule "Read them for the work; change none of them -- no ticket's status, fields or comments, nothing on the pull request -- unless I ask you to:\n"))
      (puthash "lead-p" "lead" ghostherd--roles)
      (puthash "lead-q" "lead" ghostherd--roles)
      (cl-letf (((symbol-function 'ghostherd-prompt)
                 (lambda (s text &rest _) (push (cons (ghostherd-session-name s) text) said))))
        (ghostherd-tell lead "plan it")
        (should (equal (cdar said)
                       (concat "[What this project works on, as the herd sees it.  " rule
                               "- branch x in ~/p/\n- its pull request #3, open: Login -- https://h/3]\n\nplan it")))
        (ghostherd-tell lead "status?")
        (should (equal (cdar said) "status?"))
        (ghostherd-tell rd "go")
        (should (equal (car said) '("rd" . "go")))
        (setcdr (assoc "~/p/" work) (list "branch x in ~/p/" "its pull request #3, merged: Login -- https://h/3"))
        (ghostherd-tell lead "now?")
        (should (equal (cdar said)
                       (concat "[Since you last heard from me, I wrote to the team directly:\n- to rd: go]\n\n"
                               "[What this project works on has changed since you were told.  Now:\n"
                               "- branch x in ~/p/\n- its pull request #3, merged: Login -- https://h/3]\n\nnow?")))
        ;; Nothing said of it: nothing told, and not taken for a change.
        (setcdr (assoc "~/p/" work) nil)
        (ghostherd-tell lead "and?")
        (should (equal (cdar said) "and?"))
        ;; A role it takes with its first message comes before it all.
        (puthash "~/q/" (list (cons "ops" "hold the deploy")) ghostherd--lead-unheard)
        (ghostherd-tell other "start" "ROLE\n---\n\n")
        (should (equal (car said)
                       (cons "lead-q" (concat "ROLE\n---\n\n"
                                              "[Since you last heard from me, I wrote to the team directly:\n- to ops: hold the deploy]\n\n"
                                              "[What this project works on, as the herd sees it.  "
                                              rule "- branch main in ~/q/]\n\nstart"))))
        ;; And the role says so as well, for every message after.
        (should (string-match-p "change none of them -- no ticket's status, fields or\ncomments, nothing on the pull request -- unless the user asks you to"
                                ghostherd--lead-role))))))

(ert-deftest ghostherd-test-a-new-lead-looks-before-it-is-ready ()
  "What its project works on is asked as the lead starts, so a look has
had the time it took to come to its prompt."
  (ghostherd-tests--with-leads
    (let* ((asked 0)
           (ghostherd-lead-kind 'claude)
           (ghostherd-project-work-functions (list (lambda (_p) (cl-incf asked) (list "branch x in ~/p/"))))
           (said nil))
      (cl-letf (((symbol-function 'ghostherd-prompt)
                 (lambda (_s text &rest _) (push text said))))
        (let ((lead (ghostherd-lead-say "~/p/" "plan it")))
          (should (= asked 1))
          (should-not said)
          (setf (ghostherd-session-state lead) 'idle)
          (let ((next (pop later))) (apply (car next) (cdr next)))
          (should (= asked 2))
          (should (string-suffix-p "- branch x in ~/p/]\n\nplan it" (car said))))))))

(ert-deftest ghostherd-test-the-page-tells-a-lead-what-its-project-works-on ()
  "From the page's last look: the project's branch, its pull request and
the tickets that names; an agent's checkout where it is on another; the
open pull requests and tickets its agents' conversations name, each
once and only so many.  Nothing of another project's."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (ghostherd-tests--with-herd ((ghostherd-memory-page--checkouts (make-hash-table :test 'equal))
                               (ghostherd-memory-page--prs (make-hash-table :test 'equal))
                               (ghostherd-memory-page--task-briefs (make-hash-table :test 'equal))
                               (ghostherd-memory-page--links (make-hash-table :test 'equal))
                               (ghostherd-memory-page--pr-briefs (make-hash-table :test 'equal))
                               (ghostherd-memory-page--remotes (make-hash-table :test 'equal)))
    (let ((ghostherd-memory-page--work-at (float-time))
          (now (float-time))
          (brief (lambda (name status type)
                   (list :at (float-time) :brief `((name . ,name) (status (name . ,status) (type . ,type))
                                                    (url . ,(concat "https://c/t/" (downcase name))))))))
      (ghostherd-tests--session :name "lead-p" :kind 'claude :project "~/p/")
      (ghostherd-tests--session :name "rd" :kind 'claude :project "~/p/")
      (ghostherd-tests--session :name "x" :kind 'claude :project "~/q/")
      (puthash "~/p/" '("~/p/" . "feat/login") ghostherd-memory-page--checkouts)
      (puthash '(agent . "lead-p") '("~/p/" . "feat/login") ghostherd-memory-page--checkouts)
      (puthash '(agent . "rd") '("~/wt/fix/" . "fix/typo") ghostherd-memory-page--checkouts)
      (puthash "~/q/" '("~/q/" . "main") ghostherd-memory-page--checkouts)
      (puthash '(agent . "x") '("~/q/" . "main") ghostherd-memory-page--checkouts)
      (puthash '("~/p/" . "feat/login")
               (list :at now :pr '((id . 12) (title . "Login\n  page") (state . "OPEN") (draft . t)
                                   (url . "https://h/12") (tasks "86a" "86b")))
               ghostherd-memory-page--prs)
      (puthash '("~/wt/fix/" . "fix/typo") (list :at now :pr '((error . "no pull request")))
               ghostherd-memory-page--prs)
      (puthash '("~/q/" . "main") (list :at now :pr '((id . 1) (title . "Q") (state . "OPEN") (url . "https://h/q1")))
               ghostherd-memory-page--prs)
      (puthash "86a" (funcall brief "Login" "in progress" "custom") ghostherd-memory-page--task-briefs)
      (puthash "86c" (funcall brief "Typo" "to do" "open") ghostherd-memory-page--task-briefs)
      (puthash "86d" (funcall brief "Old" "complete" "closed") ghostherd-memory-page--task-briefs)
      ;; rd's conversation: the project's PR again, one open, one merged;
      ;; a ticket the PR names, one open, one done, one not looked up.
      (puthash "~/wt/fix/" (cons now '(:kind github :owner "O" :repo "R")) ghostherd-memory-page--remotes)
      (dolist (pr '((12 "OPEN" "https://h/12") (20 "OPEN" "https://h/20") (21 "MERGED" "https://h/21")
                    (2 "OPEN" "https://w/b/2" "w" "b")))
        (puthash (list "github" (or (nth 3 pr) "o") (or (nth 4 pr) "r") (car pr))
                 (list :at now :pr `((title . ,(format "PR %d" (car pr))) (state . ,(nth 1 pr)) (url . ,(nth 2 pr))))
                 ghostherd-memory-page--pr-briefs))
      (puthash "rd" (list :sig "s" :links
                          (list :prs (append
                                      (mapcar (lambda (id) (list :host "github" :owner "o" :repo "r" :id id :last 1))
                                              '(12 20 21))
                                      ;; Another repository's, which it works on too.
                                      '((:host "github" :owner "w" :repo "b" :id 2 :last 1)))
                                :tasks (mapcar (lambda (id) (list :id id :last 1)) '("86a" "86c" "86d" "86e"))))
               ghostherd-memory-page--links)
      (should (equal (ghostherd-memory-page--lead-work "~/p/")
                     '("branch feat/login in ~/p/"
                       "its pull request #12, open, draft: Login page -- https://h/12"
                       "a ticket its pull request names: ClickUp task 86a, in progress: Login -- https://c/t/login"
                       "a ticket its pull request names: ClickUp task 86b"
                       "rd works on branch fix/typo in ~/wt/fix/"
                       "named in rd's conversation: pull request #20, open: PR 20 -- https://h/20"
                       "named in rd's conversation: pull request w/b#2, open: PR 2 -- https://w/b/2"
                       "named in rd's conversation: ClickUp task 86c, to do: Typo -- https://c/t/typo")))
      (let ((ghostherd-memory-page--lead-named 1))
        (should (equal (last (ghostherd-memory-page--lead-work "~/p/"))
                       '("named in rd's conversation: pull request #20, open: PR 20 -- https://h/20"))))
      (let ((ghostherd-memory-page--lead-named 0))
        (should (equal (last (ghostherd-memory-page--lead-work "~/p/"))
                       '("rd works on branch fix/typo in ~/wt/fix/"))))
      (should (memq #'ghostherd-memory-page--lead-work ghostherd-project-work-functions))
      (should-not (ghostherd-memory-page--lead-work "~/r/"))
      ;; A look a minute old: another is started, and the last one told.
      (let ((looked 0))
        (cl-letf (((symbol-function 'ghostherd-memory-page--work-look) (lambda () (cl-incf looked))))
          (ghostherd-memory-page--lead-work "~/p/")
          (should (= looked 0))
          (let ((ghostherd-memory-page--work-at (- (float-time) 61)))
            (should (equal (car (ghostherd-memory-page--lead-work "~/p/")) "branch feat/login in ~/p/"))
            (should (= looked 1))))))))

(ert-deftest ghostherd-test-waiting-for-a-prompt-gives-up ()
  "A session that never comes to its prompt is logged, its message not
sent; one gone is let be."
  (ghostherd-tests--with-leads
    (ghostherd-tests--with-log
      (let ((s (ghostherd-tests--session :name "a" :state 'starting)) (ran 0))
        (ghostherd--when-ready s (lambda () (cl-incf ran)) 1)
        (let ((next (pop later))) (apply (car next) (cdr next)))
        (should (= ran 0))
        (should-not later)
        (should (string-match-p "never came to its prompt" (ghostherd-log-entry-text (car ghostherd--log))))
        (remhash "a" ghostherd--sessions)
        (setf (ghostherd-session-state s) 'idle)
        (ghostherd--when-ready s (lambda () (cl-incf ran)))
        (should (= ran 0))))))

(ert-deftest ghostherd-test-the-page-starts-a-lead ()
  "The page's word to a project with no lead yet starts one and says so;
with one, it goes to it.  An image for the lead not yet started is kept
for it.  The page knows the lead by its role."
  (ghostherd-test--with-herd
    (let ((dir (file-name-as-directory (make-temp-file "gh-proj" t))) (asked nil) (lead nil))
      (unwind-protect
          (cl-letf (((symbol-function 'ghostherd-lead-say)
                     (lambda (project text) (push (list project text) asked)
                       (setq lead (or lead (ghostherd-tests--session :name "lead-x" :project project))))))
            (ghostherd-memory-page--handle `((op . "lead-say") (project . ,dir) (text . "plan it")))
            (should (equal asked (list (list dir "plan it"))))
            (should (string-match-p "Started lead-x" (ghostherd-test--call calls "flash")))
            (puthash "lead-x" "lead" ghostherd--roles)
            (cl-letf (((symbol-function 'ghostherd-project-lead) (lambda (_p) lead)))
              (ghostherd-memory-page--handle `((op . "lead-say") (project . ,dir) (text . "again"))))
            (should (equal (ghostherd-test--call calls "flash") "Sent to lead-x"))
            (ghostherd-memory-page--handle '((op . "lead-say") (project . "/no/such/dir") (text . "x")))
            (should (string-match-p "No project" (ghostherd-test--call calls "showError")))
            (let ((a (seq-find (lambda (x) (equal (plist-get x :name) "lead-x"))
                               (append (plist-get (progn (ghostherd-memory-page--send-herd)
                                                         (ghostherd-test--call calls "renderHerd"))
                                                  :agents)
                                       nil))))
              (should (equal (plist-get a :role) "lead")))
            (ghostherd-test--with-clipboard ghostherd-test--png-sh
              (cl-letf (((symbol-function 'ghostherd-memory-page--thumb) #'ignore))
                (ghostherd-memory-page--handle '((op . "agent-image") (name . "@lead")))))
            (should (equal (plist-get (ghostherd-test--call calls "attachImage") :name) "@lead")))
        (remhash "lead-x" ghostherd--roles)
        (clrhash ghostherd--lead-unheard)
        (delete-directory dir t)))))

;;; A lead starts and stops agents through the herd (ask 7f220a2e)

(ert-deftest ghostherd-test-started-by-rides-the-recipe ()
  "started-by is kept with the recipe -- in the hash, on the host, and for a
respawn -- so a lead still owns an agent it started after a restart."
  (ghostherd-tests--with-herd
      ((ghostherd--roles (make-hash-table :test 'equal))
       (ghostherd--started-by (make-hash-table :test 'equal))
       (ghostherd--lineage (make-hash-table :test 'equal))
       (ghostherd-tests--fake-spawned nil))
    ;; Its paths are labels: nothing starts in them, so none is missing.
    (cl-letf (((symbol-function 'ghostherd--directory-missing-p) #'ignore)
              ((symbol-function 'ghostherd--install-hooks) #'ignore)
              ((symbol-function 'ghostherd--register-eval-cmds) #'ignore)
              ((symbol-function 'ghostherd--ensure-poll-timer) #'ignore)
              ((symbol-function 'ghostherd--sidebar-refresh) #'ignore))
      (let ((s (ghostherd-spawn 'grok :name "imp" :project "/p/" :backend 'fake
                                :started-by "lead-p" :display nil)))
        (should (equal (ghostherd-session-started-by s) "lead-p"))
        (should (equal (plist-get ghostherd-tests--fake-spawned :started-by) "lead-p"))
        (should (equal (plist-get (ghostherd-session-recipe s) :started-by) "lead-p"))))))

(ert-deftest ghostherd-test-lead-start-guards ()
  "Only a lead may start, only a real kind, and not at all when turned off."
  (ghostherd-tests--with-lead-start
    (let ((ghostherd-lead-may-start nil))
      (should (string-match-p "set not to start" (ghostherd-lead-start "lead-p" "grok"))))
    (should (string-match-p "no agent kind" (ghostherd-lead-start "lead-p" "zzz")))
    (puthash "lead-p" nil ghostherd--roles)
    (should (string-match-p "not a lead" (ghostherd-lead-start "lead-p" "grok")))))

(ert-deftest ghostherd-test-lead-start-caps-per-project ()
  "Past the per-lead cap the herd refuses and lists the ones already up."
  (ghostherd-tests--with-lead-start
    (dotimes (i 2)
      (ghostherd-tests--session :name (format "a%d" i) :kind 'grok :backend 'fake :project repo)
      (puthash (format "a%d" i) "lead-p" ghostherd--started-by))
    (let ((r (ghostherd-lead-start "lead-p" "grok" :new t)))
      (should (string-match-p "max 2" r))
      (should (string-match-p "a0" r))
      (should (string-match-p "a1" r)))))

(ert-deftest ghostherd-test-lead-start-refuses-a-used-up-kind ()
  "A used-up kind is refused, naming one with quota left."
  (ghostherd-tests--with-lead-start
    (cl-letf (((symbol-function 'ghostherd-usage-used-up)
               (lambda (k &optional _) (eq k 'grok))))
      (let ((r (ghostherd-lead-start "lead-p" "grok")))
        (should (string-match-p "used up" r))
        (should (string-match-p "claude\\|agy" r))))))

(ert-deftest ghostherd-test-lead-start-reuses-an-idle-merged-agent ()
  "An idle agent the lead started, merged and clean, is reused -- not started
anew -- its worktree fast-forwarded, its context kept."
  (ghostherd-tests--with-lead-start
    (let ((wt (ghostherd--worktree-create repo "old-imp")))
      (ghostherd-tests--session :name "old-imp" :kind 'grok :backend 'fake
                                :project repo :state 'idle)
      (puthash "old-imp" "lead-p" ghostherd--started-by)
      (puthash "old-imp" wt ghostherd--worktrees)
      (let ((r (ghostherd-lead-start "lead-p" "grok")))
        (should (string-match-p "reused old-imp" r))
        ;; it is the same agent, still alive -- not a fresh spawn
        (should (ghostherd-get "old-imp"))
        (should-not (ghostherd-get "grok-repo"))))))

(ert-deftest ghostherd-test-lead-start-spawns-a-worktree-agent ()
  "With the lead already in the main checkout, a new agent defaults to its own
worktree, off HEAD, carrying started-by."
  (ghostherd-tests--with-lead-start
    (let ((r (ghostherd-lead-start "lead-p" "grok" :new t)))
      (should (string-match-p "\\`started grok-repo (grok, worktree" r))
      (let ((s (ghostherd-get "grok-repo")))
        (should s)
        (should (equal (ghostherd-session-started-by s) "lead-p"))
        (should (ghostherd-session-worktree s))
        (should (file-directory-p (plist-get (ghostherd-session-worktree s) :path)))))))

(ert-deftest ghostherd-test-lead-start-no-worktree-override ()
  "--no-worktree runs the agent in the main checkout."
  (ghostherd-tests--with-lead-start
    (let ((r (ghostherd-lead-start "lead-p" "grok" :worktree "no" :new t)))
      (should (string-match-p "in the main checkout" r))
      (should-not (ghostherd-session-worktree (ghostherd-get "grok-repo"))))))

(ert-deftest ghostherd-test-lead-start-grok-gets-the-memory-rules ()
  "A grok agent is started with the shared-memory rules in --rules."
  (ghostherd-tests--with-lead-start
    (ghostherd-lead-start "lead-p" "grok" :worktree "no" :new t)
    (let ((args (plist-get ghostherd-tests--fake-spawned :args)))
      (should (member "--rules" args))
      (should (seq-some (lambda (a) (and (stringp a)
                                         (string-match-p "recall" a)))
                        args)))))

(ert-deftest ghostherd-test-lead-start-agy-is-told-the-memory-rules ()
  "agy has no rules flag, so it is told the memory rules as a first message."
  (ghostherd-tests--with-lead-start
    (let (told)
      (cl-letf (((symbol-function 'ghostherd--when-ready)
                 (lambda (_s fn &rest _) (funcall fn)))
                ((symbol-function 'ghostherd-prompt)
                 (lambda (s text &rest _) (push (cons (ghostherd-session-name s) text) told) s)))
        (ghostherd-lead-start "lead-p" "agy" :worktree "no" :new t)
        (should (string-match-p "recall" (or (cdr (assoc "agy-repo" told)) "")))))))

(ert-deftest ghostherd-test-claude-memory-link-is-a-noop-without-the-sidecar ()
  "The claude memory-link hook asks the sidecar for a claude session, and is a
no-op for another CLI or when no sidecar is up."
  (should (memq #'ghostherd--memory-link-claude ghostherd-session-created-hook))
  (let (calls)
    (cl-letf (((symbol-function 'ghostherd-memory-rpc-url) (lambda () nil))
              ((symbol-function 'ghostherd-memory-request-async)
               (lambda (&rest a) (push a calls))))
      (ghostherd--memory-link-claude
       (ghostherd-session--create :id "c" :name "c" :kind 'claude :project "/p/"))
      (should-not calls))                ; no sidecar -> no call
    (cl-letf (((symbol-function 'ghostherd-memory-rpc-url) (lambda () "http://x"))
              ((symbol-function 'ghostherd-memory-request-async)
               (lambda (&rest a) (push a calls))))
      (ghostherd--memory-link-claude
       (ghostherd-session--create :id "g" :name "g" :kind 'grok :project "/p/"))
      (should-not calls)                 ; not claude -> no call
      (ghostherd--memory-link-claude
       (ghostherd-session--create :id "c" :name "c" :kind 'claude :project "/p/"))
      (should calls)                     ; claude + sidecar -> asks to link
      (should (equal (nth 0 (car calls)) "memory_link_claude")))))

(defun ghostherd-tests--write-memory-index (dir project lines)
  "Write LINES as PROJECT's generated MEMORY.md under store root DIR."
  (let ((store (expand-file-name (ghostherd--memory-slug project) dir)))
    (make-directory store t)
    (with-temp-file (expand-file-name "MEMORY.md" store)
      (insert "# Memory\n\n## This project\n\n" (string-join lines "\n") "\n"))))

(ert-deftest ghostherd-test-memory-opening-embeds-the-index ()
  "A grok/agy opening carries the index itself (one line per fact), not only
the instruction to recall."
  (let* ((tmp (make-temp-file "gh-mem" t))
         (ghostherd-memory-dir tmp)
         (proj "/work/proj/"))
    (unwind-protect
        (progn
          (ghostherd-tests--write-memory-index
           tmp proj '("- [Never commit to main](branch.md) — use a branch"
                      "- [Retry](retry.md) — worker.py"))
          (let ((op (ghostherd--memory-opening proj)))
            (should (string-match-p "recall" op))               ; the rule stays
            (should (string-match-p "Never commit to main" op)) ; the index is in it
            (should (string-match-p "retry.md" op))))
      (delete-directory tmp t))))

(ert-deftest ghostherd-test-memory-opening-is-just-the-rule-without-a-store ()
  "No store yet -> just the rule, no empty index block."
  (let ((ghostherd-memory-dir (make-temp-file "gh-mem" t)))
    (should (equal (ghostherd--memory-opening "/no/such/proj/") ghostherd--memory-rules))))

(ert-deftest ghostherd-test-memory-opening-caps-a-huge-index ()
  "Over the cap, the index is truncated with a note pointing to recall."
  (let* ((tmp (make-temp-file "gh-mem" t))
         (ghostherd-memory-dir tmp)
         (ghostherd-memory-opening-max 400)
         (proj "/work/big/"))
    (unwind-protect
        (progn
          (ghostherd-tests--write-memory-index
           tmp proj (cl-loop for i from 1 to 100
                             collect (format "- [Fact %d](f%d.md) — a reasonably long gloss" i i)))
          (let* ((op (ghostherd--memory-opening proj))
                 (index (substring op (string-match "One line per fact" op))))
            ;; Whole lines only, and how many were left out -- never a
            ;; fact cut short or dropped without a word.
            (should (cl-every (lambda (l) (string-match-p "\\`- \\[Fact [0-9]+\\](f[0-9]+\\.md) — a reasonably long gloss\\'" l))
                              (seq-filter (lambda (l) (string-prefix-p "- " l))
                                          (split-string op "\n"))))
            (let ((shown (seq-count (lambda (l) (string-prefix-p "- " l)) (split-string op "\n"))))
              (should (> shown 0))
              (should (string-match-p (format "^%d more, not listed" (- 100 shown)) op)))
            (should (<= (length (string-trim-right index)) (+ 400 (length "One line per fact:\n\n"))))))
      (delete-directory tmp t))))

(defmacro ghostherd-tests--with-opening-sidecar (reply &rest body)
  "Run BODY with the sidecar answering `memory_opening' with REPLY (a
plist, or `error' to fail), its calls collected in `asked'."
  (declare (indent 1))
  `(let ((asked nil))
     (cl-letf (((symbol-function 'ghostherd-memory-rpc-url) (lambda () "http://x"))
               ((symbol-function 'ghostherd-memory-request)
                (lambda (method params &optional timeout)
                  (push (list method params timeout) asked)
                  (if (eq ,reply 'error) (error "sidecar down") ,reply))))
       ,@body)))

(defconst ghostherd-tests--composed-opening
  (concat "Its rules, in full -- follow them:\n\n"
          "### Branch discipline  (feedback)\nNever commit or push to main.\n\n"
          "The other facts, one line each, newest first:\n\n"
          "- [Deploy trigger](deploy-trigger.md) — the three entry points\n"
          "3 more, not listed — \"$GHOSTHERD_HERD\" recall <topic> for any: a, b, c\n")
  "An opening as the sidecar composes it (shared_memory.opening).")

(ert-deftest ghostherd-test-memory-opening-is-the-sidecars-composition ()
  "With the sidecar up, the opening is what it composes -- the rules in full,
the lines, the names of the rest -- for the project's main checkout, within
the cap this Emacs sets; and that is what grok's --rules carries."
  (let ((ghostherd-memory-opening-max 1234)
        (ghostherd-memory-opening-timeout 2)
        (ghostherd-memory-dir (make-temp-file "gh-mem" t)))
    (ghostherd-tests--with-opening-sidecar (list :text ghostherd-tests--composed-opening
                                                 :full ["branch-discipline"])
      (let ((op (ghostherd--memory-opening "~/work/proj/")))
        (should (string-prefix-p ghostherd--memory-rules op))
        (should (string-search "Never commit or push to main." op))
        (should (string-search "3 more, not listed" op))
        (pcase-let ((`(,method ,params ,timeout) (car asked)))
          (should (equal method "memory_opening"))
          (should (equal (plist-get params :project) (expand-file-name "~/work/proj/")))
          (should (= (plist-get params :max) 1234))
          (should-not (plist-member params :agent))
          (should (= timeout 2))))
      (let ((rules (cadr (member "--rules" (ghostherd--args-with-memory "/p/" nil)))))
        (should (string-search "### Branch discipline" rules)))
      ;; Named, the agent starting is who the sidecar counts the rules as
      ;; reaching (ask 17f83e5d).
      (ghostherd--args-with-memory "/p/" nil "grok-p")
      (should (equal (plist-get (cadr (car asked)) :agent) "grok-p"))
      (ghostherd--memory-opening "/p/" "agy-p")
      (should (equal (plist-get (cadr (car asked)) :agent) "agy-p")))))

(ert-deftest ghostherd-test-a-spawned-grok-and-agy-name-themselves-to-the-opening ()
  "A grok's --rules and an agy's first message ask for the opening as the
agent starting, so its rules count as reaching that agent."
  (let (asked)
    (cl-letf (((symbol-function 'ghostherd--memory-opening)
               (lambda (p &optional a) (push (list p a) asked) "MEMORY"))
              ((symbol-function 'ghostherd--when-ready) (lambda (_s f &rest _) (funcall f)))
              ((symbol-function 'ghostherd-prompt) #'ignore)
              ((symbol-function 'ghostherd--agy-guard-mode) (lambda (_d) nil)))
      (ghostherd-tests--with-herd ()
        (ghostherd-spawn 'grok :name "grok-p" :project "/tmp/" :directory "/tmp/" :backend 'fake :display nil)
        (should (equal (cadr (car asked)) "grok-p"))
        (ghostherd-spawn 'agy :name "agy-p" :project "/tmp/" :directory "/tmp/" :backend 'fake :display nil)
        (should (equal (cadr (car asked)) "agy-p"))))))

(ert-deftest ghostherd-test-the-memory-tab-gets-each-facts-uses ()
  "The facts go to the page with their uses, and since when the herd counts
them -- nothing when the sidecar does not say."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let ((ghostherd-memory-page--memory-key "-k")
        (ghostherd-memory-page--memory-sig nil)
        (result (list :key "-k" :uses_since "2026-10-26T00:00:00+00:00" :uses_days 30
                      :facts (list (list :name "a" :title "A" :uses (list :ask 2 :recall 1 :opening 0 :used 3
                                                                          :last "2026-10-26T01:00:00+00:00"
                                                                          :last_opening nil :total 3)))))
        pushed)
    (cl-letf (((symbol-function 'xwapp-live-p) (lambda (&rest _) t))
              ((symbol-function 'ghostherd-memory-page--memory-proposals-tick) #'ignore)
              ((symbol-function 'ghostherd-memory-page--memory-checks-tick) #'ignore)
              ((symbol-function 'ghostherd-memory-request-async)
               (lambda (method cb &rest _) (when (equal method "herd_memory_list") (funcall cb result))))
              ((symbol-function 'ghostherd-memory-page--js) (lambda (fn obj) (push (cons fn obj) pushed))))
      (ghostherd-memory-page--memory-tick)
      (let ((json (json-encode (cdr (assoc "renderMemory" pushed)))))
        (should (string-search "\"usesSince\":\"2026-10-26T00:00:00+00:00\"" json))
        (should (string-search "\"usesDays\":30" json))
        (should (string-search "\"uses\":{\"ask\":2,\"recall\":1,\"opening\":0,\"used\":3" json)))
      ;; An older sidecar: no counts, and the page is told none.
      (setq pushed nil ghostherd-memory-page--memory-sig nil
            result (list :key "-k" :facts (list (list :name "a" :title "A"))))
      (ghostherd-memory-page--memory-tick)
      (should (string-search "\"usesSince\":null" (json-encode (cdr (assoc "renderMemory" pushed))))))))

(ert-deftest ghostherd-test-memory-opening-of-an-empty-store-is-the-rule ()
  "The sidecar finds no fact: just the rule to recall, no empty block --
not a stale MEMORY.md left on disk."
  (let ((ghostherd-memory-dir (make-temp-file "gh-mem" t)))
    (ghostherd-tests--write-memory-index ghostherd-memory-dir "/p/" '("- [Stale](s.md) — gone"))
    (ghostherd-tests--with-opening-sidecar (list :text "" :full [])
      (should (equal (ghostherd--memory-opening "/p/") ghostherd--memory-rules)))))

(ert-deftest ghostherd-test-memory-opening-falls-back-to-the-index ()
  "The sidecar does not answer: the index lines from MEMORY.md instead,
whole, within the cap, with how many were left out."
  (let* ((tmp (make-temp-file "gh-mem" t))
         (ghostherd-memory-dir tmp)
         (ghostherd-memory-opening-max 300)
         (proj "/work/fb/"))
    (unwind-protect
        (progn
          (ghostherd-tests--write-memory-index
           tmp proj (cl-loop for i from 1 to 20
                             collect (format "- [Fact %d](f%d.md) — gloss number %d" i i i)))
          (ghostherd-tests--with-opening-sidecar 'error
            (let* ((op (ghostherd--memory-opening proj))
                   (lines (seq-filter (lambda (l) (string-prefix-p "- " l)) (split-string op "\n"))))
              (should asked)                       ; it asked first
              (should (string-search "Fact 1]" op))
              (should (cl-every (lambda (l) (string-match-p "\\`- \\[Fact \\([0-9]+\\)\\](f\\1\\.md) — gloss number \\1\\'" l))
                                lines))
              (should (string-match-p (format "^%d more, not listed" (- 20 (length lines))) op))
              (should (<= (length (substring op (string-match "One line per fact" op)))
                          (+ 300 (length "One line per fact:\n\n")))))))
      (delete-directory tmp t))))

(ert-deftest ghostherd-test-spawn-grok-gets-the-index-in-rules ()
  "Any grok spawn -- not only `herd start' -- gets the index in --rules."
  (let* ((tmp (make-temp-file "gh-mem" t))
         (ghostherd-memory-dir tmp))
    (unwind-protect
        (progn
          (ghostherd-tests--write-memory-index tmp "/p/" '("- [Fact A](a.md) — gloss"))
          (ghostherd-tests--with-herd
              ((ghostherd--roles (make-hash-table :test 'equal))
               (ghostherd--started-by (make-hash-table :test 'equal))
               (ghostherd--lineage (make-hash-table :test 'equal))
               (ghostherd-tests--fake-spawned nil))
            ;; Its paths are labels: nothing starts in them, so none is missing.
            (cl-letf (((symbol-function 'ghostherd--directory-missing-p) #'ignore)
                      ((symbol-function 'ghostherd--install-hooks) #'ignore)
                      ((symbol-function 'ghostherd--register-eval-cmds) #'ignore)
                      ((symbol-function 'ghostherd--ensure-poll-timer) #'ignore)
                      ((symbol-function 'ghostherd--sidebar-refresh) #'ignore))
              (ghostherd-spawn 'grok :name "g" :project "/p/" :backend 'fake :display nil)
              (let* ((args (plist-get ghostherd-tests--fake-spawned :args))
                     (rules (cadr (member "--rules" args))))
                (should (member "--rules" args))
                (should (string-match-p "Fact A" rules))
                (should (string-match-p (regexp-quote ghostherd--memory-mark) rules))))))
      (delete-directory tmp t))))

(ert-deftest ghostherd-test-grok-rules-replace-a-stale-memory-block ()
  "A re-launch whose args already carry a memory block replaces it, keeping
role text before it -- never two blocks, always the current index."
  (let* ((tmp (make-temp-file "gh-mem" t))
         (ghostherd-memory-dir tmp))
    (unwind-protect
        (progn
          (ghostherd-tests--write-memory-index tmp "/p/" '("- [New](n.md) — now"))
          (let* ((stale (concat "role text\n\n" ghostherd--memory-mark "\nOLD INDEX"))
                 (out (ghostherd--args-with-memory "/p/" (list "--rules" stale)))
                 (rules (cadr (member "--rules" out))))
            (should (string-match-p "role text" rules))        ; role kept
            (should (string-match-p "New" rules))              ; current index
            (should-not (string-match-p "OLD INDEX" rules))    ; stale replaced
            (should (= 2 (length (split-string rules (regexp-quote ghostherd--memory-mark)))))))
      (delete-directory tmp t))))

(ert-deftest ghostherd-test-spawn-agy-is-told-the-index ()
  "Any agy spawn is told the opening, with the index, as a first message."
  (let* ((tmp (make-temp-file "gh-mem" t))
         (ghostherd-memory-dir tmp) (told nil))
    (unwind-protect
        (progn
          (ghostherd-tests--write-memory-index tmp "/p/" '("- [Fact B](b.md) — gloss"))
          (ghostherd-tests--with-herd
              ((ghostherd--roles (make-hash-table :test 'equal))
               (ghostherd--started-by (make-hash-table :test 'equal))
               (ghostherd--lineage (make-hash-table :test 'equal))
               (ghostherd-tests--fake-spawned nil))
            ;; Its paths are labels: nothing starts in them, so none is missing.
            (cl-letf (((symbol-function 'ghostherd--directory-missing-p) #'ignore)
                      ((symbol-function 'ghostherd--install-hooks) #'ignore)
                      ((symbol-function 'ghostherd--register-eval-cmds) #'ignore)
                      ((symbol-function 'ghostherd--ensure-poll-timer) #'ignore)
                      ((symbol-function 'ghostherd--sidebar-refresh) #'ignore)
                      ((symbol-function 'ghostherd--when-ready) (lambda (_s fn &rest _) (funcall fn)))
                      ((symbol-function 'ghostherd-prompt) (lambda (_s text &rest _) (push text told))))
              (ghostherd-spawn 'agy :name "a" :project "/p/" :backend 'fake :display nil)
              (should (string-match-p "Fact B" (or (car told) "")))
              (should (string-match-p "recall" (or (car told) ""))))))
      (delete-directory tmp t))))

(ert-deftest ghostherd-test-lead-stop-guards-and-cleans ()
  "Stop only an agent this lead started, not one owing an ask; a merged, clean
worktree is removed."
  (ghostherd-tests--with-lead-start
    (ghostherd-tests--session :name "imp" :kind 'grok :backend 'fake :project repo)
    (should (string-match-p "did not start" (ghostherd-lead-stop "lead-p" "imp")))
    (puthash "imp" "lead-p" ghostherd--started-by)
    (setq ghostherd--herd-asks (list (list :to "imp" :from "x" :status "delivered")))
    (should (string-match-p "open or queued ask" (ghostherd-lead-stop "lead-p" "imp")))
    (setq ghostherd--herd-asks nil)
    (should (string-match-p "\\`stopped imp" (ghostherd-lead-stop "lead-p" "imp")))
    (should-not (ghostherd-get "imp"))))

(ert-deftest ghostherd-test-lead-stop-removes-a-merged-worktree ()
  "A stopped agent's worktree, merged and clean, is removed and said to be."
  (ghostherd-tests--with-lead-start
    (let ((wt (ghostherd--worktree-create repo "imp")))
      (ghostherd-tests--session :name "imp" :kind 'grok :backend 'fake :project repo)
      (puthash "imp" "lead-p" ghostherd--started-by)
      (puthash "imp" wt ghostherd--worktrees)
      (should (file-directory-p (plist-get wt :path)))
      (let ((r (ghostherd-lead-stop "lead-p" "imp")))
        (should (string-match-p "merged and clean, so it was removed" r))
        (should-not (file-directory-p (plist-get wt :path)))))))

(ert-deftest ghostherd-test-worktree-attributed-to-main-checkout ()
  "An agent in a project's git worktree belongs to the project, not a new
one: `ghostherd--worktree-main-checkout' collapses a linked worktree to
its main checkout -- nil for the main checkout itself or a plain directory
-- and `ghostherd--project-root' of a worktree is the main checkout."
  (ghostherd-tests--with-repo
    (let ((wt (concat base "wt/"))
          (plain (concat base "plain/")))
      (make-directory plain t)
      (funcall git repo "worktree" "add" "-q" "-b" "feat/y" (directory-file-name wt))
      (should (equal (directory-file-name (file-truename (ghostherd--worktree-main-checkout wt)))
                     (directory-file-name (file-truename repo))))
      (should-not (ghostherd--worktree-main-checkout repo))
      (should-not (ghostherd--worktree-main-checkout plain))
      (should (equal (directory-file-name (file-truename (ghostherd--project-root wt)))
                     (directory-file-name (file-truename repo)))))))

(ert-deftest ghostherd-test-worktree-attribution-for-bare-and-submodule ()
  "A bare repo's worktrees group under the bare repo (it has no main
checkout, so the common dir's parent is wrong); a submodule's worktree
belongs to the submodule's own working tree, not the superproject's
.git/modules it is folded under."
  (ghostherd-tests--with-repo
    (let* ((truename (lambda (d) (directory-file-name (file-truename d))))
           (git* (lambda (dir &rest args)
                   (let ((default-directory dir))
                     (apply #'call-process "git" nil nil nil
                            "-c" "user.email=t@t" "-c" "user.name=t"
                            "-c" "protocol.file.allow=always" args)))))
      ;; ---- A bare repo and a worktree of it ----
      (let ((bare (concat base "bare.git/"))
            (bare-wt (concat base "bare-wt/")))
        (funcall git* base "clone" "-q" "--bare" (directory-file-name repo) (directory-file-name bare))
        (funcall git* bare "worktree" "add" "-q" (directory-file-name bare-wt) "main")
        (should (equal (funcall truename (ghostherd--worktree-main-checkout bare-wt))
                       (funcall truename bare)))
        (should (equal (funcall truename (ghostherd--project-root bare-wt))
                       (funcall truename bare))))
      ;; ---- A submodule and a linked worktree of it ----
      (let ((super (concat base "super/"))
            (sub (concat base "sub/"))
            (sub-wt (concat base "sub-wt/"))
            (subco (concat base "super/libs/foo/")))
        (make-directory super t) (make-directory sub t)
        (dolist (r (list super sub))
          (funcall git* r "init" "-q" "-b" "main")
          (funcall git* r "commit" "-q" "--allow-empty" "-m" "init"))
        (funcall git* super "submodule" "add" "-q" (directory-file-name sub) "libs/foo")
        (funcall git* super "commit" "-q" "-m" "addsub")
        (funcall git* subco "worktree" "add" "-q" (directory-file-name sub-wt))
        ;; Its own working tree, not .git/modules.
        (should (equal (funcall truename (ghostherd--worktree-main-checkout sub-wt))
                       (funcall truename subco)))
        (should (equal (funcall truename (ghostherd--project-root sub-wt))
                       (funcall truename subco)))))))

(ert-deftest ghostherd-test-relay-brief-carries-the-worktree-branch-state ()
  "A relay brief adds where a worktree branch stands against its base --
base, ahead/behind, its own commits, a diffstat, what is uncommitted --
so an heir to a diverged worktree is not left with only the transcript; no
full diff.  Nil for an agent in no worktree."
  (ghostherd-tests--with-repo
    (let ((wt (concat base "wt/"))
          (ghostherd--worktrees (make-hash-table :test 'equal)))
      (funcall git repo "worktree" "add" "-q" "-b" "feat/x" (directory-file-name wt))
      (with-temp-file (expand-file-name "a.txt" wt) (insert "one\n"))
      (funcall git wt "add" "a.txt")
      (funcall git wt "commit" "-q" "-m" "feat: add a")
      (with-temp-file (expand-file-name "a.txt" wt) (insert "one\ntwo\n"))
      (funcall git wt "commit" "-q" "-am" "feat: extend a")
      (with-temp-file (expand-file-name "new.txt" wt) (insert "scratch\n"))
      (let ((s (ghostherd-session--create :id "rd" :name "rd" :kind 'claude :project repo)))
        (puthash "rd" (list :path wt :branch "feat/x" :base "main") ghostherd--worktrees)
        (let ((brief (ghostherd--worktree-brief s)))
          (should brief)
          (should (string-match-p "branch feat/x, forked from main" brief))
          (should (string-match-p "2 commit(s) ahead of main, 0 behind" brief))
          (should (string-match-p "feat: add a" brief))
          (should (string-match-p "feat: extend a" brief))
          (should (string-match-p "a\\.txt" brief))          ; the diffstat
          (should (string-match-p "Uncommitted in the worktree" brief))
          (should (string-match-p "new\\.txt" brief))        ; git status
          (should-not (string-match-p "diff --git" brief)))  ; no full diff
        (clrhash ghostherd--worktrees)
        (should-not (ghostherd--worktree-brief s))))))

(ert-deftest ghostherd-test-worktree-create-makes-a-branch-off-head ()
  "`ghostherd--worktree-create' adds a git worktree under
`ghostherd-worktree-root' on a branch named for the agent, off the main
checkout's HEAD; a second at the same path refuses; a non-git dir refuses."
  (ghostherd-tests--with-repo
    (let ((ghostherd-worktree-root (concat base "trees/")))
      (let ((wt (ghostherd--worktree-create repo "alice")))
        (should (equal (plist-get wt :branch) "alice"))
        (should (equal (plist-get wt :base) "main"))
        (should (file-directory-p (plist-get wt :path)))
        (should (string-prefix-p (file-truename ghostherd-worktree-root)
                                 (file-truename (plist-get wt :path))))
        (should (equal (with-temp-buffer
                         (let ((default-directory (plist-get wt :path)))
                           (call-process "git" nil '(t nil) nil "symbolic-ref" "--short" "HEAD"))
                         (string-trim (buffer-string)))
                       "alice"))
        (should-error (ghostherd--worktree-create repo "alice") :type 'user-error))
      (should-error (ghostherd--worktree-create base "bob") :type 'user-error))))

;;; Handoffs: work for another project's lead (ask 2f40d99a)

(ert-deftest ghostherd-test-handoff-starts-a-lead-in-a-checkout ()
  "A project with no lead gets one, with the lead's role, its notes saying
for whom; it counts against the cap, and is pushed while you are away --
the same rules as any push.  The next handoff finds it: no second lead."
  (ghostherd-tests--with-handoff
    (let ((r (ghostherd-handoff-lead "lead-p" other repo "Rename token")))
      (should (equal (car r) "lead-other"))
      (should (string-match-p "\\`started lead-other (claude) in .*/other/ for a handoff from lead-p in repo\\'"
                              (cdr r)))
      (let ((lead (ghostherd-get "lead-other")))
        (should (equal (ghostherd-session-role lead) "lead"))
        (should (equal (ghostherd--session-project-name lead) (ghostherd--project-name other)))
        (should (string-match-p "started for a handoff from lead-p in repo" (ghostherd-session-notes lead)))
        (should (equal (last (plist-get ghostherd-tests--fake-spawned :args) 2)
                       (list "--append-system-prompt" ghostherd--lead-role)))
        (should-not (gethash "lead-other" ghostherd--role-owed))
        (should (equal (gethash "lead-other" ghostherd--handoff-leads) "lead-p"))
        ;; Not the sender's to stop: it did not start it through `herd start'.
        (should-not (ghostherd-session-started-by lead))
        (should (string-match-p "did not start" (ghostherd-lead-stop "lead-p" "lead-other"))))
      (should (equal (mapcar (lambda (p) (list (plist-get p :kind) (plist-get p :session) (plist-get p :by)
                                               (plist-get p :from) (plist-get p :head)))
                             ghostherd--herd-pushes)
                     `(("lead-started" "lead-other" "lead-p" ,(ghostherd--project-name repo) "Rename token"))))
      (should (equal (ghostherd-handoff-lead "lead-p" other repo "more")
                     (cons "lead-other" "handed to lead-other, other's lead")))
      ;; From a plain shell: the user's.
      (let ((third (concat base "third/")))
        (make-directory third t)
        (funcall git third "init" "-q" "-b" "main")
        (should (string-suffix-p "for a handoff from the user" (cdr (ghostherd-handoff-lead "user" third nil "x"))))
        (should (string-suffix-p "for a handoff from the user"
                                 (ghostherd-session-notes (ghostherd-get "lead-third")))))
      ;; Named by a subdirectory, it is still that project's lead.
      (make-directory (concat other "src") t)
      (should (equal (car (ghostherd-handoff-lead "lead-p" (concat other "src") repo "x")) "lead-other"))
      (should (= (length (ghostherd-sessions)) 3)))))

(ert-deftest ghostherd-test-handoff-push-keeps-the-away-rules ()
  "At the desk, looking at the herd page, the start is not pushed."
  (ghostherd-tests--with-handoff
    (let ((ghostherd-present-functions (list (lambda () t))))
      (should (car (ghostherd-handoff-lead "lead-p" other repo "x")))
      (should-not ghostherd--herd-pushes))))

(ert-deftest ghostherd-test-handoff-takes-the-projects-kind-and-owes-the-role ()
  "The project's own default kind (its .dir-locals.el) runs its lead.  An
agy takes no role as it starts: its first message carries it -- an ask
or the user's -- and only the first."
  (ghostherd-tests--with-handoff
    (with-temp-file (concat other ".dir-locals.el")
      (insert "((nil . ((ghostherd-project-kind . agy))))\n"))
    (let ((said nil))
      (cl-letf (((symbol-function 'ghostherd-message)
                 (lambda (_from _to text &rest _) (push text said)))
                ((symbol-function 'ghostherd-prompt)
                 (lambda (_s text &rest _) (push text said))))
        (should (string-match-p "(agy)" (cdr (ghostherd-handoff-lead "lead-p" other repo "x"))))
        (let ((lead (ghostherd-get "lead-other")))
          (should (eq (ghostherd-session-kind lead) 'agy))
          (should-not (member ghostherd--lead-role (plist-get ghostherd-tests--fake-spawned :args)))
          (should (gethash "lead-other" ghostherd--role-owed))
          ;; After the ask, so the room's chat still reads it as one.
          (ghostherd--ask-deliver "k1" "lead-p" "lead-other" "the brief")
          (should (string-prefix-p "[ask k1 " (car said)))
          (should (string-suffix-p (concat "as it starts:\n\n" ghostherd--lead-role) (car said)))
          (ghostherd--ask-deliver "k2" "lead-p" "lead-other" "more")
          (should (string-prefix-p "[ask k2 " (car said)))
          (should-not (string-search ghostherd--lead-role (car said)))
          ;; A lead taking an ask is told to hand on what another project needs.
          (should (string-suffix-p "(\"$GHOSTHERD_HERD\" handoff PROJECT)." (car said)))
          ;; Owed and not yet told: the user's first message carries it instead.
          (puthash "lead-other" ghostherd--lead-role ghostherd--role-owed)
          (ghostherd-tell lead "hello")
          (should (string-prefix-p ghostherd--lead-role (car said)))
          (ghostherd-tell lead "again")
          (should (equal (car said) "again")))))))

(ert-deftest ghostherd-test-handoff-in-the-words-agents-read ()
  "A lead is told when and how to hand off, and how to take one in; every
taker of an ask, what to do with work another project needs -- a lead
hands it on, any other agent puts it in its answer; the protocol has it."
  (dolist (words '("do not edit its checkout"
                   "\"$GHOSTHERD_HERD\" handoff PROJECT --wait"
                   "what you found; the evidence -- files and\nlines, logs, tickets, commits; what is needed of that project; how it\nwill know it is done; and who waits on it"
                   "Your agents do not hand off"
                   "Take it in as your own work -- plan it here"
                   "answer with the result"))
    (should (string-search words ghostherd--lead-role)))
  (let ((lead (ghostherd--ask-text "a1" "x" nil t))
        (other (ghostherd--ask-text "a1" "x")))
    (should (string-suffix-p "hand it to that project's lead (\"$GHOSTHERD_HERD\" handoff PROJECT)." lead))
    (should (string-suffix-p "say it in your answer -- what, where, why -- and whoever asked takes it on." other))
    (dolist (text (list lead other))
      (should (string-search "not yours to make in its checkout" text))))
  (should (string-search "herd_handoff\n{project, body, from, wait, rounds}" ghostherd--herd-protocol)))

(ert-deftest ghostherd-test-handoff-refusals ()
  "No directory, not a git checkout, the herd set not to, the sender's own
project, a dead lead, and past the cap -- each refused, saying why."
  (ghostherd-tests--with-handoff
    (let ((refused (lambda (project &optional from)
                     (let ((r (ghostherd-handoff-lead "lead-p" project (or from repo) "x")))
                       (should-not (car r))
                       (cdr r)))))
      (should (string-match-p "\\`no directory .*/nope/" (funcall refused (concat base "nope/"))))
      (should (string-match-p "plain/ is not a git checkout, and has no lead" (funcall refused plain)))
      (let ((ghostherd-handoff-may-start-lead nil))
        (should (string-match-p "ghostherd-handoff-may-start-lead is off" (funcall refused other))))
      (should (string-match-p "repo is lead-p's own project" (funcall refused repo)))
      (should-not ghostherd--herd-pushes)
      (should (= (length (ghostherd-sessions)) 1))
      ;; The cap: leads the herd started for handoffs and are still up.
      (let ((ghostherd-handoff-lead-max 1))
        (should (car (ghostherd-handoff-lead "lead-p" other repo "x")))
        (let ((third (concat base "third/")))
          (make-directory third t)
          (funcall git third "init" "-q" "-b" "main")
          (should (string-match-p "the herd has 1 up that it started for handoffs (max 1): lead-other"
                                  (funcall refused third)))
          ;; Its lead gone, there is room again; a dead one is said plainly.
          (setf (ghostherd-session-state (ghostherd-get "lead-other")) 'dead)
          (should (string-match-p "lead-other, other's lead, is dead" (funcall refused other)))
          (should (equal (car (ghostherd-handoff-lead "lead-p" third repo "x")) "lead-third")))))))

(ert-deftest ghostherd-test-handoff-command-names-the-lead ()
  "The sidecar's `handoff' command answers with the lead it went to, as
:lead, or only why not."
  (ghostherd-tests--with-handoff
    (let ((run (lambda (project)
                 (ghostherd--herd-run-command
                  (list :id "c1" :op "handoff" :session "lead-p"
                        :args (list :by "lead-p" :project project
                                    :from_project (ghostherd--project-name repo) :head "x"))))))
      (let ((r (funcall run (ghostherd--project-name other))))
        (should (equal (plist-get r :id) "c1"))
        (should (equal (plist-get r :lead) "lead-other"))
        (should (string-prefix-p "started lead-other" (plist-get r :text))))
      (let ((r (funcall run plain)))
        (should-not (plist-member r :lead))
        (should (string-match-p "not a git checkout" (plist-get r :text)))))))

(ert-deftest ghostherd-test-handoff-on-the-page-and-in-the-log ()
  "The page gets the projects a handoff went to and came from; one that
failed before it had a taker is logged under its asker and pushed with
the lead it was for."
  ;; Loaded here, outside the bindings below: loading it adds to hooks.
  (skip-unless (require 'ghostherd-memory-page nil t))
  (ghostherd-tests--with-handoff
    (let ((ghostherd--asks-open (make-hash-table :test 'equal))
          (ghostherd--asks-settled (make-hash-table :test 'equal))
          (ghostherd--herd-asks nil)
          (ghostherd--herd-asks-taken t)
          (ask (lambda (status)
                 (list :id "h1" :from "lead-p" :to "" :status status :head "Rename token"
                       :error (if (equal status "failed") "not a git checkout" "")
                       :project "~/src/other/" :from_project "~/src/repo/"))))
      (let ((k (ghostherd-memory-page--ask (funcall ask "queued"))))
        (should (equal (list (plist-get k :project) (plist-get k :fromProject))
                       '("~/src/other/" "~/src/repo/")))
        (should (json-encode k)))
      (ghostherd--herd-take-asks (list (funcall ask "queued")))
      (ghostherd--herd-take-asks (list (funcall ask "failed")))
      (should (equal (mapcar (lambda (p) (list (plist-get p :kind) (plist-get p :session) (plist-get p :to)))
                             ghostherd--herd-pushes)
                     '(("ask-failed" "lead-p" "other's lead"))))
      (should (equal (mapcar #'ghostherd-log-entry-session ghostherd--log) '("lead-p"))))))

(provide 'ghostherd-lead-tests)
;;; ghostherd-lead-tests.el ends here

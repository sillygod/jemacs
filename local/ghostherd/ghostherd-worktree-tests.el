;;; ghostherd-worktree-tests.el --- Tests for ghostherd-worktree.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Part of ghostherd's tests; ghostherd-tests.el runs them all.  This
;; file alone:
;;
;;   emacs --batch --init-directory=~/.emacs.d/emacs-home/ \
;;         -L . -L ../xwapp -l ghostherd-worktree-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ghostherd-test-helpers)

(declare-function ghostherd-inbox--agent-items "ghostherd-memory-page" (a))
(declare-function ghostherd-memory-page--agent "ghostherd-memory-page" (s))
(declare-function ghostherd-memory-page--agent-new "ghostherd-memory-page" (intent))
(declare-function ghostherd-memory-page--agent-work "ghostherd-memory-page" (s))
(declare-function ghostherd-memory-page--handle "ghostherd-memory-page" (intent))
(declare-function ghostherd-memory-page--projects-work "ghostherd-memory-page" ())
(declare-function ghostherd-memory-page--size-parts-of "ghostherd-memory-page" (s))
(declare-function ghostherd-memory-page--size-where "ghostherd-memory-page" (sizes deps))
(declare-function ghostherd-memory-page--strays-look "ghostherd-memory-page" (project))
(declare-function ghostherd-memory-page--worktree-deps "ghostherd-memory-page" (s))
(declare-function ghostherd-memory-page--worktree-size "ghostherd-memory-page" (id))
(defvar ghostherd-memory-page--size-parts-most)

;;; Trust for the worktrees the herd creates (ask 2befbbd6)

(defmacro ghostherd-tests--with-trust (&rest body)
  "BODY in a clean herd over `with-repo''s temp `repo', worktrees under
`base', and CLI trust stores of its own under `base' -- `grok-store',
`agy-store' -- with the herd's record of what it trusted, and its log."
  (declare (indent 0) (debug t))
  `(ghostherd-tests--with-repo
     (let* ((grok-store (concat base "home/.grok/trusted_folders.toml"))
            (agy-store (concat base "home/.gemini/antigravity-cli/settings.json"))
            (client (expand-file-name "bin/herd" (file-name-directory (locate-library "ghostherd")))))
       (ghostherd-tests--with-herd
           ((ghostherd-worktree-root (concat base "trees/"))
            (ghostherd-trust-stores `((grok . ,grok-store) (agy . ,agy-store)))
            (ghostherd-pretrust-file (concat base "pretrust.eld")))
         (ghostherd-tests--with-log
           (cl-letf (((symbol-function 'ghostherd-herd-client) (lambda () client)))
             (make-directory (file-name-directory grok-store) t)
             (make-directory (file-name-directory agy-store) t)
             ,@body))))))

(defun ghostherd-tests--file (file)
  "FILE's text, or nil when there is none."
  (and (file-exists-p file)
       (with-temp-buffer (insert-file-contents file) (buffer-string))))

(ert-deftest ghostherd-test-a-new-worktree-is-trusted-where-its-cli-trusts-the-project ()
  "A worktree the herd creates for a grok or agy agent, off a project that
CLI trusts by its own store, is recorded trusted there -- logged, and
remembered -- so the agent does not start on the trust prompt.  Its
removal drops the entry, the store as it was."
  (ghostherd-tests--with-trust
    (let ((main (directory-file-name repo)))
      (dolist (case `((grok ,grok-store
                            ,(format "[folders.%S]\ntrusted = true\ndecided_at = 1792824400\n" main))
                      (agy ,agy-store
                           ,(format "{\n  \"model\": \"Gemini 4 Argon (High)\",\n  \"trustedWorkspaces\": [\n    %S\n  ]\n}\n"
                                    (concat main "/")))))
        (pcase-let ((`(,kind ,store ,before) case))
          (with-temp-file store (insert before))
          (let* ((name (format "%s-rd" kind))
                 (wt (ghostherd--worktree-create repo name nil kind))
                 (path (directory-file-name (file-truename (plist-get wt :path)))))
            (should (string-search path (ghostherd-tests--file store)))
            (should (equal (mapcar (lambda (e) (plist-get e :path)) (ghostherd--pretrusted))
                           (list path)))
            (should (string-match-p (format "trusted .*%s for %s, as it trusts" name kind)
                                    (ghostherd-log-entry-text (car ghostherd--log))))
            (should (equal (ghostherd-log-entry-session (car ghostherd--log)) name))
            (should (ghostherd--worktree-remove repo path nil name))
            (should (equal (ghostherd-tests--file store) before))
            (should-not (ghostherd--pretrusted))
            (should (string-match-p (format "dropped the trust it gave %s" kind)
                                    (ghostherd-log-entry-text (car ghostherd--log))))))))))

(ert-deftest ghostherd-test-only-what-the-herd-made-of-a-trusted-project-is-trusted ()
  "Anything else still asks the user: a project the CLI does not trust, a
CLI with no store (claude needs none), the feature off, a path the herd
did not create under its worktree root.  And an entry already there is
not the herd's to drop."
  (ghostherd-tests--with-trust
    (let ((main (directory-file-name repo))
          (untouched (lambda () (should-not (ghostherd-tests--file grok-store)))))
      ;; Not trusted: nothing written, nothing logged.
      (ghostherd--worktree-create repo "g1" nil 'grok)
      (funcall untouched)
      (should-not (or (ghostherd--pretrusted) ghostherd--log))
      (with-temp-file grok-store
        (insert (format "[folders.%S]\ntrusted = false\ndecided_at = 1\n" main)))
      (ghostherd--worktree-create repo "g2" nil 'grok)
      (should-not (ghostherd--pretrusted))
      (with-temp-file grok-store
        (insert (format "[folders.%S]\ntrusted = true\ndecided_at = 1\n" main)))
      (let ((before (ghostherd-tests--file grok-store)))
        ;; claude keeps no store of the herd's to write.
        (ghostherd--worktree-create repo "c1" nil 'claude)
        ;; The feature off.
        (let ((ghostherd-pretrust nil))
          (ghostherd--worktree-create repo "g3" nil 'grok))
        ;; A path not under the worktree root: the main checkout, or any
        ;; other directory.
        (should-not (ghostherd--worktree-pretrust 'grok repo repo "g4"))
        (make-directory (concat base "elsewhere/") t)
        (should-not (ghostherd--worktree-pretrust 'grok repo (concat base "elsewhere/") "g4"))
        (should (equal (ghostherd-tests--file grok-store) before))
        (should-not (ghostherd--pretrusted)))
      ;; Trusted already: not recorded as the herd's, so not dropped.
      (let* ((path (directory-file-name (file-truename (ghostherd--worktree-path repo "g5"))))
             (store (concat (ghostherd-tests--file grok-store)
                            (format "\n[folders.%S]\ntrusted = true\ndecided_at = 2\n" path))))
        (with-temp-file grok-store (insert store))
        (ghostherd--worktree-create repo "g5" nil 'grok)
        (should-not (ghostherd--pretrusted))
        (ghostherd--worktree-remove repo path nil "g5")
        (should (equal (ghostherd-tests--file grok-store) store))))))

(ert-deftest ghostherd-test-trust-the-herd-cannot-record-is-taken-back ()
  "What the herd adds it must drop with the worktree, so it must remember
it: when its record cannot be written, the entry is taken back out, and
the agent asks as it would have.  A record not there yet is made."
  (ghostherd-tests--with-trust
    (let ((before (format "[folders.%S]\ntrusted = true\ndecided_at = 1\n" (directory-file-name repo))))
      (with-temp-file grok-store (insert before))
      (with-temp-file (concat base "afile") (insert "not a directory"))
      (let ((ghostherd-pretrust-file (concat base "afile/pretrust.eld")))
        (should-not (plist-get (ghostherd--worktree-create repo "g1" nil 'grok) :trusted))
        (should (equal (ghostherd-tests--file grok-store) before))
        (should (string-match-p "not trusted for grok"
                                (ghostherd-log-entry-text (car ghostherd--log)))))
      (let ((ghostherd-pretrust-file (concat base "new/dir/pretrust.eld")))
        (ghostherd--worktree-create repo "g2" nil 'grok)
        (should (= (length (ghostherd--pretrusted)) 1))))))

(ert-deftest ghostherd-test-every-worktree-the-herd-creates-says-its-kind ()
  "`ghostherd-new', the page's New agent and `herd start --worktree' each
create the worktree for its agent's kind, which decides the trust."
  (let ((made nil))
    (cl-letf (((symbol-function 'ghostherd--worktree-create)
               (lambda (project name &optional branch kind)
                 (push (cons name kind) made)
                 (list :path (file-name-as-directory (ghostherd--worktree-path project name))
                       :branch (or branch name) :base "main")))
              ((symbol-function 'ghostherd--worktree-deps-setup) #'ignore))
      (ghostherd-tests--with-lead-start
        (ghostherd-lead-start "lead-p" "agy" :worktree "yes" :name "ls")
        (cl-letf (((symbol-function 'ghostherd-spawn) (lambda (&rest _) nil))
                  ((symbol-function 'read-string) (lambda (_p &optional initial &rest _) (or initial "")))
                  ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (ghostherd--new-fresh 'grok repo repo nil)
          (when (require 'ghostherd-memory-page nil t)
            (cl-letf (((symbol-function 'ghostherd-memory-page--js) #'ignore)
                      ((symbol-function 'ghostherd-memory-page--send-herd) #'ignore))
              (ghostherd-memory-page--handle `((op . "agent-new") (project . ,repo) (kind . "claude")
                                               (name . "pg") (worktree . t))))))))
    (should (equal (alist-get "ls" made nil nil #'equal) 'agy))
    (should (equal (alist-get "grok" made nil nil #'equal) 'grok))
    (when (featurep 'ghostherd-memory-page)
      (should (equal (alist-get "pg" made nil nil #'equal) 'claude)))))

(ert-deftest ghostherd-test-worktree-spawn-keeps-and-restores-it ()
  "A spawned worktree agent carries its worktree in the hash and the
recipe; the recipe respawns it in the worktree, not the main checkout; the
tmux options round-trip; and restore rebuilds the worktree from them."
  (should (equal (ghostherd-tmux--decode :worktree (ghostherd-tmux--encode :worktree "/w/")) "/w/"))
  (should (assq :worktree ghostherd-tmux--recipe-options))
  (should (assq :branch ghostherd-tmux--recipe-options))
  (should (assq :base ghostherd-tmux--recipe-options))
  (ghostherd-tests--with-herd ((ghostherd--lineage (make-hash-table :test 'equal))
                               (ghostherd--roles (make-hash-table :test 'equal))
                               (ghostherd--worktrees (make-hash-table :test 'equal))
                               (ghostherd-known-backends '(fake))
                               (ghostherd-tests--fake-recipes nil)
                               (ghostherd-tests--fake-spawned nil))
    ;; Its paths are labels: nothing starts in them, so none is missing.
    (cl-letf (((symbol-function 'ghostherd--directory-missing-p) #'ignore)
              ((symbol-function 'ghostherd--install-hooks) #'ignore)
              ((symbol-function 'ghostherd--register-eval-cmds) #'ignore)
              ((symbol-function 'ghostherd--ensure-poll-timer) #'ignore)
              ((symbol-function 'ghostherd--sidebar-refresh) #'ignore))
      (let ((s (ghostherd-spawn 'claude :name "wk" :project "/p/" :backend 'fake
                                :directory "/w/wk/" :worktree "/w/wk/"
                                :branch "wk" :base "master" :display nil)))
        (should (equal (ghostherd-session-worktree s)
                       '(:path "/w/wk/" :branch "wk" :base "master")))
        (should (equal (plist-get ghostherd-tests--fake-spawned :worktree) "/w/wk/"))
        (should (equal (plist-get ghostherd-tests--fake-spawned :branch) "wk"))
        (let ((r (ghostherd-session-recipe s)))
          (should (equal (plist-get r :worktree) "/w/wk/"))
          (should (equal (plist-get r :directory) "/w/wk/"))
          (should (equal (plist-get r :branch) "wk"))
          (should (equal (plist-get r :base) "master"))))
      ;; An agent with no worktree: nothing in the hash; recipe stays in the project.
      (let ((p (ghostherd-spawn 'claude :name "plain" :project "/p/" :backend 'fake :display nil)))
        (should-not (ghostherd-session-worktree p))
        (should-not (plist-get (ghostherd-session-recipe p) :worktree))
        (should (equal (plist-get (ghostherd-session-recipe p) :directory) "/p/")))
      ;; Restore rebuilds the worktree hash from the recipe.
      (setq ghostherd-tests--fake-recipes
            '((:name "rk" :kind claude :host-id "gh-rk" :project "/p/"
               :worktree "/w/rk/" :branch "rk" :base "master")))
      (ghostherd-restore)
      (should (equal (ghostherd-session-worktree (ghostherd-get "rk"))
                     '(:path "/w/rk/" :branch "rk" :base "master"))))))

(ert-deftest ghostherd-test-worktree-cleanup-removes-merged-keeps-unmerged ()
  "On a kill, a merged-and-clean worktree and its branch are removed; an
unmerged or unclean one is kept, and `ghostherd-worktree-forget' removes it
-- keeping its branch -- after confirmation."
  (ghostherd-tests--with-herd ((ghostherd--worktrees (make-hash-table :test 'equal)))
    (ghostherd-tests--with-repo
      (let ((ghostherd-worktree-root (concat base "trees/")))
        (cl-letf (((symbol-function 'ghostherd--sidebar-refresh) #'ignore))
          (cl-flet ((branch-p (b) (eq 0 (let ((default-directory repo))
                                          (call-process "git" nil nil nil "rev-parse"
                                                        "--verify" "-q"
                                                        (concat "refs/heads/" b))))))
            ;; Merged + clean (never committed): removed with its branch.
            (let ((wt (ghostherd--worktree-create repo "clean"))
                  (s (ghostherd-tests--session :name "clean" :kind 'claude :project repo)))
              (should (ghostherd--worktree-clean-p (plist-get wt :path)))
              (should (ghostherd--worktree-merged-p repo "clean" "main"))
              (puthash "clean" wt ghostherd--worktrees)
              (ghostherd--worktree-cleanup s)
              (should-not (file-directory-p (plist-get wt :path)))
              (should-not (branch-p "clean")))
            ;; Unmerged (a commit on the branch): kept, branch kept.
            (let ((wt (ghostherd--worktree-create repo "ahead"))
                  (s (ghostherd-tests--session :name "ahead" :kind 'claude :project repo)))
              (funcall git (plist-get wt :path) "commit" "-q" "--allow-empty" "-m" "work")
              (should-not (ghostherd--worktree-merged-p repo "ahead" "main"))
              (puthash "ahead" wt ghostherd--worktrees)
              (ghostherd--worktree-cleanup s)
              (should (file-directory-p (plist-get wt :path)))
              (should (branch-p "ahead"))
              ;; The user removes it by hand: worktree gone, branch kept.
              (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
                (ghostherd-worktree-forget (plist-get wt :path)))
              (should-not (file-directory-p (plist-get wt :path)))
              (should (branch-p "ahead")))
            ;; Unclean (an untracked file): kept.
            (let ((wt (ghostherd--worktree-create repo "dirty"))
                  (s (ghostherd-tests--session :name "dirty" :kind 'claude :project repo)))
              (with-temp-file (expand-file-name "scratch.txt" (plist-get wt :path)) (insert "x"))
              (should-not (ghostherd--worktree-clean-p (plist-get wt :path)))
              (puthash "dirty" wt ghostherd--worktrees)
              (ghostherd--worktree-cleanup s)
              (should (file-directory-p (plist-get wt :path)))
              (should (branch-p "dirty")))))))))

(ert-deftest ghostherd-test-worktree-deps-git-ignored ()
  "What the herd carries in is ignored at the source: after setup the
worktree's git status is clean -- even a venv symlink a `.venv/' ignore
would miss -- the patterns are anchored in the shared info/exclude, the
main checkout is unaffected, and a real untracked file still shows."
  (ghostherd-tests--with-herd ((ghostherd--worktrees (make-hash-table :test 'equal))
                               (ghostherd--worktree-deps-status (make-hash-table :test 'equal)))
    (ghostherd-tests--with-repo
      (with-temp-file (expand-file-name ".gitignore" repo) (insert ".venv/\nnode_modules/\n"))
      (funcall git repo "add" ".gitignore")
      (funcall git repo "commit" "-q" "-m" "ignore")
      (make-directory (expand-file-name ".venv/bin" repo) t)
      (with-temp-file (expand-file-name ".venv/bin/python" repo) (insert "#!/bin/sh\n"))
      (make-directory (expand-file-name "node_modules/pkg" repo) t)
      (with-temp-file (expand-file-name "node_modules/pkg/i.js" repo) (insert "x\n"))
      (let* ((ghostherd-worktree-root (concat base "trees/"))
             (wt (ghostherd--worktree-create repo "dev"))
             (path (plist-get wt :path)))
        (ghostherd--worktree-deps-setup repo path "dev")
        (let ((n 0)) (while (and (not (gethash "dev" ghostherd--worktree-deps-status)) (< n 100))
                       (accept-process-output nil 0.1) (setq n (1+ n))))
        ;; venv is a symlink (a `.venv/' ignore misses it), yet the worktree
        ;; reads clean because it is excluded at the source.
        (should (file-symlink-p (directory-file-name (expand-file-name ".venv" path))))
        (should (ghostherd--worktree-clean-p path))
        ;; The main checkout is unaffected (its .venv/node_modules are
        ;; already ignored there, so the anchored patterns are harmless).
        (should (string-empty-p (with-temp-buffer
                                  (let ((default-directory repo))
                                    (call-process "git" nil '(t nil) nil "status" "--porcelain"))
                                  (string-trim (buffer-string)))))
        ;; Anchored patterns in the shared info/exclude.
        (with-temp-buffer
          (insert-file-contents (expand-file-name ".git/info/exclude" repo))
          (should (string-match-p "^/\\.venv$" (buffer-string)))
          (should (string-match-p "^/node_modules$" (buffer-string))))
        ;; A real untracked file still shows -- cleanup would keep it.
        (with-temp-file (expand-file-name "real.txt" path) (insert "x"))
        (should-not (ghostherd--worktree-clean-p path))))))

(ert-deftest ghostherd-test-worktree-card-shows-ahead-and-size ()
  "A worktree agent's card carries its branch, how far it is ahead of its
base, and the worktree's size on disk -- :json-false until measured."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (ghostherd-tests--with-herd
      ((ghostherd--worktrees (make-hash-table :test 'equal))
       (ghostherd-memory-page--checkouts (make-hash-table :test 'equal))
       (ghostherd-memory-page--prs (make-hash-table :test 'equal))
       (ghostherd-memory-page--changes (make-hash-table :test 'equal))
       (ghostherd-memory-page--ahead (make-hash-table :test 'equal))
       (ghostherd-memory-page--size (make-hash-table :test 'equal))
       (ghostherd-memory-page--size-parts (make-hash-table :test 'equal)))
    (let ((s (ghostherd-tests--session :name "wk" :kind 'claude :project "/p/")))
      (puthash "wk" '(:path "/w/wk/" :branch "wk" :base "master") ghostherd--worktrees)
      (puthash (cons 'agent "wk") '("/w/wk/" . "wk") ghostherd-memory-page--checkouts)
      (puthash (ghostherd--project-name "/p/") '("/p/" . "main") ghostherd-memory-page--checkouts)
      (puthash "wk" 3 ghostherd-memory-page--ahead)
      (puthash "wk" 5242880 ghostherd-memory-page--size)
      (puthash "wk" '(:parts (("node_modules" . 4194304)) :rest 1048576 :linked nil)
               ghostherd-memory-page--size-parts)
      (let ((w (ghostherd-memory-page--agent-work s)))
        (should (equal (plist-get w :sizeParts)
                       '(:parts [(:path "node_modules" :bytes 4194304 :how "")]
                         :rest 1048576 :linked [])))
        (should (equal (plist-get w :root) "/w/wk/"))
        (should (equal (plist-get w :branch) "wk"))
        (should (equal (plist-get w :base) "master"))
        (should (equal (plist-get w :ahead) 3))
        (should (equal (plist-get w :size) 5242880)))
      (remhash "wk" ghostherd-memory-page--ahead)
      (remhash "wk" ghostherd-memory-page--size)
      (remhash "wk" ghostherd-memory-page--size-parts)
      (let ((w (ghostherd-memory-page--agent-work s)))
        (should (eq (plist-get w :sizeParts) :json-false))
        (should (eq (plist-get w :ahead) :json-false))
        (should (eq (plist-get w :size) :json-false))))))

(ert-deftest ghostherd-test-worktree-size-says-where ()
  "A worktree's size chip names where its space is, from `du''s listing:
a dependency whole (node_modules is never broken into its packages), a
directory broken into its big parts when it splits or holds a big
dependency (server/ into server/.venv and server/data), one that does not
split kept itself (server/data, all of it one models dir), and the rest
summed.  Parts under a twentieth of the whole are the rest."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let* ((mb (lambda (n) (* n 1024)))
         ;; rd-tabs as `du -k -d 4' measured it: 800 MB.
         (sizes `(("" . ,(funcall mb 800))
                  ("node_modules" . ,(funcall mb 480))
                  ("node_modules/next" . ,(funcall mb 120))
                  ("node_modules/@swc" . ,(funcall mb 90))
                  ("server" . ,(funcall mb 316))
                  ("server/.venv" . ,(funcall mb 220))
                  ("server/.venv/lib" . ,(funcall mb 219))
                  ("server/data" . ,(funcall mb 91))
                  ("server/data/models" . ,(funcall mb 91))
                  ("src" . ,(funcall mb 3))))
         (deps '(".venv" "venv" "node_modules"))
         (got (ghostherd-memory-page--size-where sizes deps)))
    (should (equal (mapcar #'car (car got)) '("node_modules" "server/.venv" "server/data")))
    (should (equal (cdr (car (car got))) (* 480 1024 1024)))
    (should (equal (cdr got) (* (- 800 480 220 91) 1024 1024)))
    ;; A server/ whose only big part is its venv still names the venv.
    (let ((got (ghostherd-memory-page--size-where
                `(("" . ,(funcall mb 300)) ("server" . ,(funcall mb 240))
                  ("server/.venv" . ,(funcall mb 230)) ("docs" . ,(funcall mb 60)))
                deps)))
      (should (equal (mapcar #'car (car got)) '("server/.venv" "docs"))))
    ;; With no dependency named, a dir of one big part is itself.
    (let ((got (ghostherd-memory-page--size-where
                `(("" . ,(funcall mb 300)) ("server" . ,(funcall mb 240))
                  ("server/.venv" . ,(funcall mb 230)) ("docs" . ,(funcall mb 60)))
                nil)))
      (should (equal (mapcar #'car (car got)) '("server" "docs"))))
    ;; At most `ghostherd-memory-page--size-parts-most' named; the rest summed.
    (let* ((many (cons `("" . ,(funcall mb 100))
                       (mapcar (lambda (i) (cons (format "d%d" i) (funcall mb 10))) (number-sequence 1 9))))
           (got (ghostherd-memory-page--size-where many nil)))
      (should (= (length (car got)) ghostherd-memory-page--size-parts-most))
      (should (equal (cdr got) (* (- 100 (* 10 ghostherd-memory-page--size-parts-most)) 1024 1024))))
    ;; Nothing measured: no parts.
    (should-not (car (ghostherd-memory-page--size-where nil deps)))))

(ert-deftest ghostherd-test-worktree-size-one-du-filtered ()
  "The size and where it is come from one `du' in the background,
filtered before Emacs reads it -- a node_modules of hundreds of packages
is a few lines -- with a carried-in dependency that is a symlink said to
take no space; the card carries the parts, and what the herd made a part
as."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (ghostherd-tests--with-herd
      ((ghostherd--worktrees (make-hash-table :test 'equal))
       (ghostherd--worktree-deps-status (make-hash-table :test 'equal))
       (ghostherd-memory-page--size (make-hash-table :test 'equal))
       (ghostherd-memory-page--size-parts (make-hash-table :test 'equal))
       (ghostherd-memory-page--size-at (make-hash-table :test 'equal))
       (ghostherd-memory-page--size-running (make-hash-table :test 'equal)))
    (ghostherd-tests--with-repo
      (let* ((wt (concat base "wt/"))
             (blob (lambda (rel kb)
                     (make-directory (file-name-directory (expand-file-name rel wt)) t)
                     (with-temp-file (expand-file-name rel wt)
                       (insert (make-string (* kb 1024) ?x)))))
             (s (ghostherd-tests--session :name "wk" :kind 'claude :project repo))
             (procs 0) (read-lines nil))
        (funcall blob "node_modules/big/index.js" 2048)
        (dotimes (i 300) (funcall blob (format "node_modules/p%d/i.js" i) 1))
        (funcall blob "server/data/models/m.onnx" 1024)
        (funcall blob "src/a.el" 4)
        (make-directory (expand-file-name "server" wt) t)
        (make-symbolic-link (directory-file-name repo) (expand-file-name "server/.venv" wt))
        (puthash "wk" (list :path wt :branch "wk" :base "main") ghostherd--worktrees)
        (puthash "wk" '(("node_modules" :status done :how "clone")
                        ("server/.venv" :status done :how "symlink"))
                 ghostherd--worktree-deps-status)
        (cl-letf* ((make-process-orig (symbol-function 'make-process))
                   ((symbol-function 'make-process)
                    (lambda (&rest args)
                      (when (equal (plist-get args :name) "ghostherd-du") (setq procs (1+ procs)))
                      (apply make-process-orig args)))
                   (read-orig (symbol-function 'ghostherd-memory-page--size-read))
                   ((symbol-function 'ghostherd-memory-page--size-read)
                    (lambda (root out)
                      (setq read-lines (length (split-string out "\n" t)))
                      (funcall read-orig root out)))
                   ((symbol-function 'ghostherd-memory-page--work-changed) #'ignore))
          (ghostherd-memory-page--worktree-size "wk")
          ;; In flight: a second look starts no second du.
          (ghostherd-memory-page--worktree-size "wk")
          (let ((n 0))
            (while (and (not (gethash "wk" ghostherd-memory-page--size-at)) (< n 100))
              (accept-process-output nil 0.1) (setq n (1+ n)))))
        (should (= procs 1))
        (should (< read-lines 20))
        (should (> (gethash "wk" ghostherd-memory-page--size) (* 3 1024 1024)))
        (let ((where (gethash "wk" ghostherd-memory-page--size-parts)))
          ;; server/ does not split -- its venv is a link, no space -- so
          ;; it is itself.
          (should (equal (mapcar #'car (plist-get where :parts)) '("node_modules" "server")))
          (should (equal (plist-get where :linked) '("server/.venv"))))
        (let ((sp (ghostherd-memory-page--size-parts-of s)))
          (should (equal (append (plist-get sp :parts) nil)
                         (list (list :path "node_modules"
                                     :bytes (cdr (assoc "node_modules" (plist-get (gethash "wk" ghostherd-memory-page--size-parts) :parts)))
                                     :how "clone")
                               (list :path "server"
                                     :bytes (cdr (assoc "server" (plist-get (gethash "wk" ghostherd-memory-page--size-parts) :parts)))
                                     :how ""))))
          (should (equal (plist-get sp :linked) ["server/.venv"])))
        ;; Within the TTL, no du again.
        (setq procs 0)
        (ghostherd-memory-page--worktree-size "wk")
        (should (= procs 0))
        ;; Unmeasured: :json-false.
        (clrhash ghostherd-memory-page--size-parts)
        (should (eq (ghostherd-memory-page--size-parts-of s) :json-false))))))

(ert-deftest ghostherd-test-worktree-new-agent-opts-in ()
  "The page's +New with worktree on creates the worktree and spawns in it;
when git cannot, it says so and spawns nothing."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--with-repo
      (let ((ghostherd-worktree-root (concat base "trees/"))
            spawned flashed)
        (cl-letf (((symbol-function 'ghostherd-spawn)
                   (lambda (kind &rest plist)
                     (setq spawned (cons kind plist))
                     (ghostherd-session--create :id "x" :name "wk")))
                  ((symbol-function 'ghostherd-memory-page--js)
                   (lambda (_fn &rest args) (push (car args) flashed)))
                  ((symbol-function 'ghostherd-memory-page--send-herd) #'ignore)
                  ((symbol-function 'ghostherd-project-defaults) (lambda (&rest _) (cons nil nil))))
          ;; worktree on: a worktree is made and the spawn is in it.
          (ghostherd-memory-page--agent-new
           `((kind . "claude") (project . ,repo) (name . "wk") (worktree . t)))
          (should spawned)
          (let ((plist (cdr spawned)))
            (should (plist-get plist :worktree))
            (should (equal (plist-get plist :branch) "wk"))
            (should (equal (plist-get plist :directory) (plist-get plist :worktree)))
            (should (file-directory-p (plist-get plist :worktree))))
          ;; A non-git directory: no worktree, no spawn, a flash saying so.
          (setq spawned nil flashed nil)
          (ghostherd-memory-page--agent-new
           `((kind . "claude") (project . ,base) (name . "nope") (worktree . t)))
          (should-not spawned)
          (should (seq-find (lambda (m) (and (stringp m) (string-match-p "No worktree" m))) flashed)))))))

(ert-deftest ghostherd-test-worktree-snapshot-has-directory-and-branch ()
  "The herd snapshot (what `herd list' reads) carries each agent's directory
and branch: a worktree agent its worktree path and branch, others the
project directory and no branch."
  (ghostherd-tests--with-herd ((ghostherd--worktrees (make-hash-table :test 'equal))
                               (ghostherd--roles (make-hash-table :test 'equal)))
    (ghostherd-tests--session :name "wk" :kind 'claude :project "/p/shop/")
    (ghostherd-tests--session :name "pl" :kind 'claude :project "/p/shop/")
    (puthash "wk" '(:path "/w/shop/wk/" :branch "wk" :base "main") ghostherd--worktrees)
    (let* ((snap (ghostherd--herd-snapshot))
           (wks (seq-find (lambda (e) (equal (plist-get e :name) "wk")) snap))
           (pls (seq-find (lambda (e) (equal (plist-get e :name) "pl")) snap)))
      (should (equal (plist-get wks :directory) "/w/shop/wk/"))
      (should (equal (plist-get wks :branch) "wk"))
      (should (equal (plist-get pls :directory) "/p/shop/"))
      (should (equal (plist-get pls :branch) "")))))

(ert-deftest ghostherd-test-worktree-deps-carried-in ()
  "Dependencies are carried into a worktree in the background: a venv is
symlinked, node_modules is cloned (a real copy, not a link), an explicit
path with no source is skipped, and the card surfaces only failures."
  (ghostherd-tests--with-herd ((ghostherd--worktrees (make-hash-table :test 'equal))
                               (ghostherd--worktree-deps-status (make-hash-table :test 'equal)))
    (ghostherd-tests--with-repo
      (with-temp-file (expand-file-name ".gitignore" repo) (insert ".venv/\nnode_modules/\n"))
      (funcall git repo "add" ".gitignore")
      (funcall git repo "commit" "-q" "-m" "ignore")
      (make-directory (expand-file-name ".venv/bin" repo) t)
      (with-temp-file (expand-file-name ".venv/bin/python" repo) (insert "#!/bin/sh\n"))
      (make-directory (expand-file-name "node_modules/pkg" repo) t)
      (with-temp-file (expand-file-name "node_modules/pkg/index.js" repo) (insert "x\n"))
      (let* ((ghostherd-worktree-root (concat base "trees/"))
             (wt (ghostherd--worktree-create repo "dev"))
             (path (plist-get wt :path)))
        ;; Bare names resolve to the ignored dirs; an explicit missing path
        ;; is kept and then skipped no-source.
        (ghostherd--worktree-deps-setup
         repo path "dev" '((".venv" . symlink) ("node_modules" . clone) ("gone/dep" . clone)))
        (let ((tries 0))
          (while (and (not (gethash "dev" ghostherd--worktree-deps-status)) (< tries 100))
            (accept-process-output nil 0.1)
            (setq tries (1+ tries))))
        (let ((st (gethash "dev" ghostherd--worktree-deps-status)))
          (should (eq (plist-get (alist-get ".venv" st nil nil #'equal) :status) 'done))
          (should (eq (plist-get (alist-get "node_modules" st nil nil #'equal) :status) 'done))
          (should (eq (plist-get (alist-get "gone/dep" st nil nil #'equal) :status) 'skipped))
          (should (equal (plist-get (alist-get "gone/dep" st nil nil #'equal) :reason) "no-source")))
        (should (file-symlink-p (directory-file-name (expand-file-name ".venv" path))))
        (should (file-directory-p (expand-file-name "node_modules/pkg" path)))
        (should-not (file-symlink-p (directory-file-name (expand-file-name "node_modules" path))))
        (when (require 'ghostherd-memory-page nil t)
          (let ((s (ghostherd-tests--session :name "dev" :kind 'claude :project repo)))
            (puthash "dev" wt ghostherd--worktrees)
            ;; A missing source is not a failure worth a card chip.
            (should (eq (ghostherd-memory-page--worktree-deps s) :json-false))))))))

(ert-deftest ghostherd-test-worktree-venv-from-lock ()
  "The venv method builds a per-worktree virtualenv from the worktree's own
uv.lock (uv sync --frozen, uv hardlinking from its cache); it is skipped,
and said so on the card, when there is no uv on PATH or no uv.lock there --
never a full copy, never a lock-less install."
  (ghostherd-tests--with-herd ((ghostherd--worktree-deps-status (make-hash-table :test 'equal)))
    (ghostherd-tests--with-repo
      (let* ((ghostherd-worktree-root (concat base "trees/"))
             (wt (ghostherd--worktree-create repo "dev"))
             (path (plist-get wt :path))
             (bin (concat base "bin/"))
             (nox (concat base "nox/"))
             (process-environment (copy-sequence process-environment))
             (status (lambda () (plist-get (alist-get ".venv"
                                                      (gethash "dev" ghostherd--worktree-deps-status)
                                                      nil nil #'equal)
                                           :status)))
             (reason (lambda () (plist-get (alist-get ".venv"
                                                      (gethash "dev" ghostherd--worktree-deps-status)
                                                      nil nil #'equal)
                                           :reason)))
             (run (lambda ()
                    (clrhash ghostherd--worktree-deps-status)
                    (ghostherd--worktree-deps-setup repo path "dev" '((".venv" . venv)))
                    (let ((n 0)) (while (and (not (gethash "dev" ghostherd--worktree-deps-status)) (< n 100))
                                   (accept-process-output nil 0.1) (setq n (1+ n)))))))
        (make-directory nox t)
        ;; No uv on PATH: skipped, said so (sh is found by `exec-path', not PATH).
        (setenv "PATH" (directory-file-name nox))
        (funcall run)
        (should (eq (funcall status) 'skipped))
        (should (equal (funcall reason) "no-uv"))
        ;; A fake uv that honours UV_PROJECT_ENVIRONMENT and --frozen.
        (make-directory bin t)
        (with-temp-file (expand-file-name "uv" bin)
          (insert "#!/bin/sh\n[ \"$1\" = sync ] || exit 2\n"
                  "mkdir -p \"$UV_PROJECT_ENVIRONMENT\" && : > \"$UV_PROJECT_ENVIRONMENT/pyvenv.cfg\"\n"))
        (set-file-modes (expand-file-name "uv" bin) #o755)
        (setenv "PATH" (concat (directory-file-name bin) ":/usr/bin:/bin"))
        ;; uv there but no uv.lock: skipped, said so -- never a lock-less install.
        (funcall run)
        (should (eq (funcall status) 'skipped))
        (should (equal (funcall reason) "no-uv.lock"))
        ;; And the card says so (a venv skip is worth a chip, unlike no-source).
        (when (require 'ghostherd-memory-page nil t)
          (let* ((sess (ghostherd-tests--session :name "dev" :kind 'claude :project repo))
                 (card (ghostherd-memory-page--worktree-deps sess)))
            (should (vectorp card))
            (should (seq-find (lambda (c) (equal (plist-get c :reason) "no-uv.lock"))
                              (append card nil)))))
        ;; With the lock: built, the venv where the spec named it.
        (with-temp-file (expand-file-name "uv.lock" path) (insert "version = 1\n"))
        (funcall run)
        (should (eq (funcall status) 'done))
        (should (file-exists-p (expand-file-name ".venv/pyvenv.cfg" path)))))))

(defmacro ghostherd-tests--with-uv-repo (&rest body)
  "BODY in `with-repo' whose `repo' is a uv project in server/: a tracked
server/uv.lock and an ignored server/.venv, the main checkout's venv.
`wt' is a worktree of it, `path' its directory, `bin' a dir with a fake
`uv' (that writes UV_PROJECT_ENVIRONMENT/pyvenv.cfg and logs its cwd and
args to `log', failing when UV_FAKE_FAIL is set) on a PATH of its own,
and `run' a lambda (&optional SPECS) setting the deps up and waiting.
The real uv and the user's cache are never run."
  (declare (indent 0) (debug t))
  `(ghostherd-tests--with-herd ((ghostherd--worktree-deps-status (make-hash-table :test 'equal)))
     (ghostherd-tests--with-repo
       (with-temp-file (expand-file-name ".gitignore" repo) (insert ".venv/\n"))
       (make-directory (expand-file-name "server" repo) t)
       (with-temp-file (expand-file-name "server/uv.lock" repo) (insert "version = 1\n"))
       (funcall git repo "add" ".gitignore" "server/uv.lock")
       (funcall git repo "commit" "-q" "-m" "uv")
       (make-directory (expand-file-name "server/.venv/bin" repo) t)
       (with-temp-file (expand-file-name "server/.venv/pyvenv.cfg" repo) (insert "main\n"))
       (let* ((ghostherd-worktree-root (concat base "trees/"))
              (wt (ghostherd--worktree-create repo "dev"))
              (path (plist-get wt :path))
              (bin (concat base "bin/"))
              (log (concat base "uv.log"))
              (process-environment (copy-sequence process-environment))
              (run (lambda (&optional specs)
                     (clrhash ghostherd--worktree-deps-status)
                     (ghostherd--worktree-deps-setup repo path "dev" specs)
                     (let ((n 0))
                       (while (and (not (gethash "dev" ghostherd--worktree-deps-status)) (< n 100))
                         (accept-process-output nil 0.1) (setq n (1+ n)))))))
         (ignore wt run)
         (make-directory bin t)
         (with-temp-file (expand-file-name "uv" bin)
           (insert "#!/bin/sh\n"
                   "echo \"$PWD $* $UV_PROJECT_ENVIRONMENT\" >> " (shell-quote-argument log) "\n"
                   "[ -n \"$UV_FAKE_FAIL\" ] && { mkdir -p \"$UV_PROJECT_ENVIRONMENT/lib\"; exit 1; }\n"
                   "mkdir -p \"$UV_PROJECT_ENVIRONMENT\" && echo own > \"$UV_PROJECT_ENVIRONMENT/pyvenv.cfg\"\n"))
         (set-file-modes (expand-file-name "uv" bin) #o755)
         (setenv "PATH" (concat (directory-file-name bin) ":/usr/bin:/bin"))
         (setenv "UV_FAKE_FAIL" nil)
         ,@body))))

(ert-deftest ghostherd-test-worktree-uv-venv-by-default ()
  "By default a uv project's venv, found at any depth (server/.venv), is
the worktree's own -- `uv sync --frozen' in its directory, from the
worktree's own lock, into the worktree's path -- not a symlink to the main
checkout's, whose venv is left alone.  The outcome says how it was made."
  (ghostherd-tests--with-uv-repo
    (should (equal (alist-get ".venv" ghostherd-worktree-deps nil nil #'equal) 'uv))
    (funcall run)
    (let ((venv (expand-file-name "server/.venv" path))
          (st (alist-get "server/.venv" (gethash "dev" ghostherd--worktree-deps-status)
                         nil nil #'equal)))
      (should (eq (plist-get st :status) 'done))
      (should (equal (plist-get st :how) "uv"))
      (should-not (file-symlink-p venv))
      (should (equal (with-temp-buffer (insert-file-contents (expand-file-name "pyvenv.cfg" venv))
                                       (buffer-string))
                     "own\n"))
      ;; Synced in the worktree's server/, frozen, into the worktree's venv.
      (let ((line (with-temp-buffer (insert-file-contents log) (string-trim (buffer-string)))))
        (should (equal line (format "%s sync --frozen %s"
                                    (directory-file-name (expand-file-name "server" path))
                                    venv))))
      ;; The main checkout's venv is untouched.
      (should (equal (with-temp-buffer
                       (insert-file-contents (expand-file-name "server/.venv/pyvenv.cfg" repo))
                       (buffer-string))
                     "main\n"))
      ;; Ignored at the source, so the worktree still reads clean.
      (should (ghostherd--worktree-clean-p path)))))

(ert-deftest ghostherd-test-worktree-uv-venv-falls-back-to-symlink ()
  "With no uv.lock beside the venv, or no uv on PATH, the venv is
symlinked to the main checkout's as before; uv is never run then."
  (ghostherd-tests--with-uv-repo
    ;; No uv on PATH, the system's tools still there.
    (setenv "PATH" "/usr/bin:/bin")
    (funcall run)
    (let ((venv (directory-file-name (expand-file-name "server/.venv" path))))
      (should (file-symlink-p venv))
      (should (equal (plist-get (alist-get "server/.venv" (gethash "dev" ghostherd--worktree-deps-status)
                                           nil nil #'equal)
                                :how)
                     "symlink"))
      (should-not (file-exists-p log))
      ;; uv there, but no uv.lock in the worktree's server/: a symlink too.
      (delete-file venv)
      (delete-file (expand-file-name "server/uv.lock" path))
      (setenv "PATH" (concat (directory-file-name bin) ":/usr/bin:/bin"))
      (funcall run)
      (should (file-symlink-p venv))
      (should-not (file-exists-p log)))))

(ert-deftest ghostherd-test-worktree-uv-venv-sync-failure-said ()
  "A uv sync that fails is skipped and said so on the card, its partial
venv removed -- never a symlink to the main checkout's in its place."
  (ghostherd-tests--with-uv-repo
    (setenv "UV_FAKE_FAIL" "1")
    (funcall run)
    (let ((venv (directory-file-name (expand-file-name "server/.venv" path)))
          (st (alist-get "server/.venv" (gethash "dev" ghostherd--worktree-deps-status)
                         nil nil #'equal)))
      (should (eq (plist-get st :status) 'skipped))
      (should (equal (plist-get st :reason) "uv-sync-failed"))
      (should-not (file-exists-p venv))
      (should-not (file-symlink-p venv))
      (when (require 'ghostherd-memory-page nil t)
        (let ((card (ghostherd-memory-page--worktree-deps
                     (ghostherd-tests--session :name "dev" :kind 'claude :project repo))))
          (should (vectorp card))
          (should (equal (plist-get (aref card 0) :reason) "uv-sync-failed")))))))

(ert-deftest ghostherd-test-worktree-uv-venv-user-choice-wins ()
  "The user's own `ghostherd-worktree-deps', and a project's
`ghostherd-project-worktree-deps' dir-local, win over the uv default: a
symlink stays a symlink.  The dir-local may name the uv method."
  (ghostherd-tests--with-uv-repo
    (let ((ghostherd-worktree-deps '((".venv" . symlink))))
      (funcall run))
    (should (file-symlink-p (directory-file-name (expand-file-name "server/.venv" path))))
    (should-not (file-exists-p log))
    (with-temp-file (expand-file-name ".dir-locals.el" repo)
      (prin1 '((nil . ((ghostherd-project-worktree-deps . ((".venv" . symlink))))))
             (current-buffer)))
    (should (equal (ghostherd--worktree-deps-for repo) '((".venv" . symlink))))
    (should (funcall (get 'ghostherd-project-worktree-deps 'safe-local-variable)
                     '((".venv" . uv))))
    (should-not (funcall (get 'ghostherd-project-worktree-deps 'safe-local-variable)
                         '((".venv" . rm))))))

(ert-deftest ghostherd-test-worktree-deps-resolve-finds-nested ()
  "Bare-name deps are found by directory name among the main checkout's
ignored directories at any depth; build caches and dirs inside an ignored
dir are left out; explicit paths pass through."
  (ghostherd-tests--with-repo
    (with-temp-file (expand-file-name ".gitignore" repo)
      (insert ".venv/\nnode_modules/\n.next/\nvendor/\n"))
    (funcall git repo "add" ".gitignore")
    (funcall git repo "commit" "-q" "-m" "ignore")
    (dolist (d '("backend/.venv" "frontend/node_modules" ".next"
                 "vendor/sub/node_modules"))
      (make-directory (expand-file-name d repo) t)
      (with-temp-file (expand-file-name (concat d "/f") repo) (insert "x")))
    (let ((got (ghostherd--worktree-deps-resolve
                repo '((".venv" . symlink) ("venv" . symlink)
                       ("node_modules" . clone) ("explicit/dep" . symlink)))))
      ;; Found at depth, by name.
      (should (member '("backend/.venv" . symlink) got))
      (should (member '("frontend/node_modules" . clone) got))
      ;; A build cache (.next) is not a named dep -> out.
      (should-not (assoc ".next" got))
      ;; --directory does not descend into vendor/, so its nested
      ;; node_modules is never seen.
      (should-not (member '("vendor/sub/node_modules" . clone) got))
      ;; An explicit path passes through unchanged.
      (should (member '("explicit/dep" . symlink) got)))))

(ert-deftest ghostherd-test-worktree-deps-script-never-full-copies ()
  "The dependency script clones with copy-on-write and symlinks, and on a
failed clone skips rather than copying in full."
  (let ((script (ghostherd--worktree-deps-script
                 '(("node_modules" . clone) (".venv" . symlink)) "/m/" "/w/")))
    (should (string-match-p "cp -c -R" script))
    (should (string-match-p "no-copy-on-write" script))
    (should (string-match-p "ln -s" script))
    ;; No full-copy fallback anywhere.
    (should-not (string-match-p "cp -R" script))))

(ert-deftest ghostherd-test-worktree-deps-per-project-override ()
  "`ghostherd--worktree-deps-for' uses the project's dir-local when set,
else the default `ghostherd-worktree-deps'."
  (ghostherd-tests--with-repo
    (should (equal (ghostherd--worktree-deps-for repo) ghostherd-worktree-deps))
    (with-temp-file (expand-file-name ".dir-locals.el" repo)
      (prin1 '((nil . ((ghostherd-project-worktree-deps . (("node_modules" . clone))))))
             (current-buffer)))
    (should (equal (ghostherd--worktree-deps-for repo) '(("node_modules" . clone))))))

(ert-deftest ghostherd-test-worktree-strays-shown-on-project ()
  "A worktree no live agent is in is listed on its project as an idle
worktree the user can remove; an occupied one is not."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (ghostherd-tests--with-herd
      ((ghostherd--worktrees (make-hash-table :test 'equal))
       (ghostherd-memory-page--strays (make-hash-table :test 'equal))
       (ghostherd-memory-page--checkouts (make-hash-table :test 'equal))
       (ghostherd-memory-page--prs (make-hash-table :test 'equal))
       (ghostherd-memory-page--changes (make-hash-table :test 'equal)))
    (ghostherd-tests--with-repo
      (let* ((ghostherd-worktree-root (concat base "trees/"))
             (wt (ghostherd--worktree-create repo "ghost"))
             (key (ghostherd--project-name repo)))
        ;; No agent is in it: a stray.
        (ghostherd-memory-page--strays-look repo)
        (let ((strays (gethash key ghostherd-memory-page--strays)))
          (should (equal (length strays) 1))
          (should (equal (cdr (car strays)) "ghost")))
        ;; An agent in it: no longer a stray.
        (let ((s (ghostherd-tests--session :name "a" :kind 'claude :project repo)))
          (ignore s)
          (puthash "a" wt ghostherd--worktrees)
          (ghostherd-memory-page--strays-look repo)
          (should-not (gethash key ghostherd-memory-page--strays)))
        ;; Gone again: the project payload carries it.
        (remhash "a" ghostherd--worktrees)
        (ghostherd-memory-page--strays-look repo)
        (puthash key (cons repo "main") ghostherd-memory-page--checkouts)
        (let* ((pw (append (ghostherd-memory-page--projects-work) nil))
               (entry (seq-find (lambda (e) (equal (plist-get e :project) key)) pw))
               (strays (append (plist-get entry :strays) nil)))
          (should (equal (length strays) 1))
          (should (equal (plist-get (car strays) :branch) "ghost")))))))

;;; An agent whose directory is gone (ask 0c54c5b6)
;;
;; rd-tabs and rd-reading's worktrees were removed outside the herd once
;; their branches merged; `herd list' still named the directories, `herd
;; stop' said they were "not merged or not clean, so ... kept", and a
;; respawn brought both up in the home directory.

(defmacro ghostherd-tests--with-gone (&rest body)
  "BODY in a clean herd over a temp git `repo' (`with-repo') on the fake
backend, the gone-directory tables fresh and worktrees under the temp dir.
`wt-of' makes agent NAME a worktree agent of `repo', in a real git
worktree off `main', and returns the worktree's path."
  (declare (indent 0) (debug t))
  `(ghostherd-tests--with-repo
     (ghostherd-tests--with-herd
         ((ghostherd--roles (make-hash-table :test 'equal))
          (ghostherd--worktrees (make-hash-table :test 'equal))
          (ghostherd--worktree-stat (make-hash-table :test 'equal))
          (ghostherd--started-by (make-hash-table :test 'equal))
          (ghostherd--lineage (make-hash-table :test 'equal))
          (ghostherd--relaying (make-hash-table :test 'equal))
          (ghostherd--dir-gone (make-hash-table :test 'equal))
          (ghostherd--dir-gone-reading (make-hash-table :test 'equal))
          (ghostherd--dir-checked-at 0)
          (ghostherd--log nil)
          (ghostherd--herd-asks nil)
          (ghostherd-relay-kinds '(claude grok agy))
          (ghostherd-tests--fake-spawned nil)
          (ghostherd-backend 'fake)
          (ghostherd-worktree-root (concat base "wts/"))
          (ghostherd-lead-may-start t))
       (cl-letf (((symbol-function 'ghostherd--install-hooks) #'ignore)
                 ((symbol-function 'ghostherd--register-eval-cmds) #'ignore)
                 ((symbol-function 'ghostherd--ensure-poll-timer) #'ignore)
                 ((symbol-function 'ghostherd--host-kill) #'ignore)
                 ((symbol-function 'ghostherd--sidebar-refresh) #'ignore))
         (let ((wt-of (lambda (name)
                        (let ((wt (ghostherd--worktree-create repo name)))
                          (ghostherd-tests--session :name name :kind 'claude :state 'idle
                                                    :backend 'fake :project repo)
                          (puthash name wt ghostherd--worktrees)
                          (plist-get wt :path)))))
           ;; A read left going would be heard in the next test, by its tables.
           (unwind-protect (progn ,@body)
             (ghostherd-tests--gone-read-wait t)))))))

(defun ghostherd-tests--gone-reads ()
  "The background branch reads still going, or not yet heard from."
  (seq-filter (lambda (p) (and (string-prefix-p "ghostherd-gone" (process-name p))
                               (or (process-live-p p) (buffer-live-p (process-buffer p)))))
              (process-list)))

(defun ghostherd-tests--gone-read-wait (&optional quiet)
  "Wait, five seconds at most, for every background branch read to end
and be heard; unless QUIET, fail when one has not."
  (let ((until (+ (float-time) 5)))
    (while (and (ghostherd-tests--gone-reads) (< (float-time) until))
      (accept-process-output nil 0.02))
    (unless quiet
      (should-not (ghostherd-tests--gone-reads))
      (should (zerop (hash-table-count ghostherd--dir-gone-reading))))))

(defun ghostherd-tests--logged-p (re)
  "Non-nil when a log entry's text matches RE."
  (seq-find (lambda (e) (string-match-p re (ghostherd-log-entry-text e))) ghostherd--log))

(ert-deftest ghostherd-test-gone-the-poll-marks-and-unmarks ()
  "The poll looks once a minute: an agent whose worktree is gone is marked,
then where its branch stands is read; one whose directory is there is not.
A look inside the minute does nothing unless asked now.  One whose
directory is back is unmarked as soon as it is shown, not a minute on; an
agent that leaves the herd takes its mark with it."
  (ghostherd-tests--with-gone
    (let ((path (funcall wt-of "rd-tabs")))
      (ghostherd-tests--session :name "here" :kind 'claude :backend 'fake :project repo)
      (should-not (ghostherd--check-directories))
      (should (zerop (hash-table-count ghostherd--dir-gone)))
      (delete-directory path t)                  ; removed outside the herd
      (should-not (ghostherd--check-directories))  ; inside the minute: not looked
      (should-not (gethash "rd-tabs" ghostherd--dir-gone))
      (should (ghostherd--check-directories t))
      (let ((g (gethash "rd-tabs" ghostherd--dir-gone)))
        (should (equal (plist-get g :path) path))
        (should (eq (plist-get g :worktree) t)))
      (should-not (gethash "here" ghostherd--dir-gone))
      (should (ghostherd-tests--logged-p "its worktree .*rd-tabs/ is gone"))
      (ghostherd-tests--gone-read-wait)
      (should (eq (plist-get (gethash "rd-tabs" ghostherd--dir-gone) :state) 'merged))
      ;; A minute on, the poll looks by itself; still gone is no change.
      (setq ghostherd--dir-checked-at (- (float-time) ghostherd-directory-check-every 1))
      (should-not (ghostherd--check-directories))
      (should (< (- (float-time) ghostherd--dir-checked-at) 5))
      (ghostherd-tests--gone-read-wait)
      ;; Back: unmarked when shown.
      (make-directory path t)
      (should-not (ghostherd-session-directory-gone (ghostherd-get "rd-tabs")))
      (should-not (gethash "rd-tabs" ghostherd--dir-gone))
      (should (ghostherd-tests--logged-p "its directory is back"))
      ;; Gone again, then it leaves the herd: the mark goes with it.
      (delete-directory path t)
      (should (ghostherd--check-directories t))
      (ghostherd-tests--gone-read-wait)
      (remhash "rd-tabs" ghostherd--sessions)
      (should (ghostherd--check-directories t))
      (should (zerop (hash-table-count ghostherd--dir-gone)))
      ;; An agent with neither worktree nor project has nothing to look at.
      (ghostherd-tests--session :name "nowhere" :kind 'shell :backend 'fake)
      (should-not (ghostherd--check-directories t)))))

(ert-deftest ghostherd-test-gone-where-its-branch-stands ()
  "Where a gone worktree's branch stands, read by git in the main checkout:
merged; commits not on its base, and how many; deleted; or unknown when
the main checkout is gone as well.  The poll's background read and a
command's read now say the same."
  (ghostherd-tests--with-gone
    (let ((merged (funcall wt-of "m"))
          (ahead (funcall wt-of "a"))
          (deleted (funcall wt-of "d")))
      (funcall git ahead "commit" "-q" "--allow-empty" "-m" "one")
      (funcall git ahead "commit" "-q" "--allow-empty" "-m" "two")
      (delete-directory merged t)
      (delete-directory ahead t)
      ;; Removed as a lead does once it merged: the worktree and the branch.
      (funcall git repo "worktree" "remove" "--force" (directory-file-name deleted))
      (funcall git repo "branch" "-D" "d")
      (should (equal (ghostherd--gone-branch-now (ghostherd-get "m")) '(merged . 0)))
      (should (equal (ghostherd--gone-branch-now (ghostherd-get "a")) '(ahead . 2)))
      (should (equal (ghostherd--gone-branch-now (ghostherd-get "d")) '(deleted . 0)))
      (ghostherd--check-directories t)
      (ghostherd-tests--gone-read-wait)
      (should (equal (mapcar (lambda (n) (plist-get (gethash n ghostherd--dir-gone) :state))
                             '("m" "a" "d"))
                     '(merged ahead deleted)))
      (let ((a (gethash "a" ghostherd--dir-gone)))
        (should (= (plist-get a :ahead) 2))
        (should (equal (list (plist-get a :branch) (plist-get a :base)) '("a" "main")))
        (should (equal (plist-get a :path) ahead)))
      ;; The main checkout gone too: git cannot say.
      (delete-directory repo t)
      (should (equal (ghostherd--gone-branch-now (ghostherd-get "m")) '(unknown . 0)))
      (should (equal (ghostherd--gone-branch-parse "") '(unknown . 0)))
      (should (equal (ghostherd--gone-branch-parse "warning: x\n3\n") '(ahead . 3)))
      ;; No worktree: nothing to read.
      (ghostherd-tests--session :name "plain" :kind 'claude :backend 'fake :project repo)
      (should-not (ghostherd--gone-branch-now (ghostherd-get "plain"))))))

(ert-deftest ghostherd-test-gone-herd-list-says-so ()
  "`herd list' says an agent's directory is gone: the snapshot Emacs ticks
carries directoryMissing, true only for that agent."
  (ghostherd-tests--with-gone
    (let ((path (funcall wt-of "rd-tabs"))
          (row (lambda (rows name) (seq-find (lambda (r) (equal (plist-get r :name) name)) rows))))
      (ghostherd-tests--session :name "here" :kind 'claude :backend 'fake :project repo)
      (should (eq (plist-get (funcall row (ghostherd--herd-snapshot) "rd-tabs") :directoryMissing)
                  :json-false))
      (delete-directory path t)
      (ghostherd--check-directories t)
      (ghostherd-tests--gone-read-wait)
      (let ((rows (ghostherd--herd-snapshot)))
        (should (eq (plist-get (funcall row rows "rd-tabs") :directoryMissing) t))
        (should (equal (plist-get (funcall row rows "rd-tabs") :directory)
                       (ghostherd--abbreviate path)))
        (should (eq (plist-get (funcall row rows "here") :directoryMissing) :json-false))))))

(ert-deftest ghostherd-test-gone-spawn-refuses-a-missing-directory ()
  "Nothing starts an agent in a directory that is not there: tmux would
start it in the home directory instead, without a word.  Refused before
the host is asked, on tmux as on any backend."
  (ghostherd-tests--with-gone
    (let ((missing (concat base "no-such/"))
          (tmux-calls nil))
      (should (string-match-p
               "x.s directory .*no-such/ is not there: not starting it"
               (cadr (should-error (ghostherd-spawn 'claude :name "x" :project repo :directory missing
                                                    :display nil)
                                   :type 'user-error))))
      (should-not ghostherd-tests--fake-spawned)
      (should-not (ghostherd-get "x"))
      ;; A project that is gone, with no directory of its own: the same.
      (should-error (ghostherd-spawn 'claude :name "y" :project missing :display nil)
                    :type 'user-error)
      ;; On tmux, tmux is never asked.
      (cl-letf (((symbol-function 'ghostherd-tmux--ensure-available) #'ignore)
                ((symbol-function 'ghostherd-tmux--call)
                 (lambda (args) (push args tmux-calls) '(0 . ""))))
        (should-error (ghostherd-spawn 'claude :name "t" :project repo :directory missing
                                       :backend 'tmux :display nil)
                      :type 'user-error))
      (should-not tmux-calls)
      ;; There, it starts.
      (should (ghostherd-spawn 'claude :name "z" :project repo :display nil))
      (should (equal (plist-get ghostherd-tests--fake-spawned :directory) repo)))))

(ert-deftest ghostherd-test-gone-no-path-starts-it-there ()
  "An agent whose worktree is gone is started by no path: a respawn, fresh
or continuing; the resume backstop's fresh start; a relay, asked and when
its brief comes back; `herd relay'; Telegram's Respawn.  Each refuses
before it kills the old one, so the agent stays in the herd, marked, for
the user to stop."
  (ghostherd-tests--with-gone
    (let ((path (funcall wt-of "rd-tabs"))
          (spawned 0) (asked nil))
      (delete-directory path t)
      (cl-letf (((symbol-function 'ghostherd-spawn) (lambda (&rest _) (cl-incf spawned) nil))
                ((symbol-function 'ghostherd-memory-rpc-url) (lambda () "http://127.0.0.1:1/jsonrpc"))
                ((symbol-function 'ghostherd-memory-request-async)
                 (lambda (method &rest _) (push method asked)))
                ((symbol-function 'run-at-time) (lambda (_t _r fn &rest args) (apply fn args))))
        (let ((s (ghostherd-get "rd-tabs")))
          (dolist (continue '(nil t))
            (should (string-match-p
                     "rd-tabs.s worktree .*rd-tabs/ is gone: not respawning it (it would start in your home directory); stop it instead"
                     (cadr (should-error (ghostherd-respawn s continue) :type 'user-error)))))
          (should (gethash "rd-tabs" ghostherd--dir-gone)) ; marked by the refusal
          (should-error (ghostherd--respawn-fresh s) :type 'user-error)
          ;; Its resumed start died at once: the backstop leaves it as it is.
          (setf (ghostherd-session-args s) '("-r" "c1")
                (ghostherd-session-started-at s) (current-time))
          (ghostherd--resume-backstop s 'starting 'dead)
          (should (ghostherd-tests--logged-p "not started fresh: its directory is gone"))
          (should (string-match-p "is gone: not relaying it"
                                  (cadr (should-error (ghostherd-relay s 'grok) :type 'user-error))))
          (should-not (member "herd_relay_brief" asked))
          (should-not (gethash "rd-tabs" ghostherd--relaying))
          ;; The brief came back after it went: no heir.
          (should-not (ghostherd--relay-hand s 'grok "BRIEF" nil))
          (should (ghostherd-tests--logged-p "not relayed to grok: its directory is gone"))
          ;; Asked over the wire, by its lead.
          (ghostherd-tests--session :name "lead-p" :kind 'claude :backend 'fake :project repo)
          (puthash "lead-p" "lead" ghostherd--roles)
          (should (string-match-p "is gone: not relaying it"
                                  (ghostherd--relay-request "lead-p" "rd-tabs" "grok" "")))
          ;; Telegram's Respawn.
          (should (string-match-p "is gone: not respawning it"
                                  (plist-get (ghostherd--herd-run-command
                                              (list :id "c1" :op "respawn" :session "rd-tabs"))
                                             :text)))
          (should (= spawned 0))
          (should (eq (ghostherd-get "rd-tabs") s)))))))

(ert-deftest ghostherd-test-gone-restore-looks-at-once ()
  "A herd restored after an Emacs restart is looked at by the next poll,
not a minute on, so an agent restored with its worktree gone is marked at
once.  The restore itself starts nothing."
  (ghostherd-tests--with-gone
    (let ((ghostherd-known-backends '(fake))
          (ghostherd-tests--fake-recipes
           `((:name "rk" :kind claude :host-id "gh-rk" :project ,repo
              :worktree ,(concat base "wts/repo/rk/") :branch "rk" :base "main"))))
      (setq ghostherd--dir-checked-at (float-time))
      (ghostherd-restore)
      (should-not ghostherd-tests--fake-spawned)
      (should (= ghostherd--dir-checked-at 0))
      (should (ghostherd--check-directories))
      (ghostherd-tests--gone-read-wait)
      (should (eq (plist-get (gethash "rk" ghostherd--dir-gone) :state) 'deleted)))))

(ert-deftest ghostherd-test-gone-herd-stop-says-it-was-already-gone ()
  "`herd stop' of an agent whose worktree is gone says so -- not \"not
merged or not clean, so it was kept\" -- with where its branch stands, and
still removes the agent; the branch is left as it is."
  (ghostherd-tests--with-gone
    (pcase-dolist (`(,name ,commits ,re)
                   '(("m" 0 "its worktree .*/m/ was already gone (branch m is merged into main)\\'")
                     ("a" 1 "its worktree .*/a/ was already gone (branch a has 1 commit not on main; the branch is kept)\\'")
                     ("b" 3 "(branch b has 3 commits not on main; the branch is kept)\\'")))
      (let ((path (funcall wt-of name)))
        (puthash name "lead-p" ghostherd--started-by)
        (dotimes (_ commits) (funcall git path "commit" "-q" "--allow-empty" "-m" "w"))
        (delete-directory path t)
        (let ((r (ghostherd-lead-stop "lead-p" name)))
          (should (string-prefix-p (format "stopped %s; " name) r))
          (should (string-match-p re r))
          (should-not (string-match-p "not merged or not clean" r)))
        (should-not (ghostherd-get name))
        (should-not (gethash name ghostherd--dir-gone))
        (should (eq 0 (funcall git repo "rev-parse" "-q" "--verify" (concat "refs/heads/" name))))))
    ;; Removed with its branch, as a lead does after merging.
    (let ((path (funcall wt-of "d")))
      (puthash "d" "lead-p" ghostherd--started-by)
      (funcall git repo "worktree" "remove" "--force" (directory-file-name path))
      (funcall git repo "branch" "-D" "d")
      (should (string-match-p "was already gone (branch d is gone too)\\'"
                              (ghostherd-lead-stop "lead-p" "d"))))
    ;; No worktree, its directory gone.
    (let ((dir (concat base "proj/")))
      (make-directory dir)
      (ghostherd-tests--session :name "plain" :kind 'claude :backend 'fake :project dir)
      (puthash "plain" "lead-p" ghostherd--started-by)
      (delete-directory dir t)
      (should (string-match-p "\\`stopped plain; its directory .*/proj/ was already gone\\'"
                              (ghostherd-lead-stop "lead-p" "plain"))))
    ;; A worktree that is there is still removed when merged and clean.
    (funcall wt-of "ok")
    (puthash "ok" "lead-p" ghostherd--started-by)
    (should (string-match-p "merged and clean, so it was removed" (ghostherd-lead-stop "lead-p" "ok")))))

(ert-deftest ghostherd-test-gone-stop-it ()
  "The Inbox's Stop stops an agent whose directory is gone, as `herd stop'
does; one whose directory is there is not stopped -- what offered it was
out of date."
  (ghostherd-tests--with-gone
    (let ((path (funcall wt-of "rd-tabs")))
      (should (string-match-p "directory is back: not stopping it"
                              (cadr (should-error (ghostherd-stop-gone "rd-tabs") :type 'user-error))))
      (should (ghostherd-get "rd-tabs"))
      (delete-directory path t)
      (should (equal (ghostherd-stop-gone "rd-tabs") "Stopped rd-tabs; its worktree was already gone"))
      (should-not (ghostherd-get "rd-tabs"))
      (should-not (gethash "rd-tabs" ghostherd--dir-gone)))))

(ert-deftest ghostherd-test-gone-the-sidebar-row-says-so ()
  "The sidebar row of an agent whose worktree is gone says so: its mark in
the glyph column, `wt gone' for its state, a line in words above its
screen; `/gone' finds it, by the word.  Back, the row is as before."
  (ghostherd-tests--with-gone
    (let ((path (funcall wt-of "rd-tabs")))
      (delete-directory path t)
      (ghostherd--check-directories t)
      (ghostherd-tests--gone-read-wait)
      (let ((s (ghostherd-get "rd-tabs")))
        (should (equal (ghostherd--sidebar-cell s 'state 'idle 'default) "wt gone"))
        (should (string-suffix-p ghostherd-gone-glyph (ghostherd--sidebar-cell s 'glyph 'idle 'default)))
        (let ((ghostherd--sidebar-target-width 300))
          (should (string-match-p "\\`worktree .*rd-tabs/ is gone: a respawn is refused; stop it instead\n"
                                  (ghostherd--sidebar-preview-body s))))
        (should (string-suffix-p " gone" (ghostherd--sidebar-session-haystack s)))
        (make-directory path t)
        (should (equal (ghostherd--sidebar-cell s 'state 'idle 'default) "idle"))
        (should-not (string-suffix-p ghostherd-gone-glyph (ghostherd--sidebar-cell s 'glyph 'idle 'default)))
        (should-not (string-suffix-p " gone" (ghostherd--sidebar-session-haystack s)))))))

(ert-deftest ghostherd-test-gone-inbox-item-by-its-branch ()
  "An agent whose directory is gone is a Needs-you item with a Stop, its
words by where its branch stands: merged, or deleted as after a merge --
safe to stop; commits not on its base -- work may be lost, and how many;
unknown or not read yet -- look first.  None while it is there."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let* ((gone (lambda (&rest g)
                 (append g (list :path "~/.ghostherd/worktrees/jnotes/rd-tabs/" :worktree t
                                 :branch "rd-tabs" :base "master" :state "" :ahead 0 :since 1000))))
         (item (lambda (g)
                 (seq-find (lambda (x) (equal (plist-get x :kind) "gone"))
                           (ghostherd-inbox--agent-items
                            (list :name "rd-tabs" :kind "claude" :state "idle" :role ""
                                  :project "~/src/jnotes/" :gone g)))))
         (merged (funcall item (funcall gone :state "merged")))
         (deleted (funcall item (funcall gone :state "deleted")))
         (ahead (funcall item (funcall gone :state "ahead" :ahead 3)))
         (one (funcall item (funcall gone :state "ahead" :ahead 1)))
         (unknown (funcall item (funcall gone :state "unknown")))
         (reading (funcall item (funcall gone)))
         (plain (funcall item (list :path "~/src/scratch/" :worktree :json-false :branch "" :base ""
                                    :state "" :ahead 0 :since 1000))))
    (should (equal (plist-get merged :id) "gone:rd-tabs"))
    (should (equal (plist-get merged :group) "need"))
    (should (equal (plist-get merged :tag) "Gone"))
    (should (equal (plist-get merged :at) 1000))
    (should (equal (plist-get merged :title) "Worktree gone — safe to stop"))
    (should (equal (plist-get merged :preview)
                   "Branch rd-tabs is merged into master: nothing is lost by stopping it"))
    (should (string-match-p "\\`rd-tabs's worktree `~/.ghostherd/worktrees/jnotes/rd-tabs/` is gone: it was removed outside the herd."
                            (plist-get merged :body)))
    (should (string-match-p "nothing is lost by stopping it" (plist-get merged :body)))
    (should (string-match-p "A respawn is refused: it would start in your home directory." (plist-get merged :body)))
    (should (eq (plist-get merged :safe) t))
    (should (equal (plist-get deleted :title) "Worktree gone — safe to stop"))
    (should (string-match-p "gone too, as after a merge" (plist-get deleted :preview)))
    (should (eq (plist-get deleted :safe) t))
    (should (equal (plist-get ahead :title) "Worktree gone — 3 commits not merged"))
    (should (equal (plist-get ahead :preview)
                   "Branch rd-tabs has 3 commits not on master: stopping it may lose that work"))
    (should (string-match-p "still has 3 commits that are not on `master`" (plist-get ahead :body)))
    (should (string-match-p "that work may be lost" (plist-get ahead :body)))
    (should (eq (plist-get ahead :safe) :json-false))
    (should (equal (plist-get ahead :tone) "red"))
    (should (equal (plist-get merged :tone) "warn"))
    (should (equal (plist-get one :title) "Worktree gone — 1 commit not merged"))
    (should (string-match-p "1 commit that is not on" (plist-get one :body)))
    (should (eq (plist-get unknown :safe) :json-false))
    (should (string-match-p "Could not tell" (plist-get unknown :preview)))
    (should (string-match-p "Reading where branch rd-tabs stands" (plist-get reading :preview)))
    (should (eq (plist-get reading :safe) :json-false))
    (should (equal (plist-get plain :title) "Directory gone"))
    (should (equal (plist-get plain :whyLabel) "Its directory is gone"))
    (should (eq (plist-get plain :safe) t))
    (should-not (funcall item nil))))

(ert-deftest ghostherd-test-gone-the-page-has-it-and-stops-it ()
  "The page's agent carries where its gone worktree's branch stands (null
for one that is there), so its card and its Inbox item say so; the Stop
they offer stops it, and a Respawn of it is refused on the page."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (ghostherd-tests--with-gone
    (let ((path (funcall wt-of "rd-tabs"))
          (pushes nil))
      (ghostherd-tests--session :name "here" :kind 'claude :backend 'fake :project repo)
      (funcall git path "commit" "-q" "--allow-empty" "-m" "w")
      (delete-directory path t)
      (ghostherd--check-directories t)
      (ghostherd-tests--gone-read-wait)
      (let ((g (plist-get (ghostherd-memory-page--agent (ghostherd-get "rd-tabs")) :gone)))
        (should (equal (plist-get g :state) "ahead"))
        (should (= (plist-get g :ahead) 1))
        (should (eq (plist-get g :worktree) t))
        (should (equal (list (plist-get g :branch) (plist-get g :base)) '("rd-tabs" "main")))
        (should (equal (plist-get g :path) (ghostherd--abbreviate path))))
      (should-not (plist-get (ghostherd-memory-page--agent (ghostherd-get "here")) :gone))
      (should (equal (mapcar (lambda (it) (plist-get it :id))
                             (seq-filter (lambda (it) (equal (plist-get it :kind) "gone"))
                                         (mapcan #'ghostherd-inbox--agent-items
                                                 (mapcar #'ghostherd-memory-page--agent (ghostherd-sessions)))))
                     '("gone:rd-tabs")))
      (cl-letf (((symbol-function 'ghostherd-memory-page--js) (lambda (fn obj) (push (cons fn obj) pushes)))
                ((symbol-function 'ghostherd-memory-page--send-herd) #'ignore)
                ((symbol-function 'ghostherd-memory-page--serve-files) #'ignore))
        (ghostherd-memory-page--handle '((op . "agent-respawn") (name . "rd-tabs") (continue . t)))
        (should (string-match-p "is gone: not respawning it" (cdr (assoc "showError" pushes))))
        (should (ghostherd-get "rd-tabs"))
        (ghostherd-memory-page--handle '((op . "agent-stop-gone") (name . "rd-tabs")))
        (should (equal (cdr (assoc "flash" pushes)) "Stopped rd-tabs; its worktree was already gone"))
        (should-not (ghostherd-get "rd-tabs"))
        ;; One that is there is not stopped by it.
        (ghostherd-memory-page--handle '((op . "agent-stop-gone") (name . "here")))
        (should (string-match-p "back: not stopping it" (cdr (assoc "showError" pushes))))
        (should (ghostherd-get "here"))))))

(provide 'ghostherd-worktree-tests)
;;; ghostherd-worktree-tests.el ends here

;;; ghostherd-worktree.el --- Git worktrees for agents, their trust, a directory gone -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of ghostherd (see ghostherd.el).  The git worktrees the herd
;; makes for its agents: where they go, the CLIs' trust for them, the
;; dependencies a new one is given, and an agent whose directory or
;; branch is gone.

;;; Code:

(require 'ghostherd-core)

(declare-function ghostherd--log-add "ghostherd-log" (session kind text &optional screen why pinned note))
(declare-function ghostherd--sidebar-refresh "ghostherd" ())

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

(defvar ghostherd--started-by (make-hash-table :test 'equal)
  "Session id -> the lead that started it through `herd start', or nothing.
A table rather than a slot, as the role and worktree are; written onto the
host with the recipe, so a lead still owns the agents it started after a
restart.  `herd stop' and the reuse in `herd start' act only on these.")

(defun ghostherd-session-started-by (session)
  "The lead that started SESSION through the herd, or nil."
  (gethash (ghostherd-session-id session) ghostherd--started-by))

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

;;; Trust for the worktrees the herd creates (ask 2befbbd6)
;;
;; Each CLI asks, the first time it starts in a folder, whether to trust
;; it, and an agent parked on that prompt takes no work until the user
;; answers.  A worktree the herd has just created off a project its CLI
;; already trusts, by the CLI's own record, is the same repository: the
;; herd records it trusted in that CLI's store, so the agent starts at its
;; prompt.  Nothing else is trusted for the user -- a project the CLI does
;; not trust, a directory typed in, a conversation's own, the main
;; checkout.  bin/herd_trust.py reads and writes each store as its CLI
;; does, one entry at a time; what it added is dropped with the worktree.

(defcustom ghostherd-pretrust t
  "Non-nil: a worktree the herd creates is recorded trusted in its agent's
CLI's store when that CLI trusts the project's main checkout, so the
agent does not start on a trust prompt.  See `ghostherd-trust-stores'."
  :type 'boolean
  :group 'ghostherd)

(defcustom ghostherd-trust-stores
  `((grok . ,(expand-file-name "trusted_folders.toml" (or (getenv "GROK_HOME") "~/.grok")))
    (agy . "~/.gemini/antigravity-cli/settings.json"))
  "Where each CLI keeps the folders it trusts, as (KIND . FILE).
grok: a [folders.\"PATH\"] table of `trusted' and `decided_at'.  agy: its
settings' \"trustedWorkspaces\" list.  claude needs no entry: it counts
a linked worktree trusted when its main checkout is."
  :type '(alist :key-type symbol :value-type file)
  :group 'ghostherd)

(defcustom ghostherd-pretrust-file (locate-user-emacs-file "ghostherd-pretrust.eld")
  "Where the herd keeps the trust it recorded -- (:path :kind :store
:name) each -- to drop it when it removes the worktree."
  :type 'file
  :group 'ghostherd)

(defun ghostherd--trust-run (op kind store path)
  "Run bin/herd_trust.py OP for KIND's STORE and PATH: (STATUS . OUTPUT)."
  (let ((script (and (fboundp 'ghostherd-herd-client) (ghostherd-herd-client)
                     (expand-file-name "herd_trust.py"
                                       (file-name-directory (ghostherd-herd-client))))))
    (if (not (and script (file-executable-p script)))
        (cons -1 "bin/herd_trust.py not found")
      (with-temp-buffer
        (let ((status (ignore-errors
                        (call-process script nil '(t t) nil op (symbol-name kind)
                                      (expand-file-name store) path))))
          (cons (or status -1) (string-trim (buffer-string))))))))

(defun ghostherd--pretrusted ()
  "The trust the herd recorded, read from `ghostherd-pretrust-file'."
  (and ghostherd-pretrust-file (file-exists-p ghostherd-pretrust-file)
       (ignore-errors
         (with-temp-buffer
           (insert-file-contents ghostherd-pretrust-file)
           (read (current-buffer))))))

(defun ghostherd--pretrusted-save (entries)
  "Keep ENTRIES as the trust the herd recorded."
  (when ghostherd-pretrust-file
    (make-directory (file-name-directory (expand-file-name ghostherd-pretrust-file)) t)
    (with-temp-file ghostherd-pretrust-file
      (let ((print-length nil) (print-level nil))
        (prin1 entries (current-buffer))))))

(defun ghostherd--worktree-pretrust (kind project path name)
  "Record worktree PATH, just created off PROJECT for agent NAME, trusted
in KIND's store when KIND trusts PROJECT's main checkout by that store.
Logged under NAME; non-nil when recorded."
  (when-let* ((ghostherd-pretrust)
              (store (alist-get kind ghostherd-trust-stores))
              (root (file-name-as-directory (expand-file-name ghostherd-worktree-root)))
              (path (directory-file-name (file-truename path)))
              ((string-prefix-p (file-name-as-directory (file-truename root))
                                (file-name-as-directory path)))
              (main (directory-file-name (file-truename project)))
              ((eq 0 (car (ghostherd--trust-run "trusts" kind store main)))))
    (let ((r (ghostherd--trust-run "add" kind store path)))
      (cond
       ((not (eq 0 (car r)))
        (ghostherd--log-add name 'life (format "not trusted for %s: %s" kind (cdr r)))
        nil)
       ((not (equal (cdr r) "added")) nil)
       ;; Unrecorded, it would never be dropped: take it back.
       ((not (ignore-errors
               (ghostherd--pretrusted-save
                (cons (list :path path :kind kind :store store :name name)
                      (ghostherd--pretrusted)))
               t))
        (ghostherd--trust-run "remove" kind store path)
        (ghostherd--log-add name 'life (format "not trusted for %s: %s cannot be written"
                                               kind ghostherd-pretrust-file))
        nil)
       (t
        (ghostherd--log-add name 'life
                            (format "trusted %s for %s, as it trusts %s"
                                    (abbreviate-file-name path) kind
                                    (abbreviate-file-name main)))
        t)))))

(defun ghostherd--worktree-untrust (path)
  "Drop the trust the herd recorded for worktree PATH, now removed."
  (let* ((path (directory-file-name (expand-file-name path)))
         (all (ghostherd--pretrusted))
         (mine (seq-filter (lambda (e) (member (plist-get e :path)
                                               (list path (ignore-errors
                                                            (directory-file-name (file-truename path))))))
                           all)))
    (dolist (e mine)
      (let ((r (ghostherd--trust-run "remove" (plist-get e :kind) (plist-get e :store)
                                     (plist-get e :path))))
        (ghostherd--log-add (or (plist-get e :name) path) 'life
                            (if (eq 0 (car r))
                                (format "dropped the trust it gave %s in %s"
                                        (plist-get e :kind) (abbreviate-file-name path))
                              (format "could not drop the trust it gave %s in %s: %s"
                                      (plist-get e :kind) (abbreviate-file-name path) (cdr r))))))
    (when mine
      (ghostherd--pretrusted-save (seq-difference all mine)))))

(defun ghostherd--worktree-create (project name &optional branch kind)
  "Create a git worktree of PROJECT for agent NAME; return its plist.
The worktree goes under `ghostherd-worktree-root' on a new BRANCH (NAME by
default) off PROJECT's current HEAD.  Returns (:path :branch :base).
For an agent of KIND, it is recorded trusted where KIND trusts PROJECT
(`ghostherd--worktree-pretrust').  Signals a `user-error' when git
cannot, so the caller may fall back to a plain directory."
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
    (when kind
      (ghostherd--worktree-pretrust kind project path name))
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

(defcustom ghostherd-worktree-stat-ttl 20
  "Seconds a worktree's commits-ahead and merged state are cached for the
snapshot `herd list' reads, so git is not run every poll.  A worktree's
branch moves rarely; this bounds the cost to one check every so often."
  :type 'number
  :group 'ghostherd)

(defvar ghostherd--worktree-stat (make-hash-table :test 'equal)
  "Session id -> (AT AHEAD MERGED): a worktree's commits ahead of its base
and whether it is merged, cached for `ghostherd-worktree-stat-ttl' seconds.")

(defun ghostherd--worktree-stat (session)
  "(:ahead N :merged BOOL) for SESSION's worktree, cached; nil with no
worktree.  AHEAD is its own commits past its base; MERGED is t once the
base contains them -- the work is in, the worktree removable."
  (when-let* ((wt (ghostherd-session-worktree session))
              (path (plist-get wt :path))
              (branch (plist-get wt :branch))
              (base (plist-get wt :base))
              (main (ghostherd-session-project session)))
    (let* ((id (ghostherd-session-id session))
           (c (gethash id ghostherd--worktree-stat)))
      (if (and c (< (- (float-time) (nth 0 c)) ghostherd-worktree-stat-ttl))
          (list :ahead (nth 1 c) :merged (nth 2 c))
        (let* ((ahead (or (and (file-directory-p (expand-file-name main))
                               (let ((default-directory
                                      (file-name-as-directory (expand-file-name main))))
                                 (with-temp-buffer
                                   (and (eq 0 (ignore-errors
                                                (call-process "git" nil '(t nil) nil "rev-list"
                                                              "--count" (concat base ".." branch))))
                                        (string-to-number (string-trim (buffer-string)))))))
                          0))
               (merged (ghostherd--worktree-merged-p main branch base)))
          (puthash id (list (float-time) ahead merged) ghostherd--worktree-stat)
          (list :ahead ahead :merged merged))))))

;;; An agent whose directory is gone
;;
;; A worktree removed outside the herd -- `git worktree remove' by hand
;; once its branch merged, an `rm -rf' -- leaves its agent where it was,
;; and the herd went on as if the directory were there: `herd list' named
;; it, `herd stop' said the worktree "was not merged or not clean, so it
;; was kept", and a respawn started the agent in the home directory --
;; tmux's `new-session -c' falls back to $HOME, without a word, when the
;; directory is missing, and there claude asked to trust all of ~.  So
;; the poll looks, once a minute, whether each agent's directory is still
;; there.  One that is gone is marked -- its sidebar row, its card, `herd
;; list' -- the Inbox tells the user where its branch stands, and nothing
;; starts it there again.  It is never stopped for the user: whether its
;; work is safe to leave is theirs to say.

(defcustom ghostherd-directory-check-every 60
  "Seconds between the poll's looks at whether each agent's directory is
still there: a stat per agent.  One found gone has its branch read again,
by git in the background, at each look; one marked gone is looked at
again whenever it is shown, so it is unmarked as soon as it is back."
  :type 'number
  :group 'ghostherd)

(defcustom ghostherd-gone-glyph "✗"
  "Marker in the sidebar's glyph column for an agent whose directory is
gone.  It takes the spare character the detached marker rides in."
  :type 'string
  :group 'ghostherd)

(defvar ghostherd--dir-gone (make-hash-table :test 'equal)
  "Session id -> a plist of its directory that is gone: :path, :since
\(float time) and :worktree; for a worktree, once git has said where its
branch stands, :branch, :base, :state (`merged', `deleted', `ahead' or
`unknown') and :ahead, its commits not on base.")

(defvar ghostherd--dir-gone-reading (make-hash-table :test 'equal)
  "Session id -> the git process reading where its gone worktree's branch
stands, while it does.")

(defvar ghostherd--dir-checked-at 0
  "When the poll last looked at every agent's directory (float time).")

(defun ghostherd--recorded-directory (session)
  "Where SESSION works by its recipe -- its worktree, else its project --
which is where a respawn would start it; nil when it has neither."
  (or (plist-get (ghostherd-session-worktree session) :path)
      (ghostherd-session-project session)))

(defun ghostherd--directory-missing-p (dir)
  "Non-nil when DIR, a local directory, is not there; nil for no DIR.
No file-name handlers: an agent's directory is local (tmux starts it
there), and their regexps would cost more than the stat."
  (and (stringp dir) (not (string-empty-p dir))
       (let ((file-name-handler-alist nil))
         (not (file-directory-p (expand-file-name dir))))))

(defun ghostherd--mark-gone (session &optional no-read)
  "Mark SESSION's directory gone, when it is and is not marked yet; then,
unless NO-READ, read where its branch stands.  Its entry, or nil when it
is there."
  (let* ((id (ghostherd-session-id session))
         (dir (ghostherd--recorded-directory session))
         (was (gethash id ghostherd--dir-gone)))
    (when (ghostherd--directory-missing-p dir)
      (unless (and was (equal (plist-get was :path) dir))
        (let ((worktree (and (plist-get (ghostherd-session-worktree session) :path) t)))
          (puthash id (list :path dir :since (float-time) :worktree worktree)
                   ghostherd--dir-gone)
          (ghostherd--log-add session 'life
                              (format "its %s %s is gone"
                                      (if worktree "worktree" "directory")
                                      (abbreviate-file-name dir)))))
      (unless no-read
        (ghostherd--gone-branch-read session))
      (gethash id ghostherd--dir-gone))))

(defun ghostherd--unmark-gone (session)
  "Unmark SESSION, whose directory is back."
  (when (gethash (ghostherd-session-id session) ghostherd--dir-gone)
    (remhash (ghostherd-session-id session) ghostherd--dir-gone)
    (ghostherd--log-add session 'life "its directory is back")))

(defun ghostherd-session-directory-gone (session)
  "SESSION's gone directory as `ghostherd--dir-gone' has it, or nil.
Looked at again here -- one stat, and only for an agent marked gone -- so
one whose directory is back is unmarked at once, not at the next look."
  (when-let* ((gone (gethash (ghostherd-session-id session) ghostherd--dir-gone)))
    (if (ghostherd--directory-missing-p (plist-get gone :path))
        gone
      (ghostherd--unmark-gone session)
      nil)))

(defun ghostherd--check-directories (&optional now)
  "Mark each agent whose directory is gone and unmark each whose is back;
at most once every `ghostherd-directory-check-every' seconds, unless NOW.
From the poll.  Returns non-nil when a mark changed."
  (when (or now (>= (- (float-time) ghostherd--dir-checked-at)
                    ghostherd-directory-check-every))
    (setq ghostherd--dir-checked-at (float-time))
    (let ((before (hash-table-count ghostherd--dir-gone))
          (paths (let (l) (maphash (lambda (id g) (push (cons id (plist-get g :path)) l))
                                   ghostherd--dir-gone)
                      l))
          changed)
      (dolist (s (hash-table-values ghostherd--sessions))
        (if (ghostherd--mark-gone s)
            (unless (equal (cdr (assoc (ghostherd-session-id s) paths))
                           (ghostherd--recorded-directory s))
              (setq changed t))
          (when (gethash (ghostherd-session-id s) ghostherd--dir-gone)
            (ghostherd--unmark-gone s)
            (setq changed t))))
      ;; An agent that left the herd takes its mark with it.
      (dolist (id (mapcar #'car paths))
        (unless (gethash id ghostherd--sessions)
          (remhash id ghostherd--dir-gone)))
      (when (or changed (/= before (hash-table-count ghostherd--dir-gone)))
        (ghostherd--sidebar-refresh)
        t))))

(defconst ghostherd--gone-branch-script
  "cd \"$1\" 2>/dev/null && git rev-parse --git-dir >/dev/null 2>&1 || { echo unknown; exit 0; }
git rev-parse -q --verify \"refs/heads/$2\" >/dev/null 2>&1 || { echo deleted; exit 0; }
git rev-list --count \"$3..refs/heads/$2\" 2>/dev/null || echo unknown"
  "Where a gone worktree's branch stands, read in its main checkout $1:
`deleted' when branch $2 is no more; else how many of its commits are not
on base $3, 0 when it is merged; `unknown' when git cannot say -- the
main checkout gone as well, or the base.")

(defun ghostherd--gone-branch-parse (out)
  "(STATE . AHEAD) from `ghostherd--gone-branch-script''s OUT: STATE
`merged', `deleted', `ahead' or `unknown', AHEAD the commits not on base."
  (let ((s (car (last (split-string (or out "") "\n" t "[ \t\r]+")))))
    (cond ((equal s "deleted") '(deleted . 0))
          ((and s (string-match-p "\\`[0-9]+\\'" s))
           (let ((n (string-to-number s)))
             (if (zerop n) '(merged . 0) (cons 'ahead n))))
          (t '(unknown . 0)))))

(defun ghostherd--gone-branch-args (session)
  "The script's arguments (MAIN BRANCH BASE) for SESSION's worktree, or
nil when it has none to read."
  (let* ((wt (ghostherd-session-worktree session))
         (branch (plist-get wt :branch))
         (base (plist-get wt :base))
         (main (ghostherd-session-project session)))
    (and (plist-get wt :path) (stringp branch) (stringp base) (stringp main)
         (list (expand-file-name main) branch base))))

(defun ghostherd--gone-branch-put (session args out)
  "Put where SESSION's branch stands, from the script's OUT for ARGS, into
its gone entry, when it still has one."
  (let ((id (ghostherd-session-id session)))
    (when-let* ((gone (gethash id ghostherd--dir-gone)))
      (pcase-let ((`(,state . ,ahead) (ghostherd--gone-branch-parse out)))
        (puthash id (append (list :branch (nth 1 args) :base (nth 2 args)
                                  :state state :ahead ahead)
                            (list :path (plist-get gone :path) :since (plist-get gone :since)
                                  :worktree (plist-get gone :worktree)))
                 ghostherd--dir-gone)))))

(defun ghostherd--gone-branch-read (session)
  "Read where SESSION's gone worktree's branch stands into its entry, in
the background: git takes milliseconds a call, and this runs from the
poll.  One read at a time per agent; nothing for one with no worktree."
  (let ((id (ghostherd-session-id session))
        (args (ghostherd--gone-branch-args session)))
    (when (and args (not (process-live-p (gethash id ghostherd--dir-gone-reading))))
      (let ((buf (generate-new-buffer " *ghostherd-gone*"))
            ;; The buffer the timer runs in may itself be in a directory
            ;; that is gone, and a process cannot start there.
            (default-directory "/"))
        (condition-case err
            (puthash
             id
             (make-process
              :name "ghostherd-gone" :buffer buf :noquery t :connection-type 'pipe
              :command (append (list "sh" "-c" ghostherd--gone-branch-script "sh") args)
              :sentinel
              (lambda (proc _event)
                (unless (process-live-p proc)
                  ;; Its own entry only: an agent stopped and started again
                  ;; under the name may be reading by now.
                  (when (eq (gethash id ghostherd--dir-gone-reading) proc)
                    (remhash id ghostherd--dir-gone-reading))
                  (let ((out (and (buffer-live-p buf)
                                  (with-current-buffer buf (buffer-string)))))
                    (when (buffer-live-p buf) (kill-buffer buf))
                    (when (eq (ghostherd-get id) session)
                      (ghostherd--gone-branch-put session args out))))))
             ghostherd--dir-gone-reading)
          (error
           (when (buffer-live-p buf) (kill-buffer buf))
           (message "ghostherd: reading %s's branch: %s" id (error-message-string err))))))))

(defun ghostherd--gone-branch-now (session)
  "Where SESSION's gone worktree's branch stands, read now: (STATE . AHEAD)
as `ghostherd--gone-branch-parse' gives it, or nil with no worktree.  For
a command, not the poll: it waits on git."
  (when-let* ((args (ghostherd--gone-branch-args session)))
    (with-temp-buffer
      (let ((default-directory "/"))
        (apply #'call-process "sh" nil '(t nil) nil "-c" ghostherd--gone-branch-script "sh" args))
      (let ((out (buffer-string)))
        (ghostherd--gone-branch-put session args out)
        (ghostherd--gone-branch-parse out)))))

(defun ghostherd--gone-branch-words (branch base state ahead)
  "Where BRANCH stands against BASE, by STATE and AHEAD, in a few words."
  (pcase state
    ('merged (format "branch %s is merged into %s" branch base))
    ('deleted (format "branch %s is gone too" branch))
    ('ahead (format "branch %s has %d commit%s not on %s" branch ahead
                    (if (= ahead 1) "" "s") base))
    (_ (format "whether branch %s is merged into %s could not be read" branch base))))

(defun ghostherd--refuse-gone (session doing)
  "Signal a `user-error' when SESSION's directory is gone, rather than
DOING it (\"respawning\", \"relaying\"): it would start in the home
directory.  SESSION is marked gone first, so the Inbox says so."
  (when (ghostherd--mark-gone session)
    (let ((gone (gethash (ghostherd-session-id session) ghostherd--dir-gone)))
      (user-error "%s's %s %s is gone: not %s it (it would start in your home directory); stop it instead"
                  (ghostherd-session-name session)
                  (if (plist-get gone :worktree) "worktree" "directory")
                  (abbreviate-file-name (plist-get gone :path))
                  doing))))

(defun ghostherd--worktree-remove (main path &optional force branch)
  "Remove git worktree PATH of the MAIN checkout; return t on success.
With FORCE, discard the worktree's uncommitted changes.  With BRANCH, also
delete it with `git branch -d', which refuses an unmerged branch -- so a
branch's committed work is never lost here.  The trust the herd recorded
for it goes with it (`ghostherd--worktree-untrust')."
  (let ((default-directory (file-name-as-directory (expand-file-name main))))
    (with-temp-buffer
      (when (eq 0 (apply #'call-process "git" nil '(t t) nil
                         `("worktree" "remove" ,@(and force '("--force"))
                           ,(directory-file-name (expand-file-name path)))))
        (when branch
          (call-process "git" nil nil nil "branch" "-d" branch))
        (ghostherd--worktree-untrust path)
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
;; Carrying them in makes a worktree usable at once.  A uv project's venv
;; is the worktree's own, synced from its own lock: a symlink to the main
;; checkout's would let a `uv sync' or `uv run' in the worktree rewrite the
;; main checkout's venv to the branch's lock.  uv clones from its cache
;; (copy-on-write on APFS), so that costs little disk and, warm, well under
;; a second.  Any other venv holds absolute paths, so it is symlinked to the
;; main checkout's rather than copied; a node_modules is cloned
;; copy-on-write (APFS `cp -c'), so the worktree may diverge it cheaply.
;; Build caches are simply left off the list.  A clone that cannot be made
;; is skipped and said so on the card -- never a full copy, since the disk
;; is not big.  All of it runs in the background.

(defcustom ghostherd-worktree-deps
  '((".venv" . uv) ("venv" . uv) ("node_modules" . clone))
  "Dependencies carried into a new worktree, each (PATH . METHOD).
PATH is relative to the checkout root; a bare name (no slash) is found by
name among the main checkout's ignored directories at any depth, so
`.venv' also finds server/.venv.  METHOD is one of:
 - `uv' -- the worktree's own venv when the venv's directory in the
   worktree has a `uv.lock' and `uv' is on PATH: `uv sync --frozen' there,
   from the worktree's own lock.  uv clones from its cache (copy-on-write
   on APFS, its default link mode on macOS), so it is cheap, and a `uv
   sync' in the worktree then never touches the main checkout's venv, as
   it would through a symlink.  With no `uv.lock' there or no `uv', a
   symlink, as `symlink'.  A sync that fails is skipped and said so on the
   card, its partial venv removed -- never a symlink in its place;
 - `symlink' -- a link to the main checkout's, for a thing that holds
   absolute paths and breaks when copied, like a venv uv does not manage;
 - `clone' -- an APFS copy-on-write copy (cp -c) the worktree may then
   diverge, like node_modules;
 - `venv' -- a per-worktree virtualenv made at PATH as named (never
   found), by `uv sync --frozen' when PATH's directory has a `uv.lock' --
   for a venv the main checkout does not have.  Skipped, and said so on the
   card, when there is no `uv.lock' there or no `uv' on PATH -- never a
   lock-less install.
A uv/symlink/clone PATH with no source in the main checkout is skipped; a
clone that cannot be made is skipped and said so, never copied in full.
Build caches are left off this list.  Override per project with a
`ghostherd-project-worktree-deps' dir-local; your own value of this, or
that dir-local, wins over the default."
  :type '(alist :key-type string
                :value-type (choice (const uv) (const symlink) (const clone) (const venv)))
  :group 'ghostherd)

(defvar ghostherd-project-worktree-deps 'unset
  "Per-project override of `ghostherd-worktree-deps', from .dir-locals.el.

  ((nil . ((ghostherd-project-worktree-deps . ((\".venv\" . venv))))))")
(put 'ghostherd-project-worktree-deps 'safe-local-variable
     (lambda (v) (and (listp v)
                      (seq-every-p (lambda (e) (and (consp e) (stringp (car e))
                                                    (memq (cdr e) '(uv symlink clone venv))))
                                   v))))

(defvar ghostherd--worktree-deps-status (make-hash-table :test 'equal)
  "Session id -> alist of its worktree dependencies' outcome: PATH ->
(:status STATUS :how HOW :reason REASON), STATUS `done' or `skipped', HOW
for a done one what it is: \"uv\" (its own venv), \"symlink\" or
\"clone\".")

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
        (cond
         ;; A venv is created, not found, so its path is taken as named
         ;; (relative to the worktree) -- not matched against existing dirs.
         ((eq method 'venv)
          (push (cons (directory-file-name name) method) out))
         ((string-match-p "/" name)
          (push (cons (directory-file-name name) method) out))
         (t (dolist (d ignored)
              (when (equal (file-name-nondirectory d) name)
                (push (cons d method) out)))))))
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
status line per spec -- `done PATH HOW' or `skip PATH REASON' -- and never
copying a dependency in full."
  (mapconcat
   (lambda (spec)
     (let* ((name (car spec))
            (src (shell-quote-argument (directory-file-name (expand-file-name name main))))
            (dstf (directory-file-name (expand-file-name name worktree)))
            (dst (shell-quote-argument dstf))
            (par (shell-quote-argument (directory-file-name (file-name-directory dstf))))
            (tag (shell-quote-argument name))
            (link (format "elif mkdir -p %s 2>/dev/null && ln -s %s %s 2>/dev/null; then echo done %s symlink; else echo skip %s link-failed; fi"
                          par src dst tag tag)))
       (pcase (cdr spec)
         ('symlink
          (format "if [ ! -e %s ]; then echo skip %s no-source; elif [ -e %s ]; then echo skip %s exists; %s"
                  src tag dst tag link))
         ('clone
          (format "if [ ! -e %s ]; then echo skip %s no-source; elif [ -e %s ]; then echo skip %s exists; elif mkdir -p %s 2>/dev/null && cp -c -R %s %s 2>/dev/null; then echo done %s clone; else echo skip %s no-copy-on-write; fi"
                  src tag dst tag par src dst tag tag))
         ;; The worktree's own venv, from its own lock, when its directory
         ;; has one and there is a uv: `uv sync --frozen' there, uv cloning
         ;; from its cache.  Else the symlink, as before.  A failed sync
         ;; is said so, its partial venv gone -- not a symlink, which would
         ;; bring back a worktree's `uv sync' writing the main checkout's.
         ('uv
          (format "if [ ! -e %s ]; then echo skip %s no-source; elif [ -e %s ] || [ -L %s ]; then echo skip %s exists; elif [ -f %s/uv.lock ] && command -v uv >/dev/null 2>&1; then if ( cd %s && UV_PROJECT_ENVIRONMENT=%s uv sync --frozen >/dev/null 2>&1 ); then echo done %s uv; else rm -rf %s; echo skip %s uv-sync-failed; fi; %s"
                  src tag dst dst tag par par dst tag dst tag link))
         ;; A fresh virtualenv from the worktree's own lock: `uv sync
         ;; --frozen' in the venv's directory (where uv.lock sits), uv
         ;; cloning from its cache.  Skipped, not faked, when there is
         ;; no uv.lock there or no uv -- never a lock-less install.
         ('venv
          (format "if [ -e %s ]; then echo skip %s exists; elif ! command -v uv >/dev/null 2>&1; then echo skip %s no-uv; elif [ ! -f %s/uv.lock ]; then echo skip %s no-uv.lock; elif ( cd %s && UV_PROJECT_ENVIRONMENT=%s uv sync --frozen >/dev/null 2>&1 ); then echo done %s uv; else echo skip %s uv-sync-failed; fi"
                  dst tag tag par tag par dst tag tag))
         (_ (format "echo skip %s unknown-method" tag)))))
   specs
   "\n"))

(defun ghostherd--worktree-deps-record (id output)
  "Store the dependency OUTPUT (status lines) for session ID."
  (let (alist)
    (dolist (line (split-string (or output "") "\n" t))
      (pcase (split-string (string-trim line) " ")
        (`("done" ,name . ,how)
         (push (cons name (list :status 'done :how (car how))) alist))
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

(provide 'ghostherd-worktree)

;; Local Variables:
;; generated-autoload-load-name: "ghostherd"
;; End:

;;; ghostherd-worktree.el ends here

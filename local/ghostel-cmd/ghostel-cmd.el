;;; ghostel-cmd.el --- Per-project Ghostel command overlay -*- lexical-binding: t; -*-

;; Author: Jing
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience, terminals
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Bookmark command templates per project, with {{var}} / {{var:default}}
;; / {{var:a|b|c}} (choices) placeholders and built-ins taken from the
;; caller: {{file}}, {{file-abs}}, {{dir}}, {{line}}, {{symbol}},
;; {{root}}, {{project-name}}, {{branch}}.  The overlay (posframe when
;; available, side window otherwise) is the main UI:
;;
;;   j/k move   RET paste (fill vars first)   C-RET paste+Enter
;;   / filter   a add   e edit   d delete   M-j/M-k reorder   q quit
;;   A add global   m move to global / back (a task: copy into the project)
;;   C-y yank, C-\ input method (in the filter and fill input)
;;   C-n/C-p pick a choice while filling {{var:a|b|c}}
;;
;; Global commands, shared by every project, follow the project's own
;; in the list, then the tasks found in the project root: package.json
;; scripts, Makefile targets and justfile recipes, read-only.  The
;; files are only parsed, never run; `ghostel-cmd-discover' picks them.
;;
;; Filling happens on a footer line in the overlay, not a chain of
;; minibuffers.  Paste does not press Enter unless C-RET.
;;
;; `ghostel-project-cmd-run' is an alias for the overlay so existing
;; keybindings keep working.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)

(declare-function posframe-workable-p "posframe")
(declare-function posframe-show "posframe")
(declare-function posframe-hide "posframe")
(declare-function posframe-refresh "posframe")
(declare-function posframe-poshandler-frame-center "posframe")
(declare-function project-current "project")
(declare-function project-root "project")
(declare-function project-run-ghostel "jemacs-project")
(declare-function new-terminal "jemacs-project")
(declare-function ghostel-paste-string "ghostel")
(declare-function ghostel-send-key "ghostel")
(declare-function magit-get-current-branch "magit-git")
(declare-function vc-git--current-branch "vc-git")
(declare-function evil-define-key* "evil")
(declare-function evil-make-intercept-map "evil")
(declare-function evil-normalize-keymaps "evil")


;;; Customization

(defgroup ghostel-cmd nil
  "Per-project Ghostel command bookmarks."
  :group 'tools
  :prefix "ghostel-cmd-")

(defcustom ghostel-cmd-save-file
  (locate-user-emacs-file "ghostel-project-commands.eld")
  "File that persists project command bookmarks."
  :type 'file
  :group 'ghostel-cmd)

(defcustom ghostel-cmd-use-posframe t
  "Show the command list in a posframe overlay when possible."
  :type 'boolean
  :group 'ghostel-cmd)

(defcustom ghostel-cmd-posframe-width 72
  "Minimum character width of the overlay."
  :type 'integer
  :group 'ghostel-cmd)

(defcustom ghostel-cmd-posframe-width-ratio 0.55
  "Fraction of the parent frame width used by the overlay.
Nil means a fixed `ghostel-cmd-posframe-width'."
  :type '(choice (const :tag "Fixed width" nil) number)
  :group 'ghostel-cmd)

(defcustom ghostel-cmd-preview-max-lines 3
  "Most lines the preview under the list wraps a long command onto.
The rest is cut with an ellipsis, so a huge command never pushes the
list out of the overlay."
  :type 'integer
  :group 'ghostel-cmd)

(defcustom ghostel-cmd-discover t
  "Which tasks the overlay finds in the project root, after the bookmarks.
t lists package.json scripts, Makefile targets and justfile recipes,
nil none, and a list the sources named in it: npm, make, just.  The
files are only parsed; nothing is run to find the tasks."
  :type '(choice (const :tag "All" t)
                 (const :tag "None" nil)
                 (set (const npm) (const make) (const just)))
  :group 'ghostel-cmd)

(defcustom ghostel-cmd-sidebar-side 'right
  "Side window side when not using posframe."
  :type '(choice (const left) (const right))
  :group 'ghostel-cmd)

(defcustom ghostel-cmd-sidebar-width 56
  "Side window width when not using posframe."
  :type 'integer
  :group 'ghostel-cmd)


;;; Store

(defvar ghostel-cmd--cache nil
  "Alist of (PROJECT-ROOT . (CMD-PLIST ...)).
The global commands sit under `ghostel-cmd--global-key'.")

(defconst ghostel-cmd--global-key :global
  "Save-file key of the commands every project shares.
Not a path, so no project root can take it.")

(defvar ghostel-cmd--cache-stamp nil
  "(FILE . MTIME) of the save file when we last read or wrote it.
Nil until the first read; MTIME is nil when the file did not exist.")

(defconst ghostel-cmd--var-regexp
  "{{\\s-*\\([[:alpha:]_][^:} \t\n]*\\)\\(?::\\([^}]*\\)\\)?\\s-*}}"
  "Match {{var}} or {{var:default}}.
A name starts with a letter or underscore; any other {{...}} is
literal, such as docker's {{.State.Status}} or helm's {{ .Values.x }}.")

(defun ghostel-cmd--current-project-root ()
  "Normalized project root, or `default-directory'."
  (file-name-as-directory
   (expand-file-name
    (or (when-let* ((proj (project-current nil)))
          (project-root proj))
        default-directory))))

(defun ghostel-cmd--load ()
  "Read `ghostel-cmd-save-file', or nil when it is missing or empty.
Signals when the file does not parse."
  (when (file-exists-p ghostel-cmd-save-file)
    (with-temp-buffer
      (insert-file-contents ghostel-cmd-save-file)
      (set-syntax-table emacs-lisp-mode-syntax-table)
      (goto-char (point-min))
      (forward-comment (buffer-size))
      (unless (eobp)
        (read (current-buffer))))))

(defun ghostel-cmd--file-stamp ()
  (cons ghostel-cmd-save-file
        (file-attribute-modification-time
         (file-attributes ghostel-cmd-save-file))))

(defun ghostel-cmd--save ()
  "Write `ghostel-cmd--cache' to disk."
  (let ((dir (file-name-directory ghostel-cmd-save-file)))
    (when (and dir (not (file-directory-p dir)))
      (make-directory dir t)))
  (with-temp-file ghostel-cmd-save-file
    (let ((print-length nil)
          (print-level nil)
          (print-circle nil))
      (prin1 (or ghostel-cmd--cache '()) (current-buffer))
      (insert "\n")))
  (setq ghostel-cmd--cache-stamp (ghostel-cmd--file-stamp)))

(defun ghostel-cmd--ensure-cache (&optional for-write)
  "Re-read the save file when it changed since we last read or wrote it.
A file that does not parse leaves the cache as it was; with FOR-WRITE
it signals instead, so a save never writes over a half-done edit."
  (let ((stamp (ghostel-cmd--file-stamp)))
    (unless (equal stamp ghostel-cmd--cache-stamp)
      (condition-case err
          (setq ghostel-cmd--cache (ghostel-cmd--load)
                ghostel-cmd--cache-stamp stamp)
        (error
         (funcall (if for-write #'user-error #'message)
                  "ghostel-cmd: could not load %s: %s"
                  ghostel-cmd-save-file (error-message-string err)))))))

(defun ghostel-cmd--get-commands (&optional root)
  "Command plists for ROOT."
  (ghostel-cmd--ensure-cache)
  (let ((r (or root (ghostel-cmd--current-project-root))))
    (alist-get r ghostel-cmd--cache nil nil #'equal)))

(defun ghostel-cmd--update-many (changes)
  "Apply each (KEY . FN) of CHANGES to the commands under KEY; persist once.
Each FN sees the commands as they are on disk right now, so an outside
edit of the save file is never written over.  Return the new lists."
  (ghostel-cmd--ensure-cache t)
  (let ((new (mapcar (lambda (change)
                       (funcall (cdr change)
                                (alist-get (car change) ghostel-cmd--cache
                                           nil nil #'equal)))
                     changes)))
    (cl-mapc (lambda (change cmds)
               (setf (alist-get (car change) ghostel-cmd--cache nil t #'equal)
                     cmds))
             changes new)
    (ghostel-cmd--save)
    new))

(defun ghostel-cmd--update-commands (fn &optional root)
  "Replace ROOT's commands with (FN COMMANDS) and persist.
FN sees the commands as they are on disk right now, so an outside
edit of the save file is never written over."
  (car (ghostel-cmd--update-many
        (list (cons (or root (ghostel-cmd--current-project-root)) fn)))))

(defun ghostel-cmd--set-commands (cmds &optional root)
  "Replace commands for ROOT with CMDS and persist."
  (ghostel-cmd--update-commands (lambda (_) cmds) root))

(defun ghostel-cmd--store-key (scope root)
  "Save-file key of the commands of SCOPE, project or global, under ROOT."
  (if (eq scope 'global) ghostel-cmd--global-key root))

(defun ghostel-cmd--scope-rows (scope records)
  "RECORDS as rows ((SCOPE . NAME) . RECORD)."
  (mapcar (lambda (r) (cons (cons scope (plist-get r :name)) r)) records))

(defun ghostel-cmd--rows (root)
  "Every command listed for ROOT, as rows ((SCOPE . NAME) . RECORD):
the project's own, then the global ones, then the tasks found in ROOT."
  (append (ghostel-cmd--scope-rows 'project (ghostel-cmd--get-commands root))
          (ghostel-cmd--scope-rows
           'global (ghostel-cmd--get-commands ghostel-cmd--global-key))
          (ghostel-cmd--tasks root)
          (ghostel-cmd--url-entries root)))

(defun ghostel-cmd--task-p (scope)
  "Non-nil when SCOPE is a discovered task's, not a bookmark's."
  (not (memq scope '(project global))))


;;; Discovered tasks

(defconst ghostel-cmd--task-sources
  '((npm ghostel-cmd--npm-tasks "package.json")
    (make ghostel-cmd--make-tasks "GNUmakefile" "makefile" "Makefile")
    (just ghostel-cmd--just-tasks "justfile" "Justfile" ".justfile")
    (compose ghostel-cmd--compose-tasks
             "compose.yaml" "compose.yml"
             "docker-compose.yaml" "docker-compose.yml"))
  "Per source: its parser and the files it reads, the first found wins,
in the order make, just and docker compose themselves look for them.")

(defconst ghostel-cmd--compose-files
  '("compose.yaml" "compose.yml" "docker-compose.yaml" "docker-compose.yml")
  "Compose base files, in docker's own precedence; the first found wins.")

(defconst ghostel-cmd--compose-override-files
  '("compose.override.yaml" "compose.override.yml"
    "docker-compose.override.yaml" "docker-compose.override.yml")
  "Compose override files, pulled into the task stamp so a change to one
re-discovers even though it is not the base file the source found.")

(defconst ghostel-cmd--vite-config-files
  '("vite.config.ts" "vite.config.js" "vite.config.mjs"
    "vite.config.cjs" "vite.config.mts" "vite.config.cts")
  "Vite config files, any one of which gives a Vite dev-server URL.")

(defconst ghostel-cmd--next-config-files
  '("next.config.js" "next.config.mjs" "next.config.ts")
  "Next.js config files: their presence marks a project as clearly Next.")

(defun ghostel-cmd--source-file (root files)
  "The first of FILES that is a regular file under ROOT, or nil."
  (cl-loop for f in files
           for path = (expand-file-name f root)
           when (file-regular-p path) return path))

(defconst ghostel-cmd--npm-lockfiles
  '(("pnpm-lock.yaml" . "pnpm")
    ("yarn.lock" . "yarn")
    ("bun.lock" . "bun")
    ("bun.lockb" . "bun"))
  "Lockfiles that name the runner of package.json scripts, first wins.")

(defvar ghostel-cmd--tasks-cache nil
  "Alist of (ROOT STAMP . ROWS): the tasks found in ROOT while the
files they come from had the mtimes in STAMP.")

(defun ghostel-cmd--shell-arg (s)
  "S as one shell word, quoted only when it needs it."
  (if (string-match-p "\\`[[:alnum:]_:./@%+=,-]+\\'" s)
      s
    (shell-quote-argument s)))

(defun ghostel-cmd--task (scope name cmd desc)
  "A row for the task NAME of SCOPE, running CMD."
  (cons (cons scope name)
        (list :name name :cmd cmd :desc (or desc ""))))

(defun ghostel-cmd--npm-runner (root json)
  "The runner of ROOT's package.json scripts: the one its lockfile
names, else the packageManager field of JSON, else npm."
  (or (cdr (cl-find-if (lambda (lock)
                         (file-exists-p (expand-file-name (car lock) root)))
                       ghostel-cmd--npm-lockfiles))
      (let ((pm (alist-get 'packageManager json)))
        (and (stringp pm)
             (string-match "\\`\\(npm\\|pnpm\\|yarn\\|bun\\)\\(?:@\\|\\'\\)" pm)
             (match-string 1 pm)))
      "npm"))

(defun ghostel-cmd--npm-tasks (file)
  "The scripts of package.json FILE, run as `RUNNER run NAME'.
The script itself is the desc."
  (let* ((json (with-temp-buffer
                 (insert-file-contents file)
                 (json-parse-buffer :object-type 'alist
                                    :null-object nil :false-object nil)))
         (json (and (consp json) json))
         (scripts (alist-get 'scripts json))
         (runner (ghostel-cmd--npm-runner (file-name-directory file) json)))
    (cl-loop for (key . body) in (and (consp scripts) scripts)
             for name = (symbol-name key)
             when (and (stringp body) (not (string-empty-p name)))
             collect (ghostel-cmd--task
                      (intern runner) name
                      (format "%s run %s" runner (ghostel-cmd--shell-arg name))
                      body))))

(defconst ghostel-cmd--make-rule-regexp
  "\\`\\([^:#=\t$][^:#=$]*\\)::?\\([^:=].*\\)?\\'"
  "A rule line: targets, then : or ::, not an assignment such as :=.
Targets holding a $ (built from a variable) never match.")

(defun ghostel-cmd--make-targets (file)
  "Alist of (TARGET . DESC) for the tasks the rules of FILE name.
A task is a literal target declared .PHONY, or one with no / and no
file extension: foo.o and build/out.bin are files, not tasks.  Left
out too: recipe lines, assignments, define blocks, directives, rules
built from variables, pattern rules, targets starting with a dot
(.PHONY and the other special ones) and target-specific variables.
A `## text' after a rule is its DESC."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (while (search-forward "\\\n" nil t)
      (replace-match " " t t))
    (goto-char (point-min))
    (let ((targets nil)
          (phony nil)
          (depth 0))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (cond
           ((string-match-p "\\`[ \t]*\\(?:\\(?:export\\|override\\|private\\)[ \t]+\\)*define\\_>"
                            line)
            (cl-incf depth))
           ((> depth 0)
            (when (string-match-p "\\`[ \t]*endef\\_>" line)
              (cl-decf depth)))
           ((string-prefix-p "\t" line))
           ((string-match-p "\\`[ \t]*-?\\(?:s?include\\|if\\(?:n?eq\\|n?def\\)\\|else\\|endif\\|\\(?:un\\)?export\\|override\\|private\\|vpath\\|undefine\\)\\_>"
                            line))
           ((string-match ghostel-cmd--make-rule-regexp line)
            (let* ((head (match-string 1 line))
                   (rest (or (match-string 2 line) ""))
                   (desc (when (string-match "##+[ \t]*\\(.*?\\)[ \t]*\\'" rest)
                           (match-string 1 rest)))
                   (prereqs (car (split-string rest "[#;]"))))
              (cond
               ((equal (string-trim head) ".PHONY")
                (setq phony (append (split-string prereqs) phony)))
               ((or (string-search "=" prereqs)
                    (string-match-p "[()]" head)))
               (t
                (dolist (name (split-string head))
                  (unless (or (string-prefix-p "." name)
                              (string-match-p "[%*?[\\]" name))
                    (let ((known (assoc name targets)))
                      (cond ((null known) (push (cons name desc) targets))
                            ((null (cdr known)) (setcdr known desc)))))))))))
          (forward-line 1)))
      (when (> depth 0)
        (error "A define has no endef"))
      (cl-remove-if-not (lambda (target)
                          (or (member (car target) phony)
                              (not (string-match-p "/\\|\\.[^./]+\\'"
                                                   (car target)))))
                        (nreverse targets)))))

(defun ghostel-cmd--make-tasks (file)
  "The targets of make FILE, run as `make TARGET'."
  (mapcar (lambda (target)
            (ghostel-cmd--task 'make (car target)
                               (concat "make " (ghostel-cmd--shell-arg (car target)))
                               (cdr target)))
          (ghostel-cmd--make-targets file)))

(defun ghostel-cmd--just-recipe-name (line)
  "The name of the recipe LINE starts, or nil.
A recipe is a name and its parameters up to an unquoted colon that
does not start :=, which would make it an assignment, an alias or a
setting."
  (when (string-match "\\`@?\\([[:alpha:]_][[:alnum:]_-]*\\)" line)
    (let ((name (match-string 1 line))
          (i (match-end 0))
          (quoting nil)
          (found nil))
      (while (and (not found) (< i (length line)))
        (let ((ch (aref line i)))
          (cond (quoting (when (eq ch quoting) (setq quoting nil)))
                ((memq ch '(?\" ?\' ?`)) (setq quoting ch))
                ((eq ch ?#) (setq found 'no))
                ((eq ch ?:)
                 (setq found (if (eq (and (< (1+ i) (length line))
                                          (aref line (1+ i)))
                                     ?=)
                                 'no
                               'yes)))))
        (setq i (1+ i)))
      (and (eq found 'yes) name))))

(defun ghostel-cmd--just-tasks (file)
  "The public recipes of justfile FILE, run as `just RECIPE'.
A comment right above one, or its [doc(...)] attribute, is the desc;
[private] ones and those starting with _ are left out, as just
--list leaves them out."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((tasks nil)
          (doc nil)
          (private nil))
      (while (not (eobp))
        (let* ((line (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))
               (name (ghostel-cmd--just-recipe-name line)))
          (cond
           ((string-match "\\`#\\(?:[^!].*\\)?\\'" line)
            (setq doc (string-trim (substring line 1))))
           ((string-prefix-p "[" line)
            (when (string-match-p "\\_<private\\_>" line)
              (setq private t))
            (when (string-match "\\_<doc(\\s-*\\([\"']\\)\\(.*?\\)\\1" line)
              (setq doc (match-string 2 line))))
           (t
            (when (and name (not private) (not (string-prefix-p "_" name)))
              (push (ghostel-cmd--task 'just name (concat "just " name) doc)
                    tasks))
            (setq doc nil
                  private nil))))
        (forward-line 1))
      (nreverse tasks))))

;;; docker compose — a parse-only task source and service URLs
;;
;; A careful subset parser, not a YAML library: it must keep the inline
;; comments (a port's hint) that a YAML reader would drop, and it must
;; never run docker or resolve a .env.  It reads service names and each
;; port's published/target/proto, resolving ${VAR:-default} to its
;; default and a range a-b to its start; a bare ${VAR} (no default, no
;; .env) leaves that port out rather than inventing one.

(defconst ghostel-cmd--compose-non-web-ports
  '(5432 3306 6379 27017 5672 9092 2181 11211 1521 50051
    25 110 143 465 587 993 995)
  "Published or target ports that are clearly not HTTP (databases, gRPC,
mail): their URL is marked, and `ghostel-cmd-open-url' never picks them.")

(defun ghostel-cmd--compose-indent (line)
  "Leading-space count of LINE, or nil when LINE is blank or a comment.
YAML forbids tab indentation, so only spaces count."
  (let ((i 0) (n (length line)))
    (while (and (< i n) (eq (aref line i) ?\s)) (setq i (1+ i)))
    (and (< i n) (not (eq (aref line i) ?#)) i)))

(defun ghostel-cmd--compose-split-comment (s)
  "Split S into (VALUE . COMMENT): COMMENT is the text after the first
unquoted `#', trimmed, or nil.  VALUE keeps its indentation."
  (let ((i 0) (n (length s)) (q nil) (cut nil))
    (while (and (< i n) (not cut))
      (let ((ch (aref s i)))
        (cond (q (when (eq ch q) (setq q nil)))
              ((memq ch '(?\" ?\')) (setq q ch))
              ((eq ch ?#) (setq cut i))))
      (setq i (1+ i)))
    (if cut
        (cons (string-trim-right (substring s 0 cut))
              (let ((c (string-trim (substring s (1+ cut)))))
                (and (not (string-empty-p c)) c)))
      (cons (string-trim-right s) nil))))

(defun ghostel-cmd--compose-keyval (c)
  "For a mapping line C (`key' or `key: value'), (KEY . VALUE); else nil."
  (when (string-match "\\`\\([A-Za-z0-9_.-]+\\):\\(?:[ \t]+\\(.*\\)\\)?\\'" c)
    (cons (match-string 1 c) (string-trim (or (match-string 2 c) "")))))

(defun ghostel-cmd--compose-split-colons (s)
  "Split S on `:' not inside a ${...}."
  (let ((parts nil) (start 0) (i 0) (n (length s)) (depth 0))
    (while (< i n)
      (let ((ch (aref s i)))
        (cond ((and (eq ch ?$) (< (1+ i) n) (eq (aref s (1+ i)) ?{))
               (setq depth (1+ depth) i (1+ i)))
              ((and (eq ch ?}) (> depth 0)) (setq depth (1- depth)))
              ((and (eq ch ?:) (= depth 0))
               (push (substring s start i) parts) (setq start (1+ i)))))
      (setq i (1+ i)))
    (push (substring s start) parts)
    (nreverse parts)))

(defun ghostel-cmd--compose-resolve-port (tok)
  "Integer port from TOK: ${VAR:-D}/${VAR-D} -> D, a range a-b -> a, a
number -> itself; nil when it cannot be known (a bare ${VAR}: no .env)."
  (setq tok (string-trim (or tok "")))
  (cond
   ((string-empty-p tok) nil)
   ((string-match "\\`\\${[A-Za-z_][A-Za-z0-9_]*:?-\\(.*\\)}\\'" tok)
    (ghostel-cmd--compose-resolve-port (match-string 1 tok)))
   ((string-prefix-p "${" tok) nil)
   ((string-match "\\`\\([0-9]+\\)-[0-9]+\\'" tok)
    (string-to-number (match-string 1 tok)))
   ((string-match-p "\\`[0-9]+\\'" tok) (string-to-number tok))
   (t nil)))

(defun ghostel-cmd--compose-port-short (spec)
  "A (:published N :target M :proto S) from short-syntax SPEC, or nil
when the published port cannot be known."
  (setq spec (string-trim spec))
  (when (and (>= (length spec) 2)
             (memq (aref spec 0) '(?\" ?\'))
             (eq (aref spec (1- (length spec))) (aref spec 0)))
    (setq spec (substring spec 1 -1)))
  (let ((proto "tcp"))
    (when (string-match "/\\([a-zA-Z]+\\)\\'" spec)
      (setq proto (downcase (match-string 1 spec))
            spec (substring spec 0 (match-beginning 0))))
    (let* ((parts (ghostel-cmd--compose-split-colons spec))
           (n (length parts))
           (pub (ghostel-cmd--compose-resolve-port
                 (nth (if (= n 1) 0 (- n 2)) parts)))
           (tgt (ghostel-cmd--compose-resolve-port (nth (1- n) parts))))
      (and pub (list :published pub :target (or tgt pub) :proto proto)))))

(defun ghostel-cmd--compose-port-long (alist)
  "A port plist from long-syntax ALIST of (key . value), or nil with no
fixed published port (docker would assign one)."
  (let ((pub (ghostel-cmd--compose-resolve-port (cdr (assoc "published" alist))))
        (tgt (ghostel-cmd--compose-resolve-port (cdr (assoc "target" alist))))
        (proto (or (cdr (assoc "protocol" alist)) "tcp")))
    (and pub (list :published pub :target (or tgt pub) :proto (downcase proto)))))

(defun ghostel-cmd--compose-parse (text)
  "Parse compose TEXT to an alist (SERVICE . PORTS), order kept.
Each port is (:published N :target M :proto S :hint H)."
  (let ((services nil) (in-services nil) (services-indent nil)
        (service nil) (service-indent nil)
        (in-ports nil) (ports-indent nil)
        (long nil) (long-hint nil))
    (cl-labels
        ((add-port (port hint)
           (when (and port service)
             (let ((cell (assq-get service)))
               (when hint (setq port (append port (list :hint hint))))
               (setcdr cell (append (cdr cell) (list port))))))
         (assq-get (name)
           (or (assoc name services)
               (car (push (cons name nil) (cdr (last services))))))
         (flush-long ()
           (when long
             (add-port (ghostel-cmd--compose-port-long (nreverse long)) long-hint)
             (setq long nil long-hint nil))))
      ;; `services' needs a head cell so `last'/`setcdr' can append in order.
      (setq services (list 'head))
      (dolist (raw (split-string text "\n"))
        (let ((indent (ghostel-cmd--compose-indent raw)))
          (when indent
            (let* ((sc (ghostel-cmd--compose-split-comment raw))
                   (c (string-trim (car sc)))
                   (cmt (cdr sc)))
              (cond
               ((not in-services)
                (when (equal c "services:")
                  (setq in-services t services-indent indent)))
               ((<= indent services-indent)
                (flush-long)
                (setq in-services nil service nil service-indent nil
                      in-ports nil))
               (t
                (when (null service-indent) (setq service-indent indent))
                (cond
                 ;; a service name
                 ((and (= indent service-indent)
                       (ghostel-cmd--compose-keyval c)
                       (string-empty-p (cdr (ghostel-cmd--compose-keyval c))))
                  (flush-long)
                  (setq service (car (ghostel-cmd--compose-keyval c))
                        in-ports nil ports-indent nil)
                  (assq-get service))
                 ;; a ports list item
                 ((and in-ports (> indent ports-indent)
                       (or (string-prefix-p "- " c) (string= c "-")))
                  (flush-long)
                  (let ((item (string-trim (substring c 1))))
                    (if (ghostel-cmd--compose-keyval item)
                        ;; long syntax starts on the dash line
                        (let ((kv (ghostel-cmd--compose-keyval item)))
                          (setq long (list kv) long-hint cmt))
                      (add-port (ghostel-cmd--compose-port-short item) cmt))))
                 ;; a continuation key of a long-syntax port
                 ((and in-ports long (> indent ports-indent)
                       (ghostel-cmd--compose-keyval c))
                  (push (ghostel-cmd--compose-keyval c) long)
                  (when cmt (setq long-hint (or long-hint cmt))))
                 ;; a key under the service (opens or closes `ports')
                 ((and (> indent service-indent)
                       (ghostel-cmd--compose-keyval c))
                  (flush-long)
                  (if (equal (car (ghostel-cmd--compose-keyval c)) "ports")
                      (setq in-ports t ports-indent indent)
                    (setq in-ports nil)))
                 (t (flush-long)))))))))
      (flush-long)
      (cdr services))))

(defun ghostel-cmd--compose-override-file (file)
  "FILE's override sibling (foo.yml -> foo.override.yml), or nil."
  (let ((dir (file-name-directory file))
        (base (file-name-base file))
        (ext (file-name-extension file)))
    (expand-file-name (format "%s.override.%s" base ext) dir)))

(defun ghostel-cmd--compose-merge-ports (base over)
  "BASE ports with OVER's layered on: a published port in both takes
OVER's, the rest of OVER's are appended."
  (let ((result (copy-sequence base)))
    (dolist (p over result)
      (let ((cell (cl-find (plist-get p :published) result
                           :key (lambda (q) (plist-get q :published)))))
        (if cell
            (setq result (cl-substitute p cell result :test #'eq))
          (setq result (append result (list p))))))))

(defun ghostel-cmd--compose-services (file)
  "Services of compose FILE merged with its override: (NAME . PORTS)."
  (let* ((base (ghostel-cmd--compose-parse
                (with-temp-buffer (insert-file-contents file) (buffer-string))))
         (ov (ghostel-cmd--compose-override-file file))
         (over (and ov (file-regular-p ov)
                    (ghostel-cmd--compose-parse
                     (with-temp-buffer (insert-file-contents ov) (buffer-string))))))
    (if (null over)
        base
      (let ((result (mapcar (lambda (s) (cons (car s) (copy-sequence (cdr s)))) base)))
        (dolist (svc over)
          (let ((cell (assoc (car svc) result)))
            (if cell
                (setcdr cell (ghostel-cmd--compose-merge-ports (cdr cell) (cdr svc)))
              (setq result (append result (list svc))))))
        result))))

(defun ghostel-cmd--compose-tasks (file)
  "The docker compose tasks for FILE: up / up --build / down / ps always,
and logs / restart / exec / build with the file's services as choices."
  (let* ((services (ghostel-cmd--compose-services file))
         (names (mapcar #'car services))
         (choice (and names (concat "{{service:" (string-join names "|") "}}")))
         (tasks (list
                 (ghostel-cmd--task 'compose "up" "docker compose up -d"
                                    "start in the background")
                 (ghostel-cmd--task 'compose "up-build" "docker compose up -d --build"
                                    "rebuild, then start")
                 (ghostel-cmd--task 'compose "down" "docker compose down"
                                    "stop and remove")
                 (ghostel-cmd--task 'compose "ps" "docker compose ps"
                                    "services and their state"))))
    (if (null choice)
        tasks
      (append tasks
              (list
               (ghostel-cmd--task 'compose "logs"
                                  (concat "docker compose logs -f " choice)
                                  "follow a service's logs")
               (ghostel-cmd--task 'compose "restart"
                                  (concat "docker compose restart " choice)
                                  "restart a service")
               (ghostel-cmd--task 'compose "exec"
                                  (concat "docker compose exec " choice " sh")
                                  "a shell in a service")
               (ghostel-cmd--task 'compose "build"
                                  (concat "docker compose build " choice)
                                  "build a service"))))))

;;; Service URLs — an "open in a browser" entry, not a paste
;;
;; From the compose published ports and the Vite / dev-server ports: one
;; URL entry per published port, http://localhost:<published>.  A quick,
;; non-blocking TCP probe marks which are up.  RET opens it in the system
;; browser, C-RET in an Emacs xwidget.

(defvar ghostel-cmd--urls-cache nil
  "Alist of (ROOT STAMP . ROWS): the URL entries found in ROOT while the
files they come from had the mtimes in STAMP.")

(defun ghostel-cmd--url-row (name url &rest props)
  "A URL entry row (scope url): NAME opens URL.  PROPS adds :web etc."
  (cons (cons 'url name)
        (append (list :name name :cmd url :url url) props)))

(defun ghostel-cmd--compose-hint-label (hint)
  "A short label from an inline HINT: its text before any URL, trimmed of
surrounding dashes, colons and space."
  (when hint
    (let ((h (string-trim (replace-regexp-in-string "https?://[^ \t]+" "" hint)
                          "[-–—:[:space:]]+" "[-–—:[:space:]]+")))
      (and (not (string-empty-p h)) h))))

(defun ghostel-cmd--compose-web-p (port)
  "Non-nil when PORT (a port plist) looks like a web (HTTP) port."
  (let ((hint (plist-get port :hint))
        (case-fold-search t))
    (not (or (and hint (string-match-p "grpc" hint))
             (memq (plist-get port :published) ghostel-cmd--compose-non-web-ports)
             (memq (plist-get port :target) ghostel-cmd--compose-non-web-ports)
             (equal (plist-get port :proto) "udp")))))

(defun ghostel-cmd--compose-urls (root)
  "URL entries from ROOT's compose file: one per published port."
  (when-let* ((file (ghostel-cmd--source-file root ghostel-cmd--compose-files)))
    (let (rows)
      (dolist (svc (ghostel-cmd--compose-services file))
        (let* ((service (car svc))
               (ports (cdr svc))
               (multi (> (length ports) 1)))
          (dolist (port ports)
            (let* ((pub (plist-get port :published))
                   (web (ghostel-cmd--compose-web-p port))
                   (hint (ghostel-cmd--compose-hint-label (plist-get port :hint)))
                   (label (cond (hint (concat service " — " hint))
                                (multi (format "%s:%d" service pub))
                                (t service)))
                   (url (format "http://localhost:%d" pub)))
              (push (ghostel-cmd--url-row label url
                                          :web web :source "compose"
                                          :service service :published pub
                                          :target (plist-get port :target))
                    rows)))))
      (nreverse rows))))

(defun ghostel-cmd--vite-port (file)
  "The server.port set in Vite config FILE, or nil."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (re-search-forward
           "server[ \t\n]*:[ \t\n]*{[^}]*?\\_<port\\_>[ \t\n]*:[ \t\n]*\\([0-9]+\\)"
           nil t)
      (string-to-number (match-string 1)))))

(defun ghostel-cmd--vite-urls (root)
  "A URL entry for ROOT's Vite dev server, if it has a Vite config."
  (when-let* ((file (ghostel-cmd--source-file root ghostel-cmd--vite-config-files)))
    (let ((port (or (ghostel-cmd--vite-port file) 5173)))
      (list (ghostel-cmd--url-row
             (format "vite :%d" port) (format "http://localhost:%d" port)
             :web t :source "vite" :published port :target port)))))

(defun ghostel-cmd--script-port (s)
  "The --port / -p value in a dev script S, or nil."
  (when (string-match "\\(?:--port[= ]\\|-p[= ]\\)\\([0-9]+\\)" s)
    (string-to-number (match-string 1 s))))

(defun ghostel-cmd--dev-script-urls (root)
  "A URL entry from ROOT's package.json dev script --port / -p, if any."
  (let ((file (expand-file-name "package.json" root)))
    (when (file-regular-p file)
      (when-let* ((json (ignore-errors
                          (with-temp-buffer
                            (insert-file-contents file)
                            (json-parse-buffer :object-type 'alist
                                               :null-object nil :false-object nil))))
                  (scripts (and (consp json) (alist-get 'scripts json)))
                  (dev (and (consp scripts) (alist-get 'dev scripts)))
                  (port (and (stringp dev) (ghostel-cmd--script-port dev))))
        (list (ghostel-cmd--url-row
               (format "dev :%d" port) (format "http://localhost:%d" port)
               :web t :source "dev" :published port :target port))))))

(defun ghostel-cmd--package-has-dep-p (root name)
  "Non-nil when ROOT's package.json lists dependency NAME."
  (let ((file (expand-file-name "package.json" root)))
    (and (file-regular-p file)
         (when-let* ((json (ignore-errors
                             (with-temp-buffer
                               (insert-file-contents file)
                               (json-parse-buffer :object-type 'alist
                                                  :null-object nil :false-object nil)))))
           (and (consp json)
                (cl-some (lambda (k) (assoc (intern name) (alist-get k json)))
                         '(dependencies devDependencies)))))))

(defun ghostel-cmd--next-urls (root)
  "A URL entry for Next.js (port 3000) when ROOT clearly uses it."
  (when (or (ghostel-cmd--source-file root ghostel-cmd--next-config-files)
            (ghostel-cmd--package-has-dep-p root "next"))
    (list (ghostel-cmd--url-row "next :3000" "http://localhost:3000"
                                :web t :source "next" :published 3000 :target 3000))))

(defun ghostel-cmd--dedup-urls (rows)
  "ROWS with duplicate URLs dropped (first wins), web entries before the
rest, and each id made unique by its port when two share a label."
  (let ((seen-url (make-hash-table :test #'equal))
        (kept nil))
    (dolist (row rows)
      (let ((url (plist-get (cdr row) :url)))
        (unless (gethash url seen-url)
          (puthash url t seen-url)
          (push row kept))))
    (setq kept (cl-stable-sort (nreverse kept)
                               (lambda (a b)
                                 (and (plist-get (cdr a) :web)
                                      (not (plist-get (cdr b) :web))))))
    (let ((seen-id (make-hash-table :test #'equal)))
      (mapcar
       (lambda (row)
         (let ((id (car row)))
           (if (not (gethash id seen-id))
               (progn (puthash id t seen-id) row)
             (let* ((rec (cdr row))
                    (name (format "%s:%d" (plist-get rec :name)
                                  (or (plist-get rec :published) 0))))
               (puthash (cons 'url name) t seen-id)
               (cons (cons 'url name) (plist-put (copy-sequence rec) :name name))))))
       kept))))

(defun ghostel-cmd--gather-urls (root)
  "All URL entries for ROOT, deduped and ordered (web first)."
  (ghostel-cmd--dedup-urls
   (condition-case err
       (append (ghostel-cmd--compose-urls root)
               (ghostel-cmd--vite-urls root)
               (ghostel-cmd--dev-script-urls root)
               (ghostel-cmd--next-urls root))
     (error (message "ghostel-cmd: could not read a URL source: %s"
                     (error-message-string err))
            nil))))

;;; A quick, non-blocking TCP probe: which URLs are up

(defcustom ghostel-cmd-probe-timeout 0.12
  "Seconds a service-URL TCP probe waits before it counts as down.
The probe never blocks the overlay: a result arrives and the list
redraws, or the timeout marks it down."
  :type 'number
  :group 'ghostel-cmd)

(defconst ghostel-cmd--probe-ttl 5
  "Seconds a probe result stays fresh, so a redraw does not re-probe.")

(defvar ghostel-cmd--probe-cache (make-hash-table :test #'equal)
  "HOST:PORT -> (STATUS . TIME): `up', `down' or `probing', last set when.")

(defun ghostel-cmd--url-host-port (url)
  "(HOST . PORT) of URL http://HOST:PORT..., or nil."
  (when (string-match "\\`https?://\\([^:/]+\\):\\([0-9]+\\)" url)
    (cons (match-string 1 url) (string-to-number (match-string 2 url)))))

(defun ghostel-cmd--probe-status (url)
  "Fresh probe status of URL (`up'/`down'/`probing'), or nil when unknown."
  (when-let* ((hp (ghostel-cmd--url-host-port url))
              (hit (gethash (format "%s:%d" (car hp) (cdr hp))
                            ghostel-cmd--probe-cache)))
    (and (< (- (float-time) (cdr hit)) ghostel-cmd--probe-ttl)
         (car hit))))

(defun ghostel-cmd--probe (host port callback)
  "Probe HOST:PORT with an async TCP connect; call CALLBACK with `up' or
`down' once, within `ghostel-cmd-probe-timeout'."
  (let* ((done nil)
         (finish (lambda (status) (unless done (setq done t) (funcall callback status))))
         (proc (condition-case nil
                   (make-network-process
                    :name "ghostel-cmd-probe" :host host :service port
                    :nowait t :coding 'binary
                    :sentinel
                    ;; Finish BEFORE deleting: `delete-process' re-enters this
                    ;; sentinel with `closed', which would otherwise mark a
                    ;; connected port down before the `open' result lands.
                    (lambda (p _event)
                      (pcase (process-status p)
                        ('open (funcall finish 'up)
                               (ignore-errors (delete-process p)))
                        ((or 'failed 'closed 'exit)
                         (funcall finish 'down)
                         (ignore-errors (delete-process p))))))
                 (error nil))))
    (if (null proc)
        (funcall finish 'down)
      (run-at-time ghostel-cmd-probe-timeout nil
                   (lambda ()
                     (unless done
                       (ignore-errors (delete-process proc))
                       (funcall finish 'down)))))))

(defun ghostel-cmd--probe-urls (rows &optional on-result)
  "Start a probe for each URL in ROWS whose status is not fresh.
Non-blocking; ON-RESULT runs after each probe settles."
  (dolist (row rows)
    (when-let* ((url (plist-get (cdr row) :url))
                (hp (ghostel-cmd--url-host-port url))
                (key (format "%s:%d" (car hp) (cdr hp))))
      (let ((hit (gethash key ghostel-cmd--probe-cache)))
        (unless (and hit (< (- (float-time) (cdr hit)) ghostel-cmd--probe-ttl))
          (puthash key (cons 'probing (float-time)) ghostel-cmd--probe-cache)
          (ghostel-cmd--probe
           (car hp) (cdr hp)
           (lambda (status)
             (puthash key (cons status (float-time)) ghostel-cmd--probe-cache)
             (when on-result (funcall on-result)))))))))

(defun ghostel-cmd--url-entries (root)
  "URL entries discovered in ROOT, cached by the mtimes of their files."
  (when ghostel-cmd-discover
    (let* ((files (append ghostel-cmd--compose-files
                          ghostel-cmd--compose-override-files
                          ghostel-cmd--vite-config-files
                          ghostel-cmd--next-config-files
                          '("package.json")))
           (stamp (cons ghostel-cmd-discover
                        (mapcar (lambda (f)
                                  (file-attribute-modification-time
                                   (file-attributes (expand-file-name f root))))
                                files)))
           (cached (assoc root ghostel-cmd--urls-cache)))
      (if (equal (cadr cached) stamp)
          (cddr cached)
        (let ((rows (ghostel-cmd--gather-urls root)))
          (setf (alist-get root ghostel-cmd--urls-cache nil nil #'equal)
                (cons stamp rows))
          rows)))))

(defun ghostel-cmd--discover (root)
  "Rows of the tasks in ROOT's files, source by source.
A file that does not parse gives no tasks, and a message."
  (cl-loop
   for (source parser . files) in ghostel-cmd--task-sources
   for file = (cl-loop for f in files
                       for path = (expand-file-name f root)
                       when (file-regular-p path) return path)
   when (and file
             (or (eq ghostel-cmd-discover t)
                 (memq source ghostel-cmd-discover)))
   append (condition-case err
              (cl-remove-duplicates (funcall parser file)
                                    :key #'car :test #'equal :from-end t)
            (error
             (message "ghostel-cmd: could not read %s: %s"
                      (abbreviate-file-name file) (error-message-string err))
             nil))))

(defun ghostel-cmd--tasks (root)
  "Rows of the tasks found in ROOT, parsed again once one of their
files, or `ghostel-cmd-discover', changed."
  (when ghostel-cmd-discover
    (let ((stamp (cons ghostel-cmd-discover
                       (mapcar (lambda (f)
                                 (file-attribute-modification-time
                                  (file-attributes (expand-file-name f root))))
                               (append (mapcar #'car ghostel-cmd--npm-lockfiles)
                                       ghostel-cmd--compose-override-files
                                       (mapcan (lambda (source)
                                                 (copy-sequence (cddr source)))
                                               ghostel-cmd--task-sources)))))
          (cached (assoc root ghostel-cmd--tasks-cache)))
      (if (equal (cadr cached) stamp)
          (cddr cached)
        (let ((rows (ghostel-cmd--discover root)))
          (setf (alist-get root ghostel-cmd--tasks-cache nil nil #'equal)
                (cons stamp rows))
          rows)))))


;;; Templates

(defvar ghostel-cmd--caller-file nil)
(defvar ghostel-cmd--caller-line nil)
(defvar ghostel-cmd--caller-dir nil)
(defvar ghostel-cmd--caller-symbol nil)
(defvar ghostel-cmd--root nil)

(defconst ghostel-cmd--builtin-names
  '("root" "project-root" "project-name" "project" "branch"
    "file" "file-abs" "dir" "line" "symbol")
  "Placeholders taken fresh from the caller, never remembered.")

(defvar ghostel-cmd--last-values (make-hash-table :test #'equal)
  "Value last filled per (ROOT SCOPE COMMAND-NAME VAR), for this Emacs session.")

(defun ghostel-cmd--builtin-var (var-name root)
  "Auto default for VAR-NAME under ROOT, or nil."
  (pcase var-name
    ((or "root" "project-root")
     (directory-file-name root))
    ((or "project-name" "project")
     (file-name-nondirectory (directory-file-name root)))
    ("branch"
     (or (when (fboundp 'magit-get-current-branch)
           (magit-get-current-branch))
         (when (fboundp 'vc-git--current-branch)
           (vc-git--current-branch))))
    ("file"
     (when-let* ((file (or ghostel-cmd--caller-file buffer-file-name)))
       (file-relative-name file root)))
    ("file-abs"
     (when-let* ((file (or ghostel-cmd--caller-file buffer-file-name)))
       (expand-file-name file)))
    ("dir"
     (directory-file-name
      (file-relative-name (or ghostel-cmd--caller-dir default-directory) root)))
    ("symbol"
     ghostel-cmd--caller-symbol)
    ("line"
     (number-to-string (or ghostel-cmd--caller-line (line-number-at-pos))))
    (_ nil)))

(defun ghostel-cmd--split-choices (spec)
  "The choices SPEC lists as a|b|c, trimmed, or nil when it has no `|'."
  (when (and spec (string-search "|" spec))
    (delete-dups (mapcar #'string-trim (split-string spec "|")))))

(defun ghostel-cmd--extract-variables (template root)
  "Ordered alist of (NAME . DEFAULT) from TEMPLATE.
The default of {{name:a|b|c}} is its first choice."
  (let ((vars nil)
        (start 0))
    (while (string-match ghostel-cmd--var-regexp template start)
      (setq start (match-end 0))
      (let* ((name (match-string 1 template))
             (spec (match-string 2 template))
             (def-val (or (car (ghostel-cmd--split-choices spec)) spec))
             (existing (assoc name vars)))
        (if existing
            (when (and (null (cdr existing)) def-val)
              (setcdr existing def-val))
          (push (cons name
                      (or def-val (ghostel-cmd--builtin-var name root)))
                vars))))
    (nreverse vars)))

(defun ghostel-cmd--extract-choices (template)
  "Alist of (NAME . CHOICES) for the {{name:a|b|c}} in TEMPLATE.
The first {{name:...}} of a name decides, as it does its default."
  (let ((specs nil)
        (start 0))
    (while (string-match ghostel-cmd--var-regexp template start)
      (setq start (match-end 0))
      (let ((name (match-string 1 template))
            (spec (match-string 2 template)))
        (when (and spec (not (assoc name specs)))
          (push (cons name spec) specs))))
    (cl-loop for (name . spec) in (nreverse specs)
             for choices = (ghostel-cmd--split-choices spec)
             when choices collect (cons name choices))))

(defun ghostel-cmd--memory-key (root scope record var)
  "Key of the last value of VAR for RECORD, of SCOPE, run under ROOT.
ROOT counts for a global command too: each project fills its own."
  (list root (or scope 'project) (plist-get record :name) var))

(defun ghostel-cmd--fill-vars (record root &optional scope)
  "Ordered alist of (NAME . DEFAULT) for RECORD's template under ROOT.
The value last filled beats the template's default; built-ins are
always taken fresh.  SCOPE is RECORD's, project by default."
  (mapcar (lambda (pair)
            (let ((last (gethash (ghostel-cmd--memory-key root scope record
                                                          (car pair))
                                 ghostel-cmd--last-values)))
              (if last (cons (car pair) last) pair)))
          (ghostel-cmd--extract-variables (plist-get record :cmd) root)))

(defun ghostel-cmd--remember (root scope record var value &optional keep-empty)
  "Keep VALUE as the last one filled for VAR of RECORD, of SCOPE, under ROOT.
An empty VALUE forgets it, unless KEEP-EMPTY: an empty choice was picked."
  (unless (member var ghostel-cmd--builtin-names)
    (let ((key (ghostel-cmd--memory-key root scope record var)))
      (if (and (string-empty-p value) (not keep-empty))
          (remhash key ghostel-cmd--last-values)
        (puthash key value ghostel-cmd--last-values)))))

(defun ghostel-cmd--apply (template values)
  "Replace {{name}} in TEMPLATE using VALUES alist."
  (let ((result template))
    (dolist (pair values result)
      (let ((pattern (format "{{\\s-*%s\\(?::[^}]*\\)?\\s-*}}"
                             (regexp-quote (car pair)))))
        (setq result
              (replace-regexp-in-string
               pattern (or (cdr pair) "") result t t))))))

(defun ghostel-cmd--preview (template _root values &optional current-name current-input)
  "TEMPLATE with VALUES applied; CURRENT-NAME shown as CURRENT-INPUT."
  (ghostel-cmd--apply
   template
   (if current-name
       (cons (cons current-name (or current-input ""))
             (cl-remove current-name values :key #'car :test #'equal))
     values)))


;;; Overlay state

(defvar ghostel-cmd--query "")
(defvar ghostel-cmd--filtering nil)
(defvar ghostel-cmd--fill nil
  "Plist while filling: :record :root :scope :vars :choices :index :values
:input :picked :submit.  :picked is non-nil while :input is a choice
picked as a whole, which typing replaces.")
(defvar ghostel-cmd--help-visible nil)
(defvar ghostel-cmd--preview-start nil)
(defvar ghostel-cmd--preview-id nil)
(defvar ghostel-cmd--target-width nil)
(defvar ghostel-cmd--posframe-parent nil)
(defvar ghostel-cmd--posframe-fitted-width nil)
(defvar ghostel-cmd--posframe-refitting nil)
(defvar ghostel-cmd--match-count 0)
(defvar ghostel-cmd--total-count 0)
(defvar ghostel-cmd--goto-id nil
  "Name the next redraw puts point on, instead of the row at point.")

(defconst ghostel-cmd--buffer-name "*ghostel-cmd*")

(defconst ghostel-cmd-sidebar-help-commands
  '(("RET"      "Paste into Ghostel (fill {{vars}}); a URL opens in the browser")
    ("C-RET"    "Paste and press Enter; a URL opens in an Emacs xwidget")
    ("u"        "Open the project's frontend in the browser")
    ("j, k"     "Move")
    ("a"        "Add template to the project")
    ("A"        "Add global template, shared by every project")
    ("e"        "Edit at point")
    ("d"        "Delete at point")
    ("m"        "Move to the global list / back; a task: copy to the project")
    ("M-j, M-k" "Move command down / up (saved)")
    ("C-n, C-p" "Filling {{name:a|b}}: pick the next / previous choice")
    ("/"        "Live-narrow (flex)")
    ("C-y"      "Yank into filter / fill input")
    ("C-\\"     "Type filter / fill input with an input method")
    ("o"        "Open save file")
    ("?"        "This help")
    ("q, Esc"   "Quit / cancel fill"))
  "Key legend for the overlay.")


;;; Flex / table

(defun ghostel-cmd--flex-regexp (query)
  (mapconcat (lambda (ch) (regexp-quote (char-to-string ch)))
             (string-to-list query)
             ".*"))

(defun ghostel-cmd--query-matches-p (record query)
  (or (not query)
      (string-empty-p (string-trim query))
      (let ((hay (mapconcat #'identity
                            (delq nil
                                  (list (plist-get record :name)
                                        (plist-get record :cmd)
                                        (plist-get record :desc)))
                            " "))
            (case-fold-search t))
        (cl-every (lambda (token)
                    (string-match-p (ghostel-cmd--flex-regexp token) hay))
                  (split-string query)))))

(defun ghostel-cmd--available-width ()
  (or ghostel-cmd--target-width
      (when-let* ((buf (get-buffer ghostel-cmd--buffer-name))
                  (win (get-buffer-window buf t)))
        (window-body-width win))
      ghostel-cmd-sidebar-width))

(defun ghostel-cmd--format (&optional tag-width)
  "Tabulated-list format for the current width.
TAG-WIDTH, a number, adds a first `From' column that wide, telling where
each command comes from.  No column sorts: the list keeps your order,
its sections never mix."
  (let* ((budget (max 24 (1- (ghostel-cmd--available-width))))
         (tag (or tag-width 0))
         (name 18)
         (desc 14)
         (cmd (max 12 (- budget (if tag-width (1+ tag) 0) name desc 2))))
    (vconcat (when tag-width (list (list "From" tag nil)))
             (vector (list "Name" name nil)
                     (list "Command" cmd nil)
                     (list "Desc" desc nil)))))

(defun ghostel-cmd--tag (scope)
  "The From column of a row of SCOPE."
  (propertize (if (eq scope 'project) "proj" (symbol-name scope))
              'face (pcase scope
                      ('project 'font-lock-type-face)
                      ('global 'font-lock-constant-face)
                      ('url 'font-lock-string-face)
                      (_ 'font-lock-comment-face))))

(defun ghostel-cmd--url-glyph (record)
  "The up/down glyph for a URL RECORD, from its last probe."
  (pcase (ghostel-cmd--probe-status (plist-get record :url))
    ('up (propertize "●" 'face 'success))
    ('down (propertize "○" 'face 'shadow))
    (_ (propertize "·" 'face 'shadow))))

(defun ghostel-cmd--url-desc (record)
  "The Desc text for a URL RECORD: its source, and `not web' when marked."
  (concat (or (plist-get record :source) "url")
          (unless (plist-get record :web) "  not web")))

(defun ghostel-cmd--name-cell (record)
  "The Name-column string for RECORD: a URL gets its up/down glyph."
  (if (plist-get record :url)
      (concat (ghostel-cmd--url-glyph record) " "
              (propertize (plist-get record :name)
                          'face 'font-lock-function-name-face))
    (propertize (plist-get record :name) 'face 'font-lock-keyword-face)))

(defun ghostel-cmd--truncate (s width)
  (truncate-string-to-width (or s "") (max 1 width) nil nil t))

(defun ghostel-cmd--wrap (s width max-lines)
  "S cut into at most MAX-LINES lines of at most WIDTH columns.
A line breaks at a space in its second half, else mid-word; when S
does not fit, the last line ends in an ellipsis."
  (let ((rest (string-trim-right (or s "") "\n+"))
        (max-lines (max 1 max-lines))
        (lines nil))
    (while (and rest (< (length lines) max-lines))
      (let* ((nl (string-search "\n" rest))
             (line (if nl (substring rest 0 nl) rest)))
        (if (<= (string-width line) width)
            (setq rest (and nl (substring rest (1+ nl))))
          (let* ((head (truncate-string-to-width line width))
                 (space (cl-position ?\s head :from-end t))
                 (cut (if (and space (> space (/ width 2)))
                          space
                        (max 1 (length head)))))
            (setq rest (substring rest (if (eql cut space) (1+ cut) cut))
                  line (substring line 0 cut))))
        (push line lines)))
    (when rest
      (setcar lines (concat (truncate-string-to-width (car lines) (1- width))
                            "…")))
    (nreverse lines)))

(defun ghostel-cmd--tail (s width)
  "S, or an ellipsis and the end of S when S is wider than WIDTH."
  (let ((w (string-width s)))
    (if (<= w width)
        s
      (concat "…" (truncate-string-to-width s w (- w (1- width)))))))

(defun ghostel-cmd--build-entries ()
  "Rebuild `tabulated-list-entries' from the current project.
A row's id is (SCOPE . NAME); a From column tags each row as soon as
the list holds more than the project's own commands."
  (let* ((root (or ghostel-cmd--root (ghostel-cmd--current-project-root)))
         (rows (ghostel-cmd--rows root))
         (tagged (cl-some (lambda (row) (not (eq (caar row) 'project))) rows))
         (tagwidth (when tagged
                     (min 8 (apply #'max 4
                                   (mapcar (lambda (r)
                                             (string-width (ghostel-cmd--tag (caar r))))
                                           rows)))))
         (query ghostel-cmd--query)
         (fmt (setq tabulated-list-format (ghostel-cmd--format tagwidth)))
         (off (if tagwidth 1 0))
         (nw (cadr (aref fmt off)))
         (cw (cadr (aref fmt (+ off 1))))
         (dw (cadr (aref fmt (+ off 2))))
         (matched (seq-filter (lambda (row)
                                (ghostel-cmd--query-matches-p (cdr row) query))
                              rows)))
    (tabulated-list-init-header)
    (setq ghostel-cmd--total-count (length rows)
          ghostel-cmd--match-count (length matched)
          tabulated-list-entries
          (mapcar
           (lambda (row)
             (let* ((c (cdr row))
                    (desc (if (plist-get c :url)
                              (ghostel-cmd--url-desc c)
                            (or (plist-get c :desc) ""))))
               (list (car row)
                     (vconcat
                      (when tagwidth (list (ghostel-cmd--tag (caar row))))
                      (vector
                       (ghostel-cmd--truncate (ghostel-cmd--name-cell c) nw)
                       (ghostel-cmd--truncate (plist-get c :cmd) cw)
                       (propertize (ghostel-cmd--truncate desc dw)
                                   'face 'font-lock-comment-face))))))
           matched))
    ;; Probe the service URLs without blocking; redraw as each settles.
    (ghostel-cmd--probe-urls rows #'ghostel-cmd--refresh-overlay)))

(defun ghostel-cmd--row-at-point ()
  "(SCOPE . RECORD) for the row at point."
  (when-let* ((id (tabulated-list-get-id))
              (root (or ghostel-cmd--root (ghostel-cmd--current-project-root)))
              (row (assoc id (ghostel-cmd--rows root))))
    (cons (car id) (cdr row))))

(defun ghostel-cmd--record-at-point ()
  "Command plist for the row at point."
  (cdr (ghostel-cmd--row-at-point)))

(defun ghostel-cmd--bookmark-at-point ()
  "(SCOPE . RECORD) for the project or global command at point.
A discovered task is read-only: it signals."
  (let ((row (or (ghostel-cmd--row-at-point)
                 (user-error "No command at point"))))
    (when (ghostel-cmd--task-p (car row))
      (user-error "'%s' is a %s task, read-only: m copies it to the project"
                  (plist-get (cdr row) :name) (car row)))
    row))

(defun ghostel-cmd--erase-preview ()
  (when (and ghostel-cmd--preview-start
             (marker-position ghostel-cmd--preview-start))
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (delete-region ghostel-cmd--preview-start (point-max))))
  (setq ghostel-cmd--preview-start nil))

(defun ghostel-cmd--help-text ()
  (let* ((rows ghostel-cmd-sidebar-help-commands)
         (width (apply #'max 3 (mapcar (lambda (row) (length (car row))) rows)))
         (fmt (format "%%-%ds  %%s" width)))
    (mapconcat (lambda (row) (format fmt (car row) (cadr row))) rows "\n")))

(defun ghostel-cmd--preview-text (record)
  "Preview body for RECORD, including fill state."
  (let* ((root (or ghostel-cmd--root (ghostel-cmd--current-project-root)))
         (tmpl (or (plist-get record :cmd) ""))
         (fill ghostel-cmd--fill)
         (width (max 20 (1- (ghostel-cmd--available-width)))))
    (cond
     ((null record)
      (propertize "(no commands — press a to add)" 'face 'shadow))
     (fill
      (ghostel-cmd--fill-text fill tmpl root width))
     (t
      (string-join
       (ghostel-cmd--wrap tmpl width ghostel-cmd-preview-max-lines) "\n")))))

(defun ghostel-cmd--fill-text (fill tmpl root width)
  "TMPL as FILL has it so far, wrapped to WIDTH, then the input line.
ROOT is where TMPL runs."
  (let* ((vars (plist-get fill :vars))
         (idx (plist-get fill :index))
         (pair (nth idx vars))
         (name (car pair))
         (input (plist-get fill :input))
         (shown (ghostel-cmd--preview
                 tmpl root (plist-get fill :values) name input))
         (label (format "{{%s}}  " name)))
    (concat
     (string-join
      (ghostel-cmd--wrap shown width ghostel-cmd-preview-max-lines) "\n")
     "\n"
     (propertize
      (concat label
              (ghostel-cmd--tail
               (if (or (plist-get fill :picked)
                       (and input (not (string-empty-p input))))
                   (or input "")
                 (or (cdr pair) ""))
               (max 8 (- width (string-width label)))))
      'face 'highlight))))

(defun ghostel-cmd--draw-preview ()
  (when (derived-mode-p 'ghostel-cmd-sidebar-mode)
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t)
          (record (and (not ghostel-cmd--help-visible)
                       (ghostel-cmd--record-at-point)))
          (width (max 10 (or (ignore-errors (window-body-width))
                             (ghostel-cmd--available-width)))))
      (save-excursion
        (ghostel-cmd--erase-preview)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (setq ghostel-cmd--preview-start (point-marker))
        (insert (propertize (make-string width ?─) 'face 'shadow) "\n")
        (insert (if ghostel-cmd--help-visible
                    (ghostel-cmd--help-text)
                  (ghostel-cmd--preview-text record)))
        (unless (bolp) (insert "\n")))
      (restore-buffer-modified-p nil))))

(defun ghostel-cmd--print (&optional update)
  (let ((id (or ghostel-cmd--goto-id (tabulated-list-get-id))))
    (setq ghostel-cmd--goto-id nil)
    (ghostel-cmd--erase-preview)
    (tabulated-list-print update)
    (when id
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (equal (tabulated-list-get-id) id)))
        (forward-line 1))
      (when (eobp) (goto-char (point-min))))
    (ghostel-cmd--draw-preview)
    (setq ghostel-cmd--preview-id (tabulated-list-get-id))))

(defun ghostel-cmd--confine-point ()
  (when (and ghostel-cmd--preview-start
             (marker-position ghostel-cmd--preview-start)
             (>= (point) ghostel-cmd--preview-start)
             (> ghostel-cmd--preview-start (point-min)))
    (goto-char (1- ghostel-cmd--preview-start))
    (beginning-of-line)))

(defun ghostel-cmd--preview-on-command ()
  (when (derived-mode-p 'ghostel-cmd-sidebar-mode)
    (ghostel-cmd--confine-point)
    (unless (or ghostel-cmd--help-visible ghostel-cmd--fill)
      (let ((id (tabulated-list-get-id)))
        (unless (equal id ghostel-cmd--preview-id)
          (setq ghostel-cmd--preview-id id)
          (ghostel-cmd--draw-preview))))))

(defun ghostel-cmd--refresh-overlay ()
  (when-let* ((buf (get-buffer ghostel-cmd--buffer-name)))
    (with-current-buffer buf
      (ghostel-cmd--build-entries)
      (ghostel-cmd--print t))
    (when (ghostel-cmd--posframe-showing-p)
      (ghostel-cmd--show-posframe buf))))

(defun ghostel-cmd--choices-text (choices current)
  "CHOICES on one line, CURRENT marked with brackets."
  (mapconcat (lambda (choice)
               (let ((shown (if (string-empty-p choice) "\"\"" choice)))
                 (if (equal choice current)
                     (propertize (format "[%s]" shown) 'face 'bold)
                   shown)))
             choices " "))

(defun ghostel-cmd--footer ()
  "The overlay's mode line; a % in it shows as itself."
  (let* ((root (or ghostel-cmd--root (ghostel-cmd--current-project-root)))
         (name (file-name-nondirectory (directory-file-name root)))
         (fill ghostel-cmd--fill)
         (querying (or ghostel-cmd--filtering
                       (and ghostel-cmd--query
                            (not (string-empty-p ghostel-cmd--query))))))
    (string-replace
     "%" "%%"
     (concat
      " "
      name
      (when querying
        (format "  /%s  %d/%d"
                ghostel-cmd--query
                ghostel-cmd--match-count
                ghostel-cmd--total-count))
      (cond
       (fill
        (ghostel-cmd--fill-footer fill))
       (ghostel-cmd--filtering
        "   RET apply  C-n/C-p move  Esc clear")
       (ghostel-cmd--help-visible
        "   ? close help  Esc close")
       (t
        "   RET paste  C-RET send  a add  / filter  ? keys  Esc close"))))))

(defun ghostel-cmd--fill-footer (fill)
  "The footer's part while FILL asks for a variable: its name, its
default or choices, and the keys."
  (let* ((pair (nth (plist-get fill :index) (plist-get fill :vars)))
         (def (cdr pair))
         (choices (ghostel-cmd--fill-choices fill)))
    (if choices
        (format "   {{%s}} %s  C-n/C-p pick  TAB next  RET paste  Esc cancel"
                (car pair)
                (ghostel-cmd--choices-text choices (plist-get fill :input)))
      (format "   {{%s}}%s  TAB next  RET paste  Esc cancel"
              (car pair)
              (if def (format " [%s]" def) "")))))


;;; Filter / fill maps

(defun ghostel-cmd--return-event-p (ev)
  (or (memq ev '(return kp-enter newline S-return))
      (and (characterp ev) (memq ev '(?\r ?\n)))
      (memq (event-basic-type ev) '(return kp-enter newline))))

(defun ghostel-cmd--yank-text ()
  "The latest kill as one line: outer newlines dropped, inner ones spaces.
A query or a {{var}} value lives on one line of one command, so a
one-per-line list yanks as space-separated words."
  (replace-regexp-in-string
   "[\r\n]+" " "
   (string-trim (substring-no-properties (current-kill 0))
                "[\r\n]+" "[\r\n]+")))

(defvar ghostel-cmd--keep-focus nil
  "Non-nil while the minibuffer edits an overlay input.
Redraws then leave input focus in the minibuffer.")

(defun ghostel-cmd--read-line (prompt initial on-change)
  "Read PROMPT in the minibuffer from INITIAL, `default-input-method' on.
ON-CHANGE gets the text after each command, so the overlay follows.
Input methods such as rime or quail compose only in a writable buffer
and evil turns them off in normal state; the minibuffer is neither."
  (minibuffer-with-setup-hook
      (lambda ()
        (when default-input-method
          (activate-input-method default-input-method))
        (let ((last initial))
          (add-hook 'post-command-hook
                    (lambda ()
                      (let ((text (minibuffer-contents-no-properties)))
                        (unless (equal text last)
                          (setq last text)
                          (let ((ghostel-cmd--keep-focus t))
                            (funcall on-change text)))))
                    nil t)))
    (read-string prompt initial)))

(defun ghostel-cmd-filter-self-insert ()
  (interactive)
  (let ((ev last-command-event))
    (cond
     ((ghostel-cmd--return-event-p ev)
      (ghostel-cmd-filter-confirm))
     ((and (characterp ev) (>= ev 32) (not (eq ev 127)))
      (setq ghostel-cmd--query
            (concat ghostel-cmd--query (char-to-string ev)))
      (ghostel-cmd--refresh-overlay)))))

(defun ghostel-cmd-filter-backspace ()
  (interactive)
  (when (> (length ghostel-cmd--query) 0)
    (setq ghostel-cmd--query (substring ghostel-cmd--query 0 -1))
    (ghostel-cmd--refresh-overlay)))

(defun ghostel-cmd-filter-clear ()
  (interactive)
  (setq ghostel-cmd--query "")
  (ghostel-cmd--refresh-overlay))

(defun ghostel-cmd-filter-yank ()
  "Append the latest kill to the query."
  (interactive)
  (setq ghostel-cmd--query (concat ghostel-cmd--query (ghostel-cmd--yank-text)))
  (ghostel-cmd--refresh-overlay))

(defun ghostel-cmd-filter-read ()
  "Edit the query in the minibuffer, where an input method works.
The list narrows as you type; C-g puts the old query back."
  (interactive)
  (let ((old ghostel-cmd--query)
        (set (lambda (query)
               (setq ghostel-cmd--query query)
               (ghostel-cmd--refresh-overlay))))
    (condition-case nil
        (funcall set (ghostel-cmd--read-line "Filter: " old set))
      (quit (funcall set old)))))

(defun ghostel-cmd--set-filtering (on)
  (setq ghostel-cmd--filtering (and on t))
  (when-let* ((buf (get-buffer ghostel-cmd--buffer-name)))
    (with-current-buffer buf
      (ghostel-cmd-filter-mode (if ghostel-cmd--filtering 1 -1))
      (when (fboundp 'evil-normalize-keymaps)
        (evil-normalize-keymaps))))
  (force-mode-line-update t))

(defun ghostel-cmd-filter-confirm ()
  (interactive)
  (ghostel-cmd--set-filtering nil))

(defun ghostel-cmd-filter ()
  (interactive)
  (when ghostel-cmd--fill
    (user-error "Finish or cancel fill first"))
  (ghostel-cmd--set-filtering t))

(defvar-keymap ghostel-cmd-filter-map
  :doc "Live-narrow keymap."
  "RET"         #'ghostel-cmd-filter-confirm
  "C-m"         #'ghostel-cmd-filter-confirm
  "<return>"    #'ghostel-cmd-filter-confirm
  "C-n"         #'next-line
  "C-p"         #'previous-line
  "<down>"      #'next-line
  "<up>"        #'previous-line
  "DEL"         #'ghostel-cmd-filter-backspace
  "<backspace>" #'ghostel-cmd-filter-backspace
  "<delete>"    #'ghostel-cmd-filter-backspace
  "C-h"         #'ghostel-cmd-filter-backspace
  "C-u"         #'ghostel-cmd-filter-clear
  "C-y"         #'ghostel-cmd-filter-yank
  "C-\\"        #'ghostel-cmd-filter-read
  "C-g"         #'ghostel-cmd-quit
  "<escape>"    #'ghostel-cmd-quit)

(define-key ghostel-cmd-filter-map [t] #'ghostel-cmd-filter-self-insert)
(define-key ghostel-cmd-filter-map [return] #'ghostel-cmd-filter-confirm)
(define-key ghostel-cmd-filter-map [kp-enter] #'ghostel-cmd-filter-confirm)

(defun ghostel-cmd--bind-printable-keys (map command)
  "Bind every printable ASCII key in MAP to COMMAND."
  (let ((c 32))
    (while (< c 127)
      (define-key map (vector c) command)
      (setq c (1+ c)))))

(ghostel-cmd--bind-printable-keys ghostel-cmd-filter-map
                                  #'ghostel-cmd-filter-self-insert)

(define-minor-mode ghostel-cmd-filter-mode
  "Live-narrow the command overlay."
  :lighter nil
  :keymap ghostel-cmd-filter-map)

(defun ghostel-cmd-fill-self-insert ()
  (interactive)
  (let ((ev last-command-event))
    (cond
     ((ghostel-cmd--return-event-p ev)
      (ghostel-cmd-fill-accept nil))
     ((and (characterp ev) (>= ev 32) (not (eq ev 127)))
      (when ghostel-cmd--fill
        (ghostel-cmd--fill-insert (char-to-string ev)))))))

(defun ghostel-cmd--fill-changed (redraw)
  "Show the fill as it now is: the filling overlay's :redraw, else REDRAW.
Another overlay fills on its own footer by giving :buffer, :redraw and
:deliver to `ghostel-cmd--start-fill'."
  (funcall (or (plist-get ghostel-cmd--fill :redraw) redraw))
  (force-mode-line-update t))

(defun ghostel-cmd--fill-set-input (input)
  "Make INPUT, typed rather than picked, the value being filled."
  (plist-put ghostel-cmd--fill :input input)
  (plist-put ghostel-cmd--fill :picked nil)
  (ghostel-cmd--fill-changed #'ghostel-cmd--draw-preview))

(defun ghostel-cmd--fill-insert (text)
  "Append TEXT to the value being filled; it replaces a picked choice."
  (ghostel-cmd--fill-set-input
   (concat (if (plist-get ghostel-cmd--fill :picked)
               ""
             (or (plist-get ghostel-cmd--fill :input) ""))
           text)))

(defun ghostel-cmd-fill-backspace ()
  "Delete the last char of the value being filled, or a picked choice."
  (interactive)
  (when-let* ((fill ghostel-cmd--fill)
              (input (plist-get fill :input)))
    (cond ((plist-get fill :picked)
           (ghostel-cmd--fill-set-input ""))
          ((> (length input) 0)
           (ghostel-cmd--fill-set-input (substring input 0 -1))))))

(defun ghostel-cmd-fill-clear ()
  (interactive)
  (when ghostel-cmd--fill
    (ghostel-cmd--fill-set-input "")))

(defun ghostel-cmd-fill-yank ()
  "Append the latest kill to the value being filled."
  (interactive)
  (when ghostel-cmd--fill
    (ghostel-cmd--fill-insert (ghostel-cmd--yank-text))))

(defun ghostel-cmd--fill-choices (&optional fill)
  "The choices of the variable FILL is asking for, or nil."
  (let ((fill (or fill ghostel-cmd--fill)))
    (cdr (assoc (car (nth (plist-get fill :index) (plist-get fill :vars)))
                (plist-get fill :choices)))))

(defun ghostel-cmd--fill-enter (fill index)
  "Make the variable at INDEX the one FILL asks for.
Its input starts at its value, picked when that is one of its choices."
  (let ((value (or (cdr (nth index (plist-get fill :vars))) "")))
    (plist-put fill :index index)
    (plist-put fill :input value)
    (plist-put fill :picked
               (and (member value (ghostel-cmd--fill-choices fill)) t))))

(defun ghostel-cmd--fill-cycle (step)
  "Pick the choice STEP away from the current one, wrapping around.
From free text, down picks the first choice and up the last."
  (when-let* ((fill ghostel-cmd--fill)
              (choices (ghostel-cmd--fill-choices fill)))
    (let ((n (length choices))
          (pos (cl-position (plist-get fill :input) choices :test #'equal)))
      (plist-put fill :input
                 (nth (cond (pos (mod (+ pos step) n))
                            ((> step 0) 0)
                            (t (1- n)))
                      choices))
      (plist-put fill :picked t)
      (ghostel-cmd--fill-changed #'ghostel-cmd--draw-preview))))

(defun ghostel-cmd-fill-next-choice ()
  "Pick the next choice of a {{name:a|b|c}} variable."
  (interactive)
  (ghostel-cmd--fill-cycle 1))

(defun ghostel-cmd-fill-previous-choice ()
  "Pick the previous choice of a {{name:a|b|c}} variable."
  (interactive)
  (ghostel-cmd--fill-cycle -1))

(defun ghostel-cmd-fill-read ()
  "Edit the value being filled in the minibuffer, with an input method.
The preview follows as you type; C-g puts the old value back."
  (interactive)
  (when-let* ((fill ghostel-cmd--fill))
    (let* ((old (or (plist-get fill :input) ""))
           (old-picked (plist-get fill :picked))
           (name (car (nth (plist-get fill :index) (plist-get fill :vars))))
           (set (lambda (input &optional picked)
                  (plist-put fill :input input)
                  (plist-put fill :picked picked)
                  (ghostel-cmd--fill-changed #'ghostel-cmd--redraw))))
      (condition-case nil
          (funcall set (ghostel-cmd--read-line (format "{{%s}}: " name) old set))
        (quit (funcall set old old-picked))))))

(defun ghostel-cmd--set-filling (on &optional buffer)
  "Turn the fill keys ON or off in BUFFER, the overlay's by default."
  (when-let* ((buf (or buffer (get-buffer ghostel-cmd--buffer-name)))
              ((buffer-live-p buf)))
    (with-current-buffer buf
      (ghostel-cmd-fill-mode (if on 1 -1))
      (when (fboundp 'evil-normalize-keymaps)
        (evil-normalize-keymaps))))
  (force-mode-line-update t))

(defun ghostel-cmd--start-fill (record root scope submit &rest props)
  "Start filling the {{vars}} of RECORD, of SCOPE, run under ROOT.
Return nil, starting nothing, when its command has none.  SUBMIT
non-nil presses Enter after the paste.  PROPS are :buffer, the overlay
that fills, :redraw, which shows it, and :deliver, called with the
filled command and SUBMIT; this overlay's own by default."
  (when-let* ((vars (ghostel-cmd--fill-vars record root scope)))
    (setq ghostel-cmd--fill
          (append (list :record record
                        :root root
                        :scope scope
                        :vars vars
                        :choices (ghostel-cmd--extract-choices
                                  (plist-get record :cmd))
                        :index 0
                        :values nil
                        :input ""
                        :picked nil
                        :submit submit)
                  props))
    (ghostel-cmd--fill-enter ghostel-cmd--fill 0)
    (ghostel-cmd--set-filling t (plist-get props :buffer))
    t))

(defun ghostel-cmd-fill-cancel ()
  "Leave fill mode without pasting."
  (interactive)
  (let ((fill ghostel-cmd--fill))
    (setq ghostel-cmd--fill nil)
    (ghostel-cmd--set-filling nil (plist-get fill :buffer))
    (funcall (or (plist-get fill :redraw) #'ghostel-cmd--draw-preview))
    (force-mode-line-update t)))

(defun ghostel-cmd--fill-commit-current ()
  "Store the current field's value.  Return non-nil when more vars remain."
  (let* ((fill ghostel-cmd--fill)
         (vars (plist-get fill :vars))
         (idx (plist-get fill :index))
         (pair (nth idx vars))
         (name (car pair))
         (default (or (cdr pair) ""))
         (raw (or (plist-get fill :input) ""))
         (picked (plist-get fill :picked))
         (val (if (and (string-empty-p raw) (not picked)) default raw))
         (values (cons (cons name val)
                       (cl-remove name (plist-get fill :values)
                                  :key #'car :test #'equal)))
         (next (1+ idx)))
    (ghostel-cmd--remember (plist-get fill :root) (plist-get fill :scope)
                           (plist-get fill :record) name val picked)
    (plist-put fill :values values)
    (if (>= next (length vars))
        nil
      (ghostel-cmd--fill-enter fill next)
      t)))

(defun ghostel-cmd-fill-accept (&optional submit)
  "Accept the current field; paste when it was the last.
SUBMIT non-nil also presses Enter in the terminal."
  (interactive)
  (when ghostel-cmd--fill
    (when submit
      (plist-put ghostel-cmd--fill :submit t))
    (if (ghostel-cmd--fill-commit-current)
        (ghostel-cmd--fill-changed #'ghostel-cmd--draw-preview)
      (let* ((fill ghostel-cmd--fill)
             (record (plist-get fill :record))
             (cmd (ghostel-cmd--apply (plist-get record :cmd)
                                      (plist-get fill :values)))
             (do-submit (plist-get fill :submit)))
        (setq ghostel-cmd--fill nil)
        (ghostel-cmd--set-filling nil (plist-get fill :buffer))
        (funcall (or (plist-get fill :deliver) #'ghostel-cmd--deliver)
                 cmd do-submit)))))

(defun ghostel-cmd-fill-accept-send ()
  (interactive)
  (ghostel-cmd-fill-accept t))

(defvar-keymap ghostel-cmd-fill-map
  :doc "Keymap while filling {{vars}}."
  "RET"         #'ghostel-cmd-fill-accept
  "C-m"         #'ghostel-cmd-fill-accept
  "<return>"    #'ghostel-cmd-fill-accept
  "TAB"         #'ghostel-cmd-fill-accept
  "<tab>"       #'ghostel-cmd-fill-accept
  "C-RET"       #'ghostel-cmd-fill-accept-send
  "C-<return>"  #'ghostel-cmd-fill-accept-send
  "DEL"         #'ghostel-cmd-fill-backspace
  "<backspace>" #'ghostel-cmd-fill-backspace
  "<delete>"    #'ghostel-cmd-fill-backspace
  "C-h"         #'ghostel-cmd-fill-backspace
  "C-u"         #'ghostel-cmd-fill-clear
  "C-y"         #'ghostel-cmd-fill-yank
  "C-\\"        #'ghostel-cmd-fill-read
  "C-n"         #'ghostel-cmd-fill-next-choice
  "C-p"         #'ghostel-cmd-fill-previous-choice
  "<down>"      #'ghostel-cmd-fill-next-choice
  "<up>"        #'ghostel-cmd-fill-previous-choice
  "C-g"         #'ghostel-cmd-fill-cancel
  "<escape>"    #'ghostel-cmd-fill-cancel)

(define-key ghostel-cmd-fill-map [t] #'ghostel-cmd-fill-self-insert)
(define-key ghostel-cmd-fill-map [return] #'ghostel-cmd-fill-accept)
(define-key ghostel-cmd-fill-map [tab] #'ghostel-cmd-fill-accept)
(define-key ghostel-cmd-fill-map [(control return)] #'ghostel-cmd-fill-accept-send)

(ghostel-cmd--bind-printable-keys ghostel-cmd-fill-map
                                  #'ghostel-cmd-fill-self-insert)

(define-minor-mode ghostel-cmd-fill-mode
  "Fill {{vars}} on the overlay footer."
  :lighter nil
  :keymap ghostel-cmd-fill-map)


;;; Deliver / CRUD

(defun ghostel-cmd--put-command (cmds record &optional old-name)
  "CMDS with RECORD in place of OLD-NAME, else of its namesake, else last.
Any other command with RECORD's name is dropped, so names stay unique."
  (let* ((name (plist-get record :name))
         (key (lambda (c) (plist-get c :name)))
         (anchor (or (and old-name
                          (cl-find old-name cmds :key key :test #'equal))
                     (cl-find name cmds :key key :test #'equal))))
    (if (null anchor)
        (append cmds (list record))
      (delq nil
            (mapcar (lambda (c)
                      (cond ((eq c anchor) record)
                            ((equal (funcall key c) name) nil)
                            (t c)))
                    cmds)))))

(defun ghostel-cmd--confirm-replace (cmds name &optional old-name)
  "Ask before NAME takes the place of another command in CMDS."
  (when (and (not (equal name old-name))
             (cl-find name cmds
                      :key (lambda (c) (plist-get c :name))
                      :test #'equal)
             (not (yes-or-no-p (format "Replace the existing '%s'? " name))))
    (user-error "Kept the existing '%s'" name)))

(defun ghostel-cmd--existing-terminal ()
  "Return the project Ghostel buffer if it already exists, else any Ghostel.
Does not display the buffer."
  (require 'ghostel)
  (or (when (and (fboundp 'ghostel--find-buffer-by-identity)
                 (fboundp 'ghostel--project-buffer-name)
                 (project-current nil))
        (ghostel--find-buffer-by-identity
         (ghostel--project-buffer-name
          (project-root (project-current t)))))
      (when (fboundp 'ghostel--all-buffers)
        (car (ghostel--all-buffers)))))

(defun ghostel-cmd--create-terminal ()
  "Create a Ghostel buffer without replacing the selected window."
  (let ((display-buffer-overriding-action
         '((display-buffer-reuse-window
            display-buffer-pop-up-window)
           (inhibit-same-window . t)
           (reusable-frames . visible))))
    (cond
     ((and (fboundp 'project-run-ghostel) (project-current nil))
      (project-run-ghostel))
     ((fboundp 'new-terminal)
      (new-terminal))
     ((fboundp 'ghostel)
      (funcall 'ghostel))
     (t
      (user-error "No Ghostel terminal helper loaded")))))

(defun ghostel-cmd--display-terminal (buf)
  "Show BUF in a window that already has it, or in a new split.
Never replaces the selected window."
  (pop-to-buffer
   buf
   '((display-buffer-reuse-window
      display-buffer-pop-up-window)
     (inhibit-same-window . t)
     (reusable-frames . visible))))

(defun ghostel-cmd--deliver (cmd-string submit &optional terminal)
  "Paste CMD-STRING into Ghostel.  SUBMIT non-nil presses Enter.
TERMINAL, a live Ghostel buffer, takes it; else the project's.
Keeps the current file window: reuse a visible terminal window, or
split if none is showing."
  (ghostel-cmd--hide-overlay)
  (require 'ghostel)
  (let ((term-buf (or (and (buffer-live-p terminal) terminal)
                      (ghostel-cmd--existing-terminal)
                      (ghostel-cmd--create-terminal))))
    (when (buffer-live-p term-buf)
      (ghostel-cmd--display-terminal term-buf)
      (with-current-buffer term-buf
        (ghostel-paste-string cmd-string)
        (when submit
          (ghostel-send-key "return"))))))

(defun ghostel-cmd--open-url (url &optional in-emacs)
  "Open URL in the system browser, or an Emacs xwidget when IN-EMACS."
  (ghostel-cmd--hide-overlay)
  (if in-emacs
      (if (fboundp 'xwidget-webkit-browse-url)
          (xwidget-webkit-browse-url url)
        (user-error "This Emacs has no xwidget-webkit support"))
    (browse-url url))
  (message "Opening %s%s" url (if in-emacs " (xwidget)" "")))

(defun ghostel-cmd-paste (&optional submit)
  "Act on the entry at point: open a service URL, else paste a command.
A URL entry opens in the system browser, or an Emacs xwidget with a
prefix (C-RET).  A command fills its {{vars}} in the overlay first."
  (interactive)
  (when ghostel-cmd--filtering
    (ghostel-cmd--set-filtering nil))
  (let* ((row (ghostel-cmd--row-at-point))
         (record (cdr row))
         (root (or ghostel-cmd--root (ghostel-cmd--current-project-root))))
    (unless record
      (user-error "No command at point"))
    (cond
     ((plist-get record :url)
      (ghostel-cmd--open-url (plist-get record :url) submit))
     ((ghostel-cmd--start-fill record root (car row) submit)
      (ghostel-cmd--draw-preview)
      (force-mode-line-update t))
     (t
      (ghostel-cmd--deliver (plist-get record :cmd) submit)))))

(defun ghostel-cmd-paste-send ()
  (interactive)
  (ghostel-cmd-paste t))


;;; Open the most likely frontend (SPC p o)

(defconst ghostel-cmd--frontend-name-regexp
  "\\(?:front\\(?:end\\)?\\|web\\|ui\\|app\\|client\\)"
  "Service / source names that mark the frontend, picked first.")

(defconst ghostel-cmd--web-port-order
  '(8080 3000 5173 4200 80 8000)
  "Preference among web (target) ports when no name says which is the
frontend.")

(defun ghostel-cmd--url-pick-key (root)
  "Save-file key under which ROOT's remembered open-url pick is kept."
  (cons :url-pick root))

(defun ghostel-cmd--url-pick (root)
  "The URL-entry name the user last chose for ROOT, or nil."
  (ghostel-cmd--ensure-cache)
  (alist-get (ghostel-cmd--url-pick-key root) ghostel-cmd--cache nil nil #'equal))

(defun ghostel-cmd--remember-url-pick (root name)
  "Keep NAME as ROOT's open-url pick; persist it."
  (ghostel-cmd--update-many
   (list (cons (ghostel-cmd--url-pick-key root) (lambda (_) name)))))

(defun ghostel-cmd--web-urls (root)
  "The web URL rows discovered for ROOT (non-web excluded)."
  (cl-remove-if-not (lambda (row) (and (plist-get (cdr row) :url)
                                       (plist-get (cdr row) :web)))
                    (ghostel-cmd--url-entries root)))

(defun ghostel-cmd--sole (list)
  "The one element of LIST, or nil when it has none or more than one."
  (and (= (length list) 1) (car list)))

(defun ghostel-cmd--frontend-candidate (urls)
  "The most likely frontend among URL rows URLS, or nil when unsure.
By name (frontend/web/ui/app/client), then a known web port, then the
dev server; nil when none decides it."
  (or
   (ghostel-cmd--sole
    (cl-remove-if-not
     (lambda (row)
       (let ((rec (cdr row)) (case-fold-search t))
         (string-match-p ghostel-cmd--frontend-name-regexp
                         (format "%s %s" (or (plist-get rec :service) "")
                                 (or (plist-get rec :source) "")))))
     urls))
   (cl-loop for p in ghostel-cmd--web-port-order
            for hit = (cl-find p urls :key (lambda (row)
                                             (plist-get (cdr row) :target)))
            when hit return hit)
   (ghostel-cmd--sole
    (cl-remove-if-not
     (lambda (row) (member (plist-get (cdr row) :source) '("vite" "dev" "next")))
     urls))))

(defun ghostel-cmd--ask-url (urls)
  "Ask which URL row of URLS to open."
  (let* ((alist (mapcar (lambda (row)
                          (cons (format "%s  (%s)" (plist-get (cdr row) :name)
                                        (plist-get (cdr row) :url))
                                row))
                        urls))
         (choice (completing-read "Open which service URL? " alist nil t)))
    (cdr (assoc choice alist))))

(defun ghostel-cmd-open-url (&optional in-emacs)
  "Open the project's most likely frontend in the browser.
Pick it by service name (frontend/web/ui/app/client), then a known web
port, then the dev server.  Ask only when it cannot tell, and remember
that pick for next time.  With a prefix (\\[universal-argument]), open
in an Emacs xwidget instead of the system browser."
  (interactive "P")
  (let* ((root (ghostel-cmd--current-project-root))
         (urls (ghostel-cmd--web-urls root)))
    (unless urls
      (user-error "No web service URL here (compose, Vite or a dev script)"))
    (let* ((remembered (ghostel-cmd--url-pick root))
           (row (or (and remembered
                         (cl-find remembered urls
                                  :key (lambda (r) (plist-get (cdr r) :name))
                                  :test #'equal))
                    (ghostel-cmd--frontend-candidate urls)))
           (asked (null row)))
      (when asked
        (setq row (ghostel-cmd--ask-url urls)))
      (when row
        (when asked
          (ghostel-cmd--remember-url-pick root (plist-get (cdr row) :name)))
        (ghostel-cmd--open-url (plist-get (cdr row) :url) in-emacs)))))

(defun ghostel-cmd--with-parent-input (fn)
  "Run FN on the parent frame so minibuffer prompts work, then reopen."
  (let ((parent ghostel-cmd--posframe-parent)
        (root ghostel-cmd--root)
        (file ghostel-cmd--caller-file)
        (line ghostel-cmd--caller-line)
        (dir ghostel-cmd--caller-dir)
        (symbol ghostel-cmd--caller-symbol))
    (ghostel-cmd--hide-posframe)
    (when (and parent (frame-live-p parent))
      (select-frame-set-input-focus parent))
    (unwind-protect
        (funcall fn)
      (setq ghostel-cmd--root root
            ghostel-cmd--caller-file file
            ghostel-cmd--caller-line line
            ghostel-cmd--caller-dir dir
            ghostel-cmd--caller-symbol symbol)
      (when (and root (not (minibufferp)))
        (ghostel-cmd-sidebar)))))

(defun ghostel-cmd--add (initial-cmd scope)
  "Ask for a command template from INITIAL-CMD and add it to SCOPE."
  (let ((run
         (lambda ()
           (let* ((root (or ghostel-cmd--root
                            (ghostel-cmd--current-project-root)))
                  (key (ghostel-cmd--store-key scope root))
                  (cmd (read-string (if (eq scope 'global)
                                        "Global command template: "
                                      "Command template: ")
                                    (or initial-cmd "")))
                  (suggested
                   (when (string-match "\\`[ \t]*\\([a-zA-Z0-9_-]+\\)" cmd)
                     (match-string 1 cmd)))
                  (name (read-string "Name / alias: " nil nil suggested))
                  (desc (read-string "Description (optional): "))
                  (record (list :name name :cmd cmd :desc desc)))
             (when (string-empty-p name)
               (user-error "Command name cannot be empty"))
             (when (string-empty-p cmd)
               (user-error "Command cannot be empty"))
             (ghostel-cmd--confirm-replace (ghostel-cmd--get-commands key) name)
             (ghostel-cmd--update-commands
              (lambda (cmds) (ghostel-cmd--put-command cmds record))
              key)
             (setq ghostel-cmd--goto-id (cons scope name))
             (message (if (eq scope 'global) "Added global '%s'" "Added '%s'")
                      name)))))
    (if (ghostel-cmd--posframe-showing-p)
        (ghostel-cmd--with-parent-input run)
      (funcall run)
      (when (get-buffer ghostel-cmd--buffer-name)
        (ghostel-cmd--refresh-overlay)))))

(defun ghostel-cmd-add (&optional initial-cmd)
  "Add a command template to the project.  Region text is the default command."
  (interactive
   (list (when (use-region-p)
           (buffer-substring-no-properties (region-beginning) (region-end)))))
  (ghostel-cmd--add initial-cmd 'project))

(defun ghostel-cmd-add-global (&optional initial-cmd)
  "Add a command template every project shares.
Region text is the default command."
  (interactive
   (list (when (use-region-p)
           (buffer-substring-no-properties (region-beginning) (region-end)))))
  (ghostel-cmd--add initial-cmd 'global))

(defun ghostel-cmd-edit ()
  "Edit the command at point, in its own list."
  (interactive)
  (pcase-let ((`(,scope . ,record) (ghostel-cmd--bookmark-at-point)))
    (let ((run
           (lambda ()
             (let* ((root (or ghostel-cmd--root
                              (ghostel-cmd--current-project-root)))
                    (key (ghostel-cmd--store-key scope root))
                    (old-name (plist-get record :name))
                    (new-name (read-string "Name / alias: " old-name))
                    (new-cmd (read-string "Command template: "
                                          (plist-get record :cmd)))
                    (new-desc (read-string "Description (optional): "
                                           (or (plist-get record :desc) "")))
                    (new (list :name new-name :cmd new-cmd :desc new-desc)))
               (when (string-empty-p new-name)
                 (user-error "Command name cannot be empty"))
               (when (string-empty-p new-cmd)
                 (user-error "Command cannot be empty"))
               (ghostel-cmd--confirm-replace
                (ghostel-cmd--get-commands key) new-name old-name)
               (ghostel-cmd--update-commands
                (lambda (cmds) (ghostel-cmd--put-command cmds new old-name))
                key)
               (setq ghostel-cmd--goto-id (cons scope new-name))
               (message "Updated '%s'" new-name)))))
      (if (ghostel-cmd--posframe-showing-p)
          (ghostel-cmd--with-parent-input run)
        (funcall run)
        (ghostel-cmd--refresh-overlay)))))

(defun ghostel-cmd--remove-command (cmds name)
  "CMDS without the one called NAME."
  (cl-remove name cmds :key (lambda (c) (plist-get c :name)) :test #'equal))

(defun ghostel-cmd-delete ()
  "Delete the command at point from its list."
  (interactive)
  (pcase-let ((`(,scope . ,record) (ghostel-cmd--bookmark-at-point)))
    (let ((run
           (lambda ()
             (let* ((root (or ghostel-cmd--root
                              (ghostel-cmd--current-project-root)))
                    (name (plist-get record :name)))
               (when (yes-or-no-p (format "Delete '%s'? " name))
                 (ghostel-cmd--update-commands
                  (lambda (cmds) (ghostel-cmd--remove-command cmds name))
                  (ghostel-cmd--store-key scope root))
                 (message "Deleted '%s'" name))))))
      (if (ghostel-cmd--posframe-showing-p)
          (ghostel-cmd--with-parent-input run)
        (funcall run)
        (ghostel-cmd--refresh-overlay)))))

(defun ghostel-cmd-move-scope ()
  "Move the command at point from the project's list to the global one,
or back.  A discovered task is copied into the project's list, where
it can be edited."
  (interactive)
  (pcase-let* ((`(,from . ,record) (or (ghostel-cmd--row-at-point)
                                       (user-error "No command at point")))
               (task (ghostel-cmd--task-p from))
               (record (if task (copy-sequence record) record))
               (root (or ghostel-cmd--root (ghostel-cmd--current-project-root)))
               (name (plist-get record :name))
               (to (if (eq from 'project) 'global 'project))
               (to-key (ghostel-cmd--store-key to root))
               (taken (cl-find name (ghostel-cmd--get-commands to-key)
                               :key (lambda (c) (plist-get c :name))
                               :test #'equal))
               (run
                (lambda ()
                  (ghostel-cmd--confirm-replace
                   (ghostel-cmd--get-commands to-key) name)
                  (ghostel-cmd--update-many
                   (cons (cons to-key
                               (lambda (cmds)
                                 (ghostel-cmd--put-command cmds record)))
                         (unless task
                           (list (cons (ghostel-cmd--store-key from root)
                                       (lambda (cmds)
                                         (ghostel-cmd--remove-command
                                          cmds name)))))))
                  (setq ghostel-cmd--goto-id (cons to name))
                  (message (cond (task "Saved '%s' to the project")
                                 ((eq to 'global)
                                  "Moved '%s' to the global commands")
                                 (t "Moved '%s' to the project"))
                           name))))
    (if (and taken (ghostel-cmd--posframe-showing-p))
        (ghostel-cmd--with-parent-input run)
      (funcall run)
      (ghostel-cmd--refresh-overlay))))

(defun ghostel-cmd--move-command (cmds name neighbor after)
  "CMDS with NAME moved just AFTER NEIGHBOR, or just before it."
  (let* ((key (lambda (c) (plist-get c :name)))
         (rec (cl-find name cmds :key key :test #'equal))
         (rest (remq rec cmds))
         (pos (cl-position neighbor rest :key key :test #'equal)))
    (if (not (and rec pos))
        cmds
      (let ((i (if after (1+ pos) pos)))
        (append (take i rest) (list rec) (nthcdr i rest))))))

(defun ghostel-cmd--move-at-point (step)
  "Move the command at point past the row STEP lines away, and save.
Under a filter that row is the next visible one, not a hidden one.  A
command stays in its own list: the first row of the next list is no
neighbor."
  (ghostel-cmd--bookmark-at-point)
  (let ((id (tabulated-list-get-id))
        (neighbor (save-excursion
                    (forward-line step)
                    (tabulated-list-get-id)))
        (root (or ghostel-cmd--root (ghostel-cmd--current-project-root))))
    (when (and neighbor
               (not (equal neighbor id))
               (eq (car neighbor) (car id)))
      (ghostel-cmd--update-commands
       (lambda (cmds)
         (ghostel-cmd--move-command cmds (cdr id) (cdr neighbor) (> step 0)))
       (ghostel-cmd--store-key (car id) root))
      (ghostel-cmd--refresh-overlay))))

(defun ghostel-cmd-move-down ()
  "Move the command at point one row down; the order is saved."
  (interactive)
  (ghostel-cmd--move-at-point 1))

(defun ghostel-cmd-move-up ()
  "Move the command at point one row up; the order is saved."
  (interactive)
  (ghostel-cmd--move-at-point -1))

(defun ghostel-cmd-open-save-file ()
  "Visit the on-disk bookmark file."
  (interactive)
  (ghostel-cmd--hide-overlay)
  (find-file ghostel-cmd-save-file))


;;; Posframe / side window

(defun ghostel-cmd--posframe-available-p ()
  (and (require 'posframe nil t)
       (fboundp 'posframe-workable-p)
       (posframe-workable-p)))

(defun ghostel-cmd--use-posframe-p ()
  (and ghostel-cmd-use-posframe
       (ghostel-cmd--posframe-available-p)))

(defun ghostel-cmd--posframe-frame ()
  (when-let* ((buf (get-buffer ghostel-cmd--buffer-name)))
    (and (boundp 'posframe--frame)
         (buffer-local-value 'posframe--frame buf))))

(defun ghostel-cmd--posframe-showing-p ()
  (when-let* ((frame (ghostel-cmd--posframe-frame)))
    (and (frame-live-p frame)
         (frame-visible-p frame))))

(defun ghostel-cmd--posframe-hooks (on)
  "Add the global hooks a shown posframe needs when ON, else remove them."
  (dolist (hook '((window-size-change-functions
                   . ghostel-cmd--posframe-on-parent-resize)
                  (window-selection-change-functions
                   . ghostel-cmd--hide-on-focus-leave)))
    (if on
        (add-hook (car hook) (cdr hook))
      (remove-hook (car hook) (cdr hook)))))

(defun ghostel-cmd--hide-posframe (&optional keep-focus)
  "Hide the posframe and give focus back to its parent frame.
KEEP-FOCUS non-nil leaves focus where it is."
  (let ((parent ghostel-cmd--posframe-parent)
        (buf (get-buffer ghostel-cmd--buffer-name)))
    (setq ghostel-cmd--posframe-parent nil
          ghostel-cmd--posframe-fitted-width nil
          ghostel-cmd--fill nil
          ghostel-cmd--help-visible nil)
    (ghostel-cmd--set-filtering nil)
    (ghostel-cmd--set-filling nil)
    (ghostel-cmd--posframe-hooks nil)
    (when (and buf (fboundp 'posframe-hide))
      (posframe-hide buf))
    (when (and parent (frame-live-p parent) (not keep-focus))
      (select-frame-set-input-focus parent))))

(defun ghostel-cmd--hide-on-focus-leave (_frame)
  "Hide the posframe once a window outside it is selected.
A click in another window would otherwise leave it in the middle of
the frame, deaf to q.  The minibuffer does not count: it may be
editing for the overlay."
  (when-let* ((frame (ghostel-cmd--posframe-frame))
              ((ghostel-cmd--posframe-showing-p))
              (win (selected-window))
              ((not (eq (window-frame win) frame)))
              ((not (window-minibuffer-p win))))
    (ghostel-cmd--hide-posframe t)))

(defun ghostel-cmd--hide-overlay ()
  (if (ghostel-cmd--posframe-showing-p)
      (ghostel-cmd--hide-posframe)
    (when-let* ((buf (get-buffer ghostel-cmd--buffer-name))
                (win (get-buffer-window buf)))
      (quit-window nil win))))

(defun ghostel-cmd-quit ()
  "Clear query, cancel fill, or dismiss the overlay."
  (interactive)
  (cond
   (ghostel-cmd--fill
    (ghostel-cmd-fill-cancel))
   ((and ghostel-cmd--query (not (string-empty-p ghostel-cmd--query)))
    (setq ghostel-cmd--query "")
    (ghostel-cmd--set-filtering nil)
    (ghostel-cmd--refresh-overlay))
   (ghostel-cmd--help-visible
    (setq ghostel-cmd--help-visible nil)
    (ghostel-cmd--redraw)
    (force-mode-line-update t))
   (t
    (setq ghostel-cmd--query "")
    (ghostel-cmd--set-filtering nil)
    (if (ghostel-cmd--posframe-showing-p)
        (ghostel-cmd--hide-posframe)
      (quit-window)))))

(defun ghostel-cmd--redraw ()
  "Redraw the preview pane and refit a visible posframe."
  (when-let* ((buf (get-buffer ghostel-cmd--buffer-name)))
    (with-current-buffer buf
      (when (derived-mode-p 'ghostel-cmd-sidebar-mode)
        (ghostel-cmd--draw-preview)))
    (when (ghostel-cmd--posframe-showing-p)
      (ghostel-cmd--show-posframe buf))))

(defun ghostel-cmd-help ()
  "Toggle the key legend in the overlay preview pane."
  (interactive)
  (setq ghostel-cmd--help-visible (not ghostel-cmd--help-visible))
  (ghostel-cmd--redraw)
  (force-mode-line-update t))

(defun ghostel-cmd--close-side-windows (buf)
  (dolist (win (get-buffer-window-list buf nil t))
    (when (and (window-parameter win 'window-side)
               (not (frame-parent (window-frame win))))
      (ignore-errors (delete-window win)))))

(defun ghostel-cmd--posframe-border-color ()
  (or (face-foreground 'vertical-border nil t)
      (face-foreground 'mode-line-inactive nil t)
      "gray50"))

(defun ghostel-cmd--posframe-parent-frame ()
  (or (and (frame-live-p ghostel-cmd--posframe-parent)
           ghostel-cmd--posframe-parent)
      (frame-parent (selected-frame))
      (selected-frame)))

(defun ghostel-cmd--posframe-char-width (&optional frame)
  (let* ((frame (or frame (ghostel-cmd--posframe-parent-frame)))
         (avail (max 1 (frame-width frame)))
         (margin (min 12 (max 4 (/ avail 16))))
         (ceil (max 48 (- avail margin)))
         (floor (min ghostel-cmd-posframe-width ceil)))
    (if (null ghostel-cmd-posframe-width-ratio)
        floor
      (let ((wanted (round (* ghostel-cmd-posframe-width-ratio avail))))
        (max floor (min wanted ceil))))))

(defun ghostel-cmd--posframe-max-height (&optional frame)
  (let* ((frame (or frame (ghostel-cmd--posframe-parent-frame)))
         (avail (max 8 (frame-height frame))))
    (max 8 (min (- avail 4) (round (* 0.6 avail))))))

(defun ghostel-cmd--preview-reserved-lines ()
  (if ghostel-cmd--help-visible
      (max 8 (length (split-string (ghostel-cmd--help-text) "\n")))
    ;; Separator, the wrapped command, and the fill input line.
    (+ 2 ghostel-cmd-preview-max-lines)))

(defun ghostel-cmd--show-posframe (buf)
  (ghostel-cmd--close-side-windows buf)
  (let* ((parent (ghostel-cmd--posframe-parent-frame))
         (width (or ghostel-cmd--target-width
                    (ghostel-cmd--posframe-char-width parent)))
         (maxh (ghostel-cmd--posframe-max-height parent))
         (preview (ghostel-cmd--preview-reserved-lines))
         (rows (with-current-buffer buf
                 (max 1 (length tabulated-list-entries))))
         (table (max 4 (min rows (max 4 (- maxh preview 1)))))
         (height (+ 1 table preview)))
    (setq ghostel-cmd--posframe-parent parent
          ghostel-cmd--posframe-fitted-width width)
    (ghostel-cmd--posframe-hooks t)
    (with-selected-frame parent
      (posframe-show
       buf
       :poshandler #'posframe-poshandler-frame-center
       :position (point)
       :width width
       :min-width (min 48 width)
       :height height
       :min-height height
       :max-height (max height maxh)
       :left-fringe 12
       :right-fringe 12
       :border-width 2
       :border-color (ghostel-cmd--posframe-border-color)
       :respect-header-line t
       :respect-mode-line t
       :lines-truncate t
       :cursor 'box
       :accept-focus t
       :window-point (with-current-buffer buf (point))))
    (when-let* (((not ghostel-cmd--keep-focus))
                (frame (ghostel-cmd--posframe-frame))
                (win (get-buffer-window buf frame)))
      (select-frame-set-input-focus frame)
      (select-window win))))

(defun ghostel-cmd--posframe-on-parent-resize (frame)
  (when (and (not ghostel-cmd--posframe-refitting)
             ghostel-cmd--posframe-parent
             (eq frame ghostel-cmd--posframe-parent)
             (ghostel-cmd--posframe-showing-p))
    (let ((width (ghostel-cmd--posframe-char-width frame)))
      (unless (eql width ghostel-cmd--posframe-fitted-width)
        (let ((ghostel-cmd--posframe-refitting t)
              (ghostel-cmd--target-width width))
          (setq ghostel-cmd--posframe-fitted-width width)
          (when-let* ((buf (get-buffer ghostel-cmd--buffer-name)))
            (ghostel-cmd--refresh-overlay)))))))

(defun ghostel-cmd--show-side-window (buf)
  (when (fboundp 'posframe-hide)
    (posframe-hide buf))
  (ghostel-cmd--posframe-hooks nil)
  (setq ghostel-cmd--posframe-parent nil)
  (pop-to-buffer
   buf
   `((display-buffer-in-side-window)
     (side . ,ghostel-cmd-sidebar-side)
     (slot . 0)
     (window-width . ,ghostel-cmd-sidebar-width)
     (preserve-size . (t . nil)))))

(defun ghostel-cmd--prepare-buffer ()
  (let ((buf (get-buffer-create ghostel-cmd--buffer-name)))
    (with-current-buffer buf
      (setq-local default-directory
                  (or ghostel-cmd--root default-directory))
      (unless (derived-mode-p 'ghostel-cmd-sidebar-mode)
        (ghostel-cmd-sidebar-mode))
      (ghostel-cmd--build-entries)
      (ghostel-cmd--print t))
    buf))

;;;###autoload
(defun ghostel-cmd-sidebar ()
  "Open the project command overlay.

Posframe when available, side window otherwise.  RET pastes into
Ghostel after filling {{vars}} on the overlay footer.  C-RET also
presses Enter."
  (interactive)
  (setq ghostel-cmd--root (ghostel-cmd--current-project-root)
        ghostel-cmd--caller-file buffer-file-name
        ghostel-cmd--caller-line (line-number-at-pos)
        ghostel-cmd--caller-dir default-directory
        ghostel-cmd--caller-symbol (thing-at-point 'symbol t)
        ghostel-cmd--query ""
        ghostel-cmd--help-visible nil
        ghostel-cmd--fill nil
        ghostel-cmd--preview-id nil)
  (ghostel-cmd--set-filtering nil)
  (let* ((overlay (ghostel-cmd--use-posframe-p))
         (ghostel-cmd--target-width
          (and overlay (ghostel-cmd--posframe-char-width)))
         (buf (ghostel-cmd--prepare-buffer)))
    (if overlay
        (ghostel-cmd--show-posframe buf)
      (ghostel-cmd--show-side-window buf))
    buf))


;;; Mode / keys

(defvar-keymap ghostel-cmd-sidebar-mode-map
  :doc "Keymap for the command overlay."
  "?"          #'ghostel-cmd-help
  "n"          #'next-line
  "p"          #'previous-line
  "RET"        #'ghostel-cmd-paste
  "<return>"   #'ghostel-cmd-paste
  "C-RET"      #'ghostel-cmd-paste-send
  "C-<return>" #'ghostel-cmd-paste-send
  "u"          #'ghostel-cmd-open-url
  "a"          #'ghostel-cmd-add
  "A"          #'ghostel-cmd-add-global
  "e"          #'ghostel-cmd-edit
  "d"          #'ghostel-cmd-delete
  "m"          #'ghostel-cmd-move-scope
  "M-j"        #'ghostel-cmd-move-down
  "M-k"        #'ghostel-cmd-move-up
  "M-<down>"   #'ghostel-cmd-move-down
  "M-<up>"     #'ghostel-cmd-move-up
  "/"          #'ghostel-cmd-filter
  "o"          #'ghostel-cmd-open-save-file
  "g"          #'tabulated-list-revert
  "q"          #'ghostel-cmd-quit
  "C-g"        #'ghostel-cmd-quit
  "<escape>"   #'ghostel-cmd-quit)

(defun ghostel-cmd--maybe-setup-evil ()
  (when (fboundp 'evil-make-intercept-map)
    (evil-make-intercept-map ghostel-cmd-filter-map 'normal)
    (evil-make-intercept-map ghostel-cmd-fill-map 'normal))
  (when (fboundp 'evil-define-key*)
    (evil-define-key* 'normal ghostel-cmd-filter-map
      (kbd "RET") #'ghostel-cmd-filter-confirm
      (kbd "<return>") #'ghostel-cmd-filter-confirm
      (kbd "C-m") #'ghostel-cmd-filter-confirm
      (kbd "<escape>") #'ghostel-cmd-quit)
    (evil-define-key* 'normal ghostel-cmd-fill-map
      (kbd "RET") #'ghostel-cmd-fill-accept
      (kbd "<return>") #'ghostel-cmd-fill-accept
      (kbd "TAB") #'ghostel-cmd-fill-accept
      (kbd "<tab>") #'ghostel-cmd-fill-accept
      (kbd "C-RET") #'ghostel-cmd-fill-accept-send
      (kbd "C-<return>") #'ghostel-cmd-fill-accept-send
      (kbd "C-n") #'ghostel-cmd-fill-next-choice
      (kbd "C-p") #'ghostel-cmd-fill-previous-choice
      (kbd "<down>") #'ghostel-cmd-fill-next-choice
      (kbd "<up>") #'ghostel-cmd-fill-previous-choice
      (kbd "<escape>") #'ghostel-cmd-fill-cancel)
    (evil-define-key* 'normal ghostel-cmd-sidebar-mode-map
      (kbd "?") #'ghostel-cmd-help
      (kbd "RET") #'ghostel-cmd-paste
      (kbd "<return>") #'ghostel-cmd-paste
      (kbd "C-RET") #'ghostel-cmd-paste-send
      (kbd "C-<return>") #'ghostel-cmd-paste-send
      (kbd "u") #'ghostel-cmd-open-url
      (kbd "a") #'ghostel-cmd-add
      (kbd "A") #'ghostel-cmd-add-global
      (kbd "e") #'ghostel-cmd-edit
      (kbd "d") #'ghostel-cmd-delete
      (kbd "m") #'ghostel-cmd-move-scope
      (kbd "M-j") #'ghostel-cmd-move-down
      (kbd "M-k") #'ghostel-cmd-move-up
      (kbd "M-<down>") #'ghostel-cmd-move-down
      (kbd "M-<up>") #'ghostel-cmd-move-up
      (kbd "/") #'ghostel-cmd-filter
      (kbd "o") #'ghostel-cmd-open-save-file
      (kbd "gr") #'tabulated-list-revert
      (kbd "q") #'ghostel-cmd-quit
      (kbd "<escape>") #'ghostel-cmd-quit)))

(with-eval-after-load 'evil
  (ghostel-cmd--maybe-setup-evil))

(define-derived-mode ghostel-cmd-sidebar-mode tabulated-list-mode "Ghostel-cmd"
  "Listing of per-project Ghostel command templates."
  (setq tabulated-list-padding 1)
  (setq tabulated-list-format (ghostel-cmd--format))
  (setq truncate-lines t)
  (setq-local mode-line-format '("" (:eval (ghostel-cmd--footer))))
  (hl-line-mode 1)
  (add-hook 'tabulated-list-revert-hook #'ghostel-cmd--build-entries nil t)
  (add-hook 'post-command-hook #'ghostel-cmd--preview-on-command nil t)
  (when (fboundp 'evil-normalize-keymaps)
    (evil-normalize-keymaps))
  (tabulated-list-init-header))


;;; Aliases (SPC p c / C-c c)

(defalias 'ghostel-project-cmd-run 'ghostel-cmd-sidebar)
(defalias 'ghostel-project-cmd-open-url 'ghostel-cmd-open-url)
(defalias 'ghostel-project-cmd-add 'ghostel-cmd-add)
(defalias 'ghostel-project-cmd-edit 'ghostel-cmd-edit)
(defalias 'ghostel-project-cmd-delete 'ghostel-cmd-delete)
(defalias 'ghostel-project-cmd-open-save-file 'ghostel-cmd-open-save-file)
(defalias 'ghostel-project-cmd--current-project-root
  'ghostel-cmd--current-project-root)

(provide 'ghostel-cmd)
;;; ghostel-cmd.el ends here

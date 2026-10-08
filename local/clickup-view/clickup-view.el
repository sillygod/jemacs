;;; clickup-view.el --- ClickUp tasks in an xwidget -*- lexical-binding: t; -*-

;; Author: Jing
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Read a ClickUp task, change its status, comment and reply, and browse
;; Space ▸ Folder ▸ List, without leaving Emacs.  Built on xwapp: Elisp
;; owns HTTP and the token, ui/ only renders.
;;
;;   M-x clickup-view        the task this branch's commits reference,
;;                           or the browser when there is none (C-u: browse)
;;   M-x clickup-view-task   a task by id, URL or CU-id
;;   M-x clickup-set-status  change a status from the minibuffer, no page
;;
;; Token: CLICKUP_API_TOKEN, a personal `pk_' token sent bare (no Bearer).
;;
;; Comment bodies arrive as Quill delta ops, not markdown.  They are
;; turned into HTML here rather than in the page because the page can
;; issue intents -- change a status, post a comment -- so markup slipped
;; into a comment must never reach it unescaped.  Outgoing comments are
;; converted from markdown into delta ops, as the clickup-task-manager
;; skill does: ClickUp stores `comment_text' verbatim, raw `**' and all.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url-util)
(require 'xwapp)


;;; Customization

(defgroup clickup-view nil
  "ClickUp tasks inside Emacs."
  :group 'tools
  :prefix "clickup-view-")

(defcustom clickup-view-team-id nil
  "ClickUp workspace (team) id.  Nil means the token's first workspace."
  :type '(choice (const nil) string)
  :group 'clickup-view)

(defcustom clickup-view-scan-commits 50
  "How many commits of the current branch to search for task references."
  :type 'integer
  :group 'clickup-view)

(defcustom clickup-view-poll-interval 0.2
  "Seconds between title-intent polls."
  :type 'number
  :group 'clickup-view)


;;; State

(defconst clickup-view--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this file and the ui/ folder.")

(defconst clickup-view--api "https://api.clickup.com/api/v2")

(defconst clickup-view--comment-page 25
  "Comments ClickUp returns per request; a full page means maybe more.")

(defvar clickup-view--team nil "Workspace id in use.")
(defvar clickup-view--me nil "The token's user, normalized.")
(defvar clickup-view--spaces nil "Spaces of the workspace, normalized.")
(defvar clickup-view--space-cache (make-hash-table :test #'equal)
  "Space id → its folders and folderless lists.")
(defvar clickup-view--list-cache (make-hash-table :test #'equal)
  "List id → name and statuses.  Statuses belong to a list, not a space.")
(defvar clickup-view--pending nil
  "Task id to show once the page reports ready; nil shows the browser.")

(defvar clickup-view--app
  (xwapp-create :buffer-name "*clickup*"
                :index (expand-file-name "ui/index.html" clickup-view--dir)
                :prefixes '("clickup:")
                :namespace "CU"
                :idle-title "ClickUp"
                :handler #'clickup-view--handle-intent
                :on-kill #'clickup-view--on-kill)
  "The page, buffer and intent channel; see `xwapp'.")


;;; Small helpers

(defun clickup-view--truthy (v)
  "Non-nil when V is a true value from the page.
Intents parse JSON false as `:false', which Lisp counts as true."
  (and v (not (memq v '(:false :null)))))

(defun clickup-view--int (v)
  "V as an integer: ClickUp sends some counts as strings."
  (cond ((integerp v) v)
        ((numberp v) (truncate v))
        ((and (stringp v) (string-match-p "\\`[0-9]+\\'" v)) (string-to-number v))
        (t 0)))

(defun clickup-view--bool (v)
  "V as a JSON boolean."
  (if (clickup-view--truthy v) t :json-false))

(defun clickup-view--vec (fn xs)
  "FN over XS as a vector, so an empty result encodes as [] not null."
  (vconcat (mapcar fn xs)))


;;; Task references

(defconst clickup-view--id-re "[0-9a-z]*[0-9][0-9a-z]*"
  "A native task id: lowercase letters and digits, at least one digit.
Every id in the workspace has digits (600 sampled, 9-10 characters);
words do not, so \"CU-ids\" in a commit message is not a task.")

(defconst clickup-view--ref-re
  (concat
   ;; https://app.clickup.com/t/<id>, or /t/<team>/<id>.  The id must not
   ;; be followed by "/": that segment would be the team, and a custom id
   ;; after it (ABC-12) is not a native id.
   "app\\.clickup\\.com/t/\\(?:[0-9]+/\\)?\\(" clickup-view--id-re "\\)\\(?:[^/0-9a-zA-Z-]\\|\\'\\)"
   ;; CU-<id> on its own, not inside a word.  Spelled out rather than
   ;; \\_< \\_>, which follow the current buffer's syntax table: in
   ;; elisp-mode "-" is a symbol char and feature/fix-CU-abc1 would miss.
   "\\|\\(?:\\`\\|[^0-9A-Za-z_]\\)CU-\\(" clickup-view--id-re "\\)\\(?:[^0-9A-Za-z_]\\|\\'\\)")
  "Matches a ClickUp task link or a CU-<id> reference.")

(defun clickup-view--task-refs (text)
  "Native task ids TEXT references, in order of first appearance."
  (let ((case-fold-search nil)
        (pos 0)
        ids)
    (when (stringp text)
      (while (string-match clickup-view--ref-re text pos)
        (let ((id (or (match-string 1 text) (match-string 2 text))))
          (unless (member id ids) (push id ids)))
        (setq pos (max (1+ (match-beginning 0))
                       (or (match-end 2) (match-end 1)))))
      (nreverse ids))))

(defun clickup-view--normalize-id (s)
  "Task id from S: a link, a CU-<id>, or a bare id.  Nil if none."
  (when (stringp s)
    (let ((s (string-trim s))
          (case-fold-search nil))
      (or (car (clickup-view--task-refs s))
          (and (string-match-p (concat "\\`" clickup-view--id-re "\\'") s) s)))))

(defun clickup-view--git (&rest args)
  "Run git ARGS in the current repo.  Return trimmed stdout or nil."
  (when-let* (((executable-find "git"))
              (root (locate-dominating-file default-directory ".git")))
    (let ((default-directory root))
      (with-temp-buffer
        (when (eq 0 (apply #'call-process "git" nil t nil args))
          (let ((out (string-trim (buffer-string))))
            (and (not (string-empty-p out)) out)))))))

(defun clickup-view--base-ref ()
  "The remote default branch to diff the current branch against, or nil."
  (seq-find (lambda (ref) (clickup-view--git "rev-parse" "--verify" "--quiet" ref))
            '("origin/HEAD" "origin/main" "origin/master" "origin/develop")))

(defun clickup-view--branch-refs ()
  "Tasks the current branch references, as ((ID . WHERE) ...).
Looks at the branch name and at the commits not yet on the remote
default branch; without one, only HEAD's own commit, so references
from old history on main do not leak in."
  (let* ((branch (clickup-view--git "rev-parse" "--abbrev-ref" "HEAD"))
         (base (clickup-view--base-ref))
         (log (if base
                  (clickup-view--git "log" "--format=%x1e%s%n%b"
                                     (format "-n%d" clickup-view-scan-commits)
                                     (concat base "..HEAD"))
                (clickup-view--git "log" "--format=%x1e%s%n%b" "-n1")))
         refs)
    (dolist (id (clickup-view--task-refs branch))
      (push (cons id (format "branch %s" branch)) refs))
    (dolist (commit (and log (split-string log "\x1e" t)))
      (let ((subject (car (split-string (string-trim commit) "\n"))))
        (dolist (id (clickup-view--task-refs commit))
          (unless (assoc id refs)
            (push (cons id subject) refs)))))
    (nreverse refs)))

(defun clickup-view--read-task-id ()
  "Ask for a task as an id, a link or a CU-<id>."
  (let ((id (clickup-view--normalize-id
             (read-string "ClickUp task (id, link or CU-id): "))))
    (or id (user-error "Not a ClickUp task id"))))

(defun clickup-view--pick-task-id (&optional allow-none)
  "The task the current branch references, asking when several do.
With none: nil if ALLOW-NONE, else ask for one."
  (let ((refs (clickup-view--branch-refs)))
    (cond
     ((null refs) (unless allow-none (clickup-view--read-task-id)))
     ((null (cdr refs)) (caar refs))
     (t
      (let* ((cands (mapcar (lambda (r) (cons (format "%s  %s" (car r) (cdr r)) (car r)))
                            refs))
             (pick (completing-read "ClickUp task: " cands nil t)))
        (cdr (assoc pick cands)))))))


;;; HTTP

(defun clickup-view--token ()
  "The personal API token, or a `user-error' saying where to put it."
  (let ((tok (getenv "CLICKUP_API_TOKEN")))
    (if (and tok (not (string-empty-p tok)))
        tok
      (user-error "CLICKUP_API_TOKEN is not set in Emacs (GUI Emacs does not read shell rc files; setenv it in settings.el)"))))

(defun clickup-view--headers ()
  `(("Accept" . "application/json")
    ("Authorization" . ,(clickup-view--token))))

(defun clickup-view--url (path &optional query)
  "API URL for PATH plus QUERY, an alist of (KEY . VALUE).
A nil VALUE drops the pair, a list VALUE repeats the key, t and
`:json-false' become true and false."
  (let ((pairs
         (mapcan (lambda (kv)
                   (let ((k (url-hexify-string (format "%s" (car kv))))
                         (v (cdr kv)))
                     (mapcar (lambda (x)
                               (concat k "=" (url-hexify-string
                                              (pcase x
                                                ('t "true")
                                                (:json-false "false")
                                                (_ (format "%s" x))))))
                             (cond ((null v) nil)
                                   ((listp v) v)
                                   (t (list v))))))
                 query)))
    (concat clickup-view--api path
            (if pairs (concat "?" (string-join pairs "&")) ""))))

(defun clickup-view--http (url headers callback &optional method json-body)
  "Request URL; see `xwapp-http'.  Kept as the seam tests replace."
  (xwapp-http url headers callback method json-body))

(defun clickup-view--api-error-fields (body)
  "ClickUp's (ERR . ECODE) from an error BODY {\"err\":..., \"ECODE\":...}."
  (when (and body (not (string-empty-p (string-trim body))))
    (ignore-errors
      (let* ((raw (xwapp-parse-json body))
             (err (alist-get 'err raw)))
        (when (and (stringp err) (not (string-empty-p err)))
          (cons err (alist-get 'ECODE raw)))))))

(defun clickup-view--api-error (body)
  "ClickUp's own error sentence in BODY, with its ECODE."
  (when-let* ((f (clickup-view--api-error-fields body)))
    (if (stringp (cdr f)) (format "%s (%s)" (car f) (cdr f)) (car f))))

(defun clickup-view--error-text (code err body &optional url)
  "One readable line for a failed request."
  (xwapp-scrub-error
   (cond
    ((and err code (< code 300))
     ;; The reply arrived but could not be handled.
     (error-message-string err))
    ;; 401 is not always the token: a task outside the workspace, or
    ;; one that does not exist, is "Team not authorized" (OAUTH_027).
    ((and (eq code 401)
          (equal (cdr (clickup-view--api-error-fields body)) "OAUTH_027"))
     "Not found in your workspace, or no access to it (HTTP 401, OAUTH_027)")
    ((eq code 401)
     (format "ClickUp rejected the token (%s). Check CLICKUP_API_TOKEN."
             (or (clickup-view--api-error body) "HTTP 401")))
    ((and code (clickup-view--api-error body))
     (format "HTTP %s — %s" code (clickup-view--api-error body)))
    (code (format "HTTP %s%s" code (if url (format " · %s" url) "")))
    (err (format "Network error: %s" (error-message-string err)))
    (t "Request failed"))))

(defun clickup-view--show-error (msg)
  "Report MSG in the page, and in the echo area for minibuffer commands."
  (message "ClickUp: %s" msg)
  (clickup-view--js "showError" msg))

(defun clickup-view--fail (code err body &optional url)
  (clickup-view--show-error (clickup-view--error-text code err body url)))

(defun clickup-view--request (method path ok-fn &optional query body fail-fn)
  "METHOD PATH with QUERY and JSON BODY; OK-FN gets the parsed reply.
FAIL-FN defaults to reporting in the page; see `xwapp-json-callback'."
  (let ((url (clickup-view--url path query)))
    (clickup-view--http url (clickup-view--headers)
                        (xwapp-json-callback url ok-fn (or fail-fn #'clickup-view--fail))
                        method body)))

(defun clickup-view--request-sync (method path &optional query body)
  "Like `clickup-view--request', but wait and return the reply.
For minibuffer commands: a url.el callback runs with quitting
inhibited, so prompting from one would make C-g dead.
Signals a `user-error' on failure."
  (let (done reply failure)
    (clickup-view--request method path
                           (lambda (raw) (setq reply raw done t))
                           query body
                           (lambda (&rest args)
                             (setq failure (apply #'clickup-view--error-text args)
                                   done t)))
    (with-timeout (30 (user-error "ClickUp: no reply after 30s"))
      (while (not done)
        (accept-process-output nil 0.05)))
    (when failure (user-error "ClickUp: %s" failure))
    reply))


;;; Normalize

(defun clickup-view--person (u)
  (when u
    `((id . ,(alist-get 'id u))
      (name . ,(or (alist-get 'username u) (alist-get 'email u) ""))
      (initials . ,(or (alist-get 'initials u) ""))
      (color . ,(or (alist-get 'color u) ""))
      (avatar . ,(or (alist-get 'profilePicture u) "")))))

(defun clickup-view--status (s)
  `((name . ,(or (alist-get 'status s) ""))
    (color . ,(or (alist-get 'color s) ""))
    (type . ,(or (alist-get 'type s) ""))
    (order . ,(clickup-view--int (alist-get 'orderindex s)))))

(defun clickup-view--statuses (raw)
  "Normalized statuses of RAW list, in the list's own order."
  (vconcat (sort (mapcar #'clickup-view--status (alist-get 'statuses raw))
                 (lambda (a b) (< (alist-get 'order a) (alist-get 'order b))))))

(defun clickup-view--priority (p)
  (when p
    `((name . ,(or (alist-get 'priority p) ""))
      (color . ,(or (alist-get 'color p) "")))))

(defun clickup-view--row (raw)
  "A task as a row of a list."
  `((id . ,(alist-get 'id raw))
    (name . ,(or (alist-get 'name raw) ""))
    (status . ,(clickup-view--status (alist-get 'status raw)))
    (priority . ,(clickup-view--priority (alist-get 'priority raw)))
    (assignees . ,(clickup-view--vec #'clickup-view--person (alist-get 'assignees raw)))
    (due . ,(or (alist-get 'due_date raw) ""))
    (parent . ,(alist-get 'parent raw))
    (updated . ,(or (alist-get 'date_updated raw) ""))
    (url . ,(or (alist-get 'url raw) ""))))

(defun clickup-view--check-item (raw)
  `((id . ,(alist-get 'id raw))
    (name . ,(or (alist-get 'name raw) ""))
    (resolved . ,(clickup-view--bool (alist-get 'resolved raw)))
    (children . ,(clickup-view--vec #'clickup-view--check-item
                                    (seq-filter #'consp (alist-get 'children raw))))))

(defun clickup-view--checklist (raw)
  `((id . ,(alist-get 'id raw))
    (name . ,(or (alist-get 'name raw) ""))
    (items . ,(clickup-view--vec #'clickup-view--check-item (alist-get 'items raw)))))

(defun clickup-view--attachment (raw)
  `((id . ,(alist-get 'id raw))
    (title . ,(or (alist-get 'title raw) ""))
    (url . ,(or (alist-get 'url raw) ""))
    (thumb . ,(or (alist-get 'thumbnail_medium raw)
                  (alist-get 'thumbnail_small raw) ""))
    (ext . ,(or (alist-get 'extension raw) ""))
    (mimetype . ,(or (alist-get 'mimetype raw) ""))
    (size . ,(clickup-view--int (alist-get 'size raw)))))

(defun clickup-view--tag (raw)
  `((name . ,(or (alist-get 'name raw) ""))
    (fg . ,(or (alist-get 'tag_fg raw) ""))
    (bg . ,(or (alist-get 'tag_bg raw) ""))))

(defun clickup-view--space-by-id (id)
  (seq-find (lambda (s) (equal (alist-get 'id s) id)) clickup-view--spaces))

(defun clickup-view--space-name (id)
  (alist-get 'name (clickup-view--space-by-id id)))

(defun clickup-view--folder-ref (folder)
  "FOLDER as its id and name; nil for a folderless list's hidden folder.
That one is not a folder to the reader: no breadcrumb for it."
  (unless (or (null folder) (clickup-view--truthy (alist-get 'hidden folder)))
    `((id . ,(alist-get 'id folder))
      (name . ,(or (alist-get 'name folder) "")))))

(defun clickup-view--list-info (list-id raw)
  "LIST-ID's name, statuses, space and folder, from GET /list RAW."
  (let ((space (alist-get 'space raw)))
    `((id . ,list-id)
      (name . ,(or (alist-get 'name raw) ""))
      (statuses . ,(clickup-view--statuses raw))
      (space . ((id . ,(alist-get 'id space))
                (name . ,(or (alist-get 'name space)
                             (clickup-view--space-name (alist-get 'id space))
                             ""))))
      (folder . ,(clickup-view--folder-ref (alist-get 'folder raw))))))

(defun clickup-view--task (raw)
  "A task for the detail page."
  (let ((space-id (alist-get 'id (alist-get 'space raw))))
    (append
     (clickup-view--row raw)
     `((custom_id . ,(alist-get 'custom_id raw))
       (description . ,(or (alist-get 'markdown_description raw)
                           (alist-get 'text_content raw)
                           (alist-get 'description raw)
                           ""))
       (creator . ,(clickup-view--person (alist-get 'creator raw)))
       (tags . ,(clickup-view--vec #'clickup-view--tag (alist-get 'tags raw)))
       (start . ,(or (alist-get 'start_date raw) ""))
       (created . ,(or (alist-get 'date_created raw) ""))
       (list . ((id . ,(alist-get 'id (alist-get 'list raw)))
                (name . ,(or (alist-get 'name (alist-get 'list raw)) ""))))
       (folder . ,(clickup-view--folder-ref (alist-get 'folder raw)))
       (space . ((id . ,space-id)
                 (name . ,(or (clickup-view--space-name space-id) ""))))
       (top_parent . ,(alist-get 'top_level_parent raw))
       (checklists . ,(clickup-view--vec #'clickup-view--checklist (alist-get 'checklists raw)))
       (subtasks . ,(clickup-view--vec #'clickup-view--row (alist-get 'subtasks raw)))
       (attachments . ,(clickup-view--vec #'clickup-view--attachment
                                          (alist-get 'attachments raw)))))))

(defun clickup-view--comment (raw)
  `((id . ,(alist-get 'id raw))
    (author . ,(clickup-view--person (alist-get 'user raw)))
    (date . ,(or (alist-get 'date raw) ""))
    (replies . ,(clickup-view--int (alist-get 'reply_count raw)))
    (html . ,(clickup-view--delta-html (alist-get 'comment raw)
                                       (alist-get 'comment_text raw)))))

(defun clickup-view--space (raw)
  `((id . ,(alist-get 'id raw))
    (name . ,(or (alist-get 'name raw) ""))
    (color . ,(or (alist-get 'color raw) ""))))

(defun clickup-view--list (raw)
  `((id . ,(alist-get 'id raw))
    (name . ,(or (alist-get 'name raw) ""))
    (count . ,(clickup-view--int (alist-get 'task_count raw)))))

(defun clickup-view--folder (raw)
  `((id . ,(alist-get 'id raw))
    (name . ,(or (alist-get 'name raw) ""))
    (lists . ,(clickup-view--vec #'clickup-view--list (alist-get 'lists raw)))))


;;; Comment delta → HTML

(defun clickup-view--esc (s)
  "S with HTML specials escaped."
  (replace-regexp-in-string
   "[&<>\"']"
   (lambda (c) (pcase c ("&" "&amp;") ("<" "&lt;") (">" "&gt;")
                      ("\"" "&quot;") ("'" "&#39;")))
   (or s "") t t))

(defun clickup-view--safe-url (url)
  "URL if it is http(s) or mailto, else nil.  No javascript: links."
  (and (stringp url)
       (string-match-p "\\`\\(?:https?://\\|mailto:\\)" url)
       url))

(defun clickup-view--link (url label)
  "An anchor to URL around LABEL (already HTML), or LABEL alone."
  (if-let* ((safe (clickup-view--safe-url url)))
      (format "<a href=\"%s\">%s</a>" (clickup-view--esc safe) label)
    label))

(defun clickup-view--inline (text attrs)
  "TEXT escaped and wrapped in the inline formats of ATTRS."
  (let ((h (clickup-view--esc text)))
    (when (alist-get 'code attrs) (setq h (concat "<code>" h "</code>")))
    (when (alist-get 'bold attrs) (setq h (concat "<strong>" h "</strong>")))
    (when (alist-get 'italic attrs) (setq h (concat "<em>" h "</em>")))
    (when (alist-get 'underline attrs) (setq h (concat "<u>" h "</u>")))
    (when (alist-get 'strike attrs) (setq h (concat "<s>" h "</s>")))
    (if (alist-get 'link attrs)
        (clickup-view--link (alist-get 'link attrs) h)
      h)))

(defun clickup-view--cell-html (cell)
  "A table-embed CELL: its own little delta, under `insert'."
  (string-remove-suffix
   "<br>"
   (mapconcat (lambda (op)
                (let ((ins (alist-get 'insert op)))
                  (if (stringp ins)
                      (mapconcat (lambda (part)
                                   (if (string-empty-p part)
                                       ""
                                     (clickup-view--inline part (alist-get 'attributes op))))
                                 (split-string ins "\n") "<br>")
                    "")))
              (alist-get 'content cell) "")))

(defun clickup-view--table-html (te)
  "A comment table from table-embed TE: cells are keyed \"row:col\"."
  (let ((rows (length (alist-get 'rows te)))
        (cols (length (alist-get 'columns te)))
        (cells (alist-get 'cells te)))
    (concat
     "<table>"
     (mapconcat
      (lambda (r)
        (concat "<tr>"
                (mapconcat
                 (lambda (c)
                   (let ((cell (alist-get (intern (format "%d:%d" r c)) cells)))
                     (format "<td>%s</td>" (clickup-view--cell-html cell))))
                 (number-sequence 1 cols) "")
                "</tr>"))
      (number-sequence 1 rows) "")
     "</table>")))

(defun clickup-view--embed-html (op)
  "HTML for a non-text op that sits inside a line."
  (let ((text (alist-get 'text op)))
    (pcase (alist-get 'type op)
      ("tag"
       (let ((name (or (alist-get 'username (alist-get 'user op)) text "")))
         (format "<span class=\"cu-mention\">@%s</span>"
                 (clickup-view--esc (string-remove-prefix "@" name)))))
      ("task_mention"
       (let ((id (alist-get 'task_id (alist-get 'task_mention op))))
         (if (and (stringp id) (string-match-p "\\`[0-9a-z]+\\'" id))
             (format "<a class=\"cu-task\" data-task=\"%s\" href=\"https://app.clickup.com/t/%s\">%s</a>"
                     id id (clickup-view--esc (if (and text (not (string-empty-p text)))
                                                  text
                                                (concat "#" id))))
           (clickup-view--esc text))))
      ("link_mention"
       (let ((url (alist-get 'url (alist-get 'link_mention op))))
         (clickup-view--link url (clickup-view--esc
                                  (if (and text (not (string-empty-p text))) text (or url ""))))))
      ("view_mention"
       (let ((url (alist-get 'viewUrl (alist-get 'view_mention op))))
         (clickup-view--link url (clickup-view--esc
                                  (if (and text (not (string-empty-p text))) text "ClickUp view")))))
      ("image"
       (let* ((img (alist-get 'image op))
              (src (clickup-view--safe-url (or (alist-get 'thumbnail_large img)
                                               (alist-get 'url img)))))
         (if src
             (format "<img class=\"cu-img\" src=\"%s\" alt=\"%s\">"
                     (clickup-view--esc src)
                     (clickup-view--esc (or (alist-get 'name img) "")))
           "")))
      (_ (clickup-view--esc (if (stringp text) text ""))))))

(defun clickup-view--delta-lines (ops)
  "Split delta OPS into lines: a list of (KIND . HTML).
KIND is the line's block attributes, or `:block' for HTML that stands
on its own (divider, table)."
  (let (lines cur)
    (cl-flet ((end-line (attrs)
                (push (cons (or attrs '()) (apply #'concat (nreverse cur))) lines)
                (setq cur nil)))
      (dolist (op ops)
        (let ((type (or (alist-get 'type op) "text"))
              (attrs (alist-get 'attributes op))
              (text (alist-get 'text op)))
          (pcase type
            ("text"
             (let ((parts (split-string (if (stringp text) text "") "\n")))
               (while parts
                 (let ((part (pop parts)))
                   (unless (string-empty-p part)
                     (push (clickup-view--inline part attrs) cur))
                   ;; Each newline ends a line; Quill keeps the line's
                   ;; block format on the newline's attributes.
                   (when parts (end-line attrs))))))
            ("divider"
             (when cur (end-line nil))
             (push (cons :block "<hr>") lines))
            ("table-embed"
             (when cur (end-line nil))
             (push (cons :block (clickup-view--table-html (alist-get 'table-embed op)))
                   lines))
            (_ (push (clickup-view--embed-html op) cur)))))
      (when cur (end-line nil)))
    (nreverse lines)))

(defun clickup-view--line-kind (attrs)
  (cond
   ((eq attrs :block) 'block)
   ((alist-get 'code-block attrs) 'pre)
   ((member (alist-get 'list attrs) '("ordered")) 'ol)
   ((alist-get 'list attrs) 'ul)
   ((assq 'blockquote attrs) 'quote)
   ((alist-get 'header attrs) 'h)
   (t 'p)))

(defun clickup-view--delta-html (ops &optional fallback)
  "HTML for comment delta OPS; FALLBACK plain text when there are none."
  (if (not (consp ops))
      (mapconcat (lambda (line) (format "<p>%s</p>" (clickup-view--esc line)))
                 (split-string (or fallback "") "\n" t) "")
    (let ((open nil) out)
      (cl-flet ((close ()
                  (when open
                    (push (pcase open ('pre "</code></pre>") ('ol "</ol>")
                                 ('ul "</ul>") ('quote "</blockquote>"))
                          out)
                    (setq open nil))))
        (dolist (line (clickup-view--delta-lines ops))
          (let* ((attrs (car line))
                 (html (cdr line))
                 (kind (clickup-view--line-kind attrs)))
            (unless (eq kind open) (close))
            (pcase kind
              ('block (push html out))
              ('h (push (format "<h%d>%s</h%d>"
                                (min 3 (max 1 (clickup-view--int (alist-get 'header attrs))))
                                html
                                (min 3 (max 1 (clickup-view--int (alist-get 'header attrs)))))
                        out))
              ('p (push (format "<p>%s</p>" (if (string-empty-p html) "<br>" html)) out))
              (_
               (unless open
                 (push (pcase kind ('pre "<pre><code>") ('ol "<ol>") ('ul "<ul>")
                              ('quote "<blockquote>"))
                       out)
                 (setq open kind))
               (push (pcase kind
                       ('pre (concat html "\n"))
                       ('quote (format "<p>%s</p>" html))
                       (_ (let ((list (alist-get 'list attrs))
                                (indent (clickup-view--int (alist-get 'indent attrs))))
                            (format "<li%s%s>%s</li>"
                                    (if (> indent 0) (format " class=\"indent-%d\"" (min indent 6)) "")
                                    (pcase list
                                      ("checked" " data-check=\"done\"")
                                      ("unchecked" " data-check=\"todo\"")
                                      (_ ""))
                                    html))))
                     out)))))
        (close))
      (apply #'concat (nreverse out)))))


;;; Markdown → comment delta
;;
;; A port of the clickup-task-manager skill's markdown_delta.py, kept
;; op-for-op identical (the tests diff against its output): headings,
;; bullet / ordered lists, blockquotes, fenced code, tables, rules,
;; **bold**, `code`, [text](url).  Comments have no table block, so a
;; table becomes one bullet per row.

(defconst clickup-view--md-inline-re
  "\\*\\*\\(.+?\\)\\*\\*\\|`\\([^`]+\\)`\\|\\[\\([^]]+\\)\\](\\([^)[:space:]]+\\))")

(defconst clickup-view--md-table-sep-re
  "\\`|?[[:space:]:|-]+|[[:space:]:|-]*\\'")

(defun clickup-view--md-attrs (&rest kvs)
  "An attributes object from KVS; empty encodes as {} not null."
  (if kvs
      (cl-loop for (k v) on kvs by #'cddr collect (cons k v))
    (make-hash-table)))

(defun clickup-view--md-op (text attrs)
  `((text . ,text) (attributes . ,attrs)))

(defun clickup-view--md-attr-list (attrs)
  (if (hash-table-p attrs) nil attrs))

(defun clickup-view--md-inline (text)
  (let ((pos 0) ops)
    (while (string-match clickup-view--md-inline-re text pos)
      (let ((beg (match-beginning 0))
            (end (match-end 0))
            (bold (match-string 1 text))
            (code (match-string 2 text))
            (label (match-string 3 text))
            (url (match-string 4 text)))
        (when (> beg pos)
          (push (clickup-view--md-op (substring text pos beg) (clickup-view--md-attrs)) ops))
        (cond
         (bold
          ;; Bold may wrap code or a link.
          (dolist (op (clickup-view--md-inline bold))
            (push (clickup-view--md-op
                   (alist-get 'text op)
                   (append (clickup-view--md-attr-list (alist-get 'attributes op))
                           '((bold . t))))
                  ops)))
         (code (push (clickup-view--md-op code '((code . t))) ops))
         (t (push (clickup-view--md-op label `((link . ,url))) ops)))
        (setq pos end)))
    (when (< pos (length text))
      (push (clickup-view--md-op (substring text pos) (clickup-view--md-attrs)) ops))
    (nreverse ops)))

(defun clickup-view--md-line (text &optional block)
  (append (clickup-view--md-inline text)
          (list (clickup-view--md-op "\n" (or block (clickup-view--md-attrs))))))

(defun clickup-view--md-cells (row)
  (mapcar #'string-trim
          (split-string (string-trim (string-trim row) "|+" "|+") "|")))

(defun clickup-view--md-table-row (head row)
  (let* ((first (or (car row) ""))
         (first (if (and (not (string-empty-p first))
                         (not (string-prefix-p "**" first)))
                    (format "**%s**" first)
                  first))
         (rest (string-join
                (cl-loop for h in (cdr head)
                         for c in (cdr row)
                         when (and (not (string-empty-p c))
                                   (not (member c '("—" "-"))))
                         collect (if (string-empty-p h) c (format "%s：%s" h c)))
                "；")))
    (if (and (not (string-empty-p first)) (not (string-empty-p rest)))
        (format "%s　%s" first rest)
      (if (string-empty-p first) rest first))))

(defun clickup-view--md-indent (lead)
  "List depth from leading whitespace LEAD: two columns per level."
  (/ (string-width (replace-regexp-in-string "\t" "  " lead)) 2))

(defun clickup-view--md-lines (markdown)
  "MARKDOWN split like Python's splitlines: no phantom last line."
  (let ((lines (split-string markdown "\r\n\\|[\n\r]")))
    (if (and lines (string-empty-p (car (last lines))))
        (butlast lines)
      lines)))

(defun clickup-view--md-to-delta (markdown)
  "Ops for the `comment' field of a ClickUp comment, from MARKDOWN."
  (let* ((lines (vconcat (clickup-view--md-lines markdown)))
         (n (length lines))
         (i 0)
         ops)
    (cl-flet ((emit (xs) (setq ops (nconc ops xs))))
      (while (< i n)
        (let* ((s (aref lines i))
               (stripped (string-trim s)))
          (cond
           ((string-prefix-p "```" stripped)
            (let ((lang (string-trim (substring stripped 3))))
              (when (string-empty-p lang) (setq lang "plain"))
              (cl-incf i)
              (while (and (< i n)
                          (not (string-prefix-p "```" (string-trim (aref lines i)))))
                (emit (list (clickup-view--md-op (aref lines i) (clickup-view--md-attrs))
                            (clickup-view--md-op
                             "\n" `((code-block . ((code-block . ,lang)))))))
                (cl-incf i))
              (cl-incf i)))
           ((and (string-prefix-p "|" stripped)
                 (< (1+ i) n)
                 (string-match-p clickup-view--md-table-sep-re
                                 (string-trim (aref lines (1+ i)))))
            (let ((head (clickup-view--md-cells s)))
              (cl-incf i 2)
              (while (and (< i n) (string-prefix-p "|" (string-trim (aref lines i))))
                (emit (clickup-view--md-line
                       (clickup-view--md-table-row head (clickup-view--md-cells (aref lines i)))
                       '((list . "bullet"))))
                (cl-incf i))))
           (t
            (cond
             ((string-match-p "\\`\\(?:-\\{3,\\}\\|\\*\\{3,\\}\\|_\\{3,\\}\\)\\'" stripped)
              (emit (list `((type . "divider") (attributes . ,(clickup-view--md-attrs))
                            (divider . t)))))
             ((string-match "\\`\\(#\\{1,6\\}\\)[[:space:]]+\\(.*\\)" s)
              (let ((level (min 3 (length (match-string 1 s))))
                    (body (match-string 2 s)))
                (emit (clickup-view--md-line body `((header . ,level))))))
             ((string-match "\\`\\([[:space:]]*\\)[-*+][[:space:]]+\\(.*\\)" s)
              (let ((indent (clickup-view--md-indent (match-string 1 s)))
                    (body (match-string 2 s)))
                (emit (clickup-view--md-line
                       body `((list . "bullet") ,@(when (> indent 0) `((indent . ,indent))))))))
             ((string-match "\\`\\([[:space:]]*\\)[0-9]+\\.[[:space:]]+\\(.*\\)" s)
              (let ((indent (clickup-view--md-indent (match-string 1 s)))
                    (body (match-string 2 s)))
                (emit (clickup-view--md-line
                       body `((list . "ordered") ,@(when (> indent 0) `((indent . ,indent))))))))
             ((string-match "\\`>[[:space:]]?\\(.*\\)" s)
              (emit (clickup-view--md-line (match-string 1 s)
                                           `((blockquote . ,(make-hash-table))))))
             (t (emit (clickup-view--md-line s))))
            (cl-incf i))))))
    ops))


;;; Bridge

(defun clickup-view--js (fn obj)
  "Call CU.FN with JSON-encoded OBJ in the page."
  (xwapp-js clickup-view--app fn obj))

(defun clickup-view--flash (msg)
  (clickup-view--js "flash" msg))


;;; Fetch

(defun clickup-view--with-context (then)
  "Call THEN once the workspace, the user and the spaces are known."
  (cond
   ((null clickup-view--team)
    (if clickup-view-team-id
        (progn (setq clickup-view--team clickup-view-team-id)
               (clickup-view--with-context then))
      (clickup-view--request
       "GET" "/team"
       (lambda (raw)
         (let ((team (car (alist-get 'teams raw))))
           (unless team (error "This token sees no ClickUp workspace"))
           (setq clickup-view--team (alist-get 'id team))
           (clickup-view--with-context then))))))
   ((null clickup-view--me)
    (clickup-view--request
     "GET" "/user"
     (lambda (raw)
       (setq clickup-view--me (clickup-view--person (alist-get 'user raw)))
       (clickup-view--with-context then))))
   ((null clickup-view--spaces)
    (clickup-view--request
     "GET" (format "/team/%s/space" clickup-view--team)
     (lambda (raw)
       (setq clickup-view--spaces
             (mapcar #'clickup-view--space (alist-get 'spaces raw)))
       (clickup-view--with-context then))
     '((archived . :json-false))))
   (t (funcall then))))

(defun clickup-view--send-context ()
  (clickup-view--js "setContext"
                    `((me . ,clickup-view--me)
                      (team . ,clickup-view--team)
                      (spaces . ,(vconcat clickup-view--spaces)))))

(defun clickup-view--on-ready ()
  (clickup-view--with-context
   (lambda ()
     (clickup-view--send-context)
     (let ((id clickup-view--pending))
       (setq clickup-view--pending nil)
       (if id
           (clickup-view--load-task id)
         (clickup-view--js "showHome" nil))))))

(defun clickup-view--load-spaces ()
  "Fetch the spaces again and send them, with the user, to the page."
  (setq clickup-view--spaces nil)
  (clrhash clickup-view--space-cache)
  (clickup-view--with-context #'clickup-view--send-context))

(defun clickup-view--load-space (space-id &optional force)
  "Send SPACE-ID's folders and folderless lists; cached unless FORCE."
  (let ((cached (gethash space-id clickup-view--space-cache)))
    (if (and cached (not force))
        (clickup-view--js "renderSpace" cached)
      (clickup-view--request
       "GET" (format "/space/%s/folder" space-id)
       (lambda (folders)
         (clickup-view--request
          "GET" (format "/space/%s/list" space-id)
          (lambda (lists)
            (let ((payload `((id . ,space-id)
                             (name . ,(or (clickup-view--space-name space-id) ""))
                             (color . ,(or (alist-get 'color (clickup-view--space-by-id space-id))
                                           ""))
                             (folders . ,(clickup-view--vec #'clickup-view--folder
                                                            (alist-get 'folders folders)))
                             (lists . ,(clickup-view--vec #'clickup-view--list
                                                          (alist-get 'lists lists))))))
              (puthash space-id payload clickup-view--space-cache)
              (clickup-view--js "renderSpace" payload)))
          '((archived . :json-false))))
       '((archived . :json-false))))))

(defun clickup-view--with-list (list-id then)
  "Call THEN with LIST-ID's `clickup-view--list-info', fetched once per session."
  (if-let* ((cached (gethash list-id clickup-view--list-cache)))
      (funcall then cached)
    (clickup-view--request
     "GET" (format "/list/%s" list-id)
     (lambda (raw)
       (funcall then (puthash list-id (clickup-view--list-info list-id raw)
                              clickup-view--list-cache))))))

(defun clickup-view--load-list (intent)
  "Send one page of a list's tasks.  INTENT: id, page, closed, mine.
No flash: the page fetches the pages after the first by itself and
shows its own progress."
  (let* ((list-id (alist-get 'id intent))
         (page (clickup-view--int (alist-get 'page intent)))
         (closed (clickup-view--truthy (alist-get 'closed intent)))
         (mine (clickup-view--truthy (alist-get 'mine intent))))
    (clickup-view--with-list
     list-id
     (lambda (info)
       (clickup-view--request
        "GET" (format "/list/%s/task" list-id)
        (lambda (raw)
          (clickup-view--js
           "renderList"
           `((id . ,list-id)
             (name . ,(alist-get 'name info))
             (statuses . ,(alist-get 'statuses info))
             (space . ,(alist-get 'space info))
             (folder . ,(alist-get 'folder info))
             (page . ,page)
             (last_page . ,(clickup-view--bool (alist-get 'last_page raw)))
             (closed . ,(clickup-view--bool closed))
             (mine . ,(clickup-view--bool mine))
             (tasks . ,(clickup-view--vec #'clickup-view--row (alist-get 'tasks raw))))))
        `((page . ,page)
          (subtasks . t)
          (include_closed . ,(if closed t :json-false))
          ("assignees[]" . ,(and mine (alist-get 'id clickup-view--me)))))))))

(defun clickup-view--load-task (id)
  "Send task ID, then its list's statuses, then its comments."
  (clickup-view--flash "Loading task…")
  (clickup-view--request
   "GET" (format "/task/%s" id)
   (lambda (raw)
     (let* ((task (clickup-view--task raw))
            (list-id (alist-get 'id (alist-get 'list task)))
            (info (gethash list-id clickup-view--list-cache)))
       (clickup-view--js "renderTask"
                         `((task . ,task)
                           (statuses . ,(alist-get 'statuses info))))
       (unless info
         (clickup-view--with-list
          list-id
          (lambda (info)
            (clickup-view--js "setStatuses"
                              `((list_id . ,list-id)
                                (statuses . ,(alist-get 'statuses info)))))))
       (clickup-view--load-comments id)))
   '((include_markdown_description . t)
     (include_subtasks . t))))

(defun clickup-view--load-comments (task-id &optional start start-id)
  "Send TASK-ID's newest comments, or those older than START / START-ID."
  (clickup-view--request
   "GET" (format "/task/%s/comment" task-id)
   (lambda (raw)
     (let ((items (alist-get 'comments raw)))
       (clickup-view--js
        "setComments"
        `((task_id . ,task-id)
          (items . ,(clickup-view--vec #'clickup-view--comment items))
          (has_more . ,(clickup-view--bool (>= (length items) clickup-view--comment-page)))
          (append . ,(clickup-view--bool start))))))
   (when start `((start . ,start) (start_id . ,start-id)))))

(defun clickup-view--load-replies (comment-id)
  (clickup-view--request
   "GET" (format "/comment/%s/reply" comment-id)
   (lambda (raw)
     (clickup-view--js
      "setReplies"
      `((comment_id . ,comment-id)
        (items . ,(clickup-view--vec #'clickup-view--comment (alist-get 'comments raw))))))))


;;; Write

(defun clickup-view--comment-body (markdown)
  `((comment . ,(vconcat (clickup-view--md-to-delta markdown)))
    (notify_all . :json-false)))

(defun clickup-view--change-status (task-id status)
  (clickup-view--request
   "PUT" (format "/task/%s" task-id)
   (lambda (raw)
     (let ((st (clickup-view--status (alist-get 'status raw))))
       (clickup-view--js "statusChanged" `((task_id . ,task-id) (status . ,st)))
       (clickup-view--flash (format "Status → %s" (alist-get 'name st)))))
   nil `((status . ,status))))

(defun clickup-view--post-comment (task-id text)
  (if (string-empty-p (string-trim (or text "")))
      (clickup-view--show-error "Comment is empty.")
    (clickup-view--request
     "POST" (format "/task/%s/comment" task-id)
     (lambda (_)
       (clickup-view--js "posted" `((task_id . ,task-id) (comment_id)))
       (clickup-view--flash "Comment posted")
       (clickup-view--load-comments task-id))
     nil (clickup-view--comment-body text))))

(defun clickup-view--post-reply (task-id comment-id text)
  (if (string-empty-p (string-trim (or text "")))
      (clickup-view--show-error "Reply is empty.")
    (clickup-view--request
     "POST" (format "/comment/%s/reply" comment-id)
     (lambda (_)
       (clickup-view--js "posted" `((task_id . ,task-id) (comment_id . ,comment-id)))
       (clickup-view--flash "Reply posted")
       ;; Only the thread: reloading the comments would drop the older
       ;; pages the page has loaded.  The page counts the replies itself.
       (clickup-view--load-replies comment-id))
     nil (clickup-view--comment-body text))))


;;; Intents

(defun clickup-view--handle-intent (intent)
  "Dispatch INTENT from the page.  Errors are shown there, not lost."
  (condition-case e
      (pcase (alist-get 'op intent)
        ("ready" (clickup-view--on-ready))
        ("refresh-spaces" (clickup-view--load-spaces))
        ("open-space" (clickup-view--load-space (alist-get 'id intent)
                                                (clickup-view--truthy (alist-get 'force intent))))
        ("open-list" (clickup-view--load-list intent))
        ("open-task"
         (clickup-view--load-task
          (or (clickup-view--normalize-id (alist-get 'id intent))
              (user-error "Not a ClickUp task id: %s" (alist-get 'id intent)))))
        ("more-comments" (clickup-view--load-comments (alist-get 'task_id intent)
                                                      (alist-get 'start intent)
                                                      (alist-get 'start_id intent)))
        ("load-replies" (clickup-view--load-replies (alist-get 'comment_id intent)))
        ("set-status" (clickup-view--change-status (alist-get 'task_id intent)
                                                   (alist-get 'status intent)))
        ("post-comment" (clickup-view--post-comment (alist-get 'task_id intent)
                                                    (alist-get 'text intent)))
        ("post-reply" (clickup-view--post-reply (alist-get 'task_id intent)
                                                (alist-get 'comment_id intent)
                                                (alist-get 'text intent)))
        ("open-browser"
         (when-let* ((url (clickup-view--safe-url (alist-get 'url intent))))
           (browse-url url)))
        ("copy-url"
         (when-let* ((url (alist-get 'url intent)))
           (xwapp-copy url)
           (clickup-view--flash "Copied link")))
        (_ nil))
    (error (clickup-view--show-error (xwapp-scrub-error (error-message-string e))))))


;;; Commands

(defun clickup-view--on-kill ()
  (setq clickup-view--pending nil))

(defun clickup-view--open (id)
  "Show the page on task ID, or on the browser when ID is nil."
  (clickup-view--token)
  (setq clickup-view--pending id)
  (setf (xwapp-poll-interval clickup-view--app) clickup-view-poll-interval)
  (xwapp-open clickup-view--app))

;;;###autoload
(defun clickup-view (&optional browse)
  "Open the ClickUp task the current branch references.
Looks at the branch name and its commits for task links and CU-ids;
asks when there are several, shows the browser when there are none.
With prefix arg BROWSE, go straight to the browser."
  (interactive "P")
  (clickup-view--open (unless browse (clickup-view--pick-task-id t))))

;;;###autoload
(defun clickup-view-task (id)
  "Open ClickUp task ID: an id, a task link, or a CU-<id>."
  (interactive (list (clickup-view--read-task-id)))
  (clickup-view--open (or (clickup-view--normalize-id id)
                          (user-error "Not a ClickUp task id: %s" id))))

;;;###autoload
(defun clickup-set-status (&optional id)
  "Change the status of task ID from the minibuffer.
ID defaults to the task the current branch references."
  (interactive)
  (let* ((id (or id (clickup-view--pick-task-id)))
         (raw (clickup-view--request-sync "GET" (format "/task/%s" id)))
         (name (alist-get 'name raw))
         (now (alist-get 'status (alist-get 'status raw)))
         (list-id (alist-get 'id (alist-get 'list raw)))
         (info (or (gethash list-id clickup-view--list-cache)
                   (puthash list-id
                            (clickup-view--list-info
                             list-id (clickup-view--request-sync "GET" (format "/list/%s" list-id)))
                            clickup-view--list-cache)))
         (choices (seq-remove (lambda (s) (equal s now))
                              (mapcar (lambda (s) (alist-get 'name s))
                                      (alist-get 'statuses info))))
         (pick (completing-read (format "%s [%s] → " name now) choices nil t))
         (done (clickup-view--request-sync "PUT" (format "/task/%s" id) nil
                                           `((status . ,pick))))
         (st (clickup-view--status (alist-get 'status done))))
    (clickup-view--js "statusChanged" `((task_id . ,id) (status . ,st)))
    (message "ClickUp: %s  %s → %s" name now (alist-get 'name st))))

(provide 'clickup-view)
;;; clickup-view.el ends here

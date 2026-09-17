;;; pr-view.el --- Bitbucket / GitHub PR viewer in xwidget -*- lexical-binding: t; -*-

;; Author: Jing
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, vc, git
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; GUI Emacs PR list + detail (description + unified diff) inside
;; xwidget-webkit.  Elisp owns HTTP and secrets; the local HTML UI only
;; renders.  Intents travel via document.title ("prview:{json}").
;;
;;   M-x pr-view
;;   M-x bitbucket-pr   ; alias
;;
;; Detects github.com / bitbucket.org from origin, or use
;; `pr-view-github-owner' / `pr-view-bitbucket-workspace'.
;; Bitbucket Cloud: API token (BITBUCKET_TOKEN / Bearer). App passwords
;; are deprecated.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'auth-source)

(declare-function xwidget-webkit-new-session "xwidget")
(declare-function xwidget-webkit-goto-uri "xwidget")
(declare-function xwidget-webkit-execute-script "xwidget")
(declare-function xwidget-webkit-title "xwidget")
(declare-function xwidget-webkit-current-session "xwidget")
(declare-function xwidget-webkit-last-session "xwidget")
(declare-function xwidget-webkit--create-new-session-buffer "xwidget")
(declare-function xwidget-buffer "xwidget")
(declare-function get-buffer-xwidgets "xwidget")
(declare-function xwidget-live-p "xwidget")


;;; Customization

(defgroup pr-view nil
  "Embedded pull-request viewer."
  :group 'vc
  :prefix "pr-view-")

(defcustom pr-view-bitbucket-workspace nil
  "Bitbucket Cloud workspace slug.  Nil means detect from git remote."
  :type '(choice (const nil) string)
  :group 'pr-view)

(defcustom pr-view-bitbucket-repo nil
  "Bitbucket repository slug.  Nil means detect from git remote."
  :type '(choice (const nil) string)
  :group 'pr-view)

(defcustom pr-view-bitbucket-username nil
  "Optional Atlassian account email for auth-source lookup.
Bitbucket Cloud API tokens use Bearer auth, so this is not sent
as a password.  Nil means match any `api.bitbucket.org' entry."
  :type '(choice (const nil) string)
  :group 'pr-view)

(defcustom pr-view-github-owner nil
  "GitHub owner/org.  Nil means detect from git remote."
  :type '(choice (const nil) string)
  :group 'pr-view)

(defcustom pr-view-github-repo nil
  "GitHub repository name.  Nil means detect from git remote."
  :type '(choice (const nil) string)
  :group 'pr-view)

(defcustom pr-view-poll-interval 0.2
  "Seconds between title-intent polls."
  :type 'number
  :group 'pr-view)


;;; State

(defconst pr-view--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this file and the ui/ folder.")

(defconst pr-view--buffer-name "*pr-view*")
(defconst pr-view--intent-prefix "prview:")

(defvar-local pr-view--xw nil)
(defvar pr-view--poll-timer nil)
(defvar pr-view--host nil
  "Plist :kind :owner :repo.")
(defvar pr-view--list-cache nil)
(defvar pr-view--last-url nil)
(defvar pr-view--inflight nil)
(defvar pr-view--last-title nil)
(defvar pr-view--list-states '("OPEN" "MERGED")
  "PR states currently shown in the list (OPEN, MERGED, DECLINED, DRAFT).")
(defvar pr-view--last-comments nil)
(defvar pr-view--last-members nil)


;;; Remote detection

(defun pr-view--git (&rest args)
  "Run git ARGS in the current repo.  Return stdout or nil."
  (when (executable-find "git")
    (let* ((default-directory
            (or (locate-dominating-file default-directory ".git")
                default-directory))
           (buf (generate-new-buffer " *pr-view-git*"))
           code out)
      (unwind-protect
          (progn
            (setq code (apply #'call-process "git" nil buf nil args))
            (setq out (string-trim (with-current-buffer buf (buffer-string))))
            (and (eq code 0) (not (string-empty-p out)) out))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(defun pr-view--git-remote-url ()
  "Return origin URL for the current repo, or nil."
  (pr-view--git "config" "--get" "remote.origin.url"))

(defun pr-view--git-current-branch ()
  (pr-view--git "rev-parse" "--abbrev-ref" "HEAD"))

(defun pr-view--git-default-branch ()
  (let ((head (pr-view--git "symbolic-ref" "--short" "refs/remotes/origin/HEAD")))
    (cond
     ((and head (string-match "\\`origin/\\(.+\\)\\'" head))
      (match-string 1 head))
     (t "main"))))

(defun pr-view--parse-remote (url)
  "Parse git remote URL into (:kind KIND :owner OWNER :repo REPO) or nil.
KIND is `github' or `bitbucket'."
  (when (and url (stringp url))
    (setq url (string-trim url))
    (setq url (replace-regexp-in-string "/+\\'" "" url))
    (setq url (replace-regexp-in-string "\\.git\\'" "" url))
    (cond
     ((string-match "github\\.com[:/]\\([^/]+\\)/\\([^/]+\\)\\'" url)
      (list :kind 'github
            :owner (match-string 1 url)
            :repo (match-string 2 url)))
     ((string-match "bitbucket\\.org[:/]\\([^/]+\\)/\\([^/]+\\)\\'" url)
      (list :kind 'bitbucket
            :owner (match-string 1 url)
            :repo (match-string 2 url))))))

(defun pr-view--resolve-host (&optional kind)
  "Return host plist, preferring explicit custom vars then git remote."
  (let* ((remote (pr-view--parse-remote (pr-view--git-remote-url)))
         (kind (or kind
                   (plist-get remote :kind)
                   (and pr-view-github-owner 'github)
                   (and pr-view-bitbucket-workspace 'bitbucket))))
    (pcase kind
      ('github
       (list :kind 'github
             :owner (or pr-view-github-owner (plist-get remote :owner))
             :repo (or pr-view-github-repo (plist-get remote :repo))))
      ('bitbucket
       (list :kind 'bitbucket
             :owner (or pr-view-bitbucket-workspace (plist-get remote :owner))
             :repo (or pr-view-bitbucket-repo (plist-get remote :repo))))
      (_ nil))))


;;; Auth

(defun pr-view--auth-secret (host &optional user)
  "Return secret from auth-source for HOST, optionally USER."
  (when-let* ((found (car (apply #'auth-source-search
                                 :host host
                                 :max 1
                                 (when user (list :user user)))))
              (sec (plist-get found :secret)))
    (if (functionp sec) (funcall sec) sec)))

(defun pr-view--github-token ()
  "GitHub PAT from env, auth-source, or `gh auth token'."
  (or (let ((e (getenv "GITHUB_TOKEN")))
        (and e (not (string-empty-p e)) e))
      (pr-view--auth-secret "api.github.com")
      (when (executable-find "gh")
        (let ((tok (string-trim
                    (with-output-to-string
                      (with-current-buffer standard-output
                        (call-process "gh" nil t nil "auth" "token"))))))
          (and (not (string-empty-p tok))
               (not (string-match-p "error\\|not logged" tok))
               tok)))))

(defun pr-view--bitbucket-user ()
  "Optional Atlassian email used only to pick an auth-source entry."
  (let ((u (or pr-view-bitbucket-username
               (getenv "BITBUCKET_USER")
               (getenv "BITBUCKET_USERNAME"))))
    (and u (not (string-empty-p u)) u)))

(defun pr-view--bitbucket-token ()
  "Bitbucket Cloud API token (not an App password).
App passwords can no longer be created; use an API token with scopes
`read:repository:bitbucket' and `read:pullrequest:bitbucket'."
  (or (let ((e (or (getenv "BITBUCKET_TOKEN")
                   (getenv "BITBUCKET_API_TOKEN"))))
        (and e (not (string-empty-p e)) e))
      (let ((user (pr-view--bitbucket-user)))
        (or (pr-view--auth-secret "api.bitbucket.org" user)
            (pr-view--auth-secret "bitbucket.org" user)))))

(defun pr-view--headers (host &optional accept)
  "HTTP headers for HOST.  ACCEPT defaults to application/json."
  (let ((accept (or accept "application/json"))
        headers)
    (push (cons "Accept" accept) headers)
    (pcase (plist-get host :kind)
      ('github
       (let ((tok (pr-view--github-token)))
         (unless tok
           (user-error "No GitHub token (auth-source host api.github.com, GITHUB_TOKEN, or gh auth token)"))
         (push (cons "Authorization" (concat "Bearer " tok)) headers)
         (push (cons "X-GitHub-Api-Version" "2022-11-28") headers)))
      ('bitbucket
       (let ((tok (pr-view--bitbucket-token)))
         (unless tok
           (user-error "No Bitbucket API token (BITBUCKET_TOKEN, or auth-source host api.bitbucket.org)"))
         (push (cons "Authorization" (concat "Bearer " tok)) headers))))
    headers))


;;; HTTP

(defun pr-view--url (host path)
  (pcase (plist-get host :kind)
    ('github
     (format "https://api.github.com/repos/%s/%s%s"
             (plist-get host :owner) (plist-get host :repo) path))
    ('bitbucket
     (format "https://api.bitbucket.org/2.0/repositories/%s/%s%s"
             (plist-get host :owner) (plist-get host :repo) path))))

(defun pr-view--absolutize (loc base)
  "Turn possibly-relative LOC into an absolute URL using BASE."
  (cond
   ((and loc (string-match-p "\\`https?://" loc)) loc)
   ((and loc (string-prefix-p "//" loc))
    (concat "https:" loc))
   ((and loc (string-prefix-p "/" loc))
    (let ((u (url-generic-parse-url base)))
      (format "%s://%s%s" (or (url-type u) "https") (url-host u) loc)))
   (t loc)))

(defun pr-view--http (url headers callback &optional method json-body hops)
  "Request URL with HEADERS.  CALLBACK is (lambda (body status-code err)).
METHOD defaults to GET.  JSON-BODY is an Elisp object json-encoded as the body.
3xx redirects are followed with Authorization kept (url.el would drop it)."
  (let ((url-request-method (or method "GET"))
        (url-request-extra-headers
         (let ((h (copy-alist headers)))
           (when json-body
             (push '("Content-Type" . "application/json") h))
           h))
        (url-request-data
         (when json-body
           (encode-coding-string (json-encode json-body) 'utf-8)))
        (url-mime-accept-string (or (cdr (assoc "Accept" headers))
                                    "application/json"))
        (url-max-redirections 0)
        (url-show-status nil)
        (hops (or hops 0)))
    (url-retrieve
     url
     (lambda (status)
       (let ((err (plist-get status :error))
             (buf (current-buffer))
             code body location)
         (unwind-protect
             (progn
               (goto-char (point-min))
               (setq code (and (re-search-forward "^HTTP/[^ ]+ \\([0-9]+\\)" nil t)
                               (string-to-number (match-string 1))))
               (goto-char (point-min))
               (when (re-search-forward "^[Ll]ocation:[ \t]*\\(.*\\)$" nil t)
                 (setq location (string-trim (match-string 1))))
               (goto-char (point-min))
               (when (re-search-forward "\n\n" nil t)
                 (setq body (decode-coding-string
                             (buffer-substring-no-properties (point) (point-max))
                             'utf-8)))
               (if (and location
                        (memq code '(301 302 303 307 308))
                        (< hops 5))
                   (pr-view--http (pr-view--absolutize location url)
                                  headers callback method json-body
                                  (1+ hops))
                 (funcall callback body code err)))
           (when (buffer-live-p buf)
             (kill-buffer buf))))))))

(defun pr-view--http-json (url headers ok-fn &optional method json-body)
  (pr-view--http
   url headers
   (lambda (body code err)
     (cond
      ((and code (>= code 400))
       (pr-view--fail code err body url))
      (err
       (pr-view--fail code err body url))
      (t
       (condition-case e
           (funcall ok-fn (json-parse-string
                           (if (and body (not (string-empty-p (string-trim body))))
                               body "{}")
                           :object-type 'alist
                           :array-type 'list
                           :null-object nil
                           :false-object nil))
         (error
          (setq pr-view--inflight nil)
          (pr-view--js "showError" (error-message-string e)))))))
   method json-body))

(defun pr-view--fail (code err body &optional url)
  (setq pr-view--inflight nil)
  (let ((msg (cond
              ((memq code '(401 403))
               "Auth failed (401/403). Check token scopes (GitHub: repo; Bitbucket: read/write:pullrequest:bitbucket + read:repository:bitbucket).")
              (code (format "HTTP %s%s%s" code
                            (if url (format " · %s" url) "")
                            (if (and body (> (length body) 0))
                                (concat " — " (string-trim
                                               (substring body 0 (min 180 (length body)))))
                              "")))
              (err (format "Network error: %s" (error-message-string err)))
              (t "Request failed"))))
    (pr-view--js "showError" msg)))


;;; Normalize

(defun pr-view--json-bool (x)
  "Encode X as a JSON boolean.  Do not use `:false' — json-encode
turns that keyword into the string \"false\", which is truthy in JS."
  (if (eq x t) t json-false))

(defun pr-view--id-str (id)
  (cond
   ((integerp id) (number-to-string id))
   ((numberp id) (number-to-string (truncate id)))
   (t (format "%s" id))))

(defun pr-view--item (id title author source dest updated url)
  `((id . ,id)
    (title . ,(or title ""))
    (author . ,(or author ""))
    (source . ,(or source ""))
    (destination . ,(or dest ""))
    (updated_on . ,(or updated ""))
    (url . ,(or url ""))))

(defun pr-view--avatar (user)
  (or (alist-get 'href (alist-get 'avatar (alist-get 'links user)))
      (alist-get 'avatar_url user)
      ""))

(defun pr-view--person (user)
  (let ((name (or (alist-get 'display_name user)
                  (alist-get 'nickname user)
                  (alist-get 'login user)
                  "")))
    `((name . ,name)
      (avatar . ,(pr-view--avatar user))
      (uuid . ,(or (alist-get 'uuid user) ""))
      (nickname . ,(or (alist-get 'nickname user)
                       (alist-get 'login user)
                       "")))))

(defun pr-view--reviewers (raw)
  (vconcat
   (mapcar #'pr-view--person
           (or (alist-get 'reviewers raw)
               (alist-get 'requested_reviewers raw)
               '()))))

(defun pr-view--bb-item (raw)
  (let ((author (alist-get 'author raw)))
    (append
     (pr-view--item
      (alist-get 'id raw)
      (alist-get 'title raw)
      (or (alist-get 'display_name author)
          (alist-get 'nickname author))
      (alist-get 'name (alist-get 'branch (alist-get 'source raw)))
      (alist-get 'name (alist-get 'branch (alist-get 'destination raw)))
      (alist-get 'updated_on raw)
      (alist-get 'href (alist-get 'html (alist-get 'links raw))))
     `((state . ,(or (alist-get 'state raw) "OPEN"))
       (draft . ,(pr-view--json-bool (eq (alist-get 'draft raw) t)))
       (author_avatar . ,(pr-view--avatar author))
       (created_on . ,(or (alist-get 'created_on raw) ""))
       (comment_count . ,(or (alist-get 'comment_count raw) 0))
       (reviewers . ,(pr-view--reviewers raw))))))

(defun pr-view--gh-state (raw)
  (cond
   ((alist-get 'merged_at raw) "MERGED")
   ((string-equal (format "%s" (alist-get 'state raw)) "closed") "DECLINED")
   (t "OPEN")))

(defun pr-view--gh-item (raw)
  (let ((user (or (alist-get 'user raw) (alist-get 'author raw))))
    (append
     (pr-view--item
      (alist-get 'number raw)
      (alist-get 'title raw)
      (alist-get 'login user)
      (alist-get 'ref (alist-get 'head raw))
      (alist-get 'ref (alist-get 'base raw))
      (alist-get 'updated_at raw)
      (alist-get 'html_url raw))
     `((state . ,(pr-view--gh-state raw))
       (draft . ,(pr-view--json-bool (eq (alist-get 'draft raw) t)))
       (author_avatar . ,(pr-view--avatar user))
       (created_on . ,(or (alist-get 'created_at raw) ""))
       (comment_count . ,(or (alist-get 'comments raw) 0))
       (reviewers . ,(pr-view--reviewers raw))))))

(defun pr-view--side-hash (side)
  (or (alist-get 'hash (alist-get 'commit side)) ""))

(defun pr-view--bb-detail (raw)
  (let ((author (alist-get 'author raw))
        (source (alist-get 'source raw))
        (dest (alist-get 'destination raw)))
    `((id . ,(alist-get 'id raw))
      (title . ,(or (alist-get 'title raw) ""))
      (description . ,(or (alist-get 'description raw) ""))
      (author . ,(or (alist-get 'display_name author) ""))
      (author_avatar . ,(pr-view--avatar author))
      (source . ,(or (alist-get 'name (alist-get 'branch source)) ""))
      (destination . ,(or (alist-get 'name (alist-get 'branch dest)) ""))
      (source_hash . ,(pr-view--side-hash source))
      (destination_hash . ,(pr-view--side-hash dest))
      (state . ,(or (alist-get 'state raw) ""))
      (draft . ,(pr-view--json-bool (eq (alist-get 'draft raw) t)))
      (created_on . ,(or (alist-get 'created_on raw) ""))
      (updated_on . ,(or (alist-get 'updated_on raw) ""))
      (comment_count . ,(or (alist-get 'comment_count raw) 0))
      (reviewers . ,(pr-view--reviewers raw))
      (mergeable . t)
      (url . ,(or (alist-get 'href (alist-get 'html (alist-get 'links raw))) "")))))

(defun pr-view--gh-detail (raw)
  (let ((user (alist-get 'user raw)))
    `((id . ,(alist-get 'number raw))
      (title . ,(or (alist-get 'title raw) ""))
      (description . ,(or (alist-get 'body raw) ""))
      (author . ,(or (alist-get 'login user) ""))
      (author_avatar . ,(pr-view--avatar user))
      (source . ,(or (alist-get 'ref (alist-get 'head raw)) ""))
      (destination . ,(or (alist-get 'ref (alist-get 'base raw)) ""))
      (source_hash . ,(or (alist-get 'sha (alist-get 'head raw)) ""))
      (destination_hash . ,(or (alist-get 'sha (alist-get 'base raw)) ""))
      (state . ,(pr-view--gh-state raw))
      (draft . ,(pr-view--json-bool (eq (alist-get 'draft raw) t)))
      (created_on . ,(or (alist-get 'created_at raw) ""))
      (updated_on . ,(or (alist-get 'updated_at raw) ""))
      (comment_count . ,(or (alist-get 'comments raw) 0))
      (reviewers . ,(pr-view--reviewers raw))
      (mergeable . ,(not (eq (alist-get 'mergeable raw) :false)))
      (url . ,(or (alist-get 'html_url raw) "")))))


;;; Bridge

(defun pr-view--session ()
  (when-let* ((buf (get-buffer pr-view--buffer-name)))
    (with-current-buffer buf
      (or (and pr-view--xw (xwidget-live-p pr-view--xw) pr-view--xw)
          (car (ignore-errors (get-buffer-xwidgets buf)))
          (ignore-errors (xwidget-webkit-current-session))))))

(defun pr-view--js (fn obj)
  "Call BB.FN with JSON-encoded OBJ in the widget."
  (when-let* ((xw (pr-view--session)))
    (xwidget-webkit-execute-script
     xw
     (format "window.BB && BB[%s](%s);"
             (json-encode fn)
             (json-encode obj)))))

(defun pr-view--parse-intent (title)
  "Parse TITLE of the form prview:{json} or bbpr:{json}.  Return alist or nil."
  (when (and title (stringp title))
    (cond
     ((string-prefix-p pr-view--intent-prefix title)
      (ignore-errors
        (json-parse-string (substring title (length pr-view--intent-prefix))
                           :object-type 'alist
                           :array-type 'list)))
     ((string-prefix-p "bbpr:" title)
      (ignore-errors
        (json-parse-string (substring title 5)
                           :object-type 'alist
                           :array-type 'list))))))

(defun pr-view--clear-title ()
  (when-let* ((xw (pr-view--session)))
    (xwidget-webkit-execute-script xw "document.title = 'PR View';")))

(defun pr-view--handle-intent (intent)
  (let ((op (alist-get 'op intent)))
    (pcase op
      ("ready"
       (setq pr-view--list-states '("OPEN" "MERGED"))
       (pr-view--fetch-list))
      ("refresh-list"
       (setq pr-view--list-states
             (pr-view--normalize-states (alist-get 'states intent)))
       (pr-view--fetch-list))
      ("open-pr" (pr-view--fetch-detail (alist-get 'id intent)))
      ("back-list"
       (if pr-view--list-cache
           (pr-view--js "renderPrList" pr-view--list-cache)
         (pr-view--fetch-list)))
      ("open-browser"
       (let ((url (or (alist-get 'url intent) pr-view--last-url)))
         (when url (browse-url url))))
      ("create-form" (pr-view--fetch-create-form))
      ("create-pr" (pr-view--create-pr intent))
      ("approve-pr" (pr-view--approve-pr (alist-get 'id intent)))
      ("create-comment" (pr-view--create-comment intent))
      ("edit-comment" (pr-view--edit-comment intent))
      ("delete-comment" (pr-view--delete-comment intent))
      ("like-comment" (pr-view--like-comment intent))
      ("add-reviewer" (pr-view--change-reviewer intent 'add))
      ("remove-reviewer" (pr-view--change-reviewer intent 'remove))
      ("merge-pr" (pr-view--merge-pr intent))
      (_ nil))))

(defun pr-view--poll-title ()
  (when (and (get-buffer pr-view--buffer-name)
             (pr-view--session))
    (let* ((xw (pr-view--session))
           (title (ignore-errors (xwidget-webkit-title xw)))
           (intent (pr-view--parse-intent title)))
      (when (and intent (not (equal title pr-view--last-title)))
        (setq pr-view--last-title title)
        (pr-view--clear-title)
        (pr-view--handle-intent intent)))))

(defun pr-view--start-poll ()
  (pr-view--stop-poll)
  (setq pr-view--poll-timer
        (run-at-time 0.4 pr-view-poll-interval #'pr-view--poll-title)))

(defun pr-view--stop-poll ()
  (when (timerp pr-view--poll-timer)
    (cancel-timer pr-view--poll-timer)
    (setq pr-view--poll-timer nil)))


;;; Fetch

(defun pr-view--start-request (msg)
  "Begin an inflight request and show MSG.  Return nil if already busy."
  (unless pr-view--inflight
    (setq pr-view--inflight t)
    (pr-view--js "setStatus" msg)
    t))

(defun pr-view--normalize-states (raw)
  (let ((xs (cond
             ((null raw) '("OPEN"))
             ((vectorp raw) (append raw nil))
             ((stringp raw) (list raw))
             ((listp raw) raw)
             (t '("OPEN")))))
    (or (mapcar (lambda (s) (upcase (format "%s" s))) xs)
        '("OPEN"))))

(defun pr-view--list-path (host states)
  (let ((up (pr-view--normalize-states states)))
    (pcase (plist-get host :kind)
      ('github
       (let ((gh (cond
                  ((and (member "OPEN" up)
                        (not (or (member "MERGED" up)
                                 (member "DECLINED" up)
                                 (member "CLOSED" up))))
                   "open")
                  ((not (member "OPEN" up)) "closed")
                  (t "all"))))
         (format "/pulls?state=%s&per_page=50&sort=updated&direction=desc" gh)))
      ('bitbucket
       (let ((api (delete-dups
                   (mapcar (lambda (s)
                             (pcase s
                               ("DRAFT" "OPEN")
                               ("CLOSED" "DECLINED")
                               (x x)))
                           up))))
         (concat "/pullrequests?"
                 (mapconcat (lambda (s)
                              (concat "state=" (url-hexify-string s)))
                            api "&")
                 "&pagelen=50&sort=-updated_on"
                 "&fields="
                 (url-hexify-string
                  "+values.reviewers,+values.comment_count,+values.draft,+values.author.links.avatar,+values.created_on")))))))

(defun pr-view--item-in-states (item states)
  (let ((st (upcase (format "%s" (or (alist-get 'state item) ""))))
        (draft (eq (alist-get 'draft item) t))
        (up (pr-view--normalize-states states)))
    (or (and draft (member "DRAFT" up))
        (and (equal st "OPEN") (not draft) (member "OPEN" up))
        (and (equal st "MERGED") (member "MERGED" up))
        (and (member st '("DECLINED" "SUPERSEDED" "CLOSED"))
             (or (member "DECLINED" up) (member "CLOSED" up))))))

(defun pr-view--fetch-list ()
  (unless pr-view--host
    (pr-view--js "showError" "No GitHub/Bitbucket remote detected."))
  (when (and pr-view--host (pr-view--start-request "Loading pull requests…"))
    (condition-case e
        (let ((host pr-view--host)
              (states pr-view--list-states)
              (path (pr-view--list-path pr-view--host pr-view--list-states)))
          (pr-view--http-json
           (pr-view--url host path)
           (pr-view--headers host)
           (lambda (raw)
             (setq pr-view--inflight nil)
             (let* ((kind (plist-get host :kind))
                    (rows (pcase kind
                            ('github raw)
                            ('bitbucket (alist-get 'values raw))))
                    (items (seq-filter
                            (lambda (it) (pr-view--item-in-states it states))
                            (mapcar (if (eq kind 'github)
                                        #'pr-view--gh-item
                                      #'pr-view--bb-item)
                                    (or rows '()))))
                    (payload `((forge . ,(symbol-name kind))
                               (owner . ,(plist-get host :owner))
                               (repo . ,(plist-get host :repo))
                               (states . ,(vconcat states))
                               (items . ,(vconcat items)))))
               (setq pr-view--list-cache payload)
               (pr-view--js "renderPrList" payload)))))
      ((error user-error)
       (setq pr-view--inflight nil)
       (pr-view--js "showError" (error-message-string e))))))

(defun pr-view--detail-from (kind raw)
  (pcase kind
    ('github (pr-view--gh-detail raw))
    ('bitbucket (pr-view--bb-detail raw))))

(defun pr-view--bb-diff-path (pr)
  "Direct repo diff path.  Avoid /pullrequests/{id}/diff — that 302s
and Emacs url.el strips Authorization on redirect (private repos 404)."
  (let ((src (or (alist-get 'source_hash pr) (alist-get 'source pr) ""))
        (dst (or (alist-get 'destination_hash pr) (alist-get 'destination pr) "")))
    (when (and (not (string-empty-p src)) (not (string-empty-p dst)))
      (concat "/diff/" (url-hexify-string dst)
              "%2E%2E" (url-hexify-string src)
              "?topic=true"))))

(defun pr-view--diff-url-and-accept (host kind id-str pr)
  (pcase kind
    ('github
     (cons (pr-view--url host (format "/pulls/%s" id-str))
           "application/vnd.github.diff"))
    ('bitbucket
     ;; Official endpoint 302s to /diff/{spec}.  We follow that
     ;; ourselves and keep the Bearer token (needed for merged PRs).
     (cons (pr-view--url host (format "/pullrequests/%s/diff" id-str))
           "*/*"))))

(defun pr-view--bb-comment (raw)
  (unless (eq (alist-get 'deleted raw) t)
    (let ((user (alist-get 'user raw))
          (inline (alist-get 'inline raw)))
      `((id . ,(alist-get 'id raw))
        (author . ,(or (alist-get 'display_name user)
                       (alist-get 'nickname user) ""))
        (avatar . ,(pr-view--avatar user))
        (created_on . ,(or (alist-get 'created_on raw) ""))
        (content . ,(or (alist-get 'raw (alist-get 'content raw)) ""))
        (parent_id . ,(alist-get 'id (alist-get 'parent raw)))
        (inline_path . ,(or (alist-get 'path inline) ""))
        (like_count . 0)))))

(defun pr-view--gh-comment (raw)
  `((id . ,(alist-get 'id raw))
    (author . ,(or (alist-get 'login (alist-get 'user raw)) ""))
    (avatar . ,(pr-view--avatar (alist-get 'user raw)))
    (created_on . ,(or (alist-get 'created_at raw) ""))
    (content . ,(or (alist-get 'body raw) ""))
    (parent_id . nil)
    (inline_path . ,(or (alist-get 'path raw) ""))
    (like_count . ,(or (alist-get '+1 (alist-get 'reactions raw)) 0))))

(defun pr-view--detail-payload (pr forge extra)
  (append
   `((pr . ,pr)
     (forge . ,forge)
     (comments . ,(vconcat (or pr-view--last-comments '()))))
   extra))

(defun pr-view--fetch-comments (host kind id-str)
  (let ((url (pcase kind
               ('github
                (pr-view--url host (format "/issues/%s/comments?per_page=50" id-str)))
               ('bitbucket
                (pr-view--url host (format "/pullrequests/%s/comments?pagelen=50" id-str))))))
    (pr-view--comments-page host kind url nil)))

(defun pr-view--comments-page (host kind url acc)
  (pr-view--http
   url (pr-view--headers host)
   (lambda (body code err)
     (when (and (not err) code (< code 400) body)
       (condition-case nil
           (let* ((raw (json-parse-string body
                                          :object-type 'alist
                                          :array-type 'list
                                          :null-object nil
                                          :false-object nil))
                  (norm (if (eq kind 'github)
                            #'pr-view--gh-comment
                          #'pr-view--bb-comment))
                  (rows (pcase kind
                          ('github raw)
                          ('bitbucket (alist-get 'values raw))))
                  (acc (append acc (delq nil (mapcar norm (or rows '())))))
                  (next (and (eq kind 'bitbucket) (alist-get 'next raw))))
             (if (and next (stringp next) (not (string-empty-p next)))
                 (pr-view--comments-page host kind next acc)
               (setq pr-view--last-comments acc)
               (pr-view--js "setComments" `((items . ,(vconcat acc))))))
         (error nil))))))

(defun pr-view--after-comment (host kind id-str)
  (setq pr-view--inflight nil)
  (pr-view--js "setStatus" "")
  (pr-view--fetch-comments host kind id-str))

(defun pr-view--create-comment (intent)
  (let* ((id (alist-get 'id intent))
         (parent (alist-get 'parent_id intent))
         (text (string-trim (format "%s" (or (alist-get 'content intent) "")))))
    (when (string-empty-p text)
      (pr-view--js "showError" "Comment is empty."))
    (when (and pr-view--host id (not (string-empty-p text))
               (pr-view--start-request "Posting comment…"))
      (condition-case e
          (let* ((host pr-view--host)
                 (kind (plist-get host :kind))
                 (id-str (pr-view--id-str id))
                 (path (pcase kind
                         ('github (format "/issues/%s/comments" id-str))
                         ('bitbucket (format "/pullrequests/%s/comments" id-str))))
                 (body (pcase kind
                         ('github `((body . ,text)))
                         ('bitbucket
                          (let ((b `((content . ((raw . ,text))))))
                            (if parent
                                (append b `((parent . ((id . ,parent)))))
                              b))))))
            (pr-view--http-json
             (pr-view--url host path)
             (pr-view--headers host)
             (lambda (_raw) (pr-view--after-comment host kind id-str))
             "POST" body))
        ((error user-error)
         (setq pr-view--inflight nil)
         (pr-view--js "showError" (error-message-string e)))))))

(defun pr-view--comment-item-url (host kind pr-id comment-id)
  (pcase kind
    ('github (pr-view--url host (format "/issues/comments/%s" comment-id)))
    ('bitbucket (pr-view--url host (format "/pullrequests/%s/comments/%s"
                                          pr-id comment-id)))))

(defun pr-view--edit-comment (intent)
  (let* ((id (alist-get 'id intent))
         (cid (alist-get 'comment_id intent))
         (text (string-trim (format "%s" (or (alist-get 'content intent) "")))))
    (when (or (string-empty-p text) (not cid))
      (pr-view--js "showError" "Missing comment text."))
    (when (and pr-view--host id cid (not (string-empty-p text))
               (pr-view--start-request "Updating comment…"))
      (condition-case e
          (let* ((host pr-view--host)
                 (kind (plist-get host :kind))
                 (id-str (pr-view--id-str id))
                 (body (pcase kind
                         ('github `((body . ,text)))
                         ('bitbucket `((content . ((raw . ,text)))))))
                 (method (pcase kind ('github "PATCH") (_ "PUT"))))
            (pr-view--http-json
             (pr-view--comment-item-url host kind id-str cid)
             (pr-view--headers host)
             (lambda (_raw) (pr-view--after-comment host kind id-str))
             method body))
        ((error user-error)
         (setq pr-view--inflight nil)
         (pr-view--js "showError" (error-message-string e)))))))

(defun pr-view--delete-comment (intent)
  (let ((id (alist-get 'id intent))
        (cid (alist-get 'comment_id intent)))
    (unless cid
      (pr-view--js "showError" "Missing comment id."))
    (when (and pr-view--host id cid (pr-view--start-request "Deleting comment…"))
      (condition-case e
          (let* ((host pr-view--host)
                 (kind (plist-get host :kind))
                 (id-str (pr-view--id-str id)))
            (pr-view--http-json
             (pr-view--comment-item-url host kind id-str cid)
             (pr-view--headers host)
             (lambda (_raw) (pr-view--after-comment host kind id-str))
             "DELETE"))
        ((error user-error)
         (setq pr-view--inflight nil)
         (pr-view--js "showError" (error-message-string e)))))))

(defun pr-view--like-comment (intent)
  (let ((id (alist-get 'id intent))
        (cid (alist-get 'comment_id intent)))
    (unless (and pr-view--host id cid)
      (pr-view--js "showError" "Missing comment id."))
    (when (and pr-view--host id cid (pr-view--start-request "Liking comment…"))
      (condition-case e
          (let* ((host pr-view--host)
                 (kind (plist-get host :kind))
                 (id-str (pr-view--id-str id))
                 (cid-str (pr-view--id-str cid)))
            (pcase kind
              ('github
               (pr-view--http-json
                (pr-view--url host (format "/issues/comments/%s/reactions" cid-str))
                (pr-view--headers host)
                (lambda (_raw) (pr-view--after-comment host kind id-str))
                "POST" '((content . "+1"))))
              ('bitbucket
               ;; Cloud has no public like API; try the internal likes endpoint.
               (pr-view--http-json
                (format "https://api.bitbucket.org/internal/repositories/%s/%s/pullrequests/%s/comments/%s/likes"
                        (plist-get host :owner) (plist-get host :repo)
                        id-str cid-str)
                (pr-view--headers host)
                (lambda (_raw) (pr-view--after-comment host kind id-str))
                "POST"))))
        ((error user-error)
         (setq pr-view--inflight nil)
         (pr-view--js "showError" (error-message-string e)))))))

(defun pr-view--member-person (raw)
  (pr-view--person (or (alist-get 'user raw) raw)))

(defun pr-view--fetch-members (host)
  (let ((kind (plist-get host :kind))
        (url (pcase (plist-get host :kind)
               ('github (pr-view--url host "/collaborators?per_page=100"))
               ('bitbucket
                (format "https://api.bitbucket.org/2.0/workspaces/%s/members?pagelen=100"
                        (plist-get host :owner))))))
    (pr-view--http
     url (pr-view--headers host)
     (lambda (body code err)
       (when (and (not err) code (< code 400) body)
         (condition-case nil
             (let* ((raw (json-parse-string body
                                            :object-type 'alist
                                            :array-type 'list
                                            :null-object nil
                                            :false-object nil))
                    (rows (pcase kind
                            ('github raw)
                            ('bitbucket (alist-get 'values raw))))
                    (people (mapcar #'pr-view--member-person (or rows '()))))
               (setq pr-view--last-members people)
               (pr-view--js "setMembers" `((items . ,(vconcat people)))))
           (error nil)))))))

(defun pr-view--resolve-member (query)
  "Find a cached member whose name/nickname/uuid matches QUERY."
  (let ((q (downcase (string-trim (format "%s" query)))))
    (seq-find
     (lambda (p)
       (or (string-equal q (downcase (format "%s" (or (alist-get 'nickname p) ""))))
           (string-equal q (downcase (format "%s" (or (alist-get 'name p) ""))))
           (string-equal q (downcase (format "%s" (or (alist-get 'uuid p) ""))))))
     pr-view--last-members)))

(defun pr-view--change-reviewer (intent op)
  (let ((id (alist-get 'id intent))
        (who (or (alist-get 'nickname intent)
                 (alist-get 'uuid intent)
                 (alist-get 'name intent))))
    (unless (and pr-view--host id who)
      (pr-view--js "showError" "Missing reviewer."))
    (when (and pr-view--host id who (pr-view--start-request "Updating reviewers…"))
      (condition-case e
          (let* ((host pr-view--host)
                 (kind (plist-get host :kind))
                 (id-str (pr-view--id-str id)))
            (pcase kind
              ('github
               (pr-view--http-json
                (pr-view--url host (format "/pulls/%s/requested_reviewers" id-str))
                (pr-view--headers host)
                (lambda (_raw)
                  (setq pr-view--inflight nil)
                  (pr-view--js "setStatus" "")
                  (pr-view--refresh-reviewers host kind id-str))
                (if (eq op 'remove) "DELETE" "POST")
                `((reviewers . ,(vector (format "%s" who))))))
              ('bitbucket
               (pr-view--bb-put-reviewers host id-str who op))))
        ((error user-error)
         (setq pr-view--inflight nil)
         (pr-view--js "showError" (error-message-string e)))))))

(defun pr-view--refresh-reviewers (host kind id-str)
  (pr-view--http-json
   (pr-view--url host (pcase kind
                        ('github (format "/pulls/%s" id-str))
                        ('bitbucket (format "/pullrequests/%s" id-str))))
   (pr-view--headers host)
   (lambda (raw)
     (let ((pr (pr-view--detail-from kind raw)))
       (pr-view--js "setReviewers"
                    `((items . ,(or (alist-get 'reviewers pr) []))))))))

(defun pr-view--bb-put-reviewers (host id-str who op)
  (pr-view--http-json
   (pr-view--url host (format "/pullrequests/%s" id-str))
   (pr-view--headers host)
   (lambda (cur)
     (let* ((title (alist-get 'title cur))
            (desc (or (alist-get 'description cur) ""))
            (current (mapcar #'pr-view--person (or (alist-get 'reviewers cur) '())))
            (member (pr-view--resolve-member who))
            (uuid (or (and member (alist-get 'uuid member))
                      (and (string-prefix-p "{" (format "%s" who)) (format "%s" who))))
            (next (pcase op
                    ('remove
                     (seq-remove
                      (lambda (p)
                        (or (string-equal (format "%s" (alist-get 'uuid p)) (format "%s" who))
                            (string-equal (downcase (format "%s" (alist-get 'name p)))
                                          (downcase (format "%s" who)))
                            (string-equal (downcase (format "%s" (alist-get 'nickname p)))
                                          (downcase (format "%s" who)))))
                      current))
                    (_
                     (unless uuid
                       (error "Unknown reviewer '%s' (pick from the list, or use a uuid)" who))
                     (if (seq-find (lambda (p) (string-equal (alist-get 'uuid p) uuid)) current)
                         current
                       (append current (list `((uuid . ,uuid) (name . ,who)))))))))
       (when (not (string-equal (format "%s" (alist-get 'state cur)) "OPEN"))
         (error "Bitbucket only allows changing reviewers on an OPEN pull request"))
       (pr-view--http-json
        (pr-view--url host (format "/pullrequests/%s" id-str))
        (pr-view--headers host)
        (lambda (updated)
          (setq pr-view--inflight nil)
          (pr-view--js "setStatus" "")
          (pr-view--js "setReviewers"
                       `((items . ,(pr-view--reviewers updated)))))
        "PUT"
        `((title . ,title)
          (description . ,desc)
          (reviewers . ,(vconcat
                         (mapcar (lambda (p) `((uuid . ,(alist-get 'uuid p))))
                                 next)))))))
   "GET"))

(defun pr-view--fetch-detail (id)
  (unless (and pr-view--host id)
    (pr-view--js "showError" "Missing PR id."))
  (when (and pr-view--host id
             (pr-view--start-request (format "Loading PR #%s…" id)))
    (condition-case e
        (let* ((host pr-view--host)
               (kind (plist-get host :kind))
               (id-str (pr-view--id-str id))
               (detail-path (pcase kind
                              ('github (format "/pulls/%s" id-str))
                              ('bitbucket (format "/pullrequests/%s" id-str)))))
          (pr-view--http-json
           (pr-view--url host detail-path)
           (pr-view--headers host)
           (lambda (raw)
             (let* ((pr (pr-view--detail-from kind raw))
                    (forge (symbol-name kind))
                    (pair (pr-view--diff-url-and-accept host kind id-str pr))
                    (diff-url (car pair))
                    (diff-accept (cdr pair)))
               (setq pr-view--last-url (alist-get 'url pr)
                     pr-view--inflight nil
                     pr-view--last-comments nil)
               (pr-view--js "renderPrDetail"
                            (pr-view--detail-payload
                             pr forge
                             `((diff . "")
                               (diff_loading . t))))
               (pr-view--fetch-comments host kind id-str)
               (pr-view--fetch-members host)
               (pr-view--http
                diff-url
                (pr-view--headers host diff-accept)
                (lambda (diff code err)
                  (cond
                   ((or err (and code (>= code 400)))
                    (pr-view--js "renderPrDetail"
                                 (pr-view--detail-payload
                                  pr forge
                                  `((diff . "")
                                    (diff_error . ,(format "HTTP %s" (or code "?")))))))
                   (t
                    (pr-view--js "renderPrDetail"
                                 (pr-view--detail-payload
                                  pr forge
                                  `((diff . ,(or diff "")))))))))))))
      ((error user-error)
       (setq pr-view--inflight nil)
       (pr-view--js "showError" (error-message-string e))))))

(defun pr-view--branch-names (kind raw)
  (mapcar (lambda (row)
            (or (alist-get 'name row) ""))
          (pcase kind
            ('github raw)
            ('bitbucket (or (alist-get 'values raw) '())))))

(defun pr-view--fetch-create-form ()
  (unless pr-view--host
    (pr-view--js "showError" "No GitHub/Bitbucket remote detected."))
  (when (and pr-view--host (pr-view--start-request "Loading branches…"))
    (condition-case e
        (let ((host pr-view--host)
              (kind (plist-get pr-view--host :kind))
              (path (pcase (plist-get pr-view--host :kind)
                      ('github "/branches?per_page=100")
                      ('bitbucket "/refs/branches?pagelen=100"))))
          (pr-view--http-json
           (pr-view--url host path)
           (pr-view--headers host)
           (lambda (raw)
             (setq pr-view--inflight nil)
             (pr-view--js "renderCreateForm"
                          `((forge . ,(symbol-name kind))
                            (owner . ,(plist-get host :owner))
                            (repo . ,(plist-get host :repo))
                            (source . ,(or (pr-view--git-current-branch) ""))
                            (destination . ,(or (pr-view--git-default-branch) "main"))
                            (branches . ,(vconcat (pr-view--branch-names kind raw))))))))
      ((error user-error)
       (setq pr-view--inflight nil)
       (pr-view--js "showError" (error-message-string e))))))

(defun pr-view--create-payload (kind title desc source dest close)
  (pcase kind
    ('bitbucket
     `((title . ,title)
       (description . ,(or desc ""))
       (source . ((branch . ((name . ,source)))))
       (destination . ((branch . ((name . ,dest)))))
       (close_source_branch . ,(if close t json-false))))
    ('github
     `((title . ,title)
       (body . ,(or desc ""))
       (head . ,source)
       (base . ,dest)))))

(defun pr-view--create-pr (intent)
  (let* ((title (format "%s" (or (alist-get 'title intent) "")))
         (source (format "%s" (or (alist-get 'source intent) "")))
         (dest (format "%s" (or (alist-get 'destination intent) "")))
         (ok (and pr-view--host
                  (not (string-empty-p title))
                  (not (string-empty-p source))
                  (not (string-empty-p dest)))))
    (unless ok
      (pr-view--js "showError" "Title, source branch and destination branch are required."))
    (when (and ok (pr-view--start-request "Creating pull request…"))
      (condition-case e
          (let* ((host pr-view--host)
                 (kind (plist-get host :kind))
                 (path (pcase kind
                         ('github "/pulls")
                         ('bitbucket "/pullrequests")))
                 (body (pr-view--create-payload
                        kind title
                        (or (alist-get 'description intent) "")
                        source dest
                        (not (member (alist-get 'close_source intent)
                                     '(nil :false json-false))))))
            (pr-view--http-json
             (pr-view--url host path)
             (pr-view--headers host)
             (lambda (raw)
               (setq pr-view--inflight nil)
               (let ((id (or (alist-get 'id raw) (alist-get 'number raw))))
                 (if id
                     (pr-view--fetch-detail id)
                   (pr-view--js "showError" "Created, but no PR id in response."))))
             "POST"
             body))
        ((error user-error)
         (setq pr-view--inflight nil)
         (pr-view--js "showError" (error-message-string e)))))))

(defun pr-view--approve-pr (id)
  (unless (and pr-view--host id)
    (pr-view--js "showError" "Missing PR id."))
  (when (and pr-view--host id (pr-view--start-request (format "Approving #%s…" id)))
    (condition-case e
        (let* ((host pr-view--host)
               (kind (plist-get host :kind))
               (id-str (format "%s" id))
               (path (pcase kind
                       ('github (format "/pulls/%s/reviews" id-str))
                       ('bitbucket (format "/pullrequests/%s/approve" id-str))))
               (body (pcase kind
                       ('github '((event . "APPROVE")))
                       (_ nil))))
          (pr-view--http-json
           (pr-view--url host path)
           (pr-view--headers host)
           (lambda (_raw)
             (setq pr-view--inflight nil)
             (pr-view--js "setStatus" (format "Approved #%s" id) )
             (pr-view--fetch-detail id))
           "POST"
           body))
      ((error user-error)
       (setq pr-view--inflight nil)
       (pr-view--js "showError" (error-message-string e))))))

(defun pr-view--merge-strategy (kind raw)
  (let ((s (upcase (format "%s" (or raw "merge_commit")))))
    (pcase kind
      ('github
       (pcase s
         ("SQUASH" "squash")
         ((or "REBASE" "FAST_FORWARD") "rebase")
         (_ "merge")))
      (_
       (pcase s
         ("SQUASH" "squash")
         ((or "REBASE" "FAST_FORWARD") "fast_forward")
         (_ "merge_commit"))))))

(defun pr-view--merge-pr (intent)
  (let ((id (alist-get 'id intent)))
    (unless (and pr-view--host id)
      (pr-view--js "showError" "Missing PR id."))
    (when (and pr-view--host id
               (pr-view--start-request (format "Merging #%s…" id)))
      (condition-case e
          (let* ((host pr-view--host)
                 (kind (plist-get host :kind))
                 (id-str (format "%s" id))
                 (strategy (pr-view--merge-strategy
                            kind (alist-get 'strategy intent)))
                 (close (not (member (alist-get 'close_source intent)
                                     '(nil :false json-false))))
                 (path (pcase kind
                         ('github (format "/pulls/%s/merge" id-str))
                         ('bitbucket (format "/pullrequests/%s/merge" id-str))))
                 (method (pcase kind ('github "PUT") (_ "POST")))
                 (body (pcase kind
                         ('github `((merge_method . ,strategy)))
                         (_ `((type . "pullrequest")
                              (close_source_branch . ,(if close t json-false))
                              (merge_strategy . ,strategy))))))
            (pr-view--http-json
             (pr-view--url host path)
             (pr-view--headers host)
             (lambda (_raw)
               (setq pr-view--inflight nil)
               (pr-view--fetch-detail id))
             method
             body))
        ((error user-error)
         (setq pr-view--inflight nil)
         (pr-view--js "showError" (error-message-string e)))))))


;;; Commands

(defun pr-view--ui-index ()
  (expand-file-name "ui/index.html" pr-view--dir))

(defun pr-view--file-url ()
  (concat "file://" (pr-view--ui-index)))

(defun pr-view--on-kill ()
  (pr-view--stop-poll)
  (setq pr-view--xw nil
        pr-view--inflight nil
        pr-view--last-title nil))

;;;###autoload
(defun pr-view (&optional kind)
  "Open the pull-request viewer for the current repository.
KIND is `github' or `bitbucket'; nil auto-detects from origin."
  (interactive)
  (unless (featurep 'xwidget-internal)
    (user-error "xwidget-webkit required. Rebuild Emacs with xwidgets"))
  (require 'xwidget)
  (unless (file-readable-p (pr-view--ui-index))
    (user-error "Missing UI at %s" (pr-view--ui-index)))
  (let ((host (pr-view--resolve-host kind)))
    (unless (and host (plist-get host :owner) (plist-get host :repo))
      (user-error "Cannot detect GitHub/Bitbucket repo. Set pr-view-github-* or pr-view-bitbucket-*"))
    (setq pr-view--host host
          pr-view--list-cache nil
          pr-view--inflight nil
          pr-view--last-title nil)
    (if-let* ((buf (get-buffer pr-view--buffer-name)))
        (progn
          (pop-to-buffer buf)
          (when-let* ((xw (pr-view--session)))
            (xwidget-webkit-goto-uri xw (pr-view--file-url))))
      (let ((buf (xwidget-webkit--create-new-session-buffer (pr-view--file-url))))
        (switch-to-buffer buf)
        (setq-local xwidget-webkit-buffer-name-format pr-view--buffer-name)
        (rename-buffer pr-view--buffer-name t)
        (setq-local pr-view--xw (or (xwidget-webkit-current-session)
                                    (xwidget-webkit-last-session)))
        (add-hook 'kill-buffer-hook #'pr-view--on-kill nil t)
        (xwidget-webkit-goto-uri pr-view--xw (pr-view--file-url))))
    (pr-view--start-poll)
    (pr-view--js "setStatus" "Loading…")))

;;;###autoload
(defun pr-view-github ()
  "Open PR View forced to GitHub."
  (interactive)
  (pr-view 'github))

;;;###autoload
(defun pr-view-bitbucket ()
  "Open PR View forced to Bitbucket Cloud."
  (interactive)
  (pr-view 'bitbucket))

;;;###autoload
(defalias 'bitbucket-pr #'pr-view-bitbucket)

;;;###autoload
(defun pr-view-open-in-browser ()
  "Open the last viewed PR (or repo PRs page) in the system browser."
  (interactive)
  (if pr-view--last-url
      (browse-url pr-view--last-url)
    (user-error "No PR URL yet — open a pull request first")))

(provide 'pr-view)
;;; pr-view.el ends here

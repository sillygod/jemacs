;;; pr-view.el --- Bitbucket / GitHub PR viewer in xwidget -*- lexical-binding: t; -*-

;; Author: Jing
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (xwapp "0.1.0"))
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
;;
;; With clickup-view installed, a PR also shows the ClickUp tasks it
;; cites, and opens them there.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'auth-source)
(require 'xwapp)


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

(defvar pr-view--host nil
  "Plist :kind :owner :repo.")
(defvar pr-view--list-cache nil)
(defvar pr-view--last-url nil)
(defvar pr-view--inflight nil)
(defvar pr-view--list-states '("OPEN" "MERGED")
  "PR states currently shown in the list (OPEN, MERGED, DECLINED, DRAFT).")
(defvar pr-view--pending-pr nil
  "A PR for the page to open once it is ready, in place of the list.
`pr-view-open-pr' sets it, for another package.")
(defvar pr-view--pending-local nil
  "A checkout root whose changes the page is to show once it is ready.
`pr-view-changes' sets it.")
(defvar pr-view--local nil
  "The checkout root whose changes the page shows, or nil.")
(defvar pr-view--last-comments nil)
(defvar pr-view--last-members nil)
(defvar pr-view--last-detail-raw nil
  "Raw detail JSON of the open PR, kept so a later verdict fetch can
rebuild the reviewer chips without asking for the PR again.")

(defvar pr-view--app
  (xwapp-create :buffer-name pr-view--buffer-name
                :index (expand-file-name "ui/index.html" pr-view--dir)
                :prefixes (list pr-view--intent-prefix "bbpr:")
                :namespace "BB"
                :idle-title "PR View"
                :handler #'pr-view--handle-intent
                :on-kill #'pr-view--on-kill
                :reopen #'pr-view-reopen)
  "The page, buffer and intent channel; see `xwapp'.")


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

(defun pr-view--pr-of-url (url)
  "The pull request URL is, as (HOST . ID), HOST as `pr-view--parse-remote'
gives one; or nil when URL is not a GitHub or Bitbucket pull request."
  (when (and (stringp url)
             (string-match "\\`https://\\(github\\.com\\|bitbucket\\.org\\)/\\([[:alnum:]_][[:alnum:]_.-]*\\)/\\([[:alnum:]_][[:alnum:]_.-]*\\)/\\(?:pull\\|pull-requests\\)/\\([0-9]+\\)\\(?:[/?#].*\\)?\\'"
                           url))
    (let ((kind (if (equal (match-string 1 url) "github.com") 'github 'bitbucket))
          (path (if (string-search "/pull-requests/" url) "pull-requests" "pull")))
      ;; Each forge's own word for it: a github.com/x/y/pull-requests/1 is no PR.
      (when (equal path (if (eq kind 'github) "pull" "pull-requests"))
        (cons (list :kind kind :owner (match-string 2 url) :repo (match-string 3 url))
              (string-to-number (match-string 4 url)))))))

(defun pr-view--repo-of (spec)
  "The repository SPEC, \"KIND/OWNER/REPO\", names, as a host; or nil."
  (when (and (stringp spec)
             (string-match "\\`\\(github\\|bitbucket\\)/\\([[:alnum:]_][[:alnum:]_.-]*\\)/\\([[:alnum:]_][[:alnum:]_.-]*\\)\\'" spec))
    (list :kind (intern (match-string 1 spec))
          :owner (match-string 2 spec) :repo (match-string 3 spec))))

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

(defun pr-view--http (url headers callback &optional method json-body)
  "Request URL; see `xwapp-http'.  Kept as pr-view's seam for tests."
  (xwapp-http url headers callback method json-body))

(defun pr-view--http-json (url headers ok-fn &optional method json-body)
  (pr-view--http url headers
                 (xwapp-json-callback url ok-fn #'pr-view--fail)
                 method json-body))

(defun pr-view--error-message (body)
  "The human sentence out of an API error BODY, if it has one.
Bitbucket answers `{\"error\":{\"message\":...}}\' and GitHub
`{\"message\":...}\'; both are worth more than 180 characters of the
raw JSON, which is what a merge refused for conflicts used to show."
  (when (and body (not (string-empty-p (string-trim body))))
    (ignore-errors
      (let* ((raw (json-parse-string body
                                     :object-type 'alist
                                     :array-type 'list
                                     :null-object nil
                                     :false-object nil))
             (msg (or (alist-get 'message (alist-get 'error raw))
                      (alist-get 'message raw))))
        (and (stringp msg) (not (string-empty-p msg)) (string-trim msg))))))

(defun pr-view--fail (code err body &optional url)
  (setq pr-view--inflight nil)
  (let ((msg (cond
              ((and err code (< code 300))
               ;; The reply arrived but could not be handled.
               (error-message-string err))
              ((memq code '(401 403))
               "Auth failed (401/403). Check token scopes (GitHub: repo; Bitbucket: read/write:pullrequest:bitbucket + read:repository:bitbucket).")
              ((and code (pr-view--error-message body))
               (format "HTTP %s — %s" code (pr-view--error-message body)))
              (code (format "HTTP %s%s%s" code
                            (if url (format " · %s" url) "")
                            (if (and body (> (length body) 0))
                                (concat " — " (string-trim
                                               (substring body 0 (min 180 (length body)))))
                              "")))
              (err (format "Network error: %s" (error-message-string err)))
              (t "Request failed"))))
    (pr-view--js "showError" (xwapp-scrub-error msg))))


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

(defun pr-view--person (user &optional status)
  (let ((name (or (alist-get 'display_name user)
                  (alist-get 'nickname user)
                  (alist-get 'login user)
                  "")))
    `((name . ,name)
      (avatar . ,(pr-view--avatar user))
      (uuid . ,(or (alist-get 'uuid user) ""))
      (nickname . ,(or (alist-get 'nickname user)
                       (alist-get 'login user)
                       ""))
      (status . ,(or status "")))))

(defun pr-view--person-key (user)
  "Stable identity for USER across the reviewer and verdict arrays.
Bitbucket keys on `uuid', GitHub on `login'; neither forge repeats
the other's field, so one lookup covers both."
  (let ((k (or (alist-get 'uuid user)
               (alist-get 'account_id user)
               (alist-get 'login user)
               (alist-get 'nickname user)
               (alist-get 'display_name user))))
    (and k (format "%s" k))))

(defun pr-view--reviewers (raw &optional verdicts)
  "Reviewer list from RAW, each tagged with its verdict from VERDICTS."
  (vconcat
   (mapcar (lambda (u)
             (pr-view--person
              u (cdr (assoc (pr-view--person-key u) verdicts))))
           (or (alist-get 'reviewers raw)
               (alist-get 'requested_reviewers raw)
               '()))))

(defun pr-view--bb-verdicts (raw)
  "Reviewer key -> verdict, read from Bitbucket's `participants'.
The `reviewers' array carries no verdict at all -- who approved is
only ever in the participant entry, which is why the chips used to
look the same whether or not anyone had approved."
  (let ((map nil))
    (dolist (part (or (alist-get 'participants raw) '()))
      (let* ((user (alist-get 'user part))
             (key (pr-view--person-key user))
             (state (downcase (format "%s" (or (alist-get 'state part) ""))))
             (verdict (cond
                       ((eq (alist-get 'approved part) t) "approved")
                       ((string-equal state "changes_requested")
                        "changes_requested")
                       (t nil))))
        (when (and key verdict)
          (setf (alist-get key map nil nil #'equal) verdict))))
    map))

(defun pr-view--gh-verdicts (reviews)
  "Reviewer key -> latest verdict from GitHub /pulls/{id}/reviews.
Reviews come back oldest first, so a later entry wins.  COMMENTED
carries no verdict and must not clear an earlier approval;
DISMISSED does clear one."
  (let ((map nil))
    (dolist (review (or reviews '()))
      (let* ((user (alist-get 'user review))
             (key (pr-view--person-key user))
             (state (upcase (format "%s" (or (alist-get 'state review) ""))))
             (verdict (cond
                       ((string-equal state "APPROVED") "approved")
                       ((string-equal state "CHANGES_REQUESTED")
                        "changes_requested")
                       ((string-equal state "DISMISSED") "")
                       (t nil))))
        (when (and key verdict)
          (setf (alist-get key map nil nil #'equal) verdict))))
    map))

(defun pr-view--gh-reviewers (raw reviews)
  "Reviewer chips for GitHub: still-requested reviewers, plus verdicts.
GitHub drops a reviewer from `requested_reviewers' the moment they
review, so that list on its own can never show an approval -- the
people who already answered have to be added back from REVIEWS."
  (let* ((verdicts (pr-view--gh-verdicts reviews))
         (requested (or (alist-get 'requested_reviewers raw) '()))
         (seen (delq nil (mapcar #'pr-view--person-key requested)))
         (out (mapcar (lambda (u)
                        (pr-view--person
                         u (cdr (assoc (pr-view--person-key u) verdicts))))
                      requested)))
    (dolist (review (or reviews '()))
      (let* ((user (alist-get 'user review))
             (key (pr-view--person-key user))
             (verdict (cdr (assoc key verdicts))))
        (when (and key verdict (not (string-empty-p verdict))
                   (not (member key seen)))
          (push key seen)
          (setq out (append out (list (pr-view--person user verdict)))))))
    (vconcat out)))

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
       (reviewers . ,(pr-view--reviewers raw (pr-view--bb-verdicts raw)))))))

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
      (reviewers . ,(pr-view--reviewers raw (pr-view--bb-verdicts raw)))
      ;; Filled in by `pr-view--fetch-conflicts': the pull request
      ;; itself says nothing about mergeability, and diffstat calls a
      ;; conflicted file plain `modified'.  /conflicts is the endpoint
      ;; that knows.
      (merge_state . "")
      (conflict . ,(pr-view--json-bool nil))
      (conflict_files . [])
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
      ;; `mergeable' is a JSON boolean, and this parser maps both false
      ;; and null to nil -- so the old (not (eq ... :false)) test was
      ;; always true and no PR ever looked unmergeable.  The state is a
      ;; string and survives: "dirty" is GitHub's word for conflicts.
      (conflict_files . [])
      (merge_state . ,(format "%s" (or (alist-get 'mergeable_state raw) "")))
      (conflict . ,(pr-view--json-bool
                    (equal (format "%s" (alist-get 'mergeable_state raw))
                           "dirty")))
      (mergeable . ,(pr-view--json-bool
                     (not (equal (format "%s" (alist-get 'mergeable_state raw))
                                 "dirty"))))
      (url . ,(or (alist-get 'html_url raw) "")))))


;;; Bridge

(defun pr-view--js (fn obj)
  "Call BB.FN with JSON-encoded OBJ in the widget."
  (xwapp-js pr-view--app fn obj))

(defun pr-view--parse-intent (title)
  "Parse TITLE of the form prview:{json} or bbpr:{json}.  Return alist or nil."
  (xwapp-parse-intent title (xwapp-prefixes pr-view--app)))

(defun pr-view--handle-intent (intent)
  (let ((op (alist-get 'op intent)))
    (pcase op
      ("ready"
       (setq pr-view--list-states '("OPEN" "MERGED"))
       ;; A page in a tab says what it showed: one opened on the phone,
       ;; or one coming back after a restart, which Emacs knows nothing of.
       (let ((pr (pr-view--pr-of-url (alist-get 'pr intent)))
             (repo (pr-view--repo-of (alist-get 'repo intent)))
             (local (let ((l (alist-get 'local intent)))
                      (and (stringp l) (not (string-empty-p l)) l))))
         (setq pr-view--local nil)
         (when (or pr repo)
           (setq pr-view--host (or (car pr) repo)
                 pr-view--list-cache nil
                 pr-view--pending-pr (cdr pr)
                 pr-view--pending-local nil))
         (cond
          (local
           (setq pr-view--pending-pr nil pr-view--pending-local nil)
           (pr-view--local-asked local))
          (pr-view--pending-local
           (let ((root pr-view--pending-local))
             (setq pr-view--pending-local nil)
             (pr-view--local-show root)))
          (pr-view--pending-pr
           (let ((id pr-view--pending-pr))
             (setq pr-view--pending-pr nil)
             (pr-view--fetch-detail id)))
          (t (pr-view--fetch-list)))))
      ("local"
       (let ((root (alist-get 'root intent)))
         (if (equal root pr-view--local)
             (pr-view--local-show root)
           (pr-view--local-asked root))))
      ("local-review" (pr-view--local-review intent))
      ("pr-review" (pr-view--pr-review intent))
      ("refresh-list"
       (setq pr-view--list-states
             (pr-view--normalize-states (alist-get 'states intent)))
       (pr-view--fetch-list))
      ("open-pr" (pr-view--fetch-detail (alist-get 'id intent)))
      ("back-list"
       (setq pr-view--local nil)
       (if pr-view--list-cache
           (pr-view--js "renderPrList" pr-view--list-cache)
         (pr-view--fetch-list)))
      ("open-browser"
       (let ((url (or (alist-get 'url intent) pr-view--last-url)))
         (when url (browse-url url))))
      ("copy-url" (pr-view--copy-url (alist-get 'url intent)))
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
      ("open-task" (pr-view--open-task (alist-get 'ref intent)))
      (_ nil))))


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
       (pr-view--js "showError" (xwapp-scrub-error (error-message-string e)))))))

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

(defun pr-view--gh-file-fragment (file)
  "Unified-diff fragment for one GitHub /pulls/{id}/files item."
  (let* ((status (format "%s" (or (alist-get 'status file) "modified")))
         (new (or (alist-get 'filename file) "unknown"))
         (old (or (alist-get 'previous_filename file) new))
         (patch (alist-get 'patch file))
         (lines (list (format "diff --git a/%s b/%s" old new))))
    (pcase status
      ("added"
       (push "new file mode 100644" lines)
       (push "--- /dev/null" lines)
       (push (format "+++ b/%s" new) lines))
      ("removed"
       (push "deleted file mode 100644" lines)
       (push (format "--- a/%s" old) lines)
       (push "+++ /dev/null" lines))
      ("renamed"
       (push (format "rename from %s" old) lines)
       (push (format "rename to %s" new) lines)
       (push (format "--- a/%s" old) lines)
       (push (format "+++ b/%s" new) lines))
      (_
       (push (format "--- a/%s" old) lines)
       (push (format "+++ b/%s" new) lines)))
    (when (and patch (stringp patch) (not (string-empty-p patch)))
      (push (string-trim-right patch) lines))
    (concat (mapconcat #'identity (nreverse lines) "\n") "\n")))

(defun pr-view--gh-files-to-diff (files)
  "Stitch GitHub pull-file alists into a unified diff."
  (mapconcat #'pr-view--gh-file-fragment (or files '()) ""))

(defun pr-view--show-diff (pr forge diff)
  (pr-view--js "renderPrDetail"
               (pr-view--detail-payload pr forge `((diff . ,(or diff ""))))))

(defun pr-view--show-diff-error (pr forge code)
  (pr-view--js "renderPrDetail"
               (pr-view--detail-payload
                pr forge
                `((diff . "")
                  (diff_error . ,(format "HTTP %s" (or code "?")))))))

(defun pr-view--gh-fetch-files-diff (host id-str ok-fn fail-fn)
  "GET /pulls/{id}/files (paginated) and stitch patches.  GitHub 406s
the unified diff on PRs over ~20k lines or 300 files."
  (pr-view--gh-files-pages host id-str 1 nil ok-fn fail-fn))

(defun pr-view--gh-files-pages (host id-str page acc ok-fn fail-fn)
  (pr-view--http
   (pr-view--url host (format "/pulls/%s/files?per_page=100&page=%d" id-str page))
   (pr-view--headers host)
   (lambda (body code err)
     (cond
      ((or err (and code (>= code 400)))
       (funcall fail-fn code))
      (t
       (condition-case nil
           (let* ((rows (json-parse-string
                         (if (and body (not (string-empty-p (string-trim body))))
                             body "[]")
                         :object-type 'alist
                         :array-type 'list
                         :null-object nil
                         :false-object nil))
                  (rows (if (and (listp rows)
                                 (or (null rows)
                                     (consp (car-safe (car-safe rows)))))
                            rows
                          '()))
                  (acc (append acc rows))
                  (n (length rows)))
             (if (and (= n 100) (< page 30))
                 (pr-view--gh-files-pages host id-str (1+ page) acc ok-fn fail-fn)
               (funcall ok-fn (pr-view--gh-files-to-diff acc))))
         (error (funcall fail-fn code))))))))

(defun pr-view--fetch-diff (host kind id-str pr forge)
  (let* ((pair (pr-view--diff-url-and-accept host kind id-str pr))
         (diff-url (car pair))
         (diff-accept (cdr pair)))
    (pr-view--http
     diff-url
     (pr-view--headers host diff-accept)
     (lambda (diff code err)
       (cond
        ;; GitHub refuses application/vnd.github.diff with 406 when the
        ;; PR is too large.  Fall back to per-file patches.
        ((and (eq kind 'github) (memq code '(406 415)))
         (pr-view--gh-fetch-files-diff
          host id-str
          (lambda (stitched) (pr-view--show-diff pr forge stitched))
          (lambda (code2) (pr-view--show-diff-error pr forge code2))))
        ((or err (and code (>= code 400)))
         (pr-view--show-diff-error pr forge code))
        (t
         (pr-view--show-diff pr forge diff)))))))

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
  (let* ((branch (let ((b (alist-get 'source pr))) (and (stringp b) (not (string-empty-p b)) b)))
         (repo (and pr-view--host (plist-get pr-view--host :owner) (plist-get pr-view--host :repo)
                    (format "%s/%s" (plist-get pr-view--host :owner) (plist-get pr-view--host :repo))))
         ;; Who a review of this PR's diff may go to -- the agent on its head
         ;; branch, the project's lead -- for the page to offer; the comments
         ;; go to a herd agent, never to the forge.
         (review `((branch . ,(or branch ""))
                   (repo . ,(or repo ""))
                   (recipients . ,(if branch (pr-view--local-recipients-json nil (list :branch branch :repo repo)) [])))))
    (append
     `((pr . ,pr)
       (forge . ,forge)
       (comments . ,(vconcat (or pr-view--last-comments '())))
       (review . ,review)
       ;; Which agent on the head branch edited each file, for the file tree.
       (editors . ,(if branch (pr-view--local-editors-json nil (list :branch branch :repo repo)) [])))
     extra)))

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

(defun pr-view--commit-raw-name (raw)
  "Author name out of a git ident like \"Jing Ye <jing@example.com>\"."
  (when (stringp raw)
    (let ((name (string-trim (car (split-string raw "<")))))
      (and (not (string-empty-p name)) name))))

(defun pr-view--short-hash (hash)
  (substring hash 0 (min 8 (length hash))))

(defun pr-view--bb-commit (raw)
  (let* ((author (alist-get 'author raw))
         (user (alist-get 'user author))
         (hash (format "%s" (or (alist-get 'hash raw) ""))))
    `((hash . ,hash)
      (short . ,(pr-view--short-hash hash))
      (message . ,(or (alist-get 'message raw) ""))
      (author . ,(or (alist-get 'display_name user)
                     (alist-get 'nickname user)
                     (pr-view--commit-raw-name (alist-get 'raw author))
                     ""))
      (avatar . ,(pr-view--avatar user))
      (date . ,(or (alist-get 'date raw) ""))
      (url . ,(or (alist-get 'href (alist-get 'html (alist-get 'links raw)))
                  "")))))

(defun pr-view--gh-commit (raw)
  (let* ((commit (alist-get 'commit raw))
         (ident (alist-get 'author commit))
         (user (alist-get 'author raw))
         (hash (format "%s" (or (alist-get 'sha raw) ""))))
    `((hash . ,hash)
      (short . ,(pr-view--short-hash hash))
      (message . ,(or (alist-get 'message commit) ""))
      (author . ,(or (alist-get 'login user)
                     (alist-get 'name ident)
                     ""))
      (avatar . ,(pr-view--avatar user))
      (date . ,(or (alist-get 'date ident) ""))
      (url . ,(or (alist-get 'html_url raw) "")))))

(defun pr-view--fetch-commits (host kind id-str)
  (pr-view--commits-page
   host kind id-str
   (pcase kind
     ('github
      (pr-view--url host (format "/pulls/%s/commits?per_page=100" id-str)))
     ('bitbucket
      (pr-view--url host (format "/pullrequests/%s/commits?pagelen=50" id-str))))
   nil))

(defun pr-view--commits-page (host kind id-str url acc)
  "Collect PR ID-STR's commits, newest first, and the tasks they cite.
Failures stay silent: the tab is supplementary, and
`pr-view--http-json' would turn a 403 on it into an error banner
over a detail view that is otherwise fine."
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
                  (norm (if (eq kind 'github)
                            #'pr-view--gh-commit
                          #'pr-view--bb-commit))
                  (acc (append acc (mapcar norm (or rows '()))))
                  (next (and (eq kind 'bitbucket) (alist-get 'next raw))))
             (if (and next (stringp next) (not (string-empty-p next)))
                 (pr-view--commits-page host kind id-str next acc)
               ;; GitHub answers oldest first, Bitbucket newest first.
               (let ((newest-first (if (eq kind 'github) (reverse acc) acc)))
                 (pr-view--js "setCommits" `((items . ,(vconcat newest-first))))
                 (pr-view--cite-tasks id-str (pr-view--commit-sources newest-first)))))
         (error nil))))))

(defun pr-view--bb-conflict (raw)
  "One entry of Bitbucket's /pullrequests/{id}/conflicts."
  `((path . ,(or (alist-get 'path raw) ""))
    (scenario . ,(format "%s" (or (alist-get 'scenario raw) "")))
    (message . ,(or (alist-get 'message raw) ""))))

(defun pr-view--fetch-conflicts (host id-str)
  "Bitbucket only: which files conflict, and why.

The pull request carries no mergeability at all and diffstat reports a
conflicted file as plain `modified\' -- both checked against a PR whose
web UI was badging one.  /conflicts is the endpoint that answers, and
it answers per file: `File modified in both source and destination\'."
  (pr-view--conflicts-page
   host
   (pr-view--url host (format "/pullrequests/%s/conflicts" id-str))
   nil))

(defun pr-view--conflicts-page (host url acc)
  "Collect conflicts.  Silent on failure: a detail view that loaded is
worth more than an error banner about the badge on it."
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
                  (acc (append acc (mapcar #'pr-view--bb-conflict
                                           (or (alist-get 'values raw) '()))))
                  (next (alist-get 'next raw)))
             (if (and next (stringp next) (not (string-empty-p next)))
                 (pr-view--conflicts-page host next acc)
               (pr-view--js
                "setConflicts"
                `((conflict . ,(pr-view--json-bool (and acc t)))
                  (files . ,(vconcat acc))
                  (message . ,(if acc
                                  "This pull request can't be merged. Resolve the conflicts first."
                                ""))))))
         (error nil))))))

(defun pr-view--fetch-reviews (host id-str)
  "GitHub only: fetch verdicts and repaint the reviewer chips.
Bitbucket ships them inside the PR itself; GitHub keeps them in a
separate collection, so the chips arrive a moment after the view."
  (pr-view--http
   (pr-view--url host (format "/pulls/%s/reviews?per_page=100" id-str))
   (pr-view--headers host)
   (lambda (body code err)
     (when (and (not err) code (< code 400) body pr-view--last-detail-raw)
       (condition-case nil
           (let ((reviews (json-parse-string body
                                             :object-type 'alist
                                             :array-type 'list
                                             :null-object nil
                                             :false-object nil)))
             (pr-view--js
              "setReviewers"
              `((items . ,(pr-view--gh-reviewers pr-view--last-detail-raw
                                                 reviews)))))
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
         (pr-view--js "showError" (xwapp-scrub-error (error-message-string e))))))))

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
         (pr-view--js "showError" (xwapp-scrub-error (error-message-string e))))))))

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
         (pr-view--js "showError" (xwapp-scrub-error (error-message-string e))))))))

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
         (pr-view--js "showError" (xwapp-scrub-error (error-message-string e))))))))

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
         (pr-view--js "showError" (xwapp-scrub-error (error-message-string e))))))))

(defun pr-view--refresh-reviewers (host kind id-str)
  (pr-view--http-json
   (pr-view--url host (pcase kind
                        ('github (format "/pulls/%s" id-str))
                        ('bitbucket (format "/pullrequests/%s" id-str))))
   (pr-view--headers host)
   (lambda (raw)
     (setq pr-view--last-detail-raw raw)
     (let ((pr (pr-view--detail-from kind raw)))
       (pr-view--js "setReviewers"
                    `((items . ,(or (alist-get 'reviewers pr) []))))
       ;; GitHub keeps verdicts in a separate collection, so the PR on
       ;; its own would repaint the chips with every approval erased.
       (when (eq kind 'github)
         (pr-view--fetch-reviews host id-str))))))

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
                       `((items . ,(pr-view--reviewers
                                    updated
                                    (pr-view--bb-verdicts updated))))))
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
                    (forge (symbol-name kind)))
               (setq pr-view--last-url (alist-get 'url pr)
                     pr-view--inflight nil
                     pr-view--last-comments nil
                     pr-view--last-detail-raw raw)
               (pr-view--js "renderPrDetail"
                            (pr-view--detail-payload
                             pr forge
                             `((diff . "")
                               (diff_loading . t))))
               ;; Before the commits are asked for: they add to these.
               ;; Demoted: ClickUp is extra and must not stop the rest.
               (with-demoted-errors "pr-view: ClickUp tasks: %S"
                 (pr-view--detail-tasks id-str pr))
               (pr-view--fetch-comments host kind id-str)
               (pr-view--fetch-members host)
               (pr-view--fetch-commits host kind id-str)
               (when (eq kind 'github)
                 (pr-view--fetch-reviews host id-str))
               (when (eq kind 'bitbucket)
                 (pr-view--fetch-conflicts host id-str))
               (pr-view--fetch-diff host kind id-str pr forge)))))
      ((error user-error)
       (setq pr-view--inflight nil)
       (pr-view--js "showError" (xwapp-scrub-error (error-message-string e)))))))

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
       (pr-view--js "showError" (xwapp-scrub-error (error-message-string e)))))))

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
         (pr-view--js "showError" (xwapp-scrub-error (error-message-string e))))))))

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
       (pr-view--js "showError" (xwapp-scrub-error (error-message-string e)))))))

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

(defconst pr-view--conflict-codes '(409 412)
  "HTTP codes a forge uses to refuse a merge it cannot perform.")

(defun pr-view--merge-conflict-p (code message)
  "Non-nil when a refused merge was refused for conflicts.
The code alone is enough on GitHub; Bitbucket answers 400 with the
reason in the message, so the text decides there."
  (or (memq code pr-view--conflict-codes)
      (and message
           (let ((case-fold-search t))
             (string-match-p "conflict\\|not mergeable\\|cannot be merged"
                             message))
           t)))

(defun pr-view--merge-refused (code resp err url)
  "Show why a merge did not happen, and mark the PR if it was conflicts.
A backstop for /conflicts, not a replacement: the forge is the only
authority on whether a merge will actually be allowed, and its refusal
has to survive on screen rather than scroll past in a status line.
There is deliberately no pre-flight merge call -- Bitbucket's merge
endpoint takes no `dry_run\', so asking would be doing."
  (let* ((message (pr-view--error-message resp))
         (conflict (pr-view--merge-conflict-p code message)))
    (when conflict
      (pr-view--js "setConflicts"
                   `((conflict . t)
                     (message . ,(or message "The merge was refused.")))))
    (pr-view--fail code err resp url)))

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
            (pr-view--http
             (pr-view--url host path)
             (let ((h (copy-alist (pr-view--headers host))))
               (push '("Content-Type" . "application/json") h)
               h)
             (lambda (resp code err)
               (setq pr-view--inflight nil)
               (if (and code (< code 400) (not err))
                   (pr-view--fetch-detail id)
                 (pr-view--merge-refused code resp err
                                         (pr-view--url host path))))
             method
             body))
        ((error user-error)
         (setq pr-view--inflight nil)
         (pr-view--js "showError" (xwapp-scrub-error (error-message-string e))))))))


;;; ClickUp tasks
;;
;; With clickup-view installed, a PR shows the ClickUp tasks its title,
;; branch, description and commits cite, each with its status, and a
;; click opens one there.  Without it, a PR looks as it always did.

(declare-function clickup-view-task-refs "clickup-view" (text))
(declare-function clickup-view-task-brief "clickup-view" (id callback))
(declare-function clickup-view-task "clickup-view" (id))

(defconst pr-view--task-lookups 20
  "Most ClickUp tasks looked up for one PR; the rest show their id only.
ClickUp allows 100 requests a minute on most plans, and a release PR
can cite dozens of tasks across its commits.")

(defvar pr-view--tasks nil
  "The open PR's ClickUp tasks as (PR-ID ITEM...), in the order found.
Each ITEM has id, where and loading, then name, status and url, or
error, once looked up.")

(defun pr-view--clickup-p ()
  "Non-nil when clickup-view can be loaded."
  (require 'clickup-view nil t))

(defun pr-view--task-item (id)
  (seq-find (lambda (it) (equal (alist-get 'id it) id)) (cdr pr-view--tasks)))

(defun pr-view--send-tasks ()
  (pr-view--js "setTasks" `((pr_id . ,(car pr-view--tasks))
                            (items . ,(vconcat (cdr pr-view--tasks))))))

(defun pr-view--cite-tasks (pr-id sources)
  "Add the tasks SOURCES cite to PR-ID's, and look up the new ones.
SOURCES is ((TEXT . WHERE) ...); a task keeps the first WHERE it is
found in.  Ignored once another PR is open: commits arrive late."
  (when (and (equal (car pr-view--tasks) pr-id) (pr-view--clickup-p))
    (let ((known (length (cdr pr-view--tasks)))
          new)
      (pcase-dolist (`(,text . ,where) sources)
        (dolist (id (clickup-view-task-refs text))
          (unless (or (pr-view--task-item id) (assoc id new))
            (push (cons id where) new))))
      (when new
        (let ((items (seq-map-indexed
                      (lambda (ref i)
                        `((id . ,(car ref))
                          (where . ,(cdr ref))
                          (loading . ,(pr-view--json-bool
                                       (< (+ known i) pr-view--task-lookups)))))
                      (nreverse new))))
          (setcdr pr-view--tasks (append (cdr pr-view--tasks) items))
          (pr-view--send-tasks)
          (dolist (it items)
            (when (eq (alist-get 'loading it) t)
              (clickup-view-task-brief
               (alist-get 'id it)
               (lambda (brief) (pr-view--task-looked-up pr-id brief))))))))))

(defun pr-view--task-looked-up (pr-id brief)
  "Put BRIEF, from `clickup-view-task-brief', on PR-ID's task."
  (when (equal (car pr-view--tasks) pr-id)
    (let ((id (alist-get 'id brief)))
      (setcdr pr-view--tasks
              (mapcar (lambda (it)
                        (if (equal (alist-get 'id it) id)
                            `((where . ,(alist-get 'where it))
                              (loading . ,json-false)
                              ,@brief)
                          it))
                      (cdr pr-view--tasks)))
      (pr-view--send-tasks))))

(defun pr-view--detail-tasks (pr-id pr)
  "Start PR-ID's tasks afresh from PR's title, branch and description."
  (setq pr-view--tasks (list pr-id))
  (pr-view--cite-tasks pr-id `((,(alist-get 'title pr) . "the title")
                               (,(alist-get 'source pr) . "the branch name")
                               (,(alist-get 'description pr) . "the description"))))

(defun pr-view--commit-sources (newest-first)
  "NEWEST-FIRST commits as sources for `pr-view--cite-tasks', oldest first."
  (mapcar (lambda (c)
            (let ((msg (or (alist-get 'message c) "")))
              (cons msg (format "commit %s: %s" (alist-get 'short c)
                                (car (split-string msg "\n"))))))
          (reverse newest-first)))

(defun pr-view--open-task (ref)
  "Open ClickUp task REF, an id or a task link, in clickup-view.
The browser gets it when clickup-view cannot: not installed, no token,
no xwidgets, or a custom id it does not read."
  (let ((url (cond ((not (stringp ref)) nil)
                   ((string-match-p "\\`https?://app\\.clickup\\.com/" ref) ref)
                   ((string-match-p "\\`[0-9a-z]+\\'" ref)
                    (concat "https://app.clickup.com/t/" ref)))))
    (when url
      (if (pr-view--clickup-p)
          (condition-case nil
              ;; From pr-view's window, which the click came from: a
              ;; click in the xwidget need not have selected it.
              (progn
                (when-let* ((buf (xwapp-buffer pr-view--app))
                            (win (get-buffer-window buf)))
                  (select-window win))
                (clickup-view-task ref))
            (user-error (browse-url url)))
        (browse-url url)))))


;;; Local changes
;;
;; What a checkout has not committed -- an agent's work before it is a
;; commit, let alone a pull request -- read from git here and shown in a
;; pull request's diff view.  The page names the checkout by its root
;; (#local=ROOT), and is shown one opened here with `pr-view-changes', or
;; one another package vouches for -- the herd's checkouts -- and no
;; other: the page's address must not read whatever directory it names.
;; Against HEAD, staged or not; files git does not track yet as new ones,
;; but not those it ignores, nor what is not a plain file.  git's diff
;; drivers and textconv filters are not run.

(defconst pr-view--local-max (* 2 1024 1024)
  "Most characters of a checkout's diff sent to the page; past it, the
files left out are counted.")
(defconst pr-view--local-untracked-max 50
  "Most files git does not track yet that are shown.")
(defconst pr-view--local-file-max (* 128 1024)
  "Bytes of the largest new file shown: a larger one is named, not shown.")
(defconst pr-view--local-timeout 20
  "Seconds git has to say what a checkout changed.")

(defvar pr-view-local-allowed-functions nil
  "Functions of a checkout's root, as `abbreviate-file-name' writes it
with a final slash: non-nil when a page may show what it has not
committed.  One opened here with `pr-view-changes' is shown anyway.")

(defvar pr-view-local-recipients-functions nil
  "Functions of a checkout's root, as for `pr-view-local-allowed-functions',
saying who may be sent a review of what it has not committed: each
returns a list of plists (:id ID :label LABEL :send FN) -- ID a string
the page names it by, LABEL how the page shows it, FN called with the
review's text and returning a sentence saying what it did.  The first
is offered first.  With nobody to send it to, the page takes no
comments.")

(defvar pr-view-local-editors-functions nil
  "Functions of a checkout's root, as for `pr-view-local-recipients-functions'
\(passed the PR plist too when they take it), saying which agent changed
which file shown in a diff.  Each returns a list of plists (:file REL :by
BY): REL a path as the diff's file tree names it, BY a list of (:id ID
:label LABEL) -- the agents that edited that file, ID a string, LABEL how
the page marks it.  Entries for a file are merged across functions, an
agent named once per file.")

(defvar pr-view--local-opened nil
  "Checkout roots opened here with `pr-view-changes'.")
(defun pr-view--local-git (root &rest args)
  "Run git ARGS at checkout ROOT: its output, or nil when it failed."
  (let ((default-directory (file-name-as-directory (expand-file-name root))))
    (with-temp-buffer
      (when (eq 0 (ignore-errors (apply #'call-process "git" nil '(t nil) nil args)))
        (buffer-string)))))

(defun pr-view--local-root (dir)
  "The root of the checkout DIR is in, abbreviated with a final slash;
nil when DIR is in none."
  (when (and (stringp dir) (not (string-empty-p dir))
             (file-directory-p (expand-file-name dir)))
    (when-let* ((top (pr-view--local-git dir "rev-parse" "--show-toplevel")))
      (abbreviate-file-name (file-name-as-directory (string-trim top))))))

(defun pr-view--local-allowed-p (root)
  "Non-nil when the page may show checkout ROOT's changes."
  (and (stringp root)
       (or (member root pr-view--local-opened)
           (run-hook-with-args-until-success 'pr-view-local-allowed-functions root))
       t))

(defun pr-view--local-base (root)
  "What ROOT's changes are against: HEAD, or before its first commit the
empty tree."
  (if (pr-view--local-git root "rev-parse" "--verify" "-q" "HEAD")
      "HEAD"
    (string-trim (or (pr-view--local-git root "hash-object" "-t" "tree" "/dev/null")
                     "4b825dc642cb6eb9a060e54bf8d69288fbee4904"))))

(defun pr-view--local-clean (s)
  "S with a byte that is no UTF-8 as U+FFFD, for JSON."
  (replace-regexp-in-string (rx (any (#x3fff80 . #x3fffff))) "\uFFFD" s t t))

(defun pr-view--local-text (file)
  "FILE's text when it can be shown, else why not: (text . TEXT) or
(why . WHY).  Not a plain file -- a link may point out of the checkout
-- binary, or larger than `pr-view--local-file-max'."
  (let ((size (file-attribute-size (file-attributes file))))
    (cond
     ((or (file-symlink-p file) (not (file-regular-p file)))
      '(why . "not a plain file"))
     ((> (or size 0) pr-view--local-file-max)
      (cons 'why (format "too large to show: %s" (file-size-human-readable size))))
     (t
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally file)
        (if (search-forward "\0" nil t)
            '(why . "binary")
          (cons 'text (pr-view--local-clean
                       (decode-coding-string (buffer-string) 'utf-8)))))))))

(defun pr-view--local-untracked (root)
  "The files at ROOT git does not track yet and does not ignore, as
\(FILES . LEFT-OUT): FILES the first `pr-view--local-untracked-max',
each (PATH . TEXT-OR-WHY) as `pr-view--local-text' gives it."
  (let* ((out (pr-view--local-git root "-c" "core.quotePath=false"
                                  "ls-files" "--others" "--exclude-standard" "-z"))
         (paths (split-string (or out "") "\0" t)))
    (cons (mapcar (lambda (path)
                    (cons path (pr-view--local-text
                                (expand-file-name path (expand-file-name root)))))
                  (seq-take paths pr-view--local-untracked-max))
          (max 0 (- (length paths) pr-view--local-untracked-max)))))

(defun pr-view--local-lines (text)
  "TEXT's lines, without the empty one after a final newline."
  (let ((lines (split-string text "\n")))
    (if (string-suffix-p "\n" text) (butlast lines) lines)))

(defun pr-view--local-new-file (path text)
  "A diff that adds PATH with TEXT; nil TEXT, a file not shown."
  (let ((head (format "diff --git a/%s b/%s\nnew file mode 100644\n" path path)))
    (if (or (null text) (string-empty-p text))
        head
      (let ((lines (pr-view--local-lines text)))
        (concat head "--- /dev/null\n+++ b/" path "\n"
                (format "@@ -0,0 +1,%d @@\n" (length lines))
                (mapconcat (lambda (l) (concat "+" l "\n")) lines "")
                (unless (string-suffix-p "\n" text) "\\ No newline at end of file\n"))))))

(defun pr-view--local-files (diff)
  "How many files DIFF changes."
  (let ((n 0) (i 0))
    (while (string-match "^diff --git " diff i)
      (setq n (1+ n) i (match-end 0)))
    n))

(defun pr-view--local-cap (diff)
  "DIFF cut at a file's start to `pr-view--local-max', as (DIFF . LEFT-OUT),
LEFT-OUT the files cut."
  (if (<= (length diff) pr-view--local-max)
      (cons diff 0)
    (let ((i 0) at)
      (while (and (setq i (string-search "\ndiff --git " diff i))
                  (< i pr-view--local-max))
        (setq at i i (1+ i)))
      (let ((kept (if at (substring diff 0 (1+ at)) "")))
        (cons kept (pr-view--local-files (substring diff (length kept))))))))

(defun pr-view--local-diff-args (base &rest more)
  "git's arguments for checkout's diff against BASE, then MORE."
  (append (list "-c" "core.quotePath=false" "diff" "--no-color" "--no-ext-diff" "--no-textconv" "-M")
          more (list base "--")))

(defun pr-view--local-payload (root diff err)
  "The page's payload for ROOT: its tracked changes DIFF, as git said them
-- or ERR why not -- and its untracked files."
  (let* ((untracked (pr-view--local-untracked root))
         (notes nil)
         (whole (concat (pr-view--local-clean (or diff ""))
                        (mapconcat (lambda (f)
                                     (when (eq (cadr f) 'why)
                                       (push (cons (car f) (cddr f)) notes))
                                     (pr-view--local-new-file
                                      (car f) (and (eq (cadr f) 'text) (cddr f))))
                                   (car untracked) "")))
         (cap (pr-view--local-cap whole))
         (branch (string-trim (or (pr-view--local-git root "rev-parse" "--abbrev-ref" "HEAD") "")))
         (head (string-trim (or (pr-view--local-git root "log" "-1" "--format=%h %s") "")))
         (host (let ((default-directory (file-name-as-directory (expand-file-name root))))
                 (pr-view--parse-remote (pr-view--git-remote-url)))))
    `((root . ,root)
      (branch . ,branch)
      (head . ,head)
      (forge . ,(if host (symbol-name (plist-get host :kind)) ""))
      (diff . ,(car cap))
      (more . ,(+ (cdr cap) (cdr untracked)))
      (notes . ,(or (nreverse notes) :json-false))
      (diff_error . ,(or err "")))))

(defun pr-view--local-read (root callback)
  "Read what checkout ROOT has not committed, then call CALLBACK with the
page's payload.  git runs apart from Emacs, for as long as
`pr-view--local-timeout'; its output past twice `pr-view--local-max' is
not kept."
  (let* ((default-directory (file-name-as-directory (expand-file-name root)))
         (out (generate-new-buffer " *pr-view-local*"))
         (errs (generate-new-buffer " *pr-view-local-err*"))
         (over nil)
         (timer nil)
         (finish
          (lambda (proc)
            (when (buffer-live-p out)
              (when (timerp timer) (cancel-timer timer))
              (let ((diff (with-current-buffer out (buffer-string)))
                    (why (cond (over nil)
                               ((eq (process-exit-status proc) 0) nil)
                               ((not (eq (process-status proc) 'exit)) "git took too long")
                               (t (or (car (split-string (with-current-buffer errs (buffer-string)) "\n" t))
                                      "git could not say")))))
                (kill-buffer out)
                (when (buffer-live-p errs) (kill-buffer errs))
                (funcall callback (pr-view--local-payload root (and (not why) diff) why))))))
         (proc (make-process
                :name "pr-view-local" :buffer out :stderr errs :noquery t
                :connection-type 'pipe :coding 'utf-8
                :command (cons "git" (pr-view--local-diff-args (pr-view--local-base root)))
                :filter (lambda (p s)
                          (when (buffer-live-p out)
                            (with-current-buffer out
                              (goto-char (point-max))
                              (insert s)
                              (when (> (buffer-size) (* 2 pr-view--local-max))
                                (setq over t)
                                (delete-process p)))))
                :sentinel (lambda (p _event)
                            (unless (process-live-p p) (funcall finish p))))))
    (setq timer (run-at-time pr-view--local-timeout nil
                             (lambda () (when (process-live-p proc) (delete-process proc)))))
    proc))

(defun pr-view--local-show (root)
  "Show checkout ROOT's changes on the page, once git has said them."
  (setq pr-view--local root
        pr-view--inflight nil)
  (if (not (file-directory-p (expand-file-name root)))
      (pr-view--js "showError" (format "No checkout at %s any more" root))
    (pr-view--local-read-for root)))

(defun pr-view--local-read-for (root)
  "Read checkout ROOT's changes for the page."
  (let ((host (let ((default-directory (file-name-as-directory (expand-file-name root))))
                (pr-view--parse-remote (pr-view--git-remote-url)))))
    (when host (setq pr-view--host host pr-view--list-cache nil)))
  (pr-view--js "setStatus" "Reading the changes…")
  (pr-view--local-read
   root (lambda (payload)
          (when (equal pr-view--local root)
            (pr-view--js "renderLocal"
                         (append payload
                                 `((recipients . ,(pr-view--local-recipients-json root))
                                   ;; Which agent here edited each file, for the file tree.
                                   (editors . ,(pr-view--local-editors-json root)))))))))

(defun pr-view--local-asked (root)
  "The page asks for checkout ROOT's changes: show them, if it may."
  (if (pr-view--local-allowed-p root)
      (pr-view--local-show root)
    (pr-view--js "showError" (format "Not a checkout this page may show: %s" root))))

(defun pr-view-local-stat (dir)
  "What the checkout DIR is in has not committed, in numbers, for another
package: (:root ROOT :files N :added A :deleted D :untracked U
:untracked-added UA) as the page counts them -- a file git does not
track yet is one, its lines added; U of the N are those, UA of the A
their lines -- or nil when DIR is in no checkout.  Run as it is asked,
not apart."
  (when-let* ((root (pr-view--local-root dir))
              ;; git runs no textconv for a count; told not to all the same.
           (stat (pr-view--local-git root "-c" "core.quotePath=false" "diff" "--numstat"
                                        "--no-ext-diff" "--no-textconv" "-M"
                                        (pr-view--local-base root) "--")))
    (let ((files 0) (added 0) (deleted 0) (new-lines 0)
          (untracked (pr-view--local-untracked root)))
      (dolist (line (split-string stat "\n" t))
        (when (string-match "\\`\\([0-9]+\\|-\\)\t\\([0-9]+\\|-\\)\t" line)
          (setq files (1+ files)
                added (+ added (string-to-number (match-string 1 line)))
                deleted (+ deleted (string-to-number (match-string 2 line))))))
      (dolist (f (car untracked))
        (when (eq (cadr f) 'text)
          (setq new-lines (+ new-lines (length (pr-view--local-lines (cddr f)))))))
      (let ((new (+ (length (car untracked)) (cdr untracked))))
        (list :root root :files (+ files new) :added (+ added new-lines) :deleted deleted
              :untracked new :untracked-added new-lines)))))

;;;###autoload
(defun pr-view-changes (&optional dir)
  "Show what the checkout DIR is in has not committed, in pr-view's diff
view: against HEAD, staged or not, files git does not track yet as new.
DIR defaults to the current directory."
  (interactive)
  (let ((root (or (pr-view--local-root (or dir default-directory))
                  (user-error "Not in a git checkout: %s" (or dir default-directory)))))
    (cl-pushnew root pr-view--local-opened :test #'equal)
    (setq pr-view--pending-local root
          pr-view--pending-pr nil)
    (setf (xwapp-poll-interval pr-view--app) pr-view-poll-interval)
    (xwapp-open pr-view--app)
    (pr-view--js "setStatus" "Loading…")))

;;; A review of local changes
;;
;; Comments on lines of a checkout's diff, written in the page and sent
;; together, as one text, to one of those another package says may get
;; it -- ghostherd's agents working there.  The page keeps them until
;; they are sent.  Emacs writes the text: each comment at its file and
;; line, with the line as the page showed it, for the agent to find it
;; by once the line has moved.

(defconst pr-view--review-max 50
  "Most comments in one review.")
(defconst pr-view--review-text-max 4000
  "Most characters of a comment, or of the note after them.")
(defconst pr-view--review-code-max 300
  "Most characters of the line a comment quotes.")

(defun pr-view--local-recipients (root &optional pr)
  "Who a review may be sent to, as `pr-view-local-recipients-functions'
say, the well-formed only.  Of checkout ROOT's changes when PR is nil;
with PR, a plist describing a pull request (its head branch and repo),
of that PR -- passed only to a hook function that takes a second argument,
so one written for a checkout still works."
  (let (out)
    (run-hook-wrapped 'pr-view-local-recipients-functions
                      (lambda (f)
                        (let ((res (with-demoted-errors "pr-view: %S"
                                     (if (and pr (let ((m (cdr (func-arity f))))
                                                   (or (eq m 'many) (and (integerp m) (>= m 2)))))
                                         (funcall f root pr)
                                       (funcall f root)))))
                          (setq out (append out res)))
                        nil))
    (seq-filter (lambda (r)
                  (and (stringp (plist-get r :id)) (not (string-empty-p (plist-get r :id)))
                       (stringp (plist-get r :label))
                       (functionp (plist-get r :send))))
                out)))

(defun pr-view--local-recipients-json (root &optional pr)
  "Who a review may be sent to, as the page shows them (see
`pr-view--local-recipients')."
  (vconcat (mapcar (lambda (r) `((id . ,(plist-get r :id)) (label . ,(plist-get r :label))))
                   (pr-view--local-recipients root pr))))

(defun pr-view--local-editors (root &optional pr)
  "Which agent(s) edited each file shown, as `pr-view-local-editors-functions'
say.  Of checkout ROOT's changes when PR is nil; with PR, a plist of a pull
request's head branch and repo, of that PR -- passed only to a function
that takes a second argument, as for `pr-view--local-recipients'.  Each
entry is (:file REL :by ((:id ID :label LABEL) ...))."
  (let (out)
    (run-hook-wrapped 'pr-view-local-editors-functions
                      (lambda (f)
                        (let ((res (with-demoted-errors "pr-view: %S"
                                     (if (and pr (let ((m (cdr (func-arity f))))
                                                   (or (eq m 'many) (and (integerp m) (>= m 2)))))
                                         (funcall f root pr)
                                       (funcall f root)))))
                          (setq out (append out (if (vectorp res) (append res nil) res))))
                        nil))
    out))

(defun pr-view--local-editors-json (root &optional pr)
  "Which agent(s) edited each file, as the page marks them: a vector of
 ((file . REL) (by . [((id . ID) (label . LABEL)) ...])), one entry per
file, the agents merged across functions and named once each, the
ill-formed dropped (see `pr-view--local-editors')."
  (let ((by-file (make-hash-table :test 'equal)) (order nil))
    (dolist (e (pr-view--local-editors root pr))
      (let ((file (plist-get e :file)))
        (when (and (stringp file) (not (string-empty-p file)))
          (dolist (b (let ((by (plist-get e :by))) (if (vectorp by) (append by nil) by)))
            (let ((id (plist-get b :id)) (label (plist-get b :label)))
              (when (and (stringp id) (not (string-empty-p id)) (stringp label))
                (unless (gethash file by-file) (push file order))
                (let ((marks (gethash file by-file)))
                  (unless (seq-find (lambda (m) (equal (plist-get m :id) id)) marks)
                    (puthash file (append marks (list (list :id id :label label))) by-file)))))))))
    (vconcat
     (mapcar (lambda (file)
               `((file . ,file)
                 (by . ,(vconcat (mapcar (lambda (m)
                                           `((id . ,(plist-get m :id)) (label . ,(plist-get m :label))))
                                         (gethash file by-file))))))
             (nreverse order)))))

(defun pr-view--review-cut (s max)
  "S cut to MAX characters, the cut said."
  (if (> (length s) max) (concat (substring s 0 max) "…") s))

(defun pr-view--review-comment (c)
  "Comment C as the page sent it, checked: (PATH SIDE LINE END QUOTED TEXT).
A single line has END equal to LINE and QUOTED one line; a range has END
greater than LINE and QUOTED the lines it covers.  The page sends `end'
and `lines' (the lines, same side) for a range; a single-line comment with
just `code' -- the shape sent before ranges -- still works."
  (let* ((c (and (consp c) c))
         (path (alist-get 'path c))
         (side (alist-get 'side c))
         (line (alist-get 'line c))
         (end (alist-get 'end c))
         (lines (alist-get 'lines c))
         (code (alist-get 'code c))
         (text (alist-get 'text c))
         (end (if (and (natnump end) (natnump line) (> end line)) end line)))
    (unless (and (stringp path) (not (string-empty-p path)) (not (string-match-p "[\n\r]" path))
                 (member side '("new" "old"))
                 (natnump line) (> line 0)
                 (natnump end) (>= end line)
                 (or (null lines) (and (listp lines) (seq-every-p #'stringp lines)))
                 (or (null code) (stringp code))
                 (stringp text) (not (string-empty-p (string-trim text))))
      (user-error "Not a comment on a line: %S" c))
    (list path side line end
          (if (and (> end line) (consp lines))
              (mapcar (lambda (l) (pr-view--review-cut l pr-view--review-code-max)) lines)
            (list (pr-view--review-cut (replace-regexp-in-string "[\n\r]" " " (or code "")) pr-view--review-code-max)))
          (pr-view--review-cut (string-trim text) pr-view--review-text-max))))

(defun pr-view--review-body (comments)
  "The numbered body for COMMENTS (each from `pr-view--review-comment'):
`N. file:line' or `file:start-end' for a range, the quoted line(s), then
the comment's text.  Shared by the checkout and the PR reviews."
  (let ((n 0))
    (mapconcat
     (pcase-lambda (`(,path ,side ,line ,end ,quoted ,text))
       (concat "\n" (format "%d. " (cl-incf n))
               (if (equal side "new")
                   (if (> end line) (format "%s:%d-%d" path line end) (format "%s:%d" path line))
                 (if (> end line)
                     (format "%s, lines %d-%d before, taken out:" path line end)
                   (format "%s, line %d before, taken out:" path line)))
               "\n"
               (mapconcat (lambda (cd) (concat "   " (if (string-blank-p cd) "(an empty line)" (concat "> " cd))))
                          quoted "\n")
               "\n"
               (mapconcat (lambda (l) (if (string-empty-p l) "" (concat "   " l)))
                          (split-string text "\n") "\n")
               "\n"))
     comments "")))

(defun pr-view--review-note (note)
  "NOTE after a review's comments, trimmed and cut, or empty."
  (let ((note (if (stringp note) (string-trim note) "")))
    (if (string-empty-p note) ""
      (concat "\n" (pr-view--review-cut note pr-view--review-text-max) "\n"))))

(defun pr-view--review-checked (comments)
  "COMMENTS checked, as `pr-view--review-comment' makes them; refused when
there are none or too many."
  (let ((out (mapcar #'pr-view--review-comment comments)))
    (unless out (user-error "No comments to send"))
    (when (> (length out) pr-view--review-max)
      (user-error "At most %d comments go in one review" pr-view--review-max))
    out))

(defun pr-view--local-review-text (root comments note)
  "What is sent for COMMENTS on checkout ROOT's changes, NOTE after them.
COMMENTS as the page sends them: each an alist of `path', `side' --
\"new\" for a line the changes have, \"old\" for one they took out --
`line', its number on that side, `code', the line as the page showed it,
`text', and, for a range, `end' and the `lines' it covers."
  (let ((comments (pr-view--review-checked comments))
        (branch (string-trim (or (pr-view--local-git root "rev-parse" "--abbrev-ref" "HEAD") ""))))
    (concat
     (format "Comments on what is not committed in %s%s, each on a line of its diff against HEAD:\n"
             root (if (member branch '("" "HEAD")) "" (format " (branch %s)" branch)))
     (pr-view--review-body comments)
     (pr-view--review-note note))))

(defun pr-view--local-review (intent)
  "Send the review INTENT carries, of a checkout's changes, to the one it
names; the page is told it went, or why not, and who may get the next."
  (let* ((root (alist-get 'root intent))
         (root (and (stringp root) root)))
    (condition-case e
        (progn
          (unless (pr-view--local-allowed-p root)
            (user-error "Not a checkout this page may show: %s" root))
          (let* ((to (alist-get 'to intent))
                 (who (or (seq-find (lambda (r) (equal (plist-get r :id) to))
                                    (pr-view--local-recipients root))
                          (user-error "%s cannot be sent it now" to)))
                 (text (pr-view--local-review-text root (alist-get 'comments intent)
                                                   (alist-get 'note intent)))
                 (said (funcall (plist-get who :send) text)))
            (pr-view--js "reviewSent"
                         `((root . ,root)
                           (said . ,(if (stringp said) said (format "Sent to %s" (plist-get who :label))))
                           (recipients . ,(pr-view--local-recipients-json root))))))
      (error
       (pr-view--js "reviewFailed"
                    `((root . ,root)
                      (error . ,(xwapp-scrub-error (error-message-string e)))
                      (recipients . ,(if (pr-view--local-allowed-p root)
                                         (pr-view--local-recipients-json root)
                                       []))))))))

(defun pr-view--pr-review-text (branch comments note)
  "What is sent for COMMENTS on a pull request's diff (head BRANCH), NOTE
after them -- the same body and note as a checkout review, a header that
names the pull request's branch."
  (concat
   (format "Comments on the pull request%s, each on a line of its diff:\n"
           (if (and (stringp branch) (not (string-empty-p branch))) (format " (branch %s)" branch) ""))
   (pr-view--review-body (pr-view--review-checked comments))
   (pr-view--review-note note)))

(defun pr-view--pr-review (intent)
  "Send the review INTENT carries, of a pull request's diff, to the one it
names -- an agent on the PR's head branch, its project's lead, or a lead to
start, from the same recipients machinery.  Never to the forge: the
comments go to a herd agent only.  The page is told it went, or why not."
  (let* ((branch (let ((b (alist-get 'branch intent))) (and (stringp b) b)))
         (repo (let ((r (alist-get 'repo intent))) (and (stringp r) r)))
         (pr (list :branch branch :repo repo)))
    (condition-case e
        (let* ((to (alist-get 'to intent))
               (who (or (seq-find (lambda (r) (equal (plist-get r :id) to))
                                  (pr-view--local-recipients nil pr))
                        (user-error "%s cannot be sent it now" to)))
               (text (pr-view--pr-review-text branch (alist-get 'comments intent) (alist-get 'note intent)))
               (said (funcall (plist-get who :send) text)))
          (pr-view--js "reviewSent"
                       `((pr . t)
                         (said . ,(if (stringp said) said (format "Sent to %s" (plist-get who :label))))
                         (recipients . ,(pr-view--local-recipients-json nil pr)))))
      (error
       (pr-view--js "reviewFailed"
                    `((pr . t)
                      (error . ,(xwapp-scrub-error (error-message-string e)))
                      (recipients . ,(pr-view--local-recipients-json nil pr))))))))


;;; For other packages
;;
;; Another package -- ghostherd's herd page -- shows the PR an agent's
;; branch is on, and opens it here.  The lookup goes to DIR's own
;; remote, not to `pr-view-github-owner' and its kind: those name the
;; repository pr-view itself opens, which need not be DIR's.

(defun pr-view--branch-pr-path (host branch)
  "The query for BRANCH's pull requests on HOST, newest first."
  (pcase (plist-get host :kind)
    ('github
     (format "/pulls?state=all&per_page=10&head=%s"
             (url-hexify-string (concat (plist-get host :owner) ":" branch))))
    ('bitbucket
     (concat "/pullrequests?state=OPEN&state=MERGED&state=DECLINED&pagelen=10&sort=-updated_on&q="
             (url-hexify-string (format "source.branch.name=\"%s\"" branch))))))

(defun pr-view--branch-pr-pick (host branch raw)
  "Of RAW's pull requests, the one to show for BRANCH: an open one, else
the newest.  As `pr-view-branch-pr' gives it, or nil."
  (let* ((rows (if (eq (plist-get host :kind) 'github)
                   (and (listp raw) raw)
                 (alist-get 'values raw)))
         (rows (seq-filter
                (lambda (r) (equal (if (eq (plist-get host :kind) 'github)
                                       (alist-get 'ref (alist-get 'head r))
                                     (alist-get 'name (alist-get 'branch (alist-get 'source r))))
                                   branch))
                rows))
         (item (lambda (r) (if (eq (plist-get host :kind) 'github) (pr-view--gh-item r) (pr-view--bb-item r))))
         (raw-pr (or (seq-find (lambda (r) (equal (alist-get 'state (funcall item r)) "OPEN")) rows)
                     (car rows))))
    (when raw-pr
      (pr-view--brief host raw-pr))))

(defun pr-view--brief (host raw)
  "HOST's pull request RAW, a list row or its detail, as another package
gets it: id, title, state, draft, url, source, and the ClickUp tasks its
title, branch and description cite."
  (let* ((it (if (eq (plist-get host :kind) 'github) (pr-view--gh-item raw) (pr-view--bb-item raw)))
         (description (or (alist-get 'description raw) (alist-get 'body raw) "")))
    `((id . ,(alist-get 'id it))
      (title . ,(alist-get 'title it))
      (state . ,(alist-get 'state it))
      (draft . ,(alist-get 'draft it))
      (url . ,(alist-get 'url it))
      (source . ,(alist-get 'source it))
      (tasks . ,(and (require 'clickup-view nil t)
                     (clickup-view-task-refs
                      (string-join (list (alist-get 'title it) (alist-get 'source it) description)
                                   "\n")))))))

(defun pr-view--brief-failed (callback)
  "A fail function for `xwapp-json-callback' that tells CALLBACK why."
  (lambda (code err _body &optional _url)
    (funcall callback
             `((error . ,(cond ((memq code '(401 403)) "the forge refused the token")
                               ((eq code 404) "no such pull request")
                               (code (format "HTTP %s" code))
                               (err (error-message-string err))
                               (t "request failed")))))))

(defun pr-view-remote (dir)
  "The forge repository DIR's origin is: (:kind KIND :owner OWNER :repo
REPO), KIND `github' or `bitbucket'; or nil."
  (let ((default-directory (file-name-as-directory (expand-file-name dir))))
    (pr-view--parse-remote (pr-view--git-remote-url))))

(defun pr-view-pr-brief (dir id callback)
  "Look up pull request ID of DIR's repository, for another package.
CALLBACK gets it as `pr-view-branch-pr' gives one, or ((error . TEXT)).
Nothing goes to pr-view's page."
  (if-let* ((host (pr-view-remote dir)))
      (pr-view-repo-pr-brief host id callback)
    (funcall callback '((error . "not a GitHub or Bitbucket repository")))))

(defun pr-view--remote-p (remote)
  "Non-nil when REMOTE names a repository as `pr-view-remote' does."
  (and (memq (plist-get remote :kind) '(github bitbucket))
       (seq-every-p (lambda (k)
                      (let ((v (plist-get remote k)))
                        (and (stringp v) (string-match-p "\\`[[:alnum:]_][[:alnum:]_.-]*\\'" v))))
                    '(:owner :repo))))

(defun pr-view-repo-pr-brief (remote id callback)
  "Look up pull request ID of REMOTE's repository, with no checkout of
it: REMOTE as `pr-view-remote' gives one.  A forge's token is the same
for each of its repositories.  CALLBACK as for `pr-view-pr-brief'."
  (if (not (pr-view--remote-p remote))
      (funcall callback '((error . "not a GitHub or Bitbucket repository")))
    (condition-case e
        (pr-view--http
         (pr-view--url remote (format (if (eq (plist-get remote :kind) 'github) "/pulls/%s" "/pullrequests/%s")
                                      (pr-view--id-str id)))
         (pr-view--headers remote)
         (xwapp-json-callback
          "PR" (lambda (raw) (funcall callback (pr-view--brief remote raw)))
          (pr-view--brief-failed callback)))
      (error (funcall callback `((error . ,(error-message-string e))))))))

(defun pr-view-branch-pr (dir callback)
  "Look up the pull request of the branch checked out at DIR.
For another package to show.  CALLBACK gets the PR as ((id . N)
(title . T) (state . OPEN|MERGED|DECLINED) (draft . BOOL) (url . U)
(source . BRANCH) (tasks . IDS)) -- TASKS the ClickUp tasks its title,
branch and description cite, when clickup-view is there -- an open one
before a closed one, then the newest; nil when DIR is not on a branch
of a GitHub or Bitbucket repository, or is on its default branch, or
the branch has none; ((error . TEXT)) when the forge could not be read.
Nothing goes to pr-view's page."
  (let* ((default-directory (file-name-as-directory (expand-file-name dir)))
         (host (pr-view--parse-remote (pr-view--git-remote-url)))
         (branch (pr-view--git-current-branch)))
    (if (not (and host branch (not (member branch (list "HEAD" (pr-view--git-default-branch))))))
        (funcall callback nil)
      (condition-case e
          (pr-view--http
           (pr-view--url host (pr-view--branch-pr-path host branch))
           (pr-view--headers host)
           (xwapp-json-callback
            "branch PR"
            (lambda (raw) (funcall callback (pr-view--branch-pr-pick host branch raw)))
            (pr-view--brief-failed callback)))
        (error (funcall callback `((error . ,(error-message-string e)))))))))

(defun pr-view-open-pr (dir id)
  "Open pull request ID of the repository at DIR in pr-view."
  (let ((default-directory (file-name-as-directory (expand-file-name dir))))
    (setq pr-view--pending-pr id
          pr-view--pending-local nil)
    (pr-view)))

(defun pr-view-open-remote-pr (remote id)
  "Open pull request ID of REMOTE's repository in pr-view, with no
checkout of it: REMOTE as `pr-view-remote' gives one -- as a tab on
your phone opens one by its address."
  (unless (pr-view--remote-p remote)
    (user-error "Not a GitHub or Bitbucket repository"))
  (setq pr-view--pending-pr id
        pr-view--pending-local nil)
  (pr-view--open-on remote))


;;; Commands

(defun pr-view--on-kill ()
  (setq pr-view--inflight nil))

;;;###autoload
(defun pr-view-reopen ()
  "Make pr-view ready for a browser tab that comes to it by itself: one
on your phone, opened from the herd page, or one coming back after a
restart.  The tab says which pull request or repository it shows.
xwapp calls this; autoloaded, so that once Emacs is up xwapp can load
the package through it, for that tab to find."
  (setq pr-view--list-cache nil
        pr-view--inflight nil
        pr-view--pending-pr nil
        pr-view--pending-local nil
        pr-view--local nil))

;;;###autoload
(defun pr-view (&optional kind)
  "Open the pull-request viewer for the current repository.
KIND is `github' or `bitbucket'; nil auto-detects from origin."
  (interactive)
  (let ((host (pr-view--resolve-host kind)))
    (unless (and host (plist-get host :owner) (plist-get host :repo))
      (user-error "Cannot detect GitHub/Bitbucket repo. Set pr-view-github-* or pr-view-bitbucket-*"))
    (pr-view--open-on host)))

(defun pr-view--open-on (host)
  "Open the page on HOST's repository."
  (setq pr-view--host host
        pr-view--list-cache nil
        pr-view--inflight nil)
  (setf (xwapp-poll-interval pr-view--app) pr-view-poll-interval)
  (xwapp-open pr-view--app)
  (pr-view--js "setStatus" "Loading…"))

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

(defun pr-view--copy-url (&optional url)
  "Copy URL (or last PR URL) to the kill ring and clipboard."
  (let ((url (or url pr-view--last-url)))
    (if (or (not url) (string-empty-p (format "%s" url)))
        (pr-view--js "showError" "No PR URL yet.")
      (setq url (format "%s" url))
      (xwapp-copy url)
      (pr-view--js "setStatus" "Copied PR link"))))

;;;###autoload
(defun pr-view-open-in-browser ()
  "Open the last viewed PR (or repo PRs page) in the system browser."
  (interactive)
  (if pr-view--last-url
      (browse-url pr-view--last-url)
    (user-error "No PR URL yet — open a pull request first")))

;;;###autoload
(defun pr-view-copy-url ()
  "Copy the last viewed PR URL to the clipboard."
  (interactive)
  (if pr-view--last-url
      (progn
        (pr-view--copy-url pr-view--last-url)
        (message "Copied %s" pr-view--last-url))
    (user-error "No PR URL yet — open a pull request first")))

(provide 'pr-view)
;;; pr-view.el ends here

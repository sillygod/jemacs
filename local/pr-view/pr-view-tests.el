;;; pr-view-tests.el --- Tests for pr-view  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;   emacs --batch --init-directory=~/.emacs.d/emacs-home/ \
;;         -L ~/.emacs.d/local/xwapp -L ~/.emacs.d/local/pr-view \
;;         -L ~/.emacs.d/local/clickup-view \
;;         -l pr-view.el -l pr-view-tests.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Without clickup-view on the load path the ClickUp task tests skip.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'pr-view)

(ert-deftest pr-view-test-parse-github-ssh ()
  (should (equal (pr-view--parse-remote "git@github.com:acme/app.git")
                 '(:kind github :owner "acme" :repo "app"))))

(ert-deftest pr-view-test-parse-github-https ()
  (should (equal (pr-view--parse-remote "https://github.com/acme/app")
                 '(:kind github :owner "acme" :repo "app"))))

(ert-deftest pr-view-test-parse-bitbucket-ssh ()
  (should (equal (pr-view--parse-remote "git@bitbucket.org:ws/repo.git")
                 '(:kind bitbucket :owner "ws" :repo "repo"))))

(ert-deftest pr-view-test-parse-bitbucket-https ()
  (should (equal (pr-view--parse-remote
                  "https://bitbucket.org/ws/repo.git")
                 '(:kind bitbucket :owner "ws" :repo "repo"))))

(ert-deftest pr-view-test-parse-unknown ()
  (should-not (pr-view--parse-remote "git@gitlab.com:x/y.git"))
  (should-not (pr-view--parse-remote nil)))

(ert-deftest pr-view-test-parse-ssh-scheme-and-slash ()
  (should (equal (pr-view--parse-remote "ssh://git@github.com/acme/app.git")
                 '(:kind github :owner "acme" :repo "app")))
  (should (equal (pr-view--parse-remote "https://github.com/acme/app/")
                 '(:kind github :owner "acme" :repo "app")))
  (should (equal (pr-view--parse-remote
                  "https://bitbucket.org/ws/repo.git/")
                 '(:kind bitbucket :owner "ws" :repo "repo"))))

(ert-deftest pr-view-test-bb-normalize-item ()
  (let* ((raw (json-parse-string
               "{\"id\":7,\"title\":\"Fix leak\",\"updated_on\":\"2026-01-02T03:04:05+00:00\",\"author\":{\"display_name\":\"Ada\"},\"source\":{\"branch\":{\"name\":\"fix\"}},\"destination\":{\"branch\":{\"name\":\"main\"}},\"links\":{\"html\":{\"href\":\"https://bitbucket.org/ws/repo/pull-requests/7\"}}}"
               :object-type 'alist))
         (it (pr-view--bb-item raw)))
    (should (equal (alist-get 'id it) 7))
    (should (equal (alist-get 'title it) "Fix leak"))
    (should (equal (alist-get 'author it) "Ada"))
    (should (equal (alist-get 'source it) "fix"))
    (should (equal (alist-get 'destination it) "main"))
    (should (string-match-p "pull-requests/7" (alist-get 'url it)))))

(ert-deftest pr-view-test-gh-normalize-item ()
  (let* ((raw (json-parse-string
               "{\"number\":12,\"title\":\"Add UI\",\"updated_at\":\"2026-02-01T00:00:00Z\",\"user\":{\"login\":\"ada\"},\"head\":{\"ref\":\"feat\"},\"base\":{\"ref\":\"main\"},\"html_url\":\"https://github.com/acme/app/pull/12\"}"
               :object-type 'alist))
         (it (pr-view--gh-item raw)))
    (should (equal (alist-get 'id it) 12))
    (should (equal (alist-get 'author it) "ada"))
    (should (equal (alist-get 'source it) "feat"))))

(ert-deftest pr-view-test-intent-prview-and-bbpr ()
  (should (equal (alist-get 'op (pr-view--parse-intent "prview:{\"op\":\"open-pr\",\"id\":3}"))
                 "open-pr"))
  (should (equal (alist-get 'id (pr-view--parse-intent "bbpr:{\"op\":\"open-pr\",\"id\":3}"))
                 3))
  (should-not (pr-view--parse-intent "PR View"))
  (should-not (pr-view--parse-intent "prview:{not json")))

(ert-deftest pr-view-test-js-payload-encodes-quotes ()
  (let ((js (json-encode (pr-view--item 1 "say \"hi\"" "a" "b" "c" "" "u"))))
    (should (string-match-p "say \\\\\"hi\\\\\"" js))))

(ert-deftest pr-view-test-list-payload-is-object ()
  (let* ((item (pr-view--item 1 "t" "a" "s" "d" "u" "http://x"))
         (payload `((forge . "github")
                    (items . ,(vector item))))
         (s (json-encode payload)))
    (should (string-prefix-p "{" s))
    (should (string-match-p "\"items\"" s))))

(ert-deftest pr-view-test-gh-detail-null-body ()
  (let* ((raw (json-parse-string
               "{\"number\":1,\"title\":\"t\",\"body\":null,\"user\":{\"login\":\"ada\"},\"head\":{\"ref\":\"f\"},\"base\":{\"ref\":\"m\"},\"state\":\"open\",\"created_at\":\"c\",\"updated_at\":\"u\",\"html_url\":\"http://x\"}"
               :object-type 'alist
               :null-object nil))
         (pr (pr-view--gh-detail raw)))
    (should (equal (alist-get 'id pr) 1))
    (should (equal (alist-get 'description pr) ""))
    (should (equal (alist-get 'author pr) "ada"))))

(ert-deftest pr-view-test-bb-detail-fields ()
  (let* ((raw (json-parse-string
               "{\"id\":9,\"title\":\"T\",\"description\":\"hello\",\"state\":\"OPEN\",\"created_on\":\"c\",\"updated_on\":\"u\",\"author\":{\"display_name\":\"Ada\"},\"source\":{\"branch\":{\"name\":\"fix\"},\"commit\":{\"hash\":\"aaa111\"}},\"destination\":{\"branch\":{\"name\":\"main\"},\"commit\":{\"hash\":\"bbb222\"}},\"links\":{\"html\":{\"href\":\"http://bb/9\"}}}"
               :object-type 'alist))
         (pr (pr-view--bb-detail raw)))
    (should (equal (alist-get 'id pr) 9))
    (should (equal (alist-get 'description pr) "hello"))
    (should (equal (alist-get 'source pr) "fix"))
    (should (equal (alist-get 'source_hash pr) "aaa111"))
    (should (equal (alist-get 'destination_hash pr) "bbb222"))
    (should (equal (alist-get 'url pr) "http://bb/9"))))

(ert-deftest pr-view-test-bb-diff-path-skips-redirect ()
  (let* ((pr '((source_hash . "aaa111")
               (destination_hash . "bbb222")))
         (path (pr-view--bb-diff-path pr)))
    (should (string-prefix-p "/diff/" path))
    (should (string-match-p "%2E%2E" path))
    (should (string-match-p "topic=true" path))
    (should-not (string-match-p "pullrequests" path))))

(ert-deftest pr-view-test-bb-comment-normalize ()
  (let* ((raw (json-parse-string
               "{\"id\":9,\"created_on\":\"c\",\"deleted\":false,\"content\":{\"raw\":\"hello\"},\"user\":{\"display_name\":\"Ada\"},\"parent\":null}"
               :object-type 'alist
               :null-object nil
               :false-object nil))
         (c (pr-view--bb-comment raw)))
    (should (equal (alist-get 'id c) 9))
    (should (equal (alist-get 'author c) "Ada"))
    (should (equal (alist-get 'content c) "hello"))))

(ert-deftest pr-view-test-comment-payload-bb ()
  (let ((s (json-encode '((content . ((raw . "hi")))))))
    (should (string-match-p "\"raw\":\"hi\"" s))))

(ert-deftest pr-view-test-reply-payload-has-parent ()
  (let ((s (json-encode '((content . ((raw . "r")))
                          (parent . ((id . 9)))))))
    (should (string-match-p "\"parent\"" s))
    (should (string-match-p "\"id\":9" s))))

(ert-deftest pr-view-test-bb-reviewer-approval-from-participants ()
  "Bitbucket puts the verdict in `participants', never in `reviewers'."
  (let* ((raw (json-parse-string
               "{\"id\":1,\"title\":\"t\",\"state\":\"OPEN\",\"author\":{\"display_name\":\"A\"},\"source\":{\"branch\":{\"name\":\"f\"}},\"destination\":{\"branch\":{\"name\":\"main\"}},\"reviewers\":[{\"display_name\":\"Jing Ye\",\"uuid\":\"{jy}\"},{\"display_name\":\"Jim Lee\",\"uuid\":\"{jl}\"},{\"display_name\":\"Ernie\",\"uuid\":\"{er}\"}],\"participants\":[{\"user\":{\"uuid\":\"{jy}\"},\"role\":\"REVIEWER\",\"approved\":true,\"state\":\"approved\"},{\"user\":{\"uuid\":\"{er}\"},\"role\":\"REVIEWER\",\"approved\":false,\"state\":\"changes_requested\"},{\"user\":{\"uuid\":\"{jl}\"},\"role\":\"REVIEWER\",\"approved\":false,\"state\":null}]}"
               :object-type 'alist :array-type 'list
               :null-object nil :false-object nil))
         (revs (alist-get 'reviewers (pr-view--bb-detail raw))))
    (should (equal (alist-get 'status (aref revs 0)) "approved"))
    (should (equal (alist-get 'status (aref revs 1)) ""))
    (should (equal (alist-get 'status (aref revs 2)) "changes_requested"))))

(ert-deftest pr-view-test-bb-reviewers-without-participants ()
  "No participants array is not an approval."
  (let* ((raw (json-parse-string
               "{\"reviewers\":[{\"display_name\":\"Ken\",\"uuid\":\"{k}\"}]}"
               :object-type 'alist :array-type 'list
               :null-object nil :false-object nil))
         (revs (pr-view--reviewers raw (pr-view--bb-verdicts raw))))
    (should (equal (alist-get 'status (aref revs 0)) ""))))

(ert-deftest pr-view-test-gh-reviewers-keep-approvers ()
  "GitHub drops a reviewer from `requested_reviewers' once they review,
so the chips have to come back from the reviews collection."
  (let* ((raw (json-parse-string
               "{\"requested_reviewers\":[{\"login\":\"pending\"}]}"
               :object-type 'alist :array-type 'list
               :null-object nil :false-object nil))
         (reviews (json-parse-string
                   "[{\"user\":{\"login\":\"ada\"},\"state\":\"COMMENTED\"},{\"user\":{\"login\":\"ada\"},\"state\":\"APPROVED\"},{\"user\":{\"login\":\"ada\"},\"state\":\"COMMENTED\"},{\"user\":{\"login\":\"bob\"},\"state\":\"CHANGES_REQUESTED\"}]"
                   :object-type 'alist :array-type 'list
                   :null-object nil :false-object nil))
         (revs (pr-view--gh-reviewers raw reviews)))
    (should (= (length revs) 3))
    (should (equal (alist-get 'name (aref revs 0)) "pending"))
    (should (equal (alist-get 'status (aref revs 0)) ""))
    ;; A later COMMENTED must not erase the approval it follows.
    (should (equal (alist-get 'name (aref revs 1)) "ada"))
    (should (equal (alist-get 'status (aref revs 1)) "approved"))
    (should (equal (alist-get 'status (aref revs 2)) "changes_requested"))))

(ert-deftest pr-view-test-gh-dismissed-review-clears-approval ()
  (let* ((reviews (json-parse-string
                   "[{\"user\":{\"login\":\"ada\"},\"state\":\"APPROVED\"},{\"user\":{\"login\":\"ada\"},\"state\":\"DISMISSED\"}]"
                   :object-type 'alist :array-type 'list
                   :null-object nil :false-object nil))
         (revs (pr-view--gh-reviewers '() reviews)))
    ;; Nothing left to say, so no chip is invented for them.
    (should (= (length revs) 0))
    (should (equal (cdr (assoc "ada" (pr-view--gh-verdicts reviews))) ""))))

(ert-deftest pr-view-test-bb-commit-normalize ()
  (let* ((raw (json-parse-string
               "{\"hash\":\"34ef91addeadbeef\",\"message\":\"fix: subject\\n\\nbody line\",\"date\":\"2026-09-18T01:00:00+00:00\",\"author\":{\"raw\":\"Jing Ye <jing@example.com>\",\"user\":{\"display_name\":\"Jing Ye\",\"links\":{\"avatar\":{\"href\":\"http://a\"}}}},\"links\":{\"html\":{\"href\":\"http://c\"}}}"
               :object-type 'alist :array-type 'list
               :null-object nil :false-object nil))
         (c (pr-view--bb-commit raw)))
    (should (equal (alist-get 'short c) "34ef91ad"))
    (should (equal (alist-get 'author c) "Jing Ye"))
    (should (equal (alist-get 'avatar c) "http://a"))
    (should (equal (alist-get 'url c) "http://c"))
    (should (string-prefix-p "fix: subject" (alist-get 'message c)))))

(ert-deftest pr-view-test-bb-commit-falls-back-to-git-ident ()
  "A commit by somebody with no Bitbucket account still has a name."
  (let* ((raw (json-parse-string
               "{\"hash\":\"abc1234567\",\"message\":\"m\",\"author\":{\"raw\":\"Jing Ye <jing@example.com>\"}}"
               :object-type 'alist :array-type 'list
               :null-object nil :false-object nil))
         (c (pr-view--bb-commit raw)))
    (should (equal (alist-get 'author c) "Jing Ye"))))

(ert-deftest pr-view-test-gh-commit-normalize ()
  (let* ((raw (json-parse-string
               "{\"sha\":\"deadbeefcafe\",\"html_url\":\"http://c\",\"commit\":{\"message\":\"feat: x\",\"author\":{\"name\":\"Ada\",\"date\":\"2026-09-18T01:00:00Z\"}},\"author\":{\"login\":\"ada\",\"avatar_url\":\"http://a\"}}"
               :object-type 'alist :array-type 'list
               :null-object nil :false-object nil))
         (c (pr-view--gh-commit raw)))
    (should (equal (alist-get 'short c) "deadbeef"))
    (should (equal (alist-get 'author c) "ada"))
    (should (equal (alist-get 'date c) "2026-09-18T01:00:00Z"))
    (should (equal (alist-get 'url c) "http://c"))))

(ert-deftest pr-view-test-commits-are-newest-first ()
  "Bitbucket answers newest first and GitHub oldest first; the tab
must not flip order depending on the forge."
  (dolist (case '((github . "[{\"sha\":\"a\",\"commit\":{\"message\":\"old\",\"author\":{\"date\":\"2026-01-01T00:00:00Z\"}}},{\"sha\":\"b\",\"commit\":{\"message\":\"new\",\"author\":{\"date\":\"2026-02-01T00:00:00Z\"}}}]")
                  (bitbucket . "{\"values\":[{\"hash\":\"b\",\"message\":\"new\",\"date\":\"2026-02-01T00:00:00+00:00\"},{\"hash\":\"a\",\"message\":\"old\",\"date\":\"2026-01-01T00:00:00+00:00\"}]}")))
    (let (sent)
      (cl-letf (((symbol-function 'pr-view--http)
                 (lambda (_url _headers cb) (funcall cb (cdr case) 200 nil)))
                ((symbol-function 'pr-view--headers) (lambda (&rest _) nil))
                ((symbol-function 'pr-view--url) (lambda (&rest _) "u"))
                ((symbol-function 'pr-view--js)
                 (lambda (fn obj)
                   (when (equal fn "setCommits") (setq sent obj)))))
        (pr-view--fetch-commits nil (car case) "1"))
      (let ((items (alist-get 'items sent)))
        (should (= (length items) 2))
        (should (equal (alist-get 'message (aref items 0)) "new"))
        (should (equal (alist-get 'message (aref items 1)) "old"))))))

(ert-deftest pr-view-test-commits-failure-is-silent ()
  "The commits tab is supplementary: a 403 on it must not paint an
error over a detail view that is otherwise fine."
  (let (calls)
    (cl-letf (((symbol-function 'pr-view--http)
               (lambda (_url _headers cb) (funcall cb "{}" 403 nil)))
              ((symbol-function 'pr-view--headers) (lambda (&rest _) nil))
              ((symbol-function 'pr-view--url) (lambda (&rest _) "u"))
              ((symbol-function 'pr-view--js)
               (lambda (fn obj) (push (cons fn obj) calls))))
      (pr-view--fetch-commits nil 'bitbucket "1"))
    (should-not calls)))

(ert-deftest pr-view-test-ui-renders-commits-and-verdicts ()
  "The UI has to know about both, or the elisp side talks to nobody."
  (let ((js (with-temp-buffer
              (insert-file-contents (expand-file-name "ui/app.js" pr-view--dir))
              (buffer-string))))
    (should (string-match-p "setCommits" js))
    (should (string-match-p "data-tab=.commits." js))
    (should (string-match-p "changes_requested" js))))

(ert-deftest pr-view-test-ui-never-calls-window-confirm ()
  "`window.confirm' returns false immediately inside xwidget's WebKit --
no dialog is drawn and nothing is asked.  Every guarded action was
therefore cancelled in silence, which is why Merge did nothing at all."
  ;; Comments stripped first: the note explaining this names the call it
  ;; forbids, and a rule that reads prose is the bug one directory over.
  (let ((js (with-temp-buffer
              (insert-file-contents (expand-file-name "ui/app.js" pr-view--dir))
              (goto-char (point-min))
              (while (re-search-forward "^[ \t]*//.*$" nil t)
                (replace-match ""))
              (buffer-string))))
    (should-not (string-match-p "window\\.confirm" js))
    (should-not (string-match-p "window\\.alert" js))
    (should-not (string-match-p "window\\.prompt" js))
    (should (string-match-p "askConfirm" js))))

(ert-deftest pr-view-test-gh-conflict-from-merge-state ()
  "`mergeable' is a JSON boolean and this parser maps false and null
alike to nil, so the old test could never be false.  The state is a
string: GitHub calls a conflicted PR `dirty'."
  (let* ((dirty (json-parse-string
                 "{\"number\":1,\"mergeable\":false,\"mergeable_state\":\"dirty\",\"user\":{\"login\":\"a\"},\"head\":{\"ref\":\"f\"},\"base\":{\"ref\":\"m\"}}"
                 :object-type 'alist :array-type 'list
                 :null-object nil :false-object nil))
         (clean (json-parse-string
                 "{\"number\":1,\"mergeable\":true,\"mergeable_state\":\"clean\",\"user\":{\"login\":\"a\"},\"head\":{\"ref\":\"f\"},\"base\":{\"ref\":\"m\"}}"
                 :object-type 'alist :array-type 'list
                 :null-object nil :false-object nil))
         (unknown (json-parse-string
                   "{\"number\":1,\"user\":{\"login\":\"a\"},\"head\":{\"ref\":\"f\"},\"base\":{\"ref\":\"m\"}}"
                   :object-type 'alist :array-type 'list
                   :null-object nil :false-object nil)))
    (should (eq (alist-get 'conflict (pr-view--gh-detail dirty)) t))
    (should (equal (alist-get 'merge_state (pr-view--gh-detail dirty)) "dirty"))
    (should-not (eq (alist-get 'conflict (pr-view--gh-detail clean)) t))
    ;; GitHub is still computing: not a conflict, and not a claim either.
    (should-not (eq (alist-get 'conflict (pr-view--gh-detail unknown)) t))
    (should (string-match-p "\"conflict\":false"
                            (json-encode (pr-view--gh-detail clean))))))

(ert-deftest pr-view-test-bb-conflicts-endpoint ()
  "Bitbucket answers per file, with a sentence.  Neither the pull
request nor diffstat carries this -- diffstat's `status' enum is only
added/removed/modified/renamed, and a conflicted file comes back
`modified'."
  (let (sent)
    (cl-letf (((symbol-function 'pr-view--http)
               (lambda (_url _headers cb)
                 (funcall cb
                          "{\"values\":[{\"path\":\"frontend/public/releases.json\",\"scenario\":\"content\",\"message\":\"File modified in both source and destination\"}],\"pagelen\":500,\"size\":1,\"page\":1}"
                          200 nil)))
              ((symbol-function 'pr-view--headers) (lambda (&rest _) nil))
              ((symbol-function 'pr-view--url) (lambda (&rest _) "u"))
              ((symbol-function 'pr-view--js)
               (lambda (fn obj)
                 (when (equal fn "setConflicts") (setq sent obj)))))
      (pr-view--fetch-conflicts nil "322"))
    (should (eq (alist-get 'conflict sent) t))
    (let ((files (alist-get 'files sent)))
      (should (= (length files) 1))
      (should (equal (alist-get 'path (aref files 0))
                     "frontend/public/releases.json"))
      (should (equal (alist-get 'scenario (aref files 0)) "content"))
      (should (string-match-p "both source and destination"
                              (alist-get 'message (aref files 0)))))))

(ert-deftest pr-view-test-bb-no-conflicts-says-so ()
  (let (sent)
    (cl-letf (((symbol-function 'pr-view--http)
               (lambda (_url _headers cb)
                 (funcall cb "{\"values\":[],\"size\":0}" 200 nil)))
              ((symbol-function 'pr-view--headers) (lambda (&rest _) nil))
              ((symbol-function 'pr-view--url) (lambda (&rest _) "u"))
              ((symbol-function 'pr-view--js)
               (lambda (fn obj)
                 (when (equal fn "setConflicts") (setq sent obj)))))
      (pr-view--fetch-conflicts nil "1"))
    (should (equal (alist-get 'files sent) []))
    (should (string-match-p "\"conflict\":false" (json-encode sent)))))

(ert-deftest pr-view-test-no-preflight-merge-call ()
  "Bitbucket's merge endpoint takes no `dry_run' -- its published spec
lists only `async' -- so a pre-flight merge would be a merge.  Nothing
may POST to /merge except the user pressing Merge."
  (let ((src (with-temp-buffer
               (insert-file-contents (expand-file-name "pr-view.el" pr-view--dir))
               (buffer-string))))
    ;; The query-string form is the dangerous one; the docstring
    ;; explaining why it is absent is not.
    (should-not (string-match-p "dry_run=" src))
    (should-not (string-match-p "(dry_run" src))))

(ert-deftest pr-view-test-refused-merge-reports-the-conflict ()
  "A merge is the only thing that can answer `would this merge' on
Bitbucket, so its refusal has to land somewhere that stays on screen."
  (let (calls)
    (cl-letf (((symbol-function 'pr-view--js)
               (lambda (fn obj) (push (cons fn obj) calls))))
      (pr-view--merge-refused
       409 "{\"error\":{\"message\":\"The pull request has conflicts\"}}" nil "u"))
    (let ((conf (cdr (assoc "setConflicts" calls))))
      (should conf)
      (should (eq (alist-get 'conflict conf) t))
      (should (equal (alist-get 'message conf) "The pull request has conflicts")))
    (should (assoc "showError" calls))))

(ert-deftest pr-view-test-refused-merge-that-is-not-a-conflict ()
  "Not every refusal is a conflict; only a conflict gets the badge."
  (let (calls)
    (cl-letf (((symbol-function 'pr-view--js)
               (lambda (fn obj) (push (cons fn obj) calls))))
      (pr-view--merge-refused
       403 "{\"error\":{\"message\":\"You do not have permission\"}}" nil "u"))
    (should-not (assoc "setConflicts" calls))
    (should (assoc "showError" calls))))

(ert-deftest pr-view-test-merge-conflict-detection ()
  (should (pr-view--merge-conflict-p 409 nil))
  (should (pr-view--merge-conflict-p 412 nil))
  ;; Bitbucket answers 400 and puts the reason in the message.
  (should (pr-view--merge-conflict-p 400 "The pull request has conflicts"))
  (should (pr-view--merge-conflict-p 405 "Pull Request is not mergeable"))
  (should-not (pr-view--merge-conflict-p 403 "You do not have permission"))
  (should-not (pr-view--merge-conflict-p 500 nil)))

(ert-deftest pr-view-test-api-error-message-is-readable ()
  "A merge refused for conflicts used to show 180 characters of JSON."
  (should (equal (pr-view--error-message
                  "{\"type\":\"error\",\"error\":{\"message\":\"The pull request has conflicts\"}}")
                 "The pull request has conflicts"))
  (should (equal (pr-view--error-message "{\"message\":\"Pull Request is not mergeable\"}")
                 "Pull Request is not mergeable"))
  (should-not (pr-view--error-message "not json"))
  (should-not (pr-view--error-message ""))
  (let (shown)
    (cl-letf (((symbol-function 'pr-view--js)
               (lambda (fn obj) (when (equal fn "showError") (setq shown obj)))))
      (pr-view--fail 409 nil "{\"error\":{\"message\":\"The pull request has conflicts\"}}" "u"))
    (should (string-match-p "409" shown))
    (should (string-match-p "has conflicts" shown))
    (should-not (string-match-p "{" shown))))

(ert-deftest pr-view-test-ui-shows-conflicts ()
  (let ((js (with-temp-buffer
              (insert-file-contents (expand-file-name "ui/app.js" pr-view--dir))
              (buffer-string))))
    (should (string-match-p "setConflicts" js))
    (should (string-match-p "conflictChip" js))
    (should (string-match-p "conflictBanner" js))))

(ert-deftest pr-view-test-ui-has-no-secrets-or-api ()
  (dolist (rel '("ui/app.js" "ui/index.html" "ui/style.css"))
    (let ((s (with-temp-buffer
               (insert-file-contents (expand-file-name rel pr-view--dir))
               (buffer-string))))
      (should-not (string-match-p "api\\.github\\.com" s))
      (should-not (string-match-p "api\\.bitbucket\\.org" s))
      (should-not (string-match-p "Authorization" s))
      (should-not (string-match-p "GITHUB_TOKEN" s)))))

(ert-deftest pr-view-test-fail-401-message ()
  (let (shown)
    (cl-letf (((symbol-function 'pr-view--js)
               (lambda (fn obj)
                 (when (equal fn "showError")
                   (setq shown obj)))))
      (pr-view--fail 401 nil "bad")
      (should (string-match-p "401/403" shown))
      (should (string-match-p "pullrequest:bitbucket" shown)))))

(ert-deftest pr-view-test-bitbucket-bearer-token ()
  (cl-letf (((symbol-function 'pr-view--bitbucket-token)
             (lambda () "ATAT-secret")))
    (let ((hs (pr-view--headers '(:kind bitbucket :owner "w" :repo "r"))))
      (should (equal (cdr (assoc "Authorization" hs))
                     "Bearer ATAT-secret"))
      (should-not (string-match-p "Basic " (cdr (assoc "Authorization" hs)))))))

(ert-deftest pr-view-test-bitbucket-token-env ()
  (let ((process-environment
         (append '("BITBUCKET_TOKEN=from-env" "BITBUCKET_USER=you@ex.com")
                 process-environment)))
    (should (equal (pr-view--bitbucket-token) "from-env"))))

(ert-deftest pr-view-test-bb-item-state-reviewers ()
  (let* ((raw (json-parse-string
               "{\"id\":1,\"title\":\"t\",\"state\":\"MERGED\",\"draft\":false,\"comment_count\":4,\"created_on\":\"c\",\"updated_on\":\"u\",\"author\":{\"display_name\":\"Jing Ye\",\"links\":{\"avatar\":{\"href\":\"http://a\"}}},\"source\":{\"branch\":{\"name\":\"f\"}},\"destination\":{\"branch\":{\"name\":\"main\"}},\"reviewers\":[{\"display_name\":\"Ken\"}],\"links\":{\"html\":{\"href\":\"http://x\"}}}"
               :object-type 'alist
               :array-type 'list
               :false-object nil))
         (it (pr-view--bb-item raw))
         (encoded (json-encode it)))
    (should (equal (alist-get 'state it) "MERGED"))
    (should (equal (alist-get 'comment_count it) 4))
    (should (equal (alist-get 'author_avatar it) "http://a"))
    (should (equal (alist-get 'name (aref (alist-get 'reviewers it) 0)) "Ken"))
    (should (string-match-p "\"draft\":false" encoded))
    (should-not (string-match-p "\"draft\":\"false\"" encoded))))

(ert-deftest pr-view-test-draft-false-is-json-boolean ()
  (let ((s (json-encode `((draft . ,(pr-view--json-bool nil))))))
    (should (equal s "{\"draft\":false}"))))

(ert-deftest pr-view-test-gh-merged-state ()
  (let* ((raw (json-parse-string
               "{\"number\":1,\"title\":\"t\",\"state\":\"closed\",\"merged_at\":\"2026-01-01T00:00:00Z\",\"user\":{\"login\":\"ada\"},\"head\":{\"ref\":\"f\"},\"base\":{\"ref\":\"m\"},\"html_url\":\"u\"}"
               :object-type 'alist
               :null-object nil))
         (it (pr-view--gh-item raw)))
    (should (equal (alist-get 'state it) "MERGED"))))

(ert-deftest pr-view-test-list-path-states ()
  (should (string-match-p "state=OPEN"
                          (pr-view--list-path '(:kind bitbucket) '("OPEN"))))
  (should (string-match-p "state=MERGED"
                          (pr-view--list-path '(:kind bitbucket) '("OPEN" "MERGED"))))
  (should (string-match-p "state=all"
                          (pr-view--list-path '(:kind github) '("OPEN" "MERGED"))))
  (should (string-match-p "state=open"
                          (pr-view--list-path '(:kind github) '("OPEN")))))

(ert-deftest pr-view-test-item-in-states ()
  (should (pr-view--item-in-states '((state . "OPEN") (draft . :false)) '("OPEN")))
  (should-not (pr-view--item-in-states '((state . "OPEN") (draft . t)) '("OPEN")))
  (should (pr-view--item-in-states '((state . "OPEN") (draft . t)) '("DRAFT")))
  (should (pr-view--item-in-states '((state . "MERGED")) '("MERGED"))))

(ert-deftest pr-view-test-create-payload-bb ()
  (let ((s (json-encode (pr-view--create-payload 'bitbucket "T" "d" "feat" "main" t))))
    (should (string-match-p "\"title\":\"T\"" s))
    (should (string-match-p "\"name\":\"feat\"" s))
    (should (string-match-p "close_source_branch" s))))

(ert-deftest pr-view-test-merge-strategy-map ()
  (should (equal (pr-view--merge-strategy 'github "squash") "squash"))
  (should (equal (pr-view--merge-strategy 'github "merge_commit") "merge"))
  (should (equal (pr-view--merge-strategy 'bitbucket "squash") "squash"))
  (should (equal (pr-view--merge-strategy 'bitbucket "merge") "merge_commit")))

(ert-deftest pr-view-test-intent-states-array ()
  (should (equal (alist-get 'states (pr-view--parse-intent
                                     "prview:{\"op\":\"refresh-list\",\"states\":[\"OPEN\",\"MERGED\"]}"))
                 '("OPEN" "MERGED"))))

(ert-deftest pr-view-test-gh-files-to-diff ()
  (let* ((files '(((filename . "a.txt")
                   (status . "modified")
                   (patch . "@@ -1 +1 @@\n-old\n+new"))
                  ((filename . "b.txt")
                   (status . "added")
                   (patch . "@@ -0,0 +1 @@\n+hi"))
                  ((filename . "c.txt")
                   (previous_filename . "old-c.txt")
                   (status . "renamed")
                   (patch . "@@ -1 +1 @@\n-x\n+y"))
                  ((filename . "bin.dat")
                   (status . "removed"))))
         (d (pr-view--gh-files-to-diff files)))
    (should (string-match-p "diff --git a/a.txt b/a.txt" d))
    (should (string-match-p "--- a/a.txt" d))
    (should (string-match-p "\\+new" d))
    (should (string-match-p "new file mode" d))
    (should (string-match-p "diff --git a/b.txt b/b.txt" d))
    (should (string-match-p "rename from old-c.txt" d))
    (should (string-match-p "diff --git a/old-c.txt b/c.txt" d))
    (should (string-match-p "deleted file mode" d))
    (should (string-match-p "diff --git a/bin.dat b/bin.dat" d))))

(ert-deftest pr-view-test-gh-diff-url-media-type ()
  (let ((pair (pr-view--diff-url-and-accept
               '(:kind github :owner "acme" :repo "app")
               'github "1963" nil)))
    (should (string-match-p "/pulls/1963\\'" (car pair)))
    (should (equal (cdr pair) "application/vnd.github.diff"))))

(ert-deftest pr-view-test-gh-diff-406-falls-back-to-files ()
  (let (urls shown)
    (cl-letf (((symbol-function 'pr-view--headers)
               (lambda (&rest _) '(("Authorization" . "Bearer x"))))
              ((symbol-function 'pr-view--http)
               (lambda (url _headers callback &rest _)
                 (push url urls)
                 (cond
                  ((string-match-p "/pulls/1963/files" url)
                   (funcall callback
                            "[{\"filename\":\"a.txt\",\"status\":\"modified\",\"patch\":\"@@ -1 +1 @@\\n-old\\n+new\"}]"
                            200 nil))
                  ((string-match-p "/pulls/1963" url)
                   (funcall callback "too large" 406 nil)))))
              ((symbol-function 'pr-view--js)
               (lambda (fn obj)
                 (when (equal fn "renderPrDetail")
                   (setq shown obj)))))
      (pr-view--fetch-diff
       '(:kind github :owner "acme" :repo "app")
       'github "1963"
       '((id . 1963) (title . "t"))
       "github")
      (should (cl-some (lambda (u) (string-match-p "/files" u)) urls))
      (should (string-match-p "diff --git a/a.txt" (alist-get 'diff shown)))
      (should-not (alist-get 'diff_error shown)))))

(ert-deftest pr-view-test-handler-error-reaches-page ()
  "A reply that arrives but breaks the handler shows its own message,
not \"HTTP 200\" or \"Network error\", and frees the next request."
  (let (shown)
    (cl-letf (((symbol-function 'pr-view--http)
               (lambda (_url _headers cb &rest _) (funcall cb "{}" 200 nil)))
              ((symbol-function 'pr-view--js)
               (lambda (fn obj) (when (equal fn "showError") (setq shown obj)))))
      (setq pr-view--inflight t)
      (pr-view--http-json "u" nil (lambda (_) (error "Boom in handler")))
      (should (equal shown "Boom in handler"))
      (should-not pr-view--inflight))))

;;; ClickUp tasks

(defmacro pr-view-test--with-tasks (&rest body)
  "Run BODY with ClickUp lookups and page calls recorded, not sent.
Inside BODY, `lookups' holds (ID . CALLBACK) oldest first and
`sent' the setTasks payloads, oldest first."
  (declare (indent 0))
  `(progn
     (skip-unless (pr-view--clickup-p))
     (let ((pr-view--tasks nil) (lookups nil) (sent nil))
       (cl-letf (((symbol-function 'clickup-view-task-brief)
                  (lambda (id cb) (setq lookups (append lookups (list (cons id cb))))))
                 ((symbol-function 'pr-view--js)
                  (lambda (fn obj)
                    (when (equal fn "setTasks") (setq sent (append sent (list obj)))))))
         ,@body))))

(defun pr-view-test--task-ids (payload)
  (mapcar (lambda (it) (alist-get 'id it)) (alist-get 'items payload)))

(ert-deftest pr-view-test-tasks-from-detail-then-commits ()
  "The title, branch and description first; commits add only new ones."
  (pr-view-test--with-tasks
    (pr-view--detail-tasks
     "7" '((title . "CU-86abc1 Fix retry")
           (source . "feature/CU-86abc1-retry")
           (description . "See https://app.clickup.com/t/86zz9 -- accepts CU-ids")))
    (should (equal (pr-view-test--task-ids (car (last sent))) '("86abc1" "86zz9")))
    (should (equal (alist-get 'where (aref (alist-get 'items (car (last sent))) 0)) "the title"))
    (should (equal (mapcar #'car lookups) '("86abc1" "86zz9")))
    (pr-view--cite-tasks "7" (pr-view--commit-sources
                              '(((short . "b2") (message . "Retry CU-86new2\n\nAlso CU-86abc1"))
                                ((short . "a1") (message . "Start CU-86abc1")))))
    (let ((last (car (last sent))))
      (should (equal (alist-get 'pr_id last) "7"))
      (should (equal (pr-view-test--task-ids last) '("86abc1" "86zz9" "86new2")))
      (should (equal (alist-get 'where (aref (alist-get 'items last) 2))
                     "commit b2: Retry CU-86new2")))
    (should (equal (mapcar #'car lookups) '("86abc1" "86zz9" "86new2")))
    ;; A lookup lands on its own task and keeps where it was found.
    (funcall (cdr (nth 1 lookups))
             '((id . "86zz9") (name . "Retry loop")
               (status . ((name . "in progress") (color . "#1090e0") (type . "custom")))))
    (let ((it (aref (alist-get 'items (car (last sent))) 1)))
      (should (equal (alist-get 'name it) "Retry loop"))
      (should (equal (alist-get 'where it) "the description"))
      (should (eq (alist-get 'loading it) json-false)))))

(ert-deftest pr-view-test-tasks-of-a-closed-pr-are-dropped ()
  "Commits and lookups for the PR left behind must not reach the next."
  (pr-view-test--with-tasks
    (pr-view--detail-tasks "7" '((title . "CU-86abc1")))
    (pr-view--detail-tasks "8" '((title . "no task")))
    (setq sent nil)
    (funcall (cdr (car lookups)) '((id . "86abc1") (name . "old")))
    (pr-view--cite-tasks "7" '(("CU-86late1" . "commit")))
    (should-not sent)
    (should (equal pr-view--tasks '("8")))))

(ert-deftest pr-view-test-task-lookups-are-capped ()
  "A release PR citing dozens of tasks must not spend the rate limit."
  (pr-view-test--with-tasks
    (pr-view--detail-tasks
     "7" `((description . ,(mapconcat (lambda (i) (format "CU-86t%d" i))
                                      (number-sequence 1 25) " "))))
    (should (= (length lookups) pr-view--task-lookups))
    (let ((items (alist-get 'items (car (last sent)))))
      (should (= (length items) 25))
      (should (eq (alist-get 'loading (aref items 19)) t))
      (should (eq (alist-get 'loading (aref items 20)) json-false)))))

(ert-deftest pr-view-test-commits-cite-tasks ()
  "The commits fetch hands its messages on, for the PR it was asked for."
  (pr-view-test--with-tasks
    (setq pr-view--tasks (list "1"))
    (cl-letf (((symbol-function 'pr-view--http)
               (lambda (_url _headers cb)
                 (funcall cb "{\"values\":[{\"hash\":\"b2c3\",\"message\":\"Fix CU-86abc1\"}]}" 200 nil)))
              ((symbol-function 'pr-view--headers) (lambda (&rest _) nil))
              ((symbol-function 'pr-view--url) (lambda (&rest _) "u")))
      (pr-view--fetch-commits nil 'bitbucket "1"))
    (should (equal (pr-view-test--task-ids (car (last sent))) '("86abc1")))))

(ert-deftest pr-view-test-clickup-failure-does-not-stop-the-pr ()
  "ClickUp is extra: if finding tasks breaks, the PR still loads."
  (let (fetched)
    (cl-letf (((symbol-function 'pr-view--http)
               (lambda (_u _h cb &rest _) (funcall cb "{\"id\":7,\"title\":\"t\"}" 200 nil)))
              ((symbol-function 'pr-view--headers) (lambda (&rest _) nil))
              ((symbol-function 'pr-view--js) #'ignore)
              ((symbol-function 'message) #'ignore)
              ((symbol-function 'pr-view--detail-tasks) (lambda (&rest _) (error "Boom")))
              ((symbol-function 'pr-view--fetch-comments) (lambda (&rest _) (push 'comments fetched)))
              ((symbol-function 'pr-view--fetch-members) #'ignore)
              ((symbol-function 'pr-view--fetch-commits) (lambda (&rest _) (push 'commits fetched)))
              ((symbol-function 'pr-view--fetch-conflicts) #'ignore)
              ((symbol-function 'pr-view--fetch-diff) (lambda (&rest _) (push 'diff fetched))))
      (let ((pr-view--host '(:kind bitbucket :owner "w" :repo "r"))
            (pr-view--inflight nil))
        (pr-view--fetch-detail 7)))
    (should (memq 'comments fetched))
    (should (memq 'commits fetched))
    (should (memq 'diff fetched))))

(ert-deftest pr-view-test-open-task ()
  "clickup-view opens a task; the browser gets it when that refuses."
  (skip-unless (pr-view--clickup-p))
  (let (opened browsed (refuse nil))
    (cl-letf (((symbol-function 'clickup-view-task)
               (lambda (ref)
                 (if refuse (user-error "CLICKUP_API_TOKEN is not set") (setq opened ref))))
              ((symbol-function 'browse-url) (lambda (u &rest _) (setq browsed u))))
      (pr-view--handle-intent '((op . "open-task") (ref . "86abc1") (n . 1)))
      (should (equal opened "86abc1"))
      (setq refuse t)
      (pr-view--handle-intent '((op . "open-task") (ref . "86abc1")))
      (should (equal browsed "https://app.clickup.com/t/86abc1"))
      (setq browsed nil opened nil)
      (pr-view--open-task "https://app.clickup.com/t/9008/ABC-12")
      (should (equal browsed "https://app.clickup.com/t/9008/ABC-12"))
      (setq browsed nil)
      (pr-view--open-task "javascript:alert(1)")
      (pr-view--open-task "https://evil.example/t/86abc1")
      (should-not browsed)
      (should-not opened))))

(ert-deftest pr-view-test-ui-knows-tasks ()
  "The page renders setTasks and asks for open-task, and its intents go
through xwapp's bridge, whose counter keeps a repeated click from being
taken for the last one."
  (let ((js (with-temp-buffer
              (insert-file-contents (expand-file-name "ui/app.js" pr-view--dir))
              (buffer-string)))
        (html (with-temp-buffer
                (insert-file-contents (expand-file-name "ui/index.html" pr-view--dir))
                (buffer-string))))
    (should (string-match-p "setTasks" js))
    (should (string-match-p "\"open-task\"" js))
    (should (string-search "XW.bridge(PREFIX)" js))
    (should (string-search "../../xwapp/ui/xwapp.js" html))
    ;; The CSP refuses inline handlers, so the page must not have any.
    (should-not (string-match-p "on\\(error\\|click\\|load\\)=" js))))

;;; For other packages

(defmacro pr-view-tests--on-branch (remote branch &rest body)
  "BODY in a repository whose origin is REMOTE, on BRANCH (default main)."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'pr-view--git-remote-url) (lambda () ,remote))
             ((symbol-function 'pr-view--git-current-branch) (lambda () ,branch))
             ((symbol-function 'pr-view--git-default-branch) (lambda () "main"))
             ((symbol-function 'pr-view--headers) (lambda (&rest _) nil)))
     ,@body))

(ert-deftest pr-view-test-branch-pr-asks-the-branch-own-forge ()
  "The query names the branch on DIR's own remote, encoded."
  (should (equal (pr-view--branch-pr-path '(:kind github :owner "me" :repo "r") "feat/a b")
                 "/pulls?state=all&per_page=10&head=me%3Afeat%2Fa%20b"))
  (should (equal (pr-view--branch-pr-path '(:kind bitbucket :owner "w" :repo "r") "feat/x")
                 (concat "/pullrequests?state=OPEN&state=MERGED&state=DECLINED&pagelen=10&sort=-updated_on&q="
                         "source.branch.name%3D%22feat%2Fx%22")))
  (let ((pr-view-github-owner "someone-else") asked)
    (pr-view-tests--on-branch "git@github.com:me/r.git" "feat/x"
      (cl-letf (((symbol-function 'pr-view--http) (lambda (url &rest _) (setq asked url))))
        (pr-view-branch-pr "/tmp/" #'ignore)))
    (should (string-prefix-p "https://api.github.com/repos/me/r/pulls?" asked))))

(ert-deftest pr-view-test-branch-pr-picks-the-open-one ()
  "Of the branch's PRs an open one, else the newest; another branch's
are not it.  The ClickUp tasks its title and description cite go with it."
  (let ((rows `(((number . 9) (title . "Old try") (state . "closed") (merged_at . nil)
                 (head . ((ref . "feat/x"))) (base . ((ref . "main"))) (html_url . "u9") (user . ((login . "a"))))
                ((number . 12) (title . "Login, CU-86abc1def") (state . "open")
                 (body . "See https://app.clickup.com/t/86xyz9876")
                 (head . ((ref . "feat/x"))) (base . ((ref . "main"))) (html_url . "u12") (user . ((login . "a"))))
                ((number . 13) (title . "Other") (state . "open")
                 (head . ((ref . "feat/y"))) (base . ((ref . "main"))) (html_url . "u13") (user . ((login . "a")))))))
    (let ((pr (pr-view--branch-pr-pick '(:kind github) "feat/x" rows)))
      (should (equal (list (alist-get 'id pr) (alist-get 'state pr) (alist-get 'url pr) (alist-get 'source pr))
                     '(12 "OPEN" "u12" "feat/x")))
      (when (require 'clickup-view nil t)
        (should (equal (alist-get 'tasks pr) '("86abc1def" "86xyz9876")))))
    (should (equal (alist-get 'id (pr-view--branch-pr-pick '(:kind github) "feat/x" (list (car rows)))) 9))
    (should-not (pr-view--branch-pr-pick '(:kind github) "feat/z" rows)))
  (let ((pr (pr-view--branch-pr-pick
             '(:kind bitbucket) "feat/x"
             '((values . (((id . 87) (title . "Sentry") (state . "MERGED")
                           (source . ((branch . ((name . "feat/x")))))
                           (destination . ((branch . ((name . "main")))))
                           (links . ((html . ((href . "b87"))))))))))))
    (should (equal (list (alist-get 'id pr) (alist-get 'state pr) (alist-get 'url pr)) '(87 "MERGED" "b87")))))

(ert-deftest pr-view-test-branch-pr-says-nothing-or-why ()
  "No PR is looked for on the default branch, a detached head or a repo on
no forge; a forge that refuses says so, to the caller and not the page."
  (let (asked got)
    (cl-letf (((symbol-function 'pr-view--http) (lambda (&rest _) (setq asked t)))
              ((symbol-function 'pr-view--js) (lambda (&rest _) (error "Not the page's"))))
      (dolist (case '(("git@github.com:me/r.git" "main") ("git@github.com:me/r.git" "HEAD")
                      ("git@gitlab.com:me/r.git" "feat/x") (nil "feat/x")))
        (setq got 'unset)
        (pr-view-tests--on-branch (car case) (cadr case)
          (pr-view-branch-pr "/tmp/" (lambda (r) (setq got r))))
        (should-not got))
      (should-not asked))
    (pr-view-tests--on-branch "git@bitbucket.org:w/r.git" "feat/x"
      (cl-letf (((symbol-function 'pr-view--http)
                 (lambda (_url _headers cb &rest _) (funcall cb "{}" 401 nil)))
                ((symbol-function 'pr-view--js) (lambda (&rest _) (error "Not the page's"))))
        (pr-view-branch-pr "/tmp/" (lambda (r) (setq got r)))))
    (should (equal got '((error . "the forge refused the token"))))))

(ert-deftest pr-view-test-pr-brief-by-id ()
  "A PR by its number, on DIR's own forge: its state and the tasks it
cites; a missing one, or a repo on no forge, says why."
  (let (got asked)
    (pr-view-tests--on-branch "git@bitbucket.org:w/r.git" "feat/x"
      (should (equal (pr-view-remote "/tmp/") '(:kind bitbucket :owner "w" :repo "r")))
      (cl-letf (((symbol-function 'pr-view--http)
                 (lambda (url _headers cb &rest _)
                   (setq asked url)
                   (funcall cb (json-encode '((id . 371) (title . "Retry CU-86abc1def") (state . "MERGED")
                                               (description . "")
                                               (source . ((branch . ((name . "fix/retry")))))
                                               (destination . ((branch . ((name . "main")))))
                                               (links . ((html . ((href . "b371")))))))
                            200 nil))))
        (pr-view-pr-brief "/tmp/" 371 (lambda (r) (setq got r)))))
    (should (string-suffix-p "/repositories/w/r/pullrequests/371" asked))
    (should (equal (list (alist-get 'id got) (alist-get 'state got) (alist-get 'source got) (alist-get 'url got))
                   '(371 "MERGED" "fix/retry" "b371")))
    (when (require 'clickup-view nil t)
      (should (equal (alist-get 'tasks got) '("86abc1def"))))
    (pr-view-tests--on-branch "git@github.com:me/r.git" "feat/x"
      (cl-letf (((symbol-function 'pr-view--http)
                 (lambda (_url _headers cb &rest _) (funcall cb "{\"message\":\"Not Found\"}" 404 nil))))
        (pr-view-pr-brief "/tmp/" 9 (lambda (r) (setq got r)))))
    (should (equal got '((error . "no such pull request"))))
    (pr-view-tests--on-branch nil "feat/x"
      (pr-view-pr-brief "/tmp/" 9 (lambda (r) (setq got r))))
    (should (equal got '((error . "not a GitHub or Bitbucket repository"))))))

(ert-deftest pr-view-test-pr-brief-of-a-repository ()
  "A PR of a repository with no checkout here: asked of its forge by
its name, git not asked; a name that is no repository's is not sent."
  (let (got asked)
    (cl-letf (((symbol-function 'pr-view--git-remote-url) (lambda () (error "Not git's to say")))
              ((symbol-function 'pr-view--headers) (lambda (&rest _) nil))
              ((symbol-function 'pr-view--http)
               (lambda (url _headers cb &rest _)
                 (setq asked url)
                 (funcall cb (json-encode '((number . 2) (title . "Pair debug projects") (state . "open")
                                            (draft . :json-false) (body . "")
                                            (html_url . "https://github.com/wingeatech/backend-interview/pull/2")
                                            (head . ((ref . "feat/pair-debug-projects")))))
                          200 nil))))
      (pr-view-repo-pr-brief '(:kind github :owner "wingeatech" :repo "backend-interview") 2
                             (lambda (r) (setq got r)))
      (should (equal asked "https://api.github.com/repos/wingeatech/backend-interview/pulls/2"))
      (should (equal (list (alist-get 'id got) (alist-get 'state got) (alist-get 'source got) (alist-get 'url got))
                     '(2 "OPEN" "feat/pair-debug-projects" "https://github.com/wingeatech/backend-interview/pull/2")))
      (setq asked nil)
      (dolist (bad '((:kind github :owner "../x" :repo "r") (:kind github :owner "w" :repo "r/pulls/1?x=")
                     (:kind gitlab :owner "w" :repo "r") (:kind github :owner "w")))
        (pr-view-repo-pr-brief bad 1 (lambda (r) (setq got r)))
        (should (equal got '((error . "not a GitHub or Bitbucket repository")))))
      (should-not asked))))

(ert-deftest pr-view-test-open-a-pr-with-no-checkout ()
  "A PR of another repository opens on that repository, its detail once
the page is ready; a name that is no repository's is refused."
  (let (opened fetched)
    (cl-letf (((symbol-function 'xwapp-open) (lambda (&rest _) (setq opened pr-view--host)))
              ((symbol-function 'pr-view--js) #'ignore)
              ((symbol-function 'pr-view--fetch-detail) (lambda (id) (push (list 'detail id) fetched)))
              ((symbol-function 'pr-view--fetch-list) (lambda () (push 'list fetched))))
      (let ((pr-view--pending-pr nil) (pr-view--host nil))
        (pr-view-open-remote-pr '(:kind github :owner "wingeatech" :repo "backend-interview") 2)
        (should (equal opened '(:kind github :owner "wingeatech" :repo "backend-interview")))
        (pr-view--handle-intent '((op . "ready")))
        (should (equal fetched '((detail 2))))
        (should-error (pr-view-open-remote-pr '(:kind github :owner "w" :repo "../../x") 1) :type 'user-error)))))

(ert-deftest pr-view-test-open-pr-opens-it-once-ready ()
  "Opened for another package, the page shows that PR once ready, not
the list; the next time it is ready, the list again."
  (let (opened fetched)
    (cl-letf (((symbol-function 'pr-view) (lambda (&rest _) (setq opened default-directory)))
              ((symbol-function 'pr-view--fetch-detail) (lambda (id) (push (list 'detail id) fetched)))
              ((symbol-function 'pr-view--fetch-list) (lambda () (push 'list fetched))))
      (let ((pr-view--pending-pr nil))
        (pr-view-open-pr "/tmp/repo" 87)
        (should (equal opened "/tmp/repo/"))
        (pr-view--handle-intent '((op . "ready")))
        (pr-view--handle-intent '((op . "ready")))
        (should (equal (reverse fetched) '((detail 87) list)))))))

(ert-deftest pr-view-test-a-pull-request-by-its-address ()
  "A GitHub or Bitbucket pull request's address, each in its own word for
it, gives its repository and number; nothing else does -- not another
site, not plain http, not a name that leaves the repository's path."
  (should (equal (pr-view--pr-of-url "https://bitbucket.org/unno/shop/pull-requests/42")
                 '((:kind bitbucket :owner "unno" :repo "shop") . 42)))
  (should (equal (pr-view--pr-of-url "https://github.com/me/my.app/pull/7/files")
                 '((:kind github :owner "me" :repo "my.app") . 7)))
  (dolist (url '("https://github.com/me/x/pull-requests/4" "https://bitbucket.org/ws/r/pull/5"
                 "http://github.com/me/x/pull/4" "https://evil.example/github.com/a/b/pull/1"
                 "https://bitbucket.org/../r/pull-requests/1" "https://github.com/a/b%2F../pull/1"
                 "https://github.com/me/x/pull/" "" nil :false))
    (should-not (pr-view--pr-of-url url)))
  (should (equal (pr-view--repo-of "bitbucket/unno/shop") '(:kind bitbucket :owner "unno" :repo "shop")))
  (should (equal (pr-view--repo-of "github/me/my.app") '(:kind github :owner "me" :repo "my.app")))
  (dolist (spec '("gitlab/a/b" "github/../x" "github/a" "github/a/b/c" "" nil))
    (should-not (pr-view--repo-of spec))))

(ert-deftest pr-view-test-a-tab-says-what-it-shows ()
  "A page in a tab says what it showed -- opened on the phone at a pull
request, or back after a restart, which Emacs knows nothing of: that
pull request, or that repository's list.  Without, as before."
  (let (fetched)
    (cl-letf (((symbol-function 'pr-view--fetch-detail)
               (lambda (id) (push (list 'detail id pr-view--host) fetched)))
              ((symbol-function 'pr-view--fetch-list)
               (lambda () (push (list 'list pr-view--host) fetched))))
      (let ((pr-view--host '(:kind github :owner "me" :repo "x"))
            (pr-view--pending-pr 87)
            (pr-view--list-cache '(stale)))
        (pr-view--handle-intent '((op . "ready") (pr . "https://bitbucket.org/unno/shop/pull-requests/42") (repo . "")))
        (should (equal (car fetched) '(detail 42 (:kind bitbucket :owner "unno" :repo "shop"))))
        (should-not pr-view--list-cache)
        (should-not pr-view--pending-pr)
        (pr-view--handle-intent '((op . "ready") (pr . "") (repo . "github/me/y")))
        (should (equal (car fetched) '(list (:kind github :owner "me" :repo "y"))))
        ;; What it says that is no pull request or repository: as before.
        (setq pr-view--pending-pr 5)
        (pr-view--handle-intent '((op . "ready") (pr . "https://example.com/x") (repo . "gitlab/a/b")))
        (should (equal (car fetched) '(detail 5 (:kind github :owner "me" :repo "y"))))
        (pr-view--handle-intent '((op . "ready")))
        (should (equal (car fetched) '(list (:kind github :owner "me" :repo "y"))))))))

(ert-deftest pr-view-test-opens-again-by-itself ()
  "The page can open by itself, for a tab on your phone or one back after
a restart; what it opens with is the tab's to say, not what was left."
  (should (eq (xwapp-reopen pr-view--app) #'pr-view-reopen))
  (let ((pr-view--list-cache '(stale)) (pr-view--inflight t) (pr-view--pending-pr 3))
    (pr-view-reopen)
    (should-not (or pr-view--list-cache pr-view--inflight pr-view--pending-pr))))

;;; Local changes

(defmacro pr-view-tests--repo (var &rest body)
  "BODY with VAR a new git checkout, git's own and the user's config
left out."
  (declare (indent 1))
  `(let* ((,var (file-name-as-directory (file-truename (make-temp-file "pr-view-local-" t))))
          (process-environment
           (append (list "GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_NOSYSTEM=1"
                         "GIT_AUTHOR_NAME=t" "GIT_AUTHOR_EMAIL=t@example.com"
                         "GIT_COMMITTER_NAME=t" "GIT_COMMITTER_EMAIL=t@example.com")
                   process-environment)))
     (unwind-protect
         (progn (pr-view-tests--git ,var "init" "-q" "-b" "main") ,@body)
       (delete-directory ,var t))))

(defun pr-view-tests--git (root &rest args)
  (let ((default-directory root))
    (should (eq 0 (apply #'call-process "git" nil nil nil args)))))

(defun pr-view-tests--write (root path text)
  (let ((file (expand-file-name path root)))
    (make-directory (file-name-directory file) t)
    (let ((coding-system-for-write 'no-conversion))
      (write-region text nil file nil 'silent))))

(defun pr-view-tests--local (root)
  "ROOT's changes as the page is sent them."
  (let (got)
    (pr-view--local-read root (lambda (payload) (setq got payload)))
    (with-timeout (10 (error "git did not finish"))
      (while (not got) (accept-process-output nil 0.05)))
    got))

(defun pr-view-tests--counted (diff)
  "The lines DIFF adds and deletes, as the page counts them."
  (let ((added 0) (deleted 0) (in nil))
    (dolist (line (split-string diff "\n"))
      (cond ((string-prefix-p "diff --git " line) (setq in nil))
            ((string-prefix-p "@@ " line) (setq in t))
            ((and in (string-prefix-p "+" line)) (cl-incf added))
            ((and in (string-prefix-p "-" line)) (cl-incf deleted))))
    (list added deleted)))

(ert-deftest pr-view-test-local-changes-of-a-checkout ()
  "What a checkout has not committed: staged or not, against HEAD; a new
file whole; one not shown named with why -- binary, too large, a link,
which is not followed -- and an ignored one not at all."
  (pr-view-tests--repo root
    (let ((outside (make-temp-file "pr-view-secret-")))
      (unwind-protect
          (let ((pr-view--local-file-max 64))
            (pr-view-tests--write outside "" "the secret\n")
            (pr-view-tests--write root "a.txt" "one\ntwo\n")
            (pr-view-tests--write root "b.txt" "keep\n")
            (pr-view-tests--write root ".gitignore" "*.log\n")
            (pr-view-tests--git root "add" ".")
            (pr-view-tests--git root "commit" "-q" "-m" "First things")
            (pr-view-tests--write root "a.txt" "one\nTWO\n")
            (pr-view-tests--git root "add" "a.txt")
            (pr-view-tests--write root "b.txt" "keep\nmore\n")
            (pr-view-tests--write root "new dir/new.txt" "hello")
            (pr-view-tests--write root "bin.dat" "a\0b")
            (pr-view-tests--write root "big.txt" (make-string 100 ?x))
            (pr-view-tests--write root "debug.log" "ignored\n")
            (make-symbolic-link outside (expand-file-name "link" root))
            (let* ((p (pr-view-tests--local root))
                   (diff (alist-get 'diff p)))
              (should (equal (alist-get 'root p) root))
              (should (equal (alist-get 'branch p) "main"))
              (should (string-match-p "\\`[0-9a-f]+ First things\\'" (alist-get 'head p)))
              (should (equal (alist-get 'diff_error p) ""))
              (should (= (alist-get 'more p) 0))
              ;; Staged and not, both.
              (should (string-match-p "^-two$" diff))
              (should (string-match-p "^\\+TWO$" diff))
              (should (string-match-p "^\\+more$" diff))
              ;; New, whole, with a space in its path.
              (should (string-match-p "^diff --git a/new dir/new.txt b/new dir/new.txt\nnew file mode 100644\n--- /dev/null\n\\+\\+\\+ b/new dir/new.txt\n@@ -0,0 \\+1,1 @@\n\\+hello\n\\\\ No newline at end of file$" diff))
              (should (equal (alist-get 'notes p)
                             '(("big.txt" . "too large to show: 100") ("bin.dat" . "binary") ("link" . "not a plain file"))))
              (should (string-match-p "^diff --git a/link b/link\nnew file mode" diff))
              (should-not (string-match-p "secret" diff))
              (should-not (string-match-p "debug.log" diff))
              ;; As many as pr-view-local-stat counts, by construction.
              (should (equal (pr-view-local-stat (concat root "new dir/"))
                             (list :root root :files 6 :added 3 :deleted 1 :untracked 4 :untracked-added 1)))
              (should (equal (pr-view-tests--counted diff) '(3 1)))
              (should (= (pr-view--local-files diff) 6))))
        (delete-file outside)))))

(ert-deftest pr-view-test-local-changes-before-the-first-commit ()
  "Before its first commit a checkout's changes are against nothing."
  (pr-view-tests--repo root
    (pr-view-tests--write root "staged.txt" "s\n")
    (pr-view-tests--git root "add" "staged.txt")
    (pr-view-tests--write root "loose.txt" "l\n")
    (let ((diff (alist-get 'diff (pr-view-tests--local root))))
      (should (string-match-p "^\\+s$" diff))
      (should (string-match-p "^\\+l$" diff)))
    (should (equal (pr-view-local-stat root) (list :root root :files 2 :added 2 :deleted 0 :untracked 1 :untracked-added 1)))))

(ert-deftest pr-view-test-local-changes-run-none-of-the-checkouts-commands ()
  "A checkout's textconv filter or external diff could be any command:
git is told to run neither."
  (pr-view-tests--repo root
    (let ((ran (expand-file-name "ran" (make-temp-file "pr-view-ran-" t))))
      (pr-view-tests--write root "a.txt" "one\n")
      (pr-view-tests--write root ".gitattributes" "*.txt diff=evil\n")
      (pr-view-tests--git root "add" ".")
      (pr-view-tests--git root "commit" "-q" "-m" "x")
      (pr-view-tests--git root "config" "diff.evil.textconv" (format "touch %s; cat" ran))
      (pr-view-tests--git root "config" "diff.external" (format "touch %s; true" ran))
      (pr-view-tests--write root "a.txt" "two\n")
      (should (string-match-p "^\\+two$" (alist-get 'diff (pr-view-tests--local root))))
      (should (plist-get (pr-view-local-stat root) :files))
      (should-not (file-exists-p ran))
      (delete-directory (file-name-directory ran) t))))

(ert-deftest pr-view-test-local-changes-too-long-are-cut-between-files ()
  "Past the most the page is sent, whole files are left out, and counted;
a byte that is no UTF-8 does not stop the page's JSON."
  (pr-view-tests--repo root
    (pr-view-tests--write root "seed" "x\n")
    (pr-view-tests--git root "add" ".")
    (pr-view-tests--git root "commit" "-q" "-m" "x")
    (dotimes (i 5)
      (pr-view-tests--write root (format "f%d.txt" i) (concat (make-string 300 ?y) "\n")))
    (pr-view-tests--write root "latin.txt" "caf\351\n")
    (let ((pr-view--local-max 1000))
      (let* ((p (pr-view-tests--local root))
             (diff (alist-get 'diff p)))
        (should (<= (length diff) 1000))
        (should (= (+ (pr-view--local-files diff) (alist-get 'more p)) 6))
        (should (> (alist-get 'more p) 0))
        (should (string-suffix-p "\n" diff))
        (should (json-encode p))))
    (should (string-match-p "caf\uFFFD" (alist-get 'diff (pr-view-tests--local root))))
    ;; So many new files: the first so many, the rest counted.
    (let* ((pr-view--local-untracked-max 2)
           (p (pr-view-tests--local root)))
      (should (= (pr-view--local-files (alist-get 'diff p)) 2))
      (should (= (alist-get 'more p) 4))
      (should (= (plist-get (pr-view-local-stat root) :files) 6))
      (should (= (plist-get (pr-view-local-stat root) :untracked) 6)))))

(ert-deftest pr-view-test-local-changes-only-of-a-checkout-it-may-show ()
  "A page names a checkout in its address: shown when it was opened here
or another package vouches for it, else refused -- and git is not run
there."
  (pr-view-tests--repo herd
    (pr-view-tests--repo elsewhere
      (let (read errors pr-view--local-opened pr-view-local-allowed-functions)
        (cl-letf (((symbol-function 'pr-view--local-read) (lambda (root _cb) (push root read)))
                  ((symbol-function 'pr-view--js)
                   (lambda (fn arg) (when (equal fn "showError") (push arg errors))))
                  ((symbol-function 'pr-view--fetch-list) #'ignore))
          (pr-view--handle-intent `((op . "ready") (local . ,elsewhere)))
          (should-not read)
          (should (string-match-p "Not a checkout this page may show" (car errors)))
          (add-hook 'pr-view-local-allowed-functions (lambda (root) (equal root herd)))
          (pr-view--handle-intent `((op . "ready") (local . ,herd)))
          (should (equal read (list herd)))
          (should (equal pr-view--local herd))
          ;; Its refresh, and one for another checkout.
          (pr-view--handle-intent `((op . "local") (root . ,herd)))
          (pr-view--handle-intent `((op . "local") (root . ,elsewhere)))
          (should (equal read (list herd herd)))
          ;; One gone since, said so.
          (setq errors nil)
          (let ((gone (concat herd "gone/")))
            (add-hook 'pr-view-local-allowed-functions (lambda (root) (equal root gone)))
            (pr-view--handle-intent `((op . "local") (root . ,gone)))
            (should (equal read (list herd herd)))
            (should (string-match-p "No checkout at .*gone/ any more" (car errors))))
          ;; Back to the list leaves it.
          (pr-view--handle-intent '((op . "back-list")))
          (should-not pr-view--local))))))

(ert-deftest pr-view-test-changes-opens-a-checkout-here ()
  "Opened in Emacs, a checkout's changes are shown once the page is
ready, and that checkout the page may show again; not a checkout, said."
  (pr-view-tests--repo root
    (let (opened read pr-view--local-opened pr-view--pending-local)
      (cl-letf (((symbol-function 'xwapp-open) (lambda (_app) (setq opened t)))
                ((symbol-function 'pr-view--js) #'ignore)
                ((symbol-function 'pr-view--local-read) (lambda (r _cb) (push r read))))
        (pr-view-changes root)
        (should opened)
        (should (equal pr-view--local-opened (list (pr-view--local-root root))))
        (pr-view--handle-intent '((op . "ready")))
        (should (equal read (list (pr-view--local-root root))))
        ;; Once: the next time it is ready, the list.
        (should-not pr-view--pending-local)
        (should (pr-view--local-allowed-p (pr-view--local-root root)))
        (let ((plain (make-temp-file "pr-view-plain-" t)))
          (should-error (pr-view-changes plain) :type 'user-error)
          (should-not (pr-view-local-stat plain))
          (delete-directory plain t))))))

(defconst pr-view-tests--comment
  '((path . "a.txt") (side . "new") (line . 1) (code . "x") (text . "Why?"))
  "A comment on a line, as the page sends one.")

(ert-deftest pr-view-test-local-review-text ()
  "A review is one text: the checkout and its branch, then each comment
numbered, at its file and line -- a line taken out said so -- with the
line as it was shown, then the note.  Too long, a part is cut, and says
so; not a comment, or none, or too many, it is refused."
  (pr-view-tests--repo root
    (pr-view-tests--write root "a.txt" "x\n")
    (pr-view-tests--git root "add" ".")
    (pr-view-tests--git root "commit" "-q" "-m" "x")
    (pr-view-tests--git root "checkout" "-q" "-b" "fix/retry")
    (should (equal (pr-view--local-review-text
                    root
                    '(((path . "src/pay.py") (side . "new") (line . 2) (code . "    total = order.total")
                       (text . "Name it amount.\n\nAnd round it."))
                      ((path . "src/pay.py") (side . "old") (line . 1) (code . "def pay(order):")
                       (text . "  Keep this name.  "))
                      ((path . "README.md") (side . "new") (line . 3) (code . " ") (text . "No blank line.")))
                    " Run the tests after. ")
                   (concat "Comments on what is not committed in " root
                           " (branch fix/retry), each on a line of its diff against HEAD:\n"
                           "\n1. src/pay.py:2\n   >     total = order.total\n   Name it amount.\n\n   And round it.\n"
                           "\n2. src/pay.py, line 1 before, taken out:\n   > def pay(order):\n   Keep this name.\n"
                           "\n3. README.md:3\n   (an empty line)\n   No blank line.\n"
                           "\nRun the tests after.\n")))
    ;; Not on a branch: none named.  No note: nothing after.
    (pr-view-tests--git root "checkout" "-q" "--detach")
    (should (string-match-p (concat "\\`Comments on what is not committed in " (regexp-quote root)
                                    ", each on a line .*\n\n1\\. a\\.txt:1\n   > x\n   Why\\?\n\\'")
                            (pr-view--local-review-text root (list pr-view-tests--comment) "  ")))
    (let ((pr-view--review-text-max 5) (pr-view--review-code-max 3))
      (should (string-match-p "\n   > a b…\n   12345…\n\nabcde…\n\\'"
                              (pr-view--local-review-text
                               root '(((path . "a") (side . "new") (line . 1) (code . "a\nbcdef") (text . "1234567")))
                               "abcdefgh"))))
    (dolist (bad `(nil
                   "a string"
                   (((path . "") (side . "new") (line . 1) (text . "x")))
                   (((path . "a\nb") (side . "new") (line . 1) (text . "x")))
                   (((path . "a") (side . "mid") (line . 1) (text . "x")))
                   (((path . "a") (side . "new") (line . 0) (text . "x")))
                   (((path . "a") (side . "new") (line . 1.5) (text . "x")))
                   (((path . "a") (side . "new") (line . 1) (text . " \n ")))
                   (((path . "a") (side . "new") (line . 1) (code . 7) (text . "x")))
                   (,pr-view-tests--comment 7)))
      (should-error (pr-view--local-review-text root bad nil) :type 'user-error))
    (let ((pr-view--review-max 2))
      (should (pr-view--local-review-text root (make-list 2 pr-view-tests--comment) nil))
      (should-error (pr-view--local-review-text root (make-list 3 pr-view-tests--comment) nil)
                    :type 'user-error))))

(ert-deftest pr-view-test-review-text-covers-a-range ()
  "A comment can cover a range of lines: file:start-end (or lines L-E
before, taken out), each quoted; a single line (no `end' over `line', or
the older `code'-only shape) reads as before."
  (pr-view-tests--repo root
    (pr-view-tests--write root "a.txt" "x\n")
    (pr-view-tests--git root "add" ".")
    (pr-view-tests--git root "commit" "-q" "-m" "x")
    (pr-view-tests--git root "checkout" "-q" "--detach")
    ;; A range on the new side.
    (should (string-match-p
             "\n1\\. src/pay\\.py:2-4\n   > a = 1\n   > b = 2\n   > c = 3\n   Name them\\.\n"
             (pr-view--local-review-text
              root '(((path . "src/pay.py") (side . "new") (line . 2) (end . 4)
                      (lines . ("a = 1" "b = 2" "c = 3")) (code . "a = 1") (text . "Name them.")))
              nil)))
    ;; A range on the old side.
    (should (string-match-p
             "\n1\\. src/pay\\.py, lines 1-2 before, taken out:\n   > old1\n   > old2\n   Why\\?\n"
             (pr-view--local-review-text
              root '(((path . "src/pay.py") (side . "old") (line . 1) (end . 2)
                      (lines . ("old1" "old2")) (text . "Why?")))
              nil)))
    ;; end not over line, or no lines: a single line, as before.
    (should (string-match-p
             "\n1\\. a\\.txt:1\n   > x\n   Why\\?\n"
             (pr-view--local-review-text root (list pr-view-tests--comment) nil)))
    ;; A range whose `lines' is not a list of strings is refused.
    (should-error (pr-view--local-review-text
                   root '(((path . "a") (side . "new") (line . 1) (end . 3) (lines . [1 2]) (text . "x")))
                   nil)
                  :type 'user-error)))

(ert-deftest pr-view-test-local-review-goes-to-whom-it-names ()
  "From the page, a review goes to the one it names, of those another
package says may get one for that checkout -- and the page is told so,
with who may get the next.  Not for a checkout the page may not show,
nor to one not offered, nor when it is no review: then the page is told
why, and nothing is sent."
  (pr-view-tests--repo root
    (let* (sent js asked
           (pr-view--local-opened nil)
           (pr-view-local-allowed-functions (list (lambda (r) (equal r root))))
           (pr-view-local-recipients-functions
            (list (lambda (r)
                    (push r asked)
                    (and (equal r root)
                         (list (list :id "lead" :label "lead · claude"
                                     :send (lambda (text) (push (list "lead" text) sent) "Sent to the lead"))
                               (list :id "rd" :label "rd · grok"
                                     :send (lambda (text) (push (list "rd" text) sent) nil))
                               '(:id "" :label "nobody" :send ignore)
                               '(:id "mute" :label "mute"))))
                  (lambda (_r) (error "A broken one"))))
           (said (lambda () (cdr (car js)))))
      (cl-letf (((symbol-function 'pr-view--js) (lambda (fn arg) (push (cons fn arg) js))))
        (should (equal (pr-view--local-recipients-json root)
                       [((id . "lead") (label . "lead · claude")) ((id . "rd") (label . "rd · grok"))]))
        (should (equal (pr-view--local-recipients-json "/etc/") []))
        (pr-view--handle-intent `((op . "local-review") (root . ,root) (to . "rd")
                                  (comments . (,pr-view-tests--comment)) (note . "Then commit.")))
        (should (equal (mapcar #'car sent) '("rd")))
        (should (string-match-p "\\`Comments on what is not committed in .*\n\n1\\. a\\.txt:1\n   > x\n   Why\\?\n\nThen commit\\.\n\\'"
                                (cadr (car sent))))
        (should (equal (caar js) "reviewSent"))
        (should (equal (alist-get 'root (funcall said)) root))
        ;; One that says nothing of what it did: sent to it, by its label.
        (should (equal (alist-get 'said (funcall said)) "Sent to rd · grok"))
        (should (equal (alist-get 'recipients (funcall said)) (pr-view--local-recipients-json root)))
        (pr-view--handle-intent `((op . "local-review") (root . ,root) (to . "lead")
                                  (comments . (,pr-view-tests--comment))))
        (should (equal (mapcar #'car sent) '("lead" "rd")))
        (should (equal (alist-get 'said (funcall said)) "Sent to the lead"))
        ;; Refused, each: nothing sent, the page told why.
        (setq sent nil js nil asked nil)
        (dolist (intent `(((op . "local-review") (root . "/etc/") (to . "rd") (comments . (,pr-view-tests--comment)))
                          ((op . "local-review") (root . 7) (to . "rd") (comments . (,pr-view-tests--comment)))
                          ((op . "local-review") (root . ,root) (to . "mute") (comments . (,pr-view-tests--comment)))
                          ((op . "local-review") (root . ,root) (to . "") (comments . (,pr-view-tests--comment)))
                          ((op . "local-review") (root . ,root) (to . "rd"))
                          ((op . "local-review") (root . ,root) (to . "rd")
                           (comments . (((path . "a.txt") (side . "new") (line . -1) (text . "x")))))))
          (pr-view--handle-intent intent))
        (should-not sent)
        (should (equal (mapcar #'car js) (make-list 6 "reviewFailed")))
        (let ((failed (mapcar #'cdr (reverse js))))
          (should (equal (mapcar (lambda (f) (alist-get 'root f)) failed) (list "/etc/" nil root root root root)))
          (should (equal (mapcar (lambda (f) (car (split-string (alist-get 'error f) ":"))) failed)
                         '("Not a checkout this page may show" "Not a checkout this page may show"
                           "mute cannot be sent it now" " cannot be sent it now"
                           "No comments to send" "Not a comment on a line")))
          ;; Told who may get it, for a checkout it may show only: nobody
          ;; is asked about another.
          (should (equal (mapcar (lambda (f) (length (alist-get 'recipients f))) failed) '(0 0 2 2 2 2)))
          (should (equal (delete-dups asked) (list root))))))))

(ert-deftest pr-view-test-local-changes-say-who-may-get-a-review ()
  "A checkout's changes come to the page with who may be sent a review of
them, and with none when nobody may."
  (pr-view-tests--repo root
    (let ((pr-view--local root) js
          (pr-view-local-recipients-functions
           (list (lambda (r) (and (equal r root) (list (list :id "rd" :label "rd · claude" :send #'ignore)))))))
      (cl-letf (((symbol-function 'pr-view--js) (lambda (fn arg) (push (cons fn arg) js)))
                ((symbol-function 'pr-view--local-read) (lambda (r cb) (funcall cb `((root . ,r) (diff . ""))))))
        (pr-view--local-read-for root)
        (should (equal (alist-get 'recipients (cdr (assoc "renderLocal" js)))
                       [((id . "rd") (label . "rd · claude"))]))
        (setq pr-view-local-recipients-functions nil js nil)
        (pr-view--local-read-for root)
        (should (equal (alist-get 'recipients (cdr (assoc "renderLocal" js))) []))))))

(ert-deftest pr-view-test-local-editors-map-files-to-agents ()
  "Which agent edited which file comes from the editors hooks, merged by
file with each agent named once and the ill-formed dropped; a hook that
takes the PR plist is given it, one that does not is not; a broken one
does not stop the rest; and the checkout's changes carry the map."
  (pr-view-tests--repo root
    (let* ((pr-view--local root)
           seen js
           (pr-view-local-editors-functions
            (list
             ;; A one-argument hook (checkout only): never given the PR.
             (lambda (r)
               (push (list 1 r) seen)
               (and (equal r root)
                    (list (list :file "a.txt" :by (list (list :id "rd" :label "rd · grok")))
                          ;; An ill-formed mark (no id) is dropped; an entry
                          ;; with no file is dropped whole.
                          (list :file "b.txt" :by (list '(:id "" :label "x") (list :id "qa" :label "qa · agy")))
                          (list :by (list (list :id "z" :label "z"))))))
             ;; A hook that also takes the PR plist: given it when there is
             ;; one, called with one argument (pr nil) for a checkout.
             (lambda (r &optional pr)
               (push (list 2 r pr) seen)
               (and (equal r root)
                    ;; Another agent on a.txt, and rd again -- merged, rd once.
                    (list (list :file "a.txt" :by (list (list :id "lead" :label "lead · claude")
                                                        (list :id "rd" :label "rd · grok"))))))
             ;; A broken one does not stop the rest.
             (lambda (&rest _) (error "boom")))))
      (cl-letf (((symbol-function 'pr-view--js) (lambda (fn arg) (push (cons fn arg) js)))
                ((symbol-function 'pr-view--local-read) (lambda (r cb) (funcall cb `((root . ,r) (diff . ""))))))
        ;; Of the checkout: merged by file, agents in order, each named once.
        (should (equal (pr-view--local-editors-json root)
                       [((file . "a.txt") (by . [((id . "rd") (label . "rd · grok"))
                                                 ((id . "lead") (label . "lead · claude"))]))
                        ((file . "b.txt") (by . [((id . "qa") (label . "qa · agy"))]))]))
        ;; A checkout no hook speaks for: empty.
        (should (equal (pr-view--local-editors-json "/etc/") []))
        ;; The PR plist reaches only the two-argument hook.
        (setq seen nil)
        (pr-view--local-editors nil (list :branch "feat/y" :repo "o/r"))
        (should (seq-find (lambda (x) (and (eq (car x) 2) (equal (nth 2 x) '(:branch "feat/y" :repo "o/r")))) seen))
        (should-not (seq-find (lambda (x) (and (eq (car x) 1) (nth 2 x))) seen))
        ;; The checkout's changes come to the page with the map.
        (setq js nil)
        (pr-view--local-read-for root)
        (should (equal (alist-get 'editors (cdr (assoc "renderLocal" js)))
                       (pr-view--local-editors-json root)))
        ;; With no editors hook at all: an empty map, never an error.
        (setq pr-view-local-editors-functions nil)
        (should (equal (pr-view--local-editors-json root) []))))))

(ert-deftest pr-view-test-ui-shows-local-changes ()
  "The page has a view for a checkout's changes, asks for one by its
address, and says why a file is not shown."
  (let ((js (with-temp-buffer
              (insert-file-contents (expand-file-name "ui/app.js" pr-view--dir))
              (buffer-string))))
    (should (string-match-p "renderLocal: renderLocal" js))
    (should (string-match-p "local: p.get(\"local\")" js))
    (should (string-match-p "\"Not shown: \" \\+ why" js))))

(provide 'pr-view-tests)
;;; pr-view-tests.el ends here

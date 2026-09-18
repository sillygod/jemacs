;;; pr-view-tests.el --- Tests for pr-view  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;   emacs --batch --init-directory=~/.emacs.d/emacs-home/ \
;;         -L ~/.emacs.d/local/pr-view \
;;         -l pr-view.el -l pr-view-tests.el \
;;         -f ert-run-tests-batch-and-exit

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

(ert-deftest pr-view-test-absolutize-location ()
  (should (equal (pr-view--absolutize
                  "https://api.bitbucket.org/2.0/x"
                  "https://api.bitbucket.org/2.0/y")
                 "https://api.bitbucket.org/2.0/x"))
  (should (equal (pr-view--absolutize
                  "/2.0/repositories/ws/r/diff/aa..bb"
                  "https://api.bitbucket.org/2.0/repositories/ws/r/pullrequests/1/diff")
                 "https://api.bitbucket.org/2.0/repositories/ws/r/diff/aa..bb")))

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

(ert-deftest pr-view-test-utf8-body-is-unibyte ()
  (let ((s (pr-view--utf8-bytes (json-encode '((title . "feat → master"))))))
    (should-not (multibyte-string-p s))
    (should (string-match-p "feat" s))))

(ert-deftest pr-view-test-scrub-hides-bearer ()
  (should (string-match-p "\\*\\*\\*"
                          (pr-view--scrub-error
                           "Authorization: Bearer ATATT-secret extra"))))

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

(ert-deftest pr-view-test-extra-headers-drop-accept ()
  (let ((h (pr-view--extra-headers
            '(("Accept" . "application/vnd.github.diff")
              ("Authorization" . "Bearer x")
              ("X-GitHub-Api-Version" . "2022-11-28")))))
    (should-not (assoc "Accept" h))
    (should (equal (cdr (assoc "Authorization" h)) "Bearer x"))
    (should (assoc "X-GitHub-Api-Version" h))))

(ert-deftest pr-view-test-extra-headers-json-content-type ()
  (let ((h (pr-view--extra-headers '(("Accept" . "application/json")) t)))
    (should (equal (cdr (assoc "Content-Type" h))
                   "application/json; charset=utf-8"))
    (should-not (assoc "Accept" h))))

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

(provide 'pr-view-tests)
;;; pr-view-tests.el ends here

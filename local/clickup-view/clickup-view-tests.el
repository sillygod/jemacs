;;; clickup-view-tests.el --- Tests for clickup-view  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;   emacs --batch -L ~/.emacs.d/local/xwapp -L ~/.emacs.d/local/clickup-view \
;;         -l clickup-view.el -l clickup-view-tests.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; fixtures/md-sample.delta.json is the output of the clickup-task-manager
;; skill's markdown_delta.py on fixtures/md-sample.md; the Elisp port must
;; match it op for op.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'clickup-view)

(defconst clickup-view-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defun clickup-view-test--fixture (name)
  (with-temp-buffer
    (insert-file-contents (expand-file-name (concat "fixtures/" name)
                                            clickup-view-test--dir))
    (buffer-string)))

(defun clickup-view-test--json (s)
  (json-parse-string s :object-type 'alist :array-type 'list
                     :null-object nil :false-object nil))

(defmacro clickup-view-test--with-api (routes &rest body)
  "Run BODY with HTTP answered from ROUTES and page calls recorded.
ROUTES is a list of (METHOD URL-REGEXP CODE BODY).  Inside BODY,
`requests' holds (METHOD URL JSON-BODY) newest first and `calls'
holds (FN . OBJ) page calls, oldest first."
  (declare (indent 1))
  `(let ((requests nil) (page-calls nil)
         (clickup-view--team "900") (clickup-view--me '((id . 7) (name . "me")))
         (clickup-view--spaces '(((id . "s1") (name . "Back-End"))))
         (clickup-view--list-cache (make-hash-table :test #'equal))
         (clickup-view--space-cache (make-hash-table :test #'equal)))
     (cl-letf (((symbol-function 'clickup-view--token) (lambda () "pk_test"))
               ((symbol-function 'clickup-view--http)
                (lambda (url _headers cb &optional method body)
                  (push (list (or method "GET") url body) requests)
                  (let ((route (seq-find (lambda (r)
                                           (and (equal (nth 0 r) (or method "GET"))
                                                (string-match-p (nth 1 r) url)))
                                         ,routes)))
                    (if route
                        (funcall cb (nth 3 route) (nth 2 route) nil)
                      (funcall cb "{\"err\":\"no route\"}" 404 nil)))))
               ((symbol-function 'clickup-view--js)
                (lambda (fn obj) (push (cons fn obj) page-calls)))
               ((symbol-function 'message) #'ignore))
       (cl-macrolet ((calls () '(reverse page-calls)))
         ,@body))))

(defun clickup-view-test--call (calls fn)
  "The object of the last page call to FN in CALLS."
  (cdr (car (last (seq-filter (lambda (c) (equal (car c) fn)) calls)))))

(defconst clickup-view-test--task-json
  "{\"id\":\"abc123\",\"custom_id\":null,\"name\":\"Fix retry <loop>\",
    \"status\":{\"status\":\"in progress\",\"color\":\"#1090e0\",\"type\":\"custom\",\"orderindex\":1},
    \"priority\":{\"priority\":\"high\",\"color\":\"#f8ae00\"},
    \"assignees\":[{\"id\":7,\"username\":\"Ada\",\"initials\":\"A\",\"color\":\"#000\",\"profilePicture\":null}],
    \"creator\":{\"id\":8,\"username\":\"Bob\"},
    \"tags\":[{\"name\":\"backend\",\"tag_fg\":\"#fff\",\"tag_bg\":\"#000\"}],
    \"due_date\":\"1791338403957\",\"start_date\":null,\"date_created\":\"1\",\"date_updated\":\"2\",
    \"markdown_description\":\"## Plan\\n- one\",\"text_content\":\"Plan one\",
    \"list\":{\"id\":\"L1\",\"name\":\"BACKLOG\"},
    \"folder\":{\"id\":\"F1\",\"name\":\"hidden\",\"hidden\":true},
    \"space\":{\"id\":\"s1\"},
    \"parent\":null,\"top_level_parent\":null,
    \"checklists\":[{\"id\":\"c1\",\"name\":\"Release\",\"items\":[{\"id\":\"i1\",\"name\":\"tag\",\"resolved\":true,\"children\":[]}]}],
    \"subtasks\":[{\"id\":\"sub1\",\"name\":\"child\",\"status\":{\"status\":\"Open\",\"orderindex\":0},\"assignees\":[],\"parent\":\"abc123\"}],
    \"attachments\":[{\"id\":\"a1\",\"title\":\"shot.png\",\"url\":\"https://t900.p.clickup-attachments.com/x.png\",\"thumbnail_medium\":\"https://t900.p.clickup-attachments.com/m.png\",\"extension\":\"png\",\"mimetype\":\"image/png\",\"size\":10}],
    \"url\":\"https://app.clickup.com/t/abc123\"}")

(defconst clickup-view-test--list-json
  "{\"id\":\"L1\",\"name\":\"BACKLOG\",\"space\":{\"id\":\"s1\",\"name\":\"Back-End\"},
    \"folder\":{\"id\":\"F9\",\"name\":\"hidden\",\"hidden\":true},\"statuses\":[
     {\"status\":\"Closed\",\"type\":\"closed\",\"orderindex\":5,\"color\":\"#000\"},
     {\"status\":\"Open\",\"type\":\"open\",\"orderindex\":0,\"color\":\"#aaa\"},
     {\"status\":\"in progress\",\"type\":\"custom\",\"orderindex\":1,\"color\":\"#1090e0\"}]}")

(defconst clickup-view-test--comments-json
  "{\"comments\":[{\"id\":\"90\",\"date\":\"1791338403957\",\"reply_count\":2,
     \"user\":{\"id\":7,\"username\":\"Ada\"},\"comment_text\":\"hi\",
     \"comment\":[{\"text\":\"hi\"},{\"text\":\"\\n\",\"attributes\":{}}]}]}")


;;; Task references

(ert-deftest clickup-view-test-refs-from-links-and-cu-ids ()
  (should (equal (clickup-view-task-refs
                  "fix: retry (https://app.clickup.com/t/z90hj32fg8) and CU-86abc123, again CU-86abc123")
                 '("z90hj32fg8" "86abc123")))
  (should (equal (clickup-view-task-refs "feature/fix-CU-86abc123-retry")
                 '("86abc123")))
  (should (equal (clickup-view-task-refs "see app.clickup.com/t/abc123?comment=9")
                 '("abc123"))))

(ert-deftest clickup-view-test-refs-ignore-lookalikes ()
  (should-not (clickup-view-task-refs "xCU-abc ACU-1 CU-ABC custom CU-"))
  ;; Prose: an id has a digit, a word does not.
  (should-not (clickup-view-task-refs "open-task accepts links and CU-ids, or a CU-id."))
  (should-not (clickup-view-task-refs "pasted as CU-xxx, see app.clickup.com/t/abc"))
  (should-not (clickup-view--normalize-id "ids"))
  ;; /t/<team>/<custom id>: neither the team nor the custom id is native.
  (should-not (clickup-view-task-refs "https://app.clickup.com/t/9008039987/ABC-123"))
  (should-not (clickup-view-task-refs nil)))

(ert-deftest clickup-view-test-refs-same-in-any-buffer ()
  "The CU- match must not depend on the current buffer's syntax table."
  (with-temp-buffer
    (emacs-lisp-mode)
    (should (equal (clickup-view-task-refs "feature/fix-CU-86abc123-retry")
                   '("86abc123")))))

(ert-deftest clickup-view-test-normalize-id ()
  (should (equal (clickup-view--normalize-id " https://app.clickup.com/t/z90hj32fg8 ") "z90hj32fg8"))
  (should (equal (clickup-view--normalize-id "CU-z90hj32fg8") "z90hj32fg8"))
  (should (equal (clickup-view--normalize-id "z90hj32fg8") "z90hj32fg8"))
  (should-not (clickup-view--normalize-id "not an id"))
  (should-not (clickup-view--normalize-id "")))

(ert-deftest clickup-view-test-branch-refs ()
  (cl-letf (((symbol-function 'clickup-view--git)
             (lambda (&rest args)
               (pcase args
                 (`("rev-parse" "--abbrev-ref" "HEAD") "feature/CU-aaa111-retry")
                 (`("rev-parse" "--verify" "--quiet" "origin/HEAD") "deadbeef")
                 (`("log" . ,_)
                  "\x1e\ fix retry backoff\nRefs https://app.clickup.com/t/bbb222\n\x1estart CU-aaa111\n")))))
    (should (equal (clickup-view--branch-refs)
                   '(("aaa111" . "branch feature/CU-aaa111-retry")
                     ("bbb222" . "fix retry backoff"))))))

(ert-deftest clickup-view-test-branch-refs-without-base-reads-head-only ()
  (let (log-args)
    (cl-letf (((symbol-function 'clickup-view--git)
               (lambda (&rest args)
                 (pcase args
                   (`("rev-parse" "--abbrev-ref" "HEAD") "main")
                   (`("log" . ,rest) (setq log-args rest) "\x1eone CU-ccc333")
                   (_ nil)))))
      (should (equal (mapcar #'car (clickup-view--branch-refs)) '("ccc333")))
      (should (member "-n1" log-args)))))


;;; HTTP

(ert-deftest clickup-view-test-url-query ()
  (should (equal (clickup-view--url "/list/L1/task"
                                    '((page . 0) (subtasks . t) (include_closed . :json-false)
                                      ("assignees[]" . nil) (x . ("a b" "c"))))
                 "https://api.clickup.com/api/v2/list/L1/task?page=0&subtasks=true&include_closed=false&x=a%20b&x=c"))
  (should (equal (clickup-view--url "/user") "https://api.clickup.com/api/v2/user")))

(ert-deftest clickup-view-test-error-text ()
  (should (string-match-p "rejected the token" (clickup-view--error-text 401 nil "{}")))
  (should (equal (clickup-view--error-text 401 nil "{\"err\":\"Token invalid\",\"ECODE\":\"OAUTH_025\"}")
                 "ClickUp rejected the token (Token invalid (OAUTH_025)). Check CLICKUP_API_TOKEN."))
  ;; What ClickUp answers for a task id that does not exist.
  (should (string-prefix-p "Not found in your workspace"
                           (clickup-view--error-text
                            401 nil "{\"err\":\"Team not authorized\",\"ECODE\":\"OAUTH_027\"}")))
  (should (equal (clickup-view--error-text 400 nil "{\"err\":\"Status not found\",\"ECODE\":\"ITEM_015\"}")
                 "HTTP 400 — Status not found (ITEM_015)"))
  (should (equal (clickup-view--error-text 200 '(error "Boom") nil) "Boom"))
  (should (string-prefix-p "Network error" (clickup-view--error-text nil '(error "down") nil)))
  (should-not (string-match-p "pk_secret"
                              (clickup-view--error-text nil '(error "Authorization: pk_secret") nil))))

(ert-deftest clickup-view-test-token-missing-is-a-user-error ()
  (let ((process-environment (cons "CLICKUP_API_TOKEN" process-environment)))
    (should-error (clickup-view--token) :type 'user-error)))

(ert-deftest clickup-view-test-request-sync ()
  (clickup-view-test--with-api
      `(("GET" "/user\\'" 200 "{\"user\":{\"id\":7}}")
        ("GET" "/task/nope" 404 "{\"err\":\"Task not found\",\"ECODE\":\"ITEM_013\"}"))
    (should (equal (alist-get 'id (alist-get 'user (clickup-view--request-sync "GET" "/user"))) 7))
    (let ((e (should-error (clickup-view--request-sync "GET" "/task/nope") :type 'user-error)))
      (should (string-match-p "Task not found" (cadr e))))))


;;; Normalize

(ert-deftest clickup-view-test-task-normalize ()
  (let* ((clickup-view--spaces '(((id . "s1") (name . "Back-End"))))
         (task (clickup-view--task (clickup-view-test--json clickup-view-test--task-json))))
    (should (equal (alist-get 'name task) "Fix retry <loop>"))
    (should (equal (alist-get 'name (alist-get 'status task)) "in progress"))
    (should (equal (alist-get 'name (alist-get 'priority task)) "high"))
    (should (equal (alist-get 'name (aref (alist-get 'assignees task) 0)) "Ada"))
    (should (equal (alist-get 'avatar (aref (alist-get 'assignees task) 0)) ""))
    (should (equal (alist-get 'description task) "## Plan\n- one"))
    (should (equal (alist-get 'name (alist-get 'list task)) "BACKLOG"))
    ;; A folderless list's hidden folder gets no breadcrumb.
    (should-not (alist-get 'folder task))
    (should (equal (alist-get 'name (alist-get 'space task)) "Back-End"))
    (should (eq (alist-get 'resolved (aref (alist-get 'items (aref (alist-get 'checklists task) 0)) 0)) t))
    (should (equal (alist-get 'id (aref (alist-get 'subtasks task) 0)) "sub1"))
    (should (string-match-p "m\\.png" (alist-get 'thumb (aref (alist-get 'attachments task) 0))))
    (let ((js (json-encode task)))
      (should (string-match-p "\"subtasks\":\\[" js))
      (should (string-match-p "\"tags\":\\[{" js)))))

(ert-deftest clickup-view-test-empty-arrays-encode-as-arrays ()
  (let ((js (json-encode (clickup-view--row '((id . "x") (status . ((status . "Open"))))))))
    (should (string-match-p "\"assignees\":\\[\\]" js))))

(ert-deftest clickup-view-test-list-info-crumbs ()
  "A list knows its space and folder, for the breadcrumbs; a folderless
list's hidden folder is none."
  (let ((clickup-view--spaces '(((id . "s1") (name . "Back-End")))))
    (let ((info (clickup-view--list-info
                 "L1" (clickup-view-test--json clickup-view-test--list-json))))
      (should (equal (alist-get 'space info) '((id . "s1") (name . "Back-End"))))
      (should-not (alist-get 'folder info))
      (should (= (length (alist-get 'statuses info)) 3)))
    (let ((info (clickup-view--list-info
                 "L2" (clickup-view-test--json
                       "{\"name\":\"Horus\",\"statuses\":[],\"space\":{\"id\":\"s1\"},
                         \"folder\":{\"id\":\"F1\",\"name\":\"Crypto\",\"hidden\":false}}"))))
      (should (equal (alist-get 'id info) "L2"))
      (should (equal (alist-get 'folder info) '((id . "F1") (name . "Crypto"))))
      ;; GET /list may leave the space's name out: the cached spaces have it.
      (should (equal (alist-get 'name (alist-get 'space info)) "Back-End")))))

(ert-deftest clickup-view-test-statuses-in-list-order ()
  (should (equal (mapcar (lambda (s) (alist-get 'name s))
                         (clickup-view--statuses (clickup-view-test--json clickup-view-test--list-json)))
                 '("Open" "in progress" "Closed"))))


;;; Comment delta → HTML

(defun clickup-view-test--html (ops-json &optional fallback)
  (clickup-view--delta-html (clickup-view-test--json ops-json) fallback))

(ert-deftest clickup-view-test-html-inline-formats ()
  (should (equal (clickup-view-test--html
                  "[{\"text\":\"a \"},{\"text\":\"b\",\"attributes\":{\"bold\":true}},{\"text\":\" \"},{\"text\":\"c\",\"attributes\":{\"code\":true}},{\"text\":\" \"},{\"text\":\"d\",\"attributes\":{\"link\":\"https://x.io/?a=1&b=2\"}},{\"text\":\"\\n\"}]")
                 "<p>a <strong>b</strong> <code>c</code> <a href=\"https://x.io/?a=1&amp;b=2\">d</a></p>")))

(ert-deftest clickup-view-test-html-escapes-everything ()
  (let ((html (clickup-view-test--html
               "[{\"text\":\"<script>alert(1)</script> & 'q'\"},{\"text\":\"x\",\"attributes\":{\"link\":\"javascript:alert(1)\"}},{\"text\":\"\\n\"}]")))
    (should-not (string-match-p "<script" html))
    (should (string-match-p "&lt;script&gt;" html))
    (should (string-match-p "&#39;q&#39;" html))
    (should-not (string-match-p "javascript:" html))))

(ert-deftest clickup-view-test-html-blocks ()
  (should (equal (clickup-view-test--html
                  "[{\"text\":\"Title\"},{\"text\":\"\\n\",\"attributes\":{\"header\":2}},
                    {\"text\":\"one\"},{\"text\":\"\\n\",\"attributes\":{\"list\":\"bullet\"}},
                    {\"text\":\"deep\"},{\"text\":\"\\n\",\"attributes\":{\"list\":\"bullet\",\"indent\":1}},
                    {\"text\":\"first\"},{\"text\":\"\\n\",\"attributes\":{\"list\":\"ordered\"}},
                    {\"text\":\"x := 1\"},{\"text\":\"\\n\",\"attributes\":{\"code-block\":{\"code-block\":\"go\"}}},
                    {\"text\":\"y := <2>\"},{\"text\":\"\\n\",\"attributes\":{\"code-block\":{\"code-block\":\"go\"}}},
                    {\"text\":\"said\"},{\"text\":\"\\n\",\"attributes\":{\"blockquote\":{}}},
                    {\"type\":\"divider\",\"divider\":true},
                    {\"text\":\"tail\\n\\nend\\n\"}]")
                 (concat "<h2>Title</h2>"
                         "<ul><li>one</li><li class=\"indent-1\">deep</li></ul>"
                         "<ol><li>first</li></ol>"
                         "<pre><code>x := 1\ny := &lt;2&gt;\n</code></pre>"
                         "<blockquote><p>said</p></blockquote>"
                         "<hr>"
                         "<p>tail</p><p><br></p><p>end</p>"))))

(ert-deftest clickup-view-test-html-embeds ()
  (let ((html (clickup-view-test--html
               "[{\"type\":\"tag\",\"text\":\"@Ada\",\"user\":{\"id\":7,\"username\":\"Ada <x>\"}},
                 {\"text\":\" see \"},
                 {\"type\":\"task_mention\",\"text\":\"Fix it\",\"task_mention\":{\"task_id\":\"abc123\"}},
                 {\"type\":\"task_mention\",\"text\":\"bad\",\"task_mention\":{\"task_id\":\"x\\\" onclick=\\\"y\"}},
                 {\"type\":\"link_mention\",\"text\":\"\",\"link_mention\":{\"url\":\"https://bitbucket.org/p/1\"}},
                 {\"type\":\"image\",\"text\":\"\",\"image\":{\"name\":\"s.png\",\"url\":\"https://t.p.clickup-attachments.com/s.png\"}},
                 {\"type\":\"image\",\"image\":{\"url\":\"javascript:alert(1)\"}},
                 {\"type\":\"emoticon\",\"text\":\"🎉\"},
                 {\"text\":\"\\n\"}]")))
    (should (string-match-p "<span class=\"cu-mention\">@Ada &lt;x&gt;</span>" html))
    (should (string-match-p "data-task=\"abc123\"[^>]*>Fix it</a>" html))
    (should-not (string-match-p "onclick" html))
    (should (string-match-p "<a href=\"https://bitbucket.org/p/1\">https://bitbucket.org/p/1</a>" html))
    (should (string-match-p "<img class=\"cu-img\" src=\"https://t.p.clickup-attachments.com/s.png\" alt=\"s.png\">" html))
    (should-not (string-match-p "javascript" html))
    (should (string-match-p "🎉" html))))

(ert-deftest clickup-view-test-html-table-embed ()
  (should (equal (clickup-view-test--html
                  "[{\"type\":\"table-embed\",\"table-embed\":{
                      \"rows\":[{\"insert\":{\"id\":\"r1\"}},{\"insert\":{\"id\":\"r2\"}}],
                      \"columns\":[{\"insert\":{\"id\":\"c1\"}},{\"insert\":{\"id\":\"c2\"}}],
                      \"cells\":{\"1:1\":{\"content\":[{\"insert\":\"Case\\n\"}]},
                                 \"1:2\":{\"content\":[{\"insert\":\"a<b\",\"attributes\":{\"bold\":true}},{\"insert\":\"\\n\"}]},
                                 \"2:1\":{\"content\":[{\"insert\":\"two\\nlines\\n\"}]}}}}]")
                 "<table><tr><td>Case</td><td><strong>a&lt;b</strong></td></tr><tr><td>two<br>lines</td><td></td></tr></table>")))

(ert-deftest clickup-view-test-html-falls-back-to-plain-text ()
  (should (equal (clickup-view--delta-html nil "a <b>\nc") "<p>a &lt;b&gt;</p><p>c</p>"))
  (should (equal (clickup-view--delta-html nil nil) "")))


;;; Markdown → delta

(ert-deftest clickup-view-test-md-delta-matches-python ()
  (let ((expected (clickup-view-test--json
                   (clickup-view-test--fixture "md-sample.delta.json")))
        (actual (clickup-view-test--json
                 (json-encode (vconcat (clickup-view--md-to-delta
                                        (clickup-view-test--fixture "md-sample.md")))))))
    (should (= (length actual) (length expected)))
    (cl-loop for a in actual for e in expected for i from 0
             do (should (equal (cons i a) (cons i e))))))

(ert-deftest clickup-view-test-md-delta-empty-attributes-are-objects ()
  "ClickUp gets {} where Python sends {}, never null."
  (let ((js (json-encode (vconcat (clickup-view--md-to-delta "plain\n> q\n---")))))
    (should-not (string-match-p "null" js))
    (should (string-match-p "\"attributes\":{}" js))
    (should (string-match-p "\"blockquote\":{}" js))))


;;; Flows

(ert-deftest clickup-view-test-open-task-flow ()
  (clickup-view-test--with-api
      `(("GET" "/task/abc123\\?" 200 ,clickup-view-test--task-json)
        ("GET" "/list/L1\\'" 200 ,clickup-view-test--list-json)
        ("GET" "/task/abc123/comment" 200 ,clickup-view-test--comments-json))
    (clickup-view--handle-intent '((op . "open-task") (id . "abc123")))
    (let ((fns (mapcar #'car (calls))))
      (should (equal (seq-remove (lambda (f) (equal f "flash")) fns)
                     '("renderTask" "setStatuses" "setComments"))))
    (should (string-match-p "include_markdown_description=true"
                            (nth 1 (car (last requests)))))
    (let ((task (alist-get 'task (clickup-view-test--call (calls) "renderTask"))))
      (should (equal (alist-get 'id task) "abc123")))
    (let ((st (clickup-view-test--call (calls) "setStatuses")))
      (should (equal (alist-get 'list_id st) "L1"))
      (should (= (length (alist-get 'statuses st)) 3)))
    (let ((cs (clickup-view-test--call (calls) "setComments")))
      (should (equal (alist-get 'task_id cs) "abc123"))
      (should (eq (alist-get 'has_more cs) :json-false))
      (should (equal (alist-get 'html (aref (alist-get 'items cs) 0)) "<p>hi</p>"))
      (should (= (alist-get 'replies (aref (alist-get 'items cs) 0)) 2)))))

(ert-deftest clickup-view-test-open-task-uses-cached-statuses ()
  (clickup-view-test--with-api
      `(("GET" "/task/abc123\\?" 200 ,clickup-view-test--task-json)
        ("GET" "/task/abc123/comment" 200 "{\"comments\":[]}"))
    (puthash "L1" '((id . "L1") (name . "BACKLOG") (statuses . [((name . "Open"))]))
             clickup-view--list-cache)
    (clickup-view--handle-intent '((op . "open-task") (id . "abc123")))
    (should-not (seq-find (lambda (r) (string-match-p "/list/" (nth 1 r))) requests))
    (should (equal (alist-get 'statuses (clickup-view-test--call (calls) "renderTask"))
                   [((name . "Open"))]))))

(ert-deftest clickup-view-test-open-list-flags ()
  "Page booleans arrive as :false, which must not read as true."
  (clickup-view-test--with-api
      `(("GET" "/list/L1\\'" 200 ,clickup-view-test--list-json)
        ("GET" "/list/L1/task" 200 "{\"tasks\":[],\"last_page\":true}"))
    (clickup-view--handle-intent '((op . "open-list") (id . "L1") (page . 0)
                                   (closed . :false) (mine . t)))
    (let ((url (nth 1 (car requests))))
      (should (string-match-p "include_closed=false" url))
      (should (string-match-p "assignees%5B%5D=7" url)))
    (let ((l (clickup-view-test--call (calls) "renderList")))
      (should (equal (alist-get 'name l) "BACKLOG"))
      (should (equal (alist-get 'name (alist-get 'space l)) "Back-End"))
      (should (assq 'folder l))
      (should-not (alist-get 'folder l))
      (should (eq (alist-get 'last_page l) t))
      (should (eq (alist-get 'closed l) :json-false))
      (should (equal (alist-get 'tasks l) [])))))

(ert-deftest clickup-view-test-open-space-is-cached ()
  (clickup-view-test--with-api
      `(("GET" "/space/s1/folder" 200 "{\"folders\":[{\"id\":\"F1\",\"name\":\"Crypto\",\"lists\":[{\"id\":\"L2\",\"name\":\"Horus\",\"task_count\":3}]}]}")
        ("GET" "/space/s1/list" 200 "{\"lists\":[{\"id\":\"L1\",\"name\":\"BACKLOG\",\"task_count\":\"70\"}]}"))
    (clickup-view--handle-intent '((op . "open-space") (id . "s1")))
    (clickup-view--handle-intent '((op . "open-space") (id . "s1")))
    (should (= (length requests) 2))
    (let ((sp (clickup-view-test--call (calls) "renderSpace")))
      (should (equal (alist-get 'name sp) "Back-End"))
      (should (equal (alist-get 'name (aref (alist-get 'folders sp) 0)) "Crypto"))
      (should (= (alist-get 'count (aref (alist-get 'lists sp) 0)) 70)))
    (clickup-view--handle-intent '((op . "open-space") (id . "s1") (force . t)))
    (should (= (length requests) 4))))

(ert-deftest clickup-view-test-set-status ()
  (clickup-view-test--with-api
      `(("PUT" "/task/abc123\\'" 200 ,clickup-view-test--task-json))
    (clickup-view--handle-intent '((op . "set-status") (task_id . "abc123") (status . "in progress")))
    (should (equal (nth 2 (car requests)) '((status . "in progress"))))
    (let ((ch (clickup-view-test--call (calls) "statusChanged")))
      (should (equal (alist-get 'task_id ch) "abc123"))
      (should (equal (alist-get 'name (alist-get 'status ch)) "in progress")))))

(ert-deftest clickup-view-test-post-comment-sends-delta ()
  (clickup-view-test--with-api
      `(("POST" "/task/abc123/comment" 200 "{\"id\":\"91\"}")
        ("GET" "/task/abc123/comment" 200 "{\"comments\":[]}"))
    (clickup-view--handle-intent '((op . "post-comment") (task_id . "abc123") (text . "**done**")))
    (let* ((post (seq-find (lambda (r) (equal (car r) "POST")) requests))
           (body (clickup-view-test--json (json-encode (nth 2 post)))))
      (should (equal (alist-get 'comment body)
                     '(((text . "done") (attributes (bold . t)))
                       ((text . "\n") (attributes)))))
      (should (string-match-p "\"notify_all\":false" (json-encode (nth 2 post)))))
    ;; The page is told before the reload, so it clears the box once.
    (should (equal (clickup-view-test--call (calls) "posted")
                   '((task_id . "abc123") (comment_id))))
    (should (equal (car (car requests)) "GET"))
    (should (clickup-view-test--call (calls) "setComments"))))

(ert-deftest clickup-view-test-post-reply-reloads-only-the-thread ()
  "Reloading the comments too would drop older pages the page loaded."
  (clickup-view-test--with-api
      `(("POST" "/comment/90/reply" 200 "{}")
        ("GET" "/comment/90/reply" 200 "{\"comments\":[]}"))
    (clickup-view--handle-intent '((op . "post-reply") (task_id . "abc123") (comment_id . "90") (text . "ok")))
    (should (equal (clickup-view-test--call (calls) "posted")
                   '((task_id . "abc123") (comment_id . "90"))))
    (should (clickup-view-test--call (calls) "setReplies"))
    (should-not (clickup-view-test--call (calls) "setComments"))
    (should-not (seq-find (lambda (r) (string-match-p "/task/" (nth 1 r))) requests))))

(ert-deftest clickup-view-test-open-task-accepts-links ()
  (clickup-view-test--with-api
      `(("GET" "/task/abc123\\?" 200 ,clickup-view-test--task-json)
        ("GET" "/list/L1\\'" 200 ,clickup-view-test--list-json)
        ("GET" "/task/abc123/comment" 200 "{\"comments\":[]}"))
    (clickup-view--handle-intent '((op . "open-task") (id . "https://app.clickup.com/t/abc123")))
    (should (clickup-view-test--call (calls) "renderTask"))
    (clickup-view--handle-intent '((op . "open-task") (id . "no such thing")))
    (should (string-match-p "Not a ClickUp task id"
                            (clickup-view-test--call (calls) "showError")))))

(ert-deftest clickup-view-test-empty-comment-is-refused ()
  (clickup-view-test--with-api nil
    (clickup-view--handle-intent '((op . "post-comment") (task_id . "abc123") (text . "  \n")))
    (should-not requests)
    (should (equal (clickup-view-test--call (calls) "showError") "Comment is empty."))))

(ert-deftest clickup-view-test-api-error-reaches-page ()
  (clickup-view-test--with-api
      `(("PUT" "/task/abc123\\'" 400 "{\"err\":\"Status does not exist\",\"ECODE\":\"ITEM_015\"}"))
    (clickup-view--handle-intent '((op . "set-status") (task_id . "abc123") (status . "nope")))
    (should (equal (clickup-view-test--call (calls) "showError")
                   "HTTP 400 — Status does not exist (ITEM_015)"))))

(ert-deftest clickup-view-test-intent-errors-are-shown-not-lost ()
  (clickup-view-test--with-api nil
    (cl-letf (((symbol-function 'clickup-view--token)
               (lambda () (user-error "CLICKUP_API_TOKEN is not set"))))
      (clickup-view--handle-intent '((op . "open-task") (id . "abc123")))
      (should (string-match-p "CLICKUP_API_TOKEN"
                              (clickup-view-test--call (calls) "showError"))))))

(ert-deftest clickup-view-test-open-browser-only-http ()
  (let (opened)
    (cl-letf (((symbol-function 'browse-url) (lambda (u &rest _) (setq opened u))))
      (clickup-view--handle-intent '((op . "open-browser") (url . "file:///etc/passwd")))
      (should-not opened)
      (clickup-view--handle-intent '((op . "open-browser") (url . "https://app.clickup.com/t/x")))
      (should (equal opened "https://app.clickup.com/t/x")))))

(ert-deftest clickup-view-test-ready-loads-context-then-pending-task ()
  (clickup-view-test--with-api
      `(("GET" "/team\\'" 200 "{\"teams\":[{\"id\":\"900\",\"name\":\"UNNO\"}]}")
        ("GET" "/user\\'" 200 "{\"user\":{\"id\":7,\"username\":\"me\"}}")
        ("GET" "/team/900/space" 200 "{\"spaces\":[{\"id\":\"s1\",\"name\":\"Back-End\"}]}")
        ("GET" "/task/abc123\\?" 200 ,clickup-view-test--task-json)
        ("GET" "/list/L1\\'" 200 ,clickup-view-test--list-json)
        ("GET" "/task/abc123/comment" 200 "{\"comments\":[]}"))
    (let ((clickup-view--team nil) (clickup-view--me nil) (clickup-view--spaces nil)
          (clickup-view--pending "abc123"))
      (clickup-view--handle-intent '((op . "ready")))
      (should (equal (alist-get 'name (aref (alist-get 'spaces (clickup-view-test--call (calls) "setContext")) 0))
                     "Back-End"))
      (should (clickup-view-test--call (calls) "renderTask"))
      (should-not clickup-view--pending))))

(ert-deftest clickup-view-test-ready-without-task-shows-home ()
  (clickup-view-test--with-api nil
    (let ((clickup-view--pending nil))
      (clickup-view--handle-intent '((op . "ready")))
      (should (assoc "showHome" (calls)))
      (should-not requests))))


;;; For pr-view

(ert-deftest clickup-view-test-task-brief ()
  "Name and status for another package, and nothing in the page."
  (clickup-view-test--with-api
      `(("GET" "/task/abc123\\'" 200 ,clickup-view-test--task-json)
        ("GET" "/task/zz9\\'" 401 "{\"err\":\"Team not authorized\",\"ECODE\":\"OAUTH_027\"}"))
    (let (got)
      (clickup-view-task-brief "abc123" (lambda (b) (push b got)))
      (clickup-view-task-brief "zz9" (lambda (b) (push b got)))
      (setq got (nreverse got))
      (should (equal (alist-get 'name (nth 0 got)) "Fix retry <loop>"))
      (should (equal (alist-get 'name (alist-get 'status (nth 0 got))) "in progress"))
      (should (equal (alist-get 'color (alist-get 'status (nth 0 got))) "#1090e0"))
      (should-not (alist-get 'error (nth 0 got)))
      (should (equal (alist-get 'id (nth 1 got)) "zz9"))
      (should (string-match-p "Not found" (alist-get 'error (nth 1 got))))
      (should-not (calls)))))

(ert-deftest clickup-view-test-task-brief-without-token ()
  "No token is an answer, not an error out of another package's code."
  (clickup-view-test--with-api nil
    (cl-letf (((symbol-function 'clickup-view--token)
               (lambda () (user-error "CLICKUP_API_TOKEN is not set"))))
      (let (got)
        (clickup-view-task-brief "abc123" (lambda (b) (setq got b)))
        (should (string-match-p "CLICKUP_API_TOKEN" (alist-get 'error got)))
        (should-not requests)))))

(provide 'clickup-view-tests)
;;; clickup-view-tests.el ends here

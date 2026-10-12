;; -*- lexical-binding: t -*-
;; One Emacs serving pr-view and clickup-view to "your phone", with
;; Bitbucket and ClickUp answered from ./fixtures.  Started by
;; appslib.js: E2E_DIR is where a run writes the state file, the log and
;; the addresses; E2E_PORT is `xwapp-port'; E2E_LOCAL the directory
;; with xwapp, pr-view and clickup-view, this checkout's by default.
(require 'cl-lib)
(defvar e2e-here (file-name-directory (or load-file-name buffer-file-name)))
(defvar e2e-fixtures (expand-file-name "fixtures" e2e-here))
(defvar e2e-dir (getenv "E2E_DIR"))
(defvar e2e-local (or (getenv "E2E_LOCAL") (expand-file-name "../.." e2e-here)))
(dolist (pkg '("xwapp" "pr-view" "clickup-view"))
  (add-to-list 'load-path (expand-file-name pkg e2e-local)))
(require 'xwapp)
(setq xwapp--keep t
      xwapp-state-file (expand-file-name "state.eld" e2e-dir)
      xwapp--gone-after 1.5
      xwapp--hold 2
      xwapp-port (string-to-number (getenv "E2E_PORT"))
      xwapp-remote-hosts '("box.tail0.ts.net")
      xwapp-remote-users '("me@example.com")
      xwapp-browse-function #'ignore)
(require 'pr-view)
(require 'clickup-view)

(defun e2e-note (obj)
  (write-region (concat (json-encode (list :at (float-time) :what obj)) "\n")
                nil (expand-file-name "log.jsonl" e2e-dir) t 'silent))

(defun e2e-file (name)
  (with-temp-buffer
    (insert-file-contents (expand-file-name name e2e-fixtures))
    (buffer-string)))

;; METHOD URL-REGEXP CODE FILE: the forges, answered from fixtures.
(defconst e2e-routes
  '(("GET" "api\\.bitbucket\\.org/2\\.0/repositories/unno/shop/pullrequests/42/diff" 200 "bb-diff.txt")
    ("GET" "api\\.bitbucket\\.org/2\\.0/repositories/unno/shop/pullrequests/42/comments" 200 "bb-comments.json")
    ("GET" "api\\.bitbucket\\.org/2\\.0/repositories/unno/shop/pullrequests/42/commits" 200 "bb-commits.json")
    ("GET" "api\\.bitbucket\\.org/2\\.0/repositories/unno/shop/pullrequests/42/conflicts" 200 "bb-conflicts.json")
    ("GET" "api\\.bitbucket\\.org/2\\.0/repositories/unno/shop/pullrequests/42\\'" 200 "bb-pr.json")
    ("GET" "api\\.bitbucket\\.org/2\\.0/repositories/unno/shop/pullrequests\\?" 200 "bb-list.json")
    ("GET" "api\\.clickup\\.com/api/v2/task/86abc123/comment" 200 "cu-comments.json")
    ("GET" "api\\.clickup\\.com/api/v2/task/86abc123" 200 "cu-task.json")
    ("GET" "api\\.clickup\\.com/api/v2/list/L1/task" 200 "cu-list-tasks.json")
    ("GET" "api\\.clickup\\.com/api/v2/list/L1/member" 200 "cu-members.json")
    ("GET" "api\\.clickup\\.com/api/v2/list/L1" 200 "cu-list.json")))

;; The task as ClickUp would keep it: a PUT changes it and gets it back.
(defvar e2e-task nil)
(defun e2e-put-task (body)
  (unless e2e-task
    (setq e2e-task (json-parse-string (e2e-file "cu-task.json") :object-type 'alist :array-type 'list
                                      :null-object nil :false-object :json-false)))
  (let ((members (alist-get 'members (json-parse-string (e2e-file "cu-members.json") :object-type 'alist
                                                        :array-type 'list :null-object nil))))
    (dolist (kv body)
      (pcase (car kv)
        ('assignees
         (let ((add (append (alist-get 'add (cdr kv)) nil))
               (rem (append (alist-get 'rem (cdr kv)) nil)))
           (setf (alist-get 'assignees e2e-task)
                 (append (seq-remove (lambda (p) (memq (alist-get 'id p) rem)) (alist-get 'assignees e2e-task))
                         (seq-filter (lambda (m) (memq (alist-get 'id m) add)) members)))))
        (key (setf (alist-get key e2e-task) (if (integerp (cdr kv)) (number-to-string (cdr kv)) (cdr kv)))))))
  (json-encode e2e-task))

(defun e2e-http (url _headers cb &optional method body)
  (let ((method (or method "GET")))
    (if (and (equal method "PUT") (string-match-p "api\\.clickup\\.com/api/v2/task/86abc123\\'" url))
        (progn
          (e2e-note (list :put url :body body))
          (run-at-time 0.3 nil cb (e2e-put-task body) 200 nil))
      (let ((route (seq-find (lambda (r) (and (equal (nth 0 r) method) (string-match-p (nth 1 r) url)))
                             e2e-routes)))
        (e2e-note (list :http method :url url :answered (and route t)))
        (run-at-time 0.05 nil cb
                     (if route (e2e-file (nth 3 route)) "{\"error\":\"no route\"}")
                     (if route (nth 2 route) 404)
                     nil)))))

(advice-add 'pr-view--http :override #'e2e-http)
(advice-add 'clickup-view--http :override #'e2e-http)
(advice-add 'pr-view--bitbucket-token :override (lambda () "bb-test"))
(advice-add 'clickup-view--token :override (lambda () "pk_test"))
(setq clickup-view--team "900"
      clickup-view--me '((id . 7) (name . "me") (initials . "M"))
      clickup-view--spaces '(((id . "s1") (name . "Back-End"))))
(advice-add 'pr-view--handle-intent :before (lambda (i) (e2e-note (list :app "pr-view" :intent i))))
(advice-add 'clickup-view--handle-intent :before (lambda (i) (e2e-note (list :app "clickup-view" :intent i))))
(advice-add 'pr-view-reopen :after (lambda () (e2e-note "pr-view reopened")))
(advice-add 'clickup-view-reopen :after (lambda () (e2e-note "clickup-view reopened")))
(advice-add 'browse-url :override (lambda (url &rest _) (e2e-note (list :browse url))))
(advice-add 'xwapp-copy :override (lambda (text) (e2e-note (list :copy text))))

(write-region (json-encode (list (cons "pr" (xwapp-url pr-view--app))
                                 (cons "task" (xwapp-url clickup-view--app))))
              nil (expand-file-name "urls.json" e2e-dir) nil 'silent)
(while t (accept-process-output nil 1))

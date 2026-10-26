;; -*- lexical-binding: t -*-
;; One Emacs serving the herd page with its documents (herd-docs-sandbox.js):
;; the page's own ui/ from this checkout, and beside it what the herd
;; serves it -- the Docs (a set :sandbox, by exact names) and the HTML
;; reports a reply named (a set :sandbox, by ids) -- as ghostherd says
;; it (`ghostherd-memory-page--serve-files'), from E2E_DIR's docs/ and
;; replies/; the Docs tab's Reports as the sidecar would list them
;; (reports.json: the newest page, and the one older).  xwapp alone is
;; loaded: the herd's own Elisp is the ERT suite's.  Every request and its answer is noted, and the page the app
;; has, so the test can see what reached Emacs and what changed.
;; Started by appslib.js, as apps.el is.
(require 'cl-lib)
(defvar e2e-here (file-name-directory (or load-file-name buffer-file-name)))
(defvar e2e-dir (getenv "E2E_DIR"))
(defvar e2e-local (or (getenv "E2E_LOCAL") (expand-file-name "../.." e2e-here)))
(add-to-list 'load-path (expand-file-name "xwapp" e2e-local))
(require 'xwapp)
(setq xwapp--keep t
      xwapp-state-file (expand-file-name "state.eld" e2e-dir)
      xwapp--gone-after 1.5
      xwapp--hold 2
      xwapp-port (string-to-number (getenv "E2E_PORT"))
      xwapp-remote-hosts '("box.tail0.ts.net")
      xwapp-remote-users '("me@example.com")
      xwapp-remote-socket (expand-file-name "remote.sock" e2e-dir)
      xwapp-browse-function #'ignore)

(defun e2e-note (obj)
  (write-region (concat (json-encode (list :at (float-time) :what obj)) "\n")
                nil (expand-file-name "log.jsonl" e2e-dir) t 'silent))

(defvar e2e-docs (expand-file-name "docs/" e2e-dir))
(defvar e2e-replies (expand-file-name "replies/" e2e-dir))

(defun e2e-docs-list ()
  "The Docs, as the page gets them (`ghostherd-memory-page--send-docs')."
  (vconcat
   (mapcar (lambda (name)
             (let ((file (expand-file-name name e2e-docs)))
               (list :name name
                     :title (with-temp-buffer
                              (insert-file-contents file)
                              (goto-char (point-min))
                              (if (re-search-forward "<title>\\([^<]*\\)</title>" nil t) (match-string 1) name))
                     :where "ghostherd"
                     :bytes (file-attribute-size (file-attributes file))
                     :mtime (float-time (file-attribute-modification-time (file-attributes file))))))
           (directory-files e2e-docs nil "\\.html\\'"))))

(defun e2e-reports (&optional after)
  "The Docs tab's Reports, as `ghostherd-memory-page--send-reports' gives
them: the newest page, or the one older than AFTER."
  (let* ((all (json-parse-string
               (with-temp-buffer
                 (insert-file-contents (expand-file-name "reports.json" e2e-dir))
                 (buffer-string))
               :object-type 'plist :array-type 'array :false-object :json-false :null-object nil))
         (page (plist-get all (if after :older :first))))
    (xwapp-js e2e-app "renderReports"
              (list :reports (plist-get page :reports) :more (plist-get page :more)
                    :after (or after :json-false)))))

(defvar e2e-app nil)
(defun e2e-reopen () (e2e-note "reopened"))
(setq e2e-app
      (xwapp-create :buffer-name "*herd*"
                    :index (expand-file-name "ghostherd/ui/index.html" e2e-local)
                    :prefixes '("ghmem:") :namespace "GM" :idle-title "Herd"
                    :handler (lambda (intent)
                               (e2e-note (list :intent intent))
                               (pcase (alist-get 'op intent)
                                 ("docs" (xwapp-js e2e-app "renderDocs" (list :docs (e2e-docs-list)))
                                  (e2e-reports))
                                 ("reports-more" (e2e-reports (alist-get 'id intent)))))
                    :reopen #'e2e-reopen))

;; As ghostherd serves them.
(xwapp-serve-files e2e-app e2e-replies "[0-9a-f]\\{32\\}\\(?:\\.t\\.jpg\\|\\.png\\|\\.jpg\\|\\.gif\\|\\.webp\\)")
(xwapp-serve-files e2e-app (lambda () (list e2e-docs)) "[A-Za-z0-9][A-Za-z0-9._-]*\\.html"
                   :as "docs" :sandbox t :exact t :fresh t :max-bytes (* 8 1024 1024))
(xwapp-serve-files e2e-app e2e-replies "[0-9a-f]\\{32\\}\\.html"
                   :as "reports" :sandbox t :max-bytes (* 8 1024 1024))

;; What reached Emacs, and what it said.
(advice-add 'xwapp--answer :before
            (lambda (proc request transport)
              (process-put proc 'e2e-req (list (nth 0 request) (nth 1 request) transport))))
(advice-add 'xwapp--respond :before
            (lambda (proc code &rest _)
              (when-let* ((req (process-get proc 'e2e-req)))
                (process-put proc 'e2e-req nil)
                (e2e-note (list :req (nth 0 req) :target (nth 1 req)
                                :transport (symbol-name (nth 2 req)) :code code)))))
;; Whose the page is, now and then.
(run-at-time 0 0.2 (lambda ()
                     (write-region (format "%S" (xwapp--get e2e-app :page)) nil
                                   (expand-file-name "page.txt" e2e-dir) nil 'silent)))

(write-region (json-encode (list (cons "herd" (xwapp-url e2e-app))
                                 (cons "secret" (xwapp--secret e2e-app))))
              nil (expand-file-name "urls.json" e2e-dir) nil 'silent)
(while t (accept-process-output nil 1))

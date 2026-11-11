;; -*- lexical-binding: t -*-
;; One Emacs serving the herd page to "your phone" (wake.js), as xwapp
;; serves it: from a copy of this checkout's ghostherd/ui and xwapp/ui
;; under E2E_DIR/tree, which the test may edit -- a ui/ file changed
;; while the phone slept.  xwapp alone is loaded: the herd's own Elisp
;; is the ERT suite's.  Its handler plays the herd's Emacs as far as the
;; page needs: the sessions on ready, the herd on herd-watch -- whatever
;; E2E_DIR/herd.json holds when asked, so the test says what is new --
;; and each intent noted, with the page the app has, so the test sees
;; what reached Emacs.  Started by appslib.js, as apps.el is.
(require 'cl-lib)
(defvar e2e-here (file-name-directory (or load-file-name buffer-file-name)))
(defvar e2e-dir (getenv "E2E_DIR"))
(defvar e2e-local (or (getenv "E2E_LOCAL") (expand-file-name "../.." e2e-here)))
(defvar e2e-tree (expand-file-name "tree/" e2e-dir))
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

(defun e2e-send-herd ()
  "Send the page the herd, herd.json as it is now, as Emacs would send
it (GM.renderHerd), its JSON as written."
  (xwapp--call e2e-app (format "{\"ns\":\"GM\",\"fn\":\"renderHerd\",\"arg\":%s}"
                               (with-temp-buffer
                                 (insert-file-contents (expand-file-name "herd.json" e2e-dir))
                                 (buffer-string)))))

(defvar e2e-app nil)
(defun e2e-reopen () (e2e-note "reopened"))
(setq e2e-app
      (xwapp-create :buffer-name "*herd*"
                    :index (expand-file-name "ghostherd/ui/index.html" e2e-tree)
                    :prefixes '("ghmem:") :namespace "GM" :idle-title "Herd"
                    ;; As ghostherd's own: its scripts go to a phone lean.
                    :lean t
                    :handler (lambda (intent)
                               (e2e-note (list :intent intent))
                               (pcase (alist-get 'op intent)
                                 ("ready" (xwapp-js e2e-app "setContext"
                                                    (list :sources [] :total 0 :current_project "" :view ""
                                                          :query "" :project "")))
                                 ("herd-watch" (e2e-send-herd))))
                    :on-kill (lambda () (e2e-note "closed"))
                    :reopen #'e2e-reopen))

;; What reached Emacs, and what it said.
(advice-add 'xwapp--answer :before
            (lambda (proc request transport)
              (process-put proc 'e2e-req (list (nth 0 request) (nth 1 request) transport))))
(advice-add 'xwapp--respond :before
            (lambda (proc code &rest _)
              (when-let* ((req (process-get proc 'e2e-req)))
                (process-put proc 'e2e-req nil)
                (e2e-note (list :req (nth 0 req) :target (replace-regexp-in-string "[0-9a-f]\\{32\\}" "SECRET" (nth 1 req))
                                :code code)))))
;; Whose the page is, now and then.
(run-at-time 0 0.1 (lambda ()
                     (write-region (format "%S" (xwapp--get e2e-app :page)) nil
                                   (expand-file-name "page.txt" e2e-dir) nil 'silent)))

(write-region (json-encode (list (cons "herd" (xwapp-url e2e-app))
                                 (cons "secret" (xwapp--secret e2e-app))))
              nil (expand-file-name "urls.json" e2e-dir) nil 'silent)
(while t (accept-process-output nil 1))

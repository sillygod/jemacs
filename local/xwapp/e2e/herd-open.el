;; -*- lexical-binding: t -*-
;; One Emacs serving the herd page as the herd does -- ghostherd's own
;; page code (ghostherd-memory-page.el) behind xwapp -- for timing how a
;; phone opens it (herd-open.js).  Only the sidecar is played: its
;; answers come from here, as big as a herd of some months has them (928
;; sessions, as many as Jing's index held on 2026-11-03, made up of the
;; same fields at the same lengths), each after a few milliseconds, as
;; the sidecar's do on loopback.  The Inbox is 25 items, reports with
;; their whole text among them.  Every request that reaches Emacs, and
;; every `next' answered -- its calls and its bytes -- is noted in
;; log.jsonl.  Started by appslib.js, as apps.el is.
(require 'cl-lib)
(defvar e2e-here (file-name-directory (or load-file-name buffer-file-name)))
(defvar e2e-dir (getenv "E2E_DIR"))
(defvar e2e-local (or (getenv "E2E_LOCAL") (expand-file-name "../.." e2e-here)))
(add-to-list 'load-path (expand-file-name "xwapp" e2e-local))
(add-to-list 'load-path (expand-file-name "ghostherd" e2e-local))
;; Nothing of the user's: ghostherd keeps its files under here.
(setq user-emacs-directory (file-name-as-directory (expand-file-name "emacs.d" e2e-dir)))
(make-directory user-emacs-directory t)
(require 'xwapp)
(setq xwapp--keep t
      xwapp-state-file (expand-file-name "state.eld" e2e-dir)
      xwapp-port (string-to-number (getenv "E2E_PORT"))
      xwapp-remote-hosts '("box.tail0.ts.net")
      xwapp-remote-users '("me@example.com")
      xwapp-remote-socket (expand-file-name "remote.sock" e2e-dir)
      xwapp-browse-function #'ignore)
(require 'ghostherd)
(require 'ghostherd-memory-page)
(setq ghostherd-log-file nil)

(defun e2e-note (obj)
  (write-region (concat (json-encode (list :at (float-time) :what obj)) "\n")
                nil (expand-file-name "log.jsonl" e2e-dir) t 'silent))

;; The sidecar, played: no process, no port.
(advice-add 'ghostherd-memory-ensure :override #'always)

(defun e2e-sources (n)
  "N sessions as the sidecar lists them, at the lengths a real index has."
  (let ((agents ["claude" "claude" "agy" "grok"]) out)
    (dotimes (i n)
      (let* ((project (format "/Users/someone/src/project-%02d-%s" (% i 109) (make-string 24 ?p)))
             (sid (format "%08x-%04x-%04x-%04x-%012x" (* i 7919) i (* 3 i) (* 5 i) (* 11 i)))
             (memory (zerop (% i 3))))
        (push (list :source_path (format "/Users/someone/.claude/projects/%s/%s.jsonl"
                                         (replace-regexp-in-string "/" "-" project) sid)
                    :mtime (- 1793700000.0 (* i 3600.5))
                    :size (+ 1000 (* i 37))
                    :agent (aref agents (% i 4))
                    :session_id sid
                    :project project
                    :kind (if memory "memory" "transcript")
                    :chunks (1+ (% i 40))
                    :imported_at "2026-11-03T11:05:30.741464+00:00"
                    :title (format "Session %d" i))
              out)))
    (vconcat (nreverse out))))

(defvar e2e-sessions (e2e-sources 928))

(defun e2e-sidecar (method callback &optional params _error-callback)
  "Answer METHOD as the sidecar would, a few milliseconds later."
  (e2e-note (list :rpc method :limit (plist-get params :limit)))
  (let ((result
         (pcase method
           ("memory_list" (list :sources e2e-sessions :total (length e2e-sessions)))
           ("herd_pending_proposals" (list :proposals []))
           ("herd_inbox_state" (list :state []))
           ("herd_threads" (list :live 3 :threads []))
           (_ nil))))
    ;; Through JSON, as the answer comes: its parse is Emacs's to do.
    (let ((text (json-serialize (list :jsonrpc "2.0" :id 1 :result (or result (list :ok t)))
                                :null-object nil :false-object :json-false)))
      (run-at-time 0.005 nil
                   (lambda () (funcall callback (ghostherd-memory--parse-response text))))
      nil)))
(advice-add 'ghostherd-memory-request-async :override #'e2e-sidecar)

;; The Inbox: reports and questions with their whole text, and asks.
(defun e2e-inbox-items ()
  (let (items)
    (dotimes (i 25)
      (let* ((kind (if (< i 15) 'report (if (< i 20) 'question 'answered)))
             (said (concat (format "Item %d. " i)
                           (mapconcat #'identity (make-list 30 "The herd did this and that, and here is why.") " "))))
        (push (ghostherd-inbox--item (format "%s:agent-%d:%d" kind i (* 1000 i)) kind
                                     (format "agent-%d" i) "claude" (format "~/src/project-%02d/" i)
                                     (format "Title of item %d" i) (- (float-time) (* i 60))
                                     :state "idle" :whyLabel "It reported" :body said
                                     :preview (substring said 0 90) :replyVia "agent")
              items)))
    (nreverse items)))
(advice-add 'ghostherd-inbox--all :override (lambda (&rest _) (e2e-inbox-items)))

;; What reached Emacs, and when; what each `next' carried.
(advice-add 'xwapp--answer :before
            (lambda (_proc request _transport)
              (e2e-note (list :req (nth 0 request) :target (replace-regexp-in-string
                                                           "/[0-9a-f]\\{32\\}/" "/SECRET/" (nth 1 request))))))
(advice-add 'xwapp--intent :before
            (lambda (_app _proc _page body)
              (e2e-note (list :intent (ignore-errors (alist-get 'op (json-parse-string body :object-type 'alist)))))))
(advice-add 'xwapp--flush :before
            (lambda (app)
              (when (xwapp--get app :waiting)
                (let ((calls (reverse (xwapp--get app :outbox))))
                  (e2e-note (list :flush (vconcat
                                          (mapcar (lambda (c)
                                                    (list :fn (if (string-match "\"fn\":\"\\([^\"]+\\)\"" c)
                                                                  (match-string 1 c)
                                                                (if (string-match "\"xwapp\":\"\\([^\"]+\\)\"" c)
                                                                    (concat "xwapp:" (match-string 1 c))
                                                                  "?"))
                                                          :bytes (string-bytes c)))
                                                  calls))))))))

(write-region (json-encode (list (cons "herd" (xwapp-url ghostherd-memory-page--app))
                                 (cons "secret" (xwapp--secret ghostherd-memory-page--app))))
              nil (expand-file-name "urls.json" e2e-dir) nil 'silent)
(while t (accept-process-output nil 1))

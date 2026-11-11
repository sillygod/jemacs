;; -*- lexical-binding: t -*-
;; One Emacs serving the herd page as the herd does -- ghostherd's own
;; page code behind xwapp -- for the Agents tab's handoff caps
;; (herd-slots.js): what the page sends goes to Emacs's own handler,
;; which takes a cap only when sane and saves it with
;; customize-save-variable, into a custom-file of this run's.  The
;; sidecar is played: the leads at work and up, and the handoffs waiting,
;; are as its tick would say them.  Nothing of the user's is read or
;; written: Emacs's files, custom-file and the kept handoff leads are all
;; under this run's directory, and nothing reaches a CLI or the network.
(require 'cl-lib)
(defvar e2e-here (file-name-directory (or load-file-name buffer-file-name)))
;; First: what of your own state this Emacs would still reach, noted (its
;; HOME is the run's: appslib's `guardEnv').
(load (expand-file-name "e2e-guard" e2e-here) nil t)
(defvar e2e-dir (getenv "E2E_DIR"))
(defvar e2e-local (or (getenv "E2E_LOCAL") (expand-file-name "../.." e2e-here)))
(add-to-list 'load-path (expand-file-name "xwapp" e2e-local))
(add-to-list 'load-path (expand-file-name "ghostherd" e2e-local))
(setq user-emacs-directory (file-name-as-directory (expand-file-name "emacs.d" e2e-dir)))
(make-directory user-emacs-directory t)
;; The herd's own state, under ~/.ghostherd, the run's too -- its HOME is
;; already (appslib's guardEnv); said here for one run another way.
(setq ghostherd-handoff-leads-file (expand-file-name "ghostherd/handoff-leads.eld" e2e-dir)
      ghostherd-worktree-root (expand-file-name "ghostherd/worktrees" e2e-dir)
      ghostherd-memory-dir (expand-file-name "ghostherd/memory" e2e-dir)
      ghostherd-guard-root (expand-file-name "ghostherd/guard/" e2e-dir)
      ghostherd-primary-directory (expand-file-name "ghostherd/primary/" e2e-dir))
(require 'xwapp)
(setq xwapp--keep t
      xwapp-state-file (expand-file-name "state.eld" e2e-dir)
      xwapp-port (string-to-number (getenv "E2E_PORT"))
      xwapp-remote-hosts '("box.tail0.ts.net")
      xwapp-remote-users '("me@example.com")
      xwapp-remote-socket (expand-file-name "remote.sock" e2e-dir)
      xwapp-browse-function #'ignore
      xwapp-tailscale-program nil)
(require 'ghostherd)
(require 'ghostherd-memory-page)
(setq ghostherd-log-file nil
      ghostherd-handoff-leads-file (expand-file-name "handoff-leads.eld" e2e-dir)
      ;; Saved for good, as in a user's Emacs: into this run's custom-file.
      custom-file (expand-file-name "custom.el" e2e-dir)
      user-init-file (expand-file-name "init.el" e2e-dir)
      init-file-user "")

(defun e2e-note (obj)
  (write-region (concat (json-encode (list :at (float-time) :what obj)) "\n")
                nil (expand-file-name "log.jsonl" e2e-dir) t 'silent))

;; The sidecar, played: no process, no port.  Its tick's word on the
;; handoff leads is set here; what else the page asks of it is empty.
(advice-add 'ghostherd-memory-ensure :override #'always)
(advice-add 'ghostherd-memory-request-async :override
            (lambda (method callback &optional _params _err)
              (e2e-note (list :rpc method))
              (let ((result (pcase method
                              ("herd_inbox_state" (list :state nil))
                              ("herd_pending_proposals" (list :proposals nil))
                              (_ nil))))
                (run-at-time 0.005 nil (lambda () (funcall callback result))))
              nil))
;; Nothing that looks outside: no CLI's usage, hooks, checkouts or tally.
(dolist (f '(ghostherd-usage-refresh ghostherd-memory-page--work-look ghostherd-memory-page--hooks-refresh
             ghostherd-memory-page--tally-look ghostherd-memory-page--meter-look
             ghostherd-memory-page--subs-look ghostherd-memory-page--edits-look ghostherd--herd-tick-async))
  (advice-add f :override #'ignore))

(let ((now (* 1000 (float-time))))
  (setq ghostherd--handoff-slots
        (list :working 1 :alive 3 :working_cap 3 :alive_cap 8
              :leads (list (list :name "lead-fox" :project "~/src/fox/" :state "working" :working t)
                           (list :name "lead-ghost" :project "~/src/ghost/" :state "idle" :working nil)
                           (list :name "lead-leopard" :project "~/src/leopard/" :state "idle" :working nil))
              :queue (list (list :project "/Users/someone/src/parker/" :from "lead-argocd"
                                 :from_project "/Users/someone/src/argocd/" :since_ms (- now 250000)
                                 :asks 1 :ahead 0 :starting nil :stop "" :head "Bump the chart for the new ingress")
                           (list :project "/Users/someone/src/genesis/" :from "lead-argocd"
                                 :from_project "/Users/someone/src/argocd/" :since_ms (- now 240000)
                                 :asks 2 :ahead 1 :starting nil :stop "" :head "Rotate the deploy key")))))

(advice-add 'xwapp--intent :before
            (lambda (_app _proc _page body)
              (let ((it (ignore-errors (json-parse-string body :object-type 'alist))))
                (when (equal (alist-get 'op it) "handoff-caps")
                  (e2e-note (list :intent "handoff-caps" :field (alist-get 'field it)
                                  :value (format "%S" (alist-get 'value it))))))))
;; What the page is told back about a cap: flashed, or refused and why.
(advice-add 'ghostherd-memory-page--js :before
            (lambda (fn &optional arg)
              (when (member fn '("slotsRefused" "flash"))
                (e2e-note (list :js fn :arg (format "%S" arg))))))
(run-at-time 0 0.1 (lambda ()
                     (write-region (format "%S" (list ghostherd-handoff-lead-max ghostherd-handoff-lead-alive-max))
                                   nil (expand-file-name "caps.txt" e2e-dir) nil 'silent)))

(write-region (json-encode (list (cons "herd" (xwapp-url ghostherd-memory-page--app))
                                 (cons "secret" (xwapp--secret ghostherd-memory-page--app))))
              nil (expand-file-name "urls.json" e2e-dir) nil 'silent)
(when noninteractive
  (while t (accept-process-output nil 1)))

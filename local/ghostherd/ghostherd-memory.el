;;; ghostherd-memory.el --- Shared transcript memory via a Python sidecar -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jing

;; This file is part of ghostherd.

;;; Commentary:

;; Search past claude / grok / agy sessions from Emacs.  The CLIs already
;; keep transcripts; this does not merge those stores.  A local Python
;; process (FastAPI JSON-RPC, the same shape as ecloud) imports them into
;; Qdrant with FastEmbed and answers search over HTTP.
;;
;; The sidecar is persistent because FastEmbed's model load is slow.  It
;; is not started by `ghostherd-mode': the first memory command starts it.
;; Bind loopback only; there is no transport auth.

;;; Code:

(require 'json)
(require 'url)
(require 'subr-x)
(require 'project)

(defgroup ghostherd-memory nil
  "Shared memory sidecar for ghostherd."
  :group 'ghostherd
  :prefix "ghostherd-memory-")

(defcustom ghostherd-memory-port 49152
  "Preferred loopback port, and the start of the search if it is taken.

IANA dynamic/private ports begin here, well above ecloud (8765) and
the usual local-dev crowd.  When this port is occupied by something
that is not the sidecar, the next `ghostherd-memory-port-tries'
ports are tried.  Set `ghostherd-memory-port-tries' to 1 to pin it."
  :type 'integer
  :group 'ghostherd-memory)

(defcustom ghostherd-memory-port-tries 32
  "How many consecutive ports to try, starting at `ghostherd-memory-port'."
  :type 'integer
  :group 'ghostherd-memory)

(defcustom ghostherd-memory-host "127.0.0.1"
  "Host the sidecar binds to."
  :type 'string
  :group 'ghostherd-memory)

(defcustom ghostherd-memory-server-directory nil
  "Directory containing the sidecar's main.py.

Nil means find it: the checkout next to the real `ghostherd.el'
(straight leaves a symlink), or `server/' beside the library.
ecloud does not need a matching custom because its `:files'
already copies `server/' into the straight build; this is only a
fallback if both lookups miss."
  :type '(choice (const nil) directory)
  :group 'ghostherd-memory)

(defcustom ghostherd-memory-data-directory nil
  "Where Qdrant and the import index live.
Nil means `ghostherd-memory/' under `user-emacs-directory'."
  :type '(choice (const nil) directory)
  :group 'ghostherd-memory)

(defcustom ghostherd-memory-request-timeout 60
  "Timeout in seconds for search / status RPCs.
The first call after a cold start may load FastEmbed."
  :type 'integer
  :group 'ghostherd-memory)

(defcustom ghostherd-memory-import-timeout 600
  "Timeout in seconds for `memory_import'."
  :type 'integer
  :group 'ghostherd-memory)

(defcustom ghostherd-memory-startup-timeout 45
  "Seconds to wait for /health after spawning uvicorn."
  :type 'integer
  :group 'ghostherd-memory)

(defcustom ghostherd-memory-search-limit 8
  "Default number of hits to return."
  :type 'integer
  :group 'ghostherd-memory)

(defvar ghostherd-memory--process nil)
(defvar ghostherd-memory--port nil
  "Port the sidecar is actually bound to in this Emacs.
Nil until a start/reuse; URLs read this rather than the custom, which
is only the *preferred* first try.")
(defvar ghostherd-memory--request-id 0)

(defun ghostherd-memory--elisp-roots ()
  "Directories that may sit next to `server/'.

straight.el compiles into `straight/build/ghostherd/*.elc'.  The `.el'
there is a symlink back to the checkout, which is where `server/'
actually lives unless `:files' copied it into the build (ecloud does
that; ghostherd did not, which is why `locate-library' alone missed
main.py).  Prefer the truename of the `.el' so `uv run' uses the
checkout that already has `.venv'."
  (let* ((files (list (locate-library "ghostherd-memory")
                      (locate-library "ghostherd")
                      load-file-name
                      buffer-file-name))
         (roots nil))
    (dolist (file files)
      (when (and file (stringp file))
        (let* ((dir (file-name-directory file))
               (el (expand-file-name "ghostherd.el" dir)))
          (push dir roots)
          ;; Checkout last-pushed = first tried, so uv sees the .venv
          ;; from `cd server && uv sync', not a copy without one.
          (when (file-exists-p el)
            (push (file-name-directory (file-truename el)) roots)))))
    (delete-dups roots)))

(defun ghostherd-memory--server-directory ()
  "Return the sidecar directory, or nil if main.py is not there.

Search order:
1. `ghostherd-memory-server-directory' if it points at main.py
2. `server/' next to the real ghostherd.el (straight symlink → checkout)
3. `server/' next to the library dir (straight `:files (\"server\")`)"
  (catch 'found
    (when ghostherd-memory-server-directory
      (let ((dir (expand-file-name ghostherd-memory-server-directory)))
        (when (file-exists-p (expand-file-name "main.py" dir))
          (throw 'found dir))))
    (dolist (root (ghostherd-memory--elisp-roots))
      (let ((dir (expand-file-name "server" root)))
        (when (file-exists-p (expand-file-name "main.py" dir))
          (throw 'found dir))))))

(defun ghostherd-memory--data-directory ()
  (expand-file-name
   (or ghostherd-memory-data-directory
       (expand-file-name "ghostherd-memory" user-emacs-directory))))

(defun ghostherd-memory--current-port ()
  "Port RPC should use: the bound one, else the preferred start."
  (or ghostherd-memory--port ghostherd-memory-port))

(defun ghostherd-memory--port-candidates ()
  "Preferred port, then the next `ghostherd-memory-port-tries' - 1."
  (let* ((start ghostherd-memory-port)
         (n (max 1 ghostherd-memory-port-tries))
         (last (min 65535 (+ start n -1))))
    (number-sequence start last)))

(defun ghostherd-memory--rpc-url (&optional port)
  (format "http://%s:%d/jsonrpc"
          ghostherd-memory-host
          (or port (ghostherd-memory--current-port))))

(defun ghostherd-memory--health-url (&optional port)
  (format "http://%s:%d/health"
          ghostherd-memory-host
          (or port (ghostherd-memory--current-port))))

(defun ghostherd-memory--listening-p (port)
  "Return non-nil if something accepts TCP on the sidecar host:PORT.
A refused connect means we can try to bind it.  Not a bind probe: the
gap until uvicorn listens is closed by retrying on a dead process."
  (let ((proc (ignore-errors
                (make-network-process
                 :name "ghostherd-memory-probe"
                 :buffer nil
                 :host ghostherd-memory-host
                 :service port
                 :nowait nil))))
    (when proc
      (delete-process proc)
      t)))

(defun ghostherd-memory--health-body-ours-p (body)
  "Return non-nil if BODY is this sidecar's /health JSON."
  (let* ((json-object-type 'plist)
         (json-array-type 'list)
         (json-key-type 'keyword)
         (json-false nil)
         (json-null nil)
         (data (ignore-errors (json-read-from-string body))))
    (and data (equal (plist-get data :service) "ghostherd-memory"))))

(defun ghostherd-memory--health-ours-p (&optional port)
  "Return non-nil if PORT (or the current one) answers as this sidecar.
A 200 from some other local service must not count: we would then
reuse the wrong process instead of walking to a free port."
  (let ((url (ghostherd-memory--health-url port)))
    (condition-case _
        (let ((buf (url-retrieve-synchronously url nil nil 2)))
          (when buf
            (unwind-protect
                (with-current-buffer buf
                  (goto-char (point-min))
                  (and (re-search-forward "HTTP/[0-9.]+ 200" nil t)
                       (ghostherd-memory--health-body-ours-p
                        (ghostherd-memory--body-from-url-buffer))))
              (kill-buffer buf))))
      (error nil))))

(defun ghostherd-memory--find-running ()
  "Return a candidate port that already speaks ghostherd-memory, or nil."
  (let ((found nil)
        (ports (ghostherd-memory--port-candidates)))
    (while (and ports (not found))
      (let ((port (pop ports)))
        (when (and (ghostherd-memory--listening-p port)
                   (ghostherd-memory--health-ours-p port))
          (setq found port))))
    found))

(defun ghostherd-memory--allocate-port ()
  "First candidate that nothing is listening on, or nil."
  (let ((found nil)
        (ports (ghostherd-memory--port-candidates)))
    (while (and ports (not found))
      (let ((port (pop ports)))
        (unless (ghostherd-memory--listening-p port)
          (setq found port))))
    found))

(defun ghostherd-memory--next-id ()
  (setq ghostherd-memory--request-id (1+ ghostherd-memory--request-id)))

(defun ghostherd-memory--build-request (method params)
  (let ((request (list :jsonrpc "2.0"
                       :id (ghostherd-memory--next-id)
                       :method method)))
    (when params
      (setq request (plist-put request :params params)))
    request))

(defun ghostherd-memory--parse-response (response-string)
  "Parse RESPONSE-STRING as a JSON-RPC body.  Signal on error."
  (let* ((json-object-type 'plist)
         (json-array-type 'list)
         (json-key-type 'keyword)
         (json-false nil)
         (json-null nil)
         (response (json-read-from-string response-string))
         (error-obj (plist-get response :error)))
    (if error-obj
        (error "ghostherd-memory JSON-RPC %s: %s"
               (plist-get error-obj :code)
               (plist-get error-obj :message))
      (plist-get response :result))))

(defun ghostherd-memory--body-from-url-buffer ()
  (goto-char (point-min))
  (re-search-forward "\r?\n\r?\n" nil t)
  (buffer-substring-no-properties (point) (point-max)))

(defun ghostherd-memory-healthy-p ()
  "Return non-nil if the current port answers as this sidecar."
  (ghostherd-memory--health-ours-p))

(defun ghostherd-memory-request-async (method callback &optional params error-callback)
  "POST METHOD asynchronously; CALLBACK gets the JSON-RPC result.
Import is CPU-heavy and must not freeze Emacs on `url-retrieve-synchronously'."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string
                            (json-encode
                             (ghostherd-memory--build-request method params))
                            'utf-8)))
    (url-retrieve
     (ghostherd-memory--rpc-url)
     (lambda (status callback error-callback)
       (let ((err (plist-get status :error)))
         (cond
          (err
           (if error-callback
               (funcall error-callback (format "%s" err))
             (message "ghostherd-memory: %s" err)))
          (t
           (goto-char (point-min))
           (re-search-forward "\r?\n\r?\n" nil t)
           (let ((body (buffer-substring-no-properties (point) (point-max))))
             (condition-case parse-err
                 (funcall callback (ghostherd-memory--parse-response body))
               (error
                (if error-callback
                    (funcall error-callback (error-message-string parse-err))
                  (message "ghostherd-memory: %s"
                           (error-message-string parse-err))))))))))
     (list callback error-callback)
     t t)))

(defun ghostherd-memory-request (method &optional params timeout)
  "Synchronous JSON-RPC METHOD with PARAMS plist.
TIMEOUT defaults to `ghostherd-memory-request-timeout'."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string
                            (json-encode
                             (ghostherd-memory--build-request method params))
                            'utf-8))
         (timeout (or timeout ghostherd-memory-request-timeout))
         (buf (url-retrieve-synchronously
               (ghostherd-memory--rpc-url) nil nil timeout)))
    (unless buf
      (error "ghostherd-memory: no response from %s" (ghostherd-memory--rpc-url)))
    (unwind-protect
        (with-current-buffer buf
          (ghostherd-memory--parse-response
           (ghostherd-memory--body-from-url-buffer)))
      (kill-buffer buf))))

(defun ghostherd-memory--process-live-p ()
  (and ghostherd-memory--process
       (process-live-p ghostherd-memory--process)))

(defun ghostherd-memory--spawn (dir port)
  "Start uvicorn in DIR on PORT.  Set `ghostherd-memory--process'."
  (when (ghostherd-memory--process-live-p)
    (delete-process ghostherd-memory--process)
    (setq ghostherd-memory--process nil))
  (let* ((default-directory dir)
         (data (ghostherd-memory--data-directory))
         (process-environment
          (append
           (list (format "GHOSTHERD_MEMORY_HOST=%s" ghostherd-memory-host)
                 (format "GHOSTHERD_MEMORY_PORT=%d" port)
                 (format "GHOSTHERD_MEMORY_DIR=%s" data))
           process-environment))
         (buf (get-buffer-create "*ghostherd-memory*"))
         (proc (start-process
                "ghostherd-memory" buf
                "uv" "run" "uvicorn" "main:app"
                "--host" ghostherd-memory-host
                "--port" (number-to-string port))))
    (setq ghostherd-memory--process proc
          ghostherd-memory--port port)
    (set-process-query-on-exit-flag proc nil)
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (format "\n[%s] uv run uvicorn main:app --host %s --port %d\n"
                      (format-time-string "%F %T")
                      ghostherd-memory-host
                      port)))
    proc))

(defun ghostherd-memory-start ()
  "Start the sidecar if it is not already healthy.
Reuses a leftover sidecar in the port range.  Occupied ports that are
not ours are skipped; a bind race that kills uvicorn tries the next."
  (interactive)
  (unless (executable-find "uv")
    (user-error "ghostherd-memory: `uv' is not on PATH"))
  (let ((existing (or (and (ghostherd-memory-healthy-p)
                           (ghostherd-memory--current-port))
                      (ghostherd-memory--find-running))))
    (if existing
        (progn
          (setq ghostherd-memory--port existing)
          (message "ghostherd-memory: already running at %s"
                   (ghostherd-memory--rpc-url))
          t)
      (let ((dir (ghostherd-memory--server-directory)))
        (unless dir
          (user-error "ghostherd-memory: cannot find server/main.py next to ghostherd.el"))
        (let ((ok nil)
              (last-wait nil)
              (ports (ghostherd-memory--port-candidates)))
          (while (and ports (not ok))
            (let ((port (pop ports)))
              (cond
               ((ghostherd-memory--listening-p port)
                ;; Occupied: reuse only if it became ours since the scan.
                (when (ghostherd-memory--health-ours-p port)
                  (setq ghostherd-memory--port port
                        ok t)))
               (t
                (ghostherd-memory--spawn dir port)
                (setq last-wait (ghostherd-memory--wait-for-health
                                 ghostherd-memory-startup-timeout))
                (cond
                 ((eq last-wait 'ok) (setq ok t))
                 ((eq last-wait 'timeout)
                  (setq ports nil))
                 (t
                  (setq ghostherd-memory--process nil
                        ghostherd-memory--port nil)))))))
          (unless ok
            (if (eq last-wait 'timeout)
                (user-error "ghostherd-memory: timed out waiting for /health (see *ghostherd-memory*)")
              (user-error "ghostherd-memory: no free port in %d–%d (see *ghostherd-memory*)"
                          ghostherd-memory-port
                          (car (last (ghostherd-memory--port-candidates))))))
          (message "ghostherd-memory: ready on %s" (ghostherd-memory--rpc-url))
          t)))))

(defun ghostherd-memory--wait-for-health (timeout)
  "Wait until /health is ours, the process dies, or TIMEOUT seconds.
Return `ok', `died' or `timeout'.  A dead process is a bind failure and
should try the next port; a live process that never health-checks is
stuck, not a reason to burn the range."
  (let ((deadline (time-add (current-time) timeout))
        (result nil))
    (while (and (not result)
                (time-less-p (current-time) deadline))
      (cond
       ((ghostherd-memory-healthy-p)
        (setq result 'ok))
       ((not (ghostherd-memory--process-live-p))
        (setq result 'died))
       (t
        (accept-process-output ghostherd-memory--process 0.4))))
    (or result (if (ghostherd-memory-healthy-p) 'ok 'timeout))))

(defun ghostherd-memory-stop ()
  "Stop the sidecar process this Emacs started."
  (interactive)
  (when (ghostherd-memory--process-live-p)
    (delete-process ghostherd-memory--process))
  (setq ghostherd-memory--process nil
        ghostherd-memory--port nil)
  (message "ghostherd-memory: stopped"))

(defun ghostherd-memory-ensure ()
  "Start the sidecar unless /health already answers.
A leftover uvicorn in the port range is reused.  A live process that
is not answering is killed inside `ghostherd-memory--spawn' before a
new bind — the ecloud rule: do not tear down a slow RPC that is still
healthy."
  (cond
   ((ghostherd-memory-healthy-p) t)
   (t
    (if-let* ((found (ghostherd-memory--find-running)))
        (progn
          (setq ghostherd-memory--port found)
          t)
      (ghostherd-memory-start)))))

;;;###autoload
(defun ghostherd-memory-status ()
  "Show sidecar status."
  (interactive)
  (ghostherd-memory-ensure)
  (let ((st (ghostherd-memory-request "memory_status")))
    (message "ghostherd-memory: %d points, model %s%s"
             (or (plist-get st :points) 0)
             (or (plist-get st :embed_model) "?")
             (if (plist-get st :fake_embed) " (fake)" ""))
    st))

;;;###autoload
(defun ghostherd-memory-import (&optional force)
  "Import claude / grok / agy transcripts into the sidecar.
Prefix argument FORCE re-embeds sources that have not changed.

This is CPU-heavy (FastEmbed on every new chunk) and used to freeze
Emacs on a 10-minute synchronous HTTP wait.  It now returns at once;
progress is printed in `*ghostherd-memory*'."
  (interactive "P")
  (ghostherd-memory-ensure)
  (when-let* ((buf (get-buffer "*ghostherd-memory*")))
    (display-buffer buf))
  (message "ghostherd-memory: import running in the sidecar. Watch *ghostherd-memory*; Emacs is not frozen.")
  (ghostherd-memory-request-async
   "memory_import"
   (lambda (result)
     (if (plist-get result :busy)
         (message "ghostherd-memory: import already running (%s)"
                  (or (plist-get result :current) "…"))
       (let ((errors (plist-get result :errors)))
         (message "ghostherd-memory: imported %s chunks from %s sessions (skipped %s)%s"
                  (plist-get result :imported)
                  (plist-get result :sessions)
                  (plist-get result :skipped)
                  (if errors
                      (format ", %d errors" (length errors))
                    "")))))
   (and force (list :force t))
   (lambda (err) (message "ghostherd-memory: import failed: %s" err))))

(defun ghostherd-memory--show-hits (query hits)
  (let ((buf (get-buffer-create "*ghostherd memory*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "ghostherd memory — %s  (%d hits)\n\n"
                        query (length hits)))
        (dolist (hit hits)
          (insert (format "[%s] %s  %s  %s\n"
                          (or (plist-get hit :agent) "?")
                          (or (plist-get hit :project) "")
                          (or (plist-get hit :role) "")
                          (let ((title (plist-get hit :title)))
                            (if (and title (not (string-empty-p title)))
                                title
                              (or (plist-get hit :session_id) "")))))
          (insert (or (plist-get hit :text) ""))
          (unless (bolp) (insert "\n"))
          (insert "\n" (make-string 60 ?-) "\n\n"))
        (goto-char (point-min))
        (read-only-mode 1)))
    (pop-to-buffer buf)))

;;;###autoload
(defun ghostherd-memory-search (query &optional project)
  "Search imported transcripts for QUERY.
With a prefix argument, restrict to the current project.  The default
is all projects — that is the point of the feature."
  (interactive
   (list (read-string "Memory search: ")
         (when current-prefix-arg
           (when-let* ((p (project-current)))
             (directory-file-name (expand-file-name (project-root p)))))))
  (when (string-empty-p (string-trim query))
    (user-error "Empty query"))
  (ghostherd-memory-ensure)
  (let* ((params (list :query query :limit ghostherd-memory-search-limit))
         (params (if project (plist-put params :project project) params))
         (result (ghostherd-memory-request "memory_search" params))
         (hits (plist-get result :hits)))
    (unless hits
      (user-error "No memory hits for %s" query))
    (ghostherd-memory--show-hits query hits)
    result))

(defun ghostherd-cmd-memory-search (query &optional limit &rest _)
  "JSON search for agent shells (`ghostherd memory search')."
  (ghostherd-memory-ensure)
  (let* ((params (list :query query
                       :limit (if (and limit (not (string-empty-p limit)))
                                  (string-to-number limit)
                                ghostherd-memory-search-limit)))
         (result (ghostherd-memory-request "memory_search" params)))
    (json-encode result)))

(defun ghostherd-cmd-memory-import (&optional force &rest _)
  "JSON import for agent shells."
  (ghostherd-memory-ensure)
  (json-encode
   (ghostherd-memory-request
    "memory_import"
    (and (member force '("1" "true" "force" t)) (list :force t))
    ghostherd-memory-import-timeout)))

(provide 'ghostherd-memory)
;;; ghostherd-memory.el ends here

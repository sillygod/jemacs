;;; jworkspace.el --- Lightweight workspace management -*- lexical-binding: t -*-

;; Author: jing
;; Maintainer: jing
;; Version: version
;; Keywords: workspace
;; Package-Requires: ((emacs "27.1") (cl-lib "0.5") (map "2.1"))

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Lightweight workspaces with buffer association and optional burly
;; window-state serialization.  Enable with `jworkspace-enable' (also
;; run automatically when this library loads).  `jworkspace-startup'
;; restores a save file from `window-setup-hook' when one exists.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'map)
(require 'pcase)

;; Optional deps used only when burly serialization is active.
(declare-function bookmark-make-record "bookmark" ())
(declare-function url-parse-make-urlobj "url-parse")
(declare-function url-recreate-url "url")
(declare-function burly-url-buffer "burly" (url))
(defvar burly-major-mode-alist)
(defvar burly-window-parameters-translators)

(defgroup jworkspace nil
  "Lightweight workspaces with buffer association."
  :group 'convenience
  :prefix "jworkspace-")

;; Optional burly support for URL-based window serialization.
(defvar jworkspace--burly-available nil
  "Non-nil when burly features are loadable.")

(defun jworkspace--ensure-burly ()
  "Try to load burly; set `jworkspace--burly-available'.
Return non-nil when burly is usable."
  (or jworkspace--burly-available
      (setq jworkspace--burly-available
            (and (require 'burly nil t)
                 (featurep 'burly)
                 t))))

(defcustom jworkspace-window-persistent-parameters
  (list (cons 'burly-url 'writable)
        (cons 'jworkspace-url 'writable)
        (cons 'header-line-format 'writable)
        (cons 'mode-line-format 'writable)
        (cons 'tab-line-format 'writable)
        (cons 'no-other-window 'writable)
        (cons 'no-delete-other-windows 'writable)
        (cons 'window-preserved-size 'writable)
        (cons 'window-side 'writable)
        (cons 'window-slot 'writable))
  "Additional window parameters to persist.
See Info node `(elisp)Window Parameters'."
  :type '(alist :key-type (symbol :tag "Window parameter")
                :value-type (choice (const :tag "Not saved" nil)
                                    (const :tag "Saved" writable)))
  :group 'jworkspace)

(defcustom jworkspace-kill-buffers-on-delete nil
  "If non-nil, kill associated buffers when deleting a workspace.
Default is nil (safer): associations are pruned but buffers stay alive.
Buffers still associated with another workspace are never killed."
  :type 'boolean
  :group 'jworkspace)

(defvar jworkspace-map (make-hash-table :test 'equal)
  "A hashmap to map workspace's name to itself.")

(defvar jworkspace--save-dir-path (concat user-emacs-directory ".jworkspace")
  "The dir path for saving the workspaces settings.")

(defvar jworkspace--hooks-installed nil
  "Non-nil after `jworkspace-enable' has installed hooks.")

(defconst jworkspace--save-version 1
  "Version tag written into the save file.")

(cl-defstruct (jworkspace (:constructor jworkspace-create)
                          (:copier nil))
  name
  (window-config (progn
                   (save-window-excursion
                     (delete-other-windows)
                     (when (fboundp 'show-splash-buffer)
                       (show-splash-buffer))
                     (window-state-get (frame-root-window) 'writable))))
  (buffers nil)      ; live buffers belonging to this workspace
  temp-window-config
  (saved-ids nil))   ; printable identities not yet opened



;;; Buffer association helpers

(defun jworkspace--associable-buffer-p (buffer)
  "Return non-nil when BUFFER should be tracked by a workspace."
  (and (buffer-live-p buffer)
       (not (string-prefix-p " " (buffer-name buffer)))))

(defun jworkspace-prune-buffers (&optional workspace)
  "Remove dead buffers from WORKSPACE (or every workspace if nil).
Return the pruned buffer list for WORKSPACE, or nil when pruning all."
  (if workspace
      (setf (jworkspace-buffers workspace)
            (cl-remove-if-not #'buffer-live-p (jworkspace-buffers workspace)))
    (maphash
     (lambda (_name ws)
       (setf (jworkspace-buffers ws)
             (cl-remove-if-not #'buffer-live-p (jworkspace-buffers ws))))
     jworkspace-map)
    nil))

(defun jworkspace-add-buffer (workspace buffer)
  "Associate BUFFER with WORKSPACE if it is a live buffer.
Return BUFFER when added or already present, else nil."
  (when (and workspace (jworkspace--associable-buffer-p buffer))
    (jworkspace-prune-buffers workspace)
    (unless (memq buffer (jworkspace-buffers workspace))
      (setf (jworkspace-buffers workspace)
            (cons buffer (jworkspace-buffers workspace))))
    buffer))

(defun jworkspace-remove-buffer (workspace buffer)
  "Remove BUFFER from WORKSPACE's buffer list.
Return the updated list."
  (when workspace
    (setf (jworkspace-buffers workspace)
          (delq buffer (jworkspace-buffers workspace)))))

(defun jworkspace-get-buffers (workspace)
  "Return live buffers associated with WORKSPACE.
Opens any still-pending saved identities first (quietly, without
running `find-file-hook')."
  (when workspace
    (jworkspace--open-saved-ids workspace)
    (jworkspace-prune-buffers workspace)
    (jworkspace-buffers workspace)))

(defun jworkspace--buffers-in-windows (&optional frame)
  "Return list of unique live buffers currently shown in FRAME's windows."
  (let ((frame (or frame (selected-frame)))
        (seen nil))
    (dolist (win (window-list frame 'never))
      (let ((buf (window-buffer win)))
        (when (and (jworkspace--associable-buffer-p buf)
                   (not (memq buf seen)))
          (push buf seen))))
    (nreverse seen)))

(defun jworkspace-refresh-buffers (&optional workspace)
  "Refresh WORKSPACE buffer membership from currently displayed windows.
Merge window buffers into the existing association list, then prune dead
ones.  Defaults to the current workspace.  Return the updated list."
  (let ((ws (or workspace (jworkspace--get-current-workspace))))
    (when ws
      (dolist (buf (jworkspace--buffers-in-windows))
        (jworkspace-add-buffer ws buf))
      (jworkspace-prune-buffers ws)
      (jworkspace-buffers ws))))

(defun jworkspace--on-find-file ()
  "Associate the current file buffer with the current workspace.
Intended for `find-file-hook'."
  (when-let* ((ws (jworkspace--get-current-workspace))
              (buf (current-buffer)))
    (when (buffer-file-name buf)
      (jworkspace-add-buffer ws buf))))

(defun jworkspace--on-window-buffer-change (frame)
  "Associate buffers shown on FRAME with the current workspace.
Intended for `window-buffer-change-functions' so non-file buffers
(dired, ghostel, magit) join the workspace when displayed."
  (when (eq frame (selected-frame))
    (when-let* ((ws (jworkspace--get-current-workspace)))
      (dolist (buf (jworkspace--buffers-in-windows frame))
        (jworkspace-add-buffer ws buf)))))

;;;###autoload
(defun jworkspace-switch-to-buffer ()
  "Switch among buffers associated with the current workspace."
  (interactive)
  (let* ((ws (or (jworkspace--get-current-workspace)
                 (user-error "No current workspace")))
         (bufs (jworkspace-get-buffers ws)))
    (unless bufs
      (user-error "No buffers associated with workspace '%s'"
                  (jworkspace-name ws)))
    (let* ((names (mapcar #'buffer-name bufs))
           (choice (completing-read
                    (format "Buffer in '%s': " (jworkspace-name ws))
                    names nil t)))
      (when-let* ((buf (get-buffer choice)))
        (switch-to-buffer buf)))))

;;;###autoload
(defun jworkspace-list-buffers ()
  "List buffers associated with the current workspace in a message."
  (interactive)
  (let* ((ws (or (jworkspace--get-current-workspace)
                 (user-error "No current workspace")))
         (bufs (jworkspace-get-buffers ws)))
    (if (null bufs)
        (message "Workspace '%s' has no associated buffers"
                 (jworkspace-name ws))
      (message "Workspace '%s' buffers: %s"
               (jworkspace-name ws)
               (mapconcat #'buffer-name bufs ", ")))))


;;; Core workspace helpers

(defun jworkspace-remove-nth-element (nth list)
  "Remove the NTH index's element in the LIST."
  (if (zerop nth) (cdr list)
    (let ((last (nthcdr (1- nth) list)))
      (setcdr last (cddr last))
      list)))

(defun jworkspace--get-current-workspace ()
  "Return the current workspace in frame."
  (frame-parameter (selected-frame) 'current-workspace))

(defun jworkspace--set-current-workspace (workspace)
  "Set the current WORKSPACE structure in the frame parameter."
  (set-frame-parameter (selected-frame) 'current-workspace workspace))

(defun jworkspace--rebind-current-workspace ()
  "Point the frame parameter at the same-named workspace in `jworkspace-map'.
After a load the previous struct is no longer in the map.  Return the
rebound workspace, or nil if the old name is gone."
  (let* ((old (jworkspace--get-current-workspace))
         (name (and old (jworkspace-name old)))
         (fresh (and name (gethash name jworkspace-map))))
    (jworkspace--set-current-workspace fresh)
    fresh))

(defun jworkspace--capture-window-state (&optional frame)
  "Capture window state for FRAME using burly-aware path when available.
Prefer `jworkspace--window-state' when burly is loaded; otherwise fall
back to a writable `window-state-get' of the frame root."
  (let ((frame (or frame (selected-frame))))
    (if (jworkspace--ensure-burly)
        (jworkspace--window-state frame)
      (with-selected-frame frame
        (window-state-get (frame-root-window frame) 'writable)))))

(defun jworkspace--restore-unmaximized (&optional workspace)
  "If WORKSPACE is maximized, restore the pre-maximize layout.
Clear `temp-window-config' either way so a later capture is the
real layout.  Defaults to the current workspace."
  (let ((ws (or workspace (jworkspace--get-current-workspace))))
    (when (and ws (jworkspace-temp-window-config ws))
      (when (one-window-p)
        (ignore-errors
          (window-state-put (jworkspace-temp-window-config ws)
                            (frame-root-window))))
      (setf (jworkspace-temp-window-config ws) nil))
    ws))

(defun jworkspace--restore-window-state (state &optional workspace)
  "Put window STATE into the selected frame.
Open WORKSPACE's saved buffer identities first.  When burly is
available, reincarnate buffers from `jworkspace-url'/`burly-url'
window parameters before `window-state-put'."
  (when workspace
    (jworkspace--open-saved-ids workspace))
  (let ((ready (if (and state (jworkspace--ensure-burly))
                   (jworkspace--bufferize-window-state state)
                 state)))
    (window-state-put ready (frame-root-window) 'safe)))

(defun jworkspace-new-workspace (&optional name)
  "Create a workspace with the input NAME or the prompt input.
If NAME already exists, return the existing workspace instead of
overwriting it."
  (let ((wsname (or name
                    (read-string "Enter the new workspace name: "))))
    (when (string-empty-p (string-trim wsname))
      (user-error "Workspace name cannot be empty"))
    (or (gethash wsname jworkspace-map)
        (let ((workspace (jworkspace-create :name wsname)))
          (puthash (jworkspace-name workspace) workspace jworkspace-map)
          workspace))))

;;;###autoload
(defun jworkspace-worsapce-existp (name)
  "Check the NAME if is in the workspace list."
  (member name (hash-table-keys jworkspace-map)))

(defun jworkspace--buffer-in-other-workspace-p (buffer workspace)
  "Return non-nil if BUFFER is associated with a workspace other than WORKSPACE."
  (catch 'found
    (maphash
     (lambda (_name ws)
       (unless (eq ws workspace)
         (when (memq buffer (jworkspace-buffers ws))
           (throw 'found t))))
     jworkspace-map)
    nil))

(defun jworkspace--next-workspace-name (&optional except)
  "Return a stable next workspace name, skipping EXCEPT."
  (car (cl-remove except (sort (hash-table-keys jworkspace-map) #'string<)
                  :test #'equal)))

;;;###autoload
(defun jworkspace-delete-workspace (&optional name kill-buffers)
  "Delete the specified workspace.
NAME is prompted when nil.  Associated buffers are not killed unless
KILL-BUFFERS is non-nil (or `jworkspace-kill-buffers-on-delete' is).
Prefix argument also kills buffers.  Buffers still associated with
another workspace are never killed."
  (interactive
   (list nil current-prefix-arg))
  (let* ((name (or name
                   (completing-read
                    "Choose the workspace to be deleted: "
                    (mapcar #'jworkspace-name
                            (hash-table-values jworkspace-map))
                    nil t)))
         (workspace (gethash name jworkspace-map))
         (do-kill (or kill-buffers jworkspace-kill-buffers-on-delete))
         (was-current (eq workspace (jworkspace--get-current-workspace))))
    (unless workspace
      (user-error "No such workspace: %s" name))
    (when do-kill
      (dolist (buf (jworkspace-get-buffers workspace))
        (when (and (buffer-live-p buf)
                   (not (jworkspace--buffer-in-other-workspace-p buf workspace)))
          (kill-buffer buf))))
    (setf (jworkspace-buffers workspace) nil
          (jworkspace-saved-ids workspace) nil)
    (remhash name jworkspace-map)
    (when was-current
      (jworkspace--set-current-workspace nil)
      (when-let* ((next-name (jworkspace--next-workspace-name))
                  (next (gethash next-name jworkspace-map)))
        (jworkspace--restore-window-state (jworkspace-window-config next) next)
        (jworkspace--set-current-workspace next)
        (jworkspace-refresh-buffers next)))))

;;;###autoload
(defun jworkspace-switch-workspace (&optional name)
  "Switch to the workspace with NAME or prompt a list of workspaces.
Then switch to the one chosen.  If there is not such one,
it will create the specified workspace.

Before leaving the current workspace, un-maximize if needed, refresh
buffer membership from displayed windows, and save window state via
`jworkspace--capture-window-state'."
  (interactive)
  (let* ((workspace-name (or name
                             (completing-read "Switch to: "
                                              (mapcar #'jworkspace-name
                                                      (hash-table-values jworkspace-map)))))
         (workspace (or (gethash workspace-name jworkspace-map)
                        (jworkspace-new-workspace workspace-name)))
         (current (jworkspace--get-current-workspace)))
    (if (eq current workspace)
        (progn
          (jworkspace-refresh-buffers current)
          workspace)
      (when current
        (jworkspace--restore-unmaximized current)
        (jworkspace-refresh-buffers current)
        (setf (jworkspace-window-config current)
              (jworkspace--capture-window-state)))
      (jworkspace--restore-window-state (jworkspace-window-config workspace)
                                        workspace)
      (jworkspace--set-current-workspace workspace)
      (jworkspace-refresh-buffers workspace)
      workspace)))

;;;###autoload
(defun jworkspace-toggle-maximize-window ()
  "Maximize the current window and can toggle back the original window layout."
  (interactive)
  (when-let* ((workspace (jworkspace--get-current-workspace)))
    (if (and (one-window-p)
             (jworkspace-temp-window-config workspace))
        (progn
          (window-state-put (jworkspace-temp-window-config workspace)
                            (frame-root-window))
          (setf (jworkspace-temp-window-config workspace) nil))
      (setf (jworkspace-temp-window-config workspace)
            (jworkspace--capture-window-state))
      (delete-other-windows))))

;;;###autoload
(defun jworkspace-rename-workspace ()
  "Rename the current workspace to input name."
  (interactive)
  (when-let* ((workspace (jworkspace--get-current-workspace))
              (old (jworkspace-name workspace))
              (name (read-string (format "Rename workspace '%s' to: " old))))
    (setq name (string-trim name))
    (cond
     ((string-empty-p name)
      (user-error "Workspace name cannot be empty"))
     ((equal name old)
      (message "Workspace name unchanged"))
     ((gethash name jworkspace-map)
      (user-error "Workspace named '%s' already exists" name))
     (t
      (remhash old jworkspace-map)
      (setf (jworkspace-name workspace) name)
      (puthash name workspace jworkspace-map)
      (message "Renamed workspace '%s' to '%s'" old name)))))


;;; Persistence (printable form)

(defun jworkspace--save-file ()
  "Return the path of the workspace save file."
  (concat jworkspace--save-dir-path "/save-workspace"))

(defun jworkspace--buffer-identity (buffer)
  "Return a printable identity for BUFFER.
File buffers use the absolute file name; others use the buffer name
as `(name . BUFFER-NAME)'."
  (when (buffer-live-p buffer)
    (if-let* ((file (buffer-file-name buffer)))
        (expand-file-name file)
      (cons 'name (buffer-name buffer)))))

(defun jworkspace--find-file-quietly (file)
  "Visit FILE without running `find-file-hook'.
Return the buffer, or nil if FILE does not exist.  Does not create
placeholders for missing files."
  (or (find-buffer-visiting file)
      (when (file-exists-p file)
        (let ((find-file-hook nil)
              (find-file-not-found-functions nil)
              (enable-local-variables :safe))
          (find-file-noselect file)))))

(defun jworkspace--resolve-buffer-identity (id &optional visit)
  "Resolve a saved buffer ID to a live buffer.
ID is either a file-name string or (name . BUFFER-NAME).

When VISIT is nil, never create or open anything: return an existing
visiting/named buffer or nil.  When VISIT is non-nil, open existing
files quietly (no `find-file-hook') and look up named buffers without
creating placeholders."
  (cond
   ((and (consp id) (eq (car id) 'name))
    (get-buffer (cdr id)))
   ((stringp id)
    (if visit
        (jworkspace--find-file-quietly id)
      (find-buffer-visiting id)))
   ((bufferp id)
    (and (buffer-live-p id) id))
   (t nil)))

(defun jworkspace--open-saved-ids (workspace)
  "Turn WORKSPACE's pending `saved-ids' into live `buffers'.
Missing files and unknown names are left in `saved-ids' so a later
attempt can still find them.  Never runs `find-file-hook'."
  (when workspace
    (let ((kept nil))
      (dolist (id (jworkspace-saved-ids workspace))
        (if-let* ((buf (jworkspace--resolve-buffer-identity id t)))
            (jworkspace-add-buffer workspace buf)
          (push id kept)))
      (setf (jworkspace-saved-ids workspace) (nreverse kept))
      (jworkspace-buffers workspace))))

(defun jworkspace--workspace-to-plist (workspace)
  "Serialize WORKSPACE to a printable plist.
Stores name, buffer identities, and window state (burly-aware when
available)."
  (jworkspace-prune-buffers workspace)
  (let* ((live-ids (delq nil
                         (mapcar #'jworkspace--buffer-identity
                                 (jworkspace-buffers workspace))))
         (ids (cl-remove-duplicates
               (append live-ids (copy-sequence (jworkspace-saved-ids workspace)))
               :test #'equal)))
    (list :name (jworkspace-name workspace)
          :buffers ids
          :window-config (jworkspace-window-config workspace))))

(defun jworkspace--plist-to-workspace (plist)
  "Deserialize PLIST into a `jworkspace' struct.
Does not visit files.  Identities stay in `saved-ids' until the
workspace is switched to or its buffers are listed."
  (let* ((name (or (plist-get plist :name)
                   (and (stringp plist) plist)
                   "unnamed"))
         (buf-ids (cl-remove nil (plist-get plist :buffers)))
         (wcfg (plist-get plist :window-config)))
    (jworkspace-create
     :name name
     :buffers nil
     :saved-ids buf-ids
     :window-config (or wcfg
                        (window-state-get (frame-root-window) 'writable)))))

(defun jworkspace--map-to-printable ()
  "Return a versioned plist of workspaces from `jworkspace-map'."
  (let ((result nil)
        (current (jworkspace--get-current-workspace)))
    (maphash
     (lambda (name ws)
       (push (cons name (jworkspace--workspace-to-plist ws)) result))
     jworkspace-map)
    (list :jworkspace-version jworkspace--save-version
          :current (and current (jworkspace-name current))
          :workspaces result)))

(defun jworkspace--load-alist (data)
  "Load alist DATA of (name . plist) into `jworkspace-map'."
  (clrhash jworkspace-map)
  (dolist (entry data)
    (let* ((plist (cdr entry))
           (ws (condition-case err
                   (jworkspace--plist-to-workspace plist)
                 (error
                  (message "jworkspace: skip bad entry %S: %S"
                           (car entry) err)
                  nil))))
      (when ws
        (puthash (jworkspace-name ws) ws jworkspace-map)))))

(defun jworkspace--load-printable (data)
  "Load DATA into `jworkspace-map'.
DATA may be a versioned plist, an alist of (name . plist), an old
raw hash-table, or a list of workspace objects.  Never signal on
unrecognized forms — best effort.

Return the saved current workspace name, or nil."
  (cond
   ;; Versioned plist.
   ((and (consp data) (keywordp (car data))
         (plist-member data :jworkspace-version))
    (jworkspace--load-alist (plist-get data :workspaces))
    (plist-get data :current))
   ;; New-ish format: alist of (name . plist)
   ((and (listp data)
         (cl-every (lambda (x) (and (consp x) (stringp (car x)) (listp (cdr x))))
                   data))
    (jworkspace--load-alist data)
    nil)
   ;; Old format: raw hash-table dumped with %S
   ((hash-table-p data)
    (clrhash jworkspace-map)
    (maphash
     (lambda (name ws)
       (condition-case err
           (cond
            ((jworkspace-p ws)
             (unless (listp (ignore-errors (jworkspace-buffers ws)))
               (ignore-errors (setf (jworkspace-buffers ws) nil)))
             (unless (listp (ignore-errors (jworkspace-saved-ids ws)))
               (ignore-errors (setf (jworkspace-saved-ids ws) nil)))
             (puthash name ws jworkspace-map))
            ((listp ws)
             (let ((new (jworkspace--plist-to-workspace ws)))
               (puthash (jworkspace-name new) new jworkspace-map)))
            (t
             (message "jworkspace: ignore unknown value for %S" name)))
         (error
          (message "jworkspace: failed to load %S: %S" name err))))
     data)
    nil)
   (t
    (message "jworkspace: unrecognized save format, starting empty")
    (clrhash jworkspace-map)
    nil)))

;;;###autoload
(defun jworkspace-save-workspace ()
  "Persist workspaces into a printable file under `jworkspace--save-dir-path'.
Saves workspace name, buffer file/buffer names, and window state.
Uses burly-aware capture for the *current* workspace before writing so
the on-disk window config is fresh.  Un-maximizes first so the saved
layout is the real one."
  (interactive)
  (unless (file-exists-p jworkspace--save-dir-path)
    (mkdir jworkspace--save-dir-path t))
  (when-let* ((current (jworkspace--get-current-workspace)))
    (jworkspace--restore-unmaximized current)
    (jworkspace-refresh-buffers current)
    (setf (jworkspace-window-config current)
          (jworkspace--capture-window-state)))
  (with-temp-file (jworkspace--save-file)
    (let ((print-length nil)
          (print-level nil)
          (print-circle t))
      (prin1 (jworkspace--map-to-printable) (current-buffer))))
  (message "jworkspace: saved %d workspace(s)"
           (hash-table-count jworkspace-map)))

;;;###autoload
(defun jworkspace-load-workspace ()
  "Load workspaces from the save file.
Does not visit files or change the window layout.  Rebinds the
frame's current workspace to the same-named struct in the new map.
Return the saved current workspace name, or nil."
  (interactive)
  (let ((path (jworkspace--save-file)))
    (unless (file-exists-p path)
      (user-error "No jworkspace save file at %s" path))
    (let* ((data (with-temp-buffer
                   (insert-file-contents path)
                   (goto-char (point-min))
                   (condition-case err
                       (read (current-buffer))
                     (error
                      (user-error "jworkspace: corrupt save file: %S" err)))))
           (saved-current (jworkspace--load-printable data)))
      (jworkspace--rebind-current-workspace)
      (message "jworkspace: loaded %d workspace(s)"
               (hash-table-count jworkspace-map))
      saved-current)))

;;;###autoload
(defun jworkspace-load-and-select-workspace ()
  "Load saved workspaces then interactively switch to one."
  (interactive)
  (jworkspace-load-workspace)
  (call-interactively #'jworkspace-switch-workspace))

;;;###autoload
(defun jworkspace-startup ()
  "Restore a session at Emacs start, or create `main'.
Intended for `window-setup-hook'.  If a save file exists, load it
(without visiting files) and switch to the saved current workspace,
falling back to `main' or the first name.  On failure, create `main'."
  (let ((path (jworkspace--save-file)))
    (cond
     ((file-exists-p path)
      (condition-case err
          (let* ((saved (jworkspace-load-workspace))
                 (name (or (and saved (gethash saved jworkspace-map) saved)
                           (and (gethash "main" jworkspace-map) "main")
                           (jworkspace--next-workspace-name))))
            (if name
                (jworkspace-switch-workspace name)
              (jworkspace-new-workspace "main")
              (jworkspace-switch-workspace "main")))
        (error
         (message "jworkspace: restore failed: %S" err)
         (unless (gethash "main" jworkspace-map)
           (jworkspace-new-workspace "main"))
         (jworkspace-switch-workspace "main"))))
     (t
      (jworkspace-new-workspace "main")
      (jworkspace-switch-workspace "main")))))


;;; Burly-style window URL helpers (optional)

(defun jworkspace-buffer-url (buffer)
  "Return URL for BUFFER.
Requires burly; returns nil when burly is unavailable."
  (when (jworkspace--ensure-burly)
    (let* ((major-mode (buffer-local-value 'major-mode buffer))
           (make-url-fn (and (boundp 'burly-major-mode-alist)
                             (map-nested-elt burly-major-mode-alist
                                             (list major-mode 'make-url-fn)))))
      (cond (make-url-fn (funcall make-url-fn buffer))
            (t (or (with-current-buffer buffer
                     (when-let* ((record (ignore-errors
                                           (bookmark-make-record))))
                       (cl-labels ((encode (element)
                                     (cl-typecase element
                                       (string (encode-coding-string element 'utf-8-unix))
                                       ((satisfies proper-list-p) (mapcar #'encode element))
                                       (cons (cons (encode (car element))
                                                   (encode (cdr element))))
                                       (t element))))
                         (setf record (encode record)))
                       (when (fboundp 'burly--bookmark-record-url)
                         (burly--bookmark-record-url record))))
                   (url-recreate-url
                    (url-parse-make-urlobj
                     "emacs+burly+name" nil nil nil nil
                     (concat "?" (encode-coding-string (buffer-name buffer)
                                                       'utf-8-unix))
                     nil nil 'fullness))))))))

(defun jworkspace--windows-set-url (windows &optional nullify)
  "Set `jworkspace-url' window parameter in WINDOWS.
If NULLIFY, set the parameter to nil.  No-op when burly is missing."
  (when (jworkspace--ensure-burly)
    (dolist (window windows)
      (let ((value (if nullify nil (jworkspace-buffer-url (window-buffer window)))))
        (set-window-parameter window 'jworkspace-url value)
        (set-window-parameter window 'burly-url value)))))

(cl-defun jworkspace--window-state (&optional (frame (selected-frame)))
  "Return window state for FRAME.
When burly is available, set URL window parameters before serializing
and run burly translators.  Otherwise return a writable
`window-state-get' snapshot of the frame root."
  (if (not (jworkspace--ensure-burly))
      (with-selected-frame frame
        (window-state-get (frame-root-window frame) 'writable))
    (with-selected-frame frame
      (jworkspace--windows-set-url (window-list nil 'never))
      (let* ((window-persistent-parameters (append jworkspace-window-persistent-parameters
                                                   window-persistent-parameters))
             (window-state (window-state-get (frame-root-window frame) 'writable)))
        (jworkspace--windows-set-url (window-list nil 'never) 'nullify)
        (jworkspace--window-serialized window-state)))))

(defun jworkspace--window-serialized (state)
  "Return window STATE having serialized its parameters.
Uses `burly-window-parameters-translators' when bound; otherwise returns
STATE unchanged."
  (if (not (and (boundp 'burly-window-parameters-translators)
                burly-window-parameters-translators))
      state
    (cl-labels ((translate-state (state)
                  "Set windows' buffers in STATE."
                  (pcase state
                    (`(leaf . ,_attrs) (translate-leaf state))
                    ((pred atom) state)
                    (`(,_key . ,(pred atom)) state)
                    ((pred list) (mapcar #'translate-state state))))
                (translate-leaf (leaf)
                  "Translate window parameters in LEAF."
                  (pcase-let* ((`(leaf . ,attrs) leaf)
                               ((map parameters) attrs))
                    (pcase-dolist (`(,parameter . ,(map serialize))
                                   burly-window-parameters-translators)
                      (when (map-elt parameters parameter)
                        (setf (map-elt parameters parameter)
                              (funcall serialize (map-elt parameters parameter)))))
                    (setf (map-elt attrs 'parameters) parameters)
                    (cons 'leaf attrs))))
      (translate-state state))))

(defun jworkspace--reincarnate-url (url)
  "Return a live buffer for burly-style URL, or nil."
  (cond
   ((not (stringp url)) nil)
   ((fboundp 'burly-url-buffer)
    (ignore-errors (burly-url-buffer url)))
   (t nil)))

(defun jworkspace--bufferize-window-state (state)
  "Return window STATE with buffers reincarnated from URL parameters.
Mirrors `burly--bufferize-window-state', using `jworkspace-url' then
`burly-url'.  On failure, keep the original buffer name so
`window-state-put' can still find a live file buffer."
  (cl-labels
      ((bufferize-state (state)
         (pcase state
           (`(leaf . ,_attrs) (translate-leaf (bufferize-leaf state)))
           ((pred atom) state)
           (`(,_key . ,(pred atom)) state)
           ((pred list) (mapcar #'bufferize-state state))))
       (bufferize-leaf (leaf)
         (pcase-let* ((`(leaf . ,attrs) leaf)
                      ((map parameters buffer) attrs)
                      ((map jworkspace-url burly-url) parameters)
                      (`(,buffer-name . ,buffer-attrs) buffer)
                      (url (or jworkspace-url burly-url))
                      (new-buffer (or (and url (jworkspace--reincarnate-url url))
                                      (and (stringp buffer-name) (get-buffer buffer-name)))))
           (when new-buffer
             (setf (map-elt attrs 'buffer) (cons new-buffer buffer-attrs)))
           (cons 'leaf attrs)))
       (translate-leaf (leaf)
         (pcase-let* ((`(leaf . ,attrs) leaf)
                      ((map parameters) attrs))
           (when (and (boundp 'burly-window-parameters-translators)
                      burly-window-parameters-translators)
             (pcase-dolist (`(,parameter . ,(map deserialize))
                            burly-window-parameters-translators)
               (condition-case nil
                   (when (map-elt parameters parameter)
                     (setf (map-elt parameters parameter)
                           (funcall deserialize (map-elt parameters parameter))))
                 (error (setf parameters (map-delete parameters parameter))))))
           (setf (map-elt attrs 'parameters) parameters)
           (cons 'leaf attrs))))
    (condition-case err
        (if-let* ((leaf-pos (cl-position 'leaf state)))
            (append (cl-subseq state 0 leaf-pos)
                    (translate-leaf (bufferize-leaf (cl-subseq state leaf-pos))))
          (bufferize-state state))
      (error
       (message "jworkspace: bufferize failed, using raw state: %S" err)
       state))))


;;; Setup / enable

;;;###autoload
(defun jworkspace-enable ()
  "Install jworkspace hooks (idempotent).
Adds `jworkspace--on-find-file' to `find-file-hook' and
`jworkspace--on-window-buffer-change' to
`window-buffer-change-functions'."
  (interactive)
  (unless jworkspace--hooks-installed
    (add-hook 'find-file-hook #'jworkspace--on-find-file)
    (add-hook 'window-buffer-change-functions #'jworkspace--on-window-buffer-change)
    (setq jworkspace--hooks-installed t))
  jworkspace--hooks-installed)

;;;###autoload
(defun jworkspace-disable ()
  "Remove jworkspace hooks."
  (interactive)
  (remove-hook 'find-file-hook #'jworkspace--on-find-file)
  (remove-hook 'window-buffer-change-functions #'jworkspace--on-window-buffer-change)
  (setq jworkspace--hooks-installed nil))

;; Auto-install hooks when the library loads.
(jworkspace-enable)

(provide 'jworkspace)

;;; jworkspace.el ends here

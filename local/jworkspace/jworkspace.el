;;; jworkspace.el --- summary -*- lexical-binding: t -*-

;; Author: jing
;; Maintainer: jing
;; Version: version
;; Keywords: workspace


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

;; commentary

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defcustom jworkspace-window-persistent-parameters
  (list (cons 'burly-url 'writable)
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
                                    (const :tag "Saved" writable))))

(defvar jworkspace-map (make-hash-table :test 'equal)
  "A hashmap to map workspace's name to itself.")
;; (puthash k v jworkspace-map)
;; (gethash k jworkspace-map)
;; (remhash k jworkspace-map)

(defvar jworkspace--save-dir-path (concat user-emacs-directory ".jworkspace")
  "The dir path for saving the workspaces settings.")

(cl-defstruct (jworkspace (:constructor jworkspace-create)
                          (:copier nil))
  name
  (window-config (progn
                   (save-window-excursion
                     (delete-other-windows)
                     (show-splash-buffer)
                     (window-state-get))))
  temp-window-config nil)
  

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
  "Set the current WORKSPACE structure in the frame paramenter."
  (set-frame-parameter (selected-frame) 'current-workspace workspace))

(defun jworkspace-new-workspace (&optional name)
  "Create a workspace with the input NAME or the prompt input."
  (let* ((wsname (or name
                     (read-string "Enter the new workspace name: ")))
         (workspace (jworkspace-create :name wsname)))

    (puthash (jworkspace-name workspace) workspace jworkspace-map)))

;;;###autoload
(defun jworkspace-worsapce-existp (name)
  "Check the NAME if is in the workspace list."
  (member name (hash-table-keys jworkspace-map)))

;;;###autoload
(defun jworkspace-delete-workspace ()
  "Delete the specified workspace."
  (interactive)
  (let ((name (completing-read "Choose the workspace to be deleted: " (mapcar 'jworkspace-name (hash-table-values jworkspace-map)))))
    ;; TODO: what do I need to deal with if the current-workspace is the same as the one to be deleted.
    (remhash name jworkspace-map)
    (when (> (hash-table-count jworkspace-map) 0)
      (window-state-put
       (jworkspace-window-config
        (gethash (nth 0 (hash-table-keys jworkspace-map)) jworkspace-map))))))

      ;; (set-window-configuration
      ;;  (jworkspace-window-config
      ;;   (gethash (nth 0 (hash-table-keys jworkspace-map)) jworkspace-map))))))

;;;###autoload
(defun jworkspace-switch-workspace (&optional name)
  "Switch to the workspace with NAME or prompt a list of worksapces.
Then switch to the one choosed.  If there is not such one,
it will create the specified workspace."
  (interactive)
  (let* ((workspace-name (or name
                             (completing-read "Switch to: "
                                              (mapcar 'jworkspace-name (hash-table-values jworkspace-map)))))
         (workspace (or (gethash workspace-name jworkspace-map)
                        (jworkspace-new-workspace workspace-name))))

    (when (jworkspace--get-current-workspace)
      (setf (jworkspace-window-config (jworkspace--get-current-workspace)) (window-state-get)))

    ;; (set-window-configuration (jworkspace-window-config workspace))
    (window-state-put (jworkspace-window-config workspace))
    (jworkspace--set-current-workspace workspace)))

;;;###autoload
(defun jworkspace-toggle-maximize-window ()
  "Maximize the current window and can toggle back the original window layout."
  (interactive)
  (when-let ((workspace (jworkspace--get-current-workspace)))
    (if (and (one-window-p)
             (jworkspace-temp-window-config workspace))
        (window-state-put (jworkspace-temp-window-config workspace))
      (progn
        (setf (jworkspace-temp-window-config workspace) (window-state-get))
        (delete-other-windows)))))

;;;###autoload
(defun jworkspace-rename-workspace ()
  "Rename the current workspace to input name."
  (interactive)
  (when-let ((workspace (jworkspace--get-current-workspace))
             (name (read-string (format "Rename workspace '%s' to: " (jworkspace-name workspace)))))
    (remhash (jworkspace-name workspace) jworkspace-map)
    (setf (jworkspace-name workspace) name)
    (puthash name workspace jworkspace-map)))

(defun jworkspace-save-workspace ()
  "Persist the workspace into file.
TODO: research a way to serialze the window state .."
  (interactive)
  (unless (file-exists-p jworkspace--save-dir-path)
    (mkdir jworkspace--save-dir-path t))
  (with-temp-file (concat jworkspace--save-dir-path "/save-workspace")
    (insert (format "%S" jworkspace-map))))

;;; TODO: think how to serialize the jworkspace-map. the key point is
;;; the window-state object

(defun jworkspace-buffer-url (buffer)
  "Return URL for BUFFER."
  (let* ((major-mode (buffer-local-value 'major-mode buffer))
         (make-url-fn (map-nested-elt burly-major-mode-alist (list major-mode 'make-url-fn))))
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
		       ;; Encode all strings in record with UTF-8.
		       ;; NOTE: If we stop using URLs in the future, maybe this won't be needed.
		       (setf record (encode record)))
                     (burly--bookmark-record-url record)))
                 ;; Buffer can't seem to be bookmarked, so record it as
                 ;; a name-only buffer.  For some reason, it works
                 ;; better to use the buffer name in the query string
                 ;; rather than the filename/path part.
                 (url-recreate-url (url-parse-make-urlobj "emacs+burly+name" nil nil nil nil
                                                          (concat "?" (encode-coding-string (buffer-name buffer)
											    'utf-8-unix))
							  nil nil 'fullness)))))))

(defun jworkspace--windows-set-url (windows &optional nullify)
  "Set `jworkspace-url' window parameter in WINDOWS.
If NULLIFY, set the parameter to nil."
  (dolist (window windows)
    (let ((value (if nullify nil (jworkspace-buffer-url (window-buffer window)))))
      (set-window-parameter window 'jworkspace-url value))))

(cl-defun jworkspace--window-state (&optional (frame (selected-frame)))
  "Return window state for FRAME.
Sets `burly-url' window parameter in each window before
serializing."
  (with-selected-frame frame
    ;; Set URL window parameter for each window before saving state.
    (jworkspace--windows-set-url (window-list nil 'never))
    (let* ((window-persistent-parameters (append jworkspace-window-persistent-parameters
                                                 window-persistent-parameters))
           (window-state (window-state-get nil 'writable)))
      ;; Clear window parameters we set (because they aren't kept
      ;; current, so leaving them could be confusing).
      (jworkspace--windows-set-url (window-list nil 'never) 'nullify)
      (jworkspace--window-serialized window-state))))

(defun jworkspace--window-serialized (state)
  "Return window STATE having serialized its parameters."
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
    (translate-state state)))

(defun jworkspace-load-workspace ()
  "Load the workspaces from the list in file."
  (interactive)
  (setq jworkspace-map
        (with-temp-buffer
          (insert-file-contents (concat jworkspace--save-dir-path "/save-workspace"))
          (read (current-buffer)))))

(defun jworkspace-load-and-select-workspace ()
  (jworkspace-load-workspace)
  (call-interactively 'jworkspace-switch-workspace))

(provide 'jworkspace)

;;; jworkspace.el ends here

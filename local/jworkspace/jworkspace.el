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
                     (window-state-get)))))

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
      ;; (setf (jworkspace-window-config (jworkspace--get-current-workspace)) (current-window-configuration)))
      (setf (jworkspace-window-config (jworkspace--get-current-workspace)) (window-state-get)))

    ;; (set-window-configuration (jworkspace-window-config workspace))
    (window-state-put (jworkspace-window-config workspace))
    (jworkspace--set-current-workspace workspace)))

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

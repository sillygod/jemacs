;;; gptel-ext.el --- summary -*- lexical-binding: t -*-

;; Author: jing
;; Maintainer: jing
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: gptel


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

(require 'gptel)


(defvar gptel-ext--fabric-root "")
(defvar gptel-ext--fabric-remote-url  "https://github.com/danielmiessler/fabric")
(defvar gptel-ext--fabric-patterns-subdirectory "/patterns")
(defvar gptel-ext--fabric-patterns-path "fabric/patterns")
(defvar gptel-ext--fabric-patterns nil)

(defun gptel-ext-fabric-sparse-checkout-subdir (repo-url path branch)
  "Sparse pull fabric patterns."
  (let* ((repo-name (file-name-nondirectory (file-name-sans-extension repo-url)))
         (repo-pos (concat gptel-ext--fabric-root repo-name))
         (repo-exists (file-directory-p repo-pos)))
    (if (not repo-exists)
        (progn
          (message "creating directory %s" repo-pos)
          (make-directory repo-pos t))
      (message "repo already exists at %s" repo-pos))
    (let ((reporter (make-progress-reporter "sparse pulling patterns " 0 4)))
      (with-temp-buffer
        (cd repo-pos)
        (unless repo-exists
          (progress-reporter-update reporter 0 "| cloning repo")
          (unless (zerop (call-process "git" nil t nil "clone" "--no-checkout" repo-url "."))
            (error "Failed to clone repository %s" repo-url))
          (progress-reporter-update reporter 1 "| sparse checkout config")

          (unless (zerop (call-process "git" nil t nil "sparse-checkout" "init"))
            (error "Failed to initialize sparse checkout"))
          (progress-reporter-update reporter 2 "| sparse-checkout set")

          (unless (zerop (call-process "git" nil t nil "sparse-checkout" "set" "--no-cone" path))
            (error "Failed to set sparse checkout for %s" path))
          (progress-reporter-update reporter 3 "| main pull"))

        (let ((checkout-status (call-process "git" nil t nil "checkout" branch)))
          (unless (zerop checkout-status)
            (if repo-exists
                (warn "Failed to checkout branch %s in %s. Patterns might be outdated." branch repo-pos)
              (error "Failed to checkout branch %s in %s" branch repo-pos)))) ;; Added repo-pos to error

        (progress-reporter-update reporter 4)
        (message "Sparse pulled patterns for %s on branch %s" repo-name branch)))))


(defun gptel-ext-fabric-populate-patterns ()
  "Filter out invalid directories."
  (with-temp-buffer
    (let ((patterns-dir (concat gptel-ext--fabric-root gptel-ext--fabric-patterns-path)))
      (unless (file-directory-p patterns-dir)
        (error "Patterns directory not found: %s. Please run gptel-ext-fabric-sync-patterns first" patterns-dir))
      (setq gptel-ext--fabric-patterns (cl-remove-if-not
                                     (lambda (pattern)
                                       (f-exists-p (format "%s/%s/system.md" patterns-dir pattern))) ;; Use patterns-dir variable
                                     (directory-files patterns-dir nil "^[^.]" t)))))) ;; Use patterns-dir variable


(defun gptel-ext-fabric-yield-prompt ()
  "Completing read fabric patterns."
  (let ((pattern (completing-read "fabric-patterns: " gptel-ext--fabric-patterns)))
    (with-temp-buffer
      (insert-file-contents (format "%s%s/%s/system.md" gptel-ext--fabric-root gptel-ext--fabric-patterns-path pattern))
      (buffer-string))))

(defun gptel-ext-fabric-sync-patterns ()
  "Sparse pull patterns and populate cache."
  (interactive)
  (gptel-ext-fabric-sparse-checkout-subdir gptel-ext--fabric-remote-url gptel-ext--fabric-patterns-subdirectory "main")
  (gptel-ext-fabric-populate-patterns))

(defun gptel-ext-fabric-send ()
  "Dispatch pattern for context preceding the cursor with the selected pattern."
  (interactive)
  (let ((gptel--system-message (gptel-ext-fabric-yield-prompt)))
    (insert "\n\n")
    (gptel-send)))



(provide 'gptel-ext)

;;; gptel-ext.el ends here

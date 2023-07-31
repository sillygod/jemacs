;;; jlight.el --- summary -*- lexical-binding: t -*-

;; Author: jing
;; Maintainer: jing
;; Version: version
;; Keywords: highlight


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


;; the overlay has higher priority than the font-lock
;; that means the word highlighted by font-lock will be overwritten by the overlay
;; if both of them are covered the same area

(defvar jlight-matches '()
  "It stores the matched highlighted regions as a cons of begin and end.")

(defvar jlight-pointer 0
  "It's the index of matches currently pointed to.")


;; (defun set-pointer (index)
;;   "Set the pointer to point to the element at the specified index in the `things` list."
;;   (setq pointer-index index))

;; (defun get-pointer ()
;;   "Get the current element pointed by the pointer."
;;   (nth pointer-index things))

;; (defun move-pointer-next ()
;;   "Move the pointer to the next element in the `things` list."
;;   (setq pointer-index (mod (1+ pointer-index) (length things))))

;; (defun move-pointer-previous ()
;;   "Move the pointer to the previous element in the `things` list."
;;   (setq pointer-index (mod (1- pointer-index) (length things))))

;; (defun get-index (value)
;;   "Get the index of the first occurrence of VALUE in the `things` list."
;;   (cl-position value things))

(defun keymap-help-doc (keymap)
  "Display the key bindings for the specified `KEYMAP'."
  (interactive)
  (let ((buf (get-buffer-create "*Help*")))
    (with-help-window "*Help*"
      (with-current-buffer buf
        (insert (substitute-command-keys
                 (format "\\{%s}" (symbol-name keymap))))))))


(defun highlight-selected-word ()
  "Highlight the selected word using overlays."
  (interactive)
  (let ((selected-word (if (region-active-p)
                           (buffer-substring-no-properties (region-beginning) (region-end))
                         (thing-at-point 'word))))
    (when selected-word
      (let ((regexp (regexp-quote selected-word)))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward regexp nil t)
            (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
              (overlay-put overlay 'face '(:background "yellow"))
              (overlay-put overlay 'highlight-selected-word t))))))))

(defun clear-highlight ()
  "Clear the highlighted words."
  (interactive)
  (remove-overlays (point-min) (point-max) 'highlight-selected-word t))

(defun goto-next-highlighted-word ()
  "Move to the next highlighted word."
  (interactive)
  (require 'cl-lib)
  (let ((matches (cl-loop for overlay in (overlays-in (point) (point-max))
                          if (overlay-get overlay 'highlight-selected-word)
                          collect (cons (overlay-start overlay) (overlay-end overlay)))))

    (goto-char (car (cadr matches)))))

;; (defun get-highlighted-occurrences ()
;;   "Retrieve the positions of highlighted occurrences."
;;   (interactive)
;;   (if (null hi-lock-interactive-patterns)
;;       (message "No highlighted occurrences.")
;;     (let ((matches (cl-loop for overlay in (overlays-in (point-min) (point-max))
;;                             if (eq (overlay-get overlay 'face) 'hi-yellow)
;;                             collect (cons (overlay-start overlay) (overlay-end overlay)))))
;;       matches)))


(provide 'jlight)

;;; jlight.el ends here

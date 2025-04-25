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

;;; Code:

(require 'cl-lib)


;; the overlay has higher priority than the font-lock
;; that means the word highlighted by font-lock will be overwritten by the overlay
;; if both of them are covered the same area

;;; Faces

(defvar jlight-matched-overlay-face
  '(:inherit highlight)
  "Symbol Overlay default face.")

(defvar jlight-current-matched-overlay-face
  '(:inherit shr-mark))

(defvar jlight-highlight-priority 1000
  "Symbol highlight priority.")

(defvar ignore-clear-post-commands '(highlight-selected-word
                                     wrap-mark-operation
                                     my-toggle-case-sensitive
                                     expand-and-highlight-region
                                     contract-and-highlight-region
                                     goto-next-highlighted-word
                                     goto-prev-highlighted-word)
  "The commands ignored to trigger hook while words are highlighted.")

(defvar-local jlight-matches '()
  "It stores the matched highlighted regions as a cons of begin and end.")

(defvar-local jlight-pointer 0
  "It's the index of matches currently pointed to.")

(defun set-pointer (index)
  "Set the pointer to the element at INDEX in the `jlight-matches` list."
  (setq-local jlight-pointer index))

(defun get-pointer (&optional index)
  "Get the current element pointed by the pointer.
When `INDEX' is not given, use the jlight-pointer instead."
  (let ((i (or index jlight-pointer)))
    (nth i jlight-matches)))

(defun move-pointer-next ()
  "Move the pointer to the next element in the `jlight-matches` list."
  (set-pointer (mod (1+ jlight-pointer) (length jlight-matches))))

(defun move-pointer-previous ()
  "Move the pointer to the previous element in the `jlight-matches` list."
  (set-pointer (mod (1- jlight-pointer) (length jlight-matches))))

(defun jlight-get-matched-thing ()
  "Get the matched word."
  (let ((region (cl-first jlight-matches)))
    (buffer-substring-no-properties (car region) (cdr region))))

(defun jlight-get-startpoint-of-first-matched ()
  "Return the beg point of the first-matched thing.
It's useful for the function like iedit restrict region."
  (car (cl-first jlight-matches)))

(defun jlight-get-endpoint-of-last-matched ()
  "Return the end point of the last-matched thing.
It's useful for the function like iedit restrict region."
  (cdar (last jlight-matches)))


(defun get-index (value)
  "Get the index of the first occurrence of VALUE in the `jlight-matches` list."
  (cl-position value jlight-matches :test #'(lambda (x y) (equal x y))))

(defun keymap-help-doc (keymap)
  "Display the key bindings for the specified `KEYMAP'."
  (interactive)
  (let ((buf (get-buffer-create "*Help*")))
    (with-help-window "*Help*"
      (with-current-buffer buf
        (insert (substitute-command-keys
                 (format "\\{%s}" (symbol-name keymap))))))))

(defun reset-point-and-maches ()
  "Clear the sotred matched positions and reset the pointer."
  (setq-local jlight-matches '())
  (set-pointer 0))

;;;###autoload
(defun has-jlight-matches-p ()
  "Return the state if highlighted matches exist."
  (eq jlight-matches '()))

;;;###autoload
(defun highlight-selected-word (&optional case-sensitive)
  "Highlight the selected word using overlays.
CASE-SENSITIVE affect the buffer local variable `case-fold-search'."
  (interactive "P")

  (unless (eq jlight-matches '())
    (when (region-active-p)
      (let* ((begin (region-beginning))
             (end (region-end)))

        (clear-highlight t)
        (goto-char begin)
        ;; Moving cursor to the begin is very important or the (point) of the endpoint will vary from your cursor's position
        (set-mark end))))
  (unless (member 'clear-highlight post-command-hook)
    (add-hook 'post-command-hook 'clear-highlight nil t))

  (let ((selected-word (if (region-active-p)
                           (buffer-substring-no-properties (region-beginning) (region-end))
                         (thing-at-point 'word)))
        (case-fold-search (not case-sensitive)) ;; case-fold-search t means case insensitive
        (cur-pos (point)))
    (when selected-word
      (let ((regexp (regexp-quote selected-word)))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward regexp nil t)
            (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
              (add-to-list 'jlight-matches (cons (match-beginning 0) (match-end 0)) t)
              (if (and (>= cur-pos (match-beginning 0))
                       (<= cur-pos (match-end 0)))
                  (progn
                    (overlay-put overlay 'face jlight-current-matched-overlay-face)
                    (set-pointer (get-index (cons (match-beginning 0) (match-end 0)))))
                (overlay-put overlay 'face jlight-matched-overlay-face))
              (overlay-put overlay 'priority jlight-highlight-priority)
              (overlay-put overlay 'highlight-selected-word t))))))))


;;;###autoload
(defun clear-highlight (&optional force)
  "Clear the highlighted words.  FORCE not nil means clear highlight directly."
  (interactive)
  (when (or (not (member this-command ignore-clear-post-commands))
            force)
    (if (member 'clear-highlight post-command-hook)
        (remove-hook 'post-command-hook 'clear-highlight t))
    (when (region-active-p)
      (deactivate-mark))
    (remove-overlays (point-min) (point-max) 'highlight-selected-word t)
    (reset-point-and-maches)))

(defun get-current-overlay ()
  "Iterate the overlay under current point."
  (cl-dolist (overlay (overlays-at (car (get-pointer))))
    (when (overlay-get overlay 'highlight-selected-word)
      (cl-return overlay))))


;;;###autoload
(defun goto-next-highlighted-word ()
  "Move to the next highlighted word."
  (interactive)
  (when (region-active-p)
    (deactivate-mark))
  (overlay-put (get-current-overlay) 'face jlight-matched-overlay-face)
  (move-pointer-next)
  (goto-char (car (get-pointer)))
  (overlay-put (get-current-overlay) 'face jlight-current-matched-overlay-face))

;;;###autoload
(defun goto-prev-highlighted-word ()
  "Move to the previous highlighted word."
  (interactive)
  (when (region-active-p)
    (deactivate-mark))
  (overlay-put (get-current-overlay) 'face jlight-matched-overlay-face)
  (move-pointer-previous)
  (goto-char (car (get-pointer)))
  (overlay-put (get-current-overlay) 'face jlight-current-matched-overlay-face))


;; narrow-to-region, narrow-to display region (window-start?)
;; (eq (window-buffer) (current-buffer))
;; need to keep the current matched position

(provide 'jlight)

;;; jlight.el ends here

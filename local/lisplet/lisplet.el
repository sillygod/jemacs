;;; lisplet.el --- Tiny special-position Lisp editing -*- lexical-binding: t -*-

;; Author: Jing
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: lisp, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; A handful of lispy-style commands that fire only at a sexp
;; boundary: point before `(' or after `)'.  Anywhere else the same
;; keys self-insert.
;;
;;   %  other side of the current sexp
;;   d  kill complete sexps through end of line (whole list if multiline)
;;   e  eval the current sexp
;;   i  pretty-print / indent the current sexp

;;; Code:

(defgroup lisplet nil
  "Tiny special-position Lisp editing."
  :group 'lisp
  :prefix "lisplet-")


;;; Special position

(defun lisplet--in-string-or-comment-p ()
  "Return non-nil if point is inside a string or comment."
  (nth 8 (syntax-ppss)))

(defun lisplet--at-open-p ()
  "Return non-nil if point is before an opening delimiter."
  (and (not (lisplet--in-string-or-comment-p))
       (not (eobp))
       (eq (char-syntax (char-after)) ?\()))

(defun lisplet--at-close-p ()
  "Return non-nil if point is after a closing delimiter."
  (and (not (lisplet--in-string-or-comment-p))
       (not (bobp))
       (eq (char-syntax (char-before)) ?\))))

(defun lisplet--special-p ()
  "Return non-nil if point is at a sexp boundary."
  (or (lisplet--at-open-p) (lisplet--at-close-p)))

(defun lisplet--sexp-bounds ()
  "Return (BEG . END) of the sexp at a special position.
Signal if point is not at a boundary."
  (cond
   ((lisplet--at-open-p)
    (cons (point) (save-excursion (forward-list) (point))))
   ((lisplet--at-close-p)
    (cons (save-excursion (backward-list) (point)) (point)))
   (t
    (user-error "Not at a sexp boundary"))))


;;; Commands

(defun lisplet-different ()
  "Jump to the other side of the current sexp.
With an active region, exchange point and mark."
  (interactive)
  (cond
   ((use-region-p)
    (exchange-point-and-mark))
   ((lisplet--at-open-p)
    (forward-list))
   ((lisplet--at-close-p)
    (backward-list))
   (t
    (user-error "Not at a sexp boundary"))))

(defun lisplet-kill ()
  "Kill sexps through the end of the line, keeping parens balanced.
At a multiline opening delimiter, kill the whole list."
  (interactive)
  (cond
   ((lisplet--in-string-or-comment-p)
    (kill-line))
   ((lisplet--at-open-p)
    (let ((beg (point))
          (end (save-excursion
                 (condition-case nil
                     (progn (forward-list) (point))
                   (scan-error nil)))))
      (unless end
        (user-error "Unbalanced parentheses"))
      (if (> (count-lines beg end) 1)
          (kill-region beg end)
        (goto-char beg)
        (let ((eol (line-end-position))
              (pt beg))
          (while (and (< (point) eol)
                      (ignore-errors (forward-sexp 1) t))
            (setq pt (point)))
          (kill-region beg pt)))))
   (t
    (let ((beg (point))
          (eol (line-end-position))
          (pt (point)))
      (while (and (< (point) eol)
                  (ignore-errors (forward-sexp 1) t))
        (setq pt (point)))
      (kill-region beg pt)))))

(defun lisplet-eval ()
  "Eval the sexp at point and echo the result."
  (interactive)
  (let* ((bounds (lisplet--sexp-bounds))
         (beg (car bounds))
         (end (cdr bounds)))
    (cond
     ((derived-mode-p 'emacs-lisp-mode 'lisp-interaction-mode)
      (save-excursion
        (goto-char end)
        (eval-last-sexp nil)))
     ((and (derived-mode-p 'scheme-mode) (fboundp 'geiser-eval-region))
      (geiser-eval-region beg end))
     ((and (derived-mode-p 'lisp-mode) (fboundp 'slime-eval-last-expression))
      (save-excursion
        (goto-char end)
        (call-interactively #'slime-eval-last-expression)))
     (t
      (save-excursion
        (goto-char end)
        (eval-last-sexp nil))))))

(defun lisplet-indent ()
  "Pretty-print the sexp at point."
  (interactive)
  (let ((bounds (lisplet--sexp-bounds)))
    (save-excursion
      (goto-char (car bounds))
      (indent-pp-sexp t))))


;;; Keymap: special or self-insert

(defun lisplet--insert-or-call (fn)
  "Return a command that calls FN at a sexp boundary, else inserts."
  (let ((name (intern (format "lisplet-maybe-%s" (symbol-name fn)))))
    (defalias name
      (lambda (arg)
        (interactive "p")
        (if (lisplet--special-p)
            (let ((current-prefix-arg arg))
              (call-interactively fn))
          (self-insert-command arg)))
      (format "Call `%s' at a sexp boundary, else insert the key." fn))
    name))

(defun lisplet-define-key (keymap key fn)
  "Bind KEY in KEYMAP to FN when special, else self-insert."
  (define-key keymap (kbd key) (lisplet--insert-or-call fn)))

(defvar lisplet-mode-map
  (let ((map (make-sparse-keymap)))
    (lisplet-define-key map "%" #'lisplet-different)
    (lisplet-define-key map "d" #'lisplet-kill)
    (lisplet-define-key map "e" #'lisplet-eval)
    (lisplet-define-key map "i" #'lisplet-indent)
    map)
  "Keymap for `lisplet-mode'.")

;;;###autoload
(define-minor-mode lisplet-mode
  "Tiny special-position Lisp editing.

Keys `%', `d', `e' and `i' run sexp commands when point is before
an opening delimiter or after a closing one.  Anywhere else they
insert themselves."
  :lighter " λ"
  :keymap lisplet-mode-map
  :group 'lisplet)

(provide 'lisplet)
;;; lisplet.el ends here

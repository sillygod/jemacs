;;; package --- Summary
;;; rust-repl.el                  -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; This package provides bindings for interacting with the evcxr (an implementation of REPL for rust)
;;; Code:

(require 'comint)
(require 'rust-mode)

(defconst evcxr-buffer "*Evcxr REPL*")
(defconst evcxr-buffer-name "Evcxr REPL")

(defgroup evcxr nil
  "Evcxr settings and functions."
  :group 'external
  :tag "evcxr"
  :prefix "evcxr-")

(defcustom evcxr-command "evcxr"
  "Evcxr command."
  :type 'string
  :group 'evcxr)

(defcustom evcxr-cli-arguments '()
  "Evcxr custom command line arguments."
  :type '(string)
  :group 'evcxr)

(defcustom evcxr-prompt-regexp "\\(\. \. \. \. +\\)\\|\\(gomacro> \\)"
  "Prompt regexp for `evcxr-run'."
  :type 'string
  :group 'evcxr)

(defun evcxr--get-process ()
  "Get current evcxr process associated with `evcxr-buffer'."
  (get-buffer-process evcxr-buffer))

;;;###autoload
(defun evcxr-run ()
  "Run an inferior instance of `evcxr' inside Emacs."
  (interactive)
  (let* ((buffer (comint-check-proc evcxr-buffer)))
    ;; pop to the `*Evcxr REPL*' buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (pop-to-buffer
     (if (or buffer (not (derived-mode-p 'evcxr-inferior-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create evcxr-buffer)
       (current-buffer)))
    ;; create the comint process if there is no buffer.
    (unless buffer
      (apply 'make-comint-in-buffer evcxr-buffer-name buffer
             evcxr-command nil evcxr-cli-arguments)
      (evcxr-inferior-mode))))

(defun evcxr-running-p ()
  "Cehck if evcxr REPL is running or not."
  (comint-check-proc evcxr-buffer))

(define-derived-mode evcxr-inferior-mode comint-mode evcxr-buffer-name
  "Major mode for `evcxr-run' comint buffer.")

(defvar evcxr-inferior-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map "\t" completion-at-point)
    map)
  "Basic mode map for `evcxr-run'.")

(provide 'rust-repl)
;;; rust-repl.el ends here

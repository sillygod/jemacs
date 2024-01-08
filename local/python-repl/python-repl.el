;;; python-repl.el --- Summary -*- lexical-binding: t -*-
;;
;; Author: jing
;; Maintainer: jing
;; Version: version
;; Keywords: highlight


;;; Commentary:
;;;
;;; Code:

(require 'cl-generic)
(require 'cl-lib)

(defgroup python-repl nil
  "Python repl wiwth vterm."
  :group 'python-repl)

(defcustom python-repl-hook nil
  "Hook runs after starting the repl"
  :type 'hook
  :group 'python-repl)

(defcustom python-repl-inferior-buffer-name-base "python-repl"
  "Prefix for the names of python repl buffers."
  :type 'string
  :group 'python-repl)

(defun python-repl--add-earmuffs (buffer-name)
  "Add earmuffs * * to buffer-name"
  (concat "*" buffer-name "*"))

(defun python-repl--has-running-vterm-process (buffer)
  "Return non nil if buffer has a running vterm process."
  (let ((proc (buffer-local-value 'vterm--process buffer)))
    (and proc (memq (process-status proc) '(run stop open listen connect)))))


;; why use compilation mode? Can I just send the code block to the terminal?
;; keep doing research the library

(cl-defstruct python-repl--buffer-vterm)

(provide 'python-repl)
;;; python-repl.el ends here

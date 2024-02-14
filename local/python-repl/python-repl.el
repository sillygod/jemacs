;;; python-repl.el --- Summary -*- lexical-binding: t -*-
;;
;; Author: jing
;; Maintainer: jing
;; Version: version
;; Keywords: repl


;;; Commentary:
;;;
;;; Code:

(require 'cl-generic)
(require 'cl-lib)
(require 'vterm)

(defgroup python-repl nil
  "Python repl wiwth vterm."
  :group 'python-repl)

(defcustom python-repl-hook nil
  "Hook runs after starting the repl."
  :type 'hook
  :group 'python-repl)

;; inferior" buffer usually refers to a buffer that is associated with an external process
(defvar-local python-repl-inferior-buffer nil
  "A local variable holds the current binding buffer to be interactived.")

(defun python-repl--has-running-vterm-process (buffer)
  "Return non nil if BUFFER has a running vterm process."
  (vterm-check-proc buffer))

(defun python-repl--maybe-spawn-python-shell ()
  "To ensure the python shell is spawned.
It will create a ipython shell by default or python shell when
there is no such process in the target buffer."
  )

(defun python-repl--create-default-vterm-buffer ()
  "Create the default buffer with python shell.")

(defun python-repl--select-target-vterm-buffer ()
  "Create a buffer with python shell process by default, if there is no such one.")


;; pop up the buffer automatically?
(defun python-repl-send-line ())


(defun python-repl-send-region ())


(defun python-repl-send-buffer ())


(provide 'python-repl)
;;; python-repl.el ends here

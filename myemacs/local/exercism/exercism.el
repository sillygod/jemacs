;; exercism.el --- try to reduce the cumbersome steps for uploading solution,
;; opening the exercise file, and downloading exercises.

;;;###autoload
(progn
  (defgroup exercism nil
    "Exercism Dojo"
    :group 'external)

  (defcustom exercism-bin nil
    "The path of the exercism executable file"
    :group 'exercism
    :type 'string)

  (defcustom exercism-mode-hook nil
    "Hook to run switching to the exercism minor mode"
    :group 'exercism
    :type 'hook)
  (defcustom exercism-auto-enable t
    "Enable exercism mode when in the exercism workspace"
    :group 'exercism
    :type 'boolean))

;; define some global var here

(setq exercism-mode-map (make-sparse-keymap))

(defvar *exercism-exec*
  (or exercism-bin
      (replace-regexp-in-string "\n" ""
                                (shell-command-to-string "which exercism")))
  "Set the user defined path as exercism executable path by default. We\
will use the result from command which if the user does not set it.")

(defun exercism--command-run (command &optional arg)
  "execute the command with exercism cli and its response as a string\
output"
  (if (zerop (length *exercism-exec*))
      (error "the executable of exercism is not found")
    (shell-command-to-string
     (string-join (mapcar #'prin1-to-string (list *exercism-exec* command arg)) " "))))

;; define the minor mode
;; the keymap give it nil here, we will set the key binding
;; in elsewhere
(define-minor-mode exercism-mode
  "local minor mode for exercism"
  :init-value nil
  :lighter " Exercism"
  :keymap exercism-mode-map
  :group 'exercism)

;; auto enable the minor mode when the buffer path is in the
;; exercism's workspace
;; here, we can't use the auto-mode-list. I think that's for
;; major mode
;; ex.
;; (when exercism-auto-enable
;;   (add-to-list 'auto-mode-alist
;;                (cons (concat (string-trim (exercism--command-run "workspace"))
;;                              ".+\\'") 'exercism-mode)))
;; use hook instead!

(add-hook 'find-file-hook
          (lambda ()
            (when (string-match
                   (concat (string-trim (exercism--command-run "workspace"))
                           ".+\\'")
                   buffer-file-name)
              (exercism-mode))))

;;;###autoload
(defun exercism-open()
  "Temporarily design to open a dired of the workspace
In the further, I will make this to two-column window and open the
buffer with specific files."
  (interactive)
  (let ((workspace (exercism--command-run "workspace")))
    (dired workspace)))

;;;###autoload
(defun exercism-submit()
  (interactive)
  (block nil
    (let ((buffer-name "*Exercism*")
          (current-file (buffer-file-name (get-buffer (buffer-name)))))
    (if (buffer-modified-p)
        (unless
            (y-or-n-p "Buffer is modified since last saved. Do you want to submit it?")
            (return))
      (with-output-to-temp-buffer buffer-name
        (print (exercism--command-run "submit" current-file)))
      (pop-to-buffer buffer-name)))))

;; the following functions need to scrap the web content unless exercism.io provide
;; apis

;;;###autoload
(defun exercism-download()
  )

;;;###autoload
(defun exercism-explore-track()
  )

;;;###autoload
(defun exercism-see-solution()
  )

(provide 'exercism)

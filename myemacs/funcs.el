;; Currently, put some misc helper functions here

(defun hey-god (question)
  "Reduce distraction when you search the answer for the question."
  (interactive "sAsk the god, you'll get it: ")
  (let ((buffer-name "*God's reply*")
        (exectuable-name "howdoi"))
    (with-output-to-temp-buffer buffer-name
      (shell-command (concat exectuable-name " " question)
                     buffer-name
                     "*Messages*")
      (pop-to-buffer buffer-name))))

;; defun a function to open a temp panel to show to result of base64 decode
;; (base64-decode-string "jife")

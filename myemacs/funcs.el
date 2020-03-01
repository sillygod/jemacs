;; Currently, put some misc helper functions here

(defun hey-god (question)
  "Reduce distraction when you search the answer for the question.
Powered by the howdoi"
  (interactive "sAsk the god, you'll get it: ")
  (let ((buffer-name "*God's reply*")
        (exectuable-name "howdoi"))
    (with-output-to-temp-buffer buffer-name
      (shell-command (concat exectuable-name " " question)
                     buffer-name
                     "*Messages*")
      (pop-to-buffer buffer-name))))

;; define a function to open a temp panel to show to result of base64 decode
;; (base64-decode-string "jife")

(defun myemacs-change-tag (old new)
  (when (member old (org-get-tags))
    (org-toggle-tag new 'on)
    (org-toggle-tag old 'off)))

(defun org-rename-tag (old new)
  (interactive "sCurrent tag: \nsNew tag: ")
  (org-map-entries
   (lambda () (myemacs--change-tag old new))
   (format "+%s" old)
   nil))

(defun toggle-fold ()
  (interactive)
  (save-excursion
    (end-of-line)
    (hs-toggle-hiding)))

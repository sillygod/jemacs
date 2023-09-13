;;; package --- Summary
;;; beautify.el                  -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; Code:

;; think how to beautify the org mode with svg-lib
;; TODO / DONE
;; ("\\*+ \\(TODO\\)" . ((svg-tag-make "TODO" :face 'org-todo :margin 0)))
;; ("\\*+ \\(IN PROGRESS\\)" . ((svg-tag-make "IN PROGRESS" :face 'org-todo :margin 0)))
;; ("\\*+ \\(DONE\\)" . ((svg-tag-make "DONE" :face 'org-done :margin 0)))

;; It's seems that svg tag mode is promising enough to be used
;; https://github.com/rougier/svg-tag-mode/blob/main/examples/example-2.el

(defgroup beautify nil
  "Beautify the ui."
  :group 'external)


(defun svg-tag--build-keywords (item)
  "Process an item in order to install it as a new keyword."

  (let* ((pattern  (if (string-match "\\\\(.+\\\\)" (car item))
                       (car item)
                     (format "\\(%s\\)" (car item))))
         (tag      `(funcall ',(nth 0 (cdr item)) (match-string 1)))
         (callback (nth 1 (cdr item)))
         (map (when callback
                (let ((map (make-sparse-keymap)))
                  (define-key map [mouse-1] callback)
                  map)))
         (help     (nth 2 (cdr item))))
    (setq tag ``(face nil
                 display ,,tag
                 cursor-sensor-functions (svg-tag--cursor-function)
                 ,@(if ,callback '(pointer hand))
                 ,@(if ,help `(help-echo ,,help))
                 ,@',(if map `(keymap ,map))))
    `(,pattern 1 ,tag)))




(svg-tag--build-keywords `("hi" . (svg-lib-tag "hi" nil :stroke 0)))

(provide 'beautify)
;;; beautify.el ends here

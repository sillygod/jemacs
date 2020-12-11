;;; package --- Summary
;;; counsel-jq-yq.el                  -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; Code:

(require 'swiper)

(defun counsel-jq-json (&optional query)
  "Call 'jq' with the QUERY with a default of '.'."
  (with-current-buffer
      (ivy-state-buffer ivy-last)
    (unless (region-active-p)
      (mark-whole-buffer))
    (call-process-region
     (point-min)
     (point-max)
     "jq"
     nil
     "*jq*"
     nil
     "-M"
     (or query "."))
    (deactivate-mark)))

(defun counsel-jq-query-function (input)
  ""
  (when (get-buffer "*jq*")
    (with-current-buffer "*jq*"
      (erase-buffer)))
  (counsel-jq-json input)
  (split-string
   (with-current-buffer "*jq*"
     (buffer-string)) "\n"))


(defun counsel-jq-action (x)
  "Show the result on the other window.  Trigger when pressing enter.
X is ignored."
  (split-window-vertically)
  (switch-to-buffer "*jq*"))


;;;###autoload
(defun counsel-jq ()
  "Counsel interface for jq."
  (interactive)
  (ivy-read "jq query: " #'counsel-jq-query-function
            :action #'counsel-jq-action
            :initial-input "."
            :dynamic-collection t
            :caller 'counsel-jq))


;;;###autoload
(defun counsel-yq ()
  "Counsel interface for yq."
  (interactive))

(provide 'counsel-jq-yq)

;;; counsel-jq-yq ends here

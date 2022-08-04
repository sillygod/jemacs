;;; package --- Summary
;;; jq-yq.el                  -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; Code:

(defun jq--query (query content)
  "Call 'jq' with the QUERY with a default of '.'."
    (let ((query (if (string-prefix-p "." query)
                     query
                   (concat "." query))))

      (call-process-region
       content
       nil
       "jq"
       nil
       "*jq*"
       nil
       "-M"
       query)
      (deactivate-mark)))

(defun jq-query-buffer (input content)
  ""
  (when (get-buffer "*jq*")
    (with-current-buffer "*jq*"
      (erase-buffer)))
  (jq--query input content)
  (split-string
   (with-current-buffer "*jq*"
     (buffer-string)) "\n" t))


(defun jq-action (x)
  "Show the result on the other window.  Trigger when pressing enter.
X is ignored."
  (split-window-vertically)
  (switch-to-buffer "*jq*"))

(defun wrap-to-consult (prog fn &rest args)
  (let ((res (append (if (listp prog) prog (list prog)) (apply fn args))))
    (list :command res)))

;;;###autoload
(defun consult-jq ()
  "Consult interface for jq."
  (interactive)
  (unless (featurep 'consult)
    (require 'consult))
  (let ((content (if (region-active-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (buffer-substring-no-properties (point-min) (point-max)))))
    (jq-action
     (consult--read
      (consult--async-command #'(lambda (input) (wrap-to-consult "echo" #'jq-query-buffer input content)))
      :prompt "jq query: "
      :initial "\\."
      :preview-key nil
      :sort nil))))


(defun yq--query (query content)
  "
yq example usage.
cat file | yq '' -
yq '' file "
  (let ((query (if (string-prefix-p "." query)
                   query
                 (concat "." query))))
    (call-process-region
     content
     nil
     "yq"
     nil
     "*yq*"
     nil
     "e"
     query
     "-")
    (deactivate-mark)))

(defun yq-query-buffer (input content)
  ""
  (when (get-buffer "*yq*")
    (with-current-buffer "*yq*"
      (erase-buffer)))
  (yq--query (concat input " | ... comments=\"\"") content)
  (split-string
   (with-current-buffer "*yq*"
     (buffer-string)) "\n"))

(defun yq-action (x)
  "Show the result on the other window.
X is ignored."
  (split-window-vertically)
  (switch-to-buffer "*yq*"))

;;;###autoload
(defun consult-yq ()
  "Consult interface for yq."
  (interactive)
  (unless (featurep 'consult)
    (require 'consult))
  (let ((content (if (region-active-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (buffer-substring-no-properties (point-min) (point-max)))))

    (yq-action
     (consult--read
      (consult--async-command #'(lambda (input) (wrap-to-consult "echo" #'yq-query-buffer input content)))
      :prompt "yq query: "
      :initial "\\."
      :preview-key nil
      :sort nil))))


(provide 'jq-yq)
;;; jq-yq.el ends here

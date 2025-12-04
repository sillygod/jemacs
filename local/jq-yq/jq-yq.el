;;; jq-yq.el --- Summary  -*- lexical-binding: t -*-
;; Author: jing
;; Maintainer: jing
;; Keywords: jq-yq

;;; Commentary:
;;;
;;; Code:


(defcustom jq-path-query
  "[ path(..) | map(select(type == \"string\") // \"[]\" | if test(\"^[0-9a-zA-Z_\\\\[\\\\]]*$\") then . else \"\\\"\" + . + \"\\\"\" end) | join(\".\") ] | sort | unique | .[] | split(\".[]\") | join(\"[]\") | \".\" + ."
  "jq query to extract all JSON paths."
  :type 'string
  :group 'jq)

(defvar jq--functions nil)

(defun jq--get-functions ()
  "Return all jq functions."
  (setq jq--functions
        (mapcar (lambda (c) (car (split-string c "/")))
                (split-string (shell-command-to-string "jq -n -r 'builtins | .[]'") "\n" t))))

(defun jq--call (query content &optional args)
  "Call jq with QUERY on CONTENT, with optional ARGS."
  (with-temp-buffer
    (insert content)
    (let ((status
           (call-process-region
            (point-min) (point-max)
            "jq"
            t t nil
            (or args "-M")
            query)))
      (if (zerop status)
          (buffer-string)
        (progn
          (message "%s" (propertize (buffer-string) 'face 'error))
          nil)))))

(defun jq--get-paths (content &optional query)
  "Get all JSON paths from CONTENT, optionally after applying QUERY."
  (let* ((full-query (concat (or query ".") " | " jq-path-query))
         (output (jq--call full-query content "-r")))
    (when output
      (split-string output "\n" t))))

(defun jq-identifier-p (ch)
     "Check if CH is a valid jq identifier character."
     (when ch
       (or (and (<= ?A ch) (>= ?z ch))
           (and (<= ?0 ch) (>= ?9 ch))
           (eq ?_ ch)
           (eq ?\[ ch)
           (eq ?\] ch)
           (eq ?. ch))))

(defun jq--query (query content)
  "Call 'jq' with the QUERY with a default of '.'."
    (let ((query (if (string-prefix-p "." query)
                     query
                   (concat "." query))))

      (when (get-buffer "*jq*")
        (with-current-buffer "*jq*"
          (erase-buffer)))

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

(defun jq-make-capf (content)
  "Create `completion-at-point' function for jq queries on CONTENT."
  (unless jq--functions
    (jq--get-functions))

  (lambda ()
    (let* ((input (minibuffer-contents-no-properties))
           (start (save-excursion
                    (while (let ((ch (char-before)))
                             (jq-identifier-p ch))
                      (backward-char))
                    (point)))
           (end (save-excursion
                  (while (let ((ch (char-after)))
                           (jq-identifier-p ch))
                    (forward-char))
                  (point)))
           (need-path? (eq ?. (char-after start)))
           (pipe-index (string-match-p (regexp-quote "|") input))
           (query (when pipe-index
                    (replace-regexp-in-string
                     (rx "|" (* (not "|")) line-end)
                     ""
                     input))))

      (list start end
            (if need-path?
                (cl-remove-if #'string-blank-p
                              (jq--get-paths content query))
              jq--functions)
            :category 'jq))))


(defun jq-query-buffer (input content)
  "Run jq on CONTENT with INPUT query, return split output lines."
  (unless (executable-find "jq")
    (user-error "jq not found—install it!"))
  (let ((output (with-temp-buffer
                  (insert content)
                  (if (zerop (call-process-region (point-min) (point-max) "jq" t t nil
                                                  (if (string-prefix-p "." input)
                                                      input
                                                    (concat "." input))))
                      (buffer-string)
                    (user-error "jq failed: invalid query or JSON")))))
    (split-string output "\n" t)))  ; t = omit nulls


(defun jq-action (x)
  "Show the result on the other window.  Trigger when pressing enter.
X is ignored."
  (with-current-buffer "*jq*"
    (if (featurep 'treesit)
        (json-ts-mode)))

  (unless (get-buffer-window "*jq*")
    (split-window-vertically))
  (switch-to-buffer "*jq*"))


(defun wrap-to-consult (prog fn &rest args)
  (let ((res (append (if (listp prog) prog (list prog)) (apply fn args))))
    (list res)))

(defun jq--corfu-enable-in-minibuffer ()
  "Enable Corfu in minibuffer unless Vertico is handling it."
  (when (bound-and-true-p vertico--input)
    (corfu-mode)))

;;;###autoload
(defun consult-jq ()
  "Consult interface for jq."
  (interactive)
  (unless (featurep 'consult)
    (require 'consult))
  (let* ((content (if (region-active-p)
                      (buffer-substring-no-properties (region-beginning) (region-end))
                    (buffer-substring-no-properties (point-min) (point-max))))
         (completion-at-point-functions (list (jq-make-capf content))))

    (unwind-protect
        (progn
          ;; (setq-local corfu-auto-trigger '("." " "))
          (add-hook 'minibuffer-setup-hook 'jq--corfu-enable-in-minibuffer)

          (jq-action
           (consult--read
            (jq--get-paths content)
            :prompt "jq query: "
            :initial "."
            :sort nil
            :require-match nil
            :state (lambda (_action _cand)
                     (jq--query _cand content)
                     (jq-action nil)))))

      (remove-hook 'minibuffer-setup-hook 'jq--corfu-enable-in-minibuffer))))


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
      (consult--process-collection #'(lambda (input) (wrap-to-consult "echo" #'yq-query-buffer input content)))
      :prompt "yq query: "
      :initial "."
      :preview-key nil
      :sort nil))))


(provide 'jq-yq)
;;; jq-yq.el ends here

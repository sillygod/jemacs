;;; consult-yas.el --- Consult interface for yas -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:

(require 'consult)
(require 'yasnippet)
(require 'map)

(defun consult-yas--candidates (templates)
  "Convert TEMPLATES into candidates for `completion-read'."
  (mapcar (lambda (template) (cons (concat
                                    (propertize (concat
                                                 (yas--table-name (yas--template-table template))
                                                 "") 'invisible t)
                                    (yas--template-name template)
                                    " ["
                                    (propertize (yas--template-key template)
                                                'face 'consult-key)
                                    " ]")
                                   template))
          templates))

(defun consult-yas--annotate (candidates)
  (lambda (cand)
    (when-let ((template (cdr (assoc cand candidates)))
               (table-name (yas--table-name (yas--template-table template))))
      (concat
       " "
       (propertize " " 'display `(space :align-to (- right ,(+ 1 (length table-name)))))
       table-name))))


(defun consult-yas--expand-template (template region)
  "Expand TEMPLATE at point saving REGION."
  (deactivate-mark)
  (goto-char (car region))

  ;; Restore marked region (when it existed) so that `yas-expand-snippet'
  ;; overwrites it.
  (when (not (string-equal "" (buffer-substring (car region) (cdr region))))
    (push-mark (point))
    (push-mark (cdr region) nil t))

  (yas-expand-snippet (yas--template-content template)
                      nil nil
                      (yas--template-expand-env template)))

(defun consult-yas--preview ()
  "Preview for `consult-read'.
ACTION is passed from the consult's state function
TEMPLATE is the choosen from the completion-read."
  (let* ((buf (current-buffer))
         (region-active-initially (use-region-p))
         (initial-region (if (use-region-p)
                             (cons (region-beginning) (region-end))
                           (cons (point) (point))))
         (initial-region-contents (buffer-substring (car initial-region) (cdr initial-region)))
         (region (cons (car initial-region) (cdr initial-region))))

    (lambda (action template)

      (with-current-buffer buf
        (let* ((yas-verbosity 0)
               (inhibit-redisplay t)
               (inhibit-read-only t)
               (orig-offset (- (point-max) (cdr region)))
               (yas-prompt-functions '(yas-no-prompt)))
          ;; We always undo any snippet previews before maybe setting up
          ;; some new previews.
          (delete-region (car region) (cdr region))
          (goto-char (car region))
          (setq region (cons (car initial-region) (cdr initial-region)))
          (insert initial-region-contents)
          (when (not region-active-initially)
            (setq region (cons (point) (point)))
            (setq initial-region region)
            (setq initial-region-contents (buffer-substring (car region) (cdr region))))

          ;; Restore the region if it was initially active, so that yasnippet can overwrite
          (when (and region-active-initially (eq action 'return))
            (activate-mark)
            (set-mark (car region))
            (goto-char (cdr region)))

          (when (and template (not (eq action 'return)))
            (unwind-protect
                (consult-yas--expand-template template region)
              (unwind-protect
                  (mapc #'yas--commit-snippet
                        (yas-active-snippets (point-min) (point-max)))
                (setcdr region (- (point-max) orig-offset))
                (deactivate-mark)))
            (redisplay)))))))


(defun consult-yas--tmpl (&optional all-templates)
  "Start a consult--read for yasnippet.
it get all snippets when ALL-TEMPLATES is non-nil."
  (unless (bound-and-true-p yas-minor-mode) ;; compare to boundp, it can prevent the lint error for reference of free variables
    (error "This can only be called with `yas-minor-mode' is active"))

  (barf-if-buffer-read-only)
  (let* ((buffer-undo-list t) ; Prevent querying user (and showing previews) from updating the undo-history
         (candidates
          (consult-yas--candidates
           (if all-templates
               (yas--all-templates (map-values yas--tables))
             (yas--all-templates (yas--get-snippet-tables))))))
    (consult--read
     candidates
     :prompt "Choose a snippet: "
     :annotate (consult-yas--annotate candidates)
     :lookup 'consult--lookup-cdr
     :require-match t
     :state (consult-yas--preview)
     :category 'yasnippet)))

;;;###autoload
(defun consult-yas (arg)
  "An interactive function to choose yas-snippet with consult interfaces.
When ARG is not nil, all templates will appear on the choices."
  (interactive "P")
  (let* ((template (consult-yas--tmpl arg)))
    (yas-expand-snippet (yas--template-content template)
                        nil nil
                        (yas--template-expand-env template))))

(provide 'consult-yas)
;;; consult-yas.el ends here

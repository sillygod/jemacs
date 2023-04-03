;;; package --- Summary
;;; consult-yasnippet.el                  -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; Code:

;; push-mark
;; symbol-function
;; cl-letf

;; implement a consult interface for yas snippets
;; https://github.com/mohkale/consult-yasnippet/blob/master/consult-yasnippet.el


(when (featurep 'consult)

  (defun consult-yasnippet--tmpl (&optional all-templates)
    (barf-if-buffer-read-only)
    (let* ((buffer-undo-list t) ; Prevent querying user (and showing previews) from updating the undo-history
           (candidates
            (consult-yasnippet--candidates
             (if all-templates
                 (yas--all-templates (map-values yas--tables))
               (yas--all-templates (yas--get-snippet-tables))))))
      (consult--read
       candidates
       :prompt "Choose a snippet: "
       :annotate (consult-yasnippet--annotate candidates)
       :lookup 'consult--lookup-cdr
       :require-match t
       :state (consult-yasnippet--preview)
       :category 'yasnippet)))

  (defun consult-yas (arg)
    (interactive "P")
    (let* ((template (consult-yasnippet--tmpl arg))
           (thing-start ())
           (thing-end ()))
      (yas-expand-snippet (yas--template-content template)
                          thing-start thing-end
                          (yas--template-expand-env template)))))

(provide 'consult-yasnippet)
;;; consult-yasnippet.el ends here

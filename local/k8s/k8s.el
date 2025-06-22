;;; k8s.el --- summary -*- lexical-binding: t -*-

;; Author: Jing
;; Maintainer: Jing
;; Version: 0.0.1
;; Keywords: kubernetes

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;; https://emacs.stackexchange.com/questions/82758/mode-for-editing-helm-templates-helm-as-in-helm-charts-not-emacs-helm-mode
;; https://github.com/mrjosh/helm-ls?tab=readme-ov-file
;; https://github.com/emacs-mirror/emacs/blob/master/admin/notes/tree-sitter/starter-guide
;; add helm-ls

;;; Commentary:
;;;
;;; Code:


(require 'treesit)
(require 'yaml-ts-mode)
(require 'compile)

(defgroup helm-ts-mode nil
  "Major mode for Kubernetes Helm templates."
  :group 'languages
  :prefix "helm-ts-mode-")

(defcustom helm-ts-mode-helm-executable "helm"
  "Path to the Helm executable."
  :type 'string
  :group 'helm-ts-mode)

;; Ensure tree-sitter grammar is available
(unless (treesit-language-available-p 'helm)
  (error "Tree-sitter grammar for Helm is not available. Install libtree-sitter-helm.so"))

(unless (treesit-language-available-p 'yaml)
  (error "Tree-sitter grammar for YAML is not available. Install libtree-sitter-yaml.so"))

(defvar helm-ts-mode-helm-font-lock-settings
  (treesit-font-lock-rules
   :language 'helm
   :feature 'template
   :override t
   '((template
      (variable) @font-lock-variable-name-face
      (field (identifier) @font-lock-variable-name-face)
      (selector_expression (field_identifier) @font-lock-variable-name-face)
      ["if" "else" "else if" "end" "range" "define" "block" "with" "template"] @font-lock-keyword-face
      ["{{" "{{-" "}}" "-}}"] @font-lock-delimiter-face
      (variable_definition (variable) @font-lock-variable-name-face)
      (function_call (identifier) @font-lock-function-name-face)
      (comment) @font-lock-comment-face
      (interpreted_string_literal) @font-lock-string-face
      (raw_string_literal) @font-lock-string-face))))


;; Determine the language at point
(defun helm-ts-mode-language-at-point (pos)
  "Return the language at POS in the buffer.
Returns `helm` for Go template regions and `yaml` for YAML regions."
  (let* ((node (treesit-node-at pos 'helm))
         (node-type (treesit-node-type node)))
    (cond
     ((equal node-type "template") 'helm)
     ((equal node-type "text") 'yaml)
     (t
      (let ((parent node))
        (while (and parent (not (equal (treesit-node-type parent) "template")))
          (setq parent (treesit-node-parent parent)))
        (if parent 'helm 'yaml))))))

;; Configure multiple language parsers using `treesit-range-settings`
(defun helm-ts-mode-setup-parsers ()
  "Set up parsers for `helm` and `yaml` languages using `treesit-range-settings`."
  ;; Create parsers
  (when (treesit-ready-p 'helm)
    (treesit-parser-create 'helm))
  (when (treesit-ready-p 'yaml)
    (treesit-parser-create 'yaml))
  ;; Set up range rules
  (setq-local treesit-range-settings
              (treesit-range-rules
               :embed 'yaml
               :host 'helm
               '((text) @capture)
               :embed 'helm
               :host 'helm
               '((template) @capture)))
  ;; Set language-at-point function
  (setq-local treesit-language-at-point-function #'helm-ts-mode-language-at-point))

;; Indentation rules
(defun helm-ts-mode-indent-line ()
  "Indent the current line for Helm templates."
  (let ((node (treesit-node-at (point)))
        (indent-level 0))
    (when node
      (let ((parent (treesit-node-parent node)))
        (while parent
          (when (member (treesit-node-type parent)
                        '("mapping" "sequence" "template" "directive"))
            (setq indent-level (+ indent-level 2)))
          (setq parent (treesit-node-parent parent)))))
    (indent-line-to indent-level)))

;; Helm command integration
(defun helm-ts-mode-run-helm-command (command &rest args)
  "Run a Helm COMMAND with ARGS in the current buffer's directory."
  (let ((default-directory (or (file-name-directory (buffer-file-name)) default-directory)))
    (compile (mapconcat 'identity
                        (cons helm-ts-mode-helm-executable (cons command args))
                        " "))))

(defun helm-ts-mode-lint ()
  "Run `helm lint` on the current chart directory."
  (interactive)
  (helm-ts-mode-run-helm-command "lint" "."))

(defun helm-ts-mode-template ()
  "Run `helm template` on the current chart directory."
  (interactive)
  (helm-ts-mode-run-helm-command "template" "."))

(defun helm-ts-mode-install ()
  "Run `helm install` on the current chart directory."
  (interactive)
  (let ((release-name (read-string "Release name: ")))
    (helm-ts-mode-run-helm-command "install" release-name ".")))

;; Keymap
(defvar helm-ts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") #'helm-ts-mode-lint)
    (define-key map (kbd "C-c C-t") #'helm-ts-mode-template)
    (define-key map (kbd "C-c C-i") #'helm-ts-mode-install)
    map)
  "Keymap for `helm-ts-mode'.")

;; Check if file is in a Helm chart's templates directory
(defun helm-ts-mode-p ()
  "Return non-nil if the current file is a Helm template."
  (and (buffer-file-name)
       (string-match-p "/templates/.*\\.ya?ml\\'" (buffer-file-name))))

;;;###autoload
(define-derived-mode helm-ts-mode yaml-ts-mode "Helm"
  "Major mode for editing Kubernetes Helm templates using tree-sitter."
  :group 'helm-ts-mode

  (setq-local treesit-font-lock-settings
              (append helm-ts-mode-helm-font-lock-settings
                      yaml-ts-mode--font-lock-settings))

  ;; (setq-local treesit-font-lock-settings
  ;; yaml-ts-mode--font-lock-settings)

  (treesit-add-font-lock-rules treesit-font-lock-settings)


  (helm-ts-mode-setup-parsers)

  (setq-local treesit-font-lock-feature-list
              '((template comment)
                (string type)
                (constant escape-sequence number property)
                (bracket delimiter error misc-punctuation)))


  (setq-local indent-line-function #'helm-ts-mode-indent-line)
  ;; use this instead?
  ;; (setq-local treesit-simple-indent-rules helm-ts-mode-indent-line)
  (treesit-major-mode-setup))


;;;###autoload
(add-to-list 'auto-mode-alist '("/templates/.*\\.ya?ml\\'" . helm-ts-mode))


(provide 'k8s)

;;; k8s.el ends here

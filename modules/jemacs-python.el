;;; mycraft --- Summary  -*- lexical-binding: t; -*-
;;; Copyright (C) 2020 mycraft maintainers
;;; Author: Jing
;;; package --- mycraft
;;; Commentary:

;;; Code:

(with-eval-after-load 'python
  (setq python-shell-interpreter "ipython"))

(use-package python-pytest
  :defer t
  :custom
  (python-pytest-confirm t))

(use-package poetry
  :commands
  (poetry-venv-workon
   poetry-venv-deactivate
   poetry-venv-toggle))

(use-package pyvenv
  :commands (pyvenv-mode)
  :hook
  (python-mode . pyvenv-mode))

(use-package pyimport
  :defer t
  :hook
  (before-save-hook . pyimport-remove-unused))

(use-package cython-mode
  :defer t)

(defun jemacs/python-hideshow-forward-sexp-function (arg)
  "Python specific `forward-sexp' function for `hs-minor-mode'.
    Argument ARG is ignored."
  arg  ; Shut up, byte compiler.
  (python-nav-end-of-block))

(defun jemacs/python-setup-hs-mode ()
  "Replace `hs-special-modes-alist' for `python-mode'."
  (let
      ((python-mode-hs-info
        '(python-mode
          "\\s-*\\_<\\(?:def\\|class\\|if\\|elif\\|else\\|for\\|try\\|except\\|with\\|while\\)\\_>" "" "#"
          jemacs/python-hideshow-forward-sexp-function
          nil)))
    (setq hs-special-modes-alist (cl-remove-if #'(lambda (x) (eq (car x) 'python-mode)) hs-special-modes-alist))
    (add-to-list 'hs-special-modes-alist python-mode-hs-info)
    (hs-grok-mode-type)))


(use-package python
  :commands
  (python-nav-end-of-block)
  :hook
  (python-mode . jemacs/python-setup-hs-mode))

(defvar python-run-command "python")
(defvar python-run-args "")

(defun workon-virtual-env-and-lsp ()
  (interactive)
  (poetry-venv-workon)
  (lsp-restart-workspace))

;; TODO: implement this one
(defun my-run-python ()
  "Use vterm to run python shell instead.
     Furthermore, using ipython instead if it's installed."
  (interactive)

  ;; create a vterm buffer with python shell
  ;; maybe, I can reference from the python-inferior-mode

  (if (featurep 'poetry)
      (vterm-send-string (poetry-virtualenv-path))
    (vterm-send-string "python"))
  (vterm-send-return))

(defun python-run-main ()
  (interactive)
  (shell-command
   (format (concat python-run-command " %s %s")
           (shell-quote-argument (or (file-remote-p (buffer-file-name (buffer-base-buffer)) 'localname)
                                     (buffer-file-name (buffer-base-buffer))))
           python-run-args)))

(defun lsp-with-poetry-env ()
  (let ((project (projectile-ensure-project (projectile-project-root))))
    (add-dir-local-variable 'python-mode poetry-venv (poetry-get-virtualenv))
    )
  (poetrpy-venv-workon)
  (lsp))

(use-package lsp-pyright
  :defer t
  :after (lsp-mode poetry)
  :custom ((lsp-pyright-multi-root t))
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

(provide 'jemacs-python)
;;; jemacs-python.el ends here

;;; mycraft --- Summary  -*- lexical-binding: t; -*-
;;; Copyright (C) 2020 mycraft maintainers
;;; Author: Jing
;;; package --- mycraft
;;; Commentary:

;;; Code:

(use-package go-mode
  :defer 2
  :custom
  (gofmt-command "goimports")
  :hook
  (before-save . gofmt-before-save))

(use-package protobuf-mode
  :defer t)

(use-package gomacro-mode
  :hook (go-mode . gomacro-mode))

(use-package gotests
  :defer 1
  :straight (
             :local-repo "~/Desktop/spacemacs-private/local/gotests"
             )
  ;; the file with suffix -test will be treated as tests files by default
  ;; which will function as package files.
  ;; :load-path "~/Desktop/spacemacs-private/local/go-test"
  ;; :config
  ;; (package-generate-autoloads "go-test" "~/Desktop/spacemacs-private/local/go-test")
  ;; (load-library "go-test-autoloads")
  )

(defvar go-run-command "go run")
(defvar go-run-args ""
  "Additional arguments to by supplied to `go run` during runtime.")

(defun go-run-main ()
  (interactive)
  (shell-command
   (format (concat go-run-command " %s %s")
           (shell-quote-argument (or (file-remote-p (buffer-file-name (buffer-base-buffer)) 'localname)
                                     (buffer-file-name (buffer-base-buffer))))
           go-run-args)))


;; github.com/ramya-rao-a/go-outline@latest
;; github.com/cweill/gotests/gotests@latest
;; github.com/fatih/gomodifytags@latest
;; github.com/josharian/impl@latest
;; github.com/haya14busa/goplay/cmd/goplay@latest
;; github.com/go-delve/delve/cmd/dlv@latest
;; go get -u honnef.co/go/tools/cmd/staticcheck@latest
;; golang.org/x/tools/gopls@latest
;; gopkgs

;; write a helper for upgrade these tools
;; reinstall all the tools found in the golang's bin directory?

(provide 'jemacs-go)
;;; jemacs-go.el ends here

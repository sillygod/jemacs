;; fzf.el --- Combine fzf's search function -*- lexical-binding: t; -*-
;; use the term.el to open the command line interface
;; We can combine with helm or ivy's interface but we don't consider them currently.

(require 'subr-x)

(defgroup fzf nil
  "interact with the fzf"
  :group 'external)

(defcustom fzf-executable nil
  "The path of the fzf executable"
  :type 'string
  :group 'fzf)

(defcustom fzf-window-height 15
  "The fzf window's height"
  :type 'integer
  :group 'fzf)

(defcustom fzf-window-position "bottom"
  "Set the position of the fzf window."
  :type 'string
  :group 'fzf)

(defvar *fzf-exec*
  (or fzf-executable
      (replace-regexp-in-string "\n" ""
                                (shell-command-to-string "which fzf")))
  "the path of fzf executable")


(defcustom fzf-directory-start nil
  "The path of the default start directory for fzf-directory"
  :type 'string
  :group 'fzf)

(defun fzf-grep-cmd (cmd args)
  (format (concat cmd " " args)
          (shell-quote-argument
           (if (region-active-p)
               (buffer-substring-no-properties (region-beginning) (region-end)
                                               (read-from-minibuffer (concat cmd ": ")))))))


(defun fzf-start (directory &optional cmd-stream)
  "wow, require the term module"
  (require 'term)
  (window-configuration-to-register :fzf-window)

  )

(defun fzf-directory ()
  "Starts a fzf session at the specified directory."
  (interactive)
  (fzf-start (ido-read-directory-name "Directory: " fzf-directory-start)))

;; async-shell-command

(provide 'fzf)

;; fzf.el --- Combine fzf's search function -*- lexical-binding: t; -*-
;; use the term.el to open the command line interface
;; We can combine with helm or ivy's interface but we don't consider them currently.

(require 'subr-x)

(defgroup fzf nil
  "interact with the fzf"
  :group 'external)

(defcustom exercism-executable nil
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
  (or exercism
      (replace-regexp-in-string "\n" ""
                                (shell-command-to-string "which fzf")))
  "the path of fzf executable")



;; async-shell-command

(provide 'fzf)

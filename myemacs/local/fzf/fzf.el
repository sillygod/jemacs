;; fzf.el --- Combine fzf's search function -*- lexical-binding: t; -*-
;;

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




(provide 'fzf)

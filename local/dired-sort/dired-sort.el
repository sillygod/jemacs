;;; dired-sort.el --- Quick doc search on devdocs.io -*- lexical-binding: t -*-

;;; Commentary:
;; macos built in ls doesn't support --group-directories-first flag
;; brew install coreutils
;;; Code:


(require 'dired)
(require 'ls-lisp)
(require 'savehist)

(when (eq system-type 'darwin)
  (setq insert-directory-program "gls"))

(defcustom dired-sort-suppress-setup-warning nil
  "How to handle the warning in `dired-sort-setup'."
  :type '(choice (const :tag "Display" nil)
                 (const :tag "Suppress" t))
  :group 'dired-sort)

(defvar dired-sort-sort-by-last "version"
  "The main sort criterion used last time.

The value should be one of none, time, size, version (i.e., natural, an improved
version of name and extension).

See the documentation of the \"--sort\" option of GNU ls for details.")
(add-to-list 'savehist-additional-variables 'dired-sort-sort-by-last)
(defvar dired-sort-reverse-last ?n
  "Whether reversing was enabled when sorting was used last time.

The value should be either ?y or ?n.")
(add-to-list 'savehist-additional-variables 'dired-sort-reverse-last)
(defvar dired-sort-group-directories-last ?n
  "Whether directories are grouped together when sorting was used last time.

The value should either be ?y or ?n.")
(add-to-list 'savehist-additional-variables
             'dired-sort-group-directories-last)
(defvar dired-sort-time-last "default"
  "The time option used last time.

The value should be one of default (modified time), atime, access, use, ctime or
status.  If the sort-by option is set as \"time\", the specified time will be
used as the key for sorting.

See the documentation of the \"--time\" option of GNU ls for details.")
(add-to-list 'savehist-additional-variables 'dired-sort-time-last)

;;;###autoload
(defun dired-sort (&optional sort-by reverse group-directories time)
  "Sort dired by the given criteria.

The possible values of SORT-BY, REVERSE, GROUP-DIRECTORIES and TIME are
explained in the variable `dired-sort-reverse-last',
`dired-sort-reverse-last', `dired-sort-group-directories-last' and
`dired-sort-time-last' respectively.  Besides, passing nil to any of these
arguments to use the value used last time (that is, the values of the four
variables mentioned before), even after restarting Emacs if `savehist-mode' is
enabled.  When invoked interactively, nil's are passed to all arguments."
  (interactive)
  (setq dired-sort-sort-by-last (or sort-by dired-sort-sort-by-last)
        dired-sort-reverse-last (or reverse dired-sort-reverse-last)
        dired-sort-group-directories-last
        (or group-directories dired-sort-group-directories-last)
        dired-sort-time-last (or time dired-sort-time-last))
  (dired-sort-other (dired-sort--format-switches)))

(defun dired-sort-set-switches ()
  "Set switch according to variables.
For use in `dired-mode-hook'.
t means no revert buffer.
for more detail, please go to the doc of dired-sort-other."
  (dired-sort-other (dired-sort--format-switches) t))

(defun dired-sort--format-switches ()
  "Return a `dired-listing-switches' string according to `dired-sort' settings."
  (format "%s --sort=%s %s %s %s" dired-listing-switches
          dired-sort-sort-by-last
          (if (char-equal dired-sort-reverse-last ?y)
              "-r" "")
          (if (char-equal dired-sort-group-directories-last ?y)
              "--group-directories-first" "")
          (if (not (string= dired-sort-time-last "default"))
              (concat "--time=" dired-sort-time-last) "")))

(transient-define-prefix dired-sort-transient ()
  "Dired sort transient."
  :transient-suffix 'transient--do-stay
  [["Sort by"
    ("n" (lambda () (interactive) (dired-sort "none"))
     :description (lambda () (concat "none " (if (string= dired-sort-sort-by-last "none") "[X]" "[ ]"))))
    ("t" (lambda () (interactive) (dired-sort "time"))
     :description (lambda () (concat "time " (if (string= dired-sort-sort-by-last "time") "[X]" "[ ]"))))
    ("s" (lambda () (interactive) (dired-sort "size"))
     :description (lambda () (concat "size " (if (string= dired-sort-sort-by-last "size") "[X]" "[ ]"))))
    ("v" (lambda () (interactive) (dired-sort "version"))
     :description (lambda () (concat "version " (if (string= dired-sort-sort-by-last "version") "[X]" "[ ]"))))
    ("e" (lambda () (interactive) (dired-sort "extension"))
     :description (lambda () (concat "extension " (if (string= dired-sort-sort-by-last "extension") "[X]" "[ ]"))))
    ("q" "quit" transient-quit-all)]
   ["Reverse"
    ("r" (lambda () (interactive) (dired-sort nil ?y))
     :description (lambda () (if (char-equal dired-sort-reverse-last ?y) "[X]" "[ ]")))
    ("R" (lambda () (interactive) (dired-sort nil ?n))
     :description (lambda () (if (char-equal dired-sort-reverse-last ?n) "[X]" "[ ]")))]
   ["Group Directories"
    ("g" (lambda () (interactive) (dired-sort nil nil ?y))
     :description (lambda () (if (char-equal dired-sort-group-directories-last ?y) "[X]" "[ ]")))
    ("G" (lambda () (interactive) (dired-sort nil nil ?n))
     :description (lambda () (if (char-equal dired-sort-group-directories-last ?n) "[X]" "[ ]")))]
   ["Time"
    ("d" (lambda () (interactive) (dired-sort nil nil nil "default"))
     :description (lambda () (if (string= dired-sort-time-last "default") "[X]" "[ ]")))
    ("a" (lambda () (interactive) (dired-sort nil nil nil "atime"))
     :description (lambda () (if (string= dired-sort-time-last "atime") "[X]" "[ ]")))
    ("u" (lambda () (interactive) (dired-sort nil nil nil "use"))
     :description (lambda () (if (string= dired-sort-time-last "use") "[X]" "[ ]")))
    ("c" (lambda () (interactive) (dired-sort nil nil nil "ctime"))
     :description (lambda () (if (string= dired-sort-time-last "ctime") "[X]" "[ ]")))
    ("S" (lambda () (interactive) (dired-sort nil nil nil "status"))
     :description (lambda () (if (string= dired-sort-time-last "status") "[X]" "[ ]")))]])


(defun dired-sort--display-setup-warning (msg)
  "Display setup warning according to
`dired-sort-suppress-setup-warning'."
  (let ((display-func
         (cond
          ((null dired-sort-suppress-setup-warning)
           (lambda (m) (display-warning 'dired-sort m)))
          ((eq dired-sort-suppress-setup-warning 'message) #'message)
          ((eq dired-sort-suppress-setup-warning t) #'ignore))))
    (funcall display-func msg)))

;;;###autoload
(defun dired-sort-setup ()
  "Run the default setup.

This will bind the key s in `dired-mode' to run
`dired-sort-transient', and automatically run the sorting
criteria after entering `dired-mode'.  You can choose to not call
this setup function and run a modified version of this function."

  (if (not ls-lisp-use-insert-directory-program)
      (dired-sort--display-setup-warning
       "`ls-lisp-use-insert-directory-program' is nil. The package `dired-sort'
will not work and thus is not set up by `dired-sort-setup'. Set it to t to
suppress this warning. Alternatively, set
`dired-sort-suppress-setup-warning' to suppress warning and skip setup
silently.")
    (if (not
         (with-temp-buffer
           (call-process insert-directory-program nil t nil "--version")
           (string-match-p "GNU" (buffer-string))))
        (dired-sort--display-setup-warning
         "`insert-directory-program' does
not point to GNU ls.  Please set `insert-directory-program' to GNU ls.  The
package `dired-sort' will not work and thus is not set up by
`dired-sort-setup'. Alternatively, set
`dired-sort-suppress-setup-warning' to suppress warning and skip setup
silently.")
      (with-eval-after-load 'evil-core
        (evil-define-key* 'normal dired-mode-map (kbd "s") 'dired-sort-transient)))))

(provide 'dired-sort)

;;; dired-sort.el ends here

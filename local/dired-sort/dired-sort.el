;;; dired-sort.el --- Quick doc search on devdocs.io -*- lexical-binding: t -*-

;;; Commentary:
;; To use, type M-x devdocs-search
;;; Code:

;; https://gitlab.com/xuhdev/dired-quick-sort/-/blob/master/dired-quick-sort.el

(require 'dired)
(require 'ls-lisp)
(require 'savehist)
(require 'hydra)

(defcustom dired-sort-suppress-setup-warning nil
  "How to handle the warning in `dired-sort-setup'."
  :type '(choice (const :tag "Display" nil)
                 (const :tag "Suppress" t)
                 (const :tag "Display as a message" 'message))
  :group 'dired-quick-sort)

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
  "Set switches according to variables. For use in `dired-mode-hook'.
t means no revert buffer. for more detail, please go to the doc of dired-sort-other."
  (dired-sort-other (dired-sort--format-switches) t))

(defun dired-sort--format-switches ()
  "Return a dired-listing-switches string according to
`dired-sort' settings."
  (format "%s --sort=%s %s %s %s" dired-listing-switches
          dired-sort-sort-by-last
          (if (char-equal dired-sort-reverse-last ?y)
              "-r" "")
          (if (char-equal dired-quick-sort-group-directories-last ?y)
              "--group-directories-first" "")
          (if (not (string= dired-quick-sort-time-last "default"))
              (concat "--time=" dired-quick-sort-time-last) "")))

(defun dired-sort--sort-by-last (field)
  (if (string= dired-sort-sort-by-last field) "[X]" "[ ]"))

(defhydra hydra-dired-sort (:hint none)
  "
^Sort by^                   ^Reverse^               ^Group Directories^            ^Time
^^^^^^^^^----------------------------------------------------------------------------------------------------------------
_n_: ?n? none               _r_: ?r? yes            _g_: ?g? yes                   _d_: ?d? default (last modified time)
_t_: ?t? time               _R_: ?R? no             _G_: ?G? no                    _A_: ?A? atime
_s_: ?s? size               ^ ^                     ^ ^                            _a_: ?a? access
_v_: ?v? version (natural)  ^ ^                     ^ ^                            _u_: ?u? use
_e_: ?e? extension          ^ ^                     ^ ^                            _c_: ?c? ctime
_q_: quit                   ^ ^                     ^ ^                            _S_: ?S? status
"
  ("n" (dired-sort "none")
       (dired-sort--sort-by-last "none"))
  ("t" (dired-sort "time")
       (dired-sort--sort-by-last "time"))
  ("s" (dired-sort "size")
       (dired-sort--sort-by-last "size"))
  ("v" (dired-sort "version")
       (dired-sort--sort-by-last "version"))
  ("e" (dired-sort "extension")
       (dired-sort--sort-by-last "extension"))
  ("r" (dired-sort nil ?y)
       (if (char-equal dired-sort-reverse-last ?y) "[X]" "[ ]"))
  ("R" (dired-sort nil ?n)
       (if (char-equal dired-sort-reverse-last ?n) "[X]" "[ ]"))
  ("g" (dired-sort nil nil ?y)
       (if (char-equal dired-sort-group-directories-last ?y) "[X]" "[ ]"))
  ("G" (dired-sort nil nil ?n)
       (if (char-equal dired-sort-group-directories-last ?n) "[X]" "[ ]"))
  ("d" (dired-sort nil nil nil "default")
       (if (string= dired-sort-time-last "default") "[X]" "[ ]"))
  ("A" (dired-sort nil nil nil "atime")
       (if (string= dired-sort-time-last "atime") "[X]" "[ ]"))
  ("a" (dired-sort nil nil nil "access")
       (if (string= dired-sort-time-last "access") "[X]" "[ ]"))
  ("u" (dired-sort nil nil nil "use")
       (if (string= dired-sort-time-last "use") "[X]" "[ ]"))
  ("c" (dired-sort nil nil nil "ctime")
       (if (string= dired-sort-time-last "ctime") "[X]" "[ ]"))
  ("S" (dired-sort nil nil nil "status")
       (if (string= dired-sort-time-last "status") "[X]" "[ ]"))
  ("q" nil "quit" :hint t :color blue))

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

This will bind the key S in `dired-mode' to run
`hydra-dired-sort/body', and automatically run the sorting
criteria after entering `dired-mode'.  You can choose to not call
this setup function and run a modified version of this function
to use your own preferred setup:

  ;; Replace \"S\" with other keys to invoke the dired-quick-sort hydra.
  (define-key dired-mode-map \"S\" 'hydra-dired-quick-sort/body)
  ;; Automatically use the sorting defined here to sort.
  (add-hook 'dired-mode-hook 'dired-quick-sort)"

  (if (not ls-lisp-use-insert-directory-program)
      (dired-quick-sort--display-setup-warning
"`ls-lisp-use-insert-directory-program' is nil. The package `dired-quick-sort'
will not work and thus is not set up by `dired-quick-sort-setup'. Set it to t to
suppress this warning. Alternatively, set
`dired-quick-sort-suppress-setup-warning' to suppress warning and skip setup
silently.")
    (if (not
         (with-temp-buffer
           (call-process insert-directory-program nil t nil "--version")
           (string-match-p "GNU" (buffer-string))))
        (dired-quick-sort--display-setup-warning
"`insert-directory-program' does
not point to GNU ls.  Please set `insert-directory-program' to GNU ls.  The
package `dired-quick-sort' will not work and thus is not set up by
`dired-quick-sort-setup'. Alternatively, set
`dired-quick-sort-suppress-setup-warning' to suppress warning and skip setup
silently.")
      (define-key dired-mode-map "S" 'hydra-dired-quick-sort/body)
      (add-hook 'dired-mode-hook #'dired-quick-sort-set-switches))))

(provide 'dired-quick-sort)




;;; dired-sort.el ends here

;;; rg-panel.el --- Project search over consult-ripgrep, with a filter panel -*- lexical-binding: t; -*-

;; Author: Jing
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (consult "2.0") (transient "0.7"))
;; Keywords: tools, matching
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; `rg-panel-search' is consult-ripgrep's minibuffer -- live results,
;; preview, embark -- with the filters rg takes made visible and kept:
;;
;;   rg deploy-platform/ [py · !tests/** · word]: deploy
;;
;; M-o opens a transient panel over the search.  Each toggle keeps the
;; panel open and runs the search again at once, the input you typed
;; untouched:
;;
;;   Files   i include glob   e exclude glob   t type
;;   Match   w word  c case  f fixed  h hidden  m multiline  p PCRE2
;;   Scope   r project root  . this file's dir  g uncommitted  D directory
;;   Engine  a rg <-> rga (PDF, docx, epub, ipynb, archives)
;;   Sets    x clear  s save as a preset  l apply a preset (or 上次)
;;
;; C-c C-e exports the results into a grep buffer in wgrep: edit them in
;; place for a project-wide replace, then `, ,' writes them.  C-u before
;; the command picks the scope first.  Presets are kept per project in
;; `rg-panel-save-file'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'consult)
(require 'transient)

(declare-function embark-export "embark")
(declare-function wgrep-change-to-wgrep-mode "wgrep")
(declare-function pdf-view-goto-page "pdf-view")
(declare-function project-root "project")
(defvar embark-after-export-hook)


;;; Customization

(defgroup rg-panel nil
  "Project search with a filter panel."
  :group 'consult
  :prefix "rg-panel-")

(defcustom rg-panel-save-file (locate-user-emacs-file "rg-panel-presets.eld")
  "File the presets, and each project's last set, are kept in."
  :type 'file)

(defcustom rg-panel-rga-program "rga"
  "ripgrep-all, which searches PDF, docx, epub, ipynb and archives too."
  :type 'string)

(defcustom rg-panel-hint "M-o 篩選   C-c C-e 匯出成可編輯清單   C-u 先選範圍"
  "The line under the results, or nil for none."
  :type '(choice string (const nil)))

(defcustom rg-panel-rga-notice-after 0.8
  "Seconds rga may run with no result before the prompt says it is
extracting: its first run over a folder of documents is slow, while it
converts and caches them."
  :type 'number)

(defconst rg-panel--extracted-extensions
  '("pdf" "epub" "odt" "docx" "fb2" "ipynb" "html" "htm"
    "zip" "jar" "tar" "gz" "bz2" "xz" "zst" "tgz" "tbz" "tbz2" "als"
    "sqlite" "sqlite3" "db" "mkv" "mp4" "avi" "mp3" "ogg" "flac" "webm")
  "Extensions whose rga hits come from an extractor (`rga
--rga-list-adapters'): their line numbers are the extracted text's, not
the file's, so they open by other means and are never edited in place.")


;;; Filters

(defun rg-panel--empty ()
  "No filter: rg as consult-ripgrep runs it."
  (list :include nil :exclude nil :types nil
        :word nil :case nil :fixed nil :hidden nil :multiline nil :pcre nil
        :engine 'rg))

(defconst rg-panel--flags
  '((:word      "word"      "--word-regexp")
    (:case      "case"      "--case-sensitive")
    (:fixed     "fixed"     "--fixed-strings")
    (:hidden    "hidden"    "--hidden" "--no-ignore")
    (:multiline "multiline" "--multiline")
    (:pcre      "pcre2"     "--pcre2"))
  "Each match flag: (KEY LABEL ARG...) -- its name in the prompt, and
the rg arguments it adds.")

(defun rg-panel--negate (glob)
  "GLOB as an exclusion: `!' before it, once."
  (concat "!" (string-remove-prefix "!" glob)))

(defun rg-panel--file-args (filters)
  "The arguments of FILTERS that pick files: globs and types."
  (append
   (mapcar (lambda (g) (concat "--glob=" g)) (plist-get filters :include))
   (mapcar (lambda (g) (concat "--glob=" (rg-panel--negate g))) (plist-get filters :exclude))
   (mapcar (lambda (ty) (concat "--type=" ty)) (plist-get filters :types))))

(defun rg-panel-args (filters)
  "The rg arguments FILTERS add, one argument each: no shell is in
between, so a glob with a space, or a `!', needs no quoting."
  (append
   (rg-panel--file-args filters)
   (cl-loop for (key _label . args) in rg-panel--flags
            when (plist-get filters key) append args)))

(defun rg-panel--program (filters)
  (if (eq (plist-get filters :engine) 'rga) rg-panel-rga-program "rg"))

(defun rg-panel-command (filters)
  "consult-ripgrep's command line, run by the engine FILTERS name, with
the arguments FILTERS add.  rga reads archives and compressed files with
its own adapters; rg's --search-zip over them finds nothing, so it is
left out."
  (let ((base (cdr (consult--build-args consult-ripgrep-args)))
        (rga (eq (plist-get filters :engine) 'rga)))
    (append (list (rg-panel--program filters))
            (if rga (seq-remove (lambda (a) (member a '("--search-zip" "-z"))) base) base)
            (rg-panel-args filters))))

(defun rg-panel--filter-label (filters)
  "FILTERS as the prompt shows them: `[py · !tests/** · word]', or \"\"."
  (let ((parts (append
                (plist-get filters :types)
                (mapcar (lambda (g) (if (string-search " " g) (format "%S" g) g))
                        (plist-get filters :include))
                (mapcar (lambda (g) (let ((g (rg-panel--negate g)))
                                      (if (string-search " " g) (format "%S" g) g)))
                        (plist-get filters :exclude))
                (cl-loop for (key label . _) in rg-panel--flags
                         when (plist-get filters key) collect label))))
    (if parts (concat " [" (string-join parts " · ") "]") "")))

(defun rg-panel--default-p (filters)
  (and (null (rg-panel-args filters)) (not (eq (plist-get filters :engine) 'rga))))


;;; The session

(defvar rg-panel--session nil
  "The search under way, a plist: :filters, :scope, :paths (the directory
a scope searches), :uncommitted (the files the `uncommitted' scope
searches) and :narrowed (those the filters keep), :dir (where rg runs),
:root (the project's, for presets), :here (the caller's directory),
:minibuffer, :header-ov, :hint-ov, :rerun, :busy (rga is extracting),
:globs (the project's, to complete).")

(defun rg-panel--get (key) (plist-get rg-panel--session key))
(defun rg-panel--put (key value)
  "Set KEY of the search under way; none, nothing (a late timer)."
  (when rg-panel--session
    (setq rg-panel--session (plist-put rg-panel--session key value))))

(defun rg-panel--label ()
  "Where the search looks, as the prompt shows it."
  (let* ((dir (rg-panel--get :dir))
         (name (file-name-nondirectory (directory-file-name dir)))
         (paths (rg-panel--get :paths)))
    (pcase (rg-panel--get :scope)
      ('uncommitted (format "%s/ ±%d" name (length (rg-panel--get :uncommitted))))
      ((or 'here 'pick)
       (let ((p (car paths)))
         (if (file-name-absolute-p p) (abbreviate-file-name p) (concat name "/" p))))
      (_ (concat name "/")))))

(defun rg-panel--header ()
  "The prompt: engine, where, the filters; `: ' last, where consult shows
its running mark."
  (let ((filters (rg-panel--get :filters)))
    (concat (if (eq (plist-get filters :engine) 'rga) "rga " "rg ")
            (rg-panel--label)
            (rg-panel--filter-label filters)
            (when (rg-panel--get :busy) "  · 讀取文件中，首次較慢…")
            ": ")))

(defun rg-panel--show-header ()
  "Draw the prompt as the filters now are, over the one the minibuffer was
opened with; its last two characters, where consult's running mark sits,
are left alone."
  (when-let* ((mb (rg-panel--get :minibuffer))
              ((buffer-live-p mb)))
    (with-current-buffer mb
      (let ((end (max (point-min) (- (minibuffer-prompt-end) 2)))
            (ov (rg-panel--get :header-ov)))
        (unless ov
          (setq ov (make-overlay (point-min) end))
          (rg-panel--put :header-ov ov))
        (move-overlay ov (point-min) end)
        (overlay-put ov 'display (string-remove-suffix ": " (rg-panel--header)))))))

(defun rg-panel--setup ()
  "In the search's minibuffer, as it opens: remember it, and put the hint
line under the results.  vertico draws the results in a `before-string'
at the end of the minibuffer; a higher priority comes after it."
  (rg-panel--put :minibuffer (current-buffer))
  (when rg-panel-hint
    (let ((ov (make-overlay (point-max) (point-max) nil t t)))
      (overlay-put ov 'priority 100)
      (overlay-put ov 'before-string (concat "\n" (propertize rg-panel-hint 'face 'shadow)))
      (rg-panel--put :hint-ov ov))))

(defun rg-panel--refresh ()
  "Run the search again with the filters as they now are, the input as
it is; and redraw the prompt."
  (rg-panel--show-header)
  (when-let* ((rerun (rg-panel--get :rerun)))
    (funcall rerun)))

(defun rg-panel--set (key value)
  "Set filter KEY to VALUE, and search again."
  (rg-panel--put :filters (plist-put (copy-sequence (rg-panel--get :filters)) key value))
  (rg-panel--refresh))


;;; The async pipeline -- consult-ripgrep's, with a stage that searches again

(defun rg-panel--async-rerun ()
  "A stage that remembers the input it last passed on, and passes it on
again when asked (`rg-panel--refresh'): the process stage under it then
runs rg again, as its arguments changed with the filters.  consult's
throttle above drops an input it saw already, so the filters could not
make it search again otherwise.  An input too short to search is
forgotten."
  (lambda (sink)
    (let ((input nil))
      (lambda (action)
        (pcase action
          ('setup
           ;; From the minibuffer, as consult promises for `setup'.
           (let ((mb (current-buffer)))
             (rg-panel--put :rerun
                            (lambda ()
                              (when (and input (buffer-live-p mb))
                                (with-current-buffer mb
                                  (let ((default-directory (rg-panel--get :dir)))
                                    (funcall sink input)))))))
           (funcall sink action))
          ((pred stringp) (setq input action) (funcall sink action))
          ((or 'cancel 'destroy) (setq input nil) (funcall sink action))
          (_ (funcall sink action)))))))

(defun rg-panel--async-watch ()
  "A stage that says, in the prompt, when rga runs long with no result:
its first pass over a folder of documents extracts and caches them."
  (lambda (sink)
    (let ((timer nil) (seen nil))
      (cl-flet ((busy (on)
                  (when timer (cancel-timer timer) (setq timer nil))
                  (unless (eq (and on t) (and (rg-panel--get :busy) t))
                    (rg-panel--put :busy on)
                    (rg-panel--show-header))))
        (lambda (action)
          (pcase action
            (`[indicator running]
             (setq seen nil)
             (when (and (eq (plist-get (rg-panel--get :filters) :engine) 'rga)
                        (not timer))
               (setq timer (run-at-time rg-panel-rga-notice-after nil
                                        (lambda () (setq timer nil)
                                          (unless seen (busy t)))))))
            (`[indicator ,_] (busy nil))
            ((pred consp) (setq seen t) (busy nil))
            ('destroy (busy nil)))
          (funcall sink action))))))

(defvar rg-panel--pcre (make-hash-table :test 'equal)
  "Program -> whether it takes PCRE look-ahead, asked once.  rga is rg
underneath, so rg is asked for both.")

(defun rg-panel--regexp-type (program)
  (let ((known (gethash program rg-panel--pcre 'unknown)))
    (when (eq known 'unknown)
      (setq known (if (consult--grep-lookahead-p program "-P") 'pcre 'extended))
      (puthash program known rg-panel--pcre))
    known))

(defun rg-panel--builder (input)
  "The command line for INPUT with the session's filters and scope, as
consult-ripgrep's builder makes it: (ARGS . HIGHLIGHT).  PCRE2 on takes
the input as one PCRE pattern, as typed."
  (let* ((filters (rg-panel--get :filters))
         (cmd (rg-panel-command filters))
         (paths (rg-panel--paths)))
    (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                 (flags (append cmd opts))
                 (ignore-case
                  (and (not (or (member "-s" flags) (member "--case-sensitive" flags)))
                       (or (member "-i" flags) (member "--ignore-case" flags)
                           (and (or (member "-S" flags) (member "--smart-case" flags))
                                (let (case-fold-search)
                                  (not (string-match-p "[[:upper:]]" arg))))))))
      (cond
       ((or (member "-F" flags) (member "--fixed-strings" flags))
        (cons (append cmd (list "-e" arg) opts paths)
              (apply-partially #'consult--highlight-regexps
                               (list (regexp-quote arg)) ignore-case)))
       ((plist-get filters :pcre)
        (cons (append cmd (list "-e" arg) opts paths) nil))
       (t
        (let ((type (rg-panel--regexp-type "rg")))
          (pcase-let ((`(,re . ,hl) (consult--compile-regexp arg type ignore-case)))
            (when re
              (cons (append cmd (and (eq type 'pcre) '("-P"))
                            (list "-e" (consult--join-regexps re type))
                            opts paths)
                    hl)))))))))

(defun rg-panel--collection ()
  (consult--async-pipeline
   (consult--async-min-input)
   (consult--async-throttle)
   (rg-panel--async-rerun)
   (consult--async-process #'rg-panel--builder :file-handler t)
   (rg-panel--async-watch)
   (consult--grep-format #'rg-panel--builder)))


;;; Opening a result

(defun rg-panel--cand-file (cand)
  (substring-no-properties cand 0 (next-single-property-change 0 'face cand)))

(defun rg-panel--extracted-p (cand)
  "Non-nil when CAND, a result, came from a file rga extracted."
  (and cand
       (eq (plist-get (rg-panel--get :filters) :engine) 'rga)
       (member (downcase (or (file-name-extension (rg-panel--cand-file cand)) ""))
               rg-panel--extracted-extensions)))

(defun rg-panel--cand-text (cand)
  "The matched line's text in CAND."
  (let* ((file-end (next-single-property-change 0 'face cand))
         (line-end (next-single-property-change (1+ file-end) 'face cand)))
    (substring-no-properties cand (min (length cand) (1+ line-end)))))

(defun rg-panel--page (text)
  "The page rga's PDF extractor puts before TEXT (`Page 3: ...'), or nil."
  (when (string-match "\\`Page \\([0-9]+\\): " text)
    (string-to-number (match-string 1 text))))

(defun rg-panel--open-externally (file)
  (let ((cmd (pcase system-type ('darwin "open") ('windows-nt "start") (_ "xdg-open"))))
    (start-process "rg-panel-open" nil cmd (expand-file-name file))
    (message "Opened %s" (abbreviate-file-name file))))

(defun rg-panel--open-extracted (cand)
  "Open CAND, a hit rga extracted: a PDF at its page with pdf-tools, an
HTML or notebook at the text, anything else in its own application."
  (let* ((file (expand-file-name (rg-panel--cand-file cand) (rg-panel--get :dir)))
         (ext (downcase (or (file-name-extension file) "")))
         (text (rg-panel--cand-text cand)))
    (cond
     ((and (equal ext "pdf") (require 'pdf-tools nil t))
      (find-file file)
      (when-let* ((page (rg-panel--page text))
                  ((derived-mode-p 'pdf-view-mode)))
        (pdf-view-goto-page page)))
     ((member ext '("html" "htm" "ipynb"))
      (find-file file)
      (goto-char (point-min))
      (let ((case-fold-search t)
            (words (car (split-string (string-trim text) "[ \t]\\{2,\\}" t))))
        (when (and words (search-forward words nil t))
          (goto-char (match-beginning 0)))))
     (t (rg-panel--open-externally file)))))

(defun rg-panel--state ()
  "consult-ripgrep's preview and jump, for a text hit; an extracted one
is neither previewed nor jumped to by line -- it opens after."
  (let ((inner (consult--grep-state)))
    (lambda (action cand)
      (funcall inner action (and cand (not (rg-panel--extracted-p cand)) cand)))))


;;; Scopes

(defun rg-panel--git-uncommitted (dir)
  "The files under DIR with changes git has not committed: modified,
staged, and new ones it does not ignore; deleted ones are gone.  Paths
relative to DIR."
  (let ((default-directory (file-name-as-directory dir)))
    (when (eq 0 (ignore-errors (process-file "git" nil nil nil "rev-parse" "--is-inside-work-tree")))
      (let ((files (append
                    (ignore-errors (process-lines "git" "ls-files" "--modified" "--others"
                                                  "--exclude-standard" "--" "."))
                    (ignore-errors (process-lines "git" "diff" "--name-only" "--cached"
                                                  "--relative" "--" ".")))))
        (sort (seq-filter #'file-exists-p (delete-dups files)) #'string<)))))

(defun rg-panel--narrow (files)
  "FILES, less those the file filters leave out.  rg searches a file
named on its command line whatever its globs or types say, so they are
applied here: by rg itself, listing the files it would search.  Asked
again only when the filters that pick files change."
  (let ((args (rg-panel--file-args (rg-panel--get :filters))))
    (if (null args)
        files
      (unless (equal args (car (rg-panel--get :narrowed)))
        (let* ((default-directory (rg-panel--get :dir))
               (kept (make-hash-table :test 'equal)))
          (dolist (f (ignore-errors (apply #'process-lines "rg" "--files" "--hidden" args)))
            (puthash f t kept))
          (rg-panel--put :narrowed (cons args (seq-filter (lambda (f) (gethash f kept)) files)))))
      (cdr (rg-panel--get :narrowed)))))

(defun rg-panel--paths ()
  "What rg is given to search, run at the search's directory: the
scope's directory, the uncommitted files the filters keep (the null
device if none, so that rg searches nothing), or all of it."
  (if (eq (rg-panel--get :scope) 'uncommitted)
      (or (rg-panel--narrow (rg-panel--get :uncommitted)) (list null-device))
    (or (rg-panel--get :paths) (list "."))))

(defun rg-panel--relative (dir)
  "DIR as rg is given it, run where the search runs: relative inside it."
  (let ((root (rg-panel--get :dir)))
    (if (file-in-directory-p dir root)
        (let ((rel (file-relative-name dir root)))
          (if (equal rel ".") "./" (file-name-as-directory rel)))
      (file-name-as-directory (expand-file-name dir)))))

(defun rg-panel--scope (scope &optional dir)
  "Search SCOPE: `root', `here' (the caller's directory), `uncommitted'
or `pick' (DIR, else asked for).  Return nil, and keep the scope, when
there is nothing to search in it."
  (pcase scope
    ('root (rg-panel--put :paths nil))
    ('here (rg-panel--put :paths (list (rg-panel--relative (rg-panel--get :here)))))
    ('uncommitted
     (let ((files (rg-panel--git-uncommitted (rg-panel--get :dir))))
       (if files
           (progn (rg-panel--put :uncommitted files)
                  (rg-panel--put :narrowed nil))
         (message "Nothing uncommitted here")
         (setq scope nil))))
    ('pick
     (let ((d (or dir (let ((enable-recursive-minibuffers t))
                        (read-directory-name "Search in: " (rg-panel--get :here) nil t)))))
       (rg-panel--put :paths (list (rg-panel--relative d))))))
  (when scope
    (rg-panel--put :scope scope)
    scope))

(defconst rg-panel--scope-names
  '(("project root" . root) ("this file's directory" . here)
    ("uncommitted files" . uncommitted) ("a directory…" . pick)))

(defun rg-panel--read-scope ()
  (cdr (assoc (completing-read "Search in: " rg-panel--scope-names nil t)
              rg-panel--scope-names)))


;;; Presets, per project

(defvar rg-panel--store nil "The save file as last read: ((ROOT . ((NAME . FILTERS)...))...).")
(defvar rg-panel--store-stamp nil)

(defconst rg-panel-last-name "上次"
  "The preset each project's last search is kept under.")

(defun rg-panel--store-read ()
  "The save file, read again only when it changed."
  (let ((stamp (file-attribute-modification-time (file-attributes rg-panel-save-file))))
    (unless (and rg-panel--store-stamp (equal stamp rg-panel--store-stamp))
      (setq rg-panel--store
            (and stamp (condition-case nil
                           (with-temp-buffer
                             (let ((coding-system-for-read 'utf-8))
                               (insert-file-contents rg-panel-save-file))
                             (read (current-buffer)))
                         (error rg-panel--store)))
            rg-panel--store-stamp stamp))
    rg-panel--store))

(defun rg-panel-presets (root)
  "The presets of the project at ROOT: ((NAME . FILTERS)...), the last
search's under `rg-panel-last-name'."
  (alist-get (file-name-as-directory root) (rg-panel--store-read) nil nil #'equal))

(defun rg-panel-preset-save (root name filters)
  "Keep FILTERS as NAME for the project at ROOT, over the file as it is."
  (let* ((store (copy-tree (rg-panel--store-read)))
         (key (file-name-as-directory root))
         (presets (alist-get key store nil nil #'equal)))
    (setf (alist-get name presets nil nil #'equal) filters)
    (setf (alist-get key store nil nil #'equal) presets)
    (let ((dir (file-name-directory rg-panel-save-file)))
      (unless (file-directory-p dir) (make-directory dir t)))
    (with-temp-file rg-panel-save-file
      (setq buffer-file-coding-system 'utf-8)
      (let ((print-length nil) (print-level nil))
        (prin1 store (current-buffer))
        (insert "\n")))
    (setq rg-panel--store store
          rg-panel--store-stamp (file-attribute-modification-time
                                 (file-attributes rg-panel-save-file)))
    filters))

(defun rg-panel--remember-last ()
  "Keep the session's filters as its project's last set, unless none, or
the same."
  (let ((filters (rg-panel--get :filters))
        (root (rg-panel--get :root)))
    (unless (or (rg-panel--default-p filters)
                (equal filters (cdr (assoc rg-panel-last-name (rg-panel-presets root)))))
      (rg-panel-preset-save root rg-panel-last-name filters))))


;;; The panel

(defun rg-panel--on (key)
  (if (plist-get (rg-panel--get :filters) key)
      (propertize "[開]" 'face 'success)
    (propertize "[關]" 'face 'shadow)))

(defmacro rg-panel--define-toggle (key flag doc)
  "A panel command that toggles filter KEY, labelled FLAG."
  (let ((name (intern (format "rg-panel-toggle-%s" (substring (symbol-name key) 1)))))
    `(transient-define-suffix ,name ()
       ,doc
       :transient t
       :description (lambda () (format "%-12s %s" ,flag (rg-panel--on ,key)))
       (interactive)
       (rg-panel--set ,key (not (plist-get (rg-panel--get :filters) ,key))))))

(rg-panel--define-toggle :word "word -w" "Whole words only.")
(rg-panel--define-toggle :case "case -s" "Case-sensitive; smart case when off.")
(rg-panel--define-toggle :fixed "fixed -F" "The input as a fixed string, not a regexp.")
(rg-panel--define-toggle :hidden "hidden" "Hidden and ignored files too.")
(rg-panel--define-toggle :multiline "multiline -U" "Matches may span lines.")
(rg-panel--define-toggle :pcre "PCRE2 -P" "The input as one PCRE2 pattern, as typed.")

(defun rg-panel--project-extensions ()
  "Globs for the extensions and top directories in the search's files,
listed once a search."
  (or (rg-panel--get :globs)
      (let* ((default-directory (rg-panel--get :dir))
             (files (ignore-errors (process-lines "rg" "--files" "--max-depth" "6")))
             (exts (delete-dups (delq nil (mapcar (lambda (f) (when-let* ((e (file-name-extension f)))
                                                                (concat "*." e)))
                                                  files))))
             (dirs (delete-dups (delq nil (mapcar (lambda (f) (when (string-search "/" f)
                                                                (concat (car (split-string f "/")) "/**")))
                                                  files))))
             (globs (append (sort exts #'string<) (sort dirs #'string<))))
        (rg-panel--put :globs globs)
        globs)))

(defun rg-panel--read-globs (prompt current)
  (let ((enable-recursive-minibuffers t))
    (completing-read-multiple prompt (rg-panel--project-extensions) nil nil
                              (and current (string-join current ",")))))

(transient-define-suffix rg-panel-include ()
  "Search only files matching globs, chosen from the project's."
  :transient t
  :description (lambda () (format "include  %s" (string-join (plist-get (rg-panel--get :filters) :include) " ")))
  (interactive)
  (rg-panel--set :include (rg-panel--read-globs "Include globs: "
                                                (plist-get (rg-panel--get :filters) :include))))

(transient-define-suffix rg-panel-exclude ()
  "Leave out files matching globs."
  :transient t
  :description (lambda () (format "exclude  %s" (mapconcat #'rg-panel--negate (plist-get (rg-panel--get :filters) :exclude) " ")))
  (interactive)
  (rg-panel--set :exclude (mapcar (lambda (g) (string-remove-prefix "!" g))
                                  (rg-panel--read-globs "Exclude globs: "
                                                        (plist-get (rg-panel--get :filters) :exclude)))))

(defun rg-panel--type-list ()
  "rg's file types: (NAME . GLOBS), from `rg --type-list'."
  (mapcar (lambda (line) (let ((parts (split-string line ": ")))
                           (cons (car parts) (cadr parts))))
          (ignore-errors (process-lines "rg" "--type-list"))))

(transient-define-suffix rg-panel-type ()
  "Search only files of rg types."
  :transient t
  :description (lambda () (format "type     %s" (string-join (plist-get (rg-panel--get :filters) :types) " ")))
  (interactive)
  (let* ((types (rg-panel--type-list))
         (completion-extra-properties
          (list :annotation-function (lambda (n) (concat "  " (cdr (assoc n types))))))
         (enable-recursive-minibuffers t))
    (rg-panel--set :types (completing-read-multiple
                           "Types: " types nil t
                           (string-join (plist-get (rg-panel--get :filters) :types) ",")))))

(defun rg-panel--scope-mark (scope)
  (if (eq (rg-panel--get :scope) scope) (propertize "●" 'face 'success) " "))

(defmacro rg-panel--define-scope (scope label)
  (let ((name (intern (format "rg-panel-scope-%s" scope))))
    `(transient-define-suffix ,name ()
       ,(format "Search %s." label)
       :transient t
       :description (lambda () (format "%s %s" (rg-panel--scope-mark ',scope) ,label))
       (interactive)
       (when (rg-panel--scope ',scope)
         (rg-panel--refresh)))))

(rg-panel--define-scope root "project root")
(rg-panel--define-scope here "this file's directory")
(rg-panel--define-scope uncommitted "uncommitted files")
(rg-panel--define-scope pick "a directory…")

(transient-define-suffix rg-panel-toggle-engine ()
  "Search with rga, which reads PDF, docx, epub, ipynb and archives too."
  :transient t
  :description (lambda () (if (eq (plist-get (rg-panel--get :filters) :engine) 'rga)
                              (concat "rga " (propertize "[開]" 'face 'success) " PDF/docx/epub/…")
                            (concat "rga " (propertize "[關]" 'face 'shadow) " PDF/docx/epub/…")))
  (interactive)
  (unless (or (eq (plist-get (rg-panel--get :filters) :engine) 'rga)
              (executable-find rg-panel-rga-program))
    (user-error "No %s on the PATH" rg-panel-rga-program))
  (rg-panel--set :engine (if (eq (plist-get (rg-panel--get :filters) :engine) 'rga) 'rg 'rga)))

(transient-define-suffix rg-panel-clear ()
  "Every filter off; rg again."
  :transient t
  :description "clear all"
  (interactive)
  (rg-panel--put :filters (rg-panel--empty))
  (rg-panel--refresh))

(transient-define-suffix rg-panel-save ()
  "Keep the filters as a preset of this project."
  :transient t
  :description "save as a preset…"
  (interactive)
  (let* ((enable-recursive-minibuffers t)
         (name (read-string "Preset name: ")))
    (when (string-empty-p (string-trim name)) (user-error "A preset needs a name"))
    (rg-panel-preset-save (rg-panel--get :root) (string-trim name) (rg-panel--get :filters))
    (message "Saved preset %s" (string-trim name))))

(transient-define-suffix rg-panel-load ()
  "Apply a preset of this project, or its last set (上次)."
  :transient t
  :description "apply a preset…"
  (interactive)
  (let* ((presets (rg-panel-presets (rg-panel--get :root)))
         (names (delete-dups (cons rg-panel-last-name (mapcar #'car presets))))
         (enable-recursive-minibuffers t)
         (name (completing-read "Preset: " names nil t))
         (filters (cdr (assoc name presets))))
    (unless filters (user-error "No %s here yet" name))
    (rg-panel--put :filters (append filters nil))
    (rg-panel--refresh)))

(defun rg-panel--menu-heading ()
  (concat (string-remove-suffix ": " (rg-panel--header))
          (propertize "   — the results follow each change" 'face 'shadow)))

(transient-define-prefix rg-panel-menu ()
  "The search's filters, scope and engine; the results follow each toggle."
  [:description rg-panel--menu-heading
   ["Files"
    ("i" rg-panel-include)
    ("e" rg-panel-exclude)
    ("t" rg-panel-type)]
   ["Match"
    ("w" rg-panel-toggle-word)
    ("c" rg-panel-toggle-case)
    ("f" rg-panel-toggle-fixed)
    ("h" rg-panel-toggle-hidden)
    ("m" rg-panel-toggle-multiline)
    ("p" rg-panel-toggle-pcre)]
   ["Scope"
    ("r" rg-panel-scope-root)
    ("." rg-panel-scope-here)
    ("g" rg-panel-scope-uncommitted)
    ("D" rg-panel-scope-pick)]
   ["Engine"
    ("a" rg-panel-toggle-engine)]
   ["Sets"
    ("x" rg-panel-clear)
    ("s" rg-panel-save)
    ("l" rg-panel-load)]]
  [:class transient-row
   ("q" "close" transient-quit-one)
   ("M-o" "close" transient-quit-one)])

(defun rg-panel-filters ()
  "Open the filter panel over the search."
  (interactive)
  (unless rg-panel--session (user-error "No project search under way"))
  (rg-panel-menu))


;;; Export into wgrep

(defun rg-panel--after-export ()
  "In the buffer `rg-panel-export-edit' exported to: wgrep on, which says
how to write the edits."
  (when (and (derived-mode-p 'grep-mode) (require 'wgrep nil t))
    (wgrep-change-to-wgrep-mode)))

(defun rg-panel-export-edit ()
  "Export the results into a grep buffer with wgrep on, to edit in place.
rga's results are exported read-only: their lines are extracted text.
embark keeps the hook as it is now for the buffer it makes after the
minibuffer closes, so this export, and no other, turns wgrep on."
  (interactive)
  (unless (require 'embark-consult nil t) (user-error "embark-consult is needed"))
  (let ((embark-after-export-hook
         (if (eq (plist-get (rg-panel--get :filters) :engine) 'rga)
             (progn (message "rga's results are extracted text: exported read-only")
                    embark-after-export-hook)
           (cons #'rg-panel--after-export embark-after-export-hook))))
    (embark-export)))


;;; The command

(defvar-keymap rg-panel-minibuffer-map
  :doc "Keys while the search is open."
  "M-o"     #'rg-panel-filters
  "C-c C-e" #'rg-panel-export-edit)

(defun rg-panel--root ()
  (when-let* ((proj (project-current nil)))
    (expand-file-name (project-root proj))))

;;;###autoload
(defun rg-panel-search (&optional initial pick-scope dir)
  "Search the project with ripgrep, its filters in view and at hand.
consult-ripgrep's minibuffer, preview and embark, with the directory and
the filters in the prompt.  M-o opens the filter panel, C-c C-e exports
the results into wgrep.  INITIAL is the input to start with.  With
PICK-SCOPE (\\[universal-argument]), pick where to search first.  DIR,
else the project's root, else one asked for, is where it searches."
  (interactive (list nil current-prefix-arg))
  (let* ((root (file-name-as-directory
                (expand-file-name
                 (or dir (rg-panel--root)
                     (read-directory-name "Search in: " nil nil t)))))
         (rg-panel--session (list :filters (rg-panel--empty) :scope 'root :paths nil
                                  :dir root :root root
                                  :here (expand-file-name default-directory)))
         (default-directory root))
    (when pick-scope
      (rg-panel--scope (rg-panel--read-scope)))
    (let ((cand
           (unwind-protect
               (minibuffer-with-setup-hook #'rg-panel--setup
                 (consult--read
                  (rg-panel--collection)
                  :prompt (rg-panel--header)
                  :command #'consult-ripgrep
                  :keymap rg-panel-minibuffer-map
                  :lookup #'consult--lookup-member
                  :state (rg-panel--state)
                  :initial initial
                  :add-history (thing-at-point 'symbol)
                  :require-match t
                  :category 'consult-grep
                  :group #'consult--prefix-group
                  :history '(:input consult--grep-history)
                  :sort nil))
             ;; Kept however the search ends: C-g after a look counts.
             (with-demoted-errors "rg-panel: %S" (rg-panel--remember-last)))))
      (when (rg-panel--extracted-p cand)
        (rg-panel--open-extracted cand))
      cand)))

(provide 'rg-panel)
;;; rg-panel.el ends here

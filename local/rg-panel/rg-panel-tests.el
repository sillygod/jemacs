;;; rg-panel-tests.el --- Tests for rg-panel  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;   emacs --batch -Q -L ~/.emacs.d/local/rg-panel \
;;         -l rg-panel-tests.el -f ert-run-tests-batch-and-exit
;;
;; The packages it needs are taken from straight's build directory.  No
;; network: rg and git run over temporary directories.

;;; Code:

(dolist (dep '("compat" "cond-let" "seq" "transient" "consult" "embark"
               "embark-consult" "wgrep" "vertico"))
  (let ((dir (expand-file-name dep "~/.emacs.d/emacs-home/straight/build/")))
    (when (file-directory-p dir) (add-to-list 'load-path dir))))

(require 'ert)
(require 'cl-lib)
(require 'rg-panel)
(require 'embark-consult)
(require 'wgrep)
(require 'vertico)

(defconst rg-panel-tests--args
  "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip"
  "consult-ripgrep's arguments as the user has them.")

(defun rg-panel-tests--write (dir files)
  "Write FILES, ((NAME . TEXT)...), under DIR."
  (pcase-dolist (`(,name . ,text) files)
    (let ((file (expand-file-name name dir)))
      (make-directory (file-name-directory file) t)
      (with-temp-file file (insert text)))))

(defconst rg-panel-tests--files
  '(("src/app.py" . "def deploy():\n    return 'deploy now'\n\ndef redeploy():\n    deploy()\n")
    ("src/web.js" . "function deploy() { return 1 }\n")
    ("tests/test_app.py" . "def test_deploy():\n    assert deploy()\n")
    ("docs/readme.md" . "Run deploy to ship.\nDEPLOY is loud.\n")
    ("my dir/note.txt" . "deploy in a dir with a space\n")
    (".hidden/h.txt" . "deploy hidden\n"))
  "A small project.")

(defun rg-panel-tests--git (&rest args)
  (apply #'process-lines "git" "-c" "user.email=t@t" "-c" "user.name=t"
         "-c" "core.hooksPath=/dev/null" "-c" "commit.gpgsign=false" args))

(defmacro rg-panel-tests--with-project (&rest body)
  "BODY in a session over a fresh copy of `rg-panel-tests--files', with its
own save file.  `dir' is the project."
  (declare (indent 0))
  `(let* ((dir (file-name-as-directory (file-truename (make-temp-file "rg-panel-" t))))
          (rg-panel-save-file (expand-file-name "presets.eld" (make-temp-file "rg-panel-store-" t)))
          (rg-panel--store nil)
          (rg-panel--store-stamp nil)
          (consult-ripgrep-args rg-panel-tests--args)
          (default-directory dir)
          (rg-panel--session (list :filters (rg-panel--empty) :scope 'root :paths nil
                                   :dir dir :root dir :here (expand-file-name "src/" dir))))
     (rg-panel-tests--write dir rg-panel-tests--files)
     (unwind-protect
         (progn ,@body)
       (ignore-errors (delete-directory dir t))
       (ignore-errors (delete-directory (file-name-directory rg-panel-save-file) t)))))

(defmacro rg-panel-tests--with-git (&rest body)
  "As `rg-panel-tests--with-project', the project a git repository with
changes: src/app.py modified, src/new.py new, docs/staged.md staged,
tests/test_app.py deleted."
  (declare (indent 0))
  `(rg-panel-tests--with-project
     (skip-unless (executable-find "git"))
     (rg-panel-tests--write dir '((".gitignore" . "ignored.log\n")
                                  ("ignored.log" . "deploy ignored\n")))
     (rg-panel-tests--git "init" "-q")
     (rg-panel-tests--git "add" "-A")
     (rg-panel-tests--git "commit" "-qm" "init")
     (rg-panel-tests--write dir '(("src/app.py" . "def deploy():\n    pass  # changed deploy\n")
                                  ("src/new.py" . "new deploy\n")
                                  ("docs/staged.md" . "staged deploy\n")
                                  ("ignored.log" . "deploy still ignored\n")))
     (rg-panel-tests--git "add" "docs/staged.md")
     (delete-file (expand-file-name "tests/test_app.py" dir))
     ,@body))

(defun rg-panel-tests--run (input)
  "The files rg finds INPUT in, run as the search runs it, sorted."
  (let* ((args (car (rg-panel--builder input)))
         (lines (apply #'process-lines-ignore-status (car args) (cdr args))))
    (sort (delete-dups
           (mapcar (lambda (l) (string-remove-prefix "./" (car (split-string l "\0"))))
                   lines))
          #'string<)))

(defun rg-panel-tests--filters (&rest plist)
  (let ((filters (rg-panel--empty)))
    (while plist
      (setq filters (plist-put filters (pop plist) (pop plist))))
    filters))


;;; Filters -> arguments

(ert-deftest rg-panel-test-no-filter-no-args ()
  (should (equal (rg-panel-args (rg-panel--empty)) nil)))

(ert-deftest rg-panel-test-globs-types-flags ()
  (should (equal (rg-panel-args
                  (rg-panel-tests--filters
                   :include '("*.py") :exclude '("tests/**") :types '("py" "js")
                   :word t :case t :fixed t :hidden t :multiline t :pcre t))
                 '("--glob=*.py" "--glob=!tests/**" "--type=py" "--type=js"
                   "--word-regexp" "--case-sensitive" "--fixed-strings"
                   "--hidden" "--no-ignore" "--multiline" "--pcre2"))))

(ert-deftest rg-panel-test-exclude-negated-once ()
  "An exclusion typed with its own `!' is not negated twice."
  (should (equal (rg-panel-args (rg-panel-tests--filters :exclude '("!*.min.js" "dist/**")))
                 '("--glob=!*.min.js" "--glob=!dist/**"))))

(ert-deftest rg-panel-test-glob-quoting ()
  "A glob with a space or shell characters is one argument, as typed: no
shell runs rg."
  (let ((args (rg-panel-args (rg-panel-tests--filters :include '("my dir/**" "*.{py,js}")))))
    (should (equal args '("--glob=my dir/**" "--glob=*.{py,js}")))))

(ert-deftest rg-panel-test-command-engine ()
  (let ((consult-ripgrep-args rg-panel-tests--args))
    (let ((rg (rg-panel-command (rg-panel-tests--filters :word t))))
      (should (equal (car rg) "rg"))
      (should (member "--search-zip" rg))
      (should (equal (last rg) '("--word-regexp"))))
    (let ((rga (rg-panel-command (rg-panel-tests--filters :engine 'rga :types '("py")))))
      (should (equal (car rga) rg-panel-rga-program))
      ;; rga reads archives itself; rg's --search-zip makes it find nothing.
      (should-not (member "--search-zip" rga))
      (should (member "--null" rga))
      (should (equal (last rga) '("--type=py"))))))

(ert-deftest rg-panel-test-builder ()
  (rg-panel-tests--with-project
    (let ((args (car (rg-panel--builder "deploy"))))
      (should (equal (car args) "rg"))
      (should (equal (last args 1) '(".")))
      (should (member "deploy" args)))
    (rg-panel--set :fixed t)
    (let ((args (car (rg-panel--builder "a.b(c"))))
      (should (equal (last args 3) '("-e" "a.b(c" "."))))
    (rg-panel--set :fixed nil)
    (rg-panel--set :pcre t)
    (let ((args (car (rg-panel--builder "(?<=def )deploy"))))
      (should (member "--pcre2" args))
      (should (equal (last args 3) '("-e" "(?<=def )deploy" "."))))
    (rg-panel--set :pcre nil)
    ;; Options after -- still go to rg, after the filters.
    (let ((args (car (rg-panel--builder "deploy -- -t py"))))
      (should (equal (last args 3) '("-t" "py" "."))))))

(ert-deftest rg-panel-test-filters-run ()
  "The arguments do what they say, in rg itself."
  (skip-unless (executable-find "rg"))
  (rg-panel-tests--with-project
    (should (equal (rg-panel-tests--run "deploy")
                   '("docs/readme.md" "my dir/note.txt" "src/app.py" "src/web.js" "tests/test_app.py")))
    (rg-panel--set :include '("*.py"))
    (should (equal (rg-panel-tests--run "deploy") '("src/app.py" "tests/test_app.py")))
    (rg-panel--set :exclude '("tests/**"))
    (should (equal (rg-panel-tests--run "deploy") '("src/app.py")))
    (rg-panel--put :filters (rg-panel--empty))
    (rg-panel--set :include '("my dir/**"))
    (should (equal (rg-panel-tests--run "deploy") '("my dir/note.txt")))
    (rg-panel--put :filters (rg-panel--empty))
    (rg-panel--set :types '("js"))
    (should (equal (rg-panel-tests--run "deploy") '("src/web.js")))
    (rg-panel--put :filters (rg-panel--empty))
    (should (= (length (rg-panel-tests--run "deplo")) 5))
    (rg-panel--set :word t)
    (should (equal (rg-panel-tests--run "deplo") nil))
    (rg-panel--put :filters (rg-panel--empty))
    (rg-panel--set :fixed t)
    (should (equal (rg-panel-tests--run "deploy()") '("src/app.py" "src/web.js" "tests/test_app.py")))
    (rg-panel--put :filters (rg-panel--empty))
    (should-not (member ".hidden/h.txt" (rg-panel-tests--run "deploy")))
    (rg-panel--set :hidden t)
    (should (member ".hidden/h.txt" (rg-panel-tests--run "deploy")))))

(ert-deftest rg-panel-test-case-sensitive-run ()
  (skip-unless (executable-find "rg"))
  (rg-panel-tests--with-project
    (should (member "docs/readme.md" (rg-panel-tests--run "deploy is")))
    (rg-panel--set :case t)
    (should-not (member "docs/readme.md" (rg-panel-tests--run "deploy is")))))


;;; Scopes

(ert-deftest rg-panel-test-scope-root-here-pick ()
  (rg-panel-tests--with-project
    (should (eq (rg-panel--scope 'here) 'here))
    (should (equal (rg-panel--paths) '("src/")))
    (should (equal (last (car (rg-panel--builder "deploy"))) '("src/")))
    (should (eq (rg-panel--scope 'pick (expand-file-name "my dir" dir)) 'pick))
    (should (equal (rg-panel--paths) '("my dir/")))
    (should (eq (rg-panel--scope 'pick dir) 'pick))
    (should (equal (rg-panel--paths) '("./")))
    (let ((outside (file-name-as-directory (file-truename temporary-file-directory))))
      (rg-panel--scope 'pick outside)
      (should (equal (rg-panel--paths) (list outside))))
    (should (eq (rg-panel--scope 'root) 'root))
    (should (equal (rg-panel--paths) '(".")))))

(ert-deftest rg-panel-test-scope-here-run ()
  (skip-unless (executable-find "rg"))
  (rg-panel-tests--with-project
    (rg-panel--scope 'here)
    (should (equal (rg-panel-tests--run "deploy") '("src/app.py" "src/web.js")))))

(ert-deftest rg-panel-test-uncommitted-files ()
  "Modified, new and staged files; not deleted or ignored ones."
  (rg-panel-tests--with-git
    (should (equal (rg-panel--git-uncommitted dir)
                   '("docs/staged.md" "src/app.py" "src/new.py")))
    ;; From a subdirectory: relative to it, and only under it.
    (should (equal (rg-panel--git-uncommitted (expand-file-name "src" dir))
                   '("app.py" "new.py")))
    (should (eq (rg-panel--scope 'uncommitted) 'uncommitted))
    (should (equal (rg-panel--paths) '("docs/staged.md" "src/app.py" "src/new.py")))))

(ert-deftest rg-panel-test-uncommitted-run-and-narrowed ()
  "rg searches a file named on its command line whatever the globs say:
the uncommitted files are narrowed by them first."
  (skip-unless (executable-find "rg"))
  (rg-panel-tests--with-git
    (rg-panel--scope 'uncommitted)
    (should (equal (rg-panel-tests--run "deploy") '("docs/staged.md" "src/app.py" "src/new.py")))
    (rg-panel--set :include '("*.py"))
    (should (equal (rg-panel--paths) '("src/app.py" "src/new.py")))
    (should (equal (rg-panel-tests--run "deploy") '("src/app.py" "src/new.py")))
    (rg-panel--set :exclude '("src/new.py"))
    (should (equal (rg-panel-tests--run "deploy") '("src/app.py")))
    (rg-panel--put :filters (rg-panel--empty))
    (rg-panel--set :types '("markdown"))
    (should (equal (rg-panel-tests--run "deploy") '("docs/staged.md")))
    ;; Nothing left: rg is given the null device, not the whole project.
    (rg-panel--set :types '("js"))
    (should (equal (rg-panel--paths) (list null-device)))
    (should (equal (rg-panel-tests--run "deploy") nil))))

(ert-deftest rg-panel-test-uncommitted-none ()
  "Nothing uncommitted: the scope stays as it was."
  (rg-panel-tests--with-project
    (skip-unless (executable-find "git"))
    (rg-panel-tests--git "init" "-q")
    (rg-panel-tests--git "add" "-A")
    (rg-panel-tests--git "commit" "-qm" "init")
    (should-not (rg-panel--scope 'uncommitted))
    (should (eq (rg-panel--get :scope) 'root))
    (should (equal (rg-panel--paths) '(".")))))

(ert-deftest rg-panel-test-uncommitted-not-git ()
  (rg-panel-tests--with-project
    (should-not (rg-panel--git-uncommitted dir))
    (should-not (rg-panel--scope 'uncommitted))))


;;; The prompt

(ert-deftest rg-panel-test-header ()
  (rg-panel-tests--with-project
    (let ((name (file-name-nondirectory (directory-file-name dir))))
      (should (equal (rg-panel--header) (format "rg %s/: " name)))
      (rg-panel--put :filters (rg-panel-tests--filters
                               :types '("py") :exclude '("tests/**") :word t))
      (should (equal (rg-panel--header) (format "rg %s/ [py · !tests/** · word]: " name)))
      (rg-panel--set :include '("my dir/**"))
      (should (equal (rg-panel--header)
                     (format "rg %s/ [py · \"my dir/**\" · !tests/** · word]: " name)))
      (rg-panel--put :filters (rg-panel-tests--filters :engine 'rga))
      (should (equal (rg-panel--header) (format "rga %s/: " name)))
      (rg-panel--put :busy t)
      (should (string-match-p "讀取文件中" (rg-panel--header)))
      (rg-panel--put :busy nil)
      (rg-panel--put :filters (rg-panel--empty))
      (rg-panel--scope 'here)
      (should (equal (rg-panel--header) (format "rg %s/src/: " name)))
      (rg-panel--put :scope 'uncommitted)
      (rg-panel--put :uncommitted '("a" "b" "c"))
      (should (equal (rg-panel--header) (format "rg %s/ ±3: " name))))))

(ert-deftest rg-panel-test-header-redrawn ()
  "The prompt is drawn over, its last two characters -- consult's running
mark -- left alone; the input untouched."
  (rg-panel-tests--with-project
    (with-temp-buffer
      (let ((prompt (rg-panel--header)))
        (insert prompt "#deploy")
        (cl-letf (((symbol-function 'minibuffer-prompt-end) (lambda () (1+ (length prompt)))))
          (rg-panel--put :minibuffer (current-buffer))
          (rg-panel--set :word t)
          (let ((ov (rg-panel--get :header-ov)))
            (should (equal (overlay-get ov 'display)
                           (string-remove-suffix ": " (rg-panel--header))))
            (should (string-suffix-p "[word]" (overlay-get ov 'display)))
            (should (= (overlay-start ov) 1))
            (should (= (overlay-end ov) (1- (length prompt)))))
          (should (equal (buffer-substring (- (point-max) 7) (point-max)) "#deploy")))))))

(ert-deftest rg-panel-test-hint ()
  (rg-panel-tests--with-project
    (with-temp-buffer
      (insert "#deploy")
      (rg-panel--setup)
      (should (eq (rg-panel--get :minibuffer) (current-buffer)))
      (let ((ov (rg-panel--get :hint-ov)))
        (should (= (overlay-start ov) (point-max)))
        (should (string-match-p "M-o 篩選" (overlay-get ov 'before-string)))
        (should (string-match-p "C-c C-e" (overlay-get ov 'before-string)))
        ;; Below vertico's candidates, a before-string at the same place.
        (should (> (overlay-get ov 'priority) 0)))
      (let ((rg-panel-hint nil) (rg-panel--session (list :dir dir)))
        (rg-panel--setup)
        (should-not (rg-panel--get :hint-ov))))))


;;; The search follows the panel, the input kept

(defun rg-panel-tests--pipeline ()
  "The search's pipeline from the input on, down to a sink keeping what
it gets: (ASYNC . STATE), STATE a plist of :lines and :done."
  (let* ((state (list :lines nil :done nil))
         (sink (lambda (action)
                 (pcase action
                   ('flush (plist-put state :lines nil))
                   ((pred consp) (plist-put state :lines (append (plist-get state :lines) action)))
                   (`[indicator ,s] (unless (eq s 'running) (plist-put state :done s))))))
         (async (funcall (consult--async-pipeline
                          (rg-panel--async-rerun)
                          (consult--async-process #'rg-panel--builder)
                          (rg-panel--async-watch))
                         sink)))
    (cons async state)))

(defun rg-panel-tests--wait (state)
  (with-timeout (10 (error "rg did not finish"))
    (while (not (plist-get state :done))
      (accept-process-output nil 0.02)))
  (sort (delete-dups
         (mapcar (lambda (l) (string-remove-prefix "./" (car (split-string l "\0"))))
                 (plist-get state :lines)))
        #'string<))

(ert-deftest rg-panel-test-toggle-reruns-keeps-input ()
  "A toggle in the panel runs the search again, with the input as it was,
and leaves the input alone."
  (skip-unless (executable-find "rg"))
  (rg-panel-tests--with-project
    (with-temp-buffer
      (insert "#deplo")
      (pcase-let ((`(,async . ,state) (rg-panel-tests--pipeline)))
        (funcall async 'setup)
        (rg-panel--put :minibuffer (current-buffer))
        (funcall async "deplo")
        (should (= (length (rg-panel-tests--wait state)) 5))
        (plist-put state :done nil)
        (call-interactively #'rg-panel-toggle-word)
        (should (plist-get (rg-panel--get :filters) :word))
        ;; deplo is no word: searched again, nothing.
        (should (equal (rg-panel-tests--wait state) nil))
        (should (equal (buffer-string) "#deplo"))
        ;; The input goes on; a later toggle uses the new one.
        (plist-put state :done nil)
        (funcall async "deploy")
        (should (equal (rg-panel-tests--wait state)
                       '("docs/readme.md" "my dir/note.txt" "src/app.py" "src/web.js" "tests/test_app.py")))
        (plist-put state :done nil)
        (cl-letf (((symbol-function 'completing-read-multiple) (lambda (&rest _) '("js"))))
          (call-interactively #'rg-panel-type))
        (should (equal (rg-panel-tests--wait state) '("src/web.js")))
        (plist-put state :done nil)
        (call-interactively #'rg-panel-clear)
        (should (equal (rg-panel-tests--wait state)
                       '("docs/readme.md" "my dir/note.txt" "src/app.py" "src/web.js" "tests/test_app.py")))
        (plist-put state :done nil)
        (call-interactively #'rg-panel-scope-here)
        (should (equal (rg-panel-tests--wait state) '("src/app.py" "src/web.js")))
        (should (equal (buffer-string) "#deplo"))
        (funcall async 'destroy)))))

(ert-deftest rg-panel-test-rerun-forgets-short-input ()
  "An input too short to search is not searched again by a toggle."
  (rg-panel-tests--with-project
    (let* ((got nil)
           (async (funcall (rg-panel--async-rerun) (lambda (a) (push a got)))))
      (with-temp-buffer
        (funcall async 'setup)
        (funcall async "deploy")
        (funcall async 'cancel)
        (setq got nil)
        (rg-panel--refresh)
        (should-not (cl-some #'stringp got))
        (funcall async "dep")
        (setq got nil)
        (rg-panel--refresh)
        (should (equal got '("dep")))))))

(ert-deftest rg-panel-test-rga-notice ()
  "rga running long with nothing yet says so in the prompt; the first
results take it away."
  (rg-panel-tests--with-project
    (with-temp-buffer
      (let ((rg-panel-rga-notice-after 0.05)
            (got nil))
        (insert (rg-panel--header) "#x")
        (rg-panel--put :minibuffer (current-buffer))
        (rg-panel--put :filters (rg-panel-tests--filters :engine 'rga))
        (let ((async (funcall (rg-panel--async-watch) (lambda (a) (push a got)))))
          (funcall async [indicator running])
          (accept-process-output nil 0.2)
          (should (rg-panel--get :busy))
          (should (string-match-p "讀取文件中" (overlay-get (rg-panel--get :header-ov) 'display)))
          (funcall async '("a.pdf\0001:x"))
          (should-not (rg-panel--get :busy))
          (should-not (string-match-p "讀取文件中" (overlay-get (rg-panel--get :header-ov) 'display)))
          ;; Everything is passed on.
          (should (equal (reverse got) '([indicator running] ("a.pdf\0001:x"))))
          ;; rg, never.
          (rg-panel--put :filters (rg-panel--empty))
          (funcall async [indicator running])
          (accept-process-output nil 0.2)
          (should-not (rg-panel--get :busy))
          (funcall async 'destroy))))))


;;; Presets

(ert-deftest rg-panel-test-presets-save-load ()
  (rg-panel-tests--with-project
    (let ((py (rg-panel-tests--filters :types '("py") :word t))
          (other "/elsewhere/"))
      (should-not (rg-panel-presets dir))
      (rg-panel-preset-save dir "py words" py)
      (rg-panel-preset-save other "web" (rg-panel-tests--filters :types '("js")))
      (should (equal (rg-panel-presets dir) `(("py words" . ,py))))
      ;; Read back from the file, not the cache.
      (setq rg-panel--store nil rg-panel--store-stamp nil)
      (should (equal (cdr (assoc "py words" (rg-panel-presets dir))) py))
      (should (equal (mapcar #'car (rg-panel-presets other)) '("web")))
      ;; Saved again under the same name: replaced.
      (rg-panel-preset-save dir "py words" (rg-panel-tests--filters :types '("py")))
      (should (= (length (rg-panel-presets dir)) 1))
      (should-not (plist-get (cdr (assoc "py words" (rg-panel-presets dir))) :word)))))

(ert-deftest rg-panel-test-presets-file-changed-outside ()
  "Another Emacs writing the file is seen."
  (rg-panel-tests--with-project
    (rg-panel-preset-save dir "a" (rg-panel-tests--filters :word t))
    (should (rg-panel-presets dir))
    (sleep-for 0.01)
    (with-temp-file rg-panel-save-file
      (prin1 `((,dir ("b" . ,(rg-panel-tests--filters :fixed t)))) (current-buffer)))
    (set-file-times rg-panel-save-file (time-add nil 5))
    (should (equal (mapcar #'car (rg-panel-presets dir)) '("b")))))

(ert-deftest rg-panel-test-last-used ()
  "The last set is kept as 上次 when a search ends, unless it had none."
  (rg-panel-tests--with-project
    (rg-panel--remember-last)
    (should-not (rg-panel-presets dir))
    (rg-panel--set :include '("*.py"))
    (rg-panel--remember-last)
    (should (equal (plist-get (cdr (assoc rg-panel-last-name (rg-panel-presets dir))) :include)
                   '("*.py")))
    (rg-panel--put :filters (rg-panel-tests--filters :engine 'rga))
    (rg-panel--remember-last)
    (should (eq (plist-get (cdr (assoc rg-panel-last-name (rg-panel-presets dir))) :engine) 'rga))))

(ert-deftest rg-panel-test-load-offers-last ()
  "l always offers 上次, first; applying a preset searches again."
  (rg-panel-tests--with-project
    (rg-panel-preset-save dir "py" (rg-panel-tests--filters :types '("py")))
    (let (offered reruns)
      (rg-panel--put :rerun (lambda () (push (rg-panel--get :filters) reruns)))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_p coll &rest _) (setq offered coll) "py")))
        (call-interactively #'rg-panel-load))
      (should (equal offered (list rg-panel-last-name "py")))
      (should (equal (plist-get (rg-panel--get :filters) :types) '("py")))
      (should (equal (plist-get (car reruns) :types) '("py")))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) rg-panel-last-name)))
        (should-error (call-interactively #'rg-panel-load) :type 'user-error))
      (rg-panel-preset-save dir rg-panel-last-name (rg-panel-tests--filters :word t))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) rg-panel-last-name)))
        (call-interactively #'rg-panel-load))
      (should (plist-get (rg-panel--get :filters) :word)))))

(ert-deftest rg-panel-test-save-command ()
  (rg-panel-tests--with-project
    (rg-panel--set :exclude '("tests/**"))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) " no tests ")))
      (call-interactively #'rg-panel-save))
    (should (equal (plist-get (cdr (assoc "no tests" (rg-panel-presets dir))) :exclude)
                   '("tests/**")))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "  ")))
      (should-error (call-interactively #'rg-panel-save) :type 'user-error))))


;;; Export into wgrep

(defun rg-panel-tests--export (cands)
  "Do as `embark-export' does with CANDS: the grep buffer embark-consult
makes, then the hook as it was when the export began, in that buffer."
  (let ((after embark-after-export-hook))
    (with-current-buffer (save-excursion (embark-consult-export-grep cands) (current-buffer))
      (let ((embark-after-export-hook after))
        (run-hooks 'embark-after-export-hook))
      (current-buffer))))

(ert-deftest rg-panel-test-export-wgrep ()
  (rg-panel-tests--with-project
    (let* ((cand (concat (propertize "src/app.py" 'face 'consult-file) ":"
                         (propertize "1" 'face 'consult-line-number) ":def deploy():"))
           buf)
      (cl-letf (((symbol-function 'embark-export)
                 (lambda () (setq buf (rg-panel-tests--export (list cand))))))
        (call-interactively #'rg-panel-export-edit))
      (unwind-protect
          (with-current-buffer buf
            (should (derived-mode-p 'grep-mode))
            (should (eq (current-local-map) wgrep-mode-map))
            (should-not buffer-read-only))
        (when (buffer-live-p buf)
          (with-current-buffer buf (ignore-errors (wgrep-abort-changes)))
          (kill-buffer buf))))))

(ert-deftest rg-panel-test-export-only-this-one ()
  "Only this export turns wgrep on: not one that follows, not one in rga."
  (rg-panel-tests--with-project
    (let* ((cand (concat (propertize "src/app.py" 'face 'consult-file) ":"
                         (propertize "1" 'face 'consult-line-number) ":def deploy():"))
           (bufs nil))
      (cl-letf (((symbol-function 'embark-export)
                 (lambda () (push (rg-panel-tests--export (list cand)) bufs))))
        (should-not (memq #'rg-panel--after-export embark-after-export-hook))
        ;; Another export, of anything: untouched.
        (embark-export)
        (rg-panel--put :filters (rg-panel-tests--filters :engine 'rga))
        (call-interactively #'rg-panel-export-edit))
      (unwind-protect
          (dolist (b bufs)
            (with-current-buffer b
              (should-not (eq (current-local-map) wgrep-mode-map))
              (should buffer-read-only)))
        (mapc #'kill-buffer bufs)))))


;;; Results from documents

(ert-deftest rg-panel-test-extracted ()
  (rg-panel-tests--with-project
    (let ((pdf (concat (propertize "docs/guide.pdf" 'face 'consult-file) ":"
                       (propertize "3" 'face 'consult-line-number) ":Page 7: deploy it"))
          (py (concat (propertize "src/app.py" 'face 'consult-file) ":"
                      (propertize "1" 'face 'consult-line-number) ":def deploy():")))
      (should-not (rg-panel--extracted-p pdf))
      (rg-panel--put :filters (rg-panel-tests--filters :engine 'rga))
      (should (rg-panel--extracted-p pdf))
      (should-not (rg-panel--extracted-p py))
      (should (equal (rg-panel--cand-file pdf) "docs/guide.pdf"))
      (should (equal (rg-panel--cand-text pdf) "Page 7: deploy it"))
      (should (= (rg-panel--page (rg-panel--cand-text pdf)) 7))
      (should-not (rg-panel--page "deploy it")))))

(ert-deftest rg-panel-test-open-extracted ()
  "A PDF opens at its page with pdf-tools, in its own application without;
an HTML file opens at the text."
  (rg-panel-tests--with-project
    (rg-panel--put :filters (rg-panel-tests--filters :engine 'rga))
    (rg-panel-tests--write dir '(("docs/guide.html" . "<html><body><p>Intro</p>\n<p>The deploy guide</p></body></html>\n")))
    (let ((pdf (concat (propertize "./docs/guide.pdf" 'face 'consult-file) ":"
                       (propertize "3" 'face 'consult-line-number) ":Page 7: deploy it"))
          (html (concat (propertize "./docs/guide.html" 'face 'consult-file) ":"
                        (propertize "2" 'face 'consult-line-number) ":The deploy guide"))
          opened found page)
      (cl-letf (((symbol-function 'rg-panel--open-externally) (lambda (f) (setq opened f)))
                ((symbol-function 'require)
                 (lambda (feature &rest _) (unless (eq feature 'pdf-tools) t))))
        (rg-panel--open-extracted pdf))
      (should (equal opened (expand-file-name "docs/guide.pdf" dir)))
      (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
                ((symbol-function 'find-file) (lambda (f) (setq found f) (set-buffer (get-buffer-create " *pdf*"))
                                                (setq major-mode 'pdf-view-mode)))
                ((symbol-function 'pdf-view-goto-page) (lambda (n) (setq page n))))
        (rg-panel--open-extracted pdf))
      (kill-buffer " *pdf*")
      (should (equal found (expand-file-name "docs/guide.pdf" dir)))
      (should (= page 7))
      (save-window-excursion
        (rg-panel--open-extracted html)
        (should (equal buffer-file-name (expand-file-name "docs/guide.html" dir)))
        (should (looking-at "The deploy guide"))
        (kill-buffer)))))

(ert-deftest rg-panel-test-state-no-preview-extracted ()
  "consult's preview is not asked to show a line of an extracted file."
  (rg-panel-tests--with-project
    (rg-panel--put :filters (rg-panel-tests--filters :engine 'rga))
    (let ((seen :none))
      (cl-letf (((symbol-function 'consult--grep-state)
                 (lambda () (lambda (_action cand) (setq seen cand)))))
        (let ((state (rg-panel--state))
              (pdf (concat (propertize "a.pdf" 'face 'consult-file) ":"
                           (propertize "1" 'face 'consult-line-number) ":x"))
              (txt (concat (propertize "a.txt" 'face 'consult-file) ":"
                           (propertize "1" 'face 'consult-line-number) ":x")))
          (funcall state 'preview pdf)
          (should (eq seen nil))
          (funcall state 'preview txt)
          (should (eq seen txt)))))))


;;; The command and its keys

(ert-deftest rg-panel-test-command ()
  (rg-panel-tests--with-project
    (let (read session)
      (cl-letf (((symbol-function 'consult--read)
                 (lambda (_coll &rest opts) (setq read opts session rg-panel--session) nil)))
        (rg-panel-search "deploy" nil dir))
      (should (equal (plist-get read :prompt)
                     (format "rg %s/: " (file-name-nondirectory (directory-file-name dir)))))
      (should (equal (plist-get read :initial) "deploy"))
      ;; consult-ripgrep's customizations (the user's preview key) apply.
      (should (eq (plist-get read :command) #'consult-ripgrep))
      (should (eq (plist-get read :keymap) rg-panel-minibuffer-map))
      (should (eq (plist-get read :category) 'consult-grep))
      (should (equal (plist-get session :dir) dir))
      (should (eq (plist-get session :scope) 'root)))))

(ert-deftest rg-panel-test-command-pick-scope ()
  "With a prefix argument the scope is picked first."
  (rg-panel-tests--with-project
    (let (session asked)
      (cl-letf (((symbol-function 'consult--read)
                 (lambda (&rest _) (setq session rg-panel--session) nil))
                ((symbol-function 'completing-read)
                 (lambda (_p coll &rest _) (setq asked (mapcar #'car coll)) "this file's directory")))
        (let ((default-directory (expand-file-name "src/" dir)))
          (rg-panel-search nil '(4) dir)))
      (should (member "uncommitted files" asked))
      (should (eq (plist-get session :scope) 'here))
      (should (equal (plist-get session :paths) '("src/"))))))

(ert-deftest rg-panel-test-command-remembers-on-quit ()
  "C-g after a look keeps the set as 上次 too."
  (rg-panel-tests--with-project
    (cl-letf (((symbol-function 'consult--read)
               (lambda (&rest _)
                 (rg-panel--put :filters (rg-panel-tests--filters :word t))
                 (signal 'quit nil))))
      (should (eq (condition-case nil (rg-panel-search nil nil dir) (quit 'quit)) 'quit)))
    (should (plist-get (cdr (assoc rg-panel-last-name (rg-panel-presets dir))) :word))))

(ert-deftest rg-panel-test-keys ()
  "M-o and C-c C-e in the search, and nowhere else in its minibuffer."
  (should (eq (keymap-lookup rg-panel-minibuffer-map "M-o") #'rg-panel-filters))
  (should (eq (keymap-lookup rg-panel-minibuffer-map "C-c C-e") #'rg-panel-export-edit))
  (dolist (map (list vertico-map minibuffer-local-map minibuffer-local-must-match-map
                     consult-async-map embark-general-map))
    (should-not (keymap-lookup map "M-o"))
    ;; Neither C-c nor C-c C-e: a number means C-c is no prefix there.
    (should-not (commandp (keymap-lookup map "C-c")))
    (should-not (commandp (keymap-lookup map "C-c C-e")))))

(ert-deftest rg-panel-test-panel-keys ()
  "The panel's keys, as the design has them."
  (dolist (k '(("i" . rg-panel-include) ("e" . rg-panel-exclude) ("t" . rg-panel-type)
               ("w" . rg-panel-toggle-word) ("c" . rg-panel-toggle-case)
               ("f" . rg-panel-toggle-fixed) ("h" . rg-panel-toggle-hidden)
               ("m" . rg-panel-toggle-multiline) ("p" . rg-panel-toggle-pcre)
               ("r" . rg-panel-scope-root) ("." . rg-panel-scope-here)
               ("g" . rg-panel-scope-uncommitted) ("D" . rg-panel-scope-pick)
               ("a" . rg-panel-toggle-engine)
               ("x" . rg-panel-clear) ("s" . rg-panel-save) ("l" . rg-panel-load)
               ("q" . transient-quit-one) ("M-o" . transient-quit-one)))
    (let ((suffix (transient-get-suffix 'rg-panel-menu (car k))))
      (should suffix)
      (should (eq (plist-get (cdr suffix) :command) (cdr k))))))

(ert-deftest rg-panel-test-toggles-stay-open ()
  "Each panel command but close keeps the panel open."
  (dolist (cmd '(rg-panel-include rg-panel-exclude rg-panel-type rg-panel-toggle-word
                 rg-panel-toggle-case rg-panel-toggle-fixed rg-panel-toggle-hidden
                 rg-panel-toggle-multiline rg-panel-toggle-pcre rg-panel-scope-root
                 rg-panel-scope-here rg-panel-scope-uncommitted rg-panel-scope-pick
                 rg-panel-toggle-engine rg-panel-clear rg-panel-save rg-panel-load))
    (should (eq (oref (get cmd 'transient--suffix) transient) t))))

(ert-deftest rg-panel-test-toggle-labels ()
  (rg-panel-tests--with-project
    (should (string-match-p "\\[關\\]" (rg-panel--on :word)))
    (rg-panel--set :word t)
    (should (string-match-p "\\[開\\]" (rg-panel--on :word)))
    (call-interactively #'rg-panel-toggle-word)
    (should-not (plist-get (rg-panel--get :filters) :word))))

(ert-deftest rg-panel-test-type-list ()
  (skip-unless (executable-find "rg"))
  (let ((types (rg-panel--type-list)))
    (should (assoc "py" types))
    (should (string-match-p "\\*\\.py" (cdr (assoc "py" types))))))

(ert-deftest rg-panel-test-project-globs ()
  "Include and exclude complete from what the project has."
  (skip-unless (executable-find "rg"))
  (rg-panel-tests--with-project
    (let ((globs (rg-panel--project-extensions)))
      (should (member "*.py" globs))
      (should (member "*.md" globs))
      (should (member "src/**" globs))
      (should (member "my dir/**" globs))
      (should-not (member ".hidden/**" globs)))))

(provide 'rg-panel-tests)
;;; rg-panel-tests.el ends here

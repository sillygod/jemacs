;;; ghostel-cmd-tests.el --- Tests for ghostel-cmd  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;   emacs --batch --init-directory=~/.emacs.d/emacs-home/ \
;;         -L ~/.emacs.d/local/ghostel-cmd \
;;         -l ghostel-cmd.el -l ghostel-cmd-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'project)
(require 'ghostel-cmd)

(defmacro ghostel-cmd-tests--with-store (&rest body)
  (declare (indent 0))
  `(let* ((dir (make-temp-file "ghostel-cmd-" t))
          (ghostel-cmd-save-file (expand-file-name "cmds.eld" dir))
          (ghostel-cmd--cache nil)
          (ghostel-cmd--cache-stamp nil)
          (ghostel-cmd--tasks-cache nil)
          (ghostel-cmd-discover t)
          (ghostel-cmd--root (file-name-as-directory dir))
          (ghostel-cmd--caller-file (expand-file-name "foo.py" dir))
          (ghostel-cmd--caller-line 12)
          (default-directory dir))
     (unwind-protect
         (progn ,@body)
       (ignore-errors (delete-directory dir t)))))

(defmacro ghostel-cmd-tests--with-overlay (cmds &rest body)
  "Run BODY in the overlay buffer listing CMDS, shown in the selected window.
Keys go through the real keymaps with `execute-kbd-macro'."
  (declare (indent 1))
  `(ghostel-cmd-tests--with-store
     (let ((ghostel-cmd--query "")
           (ghostel-cmd--filtering nil)
           (ghostel-cmd--fill nil)
           (ghostel-cmd--help-visible nil))
       (ghostel-cmd--set-commands ,cmds ghostel-cmd--root)
       (let ((buf (ghostel-cmd--prepare-buffer)))
         (unwind-protect
             (progn
               (set-window-buffer (selected-window) buf)
               (with-current-buffer buf ,@body))
           (kill-buffer buf))))))

(ert-deftest ghostel-cmd-test-extract-vars-order-and-defaults ()
  (let ((ghostel-cmd--caller-file nil)
        (ghostel-cmd--caller-line nil))
    (should (equal (ghostel-cmd--extract-variables
                    "pytest {{file}} -k {{expr:all}}" "/tmp/proj/")
                   '(("file" . nil)
                     ("expr" . "all"))))))

(ert-deftest ghostel-cmd-test-go-templates-stay-literal ()
  (dolist (tmpl '("docker inspect --format '{{.State.Status}}' web"
                  "docker inspect --format '{{json .}}' web"
                  "helm template . --set x={{ .Values.x }}"))
    (should-not (ghostel-cmd--extract-variables tmpl "/tmp/"))
    (should (equal (ghostel-cmd--apply tmpl '(("json" . "1"))) tmpl)))
  ;; The rule has no escape: a Go action that starts with a letter,
  ;; like {{end}}, reads as a variable.
  (should (equal (ghostel-cmd--extract-variables
                  "-o go-template='{{range .items}}{{.name}}{{end}}'" "/tmp/")
                 '(("end" . nil))))
  (let ((tmpl "docker inspect -f '{{.State.Status}}' {{ctr:web}} {{_id}}"))
    (should (equal (ghostel-cmd--extract-variables tmpl "/tmp/")
                   '(("ctr" . "web") ("_id" . nil))))
    (should (equal (ghostel-cmd--apply tmpl '(("ctr" . "db") ("_id" . "7")))
                   "docker inspect -f '{{.State.Status}}' db 7")))
  (should (equal (ghostel-cmd--extract-variables "echo {{ name }} {{n2:x y}}" "/tmp/")
                 '(("name" . nil) ("n2" . "x y")))))

(ert-deftest ghostel-cmd-test-builtin-file-and-root ()
  (ghostel-cmd-tests--with-store
    (should (equal (ghostel-cmd--builtin-var "root" ghostel-cmd--root)
                   (directory-file-name ghostel-cmd--root)))
    (should (equal (ghostel-cmd--builtin-var "file" ghostel-cmd--root)
                   "foo.py"))
    (should (equal (ghostel-cmd--builtin-var "line" ghostel-cmd--root)
                   "12"))
    (should (equal (ghostel-cmd--builtin-var "project-name" ghostel-cmd--root)
                   (file-name-nondirectory
                    (directory-file-name ghostel-cmd--root))))))

(ert-deftest ghostel-cmd-test-builtin-symbol-file-abs-dir ()
  (ghostel-cmd-tests--with-store
    (let* ((root ghostel-cmd--root)
           (ghostel-cmd--caller-file (expand-file-name "src/app/foo.py" root))
           (ghostel-cmd--caller-dir (expand-file-name "src/app/" root))
           (ghostel-cmd--caller-symbol "test_login"))
      (should (equal (ghostel-cmd--builtin-var "file-abs" root)
                     (expand-file-name "src/app/foo.py" root)))
      (should (equal (ghostel-cmd--builtin-var "file" root) "src/app/foo.py"))
      (should (equal (ghostel-cmd--builtin-var "dir" root) "src/app"))
      (should (equal (ghostel-cmd--builtin-var "symbol" root) "test_login"))
      (let ((ghostel-cmd--caller-dir root))
        (should (equal (ghostel-cmd--builtin-var "dir" root) "."))))))

(ert-deftest ghostel-cmd-test-open-captures-caller-symbol-and-dir ()
  "Opening the overlay takes {{symbol}} and {{dir}} from the caller."
  (ghostel-cmd-tests--with-store
    (let* ((root ghostel-cmd--root)
           (project-find-functions (list (lambda (_) (cons 'transient root))))
           (ghostel-cmd-use-posframe nil)
           (caller (get-buffer-create " *ghostel-cmd-caller*")))
      (unwind-protect
          (progn
            (delete-other-windows)
            (set-window-buffer (selected-window) caller)
            (with-current-buffer caller
              (insert "def test_login():\n    pass\n")
              (goto-char 7)
              (setq default-directory (expand-file-name "src/" root))
              (ghostel-cmd-sidebar))
            (should (equal ghostel-cmd--caller-symbol "test_login"))
            (should (equal (ghostel-cmd--extract-variables
                            "pytest {{dir}} -k {{symbol}}" root)
                           '(("dir" . "src") ("symbol" . "test_login")))))
        (kill-buffer caller)
        (when (get-buffer ghostel-cmd--buffer-name)
          (kill-buffer ghostel-cmd--buffer-name))
        (delete-other-windows)))))

(ert-deftest ghostel-cmd-test-apply-replaces-all-occurrences ()
  (should (equal (ghostel-cmd--apply
                  "echo {{name:x}} && {{name}}"
                  '(("name" . "hi")))
                 "echo hi && hi")))

(ert-deftest ghostel-cmd-test-preview-uses-current-input ()
  (should (equal (ghostel-cmd--preview
                  "pytest {{file}} -k {{expr}}"
                  "/tmp/"
                  '(("file" . "a.py"))
                  "expr" "foo")
                 "pytest a.py -k foo")))

(ert-deftest ghostel-cmd-test-wrap-within-cap ()
  (should (equal (ghostel-cmd--wrap "short" 20 3) '("short")))
  (should (equal (ghostel-cmd--wrap "aaaa bbbb cccc dddd eeee" 10 3)
                 '("aaaa bbbb" "cccc dddd" "eeee")))
  (should (equal (ghostel-cmd--wrap (make-string 25 ?x) 10 3)
                 (list (make-string 10 ?x) (make-string 10 ?x) (make-string 5 ?x))))
  (should (equal (ghostel-cmd--wrap "中文中文中文" 5 3) '("中文" "中文" "中文")))
  (should (equal (ghostel-cmd--wrap "a\nb\n" 10 3) '("a" "b")))
  (let ((lines (ghostel-cmd--wrap (string-join (make-list 40 "word") " ") 20 3)))
    (should (= (length lines) 3))
    (should (string-suffix-p "…" (car (last lines))))
    (should (cl-every (lambda (l) (<= (string-width l) 20)) lines))))

(ert-deftest ghostel-cmd-test-preview-wraps-long-command ()
  "The preview shows a long command over several lines, never more
than the cap, and a long fill value by its end."
  (let* ((words (cl-loop for i below 60 collect (format "arg%d" i)))
         (cmd (concat "run " (string-join words " ") " {{v}}")))
    (ghostel-cmd-tests--with-overlay (list (list :name "long" :cmd cmd))
      (let ((ghostel-cmd--target-width 41)
            (ghostel-cmd-preview-max-lines 3))
        (ghostel-cmd--refresh-overlay)
        (let ((lines (cdr (split-string (buffer-substring-no-properties
                                         ghostel-cmd--preview-start (point-max))
                                        "\n" t))))
          (should (= (length lines) 3))
          (should (string-prefix-p "run arg0 arg1" (car lines)))
          (should (string-prefix-p "arg" (nth 1 lines)))
          (should (string-suffix-p "…" (nth 2 lines))))
        (ghostel-cmd-paste)
        (execute-kbd-macro (make-string 60 ?z))
        (let ((lines (cdr (split-string (buffer-substring-no-properties
                                         ghostel-cmd--preview-start (point-max))
                                        "\n" t))))
          (should (= (length lines) 4))
          (should (string-match-p "\\`{{v}}  …z+\\'" (nth 3 lines)))
          (should (<= (string-width (nth 3 lines)) 40)))))))

(ert-deftest ghostel-cmd-test-persist-roundtrip ()
  (ghostel-cmd-tests--with-store
    (ghostel-cmd--set-commands
     (list (list :name "test" :cmd "pytest {{file}}" :desc "run"))
     ghostel-cmd--root)
    (setq ghostel-cmd--cache nil
          ghostel-cmd--cache-stamp nil)
    (let ((got (car (ghostel-cmd--get-commands ghostel-cmd--root))))
      (should (equal (plist-get got :name) "test"))
      (should (equal (plist-get got :cmd) "pytest {{file}}")))))

;; An outside edit (`o', another Emacs) stamps the file 10s and 20s on,
;; so its mtime differs from ours even on a coarse clock.
(defun ghostel-cmd-tests--write-outside (form seconds)
  (with-temp-file ghostel-cmd-save-file
    (prin1 form (current-buffer)))
  (set-file-times ghostel-cmd-save-file (time-add (current-time) seconds)))

(defun ghostel-cmd-tests--names (root)
  (mapcar (lambda (c) (plist-get c :name)) (ghostel-cmd--get-commands root)))

(ert-deftest ghostel-cmd-test-outside-edit-is-read-and-kept ()
  (ghostel-cmd-tests--with-store
    (let ((root ghostel-cmd--root))
      (ghostel-cmd--set-commands (list (list :name "a" :cmd "echo a")) root)
      (should (equal (ghostel-cmd-tests--names root) '("a")))
      (ghostel-cmd-tests--write-outside
       `((,root (:name "a" :cmd "echo a") (:name "b" :cmd "echo b"))
         ("/other/" (:name "o" :cmd "echo o")))
       10)
      (should (equal (ghostel-cmd-tests--names root) '("a" "b")))
      ;; Edited again after our last read: our next save builds on it.
      (ghostel-cmd-tests--write-outside
       `((,root (:name "a" :cmd "echo a") (:name "c" :cmd "echo c"))
         ("/other/" (:name "o" :cmd "echo o")))
       20)
      (ghostel-cmd--update-commands
       (lambda (cmds) (append cmds (list (list :name "d" :cmd "echo d"))))
       root)
      (setq ghostel-cmd--cache nil
            ghostel-cmd--cache-stamp nil)
      (should (equal (ghostel-cmd-tests--names root) '("a" "c" "d")))
      (should (equal (ghostel-cmd-tests--names "/other/") '("o"))))))

(ert-deftest ghostel-cmd-test-unreadable-file-is-not-written-over ()
  (ghostel-cmd-tests--with-store
    (let ((root ghostel-cmd--root)
          (broken "((\"/x/\" (:name")
          (inhibit-message t))
      (ghostel-cmd--set-commands (list (list :name "a" :cmd "echo a")) root)
      (with-temp-file ghostel-cmd-save-file
        (insert broken))
      (set-file-times ghostel-cmd-save-file (time-add (current-time) 10))
      (should (equal (ghostel-cmd-tests--names root) '("a")))
      (should-error (ghostel-cmd--set-commands nil root) :type 'user-error)
      (should (equal (with-temp-buffer
                       (insert-file-contents ghostel-cmd-save-file)
                       (buffer-string))
                     broken)))))

(defmacro ghostel-cmd-tests--answering (answers yes &rest body)
  "Run BODY with `read-string' returning ANSWERS in turn.
`yes-or-no-p' answers YES and counts its calls in `asked'."
  (declare (indent 2))
  `(let ((queue ,answers)
         (asked 0))
     (cl-letf (((symbol-function 'read-string) (lambda (&rest _) (pop queue)))
               ((symbol-function 'yes-or-no-p)
                (lambda (&rest _) (cl-incf asked) ,yes)))
       ,@body)))

(defun ghostel-cmd-tests--abc ()
  (list (list :name "a" :cmd "echo a")
        (list :name "b" :cmd "echo b")
        (list :name "c" :cmd "echo c")))

(ert-deftest ghostel-cmd-test-edit-keeps-place ()
  (ghostel-cmd-tests--with-overlay (ghostel-cmd-tests--abc)
    (forward-line 1)
    (ghostel-cmd-tests--answering '("b" "echo B" "") t
      (ghostel-cmd-edit)
      (should (= asked 0)))
    (should (equal (ghostel-cmd-tests--names ghostel-cmd--root) '("a" "b" "c")))
    (should (equal (plist-get (nth 1 (ghostel-cmd--get-commands ghostel-cmd--root))
                              :cmd)
                   "echo B"))
    (ghostel-cmd-tests--answering '("b2" "echo B" "") t
      (ghostel-cmd-edit))
    (should (equal (ghostel-cmd-tests--names ghostel-cmd--root) '("a" "b2" "c")))
    (should (equal (tabulated-list-get-id) '(project . "b2")))))

(ert-deftest ghostel-cmd-test-rename-onto-a-name-replaces-it ()
  (ghostel-cmd-tests--with-overlay (ghostel-cmd-tests--abc)
    (forward-line 1)
    (ghostel-cmd-tests--answering '("c" "echo B" "") nil
      (should-error (ghostel-cmd-edit) :type 'user-error)
      (should (= asked 1)))
    (should (equal (ghostel-cmd-tests--names ghostel-cmd--root) '("a" "b" "c")))
    (ghostel-cmd-tests--answering '("c" "echo B" "") t
      (ghostel-cmd-edit))
    (should (equal (ghostel-cmd--get-commands ghostel-cmd--root)
                   (list (list :name "a" :cmd "echo a")
                         (list :name "c" :cmd "echo B" :desc ""))))))

(ert-deftest ghostel-cmd-test-add-onto-a-name-replaces-it-in-place ()
  (ghostel-cmd-tests--with-overlay (ghostel-cmd-tests--abc)
    (ghostel-cmd-tests--answering '("echo A" "a" "") t
      (ghostel-cmd-add)
      (should (= asked 1)))
    (ghostel-cmd-tests--answering '("echo d" "d" "") t
      (ghostel-cmd-add)
      (should (= asked 0)))
    (should (equal (ghostel-cmd-tests--names ghostel-cmd--root) '("a" "b" "c" "d")))
    (should (equal (plist-get (car (ghostel-cmd--get-commands ghostel-cmd--root))
                              :cmd)
                   "echo A"))))

(ert-deftest ghostel-cmd-test-reorder-is-saved ()
  (ghostel-cmd-tests--with-overlay (ghostel-cmd-tests--abc)
    (execute-kbd-macro (kbd "M-j"))
    (should (equal (tabulated-list-get-id) '(project . "a")))
    (setq ghostel-cmd--cache nil
          ghostel-cmd--cache-stamp nil)
    (should (equal (ghostel-cmd-tests--names ghostel-cmd--root) '("b" "a" "c")))
    (execute-kbd-macro (kbd "M-<down>"))
    (execute-kbd-macro (kbd "M-j"))
    (should (equal (ghostel-cmd-tests--names ghostel-cmd--root) '("b" "c" "a")))
    (execute-kbd-macro (kbd "M-k"))
    (execute-kbd-macro (kbd "M-<up>"))
    (execute-kbd-macro (kbd "M-k"))
    (should (equal (ghostel-cmd-tests--names ghostel-cmd--root) '("a" "b" "c")))
    (should (equal (tabulated-list-get-id) '(project . "a")))))

(ert-deftest ghostel-cmd-test-reorder-under-filter-skips-hidden ()
  (ghostel-cmd-tests--with-overlay
      (list (list :name "x1" :cmd "echo 1")
            (list :name "y" :cmd "echo y")
            (list :name "x2" :cmd "echo 2"))
    (setq ghostel-cmd--query "x")
    (ghostel-cmd--refresh-overlay)
    (execute-kbd-macro (kbd "M-j"))
    (should (equal (ghostel-cmd-tests--names ghostel-cmd--root) '("y" "x2" "x1")))))

(ert-deftest ghostel-cmd-test-put-command-heals-duplicates ()
  (should (equal (ghostel-cmd--put-command
                  (list (list :name "x" :cmd "1")
                        (list :name "y" :cmd "2")
                        (list :name "x" :cmd "3"))
                  (list :name "x" :cmd "new"))
                 (list (list :name "x" :cmd "new")
                       (list :name "y" :cmd "2")))))

(ert-deftest ghostel-cmd-test-blank-or-comment-file-reads-empty ()
  (ghostel-cmd-tests--with-store
    (dolist (text '("" "\n  \n" ";; my commands\n"))
      (with-temp-file ghostel-cmd-save-file
        (insert text))
      (setq ghostel-cmd--cache-stamp nil)
      (should-not (ghostel-cmd--get-commands ghostel-cmd--root))
      (ghostel-cmd--set-commands (list (list :name "a" :cmd "echo a"))
                                 ghostel-cmd--root)
      (should (equal (ghostel-cmd-tests--names ghostel-cmd--root) '("a"))))))

(ert-deftest ghostel-cmd-test-query-flex-and ()
  (let ((rec (list :name "pytest" :cmd "pytest {{file}}" :desc "unit")))
    (should (ghostel-cmd--query-matches-p rec ""))
    (should (ghostel-cmd--query-matches-p rec "pyt"))
    (should (ghostel-cmd--query-matches-p rec "pt file"))
    (should-not (ghostel-cmd--query-matches-p rec "deploy"))))

(ert-deftest ghostel-cmd-test-filter-types-every-printable-char ()
  "n and p go into the query like any other char; C-n / C-p move."
  (ghostel-cmd-tests--with-overlay
      (list (list :name "npm" :cmd "npm test")
            (list :name "python" :cmd "python -m pytest"))
    (ghostel-cmd--set-filtering t)
    (execute-kbd-macro "npm")
    (should (equal ghostel-cmd--query "npm"))
    (ghostel-cmd-filter-clear)
    (execute-kbd-macro (kbd "C-n"))
    (should (equal (tabulated-list-get-id) '(project . "python")))
    (execute-kbd-macro (kbd "C-p"))
    (should (equal (tabulated-list-get-id) '(project . "npm")))
    (execute-kbd-macro "python")
    (should (equal ghostel-cmd--query "python"))))

(ert-deftest ghostel-cmd-test-non-ascii-types-into-filter-and-fill ()
  "Committed non-ASCII chars, as a system input method sends them,
reach the query and the fill input through the [t] binding."
  (ghostel-cmd-tests--with-overlay
      (list (list :name "say" :cmd "echo {{msg}}" :desc "說話"))
    (ghostel-cmd--set-filtering t)
    (execute-kbd-macro (vector ?說 ?話))
    (should (equal ghostel-cmd--query "說話"))
    (should (= ghostel-cmd--match-count 1))
    (ghostel-cmd-filter-confirm)
    (ghostel-cmd-paste)
    (execute-kbd-macro (vector ?你 ?好 ?é))
    (should (equal (plist-get ghostel-cmd--fill :input) "你好é"))))

(defun ghostel-cmd-tests--type (keys)
  "Type KEYS (`kbd' syntax), with the minibuffer reading them too.
Batch Emacs reads the minibuffer from stdin unless `noninteractive'
is nil; with it nil, the minibuffer reads the macro like a terminal."
  (let ((noninteractive nil))
    (execute-kbd-macro (kbd keys))))

(ert-deftest ghostel-cmd-test-input-method-via-minibuffer ()
  "An Emacs input method passes keys through raw in the read-only
overlay; C-\\ edits the input in the minibuffer with it on."
  (ghostel-cmd-tests--with-overlay
      (list (list :name "say" :cmd "echo {{msg}}" :desc "中文"))
    (let ((default-input-method "chinese-py"))
      (ghostel-cmd--set-filtering t)
      (activate-input-method default-input-method)
      (execute-kbd-macro "zhong")
      (should (equal ghostel-cmd--query "zhong"))
      (deactivate-input-method)
      (ghostel-cmd-filter-clear)
      (ghostel-cmd-tests--type "C-\\ zhong SPC RET")
      (should (string-prefix-p "中" ghostel-cmd--query))
      (should (= ghostel-cmd--match-count 1))
      (should ghostel-cmd--filtering)
      (let ((query ghostel-cmd--query))
        (ghostel-cmd-tests--type "C-\\ xyz C-g")
        (should (equal ghostel-cmd--query query)))
      (ghostel-cmd-filter-clear)
      (ghostel-cmd-filter-confirm)
      (ghostel-cmd-paste)
      (ghostel-cmd-tests--type "C-\\ zhong SPC RET")
      (should (string-prefix-p "中" (plist-get ghostel-cmd--fill :input)))
      (should ghostel-cmd--fill))))

(ert-deftest ghostel-cmd-test-minibuffer-edit-narrows-live ()
  (ghostel-cmd-tests--with-overlay
      (list (list :name "npm" :cmd "npm test")
            (list :name "python" :cmd "python x"))
    (let ((default-input-method nil)
          (minibuffer-local-map (copy-keymap minibuffer-local-map))
          (seen nil))
      (define-key minibuffer-local-map (kbd "<f12>")
                  (lambda () (interactive)
                    (push (cons ghostel-cmd--query ghostel-cmd--match-count) seen)))
      (ghostel-cmd--set-filtering t)
      (ghostel-cmd-tests--type "C-\\ py <f12> RET")
      (should (equal seen '(("py" . 1))))
      (should (equal ghostel-cmd--query "py")))))

(ert-deftest ghostel-cmd-test-yank-into-filter-and-fill ()
  "C-y appends the latest kill as one line: newlines become spaces."
  (ghostel-cmd-tests--with-overlay
      (list (list :name "rm" :cmd "rm {{files}}"))
    (let ((kill-ring nil)
          (interprogram-paste-function nil))
      (kill-new "a.py\nb.py\n")
      (ghostel-cmd--set-filtering t)
      (execute-kbd-macro "r")
      (execute-kbd-macro (kbd "C-y"))
      (should (equal ghostel-cmd--query "ra.py b.py"))
      (ghostel-cmd-filter-clear)
      (ghostel-cmd-filter-confirm)
      (ghostel-cmd-paste)
      (execute-kbd-macro (kbd "C-y"))
      (should (equal (plist-get ghostel-cmd--fill :input) "a.py b.py")))))

(ert-deftest ghostel-cmd-test-fill-commits-then-finishes ()
  (let ((ghostel-cmd--fill
         (list :record (list :name "t" :cmd "a {{x}} {{y}}")
               :vars '(("x" . "1") ("y" . "2"))
               :index 0
               :values nil
               :input "aa"
               :submit nil)))
    (should (ghostel-cmd--fill-commit-current))
    (should (equal (alist-get "x" (plist-get ghostel-cmd--fill :values)
                              nil nil #'equal)
                   "aa"))
    (should (= (plist-get ghostel-cmd--fill :index) 1))
    (plist-put ghostel-cmd--fill :input "")
    (should-not (ghostel-cmd--fill-commit-current))
    (should (equal (alist-get "y" (plist-get ghostel-cmd--fill :values)
                              nil nil #'equal)
                   "2"))))

(ert-deftest ghostel-cmd-test-last-value-beats-default-not-builtins ()
  (ghostel-cmd-tests--with-overlay
      (list (list :name "k" :cmd "pytest {{file}} -k {{expr:all}}"))
    (let ((ghostel-cmd--last-values (make-hash-table :test #'equal)))
      (ghostel-cmd-paste)
      (should (equal (plist-get ghostel-cmd--fill :vars)
                     '(("file" . "foo.py") ("expr" . "all"))))
      (dolist (typed '("other.py" "login"))
        (ghostel-cmd-fill-clear)
        (execute-kbd-macro typed)
        (ghostel-cmd--fill-commit-current))
      (ghostel-cmd-fill-cancel)
      (ghostel-cmd-paste)
      (should (equal (plist-get ghostel-cmd--fill :vars)
                     '(("file" . "foo.py") ("expr" . "login"))))
      (should (equal (plist-get ghostel-cmd--fill :input) "foo.py"))
      (ghostel-cmd-fill-cancel)
      ;; Per project and per command.
      (should (equal (ghostel-cmd--fill-vars
                      (list :name "k" :cmd "x {{expr:all}}") "/elsewhere/")
                     '(("expr" . "all"))))
      (should (equal (ghostel-cmd--fill-vars
                      (list :name "k2" :cmd "x {{expr:all}}") ghostel-cmd--root)
                     '(("expr" . "all")))))))

(ert-deftest ghostel-cmd-test-choices-parse ()
  "{{name:a|b|c}} lists choices, trimmed, the first the default; one
without a | stays a plain default."
  (let ((tmpl "deploy {{env: dev | staging |prod}} {{tag:latest}} {{flag:|-f}} {{env}}"))
    (should (equal (ghostel-cmd--extract-variables tmpl "/tmp/")
                   '(("env" . "dev") ("tag" . "latest") ("flag" . ""))))
    (should (equal (ghostel-cmd--extract-choices tmpl)
                   '(("env" "dev" "staging" "prod") ("flag" "" "-f"))))
    (should (equal (ghostel-cmd--apply
                    tmpl '(("env" . "prod") ("tag" . "v1") ("flag" . "")))
                   "deploy prod v1  prod"))))

(defmacro ghostel-cmd-tests--delivering (&rest body)
  "Run BODY with pastes into the terminal collected in `sent', newest first."
  (declare (indent 0))
  `(let ((sent nil)
         (ghostel-cmd--last-values (make-hash-table :test #'equal)))
     (cl-letf (((symbol-function 'ghostel-cmd--deliver)
                (lambda (cmd submit) (push (cons cmd submit) sent))))
       ,@body)))

(defun ghostel-cmd-tests--input ()
  (plist-get ghostel-cmd--fill :input))

(ert-deftest ghostel-cmd-test-choices-cycle-and-type-over ()
  "C-n / C-p and the arrows cycle the choices, marked on the footer;
typing, C-y and DEL replace a picked choice."
  (ghostel-cmd-tests--with-overlay
      (list (list :name "dep" :cmd "deploy {{env:dev|staging|prod}} {{n:1}}"))
    (ghostel-cmd-tests--delivering
      (let ((kill-ring nil)
            (interprogram-paste-function nil))
        (ghostel-cmd-paste)
        (should (equal (ghostel-cmd-tests--input) "dev"))
        (should (string-search "{{env}} [dev] staging prod  C-n/C-p pick"
                               (ghostel-cmd--footer)))
        (execute-kbd-macro (kbd "C-n"))
        (should (equal (ghostel-cmd-tests--input) "staging"))
        (execute-kbd-macro (kbd "<down> C-n"))
        (should (equal (ghostel-cmd-tests--input) "dev"))
        (execute-kbd-macro (kbd "C-p <up>"))
        (should (equal (ghostel-cmd-tests--input) "staging"))
        (should (string-search "dev [staging] prod" (ghostel-cmd--footer)))
        (execute-kbd-macro "qa")
        (should (equal (ghostel-cmd-tests--input) "qa"))
        (should (string-search "{{env}} dev staging prod" (ghostel-cmd--footer)))
        (execute-kbd-macro (kbd "C-n"))
        (should (equal (ghostel-cmd-tests--input) "dev"))
        (kill-new "eu-1")
        (execute-kbd-macro (kbd "C-y"))
        (should (equal (ghostel-cmd-tests--input) "eu-1"))
        (execute-kbd-macro (kbd "C-p DEL"))
        (should (equal (ghostel-cmd-tests--input) ""))
        (execute-kbd-macro (kbd "u s RET"))
        (should (string-search "{{n}} [1]" (ghostel-cmd--footer)))
        (execute-kbd-macro (kbd "C-n RET"))
        (should (equal sent '(("deploy us 1" . nil))))))))

(ert-deftest ghostel-cmd-test-choices-start-from-last-value ()
  "The last value starts the next fill, picked when it is a choice and
as typed text when it was free text."
  (ghostel-cmd-tests--with-overlay
      (list (list :name "dep" :cmd "deploy {{env:dev|staging|prod}}"))
    (ghostel-cmd-tests--delivering
      (ghostel-cmd-paste)
      (execute-kbd-macro (kbd "C-n C-n RET"))
      (ghostel-cmd-paste)
      (should (equal (ghostel-cmd-tests--input) "prod"))
      (should (string-search "[prod]" (ghostel-cmd--footer)))
      (execute-kbd-macro (kbd "C-n e u RET"))
      (should (equal (car sent) '("deploy eu" . nil)))
      (ghostel-cmd-paste)
      (should (equal (ghostel-cmd-tests--input) "eu"))
      (execute-kbd-macro "-1")
      (should (equal (ghostel-cmd-tests--input) "eu-1"))
      (execute-kbd-macro (kbd "<up>"))
      (should (equal (ghostel-cmd-tests--input) "prod")))))

(ert-deftest ghostel-cmd-test-choices-input-method-edits-the-pick ()
  "C-\\ edits a picked choice as text, which typing then extends; C-g
puts the pick back."
  (ghostel-cmd-tests--with-overlay
      (list (list :name "dep" :cmd "deploy {{env:dev|staging}}"))
    (let ((default-input-method nil))
      (ghostel-cmd-paste)
      (ghostel-cmd-tests--type "C-\\ - 2 RET")
      (should (equal (ghostel-cmd-tests--input) "dev-2"))
      (execute-kbd-macro "x")
      (should (equal (ghostel-cmd-tests--input) "dev-2x"))
      (execute-kbd-macro (kbd "C-n"))
      (ghostel-cmd-tests--type "C-\\ y C-g")
      (should (equal (ghostel-cmd-tests--input) "dev"))
      (execute-kbd-macro "q")
      (should (equal (ghostel-cmd-tests--input) "q")))))

(ert-deftest ghostel-cmd-test-empty-choice-pastes-empty ()
  "An empty choice, picked, pastes as empty, not as the default."
  (ghostel-cmd-tests--with-overlay
      (list (list :name "rm" :cmd "rm {{flag:-i|}} x"))
    (ghostel-cmd-tests--delivering
      (ghostel-cmd-paste)
      (should (string-search "[-i] \"\"" (ghostel-cmd--footer)))
      (execute-kbd-macro (kbd "C-n"))
      (should (string-match-p "^rm  x\n{{flag}}  $"
                              (buffer-substring-no-properties
                               ghostel-cmd--preview-start (point-max))))
      (execute-kbd-macro (kbd "RET"))
      (should (equal (car sent) '("rm  x" . nil)))
      (ghostel-cmd-paste)
      (should (string-search "-i [\"\"]" (ghostel-cmd--footer)))
      (execute-kbd-macro (kbd "RET"))
      (should (equal (car sent) '("rm  x" . nil))))))

(ert-deftest ghostel-cmd-test-footer-shows-percent ()
  "A % in a choice or the query shows as itself on the mode line."
  (let ((ghostel-cmd--root "/tmp/p/")
        (ghostel-cmd--query "50%")
        (ghostel-cmd--filtering t)
        (ghostel-cmd--fill nil))
    (should (string-search "/50%%" (ghostel-cmd--footer)))
    (setq ghostel-cmd--fill (list :vars '(("q" . "50%")) :index 0 :input "50%"
                                  :choices '(("q" "50%" "90%"))))
    (should (string-search "[50%%] 90%%" (ghostel-cmd--footer)))))

(defun ghostel-cmd-tests--ids ()
  "Ids of the listed rows, top to bottom."
  (save-excursion
    (goto-char (point-min))
    (let ((ids nil))
      (while (tabulated-list-get-id)
        (push (tabulated-list-get-id) ids)
        (forward-line 1))
      (nreverse ids))))

(defun ghostel-cmd-tests--goto (id)
  "Put point on the row ID."
  (goto-char (point-min))
  (while (and (not (eobp)) (not (equal (tabulated-list-get-id) id)))
    (forward-line 1))
  (should (equal (tabulated-list-get-id) id)))

(defun ghostel-cmd-tests--line ()
  (string-trim (buffer-substring-no-properties (line-beginning-position)
                                               (line-end-position))))

(defun ghostel-cmd-tests--globals (cmds)
  "Make CMDS the global commands and redraw the overlay."
  (ghostel-cmd--set-commands cmds ghostel-cmd--global-key)
  (ghostel-cmd--refresh-overlay))

(ert-deftest ghostel-cmd-test-global-commands-share-the-save-file ()
  "Global commands sit in the save file under a key that is no path; a
file without it reads as before, and an outside edit of it is seen."
  (ghostel-cmd-tests--with-store
    (let ((root ghostel-cmd--root)
          (global ghostel-cmd--global-key))
      (ghostel-cmd-tests--write-outside `((,root (:name "a" :cmd "echo a"))) 10)
      (should (equal (ghostel-cmd-tests--names root) '("a")))
      (should-not (ghostel-cmd--get-commands global))
      (ghostel-cmd--set-commands (list (list :name "gs" :cmd "git status")) global)
      (should (equal (with-temp-buffer
                       (insert-file-contents ghostel-cmd-save-file)
                       (read (current-buffer)))
                     `((:global (:name "gs" :cmd "git status"))
                       (,root (:name "a" :cmd "echo a")))))
      (ghostel-cmd-tests--write-outside
       `((:global (:name "gs" :cmd "git status") (:name "gl" :cmd "git log"))
         (,root (:name "a" :cmd "echo a")))
       20)
      (should (equal (ghostel-cmd-tests--names global) '("gs" "gl")))
      (should (equal (mapcar #'car (ghostel-cmd--rows root))
                     '((project . "a") (global . "gs") (global . "gl")))))))

(ert-deftest ghostel-cmd-test-global-listed-after-project-tagged ()
  "The global commands follow the project's, every row tagged; a name
in both lists shows twice.  Without globals there is no tag column."
  (ghostel-cmd-tests--with-overlay
      (list (list :name "test" :cmd "pytest") (list :name "a" :cmd "echo a"))
    (ghostel-cmd-tests--delivering
      (should (= (length tabulated-list-format) 3))
      (ghostel-cmd-tests--globals (list (list :name "gs" :cmd "git status")
                                        (list :name "test" :cmd "make test")))
      (should (equal (ghostel-cmd-tests--ids)
                     '((project . "test") (project . "a")
                       (global . "gs") (global . "test"))))
      (should (= (length tabulated-list-format) 4))
      (goto-char (point-min))
      (should (string-match-p "\\`proj +test +pytest" (ghostel-cmd-tests--line)))
      (ghostel-cmd-tests--goto '(global . "test"))
      (should (string-match-p "\\`global +test +make test" (ghostel-cmd-tests--line)))
      (execute-kbd-macro (kbd "RET"))
      (should (equal sent '(("make test" . nil)))))))

(ert-deftest ghostel-cmd-test-add-global-and-move-between-lists ()
  "A adds a global command; m moves one between the project's list and
the global one, asking before it replaces a namesake there."
  (ghostel-cmd-tests--with-overlay (ghostel-cmd-tests--abc)
    (let ((root ghostel-cmd--root)
          (global ghostel-cmd--global-key))
      (ghostel-cmd-tests--answering '("git status" "gs" "") t
        (execute-kbd-macro "A")
        (should (= asked 0)))
      (should (equal (ghostel-cmd-tests--names global) '("gs")))
      (should (equal (ghostel-cmd-tests--names root) '("a" "b" "c")))
      (should (equal (tabulated-list-get-id) '(global . "gs")))
      (ghostel-cmd-tests--goto '(project . "b"))
      (execute-kbd-macro "m")
      (should (equal (tabulated-list-get-id) '(global . "b")))
      (should (equal (ghostel-cmd-tests--names root) '("a" "c")))
      (should (equal (ghostel-cmd-tests--names global) '("gs" "b")))
      (execute-kbd-macro "m")
      (should (equal (tabulated-list-get-id) '(project . "b")))
      (setq ghostel-cmd--cache nil
            ghostel-cmd--cache-stamp nil)
      (should (equal (ghostel-cmd-tests--names root) '("a" "c" "b")))
      (should (equal (ghostel-cmd-tests--names global) '("gs")))
      (ghostel-cmd-tests--globals (list (list :name "gs" :cmd "git status")
                                        (list :name "a" :cmd "echo global a")))
      (ghostel-cmd-tests--goto '(project . "a"))
      (ghostel-cmd-tests--answering nil nil
        (should-error (ghostel-cmd-move-scope) :type 'user-error)
        (should (= asked 1)))
      (should (equal (ghostel-cmd-tests--names root) '("a" "c" "b")))
      (ghostel-cmd-tests--answering nil t
        (ghostel-cmd-move-scope))
      (should (equal (ghostel-cmd-tests--names root) '("c" "b")))
      (should (equal (ghostel-cmd--get-commands global)
                     (list (list :name "gs" :cmd "git status")
                           (list :name "a" :cmd "echo a")))))))

(ert-deftest ghostel-cmd-test-edit-delete-reorder-stay-in-scope ()
  "e, d and M-j / M-k act on the list the command at point is in;
reordering never crosses into the other list."
  (ghostel-cmd-tests--with-overlay
      (list (list :name "x" :cmd "echo px")
            (list :name "w" :cmd "echo pw")
            (list :name "y" :cmd "echo py"))
    (let ((root ghostel-cmd--root)
          (global ghostel-cmd--global-key))
      (ghostel-cmd-tests--globals (list (list :name "x" :cmd "echo gx")
                                        (list :name "z" :cmd "echo gz")))
      ;; Below y is the global x: no neighbor, though the project has an x.
      (ghostel-cmd-tests--goto '(project . "y"))
      (execute-kbd-macro (kbd "M-j"))
      (should (equal (ghostel-cmd-tests--ids)
                     '((project . "x") (project . "w") (project . "y")
                       (global . "x") (global . "z"))))
      (execute-kbd-macro (kbd "M-k"))
      (should (equal (ghostel-cmd-tests--names root) '("x" "y" "w")))
      (ghostel-cmd-tests--goto '(global . "x"))
      (execute-kbd-macro (kbd "M-k"))
      (should (equal (ghostel-cmd-tests--names global) '("x" "z")))
      (execute-kbd-macro (kbd "M-j"))
      (should (equal (ghostel-cmd-tests--names global) '("z" "x")))
      (should (equal (tabulated-list-get-id) '(global . "x")))
      (ghostel-cmd-tests--answering '("x" "echo GX" "") t
        (ghostel-cmd-edit)
        (should (= asked 0)))
      (should (equal (plist-get (nth 1 (ghostel-cmd--get-commands global)) :cmd)
                     "echo GX"))
      (should (equal (plist-get (car (ghostel-cmd--get-commands root)) :cmd)
                     "echo px"))
      (should (equal (tabulated-list-get-id) '(global . "x")))
      (ghostel-cmd-tests--answering nil t
        (ghostel-cmd-delete))
      (should (equal (ghostel-cmd-tests--names global) '("z")))
      (should (equal (ghostel-cmd-tests--names root) '("x" "y" "w"))))))

(ert-deftest ghostel-cmd-test-last-value-per-project-for-global ()
  "A global command remembers its values per project, apart from a
project command of the same name."
  (ghostel-cmd-tests--with-overlay (list (list :name "t" :cmd "proj {{v:p}}"))
    (ghostel-cmd-tests--delivering
      (ghostel-cmd-tests--globals (list (list :name "t" :cmd "glob {{v:g}}")))
      (ghostel-cmd-tests--goto '(global . "t"))
      (ghostel-cmd-paste)
      (execute-kbd-macro (kbd "C-u o n e RET"))
      (should (equal (car sent) '("glob one" . nil)))
      (ghostel-cmd-paste)
      (should (equal (ghostel-cmd-tests--input) "one"))
      (ghostel-cmd-fill-cancel)
      (ghostel-cmd-tests--goto '(project . "t"))
      (ghostel-cmd-paste)
      (should (equal (ghostel-cmd-tests--input) "p"))
      (ghostel-cmd-fill-cancel)
      (let ((rec (list :name "t" :cmd "glob {{v:g}}")))
        (should (equal (ghostel-cmd--fill-vars rec ghostel-cmd--root 'global)
                       '(("v" . "one"))))
        (should (equal (ghostel-cmd--fill-vars rec "/elsewhere/" 'global)
                       '(("v" . "g"))))))))

(defun ghostel-cmd-tests--write (file text &optional seconds)
  "Write TEXT to FILE in the project root, stamped SECONDS from now."
  (let ((path (expand-file-name file ghostel-cmd--root)))
    (with-temp-file path
      (insert text))
    (when seconds
      (set-file-times path (time-add (current-time) seconds)))))

(defun ghostel-cmd-tests--task-cmds ()
  (mapcar (lambda (row) (plist-get (cdr row) :cmd))
          (ghostel-cmd--discover ghostel-cmd--root)))

(ert-deftest ghostel-cmd-test-npm-scripts-and-runner ()
  "package.json scripts run with the runner the lockfile names, else
the one in packageManager, else npm; the script is the desc."
  (ghostel-cmd-tests--with-store
    (ghostel-cmd-tests--write
     "package.json"
     "{\"scripts\": {\"dev\": \"vite\", \"build:prod\": \"vite build\",
                   \"weird name\": \"echo\", \"nested\": {\"a\": 1}}}")
    (should (equal (ghostel-cmd--discover ghostel-cmd--root)
                   `(((npm . "dev") :name "dev" :cmd "npm run dev" :desc "vite")
                     ((npm . "build:prod") :name "build:prod"
                      :cmd "npm run build:prod" :desc "vite build")
                     ((npm . "weird name") :name "weird name"
                      :cmd ,(concat "npm run " (shell-quote-argument "weird name"))
                      :desc "echo"))))
    (ghostel-cmd-tests--write
     "package.json"
     "{\"packageManager\": \"yarn@4.1.0\", \"scripts\": {\"dev\": \"vite\"}}")
    (should (equal (ghostel-cmd-tests--task-cmds) '("yarn run dev")))
    (should (equal (caar (ghostel-cmd--discover ghostel-cmd--root)) '(yarn . "dev")))
    (dolist (case '(("bun.lockb" . "bun run dev")
                    ("bun.lock" . "bun run dev")
                    ("yarn.lock" . "yarn run dev")
                    ("pnpm-lock.yaml" . "pnpm run dev")))
      (ghostel-cmd-tests--write (car case) "")
      (should (equal (ghostel-cmd-tests--task-cmds) (list (cdr case)))))))

(defconst ghostel-cmd-tests--makefile "\
# comment: not a target
CC := gcc
CFLAGS ?= -O2
LDFLAGS += -lm
X = a:b
Y ::= z
SRCS = a.c \\
b.c: \\
c.c
.PHONY: all test clean
.DEFAULT_GOAL := all

all: build test ## Build and test everything

build: $(OUT)/app
\t$(CC) -o $@ main.c
\t@echo \"done: ok\"

$(OUT)/app: main.c
\tmkdir -p $(OUT)

%.o: %.c
\t$(CC) -c $<

test lint: build
\t./run-tests \\
--flag: value

install:: all
clean:
\trm -rf $(OUT)

debug: CFLAGS += -g
.hidden:
\ttrue

define HELP
fake: target
endef

ifneq (,a:b)
win: ; echo x=1
endif
dist/app.tar.gz: all
")

(ert-deftest ghostel-cmd-test-make-targets ()
  "Real targets only: no assignments, .PHONY or other dotted targets,
pattern or variable rules, recipe or continuation lines, define
bodies or target-specific variables.  GNUmakefile wins, as in make."
  (ghostel-cmd-tests--with-store
    (ghostel-cmd-tests--write "Makefile" ghostel-cmd-tests--makefile)
    (let ((rows (ghostel-cmd--discover ghostel-cmd--root)))
      (should (equal (mapcar #'car rows)
                     '((make . "all") (make . "build") (make . "test")
                       (make . "lint") (make . "install") (make . "clean")
                       (make . "win") (make . "dist/app.tar.gz"))))
      (should (equal (cdar rows) '(:name "all" :cmd "make all"
                                   :desc "Build and test everything"))))
    (ghostel-cmd-tests--write "GNUmakefile" "gnu:\n\ttrue\n")
    (should (equal (ghostel-cmd-tests--task-cmds) '("make gnu")))))

(defconst ghostel-cmd-tests--justfile "\
#!/usr/bin/env just --justfile
set shell := [\"bash\", \"-c\"]
set positional-arguments
export RUST_LOG := \"info\"
version := \"1.0\"
alias b := build

# Build the project
build target=\"debug\":
    cargo build --profile {{target}}

# not a doc: a blank line follows

@test *args: build
    cargo test {{args}}

[private]
helper:
    echo hi

_hidden:
    echo no

[group('ci')]
# Run CI
ci: build test

serve addr=\"localhost:8000\":
    ./serve {{addr}}

[doc('Deploy it')]
deploy env='prod':
    ./deploy
")

(ert-deftest ghostel-cmd-test-just-recipes ()
  "Public recipes only, with the comment right above or [doc] as desc;
settings, assignments, aliases and private recipes are left out."
  (ghostel-cmd-tests--with-store
    (ghostel-cmd-tests--write "justfile" ghostel-cmd-tests--justfile)
    (should (equal (mapcar (lambda (row)
                             (list (cdar row) (plist-get (cdr row) :cmd)
                                   (plist-get (cdr row) :desc)))
                           (ghostel-cmd--discover ghostel-cmd--root))
                   '(("build" "just build" "Build the project")
                     ("test" "just test" "")
                     ("ci" "just ci" "Run CI")
                     ("serve" "just serve" "")
                     ("deploy" "just deploy" "Deploy it"))))))

(ert-deftest ghostel-cmd-test-tasks-cached-by-mtime-never-run ()
  "The files are parsed again only when one changes or appears, or the
setting does; nothing is ever run to find tasks."
  (ghostel-cmd-tests--with-store
    (let ((root ghostel-cmd--root)
          (parsed 0)
          (make-tasks (symbol-function 'ghostel-cmd--make-tasks))
          (ran (lambda (&rest _) (error "Discovery ran a process"))))
      (cl-letf (((symbol-function 'ghostel-cmd--make-tasks)
                 (lambda (file) (cl-incf parsed) (funcall make-tasks file)))
                ((symbol-function 'call-process) ran)
                ((symbol-function 'process-file) ran)
                ((symbol-function 'start-process) ran)
                ((symbol-function 'make-process) ran))
        (ghostel-cmd-tests--write "Makefile" "build:\n\tcc\n")
        (should (equal (mapcar #'car (ghostel-cmd--tasks root)) '((make . "build"))))
        (ghostel-cmd--tasks root)
        (should (= parsed 1))
        (ghostel-cmd-tests--write "Makefile" "build:\n\tcc\ntest:\n" 10)
        (should (equal (mapcar #'car (ghostel-cmd--tasks root))
                       '((make . "build") (make . "test"))))
        (should (= parsed 2))
        (ghostel-cmd-tests--write "justfile" "fmt:\n    gofmt\n")
        (should (equal (mapcar #'car (ghostel-cmd--tasks root))
                       '((make . "build") (make . "test") (just . "fmt"))))
        (should (= parsed 3))
        (let ((ghostel-cmd-discover '(just)))
          (should (equal (mapcar #'car (ghostel-cmd--tasks root)) '((just . "fmt")))))
        (let ((ghostel-cmd-discover nil))
          (should-not (ghostel-cmd--tasks root)))
        (should (= parsed 3))))))

(ert-deftest ghostel-cmd-test-broken-task-file-shows-nothing ()
  "A package.json or Makefile that does not parse lists nothing for its
source, says so once, and the overlay keeps working."
  (ghostel-cmd-tests--with-overlay (list (list :name "a" :cmd "echo a"))
    (let ((messages nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when fmt (push (apply #'format-message fmt args) messages)))))
        (ghostel-cmd-tests--write "package.json" "{\"scripts\": {\"dev\": ")
        (ghostel-cmd-tests--write "Makefile" "define HELP\nfoo: bar\n")
        (ghostel-cmd-tests--write "justfile" "fmt:\n    gofmt\n")
        (ghostel-cmd--refresh-overlay)
        (should (equal (ghostel-cmd-tests--ids) '((project . "a") (just . "fmt"))))
        (should (= (length messages) 2))
        (should (cl-some (lambda (m) (string-match-p "could not read .*package\\.json" m))
                         messages))
        (should (cl-some (lambda (m) (string-match-p "could not read .*Makefile: .*endef" m))
                         messages))
        (ghostel-cmd--refresh-overlay)
        (should (= (length messages) 2))))))

(ert-deftest ghostel-cmd-test-tasks-listed-read-only-after-bookmarks ()
  "Tasks follow the bookmarks, tagged by source; RET and C-RET paste
them, e / d / M-j / M-k refuse, and m copies one into the project."
  (ghostel-cmd-tests--with-overlay (list (list :name "build" :cmd "./build.sh"))
    (ghostel-cmd-tests--delivering
      (ghostel-cmd-tests--write "Makefile" "build: ## Compile\n\tcc\nclean:\n\trm\n")
      (ghostel-cmd-tests--globals (list (list :name "gs" :cmd "git status")))
      (should (equal (ghostel-cmd-tests--ids)
                     '((project . "build") (global . "gs")
                       (make . "build") (make . "clean"))))
      (ghostel-cmd-tests--goto '(make . "build"))
      (should (string-match-p "\\`make +build +make build +Compile"
                              (ghostel-cmd-tests--line)))
      (dolist (cmd '(ghostel-cmd-edit ghostel-cmd-delete
                     ghostel-cmd-move-down ghostel-cmd-move-up))
        (should-error (funcall cmd) :type 'user-error))
      (execute-kbd-macro (kbd "RET"))
      (should (equal (car sent) '("make build" . nil)))
      (ghostel-cmd-tests--goto '(make . "clean"))
      (execute-kbd-macro (kbd "C-<return>"))
      (should (equal (car sent) '("make clean" . t)))
      (execute-kbd-macro "m")
      (should (equal (tabulated-list-get-id) '(project . "clean")))
      (should (equal (ghostel-cmd--get-commands ghostel-cmd--root)
                     (list (list :name "build" :cmd "./build.sh")
                           (list :name "clean" :cmd "make clean" :desc ""))))
      (should (member '(make . "clean") (ghostel-cmd-tests--ids)))
      (ghostel-cmd-tests--goto '(make . "build"))
      (ghostel-cmd-tests--answering nil nil
        (should-error (ghostel-cmd-move-scope) :type 'user-error)
        (should (= asked 1))))))

(ert-deftest ghostel-cmd-test-help-text-lists-overlay-keys ()
  (let ((text (ghostel-cmd--help-text)))
    (should (string-match-p "RET" text))
    (should (string-match-p "Paste" text))
    (should (string-match-p "Live-narrow" text))))

(ert-deftest ghostel-cmd-test-display-reuses-other-window ()
  (let ((file-buf (get-buffer-create " *ghostel-cmd-file*"))
        (term-buf (get-buffer-create " *ghostel-cmd-term*")))
    (unwind-protect
        (progn
          (delete-other-windows)
          (set-window-buffer (selected-window) file-buf)
          (let ((other (split-window-right)))
            (set-window-buffer other term-buf)
            (select-window (get-buffer-window file-buf))
            (ghostel-cmd--display-terminal term-buf)
            (should (eq (window-buffer (selected-window)) term-buf))
            (should (eq (window-buffer (get-buffer-window file-buf))
                        file-buf))))
      (when (buffer-live-p file-buf) (kill-buffer file-buf))
      (when (buffer-live-p term-buf) (kill-buffer term-buf))
      (delete-other-windows))))

(ert-deftest ghostel-cmd-test-display-splits-instead-of-stealing ()
  (let ((file-buf (get-buffer-create " *ghostel-cmd-file2*"))
        (term-buf (get-buffer-create " *ghostel-cmd-term2*")))
    (unwind-protect
        (progn
          (delete-other-windows)
          (set-window-buffer (selected-window) file-buf)
          (ghostel-cmd--display-terminal term-buf)
          (should (eq (window-buffer (get-buffer-window file-buf))
                      file-buf))
          (should (get-buffer-window term-buf))
          (should-not (eq (get-buffer-window file-buf)
                          (get-buffer-window term-buf))))
      (when (buffer-live-p file-buf) (kill-buffer file-buf))
      (when (buffer-live-p term-buf) (kill-buffer term-buf))
      (delete-other-windows))))

(ert-deftest ghostel-cmd-test-focus-leaving-hides-posframe ()
  "Selecting a window outside the posframe hides it, focus staying put;
the minibuffer and the posframe's own window do not."
  (let ((pf 'elsewhere)
        (hidden nil))
    (cl-letf (((symbol-function 'ghostel-cmd--posframe-frame) (lambda () pf))
              ((symbol-function 'ghostel-cmd--posframe-showing-p) (lambda () t))
              ((symbol-function 'ghostel-cmd--hide-posframe)
               (lambda (&optional keep-focus) (push keep-focus hidden))))
      (ghostel-cmd--hide-on-focus-leave (selected-frame))
      (should (equal hidden '(t)))
      (setq hidden nil)
      (with-selected-window (minibuffer-window)
        (ghostel-cmd--hide-on-focus-leave (selected-frame)))
      (should-not hidden)
      (setq pf (selected-frame))
      (ghostel-cmd--hide-on-focus-leave (selected-frame))
      (should-not hidden))))

(ert-deftest ghostel-cmd-test-hiding-removes-global-hooks ()
  "Showing the posframe adds its global hooks; hiding it removes them."
  (ghostel-cmd-tests--with-overlay (ghostel-cmd-tests--abc)
    (let ((window-size-change-functions nil)
          (window-selection-change-functions nil)
          (hooked (lambda ()
                    (list (memq #'ghostel-cmd--posframe-on-parent-resize
                                (default-value 'window-size-change-functions))
                          (memq #'ghostel-cmd--hide-on-focus-leave
                                (default-value
                                 'window-selection-change-functions))))))
      (cl-letf (((symbol-function 'posframe-show) #'ignore)
                ((symbol-function 'posframe-hide) #'ignore))
        (ghostel-cmd--show-posframe (current-buffer))
        (should (cl-every #'identity (funcall hooked)))
        (ghostel-cmd--hide-posframe t)
        (should-not (cl-some #'identity (funcall hooked)))
        (ghostel-cmd--show-posframe (current-buffer))
        (should (cl-every #'identity (funcall hooked)))
        (ghostel-cmd--show-side-window (current-buffer))
        (should-not (cl-some #'identity (funcall hooked)))))))

(ert-deftest ghostel-cmd-test-alias-run-is-sidebar ()
  (should (eq (indirect-function 'ghostel-project-cmd-run)
              (indirect-function 'ghostel-cmd-sidebar))))

(provide 'ghostel-cmd-tests)
;;; ghostel-cmd-tests.el ends here

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
    (should (equal (tabulated-list-get-id) "b2"))))

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
    (should (equal (tabulated-list-get-id) "a"))
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
    (should (equal (tabulated-list-get-id) "a"))))

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
    (should (equal (tabulated-list-get-id) "python"))
    (execute-kbd-macro (kbd "C-p"))
    (should (equal (tabulated-list-get-id) "npm"))
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

(ert-deftest ghostel-cmd-test-alias-run-is-sidebar ()
  (should (eq (indirect-function 'ghostel-project-cmd-run)
              (indirect-function 'ghostel-cmd-sidebar))))

(provide 'ghostel-cmd-tests)
;;; ghostel-cmd-tests.el ends here

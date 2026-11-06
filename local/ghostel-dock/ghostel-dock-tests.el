;;; ghostel-dock-tests.el --- Tests for ghostel-dock  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;   B=~/.emacs.d/emacs-home/straight/build
;;   emacs --batch -L $B/ghostel -L ~/.emacs.d/local/ghostel-cmd \
;;         -L ~/.emacs.d/local/ghostel-dock \
;;         -l ghostel-dock-tests.el -f ert-run-tests-batch-and-exit
;;
;; The real ghostel is loaded, its mode and hooks; `ghostel' itself is
;; stood in for, since starting a shell needs its native module.  Its
;; stand-in makes a ghostel-mode buffer the way `ghostel' does -- named
;; after `ghostel-buffer-name', its identity set, popped to through the
;; display action -- with no process.  Projects are directories with a
;; .proj file in them.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'project)
(require 'ghostel-dock)

(defvar ghostel-dock-tests--made nil
  "Buffers a test made, killed after it.")

(defvar ghostel-dock-tests--tmp nil
  "The scratch directory of the test that runs.")

;; Never the user's file: in batch `user-emacs-directory' is ~/.emacs.d/,
;; and `kill-emacs-hook' saves.  Each test binds its own.
(setq ghostel-dock-save-file
      (expand-file-name (format "ghostel-dock-tests-%d.eld" (emacs-pid))
                        temporary-file-directory))

(defun ghostel-dock-tests--find-project (dir)
  "A project for DIR: the nearest directory above it with a .proj file."
  (when-let* ((root (locate-dominating-file dir ".proj")))
    (cons 'transient (file-name-as-directory (expand-file-name root)))))

(defun ghostel-dock-tests--fake-ghostel (&optional _arg)
  "Stand-in for `ghostel' with a non-numeric ARG: a fresh terminal whose
\"shell\" is a live pipe process that never answers."
  (let ((buffer (generate-new-buffer ghostel-buffer-name)))
    (push buffer ghostel-dock-tests--made)
    (with-current-buffer buffer
      (ghostel-mode)
      (setq ghostel--managed-buffer-name (buffer-name)
            ghostel--buffer-identity (buffer-name)
            ghostel--process (make-pipe-process :name "ghostel-dock-fake"
                                                :buffer buffer :noquery t)))
    (pop-to-buffer buffer (append display-buffer--same-window-action
                                  '((category . comint))))
    buffer))

(defmacro ghostel-dock-tests--with (&rest body)
  "Run BODY in one window, with `tmp' a scratch directory and fake shells."
  (declare (indent 0) (debug t))
  `(let* ((ghostel-dock-tests--made nil)
          (ghostel-dock--layers nil)
          (ghostel-dock-layer-function nil)
          (ghostel-dock-switch-layer-function nil)
          (ghostel-dock-after-create-functions nil)
          (ghostel-dock-refresh-interval nil)
          (ghostel-dock-use-posframe nil)
          (ghostel-dock--caller nil)
          (ghostel-dock--store 'unread)
          (ghostel-dock--restored nil)
          (ghostel-dock--written nil)
          (ghostel-dock--save-timer nil)
          (ghostel-dock--waiters nil)
          (ghostel-dock--wait-timer nil)
          (ghostel-dock--unseen nil)
          (ghostel-dock-notify-after 10)
          ;; Batch Emacs's one frame reports no focus: never notify outside.
          (ghostel-dock-notify-system nil)
          (project-find-functions (list #'ghostel-dock-tests--find-project))
          (project--list nil)
          (ghostel-dock-tests--tmp
           (file-name-as-directory (file-truename (make-temp-file "ghostel-dock-" t))))
          (tmp ghostel-dock-tests--tmp)
          (ghostel-dock-save-file (expand-file-name "dock.eld" tmp))
          (default-directory tmp))
     (ignore tmp)
     (cl-letf (((symbol-function 'ghostel) #'ghostel-dock-tests--fake-ghostel))
       (save-window-excursion
         (delete-other-windows)
         (switch-to-buffer "*scratch*")
         (unwind-protect
             (progn ,@body)
           (when-let* ((manager (get-buffer ghostel-dock--buffer-name)))
             (kill-buffer manager))
           (dolist (buffer ghostel-dock-tests--made)
             (when (buffer-live-p buffer)
               (let ((kill-buffer-query-functions nil)
                     (ghostel-query-before-killing nil))
                 (kill-buffer buffer))))
           (dolist (timer (list ghostel-dock--save-timer ghostel-dock--wait-timer))
             (when (timerp timer) (cancel-timer timer)))
           (delete-directory tmp t))))))

(defun ghostel-dock-tests--project (name)
  "Make project NAME under `tmp'; return its root."
  (let ((root (file-name-as-directory (expand-file-name name ghostel-dock-tests--tmp))))
    (make-directory root t)
    (write-region "" nil (expand-file-name ".proj" root))
    root))

(defun ghostel-dock-tests--file (root &optional name)
  "Visit a file NAME in ROOT, made for the test; return its buffer."
  (let ((buffer (find-file-noselect (expand-file-name (or name "a.txt") root))))
    (push buffer ghostel-dock-tests--made)
    buffer))

(defun ghostel-dock-tests--terminal (name dir)
  "A terminal buffer NAME, as `ghostel' made it, its shell in DIR."
  (let ((buffer (generate-new-buffer name)))
    (push buffer ghostel-dock-tests--made)
    (with-current-buffer buffer
      (ghostel-mode)
      (setq default-directory (file-name-as-directory dir)
            ghostel--buffer-identity name
            ghostel--managed-buffer-name name))
    buffer))

(defun ghostel-dock-tests--live (buffer)
  "Give terminal BUFFER a live process: a shell that runs."
  (with-current-buffer buffer
    (setq ghostel--process (make-pipe-process :name "ghostel-dock-test"
                                              :buffer buffer :noquery t)))
  buffer)

(defun ghostel-dock-tests--leaves (state)
  "How many windows window STATE holds."
  (cl-count 'leaf (flatten-tree state)))


;;; Names and membership

(ert-deftest ghostel-dock-test-names ()
  (should (equal (ghostel-dock--root-name "/a/b/rd-dock/") "rd-dock"))
  (should (equal (ghostel-dock--buffer-name "/a/b/rd-dock/" "dev") "*rd-dock:dev*"))
  (should (equal (ghostel-dock--root-name "/ssh:box:/srv/app/") "app@box"))
  (should (equal (ghostel-dock--root-name "/") "/")))

(ert-deftest ghostel-dock-test-free-name ()
  (ghostel-dock-tests--with
    (let ((root (ghostel-dock-tests--project "web")))
      (should (equal (ghostel-dock--free-name root "shell") "shell"))
      (ghostel-dock--create root "shell")
      (should (equal (ghostel-dock--free-name root "shell") "shell-2"))
      (ghostel-dock--create root "shell-2")
      (should (equal (ghostel-dock--free-name root "shell") "shell-3")))))

(ert-deftest ghostel-dock-test-new-terminal-is-named-and-kept ()
  "A new terminal is *PROJECT:NAME*, belongs to its root across a cd, and
a title does not rename it."
  (ghostel-dock-tests--with
    (let* ((root (ghostel-dock-tests--project "web"))
           (made nil)
           (ghostel-dock-after-create-functions
            (list (lambda (buffer caller) (push (cons buffer caller) made))))
           (caller (ghostel-dock-tests--file root))
           (buffer (with-current-buffer caller (ghostel-dock--create root "dev" caller))))
      (should (equal (buffer-name buffer) "*web:dev*"))
      (should (equal made (list (cons buffer caller))))
      (with-current-buffer buffer
        (should (equal default-directory root))
        (should (equal ghostel-dock--name "dev"))
        (setq default-directory "/tmp/")
        (ghostel--set-title "vim notes.txt")
        (should (equal (buffer-name) "*web:dev*")))
      (should (equal (ghostel-dock--root-of buffer) root))
      (should (equal (ghostel-dock-terminals root) (list buffer))))))

(ert-deftest ghostel-dock-test-legacy-terminals-belong-to-their-project ()
  "*P-ghostel* and *P-ghostel*<2> are P's though they cd'd away; their
names stay; a plain *ghostel* goes by its directory."
  (ghostel-dock-tests--with
    (let* ((jrevba (ghostel-dock-tests--project "jrevba"))
           (other (ghostel-dock-tests--project "other"))
           (project--list (list (list jrevba) (list other)))
           (home (ghostel-dock-tests--terminal "*jrevba-ghostel*" jrevba))
           (away (ghostel-dock-tests--terminal "*jrevba-ghostel*<2>" other))
           (plain (ghostel-dock-tests--terminal "*ghostel*" (expand-file-name "src" other))))
      (make-directory (expand-file-name "src" other) t)
      (should (equal (ghostel-dock--root-of home) jrevba))
      (should (equal (ghostel-dock--root-of away) jrevba))
      (should (equal (ghostel-dock--root-of plain) other))
      (should (equal (sort (mapcar #'buffer-name (ghostel-dock-terminals jrevba)) #'string<)
                     '("*jrevba-ghostel*" "*jrevba-ghostel*<2>")))
      (should (equal (ghostel-dock--name-of away) "*jrevba-ghostel*<2>"))
      ;; Its row says shell: the project is on the row already.  Found
      ;; by, and detailed as, its own name still.
      (should (equal (ghostel-dock--label-of home) "shell"))
      (should (equal (ghostel-dock--label-of away) "shell<2>"))
      (should (equal (ghostel-dock--label-of plain) "*ghostel*"))
      (should (equal (plist-get (ghostel-dock--info away) :label) "shell<2>"))
      (should (equal (plist-get (ghostel-dock--info away) :name) "*jrevba-ghostel*<2>"))
      ;; Recorded at the first look: a later cd does not move it.
      (with-current-buffer home (setq default-directory other))
      (should (equal (ghostel-dock--root-of home) jrevba)))))

(ert-deftest ghostel-dock-test-agents-and-exec-buffers-are-not-terminals ()
  (ghostel-dock-tests--with
    (let* ((root (ghostel-dock-tests--project "web"))
           (shell (ghostel-dock-tests--terminal "*web:shell*" root))
           ;; ghostherd's mark, whatever the name
           (agent (ghostel-dock-tests--terminal "*web:helper*" root))
           ;; the name alone, when the mark is lost
           (named (ghostel-dock-tests--terminal "*ghostherd:lead*" root))
           ;; `ghostel-exec' gives no identity
           (exec (generate-new-buffer "*ghostel-exec*")))
      (push exec ghostel-dock-tests--made)
      (with-current-buffer agent (setq-local ghostherd-session-id "helper"))
      (with-current-buffer exec (ghostel-mode) (setq default-directory root))
      (should (equal (ghostel-dock-terminals root) (list shell)))
      (should-not (ghostel-dock--terminal-p (get-buffer "*scratch*"))))))

(ert-deftest ghostel-dock-test-last-used-is-most-recent ()
  (ghostel-dock-tests--with
    (let* ((root (ghostel-dock-tests--project "web"))
           (a (ghostel-dock-tests--terminal "*web:a*" root))
           (b (ghostel-dock-tests--terminal "*web:b*" root))
           (file (ghostel-dock-tests--file root)))
      (switch-to-buffer a)
      (switch-to-buffer b)
      (switch-to-buffer file)
      (should (eq (ghostel-dock-terminal) b))
      (switch-to-buffer a)
      (switch-to-buffer file)
      (should (eq (ghostel-dock-terminal) a)))))

(ert-deftest ghostel-dock-test-outside-a-project ()
  (ghostel-dock-tests--with
    (let ((dir (file-name-as-directory (expand-file-name "loose" tmp))))
      (make-directory dir)
      (let ((default-directory dir))
        (ghostel-dock-toggle))
      (let ((buffer (window-buffer (ghostel-dock--panel))))
        (should (equal (buffer-name buffer) "*loose:shell*"))
        (should (equal (ghostel-dock--root-of buffer) dir))
        (should (equal (buffer-local-value 'default-directory buffer) dir))))))


;;; The panel

(ert-deftest ghostel-dock-test-toggle-shows-and-hides-without-a-split ()
  (ghostel-dock-tests--with
    (let* ((root (ghostel-dock-tests--project "web"))
           (file (ghostel-dock-tests--file root))
           (main (selected-window)))
      (switch-to-buffer file)
      (let ((height (window-total-height main))
            (count (length (window-list))))
        (ghostel-dock-toggle)
        (let ((panel (ghostel-dock--panel)))
          (should panel)
          (should (eq (selected-window) panel))
          (should (eq (window-parameter panel 'window-side) 'bottom))
          (should (equal (buffer-name (window-buffer panel)) "*web:shell*"))
          (should (eq (window-buffer main) file)))
        ;; Again, from the panel: hidden, every window as before.
        (ghostel-dock-toggle)
        (should-not (ghostel-dock--panel))
        (should (= (length (window-list)) count))
        (should (= (window-total-height main) height))
        (should (eq (window-buffer main) file))
        ;; From the file: the same terminal, no second one.
        (select-window main)
        (ghostel-dock-toggle)
        (should (= 1 (length (ghostel-dock-terminals root))))
        ;; And from anywhere it hides.
        (select-window main)
        (ghostel-dock-toggle)
        (should-not (ghostel-dock--panel))
        (should (= (window-total-height main) height))))))

(ert-deftest ghostel-dock-test-toggle-shows-the-last-used ()
  (ghostel-dock-tests--with
    (let* ((root (ghostel-dock-tests--project "web"))
           (file (ghostel-dock-tests--file root))
           (dev (ghostel-dock-tests--terminal "*web:dev*" root))
           (test (ghostel-dock-tests--terminal "*web:test*" root)))
      (switch-to-buffer dev)
      (switch-to-buffer test)
      (switch-to-buffer file)
      (ghostel-dock-toggle)
      (should (eq (window-buffer (ghostel-dock--panel)) test)))))

(ert-deftest ghostel-dock-test-terminal-moves-into-the-panel ()
  "A terminal in a main window docks: one window shows it, the panel."
  (ghostel-dock-tests--with
    (let* ((root (ghostel-dock-tests--project "web"))
           (file (ghostel-dock-tests--file root))
           (shell (ghostel-dock-tests--terminal "*web:shell*" root))
           (main (selected-window)))
      (switch-to-buffer file)
      (switch-to-buffer shell)
      (ghostel-dock-toggle)
      (should (eq (window-buffer (ghostel-dock--panel)) shell))
      (should (eq (window-buffer main) file))
      (should (equal (get-buffer-window-list shell) (list (ghostel-dock--panel)))))))

(ert-deftest ghostel-dock-test-layers-keep-their-own-panel ()
  "A layer switch snapshots windows; wrapped, the snapshot holds no panel
and each layer gets its own panel back.  Unwrapped, the panel comes
back a side window the dock does not know for its panel."
  (ghostel-dock-tests--with
    (let* ((web (ghostel-dock-tests--project "web"))
           (api (ghostel-dock-tests--project "api"))
           (web-file (ghostel-dock-tests--file web))
           (api-file (ghostel-dock-tests--file api))
           (layer "web")
           (states (make-hash-table :test 'equal))
           (ghostel-dock-layer-function (lambda () layer))
           (switch (lambda (to file)
                     ;; jworkspace: snapshot this layer, put back the other's
                     (puthash layer (window-state-get (frame-root-window) 'writable) states)
                     (if-let* ((state (gethash to states)))
                         (window-state-put state (frame-root-window) 'safe)
                       (delete-other-windows)
                       (switch-to-buffer file))
                     (setq layer to))))
      (switch-to-buffer web-file)
      (ghostel-dock-toggle)
      (let ((web-shell (window-buffer (ghostel-dock--panel))))
        (ghostel-dock-without-panel switch "api" api-file)
        (should (= 1 (ghostel-dock-tests--leaves (gethash "web" states))))
        (should-not (ghostel-dock--panel))
        (should (eq (window-buffer (selected-window)) api-file))
        (ghostel-dock-without-panel switch "web" web-file)
        (should (= 1 (ghostel-dock-tests--leaves (gethash "api" states))))
        (should (eq (window-buffer (ghostel-dock--panel)) web-shell))
        (should (eq (window-parameter (ghostel-dock--panel) 'window-side) 'bottom))
        (should (= 2 (length (window-list))))
        ;; Unwrapped, for contrast: a side window comes back, unmarked.
        (funcall switch "api" api-file)
        (funcall switch "web" web-file)
        (should (= 2 (length (window-list))))
        (should-not (ghostel-dock--panel))
        (should (seq-some (lambda (w) (window-parameter w 'window-side))
                          (window-list)))))))


(ert-deftest ghostel-dock-test-jworkspace-layers-and-maximize ()
  "Wired as the config does, jworkspace's layers each keep their own
panel, and its maximize toggles back -- it asks `one-window-p'."
  (skip-unless (require 'jworkspace nil t))
  (ghostel-dock-tests--with
    (let* ((jworkspace-map (make-hash-table :test 'equal))
           (web (ghostel-dock-tests--project "web"))
           (api (ghostel-dock-tests--project "api"))
           (web-file (ghostel-dock-tests--file web))
           (api-file (ghostel-dock-tests--file api))
           (ghostel-dock-layer-function
            (lambda () (when-let* ((ws (jworkspace--get-current-workspace)))
                         (jworkspace-name ws))))
           (wrapped '(jworkspace-switch-workspace jworkspace-toggle-maximize-window)))
      (dolist (fn wrapped)
        (advice-add fn :around #'ghostel-dock-without-panel))
      (unwind-protect
          (progn
            (jworkspace-switch-workspace "web")
            (switch-to-buffer web-file)
            (ghostel-dock-toggle)
            (let ((web-shell (window-buffer (ghostel-dock--panel))))
              (jworkspace-switch-workspace "api")
              (should-not (ghostel-dock--panel))
              (switch-to-buffer api-file)
              (jworkspace-switch-workspace "web")
              (should (eq (window-buffer (ghostel-dock--panel)) web-shell))
              ;; Maximize a split window, then back.
              (select-window (get-buffer-window web-file))
              (split-window-right)
              (should (= 3 (length (window-list))))
              (jworkspace-toggle-maximize-window)
              (should (= 2 (length (window-list))))
              (should (eq (window-buffer (ghostel-dock--panel)) web-shell))
              (select-window (get-buffer-window web-file))
              (jworkspace-toggle-maximize-window)
              (should (= 3 (length (window-list))))
              (should (eq (window-buffer (ghostel-dock--panel)) web-shell))))
        (dolist (fn wrapped)
          (advice-remove fn #'ghostel-dock-without-panel))
        (set-frame-parameter nil 'current-workspace nil)))))


(ert-deftest ghostel-dock-test-wrap-keeps-the-panel-selected ()
  "Through a wrapped call that stays in the layer -- a maximize -- the
panel, selected before, is selected after."
  (ghostel-dock-tests--with
    (let* ((root (ghostel-dock-tests--project "web"))
           (file (ghostel-dock-tests--file root)))
      (switch-to-buffer file)
      (ghostel-dock-toggle)
      (should (eq (selected-window) (ghostel-dock--panel)))
      (ghostel-dock-without-panel (lambda () (should-not (ghostel-dock--panel))))
      (should (eq (selected-window) (ghostel-dock--panel)))
      (select-window (get-buffer-window file))
      (ghostel-dock-without-panel #'ignore)
      (should (eq (window-buffer (selected-window)) file)))))


;;; What a terminal does

(ert-deftest ghostel-dock-test-osc133-status-only-after-a-command ()
  (ghostel-dock-tests--with
    (let ((buffer (ghostel-dock-tests--terminal "*web:shell*" tmp)))
      (should-not (plist-get (ghostel-dock--info buffer) :osc133))
      (run-hook-with-args 'ghostel-command-start-functions buffer)
      (run-hook-with-args 'ghostel-command-finish-functions buffer 2)
      (should (eql (buffer-local-value 'ghostel-dock--status buffer) 2))
      (should (plist-get (ghostel-dock--info buffer) :osc133))
      ;; A prompt redrawn sends D with the status of before, and no C.
      (run-hook-with-args 'ghostel-command-finish-functions buffer 0)
      (should (eql (buffer-local-value 'ghostel-dock--status buffer) 2))
      (run-hook-with-args 'ghostel-command-start-functions buffer)
      (run-hook-with-args 'ghostel-command-finish-functions buffer 0)
      (should (eql (buffer-local-value 'ghostel-dock--status buffer) 0)))))

(ert-deftest ghostel-dock-test-output-is-stamped ()
  (ghostel-dock-tests--with
    (let* ((buffer (ghostel-dock-tests--terminal "*web:shell*" tmp))
           (process (make-pipe-process :name "ghostel-dock-out" :buffer buffer :noquery t)))
      (unwind-protect
          (progn
            (should-not (buffer-local-value 'ghostel-dock--output-at buffer))
            (ghostel--filter process "hello")
            (should (< (- (float-time) (buffer-local-value 'ghostel-dock--output-at buffer)) 5))
            (should (advice-member-p #'ghostel-dock--stamp-output 'ghostel--events-filter)))
        (delete-process process)))))

(ert-deftest ghostel-dock-test-foreground-job ()
  (ghostel-dock-tests--with
    (let ((buffer (ghostel-dock-tests--terminal "*web:shell*" tmp))
          (table '((100 (tpgid . 200) (comm . "login"))
                   (200 (comm . "node") (args . "node /Users/x/.nvm/bin/npm run dev"))
                   (101 (tpgid . 201) (comm . "login"))
                   (201 (comm . "bash") (args . "-/bin/bash --posix"))
                   (102 (tpgid . 202) (comm . "login"))
                   (202 (comm . "docker-compose") (args . "/usr/local/bin/docker-compose up")))))
      (cl-letf (((symbol-function 'process-attributes)
                 (lambda (pid) (cdr (assq pid table)))))
        (with-current-buffer buffer (setq ghostel--pid 100))
        (should (equal (ghostel-dock--foreground buffer) '(200 . "node npm run dev")))
        (with-current-buffer buffer (setq ghostel--pid 101))
        (should-not (ghostel-dock--foreground buffer))
        (with-current-buffer buffer (setq ghostel--pid 102))
        (should (equal (ghostel-dock--foreground buffer) '(202 . "docker-compose up")))))
    (should (ghostel-dock--shell-p "-bash"))
    (should (ghostel-dock--shell-p "/bin/zsh"))
    (should-not (ghostel-dock--shell-p "vim"))))

(ert-deftest ghostel-dock-test-info-texts ()
  (should (equal (ghostel-dock--age nil) ""))
  (should (equal (ghostel-dock--age 100 101) "now"))
  (should (equal (ghostel-dock--age 100 130) "30s"))
  (should (equal (ghostel-dock--age 100 400) "5m"))
  (should (equal (ghostel-dock--age 0 7300) "2h"))
  (should (equal (ghostel-dock--age 0 200000) "2d"))
  (should (equal (ghostel-dock--status-text '(:status nil)) ""))
  (should (equal (substring-no-properties (ghostel-dock--status-text '(:status 0))) "✓"))
  (should (equal (substring-no-properties (ghostel-dock--status-text '(:status 2))) "✗ 2"))
  (should (equal (ghostel-dock--dir-text '(:root "/p/web/" :dir "/p/web/")) "."))
  (should (equal (ghostel-dock--dir-text '(:root "/p/web/" :dir "/p/web/src/")) "./src"))
  (should (equal (ghostel-dock--dir-text '(:root "/p/web/" :dir "/opt/x/")) "/opt/x"))
  (should (equal (substring-no-properties (ghostel-dock--job-text '(:job nil))) "idle"))
  (should (equal (substring-no-properties (ghostel-dock--job-text '(:job exited))) "exited"))
  (should (equal (substring-no-properties (ghostel-dock--job-text '(:job "make test")))
                 "▶ make test")))

(ert-deftest ghostel-dock-test-rerun ()
  (ghostel-dock-tests--with
    (let* ((root (ghostel-dock-tests--project "web"))
           (file (ghostel-dock-tests--file root))
           (shell (ghostel-dock-tests--live (ghostel-dock-tests--terminal "*web:shell*" root)))
           (keys nil))
      (switch-to-buffer file)
      (cl-letf (((symbol-function 'ghostel-send-key)
                 (lambda (key &rest _) (push (cons (current-buffer) key) keys))))
        (ghostel-dock-rerun)
        (should (equal (reverse keys) `((,shell . "up") (,shell . "return"))))
        ;; The panel shows it; the file keeps the focus.
        (should (eq (window-buffer (ghostel-dock--panel)) shell))
        (should (eq (window-buffer (selected-window)) file))
        (setq keys nil)
        (with-current-buffer shell (setq ghostel--command-running t))
        (should-error (ghostel-dock-rerun) :type 'user-error)
        (should-not keys)))))


(ert-deftest ghostel-dock-test-starting-one-keeps-the-focus ()
  "Ghostel pops to what it starts; the dock gives the focus back, so a
rerun that had to start the terminal leaves the file selected."
  (ghostel-dock-tests--with
    (let* ((root (ghostel-dock-tests--project "web"))
           (file (ghostel-dock-tests--file root)))
      (switch-to-buffer file)
      (cl-letf (((symbol-function 'ghostel-send-key) #'ignore)
                ((symbol-function 'ghostel-dock--info)
                 (lambda (buffer) (list :root root :name (buffer-name buffer)))))
        (ghostel-dock-rerun))
      (should (equal (buffer-name (window-buffer (ghostel-dock--panel))) "*web:shell*"))
      (should (eq (window-buffer (selected-window)) file)))))


;;; The manager

(defun ghostel-dock-tests--manager-lines ()
  "The manager's rows as printed, trailing blanks cut."
  (with-current-buffer ghostel-dock--buffer-name
    (mapcar #'string-trim-right
            (split-string (buffer-substring-no-properties
                           (point-min) (1- ghostel-dock--preview-start))
                          "\n"))))

(defmacro ghostel-dock-tests--with-manager (&rest body)
  "BODY with projects web (dev, test) and api (shell), the manager opened
from api's file.  Binds web, api, dev, test, shell, api-file."
  (declare (indent 0) (debug t))
  `(ghostel-dock-tests--with
     (let* ((web (ghostel-dock-tests--project "web"))
            (api (ghostel-dock-tests--project "api"))
            (dev (ghostel-dock-tests--live (ghostel-dock-tests--terminal "*web:dev*" web)))
            (test (ghostel-dock-tests--live (ghostel-dock-tests--terminal "*web:test*" web)))
            (shell (ghostel-dock-tests--live (ghostel-dock-tests--terminal "*api:shell*" api)))
            (api-file (ghostel-dock-tests--file api)))
       (ignore dev test shell)
       (switch-to-buffer test)
       (switch-to-buffer dev)
       (switch-to-buffer shell)
       (switch-to-buffer api-file)
       (ghostel-dock-manager)
       (with-current-buffer ghostel-dock--buffer-name
         ,@body))))

(ert-deftest ghostel-dock-test-manager-lists-this-project-first ()
  (ghostel-dock-tests--with-manager
    (should (equal ghostel-dock--rows (list shell dev test)))
    (let ((lines (ghostel-dock-tests--manager-lines)))
      (should (= 3 (length lines)))
      (should (string-match-p "\\` api +shell +idle" (nth 0 lines)))
      (should (string-match-p "\\` web +dev +idle" (nth 1 lines)))
      ;; The project shows once, on its first row.
      (should (string-match-p "\\` +test +idle" (nth 2 lines))))
    (should (eq (selected-window) (get-buffer-window ghostel-dock--buffer-name)))
    (should (eq (window-parameter (selected-window) 'window-side) 'top))))

(ert-deftest ghostel-dock-test-manager-previews-and-restores ()
  (ghostel-dock-tests--with-manager
    (should (eq (tabulated-list-get-id) shell))
    (execute-kbd-macro (kbd "j"))
    (should (eq (tabulated-list-get-id) dev))
    (should (string-match-p "\\*web:dev\\*" (buffer-substring ghostel-dock--preview-start
                                                              (point-max))))
    (ghostel-dock--preview)
    (should (eq (window-buffer (ghostel-dock--panel)) dev))
    (execute-kbd-macro (kbd "j"))
    (ghostel-dock--preview)
    (should (eq (window-buffer (ghostel-dock--panel)) test))
    ;; q: the panel was not there, so it goes.
    (execute-kbd-macro (kbd "q"))
    (should-not (ghostel-dock--panel))
    (should-not (get-buffer-window ghostel-dock--buffer-name))
    (should-not (ghostel-dock--showing-p))
    (should (eq (window-buffer (selected-window)) api-file))))

(ert-deftest ghostel-dock-test-manager-quit-puts-the-panel-back ()
  (ghostel-dock-tests--with
    (let* ((web (ghostel-dock-tests--project "web"))
           (file (ghostel-dock-tests--file web))
           (a (ghostel-dock-tests--terminal "*web:a*" web))
           (b (ghostel-dock-tests--terminal "*web:b*" web)))
      (switch-to-buffer b)
      (switch-to-buffer file)
      (ghostel-dock-show a)
      (ghostel-dock-manager)
      (with-current-buffer ghostel-dock--buffer-name
        ;; It opens on the one not on screen.
        (should (eq (tabulated-list-get-id) b))
        (ghostel-dock--preview)
        (should (eq (window-buffer (ghostel-dock--panel)) b))
        (ghostel-dock-manager-quit))
      (should (eq (window-buffer (ghostel-dock--panel)) a)))))

(ert-deftest ghostel-dock-test-manager-open-in-panel-switches-layer ()
  (ghostel-dock-tests--with-manager
    (let* ((switched nil)
           (ghostel-dock-switch-layer-function (lambda (root) (push root switched))))
      (execute-kbd-macro (kbd "j"))
      (execute-kbd-macro (kbd "RET"))
      (should (equal switched (list web)))
      (should (eq (window-buffer (selected-window)) dev))
      (should (eq (selected-window) (ghostel-dock--panel)))
      (should-not (get-buffer-window ghostel-dock--buffer-name)))))

(ert-deftest ghostel-dock-test-manager-open-same-project-no-switch ()
  (ghostel-dock-tests--with-manager
    (let* ((switched nil)
           (ghostel-dock-switch-layer-function (lambda (root) (push root switched))))
      (execute-kbd-macro (kbd "RET"))
      (should-not switched)
      (should (eq (window-buffer (ghostel-dock--panel)) shell)))))

(ert-deftest ghostel-dock-test-manager-open-here ()
  (ghostel-dock-tests--with-manager
    (execute-kbd-macro (kbd "j"))
    (ghostel-dock--preview)
    (execute-kbd-macro (kbd "o"))
    (should-not (ghostel-dock--panel))
    (should (eq (window-buffer (selected-window)) dev))
    (should (= 1 (length (window-list))))))

(ert-deftest ghostel-dock-test-manager-filter ()
  (ghostel-dock-tests--with-manager
    (execute-kbd-macro (kbd "/ t e s"))
    (should ghostel-dock--filtering)
    (should (equal (mapcar #'car ghostel-dock--matched) (list test)))
    (execute-kbd-macro (kbd "RET"))
    (should-not ghostel-dock--filtering)
    (should (equal ghostel-dock--query "tes"))
    ;; Esc: the filter first, then the manager.
    (execute-kbd-macro (kbd "<escape>"))
    (should (equal ghostel-dock--query ""))
    (should (= 3 (length ghostel-dock--matched)))
    (should (get-buffer-window ghostel-dock--buffer-name))))

(ert-deftest ghostel-dock-test-manager-new-in-selected-project ()
  (ghostel-dock-tests--with-manager
    (execute-kbd-macro (kbd "j"))
    (cl-letf (((symbol-function 'read-string)
               (lambda (prompt &rest _)
                 (should (string-match-p "New terminal in web (shell)" prompt))
                 "lint")))
      (execute-kbd-macro (kbd "n")))
    (let ((made (get-buffer "*web:lint*")))
      (should made)
      (should (equal (ghostel-dock--root-of made) web))
      (should (eq (window-buffer (ghostel-dock--panel)) made))
      (should (eq (selected-window) (ghostel-dock--panel))))))

(ert-deftest ghostel-dock-test-manager-rename ()
  (ghostel-dock-tests--with-manager
    (execute-kbd-macro (kbd "j"))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "server")))
      (execute-kbd-macro (kbd "r")))
    (should (equal (buffer-name dev) "*web:server*"))
    (should (equal (ghostel-dock--name-of dev) "server"))
    (should (eq (tabulated-list-get-id) dev))
    ;; A name the project has is refused.
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "test")))
      (should-error (ghostel-dock-manager-rename) :type 'user-error))))

(ert-deftest ghostel-dock-test-manager-kill-asks-when-running ()
  (ghostel-dock-tests--with-manager
    (execute-kbd-macro (kbd "j"))
    (with-current-buffer dev (setq ghostel--command-running t))
    (let ((asked nil))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (prompt) (push prompt asked) nil)))
        (execute-kbd-macro (kbd "x")))
      (should (buffer-live-p dev))
      (should (equal (car asked) "*web:dev* is running a command; kill it? ")))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
      (execute-kbd-macro (kbd "x")))
    (should-not (buffer-live-p dev))
    (should (equal ghostel-dock--rows (list shell test)))
    ;; An idle one goes without a question.
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) (error "Asked"))))
      (ghostel-dock-first)
      (execute-kbd-macro (kbd "x")))
    (should-not (buffer-live-p shell))))

(ert-deftest ghostel-dock-test-manager-kill-idle-lists-them ()
  (ghostel-dock-tests--with-manager
    (let ((extra (ghostel-dock-tests--live (ghostel-dock-tests--terminal "*web:extra*" web)))
          (asked nil))
      (ghostel-dock-manager-refresh)
      (with-current-buffer dev (setq ghostel--command-running t))
      (execute-kbd-macro (kbd "j"))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (prompt) (push prompt asked) t)))
        (execute-kbd-macro (kbd "K")))
      (should (string-match-p "Kill 2 idle in web: " (car asked)))
      (should (buffer-live-p dev))
      (should-not (buffer-live-p test))
      (should-not (buffer-live-p extra))
      (should (buffer-live-p shell)))))

(ert-deftest ghostel-dock-test-manager-rerun-keeps-the-list ()
  (ghostel-dock-tests--with-manager
    (let ((keys nil))
      (cl-letf (((symbol-function 'ghostel-send-key)
                 (lambda (key &rest _) (push (cons (current-buffer) key) keys))))
        (execute-kbd-macro (kbd "R")))
      (should (equal (reverse keys) `((,shell . "up") (,shell . "return"))))
      (should (get-buffer-window ghostel-dock--buffer-name)))))


;;; ghostel-cmd pastes into the dock's terminal

(ert-deftest ghostel-dock-test-ghostel-cmd-delivers-to-the-project-terminal ()
  (ghostel-dock-tests--with
    (let* ((web (ghostel-dock-tests--project "web"))
           (file (ghostel-dock-tests--file web))
           (pasted nil))
      (switch-to-buffer file)
      (cl-letf (((symbol-function 'ghostel-paste-string)
                 (lambda (text) (push (cons (current-buffer) text) pasted)))
                ((symbol-function 'ghostel-send-key) #'ignore))
        ;; None yet: one is started, in the panel.
        (let ((ghostel-cmd--root web))
          (ghostel-cmd--deliver "make test" nil))
        (let ((shell (get-buffer "*web:shell*")))
          (should shell)
          (should (equal pasted `((,shell . "make test"))))
          (should (eq (window-buffer (ghostel-dock--panel)) shell))
          (should (eq (selected-window) (ghostel-dock--panel)))
          ;; The last used of several takes it.
          (let ((dev (ghostel-dock-tests--terminal "*web:dev*" web)))
            (select-window (get-buffer-window file))
            (switch-to-buffer dev)
            (switch-to-buffer file)
            (setq pasted nil)
            (let ((ghostel-cmd--root web))
              (ghostel-cmd--deliver "ls" nil))
            (should (equal pasted `((,dev . "ls"))))
            (should (eq (window-buffer (ghostel-dock--panel)) dev))))))))

;;; Phase 2: commands bound to a named terminal

(defmacro ghostel-dock-tests--keys (var &rest body)
  "BODY with what goes to terminals recorded in VAR, newest first:
(BUFFER . TEXT) for a paste, (BUFFER . KEY) for a key, (BUFFER . C-c)."
  (declare (indent 1) (debug t))
  `(cl-letf (((symbol-function 'ghostel-paste-string)
              (lambda (text) (push (cons (current-buffer) text) ,var)))
             ((symbol-function 'ghostel-send-key)
              (lambda (key &rest _) (push (cons (current-buffer) key) ,var)))
             ((symbol-function 'ghostel-send-C-c)
              (lambda () (push (cons (current-buffer) 'C-c) ,var))))
     ,@body))

(defun ghostel-dock-tests--quiet (buffer)
  "Let terminal BUFFER's shell look done printing: its output a while ago."
  (with-current-buffer buffer
    (setq ghostel-dock--output-at (- (float-time) 5)
          ghostel-dock--born (- (float-time) 10))))

(ert-deftest ghostel-dock-test-run-starts-the-named-terminal ()
  "A bound command goes to its terminal, started when missing, once its
shell shows a prompt; the panel shows it, selected."
  (ghostel-dock-tests--with
    (let* ((web (ghostel-dock-tests--project "web"))
           (file (ghostel-dock-tests--file web))
           (sent nil))
      (switch-to-buffer file)
      (ghostel-dock-tests--keys sent
        (let ((dev (ghostel-dock-run "npm run dev" "dev" web t)))
          (should (equal (buffer-name dev) "*web:dev*"))
          (should (eq (window-buffer (ghostel-dock--panel)) dev))
          (should (eq (selected-window) (ghostel-dock--panel)))
          ;; A new shell prints first: nothing goes yet.
          (should-not sent)
          (ghostel-dock-tests--quiet dev)
          (ghostel-dock--poll-waiters)
          (should (equal (reverse sent) `((,dev . "npm run dev") (,dev . "return"))))
          (should (equal (buffer-local-value 'ghostel-dock--command dev) "npm run dev"))
          (should (buffer-local-value 'ghostel-dock--command-live dev)))))))

(ert-deftest ghostel-dock-test-run-restarts-its-own-command ()
  "Run again while it still runs: interrupt, wait for the prompt, run."
  (ghostel-dock-tests--with
    (let* ((web (ghostel-dock-tests--project "web"))
           (dev (ghostel-dock-tests--live (ghostel-dock-tests--terminal "*web:dev*" web)))
           (sent nil))
      (with-current-buffer dev
        (setq ghostel-dock--name "dev"
              ghostel-dock--command "npm run dev"
              ghostel-dock--command-live (float-time)
              ghostel--command-running t))
      (ghostel-dock-tests--keys sent
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (error "Asked"))))
          (ghostel-dock-run "npm run dev" "dev" web t))
        (should (equal sent `((,dev . C-c))))
        ;; The prompt: OSC 133 D.
        (run-hook-with-args 'ghostel-command-start-functions dev)
        (with-current-buffer dev (setq ghostel--command-running nil))
        (run-hook-with-args 'ghostel-command-finish-functions dev 130)
        (ghostel-dock--poll-waiters)
        (should (equal (reverse sent)
                       `((,dev . C-c) (,dev . "npm run dev") (,dev . "return"))))))))

(ert-deftest ghostel-dock-test-run-asks-before-stopping-something-else ()
  (ghostel-dock-tests--with
    (let* ((web (ghostel-dock-tests--project "web"))
           (dev (ghostel-dock-tests--live (ghostel-dock-tests--terminal "*web:dev*" web)))
           (asked nil)
           (sent nil))
      (with-current-buffer dev
        (setq ghostel-dock--name "dev"
              ghostel--command-running t))
      (ghostel-dock-tests--keys sent
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) (push prompt asked) nil)))
          (ghostel-dock-run "npm test" "dev" web t))
        (should (equal asked '("*web:dev* is running a command; stop it and run npm test? ")))
        (should-not sent)
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
          (ghostel-dock-run "npm test" "dev" web t))
        (should (equal sent `((,dev . C-c))))
        (with-current-buffer dev (setq ghostel--command-running nil))
        (ghostel-dock--poll-waiters)
        (should (equal (car sent) (cons dev "return")))))))

(ert-deftest ghostel-dock-test-run-gives-up-when-it-never-stops ()
  "A second interrupt at half the time; past it, the command is not run."
  (ghostel-dock-tests--with
    (let* ((web (ghostel-dock-tests--project "web"))
           (dev (ghostel-dock-tests--live (ghostel-dock-tests--terminal "*web:dev*" web)))
           (ghostel-dock-stop-timeout 0.2)
           (sent nil))
      (with-current-buffer dev
        (setq ghostel-dock--name "dev"
              ghostel-dock--command "make watch"
              ghostel-dock--command-live (float-time)
              ghostel--command-running t))
      (ghostel-dock-tests--keys sent
        (ghostel-dock-run "make watch" "dev" web t)
        (sleep-for 0.15)
        (ghostel-dock--poll-waiters)
        (should (equal sent `((,dev . C-c) (,dev . C-c))))
        (sleep-for 0.15)
        (ghostel-dock--poll-waiters)
        (should (equal sent `((,dev . C-c) (,dev . C-c))))
        (should-not ghostel-dock--waiters)))))

(ert-deftest ghostel-dock-test-ghostel-cmd-bound-command-goes-to-its-terminal ()
  "ghostel-cmd's deliver for a bound command runs it through the dock."
  (ghostel-dock-tests--with
    (let* ((web (ghostel-dock-tests--project "web"))
           (file (ghostel-dock-tests--file web))
           (runs nil)
           (sent nil))
      (switch-to-buffer file)
      (ghostel-dock-tests--keys sent
        (cl-letf (((symbol-function 'ghostel-dock-run)
                   (lambda (&rest args) (push args runs))))
          (let ((ghostel-cmd--root web))
            (funcall (ghostel-cmd--deliver-to "dev") "npm run dev" t)
            (funcall (ghostel-cmd--deliver-to nil) "ls" nil))))
      (should (equal runs `(("npm run dev" "dev" ,web t))))
      ;; Unbound: the last-used terminal, as before.
      (should (equal sent `((,(get-buffer "*web:shell*") . "ls")))))))

;;; ghostel-cmd: where a command goes, told before; a busy one asks

(defmacro ghostel-dock-tests--jobs (var &rest body)
  "BODY with each terminal's foreground job looked up in VAR, an alist
\(BUFFER PGID . COMMAND); a terminal not in it is idle."
  (declare (indent 1) (debug t))
  `(cl-letf (((symbol-function 'ghostel-dock--foreground)
              (lambda (buffer) (cdr (assq buffer ,var)))))
     ,@body))

(defmacro ghostel-dock-tests--answering (asked answer &rest body)
  "BODY with `completing-read' recording what it is asked in ASKED, newest
first -- (PROMPT CANDIDATES ANNOTATIONS DEFAULT) -- and answering ANSWER,
an expression of `default'."
  (declare (indent 2) (debug t))
  `(cl-letf (((symbol-function 'completing-read)
              (lambda (prompt table &optional _pred _require _initial _hist default &rest _)
                (let* ((meta (funcall table "" nil 'metadata))
                       (annotate (alist-get 'annotation-function (cdr meta)))
                       (candidates (all-completions "" table)))
                  (push (list prompt candidates
                              (mapcar (lambda (c) (substring-no-properties
                                                   (or (funcall annotate c) "")))
                                      candidates)
                              default)
                        ,asked)
                  (let ((default default)) (ignore default) ,answer)))))
     ,@body))

(ert-deftest ghostel-dock-test-target-tells-where-ret-goes ()
  "What ghostel-cmd's footer says: the terminal RET goes to, by its row's
name, new when it starts, what it runs and whether RET asks first."
  (ghostel-dock-tests--with
    (let ((web (ghostel-dock-tests--project "web"))
          (api (ghostel-dock-tests--project "api"))
          (jobs nil))
      (ghostel-dock-tests--jobs jobs
        ;; None open: the one that starts, or comes back first.
        (ghostel-dock-tests--write-save `((,api (:name "server" :dir ,api))))
        (should (equal (ghostel-dock-target web) '(:label "shell" :new t)))
        (should (equal (ghostel-dock-target web "dev") '(:label "dev" :new t)))
        (should (equal (ghostel-dock-target api) '(:label "server" :new t)))
        (let ((old (ghostel-dock-tests--live (ghostel-dock-tests--terminal "*web-ghostel*" web))))
          ;; An old *PROJECT-ghostel* goes by shell, as its manager row.
          (should (equal (ghostel-dock-target web)
                         `(:label "shell" :buffer ,old :job nil :then nil)))
          (push (cons old '(202 . "docker-compose up")) jobs)
          (should (equal (ghostel-dock-target web)
                         `(:label "shell" :buffer ,old :job "docker-compose up" :then ask)))
          ;; OSC 133 alone says busy too.
          (setq jobs nil)
          (with-current-buffer old (setq ghostel--command-running t))
          (should (eq (plist-get (ghostel-dock-target web) :then) 'ask))
          (should (eq (plist-get (ghostel-dock-target web) :job) 'running))
          (with-current-buffer old (setq ghostel--command-running nil))
          ;; Bound: its own command running there restarts; another asks.
          (let ((dev (ghostel-dock-tests--live (ghostel-dock-tests--terminal "*web:dev*" web))))
            (with-current-buffer dev
              (setq ghostel-dock--name "dev"
                    ghostel-dock--command "npm run dev"
                    ghostel-dock--command-live (float-time)))
            (should (equal (ghostel-dock-target web "dev" "npm run dev")
                           `(:label "dev" :buffer ,dev :job nil :then nil)))
            (push (cons dev '(300 . "npm run dev")) jobs)
            (should (eq (plist-get (ghostel-dock-target web "dev" "npm run dev") :then) 'restart))
            (should (eq (plist-get (ghostel-dock-target web "dev" "npm test") :then) 'ask))
            ;; Its {{vars}} not filled: maybe its own, restarted; not told.
            (should-not (plist-get (ghostel-dock-target web "dev") :then))
            (with-current-buffer dev (setq ghostel-dock--command-live nil))
            (should (eq (plist-get (ghostel-dock-target web "dev") :then) 'ask))
            ;; Unbound: still the last used.
            (should (eq (plist-get (ghostel-dock-target web) :buffer) old))))))))

(ert-deftest ghostel-dock-test-paste-into-an-idle-terminal-as-before ()
  "An unbound command goes into the last-used terminal when it is idle:
no question, shown in the panel, selected."
  (ghostel-dock-tests--with
    (let* ((web (ghostel-dock-tests--project "web"))
           (file (ghostel-dock-tests--file web))
           (old (ghostel-dock-tests--live (ghostel-dock-tests--terminal "*web-ghostel*" web)))
           (asked nil)
           (sent nil))
      (switch-to-buffer file)
      (ghostel-dock-tests--keys sent
        (ghostel-dock-tests--answering asked (error "Asked")
          (should (eq (ghostel-dock-paste "make test" web t "test") old))))
      (should-not asked)
      (should (equal (reverse sent) `((,old . "make test") (,old . "return"))))
      (should (eq (window-buffer (ghostel-dock--panel)) old))
      (should (eq (selected-window) (ghostel-dock--panel))))))

(ert-deftest ghostel-dock-test-paste-into-a-busy-terminal-asks ()
  "The last used runs something: nothing is typed into it.  One question:
a new terminal named after the command (the default), the project's
other terminals, the busy one last; the new one gets it at its prompt."
  (ghostel-dock-tests--with
    (let* ((web (ghostel-dock-tests--project "web"))
           (file (ghostel-dock-tests--file web))
           (dev (ghostel-dock-tests--live (ghostel-dock-tests--terminal "*web:dev*" web)))
           (old (ghostel-dock-tests--live (ghostel-dock-tests--terminal "*web-ghostel*" web)))
           (jobs `((,old 202 . "docker-compose up")))
           (asked nil)
           (sent nil))
      (with-current-buffer dev (setq ghostel-dock--name "dev"))
      (switch-to-buffer dev)
      (switch-to-buffer old)
      (switch-to-buffer file)
      (ghostel-dock-tests--jobs jobs
        (ghostel-dock-tests--keys sent
          (ghostel-dock-tests--answering asked default
            (let ((logs (ghostel-dock-paste "docker compose logs -f backend" web t "logs")))
              (should (equal (buffer-name logs) "*web:logs*"))
              (should (equal (car asked)
                             '("shell is running docker-compose up; run it in (default logs): "
                               ("logs" "dev" "shell")
                               ("  new terminal"
                                "  idle  *web:dev*"
                                "  ▶ docker-compose up · paste here anyway  *web-ghostel*")
                               "logs")))
              (should (eq (window-buffer (ghostel-dock--panel)) logs))
              (should (eq (selected-window) (ghostel-dock--panel)))
              ;; A new shell prints first: it waits for the prompt.
              (should-not sent)
              (ghostel-dock-tests--quiet logs)
              (ghostel-dock--poll-waiters)
              (should (equal (reverse sent) `((,logs . "docker compose logs -f backend")
                                              (,logs . "return"))))
              ;; A paste, not a binding: nothing kept of it.
              (should-not (buffer-local-value 'ghostel-dock--command logs))
              ;; Asked again, the name is taken: logs-2, and logs is a choice.
              (setq asked nil sent nil)
              (select-window (get-buffer-window file))
              (switch-to-buffer old)
              (switch-to-buffer file)
              (ghostel-dock-paste "docker compose logs -f backend" web nil "logs")
              (should (equal (nth 1 (car asked)) '("logs-2" "logs" "dev" "shell")))
              (should (string-prefix-p "shell is running docker-compose up; paste it in (default logs-2)"
                                       (car (car asked))))
              (should (get-buffer "*web:logs-2*")))))))))

(ert-deftest ghostel-dock-test-paste-into-a-busy-terminal-other-choices ()
  "Asked where: another terminal takes it at once, the busy one takes it
anyway, a name never seen starts that terminal."
  (ghostel-dock-tests--with
    (let* ((web (ghostel-dock-tests--project "web"))
           (file (ghostel-dock-tests--file web))
           (dev (ghostel-dock-tests--live (ghostel-dock-tests--terminal "*web:dev*" web)))
           (old (ghostel-dock-tests--live (ghostel-dock-tests--terminal "*web-ghostel*" web)))
           (jobs `((,old 202 . "docker-compose up")))
           (answer nil)
           (asked nil)
           (sent nil))
      (with-current-buffer dev (setq ghostel-dock--name "dev"))
      (switch-to-buffer old)
      (switch-to-buffer file)
      (ghostel-dock-tests--jobs jobs
        (ghostel-dock-tests--keys sent
          (ghostel-dock-tests--answering asked answer
            (setq answer "dev")
            (should (eq (ghostel-dock-paste "ls" web nil "ls") dev))
            (should (equal sent `((,dev . "ls"))))
            (should (eq (window-buffer (ghostel-dock--panel)) dev))
            ;; dev is the last used now, and idle: no question.
            (setq sent nil asked nil)
            (ghostel-dock-paste "pwd" web nil "pwd")
            (should-not asked)
            (should (equal sent `((,dev . "pwd"))))
            ;; The busy one, by its label: pasted there anyway.
            (select-window (get-buffer-window file))
            (switch-to-buffer old)
            (switch-to-buffer file)
            (setq sent nil answer "shell")
            (should (eq (ghostel-dock-paste "q" web nil "q") old))
            (should (equal sent `((,old . "q"))))
            ;; A name of its own: that terminal, new.
            (select-window (get-buffer-window file))
            (switch-to-buffer old)
            (switch-to-buffer file)
            (setq sent nil answer "scratch")
            (let ((scratch (ghostel-dock-paste "top" web nil "top")))
              (should (equal (buffer-name scratch) "*web:scratch*"))
              (ghostel-dock-tests--quiet scratch)
              (ghostel-dock--poll-waiters)
              (should (equal sent `((,scratch . "top")))))))))))

(ert-deftest ghostel-dock-test-paste-labels-stay-apart ()
  "An old *PROJECT-ghostel* reads shell, unless a terminal is named shell
too: then it goes by its buffer's name.  The new name never takes a
label, a name, or a saved terminal's name."
  (ghostel-dock-tests--with
    (let* ((web (ghostel-dock-tests--project "web"))
           (shell (ghostel-dock-tests--live (ghostel-dock-tests--terminal "*web:shell*" web)))
           (old (ghostel-dock-tests--live (ghostel-dock-tests--terminal "*web-ghostel*" web)))
           (jobs `((,old 202 . "docker-compose up")))
           (asked nil)
           (sent nil))
      (with-current-buffer shell (setq ghostel-dock--name "shell"))
      (ghostel-dock-tests--write-save `((,web (:name "shell-3" :dir ,web))))
      (switch-to-buffer old)
      (should (equal (mapcar #'car (ghostel-dock--choices web)) '("*web-ghostel*" "shell")))
      (ghostel-dock-tests--jobs jobs
        (ghostel-dock-tests--keys sent
          (ghostel-dock-tests--answering asked default
            (ghostel-dock-paste "ls" web nil "shell"))))
      (should (equal (nth 1 (car asked)) '("shell-2" "shell" "*web-ghostel*")))
      (should-not sent))))

(ert-deftest ghostel-dock-test-ghostel-cmd-unbound-goes-through-the-dock ()
  "ghostel-cmd's unbound RET: the footer tells, a busy target asks, the
row's name the default; an idle one takes it as before."
  (ghostel-dock-tests--with
    (let* ((web (ghostel-dock-tests--project "web"))
           (file (ghostel-dock-tests--file web))
           (old (ghostel-dock-tests--live (ghostel-dock-tests--terminal "*web-ghostel*" web)))
           (jobs nil)
           (asked nil)
           (sent nil)
           (ghostel-cmd--targets nil)
           (ghostel-cmd--root web))
      (switch-to-buffer old)
      (switch-to-buffer file)
      (ghostel-dock-tests--jobs jobs
        (ghostel-dock-tests--keys sent
          (ghostel-dock-tests--answering asked default
            (should (equal (ghostel-cmd--target-for '(:name "logs" :cmd "docker compose logs") web)
                           "RET → shell"))
            (should (equal (ghostel-cmd--target-for '(:name "s" :cmd "x" :terminal "dev") web)
                           "RET → dev (new)"))
            (funcall (ghostel-cmd--deliver-to nil "logs") "docker compose logs" nil)
            (should-not asked)
            (should (equal sent `((,old . "docker compose logs"))))
            (setq sent nil ghostel-cmd--targets nil)
            (select-window (get-buffer-window file))
            (switch-to-buffer old)
            (switch-to-buffer file)
            (push (cons old '(202 . "docker-compose up")) jobs)
            (should (equal (ghostel-cmd--target-for '(:name "logs" :cmd "docker compose logs") web)
                           "RET → shell ▶ docker-compose up: asks"))
            (funcall (ghostel-cmd--deliver-to nil "logs") "docker compose logs" t)
            (should (equal (nth 3 (car asked)) "logs"))
            (should-not sent)
            (let ((logs (get-buffer "*web:logs*")))
              (should logs)
              (ghostel-dock-tests--quiet logs)
              (ghostel-dock--poll-waiters)
              (should (equal (reverse sent) `((,logs . "docker compose logs")
                                              (,logs . "return")))))))))))


;;; Phase 2: after a restart

(defun ghostel-dock-tests--read-save ()
  "The save file's data."
  (with-temp-buffer
    (insert-file-contents ghostel-dock-save-file)
    (read (current-buffer))))

(ert-deftest ghostel-dock-test-save-file-holds-kept-terminals-only ()
  (ghostel-dock-tests--with
    (let* ((web (ghostel-dock-tests--project "web"))
           (src (file-name-as-directory (expand-file-name "src" web)))
           (shell (ghostel-dock--create web "shell"))
           (dev (ghostel-dock--create web "dev"))
           (gone (ghostel-dock--create web "scratch")))
      (ignore shell)
      (make-directory src)
      (ghostel-dock-tests--terminal "*web-ghostel*" web)
      (with-current-buffer dev
        (setq default-directory src
              ghostel-dock--command "npm run dev"
              ghostel-dock--start t))
      (with-current-buffer gone (setq ghostel-dock--forgotten t))
      (ghostel-dock--save)
      (let* ((data (ghostel-dock-tests--read-save))
             (entries (cdr (assoc web (plist-get data :terminals)))))
        (should (eql (plist-get data :version) 1))
        (should (equal (sort (mapcar (lambda (e) (plist-get e :name)) entries) #'string<)
                       '("dev" "shell")))
        (should (equal (seq-find (lambda (e) (equal (plist-get e :name) "dev")) entries)
                       `(:name "dev" :dir ,src :command "npm run dev" :start t)))
        (should (equal (seq-find (lambda (e) (equal (plist-get e :name) "shell")) entries)
                       `(:name "shell" :dir ,web))))
      ;; Killed: gone from the file.
      (let ((kill-buffer-query-functions nil)) (kill-buffer dev))
      (ghostel-dock--save)
      (should (equal (mapcar (lambda (e) (plist-get e :name))
                             (cdr (assoc web (plist-get (ghostel-dock-tests--read-save)
                                                        :terminals))))
                     '("shell"))))))

(defun ghostel-dock-tests--write-save (data)
  "Write DATA as the save file, as a past session would have."
  (with-temp-file ghostel-dock-save-file
    (prin1 (list :version 1 :terminals data) (current-buffer))))

(ert-deftest ghostel-dock-test-restore-brings-them-back-once ()
  "The first SPC ' in a project starts its saved terminals in their
directories; only the one marked to start runs its command."
  (ghostel-dock-tests--with
    (let* ((web (ghostel-dock-tests--project "web"))
           (src (file-name-as-directory (expand-file-name "src" web)))
           (file (ghostel-dock-tests--file web))
           (sent nil))
      (make-directory src)
      (ghostel-dock-tests--write-save
       `((,web (:name "dev" :dir ,src :command "npm run dev" :start t)
               (:name "test" :dir ,web :command "npm test")
               (:name "shell" :dir "/no/such/dir/"))))
      (switch-to-buffer file)
      (ghostel-dock-tests--keys sent
        (ghostel-dock-toggle)
        (let ((dev (get-buffer "*web:dev*"))
              (test (get-buffer "*web:test*"))
              (shell (get-buffer "*web:shell*")))
          (should (and dev test shell))
          ;; The one used last before the restart shows.
          (should (eq (window-buffer (ghostel-dock--panel)) dev))
          (should (equal (buffer-local-value 'default-directory dev) src))
          (should (equal (buffer-local-value 'default-directory shell) web))
          (should (equal (buffer-local-value 'ghostel-dock--command test) "npm test"))
          (should-not (buffer-local-value 'ghostel-dock--start test))
          (should-not sent)
          (mapc #'ghostel-dock-tests--quiet (list dev test shell))
          (ghostel-dock--poll-waiters)
          (should (equal (reverse sent) `((,dev . "npm run dev") (,dev . "return"))))
          ;; Killed after, they do not come back this session.
          (let ((kill-buffer-query-functions nil)) (kill-buffer test))
          (ghostel-dock-toggle)
          (ghostel-dock-toggle)
          (should-not (get-buffer "*web:test*")))))))

(ert-deftest ghostel-dock-test-save-keeps-projects-not-back-yet ()
  "A project whose terminals have not come back this session keeps its
saved ones: they are not gone, only not started."
  (ghostel-dock-tests--with
    (let* ((web (ghostel-dock-tests--project "web"))
           (api (ghostel-dock-tests--project "api")))
      (ghostel-dock-tests--write-save
       `((,api (:name "server" :dir ,api :command "make run" :start t))))
      (let ((default-directory web))
        (ghostel-dock-terminal nil t))
      (ghostel-dock--save)
      (let ((data (plist-get (ghostel-dock-tests--read-save) :terminals)))
        (should (equal (cdr (assoc api data))
                       `((:name "server" :dir ,api :command "make run" :start t))))
        (should (equal (mapcar (lambda (e) (plist-get e :name)) (cdr (assoc web data)))
                       '("shell")))))))

(ert-deftest ghostel-dock-test-broken-save-file-is-left-alone ()
  (ghostel-dock-tests--with
    (let ((web (ghostel-dock-tests--project "web")))
      (with-temp-file ghostel-dock-save-file (insert "(:version 1 :terminals ((\"/x/\" "))
      (let ((default-directory web))
        (ghostel-dock-terminal nil t))
      (ghostel-dock--save)
      (should (equal (with-temp-buffer (insert-file-contents ghostel-dock-save-file)
                                       (buffer-string))
                     "(:version 1 :terminals ((\"/x/\" ")))))

(ert-deftest ghostel-dock-test-manager-start-and-forget ()
  (ghostel-dock-tests--with
    (let* ((web (ghostel-dock-tests--project "web"))
           (file (ghostel-dock-tests--file web))
           (dev (ghostel-dock--create web "dev"))
           (shell (ghostel-dock--create web "shell")))
      (with-current-buffer dev (setq ghostel-dock--command "npm run dev"))
      (switch-to-buffer dev)
      (switch-to-buffer file)
      (ghostel-dock-manager)
      (with-current-buffer ghostel-dock--buffer-name
        (ghostel-dock--select (cl-position dev ghostel-dock--matched :key #'car))
        (execute-kbd-macro (kbd "S"))
        (should (buffer-local-value 'ghostel-dock--start dev))
        (should (string-match-p "dev ↻" (buffer-substring-no-properties
                                         (point-min) ghostel-dock--preview-start)))
        (ghostel-dock--select (cl-position shell ghostel-dock--matched :key #'car))
        (should-error (ghostel-dock-manager-start) :type 'user-error)
        (execute-kbd-macro (kbd "F"))
        (should (buffer-local-value 'ghostel-dock--forgotten shell)))
      (ghostel-dock--save)
      (should (equal (cdr (assoc web (plist-get (ghostel-dock-tests--read-save) :terminals)))
                     `((:name "dev" :dir ,web :command "npm run dev" :start t)))))))


;;; Phase 2: the last command line, the kill guard

(ert-deftest ghostel-dock-test-last-command-from-the-screen ()
  "With OSC 133 the shell marks prompt and input; the last command line is
the input at the prompt before the last -- or at the last, while it runs."
  (ghostel-dock-tests--with
    (let ((buffer (ghostel-dock-tests--terminal "*web:shell*" tmp)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (insert (propertize "jing:~$ " 'ghostel-prompt t)
                  (propertize "make test" 'ghostel-input t) "\n"
                  "ok  12 tests\n"
                  (propertize "jing:~$ " 'ghostel-prompt t)
                  (propertize "ls -l" 'ghostel-input t)))
        (setq ghostel-dock--osc133 t))
      (should (equal (ghostel-dock--screen-command buffer) "make test"))
      (with-current-buffer buffer (setq ghostel--command-running t))
      (should (equal (ghostel-dock--screen-command buffer) "ls -l"))
      (with-current-buffer buffer (setq ghostel--pending-redraw t))
      (should-not (ghostel-dock--screen-command buffer)))))

(ert-deftest ghostel-dock-test-last-command-from-the-process ()
  (ghostel-dock-tests--with
    (let ((buffer (ghostel-dock-tests--live (ghostel-dock-tests--terminal "*web:shell*" tmp)))
          (table '((100 (tpgid . 200) (comm . "login"))
                   (200 (comm . "node") (args . "node /x/bin/vite --port 5173")))))
      (with-current-buffer buffer (setq ghostel--pid 100))
      (cl-letf (((symbol-function 'process-attributes) (lambda (pid) (cdr (assq pid table)))))
        (should (equal (plist-get (ghostel-dock--info buffer) :last) "node vite --port 5173"))
        ;; Ended: it stays the last one seen.
        (setf (alist-get 100 table) '((tpgid . 100) (comm . "login")))
        (let ((info (ghostel-dock--info buffer)))
          (should-not (plist-get info :job))
          (should (equal (plist-get info :last) "node vite --port 5173")))))))

(ert-deftest ghostel-dock-test-kill-guard-is-ghostel-s-with-osc133 ()
  "A shell giving OSC 133: ghostel's own query decides.  Without: the dock's."
  (ghostel-dock-tests--with-manager
    (let ((asked nil)
          (ghostel-query-before-killing 'auto))
      (ghostel-dock--select (cl-position dev ghostel-dock--matched :key #'car))
      (with-current-buffer dev
        (setq ghostel-dock--osc133 t
              ghostel--command-running t)
        (add-hook 'kill-buffer-query-functions #'ghostel--kill-buffer-query nil t))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (prompt) (push prompt asked) nil)))
        (execute-kbd-macro (kbd "x")))
      (should (buffer-live-p dev))
      (should (equal asked '("Buffer \"*web:dev*\" has a running process; kill it? ")))
      ;; Idle with OSC 133: no question from anyone.
      (with-current-buffer dev (setq ghostel--command-running nil))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) (error "Asked"))))
        (ghostel-dock--select (cl-position dev ghostel-dock--matched :key #'car))
        (execute-kbd-macro (kbd "x")))
      (should-not (buffer-live-p dev)))))

;;; Phase 3: other projects' saved terminals in the manager

(defmacro ghostel-dock-tests--with-saved (&rest body)
  "BODY in the manager opened from web's file, with api's terminals saved
from a past session: server (bound, starts with it), shell, worker.
Binds web, api, file, sent; `sent' collects what goes to terminals."
  (declare (indent 0) (debug t))
  `(ghostel-dock-tests--with
     (let* ((web (ghostel-dock-tests--project "web"))
            (api (ghostel-dock-tests--project "api"))
            (file (ghostel-dock-tests--file web))
            (sent nil))
       (ignore sent)
       (make-directory (expand-file-name "srv" api))
       (ghostel-dock-tests--write-save
        `((,api (:name "server" :dir ,(expand-file-name "srv/" api)
                 :command "make run" :start t)
                (:name "shell" :dir ,api)
                (:name "worker" :dir ,api :command "make work"))))
       (switch-to-buffer file)
       (ghostel-dock-tests--keys sent
         (ghostel-dock-manager)
         (with-current-buffer ghostel-dock--buffer-name
           ,@body)))))

(defun ghostel-dock-tests--goto-row (id)
  "Select the manager's row ID."
  (ghostel-dock--select (cl-position id ghostel-dock--matched :key #'car :test #'equal))
  (should (equal (tabulated-list-get-id) id))
  ;; As after a key: the details follow the row.
  (ghostel-dock--draw-preview))

(defun ghostel-dock-tests--saved-names (root)
  "Names the save file holds for ROOT."
  (mapcar (lambda (entry) (plist-get entry :name))
          (cdr (assoc root (plist-get (ghostel-dock-tests--read-save) :terminals)))))

(ert-deftest ghostel-dock-test-manager-lists-saved-terminals-dimmed ()
  (ghostel-dock-tests--with-saved
    (let ((shell (get-buffer "*web:shell*")))
      ;; This project's own: started, as before (here: none saved, a shell).
      (should-not (get-buffer "*api:server*"))
      (should (equal ghostel-dock--rows
                     (append (delq nil (list shell))
                             (list (ghostel-dock--saved-key api "server")
                                   (ghostel-dock--saved-key api "shell")
                                   (ghostel-dock--saved-key api "worker")))))
      (let ((lines (ghostel-dock-tests--manager-lines)))
        (should (string-match-p "\\` api +server ↻ +saved +\\./srv" (nth 0 lines)))
        (should (string-match-p "\\` +worker +saved" (nth 2 lines))))
      (ghostel-dock-tests--goto-row (ghostel-dock--saved-key api "server"))
      (let ((detail (buffer-substring-no-properties ghostel-dock--preview-start (point-max))))
        (should (string-match-p "\\*api:server\\*  saved, not open" detail))
        (should (string-match-p "RET starts it in /" detail))
        (should (equal (plist-get (gethash (tabulated-list-get-id) ghostel-dock--infos) :dir)
                       (expand-file-name "srv/" api)))
        (should (string-match-p "starts with: make run" detail)))
      (ghostel-dock-tests--goto-row (ghostel-dock--saved-key api "worker"))
      (should (string-match-p "bound: make work · opens its shell only"
                              (buffer-substring-no-properties ghostel-dock--preview-start (point-max))))
      ;; The filter takes saved rows too.
      (execute-kbd-macro (kbd "/ w o r k RET"))
      (should (equal (mapcar #'car ghostel-dock--matched)
                     (list (ghostel-dock--saved-key api "worker")))))))

(ert-deftest ghostel-dock-test-manager-ret-brings-back-that-one-only ()
  "RET on a saved row starts that terminal alone, in its directory, in its
project's layer; one marked S runs its command.  The others stay saved,
listed, and in the file -- a later SPC ' there does not start them."
  (ghostel-dock-tests--with-saved
    (let ((switched nil))
      (let ((ghostel-dock-switch-layer-function (lambda (root) (push root switched))))
        (ghostel-dock-tests--goto-row (ghostel-dock--saved-key api "server"))
        (execute-kbd-macro (kbd "RET")))
      (let ((server (get-buffer "*api:server*")))
        (should server)
        (should (equal switched (list api)))
        (should (eq (window-buffer (ghostel-dock--panel)) server))
        (should (eq (selected-window) (ghostel-dock--panel)))
        (should (equal (buffer-local-value 'default-directory server) (expand-file-name "srv/" api)))
        (should-not (get-buffer "*api:shell*"))
        (ghostel-dock-tests--quiet server)
        (ghostel-dock--poll-waiters)
        (should (equal (reverse sent) `((,server . "make run") (,server . "return"))))
        (should (equal (sort (ghostel-dock-tests--saved-names api) #'string<)
                       '("server" "shell" "worker")))
        ;; SPC ' in api now: the panel, not the rest.
        (let ((default-directory api))
          (ghostel-dock-toggle)
          (ghostel-dock-toggle))
        (should-not (get-buffer "*api:shell*"))
        ;; Killed, it is gone from the file; the others stay.
        (let ((kill-buffer-query-functions nil)) (kill-buffer server))
        (ghostel-dock--save)
        (should (equal (sort (ghostel-dock-tests--saved-names api) #'string<)
                       '("shell" "worker")))))))

(ert-deftest ghostel-dock-test-manager-o-on-saved-opens-it-here ()
  (ghostel-dock-tests--with-saved
    (ghostel-dock-tests--goto-row (ghostel-dock--saved-key api "shell"))
    (execute-kbd-macro (kbd "o"))
    (should (eq (window-buffer (selected-window)) (get-buffer "*api:shell*")))
    (should-not (ghostel-dock--panel))))

(ert-deftest ghostel-dock-test-manager-saved-row-keys ()
  "x drops (asked), F forgets (again: keeps), r renames, S toggles the
start, R refuses -- each in the file."
  (ghostel-dock-tests--with-saved
    ;; R: no shell to rerun in.
    (ghostel-dock-tests--goto-row (ghostel-dock--saved-key api "shell"))
    (should-error (ghostel-dock-manager-rerun) :type 'user-error)
    ;; r
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "worker")))
      (should-error (ghostel-dock-manager-rename) :type 'user-error))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "repl")))
      (execute-kbd-macro (kbd "r")))
    (should (equal (tabulated-list-get-id) (ghostel-dock--saved-key api "repl")))
    (should (member "repl" (ghostel-dock-tests--saved-names api)))
    ;; S
    (should-error (ghostel-dock-manager-start) :type 'user-error)
    (ghostel-dock-tests--goto-row (ghostel-dock--saved-key api "worker"))
    (execute-kbd-macro (kbd "S"))
    (should (plist-get (ghostel-dock--saved-entry api "worker") :start))
    (should (string-match-p "worker ↻" (ghostel-dock-tests--line-of-row)))
    ;; F, and F again
    (execute-kbd-macro (kbd "F"))
    (should-not (member "worker" (ghostel-dock-tests--saved-names api)))
    (should (equal (tabulated-list-get-id) (ghostel-dock--saved-key api "worker")))
    (execute-kbd-macro (kbd "F"))
    (should (member "worker" (ghostel-dock-tests--saved-names api)))
    (should (equal (seq-find (lambda (e) (equal (plist-get e :name) "worker"))
                             (cdr (assoc api (plist-get (ghostel-dock-tests--read-save) :terminals))))
                   `(:name "worker" :dir ,api :command "make work" :start t)))
    ;; x: asked; no keeps it
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
      (execute-kbd-macro (kbd "x")))
    (should (member "worker" (ghostel-dock-tests--saved-names api)))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
      (execute-kbd-macro (kbd "x")))
    (should-not (member "worker" (ghostel-dock-tests--saved-names api)))
    (should-not (member (ghostel-dock--saved-key api "worker") ghostel-dock--rows))))

(defun ghostel-dock-tests--line-of-row ()
  "The selected row as printed."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(ert-deftest ghostel-dock-test-manager-missing-project ()
  "A saved project whose root is gone shows missing; RET refuses, x drops,
and nothing makes the directory."
  (ghostel-dock-tests--with
    (let* ((web (ghostel-dock-tests--project "web"))
           (gone (file-name-as-directory (expand-file-name "gone" tmp)))
           (file (ghostel-dock-tests--file web)))
      (ghostel-dock-tests--write-save `((,gone (:name "shell" :dir ,gone))))
      (switch-to-buffer file)
      (ghostel-dock-manager)
      (with-current-buffer ghostel-dock--buffer-name
        (ghostel-dock-tests--goto-row (ghostel-dock--saved-key gone "shell"))
        (should (string-match-p "\\` gone +shell +missing" (ghostel-dock-tests--line-of-row)))
        (should-error (ghostel-dock-manager-open) :type 'user-error)
        (should-error (ghostel-dock-manager-new) :type 'user-error)
        (should-not (file-exists-p gone))
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
          (execute-kbd-macro (kbd "x")))
        (should-not (assoc gone (plist-get (ghostel-dock-tests--read-save) :terminals)))
        (should-not (file-exists-p gone))))))

(ert-deftest ghostel-dock-test-run-in-a-picked-project-uses-the-saved-one ()
  "A bound command for a saved terminal of a project picked from: that
terminal comes back as saved (its directory), and only the bound command
runs -- not its own start command as well."
  (ghostel-dock-tests--with-saved
    (ghostel-dock-tests--goto-row (ghostel-dock--saved-key api "shell"))
    (execute-kbd-macro (kbd "RET"))
    (setq sent nil)
    (let ((server (ghostel-dock-run "make test" "server" api t)))
      (should (equal (buffer-local-value 'default-directory server) (expand-file-name "srv/" api)))
      (ghostel-dock-tests--quiet server)
      (ghostel-dock--poll-waiters)
      (should (equal (reverse sent) `((,server . "make test") (,server . "return")))))))

(ert-deftest ghostel-dock-test-manager-open-with-broken-file ()
  "A save file that does not read: no saved rows, the file untouched."
  (ghostel-dock-tests--with
    (let ((web (ghostel-dock-tests--project "web")))
      (with-temp-file ghostel-dock-save-file (insert "(:version 1 :terminals ((\"/x/\" "))
      (switch-to-buffer (ghostel-dock-tests--file web))
      (ghostel-dock-manager)
      (with-current-buffer ghostel-dock--buffer-name
        (should-not (seq-some #'ghostel-dock--saved-key-p ghostel-dock--rows)))
      (ghostel-dock--save)
      (should (equal (with-temp-buffer (insert-file-contents ghostel-dock-save-file)
                                       (buffer-string))
                     "(:version 1 :terminals ((\"/x/\" ")))))

(ert-deftest ghostel-dock-test-restore-failure-loses-nothing ()
  "A saved terminal that does not start stays saved, listed; the project
counts as picked, so the file keeps it."
  (ghostel-dock-tests--with
    (let* ((web (ghostel-dock-tests--project "web"))
           (file (ghostel-dock-tests--file web))
           (real (symbol-function 'ghostel)))
      (ghostel-dock-tests--write-save
       `((,web (:name "dev" :dir ,web) (:name "bad" :dir ,web) (:name "shell" :dir ,web))))
      (switch-to-buffer file)
      (cl-letf (((symbol-function 'ghostel)
                 (lambda (&rest args)
                   (if (string-match-p "bad" ghostel-buffer-name)
                       (error "No shell")
                     (apply real args)))))
        (ghostel-dock-toggle))
      (should (get-buffer "*web:dev*"))
      (should (get-buffer "*web:shell*"))
      (should-not (get-buffer "*web:bad*"))
      (ghostel-dock--save)
      (should (equal (sort (ghostel-dock-tests--saved-names web) #'string<)
                     '("bad" "dev" "shell")))
      (should (member (ghostel-dock--saved-key web "bad") (ghostel-dock--saved-keys))))))

(ert-deftest ghostel-dock-test-foreground-ignores-emacs-own-group ()
  "A shell just forked still reports the terminal it came from: in a
terminal Emacs, Emacs's own group holds that foreground.  Not a job."
  (ghostel-dock-tests--with
    (let* ((buffer (ghostel-dock-tests--terminal "*web:shell*" tmp))
           (ghostel-dock--emacs-pgrp 4242)
           (table '((100 (tpgid . 4242) (comm . "bash"))
                    (4242 (comm . "emacs") (args . "emacs -nw")))))
      (with-current-buffer buffer (setq ghostel--pid 100))
      (cl-letf (((symbol-function 'process-attributes) (lambda (pid) (cdr (assq pid table)))))
        (should-not (ghostel-dock--foreground buffer))))))

(ert-deftest ghostel-dock-test-run-a-new-terminal-never-asks ()
  "A terminal the run just started waits for its prompt, whatever a look
at it says meanwhile."
  (ghostel-dock-tests--with
    (let ((web (ghostel-dock-tests--project "web"))
          (sent nil))
      (ghostel-dock-tests--keys sent
        (cl-letf (((symbol-function 'ghostel-dock--busy-p) (lambda (_) t))
                  ((symbol-function 'y-or-n-p) (lambda (&rest _) (error "Asked"))))
          (let ((dev (ghostel-dock-run "make run" "dev" web t)))
            (ghostel-dock-tests--quiet dev)
            (cl-letf (((symbol-function 'ghostel-dock--foreground) #'ignore))
              (ghostel-dock--poll-waiters))
            (should (equal (car sent) (cons dev "return")))))))))

;;; A command finished out of sight

(defun ghostel-dock-tests--ran (buffer secs status)
  "BUFFER's shell ran a command SECS long that ended with STATUS: its
OSC 133 C, then its D."
  (run-hook-with-args 'ghostel-command-start-functions buffer)
  (with-current-buffer buffer
    (setq ghostel-dock--command-at (- (float-time) secs)))
  (run-hook-with-args 'ghostel-command-finish-functions buffer status))

(defmacro ghostel-dock-tests--messages (var &rest body)
  "BODY with what goes to the echo area recorded in VAR, newest first."
  (declare (indent 1) (debug t))
  `(cl-letf (((symbol-function 'message)
              (lambda (format &rest args)
                (when format (push (apply #'format-message format args) ,var)))))
     ,@body))

(defvar ghostel-dock-tests--muted nil
  "Echo-area text a test does not look at.")

(ert-deftest ghostel-dock-test-finish-texts ()
  (should (equal (ghostel-dock--duration 45.7) "45s"))
  (should (equal (ghostel-dock--duration 192) "3m 12s"))
  (should (equal (ghostel-dock--duration 3900) "1h 5m"))
  (should (equal (substring-no-properties (ghostel-dock--finish-words 1 192))
                 "finished ✗ 1 · 3m 12s"))
  (should (equal (substring-no-properties (ghostel-dock--finish-words 0 45)) "finished ✓ · 45s"))
  (should (equal (ghostel-dock--finish-words nil 45) "finished · 45s"))
  (should (equal (substring-no-properties
                  (ghostel-dock--finished-text '(:status 1 :secs 192 :at 100) 220))
                 "● not looked at: finished ✗ 1 · 3m 12s, 2m ago"))
  (should (equal (ghostel-dock--finished-dot '(:finished nil)) ""))
  (should (eq (get-text-property 0 'face (ghostel-dock--finished-dot '(:finished (:status 0))))
              'success)))

(ert-deftest ghostel-dock-test-finish-told-out-of-sight ()
  "A command that ran long and finishes where no window shows it says so
in the echo area, and marks its terminal."
  (ghostel-dock-tests--with
    (let* ((web (ghostel-dock-tests--project "web"))
           (test (ghostel-dock-tests--live (ghostel-dock-tests--terminal "*web:test*" web)))
           (said nil))
      (ghostel-dock-tests--messages said
        (ghostel-dock-tests--ran test 192 1))
      (should (equal said '("*web:test* finished ✗ 1 · 3m 12s")))
      (let ((finished (buffer-local-value 'ghostel-dock--finished test)))
        (should (eql (plist-get finished :status) 1))
        (should (>= (plist-get finished :secs) 192))
        (should (< (- (float-time) (plist-get finished :at)) 5)))
      (should (equal ghostel-dock--unseen (list test)))
      (should (plist-get (ghostel-dock--info test) :finished))
      ;; The time runs from the first C: `make && make test' sends two.
      (run-hook-with-args 'ghostel-command-start-functions test)
      (with-current-buffer test (setq ghostel-dock--command-at (- (float-time) 45)))
      (run-hook-with-args 'ghostel-command-start-functions test)
      (setq said nil)
      (ghostel-dock-tests--messages said
        (run-hook-with-args 'ghostel-command-finish-functions test 0))
      (should (equal said '("*web:test* finished ✓ · 45s")))
      ;; The newest finish is the one marked.
      (should (eql (plist-get (buffer-local-value 'ghostel-dock--finished test) :status) 0)))))

(ert-deftest ghostel-dock-test-finish-quiet-when-seen-quick-or-not-a-terminal ()
  "Nothing is told of a finish on screen, a quick one, a prompt redrawn,
an agent's buffer, or with `ghostel-dock-notify-after' nil."
  (ghostel-dock-tests--with
    (let* ((web (ghostel-dock-tests--project "web"))
           (shown (ghostel-dock-tests--live (ghostel-dock-tests--terminal "*web:shown*" web)))
           (hidden (ghostel-dock-tests--live (ghostel-dock-tests--terminal "*web:hidden*" web)))
           (agent (ghostel-dock-tests--live (ghostel-dock-tests--terminal "*ghostherd:web*" web)))
           (said nil))
      (with-current-buffer agent (setq-local ghostherd-session-id "abc"))
      (ghostel-dock-show shown)
      (ghostel-dock-tests--messages said
        ;; On screen: seen finishing.
        (ghostel-dock-tests--ran shown 300 2)
        ;; Quick.
        (ghostel-dock-tests--ran hidden 9 1)
        ;; A prompt redrawn: a D with no C.
        (run-hook-with-args 'ghostel-command-finish-functions hidden 1)
        ;; Not a terminal of the dock's.
        (ghostel-dock-tests--ran agent 300 0)
        (let ((ghostel-dock-notify-after nil))
          (ghostel-dock-tests--ran hidden 300 0)))
      (should-not said)
      (should-not ghostel-dock--unseen)
      (dolist (buffer (list shown hidden agent))
        (should-not (buffer-local-value 'ghostel-dock--finished buffer)))
      ;; The status is kept all the same.
      (should (eql (buffer-local-value 'ghostel-dock--status shown) 2)))))

(ert-deftest ghostel-dock-test-finish-quiet-after-the-dock-s-interrupt ()
  "A bound command restarted: the dock's Ctrl-C ends it, and that end is
not told of, even with the panel hidden meanwhile.  The command run
after it is."
  (ghostel-dock-tests--with
    (let* ((web (ghostel-dock-tests--project "web"))
           (file (ghostel-dock-tests--file web))
           (dev (ghostel-dock-tests--live (ghostel-dock-tests--terminal "*web:dev*" web)))
           (said nil)
           (sent nil))
      (switch-to-buffer file)
      (run-hook-with-args 'ghostel-command-start-functions dev)
      (with-current-buffer dev
        (setq ghostel-dock--name "dev"
              ghostel-dock--command "npm run dev"
              ghostel-dock--command-live (float-time)
              ghostel-dock--command-at (- (float-time) 600)
              ghostel--command-running t))
      (ghostel-dock-tests--keys sent
        (ghostel-dock-tests--messages said
          (ghostel-dock-run "npm run dev" "dev" web t)
          (should (equal sent `((,dev . C-c))))
          ;; Hidden before the old one stops.
          (ghostel-dock-hide)
          (with-current-buffer dev (setq ghostel--command-running nil))
          (run-hook-with-args 'ghostel-command-finish-functions dev 130)
          (should-not (cl-remove-if (lambda (text) (string-prefix-p "Restarting" text)) said))
          (should-not (buffer-local-value 'ghostel-dock--hush dev))
          (ghostel-dock--poll-waiters)
          (should (equal (car sent) (cons dev "return")))
          ;; The new run, out of sight, ends: that is news.
          (ghostel-dock-tests--ran dev 30 1)))
      (should (equal (car said) "*web:dev* finished ✗ 1 · 30s")))))

(ert-deftest ghostel-dock-test-finish-mark-clears-when-shown ()
  "The mark stays until the terminal shows: in the panel, in a window, the
manager opening it.  A preview while the manager is up does not count."
  (ghostel-dock-tests--with-manager
    (ghostel-dock-tests--messages ghostel-dock-tests--muted
      (ghostel-dock-tests--ran test 60 1))
    (ghostel-dock--refresh t)
    ;; The row: a dot before its name; the details: what finished.
    (let ((lines (ghostel-dock-tests--manager-lines)))
      (should (string-match-p "\\` +● test +idle +✗ 1" (nth 2 lines))))
    (ghostel-dock-tests--goto-row test)
    (should (string-match-p "● not looked at: finished ✗ 1 · 1m 0s, just now"
                            (buffer-substring-no-properties ghostel-dock--preview-start
                                                            (point-max))))
    ;; Previewed while the manager shows: still marked.
    (ghostel-dock--preview)
    (should (eq (window-buffer (ghostel-dock--panel)) test))
    (ghostel-dock--note-seen)
    (should (buffer-local-value 'ghostel-dock--finished test))
    ;; q: the panel goes back; the terminal never showed for real.
    (execute-kbd-macro (kbd "q"))
    (ghostel-dock--note-seen)
    (should (buffer-local-value 'ghostel-dock--finished test))
    ;; Shown in a window by other means: seen.
    (with-selected-window (get-buffer-window api-file)
      (switch-to-buffer test))
    (ghostel-dock--note-seen)
    (should-not (buffer-local-value 'ghostel-dock--finished test))
    (should-not ghostel-dock--unseen)
    (should (memq #'ghostel-dock--note-seen (default-value 'window-buffer-change-functions)))
    (should (memq #'ghostel-dock--note-seen
                  (default-value 'window-selection-change-functions)))))

(ert-deftest ghostel-dock-test-finish-mark-clears-in-the-panel ()
  "Showing the terminal in the panel clears its mark at once; RET in the
manager too.  A killed one leaves the list."
  (ghostel-dock-tests--with
    (let* ((web (ghostel-dock-tests--project "web"))
           (file (ghostel-dock-tests--file web))
           (a (ghostel-dock-tests--live (ghostel-dock-tests--terminal "*web:a*" web)))
           (b (ghostel-dock-tests--live (ghostel-dock-tests--terminal "*web:b*" web)))
           (c (ghostel-dock-tests--live (ghostel-dock-tests--terminal "*web:c*" web))))
      (switch-to-buffer file)
      (ghostel-dock-tests--messages ghostel-dock-tests--muted
        (dolist (buffer (list a b c))
          (ghostel-dock-tests--ran buffer 20 0)))
      (should (= 3 (length ghostel-dock--unseen)))
      (ghostel-dock-show a)
      (should-not (buffer-local-value 'ghostel-dock--finished a))
      (should-not (memq a ghostel-dock--unseen))
      (ghostel-dock-hide)
      (ghostel-dock-manager)
      (with-current-buffer ghostel-dock--buffer-name
        (ghostel-dock-tests--goto-row b)
        (execute-kbd-macro (kbd "RET")))
      (should (eq (window-buffer (ghostel-dock--panel)) b))
      (should-not (buffer-local-value 'ghostel-dock--finished b))
      (let ((kill-buffer-query-functions nil)
            (ghostel-query-before-killing nil))
        (kill-buffer c))
      (ghostel-dock--note-seen)
      (should-not ghostel-dock--unseen))))

(ert-deftest ghostel-dock-test-finish-told-outside-when-unfocused ()
  "With `ghostel-dock-notify-system', a finish is told outside Emacs too
-- only while no Emacs frame has the focus."
  (ghostel-dock-tests--with
    (let* ((web (ghostel-dock-tests--project "web"))
           (test (ghostel-dock-tests--live (ghostel-dock-tests--terminal "*web:test*" web)))
           (told nil)
           (focus nil)
           (ghostel-dock-notify-system (lambda (title body) (push (list title body) told))))
      (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) focus)))
        (ghostel-dock-tests--messages ghostel-dock-tests--muted
          (setq focus t)
          (ghostel-dock-tests--ran test 30 0)
          (should-not told)
          (setq focus 'unknown)
          (ghostel-dock-tests--ran test 30 0)
          (should-not told)
          (setq focus nil)
          (ghostel-dock-tests--ran test 30 2)
          (should (equal told '(("*web:test*" "finished ✗ 2 · 30s"))))
          ;; osascript: the text goes as its argv, never into the script.
          (let ((ghostel-dock-notify-system 'osascript)
                (made nil))
            (cl-letf (((symbol-function 'make-process)
                       (lambda (&rest args) (push (plist-get args :command) made)))
                      ((symbol-function 'executable-find) (lambda (_) "/usr/bin/osascript")))
              (ghostel-dock-tests--ran test 30 0))
            (should (equal (car made)
                           '("osascript" "-e" "on run argv"
                             "-e" "display notification (item 2 of argv) with title (item 1 of argv)"
                             "-e" "end run" "*web:test*" "finished ✓ · 30s")))))))))

(provide 'ghostel-dock-tests)
;;; ghostel-dock-tests.el ends here

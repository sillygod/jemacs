;;; ghostel-dock.el --- Project terminals in a bottom panel, and a manager -*- lexical-binding: t; -*-

;; Author: Jing
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, terminals, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; A project's Ghostel terminals, named (dev, test, shell...) and kept
;; in one place: a bottom side window of fixed height, the panel.
;;
;;   `ghostel-dock-toggle'   shows the panel with the terminal last used
;;                           in this project (a new one when there is
;;                           none), or hides it.  It leaves no split.
;;   `ghostel-dock-manager'  lists the terminals of every project, this
;;                           project's first, in a ghostel-cmd style
;;                           overlay: what each runs, its last exit
;;                           status (OSC 133), its directory and how
;;                           long since its last output.  Moving
;;                           through it previews a terminal in the panel.
;;
;; A terminal is a buffer *PROJECT:NAME*.  It belongs to the directory
;; it was made for -- its project's root, or the directory it started
;; in outside a project -- and keeps belonging to it across `cd'.
;; Terminals made before the dock (`ghostel-project''s *PROJECT-ghostel*,
;; its *PROJECT-ghostel*<2>) are found as their project's too, and keep
;; their names.  A ghostherd agent's buffer is never one: ghostherd
;; marks it with `ghostherd-session-id'.
;;
;; The panel belongs to the window layer it is in.  Wrap what snapshots
;; or puts back a layer's windows in `ghostel-dock-without-panel' (an
;; :around advice) and set `ghostel-dock-layer-function': the layout
;; then never holds the panel, and each layer gets its own panel back.
;; A snapshot that held it would bring back a bottom side window the
;; dock does not know for its panel (its mark is not a persistent
;; parameter), showing whatever buffer the snapshot named; and
;; jworkspace's maximize, which asks `one-window-p', would never see
;; one window.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'project)
(require 'tabulated-list)
(require 'ghostel)
;; The overlay's look and feel: its windowed rows, the rule under them,
;; the posframe's size, the filter's printable keys.
(require 'ghostel-cmd)

(declare-function posframe-show "posframe")
(declare-function posframe-hide "posframe")
(declare-function posframe-poshandler-frame-top-center "posframe")
(declare-function evil-define-key* "evil")
(declare-function evil-make-intercept-map "evil")
(declare-function evil-normalize-keymaps "evil")

(defvar ghostherd-session-id)
(defvar ghostherd-terminal-mode)


;;; Customization

(defgroup ghostel-dock nil
  "Project terminals in a bottom panel."
  :group 'tools
  :prefix "ghostel-dock-")

(defcustom ghostel-dock-height 0.3
  "Height of the panel: a fraction of the frame, or a number of lines."
  :type '(choice (float :tag "Fraction of the frame") (integer :tag "Lines")))

(defcustom ghostel-dock-default-name "shell"
  "Name of a project's first terminal, and the one a new one is offered."
  :type 'string)

(defcustom ghostel-dock-preview-delay 0.15
  "Seconds the manager waits on a row before the panel previews it.
Showing a terminal resizes it, and a full-screen program in it redraws:
moving fast through the list should not strobe each one it passes."
  :type 'number)

(defcustom ghostel-dock-refresh-interval 2
  "Seconds between the manager's redraws while it shows, or nil for never.
What a terminal runs and how long since its output change by themselves."
  :type '(choice (const :tag "Never" nil) number))

(defcustom ghostel-dock-use-posframe t
  "Show the manager in a posframe when possible, else a top side window."
  :type 'boolean)

(defcustom ghostel-dock-shells
  '("bash" "zsh" "sh" "fish" "nu" "dash" "ksh" "mksh" "tcsh" "csh" "login"
    "xonsh" "elvish")
  "Programs that are a terminal's shell: one of them in the foreground is idle."
  :type '(repeat string))

(defcustom ghostel-dock-interpreters
  '("node" "python" "python3" "ruby" "perl" "deno" "bun" "bash" "sh" "zsh")
  "Programs whose script names the command: \"node /x/bin/npm run\" is npm."
  :type '(repeat string))

(defcustom ghostel-dock-layer-function nil
  "Function of no arguments naming the window layer shown now, or nil.
The panel is kept per layer: `ghostel-dock-without-panel' remembers
what the layer it leaves showed in its panel, and gives the next one
its own back.  nil: one layer per frame."
  :type '(choice (const :tag "One per frame" nil) function))

(defcustom ghostel-dock-switch-layer-function nil
  "Function called with a terminal's ROOT before the manager opens it, or nil.
It opens a terminal of another project: switching to that project's
window layer first opens it in that layer's panel."
  :type '(choice (const :tag "Stay in this layer" nil) function))

(defcustom ghostel-dock-after-create-functions nil
  "Functions called with a new terminal and the buffer it was asked from."
  :type 'hook)


;;; A terminal's place

(defvar ghostel-dock--caller nil
  "Where the manager was opened: :root, :buffer, :frame.")

(defvar-local ghostel-dock--root nil
  "Directory this terminal belongs to: its project's root, or where it began.
Set when the dock makes it or first sees it; kept across `cd'.")
(put 'ghostel-dock--root 'permanent-local t)

(defvar-local ghostel-dock--name nil
  "This terminal's name in its project, dev or test; nil for one made before.")
(put 'ghostel-dock--name 'permanent-local t)

(defvar-local ghostel-dock--output-at nil
  "`float-time' of this terminal's last output, nil before the first seen.")

(defvar-local ghostel-dock--status nil
  "Exit status of the last command that ran, from OSC 133; nil unknown.")

(defvar-local ghostel-dock--in-command nil
  "Non-nil between an OSC 133 command start and its finish.")

(defvar-local ghostel-dock--osc133 nil
  "Non-nil once the shell sent an OSC 133 command marker.")

(defconst ghostel-dock--legacy-regexp
  "\\`\\*\\(.+\\)-ghostel\\*\\(?:<[0-9]+>\\)?\\'"
  "What `ghostel-project' names a terminal: *PROJECT-ghostel*, <N> after.")

(defun ghostel-dock--local (symbol buffer)
  "SYMBOL's value in BUFFER, or nil while SYMBOL is unbound."
  (and (buffer-local-boundp symbol buffer)
       (buffer-local-value symbol buffer)))

(defun ghostel-dock--norm (dir)
  "DIR as a root: absolute, ending in a slash."
  (file-name-as-directory (expand-file-name dir)))

(defun ghostel-dock--project-root (dir)
  "Root of DIR's project, or nil.  A remote DIR is never asked: that
would walk its file system over TRAMP."
  (unless (file-remote-p dir)
    (when-let* ((proj (ignore-errors (project-current nil dir))))
      (ghostel-dock--norm (project-root proj)))))

(defun ghostel-dock--root-name (root)
  "How ROOT is named: its last directory, @HOST when remote."
  (let* ((local (or (file-remote-p root 'localname) root))
         (name (file-name-nondirectory (directory-file-name local)))
         (name (if (string-empty-p name) "/" name)))
    (if-let* ((host (file-remote-p root 'host)))
        (format "%s@%s" name host)
      name)))

(defun ghostel-dock--buffer-name (root name)
  "The buffer of ROOT's terminal NAME: *PROJECT:NAME*."
  (format "*%s:%s*" (ghostel-dock--root-name root) name))

(defun ghostel-dock--agent-p (buffer)
  "Non-nil when BUFFER shows a ghostherd agent, which has the herd page.
ghostherd sets `ghostherd-session-id' in every buffer showing an agent,
on either backend; the name is only the last resort."
  (or (ghostel-dock--local 'ghostherd-session-id buffer)
      (ghostel-dock--local 'ghostherd-terminal-mode buffer)
      (string-prefix-p "*ghostherd:" (buffer-name buffer))))

(defun ghostel-dock--terminal-p (buffer)
  "Non-nil when BUFFER is a shell terminal: one `ghostel' made, not an agent's.
`ghostel' gives what it makes an identity; `ghostel-exec' (eshell's
visual commands, compile views) does not."
  (and (buffer-live-p buffer)
       (provided-mode-derived-p (buffer-local-value 'major-mode buffer)
                                'ghostel-mode)
       (ghostel-dock--local 'ghostel--buffer-identity buffer)
       (not (ghostel-dock--agent-p buffer))))

(defun ghostel-dock--known-root (name)
  "The root of a known project named NAME, or nil."
  (seq-some (lambda (root)
              (and (not (file-remote-p root))
                   (equal (ghostel-dock--root-name root) name)
                   (ghostel-dock--norm root)))
            (ignore-errors (project-known-project-roots))))

(defun ghostel-dock--legacy-root ()
  "The root of a terminal the dock did not make, in its buffer.
Its name, *PROJECT-ghostel*, names the project even after a cd out of
it; else the project of its directory, else the directory."
  (let* ((dir (ghostel-dock--norm default-directory))
         (here (ghostel-dock--project-root dir))
         (identity (or ghostel--buffer-identity ""))
         (named (and (string-match ghostel-dock--legacy-regexp identity)
                     (match-string 1 identity))))
    (or (and named here (equal (ghostel-dock--root-name here) named) here)
        (and named (ghostel-dock--known-root named))
        here
        dir)))

(defun ghostel-dock--root-of (buffer)
  "The root terminal BUFFER belongs to.  The first look records it."
  (with-current-buffer buffer
    (or ghostel-dock--root
        (setq ghostel-dock--root (ghostel-dock--legacy-root)))))

(defun ghostel-dock--name-of (buffer)
  "BUFFER's name in its project: NAME of a buffer *PROJECT:NAME*.
A terminal made before the dock goes by its buffer's name."
  (or (buffer-local-value 'ghostel-dock--name buffer)
      (let ((prefix (format "*%s:" (ghostel-dock--root-name (ghostel-dock--root-of buffer))))
            (name (buffer-name buffer)))
        (and (string-prefix-p prefix name)
             (string-suffix-p "*" name)
             (> (length name) (1+ (length prefix)))
             (substring name (length prefix) -1)))
      (buffer-name buffer)))

(defun ghostel-dock--current-root ()
  "The root the current buffer's terminals belong to.
A terminal's own; else the project's root; else `default-directory'."
  (cond
   ((ghostel-dock--terminal-p (current-buffer))
    (ghostel-dock--root-of (current-buffer)))
   ((and (derived-mode-p 'ghostel-dock-manager-mode)
         (plist-get ghostel-dock--caller :root)))
   (t (or (ghostel-dock--project-root default-directory)
          (ghostel-dock--norm default-directory)))))

(defun ghostel-dock-terminals (&optional root)
  "ROOT's terminals, the most recently used first.
ROOT defaults to the current buffer's; t means every terminal."
  (let ((root (if (eq root t) t
                (ghostel-dock--norm (or root (ghostel-dock--current-root))))))
    (seq-filter (lambda (buffer)
                  (and (ghostel-dock--terminal-p buffer)
                       (or (eq root t)
                           (equal (ghostel-dock--root-of buffer) root))))
                (buffer-list))))

(defun ghostel-dock--named (root name)
  "ROOT's terminal NAME, or nil."
  (seq-find (lambda (buffer) (equal (ghostel-dock--name-of buffer) name))
            (ghostel-dock-terminals root)))

(defun ghostel-dock--free-name (root base)
  "BASE, or BASE-2, BASE-3...: the first ROOT has no terminal by."
  (let ((name base) (n 1))
    (while (ghostel-dock--named root name)
      (setq n (1+ n) name (format "%s-%d" base n)))
    name))


;;; What a terminal does

(defun ghostel-dock--on-command-start (buffer)
  "Note that BUFFER's shell started a command (OSC 133 C)."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq ghostel-dock--in-command t
            ghostel-dock--osc133 t))))

(defun ghostel-dock--on-command-finish (buffer status)
  "Note that BUFFER's command finished with STATUS (OSC 133 D).
A prompt redrawn sends D with no C before it, and the status of the
command before: only a D that ends a command is its status."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq ghostel-dock--osc133 t)
      (when ghostel-dock--in-command
        (setq ghostel-dock--status status
              ghostel-dock--in-command nil)))))

(defun ghostel-dock--stamp-output (process &rest _)
  "Note when PROCESS's terminal last gave output.
An :after advice on ghostel's two filters: the Emacs PTY's and the
native PTY's event pipe, which gets an event for every read."
  (when-let* ((buffer (process-buffer process))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (setq ghostel-dock--output-at (float-time)))))

(defun ghostel-dock--track (on)
  "Follow the terminals' commands and output when ON, else stop."
  (dolist (hook '((ghostel-command-start-functions . ghostel-dock--on-command-start)
                  (ghostel-command-finish-functions . ghostel-dock--on-command-finish)))
    (if on (add-hook (car hook) (cdr hook)) (remove-hook (car hook) (cdr hook))))
  (dolist (filter '(ghostel--filter ghostel--events-filter))
    (if on
        (advice-add filter :after #'ghostel-dock--stamp-output)
      (advice-remove filter #'ghostel-dock--stamp-output))))

(ghostel-dock--track t)

(defun ghostel-dock-unload-function ()
  "Stop following the terminals when the package is unloaded."
  (ghostel-dock--track nil)
  nil)

(defun ghostel-dock--shell-p (comm)
  "Non-nil when COMM, a process name, is a shell; a login shell is -bash."
  (member (file-name-nondirectory (string-remove-prefix "-" (or comm "")))
          ghostel-dock-shells))

(defun ghostel-dock--short-command (args)
  "ARGS with the directory off its program, and off an interpreter's script."
  (let* ((words (split-string (string-trim (or args "")) "[ \t\n]+" t))
         (program (and words (file-name-nondirectory (car words)))))
    (when program
      (string-join
       (cons program
             (if (and (cdr words) (member program ghostel-dock-interpreters)
                      (string-prefix-p "/" (cadr words)))
                 (cons (file-name-nondirectory (cadr words)) (cddr words))
               (cdr words)))
       " "))))

(defun ghostel-dock--foreground (buffer)
  "(PGID . COMMAND) of the job in BUFFER's foreground; nil while its shell is.
The terminal's foreground process group, which every process on it can
tell: `ghostel--pid' is the shell, or the login(1) that runs it."
  (when-let* (((not (file-remote-p (buffer-local-value 'default-directory buffer))))
              (pid (ghostel-dock--local 'ghostel--pid buffer))
              (tpgid (alist-get 'tpgid (process-attributes pid)))
              ((> tpgid 0))
              (attrs (process-attributes tpgid))
              (comm (alist-get 'comm attrs))
              ((not (ghostel-dock--shell-p comm))))
    (cons tpgid (or (ghostel-dock--short-command (alist-get 'args attrs)) comm))))

(defun ghostel-dock--info (buffer)
  "What a row tells of terminal BUFFER, a plist."
  (let* ((process (ghostel-dock--local 'ghostel--process buffer))
         (alive (process-live-p process))
         (fg (and alive (ghostel-dock--foreground buffer)))
         (running (and alive (ghostel-dock--local 'ghostel--command-running buffer))))
    (list :root (ghostel-dock--root-of buffer)
          :name (ghostel-dock--name-of buffer)
          :job (cond ((not alive) 'exited)
                     (fg (cdr fg))
                     (running 'running))
          :pid (car fg)
          :status (buffer-local-value 'ghostel-dock--status buffer)
          :osc133 (or (buffer-local-value 'ghostel-dock--osc133 buffer)
                      (and (ghostel-dock--local 'ghostel--prompt-positions buffer) t))
          :dir (buffer-local-value 'default-directory buffer)
          :output (buffer-local-value 'ghostel-dock--output-at buffer))))

(defun ghostel-dock--busy-p (info)
  "Non-nil when the terminal INFO tells of runs something."
  (let ((job (plist-get info :job)))
    (and job (not (eq job 'exited)))))

(defun ghostel-dock--job-text (info)
  "What INFO's terminal runs, as its row says it."
  (pcase (plist-get info :job)
    ('exited (propertize "exited" 'face 'error))
    ('running (propertize "▶ running" 'face 'font-lock-string-face))
    ('nil (propertize "idle" 'face 'shadow))
    (job (propertize (concat "▶ " job) 'face 'font-lock-string-face))))

(defun ghostel-dock--job-words (info)
  "What INFO's terminal runs, for a sentence."
  (pcase (plist-get info :job)
    ('exited "nothing: it exited")
    ('running "a command")
    ('nil "nothing")
    (job job)))

(defun ghostel-dock--status-text (info)
  "INFO's last exit status: ✓, ✗ N, or blank when the shell never said."
  (pcase (plist-get info :status)
    ('nil "")
    (0 (propertize "✓" 'face 'success))
    (status (propertize (format "✗ %s" status) 'face 'error))))

(defun ghostel-dock--dir-text (info)
  "INFO's directory: under its root as ./SUB, else abbreviated."
  (let* ((dir (ghostel-dock--norm (plist-get info :dir)))
         (root (plist-get info :root)))
    (if (string-prefix-p root dir)
        (let ((sub (directory-file-name (substring dir (length root)))))
          (if (string-empty-p sub) "." (concat "./" sub)))
      (abbreviate-file-name (directory-file-name dir)))))

(defun ghostel-dock--age (time &optional now)
  "How long ago TIME was, short: now, 12s, 5m, 3h, 2d; blank for nil."
  (if (null time)
      ""
    (let ((secs (floor (- (or now (float-time)) time))))
      (cond ((< secs 2) "now")
            ((< secs 60) (format "%ds" secs))
            ((< secs 3600) (format "%dm" (/ secs 60)))
            ((< secs 86400) (format "%dh" (/ secs 3600)))
            (t (format "%dd" (/ secs 86400)))))))


;;; Making a terminal

(defun ghostel-dock--alist ()
  "The panel's display action alist."
  `((side . bottom)
    (slot . 0)
    (window-height . ,ghostel-dock-height)
    (preserve-size . (nil . t))
    (window-parameters . ((ghostel-dock . t)))))

(defun ghostel-dock--create (root name &optional caller)
  "Start ROOT's terminal NAME in the panel; return it.  The selected
window stays selected.
CALLER, the buffer it was asked from, goes to
`ghostel-dock-after-create-functions'."
  (let* ((root (ghostel-dock--norm root))
         (default-directory root)
         (ghostel-buffer-name (ghostel-dock--buffer-name root name))
         ;; Ghostel sizes the terminal by the window it pops into: the panel.
         (display-buffer-overriding-action
          (cons '(display-buffer-in-side-window) (ghostel-dock--alist)))
         (selected (selected-window))
         ;; It pops to it, selecting it: whoever wants it selected says so.
         (buffer (prog1 (ghostel t)
                   (when (window-live-p selected)
                     (select-window selected)))))
    (with-current-buffer buffer
      (setq ghostel-dock--root root
            ghostel-dock--name name
            ;; A name not the buffer's: ghostel takes it for a rename by
            ;; hand, and never renames it after a title again.
            ghostel--managed-buffer-name 'ghostel-dock))
    (run-hook-with-args 'ghostel-dock-after-create-functions buffer caller)
    buffer))

(defun ghostel-dock-terminal (&optional root create)
  "The terminal last used in ROOT, the current buffer's by default.
With CREATE, start one named `ghostel-dock-default-name' when ROOT has
none: it shows in the panel."
  (let ((root (ghostel-dock--norm (or root (ghostel-dock--current-root)))))
    (or (car (ghostel-dock-terminals root))
        (and create
             (ghostel-dock--create root (ghostel-dock--free-name
                                         root ghostel-dock-default-name)
                                   (current-buffer))))))

(defun ghostel-dock--read-name (root &optional prompt initial)
  "Read a name for one of ROOT's terminals, offering a free one.
PROMPT, else \"New terminal in PROJECT\"; INITIAL is in the minibuffer."
  (let* ((default (ghostel-dock--free-name root ghostel-dock-default-name))
         (name (string-trim
                (read-string (format "%s (%s): "
                                     (or prompt (format "New terminal in %s"
                                                        (ghostel-dock--root-name root)))
                                     default)
                             initial nil default))))
    (if (string-empty-p name) default name)))

;;;###autoload
(defun ghostel-dock-new (&optional name root)
  "Start a terminal NAME in ROOT -- this project's -- and show it in the panel.
Asks for the name.  A name ROOT already has shows that terminal."
  (interactive)
  (let* ((root (ghostel-dock--norm (or root (ghostel-dock--current-root))))
         (name (or name (ghostel-dock--read-name root))))
    (ghostel-dock-show (or (ghostel-dock--named root name)
                           (ghostel-dock--create root name (current-buffer)))
                       t)))


;;; The panel

(defun ghostel-dock--root-frame ()
  "The selected frame, or its parent when it is a child: the manager's
posframe has no panel, its parent does."
  (or (frame-parent) (selected-frame)))

(defun ghostel-dock--panel (&optional frame)
  "The panel window of FRAME, the selected one by default, or nil."
  (seq-find (lambda (window) (window-parameter window 'ghostel-dock))
            (window-list (or frame (ghostel-dock--root-frame)) 'never)))

(defun ghostel-dock--skip-terminal (_window buffer _bury-or-kill)
  "Non-nil for a terminal: a window it leaves shows something else."
  (ghostel-dock--terminal-p buffer))

(defun ghostel-dock--vacate (buffer)
  "Take terminal BUFFER out of this frame's other windows.
A terminal shows in one place, the panel: two windows would size it
two ways."
  (dolist (window (get-buffer-window-list buffer 'never (ghostel-dock--root-frame)))
    (unless (window-parameter window 'ghostel-dock)
      (let ((switch-to-prev-buffer-skip #'ghostel-dock--skip-terminal))
        (switch-to-prev-buffer window)))))

(defun ghostel-dock--display (buffer)
  "Show BUFFER in the panel, made when there is none; return its window."
  (ghostel-dock--vacate buffer)
  (with-selected-frame (ghostel-dock--root-frame)
    (display-buffer-in-side-window buffer (ghostel-dock--alist))))

(defun ghostel-dock-show (&optional buffer select)
  "Show BUFFER in the panel and return its window; SELECT it when non-nil.
BUFFER defaults to the current project's terminal, started when there
is none."
  (interactive (list nil t))
  (let* ((buffer (or buffer (ghostel-dock-terminal nil t)))
         (window (ghostel-dock--display buffer)))
    (when select
      (select-window window))
    window))

(defun ghostel-dock-hide ()
  "Hide the panel.  It is a side window: its room goes back to the others."
  (interactive)
  (when-let* ((window (ghostel-dock--panel)))
    (delete-window window)))

;;;###autoload
(defun ghostel-dock-toggle (&optional new)
  "Show this project's terminal panel, selected; or hide it when it shows.
It shows the terminal last used in the project, and starts one named
`ghostel-dock-default-name' when there is none.  With NEW, a prefix
argument, start another terminal in the project, asking its name."
  (interactive "P")
  (cond
   (new (ghostel-dock-new))
   ((ghostel-dock--panel) (ghostel-dock-hide))
   (t (ghostel-dock-show nil t))))

;;;###autoload
(defun ghostel-dock-rerun (&optional buffer)
  "Run BUFFER's last command again: Up, then Return, to its shell.
BUFFER defaults to this project's terminal; the panel shows it, not
selected.  A terminal running something is left alone."
  (interactive)
  (let ((buffer (or buffer (ghostel-dock-terminal nil t))))
    (ghostel-dock--check-idle buffer)
    (unless (get-buffer-window buffer)
      (ghostel-dock--display buffer))
    (ghostel-dock--send-rerun buffer)))

(defun ghostel-dock--check-idle (buffer)
  "Signal unless terminal BUFFER's shell waits at its prompt."
  (let ((info (ghostel-dock--info buffer)))
    (when (eq (plist-get info :job) 'exited)
      (user-error "%s has exited" (buffer-name buffer)))
    (when (ghostel-dock--busy-p info)
      (user-error "%s is running %s" (buffer-name buffer)
                  (ghostel-dock--job-words info)))))

(defun ghostel-dock--send-rerun (buffer)
  "Press Up, then Return, in terminal BUFFER: its shell runs its last command."
  (with-current-buffer buffer
    (ghostel-send-key "up")
    (ghostel-send-key "return"))
  buffer)


;;; Layers

(defvar ghostel-dock--layers nil
  "((FRAME . LAYER) . BUFFER): what each layer's panel showed when left.
BUFFER is nil for a layer left with no panel.")

(defun ghostel-dock--layer-key ()
  "The key of the selected frame's layer in `ghostel-dock--layers'.
A child frame -- the manager's posframe -- has its parent's layer."
  (let ((frame (or (frame-parent) (selected-frame))))
    (cons frame
          (and ghostel-dock-layer-function
               (with-selected-frame frame
                 (funcall ghostel-dock-layer-function))))))

(defun ghostel-dock-without-panel (fn &rest args)
  "Call FN with ARGS while the panel is down, then show it as the layer now
current left it.  An :around advice for what snapshots a layer's window
layout or puts one back -- jworkspace's layer switch, its maximize.  A
panel snapshotted would come back unmarked, with whatever buffer the
snapshot named; a maximize asking `one-window-p' would count it."
  (setq ghostel-dock--layers
        (seq-filter (lambda (entry) (frame-live-p (caar entry)))
                    ghostel-dock--layers))
  (let* ((panel (ghostel-dock--panel))
         (key (ghostel-dock--layer-key))
         (selected (and panel (eq panel (selected-window)))))
    (setf (alist-get key ghostel-dock--layers nil nil #'equal)
          (and panel (window-buffer panel)))
    (when panel
      (delete-window panel))
    (unwind-protect
        (apply fn args)
      (let* ((now (ghostel-dock--layer-key))
             (buffer (alist-get now ghostel-dock--layers nil nil #'equal)))
        (when (and (buffer-live-p buffer) (not (ghostel-dock--panel)))
          (let ((window (ghostel-dock--display buffer)))
            ;; The same layer, as after a maximize: the panel had the focus.
            (when (and selected (equal now key))
              (select-window window))))))))


;;; The manager: state

(defconst ghostel-dock--buffer-name "*ghostel-dock*")

(defvar ghostel-dock--rows nil
  "The terminals the manager lists, in order: this project's first.")
(defvar ghostel-dock--infos (make-hash-table :test 'eq)
  "BUFFER -> its row's plist, as of the last look.")
(defvar ghostel-dock--matched nil
  "((BUFFER . INFO) ...): the rows the filter keeps.")
(defvar ghostel-dock--index 0 "Index in `ghostel-dock--matched' of the selected row.")
(defvar ghostel-dock--top 0 "Index in `ghostel-dock--matched' of the first row printed.")
(defvar ghostel-dock--query "")
(defvar ghostel-dock--filtering nil)
(defvar ghostel-dock--help-visible nil)
(defvar ghostel-dock--display nil "Where the manager shows: posframe or side.")
(defvar ghostel-dock--posframe-parent nil)
(defvar ghostel-dock--width nil "Columns of the posframe, nil in a side window.")
(defvar ghostel-dock--parent-size nil
  "(WIDTH . HEIGHT) of the frame the posframe was last fitted to.")
(defvar ghostel-dock--table-rows nil "Rows the posframe holds, nil in a side window.")
(defvar ghostel-dock--preview-start nil)
(defvar ghostel-dock--preview-id nil)
(defvar ghostel-dock--preview-timer nil)
(defvar ghostel-dock--preview-origin nil
  "(HAD-PANEL . BUFFER): the panel when the manager opened, put back on quit.")
(defvar ghostel-dock--refresh-timer nil)
(defvar ghostel-dock--goto nil "Buffer the next rebuild selects.")

(defconst ghostel-dock--preview-lines 2
  "Lines under the list's rule: the selected terminal told in full.")

(defconst ghostel-dock-help
  '(("j, k"        "Move; the panel previews the terminal")
    ("gg, G"       "First / last")
    ("RET"         "Open in the panel; another project's in its layer")
    ("o"           "Open in the window the list was opened from")
    ("n"           "New terminal in the selected one's project")
    ("r"           "Rename")
    ("x"           "Kill; asks when it runs something")
    ("K"           "Kill the selected project's idle ones (lists, asks)")
    ("R"           "Rerun its last command (Up, Return) when idle")
    ("/"           "Filter: name, project, command, directory")
    ("g"           "Look again")
    ("q, Esc"      "Close; the panel goes back as it was"))
  "Key legend of the manager.")


;;; The manager: rows

(defun ghostel-dock--collect ()
  "List the terminals: the caller's project's first, then each other
project's, a project's most recently used first."
  (let* ((all (ghostel-dock-terminals t))
         (here (plist-get ghostel-dock--caller :root))
         (roots (delete-dups (cons here (mapcar #'ghostel-dock--root-of all)))))
    (setq ghostel-dock--rows
          (mapcan (lambda (root)
                    (seq-filter (lambda (buffer)
                                  (equal (ghostel-dock--root-of buffer) root))
                                all))
                  roots))
    (ghostel-dock--look)))

(defun ghostel-dock--look ()
  "Find out again what each listed terminal does; forget the dead ones."
  (setq ghostel-dock--rows (seq-filter #'ghostel-dock--terminal-p ghostel-dock--rows))
  (clrhash ghostel-dock--infos)
  (dolist (buffer ghostel-dock--rows)
    (puthash buffer (ghostel-dock--info buffer) ghostel-dock--infos)))

(defun ghostel-dock--matches-p (info query)
  "Non-nil when every word of QUERY is in INFO's name, project, job or dir."
  (let ((text (downcase
               (string-join
                (list (plist-get info :name)
                      (ghostel-dock--root-name (plist-get info :root))
                      (let ((job (plist-get info :job))) (if (stringp job) job ""))
                      (ghostel-dock--dir-text info))
                " "))))
    (seq-every-p (lambda (word) (string-search word text))
                 (split-string (downcase (or query "")) nil t))))

(defun ghostel-dock--width-now ()
  "Columns the manager has."
  (or ghostel-dock--width
      (when-let* ((buffer (get-buffer ghostel-dock--buffer-name))
                  (window (get-buffer-window buffer t)))
        (window-body-width window))
      (frame-width)))

(defun ghostel-dock--format (width)
  "Tabulated-list format for WIDTH columns."
  (let* ((budget (max 60 (- width 8)))
         (project 14) (name 14) (last 5) (age 4)
         (rest (- budget project name last age))
         (job (max 12 (/ (* rest 11) 20)))
         (dir (max 8 (- rest job))))
    (vector (list "Project" project nil)
            (list "Name" name nil)
            (list "Now" job nil)
            (list "Last" last nil)
            (list "Dir" dir nil)
            (list "Out" age nil :right-align t))))

(defun ghostel-dock--under-lines ()
  "Lines under the list: the rule, then the detail or the help."
  (1+ (if ghostel-dock--help-visible
          (length ghostel-dock-help)
        ghostel-dock--preview-lines)))

(defun ghostel-dock--table-height ()
  "How many rows the list shows; fewer while the help takes their room.
A posframe is sized for what it shows; a side window gives the help
the rows' room."
  (or ghostel-dock--table-rows
      (when-let* ((buffer (get-buffer ghostel-dock--buffer-name))
                  (window (get-buffer-window buffer 'visible)))
        (max 1 (- (window-body-height window) (ghostel-dock--under-lines))))
      10))

(defun ghostel-dock--entry (row index widths now)
  "The tabulated-list entry of ROW, the INDEXth matched, cut to WIDTHS.
Its project shows on the first row of a project, and on the first row
printed.  NOW is the time the ages count from."
  (let* ((info (cdr row))
         (root (plist-get info :root))
         (first (or (= index ghostel-dock--top)
                    (not (equal root (plist-get (cdr (nth (1- index) ghostel-dock--matched))
                                                :root)))))
         (here (equal root (plist-get ghostel-dock--caller :root)))
         (cells (list (if first
                          (propertize (ghostel-dock--root-name root)
                                      'face (if here 'bold 'font-lock-type-face))
                        "")
                      (propertize (plist-get info :name) 'face 'font-lock-keyword-face)
                      (ghostel-dock--job-text info)
                      (ghostel-dock--status-text info)
                      (propertize (ghostel-cmd--tail (ghostel-dock--dir-text info)
                                                     (nth 4 widths))
                                  'face 'shadow)
                      (propertize (ghostel-dock--age (plist-get info :output) now)
                                  'face 'shadow))))
    (list (car row)
          (apply #'vector
                 (cl-mapcar (lambda (cell width) (ghostel-cmd--truncate cell width))
                            cells widths)))))

(defun ghostel-dock--build-entries ()
  "List the rows the filter keeps that fit around the selected one."
  (let* ((fmt (setq tabulated-list-format (ghostel-dock--format (ghostel-dock--width-now))))
         (widths (mapcar #'cadr fmt))
         (keep (or ghostel-dock--goto
                   (car (nth ghostel-dock--index ghostel-dock--matched))))
         (matched (seq-keep (lambda (buffer)
                              (when-let* ((info (gethash buffer ghostel-dock--infos)))
                                (and (ghostel-dock--matches-p info ghostel-dock--query)
                                     (cons buffer info))))
                            ghostel-dock--rows))
         (count (length matched))
         (height (ghostel-dock--table-height))
         (now (float-time)))
    (setq ghostel-dock--goto nil
          ghostel-dock--matched matched
          ghostel-dock--index (max 0 (min (or (and keep (cl-position keep matched :key #'car))
                                                ghostel-dock--index)
                                            (1- count)))
          ghostel-dock--top (ghostel-cmd--window-top ghostel-dock--index ghostel-dock--top
                                                     height count))
    (tabulated-list-init-header)
    (setq tabulated-list-entries
          (cl-loop for index from ghostel-dock--top
                   below (min count (+ ghostel-dock--top height))
                   collect (ghostel-dock--entry (nth index matched) index widths now)))))


;;; The manager: drawing

(defun ghostel-dock--erase-preview ()
  (when (and ghostel-dock--preview-start (marker-position ghostel-dock--preview-start))
    (let ((inhibit-read-only t))
      (delete-region ghostel-dock--preview-start (point-max))))
  (setq ghostel-dock--preview-start nil))

(defun ghostel-dock--detail (buffer info width)
  "The lines under the list telling of BUFFER and its INFO, WIDTH wide."
  (if (null buffer)
      (propertize (if (string-empty-p ghostel-dock--query)
                      "No terminals yet: n starts one."
                    "No terminal matches.")
                  'face 'shadow)
    (let* ((job (plist-get info :job))
           (head (concat (propertize (buffer-name buffer) 'face 'bold) "  "
                         (abbreviate-file-name (plist-get info :dir))))
           (parts (delq nil
                        (list
                         (pcase job
                           ('exited "exited")
                           ('running "running a command")
                           ('nil "idle")
                           (_ (format "running %s (pgid %s)" job (plist-get info :pid))))
                         (cond ((not (plist-get info :osc133))
                                "no OSC 133 from its shell: exit status unknown")
                               ((plist-get info :status)
                                (format "last exit %s" (plist-get info :status)))
                               (t "no command finished yet"))
                         (let ((age (ghostel-dock--age (plist-get info :output))))
                           (cond ((string-empty-p age) "no output seen yet")
                                 ((equal age "now") "output just now")
                                 (t (format "output %s ago" age))))))))
      (concat (ghostel-cmd--truncate head width) "\n"
              (propertize (ghostel-cmd--truncate (string-join parts " · ") width)
                          'face 'shadow)))))

(defun ghostel-dock--draw-preview ()
  "Draw the rule and, under it, the selected terminal told in full, or the help."
  (when (derived-mode-p 'ghostel-dock-manager-mode)
    (let* ((inhibit-read-only t)
           (buffer (tabulated-list-get-id))
           (width (max 20 (or (ignore-errors (window-body-width
                                               (get-buffer-window (current-buffer) t)))
                              (ghostel-dock--width-now)))))
      (save-excursion
        (ghostel-dock--erase-preview)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (setq ghostel-dock--preview-start (point-marker))
        (insert (ghostel-cmd--rule width ghostel-dock--top
                                   (- (length ghostel-dock--matched) ghostel-dock--top
                                      (length tabulated-list-entries)))
                "\n"
                (if ghostel-dock--help-visible
                    (let ((w (apply #'max (mapcar (lambda (row) (string-width (car row)))
                                                  ghostel-dock-help))))
                      (mapconcat (lambda (row) (format (format "%%-%ds  %%s" w)
                                                       (car row) (cadr row)))
                                 ghostel-dock-help "\n"))
                  (ghostel-dock--detail buffer (gethash buffer ghostel-dock--infos)
                                        (1- width))))
        (unless (bolp) (insert "\n")))
      (set-buffer-modified-p nil))))

(defun ghostel-dock--print ()
  "Print the rows that fit, point on the selected one, the detail under."
  (ghostel-dock--erase-preview)
  (tabulated-list-print)
  (goto-char (point-min))
  (forward-line (- ghostel-dock--index ghostel-dock--top))
  (ghostel-dock--draw-preview)
  (setq ghostel-dock--preview-id (tabulated-list-get-id))
  (when-let* ((window (get-buffer-window (current-buffer) t)))
    (set-window-start window (point-min))
    (set-window-point window (point))))

(defun ghostel-dock--refresh (&optional look)
  "Rebuild the list and redraw it; with LOOK, find out again what each
terminal does first."
  (when-let* ((buffer (get-buffer ghostel-dock--buffer-name)))
    (with-current-buffer buffer
      (when (derived-mode-p 'ghostel-dock-manager-mode)
        (when look (ghostel-dock--look))
        (ghostel-dock--build-entries)
        (ghostel-dock--print)))
    (force-mode-line-update t)))

(defun ghostel-dock--footer ()
  "The manager's mode line; a % in it shows as itself."
  (string-replace
   "%" "%%"
   (concat
    " terminals"
    (when-let* ((root (plist-get ghostel-dock--caller :root)))
      (concat " · " (ghostel-dock--root-name root)))
    (when (or ghostel-dock--filtering (not (string-empty-p ghostel-dock--query)))
      (concat "  "
              (if ghostel-dock--filtering
                  (propertize (concat "/" ghostel-dock--query "▏") 'face 'highlight)
                (concat "/" ghostel-dock--query))
              (format "  %d/%d" (length ghostel-dock--matched) (length ghostel-dock--rows))))
    (cond
     (ghostel-dock--filtering "   RET done  C-n/C-p move  Esc clear")
     (ghostel-dock--help-visible "   ? close help")
     (t "   RET panel  o here  n new  r rename  x kill  K idle  R rerun  / filter  ? keys")))))


;;; The manager: moving

(defun ghostel-dock--select (index)
  "Select the row at INDEX, scrolling the list when it is not printed."
  (let ((count (length ghostel-dock--matched)))
    (when (> count 0)
      (setq ghostel-dock--index (max 0 (min index (1- count))))
      (if (and (>= ghostel-dock--index ghostel-dock--top)
               (< ghostel-dock--index (+ ghostel-dock--top (length tabulated-list-entries))))
          (progn
            (goto-char (point-min))
            (forward-line (- ghostel-dock--index ghostel-dock--top)))
        (ghostel-dock--refresh)))))

(defun ghostel-dock-next (&optional n)
  "Select the Nth next terminal."
  (interactive "p")
  (ghostel-dock--select (+ ghostel-dock--index (or n 1))))

(defun ghostel-dock-previous (&optional n)
  "Select the Nth previous terminal."
  (interactive "p")
  (ghostel-dock--select (- ghostel-dock--index (or n 1))))

(defun ghostel-dock-first ()
  "Select the first terminal."
  (interactive)
  (ghostel-dock--select 0))

(defun ghostel-dock-last ()
  "Select the last terminal."
  (interactive)
  (ghostel-dock--select (1- (length ghostel-dock--matched))))

(defun ghostel-dock--confine-point ()
  (when (and ghostel-dock--preview-start
             (marker-position ghostel-dock--preview-start)
             (>= (point) ghostel-dock--preview-start)
             (> ghostel-dock--preview-start (point-min)))
    (goto-char (1- ghostel-dock--preview-start))
    (beginning-of-line)))

(defun ghostel-dock--on-command ()
  "After a command in the manager: keep point on a row, follow it."
  (when (derived-mode-p 'ghostel-dock-manager-mode)
    (ghostel-dock--confine-point)
    (when (tabulated-list-get-id)
      ;; Point moved by other means, a click say: select what it is on.
      (setq ghostel-dock--index (+ ghostel-dock--top (1- (line-number-at-pos)))))
    (let ((id (tabulated-list-get-id)))
      (unless (equal id ghostel-dock--preview-id)
        (setq ghostel-dock--preview-id id)
        (unless ghostel-dock--help-visible
          (ghostel-dock--draw-preview))
        (ghostel-dock--schedule-preview)))))


;;; The manager: previewing in the panel

(defun ghostel-dock--schedule-preview ()
  "Preview the selected terminal in the panel once the selection rests."
  (when (timerp ghostel-dock--preview-timer)
    (cancel-timer ghostel-dock--preview-timer))
  (setq ghostel-dock--preview-timer
        (run-with-timer ghostel-dock-preview-delay nil #'ghostel-dock--preview)))

(defun ghostel-dock--preview ()
  "Show the selected terminal in the caller frame's panel.
One already shown in a window there is left where it is."
  (setq ghostel-dock--preview-timer nil)
  (when-let* ((manager (get-buffer ghostel-dock--buffer-name))
              (buffer (with-current-buffer manager (tabulated-list-get-id)))
              ((buffer-live-p buffer))
              (frame (plist-get ghostel-dock--caller :frame))
              ((frame-live-p frame))
              ((not (get-buffer-window buffer frame))))
    (with-selected-frame frame
      (display-buffer-in-side-window buffer (ghostel-dock--alist)))))

(defun ghostel-dock--end-preview ()
  "Put the panel back as it was when the manager opened."
  (when (timerp ghostel-dock--preview-timer)
    (cancel-timer ghostel-dock--preview-timer))
  (setq ghostel-dock--preview-timer nil)
  (when-let* ((frame (plist-get ghostel-dock--caller :frame))
              ((frame-live-p frame)))
    (with-selected-frame frame
      (pcase-let ((`(,had . ,buffer) ghostel-dock--preview-origin)
                  (panel (ghostel-dock--panel)))
        (cond
         ((and had (buffer-live-p buffer))
          (unless (and panel (eq (window-buffer panel) buffer))
            (display-buffer-in-side-window buffer (ghostel-dock--alist))))
         (panel (delete-window panel)))))))


;;; The manager: showing and hiding

(defun ghostel-dock--use-posframe-p ()
  (and ghostel-dock-use-posframe (ghostel-cmd--posframe-available-p)))

(defun ghostel-dock--posframe-frame ()
  (when-let* ((buffer (get-buffer ghostel-dock--buffer-name)))
    (and (boundp 'posframe--frame)
         (buffer-local-value 'posframe--frame buffer))))

(defun ghostel-dock--posframe-showing-p ()
  (when-let* ((frame (ghostel-dock--posframe-frame)))
    (and (frame-live-p frame) (frame-visible-p frame))))

(defun ghostel-dock--showing-p ()
  "Non-nil while the manager is on screen.
A hidden posframe's window still shows the buffer: only visible frames count."
  (or (ghostel-dock--posframe-showing-p)
      (when-let* ((buffer (get-buffer ghostel-dock--buffer-name)))
        (and (get-buffer-window buffer 'visible) t))))

(defun ghostel-dock--posframe-hooks (on)
  (dolist (hook '((window-size-change-functions . ghostel-dock--on-parent-resize)
                  (window-selection-change-functions . ghostel-dock--on-focus-leave)))
    (if on (add-hook (car hook) (cdr hook)) (remove-hook (car hook) (cdr hook)))))

(defun ghostel-dock--panel-lines (frame)
  "Lines the panel takes, or would, in FRAME."
  (if (floatp ghostel-dock-height)
      (round (* ghostel-dock-height (frame-height frame)))
    ghostel-dock-height))

(defun ghostel-dock--show-posframe (buffer)
  "Show the manager BUFFER in a posframe at the top of the frame.
It stays clear of the panel under it, where the selected terminal shows."
  (let* ((parent (or (and (frame-live-p ghostel-dock--posframe-parent)
                          ghostel-dock--posframe-parent)
                     (frame-parent (selected-frame))
                     (selected-frame)))
         (avail (frame-width parent))
         (width (max (min 80 (- avail 4)) (min (- avail 6) (round (* 0.75 avail)))))
         (chrome ghostel-cmd--posframe-chrome-lines)
         (under (ghostel-dock--under-lines))
         (room (- (frame-height parent) (ghostel-dock--panel-lines parent) 3))
         (room (max (+ chrome under 1)
                    (min room (ghostel-cmd--posframe-max-height parent))))
         (table (max 1 (min (max 3 (length ghostel-dock--rows))
                            (- room chrome under))))
         (height (+ chrome table under)))
    (setq ghostel-dock--posframe-parent parent
          ghostel-dock--parent-size (cons (frame-width parent) (frame-height parent))
          ghostel-dock--width width
          ghostel-dock--table-rows table)
    (ghostel-dock--posframe-hooks t)
    (with-selected-frame parent
      (posframe-show buffer
                     :poshandler #'posframe-poshandler-frame-top-center
                     :width width
                     :min-width (min 60 width)
                     :height height
                     :min-height height
                     :max-height height
                     :left-fringe 12
                     :right-fringe 12
                     :border-width 2
                     :border-color (ghostel-cmd--posframe-border-color)
                     :respect-header-line t
                     :respect-mode-line t
                     :lines-truncate t
                     :cursor 'box
                     :accept-focus t
                     :window-point (with-current-buffer buffer (point))))
    (when-let* (((not ghostel-cmd--keep-focus))
                (frame (ghostel-dock--posframe-frame))
                (window (get-buffer-window buffer frame)))
      (select-frame-set-input-focus frame)
      (select-window window))))

(defun ghostel-dock--show-side-window (buffer)
  "Show the manager BUFFER in a top side window, selected."
  (setq ghostel-dock--width nil
        ghostel-dock--table-rows nil)
  (select-window
   (display-buffer-in-side-window
    buffer `((side . top) (slot . 0)
             (window-height . ,(+ 3 ghostel-dock--preview-lines
                                  (max 4 (min 12 (length ghostel-dock--rows)))))))))

(defun ghostel-dock--hide-overlay (&optional keep-focus)
  "Take the manager off screen; give focus back unless KEEP-FOCUS."
  (dolist (timer '(ghostel-dock--refresh-timer ghostel-dock--preview-timer))
    (when (timerp (symbol-value timer))
      (cancel-timer (symbol-value timer)))
    (set timer nil))
  (ghostel-dock--set-filtering nil)
  (setq ghostel-dock--help-visible nil)
  (let ((parent ghostel-dock--posframe-parent)
        (buffer (get-buffer ghostel-dock--buffer-name)))
    (setq ghostel-dock--posframe-parent nil)
    (ghostel-dock--posframe-hooks nil)
    (cond
     ((ghostel-dock--posframe-showing-p)
      (posframe-hide buffer)
      (when (and parent (frame-live-p parent) (not keep-focus))
        (select-frame-set-input-focus parent)))
     ((and buffer (get-buffer-window buffer))
      (delete-window (get-buffer-window buffer))))))

(defun ghostel-dock--close (&optional keep-panel)
  "Close the manager; the panel goes back as it was unless KEEP-PANEL."
  (ghostel-dock--hide-overlay)
  (unless keep-panel
    (ghostel-dock--end-preview)))

(defun ghostel-dock--on-focus-leave (_frame)
  "Close the manager once a window outside it is selected; not the minibuffer."
  (when-let* ((frame (ghostel-dock--posframe-frame))
              ((frame-visible-p frame))
              (window (selected-window))
              ((not (eq (window-frame window) frame)))
              ((not (window-minibuffer-p window))))
    (ghostel-dock--hide-overlay t)
    ;; Into the panel: the terminal previewed there is the one wanted.
    (unless (window-parameter window 'ghostel-dock)
      (ghostel-dock--end-preview))))

(defun ghostel-dock--on-parent-resize (frame)
  "Fit the posframe again when FRAME, its parent, changed size."
  (when (and ghostel-dock--posframe-parent
             (eq frame ghostel-dock--posframe-parent)
             (ghostel-dock--posframe-showing-p)
             (not (equal ghostel-dock--parent-size
                         (cons (frame-width frame) (frame-height frame)))))
    (let ((ghostel-cmd--keep-focus t))
      (ghostel-dock--show-posframe (get-buffer ghostel-dock--buffer-name)))
    (ghostel-dock--refresh)))

(defun ghostel-dock--tick ()
  "The manager's periodic look, while it shows and no prompt is up."
  (cond
   ((not (ghostel-dock--showing-p))
    (when (timerp ghostel-dock--refresh-timer)
      (cancel-timer ghostel-dock--refresh-timer))
    (setq ghostel-dock--refresh-timer nil))
   ((not (active-minibuffer-window))
    (ghostel-dock--refresh t))))

(defun ghostel-dock--show ()
  "Show the manager as the state has it."
  (let* ((buffer (get-buffer-create ghostel-dock--buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'ghostel-dock-manager-mode)
        (ghostel-dock-manager-mode))
      (setq default-directory (or (plist-get ghostel-dock--caller :root) default-directory)))
    (setq ghostel-dock--display (if (ghostel-dock--use-posframe-p) 'posframe 'side))
    (if (eq ghostel-dock--display 'posframe)
        (ghostel-dock--show-posframe buffer)
      (ghostel-dock--show-side-window buffer))
    (ghostel-dock--refresh)
    (when (and ghostel-dock-refresh-interval (not ghostel-dock--refresh-timer))
      (setq ghostel-dock--refresh-timer
            (run-with-timer ghostel-dock-refresh-interval ghostel-dock-refresh-interval
                            #'ghostel-dock--tick)))
    buffer))

(defun ghostel-dock--first-pick ()
  "The row the manager opens on: the project's most recent terminal not on
screen already, so RET goes back to the one before; else the first."
  (let ((here (plist-get ghostel-dock--caller :root)))
    (or (seq-find (lambda (buffer)
                    (and (equal (ghostel-dock--root-of buffer) here)
                         (not (get-buffer-window buffer))))
                  ghostel-dock--rows)
        (car ghostel-dock--rows))))

;;;###autoload
(defun ghostel-dock-manager ()
  "List every project's terminals, this project's first, in an overlay.
Each row: its project, name, what it runs now, its last exit status
(OSC 133), its directory and how long since its output.  Moving
previews the terminal in the panel; RET opens it there."
  (interactive)
  (when (derived-mode-p 'ghostel-dock-manager-mode)
    (user-error "The terminal list is open"))
  (let ((panel (ghostel-dock--panel)))
    (setq ghostel-dock--caller (list :root (ghostel-dock--current-root)
                                     :buffer (current-buffer)
                                     :frame (selected-frame))
          ghostel-dock--preview-origin (cons (and panel t) (and panel (window-buffer panel)))
          ghostel-dock--query ""
          ghostel-dock--help-visible nil
          ghostel-dock--index 0
          ghostel-dock--top 0
          ghostel-dock--matched nil
          ghostel-dock--preview-id nil))
  (ghostel-dock--collect)
  (setq ghostel-dock--goto (ghostel-dock--first-pick))
  (prog1 (ghostel-dock--show)
    ;; The row it opens on previews like any other.
    (with-current-buffer ghostel-dock--buffer-name
      (setq ghostel-dock--preview-id (tabulated-list-get-id))
      (ghostel-dock--schedule-preview))))


;;; The manager: acting

(defun ghostel-dock--selected ()
  "The terminal of the selected row; signals when there is none."
  (let ((buffer (tabulated-list-get-id)))
    (unless (buffer-live-p buffer)
      (user-error "No terminal here"))
    buffer))

(defun ghostel-dock--selected-root ()
  "The project of the selected row, else of where the manager opened."
  (if-let* ((buffer (tabulated-list-get-id))
            ((buffer-live-p buffer)))
      (ghostel-dock--root-of buffer)
    (plist-get ghostel-dock--caller :root)))

(defun ghostel-dock--ask (fn &optional closing)
  "Call FN where the minibuffer works, then show the manager again.
A posframe holds focus away from the minibuffer: hide it meanwhile.
CLOSING: FN's answer closes the manager, so it shows again only when
FN did not finish -- C-g at the prompt."
  (if (not (ghostel-dock--posframe-showing-p))
      (funcall fn)
    (let ((parent ghostel-dock--posframe-parent)
          (done nil))
      (posframe-hide (get-buffer ghostel-dock--buffer-name))
      (when (frame-live-p parent)
        (select-frame-set-input-focus parent))
      (unwind-protect
          (prog1 (funcall fn) (setq done t))
        (when (and (get-buffer ghostel-dock--buffer-name)
                   (not (and closing done)))
          (ghostel-dock--show))))))

(defun ghostel-dock--switch-layer (root)
  "Go to ROOT's layer, when it is not the one the manager opened from."
  (when (and ghostel-dock-switch-layer-function
             (not (equal root (plist-get ghostel-dock--caller :root))))
    (funcall ghostel-dock-switch-layer-function root)))

(defun ghostel-dock-manager-open (&optional here)
  "Open the selected terminal in the panel, selected.
With HERE, in the window the manager was opened from instead.  Another
project's terminal goes through `ghostel-dock-switch-layer-function'
first, so it opens in that project's layer."
  (interactive)
  (when ghostel-dock--filtering
    (ghostel-dock--set-filtering nil))
  (let* ((buffer (ghostel-dock--selected))
         (root (ghostel-dock--root-of buffer))
         (moves (or here (and ghostel-dock-switch-layer-function
                              (not (equal root (plist-get ghostel-dock--caller :root)))))))
    ;; The preview stays when the panel keeps it: no flicker back and forth.
    (ghostel-dock--close (not moves))
    (ghostel-dock--switch-layer root)
    (if (not here)
        (ghostel-dock-show buffer t)
      (when-let* ((panel (ghostel-dock--panel))
                  ((eq (window-buffer panel) buffer)))
        (delete-window panel))
      (pop-to-buffer-same-window buffer))
    buffer))

(defun ghostel-dock-manager-open-here ()
  "Open the selected terminal in the window the manager was opened from."
  (interactive)
  (ghostel-dock-manager-open t))

(defun ghostel-dock-manager-new ()
  "Start a terminal in the selected row's project; ask its name."
  (interactive)
  (let* ((root (ghostel-dock--selected-root))
         (caller (plist-get ghostel-dock--caller :buffer))
         name)
    (ghostel-dock--ask (lambda () (setq name (ghostel-dock--read-name root))) t)
    (ghostel-dock--close)
    (ghostel-dock--switch-layer root)
    (ghostel-dock-show (or (ghostel-dock--named root name)
                           (ghostel-dock--create root name
                                                 (if (buffer-live-p caller) caller
                                                   (current-buffer))))
                       t)))

(defun ghostel-dock--rename (buffer name)
  "Name terminal BUFFER NAME in its project: buffer *PROJECT:NAME*."
  (let* ((root (ghostel-dock--root-of buffer))
         (other (ghostel-dock--named root name)))
    (when (and other (not (eq other buffer)))
      (user-error "%s already has a terminal %s" (ghostel-dock--root-name root) name))
    (with-current-buffer buffer
      (rename-buffer (ghostel-dock--buffer-name root name) t)
      (setq ghostel-dock--name name
            ghostel--buffer-identity (buffer-name)
            ghostel--managed-buffer-name 'ghostel-dock))
    buffer))

(defun ghostel-dock-manager-rename ()
  "Rename the selected terminal."
  (interactive)
  (let* ((buffer (ghostel-dock--selected))
         (old (ghostel-dock--name-of buffer)))
    (ghostel-dock--ask
     (lambda ()
       (let ((name (string-trim (read-string (format "Rename %s to: " old)
                                             (buffer-local-value 'ghostel-dock--name buffer)))))
         (unless (string-empty-p name)
           (ghostel-dock--rename buffer name)))))
    (setq ghostel-dock--goto buffer)
    (ghostel-dock--refresh t)))

(defun ghostel-dock--kill (buffer)
  "Kill terminal BUFFER, asked about already.
Ghostel asks in the buffer itself, through a buffer-local
`kill-buffer-query-functions' entry: it is told not to."
  (let ((ghostel-query-before-killing nil)
        (kill-buffer-query-functions nil))
    (kill-buffer buffer)))

(defun ghostel-dock-manager-kill ()
  "Kill the selected terminal; ask first when it runs something."
  (interactive)
  (let* ((buffer (ghostel-dock--selected))
         (info (ghostel-dock--info buffer))
         (ok t))
    (when (ghostel-dock--busy-p info)
      (ghostel-dock--ask
       (lambda ()
         (setq ok (yes-or-no-p (format "%s is running %s; kill it? "
                                       (buffer-name buffer)
                                       (ghostel-dock--job-words info)))))))
    (when ok
      (ghostel-dock--kill buffer)
      (ghostel-dock--collect)
      (ghostel-dock--refresh))))

(defun ghostel-dock-manager-kill-idle ()
  "Kill the selected row's project's idle terminals, once asked."
  (interactive)
  (let* ((root (ghostel-dock--selected-root))
         (idle (seq-remove (lambda (buffer) (ghostel-dock--busy-p (ghostel-dock--info buffer)))
                           (ghostel-dock-terminals root)))
         ok)
    (unless idle
      (user-error "%s has no idle terminal" (ghostel-dock--root-name root)))
    (ghostel-dock--ask
     (lambda ()
       (setq ok (yes-or-no-p (format "Kill %d idle in %s: %s? "
                                     (length idle) (ghostel-dock--root-name root)
                                     (mapconcat #'ghostel-dock--name-of idle ", "))))))
    (when ok
      (mapc #'ghostel-dock--kill idle)
      (ghostel-dock--collect)
      (ghostel-dock--refresh))))

(defun ghostel-dock-manager-rerun ()
  "Run the selected terminal's last command again; the list stays."
  (interactive)
  (let ((buffer (ghostel-dock--selected)))
    ;; Not `ghostel-dock-rerun': its panel would be the posframe's frame's.
    ;; The panel previews the selected terminal already.
    (ghostel-dock--check-idle buffer)
    (ghostel-dock--send-rerun buffer)
    (run-with-timer 0.5 nil (lambda () (when (ghostel-dock--showing-p)
                                         (ghostel-dock--refresh t))))))

(defun ghostel-dock-manager-refresh ()
  "Look again at every terminal."
  (interactive)
  (ghostel-dock--collect)
  (ghostel-dock--refresh))

(defun ghostel-dock-manager-help ()
  "Show or hide the key legend; the posframe grows to hold it."
  (interactive)
  (setq ghostel-dock--help-visible (not ghostel-dock--help-visible))
  (when (ghostel-dock--posframe-showing-p)
    (let ((ghostel-cmd--keep-focus t))
      (ghostel-dock--show-posframe (get-buffer ghostel-dock--buffer-name))))
  (ghostel-dock--refresh))

(defun ghostel-dock-manager-quit ()
  "Clear the filter, close the help, else close the manager."
  (interactive)
  (cond
   (ghostel-dock--help-visible (ghostel-dock-manager-help))
   ((not (string-empty-p ghostel-dock--query))
    (setq ghostel-dock--query "")
    (ghostel-dock--set-filtering nil)
    (ghostel-dock--refresh))
   (t (ghostel-dock--close))))


;;; The manager: filter

(defun ghostel-dock--set-query (query)
  (setq ghostel-dock--query query)
  (ghostel-dock--refresh))

(defun ghostel-dock-filter-self-insert ()
  (interactive)
  (let ((ev last-command-event))
    (cond
     ((ghostel-cmd--return-event-p ev) (ghostel-dock-filter-done))
     ((and (characterp ev) (>= ev 32) (not (eq ev 127)))
      (ghostel-dock--set-query (concat ghostel-dock--query (char-to-string ev)))))))

(defun ghostel-dock-filter-backspace ()
  (interactive)
  (unless (string-empty-p ghostel-dock--query)
    (ghostel-dock--set-query (substring ghostel-dock--query 0 -1))))

(defun ghostel-dock-filter-clear ()
  (interactive)
  (ghostel-dock--set-query ""))

(defun ghostel-dock-filter-yank ()
  (interactive)
  (ghostel-dock--set-query (concat ghostel-dock--query (ghostel-cmd--yank-text))))

(defun ghostel-dock-filter-read ()
  "Edit the filter in the minibuffer, where an input method works."
  (interactive)
  (let ((old ghostel-dock--query))
    (condition-case nil
        (ghostel-dock--ask
         (lambda ()
           (ghostel-dock--set-query
            (ghostel-cmd--read-line "Filter: " old #'ghostel-dock--set-query))))
      (quit (ghostel-dock--set-query old)))))

(defun ghostel-dock-filter-done ()
  (interactive)
  (ghostel-dock--set-filtering nil))

(defun ghostel-dock-filter ()
  "Type to narrow the list."
  (interactive)
  (ghostel-dock--set-filtering t))

(defun ghostel-dock-filter-escape ()
  "Clear the filter and stop typing it."
  (interactive)
  (ghostel-dock--set-filtering nil)
  (ghostel-dock-filter-clear))

(defvar-keymap ghostel-dock-filter-map
  :doc "Keys while typing the manager's filter."
  "RET"         #'ghostel-dock-filter-done
  "C-m"         #'ghostel-dock-filter-done
  "<return>"    #'ghostel-dock-filter-done
  "DEL"         #'ghostel-dock-filter-backspace
  "<backspace>" #'ghostel-dock-filter-backspace
  "C-h"         #'ghostel-dock-filter-backspace
  "C-u"         #'ghostel-dock-filter-clear
  "C-y"         #'ghostel-dock-filter-yank
  "C-\\"        #'ghostel-dock-filter-read
  "C-g"         #'ghostel-dock-filter-escape
  "<escape>"    #'ghostel-dock-filter-escape)

(define-key ghostel-dock-filter-map [t] #'ghostel-dock-filter-self-insert)
(ghostel-cmd--bind-printable-keys ghostel-dock-filter-map #'ghostel-dock-filter-self-insert)
;; Bound after the map, so a reload of the file rebinds them too.
(dolist (binding '(("C-n" . ghostel-dock-next) ("C-p" . ghostel-dock-previous)
                   ("<down>" . ghostel-dock-next) ("<up>" . ghostel-dock-previous)
                   ("<return>" . ghostel-dock-filter-done)
                   ("<kp-enter>" . ghostel-dock-filter-done)))
  (keymap-set ghostel-dock-filter-map (car binding) (cdr binding)))

(define-minor-mode ghostel-dock-filter-mode
  "Typing narrows the terminal list."
  :lighter nil
  :keymap ghostel-dock-filter-map)

(defun ghostel-dock--set-filtering (on)
  (setq ghostel-dock--filtering (and on t))
  (when-let* ((buffer (get-buffer ghostel-dock--buffer-name)))
    (with-current-buffer buffer
      (ghostel-dock-filter-mode (if ghostel-dock--filtering 1 -1))
      (when (fboundp 'evil-normalize-keymaps)
        (evil-normalize-keymaps))))
  (force-mode-line-update t))


;;; The manager: mode and keys

(defvar-keymap ghostel-dock-manager-mode-map
  :doc "Keys of the terminal manager."
  "RET"        #'ghostel-dock-manager-open
  "<return>"   #'ghostel-dock-manager-open
  "o"          #'ghostel-dock-manager-open-here
  "n"          #'ghostel-dock-manager-new
  "r"          #'ghostel-dock-manager-rename
  "x"          #'ghostel-dock-manager-kill
  "K"          #'ghostel-dock-manager-kill-idle
  "R"          #'ghostel-dock-manager-rerun
  "/"          #'ghostel-dock-filter
  "g"          #'ghostel-dock-manager-refresh
  "?"          #'ghostel-dock-manager-help
  "q"          #'ghostel-dock-manager-quit
  "C-g"        #'ghostel-dock-manager-quit
  "<escape>"   #'ghostel-dock-manager-quit)

;; Motion is bound after the map, so a reload of the file rebinds it.
(dolist (binding '(("j" . ghostel-dock-next) ("k" . ghostel-dock-previous)
                   ("C-n" . ghostel-dock-next) ("C-p" . ghostel-dock-previous)
                   ("<down>" . ghostel-dock-next) ("<up>" . ghostel-dock-previous)
                   ("<home>" . ghostel-dock-first) ("<end>" . ghostel-dock-last)
                   ("M-<" . ghostel-dock-first) ("M->" . ghostel-dock-last)))
  (keymap-set ghostel-dock-manager-mode-map (car binding) (cdr binding)))

(defun ghostel-dock--setup-evil ()
  ;; One call: a keymap holds one intercept state, so a second call for
  ;; another state would take the first one's place.
  (evil-make-intercept-map ghostel-dock-filter-map)
  (evil-define-key* '(normal motion) ghostel-dock-filter-map
    (kbd "RET") #'ghostel-dock-filter-done
    (kbd "<return>") #'ghostel-dock-filter-done
    (kbd "C-m") #'ghostel-dock-filter-done
    (kbd "<escape>") #'ghostel-dock-filter-escape)
  (evil-define-key* '(normal motion) ghostel-dock-manager-mode-map
    (kbd "j") #'ghostel-dock-next
    (kbd "k") #'ghostel-dock-previous
    (kbd "C-n") #'ghostel-dock-next
    (kbd "C-p") #'ghostel-dock-previous
    (kbd "<down>") #'ghostel-dock-next
    (kbd "<up>") #'ghostel-dock-previous
    (kbd "gg") #'ghostel-dock-first
    (kbd "G") #'ghostel-dock-last
    (kbd "RET") #'ghostel-dock-manager-open
    (kbd "<return>") #'ghostel-dock-manager-open
    (kbd "o") #'ghostel-dock-manager-open-here
    (kbd "n") #'ghostel-dock-manager-new
    (kbd "r") #'ghostel-dock-manager-rename
    (kbd "x") #'ghostel-dock-manager-kill
    (kbd "K") #'ghostel-dock-manager-kill-idle
    (kbd "R") #'ghostel-dock-manager-rerun
    (kbd "/") #'ghostel-dock-filter
    (kbd "gr") #'ghostel-dock-manager-refresh
    (kbd "?") #'ghostel-dock-manager-help
    (kbd "q") #'ghostel-dock-manager-quit
    (kbd "<escape>") #'ghostel-dock-manager-quit))

(with-eval-after-load 'evil
  (ghostel-dock--setup-evil))

(define-derived-mode ghostel-dock-manager-mode tabulated-list-mode "Terminals"
  "The list of every project's terminals."
  (setq tabulated-list-padding 1)
  (setq tabulated-list-format (ghostel-dock--format (ghostel-dock--width-now)))
  (setq truncate-lines t)
  (setq-local mode-line-format '("" (:eval (ghostel-dock--footer))))
  (hl-line-mode 1)
  (add-hook 'post-command-hook #'ghostel-dock--on-command nil t)
  (when (fboundp 'evil-normalize-keymaps)
    (evil-normalize-keymaps))
  (tabulated-list-init-header))

(provide 'ghostel-dock)
;;; ghostel-dock.el ends here

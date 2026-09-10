;;; ghostel-cmd.el --- Per-project Ghostel command overlay -*- lexical-binding: t; -*-

;; Author: Jing
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience, terminals
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Bookmark command templates per project, with {{var}} / {{var:default}}
;; placeholders.  The overlay (posframe when available, side window
;; otherwise) is the main UI:
;;
;;   j/k move   RET paste (fill vars first)   C-RET paste+Enter
;;   / filter   a add   e edit   d delete   q quit
;;
;; Filling happens on a footer line in the overlay, not a chain of
;; minibuffers.  Paste does not press Enter unless C-RET.
;;
;; `ghostel-project-cmd-run' is an alias for the overlay so existing
;; keybindings keep working.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)

(declare-function posframe-workable-p "posframe")
(declare-function posframe-show "posframe")
(declare-function posframe-hide "posframe")
(declare-function posframe-refresh "posframe")
(declare-function posframe-poshandler-frame-center "posframe")
(declare-function project-current "project")
(declare-function project-root "project")
(declare-function project-run-ghostel "jemacs-project")
(declare-function new-terminal "jemacs-project")
(declare-function ghostel-paste-string "ghostel")
(declare-function ghostel-send-key "ghostel")
(declare-function magit-get-current-branch "magit-git")
(declare-function vc-git--current-branch "vc-git")
(declare-function evil-define-key* "evil")
(declare-function evil-make-intercept-map "evil")
(declare-function evil-normalize-keymaps "evil")


;;; Customization

(defgroup ghostel-cmd nil
  "Per-project Ghostel command bookmarks."
  :group 'tools
  :prefix "ghostel-cmd-")

(defcustom ghostel-cmd-save-file
  (locate-user-emacs-file "ghostel-project-commands.eld")
  "File that persists project command bookmarks."
  :type 'file
  :group 'ghostel-cmd)

(defcustom ghostel-cmd-use-posframe t
  "Show the command list in a posframe overlay when possible."
  :type 'boolean
  :group 'ghostel-cmd)

(defcustom ghostel-cmd-posframe-width 72
  "Minimum character width of the overlay."
  :type 'integer
  :group 'ghostel-cmd)

(defcustom ghostel-cmd-posframe-width-ratio 0.55
  "Fraction of the parent frame width used by the overlay.
Nil means a fixed `ghostel-cmd-posframe-width'."
  :type '(choice (const :tag "Fixed width" nil) number)
  :group 'ghostel-cmd)

(defcustom ghostel-cmd-sidebar-side 'right
  "Side window side when not using posframe."
  :type '(choice (const left) (const right))
  :group 'ghostel-cmd)

(defcustom ghostel-cmd-sidebar-width 56
  "Side window width when not using posframe."
  :type 'integer
  :group 'ghostel-cmd)


;;; Store

(defvar ghostel-cmd--cache nil
  "Alist of (PROJECT-ROOT . (CMD-PLIST ...)).")

(defconst ghostel-cmd--var-regexp
  "{{\\s-*\\([^:} \t\n]+\\)\\(?::\\([^}]*\\)\\)?\\s-*}}"
  "Match {{var}} or {{var:default}}.")

(defun ghostel-cmd--current-project-root ()
  "Normalized project root, or `default-directory'."
  (file-name-as-directory
   (expand-file-name
    (or (when-let* ((proj (project-current nil)))
          (project-root proj))
        default-directory))))

(defun ghostel-cmd--load ()
  "Read `ghostel-cmd-save-file', or nil."
  (when (file-exists-p ghostel-cmd-save-file)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents ghostel-cmd-save-file)
          (goto-char (point-min))
          (unless (= (point-min) (point-max))
            (read (current-buffer))))
      (error
       (message "ghostel-cmd: could not load %s: %s"
                ghostel-cmd-save-file (error-message-string err))
       nil))))

(defun ghostel-cmd--save ()
  "Write `ghostel-cmd--cache' to disk."
  (let ((dir (file-name-directory ghostel-cmd-save-file)))
    (when (and dir (not (file-directory-p dir)))
      (make-directory dir t)))
  (with-temp-file ghostel-cmd-save-file
    (let ((print-length nil)
          (print-level nil)
          (print-circle nil))
      (prin1 (or ghostel-cmd--cache '()) (current-buffer))
      (insert "\n"))))

(defun ghostel-cmd--ensure-cache ()
  (unless ghostel-cmd--cache
    (setq ghostel-cmd--cache (ghostel-cmd--load))))

(defun ghostel-cmd--get-commands (&optional root)
  "Command plists for ROOT."
  (ghostel-cmd--ensure-cache)
  (let ((r (or root (ghostel-cmd--current-project-root))))
    (alist-get r ghostel-cmd--cache nil nil #'equal)))

(defun ghostel-cmd--set-commands (cmds &optional root)
  "Replace commands for ROOT with CMDS and persist."
  (ghostel-cmd--ensure-cache)
  (let ((r (or root (ghostel-cmd--current-project-root))))
    (setq ghostel-cmd--cache
          (assoc-delete-all r ghostel-cmd--cache #'equal))
    (when cmds
      (push (cons r cmds) ghostel-cmd--cache))
    (ghostel-cmd--save)
    cmds))


;;; Templates

(defvar ghostel-cmd--caller-file nil)
(defvar ghostel-cmd--caller-line nil)
(defvar ghostel-cmd--root nil)

(defun ghostel-cmd--builtin-var (var-name root)
  "Auto default for VAR-NAME under ROOT, or nil."
  (pcase var-name
    ((or "root" "project-root")
     (directory-file-name root))
    ((or "project-name" "project")
     (file-name-nondirectory (directory-file-name root)))
    ("branch"
     (or (when (fboundp 'magit-get-current-branch)
           (magit-get-current-branch))
         (when (fboundp 'vc-git--current-branch)
           (vc-git--current-branch))))
    ("file"
     (when-let* ((file (or ghostel-cmd--caller-file buffer-file-name)))
       (file-relative-name file root)))
    ("line"
     (number-to-string (or ghostel-cmd--caller-line (line-number-at-pos))))
    (_ nil)))

(defun ghostel-cmd--extract-variables (template root)
  "Ordered alist of (NAME . DEFAULT) from TEMPLATE."
  (let ((vars nil)
        (start 0))
    (while (string-match ghostel-cmd--var-regexp template start)
      (let* ((name (match-string 1 template))
             (def-val (match-string 2 template))
             (existing (assoc name vars)))
        (if existing
            (when (and (null (cdr existing)) def-val)
              (setcdr existing def-val))
          (push (cons name
                      (or def-val (ghostel-cmd--builtin-var name root)))
                vars)))
      (setq start (match-end 0)))
    (nreverse vars)))

(defun ghostel-cmd--apply (template values)
  "Replace {{name}} in TEMPLATE using VALUES alist."
  (let ((result template))
    (dolist (pair values result)
      (let ((pattern (format "{{\\s-*%s\\(?::[^}]*\\)?\\s-*}}"
                             (regexp-quote (car pair)))))
        (setq result
              (replace-regexp-in-string
               pattern (or (cdr pair) "") result t t))))))

(defun ghostel-cmd--preview (template _root values &optional current-name current-input)
  "TEMPLATE with VALUES applied; CURRENT-NAME shown as CURRENT-INPUT."
  (ghostel-cmd--apply
   template
   (if current-name
       (cons (cons current-name (or current-input ""))
             (cl-remove current-name values :key #'car :test #'equal))
     values)))


;;; Overlay state

(defvar ghostel-cmd--query "")
(defvar ghostel-cmd--filtering nil)
(defvar ghostel-cmd--fill nil
  "Plist while filling: :record :vars :index :values :input :submit.")
(defvar ghostel-cmd--help-visible nil)
(defvar ghostel-cmd--preview-start nil)
(defvar ghostel-cmd--preview-id nil)
(defvar ghostel-cmd--target-width nil)
(defvar ghostel-cmd--posframe-parent nil)
(defvar ghostel-cmd--posframe-fitted-width nil)
(defvar ghostel-cmd--posframe-refitting nil)
(defvar ghostel-cmd--match-count 0)
(defvar ghostel-cmd--total-count 0)

(defconst ghostel-cmd--buffer-name "*ghostel-cmd*")

(defconst ghostel-cmd-sidebar-help-commands
  '(("RET"      "Paste into Ghostel (fill {{vars}} first)")
    ("C-RET"    "Paste and press Enter")
    ("j, k"     "Move")
    ("a"        "Add template")
    ("e"        "Edit at point")
    ("d"        "Delete at point")
    ("/"        "Live-narrow (flex)")
    ("o"        "Open save file")
    ("?"        "This help")
    ("q, Esc"   "Quit / cancel fill"))
  "Key legend for the overlay.")


;;; Flex / table

(defun ghostel-cmd--flex-regexp (query)
  (mapconcat (lambda (ch) (regexp-quote (char-to-string ch)))
             (string-to-list query)
             ".*"))

(defun ghostel-cmd--query-matches-p (record query)
  (or (not query)
      (string-empty-p (string-trim query))
      (let ((hay (mapconcat #'identity
                            (delq nil
                                  (list (plist-get record :name)
                                        (plist-get record :cmd)
                                        (plist-get record :desc)))
                            " "))
            (case-fold-search t))
        (cl-every (lambda (token)
                    (string-match-p (ghostel-cmd--flex-regexp token) hay))
                  (split-string query)))))

(defun ghostel-cmd--available-width ()
  (or ghostel-cmd--target-width
      (when-let* ((buf (get-buffer ghostel-cmd--buffer-name))
                  (win (get-buffer-window buf t)))
        (window-body-width win))
      ghostel-cmd-sidebar-width))

(defun ghostel-cmd--format ()
  "Tabulated-list format for the current width."
  (let* ((budget (max 24 (1- (ghostel-cmd--available-width))))
         (name 16)
         (desc 14)
         (cmd (max 12 (- budget name desc 2))))
    (vector (list "Name" name t)
            (list "Command" cmd t)
            (list "Desc" desc t))))

(defun ghostel-cmd--truncate (s width)
  (truncate-string-to-width (or s "") (max 1 width) nil nil t))

(defun ghostel-cmd--build-entries ()
  "Rebuild `tabulated-list-entries' from the current project."
  (setq tabulated-list-format (ghostel-cmd--format))
  (tabulated-list-init-header)
  (let* ((root (or ghostel-cmd--root (ghostel-cmd--current-project-root)))
         (cmds (or (ghostel-cmd--get-commands root) '()))
         (query ghostel-cmd--query)
         (fmt tabulated-list-format)
         (nw (cadr (aref fmt 0)))
         (cw (cadr (aref fmt 1)))
         (dw (cadr (aref fmt 2)))
         (matched (seq-filter (lambda (c) (ghostel-cmd--query-matches-p c query))
                              cmds)))
    (setq ghostel-cmd--total-count (length cmds)
          ghostel-cmd--match-count (length matched)
          tabulated-list-entries
          (mapcar
           (lambda (c)
             (list (plist-get c :name)
                   (vector
                    (propertize (ghostel-cmd--truncate (plist-get c :name) nw)
                                'face 'font-lock-keyword-face)
                    (ghostel-cmd--truncate (plist-get c :cmd) cw)
                    (propertize (ghostel-cmd--truncate (or (plist-get c :desc) "") dw)
                                'face 'font-lock-comment-face))))
           matched))))

(defun ghostel-cmd--record-at-point ()
  "Command plist for the row at point."
  (when-let* ((name (tabulated-list-get-id))
              (root (or ghostel-cmd--root (ghostel-cmd--current-project-root))))
    (cl-find name (ghostel-cmd--get-commands root)
             :key (lambda (c) (plist-get c :name))
             :test #'equal)))

(defun ghostel-cmd--erase-preview ()
  (when (and ghostel-cmd--preview-start
             (marker-position ghostel-cmd--preview-start))
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (delete-region ghostel-cmd--preview-start (point-max))))
  (setq ghostel-cmd--preview-start nil))

(defun ghostel-cmd--help-text ()
  (let* ((rows ghostel-cmd-sidebar-help-commands)
         (width (apply #'max 3 (mapcar (lambda (row) (length (car row))) rows)))
         (fmt (format "%%-%ds  %%s" width)))
    (mapconcat (lambda (row) (format fmt (car row) (cadr row))) rows "\n")))

(defun ghostel-cmd--preview-text (record)
  "Preview body for RECORD, including fill state."
  (let* ((root (or ghostel-cmd--root (ghostel-cmd--current-project-root)))
         (tmpl (or (plist-get record :cmd) ""))
         (fill ghostel-cmd--fill)
         (width (max 20 (1- (ghostel-cmd--available-width)))))
    (cond
     ((null record)
      (propertize "(no commands — press a to add)" 'face 'shadow))
     (fill
      (let* ((vars (plist-get fill :vars))
             (idx (plist-get fill :index))
             (pair (nth idx vars))
             (name (car pair))
             (input (plist-get fill :input))
             (shown (ghostel-cmd--preview
                     tmpl root (plist-get fill :values) name input)))
        (concat
         (truncate-string-to-width shown width nil nil t)
         "\n"
         (propertize
          (format "{{%s}}  %s"
                  name
                  (if (and input (not (string-empty-p input)))
                      input
                    (or (cdr pair) "")))
          'face 'highlight))))
     (t
      (truncate-string-to-width tmpl width nil nil t)))))

(defun ghostel-cmd--draw-preview ()
  (when (derived-mode-p 'ghostel-cmd-sidebar-mode)
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t)
          (record (and (not ghostel-cmd--help-visible)
                       (ghostel-cmd--record-at-point)))
          (width (max 10 (or (ignore-errors (window-body-width))
                             (ghostel-cmd--available-width)))))
      (save-excursion
        (ghostel-cmd--erase-preview)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (setq ghostel-cmd--preview-start (point-marker))
        (insert (propertize (make-string width ?─) 'face 'shadow) "\n")
        (insert (if ghostel-cmd--help-visible
                    (ghostel-cmd--help-text)
                  (ghostel-cmd--preview-text record)))
        (unless (bolp) (insert "\n")))
      (restore-buffer-modified-p nil))))

(defun ghostel-cmd--print (&optional update)
  (let ((id (tabulated-list-get-id)))
    (ghostel-cmd--erase-preview)
    (tabulated-list-print update)
    (when id
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (equal (tabulated-list-get-id) id)))
        (forward-line 1))
      (when (eobp) (goto-char (point-min))))
    (ghostel-cmd--draw-preview)
    (setq ghostel-cmd--preview-id (tabulated-list-get-id))))

(defun ghostel-cmd--confine-point ()
  (when (and ghostel-cmd--preview-start
             (marker-position ghostel-cmd--preview-start)
             (>= (point) ghostel-cmd--preview-start)
             (> ghostel-cmd--preview-start (point-min)))
    (goto-char (1- ghostel-cmd--preview-start))
    (beginning-of-line)))

(defun ghostel-cmd--preview-on-command ()
  (when (derived-mode-p 'ghostel-cmd-sidebar-mode)
    (ghostel-cmd--confine-point)
    (unless (or ghostel-cmd--help-visible ghostel-cmd--fill)
      (let ((id (tabulated-list-get-id)))
        (unless (equal id ghostel-cmd--preview-id)
          (setq ghostel-cmd--preview-id id)
          (ghostel-cmd--draw-preview))))))

(defun ghostel-cmd--refresh-overlay ()
  (when-let* ((buf (get-buffer ghostel-cmd--buffer-name)))
    (with-current-buffer buf
      (ghostel-cmd--build-entries)
      (ghostel-cmd--print t))
    (when (ghostel-cmd--posframe-showing-p)
      (ghostel-cmd--show-posframe buf))))

(defun ghostel-cmd--footer ()
  (let* ((root (or ghostel-cmd--root (ghostel-cmd--current-project-root)))
         (name (file-name-nondirectory (directory-file-name root)))
         (fill ghostel-cmd--fill)
         (querying (or ghostel-cmd--filtering
                       (and ghostel-cmd--query
                            (not (string-empty-p ghostel-cmd--query))))))
    (concat
     " "
     name
     (when querying
       (format "  /%s  %d/%d"
               ghostel-cmd--query
               ghostel-cmd--match-count
               ghostel-cmd--total-count))
     (cond
      (fill
       (let* ((pair (nth (plist-get fill :index) (plist-get fill :vars)))
              (def (cdr pair)))
         (format "   {{%s}}%s  TAB next  RET paste  Esc cancel"
                 (car pair)
                 (if def (format " [%s]" def) ""))))
      (ghostel-cmd--filtering
       "   RET apply  n/p move  Esc clear")
      (ghostel-cmd--help-visible
       "   ? close help  Esc close")
      (t
       "   RET paste  C-RET send  a add  / filter  ? keys  Esc close")))))


;;; Filter / fill maps

(defun ghostel-cmd--return-event-p (ev)
  (or (memq ev '(return kp-enter newline S-return))
      (and (characterp ev) (memq ev '(?\r ?\n)))
      (memq (event-basic-type ev) '(return kp-enter newline))))

(defun ghostel-cmd-filter-self-insert ()
  (interactive)
  (let ((ev last-command-event))
    (cond
     ((ghostel-cmd--return-event-p ev)
      (ghostel-cmd-filter-confirm))
     ((and (characterp ev) (>= ev 32) (not (eq ev 127)))
      (setq ghostel-cmd--query
            (concat ghostel-cmd--query (char-to-string ev)))
      (ghostel-cmd--refresh-overlay)))))

(defun ghostel-cmd-filter-backspace ()
  (interactive)
  (when (> (length ghostel-cmd--query) 0)
    (setq ghostel-cmd--query (substring ghostel-cmd--query 0 -1))
    (ghostel-cmd--refresh-overlay)))

(defun ghostel-cmd-filter-clear ()
  (interactive)
  (setq ghostel-cmd--query "")
  (ghostel-cmd--refresh-overlay))

(defun ghostel-cmd--set-filtering (on)
  (setq ghostel-cmd--filtering (and on t))
  (when-let* ((buf (get-buffer ghostel-cmd--buffer-name)))
    (with-current-buffer buf
      (ghostel-cmd-filter-mode (if ghostel-cmd--filtering 1 -1))
      (when (fboundp 'evil-normalize-keymaps)
        (evil-normalize-keymaps))))
  (force-mode-line-update t))

(defun ghostel-cmd-filter-confirm ()
  (interactive)
  (ghostel-cmd--set-filtering nil))

(defun ghostel-cmd-filter ()
  (interactive)
  (when ghostel-cmd--fill
    (user-error "Finish or cancel fill first"))
  (ghostel-cmd--set-filtering t))

(defvar-keymap ghostel-cmd-filter-map
  :doc "Live-narrow keymap."
  "RET"         #'ghostel-cmd-filter-confirm
  "C-m"         #'ghostel-cmd-filter-confirm
  "<return>"    #'ghostel-cmd-filter-confirm
  "n"           #'next-line
  "p"           #'previous-line
  "C-n"         #'next-line
  "C-p"         #'previous-line
  "<down>"      #'next-line
  "<up>"        #'previous-line
  "DEL"         #'ghostel-cmd-filter-backspace
  "<backspace>" #'ghostel-cmd-filter-backspace
  "<delete>"    #'ghostel-cmd-filter-backspace
  "C-h"         #'ghostel-cmd-filter-backspace
  "C-u"         #'ghostel-cmd-filter-clear
  "C-g"         #'ghostel-cmd-quit
  "<escape>"    #'ghostel-cmd-quit)

(define-key ghostel-cmd-filter-map [t] #'ghostel-cmd-filter-self-insert)
(define-key ghostel-cmd-filter-map [return] #'ghostel-cmd-filter-confirm)
(define-key ghostel-cmd-filter-map [kp-enter] #'ghostel-cmd-filter-confirm)

(defun ghostel-cmd--bind-filter-query-keys ()
  (let ((c 32))
    (while (< c 127)
      (unless (memq c '(?n ?p 127))
        (define-key ghostel-cmd-filter-map (vector c)
                    #'ghostel-cmd-filter-self-insert))
      (setq c (1+ c)))))

(ghostel-cmd--bind-filter-query-keys)

(define-minor-mode ghostel-cmd-filter-mode
  "Live-narrow the command overlay."
  :lighter nil
  :keymap ghostel-cmd-filter-map)

(defun ghostel-cmd-fill-self-insert ()
  (interactive)
  (let ((ev last-command-event))
    (cond
     ((ghostel-cmd--return-event-p ev)
      (ghostel-cmd-fill-accept nil))
     ((and (characterp ev) (>= ev 32) (not (eq ev 127)))
      (when ghostel-cmd--fill
        (plist-put ghostel-cmd--fill :input
                   (concat (or (plist-get ghostel-cmd--fill :input) "")
                           (char-to-string ev)))
        (ghostel-cmd--draw-preview)
        (force-mode-line-update t))))))

(defun ghostel-cmd-fill-backspace ()
  (interactive)
  (when-let* ((fill ghostel-cmd--fill)
              (input (plist-get fill :input)))
    (when (> (length input) 0)
      (plist-put fill :input (substring input 0 -1))
      (ghostel-cmd--draw-preview)
      (force-mode-line-update t))))

(defun ghostel-cmd-fill-clear ()
  (interactive)
  (when ghostel-cmd--fill
    (plist-put ghostel-cmd--fill :input "")
    (ghostel-cmd--draw-preview)
    (force-mode-line-update t)))

(defun ghostel-cmd--set-filling (on)
  (when-let* ((buf (get-buffer ghostel-cmd--buffer-name)))
    (with-current-buffer buf
      (ghostel-cmd-fill-mode (if on 1 -1))
      (when (fboundp 'evil-normalize-keymaps)
        (evil-normalize-keymaps))))
  (force-mode-line-update t))

(defun ghostel-cmd-fill-cancel ()
  "Leave fill mode without pasting."
  (interactive)
  (setq ghostel-cmd--fill nil)
  (ghostel-cmd--set-filling nil)
  (ghostel-cmd--draw-preview)
  (force-mode-line-update t))

(defun ghostel-cmd--fill-commit-current ()
  "Store the current field's value.  Return non-nil when more vars remain."
  (let* ((fill ghostel-cmd--fill)
         (vars (plist-get fill :vars))
         (idx (plist-get fill :index))
         (pair (nth idx vars))
         (name (car pair))
         (default (or (cdr pair) ""))
         (raw (or (plist-get fill :input) ""))
         (val (if (string-empty-p raw) default raw))
         (values (cons (cons name val)
                       (cl-remove name (plist-get fill :values)
                                  :key #'car :test #'equal)))
         (next (1+ idx)))
    (plist-put fill :values values)
    (if (>= next (length vars))
        nil
      (let ((npair (nth next vars)))
        (plist-put fill :index next)
        (plist-put fill :input (or (cdr npair) ""))
        t))))

(defun ghostel-cmd-fill-accept (&optional submit)
  "Accept the current field; paste when it was the last.
SUBMIT non-nil also presses Enter in the terminal."
  (interactive)
  (when ghostel-cmd--fill
    (when submit
      (plist-put ghostel-cmd--fill :submit t))
    (if (ghostel-cmd--fill-commit-current)
        (progn
          (ghostel-cmd--draw-preview)
          (force-mode-line-update t))
      (let* ((fill ghostel-cmd--fill)
             (record (plist-get fill :record))
             (cmd (ghostel-cmd--apply (plist-get record :cmd)
                                      (plist-get fill :values)))
             (do-submit (plist-get fill :submit)))
        (setq ghostel-cmd--fill nil)
        (ghostel-cmd--set-filling nil)
        (ghostel-cmd--deliver cmd do-submit)))))

(defun ghostel-cmd-fill-accept-send ()
  (interactive)
  (ghostel-cmd-fill-accept t))

(defvar-keymap ghostel-cmd-fill-map
  :doc "Keymap while filling {{vars}}."
  "RET"         #'ghostel-cmd-fill-accept
  "C-m"         #'ghostel-cmd-fill-accept
  "<return>"    #'ghostel-cmd-fill-accept
  "TAB"         #'ghostel-cmd-fill-accept
  "<tab>"       #'ghostel-cmd-fill-accept
  "C-RET"       #'ghostel-cmd-fill-accept-send
  "C-<return>"  #'ghostel-cmd-fill-accept-send
  "DEL"         #'ghostel-cmd-fill-backspace
  "<backspace>" #'ghostel-cmd-fill-backspace
  "<delete>"    #'ghostel-cmd-fill-backspace
  "C-h"         #'ghostel-cmd-fill-backspace
  "C-u"         #'ghostel-cmd-fill-clear
  "C-g"         #'ghostel-cmd-fill-cancel
  "<escape>"    #'ghostel-cmd-fill-cancel)

(define-key ghostel-cmd-fill-map [t] #'ghostel-cmd-fill-self-insert)
(define-key ghostel-cmd-fill-map [return] #'ghostel-cmd-fill-accept)
(define-key ghostel-cmd-fill-map [tab] #'ghostel-cmd-fill-accept)
(define-key ghostel-cmd-fill-map [(control return)] #'ghostel-cmd-fill-accept-send)

(defun ghostel-cmd--bind-fill-query-keys ()
  (let ((c 32))
    (while (< c 127)
      (unless (eq c 127)
        (define-key ghostel-cmd-fill-map (vector c)
                    #'ghostel-cmd-fill-self-insert))
      (setq c (1+ c)))))

(ghostel-cmd--bind-fill-query-keys)

(define-minor-mode ghostel-cmd-fill-mode
  "Fill {{vars}} on the overlay footer."
  :lighter nil
  :keymap ghostel-cmd-fill-map)


;;; Deliver / CRUD

(defun ghostel-cmd--terminal-buffer ()
  (cond
   ((and (fboundp 'project-run-ghostel) (project-current nil))
    (project-run-ghostel))
   ((fboundp 'new-terminal)
    (new-terminal))
   ((fboundp 'ghostel)
    (funcall 'ghostel))
   (t
    (user-error "No Ghostel terminal helper loaded"))))

(defun ghostel-cmd--deliver (cmd-string submit)
  "Paste CMD-STRING into Ghostel.  SUBMIT non-nil presses Enter."
  (ghostel-cmd--hide-overlay)
  (require 'ghostel)
  (let ((term-buf (ghostel-cmd--terminal-buffer)))
    (when (buffer-live-p term-buf)
      (pop-to-buffer term-buf
                     (append display-buffer--same-window-action
                             '((category . comint))))
      (with-current-buffer term-buf
        (ghostel-paste-string cmd-string)
        (when submit
          (ghostel-send-key "return"))))))

(defun ghostel-cmd-paste (&optional submit)
  "Paste the command at point, filling {{vars}} in the overlay first."
  (interactive)
  (when ghostel-cmd--filtering
    (ghostel-cmd--set-filtering nil))
  (let* ((record (ghostel-cmd--record-at-point))
         (root (or ghostel-cmd--root (ghostel-cmd--current-project-root))))
    (unless record
      (user-error "No command at point"))
    (let ((vars (ghostel-cmd--extract-variables (plist-get record :cmd) root)))
      (if (null vars)
          (ghostel-cmd--deliver (plist-get record :cmd) submit)
        (setq ghostel-cmd--fill
              (list :record record
                    :vars vars
                    :index 0
                    :values nil
                    :input (or (cdar vars) "")
                    :submit submit))
        (ghostel-cmd--set-filling t)
        (ghostel-cmd--draw-preview)
        (force-mode-line-update t)))))

(defun ghostel-cmd-paste-send ()
  (interactive)
  (ghostel-cmd-paste t))

(defun ghostel-cmd--with-parent-input (fn)
  "Run FN on the parent frame so minibuffer prompts work, then reopen."
  (let ((parent ghostel-cmd--posframe-parent)
        (root ghostel-cmd--root)
        (file ghostel-cmd--caller-file)
        (line ghostel-cmd--caller-line))
    (ghostel-cmd--hide-posframe)
    (when (and parent (frame-live-p parent))
      (select-frame-set-input-focus parent))
    (unwind-protect
        (funcall fn)
      (setq ghostel-cmd--root root
            ghostel-cmd--caller-file file
            ghostel-cmd--caller-line line)
      (when (and root (not (minibufferp)))
        (ghostel-cmd-sidebar)))))

(defun ghostel-cmd-add (&optional initial-cmd)
  "Add a command template.  Region text is the default command."
  (interactive
   (list (when (use-region-p)
           (buffer-substring-no-properties (region-beginning) (region-end)))))
  (let ((run
         (lambda ()
           (let* ((root (or ghostel-cmd--root
                            (ghostel-cmd--current-project-root)))
                  (cmd (read-string "Command template: " (or initial-cmd "")))
                  (suggested
                   (when (string-match "\\`[ \t]*\\([a-zA-Z0-9_-]+\\)" cmd)
                     (match-string 1 cmd)))
                  (name (read-string "Name / alias: " nil nil suggested))
                  (desc (read-string "Description (optional): "))
                  (existing (ghostel-cmd--get-commands root)))
             (when (string-empty-p name)
               (user-error "Command name cannot be empty"))
             (when (string-empty-p cmd)
               (user-error "Command cannot be empty"))
             (ghostel-cmd--set-commands
              (append
               (cl-remove name existing
                          :key (lambda (c) (plist-get c :name))
                          :test #'equal)
               (list (list :name name :cmd cmd :desc desc)))
              root)
             (message "Added '%s'" name)))))
    (if (ghostel-cmd--posframe-showing-p)
        (ghostel-cmd--with-parent-input run)
      (funcall run)
      (when (get-buffer ghostel-cmd--buffer-name)
        (ghostel-cmd--refresh-overlay)))))

(defun ghostel-cmd-edit ()
  "Edit the command at point."
  (interactive)
  (let ((record (ghostel-cmd--record-at-point)))
    (unless record
      (user-error "No command at point"))
    (let ((run
           (lambda ()
             (let* ((root (or ghostel-cmd--root
                              (ghostel-cmd--current-project-root)))
                    (old-name (plist-get record :name))
                    (new-name (read-string "Name / alias: " old-name))
                    (new-cmd (read-string "Command template: "
                                          (plist-get record :cmd)))
                    (new-desc (read-string "Description (optional): "
                                           (or (plist-get record :desc) "")))
                    (cmds (ghostel-cmd--get-commands root)))
               (when (string-empty-p new-name)
                 (user-error "Command name cannot be empty"))
               (when (string-empty-p new-cmd)
                 (user-error "Command cannot be empty"))
               (ghostel-cmd--set-commands
                (append
                 (cl-remove old-name cmds
                            :key (lambda (c) (plist-get c :name))
                            :test #'equal)
                 (list (list :name new-name :cmd new-cmd :desc new-desc)))
                root)
               (message "Updated '%s'" new-name)))))
      (if (ghostel-cmd--posframe-showing-p)
          (ghostel-cmd--with-parent-input run)
        (funcall run)
        (ghostel-cmd--refresh-overlay)))))

(defun ghostel-cmd-delete ()
  "Delete the command at point."
  (interactive)
  (let ((record (ghostel-cmd--record-at-point)))
    (unless record
      (user-error "No command at point"))
    (let ((run
           (lambda ()
             (let* ((root (or ghostel-cmd--root
                              (ghostel-cmd--current-project-root)))
                    (name (plist-get record :name)))
               (when (yes-or-no-p (format "Delete '%s'? " name))
                 (ghostel-cmd--set-commands
                  (cl-remove name (ghostel-cmd--get-commands root)
                             :key (lambda (c) (plist-get c :name))
                             :test #'equal)
                  root)
                 (message "Deleted '%s'" name))))))
      (if (ghostel-cmd--posframe-showing-p)
          (ghostel-cmd--with-parent-input run)
        (funcall run)
        (ghostel-cmd--refresh-overlay)))))

(defun ghostel-cmd-open-save-file ()
  "Visit the on-disk bookmark file."
  (interactive)
  (ghostel-cmd--hide-overlay)
  (find-file ghostel-cmd-save-file))


;;; Posframe / side window

(defun ghostel-cmd--posframe-available-p ()
  (and (require 'posframe nil t)
       (fboundp 'posframe-workable-p)
       (posframe-workable-p)))

(defun ghostel-cmd--use-posframe-p ()
  (and ghostel-cmd-use-posframe
       (ghostel-cmd--posframe-available-p)))

(defun ghostel-cmd--posframe-frame ()
  (when-let* ((buf (get-buffer ghostel-cmd--buffer-name)))
    (and (boundp 'posframe--frame)
         (buffer-local-value 'posframe--frame buf))))

(defun ghostel-cmd--posframe-showing-p ()
  (when-let* ((frame (ghostel-cmd--posframe-frame)))
    (and (frame-live-p frame)
         (frame-visible-p frame))))

(defun ghostel-cmd--hide-posframe ()
  (let ((parent ghostel-cmd--posframe-parent)
        (buf (get-buffer ghostel-cmd--buffer-name)))
    (setq ghostel-cmd--posframe-parent nil
          ghostel-cmd--posframe-fitted-width nil
          ghostel-cmd--fill nil
          ghostel-cmd--help-visible nil)
    (ghostel-cmd--set-filtering nil)
    (ghostel-cmd--set-filling nil)
    (when (and buf (fboundp 'posframe-hide))
      (posframe-hide buf))
    (when (and parent (frame-live-p parent))
      (select-frame-set-input-focus parent))))

(defun ghostel-cmd--hide-overlay ()
  (if (ghostel-cmd--posframe-showing-p)
      (ghostel-cmd--hide-posframe)
    (when-let* ((buf (get-buffer ghostel-cmd--buffer-name))
                (win (get-buffer-window buf)))
      (quit-window nil win))))

(defun ghostel-cmd-quit ()
  "Clear query, cancel fill, or dismiss the overlay."
  (interactive)
  (cond
   (ghostel-cmd--fill
    (ghostel-cmd-fill-cancel))
   ((and ghostel-cmd--query (not (string-empty-p ghostel-cmd--query)))
    (setq ghostel-cmd--query "")
    (ghostel-cmd--set-filtering nil)
    (ghostel-cmd--refresh-overlay))
   (ghostel-cmd--help-visible
    (setq ghostel-cmd--help-visible nil)
    (ghostel-cmd--redraw)
    (force-mode-line-update t))
   (t
    (setq ghostel-cmd--query "")
    (ghostel-cmd--set-filtering nil)
    (if (ghostel-cmd--posframe-showing-p)
        (ghostel-cmd--hide-posframe)
      (quit-window)))))

(defun ghostel-cmd--redraw ()
  "Redraw the preview pane and refit a visible posframe."
  (when-let* ((buf (get-buffer ghostel-cmd--buffer-name)))
    (with-current-buffer buf
      (when (derived-mode-p 'ghostel-cmd-sidebar-mode)
        (ghostel-cmd--draw-preview)))
    (when (ghostel-cmd--posframe-showing-p)
      (ghostel-cmd--show-posframe buf))))

(defun ghostel-cmd-help ()
  "Toggle the key legend in the overlay preview pane."
  (interactive)
  (setq ghostel-cmd--help-visible (not ghostel-cmd--help-visible))
  (ghostel-cmd--redraw)
  (force-mode-line-update t))

(defun ghostel-cmd--close-side-windows (buf)
  (dolist (win (get-buffer-window-list buf nil t))
    (when (and (window-parameter win 'window-side)
               (not (frame-parent (window-frame win))))
      (ignore-errors (delete-window win)))))

(defun ghostel-cmd--posframe-border-color ()
  (or (face-foreground 'vertical-border nil t)
      (face-foreground 'mode-line-inactive nil t)
      "gray50"))

(defun ghostel-cmd--posframe-parent-frame ()
  (or (and (frame-live-p ghostel-cmd--posframe-parent)
           ghostel-cmd--posframe-parent)
      (frame-parent (selected-frame))
      (selected-frame)))

(defun ghostel-cmd--posframe-char-width (&optional frame)
  (let* ((frame (or frame (ghostel-cmd--posframe-parent-frame)))
         (avail (max 1 (frame-width frame)))
         (margin (min 12 (max 4 (/ avail 16))))
         (ceil (max 48 (- avail margin)))
         (floor (min ghostel-cmd-posframe-width ceil)))
    (if (null ghostel-cmd-posframe-width-ratio)
        floor
      (let ((wanted (round (* ghostel-cmd-posframe-width-ratio avail))))
        (max floor (min wanted ceil))))))

(defun ghostel-cmd--posframe-max-height (&optional frame)
  (let* ((frame (or frame (ghostel-cmd--posframe-parent-frame)))
         (avail (max 8 (frame-height frame))))
    (max 8 (min (- avail 4) (round (* 0.6 avail))))))

(defun ghostel-cmd--preview-reserved-lines ()
  (if ghostel-cmd--help-visible
      (max 8 (length (split-string (ghostel-cmd--help-text) "\n")))
    5))

(defun ghostel-cmd--show-posframe (buf)
  (ghostel-cmd--close-side-windows buf)
  (let* ((parent (ghostel-cmd--posframe-parent-frame))
         (width (or ghostel-cmd--target-width
                    (ghostel-cmd--posframe-char-width parent)))
         (maxh (ghostel-cmd--posframe-max-height parent))
         (preview (ghostel-cmd--preview-reserved-lines))
         (rows (with-current-buffer buf
                 (max 1 (length tabulated-list-entries))))
         (table (max 4 (min rows (max 4 (- maxh preview 1)))))
         (height (+ 1 table preview)))
    (setq ghostel-cmd--posframe-parent parent
          ghostel-cmd--posframe-fitted-width width)
    (add-hook 'window-size-change-functions
              #'ghostel-cmd--posframe-on-parent-resize)
    (with-selected-frame parent
      (posframe-show
       buf
       :poshandler #'posframe-poshandler-frame-center
       :position (point)
       :width width
       :min-width (min 48 width)
       :height height
       :min-height height
       :max-height (max height maxh)
       :left-fringe 12
       :right-fringe 12
       :border-width 2
       :border-color (ghostel-cmd--posframe-border-color)
       :respect-header-line t
       :respect-mode-line t
       :lines-truncate t
       :cursor 'box
       :accept-focus t
       :window-point (with-current-buffer buf (point))))
    (when-let* ((frame (ghostel-cmd--posframe-frame))
                (win (get-buffer-window buf frame)))
      (select-frame-set-input-focus frame)
      (select-window win))))

(defun ghostel-cmd--posframe-on-parent-resize (frame)
  (when (and (not ghostel-cmd--posframe-refitting)
             ghostel-cmd--posframe-parent
             (eq frame ghostel-cmd--posframe-parent)
             (ghostel-cmd--posframe-showing-p))
    (let ((width (ghostel-cmd--posframe-char-width frame)))
      (unless (eql width ghostel-cmd--posframe-fitted-width)
        (let ((ghostel-cmd--posframe-refitting t)
              (ghostel-cmd--target-width width))
          (setq ghostel-cmd--posframe-fitted-width width)
          (when-let* ((buf (get-buffer ghostel-cmd--buffer-name)))
            (ghostel-cmd--refresh-overlay)))))))

(defun ghostel-cmd--show-side-window (buf)
  (when (fboundp 'posframe-hide)
    (posframe-hide buf))
  (setq ghostel-cmd--posframe-parent nil)
  (pop-to-buffer
   buf
   `((display-buffer-in-side-window)
     (side . ,ghostel-cmd-sidebar-side)
     (slot . 0)
     (window-width . ,ghostel-cmd-sidebar-width)
     (preserve-size . (t . nil)))))

(defun ghostel-cmd--prepare-buffer ()
  (let ((buf (get-buffer-create ghostel-cmd--buffer-name)))
    (with-current-buffer buf
      (setq-local default-directory
                  (or ghostel-cmd--root default-directory))
      (unless (derived-mode-p 'ghostel-cmd-sidebar-mode)
        (ghostel-cmd-sidebar-mode))
      (ghostel-cmd--build-entries)
      (ghostel-cmd--print t))
    buf))

;;;###autoload
(defun ghostel-cmd-sidebar ()
  "Open the project command overlay.

Posframe when available, side window otherwise.  RET pastes into
Ghostel after filling {{vars}} on the overlay footer.  C-RET also
presses Enter."
  (interactive)
  (setq ghostel-cmd--root (ghostel-cmd--current-project-root)
        ghostel-cmd--caller-file buffer-file-name
        ghostel-cmd--caller-line (line-number-at-pos)
        ghostel-cmd--query ""
        ghostel-cmd--help-visible nil
        ghostel-cmd--fill nil
        ghostel-cmd--preview-id nil)
  (ghostel-cmd--set-filtering nil)
  (let* ((overlay (ghostel-cmd--use-posframe-p))
         (ghostel-cmd--target-width
          (and overlay (ghostel-cmd--posframe-char-width)))
         (buf (ghostel-cmd--prepare-buffer)))
    (if overlay
        (ghostel-cmd--show-posframe buf)
      (ghostel-cmd--show-side-window buf))
    buf))


;;; Mode / keys

(defvar-keymap ghostel-cmd-sidebar-mode-map
  :doc "Keymap for the command overlay."
  "?"          #'ghostel-cmd-help
  "n"          #'next-line
  "p"          #'previous-line
  "RET"        #'ghostel-cmd-paste
  "<return>"   #'ghostel-cmd-paste
  "C-RET"      #'ghostel-cmd-paste-send
  "C-<return>" #'ghostel-cmd-paste-send
  "a"          #'ghostel-cmd-add
  "e"          #'ghostel-cmd-edit
  "d"          #'ghostel-cmd-delete
  "/"          #'ghostel-cmd-filter
  "o"          #'ghostel-cmd-open-save-file
  "g"          #'tabulated-list-revert
  "q"          #'ghostel-cmd-quit
  "C-g"        #'ghostel-cmd-quit
  "<escape>"   #'ghostel-cmd-quit)

(defun ghostel-cmd--maybe-setup-evil ()
  (when (fboundp 'evil-make-intercept-map)
    (evil-make-intercept-map ghostel-cmd-filter-map 'normal)
    (evil-make-intercept-map ghostel-cmd-fill-map 'normal))
  (when (fboundp 'evil-define-key*)
    (evil-define-key* 'normal ghostel-cmd-filter-map
      (kbd "RET") #'ghostel-cmd-filter-confirm
      (kbd "<return>") #'ghostel-cmd-filter-confirm
      (kbd "C-m") #'ghostel-cmd-filter-confirm
      (kbd "<escape>") #'ghostel-cmd-quit)
    (evil-define-key* 'normal ghostel-cmd-fill-map
      (kbd "RET") #'ghostel-cmd-fill-accept
      (kbd "<return>") #'ghostel-cmd-fill-accept
      (kbd "TAB") #'ghostel-cmd-fill-accept
      (kbd "<tab>") #'ghostel-cmd-fill-accept
      (kbd "C-RET") #'ghostel-cmd-fill-accept-send
      (kbd "C-<return>") #'ghostel-cmd-fill-accept-send
      (kbd "<escape>") #'ghostel-cmd-fill-cancel)
    (evil-define-key* 'normal ghostel-cmd-sidebar-mode-map
      (kbd "?") #'ghostel-cmd-help
      (kbd "RET") #'ghostel-cmd-paste
      (kbd "<return>") #'ghostel-cmd-paste
      (kbd "C-RET") #'ghostel-cmd-paste-send
      (kbd "C-<return>") #'ghostel-cmd-paste-send
      (kbd "a") #'ghostel-cmd-add
      (kbd "e") #'ghostel-cmd-edit
      (kbd "d") #'ghostel-cmd-delete
      (kbd "/") #'ghostel-cmd-filter
      (kbd "o") #'ghostel-cmd-open-save-file
      (kbd "gr") #'tabulated-list-revert
      (kbd "q") #'ghostel-cmd-quit
      (kbd "<escape>") #'ghostel-cmd-quit)))

(with-eval-after-load 'evil
  (ghostel-cmd--maybe-setup-evil))

(define-derived-mode ghostel-cmd-sidebar-mode tabulated-list-mode "Ghostel-cmd"
  "Listing of per-project Ghostel command templates."
  (setq tabulated-list-padding 1)
  (setq tabulated-list-format (ghostel-cmd--format))
  (setq truncate-lines t)
  (setq-local mode-line-format '("" (:eval (ghostel-cmd--footer))))
  (hl-line-mode 1)
  (add-hook 'tabulated-list-revert-hook #'ghostel-cmd--build-entries nil t)
  (add-hook 'post-command-hook #'ghostel-cmd--preview-on-command nil t)
  (when (fboundp 'evil-normalize-keymaps)
    (evil-normalize-keymaps))
  (tabulated-list-init-header))


;;; Aliases (SPC p c / C-c c)

(defalias 'ghostel-project-cmd-run 'ghostel-cmd-sidebar)
(defalias 'ghostel-project-cmd-add 'ghostel-cmd-add)
(defalias 'ghostel-project-cmd-edit 'ghostel-cmd-edit)
(defalias 'ghostel-project-cmd-delete 'ghostel-cmd-delete)
(defalias 'ghostel-project-cmd-open-save-file 'ghostel-cmd-open-save-file)
(defalias 'ghostel-project-cmd--current-project-root
  'ghostel-cmd--current-project-root)

(provide 'ghostel-cmd)
;;; ghostel-cmd.el ends here

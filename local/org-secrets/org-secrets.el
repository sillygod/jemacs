;;; org-secrets.el --- Org+gpg password manager with a posframe overlay -*- lexical-binding: t; -*-

;; Author: Jing
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience, outlines
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Password vault stored as org headlines in a gpg-encrypted file.
;; The overlay (posframe when available, side window otherwise) is the
;; main interface; `org-secrets' is a consult/completing-read jump that
;; copies the password.
;;
;; Headline schema (old files keep working without the new keys):
;;
;;   * Title                          :tag:
;;     :PROPERTIES:
;;     :DOMAIN: example.com
;;     :USER: name
;;     :URL: https://example.com/login
;;     :SECRET: <base64>
;;     :ENCODING: base64
;;     :ID: <org-id>
;;     :END:
;;     optional notes
;;
;; SECRET is never written into the overlay buffer.  Preview shows
;; metadata and notes only.
;;
;; Main entry points:
;;   M-x org-secrets-sidebar
;;   M-x org-secrets
;;   M-x org-secrets-new
;;   M-x org-secrets-insert-password

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-id)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)

(declare-function consult--read "consult")
(declare-function posframe-workable-p "posframe")
(declare-function posframe-show "posframe")
(declare-function posframe-hide "posframe")
(declare-function posframe-refresh "posframe")
(declare-function posframe-poshandler-frame-center "posframe")
(declare-function evil-define-key* "evil")
(declare-function evil-make-intercept-map "evil")


;;; Customization

(defgroup org-secrets nil
  "Org-based password manager with a posframe overlay."
  :group 'applications
  :prefix "org-secrets-")

(defcustom org-secrets-file nil
  "Primary vault file (org, usually `.gpg').
Nil means `management/learning.org.gpg' under `jemacs-org-root',
or ~/org/management/learning.org.gpg if that variable is unbound."
  :type '(choice (const :tag "Default under jemacs-org-root" nil)
                 (file :tag "Vault"))
  :group 'org-secrets)

(defcustom org-secrets-extra-files nil
  "Additional vault files, listed after `org-secrets-file'.
Intended for a later work/personal split."
  :type '(repeat file)
  :group 'org-secrets)

(defcustom org-secrets-heading-level 1
  "Org heading level used by `org-secrets-new'."
  :type 'integer
  :group 'org-secrets)

(defcustom org-secrets-password-length 20
  "Default length for generated passwords."
  :type 'integer
  :group 'org-secrets)

(defcustom org-secrets-password-symbols "!@#$%^&*_-+="
  "Symbol characters included in generated passwords."
  :type 'string
  :group 'org-secrets)

(defcustom org-secrets-sidebar-side 'left
  "Side window side when not using posframe."
  :type '(choice (const left) (const right))
  :group 'org-secrets)

(defcustom org-secrets-sidebar-width 36
  "Width of the secrets side window.
Used only when the list is shown as a side window."
  :type 'integer
  :group 'org-secrets)

(defcustom org-secrets-sidebar-use-posframe t
  "Show the secrets list in a posframe overlay when possible.
Nil forces the side window even when posframe is available."
  :type 'boolean
  :group 'org-secrets)

(defcustom org-secrets-sidebar-show-preview t
  "Show metadata and notes for the row at point under the list.
`v' in the list toggles this.  The secret value is never shown."
  :type 'boolean
  :group 'org-secrets)

(defcustom org-secrets-sidebar-preview-lines 14
  "Lines budgeted for the overlay preview pane."
  :type 'integer
  :group 'org-secrets)

(defcustom org-secrets-sidebar-posframe-width 72
  "Minimum character width of the secrets posframe overlay.
The overlay grows with the parent frame (see
`org-secrets-sidebar-posframe-width-ratio'); this is the floor."
  :type 'integer
  :group 'org-secrets)

(defcustom org-secrets-sidebar-posframe-width-ratio 0.6
  "Fraction of the parent frame's width used by the overlay.
Nil means a fixed `org-secrets-sidebar-posframe-width'."
  :type '(choice (const :tag "Fixed width" nil)
                 (number :tag "Fraction of frame"))
  :group 'org-secrets)


;;; Data

(cl-defstruct (org-secrets-entry
               (:constructor org-secrets-entry-create)
               (:copier nil))
  uid file org-id title domain user url secret notes tags)

(defvar org-secrets-vault-filter nil
  "When non-nil, an expanded file path; only that vault is listed.")

(defvar org-secrets--cache nil
  "Alist of (FILE . (MTIME . ENTRIES)).
ENTRIES is a list of `org-secrets-entry'.  Secrets live only here,
never as text in the overlay buffer.")

(defconst org-secrets--alnum
  (concat "abcdefghijklmnopqrstuvwxyz"
          "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
          "0123456789")
  "Alphanumeric charset for generated passwords.")


;;; Vault files

(defun org-secrets--primary-file ()
  "Return the expanded primary vault path."
  (expand-file-name
   (or org-secrets-file
       (expand-file-name
        "management/learning.org.gpg"
        (if (and (boundp 'jemacs-org-root) jemacs-org-root)
            jemacs-org-root
          (expand-file-name "~/org/"))))))

(defun org-secrets-vault-files ()
  "Return vault files: primary first, then `org-secrets-extra-files'.
Missing extra files are kept in the list so a future vault can be
created; listing skips files that do not exist yet."
  (let ((files (cons (org-secrets--primary-file)
                     (mapcar #'expand-file-name org-secrets-extra-files))))
    (delete-dups files)))

(defun org-secrets--active-files ()
  "Vault files honoring `org-secrets-vault-filter'."
  (if org-secrets-vault-filter
      (list (expand-file-name org-secrets-vault-filter))
    (org-secrets-vault-files)))

(defun org-secrets--write-file ()
  "Vault that `org-secrets-new' writes to."
  (or org-secrets-vault-filter (org-secrets--primary-file)))


;;; Encode / decode

(defun org-secrets--looks-like-base64-p (s)
  "Return non-nil if S looks like a complete base64 string."
  (and (stringp s)
       (>= (length s) 4)
       (zerop (% (length s) 4))
       (string-match-p "\\`[A-Za-z0-9+/]+=*\\'" s)))

(defun org-secrets--decode-base64 (raw)
  "Decode RAW as utf-8 bytes stored in base64."
  (decode-coding-string (base64-decode-string raw) 'utf-8))

(defun org-secrets--decode-secret (raw &optional encoding)
  "Decode RAW from a SECRET property according to ENCODING.
Historical entries have no ENCODING: base64 is tried when RAW
looks like it, otherwise RAW is treated as plaintext."
  (when (and raw (not (string-empty-p (setq raw (string-trim raw)))))
    (pcase encoding
      ("plain" raw)
      ("base64" (org-secrets--decode-base64 raw))
      (_
       (if (org-secrets--looks-like-base64-p raw)
           (or (ignore-errors (org-secrets--decode-base64 raw)) raw)
         raw)))))

(defun org-secrets--encode-secret (plain)
  "Encode PLAIN for a SECRET property (utf-8, base64, no line breaks)."
  (base64-encode-string (encode-coding-string plain 'utf-8) t))


;;; Parse

(defun org-secrets--entry-notes ()
  "Return the body of the org entry at point, without meta drawers.

`org-end-of-meta-data' with FULL skips blank lines after the
property drawer, so an entry with no body lands on the *next*
heading.  Calling `outline-next-heading' from there would skip
that sibling and swallow it as notes -- which is why a heading
with only a drawer leaked the following entry into the preview."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion
                 (if (outline-next-heading)
                     (point)
                   (point-max)))))
      (org-end-of-meta-data t)
      (when (< (point) end)
        (let ((notes (string-trim (buffer-substring-no-properties (point) end))))
          (and (not (string-empty-p notes)) notes))))))

(defun org-secrets--entry-at-point (file)
  "Build an `org-secrets-entry' from the heading at point in FILE.
Headings without DOMAIN/USER/URL/SECRET are ignored so category
parents do not become rows.  Commented and archived headings are
skipped."
  (unless (or (org-in-commented-heading-p)
              (member org-archive-tag (org-get-tags nil t)))
    (let* ((title (org-get-heading t t t t))
           (domain (org-entry-get nil "DOMAIN"))
           (user (org-entry-get nil "USER"))
           (url (org-entry-get nil "URL"))
           (raw (org-entry-get nil "SECRET"))
           (encoding (org-entry-get nil "ENCODING"))
           (org-id (org-entry-get nil "ID"))
           (tags (org-get-tags))
           (notes (org-secrets--entry-notes)))
      (when (and title (or domain user url raw))
        (org-secrets-entry-create
         :uid (format "%s@%d" (expand-file-name file) (point))
         :file (expand-file-name file)
         :org-id org-id
         :title title
         :domain domain
         :user user
         :url url
         :secret (org-secrets--decode-secret raw encoding)
         :notes (and notes (not (string-empty-p notes)) notes)
         :tags tags)))))

(defun org-secrets--parse-buffer (file)
  "Parse the current org buffer as vault FILE and return entries."
  (let (entries)
    (org-with-wide-buffer
     (org-map-entries
      (lambda ()
        (when-let* ((entry (org-secrets--entry-at-point file)))
          (push entry entries)))))
    (nreverse entries)))

(defun org-secrets--insert-file (file)
  "Insert FILE into the current buffer, decrypting `.gpg' via epa."
  (when (string-suffix-p ".gpg" file)
    (require 'epa-file))
  (insert-file-contents file))

(defun org-secrets--parse-file (file)
  "Read and parse vault FILE.  Return a list of entries."
  (with-temp-buffer
    (org-secrets--insert-file file)
    (let ((org-element-use-cache nil)
          (org-startup-folded t)
          (org-startup-with-inline-images nil))
      (delay-mode-hooks (org-mode))
      (org-secrets--parse-buffer file))))

(defun org-secrets--file-mtime (file)
  "Return FILE's modification time, or nil if it does not exist."
  (when (file-exists-p file)
    (file-attribute-modification-time (file-attributes file))))

(defun org-secrets--invalidate (file)
  "Drop FILE from `org-secrets--cache'."
  (setq org-secrets--cache
        (assoc-delete-all (expand-file-name file) org-secrets--cache)))

;;;###autoload
(defun org-secrets-forget ()
  "Drop every cached vault from memory."
  (interactive)
  (setq org-secrets--cache nil)
  (message "Forgot cached secrets"))

(defun org-secrets--load-file (file &optional force)
  "Return entries for FILE, using the mtime cache unless FORCE.
A visiting buffer that is modified is parsed in place so a visit
and edit is visible without saving first."
  (let* ((file (expand-file-name file))
         (buf (find-buffer-visiting file)))
    (cond
     ((not (file-exists-p file))
      nil)
     ((and buf (buffer-modified-p buf))
      (with-current-buffer buf
        (unless (derived-mode-p 'org-mode)
          (delay-mode-hooks (org-mode)))
        (org-secrets--parse-buffer file)))
     (t
      (let* ((mtime (org-secrets--file-mtime file))
             (cached (assoc file org-secrets--cache)))
        (if (and (not force) cached (equal (cadr cached) mtime))
            (cddr cached)
          (let ((entries (org-secrets--parse-file file)))
            (setq org-secrets--cache
                  (cons (cons file (cons mtime entries))
                        (assoc-delete-all file org-secrets--cache)))
            entries)))))))

(defun org-secrets--entries (&optional force)
  "Return entries from the active vault files.
FORCE re-reads from disk."
  (cl-loop for file in (org-secrets--active-files)
           append (org-secrets--load-file file force)))

(defun org-secrets--entry-by-uid (uid)
  "Return the cached entry with UID, or nil."
  (cl-find uid (org-secrets--entries) :key #'org-secrets-entry-uid :test #'equal))


;;; Password generator

(defun org-secrets--random-bytes (n)
  "Return a unibyte string of N random bytes."
  (if (file-readable-p "/dev/urandom")
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally "/dev/urandom" nil nil n)
        (buffer-string))
    (let ((s (make-string n 0))
          (i 0))
      (while (< i n)
        (aset s i (random 256))
        (setq i (1+ i)))
      s)))

(defun org-secrets-random-password (&optional length symbols)
  "Generate a password of LENGTH characters.
SYMBOLS defaults to `org-secrets-password-symbols'.  Mapping from
random bytes onto the charset uses rejection sampling so it is
unbiased."
  (let* ((len (or length org-secrets-password-length))
         (charset (concat org-secrets--alnum
                          (or symbols org-secrets-password-symbols)))
         (n (length charset))
         (limit (- 256 (% 256 n)))
         (out (make-string len 0))
         (i 0)
         (bytes (org-secrets--random-bytes (max 64 (* len 2))))
         (bi 0))
    (when (< n 2)
      (error "Password charset is too small"))
    (while (< i len)
      (when (>= bi (length bytes))
        (setq bytes (org-secrets--random-bytes (max 64 (* len 2)))
              bi 0))
      (let ((b (aref bytes bi)))
        (setq bi (1+ bi))
        (when (< b limit)
          (aset out i (aref charset (% b n)))
          (setq i (1+ i)))))
    out))

;;;###autoload
(defun org-secrets-insert-password (length)
  "Prompt for LENGTH and insert a generated password at point."
  (interactive "nPassword length: ")
  (insert (org-secrets-random-password length)))


;;; Copy / visit / browse

(defun org-secrets--copy (value what &optional title)
  "Put VALUE on the kill ring.  WHAT is a noun used in the echo.
TITLE is appended as `for TITLE' when non-nil.  VALUE itself is
never echoed."
  (unless (and value (not (string-empty-p value)))
    (user-error "No %s to copy" what))
  (kill-new value)
  (message "Copied %s%s" what
           (if title (format " for %s" title) "")))

(defun org-secrets-copy-password (entry)
  "Copy ENTRY's password to the kill ring."
  (org-secrets--copy (org-secrets-entry-secret entry) "password"
                     (org-secrets-entry-title entry)))

(defun org-secrets-copy-user (entry)
  "Copy ENTRY's user to the kill ring."
  (org-secrets--copy (org-secrets-entry-user entry) "user"
                     (org-secrets-entry-title entry)))

(defun org-secrets-copy-url (entry)
  "Copy ENTRY's URL, falling back to DOMAIN."
  (org-secrets--copy (or (org-secrets-entry-url entry)
                         (org-secrets-entry-domain entry))
                     "url"
                     (org-secrets-entry-title entry)))

(defun org-secrets-copy-login (entry)
  "Copy `user TAB password' for ENTRY."
  (let ((user (org-secrets-entry-user entry))
        (pass (org-secrets-entry-secret entry)))
    (unless (and user (not (string-empty-p user)))
      (user-error "No user to copy"))
    (unless (and pass (not (string-empty-p pass)))
      (user-error "No password to copy"))
    (kill-new (concat user "\t" pass))
    (message "Copied user and password for %s"
             (or (org-secrets-entry-title entry) "entry"))))

(defun org-secrets--browse-target (entry)
  "Return a URL for ENTRY, or nil."
  (let ((url (org-secrets-entry-url entry))
        (domain (org-secrets-entry-domain entry)))
    (cond
     ((and url (not (string-empty-p url)))
      (if (string-match-p "://" url) url (concat "https://" url)))
     ((and domain (not (string-empty-p domain)))
      (concat "https://" domain)))))

(defun org-secrets-browse (entry)
  "Open ENTRY's URL (or https://DOMAIN) in a browser."
  (let ((url (org-secrets--browse-target entry)))
    (unless url
      (user-error "No URL or domain to open"))
    (browse-url url)))

(defun org-secrets--goto-title-domain (title domain)
  "Move to the heading matching TITLE and DOMAIN in the current buffer.
Return the point, or nil."
  (let (found)
    (org-with-wide-buffer
     (org-map-entries
      (lambda ()
        (when (and (not found)
                   (equal (org-get-heading t t t t) title)
                   (equal (org-entry-get nil "DOMAIN") domain))
          (setq found (point))))))
    (when found
      (goto-char found))
    found))

(defun org-secrets-visit (entry)
  "Visit ENTRY's heading in the vault file."
  (find-file (org-secrets-entry-file entry))
  (let ((id (org-secrets-entry-org-id entry)))
    (or (and id (org-find-entry-with-id id))
        (org-secrets--goto-title-domain
         (org-secrets-entry-title entry)
         (org-secrets-entry-domain entry))
        (user-error "Could not find heading %s"
                    (org-secrets-entry-title entry))))
  (if (fboundp 'org-fold-show-entry)
      (progn
        (org-fold-show-entry)
        (org-fold-show-children))
    (with-no-warnings
      (org-show-entry)
      (org-show-children)))
  (org-reveal t))


;;; Write

(defun org-secrets--set-prop (key value)
  "Set org property KEY to VALUE when VALUE is non-empty."
  (when (and value (not (string-empty-p value)))
    (org-set-property key value)))

(defun org-secrets--parse-tags (s)
  "Split S into a list of org tags."
  (when (and s (not (string-empty-p (string-trim s))))
    (split-string s "[,:[:space:]]+" t)))

(defun org-secrets--with-vault-file (file fn)
  "Call FN with FILE current in an org buffer, then save.
A buffer already visiting FILE is reused and left open."
  (let* ((file (expand-file-name file))
         (existing (find-buffer-visiting file))
         (buf (or existing (find-file-noselect file))))
    (unwind-protect
        (with-current-buffer buf
          (unless (derived-mode-p 'org-mode)
            (delay-mode-hooks (org-mode)))
          (funcall fn)
          (save-buffer))
      (unless existing
        (kill-buffer buf)))))

(defun org-secrets--append-entry (file spec)
  "Append a heading described by SPEC to FILE.
SPEC is a plist with :title :domain :user :url :secret :notes :tags."
  (org-secrets--with-vault-file
   file
   (lambda ()
     (goto-char (point-max))
     (unless (bolp) (insert "\n"))
     (insert (format "\n%s %s\n"
                     (make-string (max 1 org-secrets-heading-level) ?*)
                     (plist-get spec :title)))
     (org-back-to-heading t)
     (org-secrets--set-prop "DOMAIN" (plist-get spec :domain))
     (org-secrets--set-prop "USER" (plist-get spec :user))
     (org-secrets--set-prop "URL" (plist-get spec :url))
     (org-set-property "SECRET"
                       (org-secrets--encode-secret (plist-get spec :secret)))
     (org-set-property "ENCODING" "base64")
     (org-id-get-create)
     (when-let* ((tags (plist-get spec :tags)))
       (org-set-tags tags))
     (when-let* ((notes (plist-get spec :notes)))
       (org-end-of-meta-data t)
       (insert notes)
       (unless (bolp) (insert "\n")))))
  (org-secrets--invalidate file))

;;;###autoload
(defun org-secrets-new ()
  "Add a secret to the active vault.
Empty password generates one.  The password is copied when done."
  (interactive)
  (let* ((title (read-string "Title: "))
         (domain (read-string "Domain: "))
         (user (read-string "User: "))
         (url (read-string "URL: "))
         (tags (org-secrets--parse-tags (read-string "Tags: ")))
         (typed (read-passwd "Password (empty generates): "))
         (notes (read-string "Notes: "))
         (generated (or (null typed) (string-empty-p typed)))
         (secret (if generated (org-secrets-random-password) typed))
         (file (org-secrets--write-file)))
    (when (string-empty-p (string-trim title))
      (user-error "Title is required"))
    (org-secrets--append-entry
     file
     (list :title (string-trim title)
           :domain (string-trim domain)
           :user (string-trim user)
           :url (string-trim url)
           :secret secret
           :notes (let ((n (string-trim notes)))
                    (and (not (string-empty-p n)) n))
           :tags tags))
    (kill-new secret)
    (message "%s %s and copied password"
             (if generated "Generated password for" "Stored")
             (string-trim title))))


;;; Consult / completing-read

(defun org-secrets--candidate-text (entry)
  "Return the completing-read display string for ENTRY."
  (format "%s  %s  %s"
          (or (org-secrets-entry-title entry) "")
          (or (org-secrets-entry-domain entry) "")
          (or (org-secrets-entry-user entry) "")))

(defun org-secrets--candidate-alist (entries)
  "Return an alist of (DISPLAY . ENTRY) for ENTRIES.
Duplicate display strings are disambiguated so `assoc' is unique.
`equal' on strings ignores text properties, which consult may strip."
  (let ((seen (make-hash-table :test 'equal))
        alist)
    (dolist (entry entries (nreverse alist))
      (let ((text (org-secrets--candidate-text entry)))
        (when (gethash text seen)
          (setq text (format "%s  %s"
                             text
                             (or (org-secrets-entry-org-id entry)
                                 (org-secrets-entry-uid entry)))))
        (puthash text t seen)
        (push (cons text entry) alist)))))

;;;###autoload
(defun org-secrets ()
  "Pick a secret and copy its password."
  (interactive)
  (let ((entries (org-secrets--entries)))
    (unless entries
      (user-error "No secrets in vault (%s)"
                  (org-secrets--write-file)))
    (let* ((alist (org-secrets--candidate-alist entries))
           (candidates (mapcar #'car alist))
           (choice
            (if (fboundp 'consult--read)
                (consult--read
                 candidates
                 :prompt "Secret: "
                 :sort nil
                 :require-match t
                 :preview-key nil
                 :category 'org-secrets)
              (completing-read "Secret: " candidates nil t)))
           (entry (cdr (assoc choice alist))))
      (unless entry
        (user-error "Unknown secret"))
      (org-secrets-copy-password entry))))


;;; Overlay: table, preview, filter

(defvar org-secrets--sidebar-query ""
  "Live-narrow string for the secrets list.  Empty means everything.")

(defvar org-secrets--sidebar-filtering nil
  "Non-nil while `/` has the list consuming keys as a query.")

(defvar org-secrets--sidebar-match-count 0)
(defvar org-secrets--sidebar-total-count 0)

(defvar org-secrets--sidebar-preview-id nil
  "Uid currently shown in the preview pane.")

(defvar org-secrets--sidebar-help-visible nil
  "Non-nil when the overlay preview pane is showing the key legend.")

(defvar org-secrets--sidebar-target-width nil
  "Column budget that wins over the live window's width.")

(defvar org-secrets--sidebar-posframe-parent nil
  "Parent frame to restore when the posframe overlay is dismissed.")

(defvar org-secrets--sidebar-posframe-fitted-width nil)
(defvar org-secrets--sidebar-posframe-refitting nil)

(defconst org-secrets-sidebar-help-commands
  '(("RET"    "Copy password; apply filter while narrowing")
    ("j, k"   "Move")
    ("u"      "Copy user")
    ("y"      "Copy URL / domain")
    ("w"      "Copy user TAB password")
    ("b"      "Open URL")
    ("o"      "Visit heading")
    ("N"      "New secret")
    ("s"      "Switch vault")
    ("/"      "Live-narrow (flex)")
    ("v"      "Toggle preview")
    ("gr"     "Refresh")
    ("?"      "This help")
    ("q, Esc" "Quit"))
  "Key legend for `org-secrets-sidebar-help'.
Each entry is (KEYS DESCRIPTION).  These are the overlay keys, not
whatever `where-is-internal' finds on `next-line' (C-n, <down>, …).")

(defvar org-secrets-sidebar-mode-map)

(defun org-secrets--sidebar-help-text ()
  "Return the overlay key legend as a string."
  (let* ((rows org-secrets-sidebar-help-commands)
         (width (apply #'max 3 (mapcar (lambda (row) (length (car row))) rows)))
         (line-format (format "%%-%ds  %%s" width)))
    (mapconcat (lambda (row)
                 (format line-format (car row) (cadr row)))
               rows "\n")))

(defun org-secrets-sidebar-help ()
  "Toggle the key legend in the overlay preview pane."
  (interactive)
  (setq org-secrets--sidebar-help-visible
        (not org-secrets--sidebar-help-visible))
  (let ((buf (or (get-buffer "*org-secrets*") (current-buffer))))
    (if (org-secrets--sidebar-posframe-showing-p)
        (org-secrets--sidebar-show-posframe buf)
      (org-secrets--sidebar-attach-preview buf)))
  (force-mode-line-update t))

(defvar-keymap org-secrets-sidebar-mode-map
  :doc "Keymap for `org-secrets-sidebar-mode'."
  "?"     #'org-secrets-sidebar-help
  "n"     #'next-line
  "p"     #'previous-line
  "RET"      #'org-secrets-sidebar-copy-password
  "<return>" #'org-secrets-sidebar-copy-password
  "u"     #'org-secrets-sidebar-copy-user
  "y"     #'org-secrets-sidebar-copy-url
  "w"     #'org-secrets-sidebar-copy-login
  "b"     #'org-secrets-sidebar-browse
  "o"     #'org-secrets-sidebar-visit
  "N"     #'org-secrets-sidebar-new
  "s"     #'org-secrets-sidebar-switch-vault
  "/"     #'org-secrets-sidebar-filter
  "v"     #'org-secrets-sidebar-toggle-preview
  "g"     #'org-secrets-sidebar-refresh
  "q"     #'org-secrets-sidebar-quit
  "C-g"   #'org-secrets-sidebar-quit
  "<escape>" #'org-secrets-sidebar-quit)

(define-derived-mode org-secrets-sidebar-mode tabulated-list-mode "Secrets"
  "Listing of org-secrets vault entries."
  (setq tabulated-list-padding 1)
  (setq tabulated-list-format (org-secrets--sidebar-format))
  (setq tabulated-list-sort-key (cons "Title" nil))
  (setq truncate-lines t)
  (setq-local mode-line-format '(" " (:eval (org-secrets--sidebar-footer))))
  (hl-line-mode 1)
  (add-hook 'tabulated-list-revert-hook #'org-secrets--sidebar-build-entries nil t)
  (add-hook 'post-command-hook #'org-secrets--sidebar-preview-on-command nil t)
  (tabulated-list-init-header))

(defun org-secrets--sidebar-footer ()
  "Mode-line hint row for the secrets list."
  (let* ((query org-secrets--sidebar-query)
         (querying (or org-secrets--sidebar-filtering
                       (and query (not (string-empty-p query)))))
         (counts (when querying
                   (format " %d/%d"
                           org-secrets--sidebar-match-count
                           org-secrets--sidebar-total-count)))
         (vault (when org-secrets-vault-filter
                  (format " [%s]"
                          (file-name-nondirectory org-secrets-vault-filter)))))
    (concat
     "Secrets"
     (or vault "")
     (when querying (concat "  /" query))
     (or counts "")
     (cond
      (org-secrets--sidebar-filtering
       "   RET apply  n/p move  Esc clear")
      (org-secrets--sidebar-help-visible
       "   ? close help  Esc close")
      (t
       "   RET copy  u user  / filter  N new  o visit  ? keys  Esc close")))))

(defconst org-secrets--sidebar-column-specs
  '((title  "Title"  16 mandatory)
    (domain "Domain" 18 mandatory)
    (user   "User"   12)
    (tags   "Tags"   12))
  "Candidate columns as (KEY HEADER WIDTH [MANDATORY]).")

(defun org-secrets--sidebar-available-width ()
  "Columns the secrets list actually has."
  (or org-secrets--sidebar-target-width
      (when-let* ((buf (get-buffer "*org-secrets*"))
                  (win (get-buffer-window buf t)))
        (window-body-width win))
      org-secrets-sidebar-width))

(defun org-secrets--sidebar-visible-columns ()
  "Return the column specs that fit the available width."
  (let ((budget (1- (org-secrets--sidebar-available-width)))
        (used 0)
        (kept nil))
    (dolist (spec org-secrets--sidebar-column-specs)
      (let ((cost (+ (nth 2 spec) (if kept 1 tabulated-list-padding))))
        (when (or (nth 3 spec) (<= (+ used cost) budget))
          (setq used (+ used cost))
          (push spec kept))))
    (org-secrets--sidebar-grow-columns (nreverse kept) budget used)))

(defun org-secrets--sidebar-grow-columns (columns budget used)
  "Stretch title/domain so leftover width is not empty padding."
  (let ((extra (max 0 (- budget used)))
        (weights '((title . 3) (domain . 3) (user . 2) (tags . 1))))
    (if (zerop extra)
        columns
      (let* ((keys (seq-filter (lambda (k) (assq k columns))
                               (mapcar #'car weights)))
             (total (apply #'+ (mapcar (lambda (k) (alist-get k weights))
                                       keys))))
        (if (zerop total)
            columns
          (let* ((shares
                  (mapcar (lambda (k)
                            (cons k (floor (* extra (/ (float (alist-get k weights))
                                                       total)))))
                          keys))
                 (rest (- extra (apply #'+ (mapcar #'cdr shares)))))
            (when (and (> rest 0) shares)
              (setcdr (car shares) (+ (cdr (car shares)) rest)))
            (mapcar (lambda (spec)
                      (let ((add (or (alist-get (car spec) shares) 0)))
                        (if (zerop add)
                            spec
                          (append (list (nth 0 spec)
                                        (nth 1 spec)
                                        (+ (nth 2 spec) add))
                                  (nthcdr 3 spec)))))
                    columns)))))))

(defun org-secrets--sidebar-format ()
  "Return `tabulated-list-format' for the columns that fit."
  (vconcat (mapcar (lambda (spec)
                     (list (nth 1 spec) (nth 2 spec) t))
                   (org-secrets--sidebar-visible-columns))))

(defun org-secrets--sidebar-sync-format ()
  "Re-fit `tabulated-list-format' when the available width changed."
  (when (derived-mode-p 'org-secrets-sidebar-mode)
    (let ((fitted (org-secrets--sidebar-format)))
      (unless (equal fitted tabulated-list-format)
        (setq tabulated-list-format fitted)
        (tabulated-list-init-header)))))

(defun org-secrets--sidebar-cell (entry key)
  "Return ENTRY's display string for column KEY."
  (pcase key
    ('title  (or (org-secrets-entry-title entry) ""))
    ('domain (or (org-secrets-entry-domain entry) ""))
    ('user   (or (org-secrets-entry-user entry) ""))
    ('tags   (if-let* ((tags (org-secrets-entry-tags entry)))
                 (concat ":" (mapconcat #'identity tags ":") ":")
               ""))
    (_ "")))

(defun org-secrets--flex-regexp (query)
  "Return a subsequence regexp for QUERY."
  (mapconcat (lambda (ch) (regexp-quote (char-to-string ch)))
             (string-to-list query)
             ".*"))

(defun org-secrets--entry-haystack (entry)
  "Searchable text for ENTRY.  The secret is not included."
  (mapconcat #'identity
             (delq nil
                   (list (org-secrets-entry-title entry)
                         (org-secrets-entry-domain entry)
                         (org-secrets-entry-user entry)
                         (org-secrets-entry-url entry)
                         (org-secrets-entry-notes entry)
                         (when-let* ((tags (org-secrets-entry-tags entry)))
                           (mapconcat #'identity tags " "))))
             " "))

(defun org-secrets--query-matches-p (entry query)
  "Return non-nil if ENTRY matches QUERY.
Empty QUERY matches everything.  Whitespace splits into tokens,
each of which must flex-match the haystack."
  (or (not query)
      (string-empty-p (string-trim query))
      (let ((hay (org-secrets--entry-haystack entry)))
        (cl-every (lambda (token)
                    (let ((case-fold-search t))
                      (string-match-p (org-secrets--flex-regexp token) hay)))
                  (split-string query)))))

(defun org-secrets--sidebar-build-entries (&optional force)
  "Rebuild `tabulated-list-entries' from the vault."
  (org-secrets--sidebar-sync-format)
  (let* ((columns (org-secrets--sidebar-visible-columns))
         (entries (org-secrets--entries force))
         (query org-secrets--sidebar-query)
         (matched (if (and query (not (string-empty-p (string-trim query))))
                      (cl-remove-if-not
                       (lambda (e) (org-secrets--query-matches-p e query))
                       entries)
                    entries)))
    (setq org-secrets--sidebar-total-count (length entries)
          org-secrets--sidebar-match-count (length matched)
          tabulated-list-entries
          (mapcar
           (lambda (e)
             (list (org-secrets-entry-uid e)
                   (vconcat
                    (mapcar (lambda (spec)
                              (org-secrets--sidebar-cell e (car spec)))
                            columns))))
           matched))))

(defun org-secrets--preview-body (entry)
  "Return the preview text for ENTRY.  Never includes the secret value."
  (let* ((width (max 20 (1- (org-secrets--sidebar-available-width))))
         (tags (org-secrets-entry-tags entry))
         (secret (org-secrets-entry-secret entry))
         (lines
          (list
           (or (org-secrets-entry-title entry) "(no title)")
           (format "domain   %s" (or (org-secrets-entry-domain entry) "-"))
           (format "user     %s" (or (org-secrets-entry-user entry) "-"))
           (format "url      %s" (or (org-secrets-entry-url entry) "-"))
           (format "tags     %s"
                   (if tags
                       (concat ":" (mapconcat #'identity tags ":") ":")
                     "-"))
           (format "secret   %s"
                   (if (and secret (not (string-empty-p secret)))
                       "set"
                     "missing")))))
    (when-let* ((notes (org-secrets-entry-notes entry)))
      (setq lines (append lines (cons "" (split-string notes "\n")))))
    (mapconcat (lambda (line)
                 (truncate-string-to-width line width nil nil t))
               (seq-take lines org-secrets-sidebar-preview-lines)
               "\n")))

(defun org-secrets--preview-buffer ()
  "Return the dedicated preview buffer, creating it if needed."
  (let ((buf (get-buffer-create "*org-secrets-preview*")))
    (with-current-buffer buf
      (setq-local mode-line-format '(" " (:eval (org-secrets--sidebar-footer))))
      (setq-local header-line-format nil)
      (setq-local cursor-type nil)
      (setq-local truncate-lines t)
      (setq-local buffer-read-only t))
    buf))

(defun org-secrets--preview-body-lines ()
  "Content lines in the preview window, including the separator."
  (cond
   (org-secrets--sidebar-help-visible
    (max 8 (1+ (length (split-string (org-secrets--sidebar-help-text) "\n")))))
   (org-secrets-sidebar-show-preview
    (1+ org-secrets-sidebar-preview-lines))
   (t 0)))

(defun org-secrets--preview-split-size ()
  "Total height of the preview window, including its mode-line."
  (1+ (org-secrets--preview-body-lines)))

(defun org-secrets--preview-reserved-lines ()
  "Lines the overlay must keep at the bottom for preview + footer."
  (if (or org-secrets-sidebar-show-preview
          org-secrets--sidebar-help-visible)
      (org-secrets--preview-split-size)
    1))

(defun org-secrets--sidebar-draw-preview ()
  "Rebuild the preview buffer for the selected row or the key legend.

The preview is a separate window at the bottom of the overlay, not
text after the last table row -- a 70-entry vault would push that
off-screen."
  (let ((table (get-buffer "*org-secrets*")))
    (when (and table
               (with-current-buffer table
                 (derived-mode-p 'org-secrets-sidebar-mode)))
      (let* ((entry (and (not org-secrets--sidebar-help-visible)
                         org-secrets-sidebar-show-preview
                         (with-current-buffer table
                           (ignore-errors
                             (org-secrets--sidebar-entry-at-point)))))
             (width (max 10 (org-secrets--sidebar-available-width)))
             (text (cond
                    (org-secrets--sidebar-help-visible
                     (org-secrets--sidebar-help-text))
                    (entry (org-secrets--preview-body entry))
                    (t (propertize "(no entry)" 'face 'shadow))))
             (buf (org-secrets--preview-buffer)))
        (with-current-buffer buf
          (let ((inhibit-read-only t)
                (buffer-read-only nil))
            (erase-buffer)
            (insert (propertize (make-string width ?─) 'face 'shadow) "\n")
            (insert text)
            (unless (bolp) (insert "\n"))
            (goto-char (point-min))))))))

(defun org-secrets--sidebar-detach-preview ()
  "Delete the preview window and restore the table's mode-line footer."
  (when-let* ((pbuf (get-buffer "*org-secrets-preview*")))
    (dolist (win (get-buffer-window-list pbuf nil t))
      (when (window-live-p win)
        (ignore-errors (delete-window win)))))
  (when-let* ((tbuf (get-buffer "*org-secrets*")))
    (with-current-buffer tbuf
      (setq-local mode-line-format
                  '(" " (:eval (org-secrets--sidebar-footer)))))))

(defun org-secrets--sidebar-attach-preview (table-buf)
  "Split TABLE-BUF's window and show the preview below it.

`posframe-show' leaves a single window; this runs after every show
so the preview stays pinned to the bottom while the table scrolls."
  (org-secrets--sidebar-draw-preview)
  (if (not (or org-secrets-sidebar-show-preview
               org-secrets--sidebar-help-visible))
      (org-secrets--sidebar-detach-preview)
    (when-let* ((table-win (get-buffer-window table-buf t)))
      (let* ((preview-buf (org-secrets--preview-buffer))
             (size (org-secrets--preview-split-size))
             (frame (window-frame table-win))
             (preview-win
              (cl-find-if (lambda (w) (eq (window-frame w) frame))
                          (get-buffer-window-list preview-buf nil t))))
        (with-current-buffer table-buf
          (setq-local mode-line-format nil))
        (when (and preview-win
                   (not (eql (window-total-height preview-win) size)))
          (ignore-errors (delete-window preview-win))
          (setq preview-win nil))
        (unless preview-win
          (when (<= (window-total-height table-win) (1+ size))
            (setq size (max 4 (/ (window-total-height table-win) 3))))
          (with-selected-window table-win
            (let ((dedicated (window-dedicated-p table-win)))
              (set-window-dedicated-p table-win nil)
              (setq preview-win (split-window table-win (- size) 'below))
              (set-window-dedicated-p table-win dedicated))))
        (set-window-buffer preview-win preview-buf)
        (set-window-dedicated-p preview-win t)
        (set-window-parameter preview-win 'no-other-window t)
        (set-window-start preview-win
                         (with-current-buffer preview-buf (point-min)))
        (select-window table-win)))))

(defun org-secrets--sidebar-print (&optional update)
  "Print the table and refresh the preview buffer."
  (let ((id (tabulated-list-get-id)))
    (tabulated-list-print update)
    (when id
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (equal (tabulated-list-get-id) id)))
        (forward-line 1))
      (when (eobp) (goto-char (point-min))))
    (org-secrets--sidebar-draw-preview)
    (setq org-secrets--sidebar-preview-id (tabulated-list-get-id))))

(defun org-secrets--sidebar-preview-on-command ()
  "Follow point: redraw the preview when the selected row changes."
  (when (derived-mode-p 'org-secrets-sidebar-mode)
    (when (and org-secrets-sidebar-show-preview
               (not org-secrets--sidebar-help-visible))
      (let ((id (tabulated-list-get-id)))
        (unless (equal id org-secrets--sidebar-preview-id)
          (setq org-secrets--sidebar-preview-id id)
          (org-secrets--sidebar-draw-preview))))))

(defun org-secrets-sidebar-toggle-preview ()
  "Toggle the preview pane under the secrets list."
  (interactive)
  (setq org-secrets-sidebar-show-preview
        (not org-secrets-sidebar-show-preview))
  (setq org-secrets--sidebar-preview-id nil)
  (org-secrets--sidebar-print t)
  (let ((buf (current-buffer)))
    (if (org-secrets--sidebar-posframe-showing-p)
        (org-secrets--sidebar-show-posframe buf)
      (org-secrets--sidebar-attach-preview buf)))
  (message "Preview %s" (if org-secrets-sidebar-show-preview "on" "off")))

(defun org-secrets-sidebar-refresh ()
  "Re-read the vault and redraw."
  (interactive)
  (org-secrets--sidebar-build-entries t)
  (org-secrets--sidebar-print t)
  (let ((buf (current-buffer)))
    (if (org-secrets--sidebar-posframe-showing-p)
        (org-secrets--sidebar-show-posframe buf)
      (org-secrets--sidebar-attach-preview buf)))
  (message "org-secrets refreshed"))

(defun org-secrets--sidebar-entry-at-point ()
  "Return the entry for the sidebar row at point."
  (let ((uid (tabulated-list-get-id)))
    (or (and uid (org-secrets--entry-by-uid uid))
        (user-error "No secret at point"))))

(defun org-secrets-sidebar-copy-password ()
  "Copy the password at point and dismiss the overlay."
  (interactive)
  (org-secrets-copy-password (org-secrets--sidebar-entry-at-point))
  (org-secrets--sidebar-dismiss))

(defun org-secrets-sidebar-copy-user ()
  "Copy the user at point."
  (interactive)
  (org-secrets-copy-user (org-secrets--sidebar-entry-at-point)))

(defun org-secrets-sidebar-copy-url ()
  "Copy the URL (or domain) at point."
  (interactive)
  (org-secrets-copy-url (org-secrets--sidebar-entry-at-point)))

(defun org-secrets-sidebar-copy-login ()
  "Copy `user TAB password' for the row at point and dismiss."
  (interactive)
  (org-secrets-copy-login (org-secrets--sidebar-entry-at-point))
  (org-secrets--sidebar-dismiss))

(defun org-secrets-sidebar-browse ()
  "Open the URL of the row at point."
  (interactive)
  (org-secrets-browse (org-secrets--sidebar-entry-at-point)))

(defun org-secrets-sidebar-visit ()
  "Visit the heading at point.  Dismisses the overlay first."
  (interactive)
  (let ((entry (org-secrets--sidebar-entry-at-point)))
    (org-secrets--sidebar-leave-overlay)
    (org-secrets-visit entry)))

(defun org-secrets-sidebar-new ()
  "Add a secret.  Dismisses the overlay so the minibuffer is usable."
  (interactive)
  (let ((was (org-secrets--sidebar-posframe-showing-p)))
    (org-secrets--sidebar-leave-overlay)
    (org-secrets-new)
    (when was
      (org-secrets-sidebar))))

(defun org-secrets-sidebar-switch-vault ()
  "Restrict the list to one vault, or show all."
  (interactive)
  (let* ((files (org-secrets-vault-files))
         (cands (cons (cons "all" nil)
                      (mapcar (lambda (f)
                                (cons (abbreviate-file-name f) f))
                              files)))
         (choice (completing-read
                  "Vault: " (mapcar #'car cands) nil t nil nil
                  (if org-secrets-vault-filter
                      (abbreviate-file-name org-secrets-vault-filter)
                    "all"))))
    (setq org-secrets-vault-filter (cdr (assoc choice cands)))
    (org-secrets-sidebar-refresh)))

(defun org-secrets--sidebar-set-query (query)
  "Set the live-narrow QUERY and redraw, keeping point on the same row."
  (setq org-secrets--sidebar-query (or query ""))
  (when-let* ((buf (get-buffer "*org-secrets*")))
    (with-current-buffer buf
      (when (derived-mode-p 'org-secrets-sidebar-mode)
        (org-secrets--sidebar-build-entries)
        (org-secrets--sidebar-print t)
        (org-secrets--sidebar-attach-preview buf))))
  (force-mode-line-update t))

(defun org-secrets--return-event-p (ev)
  "Return non-nil if EV is Enter in any GUI/tty encoding.

A catch-all `[t]' on the filter map is looked up *before*
`function-key-map' translates `<return>' to RET.  The default
binding then runs instead of the RET command, and a printable-only
self-insert silently drops it -- which is why Enter appeared to
do nothing."
  (or (memq ev '(return kp-enter newline S-return))
      (and (characterp ev) (memq ev '(?\r ?\n)))
      (memq (event-basic-type ev) '(return kp-enter newline))))

(defun org-secrets-sidebar-filter-self-insert ()
  "Append `last-command-event' to the live query, or apply on Enter."
  (interactive)
  (let ((ev last-command-event))
    (cond
     ((org-secrets--return-event-p ev)
      (org-secrets-sidebar-filter-confirm))
     ((and (characterp ev) (>= ev 32) (not (eq ev 127)))
      (org-secrets--sidebar-set-query
       (concat org-secrets--sidebar-query (char-to-string ev)))))))

(defun org-secrets-sidebar-filter-backspace ()
  "Drop the last character of the live query."
  (interactive)
  (when (> (length org-secrets--sidebar-query) 0)
    (org-secrets--sidebar-set-query
     (substring org-secrets--sidebar-query 0 -1))))

(defun org-secrets-sidebar-filter-clear ()
  "Empty the live query without leaving filter-mode."
  (interactive)
  (org-secrets--sidebar-set-query ""))

(defun org-secrets--set-filtering (on)
  "Turn live-narrow on or off, including `org-secrets-filter-mode'."
  (setq org-secrets--sidebar-filtering (and on t))
  (when-let* ((buf (get-buffer "*org-secrets*")))
    (with-current-buffer buf
      (org-secrets-filter-mode (if org-secrets--sidebar-filtering 1 -1))))
  (force-mode-line-update t))

(defun org-secrets-sidebar-filter-confirm ()
  "Keep the current query and leave live-narrow.
Returns j/k and RET to the overlay: move, then copy."
  (interactive)
  (org-secrets--set-filtering nil))

(defvar-keymap org-secrets-sidebar-filter-map
  :doc "Transient keymap while the secrets list is live-narrowing."
  "RET"         #'org-secrets-sidebar-filter-confirm
  "C-m"         #'org-secrets-sidebar-filter-confirm
  "<return>"    #'org-secrets-sidebar-filter-confirm
  "n"           #'next-line
  "p"           #'previous-line
  "C-n"         #'next-line
  "C-p"         #'previous-line
  "<down>"      #'next-line
  "<up>"        #'previous-line
  "DEL"         #'org-secrets-sidebar-filter-backspace
  "<backspace>" #'org-secrets-sidebar-filter-backspace
  "<delete>"    #'org-secrets-sidebar-filter-backspace
  "C-h"         #'org-secrets-sidebar-filter-backspace
  "C-u"         #'org-secrets-sidebar-filter-clear
  "C-g"         #'org-secrets-sidebar-quit
  "<escape>"    #'org-secrets-sidebar-quit)

(define-key org-secrets-sidebar-filter-map [t]
            #'org-secrets-sidebar-filter-self-insert)
;; After `[t]': specific events the catch-all would otherwise swallow
;; before function-key-map translation.
(define-key org-secrets-sidebar-filter-map [return]
            #'org-secrets-sidebar-filter-confirm)
(define-key org-secrets-sidebar-filter-map [kp-enter]
            #'org-secrets-sidebar-filter-confirm)
(define-key org-secrets-sidebar-filter-map [S-return]
            #'org-secrets-sidebar-filter-confirm)
(define-key org-secrets-sidebar-filter-map (kbd "C-j")
            #'org-secrets-sidebar-filter-confirm)

(define-minor-mode org-secrets-filter-mode
  "Live-narrow the org-secrets overlay.

A minor mode rather than `set-transient-map': evil's state maps
sit in `emulation-mode-map-alists' and can swallow RET before a
terminal-local transient map sees it.  An intercept keymap on this
mode outranks evil, so Enter applies the filter."
  :lighter nil
  :keymap org-secrets-sidebar-filter-map)

(defun org-secrets-sidebar-filter ()
  "Start live-narrowing the secrets list."
  (interactive)
  (org-secrets--set-filtering t))


;;; Overlay: posframe / side window

(defun org-secrets--posframe-available-p ()
  "Return non-nil when posframe can actually display a child frame."
  (and (require 'posframe nil t)
       (fboundp 'posframe-workable-p)
       (posframe-workable-p)))

(defun org-secrets--use-posframe-p ()
  "Return non-nil when the secrets list should open as a posframe."
  (and org-secrets-sidebar-use-posframe
       (org-secrets--posframe-available-p)))

(defun org-secrets--sidebar-posframe-frame ()
  "Return the secrets list's posframe, or nil."
  (when-let* ((buf (get-buffer "*org-secrets*")))
    (and (boundp 'posframe--frame)
         (buffer-local-value 'posframe--frame buf))))

(defun org-secrets--sidebar-posframe-showing-p ()
  "Return non-nil when the secrets list is a visible posframe."
  (when-let* ((frame (org-secrets--sidebar-posframe-frame)))
    (and (frame-live-p frame)
         (frame-visible-p frame))))

(defun org-secrets--sidebar-hide-posframe ()
  "Hide the overlay and restore input focus to its parent frame."
  (let ((parent org-secrets--sidebar-posframe-parent)
        (buf (get-buffer "*org-secrets*")))
    (setq org-secrets--sidebar-posframe-parent nil
          org-secrets--sidebar-posframe-fitted-width nil
          org-secrets--sidebar-query ""
          org-secrets--sidebar-help-visible nil
          org-secrets--sidebar-preview-id nil)
    (org-secrets--set-filtering nil)
    (when (and buf (fboundp 'posframe-hide))
      (posframe-hide buf))
    (when (and parent (frame-live-p parent))
      (select-frame-set-input-focus parent))))

(defun org-secrets--sidebar-leave-overlay ()
  "Dismiss the posframe overlay so a subsequent display uses a real window.
The overlay's window is dedicated and its frame unsplittable, so
`find-file' from there has nowhere to go."
  (when (org-secrets--sidebar-posframe-showing-p)
    (org-secrets--sidebar-hide-posframe)))

(defun org-secrets--sidebar-dismiss ()
  "Hide the overlay or quit the side window."
  (org-secrets--set-filtering nil)
  (setq org-secrets--sidebar-query "")
  (if (org-secrets--sidebar-posframe-showing-p)
      (org-secrets--sidebar-hide-posframe)
    (org-secrets--sidebar-detach-preview)
    (when-let* ((buf (get-buffer "*org-secrets*"))
                (win (get-buffer-window buf)))
      (quit-window nil win))))

(defun org-secrets-sidebar-quit ()
  "Dismiss the secrets list, or the live query first."
  (interactive)
  (cond
   ((and org-secrets--sidebar-query
         (not (string-empty-p org-secrets--sidebar-query)))
    (org-secrets--sidebar-set-query "")
    (org-secrets--set-filtering nil))
   (org-secrets--sidebar-help-visible
    (setq org-secrets--sidebar-help-visible nil)
    (let ((buf (get-buffer "*org-secrets*")))
      (if (org-secrets--sidebar-posframe-showing-p)
          (org-secrets--sidebar-show-posframe buf)
        (org-secrets--sidebar-attach-preview buf)))
    (force-mode-line-update t))
   (t
    (org-secrets--sidebar-dismiss))))

(defun org-secrets--sidebar-close-side-windows (buf)
  "Delete any side window showing BUF, so the overlay does not share a frame."
  (dolist (win (get-buffer-window-list buf nil t))
    (when (and (window-parameter win 'window-side)
               (not (frame-parent (window-frame win))))
      (ignore-errors (delete-window win)))))

(defun org-secrets--sidebar-posframe-border-color ()
  "Border colour for the overlay, taken from the current theme."
  (or (face-foreground 'vertical-border nil t)
      (face-foreground 'mode-line-inactive nil t)
      "gray50"))

(defun org-secrets--sidebar-posframe-parent-frame ()
  "Frame the overlay should be sized against and return focus to."
  (or (and (frame-live-p org-secrets--sidebar-posframe-parent)
           org-secrets--sidebar-posframe-parent)
      (frame-parent (selected-frame))
      (selected-frame)))

(defun org-secrets--sidebar-posframe-char-width (&optional frame)
  "Character width of the overlay on FRAME."
  (let* ((frame (or frame (org-secrets--sidebar-posframe-parent-frame)))
         (avail (max 1 (frame-width frame)))
         (margin (min 12 (max 4 (/ avail 16))))
         (ceil (max 48 (- avail margin)))
         (floor (min org-secrets-sidebar-posframe-width ceil)))
    (if (null org-secrets-sidebar-posframe-width-ratio)
        floor
      (let ((wanted (round (* org-secrets-sidebar-posframe-width-ratio avail))))
        (max floor (min wanted ceil))))))

(defun org-secrets--sidebar-posframe-max-height (&optional frame)
  "Max overlay height in lines, as a fraction of FRAME."
  (let* ((frame (or frame (org-secrets--sidebar-posframe-parent-frame)))
         (avail (max 8 (frame-height frame))))
    (max 8 (min (- avail 4) (round (* 0.72 avail))))))

(defun org-secrets--sidebar-refit-posframe (width)
  "Rebuild the overlay's columns for WIDTH and show it again."
  (when-let* ((buf (get-buffer "*org-secrets*")))
    (let ((org-secrets--sidebar-posframe-refitting t)
          (org-secrets--sidebar-target-width width))
      (setq org-secrets--sidebar-posframe-fitted-width width)
      (with-current-buffer buf
        (when (derived-mode-p 'org-secrets-sidebar-mode)
          (org-secrets--sidebar-build-entries)
          (org-secrets--sidebar-print t)))
      (org-secrets--sidebar-show-posframe buf))))

(defun org-secrets--sidebar-posframe-on-parent-resize (frame)
  "Refit the overlay when its parent FRAME changes size."
  (when (and (not org-secrets--sidebar-posframe-refitting)
             org-secrets--sidebar-posframe-parent
             (eq frame org-secrets--sidebar-posframe-parent)
             (org-secrets--sidebar-posframe-showing-p))
    (let ((width (org-secrets--sidebar-posframe-char-width frame)))
      (unless (eql width org-secrets--sidebar-posframe-fitted-width)
        (org-secrets--sidebar-refit-posframe width)))))

(defun org-secrets--sidebar-show-posframe (buf)
  "Show BUF as a centred, focusable posframe overlay."
  (org-secrets--sidebar-close-side-windows buf)
  (let* ((parent (org-secrets--sidebar-posframe-parent-frame))
         (width (or org-secrets--sidebar-target-width
                    (org-secrets--sidebar-posframe-char-width parent)))
         (maxh (org-secrets--sidebar-posframe-max-height parent))
         (preview (org-secrets--preview-reserved-lines))
         (rows (with-current-buffer buf
                 (max 1 (length tabulated-list-entries))))
         ;; Reserve the preview at the bottom; do not size the frame to
         ;; all rows plus preview (that clamps and hides the preview).
         (table (max 4 (min rows (max 4 (- maxh preview 1)))))
         (height (+ 1 table preview)))
    (setq org-secrets--sidebar-posframe-parent parent
          org-secrets--sidebar-posframe-fitted-width width)
    (add-hook 'window-size-change-functions
              #'org-secrets--sidebar-posframe-on-parent-resize)
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
       :border-color (org-secrets--sidebar-posframe-border-color)
       :respect-header-line t
       :respect-mode-line t
       :lines-truncate t
       :cursor 'box
       :accept-focus t
       :window-point (with-current-buffer buf (point))))
    (org-secrets--sidebar-attach-preview buf)
    (when-let* ((frame (org-secrets--sidebar-posframe-frame))
                (win (get-buffer-window buf frame)))
      (select-frame-set-input-focus frame)
      (select-window win))))

(defun org-secrets--sidebar-show-side-window (buf)
  "Show BUF in a side window -- the display used when posframe cannot."
  (when (fboundp 'posframe-hide)
    (posframe-hide buf))
  (setq org-secrets--sidebar-posframe-parent nil)
  (pop-to-buffer
   buf
   `((display-buffer-in-side-window)
     (side . ,org-secrets-sidebar-side)
     (slot . 0)
     (window-width . ,org-secrets-sidebar-width)
     (preserve-size . (t . nil))))
  (org-secrets--sidebar-attach-preview buf))

(defun org-secrets--sidebar-prepare-buffer ()
  "Return the secrets-list buffer, filled from the vault."
  (let ((buf (get-buffer-create "*org-secrets*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-secrets-sidebar-mode)
        (org-secrets-sidebar-mode))
      (org-secrets--sidebar-build-entries)
      (org-secrets--sidebar-print t))
    buf))

;;;###autoload
(defun org-secrets-sidebar ()
  "Open the secrets list.

Uses a posframe overlay when `org-secrets-sidebar-use-posframe' is
non-nil and posframe can display a child frame.  Otherwise opens
a side window.  The buffer and keys are the same either way;
Esc or `q' dismisses."
  (interactive)
  (setq org-secrets--sidebar-query ""
        org-secrets--sidebar-help-visible nil
        org-secrets--sidebar-preview-id nil)
  (org-secrets--set-filtering nil)
  (let* ((overlay (org-secrets--use-posframe-p))
         (org-secrets--sidebar-target-width
          (and overlay (org-secrets--sidebar-posframe-char-width)))
         (buf (org-secrets--sidebar-prepare-buffer)))
    (if overlay
        (org-secrets--sidebar-show-posframe buf)
      (org-secrets--sidebar-show-side-window buf))
    buf))


;;; Evil

(defun org-secrets--maybe-setup-evil ()
  "Bind overlay keys in evil normal state so they are not shadowed.

Must call `evil-define-key*', the function.  `evil-define-key' is
a macro; if this file is compiled without evil loaded the call is
left as a function and startup dies with `Invalid function'.

`gr' rather than `g' because `g' is an evil prefix.  `/' is rebound
so evil-search does not steal live-narrow.  `j'/`k' stay as motion."
  (when (fboundp 'evil-make-intercept-map)
    (evil-make-intercept-map org-secrets-sidebar-filter-map 'normal))
  (when (fboundp 'evil-define-key*)
    (evil-define-key* 'normal org-secrets-sidebar-filter-map
      (kbd "RET") #'org-secrets-sidebar-filter-confirm
      (kbd "<return>") #'org-secrets-sidebar-filter-confirm
      (kbd "C-m") #'org-secrets-sidebar-filter-confirm
      (kbd "<escape>") #'org-secrets-sidebar-quit)
    (evil-define-key* 'normal org-secrets-sidebar-mode-map
      (kbd "RET") #'org-secrets-sidebar-copy-password
      (kbd "<return>") #'org-secrets-sidebar-copy-password
      (kbd "u") #'org-secrets-sidebar-copy-user
      (kbd "y") #'org-secrets-sidebar-copy-url
      (kbd "w") #'org-secrets-sidebar-copy-login
      (kbd "b") #'org-secrets-sidebar-browse
      (kbd "o") #'org-secrets-sidebar-visit
      (kbd "N") #'org-secrets-sidebar-new
      (kbd "s") #'org-secrets-sidebar-switch-vault
      (kbd "/") #'org-secrets-sidebar-filter
      (kbd "v") #'org-secrets-sidebar-toggle-preview
      (kbd "?") #'org-secrets-sidebar-help
      (kbd "gr") #'org-secrets-sidebar-refresh
      (kbd "q") #'org-secrets-sidebar-quit
      (kbd "<escape>") #'org-secrets-sidebar-quit)))

(with-eval-after-load 'evil
  (org-secrets--maybe-setup-evil))

(provide 'org-secrets)
;;; org-secrets.el ends here

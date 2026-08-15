;;; example-config.el --- template for config.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Copy this to `config.el' (git-ignored) and edit.
;;
;; config.el is loaded from early-init.el, BEFORE straight.el and
;; use-package exist and before any module is required.  So it can only
;; hold plain values -- `setq' / `defvar', never a `use-package' form.
;;
;; Put a value here only when a module needs it *while being required*:
;; a straight `:local-repo' path, a `:load-path', an `:if'/`:when' guard,
;; or anything read by `:custom' / `:init'.  Everything else -- API keys,
;; keybindings, overrides, whole personal packages -- belongs in
;; personal_lp.org, which tangles to settings.el and is loaded last.
;; See example-personal_lp.org.

;;; Code:

;; Where your personally checked-out packages live.  Modules read this
;; instead of hardcoding an absolute path, and skip the package when it
;; is nil (the default, declared in early_init_lp.org).
;; (setq jemacs-local-repo-root "~/src/emacs-packages")

;; Root of your org files.  jemacs-note-taking derives the agenda files,
;; the journal directory, the notes file and the password store from it,
;; so this is normally the only path you need to set.
;; (setq jemacs-org-root "~/Dropbox/myorgs/")

;; Only if your org-roam notes live in a subdirectory of the root.
;; (setq jemacs-org-roam-directory "~/Dropbox/myorgs/notes/")

;; Run this config out of a different directory.
;; (setq user-emacs-directory "~/.mynewcraft.d")

;;; example-config.el ends here

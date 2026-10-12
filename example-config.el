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

;; Only if your org-shelf notes live in a subdirectory of the root.
;; (setq jemacs-org-roam-directory "~/Dropbox/myorgs/notes/")

;; Run this config out of a different directory.
;; (setq user-emacs-directory "~/.mynewcraft.d")

;; The xwapp pages -- the herd page, pr-view, clickup-view -- on your
;; phone, through `tailscale serve' on this machine.  Here rather than
;; in personal_lp.org because it names this machine: its tailnet name,
;; and the port the serve passes on to, kept fixed for it.  Plain http
;; for a serve without a certificate (`--http=80'), else "https".
;; See "From your other devices" in local/xwapp/README.org.
;;
;;   tailscale serve --bg --http=80 http://127.0.0.1:48123
;;
;; (setq xwapp-port 48123
;;       xwapp-remote-hosts '("my-mac.tail1234.ts.net" "my-mac")
;;       xwapp-remote-users '("you@example.com")   ; your Tailscale login
;;       xwapp-remote-scheme "http")

;;; example-config.el ends here

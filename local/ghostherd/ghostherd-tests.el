;;; ghostherd-tests.el --- Tests for ghostherd  -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for ghostherd's pure helpers -- the parts that need no PTY.
;; The interactive spawn/kill paths still need the manual plan in
;; docs/testing.org; driving real ghostel terminals headlessly is what
;; Phase 1 deferred.
;;
;; Run:
;;
;;   emacs --batch --init-directory=~/.emacs.d/emacs-home/ \
;;         -L . -L ../xwapp -l ghostherd.el -l ghostherd-tests.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Each part's tests are in ghostherd-PART-tests.el and run alone the
;; same way; what they share is in ghostherd-test-helpers.el.  This
;; file requires them all.

;;; Code:

(require 'ghostherd-test-helpers)
(require 'ghostherd-core-tests)
(require 'ghostherd-worktree-tests)
(require 'ghostherd-log-tests)
(require 'ghostherd-screen-tests)
(require 'ghostherd-spawn-tests)
(require 'ghostherd-comm-tests)
(require 'ghostherd-sidebar-tests)
(require 'ghostherd-mail-tests)
(require 'ghostherd-lead-tests)
(require 'ghostherd-hooks-tests)
(require 'ghostherd-primary-tests)
(require 'ghostherd-push-tests)
(require 'ghostherd-backend-tests)
(require 'ghostherd-tmux-tests)
(require 'ghostherd-memory-tests)
(require 'ghostherd-usage-tests)
(require 'ghostherd-memory-page-tests)
(require 'ghostherd-board-tests)

(provide 'ghostherd-tests)
;;; ghostherd-tests.el ends here

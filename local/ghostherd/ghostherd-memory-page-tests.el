;;; ghostherd-memory-page-tests.el --- Tests for ghostherd-memory-page.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Part of ghostherd's tests; ghostherd-tests.el runs them all.  This
;; file alone:
;;
;;   emacs --batch --init-directory=~/.emacs.d/emacs-home/ \
;;         -L . -L ../xwapp -l ghostherd-memory-page-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ghostherd-test-helpers)

;;; Memory page (pure: no xwidget, no sidecar)

(ert-deftest ghostherd-test-memory-page-search ()
  "Empty asks nothing; a query goes with its filters, its seq comes back."
  (ghostherd-test--with-page
    (setq reply (lambda (_m _p) '(:hits ((:text "a" :chunk_index 3) (:text "b" :chunk_index 9)))))
    (ghostherd-memory-page--handle '((op . "search") (seq . 7) (q . "  ") (agent . "") (project . "")))
    (should-not requests)
    (should (equal (ghostherd-test--call calls "renderHits") '(:seq 7 :q "" :hits [])))
    (ghostherd-memory-page--handle '((op . "search") (seq . 8) (q . " retry ") (agent . "claude")
                                     (project . "") (limit . 500)))
    (should (equal requests '(("memory_search" (:query "retry" :limit 120 :agent "claude")))))
    (let ((hits (ghostherd-test--call calls "renderHits")))
      (should (equal (plist-get hits :seq) 8))
      (should (vectorp (plist-get hits :hits)))
      (should (= (length (plist-get hits :hits)) 2)))))

(ert-deftest ghostherd-test-memory-page-search-failure-names-its-seq ()
  (ghostherd-test--with-page
    (setq fail "connection refused")
    (ghostherd-memory-page--handle '((op . "search") (seq . 4) (q . "x")))
    (should (equal (ghostherd-test--call calls "searchFailed")
                   '(:seq 4 :error "connection refused")))))

(ert-deftest ghostherd-test-memory-page-open-source ()
  "The page places the window; Emacs clamps it and hands focus back."
  (ghostherd-test--with-page
    (setq reply (lambda (_m _p) '(:total 900 :chunks ((:chunk_index 40) (:chunk_index 41)))))
    (ghostherd-memory-page--handle '((op . "open-source") (source_path . "/s.jsonl") (offset . 40)
                                     (limit . 5000) (focus . 80) (mode . "open")))
    (should (equal requests '(("memory_chunks" (:source_path "/s.jsonl" :offset 40 :limit 400)))))
    (let ((p (ghostherd-test--call calls "renderSource")))
      (should (equal (plist-get p :focus) 80))
      (should (equal (plist-get p :mode) "open"))
      (should (equal (plist-get p :total) 900))
      (should (vectorp (plist-get p :chunks))))
    ;; JSON null reads as :null, which json-encode refuses: it must not
    ;; reach the reply.
    (ghostherd-memory-page--handle '((op . "open-source") (source_path . "/s.jsonl") (focus . :null)))
    (should (json-encode (ghostherd-test--call calls "renderSource")))
    (should-not (plist-get (ghostherd-test--call calls "renderSource") :focus))))

(ert-deftest ghostherd-test-memory-page-context-is-arrays ()
  "json.el reads a list of plists as one alist: the sessions must
reach the page as an array of objects."
  (ghostherd-test--with-page
    (setq reply (lambda (_m _p) '(:total 2 :sources ((:source_path "/a" :agent "claude")
                                                     (:source_path "/b" :agent "grok")))))
    (let ((ghostherd-memory-page--project "/p")
          (ghostherd-memory-page--pending '(:view "sessions" :query nil :project nil)))
      (ghostherd-memory-page--handle '((op . "ready")))
      (should-not ghostherd-memory-page--pending))
    (let* ((ctx (ghostherd-test--call calls "setContext"))
           (back (json-parse-string (json-encode ctx) :object-type 'alist :array-type 'list)))
      (should (equal (plist-get ctx :view) "sessions"))
      (should (equal (plist-get ctx :current_project) "/p"))
      (should (equal (mapcar (lambda (s) (alist-get 'source_path s)) (alist-get 'sources back))
                     '("/a" "/b"))))))

(ert-deftest ghostherd-test-memory-page-links-and-copy ()
  (ghostherd-test--with-page
    (let (browsed copied)
      (cl-letf (((symbol-function 'browse-url) (lambda (u &rest _) (setq browsed u)))
                ((symbol-function 'xwapp-copy) (lambda (s) (setq copied s))))
        (ghostherd-memory-page--handle '((op . "open-browser") (url . "file:///etc/passwd")))
        (ghostherd-memory-page--handle '((op . "open-browser") (url . "javascript:alert(1)")))
        (should-not browsed)
        (ghostherd-memory-page--handle '((op . "open-browser") (url . "https://example.com/x")))
        (should (equal browsed "https://example.com/x"))
        (ghostherd-memory-page--handle '((op . "copy") (text . "a message")))
        (should (equal copied "a message"))
        (should (equal (ghostherd-test--call calls "flash") "Copied"))))))

(ert-deftest ghostherd-test-memory-page-errors-reach-the-page ()
  (ghostherd-test--with-page
    (ghostherd-memory-page--handle '((op . "open-source") (source_path . "")))
    (should (string-match-p "No source" (ghostherd-test--call calls "showError")))))


(ert-deftest ghostherd-test-memory-page-import-progress ()
  "Progress while the sidecar imports; the end of a background import
is announced and lists the sessions again."
  (ghostherd-test--with-page
    (cl-letf (((symbol-function 'xwapp-live-p) (lambda (_) t)))
      (setq reply (lambda (m _p)
                    (if (equal m "memory_status")
                        '(:import (:running t :files 3 :imported 223 :agent "claude"
                                   :current "/Users/x/.claude/projects/p/a0c6b62b-e1d2.jsonl"))
                      '(:sources nil :total 0))))
      (ghostherd-memory-page--progress-tick)
      (should (equal (ghostherd-test--call calls "importProgress")
                     '(:files 3 :imported 223 :agent "claude" :current "a0c6b62b-e1d2.jsonl")))
      (should ghostherd-memory-page--progress-shown)
      ;; It ends.
      (setq reply (lambda (m _p) (if (equal m "memory_status") '(:import nil) '(:sources nil))))
      (setq calls nil)
      (ghostherd-memory-page--progress-tick)
      (should (assoc "importProgress" calls))
      (should-not (ghostherd-test--call calls "importProgress"))
      (should (equal (ghostherd-test--call calls "flash") "Import finished"))
      (should (assoc "setContext" calls))
      (should-not ghostherd-memory-page--progress-shown)
      ;; Nothing running and nothing shown: quiet.
      (setq calls nil)
      (ghostherd-memory-page--progress-tick)
      (should-not calls))))

(ert-deftest ghostherd-test-memory-page-index-update-progress ()
  "The sidecar updating an older index shows as that, not as an import,
and its end is announced as such."
  (ghostherd-test--with-page
    (cl-letf (((symbol-function 'xwapp-live-p) (lambda (_) t)))
      (setq reply (lambda (m _p)
                    (if (equal m "memory_status")
                        '(:import (:running t :migrating t :done 1280 :total 84040
                                   :current "re-indexing words: 1280 of 84040"))
                      '(:sources nil :total 0))))
      (ghostherd-memory-page--progress-tick)
      (should (equal (ghostherd-test--call calls "importProgress")
                     '(:migrating t :done 1280 :total 84040)))
      (setq reply (lambda (m _p) (if (equal m "memory_status") '(:import nil) '(:sources nil))))
      (setq calls nil)
      (ghostherd-memory-page--progress-tick)
      (should (equal (ghostherd-test--call calls "flash") "Search index updated"))
      (should-not ghostherd-memory-page--progress-shown))))

(ert-deftest ghostherd-test-memory-page-import-waits-for-the-sidecar ()
  "An import just asked for may not have begun at the first read; the
watch must not end before the answer comes."
  (ghostherd-test--with-page
    (cl-letf (((symbol-function 'xwapp-live-p) (lambda (_) t))
              ((symbol-function 'ghostherd-memory-page--unwatch)
               (lambda () (push "unwatch" calls))))
      (setq reply (lambda (_m _p) '(:import nil)))
      (let ((ghostherd-memory-page--importing t))
        (ghostherd-memory-page--progress-tick))
      (should-not calls))))

(ert-deftest ghostherd-test-memory-page-import-flow ()
  (ghostherd-test--with-page
    (setq reply (lambda (m _p)
                  (if (equal m "memory_import") '(:imported 3 :sessions 1) '(:sources nil :total 0))))
    (ghostherd-memory-page--handle '((op . "import")))
    (should (equal (mapcar #'car requests) '("memory_import" "memory_list")))
    (should (equal (plist-get (cdr (assoc "importProgress" calls)) :current) "starting"))
    (should-not (ghostherd-test--call calls "importProgress"))
    (should (equal (ghostherd-test--call calls "flash") "Imported 3 new chunks from 1 sessions"))
    (should-not ghostherd-memory-page--importing)
    (should (>= watched 2))
    ;; A background import already running.
    (setq reply (lambda (m _p) (if (equal m "memory_import") '(:busy t) '(:sources nil))))
    (ghostherd-memory-page--handle '((op . "import")))
    (should (equal (ghostherd-test--call calls "flash") "A background import is already running"))
    ;; A failure.
    (setq fail "sidecar gone")
    (ghostherd-memory-page--handle '((op . "import")))
    (should (string-match-p "Import: sidecar gone" (ghostherd-test--call calls "showError")))
    (should-not ghostherd-memory-page--importing)))


(ert-deftest ghostherd-test-memory-page-log-entry ()
  "A state entry comes apart into old, new and reason; others pass as text."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let* ((t0 (encode-time '(0 14 2 8 10 2026)))
         (blocked (ghostherd-memory-page--log-entry
                   (ghostherd-log-entry--create
                    :time t0 :session "reviewer" :kind 'state
                    :text "working → blocked  Bash(rm -rf build/) — proceed?\n  1. Yes"
                    :screen "the screen")))
         (plain (ghostherd-memory-page--log-entry
                 (ghostherd-log-entry--create :time t0 :session "a" :kind 'state :text "starting → idle")))
         (input (ghostherd-memory-page--log-entry
                 (ghostherd-log-entry--create :time t0 :session "a" :kind 'input :text "← review the diff"))))
    (should (equal (plist-get blocked :t) (* 1000 (float-time t0))))
    (should (equal (plist-get blocked :old) "working"))
    (should (equal (plist-get blocked :new) "blocked"))
    (should (equal (plist-get blocked :reason) "Bash(rm -rf build/) — proceed?\n  1. Yes"))
    (should (equal (plist-get blocked :screen) "the screen"))
    (should (equal (plist-get plain :new) "idle"))
    (should (equal (plist-get plain :reason) ""))
    (should-not (plist-member plain :screen))
    (should (equal (plist-get input :kind) "input"))
    (should-not (plist-member input :new))))

(ert-deftest ghostherd-test-memory-page-log-flow ()
  "The log is sent oldest first, read from the file if need be; new
entries reach an open page only."
  (ghostherd-test--with-page
    (let ((ghostherd--log nil) (ghostherd--log-loaded nil) (loaded nil))
      (cl-letf (((symbol-function 'ghostherd-log-load)
                 (lambda ()
                   (setq loaded t ghostherd--log-loaded t
                         ghostherd--log
                         (list (ghostherd-log-entry--create :session "b" :kind 'state :text "idle → working")
                               (ghostherd-log-entry--create :session "a" :kind 'life :text "spawned claude on tmux"))))))
        (ghostherd-memory-page--handle '((op . "log")))
        (should loaded)
        (let ((entries (plist-get (ghostherd-test--call calls "renderLog") :entries)))
          (should (vectorp entries))
          (should (equal (mapcar (lambda (e) (plist-get e :session)) entries) '("a" "b")))))
      (setq calls nil)
      (let ((e (ghostherd-log-entry--create :session "c" :kind 'state :text "working → done")))
        (cl-letf (((symbol-function 'xwapp-session) (lambda (_) nil)))
          (ghostherd-memory-page--on-log e))
        (should-not calls)
        (cl-letf (((symbol-function 'xwapp-session) (lambda (_) t)))
          (ghostherd-memory-page--on-log e))
        (should (equal (plist-get (ghostherd-test--call calls "logAdd") :new) "done"))))))

(ert-deftest ghostherd-test-memory-page-log-needs-no-sidecar ()
  "Asked for the log, the page shows it before the sessions answer."
  (ghostherd-test--with-page
    (setq reply (lambda (_m _p) '(:sources nil :total 0)))
    (let ((ghostherd-memory-page--pending '(:view "log")))
      (ghostherd-memory-page--handle '((op . "ready")))
      (should-not ghostherd-memory-page--pending))
    (should (equal (car (car calls)) "showView"))
    (should (equal (cdr (car calls)) "log"))
    (should (assoc "setContext" calls))))

(ert-deftest ghostherd-test-log-functions-hear-each-entry ()
  "Listeners get each entry; a broken one costs the herd nothing."
  (let ((ghostherd--log nil) (ghostherd-log-file nil) (heard nil)
        (ghostherd-log-functions nil))
    (add-hook 'ghostherd-log-functions (lambda (e) (push (ghostherd-log-entry-text e) heard)))
    (add-hook 'ghostherd-log-functions (lambda (_) (error "Listener bug")))
    (cl-letf (((symbol-function 'message) #'ignore))
      (ghostherd--log-add "a" 'state "idle → working"))
    (should (equal heard '("idle → working")))
    (should (equal (ghostherd-log-entry-text (car ghostherd--log)) "idle → working"))))

;;; The agents tab

(defvar ghostherd-test--prs-app)
(defvar ghostherd-test--closed-app)

(ert-deftest ghostherd-test-herd-says-what-opens-on-the-phone ()
  "The page is told which of pr-view and clickup-view your other devices
can open, by Emacs's address for them that holds no secret: loaded,
able to open by themselves, with somewhere to be reached from.  Not
so, a chip there goes to the forge or ClickUp."
  (ghostherd-test--with-herd
    (let ((ghostherd-test--prs-app (xwapp-create :buffer-name " *t-prs*" :index "/x/prs/ui/index.html"
                                                 :reopen #'ignore))
          (ghostherd-test--closed-app (xwapp-create :buffer-name " *t-closed*" :index "/x/c/ui/index.html"))
          (xwapp-remote-hosts '("box.tail0.ts.net")))
      (should (eq (ghostherd-memory-page--opens-elsewhere 'ghostherd-test--prs-app) t))
      (should (eq (ghostherd-memory-page--opens-elsewhere 'ghostherd-test--closed-app) :json-false))
      (should (eq (ghostherd-memory-page--opens-elsewhere 'ghostherd-test--never-loaded) :json-false))
      (let ((xwapp-remote-hosts nil))
        (should (eq (ghostherd-memory-page--opens-elsewhere 'ghostherd-test--prs-app) :json-false)))
      (cl-letf (((symbol-function 'ghostherd-memory-page--opens-elsewhere)
                 (lambda (var) (if (eq var 'pr-view--app) t :json-false))))
        (ghostherd-memory-page--send-herd)
        (should (equal (plist-get (ghostherd-test--call calls "renderHerd") :open)
                       '(:pr t :task :json-false))))
      (remhash ghostherd-test--prs-app xwapp--served)
      (remhash ghostherd-test--closed-app xwapp--served))))

(ert-deftest ghostherd-test-herd-page-snapshot ()
  (ghostherd-test--with-herd
    (ghostherd-memory-page--send-herd)
    (let* ((p (ghostherd-test--call calls "renderHerd"))
           (a (aref (plist-get p :agents) 0))
           (claude (seq-find (lambda (u) (equal (plist-get u :kind) "claude")) (plist-get p :usage)))
           (agy (seq-find (lambda (u) (equal (plist-get u :kind) "agy")) (plist-get p :usage))))
      (should (equal (plist-get a :state) "blocked"))
      (should (equal (plist-get a :project) "/p/"))
      (should (eq (plist-get a :detached) t))
      (should (equal (mapcar (lambda (w) (plist-get w :used)) (plist-get claude :windows)) '(62.0 0.0)))
      (should (equal (plist-get (aref (plist-get claude :windows) 0) :resets) 1791460000000.0))
      (should (equal (plist-get agy :error) "token expired"))
      (should (member "claude" (append (plist-get p :kinds) nil)))
      (should-not (member "shell" (append (plist-get p :kinds) nil)))
      (should (json-encode p)))))

(ert-deftest ghostherd-test-herd-page-sends-asks ()
  (ghostherd-test--with-herd
    (let ((ghostherd--herd-asks
           '((:id "a1" :from "claude-api" :to "agy-a" :status "answered" :auto t
                  :error nil :head "fetch the docs" :reply_head "screen line"
                  :created_ms 1791450000000.0 :answered_ms 1791450060000.0
                  :parent "a0" :run "a0" :round 2 :of 4))))
      (ghostherd-memory-page--send-herd)
      (let ((k (aref (plist-get (ghostherd-test--call calls "renderHerd") :asks) 0)))
        (should (equal (plist-get k :id) "a1"))
        (should (equal (plist-get k :status) "answered"))
        (should (eq (plist-get k :auto) t))
        (should (equal (plist-get k :replyHead) "screen line"))
        (should (equal (plist-get k :answered) 1791450060000.0))
        (should (equal (list (plist-get k :parent) (plist-get k :run) (plist-get k :round) (plist-get k :of))
                       '("a0" "a0" 2 4)))
        (should (json-encode k))))))

(ert-deftest ghostherd-test-herd-page-shows-what-each-said ()
  (ghostherd-test--with-herd
    (let ((ghostherd--herd-links (make-hash-table :test 'equal)))
      (ghostherd--herd-take-links
       '((:session "claude-api" :cli "claude" :conversation "c-1"
                   :last_head "Tests pass." :last_ms 1791450000000.0)))
      (ghostherd-memory-page--send-herd)
      (let ((a (aref (plist-get (ghostherd-test--call calls "renderHerd") :agents) 0)))
        (should (equal (plist-get a :said) "Tests pass."))
        (should (equal (plist-get a :saidAt) 1791450000000.0))
        (should (equal (plist-get a :conversation) "c-1"))))))

(ert-deftest ghostherd-test-herd-page-hooks ()
  "The page gets each hook with an index, and opens only by that index."
  (ghostherd-test--with-herd
    (let* ((file (make-temp-file "gh-settings" nil ".json" "{\n  \"hooks\": {\n    \"Stop\": [\n      \"cmd\"\n    ]\n  }\n}\n"))
           (ghostherd-memory-page--hooks
            `(:ghostherd (:claude (:have ("Stop") :want ("SessionStart" "Stop")))
              :hooks ((:file ,file :line 4 :readers ("claude" "grok") :scope "global"
                             :source "claude settings.json" :event "Stop" :matcher ""
                             :command "cmd" :ours :json-false :enabled t)))))
      (unwind-protect
          (progn
            (ghostherd-memory-page--send-herd)
            (let* ((hooks (plist-get (ghostherd-test--call calls "renderHerd") :hooks))
                   (h (aref (plist-get hooks :list) 0)))
              (should (equal (plist-get h :i) 0))
              (should (equal (plist-get h :name) (file-name-nondirectory file)))
              (should (equal (plist-get h :readers) ["claude" "grok"]))
              (should (equal (plist-get (plist-get (plist-get hooks :status) :claude) :have) ["Stop"]))
              (should (json-encode hooks)))
            (save-window-excursion
              (ghostherd-memory-page--hook-open '((i . 0)))
              (should (equal (buffer-file-name) file))
              (should (= (line-number-at-pos) 4))
              (kill-buffer))
            (should-error (ghostherd-memory-page--hook-open '((i . 7))) :type 'user-error)
            (should-error (ghostherd-memory-page--hook-open `((i . ,file))) :type 'user-error))
        (delete-file file)))))

(ert-deftest ghostherd-test-herd-page-hooks-nothing-installed ()
  "Nothing installed is an empty list, and an empty list must reach the
page as [] -- json.el writes nil as null, and the page stopped drawing."
  (ghostherd-test--with-herd
    (let ((ghostherd-memory-page--hooks
           (json-parse-string
            "{\"ghostherd\":{\"claude\":{\"have\":[],\"want\":[\"Stop\"]}},\"hooks\":[],\"files\":[]}"
            :object-type 'plist :array-type 'list :null-object nil :false-object :json-false)))
      (ghostherd-memory-page--send-herd)
      (let ((json (json-encode (plist-get (ghostherd-test--call calls "renderHerd") :hooks))))
        (should (string-search "\"have\":[]" json))
        (should (string-search "\"want\":[\"Stop\"]" json))
        (should (string-search "\"list\":[]" json))))))

(ert-deftest ghostherd-test-herd-page-hooks-refresh-asks-for-each-project ()
  "The inventory runs in the background, with every herd project's own."
  (ghostherd-test--with-herd
    (let* ((dir (make-temp-file "gh-client" t))
           (client (expand-file-name "herd" dir))
           (ghostherd-memory-page--hooks nil)
           (ghostherd-memory-page--hooks-proc nil))
      (with-temp-file client
        (insert "#!/bin/sh\nprintf '%s\\n' \"$*\" > \"$(dirname \"$0\")/args\"\n"
                "echo '{\"ghostherd\":{},\"hooks\":[{\"file\":\"/f\",\"line\":1,\"readers\":[\"agy\"],\"ours\":false,\"enabled\":true}],\"files\":[]}'\n"))
      (set-file-modes client #o755)
      (cl-letf (((symbol-function 'ghostherd-herd-client) (lambda () client))
                ((symbol-function 'xwapp-session) (lambda (&rest _) t)))
        (ghostherd-memory-page--hooks-refresh)
        (let ((end (+ (float-time) 10)))
          (while (and (not ghostherd-memory-page--hooks) (< (float-time) end))
            (accept-process-output nil 0.05))))
      (should (equal (plist-get (car (plist-get ghostherd-memory-page--hooks :hooks)) :file) "/f"))
      (should (equal (with-temp-buffer (insert-file-contents (expand-file-name "args" dir))
                                       (string-trim (buffer-string)))
                     "hooks list --json --project /p/"))
      (should (ghostherd-test--call calls "renderHerd"))
      (delete-directory dir t))))

(defmacro ghostherd-test--with-room (&rest body)
  "BODY in the room of /p/, claude-api's transcript at `transcript'."
  (declare (indent 0))
  `(ghostherd-test--with-herd
     (let* ((file (make-temp-file "gh-room" nil ".jsonl" "{}\n"))
            (transcript file)
            (ghostherd-memory-page--room "/p/")
            (ghostherd-memory-page--room-screens nil)
            (ghostherd-memory-page--room-timer nil)
            (ghostherd-memory-page--room-sent (make-hash-table :test 'equal))
            (ghostherd-memory-page--room-reading (make-hash-table :test 'equal))
            (ghostherd-memory-page--room-chat nil)
            (ghostherd-memory-page--chat-sent nil)
            (ghostherd-memory-page--chat-reading nil)
            (ghostherd-memory-page--room-limits (make-hash-table :test 'equal))
            (ghostherd-memory-page--chat-limit nil))
       (cl-letf (((symbol-function 'ghostherd-session-transcript) (lambda (_s) transcript))
                 ((symbol-function 'ghostherd--host-capture) (lambda (&rest _) "the screen"))
                 ((symbol-function 'xwapp-session) (lambda (&rest _) t)))
         (unwind-protect (progn ,@body)
           (ghostherd-memory-page--room-close)
           (delete-file file))))))

(ert-deftest ghostherd-test-the-primary-room-is-a-chat-whatever-the-view ()
  "The primary room (primary's directory, not a git checkout, no columns view)
is read as a chat even when the page asks for columns -- the desktop default
-- so it is never stuck 'Reading the conversations…' behind an empty columns
read.  A normal room follows the view it is given."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let ((ghostherd-memory-page--room nil)
        (ghostherd-memory-page--room-chat nil)
        (ghostherd-memory-page--room-timer nil)
        (ghostherd-memory-page--room-screens nil)
        (ghostherd-memory-page--chat-sent nil)
        (ghostherd-memory-page--chat-limit nil)
        (ghostherd-memory-page--room-sent (make-hash-table :test 'equal))
        (ghostherd-memory-page--room-limits (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'ghostherd-memory-page--room-tick) #'ignore)
              ((symbol-function 'ghostherd--primary-dir-p)
               (lambda (p) (equal p "~/.ghostherd/primary/"))))
      ;; The primary room, asked for columns (the desktop default), is a chat.
      (ghostherd-memory-page--room-open '((project . "~/.ghostherd/primary/") (view . "columns")))
      (should ghostherd-memory-page--room-chat)
      (ghostherd-memory-page--room-close)
      ;; A normal room follows the view it is given.
      (ghostherd-memory-page--room-open '((project . "~/src/x/") (view . "columns")))
      (should-not ghostherd-memory-page--room-chat)
      (ghostherd-memory-page--room-close)
      (ghostherd-memory-page--room-open '((project . "~/src/x/") (view . "chat")))
      (should ghostherd-memory-page--room-chat)
      (ghostherd-memory-page--room-close))))

(ert-deftest ghostherd-test-reopening-a-room-resends-it ()
  "A room-watch is always a (re)open: a page that reloaded has lost its
conversation while Emacs kept the room, so `--room-open' clears what the page
is thought to already have -- even for the same room and view -- so the next
tick re-sends it, not leaves it on \"Reading the conversations…\"."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let ((ghostherd-memory-page--room nil)
        (ghostherd-memory-page--room-chat nil)
        (ghostherd-memory-page--room-timer nil)
        (ghostherd-memory-page--room-screens nil)
        (ghostherd-memory-page--chat-sent nil)
        (ghostherd-memory-page--chat-limit nil)
        (ghostherd-memory-page--room-sent (make-hash-table :test 'equal))
        (ghostherd-memory-page--room-limits (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'ghostherd-memory-page--room-tick) #'ignore)
              ((symbol-function 'ghostherd--primary-dir-p) (lambda (_p) nil)))
      (ghostherd-memory-page--room-open '((project . "~/p/") (view . "chat")))
      ;; Pretend the page has been sent the room's state.
      (setq ghostherd-memory-page--chat-sent '(a-signature))
      (puthash "col-a" t ghostherd-memory-page--room-sent)
      ;; A screen toggle (same room and view, no reopen) must NOT re-read.
      (ghostherd-memory-page--room-open '((project . "~/p/") (view . "chat") (screens . ["x"])))
      (should (equal ghostherd-memory-page--chat-sent '(a-signature)))
      (should (= 1 (hash-table-count ghostherd-memory-page--room-sent)))
      ;; A reopen (a page reload re-opening the same room) DOES: it clears.
      (ghostherd-memory-page--room-open '((project . "~/p/") (view . "chat") (reopen . t)))
      (should-not ghostherd-memory-page--chat-sent)
      (should (zerop (hash-table-count ghostherd-memory-page--room-sent)))
      (ghostherd-memory-page--room-close))))

(ert-deftest ghostherd-test-room-reads-a-conversation-once-per-change ()
  "The sidecar reads a transcript when it changed, not every 2s; the page
gets each entry's known fields, lists as arrays."
  (ghostherd-test--with-room
    (setq reply (lambda (_m _p)
                  '(:cli "claude" :earlier t
                    :entries ((:role "user" :kind "prompt" :text "hi" :ts "2026-10-09T01:00:00Z" :secret "x")
                              (:role "tools" :tools ((:name "Bash" :hint "ls")))
                              (:role "herd" :kind "ask" :to "qa" :ask "26910956" :text "round 2" :waiting t)))))
    (ghostherd-memory-page--room-tick)
    (should (equal requests `(("herd_conversation" (:path ,file)))))
    (let* ((p (ghostherd-test--call calls "setConversation"))
           (e (aref (plist-get p :entries) 0))
           (tools (plist-get (aref (plist-get p :entries) 1) :tools)))
      (should (equal (plist-get p :name) "claude-api"))
      (should (eq (plist-get p :earlier) t))
      (should (equal (plist-get e :text) "hi"))
      (should-not (plist-member e :secret))
      (should (equal (plist-get (aref tools 0) :hint) "ls"))
      (let ((ask (aref (plist-get p :entries) 2)))
        (should (equal (list (plist-get ask :ask) (plist-get ask :waiting) (plist-get ask :failed))
                       '("26910956" t :json-false))))
      (should (string-match-p "\"entries\":\\[{.*\"tools\":\\[{\"name\":\"Bash\"" (json-encode p))))
    (should-not (ghostherd-test--call calls "setScreen"))
    (ghostherd-memory-page--room-tick)
    (should (= (length requests) 1))
    (write-region "more\n" nil file t 'silent)
    (ghostherd-memory-page--room-tick)
    (should (= (length requests) 2))))

(ert-deftest ghostherd-test-room-shows-a-sub-agents-transcript ()
  "Open a sub-agent card: its own transcript is read with
`herd_sub_conversation' and pushed as setSubConversation, re-read only when
it changes; closing it forgets it."
  (ghostherd-test--with-room
    (setq reply (lambda (_m _p)
                  '(:earlier :json-false
                    :entries ((:role "user" :kind "prompt" :text "Review x" :ts "t")
                              (:role "assistant" :text "looking" :ts "t")))))
    (let ((subf (ghostherd-memory-page--sub-file file "a51b15851f7c7ae94")))
      (make-directory (file-name-directory subf) t)
      (write-region "{}\n" nil subf nil 'silent)
      (ghostherd-memory-page--handle
       `((op . "sub-watch") (name . "claude-api") (path . ,file) (sub . "a51b15851f7c7ae94")))
      (should (equal (car (last requests))
                     `("herd_sub_conversation" (:path ,file :sub "a51b15851f7c7ae94"))))
      (let ((p (ghostherd-test--call calls "setSubConversation")))
        (should (equal (plist-get p :name) "claude-api"))
        (should (equal (plist-get p :sub) "a51b15851f7c7ae94"))
        (should (equal (plist-get (aref (plist-get p :entries) 1) :text) "looking")))
      ;; Not re-read until the sub transcript itself changes.
      (let ((n (length requests)))
        (ghostherd-memory-page--sub-read)
        (should (= (length requests) n))
        (write-region "more\n" nil subf t 'silent)
        (ghostherd-memory-page--sub-read)
        (should (= (length requests) (1+ n))))
      (ghostherd-memory-page--handle '((op . "sub-unwatch")))
      (should-not ghostherd-memory-page--sub)
      (let ((n (length requests)))
        (ghostherd-memory-page--sub-read)
        (should (= (length requests) n))))))

(ert-deftest ghostherd-test-room-opens-a-sub-from-the-name-without-a-path ()
  "The chat view keeps no per-agent conversation, so it opens a sub with an
empty path.  Emacs resolves the transcript from the agent's name, so the
sub is still read -- not left unknown."
  (ghostherd-test--with-room
    (setq reply (lambda (_m _p)
                  '(:earlier :json-false
                    :entries ((:role "assistant" :text "looking" :ts "t")))))
    (let ((subf (ghostherd-memory-page--sub-file file "a51b15851f7c7ae94")))
      (make-directory (file-name-directory subf) t)
      (write-region "{}\n" nil subf nil 'silent)
      (ghostherd-memory-page--handle
       '((op . "sub-watch") (name . "claude-api") (path . "") (sub . "a51b15851f7c7ae94")))
      (should (equal ghostherd-memory-page--sub
                     (list "claude-api" file "a51b15851f7c7ae94")))
      (should (equal (car (last requests))
                     `("herd_sub_conversation" (:path ,file :sub "a51b15851f7c7ae94")))))))

(ert-deftest ghostherd-test-room-load-earlier-reads-more-and-keeps-it ()
  "A column's `load earlier' asks for more; Emacs reads at that limit, and
-- the limit being part of the signature -- keeps it across polls instead
of shrinking back to the default."
  (ghostherd-test--with-room
    (setq reply (lambda (_m _p)
                  '(:cli "claude" :earlier t :limit 120
                    :entries ((:role "assistant" :text "hi" :ts "t")))))
    (ghostherd-memory-page--room-tick)
    (should (equal (car (last requests)) `("herd_conversation" (:path ,file))))
    (should (eql (plist-get (ghostherd-test--call calls "setConversation") :limit) 120))
    (ghostherd-memory-page--handle
     '((op . "room-watch") (project . "/p/") (screens . [])
       (limits . [((name . "claude-api") (limit . 120))]) (chatLimit . 0)))
    (should (equal (car (last requests)) `("herd_conversation" (:path ,file :limit 120))))
    (let ((n (length requests)))
      (ghostherd-memory-page--room-tick)
      (should (= (length requests) n)))))

(ert-deftest ghostherd-test-chat-load-earlier-asks-for-more ()
  "The chat's `load earlier' grows its one limit, passed on to herd_chat."
  (ghostherd-test--with-room
    (setq reply (lambda (_m _p) '(:earlier :json-false :entries [] :missing [])))
    (ghostherd-memory-page--handle '((op . "room-watch") (project . "/p/") (screens . []) (view . "chat")))
    (should (equal (caar (last requests)) "herd_chat"))
    (should-not (plist-member (cadar (last requests)) :limit))
    (ghostherd-memory-page--handle
     '((op . "room-watch") (project . "/p/") (screens . []) (view . "chat") (chatLimit . 400)))
    (should (eql (plist-get (cadar (last requests)) :limit) 400))))

(ert-deftest ghostherd-test-room-as-one-chat ()
  "Shown as a chat, the room asks the sidecar for the project's timeline
when any transcript changed -- once, not per agent -- naming the agents
it knows none for; the page gets each item's known fields.  Back to
columns, every column is sent afresh: they were not kept up meanwhile."
  (ghostherd-test--with-room
    (puthash "claude-web" (ghostherd-session--create :id "claude-web" :name "claude-web" :kind 'claude
                                                     :state 'idle :project "/p/" :backend 'tmux)
             ghostherd--sessions)
    (cl-letf (((symbol-function 'ghostherd-session-transcript)
               (lambda (s) (and (equal (ghostherd-session-name s) "claude-api") transcript))))
      (setq reply (lambda (m _p)
                    (when (equal m "herd_chat")
                      '(:earlier t :missing ("gone")
                        :entries ((:who "user" :to "claude-api" :kind "prompt" :text "hi"
                                   :ts "2026-10-09T01:00:00Z" :agent "claude-api" :secret "x"))))))
      ;; In columns first, then the chat.
      (ghostherd-memory-page--handle '((op . "room-watch") (project . "/p/") (screens . [])))
      (should (ghostherd-test--call calls "setConversation"))
      (setq calls nil requests nil)
      (ghostherd-memory-page--handle '((op . "room-watch") (project . "/p/") (screens . []) (view . "chat")))
      (should (equal (mapcar #'car requests) '("herd_chat")))
      (should (equal (plist-get (cadr (car requests)) :agents) (vector (list :name "claude-api" :path file))))
      (let* ((p (ghostherd-test--call calls "setChat"))
             (e (aref (plist-get p :entries) 0)))
        (should (equal (list (plist-get p :project) (plist-get p :absent) (plist-get p :missing) (plist-get p :earlier))
                       '("/p/" ["claude-web"] ["gone"] t)))
        (should (equal (list (plist-get e :who) (plist-get e :to) (plist-get e :text) (plist-get e :failed))
                       '("user" "claude-api" "hi" :json-false)))
        (should-not (plist-member e :secret))
        (should (string-match-p "\"entries\":\\[{" (json-encode p))))
      (should-not (ghostherd-test--call calls "setConversation"))
      (ghostherd-memory-page--room-tick)
      (should (= (length requests) 1))
      (write-region "more\n" nil file t 'silent)
      (ghostherd-memory-page--room-tick)
      (should (= (length requests) 2))
      ;; Back to columns: each column again, though its transcript is as it was.
      (setq calls nil requests nil)
      (ghostherd-memory-page--handle '((op . "room-watch") (project . "/p/") (screens . []) (view . "columns")))
      (should (equal (mapcar #'car requests) '("herd_conversation")))
      (should (ghostherd-test--call calls "setConversation"))
      ;; And with the transcript as it was, too.
      (ghostherd-memory-page--handle '((op . "room-watch") (project . "/p/") (screens . []) (view . "chat")))
      (setq calls nil)
      (ghostherd-memory-page--handle '((op . "room-watch") (project . "/p/") (screens . []) (view . "columns")))
      (should (ghostherd-test--call calls "setConversation"))
      ;; A sidecar older than the chat says how to fix it.
      (setq calls nil fail "JSON-RPC -32601: Method not found: herd_chat")
      (ghostherd-memory-page--handle '((op . "room-watch") (project . "/p/") (screens . []) (view . "chat")))
      (should (string-match-p "older than the room" (plist-get (ghostherd-test--call calls "setChat") :error))))))

(ert-deftest ghostherd-test-room-without-a-transcript-shows-the-screen ()
  (ghostherd-test--with-room
    (setq transcript nil)
    (ghostherd-memory-page--room-tick)
    (ghostherd-memory-page--room-tick)
    (should-not requests)
    (should (equal (mapcar #'cdr (seq-filter (lambda (c) (equal (car c) "setConversation")) calls))
                   '((:name "claude-api" :entries [] :none t))))
    (should (equal (length (seq-filter (lambda (c) (equal (car c) "setScreen")) calls)) 2))
    (should (equal (plist-get (ghostherd-test--call calls "setScreen") :text) "the screen"))))

(ert-deftest ghostherd-test-room-screens-and-failures ()
  "A column the page shows by its screen gets the screen; a sidecar that
cannot read says why, and is not asked again until the file changes."
  (ghostherd-test--with-room
    (setq ghostherd-memory-page--room-screens '("claude-api")
          fail "connection refused")
    (ghostherd-memory-page--room-tick)
    (should (equal (plist-get (ghostherd-test--call calls "setConversation") :error) "connection refused"))
    (should (ghostherd-test--call calls "setScreen"))
    (ghostherd-memory-page--room-tick)
    (should (= (length requests) 1))
    ;; A sidecar from before the room: say what to do about it.
    (setq fail "ghostherd-memory JSON-RPC -32601: Method not found: herd_conversation")
    (write-region "more\n" nil file t 'silent)
    (ghostherd-memory-page--room-tick)
    (should (string-match-p "ghostherd-memory-stop" (plist-get (ghostherd-test--call calls "setConversation") :error)))))

(ert-deftest ghostherd-test-room-drops-a-late-answer ()
  "The room was left while the sidecar read: its answer goes nowhere."
  (ghostherd-test--with-room
    (let (pending)
      (cl-letf (((symbol-function 'ghostherd-memory-request-async)
                 (lambda (_m cb &rest _) (setq pending cb))))
        (ghostherd-memory-page--room-tick)
        (ghostherd-memory-page--room-tick)
        (ghostherd-memory-page--room-close)
        (funcall pending '(:entries ((:role "user" :text "late"))))
        (should-not (ghostherd-test--call calls "setConversation"))
        (should (= (hash-table-count ghostherd-memory-page--room-reading) 0))))))

(ert-deftest ghostherd-test-room-intents ()
  "Opening watches the room; the same room again only changes its screens;
closing, or leaving the tab, stops; a page gone stops it too."
  (ghostherd-test--with-room
    (setq ghostherd-memory-page--room nil)
    (ghostherd-memory-page--handle '((op . "room-watch") (project . "/p/") (screens . [])))
    (should (equal ghostherd-memory-page--room "/p/"))
    (should (timerp ghostherd-memory-page--room-timer))
    (should (= (length requests) 1))
    (ghostherd-memory-page--handle '((op . "room-watch") (project . "/p/") (screens . ["claude-api" 3])))
    (should (equal ghostherd-memory-page--room-screens '("claude-api")))
    (should (= (length requests) 1))
    (ghostherd-memory-page--handle '((op . "room-close")))
    (should-not (or ghostherd-memory-page--room ghostherd-memory-page--room-timer))
    (ghostherd-memory-page--handle '((op . "room-watch") (project . "/p/")))
    (ghostherd-memory-page--handle '((op . "herd-unwatch")))
    (should-not ghostherd-memory-page--room-timer)
    (ghostherd-memory-page--handle '((op . "room-watch") (project . "")))
    (should (ghostherd-test--call calls "showError"))
    (should-not ghostherd-memory-page--room)
    (setq ghostherd-memory-page--room "/p/")
    (cl-letf (((symbol-function 'xwapp-session) (lambda (&rest _) nil)))
      (ghostherd-memory-page--room-tick))
    (should-not ghostherd-memory-page--room)))

(ert-deftest ghostherd-test-herd-page-operations ()
  "Each operation reaches the herd function with the session, then the
herd is sent again; bad input is refused before anything is done."
  (ghostherd-test--with-herd
    (let (did)
      (cl-letf (((symbol-function 'ghostherd-prompt) (lambda (s text &rest _) (push (list 'prompt (ghostherd-session-name s) text) did)))
                ((symbol-function 'ghostherd-interrupt) (lambda (s) (push (list 'esc (ghostherd-session-name s)) did)))
                ((symbol-function 'ghostherd-abort) (lambda (s) (push (list 'abort (ghostherd-session-name s)) did)))
                ((symbol-function 'ghostherd-answer) (lambda (s n) (push (list 'answer (ghostherd-session-name s) n) did)))
                ((symbol-function 'ghostherd-respawn) (lambda (s c) (push (list 'respawn (ghostherd-session-name s) c) did)))
                ((symbol-function 'ghostherd-kill) (lambda (s k) (push (list 'kill (ghostherd-session-name s) k) did)))
                ((symbol-function 'ghostherd-set-notes) (lambda (s n) (push (list 'notes (ghostherd-session-name s) n) did)))
                ((symbol-function 'ghostherd-visit) (lambda (s) (push (list 'visit (ghostherd-session-name s)) did)))
                ((symbol-function 'ghostherd--host-capture) (lambda (_s &rest _) "SCREEN")))
        (dolist (intent '(((op . "agent-prompt") (name . "claude-api") (text . "run the tests"))
                          ((op . "agent-interrupt") (name . "claude-api"))
                          ((op . "agent-abort") (name . "claude-api"))
                          ((op . "agent-answer") (name . "claude-api") (n . 2))
                          ((op . "agent-respawn") (name . "claude-api") (continue . t))
                          ((op . "agent-notes") (name . "claude-api") (text . "reviewer"))
                          ((op . "agent-visit") (name . "claude-api"))
                          ((op . "agent-kill") (name . "claude-api"))))
          (ghostherd-memory-page--handle intent))
        (should (equal (reverse did)
                       '((prompt "claude-api" "run the tests") (esc "claude-api") (abort "claude-api")
                         (answer "claude-api" 2) (respawn "claude-api" t) (notes "claude-api" "reviewer")
                         (visit "claude-api") (kill "claude-api" t))))
        (should (equal (ghostherd-test--call calls "flash") "Killed claude-api"))
        ;; Each sends the herd after it -- again, when it did not change.
        (should (>= (length (seq-filter (lambda (c) (member (car c) '("renderHerd" "renderHerdAgain"))) calls)) 8))
        (ghostherd-memory-page--handle '((op . "agent-screen") (name . "claude-api")))
        (should (equal (ghostherd-test--call calls "setScreen") '(:name "claude-api" :text "SCREEN")))
        ;; Refused, and nothing done.
        (setq did nil)
        (dolist (bad '(((op . "agent-prompt") (name . "claude-api") (text . "   "))
                       ((op . "agent-answer") (name . "claude-api") (n . 12))
                       ((op . "agent-answer") (name . "claude-api") (n . "2"))
                       ((op . "agent-kill") (name . "nobody"))))
          (setq calls nil)
          (ghostherd-memory-page--handle bad)
          (should (ghostherd-test--call calls "showError")))
        (should-not did)))))

(ert-deftest ghostherd-test-herd-page-new-agent ()
  (ghostherd-test--with-herd
    (let ((dir (make-temp-file "gh-new-" t)) spawned)
      (unwind-protect
          (cl-letf (((symbol-function 'ghostherd-spawn)
                     (lambda (kind &rest plist)
                       (setq spawned (cons kind plist))
                       (ghostherd-session--create :name (plist-get plist :name))))
                    ((symbol-function 'ghostherd-project-defaults) (lambda (&rest _) nil))
                    ((symbol-function 'ghostherd--project-root) (lambda (&rest _) nil)))
            (ghostherd-memory-page--handle `((op . "agent-new") (project . ,dir) (kind . "grok") (name . "")))
            (should (eq (car spawned) 'grok))
            (should (equal (plist-get (cdr spawned) :name) (format "grok-%s" (file-name-nondirectory dir))))
            (should (null (plist-get (cdr spawned) :display)))
            (should (plist-member (cdr spawned) :display))
            (should (string-prefix-p "Started grok-" (ghostherd-test--call calls "flash")))
            (setq spawned nil)
            (dolist (bad `(((op . "agent-new") (project . ,dir) (kind . "shell"))
                           ((op . "agent-new") (project . ,dir) (kind . "rm"))
                           ((op . "agent-new") (project . "/nope/here") (kind . "claude"))
                           ((op . "agent-new") (project . ,dir) (kind . "claude") (name . "-t evil"))
                           ((op . "agent-new") (project . ,dir) (kind . "claude") (name . "a;b"))))
              (setq calls nil)
              (ghostherd-memory-page--handle bad)
              (should (ghostherd-test--call calls "showError")))
            (should-not spawned))
        (delete-directory dir t)))))

;;; What a project works on

(defmacro ghostherd-test--with-checkouts (&rest body)
  "BODY with `repo', a git repository on feat/x, and `wt', a worktree of
it on feat/y; the work caches its own, and `asked' recording what
pr-view, clickup-view and the sidecar are asked, which answer as `prs',
`briefs', `by-id', `remote' and `said' say.  An agent's transcript is
as `transcripts' says.  `counted' holds the checkouts whose changes
pr-view counted, as `stats' says them."
  (declare (indent 0))
  `(let* ((_ (skip-unless (require 'ghostherd-memory-page nil t)))
          ;; Its true name: git answers with it, and /var is /private/var.
          (base (file-name-as-directory (file-truename (make-temp-file "gh-work" t))))
          (repo (concat base "repo/"))
          (wt (concat base "wt/"))
          (git (lambda (dir &rest args)
                 (let ((default-directory dir))
                   (apply #'call-process "git" nil nil nil
                          "-c" "user.email=t@t" "-c" "user.name=t" args))))
          (asked nil)
          (prs nil)
          (briefs nil)
          (by-id nil)
          (remote nil)
          (said nil)
          (transcripts nil)
          (counted nil)
          (stats nil)
          (ghostherd-memory-page--changes (make-hash-table :test 'equal))
          (ghostherd-memory-page--work-at 0)
          (ghostherd-memory-page--look nil)
          (ghostherd-memory-page--chores nil)
          (ghostherd-memory-page--chore-timer nil)
          (ghostherd-memory-page--checkouts (make-hash-table :test 'equal))
          (ghostherd-memory-page--prs (make-hash-table :test 'equal))
          (ghostherd-memory-page--task-briefs (make-hash-table :test 'equal))
          (ghostherd-memory-page--links (make-hash-table :test 'equal))
          (ghostherd-memory-page--pr-briefs (make-hash-table :test 'equal))
          (ghostherd-memory-page--remotes (make-hash-table :test 'equal)))
     (make-directory repo t)
     (funcall git repo "init" "-q" "-b" "main")
     (funcall git repo "commit" "-q" "--allow-empty" "-m" "start")
     (funcall git repo "checkout" "-q" "-b" "feat/x")
     (funcall git repo "worktree" "add" "-q" "-b" "feat/y" wt)
     (unwind-protect
         (cl-letf (((symbol-function 'pr-view-branch-pr)
                    (lambda (dir cb) (push (list 'pr dir) asked)
                      (funcall cb (cdr (assoc (abbreviate-file-name dir) prs)))))
                   ((symbol-function 'clickup-view-task-brief)
                    (lambda (id cb) (push (list 'task id) asked)
                      (funcall cb (or (cdr (assoc id briefs)) `((id . ,id) (error . "no such task"))))))
                   ((symbol-function 'pr-view-remote) (lambda (_dir) remote))
                   ((symbol-function 'pr-view-local-stat)
                    (lambda (dir) (push dir counted) (cdr (assoc dir stats))))
                   ((symbol-function 'pr-view-repo-pr-brief)
                    (lambda (remote id cb) (push (list 'brief remote id) asked)
                      (funcall cb (or (cdr (assoc id by-id)) '((error . "no such pull request"))))))
                   ((symbol-function 'ghostherd-session-transcript)
                    (lambda (s) (cdr (assoc (ghostherd-session-name (ghostherd-get s)) transcripts))))
                   ((symbol-function 'ghostherd-memory-request-async)
                    (lambda (method cb &optional params _err)
                      (push (list 'links method (plist-get params :path)) asked)
                      (funcall cb (cdr (assoc (plist-get params :path) said))))))
           ,@body)
       (when (timerp ghostherd-memory-page--chore-timer)
         (cancel-timer ghostherd-memory-page--chore-timer))
       (delete-directory base t))))

(defun ghostherd-test--look ()
  "Look at the herd's work, and do every chore it gives, as a pause in
your typing would."
  (ghostherd-memory-page--work-look)
  (while ghostherd-memory-page--chores
    (ghostherd-memory-page--chore-next)))

(ert-deftest ghostherd-test-a-checkout-is-its-root-and-branch ()
  "Its root and branch, a worktree's own; not a repository, or one with
no commit yet, is none."
  (ghostherd-test--with-checkouts
    (should (equal (ghostherd-memory-page--checkout (concat repo "")) (cons (abbreviate-file-name repo) "feat/x")))
    (should (equal (ghostherd-memory-page--checkout wt) (cons (abbreviate-file-name wt) "feat/y")))
    (should-not (ghostherd-memory-page--checkout base))
    (let ((fresh (concat base "fresh/")))
      (make-directory fresh)
      (funcall git fresh "init" "-q")
      (should-not (ghostherd-memory-page--checkout fresh)))
    (should-not (ghostherd-memory-page--checkout "/no/such/dir"))))

(ert-deftest ghostherd-test-the-herd-shows-what-each-project-works-on ()
  "Each project's branch, its PR and the tasks the PR cites; an agent's
only where it is not its project's -- in a worktree.  Asked once while
the answer is fresh; a PR not looked up says so and asks no tasks."
  (ghostherd-test--with-herd
    (ghostherd-test--with-checkouts
      (setq prs `((,(abbreviate-file-name repo)
                   (id . 87) (title . "Sentry fix") (state . "OPEN") (draft . nil) (url . "u87")
                   (tasks . ("86abc1def" "86xyz9876")))
                  (,(abbreviate-file-name wt) (error . "HTTP 500")))
            briefs '(("86abc1def" (id . "86abc1def") (name . "Sentry AI fix")
                      (status . ((name . "in progress") (color . "#4194f6"))) (url . "cu1"))))
      (clrhash ghostherd--sessions)
      (ghostherd-tests--session :name "rd" :kind 'claude :project repo :state 'idle)
      (ghostherd-tests--session :name "qa" :kind 'claude :project repo :state 'idle)
      (cl-letf (((symbol-function 'ghostherd-session-directory)
                 (lambda (s) (if (equal (ghostherd-session-name s) "qa") wt repo)))
                ((symbol-function 'ghostherd--project-name)
                 (lambda (p) (and p (abbreviate-file-name (file-name-as-directory (expand-file-name p)))))))
        (ghostherd-test--look)
        (should (equal (sort (mapcar #'cadr (seq-filter (lambda (a) (eq (car a) 'pr)) asked)) #'string<)
                       (sort (list (abbreviate-file-name repo) (abbreviate-file-name wt)) #'string<)))
        (should (equal (mapcar #'cadr (seq-filter (lambda (a) (eq (car a) 'task)) asked))
                       '("86xyz9876" "86abc1def")))
        (ghostherd-memory-page--send-herd)
        (let* ((p (ghostherd-test--call calls "renderHerd"))
               (w (aref (plist-get p :work) 0))
               (agents (append (plist-get p :agents) nil))
               (of (lambda (n) (plist-get (seq-find (lambda (a) (equal (plist-get a :name) n)) agents) :work))))
          (should (equal (list (plist-get w :project) (plist-get w :branch)) (list (abbreviate-file-name repo) "feat/x")))
          (should (equal (plist-get (plist-get w :pr) :id) 87))
          (should (equal (mapcar (lambda (tk) (list (plist-get tk :id) (plist-get tk :name) (plist-get tk :color) (plist-get tk :error)))
                                 (append (plist-get w :tasks) nil))
                         '(("86abc1def" "Sentry AI fix" "#4194f6" nil) ("86xyz9876" nil nil "no such task"))))
          (should (eq (funcall of "rd") :json-false))
          (let ((qa (funcall of "qa")))
            (should (equal (plist-get qa :branch) "feat/y"))
            (should (equal (plist-get qa :error) "HTTP 500"))
            (should (eq (plist-get qa :pr) :json-false))))
        ;; Fresh: not asked again.  Stale: asked again.
        (setq asked nil)
        (ghostherd-test--look)
        (should-not asked)
        (maphash (lambda (k v) (puthash k (plist-put v :at 0) ghostherd-memory-page--prs)) ghostherd-memory-page--prs)
        (ghostherd-test--look)
        (should (= (length (seq-filter (lambda (a) (eq (car a) 'pr)) asked)) 2))))))

(ert-deftest ghostherd-test-chores-wait-for-a-pause-in-typing ()
  "Done once you stop typing, a moment's worth at a time, each once
however often asked; nothing left, nothing keeps looking."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let ((ghostherd-memory-page--chores nil)
        (ghostherd-memory-page--chore-timer nil)
        (idle nil) (clock 100.0) (done nil)
        (real-float-time (symbol-function 'float-time)))
    (cl-letf (((symbol-function 'current-idle-time)
               (lambda () (and idle (seconds-to-time idle))))
              ((symbol-function 'float-time)
               (lambda (&optional time) (if time (funcall real-float-time time) clock))))
      (unwind-protect
          (let ((chore (lambda (n) (push n done) (cl-incf clock 0.015))))
            (dolist (n '(1 2 1 3)) (ghostherd-memory-page--chore chore n))
            (should (timerp ghostherd-memory-page--chore-timer))
            (should (= (length ghostherd-memory-page--chores) 3))
            ;; Typing, or a command running: nothing.
            (dolist (i (list nil 0.1 (- ghostherd-memory-page--chore-idle 0.01)))
              (setq idle i)
              (ghostherd-memory-page--chores-work))
            (should-not done)
            ;; A pause: as many as fit, the first always.
            (setq idle ghostherd-memory-page--chore-idle)
            (ghostherd-memory-page--chores-work)
            (should (equal done '(2 1)))
            (should (timerp ghostherd-memory-page--chore-timer))
            (ghostherd-memory-page--chores-work)
            (should (equal done '(3 2 1)))
            (should-not ghostherd-memory-page--chore-timer))
        (when (timerp ghostherd-memory-page--chore-timer)
          (cancel-timer ghostherd-memory-page--chore-timer))))))

(ert-deftest ghostherd-test-a-look-is-made-in-pauses ()
  "A look runs nothing at once; the page shows the last one until it is
done; another look meanwhile adds nothing; git is asked once a
directory, the agents and their project being in one."
  (ghostherd-test--with-herd
    (ghostherd-test--with-checkouts
      (clrhash ghostherd--sessions)
      (ghostherd-tests--session :name "rd" :kind 'claude :project repo :state 'idle)
      (ghostherd-tests--session :name "qa" :kind 'claude :project repo :state 'idle)
      (puthash "~/old/" '("~/old/" . "main") ghostherd-memory-page--checkouts)
      (let ((real (symbol-function 'ghostherd-memory-page--checkout))
            (gits nil))
        (cl-letf (((symbol-function 'ghostherd-session-directory) (lambda (_s) repo))
                  ((symbol-function 'ghostherd--project-name)
                   (lambda (p) (and p (abbreviate-file-name (file-name-as-directory (expand-file-name p))))))
                  ((symbol-function 'ghostherd-memory-page--checkout)
                   (lambda (dir) (push dir gits) (funcall real dir))))
          (ghostherd-memory-page--work-look)
          (let ((chores (copy-sequence ghostherd-memory-page--chores)))
            (ghostherd-memory-page--work-look)
            (should (equal ghostherd-memory-page--chores chores)))
          (should-not gits)
          (should-not asked)
          (should (equal (hash-table-keys ghostherd-memory-page--checkouts) '("~/old/")))
          (while ghostherd-memory-page--chores
            (ghostherd-memory-page--chore-next))
          (should (= (length gits) 1))
          (should-not (gethash "~/old/" ghostherd-memory-page--checkouts))
          (should (equal (gethash (abbreviate-file-name repo) ghostherd-memory-page--checkouts)
                         (cons (abbreviate-file-name repo) "feat/x")))
          (should (equal (mapcar #'car asked) '(pr))))))))

(ert-deftest ghostherd-test-an-agent-shows-what-its-conversation-names ()
  "The PRs its conversation names -- its project's own, marked so, and
another repository's, by that repository -- and its tasks, each looked
up; read again only when the transcript changed, looked up again when
stale -- a merged one much later.  Each opens in pr-view: its own by its
checkout, another's by its repository."
  (ghostherd-test--with-herd
    (ghostherd-test--with-checkouts
      (let ((path (concat base "rd.jsonl"))
            (root (abbreviate-file-name repo)))
        (with-temp-file path (insert "{}\n"))
        (setq transcripts `(("rd" . ,path))
              remote '(:kind bitbucket :owner "Team" :repo "Deploy")
              said `((,path :prs ((:host "bitbucket" :owner "team" :repo "deploy" :id 371 :count 2 :last "t3")
                                  (:host "github" :owner "team" :repo "deploy" :id 5 :count 1 :last "t2")
                                  (:host "bitbucket" :owner "other" :repo "deploy" :id 6 :count 1 :last "t2")
                                  (:host "bitbucket" :owner "team" :repo "deploy" :id 360 :count 1 :last "t1")
                                  ;; Not a pull request's names: not asked, not shown.
                                  (:host "gitlab" :owner "team" :repo "deploy" :id 7 :count 1 :last "t0")
                                  (:host "github" :owner "../x" :repo "deploy" :id 8 :count 1 :last "t0")
                                  (:host "github" :owner "team" :repo "deploy" :id "9" :count 1 :last "t0"))
                            :tasks ((:id "86abc1def" :count 1 :last "t3") (:id "86done001" :count 1 :last "t0"))))
              by-id '((371 (id . 371) (title . "Retry alerts") (state . "OPEN") (draft . t) (url . "b371"))
                      (360 (id . 360) (title . "Sentry fix") (state . "MERGED") (draft . nil) (url . "b360")))
              briefs '(("86abc1def" (id . "86abc1def") (name . "Sentry AI fix")
                        (status . ((name . "in progress") (color . "#4194f6") (type . "custom"))) (url . "cu1"))
                       ("86done001" (id . "86done001") (name . "Old one")
                        (status . ((name . "complete") (color . "#008844") (type . "closed"))) (url . "cu2"))))
        (clrhash ghostherd--sessions)
        (ghostherd-tests--session :name "rd" :kind 'claude :project repo :state 'idle)
        (ghostherd-tests--session :name "qa" :kind 'claude :project repo :state 'idle)
        (cl-letf (((symbol-function 'ghostherd-session-directory) (lambda (_s) repo))
                  ((symbol-function 'ghostherd--project-name)
                   (lambda (p) (and p (abbreviate-file-name (file-name-as-directory (expand-file-name p)))))))
          (ghostherd-test--look)
          (should (equal (seq-filter (lambda (a) (eq (car a) 'links)) asked) `((links "herd_links" ,path))))
          (should (equal (sort (mapcar #'cdr (seq-filter (lambda (a) (eq (car a) 'brief)) asked))
                               (lambda (x y) (< (cadr x) (cadr y))))
                         '(((:kind github :owner "team" :repo "deploy") 5)
                           ((:kind bitbucket :owner "other" :repo "deploy") 6)
                           ((:kind bitbucket :owner "team" :repo "deploy") 360)
                           ((:kind bitbucket :owner "team" :repo "deploy") 371))))
          (should (equal (sort (mapcar #'cadr (seq-filter (lambda (a) (eq (car a) 'task)) asked)) #'string<)
                         '("86abc1def" "86done001")))
          (ghostherd-memory-page--send-herd)
          (let* ((agents (append (plist-get (ghostherd-test--call calls "renderHerd") :agents) nil))
                 (of (lambda (n) (plist-get (seq-find (lambda (a) (equal (plist-get a :name) n)) agents) :links)))
                 (rd (funcall of "rd")))
            (should (eq (funcall of "qa") :json-false))
            (should (equal (plist-get rd :root) root))
            (should (equal (mapcar (lambda (pr) (list (plist-get pr :id) (plist-get pr :own) (plist-get pr :state)
                                                      (plist-get pr :draft) (plist-get pr :title) (plist-get pr :url)
                                                      (plist-get pr :last) (plist-get pr :error)))
                                   (append (plist-get rd :prs) nil))
                           '((371 t "OPEN" t "Retry alerts" "b371" "t3" nil)
                             (5 :json-false nil :json-false nil nil "t2" "no such pull request")
                             (6 :json-false nil :json-false nil nil "t2" "no such pull request")
                             (360 t "MERGED" :json-false "Sentry fix" "b360" "t1" nil))))
            (should (equal (mapcar (lambda (pr) (list (plist-get pr :host) (plist-get pr :owner) (plist-get pr :repo)))
                                   (append (plist-get rd :prs) nil))
                           '(("bitbucket" "team" "deploy") ("github" "team" "deploy")
                             ("bitbucket" "other" "deploy") ("bitbucket" "team" "deploy"))))
            (should (equal (mapcar (lambda (tk) (list (plist-get tk :id) (plist-get tk :done) (plist-get tk :last)))
                                   (append (plist-get rd :tasks) nil))
                           '(("86abc1def" :json-false "t3") ("86done001" t "t0")))))
          ;; The same transcript: not read again; its answers fresh: not asked.
          (setq asked nil)
          (ghostherd-test--look)
          (should-not (seq-filter (lambda (a) (memq (car a) '(links brief task))) asked))
          ;; Stale: an open PR is asked again, a merged one not yet.
          (maphash (lambda (k v) (puthash k (plist-put v :at (- (float-time) 600)) ghostherd-memory-page--pr-briefs))
                   ghostherd-memory-page--pr-briefs)
          (ghostherd-test--look)
          (should (equal (sort (mapcar #'caddr (seq-filter (lambda (a) (eq (car a) 'brief)) asked)) #'<) '(5 6 371)))
          ;; It grew: read again.
          (setq asked nil)
          (with-temp-buffer (insert "{}\n") (append-to-file (point-min) (point-max) path))
          (ghostherd-test--look)
          (should (equal (seq-filter (lambda (a) (eq (car a) 'links)) asked) `((links "herd_links" ,path))))
          ;; A PR it names opens in pr-view: its own by its checkout,
          ;; another's by its repository -- one it named, and no other.
          (let (opened)
            (cl-letf (((symbol-function 'pr-view-open-pr) (lambda (dir id) (push (list dir id) opened)))
                      ((symbol-function 'pr-view-open-remote-pr) (lambda (r id) (push (list r id) opened))))
              (clrhash ghostherd-memory-page--prs)
              (ghostherd-memory-page--handle `((op . "open-pr") (root . ,root) (id . 360)))
              (ghostherd-memory-page--handle '((op . "open-pr") (repo . "github/team/deploy") (id . 5)))
              (dolist (bad '(((op . "open-pr") (repo . "github/team/deploy") (id . 6))
                             ((op . "open-pr") (repo . "github/evil/x") (id . 5))
                             ((op . "open-pr") (repo . "github/team/deploy/x") (id . 5))
                             ((op . "open-pr") (root . "/etc/") (id . 360))))
                (ghostherd-memory-page--handle bad))
              (should (equal (reverse opened)
                             `((,root 360) ((:kind github :owner "team" :repo "deploy") 5)))))))))))

(ert-deftest ghostherd-test-a-hung-pr-look-is-asked-again-not-wedged ()
  "A pull-request look whose answer never comes -- the Mac slept mid-request
-- does not wedge the chip: while the look is fresh it is not asked twice,
but once it has hung past the bound it is taken as abandoned and asked
afresh, and an open one the forge now calls merged turns merged.  (The bug:
a `:pending' that never cleared blocked every later look, so #395 showed
Open for days after it had merged.)"
  (ghostherd-test--with-checkouts
    (let ((ref '(:host "bitbucket" :owner "unnotech" :repo "deploy-platform" :id 395 :last "t"))
          (key '("bitbucket" "unnotech" "deploy-platform" 395)))
      (setq by-id '((395 (id . 395) (title . "x") (state . "MERGED") (url . "u"))))
      ;; A look goes out and never answers: :pending is left set.
      (cl-letf (((symbol-function 'pr-view-repo-pr-brief)
                 (lambda (_r id _cb) (push (list 'brief id) asked))))
        (ghostherd-memory-page--pr-brief-ask ref)
        (should (equal (mapcar #'cadr asked) '(395)))
        (should (ghostherd-memory-page--pending-p (gethash key ghostherd-memory-page--pr-briefs)))
        ;; A fresh pending is left alone -- not asked twice.
        (setq asked nil)
        (ghostherd-memory-page--pr-brief-ask ref)
        (should-not asked))
      ;; Make the pending hang past the bound: now it is unsure, and no longer
      ;; counts as in flight.
      (let ((e (gethash key ghostherd-memory-page--pr-briefs)))
        (puthash key (plist-put e :pending (- (float-time) (1+ ghostherd-memory-page--pending-ttl)))
                 ghostherd-memory-page--pr-briefs))
      (should (ghostherd-memory-page--pr-unsure-p (gethash key ghostherd-memory-page--pr-briefs)))
      (should-not (ghostherd-memory-page--pending-p (gethash key ghostherd-memory-page--pr-briefs)))
      ;; Asked again; this time the forge answers (the harness stub): merged.
      (ghostherd-memory-page--pr-brief-ask ref)
      (let ((e (gethash key ghostherd-memory-page--pr-briefs)))
        (should (equal (alist-get 'state (plist-get e :pr)) "MERGED"))
        (should-not (ghostherd-memory-page--pr-unsure-p e))
        (should-not (ghostherd-memory-page--pending-p e))))))

(ert-deftest ghostherd-test-a-failed-pr-look-says-it-does-not-know ()
  "A look that fails neither blanks a state nor keeps asserting it: a pull
request once seen open, whose next look errors, keeps its state but is
marked unsure -- the chip says \"?\" rather than a stale Open; a merged one
is final and is not even re-asked; a number that was never a real pull
request stays hidden (no state, an error)."
  (ghostherd-test--with-checkouts
    (let ((path (concat base "rd.jsonl"))
          (k395 '("bitbucket" "unnotech" "deploy-platform" 395))
          (k360 '("bitbucket" "unnotech" "deploy-platform" 360))
          (k999 '("bitbucket" "unnotech" "deploy-platform" 999)))
      (with-temp-file path (insert "{}\n"))
      (setq transcripts `(("rd" . ,path))
            remote '(:kind bitbucket :owner "unnotech" :repo "deploy-platform")
            said `((,path :prs ((:host "bitbucket" :owner "unnotech" :repo "deploy-platform" :id 395 :count 1 :last "t3")
                                (:host "bitbucket" :owner "unnotech" :repo "deploy-platform" :id 360 :count 1 :last "t2")
                                (:host "bitbucket" :owner "unnotech" :repo "deploy-platform" :id 999 :count 1 :last "t1"))
                          :tasks nil)))
      (clrhash ghostherd--sessions)
      (ghostherd-tests--session :name "rd" :kind 'claude :project repo :state 'idle)
      (cl-letf (((symbol-function 'ghostherd-session-directory) (lambda (_s) repo))
                ((symbol-function 'ghostherd--project-name)
                 (lambda (p) (and p (abbreviate-file-name (file-name-as-directory (expand-file-name p)))))))
        ;; First look: 395 open, 360 merged, 999 never a real PR.
        (setq by-id '((395 (id . 395) (title . "Retry") (state . "OPEN") (url . "b395"))
                      (360 (id . 360) (title . "Sentry") (state . "MERGED") (url . "b360"))))
        (ghostherd-test--look)
        (should (equal (alist-get 'state (plist-get (gethash k395 ghostherd-memory-page--pr-briefs) :pr)) "OPEN"))
        ;; Age every brief so an open one is due to be re-checked.
        (maphash (lambda (k v) (puthash k (plist-put v :at (- (float-time) 400)) ghostherd-memory-page--pr-briefs))
                 ghostherd-memory-page--pr-briefs)
        ;; Now the forge is unreachable: every look errors.
        (setq asked nil)
        (cl-letf (((symbol-function 'pr-view-repo-pr-brief)
                   (lambda (_r id cb) (push (list 'brief id) asked)
                     (funcall cb '((error . "the forge refused the token"))))))
          (ghostherd-test--look))
        ;; The open one (and the never-real one) are re-asked; the merged one,
        ;; being final, is not.
        (should (equal (sort (mapcar #'cadr (seq-filter (lambda (a) (eq (car a) 'brief)) asked)) #'<) '(395 999)))
        ;; 395 keeps its known OPEN but is unsure; 360 stays merged, sure.
        (should (equal (alist-get 'state (plist-get (gethash k395 ghostherd-memory-page--pr-briefs) :pr)) "OPEN"))
        (should (ghostherd-memory-page--pr-unsure-p (gethash k395 ghostherd-memory-page--pr-briefs)))
        (should (equal (alist-get 'state (plist-get (gethash k360 ghostherd-memory-page--pr-briefs) :pr)) "MERGED"))
        (should-not (ghostherd-memory-page--pr-unsure-p (gethash k360 ghostherd-memory-page--pr-briefs)))
        (should-not (plist-get (gethash k999 ghostherd-memory-page--pr-briefs) :pr))
        ;; As the page is told: 395 shows unsure, its id, its last state but
        ;; not asserted; 360 sure; 999 has no state and an error, so the page
        ;; (ui/app.js) leaves it out.
        (let* ((prs (append (plist-get (ghostherd-memory-page--agent-links (ghostherd-get "rd")) :prs) nil))
               (p (lambda (id) (seq-find (lambda (x) (equal (plist-get x :id) id)) prs))))
          (should (eq (plist-get (funcall p 395) :unsure) t))
          (should (equal (plist-get (funcall p 395) :state) "OPEN"))
          (should (equal (plist-get (funcall p 395) :error) "the forge refused the token"))
          (should (eq (plist-get (funcall p 360) :unsure) :json-false))
          (should (null (plist-get (funcall p 999) :state)))
          (should (plist-get (funcall p 999) :error)))))))

(ert-deftest ghostherd-test-the-page-counts-what-a-checkout-has-not-committed ()
  "Each checkout the herd is in is counted once a look, an agent's
worktree too; the work line says it -- nothing when it is clean -- and
the page is told only when it changed.  An agent that ends a turn has
its checkouts counted again, while the page is open."
  (ghostherd-test--with-herd
    (ghostherd-test--with-checkouts
      (let ((root (abbreviate-file-name repo))
            (tree (abbreviate-file-name wt)))
        (clrhash ghostherd--sessions)
        (ghostherd-tests--session :name "rd" :kind 'claude :project repo :state 'working)
        (ghostherd-tests--session :name "qa" :kind 'claude :project repo :state 'idle)
        (setq stats `((,root :root ,root :files 3 :added 12 :deleted 4 :untracked 1 :untracked-added 5)
                      (,tree :root ,tree :files 0 :added 0 :deleted 0 :untracked 0 :untracked-added 0)))
        (cl-letf (((symbol-function 'ghostherd-session-directory)
                   (lambda (s) (if (equal (ghostherd-session-name s) "qa") wt repo)))
                  ((symbol-function 'ghostherd--project-name)
                   (lambda (p) (and p (abbreviate-file-name (file-name-as-directory (expand-file-name p)))))))
          (ghostherd-test--look)
          (should (equal (sort (copy-sequence counted) #'string<) (sort (list root tree) #'string<)))
          (ghostherd-memory-page--send-herd)
          (let* ((herd (ghostherd-test--call calls "renderHerd"))
                 (work (seq-find (lambda (w) (equal (plist-get w :project) root))
                                 (append (plist-get herd :work) nil)))
                 (qa (seq-find (lambda (a) (equal (plist-get a :name) "qa"))
                               (append (plist-get herd :agents) nil))))
            (should (equal (plist-get work :changes)
                           '(:files 3 :added 12 :deleted 4 :untracked 1 :untrackedAdded 5)))
            ;; Clean: nothing to say.
            (should (equal (plist-get (plist-get qa :work) :root) tree))
            (should (eq (plist-get (plist-get qa :work) :changes) :json-false)))
          ;; Counted the same: the page is not told again; changed, it is.
          (let ((told 0))
            (cl-letf (((symbol-function 'ghostherd-memory-page--work-changed) (lambda () (cl-incf told))))
              (ghostherd-memory-page--changes-count root)
              (should (= told 0))
              (setf (alist-get root stats nil nil #'equal) (list :root root :files 4 :added 13 :deleted 4))
              (ghostherd-memory-page--changes-count root)
              (should (= told 1))))
          ;; rd ends a turn: its checkout is counted again -- with the page open.
          (setq counted nil ghostherd-memory-page--chores nil)
          (cl-letf (((symbol-function 'xwapp-live-p) (lambda (_app) nil)))
            (ghostherd--set-state (ghostherd-get "rd") 'idle))
          (should-not ghostherd-memory-page--chores)
          (setf (ghostherd-session-state (ghostherd-get "rd")) 'working)
          (cl-letf (((symbol-function 'xwapp-live-p) (lambda (_app) t)))
            (ghostherd--set-state (ghostherd-get "rd") 'idle)
            (ghostherd--set-state (ghostherd-get "qa") 'done))
          (while ghostherd-memory-page--chores (ghostherd-memory-page--chore-next))
          (should (equal counted (list root)))
          ;; So does one that ends it to wait on its sub-agents.
          (setq counted nil)
          (setf (ghostherd-session-state (ghostherd-get "rd")) 'working)
          (cl-letf (((symbol-function 'xwapp-live-p) (lambda (_app) t)))
            (ghostherd--set-state (ghostherd-get "rd") 'waiting))
          (while ghostherd-memory-page--chores (ghostherd-memory-page--chore-next))
          (should (equal counted (list root))))))))

(ert-deftest ghostherd-test-the-page-opens-a-checkouts-changes ()
  "What a checkout the herd is in has not committed opens in pr-view, and
pr-view's page may show it; another directory is neither."
  (ghostherd-test--with-herd
    (let ((ghostherd-memory-page--checkouts (make-hash-table :test 'equal)) opened)
      (puthash "~/p/" '("~/p/" . "feat/x") ghostherd-memory-page--checkouts)
      (cl-letf (((symbol-function 'pr-view-changes) (lambda (dir) (push dir opened)))
                ((symbol-function 'ghostherd-memory-page--checkout) (lambda (_dir) nil)))
        (ghostherd-memory-page--handle '((op . "open-changes") (root . "~/p/")))
        (should (equal opened '("~/p/")))
        (should (memq #'ghostherd-memory-page--herd-checkout-p pr-view-local-allowed-functions))
        (should (ghostherd-memory-page--herd-checkout-p "~/p/"))
        (setq calls nil)
        (dolist (bad '("/etc/" "~/p" 7 nil))
          (should-not (ghostherd-memory-page--herd-checkout-p bad))
          (ghostherd-memory-page--handle `((op . "open-changes") (root . ,bad))))
        (should (equal opened '("~/p/")))
        (should (= (length (seq-filter (lambda (c) (equal (car c) "showError")) calls)) 4)))
      ;; Before a look -- Emacs just restarted -- an agent's own checkout.
      (clrhash ghostherd-memory-page--checkouts)
      (cl-letf (((symbol-function 'ghostherd-memory-page--checkout)
                 (lambda (dir) (and (equal dir "/p/") '("~/q/" . "main")))))
        (should (ghostherd-memory-page--herd-checkout-p "~/q/"))
        (should-not (ghostherd-memory-page--herd-checkout-p "~/r/"))))))

(ert-deftest ghostherd-test-a-review-goes-to-an-agent-of-its-checkout ()
  "A review of what a checkout has not committed may go to its project's
lead first, then each agent working in it -- not one in another
worktree, nor a shell, nor a dead agent -- and to a lead started for it
when the project has none, last.  Sent, it is said to the one picked.
A checkout the herd is not in has nobody."
  (ghostherd-test--with-herd
    (ghostherd-test--with-checkouts
      (let* ((root (abbreviate-file-name repo))
             (tree (abbreviate-file-name wt))
             (ghostherd--roles (make-hash-table :test 'equal))
             (dirs `(("rd" . ,repo) ("qa" . ,wt) ("sh" . ,repo) ("old" . ,repo) ("lead-repo" . ,repo)))
             (to (lambda (dir) (mapcar (lambda (r) (list (plist-get r :id) (plist-get r :label)))
                                       (ghostherd-memory-page--review-recipients dir))))
             (send (lambda (dir id text)
                     (funcall (plist-get (seq-find (lambda (r) (equal (plist-get r :id) id))
                                                   (ghostherd-memory-page--review-recipients dir))
                                         :send)
                              text)))
             told started)
        (clrhash ghostherd--sessions)
        (ghostherd-tests--session :name "rd" :kind 'claude :project repo :state 'working)
        (ghostherd-tests--session :name "qa" :kind 'grok :project repo :state 'idle)
        (ghostherd-tests--session :name "sh" :kind 'shell :project repo :state 'idle)
        (ghostherd-tests--session :name "old" :kind 'agy :project repo :state 'dead)
        (cl-letf (((symbol-function 'ghostherd-session-directory)
                   (lambda (s) (cdr (assoc (ghostherd-session-name s) dirs))))
                  ((symbol-function 'ghostherd-tell)
                   (lambda (s text &rest _) (push (list (ghostherd-session-name s) text) told)))
                  ((symbol-function 'ghostherd-lead-say)
                   (lambda (project text)
                     (push (list project text) started)
                     (let ((lead (ghostherd-tests--session :name "lead-repo" :kind 'claude :project repo
                                                           :state 'idle)))
                       (puthash "lead-repo" "lead" ghostherd--roles)
                       lead))))
          (should (memq #'ghostherd-memory-page--review-recipients pr-view-local-recipients-functions))
          ;; No lead yet: whoever works there, then a lead to be started.
          (should (equal (funcall to root) `(("rd" "rd · claude") (,(concat "@lead " root) "new lead · claude"))))
          (should (equal (funcall to tree) `(("qa" "qa · grok") (,(concat "@lead " root) "new lead · claude"))))
          (should (equal (funcall send root "rd" "Comments…") "Sent to rd"))
          (should (equal told '(("rd" "Comments…"))))
          (should (equal (funcall send tree (concat "@lead " root) "More…")
                         "Started lead-repo; it gets the comments once it is ready"))
          (should (equal started `((,root "More…"))))
          ;; With a lead: the lead first, in its worktrees too.
          (should (equal (funcall to root) '(("lead-repo" "lead-repo · claude") ("rd" "rd · claude"))))
          (should (equal (funcall to tree) '(("lead-repo" "lead-repo · claude") ("qa" "qa · grok"))))
          (funcall send tree "lead-repo" "Again…")
          (should (equal (car told) '("lead-repo" "Again…")))
          ;; Nobody for a checkout the herd is not in.
          (should-not (ghostherd-memory-page--review-recipients "/etc/"))
          (should-not (ghostherd-memory-page--review-recipients (abbreviate-file-name base))))))))

(ert-deftest ghostherd-test-a-pr-review-reaches-the-head-branchs-agent ()
  "PR comments go to the agent on the PR's head branch first, then the
project's lead, then a lead to start -- the agent on the branch first, as
the user asked.  With no agent on the branch, the repo's project is found
by its remote so the lead is still offered.  A branch no agent is on and a
repo the herd does not have has nobody."
  (ghostherd-test--with-herd
    (ghostherd-test--with-checkouts
      (let* ((ghostherd--roles (make-hash-table :test 'equal))
             (ghostherd--worktrees (make-hash-table :test 'equal))
             (ghostherd-memory-page--project-remotes (make-hash-table :test 'equal))
             (ghostherd-memory-page--checkouts (make-hash-table :test 'equal))
             (pname (ghostherd--project-name repo))
             (to (lambda (branch rp)
                   (mapcar (lambda (r) (list (plist-get r :id) (plist-get r :label)))
                           (ghostherd-memory-page--review-recipients nil (list :branch branch :repo rp))))))
        (clrhash ghostherd--sessions)
        (ghostherd-tests--session :name "rd" :kind 'claude :project repo :state 'working)
        (ghostherd-tests--session :name "qa" :kind 'grok :project repo :state 'idle)
        (ghostherd-tests--session :name "sh" :kind 'shell :project repo :state 'idle)
        (puthash "qa" '(:path nil :branch "feat/y" :base "feat/x") ghostherd--worktrees)
        ;; A non-checkout agent's cached checkout is the symbol `none', not a
        ;; cons -- reading a branch off it must not choke.
        (puthash (cons 'agent "rd") 'none ghostherd-memory-page--checkouts)
        (funcall git repo "remote" "add" "origin" "https://example.com/own/thing.git")
        (cl-letf (((symbol-function 'ghostherd-session-directory)
                   (lambda (s) (if (equal (ghostherd-session-name s) "qa") wt repo)))
                  ((symbol-function 'ghostherd-lead-say)
                   (lambda (_project _text)
                     (let ((lead (ghostherd-tests--session :name "lead-repo" :kind 'claude
                                                           :project repo :state 'idle)))
                       (puthash "lead-repo" "lead" ghostherd--roles) lead))))
          ;; No lead yet: the agent on feat/y first, then a lead to start.
          ;; rd, on feat/x in the main checkout, is not offered for a feat/y PR.
          (should (equal (funcall to "feat/y" "own/thing")
                         `(("qa" "qa · grok") (,(concat "@lead " pname) "new lead · claude"))))
          ;; With a lead: the branch's agent still first, then the lead.
          (ghostherd-tests--session :name "lead-repo" :kind 'claude :project repo :state 'idle)
          (puthash "lead-repo" "lead" ghostherd--roles)
          (should (equal (funcall to "feat/y" "own/thing")
                         '(("qa" "qa · grok") ("lead-repo" "lead-repo · claude"))))
          ;; No agent on the branch, but the repo maps to the project: the lead.
          (should (equal (funcall to "no-such-branch" "own/thing")
                         '(("lead-repo" "lead-repo · claude"))))
          ;; A repo the herd does not have, no agent on the branch: nobody.
          (should-not (funcall to "no-such-branch" "other/elsewhere"))
          ;; A repo the herd does not have is nobody even if a branch matches
          ;; -- do not send to a same-named branch's agent in another repo.
          (should-not (funcall to "feat/y" "other/elsewhere")))))))

(ert-deftest ghostherd-test-the-editor-map-marks-each-file-with-its-agent ()
  "Which agent edited which file, for a diff's file tree: of a checkout, the
agents working in it, by the files they edited there; of a PR, only the
agents on its head branch.  Each agent's paths are relative to its own
checkout, so a worktree's edits never mark the main checkout's tree, and a
file edited outside the checkout is dropped.  Nothing cached for an agent
\(a shell, one not read yet) means nothing marked."
  (ghostherd-test--with-herd
    (ghostherd-test--with-checkouts
      (let* ((root (abbreviate-file-name repo))
             (tree (abbreviate-file-name wt))
             (ghostherd--roles (make-hash-table :test 'equal))
             (ghostherd--worktrees (make-hash-table :test 'equal))
             (ghostherd-memory-page--edits (make-hash-table :test 'equal))
             ;; The map as (FILE . (AGENT-ID ...)), per file in order.
             (edited (lambda (where &optional pr)
                       (mapcar (lambda (e)
                                 (cons (plist-get e :file)
                                       (mapcar (lambda (b) (plist-get b :id)) (plist-get e :by))))
                               (ghostherd-memory-page--editors where pr)))))
        (clrhash ghostherd--sessions)
        (ghostherd-tests--session :name "rd" :kind 'claude :project repo :state 'working)
        (ghostherd-tests--session :name "qa" :kind 'grok :project repo :state 'idle)
        (ghostherd-tests--session :name "sh" :kind 'shell :project repo :state 'idle)
        (puthash "qa" (list :path tree :branch "feat/y" :base "feat/x") ghostherd--worktrees)
        ;; What each edited, as the sidecar read it: absolute paths.  rd, in
        ;; the main checkout, also touched a file in the worktree and one
        ;; outside any checkout -- neither may mark its tree.
        (puthash "rd" (cons "s" (list :files (list (concat repo "src/pay.py")
                                                   (concat repo "README.md")
                                                   (concat wt "sneaky.py")
                                                   "/etc/passwd")))
                 ghostherd-memory-page--edits)
        (puthash "qa" (cons "s" (list :files (list (concat wt "lib/a.py"))))
                 ghostherd-memory-page--edits)
        (cl-letf (((symbol-function 'ghostherd-session-directory)
                   (lambda (s) (if (equal (ghostherd-session-name s) "qa") wt repo)))
                  ;; The map is built from the cache set above; warming (which
                  ;; --editors also does) is its own test, not this one.
                  ((symbol-function 'ghostherd-memory-page--edits-look) #'ignore))
          (should (memq #'ghostherd-memory-page--editors pr-view-local-editors-functions))
          ;; The main checkout: rd's files under it, relative, in order; the
          ;; worktree file and the outside one dropped; qa (in the worktree)
          ;; does not mark the main tree; sh (a shell, nothing cached) marks
          ;; nothing.
          (should (equal (funcall edited root)
                         '(("src/pay.py" "rd") ("README.md" "rd"))))
          ;; The worktree, as its own checkout: qa's file; rd is not in it.
          (should (equal (funcall edited tree) '(("lib/a.py" "qa"))))
          ;; The PR on feat/y: only qa, on that branch, its file relative to
          ;; its worktree; rd, on feat/x, is not marked.
          (should (equal (funcall edited nil (list :branch "feat/y" :repo nil))
                         '(("lib/a.py" "qa"))))
          ;; A branch no agent is on, and a checkout the herd is not in: none.
          (should-not (funcall edited nil (list :branch "no-such" :repo nil)))
          (should-not (funcall edited "/etc/")))))))

(ert-deftest ghostherd-test-asking-for-the-editor-map-warms-the-cache ()
  "Asking for the editor map warms the edits cache (so a diff opened before
the cache was warm fills in), and a fresh edits answer pushes the marks to
pr-view -- no manual refresh."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let (looked refreshed sent)
    (cl-letf (((symbol-function 'ghostherd-memory-page--edits-look) (lambda () (setq looked t)))
              ((symbol-function 'ghostherd-memory-page--agents-in-checkout) (lambda (_r) nil))
              ((symbol-function 'ghostherd-memory-page--send-herd-soon) (lambda () (setq sent t)))
              ((symbol-function 'pr-view-refresh-editors) (lambda () (setq refreshed t))))
      ;; Asking warms the cache before it reads it.
      (should-not (ghostherd-memory-page--editors "/some/root/"))
      (should looked)
      ;; A fresh answer refreshes the page and pushes the marks to pr-view.
      (ghostherd-memory-page--edits-changed)
      (should (and sent refreshed)))))

(ert-deftest ghostherd-test-a-review-from-pr-views-page-reaches-the-agent ()
  "Comments written on a herd checkout's diff in pr-view's page reach the
agent picked as one prompt, each at its file and line."
  (skip-unless (require 'pr-view nil t))
  (ghostherd-test--with-herd
    (ghostherd-test--with-checkouts
      (let ((root (abbreviate-file-name repo)) told js)
        (clrhash ghostherd--sessions)
        (ghostherd-tests--session :name "rd" :kind 'claude :project repo :state 'idle)
        (cl-letf (((symbol-function 'ghostherd-session-directory) (lambda (_s) repo))
                  ((symbol-function 'ghostherd-tell)
                   (lambda (s text &rest _) (push (list (ghostherd-session-name s) text) told)))
                  ((symbol-function 'pr-view--js) (lambda (fn arg) (push (cons fn arg) js))))
          (pr-view--handle-intent
           `((op . "local-review") (root . ,root) (to . "rd")
             (comments . (((path . "src/pay.py") (side . "new") (line . 3) (code . "return charge(total)")
                           (text . "Retry it."))))))
          (should (equal (caar js) "reviewSent"))
          (should (equal (alist-get 'said (cdar js)) "Sent to rd"))
          (should (equal (mapcar #'car told) '("rd")))
          (should (equal (cadr (car told))
                         (concat "Comments on what is not committed in " root " (branch feat/x), "
                                 "each on a line of its diff against HEAD:\n"
                                 "\n1. src/pay.py:3\n   > return charge(total)\n   Retry it.\n"))))))))

(ert-deftest ghostherd-test-sidebar-shows-a-checkouts-changes ()
  "d on a row: the status of the checkout its agent works in -- a
worktree, say -- in magit, or vc-dir without it."
  (ghostherd-tests--with-herd ()
    (let* ((dir (file-name-as-directory (make-temp-file "gh-changes" t)))
           (wt (concat dir "wt/"))
           (s (ghostherd-tests--session :name "rd" :kind 'claude :project dir))
           shown)
      (make-directory wt)
      (unwind-protect
          (cl-letf (((symbol-function 'ghostherd--sidebar-session-at-point) (lambda () s))
                    ((symbol-function 'ghostherd--sidebar-leave-overlay) #'ignore)
                    ((symbol-function 'ghostherd-session-directory) (lambda (_s) wt))
                    ((symbol-function 'magit-status-setup-buffer) (lambda (d) (push (list 'magit d) shown)))
                    ((symbol-function 'vc-dir) (lambda (d) (push (list 'vc d) shown))))
            (should (eq (keymap-lookup ghostherd-sidebar-mode-map "d") #'ghostherd-sidebar-changes))
            (cl-letf (((symbol-function 'require) (lambda (f &rest _) (eq f 'magit))))
              (ghostherd-sidebar-changes))
            (cl-letf (((symbol-function 'require) (lambda (&rest _) nil)))
              (ghostherd-sidebar-changes))
            (should (equal (reverse shown) `((magit ,wt) (vc ,wt))))
            (delete-directory wt)
            (should-error (ghostherd-sidebar-changes) :type 'user-error))
        (delete-directory dir t)))))

(ert-deftest ghostherd-test-the-page-opens-a-pr-or-a-task ()
  "A PR of a checkout the herd is in opens in pr-view, a task in
clickup-view; anything else the page names is refused."
  (ghostherd-test--with-herd
    (let ((ghostherd-memory-page--prs (make-hash-table :test 'equal)) opened)
      (puthash '("~/p/" . "feat/x") '(:at 0 :pr nil) ghostherd-memory-page--prs)
      (cl-letf (((symbol-function 'pr-view-open-pr) (lambda (dir id) (push (list 'pr dir id) opened)))
                ((symbol-function 'clickup-view-task) (lambda (id) (push (list 'task id) opened))))
        (ghostherd-memory-page--handle '((op . "open-pr") (root . "~/p/") (id . 87)))
        (ghostherd-memory-page--handle '((op . "open-task") (id . "86abc1def")))
        (should (equal (reverse opened) '((pr "~/p/" 87) (task "86abc1def"))))
        (setq calls nil)
        (dolist (bad '(((op . "open-pr") (root . "/etc/") (id . 1))
                       ((op . "open-pr") (root . "~/p/") (id . "87; rm"))
                       ((op . "open-task") (id . "../x"))))
          (ghostherd-memory-page--handle bad))
        (should (= (length opened) 2))
        (should (= (length (seq-filter (lambda (c) (equal (car c) "showError")) calls)) 3))))))

(ert-deftest ghostherd-test-page-pastes-an-image ()
  "The page asks for the clipboard's image for an agent; Emacs saves it
and attaches it, file and preview -- or says why not."
  (ghostherd-test--with-herd
    (ghostherd-test--with-clipboard ghostherd-test--png-sh
      (cl-letf (((symbol-function 'ghostherd-memory-page--thumb) (lambda (f) (concat "thumb of " f))))
        (ghostherd-memory-page--handle '((op . "agent-image") (name . "claude-api"))))
      (let ((p (ghostherd-test--call calls "attachImage")))
        (should (equal (plist-get p :name) "claude-api"))
        (should (file-exists-p (plist-get p :path)))
        (should (equal (plist-get p :thumb) (concat "thumb of " (plist-get p :path))))
        (should (equal (file-name-directory (plist-get p :path))
                       (expand-file-name ghostherd-image-directory)))))
    (setq calls nil)
    (ghostherd-test--with-clipboard "printf text"
      (ghostherd-memory-page--handle '((op . "agent-image") (name . "claude-api"))))
    (should-not (ghostherd-test--call calls "attachImage"))
    (should (equal (ghostherd-test--call calls "showError") "No image on the clipboard"))
    (ghostherd-memory-page--handle '((op . "agent-image") (name . "nobody")))
    (should (string-match-p "No agent named nobody" (ghostherd-test--call calls "showError")))))

(defconst ghostherd-test--jpeg "\xff\xd8\xff\xe0\0\x10JFIF\0\303\251\377 \n rest"
  "A JPEG's signature, then bytes a text coding would change.")

(ert-deftest ghostherd-test-page-uploads-an-image ()
  "From the page on another device the image comes in the intent, base64:
saved byte for byte as a .jpg or .png named for the agent, readable only
by you, and attached -- or refused, saying why, nothing saved."
  (ghostherd-test--with-herd
    (ghostherd-test--with-clipboard "exit 1"
      (cl-letf (((symbol-function 'ghostherd-memory-page--thumb) (lambda (f) (concat "thumb of " f))))
        (pcase-dolist (`(,bytes . ,ext) `((,ghostherd-test--jpeg . "jpg") (,ghostherd-test--png . "png")))
          (setq calls nil)
          (ghostherd-memory-page--handle `((op . "agent-image-upload") (name . "claude-api")
                                           (data . ,(base64-encode-string bytes t))))
          (let* ((p (ghostherd-test--call calls "attachImage"))
                 (file (plist-get p :path)))
            (should (equal (plist-get p :name) "claude-api"))
            (should (string-match-p (format "\\`claude-api-[0-9-]+\\.%s\\'" ext) (file-name-nondirectory file)))
            (should (equal (file-name-directory file) (expand-file-name ghostherd-image-directory)))
            (should (equal (with-temp-buffer
                             (set-buffer-multibyte nil)
                             (insert-file-contents-literally file)
                             (buffer-string))
                           bytes))
            (should (= (file-modes file) #o600))
            (should (equal (plist-get p :thumb) (concat "thumb of " file)))))
        ;; For a lead not started yet, as the clipboard's.
        (setq calls nil)
        (ghostherd-memory-page--handle `((op . "agent-image-upload") (name . "@lead")
                                         (data . ,(base64-encode-string ghostherd-test--jpeg t))))
        (should (equal (plist-get (ghostherd-test--call calls "attachImage") :name) "@lead"))
        (let ((saved (directory-files ghostherd-image-directory)))
          (pcase-dolist (`(,intent . ,why)
                         `((((name . "claude-api") (data . ,(base64-encode-string "just text" t))) . "Not a PNG or JPEG image")
                           (((name . "claude-api")) . "No image came")
                           (((name . "claude-api") (data . "%%% not base64")) . "No image came")
                           (((name . "nobody") (data . ,(base64-encode-string ghostherd-test--jpeg t))) . "No agent named nobody")))
            (setq calls nil)
            (ghostherd-memory-page--handle (cons '(op . "agent-image-upload") intent))
            (should-not (ghostherd-test--call calls "attachImage"))
            (should (string-match-p why (ghostherd-test--call calls "showError"))))
          (should (equal (directory-files ghostherd-image-directory) saved)))))))

(defconst ghostherd-test--pixel
  (base64-decode-string
   "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII=")
  "A real PNG: one pixel.")

(ert-deftest ghostherd-test-an-attached-image-has-a-preview ()
  "A preview is a PNG data URL: scaled by sips, or without it the file
itself when small.  Nothing that is not a PNG becomes one."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let* ((dir (make-temp-file "gh-thumb" t))
         (png (expand-file-name "a.png" dir))
         (junk (expand-file-name "b.png" dir))
         (big (expand-file-name "c.png" dir))
         (decode (lambda (url) (and url (string-prefix-p "data:image/png;base64," url)
                                    (base64-decode-string (substring url 22))))))
    (unwind-protect
        (let ((coding-system-for-write 'binary))
          (write-region ghostherd-test--pixel nil png nil 'silent)
          (write-region "not an image" nil junk nil 'silent)
          (write-region (concat ghostherd-test--pixel (make-string (* 300 1024) ?x)) nil big nil 'silent)
          (when (executable-find "sips")
            (should (string-prefix-p "\x89PNG" (funcall decode (ghostherd-memory-page--thumb png))))
            (should-not (ghostherd-memory-page--thumb junk)))
          (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
            (should (equal (funcall decode (ghostherd-memory-page--thumb png)) ghostherd-test--pixel))
            (should-not (ghostherd-memory-page--thumb junk))
            (should-not (ghostherd-memory-page--thumb big))))
      (delete-directory dir t))))

(ert-deftest ghostherd-test-cmd-v-is-the-kits ()
  "Cmd-V in the herd page is the page kit's, as in every xwapp page: the
kit asks xwapp for the clipboard (xwapp-tests.el).  The page's own
paste, and the s-v keys that never fired under Cmd as Meta, are gone; a
paste intent from an old page does nothing, the clipboard unread.  The
page opens as before."
  (should-not (fboundp 'ghostherd-memory-page-paste))
  (should-not (boundp 'ghostherd-memory-page-keys-mode-map))
  (ghostherd-test--with-page
    (let ((read nil))
      (cl-letf (((symbol-function 'gui-get-selection) (lambda (&rest _) (setq read t) "some text")))
        (ghostherd-memory-page--handle '((op . "paste"))))
      (should-not read)
      (should-not calls)))
  (let ((ghostherd-memory-page--project nil)
        (ghostherd-memory-page--pending nil)
        (opened nil))
    (cl-letf (((symbol-function 'ghostherd-memory-ensure) #'ignore)
              ((symbol-function 'xwapp-open) (lambda (app) (setq opened app))))
      (ghostherd-memory-page--open "agents")
      (should (eq opened ghostherd-memory-page--app))
      (should (equal (plist-get ghostherd-memory-page--pending :view) "agents")))))

(ert-deftest ghostherd-test-a-browser-tab-reopens-the-page ()
  "A tab that comes back to the herd page -- after a restart, say -- finds
it made ready as the command would, but for what to show: the tab knows
that.  A sidecar that does not start is only said."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (should (eq (xwapp-reopen ghostherd-memory-page--app) 'ghostherd-memory-page-reopen))
  (let ((ghostherd-memory-page--project nil)
        (ghostherd-memory-page--pending '(:view "search")))
    (cl-letf (((symbol-function 'ghostherd-memory-ensure) (lambda () (error "No uv")))
              ((symbol-function 'ghostherd-memory-page--current-project) (lambda () "/p")))
      (ghostherd-memory-page-reopen))
    (should-not ghostherd-memory-page--pending)
    (should (equal ghostherd-memory-page--project "/p"))))

(ert-deftest ghostherd-test-when-ready-says-when-it-will-not-be ()
  (ghostherd-tests--with-herd ((ghostherd--log nil))
    (let ((s (ghostherd-tests--session :name "rd" :kind 'grok :state 'starting))
          said)
      (ghostherd--when-ready s (lambda () (push 'told said)) 0 (lambda () (push 'never said)))
      (should (equal said '(never)))
      (remhash "rd" ghostherd--sessions)
      (ghostherd--when-ready s (lambda () (push 'told said)) 5 (lambda () (push 'gone said)))
      (should (equal said '(gone never)))
      (puthash "rd" s ghostherd--sessions)
      (setf (ghostherd-session-state s) 'idle)
      (ghostherd--when-ready s (lambda () (push 'told said)) 5 (lambda () (push 'never said)))
      (should (equal said '(told gone never))))))

(ert-deftest ghostherd-test-relay-from-telegram ()
  (ghostherd-tests--relaying
    (ghostherd-tests--session :name "rd" :kind 'claude :state 'idle :backend 'fake)
    (should (equal (ghostherd--herd-run-command '(:id "c1" :op "relay" :session "rd" :args (:kind "grok")))
                   '(:id "c1" :text "relaying rd to grok")))
    (should (eq (ghostherd-session-kind (ghostherd-get "rd")) 'grok))
    (should (equal (plist-get (ghostherd--herd-run-command '(:id "c2" :op "relay" :session "rd" :args (:kind "rm -rf")))
                              :text)
                   "Relay to what: rm -rf"))))

(ert-deftest ghostherd-test-relay-on-the-page ()
  "The page shows an heir's place in its line and where its work would
go; its Relay names the CLI, which must be one an agent runs on."
  (ghostherd-tests--relaying
    (skip-unless (require 'ghostherd-memory-page nil t))
    (let ((rd (ghostherd-tests--session :name "rd" :kind 'grok :state 'idle :backend 'fake))
          relayed)
      (puthash "rd" '(((kind . "claude") (conversation . "") (ended . "x") (reason . ""))) ghostherd--lineage)
      (ghostherd-tests--used-up 'grok)
      (let ((a (ghostherd-memory-page--agent rd)))
        (should (equal (plist-get a :generation) 2))
        (should (equal (plist-get a :line) ["claude"]))
        (should (equal (plist-get a :relayTo) "claude"))
        (should (string-match-p "its grok usage" (plist-get a :usedUp))))
      (cl-letf (((symbol-function 'ghostherd-relay) (lambda (s kind &rest _) (setq relayed (list s kind))))
                ((symbol-function 'ghostherd-memory-page--js) #'ignore)
                ((symbol-function 'ghostherd-memory-page--send-herd) #'ignore))
        (ghostherd-memory-page--agent-act "agent-relay" '((name . "rd") (kind . "agy")))
        (should (equal relayed (list rd 'agy)))
        (should-error (ghostherd-memory-page--agent-act "agent-relay" '((name . "rd") (kind . "shell")))
                      :type 'user-error)))))

(ert-deftest ghostherd-test-page-meters-each-agent-once-per-change ()
  "Each agent's meter, read by the sidecar from its transcript when that
changed -- not on every look -- and sent field by field.  None for an
agent whose transcript is not known, or that has not replied yet."
  (ghostherd-test--with-herd
    (let* ((file (make-temp-file "gh-meter" nil ".jsonl" "{}\n"))
           (ghostherd-memory-page--meters (make-hash-table :test 'equal))
           (ghostherd-memory-page--metering (make-hash-table :test 'equal))
           (ghostherd-memory-page--herd-soon nil)
           (paths `(("claude-api" . ,file))))
      (puthash "agy-x" (ghostherd-session--create :id "agy-x" :name "agy-x" :kind 'agy :state 'idle
                                                  :project "/p/" :backend 'tmux)
               ghostherd--sessions)
      (cl-letf (((symbol-function 'ghostherd-session-transcript)
                 (lambda (s) (cdr (assoc (ghostherd-session-name s) paths)))))
        (unwind-protect
            (let ((meter (lambda (name) (plist-get (ghostherd-memory-page--agent (ghostherd-get name)) :meter))))
              (setq reply (lambda (_m _p) '(:model "Opus 5.5" :effort "xhigh" :tokens 833282 :window 1000000
                                            :pending :json-false :compacted :json-false :ts "x" :secret "y")))
              (ghostherd-memory-page--meter-look)
              (ghostherd-memory-page--meter-look)
              (should (equal requests `(("herd_meter" (:path ,file)))))
              (should (equal (funcall meter "claude-api")
                             '(:model "Opus 5.5" :effort "xhigh" :tokens 833282 :window 1000000
                               :pending :json-false :compacted :json-false)))
              (should (eq (funcall meter "agy-x") :json-false))
              ;; Changed: asked again.  Compacted since its last reply: no size.
              (with-temp-file file (insert "{}\n{}\n"))
              (setq reply (lambda (_m _p) '(:model "Opus 5.5" :effort "xhigh" :tokens nil :window 1000000
                                            :pending :json-false :compacted t)))
              (ghostherd-memory-page--meter-look)
              (should (= (length requests) 2))
              (should (equal (plist-get (funcall meter "claude-api") :tokens) nil))
              (should (eq (plist-get (funcall meter "claude-api") :compacted) t))
              ;; A new conversation, not replied to yet: no meter.
              (with-temp-file file (insert "{}\n"))
              (setq reply (lambda (_m _p) nil))
              (ghostherd-memory-page--meter-look)
              (should (eq (funcall meter "claude-api") :json-false)))
          (delete-file file))))))

(ert-deftest ghostherd-test-page-asks-for-the-tally-once-an-interval ()
  "What the agents' replies cost is tallied by the sidecar as their
transcripts grow, so the page asks for it once every
`ghostherd-memory-page--tally-every' seconds, not on every look, and
sends it field by field: each agent's today and week beside its
context, each project's with its lead's and workers' apart.  None for
an agent the sidecar has counted nothing of.  A failed ask keeps the
last answer, and the next look asks again."
  (ghostherd-test--with-herd
    (let* ((ghostherd-memory-page--tally nil)
           (ghostherd-memory-page--tally-at 0)
           (ghostherd-memory-page--tallying nil)
           (ghostherd-memory-page--herd-soon nil)
           (now 1000.0)
           (span (lambda (w) (list :weighted w :input 1 :cache_write 2 :cache_read 3 :output 4)))
           (answer (list :today "2026-10-28" :days 7
                         :agents (list (list :name "claude-api" :project "/p/" :role "lead"
                                             :today (funcall span 10) :week (funcall span 70)))
                         :projects (list (list :project "/p/" :today (funcall span 10) :week (funcall span 70)
                                               :lead (list :today (funcall span 10) :week (funcall span 70))
                                               :workers (list :today (funcall span 0)
                                                              :week (funcall span "x")))))))
      (puthash "agy-x" (ghostherd-session--create :id "agy-x" :name "agy-x" :kind 'agy :state 'idle
                                                  :project "/p/" :backend 'tmux)
               ghostherd--sessions)
      (setq reply (lambda (_m _p) answer))
      (cl-letf* ((real-float-time (symbol-function 'float-time))
                 ((symbol-function 'float-time)
                  (lambda (&optional time) (if time (funcall real-float-time time) now))))
        (ghostherd-memory-page--tally-look)
        (ghostherd-memory-page--tally-look)
        (should (equal requests '(("herd_tally" nil))))
        (let ((tally (lambda (name) (plist-get (ghostherd-memory-page--agent (ghostherd-get name)) :tally))))
          (should (equal (funcall tally "claude-api")
                         '(:today (:w 10 :input 1 :write 2 :read 3 :output 4)
                           :week (:w 70 :input 1 :write 2 :read 3 :output 4))))
          (should (eq (funcall tally "agy-x") :json-false))
          (let ((p (aref (ghostherd-memory-page--tally-projects) 0)))
            (should (equal (plist-get p :project) "/p/"))
            (should (equal (plist-get (plist-get p :lead) :week) '(:w 70 :input 1 :write 2 :read 3 :output 4)))
            ;; What is not a count is none.
            (should (equal (plist-get (plist-get (plist-get p :workers) :week) :w) 0)))
          ;; Due again: asked again.
          (setq now (+ now ghostherd-memory-page--tally-every 1))
          (ghostherd-memory-page--tally-look)
          (should (= (length requests) 2))
          (setq fail "gone" now (+ now ghostherd-memory-page--tally-every 1))
          (ghostherd-memory-page--tally-look)
          (should (= (length requests) 3))
          (should-not ghostherd-memory-page--tallying)
          (should (equal (plist-get (plist-get (funcall tally "claude-api") :week) :w) 70)))
        ;; The page is sent it with the herd.
        (ghostherd-memory-page--send-herd)
        (let ((sent (ghostherd-test--call calls "renderHerd")))
          (should (vectorp (plist-get sent :projectTally)))
          (should (equal (plist-get (aref (plist-get sent :projectTally) 0) :project) "/p/")))))))

(ert-deftest ghostherd-test-page-forgets-the-sidecars-answers-on-restart ()
  "Stopping the sidecar drops what the old instance answered -- meters,
sub-agents, links and the open conversations' signatures -- so an idle
agent whose transcript will not change is read again by the new sidecar,
instead of staying bound to the old one's empty answer.  The reader's own
`load earlier' limits are intent, not an answer, and are kept."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let ((ghostherd-memory-page--meters (make-hash-table :test 'equal))
        (ghostherd-memory-page--metering (make-hash-table :test 'equal))
        (ghostherd-memory-page--subs (make-hash-table :test 'equal))
        (ghostherd-memory-page--subs-reading (make-hash-table :test 'equal))
        (ghostherd-memory-page--links (make-hash-table :test 'equal))
        (ghostherd-memory-page--room-sent (make-hash-table :test 'equal))
        (ghostherd-memory-page--room-reading (make-hash-table :test 'equal))
        (ghostherd-memory-page--room-limits (make-hash-table :test 'equal))
        (ghostherd-memory-page--chat-sent "sig") (ghostherd-memory-page--chat-reading t)
        (ghostherd-memory-page--sub-sent "sig") (ghostherd-memory-page--sub-reading t)
        (ghostherd-memory--process nil))
    (puthash "a" (cons "sig" nil) ghostherd-memory-page--meters)   ; the stuck empty answer
    (puthash "a" (cons "sig" '(:subs nil)) ghostherd-memory-page--subs)
    (puthash "a" t ghostherd-memory-page--subs-reading)
    (puthash "a" (list :sig "sig" :links nil) ghostherd-memory-page--links)
    (puthash "a" "sig" ghostherd-memory-page--room-sent)
    (puthash "a" 120 ghostherd-memory-page--room-limits)
    ;; Stopping the sidecar runs the hook that forgets them.
    (ghostherd-memory-stop)
    (should (= 0 (hash-table-count ghostherd-memory-page--meters)))
    (should (= 0 (hash-table-count ghostherd-memory-page--metering)))
    (should (= 0 (hash-table-count ghostherd-memory-page--subs)))
    (should (= 0 (hash-table-count ghostherd-memory-page--subs-reading)))
    (should (= 0 (hash-table-count ghostherd-memory-page--links)))
    (should (= 0 (hash-table-count ghostherd-memory-page--room-sent)))
    (should-not ghostherd-memory-page--chat-sent)
    (should-not ghostherd-memory-page--sub-sent)
    ;; Intent, not an answer: kept.
    (should (= 120 (gethash "a" ghostherd-memory-page--room-limits)))))

(ert-deftest ghostherd-test-page-a-failed-meter-is-not-cached ()
  "A request that fails is not an answer: the cache is left empty, so the
next look asks again rather than binding an idle agent to a sidecar that
was gone or too old until its transcript happens to move.  A success --
even an empty one -- is cached as before."
  (ghostherd-test--with-page
    (let ((ghostherd-memory-page--meters (make-hash-table :test 'equal))
          (ghostherd-memory-page--metering (make-hash-table :test 'equal)))
      (setq fail "sidecar gone")
      (ghostherd-memory-page--ask-on-change
       ghostherd-memory-page--meters ghostherd-memory-page--metering
       "a" "sig" "herd_meter" "/p/a.jsonl")
      (should (equal requests '(("herd_meter" (:path "/p/a.jsonl")))))
      (should (= 0 (hash-table-count ghostherd-memory-page--meters)))     ; not poisoned
      (should (= 0 (hash-table-count ghostherd-memory-page--metering)))   ; nothing in flight
      ;; A success is cached, even a legitimately empty one.
      (setq fail nil reply (lambda (_m _p) nil))
      (ghostherd-memory-page--ask-on-change
       ghostherd-memory-page--meters ghostherd-memory-page--metering
       "a" "sig" "herd_meter" "/p/a.jsonl")
      (should (equal (gethash "a" ghostherd-memory-page--meters) (cons "sig" nil))))))

(ert-deftest ghostherd-test-meter-signature-follows-grok-signals ()
  "A grok meter depends on its `signals.json' as much as its transcript:
its tokens live there, written a beat after the conversation, so the
signature must move when that file does -- or the meter binds stale and
is never asked again while the agent idles.  A claude meter does not read
the file, so its signature ignores it."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let* ((ghostherd--sessions (make-hash-table :test #'equal))
         (dir (make-temp-file "gh-metersig" t))
         (tx (expand-file-name "chat_history.jsonl" dir))
         (sig-file (expand-file-name "signals.json" dir))
         (grok (ghostherd-tests--session :name "gk" :kind 'grok :backend 'fake))
         (cl (ghostherd-tests--session :name "cl" :kind 'claude :backend 'fake)))
    (unwind-protect
        (progn
          (write-region "{}\n" nil tx nil 'silent)
          (write-region "{\"contextTokensUsed\":10}\n" nil sig-file nil 'silent)
          (let ((g1 (ghostherd-memory-page--meter-sig grok tx))
                (c1 (ghostherd-memory-page--meter-sig cl tx)))
            ;; signals.json changes; the transcript does not.
            (write-region "{\"contextTokensUsed\":123456}\n" nil sig-file nil 'silent)
            (let ((g2 (ghostherd-memory-page--meter-sig grok tx))
                  (c2 (ghostherd-memory-page--meter-sig cl tx)))
              (should-not (equal g1 g2))       ; grok notices
              (should (equal c1 c2))           ; claude does not
              ;; grok's signature still carries the transcript's, so a change
              ;; there is caught too.
              (should (equal (car g2) (ghostherd-memory-page--room-signature tx))))))
      (dolist (s (list grok cl))
        (when (buffer-live-p (ghostherd-session-buffer s))
          (kill-buffer (ghostherd-session-buffer s))))
      (delete-directory dir t))))

(ert-deftest ghostherd-test-memory-tick-routes-a-path-as-project ()
  "The Memory tab watches a store by its slug `key' (the picker, or `user') or
by a project `path' (the default, a room link).  The sidecar's `key' must be a
slug; a path -- which has a `/' -- must go as `project' for the sidecar to slug
it, else it is refused with \"bad project key\" (live bug 5c17375e).  A slug and
`user' go as `key'."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let (sent)
    (cl-letf (((symbol-function 'xwapp-live-p) (lambda (&rest _) t))
              ((symbol-function 'ghostherd-memory-request-async)
               (lambda (_method _cb params &rest _) (push params sent))))
      (dolist (case '(("/Users/jing/.emacs.d" . (:project "/Users/jing/.emacs.d"))
                      ("~/src/deploy-platform/" . (:project "~/src/deploy-platform/"))
                      ("-Users-jing--emacs-d" . (:key "-Users-jing--emacs-d"))
                      ("user" . (:key "user"))))
        (setq sent nil)
        (let ((ghostherd-memory-page--memory-key (car case)))
          (ghostherd-memory-page--memory-tick)
          (should (equal (car sent) (cdr case))))))))

(ert-deftest ghostherd-test-memory-tick-fetches-proposals-too ()
  "A memory tick asks for the store's facts and its promotion proposals, each
routed the same way (a path as `project', a slug or `user' as `key')."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let (calls)
    (cl-letf (((symbol-function 'xwapp-live-p) (lambda (&rest _) t))
              ((symbol-function 'ghostherd-memory-page--js) (lambda (&rest _) nil))
              ((symbol-function 'ghostherd-memory-request-async)
               (lambda (method _cb params &rest _) (push (cons method params) calls))))
      (let ((ghostherd-memory-page--memory-key "-src-dp"))
        (ghostherd-memory-page--memory-tick))
      (should (equal (assoc "herd_memory_list" calls) '("herd_memory_list" :key "-src-dp")))
      (should (equal (assoc "herd_memory_proposals" calls)
                     '("herd_memory_proposals" :key "-src-dp")))
      ;; A project path routes both as `project'.
      (setq calls nil)
      (let ((ghostherd-memory-page--memory-key "~/src/dp/"))
        (ghostherd-memory-page--memory-tick))
      (should (equal (assoc "herd_memory_proposals" calls)
                     '("herd_memory_proposals" :project "~/src/dp/"))))))

(ert-deftest ghostherd-test-memory-promotion-handlers-send-the-right-rpc ()
  "The page's Proposals controls reach the sidecar: approve and reject by the
proposal id, unpromote by store and name, and Suggest routes the store key or
path.  A refusal from the sidecar surfaces on the page, not silently."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let (sent)
    (cl-letf (((symbol-function 'xwapp-live-p) (lambda (&rest _) t))
              ((symbol-function 'ghostherd-memory-page--js) (lambda (&rest _) nil))
              ((symbol-function 'ghostherd-memory-page--memory-tick) (lambda () nil))
              ((symbol-function 'ghostherd-memory-request-async)
               (lambda (method cb params &rest _)
                 (push (list method params) sent)
                 (funcall cb '(:to "lead-dp")))))
      (ghostherd-memory-page--memory-proposal-approve '((id . "p1")))
      (should (equal (car sent) '("herd_memory_promote_approve" (:id "p1"))))
      (ghostherd-memory-page--memory-proposal-reject '((id . "p1") (reason . "too specific")))
      (should (equal (car sent) '("herd_memory_promote_reject" (:id "p1" :reason "too specific"))))
      (ghostherd-memory-page--memory-unpromote '((key . "-src-dp") (name . "branch-rule")))
      (should (equal (car sent) '("herd_memory_unpromote" (:key "-src-dp" :name "branch-rule"))))
      ;; Suggest: a slug key goes as `key'; a path as `project'; an explicit
      ;; agent rides along.
      (ghostherd-memory-page--memory-suggest '((key . "-src-dp")))
      (should (equal (car sent) '("herd_memory_suggest" (:key "-src-dp"))))
      (ghostherd-memory-page--memory-suggest '((project . "~/src/dp/") (agent . "claude-dp")))
      (should (equal (car sent) '("herd_memory_suggest" (:project "~/src/dp/" :agent "claude-dp")))))
    ;; A sidecar error is shown on the page (via --fail), not swallowed.
    (let (shown)
      (cl-letf (((symbol-function 'ghostherd-memory-page--js)
                 (lambda (fn arg &rest _) (when (equal fn "showError") (setq shown arg))))
                ((symbol-function 'ghostherd-memory-request-async)
                 (lambda (_m _cb _p &optional err) (and err (funcall err '(:message "no lead"))))))
        (ghostherd-memory-page--memory-suggest '((key . "-src-dp")))
        (should (stringp shown))))))

(ert-deftest ghostherd-test-page-reads-sub-agents-when-they-change ()
  "Each agent's sub-agents still running, read by the sidecar when its
transcript changed or a file its last answer named -- not on every
look -- and sent field by field.  None for a dead agent: they stopped
with it."
  (ghostherd-test--with-herd
    (let* ((file (make-temp-file "gh-subs" nil ".jsonl" "{}\n"))
           (folder (make-temp-file "gh-subs" t))
           (running (expand-file-name "agent-a1.jsonl" folder))
           (ghostherd-memory-page--subs (make-hash-table :test 'equal))
           (ghostherd-memory-page--subs-reading (make-hash-table :test 'equal))
           (ghostherd-memory-page--herd-soon nil))
      (write-region "{}\n" nil running nil 'silent)
      (puthash "agy-x" (ghostherd-session--create :id "agy-x" :name "agy-x" :kind 'agy :state 'idle
                                                  :project "/p/" :backend 'tmux)
               ghostherd--sessions)
      (cl-letf (((symbol-function 'ghostherd-session-transcript)
                 (lambda (s) (and (equal (ghostherd-session-name s) "claude-api") file))))
        (unwind-protect
            (let ((subs (lambda (name) (plist-get (ghostherd-memory-page--agent (ghostherd-get name)) :subs)))
                  (asked (lambda () (seq-count (lambda (r) (equal (car r) "herd_subagents")) requests))))
              (setq reply (lambda (_m _p)
                            `(:subs ((:id "a1" :who "general-purpose" :title "Build R3" :started "2026-10-14T08:37:12Z"
                                      :tools 61 :tokens 195958 :doing "Bash · build" :secret "x")
                                     "not a sub-agent")
                              :watch (,folder ,running))))
              (ghostherd-memory-page--subs-look)
              (should (equal requests `(("herd_subagents" (:path ,file)))))
              (should (equal (funcall subs "claude-api")
                             [(:id "a1" :who "general-purpose" :title "Build R3" :started "2026-10-14T08:37:12Z"
                               :tools 61 :tokens 195958 :doing "Bash · build")]))
              (should (equal (funcall subs "agy-x") []))
              ;; What it now watches is new: read once more, then not again.
              (ghostherd-memory-page--subs-look)
              (ghostherd-memory-page--subs-look)
              (should (= (funcall asked) 2))
              ;; A running one wrote: asked again.  Its transcript is the same.
              (write-region "{}\n" nil running t 'silent)
              (set-file-times running (time-add nil 5))
              (setq reply (lambda (_m _p) `(:subs nil :watch (,folder))))
              (ghostherd-memory-page--subs-look)
              (should (= (funcall asked) 3))
              (should (equal (funcall subs "claude-api") []))
              ;; Dead: none, before the next look too, and not asked.
              (setq reply (lambda (_m _p) `(:subs ((:id "a2" :tools 1)) :watch (,folder))))
              (write-region "{}\n" nil file t 'silent)
              (ghostherd-memory-page--subs-look)
              (should (= (length (funcall subs "claude-api")) 1))
              (setf (ghostherd-session-state (ghostherd-get "claude-api")) 'dead)
              (should (equal (funcall subs "claude-api") []))
              (ghostherd-memory-page--subs-look)
              (should (= (funcall asked) 4))
              (should (equal (funcall subs "claude-api") [])))
          (delete-file file)
          (delete-directory folder t))))))

(ert-deftest ghostherd-test-room-sends-a-sub-agents-report ()
  "A sub-agent's report in a column, and the line that started it: what
the page needs of each, field by field."
  (ghostherd-test--with-room
    (setq reply (lambda (_m _p)
                  '(:cli "claude"
                    :entries ((:role "tools" :tools ((:name "Agent" :hint "Build R3" :sub "a1" :state "completed")
                                                     (:name "Bash" :hint "ls")))
                              (:role "sub" :kind "report" :sub "a1" :who "general-purpose" :title "Build R3"
                               :text "## built" :status "completed" :tokens 195958 :uses 61 :ms 931357
                               :ts "2026-10-14T08:52:44Z" :secret "x")
                              (:role "sub" :kind "report" :sub "a2" :text "partial" :tokens "many")))))
    (ghostherd-memory-page--room-tick)
    (let* ((entries (plist-get (ghostherd-test--call calls "setConversation") :entries))
           (tools (plist-get (aref entries 0) :tools))
           (e (aref entries 1)))
      (should (equal (seq-map (lambda (k) (plist-get (aref tools 0) k)) '(:name :hint :sub :state))
                     '("Agent" "Build R3" "a1" "completed")))
      (should (equal (seq-map (lambda (k) (plist-get (aref tools 1) k)) '(:sub :state)) '(nil nil)))
      (should (equal (seq-map (lambda (k) (plist-get e k))
                              '(:role :sub :who :title :text :status :tokens :uses :ms :ts))
                     '("sub" "a1" "general-purpose" "Build R3" "## built" "completed" 195958 61 931357
                       "2026-10-14T08:52:44Z")))
      (should-not (plist-member e :secret))
      (should (equal (seq-map (lambda (k) (plist-get (aref entries 2) k)) '(:status :tokens :uses :ms))
                     '(nil nil nil nil))))))

(ert-deftest ghostherd-test-page-switches-a-model ()
  "The page is told whose model it can switch, and to what; its Switch
goes to `ghostherd-switch-model', and the page hears how it went."
  (ghostherd-test--with-herd
    (ghostherd-memory-page--send-herd)
    (let ((herd (ghostherd-test--call calls "renderHerd")))
      (should (equal (plist-get (plist-get herd :pickers) :claude)
                     '(:models ["Default" "Opus" "Fable" "Sonnet" "Haiku"]
                       :efforts ["low" "medium" "high" "xhigh" "max"] :scope "session")))
      (should (equal (plist-get (plist-get herd :pickers) :grok)
                     '(:models ["grok-4.7" "grok-4.7-build-fast" "grok-4.6" "grok-4.5"]
                       :efforts ["low" "medium" "high" "xhigh"] :scope "default")))
      (should (equal (plist-get (plist-get herd :pickers) :agy)
                     '(:models ["Gemini 4 Argon" "Gemini 3.8 Flash" "Claude Opus 5.5"
                                "Claude Sonnet 5.5" "GPT-OSS 120B"]
                       :efforts ["low" "medium" "high"] :scope "default")))
      (should (eq (plist-get (aref (plist-get herd :agents) 0) :switching) :json-false)))
    (let (asked (said "Set model to Sonnet 5.5 for this session only"))
      (cl-letf (((symbol-function 'ghostherd-switch-model)
                 (lambda (s model effort done)
                   (setq asked (list (ghostherd-session-name s) model effort))
                   (funcall done (not (equal model "Fable")) (if (equal model "Fable") "its picker has no Fable" said)))))
        (ghostherd-memory-page--agent-act "agent-model" '((name . "claude-api") (model . "Sonnet") (effort . "")))
        (should (equal asked '("claude-api" "Sonnet" nil)))
        (should (member (cons "flash" said) calls))
        (should (member '("flash" . "Switching claude-api to Sonnet…") calls))
        (ghostherd-memory-page--agent-act "agent-model" '((name . "claude-api") (model . "Fable") (effort . "max")))
        (should (equal asked '("claude-api" "Fable" "max")))
        (should (member '("flash" . "claude-api: its picker has no Fable") calls))
        (should-error (ghostherd-memory-page--agent-act "agent-model" '((name . "claude-api")))
                      :type 'user-error)))))

;;; Inbox item sources (each kind produced from real-shaped state)

(ert-deftest ghostherd-test-inbox-blocked-menu-item ()
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let* ((a (list :name "qa-dp" :kind "agy" :state "blocked" :role "" :project "~/p/"
                  :since 1000 :login :json-false :continueAt 0
                  :prompt (list :question "Run this command in ~/p?"
                                :options (vector (list :n 1 :label "Yes, run command")
                                                 (list :n 2 :label "No, cancel")))))
         (items (ghostherd-inbox--agent-items a))
         (it (car items)))
    (should (equal (ghostherd-tests--inbox-kinds items) '("blocked")))
    (should (equal (plist-get it :id) "blocked:qa-dp"))
    (should (equal (plist-get it :group) "need"))
    (should (equal (plist-get it :tag) "Permission"))
    (should (= 2 (length (plist-get it :options))))
    (should (equal (plist-get (aref (plist-get it :options) 0) :label) "Yes, run command"))))

(ert-deftest ghostherd-test-inbox-login-and-relay-and-continue ()
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let ((login (ghostherd-inbox--agent-items
                (list :name "c-jn" :kind "claude" :state "blocked" :role "" :project "~/jn/"
                      :login t :continueAt 0 :prompt :json-false)))
        (cont (ghostherd-inbox--agent-items
               (list :name "c-x" :kind "claude" :state "blocked" :role "" :project "~/x/"
                     :login :json-false :continueAt 1699999999000.0 :continueStopped :json-false
                     :continueAfter "after a dropped connection" :prompt :json-false)))
        (relay (ghostherd-inbox--agent-items
                (list :name "c-y" :kind "claude" :state "idle" :role "" :project "~/y/"
                      :login :json-false :continueAt 0 :prompt :json-false
                      :usedUp "Used up for the week" :relayTo "grok"))))
    (should (equal (ghostherd-tests--inbox-kinds login) '("login")))
    (should (equal (plist-get (car login) :id) "login:c-jn"))
    (should (equal (ghostherd-tests--inbox-kinds cont) '("continue")))
    (should (equal (plist-get (car cont) :group) "fyi"))
    (should (equal (ghostherd-tests--inbox-kinds relay) '("relay")))
    (should (equal (plist-get (car relay) :relayTo) "grok"))))

(ert-deftest ghostherd-test-inbox-lead-question-vs-report ()
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let ((q (ghostherd-inbox--agent-items
            (list :name "lead-dp" :kind "claude" :state "idle" :role "lead" :project "~/dp/"
                  :login :json-false :continueAt 0 :prompt :json-false
                  :said "#405 deployed.\nOpen a ClickUp card for it?" :saidAt 2000)))
        (r (ghostherd-inbox--agent-items
            (list :name "lead-dp" :kind "claude" :state "idle" :role "lead" :project "~/dp/"
                  :login :json-false :continueAt 0 :prompt :json-false
                  :said "Guard hardenings handed out; both agy agents respawned." :saidAt 2000)))
        (worker (ghostherd-inbox--agent-items
                 ;; A worker whose turn was not positively user-started is NOT
                 ;; an inbox item (here an unknown origin: no :saidFromUser).
                 (list :name "c-dp" :kind "claude" :state "idle" :role "" :project "~/dp/"
                       :login :json-false :continueAt 0 :prompt :json-false
                       :said "Finished the migration check." :saidAt 2000))))
    (should (equal (ghostherd-tests--inbox-kinds q) '("question")))
    (should (equal (plist-get (car q) :replyVia) "lead"))
    (should (equal (plist-get (car q) :group) "need"))
    (should (equal (ghostherd-tests--inbox-kinds r) '("report")))
    (should (equal (plist-get (car r) :group) "fyi"))
    (should (null worker))))

(ert-deftest ghostherd-test-inbox-worker-turn-end-by-origin ()
  "A worker's turn end is an Inbox item only when the herd POSITIVELY knows
the user started it.  An ask-started one, and one of unknown origin (from
before the tracking loaded), stay out.  A lead is surfaced either way."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let ((user-report
         (ghostherd-inbox--agent-items
          (list :name "claude-godzilla" :kind "claude" :state "idle" :role "" :project "~/gz/"
                :login :json-false :continueAt 0 :prompt :json-false
                :saidFromUser t
                :said "Shipped the import; nothing left open." :saidAt 3000)))
        (user-question
         (ghostherd-inbox--agent-items
          (list :name "claude-godzilla" :kind "claude" :state "idle" :role "" :project "~/gz/"
                :login :json-false :continueAt 0 :prompt :json-false
                :saidFromUser t
                :said "Done.\nShall I open a PR?" :saidAt 3000)))
        (ask-worker
         (ghostherd-inbox--agent-items
          (list :name "qa-gz" :kind "claude" :state "idle" :role "" :project "~/gz/"
                :login :json-false :continueAt 0 :prompt :json-false
                :saidFromUser :json-false
                :said "Ran the suite: all green." :saidAt 3000)))
        (unknown-worker
         ;; No :saidFromUser -- a turn from before the tracking loaded.  It
         ;; must NOT surface (the backlog the gate clears).
         (ghostherd-inbox--agent-items
          (list :name "claude-ghost" :kind "claude" :state "idle" :role "" :project "~/gh/"
                :login :json-false :continueAt 0 :prompt :json-false
                :said "Old answer from before tracking." :saidAt 3000)))
        (ask-lead
         (ghostherd-inbox--agent-items
          (list :name "lead-gz" :kind "claude" :state "idle" :role "lead" :project "~/gz/"
                :login :json-false :continueAt 0 :prompt :json-false
                :saidFromUser :json-false
                :said "Handed the fix to its author." :saidAt 3000))))
    ;; positively user-started worker: a Report in Updates
    (should (equal (ghostherd-tests--inbox-kinds user-report) '("report")))
    (should (equal (plist-get (car user-report) :group) "fyi"))
    ;; positively user-started worker ending by asking: a Question, to the agent
    (should (equal (ghostherd-tests--inbox-kinds user-question) '("question")))
    (should (equal (plist-get (car user-question) :group) "need"))
    (should (equal (plist-get (car user-question) :replyVia) "agent"))
    ;; ask-started worker: nothing (whoever asked reports)
    (should (null ask-worker))
    ;; unknown-origin worker: nothing (no backlog)
    (should (null unknown-worker))
    ;; a lead is surfaced even when its turn was not positively user-started
    (should (equal (ghostherd-tests--inbox-kinds ask-lead) '("report")))))

(ert-deftest ghostherd-test-inbox-lead-started-for-handoff ()
  "A lead the herd started to take a handoff is an Update, kept until read."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let ((items (ghostherd-inbox--agent-items
                (list :name "lead-jnotes" :kind "claude" :state "idle" :role "lead"
                      :project "~/src/jnotes/" :since 5000
                      :login :json-false :continueAt 0 :prompt :json-false
                      :handoffStarted t :handoffFrom "lead-deploy-platform"))))
    (should (member "lead-started" (ghostherd-tests--inbox-kinds items)))
    (let ((it (seq-find (lambda (x) (equal (plist-get x :kind) "lead-started")) items)))
      (should (equal (plist-get it :id) "lead-started:lead-jnotes"))
      (should (equal (plist-get it :group) "fyi"))
      (should (string-match-p "lead-deploy-platform" (plist-get it :body))))))

(ert-deftest ghostherd-test-turn-origin-ask-vs-user ()
  "An ask the herd delivers marks the turn it starts `ask'; any other
turn start -- a user prompt, or one typed into the terminal -- is `user'.
A mid-turn move into a busy state does not re-mark the turn, and the
pending ask mark is one-shot."
  (ghostherd-tests--with-herd ((ghostherd--turn-origin (make-hash-table :test 'equal))
                               (ghostherd--turn-ask-pending (make-hash-table :test 'equal)))
    (let ((s (ghostherd-tests--session :name "w" :kind 'claude :state 'idle)))
      ;; No ask pending: a turn start is the user's.
      (ghostherd--turn-origin-on-state s 'idle 'working)
      (should (eq (ghostherd--turn-origin-of s) 'user))
      ;; The herd delivers an ask: the next turn start is `ask'.
      (ghostherd--turn-from-ask s)
      (ghostherd--turn-origin-on-state s 'idle 'working)
      (should (eq (ghostherd--turn-origin-of s) 'ask))
      ;; A mid-turn block (OLD already busy) does not re-mark it.
      (ghostherd--turn-origin-on-state s 'working 'blocked)
      (should (eq (ghostherd--turn-origin-of s) 'ask))
      ;; A fresh turn with no ask pending (typed into the terminal) is `user'.
      (ghostherd--turn-origin-on-state s 'idle 'working)
      (should (eq (ghostherd--turn-origin-of s) 'user))
      ;; The pending mark is one-shot: an ask turn, then a plain one.
      (ghostherd--turn-from-ask s)
      (ghostherd--turn-origin-on-state s 'done 'working)
      (should (eq (ghostherd--turn-origin-of s) 'ask))
      (ghostherd--turn-origin-on-state s 'idle 'working)
      (should (eq (ghostherd--turn-origin-of s) 'user)))))

(ert-deftest ghostherd-test-ask-delivery-marks-the-turn-ask-not-user ()
  "Delivering an ask marks the turn `ask' even though the send moves the
taker to `working' synchronously (running the origin hook).  The pending
flag must be set BEFORE the message; set after, the hook saw no pending
and marked the ask turn the user's, so the taker's turn end -- after it
answered and kept working -- leaked into the Inbox as a report."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (ghostherd-tests--with-herd ((ghostherd--turn-origin (make-hash-table :test 'equal))
                               (ghostherd--turn-ask-pending (make-hash-table :test 'equal))
                               (ghostherd--asks-open (make-hash-table :test 'equal))
                               (ghostherd--role-owed (make-hash-table :test 'equal)))
    (let ((s (ghostherd-tests--session :name "qa" :kind 'claude :state 'idle)))
      ;; ghostherd-message stands in for the real send, which moves the taker
      ;; to `working' synchronously and runs the origin hook right then.
      (cl-letf (((symbol-function 'ghostherd-message)
                 (lambda (_from to &rest _)
                   (ghostherd--turn-origin-on-state (ghostherd-get to) 'idle 'working))))
        (ghostherd--ask-deliver "a1b2c3d4" "lead-x" "qa" "do the thing" 5))
      (should (eq (ghostherd--turn-origin-of s) 'ask))
      ;; It stays `ask' through a mid-turn waiting flap and past its answer...
      (ghostherd--turn-origin-on-state s 'working 'waiting)
      (ghostherd--turn-origin-on-state s 'waiting 'working)
      (should (eq (ghostherd--turn-origin-of s) 'ask))
      ;; ...so the taker's turn end is NOT an Inbox item.
      (should (null (ghostherd-inbox--agent-items
                     (list :name "qa" :kind "claude" :state "idle" :role ""
                           :login :json-false :continueAt 0 :prompt :json-false
                           :saidFromUser (if (eq (ghostherd--turn-origin-of s) 'user) t :json-false)
                           :said "Both fixed." :saidAt 2000))))
      ;; A turn the USER started still shows.
      (should (equal '("report")
                     (ghostherd-tests--inbox-kinds
                      (ghostherd-inbox--agent-items
                       (list :name "claude-gz" :kind "claude" :state "idle" :role ""
                             :login :json-false :continueAt 0 :prompt :json-false
                             :saidFromUser t :said "Shipped it." :saidAt 2000))))))))

(ert-deftest ghostherd-test-herd-mail-turn-is-not-the-users ()
  "Mail the sidecar delivers starts a turn that is not the user's -- the
herd's note that an ask was cancelled, an agent's message, a handoff --
so a worker's \"ok, stopped\" after it is no Inbox report.  The user's
own words (Telegram's prompt, `turn' \"user\") are the user's turn; an
answer to its own ask or a guard verdict (\"carry\") keeps the origin of
the turn it carries on.  Mail not submitted marks nothing: the Return
pressed on it later is someone's own."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (ghostherd-tests--with-herd ((ghostherd--turn-origin (make-hash-table :test 'equal))
                               (ghostherd--turn-ask-pending (make-hash-table :test 'equal))
                               (ghostherd--handoff-watches (make-hash-table :test 'equal)))
    (let ((s (ghostherd-tests--session :name "w" :kind 'claude :state 'idle))
          (deliver (lambda (&rest msg)
                     (ghostherd--herd-deliver-one (append msg '(:from "herd" :to "w" :body "x" :turn "")))
                     (prog1 (ghostherd--turn-origin-of "w")
                       ;; The turn ends; the next starts from idle.
                       (ghostherd--turn-origin-on-state (ghostherd-get "w") 'working 'idle)))))
      ;; The send moves the taker to `working' at once, running the hook.
      (cl-letf (((symbol-function 'ghostherd-message)
                 (lambda (_from to &rest _)
                   (ghostherd--turn-origin-on-state (ghostherd-get to) 'idle 'working)))
                ((symbol-function 'run-with-timer) #'ignore))
        (should (eq (funcall deliver :body "[herd] Ask 1 was cancelled by its asker") 'mail))
        ;; Its "ok, stopped" is no Inbox item.
        (should (null (ghostherd-inbox--agent-items
                       (list :name "w" :kind "claude" :state "idle" :role ""
                             :login :json-false :continueAt 0 :prompt :json-false
                             :saidFromUser (if (eq (ghostherd--turn-origin-of s) 'user) t :json-false)
                             :said "Ok, stopped." :saidAt 2000))))
        (should (eq (funcall deliver :from "claude-rd" :turn "") 'mail))
        (should (eq (funcall deliver :from "claude-rd" :handoff t) 'mail))
        (should (eq (funcall deliver :from "telegram" :turn "user") 'user))
        ;; Carried on: the user's work goes on as the user's, an ask's as
        ;; the ask's; with no turn known, it is not the user's.
        (should (eq (funcall deliver :from "qa" :turn "carry") 'user))
        (should (eq (funcall deliver :from "qa" :turn "carry") 'user))
        (puthash (ghostherd-session-id s) 'ask ghostherd--turn-origin)
        (should (eq (funcall deliver :from "qa" :turn "carry") 'ask))
        (remhash (ghostherd-session-id s) ghostherd--turn-origin)
        (should (eq (funcall deliver :from "qa" :turn "carry") 'mail))
        ;; A sidecar from before `turn' says nothing: left as it was.
        (cl-letf (((symbol-function 'ghostherd-message) #'ignore))
          (ghostherd--herd-deliver-one '(:from "claude-rd" :to "w" :body "x")))
        (should-not (gethash (ghostherd-session-id s) ghostherd--turn-ask-pending))
        ;; Not submitted: no mark, so the turn its Return starts is the user's.
        (cl-letf (((symbol-function 'ghostherd-message) #'ignore))
          (ghostherd--herd-deliver-one '(:from "claude-rd" :to "w" :body "draft" :submit nil :turn "")))
        (should-not (gethash (ghostherd-session-id s) ghostherd--turn-ask-pending))
        (ghostherd--turn-origin-on-state s 'idle 'working)
        (should (eq (ghostherd--turn-origin-of s) 'user))
        ;; The mail's paste never reached the CLI: its mark goes with it.
        (ghostherd--turn-origin-on-state s 'working 'idle)
        (ghostherd--turn-from-ask s 'mail)
        (cl-letf (((symbol-function 'ghostherd--log-add) #'ignore)
                  ((symbol-function 'ghostherd--role-give-back) #'ignore))
          (ghostherd--submit-lost s "x" ""))
        (ghostherd--turn-origin-on-state s 'idle 'working)
        (should (eq (ghostherd--turn-origin-of s) 'user))))))

(ert-deftest ghostherd-test-mail-turn-waits-for-its-submit ()
  "A mail's mark, as an ask's, waits for its prompt to reach the CLI: a
lost Return then pressed again starts the mail's turn, not the user's."
  (ghostherd-tests--with-herd ((ghostherd--turn-origin (make-hash-table :test 'equal))
                               (ghostherd--turn-ask-pending (make-hash-table :test 'equal)))
    (let ((s (ghostherd-tests--session :name "w" :kind 'claude :state 'idle)))
      (ghostherd--turn-from-ask s 'mail)
      (cl-letf (((symbol-function 'ghostherd--submit-pending-p) (lambda (_) t)))
        (ghostherd--turn-origin-on-state s 'idle 'working))
      (should (eq (ghostherd--turn-origin-of s) 'mail))
      (should (gethash (ghostherd-session-id s) ghostherd--turn-ask-pending))
      (ghostherd--turn-origin-on-state s 'working 'idle)
      (setf (ghostherd-session-state s) 'idle)
      (ghostherd--turn-submitted s)
      (should (equal (gethash (ghostherd-session-id s) ghostherd--turn-ask-pending) '(submitted . mail)))
      (ghostherd--turn-origin-on-state s 'idle 'working)
      (should (eq (ghostherd--turn-origin-of s) 'mail))
      (should-not (gethash (ghostherd-session-id s) ghostherd--turn-ask-pending))
      ;; A flag left from before the table held an origin is an ask's.
      (puthash (ghostherd-session-id s) t ghostherd--turn-ask-pending)
      (ghostherd--turn-origin-on-state s 'idle 'working)
      (should (eq (ghostherd--turn-origin-of s) 'ask)))))

(ert-deftest ghostherd-test-inbox-asks-p-heuristic ()
  (skip-unless (require 'ghostherd-memory-page nil t))
  (should (ghostherd-inbox--asks-p "all done.\nShall I open a PR?"))
  (should (ghostherd-inbox--asks-p "要開一張卡嗎？"))
  (should-not (ghostherd-inbox--asks-p "done, nothing left open."))
  (should-not (ghostherd-inbox--asks-p "")))

(ert-deftest ghostherd-test-inbox-guard-and-proposal-items ()
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let ((guard (ghostherd-inbox--guard-items
                (list (list :id "g1" :session "agy-tai" :summary "cat .env" :reason "secret file"
                            :rule "secret-file" :kind "secret" :approvable t :status "pending" :created_ms 500)
                      (list :id "g2" :session "x" :approvable nil :status "pending"))))   ; not approvable -> skipped
        (props (ghostherd-inbox--proposal-items
                (list (list :id "p1" :project "~/dp/" :author "lead-dp" :file "CLAUDE.md"
                            :status "pending" :diff "--- a\n+++ b\n+x"
                            :facts (vector (list :name "a" :title "Branch discipline")
                                           (list :name "b" :title "Uv lock")))))))
    (should (equal (ghostherd-tests--inbox-kinds guard) '("guard")))
    (should (equal (plist-get (car guard) :id) "guard:g1"))
    (should (equal (plist-get (car guard) :code) "cat .env"))
    (should (equal (ghostherd-tests--inbox-kinds props) '("proposal")))
    (should (equal (plist-get (car props) :title) "Promote 2 facts into CLAUDE.md"))
    (should (= 2 (length (plist-get (car props) :facts))))
    (should (string-match-p "\\+x" (plist-get (car props) :diff)))))

;;; How an answer's work went (ask_trace.py; ask 4093758e)

(ert-deftest ghostherd-test-an-answers-trace-reaches-the-threads-page ()
  "A thread's answer goes to the page with its line, whether it was rough,
and the counts behind it -- empty ones as nothing, not a broken object."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let* ((reply (json-parse-string
                 (concat "{\"id\":\"t\",\"msgs\":[{\"kind\":\"answer\",\"from\":\"a\",\"to\":\"b\",\"ask\":\"k\","
                         "\"trace\":\"How it went: 4 min · 3 tool calls, none failed\",\"rough\":false,"
                         "\"traceDetail\":{\"seen\":true,\"tools\":{\"Bash\":2,\"Read\":1},\"failed\":{},"
                         "\"asks\":{},\"failures_known\":true}}],\"lanes\":[]}")
                 :object-type 'plist :array-type 'list :false-object nil :null-object nil))
         (json (json-encode (ghostherd-threads--js reply))))
    (should (string-search "\"trace\":\"How it went: 4 min · 3 tool calls, none failed\"" json))
    (should (string-search "\"tools\":{\"Bash\":2,\"Read\":1}" json))
    (should (string-search "\"msgs\":[{" json))
    (should (string-search "\"failed\":null" json))))

;;; The checks (bin/herd_lint.py; ask bd7efaa8)

(ert-deftest ghostherd-test-a-checks-denial-is-logged-once ()
  "Each denial a check reports is one log line, of kind `lint', naming the
fact and the command -- on one line, cut short."
  (ghostherd-tests--with-herd ((ghostherd--log nil)
                               (ghostherd--lint-seen (make-hash-table :test 'equal)))
    (let ((d (list :id "d1" :session "claude-p" :key "user" :fact "grep-cli-binaries-cheaply"
                   :cli "claude" :command (concat "grep -a -o '.{0,60}x'\n  ~/.local/bin/claude "
                                                  (make-string 300 ?y)))))
      (ghostherd--herd-take-lint (list d))
      (ghostherd--herd-take-lint (list d (list :id "d2" :session "agy-p" :fact "no-ls" :cli "agy" :command "ls")))
      (should (equal (mapcar #'ghostherd-log-entry-session (reverse ghostherd--log)) '("claude-p" "agy-p")))
      (let ((first (ghostherd-log-entry-text (cadr ghostherd--log))))
        (should (string-prefix-p "check grep-cli-binaries-cheaply denied (claude): grep -a -o '.{0,60}x' ~/.local/bin/claude"
                                 first))
        (should-not (string-match-p "\n" first))
        (should (< (length first) 230)))
      (should (eq (ghostherd-log-entry-kind (car ghostherd--log)) 'lint)))))

(defconst ghostherd-tests--check-rule
  (list :fact "no-ls" :hint "List with find instead." :status "on"
        :check (list :command (list :all (list "^ls\\b") :none (list "-s")))
        :catches (list "ls") :passes (list "find ." "ls -s"))
  "A check's rule as json.el reads it from the sidecar: every list a list.")

(ert-deftest ghostherd-test-a-check-reaches-the-page-with-arrays ()
  "A check proposal's rule -- its examples and each view's patterns -- and
the store's checks go to the page as JSON arrays, never a list json.el
might take for an object."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let* ((p (ghostherd-memory-page--proposal-for-page
             (list :id "p1" :kind "lint" :status "pending" :file "lint" :author "claude-p"
                   :facts (list (list :name "no-ls" :title "No ls"))
                   :rule ghostherd-tests--check-rule)))
         (json (json-encode p)))
    (should (string-match-p "\"catches\":\\[\"ls\"\\]" json))
    (should (string-match-p "\"passes\":\\[\"find .\",\"ls -s\"\\]" json))
    (should (string-match-p "\"command\":{\"all\":\\[\"\\^ls\\\\\\\\b\"\\],\"none\":\\[\"-s\"\\]}" json))
    (should (string-match-p "\"facts\":\\[{" json)))
  (let ((ghostherd-memory-page--memory-key "-k")
        (ghostherd-memory-page--checks-sig nil)
        pushed)
    (cl-letf (((symbol-function 'ghostherd-memory-request-async)
               (lambda (method cb params &rest _)
                 (should (equal (list method params) '("herd_lint_rules" (:key "-k"))))
                 (funcall cb (list :key "-k" :rules (list (append (list :count 3 :last_command "ls")
                                                                  ghostherd-tests--check-rule))))))
              ((symbol-function 'ghostherd-memory-page--js)
               (lambda (fn obj) (push (cons fn obj) pushed))))
      (ghostherd-memory-page--memory-checks-tick "-k")
      ;; Unchanged: not pushed again.
      (ghostherd-memory-page--memory-checks-tick "-k"))
    (should (= 1 (length pushed)))
    (should (equal (caar pushed) "renderChecks"))
    (let ((json (json-encode (cdar pushed))))
      (should (string-match-p "\"rules\":\\[{" json))
      (should (string-match-p "\"count\":3" json)))))

(ert-deftest ghostherd-test-a-check-proposal-is-an-inbox-item ()
  "A check proposal is a Needs-you item of its own: the fact, the hint and
what it denies and lets through, approved by the same proposal id."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let* ((items (ghostherd-inbox--proposal-items
                 (list (list :id "p9" :kind "lint" :project "user" :author "claude-p" :file "lint"
                             :status "pending" :diff "{}" :rule ghostherd-tests--check-rule
                             :facts (list (list :name "no-ls" :title "No ls"))))))
         (it (car items)))
    (should (equal (ghostherd-tests--inbox-kinds items) '("proposal")))
    (should (equal (plist-get it :id) "proposal:p9"))
    (should (equal (plist-get it :proposalId) "p9"))
    (should (equal (plist-get it :title) "Make “No ls” a check"))
    (should (eq (plist-get it :lint) t))
    (should (equal (plist-get it :hint) "List with find instead."))
    (should (equal (plist-get it :catches) ["ls"]))
    (should (equal (plist-get it :passes) ["find ." "ls -s"]))
    (should (string-match-p "any project" (plist-get it :body)))))

(ert-deftest ghostherd-test-a-fact-for-every-project-is-an-inbox-item ()
  "A proposal to make a project fact every project's (ask d197da21) is a
Needs-you item: the fact, its text and its check, approved by its id; and
the approval says it moved."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let* ((p (list :id "p7" :kind "share" :project "-src-dp" :author "lead-dp" :file "user"
                  :status "pending" :diff "Never grep a CLI binary with a leading .{0,N}."
                  :facts (list (list :name "grep-cli" :title "Grep cli" :description "strings | grep -F"
                                     :check "off"))))
         (it (car (ghostherd-inbox--proposal-items (list p)))))
    (should (equal (plist-get it :id) "proposal:p7"))
    (should (equal (plist-get it :proposalId) "p7"))
    (should (equal (plist-get it :title) "Make “Grep cli” every project's"))
    (should (eq (plist-get it :share) t))
    (should (equal (plist-get it :text) "Never grep a CLI binary with a leading .{0,N}."))
    (should (equal (plist-get it :check) "off"))
    (should (equal (plist-get it :preview) "strings | grep -F"))
    (should (string-match-p "Its check moves too, and stays off" (plist-get it :body)))
    (should-not (plist-get it :lint))
    ;; No check: none said.
    (setf (plist-get (car (plist-get p :facts)) :check) nil)
    (let ((it (car (ghostherd-inbox--proposal-items (list p)))))
      (should (equal (plist-get it :check) ""))
      (should-not (string-match-p "check" (plist-get it :body)))))
  (let (flashed)
    (cl-letf (((symbol-function 'ghostherd-memory-page--js)
               (lambda (fn &rest args) (when (equal fn "flash") (push (car args) flashed))))
              ((symbol-function 'ghostherd-memory-page--memory-tick) (lambda () nil))
              ((symbol-function 'ghostherd-memory-request-async)
               (lambda (_method cb _params &rest _)
                 (funcall cb '(:kind "share" :fact "grep-cli" :status "applied")))))
      (ghostherd-memory-page--handle '((op . "memory-proposal-approve") (id . "p7")))
      (should (equal (car flashed) "Every project's now: grep-cli")))))

(ert-deftest ghostherd-test-the-pages-check-controls-send-the-right-rpc ()
  "Turn off, turn on and remove reach the sidecar by store and fact; an
approved check says it is on."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let (sent flashed)
    (cl-letf (((symbol-function 'ghostherd-memory-page--js)
               (lambda (fn &rest args) (when (equal fn "flash") (push (car args) flashed))))
              ((symbol-function 'ghostherd-memory-page--memory-tick) (lambda () nil))
              ((symbol-function 'ghostherd-memory-request-async)
               (lambda (method cb params &rest _)
                 (push (list method params) sent)
                 (funcall cb '(:kind "lint" :fact "no-ls" :status "applied")))))
      (ghostherd-memory-page--handle '((op . "lint-set") (key . "user") (fact . "no-ls") (status . "off")))
      (should (equal (car sent) '("herd_lint_set" (:key "user" :fact "no-ls" :status "off"))))
      (ghostherd-memory-page--handle '((op . "lint-set") (key . "~/src/dp/") (fact . "no-ls") (status . "on")))
      (should (equal (car sent) '("herd_lint_set" (:project "~/src/dp/" :fact "no-ls" :status "on"))))
      (ghostherd-memory-page--handle '((op . "lint-remove") (key . "-src-dp") (fact . "no-ls")))
      (should (equal (car sent) '("herd_lint_remove" (:key "-src-dp" :fact "no-ls"))))
      (ghostherd-memory-page--memory-proposal-approve '((id . "p9")))
      (should (equal (car flashed) "Check on: no-ls")))))

(ert-deftest ghostherd-test-inbox-ask-items ()
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let ((items (ghostherd-inbox--ask-items
                (list (list :id "k1" :from "lead-jn" :to "lead-dp" :status "failed"
                            :error "no such agent" :head "do the thing" :created 100)
                      (list :id "k2" :from "lead-jn" :to "lead-dp" :status "answered"
                            :project "~/dp/" :fromProject "~/jn/" :head "fix it"
                            :replyHead "fixed in 72f7736" :answered 200)
                      (list :id "k3" :from "a" :to "b" :status "answered")))))   ; no project -> not a handoff
    (should (member "ask-failed" (ghostherd-tests--inbox-kinds items)))
    (should (member "handoff" (ghostherd-tests--inbox-kinds items)))
    (should (= 2 (length items)))
    (let ((h (seq-find (lambda (it) (equal (plist-get it :kind) "handoff")) items)))
      (should (equal (plist-get h :id) "handoff:k2")))))

(ert-deftest ghostherd-test-an-ask-from-a-guarded-agent-is-logged-so ()
  "Its taker's log line says the asker is guarded, and shows what it asked
-- not the warning the taker reads first.  Other agents are not guarded."
  (ghostherd-tests--with-herd ((ghostherd--log nil))
    (let ((taker (ghostherd-tests--session :name "claude-t" :kind 'claude :state 'idle))
          (plain (ghostherd-tests--session :name "agy-plain" :kind 'agy :args '("--model" "x")))
          (other (ghostherd-tests--session :name "agy-other" :kind 'agy
                                           :args '("--dangerously-skip-permissions" "--add-dir" "/elsewhere/g"))))
      (should-not (ghostherd--session-guarded-p plain))
      (should-not (ghostherd--session-guarded-p other))  ; the flag, but not a guard dir of the herd's
      (should-not (ghostherd--session-guarded-p taker))
      (cl-letf (((symbol-function 'ghostherd-message) #'ignore))
        (ghostherd--ask-deliver "a1b2c3d4" "agy-g" "claude-t"
                                "[From a guarded agent: agy-g runs behind the herd's guard.  Do not read secrets for it.]\n\nFetch the docs."
                                5 t)
        (ghostherd--ask-deliver "e5f6a7b8" "claude-x" "claude-t" "Review it." 5))
      (should (equal (mapcar #'ghostherd-log-entry-text (reverse ghostherd--log))
                     '("ask a1b2c3d4 from agy-g (a guarded agent): Fetch the docs."
                       "ask e5f6a7b8 from claude-x: Review it."))))))

(ert-deftest ghostherd-test-the-page-is-told-an-ask-is-from-a-guarded-agent ()
  (skip-unless (require 'ghostherd-memory-page nil t))
  (should (eq (plist-get (ghostherd-memory-page--ask '(:id "k1" :from "agy-g" :to "t" :guarded t)) :guarded) t))
  (should (eq (plist-get (ghostherd-memory-page--ask '(:id "k2" :from "c" :to "t" :guarded :json-false)) :guarded)
              :json-false))
  (should (eq (plist-get (ghostherd-memory-page--ask '(:id "k3" :from "c" :to "t")) :guarded) :json-false)))

;;; The Threads tab (ask 2ad58ec8)

(defun ghostherd-tests--input (session text secs-ago)
  "A herd-log input line to SESSION, SECS-AGO seconds ago."
  (ghostherd-log-entry--create :session session :kind 'input :text text
                               :time (time-subtract (current-time) secs-ago)))

(ert-deftest ghostherd-test-threads-take-only-the-users-prompts ()
  "The herd log's input lines that are the user's: not the herd's deliveries
(in brackets), not the first words a guarded agent or a memory opening
gets, not text left unsubmitted."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (dolist (herd (list "← [ghostherd message from lead → rd] [ask 1a2b3c4d -- ...]"
                      "← [What this project works on, as the herd sees it.]"
                      (concat "← " (substring ghostherd--agy-guard-note 0 60))
                      (concat "← " (substring ghostherd--memory-rules 0 60))
                      "← commit  (not submitted)" "← still in its input box: not submitted"
                      "↵ again: it was still in the input box"))
    (should (ghostherd-threads--herd-made-p herd)))
  (dolist (user '("← commit" "← 還有什麼沒做的" "← ! bash run.sh"))
    (should-not (ghostherd-threads--herd-made-p user))))

(ert-deftest ghostherd-test-a-thread-opens-with-the-users-prompt ()
  "The user's last prompt to a thread's first asker before its first ask --
within the window -- is its first message, as the log has it; the list's
summary names \"user\" first and counts it."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let* ((now (* 1000 (float-time)))
         (ghostherd--log (list (ghostherd-tests--input "lead-x" "← [ghostherd message from a → lead-x]" 60)
                               (ghostherd-tests--input "lead-x" "← 三項補強都做" 300)
                               (ghostherd-tests--input "lead-x" "← an older prompt" 900)
                               (ghostherd-tests--input "lead-x" "← a prompt from a day ago" 90000)
                               (ghostherd-tests--input "rd" "← to someone else" 200)))
         (prompts (ghostherd-threads--prompts))
         (sum (list :id "t1" :participants '("lead-x" "rd") :count 2 :rootFrom "lead-x" :started (- now 120000)))
         (whole (append sum (list :lanes (list (list :name "lead-x" :kind "claude")) :msgs (list (list :kind "ask"))))))
    (should (equal (cdr (ghostherd-threads--opener prompts "lead-x" (- now 120000))) "三項補強都做"))
    (let ((s (ghostherd-threads--with-opener sum prompts)))
      (should (equal (plist-get s :participants) '("user" "lead-x" "rd")))
      (should (= (plist-get s :count) 3))
      (should-not (plist-member s :msgs)))
    (let* ((w (ghostherd-threads--with-opener whole prompts))
           (m (car (plist-get w :msgs))))
      (should (equal (mapcar (lambda (l) (plist-get l :name)) (plist-get w :lanes)) '("user" "lead-x")))
      (should (equal (list (plist-get m :kind) (plist-get m :from) (plist-get m :to) (plist-get m :text) (plist-get m :logged))
                     '("message" "user" "lead-x" "三項補強都做" t))))
    ;; Nothing within the window, or the user asked it themselves: no opener.
    (should-not (ghostherd-threads--opener prompts "lead-x" (- now (* 3 86400000))))  ; before them all
    (should-not (ghostherd-threads--opener prompts "lead-x" (- now (* 1000 (+ 900 (* 3 3600))))))  ; 3h on: out of the window
    (should-not (ghostherd-threads--opener prompts "user" now))
    (should (equal (ghostherd-threads--with-opener (plist-put (copy-sequence sum) :rootFrom "nobody") prompts)
                   (plist-put (copy-sequence sum) :rootFrom "nobody")))))

(ert-deftest ghostherd-test-threads-carry-what-a-task-cost ()
  "The Threads tab gets what each thread's task cost and lead mode against
direct mode (server/cost.py) as the sidecar said them: a week's and a
month's comparison as an array -- an empty one too -- and a thread whose
task is not known with none."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let (shown reply
        (ghostherd--log nil)
        (ghostherd-threads--watching nil) (ghostherd-threads--timer nil)
        (ghostherd-threads--limit 40) (ghostherd-threads--req nil)
        (ghostherd-threads--project "") (ghostherd-threads--sort "recent")
        (ghostherd-threads--sig nil) (ghostherd-threads--thread-sig nil))
    (cl-letf (((symbol-function 'xwapp-live-p) (lambda (&rest _) t))
              ((symbol-function 'run-at-time) (lambda (&rest _) 'a-timer))
              ((symbol-function 'timerp) (lambda (x) (eq x 'a-timer)))
              ((symbol-function 'cancel-timer) #'ignore)
              ((symbol-function 'ghostherd-memory-page--js) (lambda (fn obj) (push (cons fn obj) shown)))
              ((symbol-function 'ghostherd-memory-request-async)
               (lambda (method ok _params &optional _err)
                 (when (equal method "herd_threads") (funcall ok reply)))))
      (let ((week (list :days 7 :since "2026-10-22"
                        :lead (list :tasks 2 :median 900 :total 1800 :per100 6000 :own 40)
                        :direct (list :tasks 1 :median 700 :total 700 :per100 nil :own 100))))
        (setq reply (list :threads (list (list :id "t1" :participants '("lead" "rd") :count 2
                                               :cost (list :id "k1" :mode "lead" :total 1800 :own 700
                                                           :workers 1000 :subs 100 :lines 30 :code "yes"))
                                         (list :id "t2" :participants '("rd" "qa") :count 2 :cost nil))
                          :talking nil :live 0 :more nil :costs (list week (plist-put (copy-sequence week) :days 30))))
        (ghostherd-memory-page--handle '((op . "threads-watch")))
        (let* ((sent (cdr (assoc "renderThreads" shown)))
               (json (json-read-from-string (json-encode sent))))
          (should (vectorp (plist-get sent :costs)))
          (should (equal (mapcar (lambda (c) (alist-get 'days c)) (alist-get 'costs json)) '(7 30)))
          (should (equal (alist-get 'per100 (alist-get 'direct (aref (alist-get 'costs json) 0))) nil))
          (let ((threads (alist-get 'threads json)))
            (should (equal (alist-get 'lines (alist-get 'cost (aref threads 0))) 30))
            (should (null (alist-get 'cost (aref threads 1))))))
        ;; No tasks yet: an empty array, not a missing key.
        (setq shown nil ghostherd-threads--sig nil
              reply (list :threads nil :talking nil :live 0 :more nil :costs nil))
        (ghostherd-threads--tick)
        (should (equal (plist-get (cdr (assoc "renderThreads" shown)) :costs) []))))))

(ert-deftest ghostherd-test-threads-a-direct-task-and-its-commits ()
  "A task told to an agent directly comes whole from the sidecar: no
opener from the herd log goes before it, even with a prompt in the log
just before it began; its commits reach the page as an array -- none too
-- and a task's merged count stays a count."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let* ((th (list :id "task:k1" :rootFrom "claude-dp" :rootTo "claude-dp" :started 2000000 :direct t
                   :participants '("user" "claude-dp") :count 2
                   :msgs (list (list :kind "message" :from "user" :to "claude-dp" :at 1999000 :text "Fix it."))
                   :cost (list :id "k1" :mode "direct" :commits 2 :merged 0 :lines 30 :threadCount 2
                               :made (list (list :sha "abc" :lines 30 :merged nil :of nil)))))
         (prompts (make-hash-table :test #'equal)))
    (puthash "claude-dp" (list (cons 1999000 "Fix it.")) prompts)
    (should (eq (ghostherd-threads--with-opener th prompts) th))
    ;; The same as an ask's thread would get the log's prompt before it.
    (should (= 2 (length (plist-get (ghostherd-threads--with-opener
                                     (plist-put (copy-sequence th) :direct nil) prompts)
                                    :msgs))))
    (let* ((json (json-read-from-string (json-encode (ghostherd-threads--js th))))
           (c (alist-get 'cost json)))
      (should (eq (alist-get 'direct json) t))
      (should (equal (alist-get 'merged c) 0))
      (should (equal (alist-get 'threadCount c) 2))
      (should (vectorp (alist-get 'made c)))
      (should (equal (alist-get 'sha (aref (alist-get 'made c) 0)) "abc")))
    (should (equal (plist-get (plist-get (ghostherd-threads--js (list :cost (list :id "k2" :made nil))) :cost) :made)
                   []))))

(ert-deftest ghostherd-test-threads-go-to-the-page-as-json ()
  "The reply reads false and [] as nil: the page gets them back as false
and arrays, objects as objects."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let ((js (ghostherd-threads--js
             (list :threads (list (list :id "t1" :participants '("a" "b") :projects nil :cross nil :count 2))
                   :talking nil :live 0 :more t))))
    (should (equal (plist-get js :talking) []))
    (should (eq (plist-get js :more) t))
    (let ((t1 (aref (plist-get js :threads) 0)))
      (should (equal (plist-get t1 :participants) ["a" "b"]))
      (should (equal (plist-get t1 :projects) []))
      (should (eq (plist-get t1 :cross) :json-false))))
  (should (equal (json-encode (ghostherd-threads--js (list :msgs (list (list :kind "ask" :auto nil)))))
                 "{\"msgs\":[{\"kind\":\"ask\",\"auto\":false}]}")))

(ert-deftest ghostherd-test-the-threads-tab-asks-for-the-list-and-one-thread ()
  "Watching asks the sidecar for the list (forty, then forty more) and the
page gets it; a thread is asked for by its id, an ask or an agent, and
comes to the page whole -- or why it could not be read."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let (asked shown fail
        (ghostherd--log nil)
        (ghostherd-threads--watching nil) (ghostherd-threads--timer nil)
        (ghostherd-threads--limit 40) (ghostherd-threads--req nil)
        (ghostherd-threads--project "") (ghostherd-threads--sort "recent")
        (ghostherd-threads--sig nil) (ghostherd-threads--thread-sig nil))
    (cl-letf (((symbol-function 'xwapp-live-p) (lambda (&rest _) t))
              ((symbol-function 'run-at-time) (lambda (&rest _) 'a-timer))
              ((symbol-function 'timerp) (lambda (x) (eq x 'a-timer)))
              ((symbol-function 'cancel-timer) #'ignore)
              ((symbol-function 'ghostherd-memory-page--js) (lambda (fn obj) (push (cons fn obj) shown)))
              ((symbol-function 'ghostherd-memory-request-async)
               (lambda (method ok params &optional err)
                 (push (cons method params) asked)
                 (if fail (funcall err "ghostherd-memory JSON-RPC -32602: no thread with nobody in it")
                   (funcall ok (pcase method
                                 ("herd_threads" (list :threads (list (list :id "t1" :participants '("a" "b") :count 2))
                                                       :talking nil :live 1 :more nil))
                                 ("herd_thread" (list :id "t1" :lanes nil :msgs nil :waiting nil))))))))
      (ghostherd-memory-page--handle '((op . "threads-watch")))
      (should (equal (car asked) '("herd_threads" :limit 40 :project "" :sort "recent")))
      (should (equal (car (car shown)) "renderThreads"))
      (should (equal (plist-get (cdr (car shown)) :live) 1))
      ;; The same list again is not pushed again.
      (setq shown nil)
      (ghostherd-threads--tick)
      (should-not shown)
      (ghostherd-memory-page--handle '((op . "threads-more")))
      (should (equal (car asked) '("herd_threads" :limit 80 :project "" :sort "recent")))
      (ghostherd-memory-page--handle '((op . "thread-open") (ask . "1a2b3c4d")))
      (should (equal (car asked) '("herd_thread" :ask "1a2b3c4d")))
      (should (equal (car (car shown)) "renderThread"))
      (ghostherd-memory-page--handle '((op . "thread-open") (agent . "rd")))
      (should (equal (car asked) '("herd_thread" :agent "rd")))
      ;; The tick refreshes the open thread too.
      (setq asked nil)
      (ghostherd-threads--tick)
      (should (member '("herd_thread" :agent "rd") asked))
      (setq fail t)
      (ghostherd-memory-page--handle '((op . "thread-open") (agent . "nobody")))
      (should (string-match-p "no thread with nobody" (plist-get (cdr (car shown)) :error)))
      (setq fail nil)
      (ghostherd-memory-page--handle '((op . "thread-close")))
      (should-not ghostherd-threads--req)
      (ghostherd-memory-page--handle '((op . "threads-peek")))
      (should (equal (car (car shown)) "threadsBadge"))
      (ghostherd-memory-page--handle '((op . "threads-unwatch")))
      (should-not ghostherd-threads--watching)
      (should (= ghostherd-threads--limit 40)))))

(ert-deftest ghostherd-test-the-threads-list-is-of-the-project-and-order-the-page-keeps ()
  "The page says which project's threads, and in which order, as it
watches and when the viewer changes them: asked of the sidecar from the
first page again, with every project and the view it is of passed on."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let (asked shown
        (ghostherd--log nil)
        (ghostherd-threads--watching nil) (ghostherd-threads--timer nil)
        (ghostherd-threads--limit 40) (ghostherd-threads--req nil)
        (ghostherd-threads--project "") (ghostherd-threads--sort "recent")
        (ghostherd-threads--sig nil) (ghostherd-threads--thread-sig nil))
    (cl-letf (((symbol-function 'xwapp-live-p) (lambda (&rest _) t))
              ((symbol-function 'run-at-time) (lambda (&rest _) 'a-timer))
              ((symbol-function 'timerp) (lambda (x) (eq x 'a-timer)))
              ((symbol-function 'cancel-timer) #'ignore)
              ((symbol-function 'ghostherd-memory-page--js) (lambda (fn obj) (push (cons fn obj) shown)))
              ((symbol-function 'ghostherd-memory-request-async)
               (lambda (method ok params &optional _err)
                 (push (cons method params) asked)
                 (funcall ok (list :threads nil :talking nil :live 0 :more nil
                                   :projects '("dp" "solo")
                                   :view (list :project (plist-get params :project)
                                               :sort (plist-get params :sort)))))))
      (ghostherd-memory-page--handle '((op . "threads-watch") (project . "solo") (sort . "cost")))
      (should (equal (car asked) '("herd_threads" :limit 40 :project "solo" :sort "cost")))
      (let ((pushed (cdr (car shown))))
        (should (equal (car (car shown)) "renderThreads"))
        (should (equal (json-encode (plist-get pushed :projects)) "[\"dp\",\"solo\"]"))
        (should (equal (json-encode (plist-get pushed :view)) "{\"project\":\"solo\",\"sort\":\"cost\"}")))
      ;; A change goes back to the first page of the new view.
      (ghostherd-memory-page--handle '((op . "threads-more")))
      (should (equal (car asked) '("herd_threads" :limit 80 :project "solo" :sort "cost")))
      (ghostherd-memory-page--handle '((op . "threads-view") (project . "") (sort . "recent")))
      (should (equal (car asked) '("herd_threads" :limit 40 :project "" :sort "recent")))
      ;; An order not one of the two is the newest first; no project, every one.
      (ghostherd-memory-page--handle '((op . "threads-view") (project . 5) (sort . "size")))
      (should (equal (car asked) '("herd_threads" :limit 40 :project "" :sort "recent")))
      ;; Not watching: kept for the next watch, not asked for now.
      (ghostherd-memory-page--handle '((op . "threads-unwatch")))
      (setq asked nil)
      (ghostherd-memory-page--handle '((op . "threads-view") (project . "dp") (sort . "cost")))
      (should-not asked)
      (should (equal (list ghostherd-threads--project ghostherd-threads--sort) '("dp" "cost"))))))

;;; The Threads opener, from the user's real use (ask dbd3994a)

(ert-deftest ghostherd-test-the-log-keeps-the-users-words-behind-the-herds-notes ()
  "A prompt the herd put its notes before -- what the project works on,
what the user said to the team, a role -- is logged as the user's own
words, the herd's notes left out; the herd's own words (a relay's brief)
are logged as the herd's."
  (ghostherd-tests--with-leads
    (let* ((ghostherd--log nil)
           (ghostherd-submit-check-after nil)
           (ghostherd-project-work-functions (list (lambda (_p) (list "branch x in ~/p/"))))
           (lead (ghostherd-tests--session :name "lead-p" :kind 'claude :project "~/p/" :state 'idle))
           (rd (ghostherd-tests--session :name "rd" :kind 'claude :project "~/p/" :state 'idle))
           (sent nil))
      (puthash "lead-p" "lead" ghostherd--roles)
      (cl-letf (((symbol-function 'ghostherd--session-live-p) (lambda (_s) t))
                ((symbol-function 'ghostherd--host-send-text) (lambda (_s text _submit) (push text sent)))
                ((symbol-function 'ghostherd--set-state) #'ignore))
        (ghostherd-tell rd "check the footer")
        (ghostherd-tell lead "剛剛是我重開的沒錯 因爲emacs 突然卡 cpu100%\nsecond line")
        (should (string-prefix-p "[Since you last heard from me" (car sent)))   ; the notes went to the agent
        (ghostherd-tell lead "a brief from the herd" nil t)
        (ghostherd--relay-tell rd "where the work stands" nil nil))     ; an heir told by the herd
      (should (equal (mapcar #'ghostherd-log-entry-text
                             (seq-filter (lambda (e) (eq (ghostherd-log-entry-kind e) 'input)) (reverse ghostherd--log)))
                     '("← check the footer"
                       "← 剛剛是我重開的沒錯 因爲emacs 突然卡 cpu100%"
                       "← [from the herd] a brief from the herd"
                       "← [from the herd] where the work stands"))))))

(ert-deftest ghostherd-test-only-the-last-prompt-can-open-a-thread ()
  "The live case (2026-10-23): the user asked lead-.emacs.d something at
00:55; at 01:01 an answer came back to it and the user wrote again,
behind a note the log of then could not see past; at 01:02 it asked.
The 00:55 prompt is not what set it going -- and with the user's words
logged, 01:01's is."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let* ((now (* 1000 (float-time)))
         (ask (- now 60000))
         (entry (lambda (session text secs) (ghostherd-tests--input session text secs)))
         (old-log (list (funcall entry "lead-x" "← [What this project works on, as the herd sees it. Read them for the work; change none of …" 120)
                        (funcall entry "lead-x" "← [ghostherd message from claude-x → lead-x]" 160)
                        (funcall entry "lead-x" "← 然後 thread 現在是空的？" 540)))
         (new-log (cons (funcall entry "lead-x" "← 剛剛是我重開的沒錯 因爲emacs 突然卡 cpu100%" 120) (cdr old-log))))
    (let ((ghostherd--log old-log))
      (should-not (ghostherd-threads--opener (ghostherd-threads--prompts) "lead-x" ask)))
    (let ((ghostherd--log new-log))
      (should (equal (cdr (ghostherd-threads--opener (ghostherd-threads--prompts) "lead-x" ask))
                     "剛剛是我重開的沒錯 因爲emacs 突然卡 cpu100%")))
    ;; An answer come back is the last input: it set the turn going, not the user.
    (let ((ghostherd--log (list (funcall entry "lead-x" "← [ghostherd message from rd → lead-x]" 90)
                                (funcall entry "lead-x" "← an earlier prompt" 600))))
      (should-not (ghostherd-threads--opener (ghostherd-threads--prompts) "lead-x" ask)))
    ;; The herd's word that a prompt did not go in is no prompt: the prompt still opens.
    (let ((ghostherd--log (list (funcall entry "lead-x" "← still in its input box: not submitted" 80)
                                (funcall entry "lead-x" "← fix it" 100))))
      (should (equal (cdr (ghostherd-threads--opener (ghostherd-threads--prompts) "lead-x" ask)) "fix it")))))

(ert-deftest ghostherd-test-a-turn-ended-since-the-prompt-means-no-opener ()
  "The live case (2026-10-23): the user asked lead-.emacs.d for a doc at
00:06, and its turn ended at 00:07 waiting on the team; at 00:29 an
answer the hook brought began another turn -- no line in the log -- and
it asked again.  That ask is not the 00:06 prompt's.  A permission
prompt in the middle of a turn ends nothing."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let* ((now (* 1000 (float-time)))
         (ask (- now 60000))
         (state (lambda (text secs) (ghostherd-log-entry--create :session "lead-x" :kind 'state :text text
                                                                 :time (time-subtract (current-time) secs))))
         (prompt (ghostherd-tests--input "lead-x" "← 那通行碼的機制寫個文檔幫助了解好了~" 1400)))
    (let ((ghostherd--log (list (funcall state "waiting → working  reported working" 70)
                                (funcall state "working → waiting  waiting on claude-x and 3 shells" 1360)
                                (funcall state "waiting → working  input sent" 1400)
                                prompt)))
      (should-not (ghostherd-threads--opener (ghostherd-threads--prompts) "lead-x" ask)))
    (let ((ghostherd--log (list (funcall state "blocked → working  reported working" 90)
                                (funcall state "working → blocked  a permission prompt" 100)
                                (funcall state "done → idle  seen" 1300)
                                (funcall state "idle → done  seen" 1350)
                                (funcall state "waiting → working  input sent" 1400)
                                prompt)))
      (should (equal (cdr (ghostherd-threads--opener (ghostherd-threads--prompts) "lead-x" ask))
                     "那通行碼的機制寫個文檔幫助了解好了~")))))

(ert-deftest ghostherd-test-an-opener-is-the-askers-and-within-two-hours ()
  "Only the thread's first asker's prompts count -- never another agent's
-- and only one in the two hours before its first ask."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let* ((now (* 1000 (float-time)))
         (ask (- now 1000))
         (ghostherd--log (list (ghostherd-tests--input "rd" "← to rd, just before" 30)
                               (ghostherd-tests--input "lead-x" "← to the lead" (+ 1 (* 2 3600) -120))))
         (prompts (ghostherd-threads--prompts)))
    (should (equal (cdr (ghostherd-threads--opener prompts "lead-x" ask)) "to the lead"))
    (should (equal (cdr (ghostherd-threads--opener prompts "rd" ask)) "to rd, just before"))
    (should-not (ghostherd-threads--opener prompts "qa" ask))                          ; nobody spoke to qa
    ;; Two hours and a minute before: out.
    (let ((ghostherd--log (list (ghostherd-tests--input "lead-x" "← too old" (+ 1 (* 2 3600) 60)))))
      (should-not (ghostherd-threads--opener (ghostherd-threads--prompts) "lead-x" ask)))
    ;; The herd's deliveries and openings are never the user's.
    (dolist (herd (list "← [from the herd] a relay brief" "← [ghostherd message from a → b]"
                        (concat "← " (substring ghostherd--agy-guard-note 0 50))
                        (concat "← " (car (split-string ghostherd--lead-role "\n")))))
      (should (eq (ghostherd-threads--said herd) 'herd)))))

(ert-deftest ghostherd-test-a-prompt-behind-a-note-opens-its-thread-from-its-task ()
  "The live case (2026-10-29): a lead's thread lost its \"You → lead\"
opener when the herd's note came before the user's words, the log having
only the note's first line.  The thread's task has the words, the notes
taken off: they open it -- labelled as its task's transcript has them --
when the task is the asker's own and began between the note and the ask.
Never another agent's task, one begun before the note or after the ask,
one with a turn ended or an answer come back between, or past the window."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let* ((now (* 1000 (float-time)))
         (ask (- now 60000))
         (task-at (/ (- now 119000) 1000.0))            ; a second after the note's line
         (words "Fix the flaky test, then the docs.")
         (cost (list :id "k1" :agent "lead-x" :prompt words :at task-at))
         (note "← [Since you last heard from me, I wrote to the team directly:")
         (ghostherd--log (list (ghostherd-tests--input "lead-x" note 120)
                               (ghostherd-tests--input "lead-x" "← an older prompt" 900)))
         (prompts (ghostherd-threads--prompts))
         (sum (list :id "t1" :participants '("lead-x" "rd") :count 2 :rootFrom "lead-x" :started ask :cost cost))
         (whole (append sum (list :lanes (list (list :name "lead-x" :kind "claude")) :msgs (list (list :kind "ask")))))
         (with (lambda (c) (ghostherd-threads--with-opener (plist-put (copy-sequence sum) :cost c) prompts))))
    (should (eq (ghostherd-threads--said note) 'note))
    (should (eq (ghostherd-threads--said "← [What this project works on, as the herd sees it.  Read them for the work;")
                'note))
    (should (ghostherd-threads--herd-made-p note))
    ;; The log alone has none -- never the older prompt.
    (should-not (ghostherd-threads--opener prompts "lead-x" ask))
    (should (equal (ghostherd-threads--note-opener prompts "lead-x" ask cost) (cons (* 1000 task-at) words)))
    (let ((s (ghostherd-threads--with-opener sum prompts)))
      (should (equal (plist-get s :participants) '("user" "lead-x" "rd")))
      (should (= (plist-get s :count) 3)))
    (let* ((w (ghostherd-threads--with-opener whole prompts))
           (m (car (plist-get w :msgs)))
           (js (aref (plist-get (ghostherd-threads--js w) :msgs) 0)))
      (should (equal (list (plist-get m :from) (plist-get m :to) (plist-get m :text) (plist-get m :at))
                     (list "user" "lead-x" words (* 1000 task-at))))
      (should (equal (list (plist-get js :logged) (plist-get js :transcript)) '(:json-false t))))
    ;; A second thread of the task -- its cost shown on the first -- too.
    (should (equal (plist-get (funcall with (list :id "k1" :same "t0" :agent "lead-x" :prompt words :at task-at))
                              :participants)
                   '("user" "lead-x" "rd")))
    (dolist (c (list nil
                     (plist-put (copy-sequence cost) :agent "rd")                   ; another agent's
                     (plist-put (copy-sequence cost) :at (/ (- now 200000) 1000.0)) ; begun before the note
                     (plist-put (copy-sequence cost) :at (/ (- now 30000) 1000.0))  ; after the ask
                     (plist-put (copy-sequence cost) :prompt "")))
      (should-not (ghostherd-threads--note-opener prompts "lead-x" ask c))
      (should (equal (plist-get (funcall with c) :participants) '("lead-x" "rd"))))
    ;; A turn ended, or an answer came back, after the note: the ask is not its turn's.
    (dolist (after (list (ghostherd-log-entry--create :session "lead-x" :kind 'state :text "working → done  reported done"
                                                      :time (time-subtract (current-time) 90))
                         (ghostherd-tests--input "lead-x" "← [ghostherd message from rd → lead-x]" 90)))
      (let ((ghostherd--log (cons after ghostherd--log)))
        (should-not (ghostherd-threads--note-opener (ghostherd-threads--prompts) "lead-x" ask cost))))
    ;; An ask two hours and a minute after the note: out of the window.
    (should-not (ghostherd-threads--note-opener prompts "lead-x" (- now (* 1000 (- 120 60 (* 2 3600)))) cost))))

(ert-deftest ghostherd-test-inbox-detail-id-ms ()
  "The trailing :MS of a report/question item id, as a number, or nil."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (should (= (ghostherd-inbox--id-ms "report:lead-x:1700000000000") 1700000000000))
  (should (= (ghostherd-inbox--id-ms "question:claude-gz:42") 42))
  (should-not (ghostherd-inbox--id-ms "guard:abc"))
  (should-not (ghostherd-inbox--id-ms "ask-failed:1a2b3c4d"))
  (should-not (ghostherd-inbox--id-ms nil)))

(ert-deftest ghostherd-test-inbox-turn-start-and-thread-in-turn ()
  "The turn-start is the most recent prompt before the turn end, unless a
turn ended since (a hook-brought turn with no logged prompt); a fetched
thread counts as this turn's when its latest activity is after the turn
began, not a stale earlier one."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let* ((now (* 1000 (float-time)))
         (end (- now 1000))
         (state (lambda (text secs) (ghostherd-log-entry--create
                                     :session "lead-x" :kind 'state :text text
                                     :time (time-subtract (current-time) secs)))))
    (let* ((ghostherd--log (list (ghostherd-tests--input "lead-x" "← do the thing" 60)))
           (prompts (ghostherd-threads--prompts))
           (ts (ghostherd-inbox--turn-start prompts "lead-x" end)))
      (should (equal (cdr ts) "do the thing"))
      (should (ghostherd-inbox--thread-in-turn (list :at (- now 2000)) prompts "lead-x" end))
      (should-not (ghostherd-inbox--thread-in-turn (list :at (- (car ts) 10)) prompts "lead-x" end)))
    ;; A turn ended between the prompt and the end: no start (hook-brought).
    (let* ((ghostherd--log (list (funcall state "working → idle  done" 50)
                                 (ghostherd-tests--input "lead-x" "← older" 600)))
           (prompts (ghostherd-threads--prompts)))
      (should-not (ghostherd-inbox--turn-start prompts "lead-x" end)))))

(ert-deftest ghostherd-test-inbox-opener-thread ()
  "A turn that made no asks shows one message -- the user's opener to the
agent -- shaped for the thread renderer; with no user opener, nothing."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let* ((now (* 1000 (float-time)))
         (th (ghostherd-inbox--opener-thread "claude-gz" (cons (- now 5000) "please do X") now)))
    (should (equal (plist-get th :participants) '("user" "claude-gz")))
    (should (equal (mapcar (lambda (l) (plist-get l :name)) (plist-get th :lanes)) '("user" "claude-gz")))
    (should (= (plist-get th :count) 1))
    (let ((m (car (plist-get th :msgs))))
      (should (equal (list (plist-get m :kind) (plist-get m :from) (plist-get m :to)
                           (plist-get m :text) (plist-get m :logged))
                     '("message" "user" "claude-gz" "please do X" t)))))
  (should-not (ghostherd-inbox--opener-thread "claude-gz" (cons 1 'herd) 2))
  (should-not (ghostherd-inbox--opener-thread "claude-gz" nil 2)))

(ert-deftest ghostherd-test-inbox-detail-fetches-thread-and-full-report ()
  "Picking an Inbox item asks the sidecar for its thread (by the agent, or by
the ask it is about) and -- for a report or question -- its full last
message, and pushes each to the page tagged with the item; an ask-failed
item is fetched by its ask with no report fetch."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let (asked pushes
        (ghostherd--log nil)
        (ghostherd-inbox--detail-req nil)
        (now (* 1000 (float-time))))
    (cl-letf (((symbol-function 'ghostherd-memory-page--js) (lambda (fn obj) (push (cons fn obj) pushes)))
              ((symbol-function 'ghostherd-memory-request-async)
               (lambda (method ok params &optional _err)
                 (push (cons method params) asked)
                 (funcall ok (pcase method
                               ("herd_thread" (list :id "t1" :rootFrom "lead-x" :started (- now 5000)
                                                    :at (- now 2000)
                                                    :lanes (list (list :name "lead-x" :kind "claude"))
                                                    :msgs (list (list :kind "ask" :from "lead-x" :to "rd"
                                                                      :at (- now 3000) :head "h" :text "full body"))
                                                    :waiting nil))
                               ("herd_link" (list :link (list :session "lead-x" :last "the whole report\n\nwith more"))))))))
      (ghostherd-memory-page--handle
       (list (cons 'op "inbox-detail") (cons 'id (format "report:lead-x:%d" (truncate now)))
             (cons 'kind "report") (cons 'agent "lead-x") (cons 'ask "")))
      (should (member '("herd_thread" :agent "lead-x") asked))
      (should (member '("herd_link" :session "lead-x") asked))
      (let ((rep (seq-find (lambda (p) (and (equal (car p) "inboxDetailData")
                                            (plist-get (cdr p) :report))) pushes))
            (thr (seq-find (lambda (p) (and (equal (car p) "inboxDetailData")
                                            (plist-get (cdr p) :thread))) pushes)))
        (should (equal (plist-get (cdr rep) :report) "the whole report\n\nwith more"))
        (should (string-prefix-p "report:lead-x:" (plist-get (cdr rep) :for)))
        (should (vectorp (plist-get (plist-get (cdr thr) :thread) :msgs)))))
    (setq asked nil pushes nil ghostherd-inbox--detail-req nil)
    (cl-letf (((symbol-function 'ghostherd-memory-page--js) (lambda (fn obj) (push (cons fn obj) pushes)))
              ((symbol-function 'ghostherd-memory-request-async)
               (lambda (method ok params &optional _err)
                 (push (cons method params) asked)
                 (when (equal method "herd_thread")
                   (funcall ok (list :id "t2" :rootFrom "rd" :started 1 :at 2 :lanes nil :msgs nil :waiting nil))))))
      (ghostherd-memory-page--handle
       (list (cons 'op "inbox-detail") (cons 'id "ask-failed:1a2b3c4d")
             (cons 'kind "ask-failed") (cons 'agent "rd") (cons 'ask "1a2b3c4d")))
      (should (member '("herd_thread" :ask "1a2b3c4d") asked))
      (should-not (seq-find (lambda (a) (equal (car a) "herd_link")) asked)))))

;;; Images a reply names

(defconst ghostherd-test--image-id "0123456789abcdef0123456789abcdef")

(ert-deftest ghostherd-test-images-reach-the-page-checked ()
  "The images the sidecar says a message names reach the page field by
field: only a copy's id and extension, none of what the reply said
besides -- in a room's columns and its chat, an entry's own and the
answer an ask of it got."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let* ((id ghostherd-test--image-id)
         (good (list :id id :ext "png" :name "after.png" :w 1200 :h 800 :thumb t :src "/tmp/after.png"))
         (sent (list good
                     (list :id "../../etc/passwd" :ext "png" :name "x")
                     (list :id (upcase id) :ext "png")
                     (list :id id :ext "svg")
                     (list :id (concat id "0") :ext "png")
                     "not an image"
                     (list :id id :ext "jpg" :w -1 :h "9" :thumb nil)
                     ;; An HTML report: its title, none of the rest.
                     (list :id id :ext "html" :name "report.html" :title "The viewer" :w 0 :h 0
                           :src "/tmp/report.html")
                     (list :id id :ext "htm" :name "x")))
         (want (vector (list :id id :ext "png" :name "after.png" :w 1200 :h 800 :thumb t)
                       (list :id id :ext "jpg" :name "image" :w nil :h nil :thumb :json-false)
                       (list :id id :ext "html" :name "report.html" :title "The viewer"))))
    (should (equal (ghostherd-memory-page--images sent) want))
    (should-not (ghostherd-memory-page--images nil))
    (should-not (ghostherd-memory-page--images "x"))
    (should-not (ghostherd-memory-page--images (list (list :id "x" :ext "png"))))
    (let ((e (ghostherd-memory-page--room-entry
              (list :role "herd" :kind "ask" :text "Take one" :answer "Here" :images sent :answer_images sent))))
      (should (equal (plist-get e :images) want))
      (should (equal (plist-get e :answerImages) want)))
    (let ((e (ghostherd-memory-page--chat-entry (list :who "rd" :kind "turn" :text "Done" :images sent))))
      (should (equal (plist-get e :images) want))
      (should-not (plist-get e :answerImages)))))

(ert-deftest ghostherd-test-images-served-by-their-names-alone ()
  "The herd page is served the copies in `ghostherd-reply-images-directory' by
their names -- an id, and the copy's extension or its thumbnail's -- and
nothing else there: not the index, not another name, not a symlink."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let* ((dir (file-name-as-directory (make-temp-file "ghostherd-images" t)))
         (ghostherd-reply-images-directory dir)
         (id ghostherd-test--image-id))
    (unwind-protect
        (progn
          (dolist (f (list (concat id ".png") (concat id ".t.jpg") (concat id ".webp")
                           "index.sqlite" (concat id ".txt") (concat id ".t.png") "0123.png"))
            (with-temp-file (expand-file-name f dir) (insert f)))
          (make-symbolic-link (expand-file-name "index.sqlite" dir)
                              (expand-file-name (concat (make-string 32 ?a) ".png") dir))
          (should (equal (xwapp--files-dir ghostherd-memory-page--app) dir))
          (dolist (f (list (concat id ".png") (concat id ".t.jpg") (concat id ".webp")))
            (should (equal (xwapp--file-of ghostherd-memory-page--app f) (expand-file-name f dir))))
          (dolist (f (list "index.sqlite" (concat id ".txt") (concat id ".t.png") "0123.png"
                           (concat (make-string 32 ?a) ".png") (concat id ".PNG")
                           (concat "../" id ".png") (concat (make-string 32 ?f) ".png")))
            (should-not (xwapp--file-of ghostherd-memory-page--app f)))
          ;; Not set: a directory of their own beside the images you paste,
          ;; out of the way of their pruning (top-level files only).
          (let ((ghostherd-reply-images-directory nil)
                (ghostherd-image-directory "/home/me/.emacs.d/emacs-home/ghostherd-images/"))
            (should (equal (ghostherd-memory--reply-images-directory)
                           "/home/me/.emacs.d/emacs-home/ghostherd-images/replies/"))))
      (delete-directory dir t))))

(defmacro ghostherd-test--docs (&rest body)
  "BODY with ghostherd's docs/ in `own', yours in `mine', the copies
replies name in `kept': each with what is and is not to be served."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "ghostherd-docs" t)))
          (own (file-name-as-directory (expand-file-name "ghostherd/docs" root)))
          (mine (file-name-as-directory (expand-file-name "my notes" root)))
          (kept (file-name-as-directory (expand-file-name "replies" root)))
          (ghostherd-docs-directories (list mine (expand-file-name "missing" root)))
          (ghostherd-reply-images-directory kept)
          (id ghostherd-test--image-id)
          (put (lambda (dir name text &optional age)
                 (make-directory dir t)
                 (let ((file (expand-file-name name dir)))
                   (with-temp-file file (insert text))
                   (when age (set-file-times file (time-subtract nil age)))
                   file))))
     (unwind-protect
         (cl-letf (((symbol-function 'ghostherd-memory-page--own-docs) (lambda () own)))
           (funcall put own "phone-access.html" "<!DOCTYPE html><html><head><title>Herd 頁面門禁</title>" 3000)
           (funcall put own "sidecar.html" "<html><title lang=en>\n  Sidecar &amp; its\n memory &#x1F4BE; </title>" 2000)
           (funcall put own "notes.txt" "not a doc")
           (funcall put own "no-title.html" "<p>hi</p>" 4000)
           (funcall put mine "sidecar.html" "<title>Mine, under ghostherd's name</title>")
           (funcall put mine "mine.html" "<title>Mine</title><script>alert(1)</script>" 10)
           (funcall put mine "Upper.HTML" "<title>Not .html to the letter</title>")
           (funcall put root "outside.html" "<title>Outside</title>")
           (make-symbolic-link (expand-file-name "outside.html" root) (expand-file-name "link.html" mine))
           (funcall put kept (concat id ".html") "<!doctype html><title>A report</title>")
           (funcall put kept (concat id ".png") "PNG")
           (ghostherd-memory-page--serve-files)
           ,@body)
       (delete-directory root t))))

(ert-deftest ghostherd-test-docs-listed-by-title ()
  "The Docs tab lists ghostherd's docs and yours, newest first, each by
its <title> -- its references read, on one line -- or its name; a name
in both is ghostherd's; nothing that is not served: not .html to the
letter, a symlink, another file."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (ghostherd-test--docs
    (let ((docs (ghostherd-memory-page--docs)))
      (should (equal (mapcar (lambda (d) (plist-get d :name)) docs)
                     '("mine.html" "sidecar.html" "phone-access.html" "no-title.html")))
      (should (equal (mapcar (lambda (d) (plist-get d :title)) docs)
                     '("Mine" "Sidecar & its memory 💾" "Herd 頁面門禁" "no-title")))
      (should (equal (mapcar (lambda (d) (plist-get d :where)) docs)
                     (list (abbreviate-file-name mine) "ghostherd" "ghostherd" "ghostherd")))
      (should (equal (plist-get (car docs) :bytes) (length "<title>Mine</title><script>alert(1)</script>")))
      (should (numberp (plist-get (car docs) :mtime))))
    ;; What the page gets: the Docs at once, the reports when the sidecar answers.
    (let (sent asked)
      (cl-letf (((symbol-function 'ghostherd-memory-page--js) (lambda (fn obj) (push (cons fn obj) sent)))
                ((symbol-function 'ghostherd-memory-request-async)
                 (lambda (method &rest _) (push method asked))))
        (ghostherd-memory-page--handle '((op . "docs"))))
      (should (equal (mapcar #'car sent) '("renderDocs")))
      (should (= (length (plist-get (cdar sent) :docs)) 4))
      (should (equal asked '("herd_reports"))))))

(ert-deftest ghostherd-test-docs-reports-from-the-sidecar ()
  "The Docs tab's reports are the sidecar's (`herd_reports'), asked when
the tab is shown and never waited on: the newest page, then each older
one from the last shown, which the page adds them after.  Field by
field: one whose id is not a copy's is dropped, a gone copy says so.
A report's older copies -- its file rewritten and named again -- come
with it, each likewise.  A sidecar that cannot answer says why; an
older page asked from what is not a report's place is not asked at all."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let* ((id ghostherd-test--image-id)
         (id2 (make-string 32 ?a))
         (answer (list :reports (list (list :id id :title "A report" :name "r.html" :agent "rd"
                                            :project "x" :at 1700000000.25 :bytes 120 :gone nil
                                            :older (list (list :id id2 :title "A draft" :name "r.html"
                                                               :agent "rd" :project "x" :at 1699999000.5
                                                               :bytes 90 :gone nil :older (list (list :id id)))
                                                         (list :id "../draft" :at 1.0)))
                                      (list :id "../index" :at 1.0)
                                      (list :id (upcase id) :at 1.0)
                                      (list :id id2 :title nil :at 1700000000.0 :bytes -1 :gone t :agent 3))
                       :more t))
         asked sent)
    (cl-letf (((symbol-function 'ghostherd-memory-request-async)
               (lambda (method cb &optional params err)
                 (push (list method params) asked)
                 (if (eq answer 'fail) (funcall err "refused") (funcall cb answer))))
              ((symbol-function 'ghostherd-memory-page--js) (lambda (fn obj) (push (cons fn obj) sent)))
              ((symbol-function 'ghostherd-memory-page--send-docs) #'ignore))
      (ghostherd-memory-page--handle '((op . "docs")))
      (should (equal (car asked) '("herd_reports" (:limit 30))))
      (should (equal (car sent)
                     `("renderReports"
                       :reports [(:id ,id :title "A report" :name "r.html" :agent "rd" :project "x"
                                  :at 1700000000.25 :bytes 120 :gone :json-false
                                  ;; Its older copies, each likewise -- none of their own.
                                  :older [(:id ,id2 :title "A draft" :name "r.html" :agent "rd" :project "x"
                                           :at 1699999000.5 :bytes 90 :gone :json-false)])
                                 (:id ,id2 :title "" :name ,(concat id2 ".html") :agent "" :project ""
                                  :at 1700000000.0 :bytes 0 :gone t :older [])]
                       :more t :after :json-false)))
      ;; The page older than the last shown.
      (ghostherd-memory-page--handle `((op . "reports-more") (at . 1761469012.1234567) (id . ,id2)))
      (should (equal (car asked) `("herd_reports" (:limit 30 :before (:at 1761469012.1234567 :id ,id2)))))
      (should (equal (plist-get (cdar sent) :after) id2))
      ;; As it goes to the sidecar: the place exactly as the page had it.
      (let ((req (json-parse-string (ghostherd-memory--request-body "herd_reports" (cadar asked))
                                    :object-type 'plist)))
        (should (eql (plist-get (plist-get (plist-get req :params) :before) :at) 1761469012.1234567)))
      ;; Not a report's place: nothing asked.
      (let ((n (length asked)))
        (dolist (bad `(((at . "1") (id . ,id)) ((at . 1.0) (id . "../index")) ((id . ,id))
                       ((at . 1.0) (id . ,(upcase id))) ((at . 1.0))))
          (ghostherd-memory-page--handle (cons '(op . "reports-more") bad)))
        (should (= (length asked) n)))
      ;; The sidecar away.
      (setq answer 'fail)
      (ghostherd-memory-page--handle '((op . "docs")))
      (should (equal (car sent) '("renderReports" :error "refused" :after :json-false))))))

(ert-deftest ghostherd-test-docs-and-reports-served-sandboxed ()
  "Each set the herd page is served: the Docs by their exact names from
the folders listed, the reports a reply named by their ids -- both
sandboxed, never cached for a doc -- and a report never through the
images' set, which has no sandbox; the images as before.  Nothing past
the cap, nor anything outside."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (ghostherd-test--docs
    (let ((app ghostherd-memory-page--app))
      (should (plist-get (xwapp--set app "docs") :sandbox))
      (should (plist-get (xwapp--set app "docs") :exact))
      (should (plist-get (xwapp--set app "docs") :fresh))
      (should (plist-get (xwapp--set app "reports") :sandbox))
      (should-not (plist-get (xwapp--set app) :sandbox))
      (should (equal (xwapp--files-dirs app "docs") (list own mine (expand-file-name "missing/" root))))
      (should (equal (xwapp--file-of app "sidecar.html" "docs") (expand-file-name "sidecar.html" own)))
      (should (equal (xwapp--file-of app "mine.html" "docs") (expand-file-name "mine.html" mine)))
      (dolist (name '("notes.txt" "Upper.HTML" "upper.html" "link.html" "outside.html" "../outside.html"
                      "Mine.html" "MINE.HTML" "mine.htm" ".html"))
        (should-not (xwapp--file-of app name "docs")))
      ;; A report: by its id, in the reports' set alone.
      (let ((report (concat id ".html")))
        (should (equal (xwapp--file-of app report "reports") (expand-file-name report kept)))
        (should-not (xwapp--file-of app report))                       ; not the images' set
        (should-not (xwapp--file-of app report "docs"))                ; nor a doc
        (dolist (name (list (upcase report) (concat id ".HTML") (concat id ".png")
                            (concat (make-string 32 ?f) ".html") "index.sqlite"))
          (should-not (xwapp--file-of app name "reports"))))
      (should (equal (xwapp--file-of app (concat id ".png")) (expand-file-name (concat id ".png") kept)))
      ;; Past the cap.
      (let ((ghostherd-memory-page--doc-max-bytes 10))
        (ghostherd-memory-page--serve-files)
        (should-not (xwapp--file-of app "mine.html" "docs"))
        (should-not (xwapp--file-of app (concat id ".html") "reports")))
      (ghostherd-memory-page--serve-files))))

(ert-deftest ghostherd-test-docs-open-in-a-view-of-their-own ()
  "From the xwidget's page, a doc or a report opens in a view of Emacs's
own (`xwapp-show-file'), named by its set and its name; another set is
not asked for, and a name not served says so on the page."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (ghostherd-test--docs
    (let (shown errors)
      (cl-letf (((symbol-function 'xwapp-show-file)
                 (lambda (_app name set) (push (list set name) shown)))
                ((symbol-function 'ghostherd-memory-page--js)
                 (lambda (fn obj) (when (equal fn "showError") (push obj errors)))))
        (ghostherd-memory-page--handle '((op . "doc-open") (set . "docs") (name . "mine.html")))
        (ghostherd-memory-page--handle `((op . "doc-open") (set . "reports") (name . ,(concat id ".html"))))
        (ghostherd-memory-page--handle '((op . "doc-open") (set . "files") (name . "x.png")))
        (ghostherd-memory-page--handle '((op . "doc-open") (set . "docs") (name . 3))))
      (should (equal (reverse shown) `(("docs" "mine.html") ("reports" ,(concat id ".html")))))
      (should-not errors)
      ;; The real one refuses what is not served, on the page.
      (cl-letf (((symbol-function 'ghostherd-memory-page--js)
                 (lambda (fn obj) (when (equal fn "showError") (push obj errors)))))
        (ghostherd-memory-page--handle '((op . "doc-open") (set . "docs") (name . "link.html"))))
      (should (string-match-p "not served" (car errors))))))

(ert-deftest ghostherd-test-own-docs-from-the-checkout ()
  "ghostherd's docs/ is the checkout's, beside the .el straight's build
links to -- the build has none."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let* ((root (file-name-as-directory (make-temp-file "ghostherd-build" t)))
         (checkout (expand-file-name "local/ghostherd/" root))
         (build (expand-file-name "straight/build/ghostherd/" root)))
    (unwind-protect
        (progn
          (make-directory checkout t)
          (make-directory build t)
          (with-temp-file (expand-file-name "ghostherd-memory-page.el" checkout) (insert ";;"))
          (make-symbolic-link (expand-file-name "ghostherd-memory-page.el" checkout)
                              (expand-file-name "ghostherd-memory-page.el" build))
          (let ((ghostherd-memory-page--dir build))
            (should (equal (ghostherd-memory-page--own-docs)
                           (expand-file-name "docs/" (file-truename checkout)))))
          (let ((ghostherd-memory-page--dir checkout))
            (should (equal (ghostherd-memory-page--own-docs) (expand-file-name "docs/" (file-truename checkout))))))
      (delete-directory root t))))

(ert-deftest ghostherd-test-images-the-sidecar-is-told-where ()
  "The sidecar starts knowing where to copy images, how long to keep
them and how much room they may take."
  (let ((ghostherd-reply-images-directory "/tmp/gh-images/")
        (ghostherd-reply-images-keep-days 10)
        (ghostherd-reply-images-budget 64)
        (ghostherd-worktree-root "/tmp/gh-worktrees")
        env)
    (cl-letf (((symbol-function 'start-process)
               (lambda (&rest _) (setq env process-environment) nil))
              ((symbol-function 'set-process-query-on-exit-flag) #'ignore))
      (let ((ghostherd-memory--process nil)
            (ghostherd-memory--port nil))
        (ghostherd-memory--spawn temporary-file-directory 49999)))
    (should (member "GHOSTHERD_IMAGES_DIR=/tmp/gh-images/" env))
    (should (member "GHOSTHERD_IMAGES_KEEP_DAYS=10" env))
    (should (member "GHOSTHERD_IMAGES_BUDGET_MB=64" env))
    ;; And where agents' worktrees are, which `herd search' counts as theirs.
    (should (member "GHOSTHERD_WORKTREE_ROOT=/tmp/gh-worktrees" env))
    (when (get-buffer "*ghostherd-memory*") (kill-buffer "*ghostherd-memory*"))))

(ert-deftest ghostherd-test-images-of-a-report-in-the-inbox ()
  "A report picked in the Inbox brings the images its words name with
them, in the same push -- checked; and a thread's answer its own."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let* (pushes
         (ghostherd--log nil)
         (ghostherd-inbox--detail-req nil)
         (id ghostherd-test--image-id)
         (img (list :id id :ext "png" :name "a.png" :w 2 :h 1 :thumb t))
         (now (* 1000 (float-time))))
    (cl-letf (((symbol-function 'ghostherd-memory-page--js) (lambda (fn obj) (push (cons fn obj) pushes)))
              ((symbol-function 'ghostherd-memory-request-async)
               (lambda (method ok _params &optional _err)
                 (funcall ok (pcase method
                               ("herd_thread" (list :id "t1" :rootFrom "lead-x" :started (- now 5000) :at now
                                                    :lanes nil :waiting nil
                                                    :msgs (list (list :kind "answer" :from "rd" :to "lead-x"
                                                                      :at (- now 1000) :text "see a.png"
                                                                      :images (list img)))))
                               ("herd_link" (list :link (list :session "lead-x" :last "Shipped: /tmp/a.png"
                                                              :last_images (list img (list :id "x" :ext "png"))))))))))
      (ghostherd-memory-page--handle
       (list (cons 'op "inbox-detail") (cons 'id (format "report:lead-x:%d" (truncate now)))
             (cons 'kind "report") (cons 'agent "lead-x") (cons 'ask "")))
      (let ((rep (seq-find (lambda (p) (plist-get (cdr p) :report)) pushes)))
        (should (equal (plist-get (cdr rep) :report) "Shipped: /tmp/a.png"))
        (should (equal (plist-get (cdr rep) :images)
                       (vector (list :id id :ext "png" :name "a.png" :w 2 :h 1 :thumb t)))))
      (let* ((thr (seq-find (lambda (p) (plist-get (cdr p) :thread)) pushes))
             (msg (seq-find (lambda (m) (plist-get m :images))
                            (plist-get (plist-get (cdr thr) :thread) :msgs))))
        (should (vectorp (plist-get msg :images)))
        (should (eq (plist-get (aref (plist-get msg :images) 0) :thumb) t))))))

(provide 'ghostherd-memory-page-tests)
;;; ghostherd-memory-page-tests.el ends here

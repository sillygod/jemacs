;;; ghostherd-board-tests.el --- Tests for the herd page's Board tab  -*- lexical-binding: t; -*-

;;; Commentary:

;; Part of ghostherd's tests; ghostherd-tests.el runs them all.  This
;; file alone:
;;
;;   emacs --batch --init-directory=~/.emacs.d/emacs-home/ \
;;         -L . -L ../xwapp -l ghostherd-board-tests.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; The Board's Emacs half (ghostherd-memory-page.el, "The Board tab"): the
;; sidecar's items with what the forge says of their pull requests and
;; what waits on the user added, sent in parts -- the whole once, then the
;; cards that changed.  The sidecar's half is server/tests/test_board.py.

;;; Code:

(require 'ghostherd-test-helpers)

(defun ghostherd-board-test--card (id status &rest props)
  "A card as the sidecar sends one; PROPS, first, win."
  (append props (list :id id :status status :title (concat "work " id) :named nil :labels nil
                :asks 2 :open (if (equal status "live") 1 0) :last 1000 :agents (list "lead-a" (concat "rd-" id))
                :projects '("a") :prs nil :links nil
                :block (list :kind "answered" :head "done"))))

(defun ghostherd-board-test--reply (&rest cards)
  (list :items cards :waits (list (list :ask "aaaa0001" :from "lead-a" :to "rd-1" :fromProj "a" :toProj "a"
                                        :kind "ask" :status "delivered" :working t :head "go"))
        :edges nil :counts (list :asks 3 :xproj 4 :fyi 0 :relays 0 :failed 0) :days 1 :more nil))

(defmacro ghostherd-board-test--with (&rest body)
  "BODY with the page recorded (`ghostherd-test--with-page'), no Inbox, no
forge, the board's own state fresh."
  (declare (indent 0))
  `(ghostherd-test--with-page
     (let ((ghostherd-board--watching nil) (ghostherd-board--timer nil) (ghostherd-board--days 1)
           (ghostherd-board--v 0) (ghostherd-board--sent nil) (ghostherd-board--inflight nil)
           (ghostherd-memory-page--pr-briefs (make-hash-table :test 'equal))
           (ghostherd-memory-page--chores nil) (inbox nil))
       (cl-letf (((symbol-function 'xwapp-live-p) (lambda (&rest _) t))
                 ((symbol-function 'run-at-time) (lambda (&rest _) 'a-timer))
                 ((symbol-function 'timerp) (lambda (x) (eq x 'a-timer)))
                 ((symbol-function 'cancel-timer) #'ignore)
                 ((symbol-function 'ghostherd-memory-page--chore) #'ignore)
                 ((symbol-function 'ghostherd-inbox--all) (lambda (_p) inbox)))
         ,@body))))

(ert-deftest ghostherd-test-board-whole-then-only-what-changed ()
  "The tab watched: the whole board, as arrays the page reads; a tick
with nothing new sends nothing; a card that changed goes alone, a card
gone as its id, the open asks only when they changed."
  (ghostherd-board-test--with
    (let ((board (ghostherd-board-test--reply (ghostherd-board-test--card "w1" "live")
                                              (ghostherd-board-test--card "w2" "quiet"))))
      (setq reply (lambda (m _p) (pcase m ("herd_board" board) ("herd_inbox_state" '(:state nil)))))
      (ghostherd-memory-page--handle '((op . "board-watch") (days . 7)))
      (should (equal (car (car requests)) "herd_board"))
      (should (equal (cadr (car requests)) '(:days 7)))
      (let* ((p (ghostherd-test--call calls "boardPatch"))
             (json (json-read-from-string (json-encode p))))
        (should (eq (plist-get p :full) t))
        (should (equal (alist-get 'base json) 0))
        (should (equal (alist-get 'v json) 1))
        (should (= (length (alist-get 'cards json)) 2))
        (should (vectorp (alist-get 'agents (aref (alist-get 'cards json) 0))))
        (should (equal (alist-get 'prs (aref (alist-get 'cards json) 0)) []))
        (should (equal (alist-get 'jing json) []))
        (should (eq (alist-get 'working (aref (alist-get 'waits json) 0)) t))
        (should (equal (alist-get 'asks (alist-get 'counts (alist-get 'meta json))) 3))
        ;; A count, not a card's flag of the same name.
        (should (equal (alist-get 'xproj (alist-get 'counts (alist-get 'meta json))) 4)))
      ;; Nothing new: nothing sent.
      (setq calls nil)
      (ghostherd-board--tick)
      (should-not (ghostherd-test--call calls "boardPatch"))
      ;; One card changed, one gone.
      (setq board (ghostherd-board-test--reply (ghostherd-board-test--card "w1" "live" :asks 3)))
      (ghostherd-board--tick)
      (let ((p (ghostherd-test--call calls "boardPatch")))
        (should (equal (plist-get p :base) 1))
        (should (equal (plist-get p :v) 2))
        (should (eq (plist-get p :full) :json-false))
        (should (equal (mapcar (lambda (c) (plist-get c :id)) (append (plist-get p :cards) nil)) '("w1")))
        (should (equal (plist-get p :drop) ["w2"]))
        (should-not (plist-member p :waits))
        (should-not (plist-member p :meta)))
      ;; The page missed one: the whole again.
      (setq calls nil)
      (ghostherd-memory-page--handle '((op . "board-resend")))
      (should (eq (plist-get (ghostherd-test--call calls "boardPatch") :full) t))
      (ghostherd-memory-page--handle '((op . "board-unwatch")))
      (should-not ghostherd-board--watching))))

(ert-deftest ghostherd-test-board-pr-states-tail-and-title ()
  "A card's PRs carry what the forge said, the look a chore; a quiet item
with a PR of its own still open is `tail'; one with no name a lead gave
takes its first own PR's title; a failed look says it is unsure."
  (ghostherd-board-test--with
    (let* ((ref (lambda (id own) (list :host "bitbucket" :owner "acme" :repo "shop" :id id :own own :of "w1")))
           (board (ghostherd-board-test--reply
                   (ghostherd-board-test--card "w1" "quiet" :prs (list (funcall ref 7 t) (funcall ref 8 nil)))
                   (ghostherd-board-test--card "w2" "quiet" :named t :title "Named" :prs (list (funcall ref 9 t)))))
           (chores nil))
      (puthash '("bitbucket" "acme" "shop" 7) (list :at (float-time) :pr '((state . "OPEN") (title . "cart: hold stock")
                                                                            (destination . "production") (url . "https://x/7")))
               ghostherd-memory-page--pr-briefs)
      (puthash '("bitbucket" "acme" "shop" 9) (list :at (float-time) :pr '((state . "MERGED") (title . "t9")) :miss "HTTP 500")
               ghostherd-memory-page--pr-briefs)
      (setq reply (lambda (m _p) (pcase m ("herd_board" board) (_ nil))))
      (cl-letf (((symbol-function 'ghostherd-memory-page--chore) (lambda (fn &rest args) (push (cons fn args) chores))))
        (ghostherd-memory-page--handle '((op . "board-watch"))))
      (should (= 3 (length chores)))
      (should (eq (car (car chores)) 'ghostherd-memory-page--pr-brief-ask))
      (let* ((cards (append (plist-get (ghostherd-test--call calls "boardPatch") :cards) nil))
             (w1 (seq-find (lambda (c) (equal (plist-get c :id) "w1")) cards))
             (w2 (seq-find (lambda (c) (equal (plist-get c :id) "w2")) cards))
             (p7 (aref (plist-get w1 :prs) 0)))
        (should (equal (plist-get w1 :status) "tail"))
        (should (equal (plist-get w1 :title) "cart: hold stock"))
        (should (equal (plist-get w1 :askTitle) "work w1"))
        (should (equal (list (plist-get p7 :state) (plist-get p7 :dest) (plist-get p7 :url))
                       '("OPEN" "production" "https://x/7")))
        (should (eq (plist-get (aref (plist-get w1 :prs) 1) :state) nil))
        (should (equal (plist-get w2 :status) "quiet"))
        (should (equal (plist-get w2 :title) "Named"))
        (should (eq (plist-get (aref (plist-get w2 :prs) 0) :unsure) t))))))

(ert-deftest ghostherd-test-board-waits-on-the-user-from-the-inbox ()
  "The Inbox's Needs-you items are the Board's waits on Jing: each put on
the item its agent was last in, marked a guess; that item, unless live,
is `jing'.  A dismissed one, an Update, and a memory proposal are not."
  (ghostherd-board-test--with
    (let ((board (ghostherd-board-test--reply (ghostherd-board-test--card "w1" "quiet" :last 100)
                                              (ghostherd-board-test--card "w3" "quiet" :last 300 :agents '("lead-a"))
                                              (ghostherd-board-test--card "w2" "live" :agents '("rd-w2")))))
      (setq inbox (list (list :id "question:lead-a:1" :kind "question" :agent "lead-a" :title "Which one?" :at 50 :project "/src/a/")
                        (list :id "blocked:rd-w2" :kind "blocked" :agent "rd-w2" :title "Permission" :at 60)
                        (list :id "guard:g1" :kind "guard" :agent "rd-w1" :title "Guard" :at 70)
                        (list :id "proposal:p1" :kind "proposal" :agent "" :title "Promote" :at 80)
                        (list :id "report:lead-a:2" :kind "report" :agent "lead-a" :title "Done" :at 90)))
      (setq reply (lambda (m _p) (pcase m ("herd_board" board)
                                   ("herd_inbox_state" '(:state ((:id "guard:g1" :done t)))))))
      (ghostherd-memory-page--handle '((op . "board-watch")))
      (should (equal (cadr (assoc "herd_inbox_state" requests))
                     '(:ids ["question:lead-a:1" "blocked:rd-w2" "guard:g1" "proposal:p1" "report:lead-a:2"])))
      (let* ((p (ghostherd-test--call calls "boardPatch"))
             (jing (append (plist-get p :jing) nil))
             (card (lambda (id) (seq-find (lambda (c) (equal (plist-get c :id) id)) (append (plist-get p :cards) nil)))))
        (should (equal (mapcar (lambda (j) (list (plist-get j :id) (plist-get j :item) (plist-get j :project))) jing)
                       '(("blocked:rd-w2" "w2" "") ("question:lead-a:1" "w3" "a"))))
        (should (eq (plist-get (car jing) :guess) t))
        (should (equal (plist-get (funcall card "w3") :status) "jing"))
        (should (equal (plist-get (funcall card "w3") :jingOf) ["question:lead-a:1"]))
        (should (equal (plist-get (funcall card "w1") :status) "quiet"))
        ;; Live stays live: the page tags it instead.
        (should (equal (plist-get (funcall card "w2") :status) "live"))
        (should (equal (plist-get (funcall card "w2") :jingOf) ["blocked:rd-w2"]))))))

(ert-deftest ghostherd-test-board-fails-soft-and-one-look-at-a-time ()
  "A sidecar without the Board says so on the page; the Inbox's state
failing leaves the waits on the user as they are; a look in flight is not
asked again until it answers or is abandoned."
  (ghostherd-board-test--with
    (setq fail "Method not found: herd_board")
    (ghostherd-memory-page--handle '((op . "board-watch")))
    (should (equal (ghostherd-test--call calls "boardError") '(:error "Method not found: herd_board")))
    (should-not ghostherd-board--inflight)
    (setq fail nil requests nil)
    (let (pending)
      (cl-letf (((symbol-function 'ghostherd-memory-request-async)
                 (lambda (method cb &optional params _err)
                   (setq requests (append requests (list (list method params))))
                   (push cb pending))))
        (ghostherd-board--tick)
        (ghostherd-board--tick)
        (should (= 1 (length requests)))
        (setq ghostherd-board--inflight (- (float-time) 1000))
        (ghostherd-board--tick)
        (should (= 2 (length requests)))))))

(ert-deftest ghostherd-test-board-timeline-on-asking ()
  "A card opened: its item whole, the timeline as an array."
  (ghostherd-board-test--with
    (setq reply (lambda (_m _p) (list :id "w1" :last 5 :events (list (list :kind "ask" :fold nil :relay nil)) :asksOf nil)))
    (ghostherd-memory-page--handle '((op . "board-timeline") (id . "w1")))
    (should (equal requests '(("herd_board_item" (:id "w1")))))
    (let ((tl (ghostherd-test--call calls "boardTimeline")))
      (should (equal (plist-get tl :id) "w1"))
      (should (vectorp (plist-get tl :events)))
      (should (equal (plist-get tl :asksOf) []))
      (should (eq (plist-get (aref (plist-get tl :events) 0) :relay) :json-false)))))

;;; Phase 2: the leads' lines, nobody's items, the Inbox's needs

(ert-deftest ghostherd-test-board-nobody-has-it ()
  "An item quiet past `ghostherd-board-orphan-hours' with a lead's need
open, or whose last ask went nowhere, is nobody's -- `orphan'; quiet less
long it waits on the user; a live one stays live, a closed one done."
  (ghostherd-board-test--with
    (let* ((now (* 1000 (float-time)))
           (old (- now (* 5 3600 1000.0)))
           (need (list (list :id 7 :kind "need" :text "pick one" :agent "lead-a" :at old)))
           (board (ghostherd-board-test--reply
                   (ghostherd-board-test--card "w1" "quiet" :last old :needs need)
                   (ghostherd-board-test--card "w2" "quiet" :last (- now 60000) :needs need)
                   (ghostherd-board-test--card "w3" "quiet" :last old :unanswered (list :ask "a1" :status "cancelled"))
                   (ghostherd-board-test--card "w4" "live" :last old :needs need)
                   (ghostherd-board-test--card "w5" "done" :last old :needs need)
                   (ghostherd-board-test--card "w6" "quiet" :last old))))
      (setq reply (lambda (m _p) (pcase m ("herd_board" board) (_ nil))))
      (ghostherd-memory-page--handle '((op . "board-watch")))
      (let ((st (mapcar (lambda (c) (cons (plist-get c :id) (plist-get c :status)))
                        (append (plist-get (ghostherd-test--call calls "boardPatch") :cards) nil))))
        (should (equal (sort st (lambda (a b) (string< (car a) (car b))))
                       '(("w1" . "orphan") ("w2" . "jing") ("w3" . "orphan") ("w4" . "live") ("w5" . "done") ("w6" . "quiet")))))
      ;; Its hours are the user's.
      (let ((ghostherd-board-orphan-hours 24))
        (setq calls nil)
        (ghostherd-memory-page--handle '((op . "board-resend")))
        (should (equal (plist-get (seq-find (lambda (c) (equal (plist-get c :id) "w1"))
                                            (append (plist-get (ghostherd-test--call calls "boardPatch") :cards) nil))
                                  :status)
                       "jing"))))))

(ert-deftest ghostherd-test-board-leads-lines-before-the-inbox ()
  "What waits on the user: the leads' need lines first, newest first --
those under no item too -- each `need:N' for its event; the Inbox's after."
  (ghostherd-board-test--with
    (let ((board (append (ghostherd-board-test--reply
                          (ghostherd-board-test--card "w1" "quiet" :needs (list (list :id 3 :text "run it" :agent "lead-a" :at 200)))
                          (ghostherd-board-test--card "w2" "quiet" :needs (list (list :id 4 :text "approve" :agent "lead-a" :at 300))))
                         (list :needs (list (list :id 5 :text "which region" :agent "lead-c" :at 100))))))
      (setq inbox (list (list :id "question:lead-a:1" :kind "question" :agent "lead-a" :title "Which?" :at 400)))
      (setq reply (lambda (m _p) (pcase m ("herd_board" board) (_ nil))))
      (ghostherd-memory-page--handle '((op . "board-watch")))
      (let ((jing (append (plist-get (ghostherd-test--call calls "boardPatch") :jing) nil)))
        (should (equal (mapcar (lambda (j) (list (plist-get j :id) (plist-get j :item) (plist-get j :line))) jing)
                       '(("need:4" "w2" t) ("need:3" "w1" t) ("need:5" "" t) ("question:lead-a:1" "w1" nil))))
        (should (equal (plist-get (car jing) :event) 4))
        (should (equal (plist-get (car jing) :title) "approve"))))))

(ert-deftest ghostherd-test-board-a-need-handled ()
  "Handled on the Board, or Done on its Inbox item: the sidecar closes the
need, and the board is looked at again."
  (ghostherd-board-test--with
    (setq reply (lambda (m _p) (pcase m ("herd_board" (ghostherd-board-test--reply)) (_ nil))))
    (ghostherd-memory-page--handle '((op . "board-watch")))
    (setq requests nil)
    (ghostherd-memory-page--handle '((op . "board-need-done") (id . 9)))
    (should (equal (car requests) '("herd_board_need_close" (:id 9))))
    (should (equal (car (cadr requests)) "herd_board"))
    (setq requests nil)
    (ghostherd-memory-page--handle '((op . "inbox-set") (id . "need:12") (done . t)))
    (should (member '("herd_board_need_close" (:id 12)) requests))
    (setq requests nil)
    ;; Another item done: no need closed.
    (ghostherd-memory-page--handle '((op . "inbox-set") (id . "report:lead-a:3") (done . t)))
    (should-not (assoc "herd_board_need_close" requests))))

(ert-deftest ghostherd-test-inbox-has-the-leads-needs ()
  "The Inbox lists each need the leads' lines say waits on the user, from
the sidecar; a sidecar without them leaves the rest as it was."
  (ghostherd-test--with-page
    (let ((ghostherd-inbox--watching t) (ghostherd-inbox--sig nil))
      (cl-letf (((symbol-function 'xwapp-live-p) (lambda (&rest _) t))
                ((symbol-function 'ghostherd-sessions) (lambda () nil))
                ((symbol-function 'ghostherd--handoff-stops) (lambda () nil)))
        (setq reply (lambda (m _p)
                      (pcase m
                        ("herd_needs" '(:needs ((:id 7 :text "run rotate.sh" :agent "lead-infra" :at 1000
                                                     :item "w1" :itemTitle "Rotate the certs"))))
                        (_ nil))))
        (ghostherd-inbox--tick)
        (let* ((items (append (plist-get (ghostherd-test--call calls "renderInbox") :items) nil))
               (it (car items)))
          (should (equal (plist-get it :id) "need:7"))
          (should (equal (plist-get it :kind) "need"))
          (should (equal (plist-get it :group) "need"))
          (should (equal (plist-get it :event) 7))
          (should (string-search "Rotate the certs" (plist-get it :body)))
          (should (= 1 (plist-get (ghostherd-test--call calls "renderInbox") :count))))
        (setq fail "Method not found: herd_needs" calls nil ghostherd-inbox--sig nil)
        (ghostherd-inbox--tick)
        (should (equal (plist-get (ghostherd-test--call calls "renderInbox") :items) []))))))

;;; What a push is compared by: the value, not `sxhash-equal'

(ert-deftest ghostherd-test-inbox-tenth-items-change-is-pushed ()
  "The Inbox pushes again when only its tenth item's preview changed:
`sxhash-equal' took a list's rest a level deeper past its seventh element
and stopped three levels down, so the hash stayed and the page did not."
  (ghostherd-test--with-page
    (let ((ghostherd-inbox--sig nil)
          (items (lambda (p) (mapcar (lambda (i) (list :id (format "report:a%d:1" i) :kind "report" :group "fyi"
                                                       :preview (if (= i 9) p "same")))
                                     (number-sequence 0 9)))))
      (ghostherd-inbox--push (funcall items "first") nil)
      (ghostherd-inbox--push (funcall items "first") nil)
      (should (= 1 (length (seq-filter (lambda (c) (equal (car c) "renderInbox")) calls))))
      (ghostherd-inbox--push (funcall items "second") nil)
      (should (= 2 (length (seq-filter (lambda (c) (equal (car c) "renderInbox")) calls)))))))

(ert-deftest ghostherd-test-threads-a-threads-own-change-is-pushed ()
  "The Threads list pushes again when only one thread's message count
changed -- a key past its plist's seventh element, three levels down,
where `sxhash-equal' stopped looking.  The thread as threads.py `summary'
orders its keys."
  (ghostherd-test--with-page
    (let ((ghostherd-threads--sig nil)
          (list-of (lambda (n) (list :threads (list (list :id "t1" :title "x" :status "live" :participants '("a" "b")
                                                           :projects '("p") :cross nil :count n :open 1 :started 1 :at 2
                                                           :rootFrom "a" :rootTo "b"))
                                     :talking nil :live 1 :more nil :projects '("p") :view nil :costs nil))))
      (cl-letf (((symbol-function 'ghostherd-threads--prompts) (lambda () (make-hash-table :test #'equal))))
        (ghostherd-threads--push-list (funcall list-of 2))
        (ghostherd-threads--push-list (funcall list-of 2))
        (should (= 1 (length (seq-filter (lambda (c) (equal (car c) "renderThreads")) calls))))
        (ghostherd-threads--push-list (funcall list-of 3))
        (should (= 2 (length (seq-filter (lambda (c) (equal (car c) "renderThreads")) calls))))))))

(ert-deftest ghostherd-test-memory-eighth-facts-change-is-pushed ()
  "The Memory tab pushes again when only its eighth fact changed."
  (ghostherd-test--with-page
    (let ((ghostherd-memory-page--memory-key "user") (ghostherd-memory-page--memory-sig nil) (body "one"))
      (cl-letf (((symbol-function 'xwapp-live-p) (lambda (&rest _) t))
                ((symbol-function 'ghostherd-memory-page--memory-proposals-tick) #'ignore)
                ((symbol-function 'ghostherd-memory-page--memory-checks-tick) #'ignore))
        (setq reply (lambda (_m _p) (list :key "user"
                                          :facts (mapcar (lambda (i) (list :name (format "f%d" i) :body (if (= i 7) body "same")))
                                                         (number-sequence 0 9)))))
        (ghostherd-memory-page--memory-tick)
        (ghostherd-memory-page--memory-tick)
        (should (= 1 (length (seq-filter (lambda (c) (equal (car c) "renderMemory")) calls))))
        (setq body "two")
        (ghostherd-memory-page--memory-tick)
        (should (= 2 (length (seq-filter (lambda (c) (equal (car c) "renderMemory")) calls))))))))

(ert-deftest ghostherd-test-roles-teach-the-board ()
  "Every lead, and primary, is told how to keep the Board right: the item
an ask goes on with, a relay, closing, and the two fixed lines."
  (require 'ghostherd-primary)
  (dolist (words '("--item ID" "--relay-of\nASK" "item close ID" "需要 Jing：" "部署：" "需要 Jing：無"))
    (should (string-search (replace-regexp-in-string "\\\\n" "\n" words) ghostherd--lead-role)))
  (dolist (words '("--item ID" "--relay-of ASK" "需要 Jing：" "部署："))
    (should (string-search words ghostherd--primary-role))))

;;; Ways out of the Board: names to rooms, the badge with the tab closed

(ert-deftest ghostherd-test-board-knows-the-herd-and-each-way-to-answer ()
  "The patch names the herd's agents and the room each is under, as the
Agents tab has it, for the page's names to lead to rooms; a lead's need
says how to answer it, through its project."
  (ghostherd-board-test--with
    (let ((ghostherd--sessions (make-hash-table :test #'equal))
          (ghostherd--roles (make-hash-table :test #'equal)))
      (puthash "lead-a" (ghostherd-session--create :id "lead-a" :name "lead-a" :kind 'claude :state 'idle
                                                    :project "/src/a/" :backend 'tmux)
               ghostherd--sessions)
      (puthash "lead-a" "lead" ghostherd--roles)
      (puthash "rd-1" (ghostherd-session--create :id "rd-1" :name "rd-1" :kind 'claude :state 'working
                                                  :project "/src/a/" :backend 'tmux)
               ghostherd--sessions)
      (let ((board (ghostherd-board-test--reply
                    (ghostherd-board-test--card "w1" "quiet" :needs (list (list :id 4 :text "pick" :agent "lead-a" :at 9))))))
        (setq reply (lambda (m _p) (pcase m ("herd_board" board) (_ nil))))
        (ghostherd-memory-page--handle '((op . "board-watch")))
        (let* ((p (ghostherd-test--call calls "boardPatch"))
               (agents (append (plist-get (plist-get p :meta) :agents) nil))
               (need (aref (plist-get p :jing) 0)))
          (should (equal (mapcar (lambda (a) (list (plist-get a :name) (plist-get a :role))) agents)
                         '(("lead-a" "lead") ("rd-1" ""))))
          (should (equal (plist-get (car agents) :project) (ghostherd--project-name "/src/a/")))
          (should (equal (list (plist-get need :replyVia) (plist-get need :projectDir))
                         (list "lead" (ghostherd--project-name "/src/a/")))))))))

(ert-deftest ghostherd-test-board-badge-with-the-tab-closed ()
  "With the Board closed, its badge's counts come from `herd_board_counts'
-- never the board: as the page starts, on news at most every
`ghostherd-board--peek-every', and every `--peek-fallback' with none (an
item turns 沒人接 by time alone).  With the Board open, none is asked."
  (ghostherd-board-test--with
    (let ((ghostherd-board--peek-timer nil) (ghostherd-board--peek-soon nil) (ghostherd-board--peek-at 0)
          (ghostherd-board-orphan-hours 6) (timers nil))
      (cl-letf (((symbol-function 'run-at-time) (lambda (secs rep _fn &rest _) (push (list secs rep) timers) 'a-timer)))
        (setq reply (lambda (m _p) (pcase m ("herd_board_counts" '(:jing 2 :orphan 1)))))
        (ghostherd-memory-page--handle '((op . "board-peek")))
        (should (equal requests '(("herd_board_counts" (:orphan_hours 6)))))
        (should (equal (ghostherd-test--call calls "boardBadge") '(:jing 2 :orphan 1)))
        (should (member (list ghostherd-board--peek-fallback ghostherd-board--peek-fallback) timers))
        ;; News: one look, after the last by `--peek-every' at most; more news meanwhile, still one.
        (setq timers nil requests nil)
        (ghostherd-board--on-log nil)
        (ghostherd-board--on-log nil)
        (should (= 1 (length timers)))
        (should (> (car (car timers)) 20))
        (should-not requests)
        ;; The Board open: its patches carry the count; news asks nothing.
        (setq timers nil ghostherd-board--peek-soon nil)
        (let ((ghostherd-board--watching t))
          (ghostherd-board--on-log nil)
          (ghostherd-board--peek)
          (should-not timers)
          (should-not requests))))))

;;; Tickets: what ClickUp says of them, by the work chips' lookup and rules

(ert-deftest ghostherd-test-board-tickets-through-the-work-chips-lookup ()
  "The Board's tickets: each the cards carry, once, looked up as a chore
through the work chips' own lookup, its state from their cache -- title,
status, colour, the status's type -- unsure when the last look failed;
the most lately named first, `ghostherd-board--tickets-max' at most."
  (ghostherd-board-test--with
    (let ((ghostherd-memory-page--task-briefs (make-hash-table :test 'equal)) (asked nil)
          (brief (lambda (name st type) `((id . "x") (name . ,name) (status . ((name . ,st) (color . "#123456") (type . ,type)))
                                          (url . "https://app.clickup.com/t/x")))))
      (puthash "86a" (list :at (float-time) :brief (funcall brief "Login" "in progress" "custom")) ghostherd-memory-page--task-briefs)
      (puthash "86d" (list :at (float-time) :brief (funcall brief "Old" "complete" "closed") :miss "HTTP 500")
               ghostherd-memory-page--task-briefs)
      (let ((board (ghostherd-board-test--reply
                    (ghostherd-board-test--card "w1" "live" :tickets (list (list :id "86a" :last 300) (list :id "86d" :last 100)))
                    (ghostherd-board-test--card "w2" "quiet" :tickets (list (list :id "86a" :last 200) (list :id "bad id" :last 900))))))
        (setq reply (lambda (m _p) (pcase m ("herd_board" board) (_ nil))))
        (cl-letf (((symbol-function 'ghostherd-memory-page--chore) (lambda (fn &rest args) (push (cons fn args) asked))))
          (ghostherd-memory-page--handle '((op . "board-watch"))))
        (should (equal (sort (mapcar #'cadr asked) #'string<) '("86a" "86d")))
        (should (eq (car (car asked)) 'ghostherd-memory-page--task-ask))
        (let* ((tickets (append (plist-get (ghostherd-test--call calls "boardPatch") :tickets) nil))
               (a (car tickets)) (d (cadr tickets)))
          (should (equal (mapcar (lambda (k) (plist-get k :id)) tickets) '("86a" "86d")))
          (should (equal (list (plist-get a :name) (plist-get a :status) (plist-get a :type) (plist-get a :unsure))
                         '("Login" "in progress" "custom" :json-false)))
          ;; The last look failed: what it knew, said unsure, the miss its error.
          (should (equal (list (plist-get d :type) (plist-get d :unsure) (plist-get d :error) (plist-get d :done))
                         '("closed" t "HTTP 500" t))))
        (let ((ghostherd-board--tickets-max 1))
          (should (equal (mapcar (lambda (k) (plist-get k :id))
                                 (cl-letf (((symbol-function 'ghostherd-memory-page--chore) #'ignore))
                                   (ghostherd-board--tickets (plist-get board :items))))
                         '("86a"))))))))

(ert-deftest ghostherd-test-task-lookups-keep-what-they-knew ()
  "A task's look, as a PR's: a failed one keeps the state it knew and notes
the miss; a done or closed task is kept longer than an open one; one in
flight is not asked again until it answers or hangs past its time."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let ((ghostherd-memory-page--task-briefs (make-hash-table :test 'equal)) (asks 0) (answer nil))
    (cl-letf (((symbol-function 'clickup-view-task-brief) (lambda (_id cb) (cl-incf asks) (when answer (funcall cb answer))))
              ((symbol-function 'ghostherd-memory-page--have) (lambda (&rest _) t))
              ((symbol-function 'ghostherd-memory-page--work-changed) #'ignore))
      (setq answer '((id . "86a") (name . "Login") (status . ((name . "to do") (type . "open")))))
      (ghostherd-memory-page--task-ask "86a")
      (should (= asks 1))
      ;; Fresh: not asked.  Stale: asked; failing, it keeps what it knew.
      (ghostherd-memory-page--task-ask "86a")
      (should (= asks 1))
      (puthash "86a" (plist-put (gethash "86a" ghostherd-memory-page--task-briefs) :at (- (float-time) 400))
               ghostherd-memory-page--task-briefs)
      (setq answer '((id . "86a") (error . "HTTP 500")))
      (ghostherd-memory-page--task-ask "86a")
      (should (= asks 2))
      (let ((e (gethash "86a" ghostherd-memory-page--task-briefs)))
        (should (equal (alist-get 'name (plist-get e :brief)) "Login"))
        (should (equal (plist-get e :miss) "HTTP 500"))
        (should (equal (plist-get (ghostherd-memory-page--task "86a") :error) "HTTP 500")))
      ;; Done in ClickUp: kept past the open one's time.
      (puthash "86d" (list :at (- (float-time) 400) :brief '((status . ((type . "done"))))) ghostherd-memory-page--task-briefs)
      (ghostherd-memory-page--task-ask "86d")
      (should (= asks 2))
      ;; In flight: not asked again; hung past its time: asked.
      (setq answer nil)
      (puthash "86x" (list :pending (float-time)) ghostherd-memory-page--task-briefs)
      (ghostherd-memory-page--task-ask "86x")
      (should (= asks 2))
      (puthash "86x" (list :pending (- (float-time) 500)) ghostherd-memory-page--task-briefs)
      (ghostherd-memory-page--task-ask "86x")
      (should (= asks 3)))))

(provide 'ghostherd-board-tests)
;;; ghostherd-board-tests.el ends here

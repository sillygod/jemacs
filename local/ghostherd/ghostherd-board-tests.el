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

(provide 'ghostherd-board-tests)
;;; ghostherd-board-tests.el ends here

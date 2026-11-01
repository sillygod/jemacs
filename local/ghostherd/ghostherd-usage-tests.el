;;; ghostherd-usage-tests.el --- Tests for ghostherd-usage.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Part of ghostherd's tests; ghostherd-tests.el runs them all.  This
;; file alone:
;;
;;   emacs --batch --init-directory=~/.emacs.d/emacs-home/ \
;;         -L . -L ../xwapp -l ghostherd-usage-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ghostherd-test-helpers)

(declare-function ghostherd-memory-page--usage "ghostherd-memory-page" ())

;;; Account usage

(ert-deftest ghostherd-test-usage-remaining-from-used ()
  (should (= (ghostherd-usage--remaining 89.0) 11.0))
  (should (= (ghostherd-usage--remaining 0) 100.0))
  (should (= (ghostherd-usage--remaining 100) 0.0)))

(ert-deftest ghostherd-test-usage-parse-grok-config ()
  (let* ((raw (json-parse-string
               "{\"creditUsagePercent\":89.0,\"currentPeriod\":{\"type\":\"USAGE_PERIOD_TYPE_WEEKLY\",\"end\":\"2026-09-20T14:57:40Z\"}}"
               :object-type 'alist :null-object nil))
         (wins (ghostherd-usage--parse-grok-config raw)))
    (should (= (length wins) 1))
    (should (equal (plist-get (car wins) :label) "wk"))
    (should (= (plist-get (car wins) :remaining) 11.0))))

(ert-deftest ghostherd-test-usage-grok-new-week-is-nothing-used ()
  "Billing leaves a zero out, as protobuf's JSON does: a week just begun
has no creditUsagePercent, and is nothing used, not no numbers."
  (let* ((raw (json-parse-string
               "{\"currentPeriod\":{\"type\":\"USAGE_PERIOD_TYPE_WEEKLY\",\"start\":\"2026-10-11T14:57:40.825457+00:00\",\"end\":\"2026-10-18T14:57:40.825457+00:00\"},\"onDemandCap\":{\"val\":0},\"isUnifiedBillingUser\":true}"
               :object-type 'alist :null-object nil))
         (wins (ghostherd-usage--parse-grok-config raw)))
    (should (equal (plist-get (car wins) :label) "wk"))
    (should (= (plist-get (car wins) :remaining) 100.0))
    (should (= (plist-get (car wins) :resets-at)
               (float-time (encode-time (iso8601-parse "2026-10-18T14:57:40.825457+00:00")))))
    ;; Nor a period: nothing to read, not a guess.
    (should-not (ghostherd-usage--parse-grok-config '((isUnifiedBillingUser . t))))))

(ert-deftest ghostherd-test-usage-grok-log-as-of-when-written ()
  "A billing line in grok's log counts from when grok wrote it, and only
over numbers older than it."
  (let* ((line (lambda (ts used)
                 (format "{\"ts\":\"%s\",\"msg\":\"billing: fetched credits config\",\"ctx\":{\"config\":{\"creditUsagePercent\":%s,\"currentPeriod\":{\"type\":\"USAGE_PERIOD_TYPE_WEEKLY\",\"end\":\"2026-10-18T14:57:40Z\"}}}}"
                         ts used)))
         (tail (concat (funcall line "2026-10-11T15:18:44.171Z" 12)
                       "\n{\"ts\":\"2026-10-11T15:19:00Z\",\"msg\":\"other\"}\n"))
         (ghostherd-usage--cache nil))
    (cl-letf (((symbol-function 'ghostherd-usage--file-tail) (lambda (&rest _) tail)))
      (should (ghostherd-usage--grok-from-log))
      (let ((pl (alist-get 'grok ghostherd-usage--cache)))
        (should (= (plist-get pl :fetched)
                   (float-time (encode-time (iso8601-parse "2026-10-11T15:18:44.171Z")))))
        (should (= (plist-get (car (plist-get pl :windows)) :remaining) 88.0)))
      ;; The API has said something since: the older line stands down.
      (ghostherd-usage--put 'grok (list (ghostherd-usage--window "wk" 70.0)))
      (ghostherd-usage--grok-from-log)
      (should (= (plist-get (car (plist-get (alist-get 'grok ghostherd-usage--cache) :windows))
                            :remaining)
                 70.0)))))

(ert-deftest ghostherd-test-usage-grok-fetch-says-why-it-failed ()
  "A fetch that fails keeps the numbers and says why: a card left as it
was reads as current."
  (let ((ghostherd-usage--cache
         (list (cons 'grok (list :windows (list (ghostherd-usage--window "wk" 7.0))
                                 :fetched 100.0 :error nil))))
        (reply nil))
    (cl-letf (((symbol-function 'ghostherd-usage--grok-auth-entry)
               (lambda () `((key . "k") (user_id . "u")
                            (expires_at . ,(format-time-string "%FT%T%z" (+ (float-time) 3600))))))
              ((symbol-function 'ghostherd-usage--http-json)
               (lambda (_url _headers ok-fn &rest _) (apply ok-fn reply)))
              ((symbol-function 'ghostherd-usage--done) #'ignore))
      (setq reply '(nil 503))
      (ghostherd-usage--fetch-grok)
      (let ((pl (alist-get 'grok ghostherd-usage--cache)))
        (should (equal (plist-get pl :error) "http 503"))
        (should (= (plist-get pl :fetched) 100.0))
        (should (= (plist-get (car (plist-get pl :windows)) :remaining) 7.0)))
      (setq reply '(((config (onDemandCap (val . 0))))))
      (ghostherd-usage--fetch-grok)
      (should (equal (plist-get (alist-get 'grok ghostherd-usage--cache) :error) "billing unreadable"))
      ;; The new week, as the API sends it: nothing used, the error gone.
      (setq reply '(((config (currentPeriod (type . "USAGE_PERIOD_TYPE_WEEKLY")
                                            (end . "2026-10-18T14:57:40Z"))))))
      (ghostherd-usage--fetch-grok)
      (let ((pl (alist-get 'grok ghostherd-usage--cache)))
        (should-not (plist-get pl :error))
        (should (= (plist-get (car (plist-get pl :windows)) :remaining) 100.0))))))

(ert-deftest ghostherd-test-usage-parse-claude ()
  (let* ((raw (json-parse-string
               "{\"five_hour\":{\"utilization\":38.0,\"resets_at\":\"2026-09-18T12:00:00Z\"},\"seven_day\":{\"utilization\":12.0}}"
               :object-type 'alist :null-object nil))
         (wins (ghostherd-usage--parse-claude raw)))
    (should (equal (mapcar (lambda (w) (plist-get w :label)) wins)
                   '("5h" "7d")))
    (should (= (plist-get (nth 0 wins) :remaining) 62.0))
    (should (= (plist-get (nth 1 wins) :remaining) 88.0))))

(ert-deftest ghostherd-test-usage-parse-agy-groups-names-each-group ()
  "Each group's rows named from it, Gemini 4 Argon apart from the rest of
Gemini: two `wk' rows under one `gem' would otherwise read as one."
  (let* ((raw (json-parse-string
               "{\"groups\":[{\"displayName\":\"Gemini Models\",\"buckets\":[{\"window\":\"weekly\",\"remainingFraction\":0.993},{\"window\":\"5h\",\"remainingFraction\":1}]},{\"displayName\":\"Third-Party Models\",\"buckets\":[{\"window\":\"weekly\",\"remainingFraction\":1},{\"window\":\"5h\",\"remainingFraction\":1}]},{\"displayName\":\"Gemini 4 Argon\",\"buckets\":[{\"window\":\"weekly\",\"remainingFraction\":0}]}]}"
               :object-type 'alist :array-type 'list :null-object nil))
         (wins (ghostherd-usage--parse-agy-groups raw)))
    (should (equal (mapcar (lambda (w) (plist-get w :label)) wins)
                   '("gem-wk" "gem-5h" "3p-wk" "3p-5h" "g4-wk")))
    (should (= (plist-get (car (last wins)) :remaining) 0.0))))

(ert-deftest ghostherd-test-herd-page-usage-is-rounded ()
  "100 - 99.3 in floating point is 0.7000000000000028, and the page
printed every digit of it across the next column."
  (skip-unless (require 'ghostherd-memory-page nil t))
  (let ((ghostherd-usage--cache '((agy :windows ((:label "gem-wk" :remaining 99.3)) :fetched 1.0))))
    (let ((w (aref (plist-get (aref (ghostherd-memory-page--usage) 0) :windows) 0)))
      (should (equal (plist-get w :used) 0.7)))))

(ert-deftest ghostherd-test-usage-parse-agy-groups ()
  (let* ((raw (json-parse-string
               "{\"groups\":[{\"displayName\":\"Gemini Models\",\"buckets\":[{\"window\":\"5h\",\"remainingFraction\":0.8},{\"window\":\"weekly\",\"remainingFraction\":0.55}]}]}"
               :object-type 'alist :array-type 'list :null-object nil))
         (wins (ghostherd-usage--parse-agy-groups raw)))
    (should (equal (mapcar (lambda (w) (plist-get w :label)) wins)
                   '("5h" "wk")))
    (should (= (plist-get (nth 0 wins) :remaining) 80.0))
    (should (= (plist-get (nth 1 wins) :remaining) 55.0))))

(defconst ghostherd-tests--agy-usage-14-screen
  "└ Models & Quota

  Account: sillygodprodigy@gmail.com

GEMINI MODELS
  Models within this group: Gemini Flash, Gemini Pro

  Weekly Limit Remaining
    [██████] 100.00%
    Quota available

  Five Hour Limit Remaining
    [██████] 100.00%
    Quota available


CLAUDE AND GPT MODELS
  Models within this group: Claude Opus, Claude Sonnet, GPT-OSS

  Weekly Limit Remaining
    [██████] 100.00%
    Quota available

  Five Hour Limit Remaining
    [██████] 100.00%
    Quota available


GEMINI 4 ARGON
  For paid Pro plans, Argon quota is separate. Higher Argon quota (included as part of the larger Gemini quota bucket) i

  Gemini 4 Argon Weekly Limit
    [░░░░░░] 0.00%
    Refreshes in 163h 41m


  │Within each group, models share a weekly limit and a 5-hour limit. Quota is
  │consumed proportionally to the cost of the tokens. Thus, limits will last
  │longer with shorter tasks or using more cost-effective models. The 5-hour
  │limit smooths out aggregate demand to fairly distribute global capacity
  │across all users, while your weekly limit is tied directly to your individual
  │tier.

  ↑/↓ Scroll · pgup/pgdown Page · ctrl+end Bottom · ctrl+home Top · esc Close\n"
  "agy 1.4.0's /usage panel, captured from a scratch agy: three groups --
Gemini, Claude/GPT, and a separate Gemini 4 Argon whose weekly quota is
spent (0%).")

(ert-deftest ghostherd-test-usage-parse-agy-usage-screen ()
  "agy 1.4.0's panel: a row per group's limit, labelled from its group so
Gemini 4 Argon is its own `g4-wk' (and spent, 0%), not dropped or stuck
onto `gem'; the Gemini and Claude/GPT rows still read."
  (let ((wins (ghostherd-usage--parse-agy-usage-screen ghostherd-tests--agy-usage-14-screen)))
    (should (equal (mapcar (lambda (w) (plist-get w :label)) wins)
                   '("gem-wk" "gem-5h" "3p-wk" "3p-5h" "g4-wk")))
    (should (= (plist-get (nth 0 wins) :remaining) 100.0))
    (should (= (plist-get (nth 2 wins) :remaining) 100.0))
    (should (= (plist-get (nth 4 wins) :remaining) 0.0))
    (should (= (ghostherd-usage--used (plist-get (nth 4 wins) :remaining)) 100.0))))

(ert-deftest ghostherd-test-usage-agy-group-prefix ()
  "The group a row belongs to: Gemini 4 Argon apart from the rest of Gemini."
  (should (equal (ghostherd-usage--agy-group-prefix "GEMINI MODELS") "gem"))
  (should (equal (ghostherd-usage--agy-group-prefix "GEMINI 4 ARGON") "g4"))
  (should (equal (ghostherd-usage--agy-group-prefix "Gemini 4 Argon") "g4"))
  (should (equal (ghostherd-usage--agy-group-prefix "CLAUDE AND GPT MODELS") "3p")))
(ert-deftest ghostherd-test-usage-used-from-remaining ()
  (should (= (ghostherd-usage--used 78.0) 22.0))
  (should (= (ghostherd-usage--used 0) 100.0))
  (should (= (ghostherd-usage--used 100) 0.0)))

(ert-deftest ghostherd-test-usage-bar-and-mode-line-safe ()
  (should (string-match-p "█" (ghostherd-usage--bar 80 5)))
  (should (string-match-p "░" (ghostherd-usage--bar 80 5)))
  (should (equal (ghostherd-usage--mode-line-safe "78% 5h") "78%% 5h")))

(ert-deftest ghostherd-test-usage-bar-fills-with-what-is-spent ()
  "Filled cells are the used share, like the provider panels."
  (should (equal (substring-no-properties (ghostherd-usage--bar 100 4)) "░░░░"))
  (should (equal (substring-no-properties (ghostherd-usage--bar 0 4)) "████"))
  (should (equal (substring-no-properties (ghostherd-usage--bar 50 4)) "██░░")))

(ert-deftest ghostherd-test-usage-window-bar-prints-used ()
  "78% left reads as 22, the number claude.ai shows."
  (let ((w (ghostherd-usage--window "5h" 78.0)))
    (should (string-match-p "22 5h"
                            (substring-no-properties
                             (ghostherd-usage--window-bar w nil))))
    (should-not (string-match-p "78"
                                (substring-no-properties
                                 (ghostherd-usage--window-bar w nil))))))

(ert-deftest ghostherd-test-usage-help-spells-out-both-sides ()
  "The bare integer is ambiguous; the tooltip is not."
  (let* ((wins (list (ghostherd-usage--window
                      "5h" 78.0 (+ (float-time) (* 3600 1.05)))))
         (help (ghostherd-usage--help 'claude wins)))
    (should (string-match-p "22% used" help))
    (should (string-match-p "78% left" help))
    (should (string-match-p "resets in 1h0[23]m" help))))

(ert-deftest ghostherd-test-usage-eta ()
  (should (equal (ghostherd-usage--eta (+ (float-time) 125)) "2m"))
  (should (equal (ghostherd-usage--eta (+ (float-time) 3725)) "1h02m"))
  (should-not (ghostherd-usage--eta (- (float-time) 60)))
  (should-not (ghostherd-usage--eta nil)))

(ert-deftest ghostherd-test-usage-line-in-footer ()
  (let ((ghostherd-usage--cache
         '((claude . (:windows ((:label "5h" :remaining 62.0)) :error nil))
           (grok . (:windows ((:label "wk" :remaining 11.0)) :error nil)))))
    (let ((line (substring-no-properties (ghostherd-usage-line 120))))
      (should (string-match-p "claude" line))
      ;; 62% left of the 5h window is 38% used.
      (should (string-match-p "38" line))
      (should (string-match-p "█\\|░" line))
      (should-not (string-match-p "%" line))
      (should (string-match-p "grok" line)))
    (ghostherd-tests--with-herd ()
      (let ((ghostherd-sidebar-width 80)
            (ghostherd--sidebar-target-width 80))
        (should (string-match-p "claude"
                                (substring-no-properties
                                 (ghostherd--sidebar-footer))))))))

(ert-deftest ghostherd-test-usage-401-does-not-prompt ()
  "A 401 from our own hosts must reach our callback, not the minibuffer.
t is url.el\'s \"already tried, give up\" answer; nil would make it
retry and never activate the callback."
  (let ((called nil))
    (cl-letf (((symbol-function 'url-http-handle-authentication)
               (lambda (&rest _) (setq called t) 'orig)))
      ;; Our request: answered without consulting url.el.
      (let ((url-current-object (url-generic-parse-url
                                 "https://daily-cloudcode-pa.googleapis.com/v1internal:x")))
        (should (eq (ghostherd-usage--no-auth-prompt
                     (symbol-function 'url-http-handle-authentication) nil)
                    t))
        (should-not called))
      ;; Somebody else\'s request: untouched.
      (let ((url-current-object (url-generic-parse-url "https://example.com/")))
        (should (eq (ghostherd-usage--no-auth-prompt
                     (symbol-function 'url-http-handle-authentication) nil)
                    'orig))
        (should called)))))

(ert-deftest ghostherd-test-usage-http-deadline-fires-callback-once ()
  "A response that never lands must still finish the request.
Without this the batch counter stays pinned and gates every
later refresh."
  (let ((got nil) (deadline nil) (buf nil))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (&rest _)
                 (setq buf (generate-new-buffer " *gh-usage-test*"))))
              ((symbol-function 'run-with-timer)
               (lambda (_secs _repeat fn &rest args)
                 (setq deadline (lambda () (apply fn args)))
                 nil)))
      (ghostherd-usage--http "https://api.anthropic.com/api/oauth/usage" nil
                             (lambda (code body) (push (list code body) got)))
      (should-not got)
      (funcall deadline)
      (funcall deadline)
      (should (equal got '((0 "")))))
    (when (buffer-live-p buf) (kill-buffer buf))))

(ert-deftest ghostherd-test-usage-agy-failure-is-visible ()
  "agy must never render as a bare dash when the fetch failed.
No cache entry reads as \"not wired up\"; `agy ?\' carries the reason."
  (let ((ghostherd-usage--cache nil))
    (cl-letf (((symbol-function 'ghostherd-usage--agy-probe) (lambda () nil)))
      (ghostherd-usage--agy-fail "refresh 401"))
    (let ((pl (alist-get 'agy ghostherd-usage--cache)))
      (should pl)
      (should (equal (plist-get pl :error) "refresh 401")))
    (let ((s (ghostherd-usage--entry-string 'agy nil)))
      (should (string-match-p "agy \\?" (substring-no-properties s)))
      (should (equal (get-text-property 0 'help-echo s) "refresh 401")))))

(defmacro ghostherd-tests--with-agy-token (blob &rest body)
  "BODY with agy's saved token read as BLOB, and every request recorded."
  (declare (indent 1))
  `(let ((ghostherd-usage--cache nil) (requests nil) (done 0))
     (cl-letf (((symbol-function 'ghostherd-usage--agy-from-screens) #'ignore)
               ((symbol-function 'ghostherd-usage--agy-token-blob) (lambda () ,blob))
               ((symbol-function 'ghostherd-usage--http-json)
                (lambda (url &rest _) (push url requests)))
               ((symbol-function 'ghostherd-usage--done) (lambda () (cl-incf done))))
       ,@body)))

(ert-deftest ghostherd-test-usage-agy-expired-token-is-said-not-refreshed ()
  "agy refreshes its token in memory only; refreshing it ourselves needs a
client secret Google no longer takes.  An expired token is reported,
with when, and nothing is sent."
  (ghostherd-tests--with-agy-token
      `((access_token . "a") (refresh_token . "r")
        (expiry . ,(format-time-string "%FT%T%z" (- (float-time) 3600))))
    (let ((ghostherd-usage-agy-probe nil))
      (ghostherd-usage--fetch-agy))
    (should-not requests)
    (should (= done 1))
    ;; An hour ago is yesterday in the hour after midnight: the day then shows.
    (should (string-match-p (if (equal (format-time-string "%F" (- (float-time) 3600))
                                       (format-time-string "%F"))
                                "\\`agy's saved token expired [0-9][0-9]:[0-9][0-9]\\'"
                              "\\`agy's saved token expired [0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]\\'")
                            (plist-get (alist-get 'agy ghostherd-usage--cache) :error)))))

(ert-deftest ghostherd-test-usage-agy-fresh-token-asks-the-api ()
  (ghostherd-tests--with-agy-token
      `((access_token . "a") (expiry . ,(format-time-string "%FT%T%z" (+ (float-time) 3600))))
    (ghostherd-usage--fetch-agy)
    (should (equal requests
                   '("https://daily-cloudcode-pa.googleapis.com/v1internal:retrieveUserQuotaSummary")))))

(ert-deftest ghostherd-test-usage-agy-probe-can-be-switched-off ()
  "The probe types into your session: off means it never does."
  (ghostherd-tests--with-herd ()
    (ghostherd-tests--session :name "agy-a" :kind 'agy :state 'idle)
    (let (typed (ghostherd-usage--agy-probe-at 0))
      (cl-letf (((symbol-function 'ghostherd--host-send-text)
                 (lambda (_s text &rest _) (push text typed)))
                ((symbol-function 'ghostherd--session-live-p) (lambda (_) t))
                ((symbol-function 'run-with-timer) #'ignore))
        (let ((ghostherd-usage-agy-probe nil))
          (should-not (ghostherd-usage--agy-probe)))
        (should-not typed)
        (let ((ghostherd-usage-agy-probe t))
          (should (ghostherd-usage--agy-probe)))
        (should (equal typed '("/usage")))))))

(ert-deftest ghostherd-test-usage-failure-keeps-last-good-numbers ()
  "A failed poll reads as stale numbers, not as no numbers."
  (let ((ghostherd-usage--cache nil))
    (ghostherd-usage--put 'agy (list (ghostherd-usage--window "gem-5h" 94.0)))
    (let ((fetched (plist-get (alist-get 'agy ghostherd-usage--cache) :fetched)))
      (cl-letf (((symbol-function 'ghostherd-usage--agy-probe) (lambda () nil)))
        (ghostherd-usage--agy-fail "refresh 401"))
      (let ((pl (alist-get 'agy ghostherd-usage--cache)))
        (should (= (length (plist-get pl :windows)) 1))
        (should (equal (plist-get pl :error) "refresh 401"))
        ;; Stale means stale: the age must not be reset by the failure.
        (should (= (plist-get pl :fetched) fetched))))
    (let ((s (ghostherd-usage--entry-string 'agy nil)))
      (should (string-match-p "6 gem-5h" (substring-no-properties s)))
      (should (string-match-p "stale: refresh 401"
                              (get-text-property 0 'help-echo s))))))

(ert-deftest ghostherd-test-usage-stale-batch-unblocks-refresh ()
  "A batch older than every deadline is a dropped callback, not work."
  (let ((fetched 0))
    (cl-letf (((symbol-function 'ghostherd-usage--grok-from-log) #'ignore)
              ((symbol-function 'ghostherd-usage--redraw) #'ignore)
              ((symbol-function 'ghostherd-usage--fetch-claude)
               (lambda () (cl-incf fetched)))
              ((symbol-function 'ghostherd-usage--fetch-grok)
               (lambda () (cl-incf fetched)))
              ((symbol-function 'ghostherd-usage--fetch-agy)
               (lambda () (cl-incf fetched))))
      (let ((noninteractive nil)
            (ghostherd-usage-timeout 15)
            (ghostherd-usage--inflight 1)
            (ghostherd-usage--inflight-at (- (float-time) 1000)))
        (ghostherd-usage-refresh)
        (should (= fetched 3))
        (should (= ghostherd-usage--inflight 3)))
      ;; A batch still inside its deadline keeps gating.
      (setq fetched 0)
      (let ((noninteractive nil)
            (ghostherd-usage-timeout 15)
            (ghostherd-usage--inflight 1)
            (ghostherd-usage--inflight-at (float-time)))
        (ghostherd-usage-refresh)
        (should (= fetched 0))))))

(ert-deftest ghostherd-test-poll-tick-does-not-refresh-usage ()
  "Usage HTTP must not ride the 1.5s poll (same reason as herd mail)."
  (let ((usage nil))
    (cl-letf (((symbol-function 'ghostherd-usage-refresh)
               (lambda (&rest _) (setq usage t)))
              ((symbol-function 'ghostherd-poll-all) #'ignore)
              ((symbol-function 'ghostherd--sidebar-on-screen-p)
               (lambda () t))
              ((symbol-function 'ghostherd--sidebar-refresh) #'ignore))
      (ghostherd--poll-tick)
      (should-not usage))))

(provide 'ghostherd-usage-tests)
;;; ghostherd-usage-tests.el ends here

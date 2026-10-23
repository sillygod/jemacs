;;; artifacthub.el --- Search Artifact Hub from a ghostel-cmd style overlay -*- lexical-binding: t; -*-

;; Author: Jing
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Search https://artifacthub.io in an overlay that looks and keys like
;; ghostel-cmd's (posframe when available, side window otherwise).
;; Typing on the footer searches without blocking: url-retrieve after a
;; 0.3 s pause, two characters at least, an answer to an older query
;; dropped, every page kept five minutes (the API says max-age=300).
;;
;;   type the query   RET to the list   TAB / S-TAB kind   Esc quit
;;   j/k move   RET use the chart   C-RET use and run (in a terminal)
;;   V version   v values.yaml   o open   y copy   n more   / search
;;
;; RET depends on where the overlay was opened: a Chart.yaml gets a
;; dependency, a helmfile.yaml a release (and its repository), a
;; Ghostel terminal the helm commands, with {{release}} and
;; {{namespace}} filled on the footer by ghostel-cmd's own fill; any
;; other buffer puts `repo/chart --version V' on the kill ring.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'url)
(require 'url-http)
(require 'pulse)
(require 'ghostel-cmd)

(declare-function posframe-show "posframe")
(declare-function posframe-hide "posframe")
(declare-function posframe-poshandler-frame-center "posframe")
(declare-function evil-define-key* "evil")
(declare-function evil-make-intercept-map "evil")
(declare-function evil-normalize-keymaps "evil")
(declare-function yaml-mode "yaml-mode")

(defvar url-http-end-of-headers)
(defvar url-http-response-status)
(defvar posframe--frame)
(defvar yaml-mode-hook)


;;; Customization

(defgroup artifacthub nil
  "Search Artifact Hub."
  :group 'tools
  :prefix "artifacthub-")

(defcustom artifacthub-url "https://artifacthub.io"
  "Artifact Hub's site; its API lives under /api/v1."
  :type 'string)

(defcustom artifacthub-debounce 0.3
  "Seconds without typing before the query is searched."
  :type 'number)

(defcustom artifacthub-min-input 2
  "Shortest query searched."
  :type 'integer)

(defcustom artifacthub-page-size 20
  "Packages asked for at a time; n asks for the next ones."
  :type 'integer)

(defcustom artifacthub-cache-ttl 300
  "Seconds an answer is reused; the API says max-age=300."
  :type 'integer)

(defcustom artifacthub-timeout 15
  "Seconds before a request is given up.  url.el has no timeout."
  :type 'number)

(defcustom artifacthub-use-posframe t
  "Show the overlay in a posframe when possible."
  :type 'boolean)

(defcustom artifacthub-sidebar-width 90
  "Side window width when not using posframe."
  :type 'integer)


;;; Kinds

(defconst artifacthub-kinds
  '((0 "helm" "Helm")
    (1 "falco" "Falco")
    (2 "opa" "OPA")
    (3 "olm" "OLM")
    (4 "tbaction" "Tinkerbell")
    (5 "krew" "Krew")
    (6 "helm-plugin" "Helm plugin")
    (7 "tekton-task" "Tekton task")
    (8 "keda-scaler" "KEDA")
    (9 "coredns" "CoreDNS")
    (10 "keptn" "Keptn")
    (11 "tekton-pipeline" "Tekton pipeline")
    (12 "container" "Container")
    (13 "kubewarden" "Kubewarden")
    (14 "gatekeeper" "Gatekeeper")
    (15 "kyverno" "Kyverno")
    (16 "knative-client-plugin" "Knative")
    (17 "backstage" "Backstage")
    (18 "argo-template" "Argo")
    (19 "kubearmor" "KubeArmor")
    (20 "kcl" "KCL")
    (21 "headlamp" "Headlamp")
    (22 "inspektor-gadget" "Gadget")
    (23 "tekton-stepaction" "Tekton step")
    (24 "meshery" "Meshery")
    (25 "opencost" "OpenCost")
    (26 "radius" "Radius")
    (27 "bootc" "bootc")
    (28 "kagent" "Kagent"))
  "Each kind as (ID SEGMENT LABEL).
SEGMENT is the kind's part of a package page's path,
/packages/SEGMENT/REPO/NAME, as the site's own frontend maps it (read
2026-10-23).  It is not always the kind's name: Helm plugins are
helm-plugin, container images container, Meshery designs meshery,
Radius recipes radius.")

(defun artifacthub--kind-segment (kind)
  "The page-path segment of KIND, or nil for a kind this does not know."
  (nth 1 (assq kind artifacthub-kinds)))

(defun artifacthub--kind-label (kind)
  "A short name for KIND."
  (or (nth 2 (assq kind artifacthub-kinds))
      (format "kind %s" kind)))


;;; Packages

(defun artifacthub--repo (pkg key)
  "KEY of PKG's repository."
  (alist-get key (alist-get 'repository pkg)))

(defun artifacthub--kind (pkg)
  "PKG's kind."
  (or (artifacthub--repo pkg 'kind) 0))

(defun artifacthub--helm-p (pkg)
  "Non-nil when PKG is a Helm chart."
  (eql (artifacthub--kind pkg) 0))

(defun artifacthub--oci-p (pkg)
  "Non-nil when PKG's repository is an OCI registry."
  (string-prefix-p "oci://" (or (artifacthub--repo pkg 'url) "")))

(defun artifacthub--official-p (pkg)
  "Non-nil when PKG or its repository is marked official, as the site does."
  (or (alist-get 'official pkg) (artifacthub--repo pkg 'official)))

(defun artifacthub--package-url (pkg)
  "PKG's page on the site.
A kind this does not know falls back to a search for PKG."
  (let ((segment (artifacthub--kind-segment (artifacthub--kind pkg))))
    (if segment
        (format "%s/packages/%s/%s/%s" artifacthub-url segment
                (url-hexify-string (artifacthub--repo pkg 'name))
                (url-hexify-string (alist-get 'normalized_name pkg)))
      (format "%s/packages/search?ts_query_web=%s&kind=%s" artifacthub-url
              (url-hexify-string (alist-get 'name pkg))
              (artifacthub--kind pkg)))))

(defun artifacthub--chart-ref (pkg)
  "How helm names PKG's chart: oci://… for an OCI one, else REPO/NAME.
Artifact Hub keeps one chart per OCI repository and its URL is the
chart's own."
  (let ((name (alist-get 'name pkg)))
    (if (artifacthub--oci-p pkg)
        (let ((url (string-trim-right (artifacthub--repo pkg 'url) "/+")))
          (if (string-suffix-p (concat "/" name) url)
              url
            (concat url "/" name)))
      (concat (artifacthub--repo pkg 'name) "/" name))))

(defun artifacthub--repo-url (pkg)
  "The repository a chart depends on PKG from: an OCI one without the
chart's own name at its end; no trailing slash."
  (let ((url (string-trim-right (artifacthub--repo pkg 'url) "/+"))
        (name (alist-get 'name pkg)))
    (if (and (artifacthub--oci-p pkg)
             (string-suffix-p (concat "/" name) url))
        (substring url 0 (- (1+ (length name))))
      url)))

(defun artifacthub--url-key (url)
  "URL as compared with another repository's: no scheme, case or
trailing slash, so https://x/charts/ and a helmfile's OCI x/charts match."
  (string-trim-right
   (replace-regexp-in-string "\\`[a-z]+://" "" (downcase (string-trim url)))
   "/+"))


;;; Versions

(defun artifacthub--version-parts (version)
  "(NUMBERS . PRE) of VERSION, a semver-like string: its dotted numbers,
and its pre-release part or nil.  Nil when VERSION is not that shape."
  (when (and version
             (string-match
              "\\`[vV]?\\([0-9]+\\(?:\\.[0-9]+\\)*\\)\\(?:-\\([^+]*\\)\\)?\\(?:\\+.*\\)?\\'"
              version))
    (let ((numbers (match-string 1 version))
          (pre (match-string 2 version)))
      (cons (mapcar #'string-to-number (split-string numbers "\\.")) pre))))

(defun artifacthub--prerelease-p (version &optional flag)
  "Non-nil when VERSION is a pre-release: FLAG, the API's own mark, or a
-suffix.  The API leaves a chart's 1.2.0-alpha.0 unmarked."
  (or (eq flag t) (and (cdr (artifacthub--version-parts version)) t)))

(defun artifacthub--pre< (a b)
  "Non-nil when pre-release part A sorts before B, the semver way."
  (let ((xs (split-string a "\\.")) (ys (split-string b "\\.")) (result 'equal))
    (while (and (eq result 'equal) (or xs ys))
      (let ((x (pop xs)) (y (pop ys)))
        (setq result
              (cond ((null x) t)
                    ((null y) nil)
                    ((equal x y) 'equal)
                    ((and (string-match-p "\\`[0-9]+\\'" x) (string-match-p "\\`[0-9]+\\'" y))
                     (< (string-to-number x) (string-to-number y)))
                    ((string-match-p "\\`[0-9]+\\'" x) t)
                    ((string-match-p "\\`[0-9]+\\'" y) nil)
                    (t (string< x y))))))
    (eq result t)))

(defun artifacthub--version< (a b)
  "Non-nil when version A sorts before B: by number, a pre-release before
its release.  One that is not semver-like sorts first."
  (let ((pa (artifacthub--version-parts a)) (pb (artifacthub--version-parts b)))
    (cond
     ((null pa) (and pb t))
     ((null pb) nil)
     (t
      (let ((xs (car pa)) (ys (car pb)) (result 'equal))
        (while (and (eq result 'equal) (or xs ys))
          (let ((x (or (pop xs) 0)) (y (or (pop ys) 0)))
            (unless (= x y) (setq result (< x y)))))
        (if (not (eq result 'equal))
            result
          (let ((x (cdr pa)) (y (cdr pb)))
            (cond ((and x y) (artifacthub--pre< x y))
                  (x t)
                  (t nil)))))))))

(defun artifacthub--sorted-versions (detail)
  "DETAIL's available versions, the newest first."
  (sort (copy-sequence (alist-get 'available_versions detail))
        (lambda (a b) (artifacthub--version< (alist-get 'version b)
                                             (alist-get 'version a)))))

(defun artifacthub--latest-stable (detail)
  "The newest of DETAIL's versions that is not a pre-release; else its newest."
  (let ((versions (artifacthub--sorted-versions detail)))
    (alist-get 'version
               (or (cl-find-if-not (lambda (v)
                                     (artifacthub--prerelease-p
                                      (alist-get 'version v) (alist-get 'prerelease v)))
                                   versions)
                   (car versions)))))


;;; HTTP — url-retrieve, a timeout, a cache and dropped stale answers

(defvar artifacthub--http-function #'artifacthub--url-retrieve
  "Function (URL CALLBACK) that GETs URL without blocking.
CALLBACK is called once with (:status CODE :headers ALIST :body TEXT),
or (:error TEXT).  Tests replace it.")

(defvar artifacthub--cache (make-hash-table :test #'equal)
  "URL -> (TIME TTL . VALUE): a parsed answer and when it was got.")

(defvar artifacthub--inflight (make-hash-table :test #'equal)
  "URL -> (TIME . CALLBACKS) of a request under way.
TIME bounds it: one older than the timeout is taken as lost, never
waited on forever.")

(defun artifacthub--read-reply ()
  "The reply in the current url.el buffer.
The shape is the one `artifacthub--http-function' gives."
  (let ((status (bound-and-true-p url-http-response-status))
        (end (bound-and-true-p url-http-end-of-headers)))
    (if (not (and (integerp status) end))
        (list :error "no reply")
      (let ((headers nil)
            (body-start (save-excursion
                          (goto-char end)
                          (when (eq (char-after) ?\n) (forward-char 1))
                          (point))))
        (save-excursion
          (goto-char (point-min))
          (forward-line 1)
          (while (re-search-forward "^\\([^:\r\n]+\\):[ \t]*\\([^\r\n]*\\)" end t)
            (push (cons (downcase (match-string 1)) (match-string 2)) headers)))
        (list :status status
              :headers (nreverse headers)
              :body (decode-coding-string
                     (buffer-substring-no-properties body-start (point-max))
                     'utf-8))))))

(defun artifacthub--url-retrieve (url callback)
  "GET URL with `url-retrieve', never blocking; CALLBACK gets the reply once.
After `artifacthub-timeout' seconds the request is killed and CALLBACK
gets (:error \"timed out\")."
  (let ((done nil) (timer nil) (buffer nil))
    (cl-flet ((finish (reply)
                (unless done
                  (setq done t)
                  (when timer (cancel-timer timer))
                  (when (buffer-live-p buffer)
                    (when-let* ((proc (get-buffer-process buffer)))
                      (delete-process proc))
                    (kill-buffer buffer))
                  (funcall callback reply))))
      (setq timer (run-at-time artifacthub-timeout nil
                               (lambda () (finish (list :error "timed out")))))
      (condition-case err
          (let ((url-request-extra-headers
                 '(("Accept" . "application/json, application/yaml;q=0.9")))
                (url-show-status nil))
            (setq buffer
                  (url-retrieve url
                                (lambda (status)
                                  (finish
                                   (if (and (plist-get status :error)
                                            (not (bound-and-true-p url-http-response-status)))
                                       (list :error (error-message-string
                                                     (plist-get status :error)))
                                     (artifacthub--read-reply))))
                                nil t t)))
        (error (finish (list :error (error-message-string err))))))))

(defun artifacthub--reply-error (reply)
  "Why REPLY failed, as text, or nil when it did not."
  (let ((status (plist-get reply :status)))
    (cond ((plist-get reply :error))
          ((eql status 429) "HTTP 429: rate limited, wait a moment")
          ((not (and (integerp status) (<= 200 status 299)))
           (format "HTTP %s" status)))))

(defun artifacthub--now ()
  (float-time))

(defun artifacthub--fresh-p (hit)
  "Non-nil while HIT, a cache entry, is younger than its TTL."
  (< (- (artifacthub--now) (car hit)) (cadr hit)))

(defun artifacthub--cached (url)
  "The cached value of URL while it is fresh, as (VALUE), else nil."
  (when-let* ((hit (gethash url artifacthub--cache)))
    (if (artifacthub--fresh-p hit)
        (list (cddr hit))
      (remhash url artifacthub--cache)
      nil)))

(defun artifacthub--cache-put (url ttl value)
  "Keep VALUE for URL for TTL seconds; drop what has expired meanwhile."
  (maphash (lambda (key hit)
             (unless (artifacthub--fresh-p hit)
               (remhash key artifacthub--cache)))
           artifacthub--cache)
  (puthash url (cons (artifacthub--now) (cons ttl value)) artifacthub--cache))

(defun artifacthub--get (url parse ttl callback)
  "Call CALLBACK with (VALUE ERROR) for URL, VALUE the reply through PARSE.
A value got less than TTL seconds ago is given at once, without a
request; two asks for one URL share its request."
  (if-let* ((hit (artifacthub--cached url)))
      (funcall callback (car hit) nil)
    (let ((flight (gethash url artifacthub--inflight)))
      (if (and flight (< (- (artifacthub--now) (car flight))
                         (+ artifacthub-timeout 5)))
          (setcdr flight (cons callback (cdr flight)))
        (puthash url (list (artifacthub--now) callback) artifacthub--inflight)
        (funcall artifacthub--http-function url
                 (lambda (reply)
                   (let* ((callbacks (cdr (gethash url artifacthub--inflight)))
                          (err (artifacthub--reply-error reply))
                          (value nil))
                     (remhash url artifacthub--inflight)
                     (unless err
                       (condition-case e
                           (progn
                             (setq value (funcall parse reply))
                             (artifacthub--cache-put url ttl value))
                         (error (setq err (format "bad reply: %s"
                                                  (error-message-string e))))))
                     (dolist (cb (reverse callbacks))
                       (funcall cb value err)))))))))

(defun artifacthub--json (text)
  (json-parse-string text :object-type 'alist :array-type 'list
                     :null-object nil :false-object nil))

(defun artifacthub--api (path &rest params)
  "The API URL of PATH with query PARAMS, a plist of names and values."
  (concat artifacthub-url "/api/v1" path
          (when params
            (concat "?" (mapconcat
                         (lambda (pair)
                           (concat (substring (symbol-name (car pair)) 1) "="
                                   (url-hexify-string (format "%s" (cadr pair)))))
                         (seq-partition params 2)
                         "&")))))

(defun artifacthub--search-url (query kind offset)
  (artifacthub--api "/packages/search"
                    :ts_query_web query :kind kind
                    :facets (if (zerop offset) "true" "false")
                    :sort "relevance" :limit artifacthub-page-size :offset offset))

(defun artifacthub--parse-search (reply)
  "A search REPLY as (:packages LIST :total N :facets KINDS)."
  (let* ((data (artifacthub--json (plist-get reply :body)))
         (total (cdr (assoc "pagination-total-count" (plist-get reply :headers))))
         (facet (cl-find "kind" (alist-get 'facets data)
                         :key (lambda (f) (alist-get 'filter_key f)) :test #'equal)))
    (list :packages (alist-get 'packages data)
          :total (and total (string-to-number total))
          :facets (mapcar (lambda (o) (cons (alist-get 'id o) (alist-get 'total o)))
                          (alist-get 'options facet)))))

(defun artifacthub--detail (pkg callback)
  "Call CALLBACK with (DETAIL ERROR) for Helm chart PKG: its versions and more."
  (artifacthub--get (artifacthub--api (format "/packages/helm/%s/%s"
                                              (url-hexify-string (artifacthub--repo pkg 'name))
                                              (url-hexify-string (alist-get 'normalized_name pkg))))
                    (lambda (reply) (artifacthub--json (plist-get reply :body)))
                    artifacthub-cache-ttl callback))

(defun artifacthub--values (pkg version callback)
  "Call CALLBACK with (TEXT ERROR): the default values.yaml of PKG at VERSION.
A version's values never change, so they are kept for the session."
  (artifacthub--get (artifacthub--api (format "/packages/%s/%s/values"
                                              (alist-get 'package_id pkg)
                                              (url-hexify-string version)))
                    (lambda (reply) (plist-get reply :body))
                    most-positive-fixnum callback))


;;; Search state

(defvar artifacthub--query "")
(defvar artifacthub--kind 0)
(defvar artifacthub--packages nil
  "The packages listed, in order.")
(defvar artifacthub--total nil
  "How many packages of the kind match the query, or nil before an answer.")
(defvar artifacthub--facets nil
  "Alist of (KIND . COUNT): the query's matches per kind, the API's order.")
(defvar artifacthub--wanted nil
  "(QUERY KIND OFFSET) of the page the list waits for, or nil.
An answer for any other is stale: it is cached, never shown.")
(defvar artifacthub--error nil
  "Why the last search failed, shown on the footer.")
(defvar artifacthub--timer nil
  "The debounce timer.")
(defvar artifacthub--picked (make-hash-table :test #'equal)
  "PACKAGE-ID -> the version picked with V, for this Emacs session.")
(defvar artifacthub--busy nil
  "What the overlay is fetching for an action, shown on the footer.")
(defvar artifacthub--index 0
  "Index in `artifacthub--packages' of the selected package.")
(defvar artifacthub--top 0
  "Index of the first package the list shows.
The list shows only the rows that fit, so the preview under them
stays in view however many are loaded.")

(defun artifacthub--search-query ()
  (string-trim artifacthub--query))

(defun artifacthub--request (offset)
  "Ask for the page at OFFSET of the query in the kind.
The list takes it only when it is still the page wanted as it lands."
  (let* ((query (artifacthub--search-query))
         (kind artifacthub--kind)
         (key (list query kind offset)))
    (setq artifacthub--wanted key
          artifacthub--error nil)
    (artifacthub--get (artifacthub--search-url query kind offset)
                      #'artifacthub--parse-search artifacthub-cache-ttl
                      (lambda (page err)
                        (when (equal key artifacthub--wanted)
                          (setq artifacthub--wanted nil)
                          (if err
                              (setq artifacthub--error err)
                            (artifacthub--take-page page offset))
                          (artifacthub--refresh))))))

(defun artifacthub--take-page (page offset)
  "Show PAGE, the one at OFFSET: the first replaces the list, the rest add to it."
  (if (zerop offset)
      (setq artifacthub--packages (plist-get page :packages)
            artifacthub--total (plist-get page :total)
            artifacthub--facets (plist-get page :facets)
            artifacthub--index 0
            artifacthub--top 0)
    (let ((ids (mapcar (lambda (p) (alist-get 'package_id p)) artifacthub--packages)))
      (setq artifacthub--packages
            (append artifacthub--packages
                    (cl-remove-if (lambda (p) (member (alist-get 'package_id p) ids))
                                  (plist-get page :packages)))
            artifacthub--total (or (plist-get page :total) artifacthub--total)))))

(defun artifacthub--cancel-timer ()
  (when artifacthub--timer
    (cancel-timer artifacthub--timer)
    (setq artifacthub--timer nil)))

(defun artifacthub--query-changed ()
  "Search the query as it now is: at once when cached, else after a pause."
  (artifacthub--cancel-timer)
  (let ((query (artifacthub--search-query)))
    (cond
     ((< (length query) artifacthub-min-input)
      (setq artifacthub--packages nil
            artifacthub--total nil
            artifacthub--facets nil
            artifacthub--wanted nil
            artifacthub--error nil))
     ((artifacthub--cached (artifacthub--search-url query artifacthub--kind 0))
      (artifacthub--request 0))
     (t
      ;; Wanted from now, so an answer to the old query drops.
      (setq artifacthub--wanted (list query artifacthub--kind 0)
            artifacthub--timer (run-at-time artifacthub-debounce nil
                                            (lambda ()
                                              (setq artifacthub--timer nil)
                                              (artifacthub--request 0)))))))
  (artifacthub--refresh))


;;; YAML edits — on the text, never through a YAML library
;;
;; A Chart.yaml or helmfile.yaml is edited as lines, so every other
;; line, comment and blank line stays as it was; a reader and writer
;; would drop the comments and reflow the rest.  An entry is added in
;; the style of the ones already there: their dash and key columns,
;; their field order, their quoting, and a blank line between them when
;; they have one.

(defconst artifacthub-yaml--item-regexp "\\( *\\)-\\(?:\\( +\\)\\|$\\)"
  "A sequence item's first line: its indentation, then the gap after its dash.")

(defun artifacthub-yaml--blank-p ()
  "Non-nil when the line at point is blank or only a comment."
  (looking-at-p "[ \t]*\\(?:#.*\\)?$"))

(defun artifacthub-yaml--documents ()
  "The buffer's YAML documents as (BEG . END), in order."
  (save-excursion
    (goto-char (point-min))
    (let ((docs nil) (beg (point-min)))
      (while (re-search-forward "^\\(?:---\\|\\.\\.\\.\\)\\(?:[ \t].*\\)?$" nil t)
        (push (cons beg (line-beginning-position)) docs)
        (setq beg (min (point-max) (1+ (line-end-position)))))
      (nreverse (cons (cons beg (point-max)) docs)))))

(defun artifacthub-yaml--find-key (key doc)
  "Start of the line opening top-level KEY in DOC, or nil."
  (save-excursion
    (goto-char (car doc))
    (let ((case-fold-search nil))
      (when (re-search-forward (format "^%s:" (regexp-quote key)) (cdr doc) t)
        (line-beginning-position)))))

(defun artifacthub-yaml--unquote (raw)
  "RAW, a scalar as written, without its comment and quotes."
  (let ((value (string-trim (car (ghostel-cmd--compose-split-comment (or raw ""))))))
    (if (and (>= (length value) 2)
             (memq (aref value 0) '(?\" ?\'))
             (eq (aref value (1- (length value))) (aref value 0)))
        (substring value 1 -1)
      value)))

(defun artifacthub-yaml--field (item key)
  "ITEM's value of KEY, unquoted, or nil."
  (when-let* ((raw (cdr (assoc key (plist-get item :fields)))))
    (artifacthub-yaml--unquote raw)))

(defun artifacthub-yaml--block (key-pos end)
  "The sequence under the top-level key whose line starts at KEY-POS.
Read up to END, or to the next line at column 0 that is not one of
its items.  A plist: :items, each (:beg :fields), :fields an alist of
the item's own keys to their values as written; :dash and :key, the
columns of the first item's dash and keys; :last, the end of the last
line that is not blank or a comment; :spaced, non-nil when a blank
line parts two items."
  (save-excursion
    (goto-char key-pos)
    (let ((last (line-end-position))
          (items nil) (dash nil) (keycol nil) (spaced nil) (blank nil) (item nil))
      (forward-line 1)
      (while (and (< (point) end)
                  (not (eobp))
                  (or (artifacthub-yaml--blank-p)
                      (> (current-indentation) 0)
                      (and (looking-at-p artifacthub-yaml--item-regexp)
                           (not (looking-at-p "---")))))
        (if (artifacthub-yaml--blank-p)
            (when (looking-at-p "[ \t]*$") (setq blank t))
          (let ((indent (current-indentation)))
            (cond
             ((and (looking-at artifacthub-yaml--item-regexp)
                   (or (null dash) (= indent dash)))
              (when (and items blank) (setq spaced t))
              (setq dash indent
                    item (list :beg (line-beginning-position) :fields nil))
              (push item items)
              (let ((col (and (match-beginning 2) (- (match-end 2) (line-beginning-position)))))
                (when col
                  (unless keycol (setq keycol col))
                  (when (looking-at (concat artifacthub-yaml--item-regexp
                                            "\\([A-Za-z0-9_.-]+\\):\\(?:[ \t]+\\(.*\\)\\)?$"))
                    (plist-put item :fields
                               (list (cons (match-string 3) (or (match-string 4) ""))))))))
             ((and item (looking-at "\\( *\\)\\([A-Za-z0-9_.-]+\\):\\(?:[ \t]+\\(.*\\)\\)?$"))
              (unless keycol (setq keycol indent))
              (when (= indent keycol)
                (plist-put item :fields
                           (append (plist-get item :fields)
                                   (list (cons (match-string 2)
                                               (or (match-string 3) ""))))))))
            (setq last (line-end-position) blank nil)))
        (forward-line 1))
      (list :items (nreverse items) :dash dash :key keycol :last last :spaced spaced))))

(defun artifacthub-yaml--sequence-style (doc)
  "(DASH . KEY) columns of the first top-level key's sequence in DOC, or nil."
  (save-excursion
    (goto-char (car doc))
    (let (style)
      (while (and (not style)
                  (re-search-forward "^[A-Za-z0-9_.-]+:[ \t]*\\(?:#.*\\)?$" (cdr doc) t))
        (let ((block (artifacthub-yaml--block (line-beginning-position) (cdr doc))))
          (when (plist-get block :items)
            (setq style (cons (plist-get block :dash)
                              (or (plist-get block :key)
                                  (+ 2 (plist-get block :dash))))))))
      style)))

(defun artifacthub-yaml--needs-quotes-p (value)
  "Non-nil when VALUE would not read back as the same string unquoted."
  (let ((case-fold-search t))
    (or (string-empty-p value)
        (string-match-p "\\`[-+]?\\(?:[0-9][0-9_]*\\(?:\\.[0-9_]*\\)?\\|\\.[0-9]+\\)\\(?:e[-+]?[0-9]+\\)?\\'" value)
        (string-match-p "\\`\\(?:true\\|false\\|yes\\|no\\|on\\|off\\|null\\|~\\|y\\|n\\)\\'" value)
        (string-match-p "\\`[][{}#&*!|>'\"%@`,?:-]" value)
        (string-match-p ": \\| #\\|\\`[ \t]\\|[ \t]\\'" value))))

(defun artifacthub-yaml--scalar (value quote)
  "VALUE written with QUOTE, a quote character or nil for none when it can."
  (cond ((eq quote ?\')
         (concat "'" (string-replace "'" "''" value) "'"))
        ((or (eq quote ?\") (artifacthub-yaml--needs-quotes-p value))
         (concat "\"" (string-replace "\"" "\\\"" (string-replace "\\" "\\\\" value)) "\""))
        (t value)))

(defun artifacthub-yaml--fields (values order items)
  "VALUES, an alist of field names to plain values, as text to write.
Ordered as the first of ITEMS orders them, else by ORDER, each quoted
as the first item holding that field quotes it.  A symbol value, such
as true, is written as its name, never quoted."
  (let* ((ref (mapcar #'car (plist-get (car items) :fields)))
         (rank (lambda (k) (or (cl-position k ref :test #'equal)
                               (+ 100 (or (cl-position k order :test #'equal) 50))))))
    (mapcar (lambda (pair)
              (let* ((raw (cl-some (lambda (item)
                                     (cdr (assoc (car pair) (plist-get item :fields))))
                                   items))
                     (quote (and raw (> (length raw) 0)
                                 (memq (aref raw 0) '(?\" ?\'))
                                 (aref raw 0))))
                (cons (car pair)
                      (if (symbolp (cdr pair))
                          (symbol-name (cdr pair))
                        (artifacthub-yaml--scalar (cdr pair) quote)))))
            (cl-stable-sort (copy-sequence values) #'<
                            :key (lambda (pair) (funcall rank (car pair)))))))

(defun artifacthub-yaml--item-text (fields dash keycol)
  "Lines of an item holding FIELDS, ((NAME . TEXT)...): its dash at
column DASH, its keys at KEYCOL.  No final newline."
  (let ((gap (make-string (max 1 (- keycol dash 1)) ?\s))
        (first t))
    (mapconcat (lambda (pair)
                 (prog1 (concat (if first
                                    (concat (make-string dash ?\s) "-" gap)
                                  (make-string keycol ?\s))
                                (car pair) ": " (cdr pair))
                   (setq first nil)))
               fields "\n")))

(defun artifacthub-yaml--open-line-at (pos)
  "Go to POS, the start of a line, after a blank line; return point."
  (goto-char pos)
  (unless (or (bobp) (eq (char-before) ?\n))
    (insert "\n"))
  (unless (or (bobp)
              (save-excursion (forward-line -1) (looking-at-p "[ \t]*$")))
    (insert "\n"))
  (point))

(defun artifacthub-yaml--append (doc key values order style &optional before)
  "Add an item of VALUES to top-level sequence KEY of DOC; return (BEG . END).
VALUES is an alist of field names to plain values; ORDER and STYLE,
(DASH . KEY) columns, are used when nothing in the file says.  A
missing KEY is opened before the line at BEFORE, else at DOC's end."
  (let ((kpos (artifacthub-yaml--find-key key doc)))
    (if (null kpos)
        (let* ((style (or (artifacthub-yaml--sequence-style doc) style))
               (text (artifacthub-yaml--item-text
                      (artifacthub-yaml--fields values order nil) (car style) (cdr style)))
               (at (artifacthub-yaml--open-line-at (or before (cdr doc))))
               (beg (+ at (length key) 2)))
          (insert key ":\n" text "\n")
          (when before (insert "\n"))
          (cons beg (+ beg (length text))))
      (goto-char kpos)
      (unless (looking-at (format "%s:[ \t]*\\(\\[[ \t]*\\][ \t]*\\)?\\(?:#.*\\)?$"
                                  (regexp-quote key)))
        (user-error "`%s:' is not a block sequence here; add it by hand" key))
      (when (match-beginning 1)
        (goto-char (match-beginning 1))
        (delete-region (match-beginning 1) (match-end 1))
        (delete-horizontal-space))
      (let* ((block (artifacthub-yaml--block kpos (cdr doc)))
             (items (plist-get block :items))
             (style (if items
                        (cons (plist-get block :dash)
                              (or (plist-get block :key) (+ 2 (plist-get block :dash))))
                      (or (artifacthub-yaml--sequence-style doc) style)))
             (text (artifacthub-yaml--item-text
                    (artifacthub-yaml--fields values order items) (car style) (cdr style))))
        (goto-char (if items (plist-get block :last) (save-excursion (goto-char kpos)
                                                                    (line-end-position))))
        (insert "\n" (if (plist-get block :spaced) "\n" ""))
        (let ((beg (point)))
          (insert text)
          (cons beg (point)))))))

(defun artifacthub-yaml--items (key doc)
  "The items of top-level sequence KEY of DOC."
  (when-let* ((kpos (artifacthub-yaml--find-key key doc)))
    (plist-get (artifacthub-yaml--block kpos (cdr doc)) :items)))

(defun artifacthub-yaml-add-dependency (name version repository)
  "Add the dependency NAME at VERSION from REPOSITORY to the Chart.yaml
in the current buffer, opening `dependencies:' when it has none.
Return (BEG END ADDED): where the entry is, and nil for ADDED when
one with NAME from REPOSITORY was there already."
  (let* ((doc (car (artifacthub-yaml--documents)))
         (same (cl-find-if (lambda (item)
                             (and (equal (artifacthub-yaml--field item "name") name)
                                  (equal (artifacthub--url-key
                                          (or (artifacthub-yaml--field item "repository") ""))
                                         (artifacthub--url-key repository))))
                           (artifacthub-yaml--items "dependencies" doc))))
    (if same
        (save-excursion
          (goto-char (plist-get same :beg))
          (list (point) (line-end-position) nil))
      (let ((range (artifacthub-yaml--append
                    doc "dependencies"
                    `(("name" . ,name) ("version" . ,version) ("repository" . ,repository))
                    '("name" "version" "repository")
                    '(2 . 4))))
        (list (car range) (cdr range) t)))))

(defun artifacthub-yaml--helmfile-doc ()
  "The document of a helmfile that holds its releases, else its repositories,
else its last."
  (let ((docs (artifacthub-yaml--documents)))
    (or (cl-find-if (lambda (d) (artifacthub-yaml--find-key "releases" d)) docs)
        (cl-find-if (lambda (d) (artifacthub-yaml--find-key "repositories" d)) docs)
        (car (last docs)))))

(defun artifacthub-yaml--unique (name taken)
  "NAME, or NAME-2, NAME-3… when TAKEN has it."
  (let ((n 1) (candidate name))
    (while (member candidate taken)
      (setq n (1+ n) candidate (format "%s-%d" name n)))
    candidate))

(defun artifacthub-yaml-add-release (chart version repo url &optional oci)
  "Add a release of CHART at VERSION to the helmfile in the current buffer.
Its chart comes from the repository with URL: the one the helmfile
lists with that URL, else REPO, added under `repositories:' (as an OCI
one when OCI).  Return (BEG END REPO-USED REPO-ADDED)."
  (let* ((doc (artifacthub-yaml--helmfile-doc))
         (repos (artifacthub-yaml--items "repositories" doc))
         (known (cl-find-if (lambda (item)
                              (equal (artifacthub--url-key
                                      (or (artifacthub-yaml--field item "url") ""))
                                     (artifacthub--url-key url)))
                            repos))
         (local (if known
                    (artifacthub-yaml--field known "name")
                  (artifacthub-yaml--unique
                   repo (mapcar (lambda (i) (artifacthub-yaml--field i "name")) repos)))))
    (unless known
      (artifacthub-yaml--append
       doc "repositories"
       (append `(("name" . ,local)
                 ("url" . ,(if oci (replace-regexp-in-string "\\`oci://" "" url) url)))
               (and oci '(("oci" . true))))
       '("name" "url" "oci")
       '(0 . 2)
       (artifacthub-yaml--find-key "releases" doc))
      (setq doc (artifacthub-yaml--helmfile-doc)))
    (let* ((releases (artifacthub-yaml--items "releases" doc))
           (name (artifacthub-yaml--unique
                  chart (mapcar (lambda (i) (artifacthub-yaml--field i "name")) releases)))
           (range (artifacthub-yaml--append
                   doc "releases"
                   `(("name" . ,name) ("chart" . ,(concat local "/" chart)) ("version" . ,version))
                   '("name" "namespace" "chart" "version")
                   '(0 . 2))))
      (list (car range) (cdr range) local (not known)))))


;;; Where the overlay was opened

(defvar artifacthub--caller nil
  "Plist of where the overlay was opened: :buffer :root :target.
:target is chart, helmfile, terminal or copy.")

(defun artifacthub--chart-file-p (file)
  (member (file-name-nondirectory file) '("Chart.yaml" "requirements.yaml")))

(defun artifacthub--helmfile-p (file)
  (let ((name (file-name-nondirectory file)))
    (or (string-match-p "\\`helmfile\\(?:\\.[^/]*\\)?\\.ya?ml\\(?:\\.gotmpl\\)?\\'" name)
        (and (equal (file-name-nondirectory
                     (directory-file-name (file-name-directory file)))
                    "helmfile.d")
             (string-match-p "\\.\\(?:ya?ml\\|gotmpl\\)\\'" name)))))

(defun artifacthub--target (buffer)
  "What RET does for a chart, opened from BUFFER."
  (with-current-buffer buffer
    (let ((file buffer-file-name))
      (cond ((derived-mode-p 'ghostel-mode) 'terminal)
            ((or (null file) buffer-read-only) 'copy)
            ((artifacthub--chart-file-p file) 'chart)
            ((artifacthub--helmfile-p file) 'helmfile)
            (t 'copy)))))

(defun artifacthub--target-verb ()
  (pcase (plist-get artifacthub--caller :target)
    ('chart "add dependency")
    ('helmfile "add release")
    ('terminal "helm install")
    (_ "copy")))


;;; Overlay state

(defconst artifacthub--buffer-name "*artifacthub*")

(defvar artifacthub--input-on nil
  "Non-nil while typing goes to the query.")
(defvar artifacthub--help-visible nil)
(defvar artifacthub--preview-start nil)
(defvar artifacthub--preview-id nil)
(defvar artifacthub--rows nil
  "Rows the overlay has room for; nil to take them from its window.")
(defvar artifacthub--posframe-parent nil)
(defvar artifacthub--width nil
  "Character width the overlay is laid out for.")

(defconst artifacthub--preview-lines 4
  "Lines under the list's separator: a package's preview, the help, or a fill.")

(defconst artifacthub-help
  '(("type"       "The query; RET goes to the list, / back to typing")
    ("TAB, S-TAB" "Next / previous kind (with its count)")
    ("j, k"       "Move (C-n / C-p while typing)")
    ("RET"        "Chart.yaml: add a dependency; helmfile: add a release;")
    (""           "  terminal: helm install, filling {{vars}}; else copy")
    ("C-RET"      "In a terminal: paste and run")
    ("V"          "Pick the version (default: latest non-pre-release)")
    ("v"          "That version's default values.yaml")
    ("o, C-u o"   "Open the page in the browser / an xwidget")
    ("y, Y"       "Copy repo/chart --version (a non-Helm: its URL) / the URL")
    ("n"          "Load more")
    ("g"          "Search again, past the cache")
    ("C-y, C-\\"  "Yank into the input / type it with an input method")
    ("✓ ◆"        "Verified publisher / official")
    ("q, Esc"     "Quit; while filling, cancel"))
  "Key legend for the overlay.")


;;; Table

(defun artifacthub--format (width)
  "Tabulated-list format for WIDTH columns."
  (let* ((budget (max 60 (1- width)))
         (name (if (< budget 90) 18 24))
         (repo (if (< budget 90) 16 22))
         (version 18)
         (stars 5)
         (desc (max 10 (- budget name repo version stars 5))))
    (vector (list "Name" name nil)
            (list "Repository" repo nil)
            (list "Version (app)" version nil)
            (list "★" stars nil :right-align t)
            (list "Description" desc nil))))

(defun artifacthub--repo-cell (pkg width)
  "PKG's repository in WIDTH columns: its display name, cut so the
verified / official marks after it always show."
  (let ((marks (concat (if (artifacthub--repo pkg 'verified_publisher)
                           (propertize " ✓" 'face 'success) "")
                       (if (artifacthub--official-p pkg)
                           (propertize " ◆" 'face 'warning) ""))))
    (concat (ghostel-cmd--truncate
             (or (artifacthub--repo pkg 'display_name) (artifacthub--repo pkg 'name) "")
             (- width (string-width marks)))
            marks)))

(defun artifacthub--version-cell (pkg)
  "PKG's version and app version; a version picked with V in bold."
  (let* ((picked (gethash (alist-get 'package_id pkg) artifacthub--picked))
         (app (alist-get 'app_version pkg)))
    (concat (if picked
                (propertize picked 'face 'bold)
              (or (alist-get 'version pkg) ""))
            (if (and app (not picked)) (format " (%s)" app) ""))))

(defun artifacthub--entry (pkg widths)
  "The tabulated-list entry of PKG, cells cut to WIDTHS."
  (let* ((dim (alist-get 'deprecated pkg))
         (cells (list (propertize (or (alist-get 'name pkg) "")
                                  'face 'font-lock-keyword-face)
                      (artifacthub--repo-cell pkg (nth 1 widths))
                      (artifacthub--version-cell pkg)
                      (number-to-string (or (alist-get 'stars pkg) 0))
                      (propertize (string-replace "\n" " " (or (alist-get 'description pkg) ""))
                                  'face 'font-lock-comment-face))))
    (list (alist-get 'package_id pkg)
          (apply #'vector
                 (cl-mapcar (lambda (cell width)
                              (let ((cell (ghostel-cmd--truncate cell width)))
                                (if dim (propertize cell 'face 'shadow) cell)))
                            cells widths)))))

(defun artifacthub--table-height ()
  "How many rows the list shows; fewer while the help takes their room."
  (let ((rows (or artifacthub--rows
                  (when-let* ((buf (get-buffer artifacthub--buffer-name))
                              (win (get-buffer-window buf t)))
                    (max 4 (- (window-body-height win) artifacthub--preview-lines 1)))
                  artifacthub-page-size)))
    (if artifacthub--help-visible
        (max 1 (- rows (- (length artifacthub-help) artifacthub--preview-lines)))
      rows)))

(defun artifacthub--build-entries ()
  "List the rows that fit around the selected package."
  (let* ((fmt (setq tabulated-list-format (artifacthub--format (artifacthub--width))))
         (widths (mapcar #'cadr fmt))
         (count (length artifacthub--packages))
         (height (artifacthub--table-height)))
    (setq artifacthub--index (max 0 (min artifacthub--index (1- count)))
          artifacthub--top (ghostel-cmd--window-top
                            artifacthub--index artifacthub--top height count))
    (tabulated-list-init-header)
    (setq tabulated-list-entries
          (mapcar (lambda (pkg) (artifacthub--entry pkg widths))
                  (seq-subseq artifacthub--packages artifacthub--top
                              (min count (+ artifacthub--top height)))))))

(defun artifacthub--select (index)
  "Select the package at INDEX, scrolling the list when it is not shown."
  (let ((count (length artifacthub--packages)))
    (when (> count 0)
      (setq artifacthub--index (max 0 (min index (1- count))))
      (if (and (>= artifacthub--index artifacthub--top)
               (< artifacthub--index (+ artifacthub--top (length tabulated-list-entries))))
          (progn
            (goto-char (point-min))
            (forward-line (- artifacthub--index artifacthub--top)))
        (artifacthub--refresh)))))

(defun artifacthub-next (&optional n)
  "Select the Nth next package."
  (interactive "p")
  (artifacthub--select (+ artifacthub--index (or n 1))))

(defun artifacthub-previous (&optional n)
  "Select the Nth previous package."
  (interactive "p")
  (artifacthub--select (- artifacthub--index (or n 1))))

(defun artifacthub-first ()
  "Select the first package."
  (interactive)
  (artifacthub--select 0))

(defun artifacthub-last ()
  "Select the last package loaded."
  (interactive)
  (artifacthub--select (1- (length artifacthub--packages))))

(defun artifacthub--width ()
  (or artifacthub--width
      (when-let* ((buf (get-buffer artifacthub--buffer-name))
                  (win (get-buffer-window buf t)))
        (window-body-width win))
      artifacthub-sidebar-width))

(defun artifacthub--package-at-point ()
  "The package of the row at point, or nil."
  (when-let* ((id (and (derived-mode-p 'artifacthub-mode) (tabulated-list-get-id))))
    (cl-find id artifacthub--packages
             :key (lambda (p) (alist-get 'package_id p)) :test #'equal)))

(defun artifacthub--package ()
  "The package at point; signals when there is none."
  (or (artifacthub--package-at-point) (user-error "No package at point")))

(defun artifacthub--helm-package ()
  "The Helm chart at point; signals for another kind."
  (let ((pkg (artifacthub--package)))
    (unless (artifacthub--helm-p pkg)
      (user-error "%s is not a Helm chart: o opens it, y copies its URL"
                  (alist-get 'name pkg)))
    pkg))


;;; Preview and footer

(defun artifacthub--erase-preview ()
  (when (and artifacthub--preview-start (marker-position artifacthub--preview-start))
    (let ((inhibit-read-only t))
      (delete-region artifacthub--preview-start (point-max))))
  (setq artifacthub--preview-start nil))

(defun artifacthub--state-text ()
  "What the list is waiting for, or why it is empty."
  (cond
   (artifacthub--error (propertize artifacthub--error 'face 'error))
   ((< (length (artifacthub--search-query)) artifacthub-min-input)
    (format "Type at least %d characters to search Artifact Hub." artifacthub-min-input))
   (artifacthub--wanted "Searching…")
   (t (format "No %s package matches." (artifacthub--kind-label artifacthub--kind)))))

(defun artifacthub--preview-text (pkg width)
  "The lines under the list for PKG, WIDTH wide."
  (let ((fill ghostel-cmd--fill))
    (cond
     (artifacthub--help-visible
      (let ((w (apply #'max (mapcar (lambda (row) (string-width (car row))) artifacthub-help))))
        (mapconcat (lambda (row) (format (format "%%-%ds  %%s" w) (car row) (cadr row)))
                   artifacthub-help "\n")))
     ((artifacthub--own-fill-p)
      (ghostel-cmd--fill-text fill (plist-get (plist-get fill :record) :cmd)
                              (plist-get fill :root) width))
     ((null pkg)
      (propertize (artifacthub--state-text) 'face 'shadow))
     (t
      (let* ((helm (artifacthub--helm-p pkg))
             (picked (gethash (alist-get 'package_id pkg) artifacthub--picked))
             (head (concat
                    (propertize (if helm (artifacthub--chart-ref pkg)
                                  (format "%s/%s" (artifacthub--repo pkg 'name)
                                          (alist-get 'name pkg)))
                                'face 'bold)
                    "  " (or picked (alist-get 'version pkg) "")
                    (when-let* ((app (alist-get 'app_version pkg))) (concat " · app " app))
                    (format " · ★%d" (or (alist-get 'stars pkg) 0))
                    (when (alist-get 'deprecated pkg) (propertize " · deprecated" 'face 'error))
                    (when (alist-get 'signed pkg) " · signed")))
             (desc (ghostel-cmd--wrap (string-replace "\n" " " (or (alist-get 'description pkg) ""))
                                      width 2))
             (action (if helm
                         (format "RET %s · %s" (artifacthub--target-verb)
                                 (string-trim-right (artifacthub--repo pkg 'url) "/+"))
                       (format "%s · o open · y copy its URL" (artifacthub--kind-label
                                                              (artifacthub--kind pkg))))))
        (string-join (list (ghostel-cmd--truncate head width)
                           (propertize (string-join desc "\n") 'face 'font-lock-comment-face)
                           (propertize (ghostel-cmd--truncate action width) 'face 'shadow))
                     "\n"))))))

(defun artifacthub--draw-preview ()
  (when (derived-mode-p 'artifacthub-mode)
    (let ((inhibit-read-only t)
          (pkg (artifacthub--package-at-point))
          (width (max 20 (or (ignore-errors (window-body-width)) (artifacthub--width)))))
      (save-excursion
        (artifacthub--erase-preview)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (setq artifacthub--preview-start (point-marker))
        (insert (ghostel-cmd--rule width artifacthub--top
                                   (- (length artifacthub--packages) artifacthub--top
                                      (length tabulated-list-entries)))
                "\n"
                (artifacthub--preview-text pkg (1- width)))
        (unless (bolp) (insert "\n")))
      (set-buffer-modified-p nil))))

(defun artifacthub--print ()
  (artifacthub--erase-preview)
  (tabulated-list-print)
  (goto-char (point-min))
  (forward-line (- artifacthub--index artifacthub--top))
  (artifacthub--draw-preview)
  (setq artifacthub--preview-id (tabulated-list-get-id))
  (when-let* ((win (get-buffer-window (current-buffer) t)))
    (set-window-start win (point-min))
    (set-window-point win (point))))

(defun artifacthub--refresh ()
  "Rebuild the list from the search state and redraw the overlay."
  (when-let* ((buf (get-buffer artifacthub--buffer-name)))
    (with-current-buffer buf
      (when (derived-mode-p 'artifacthub-mode)
        (artifacthub--build-entries)
        (artifacthub--print)))
    (force-mode-line-update t)))

(defun artifacthub--redraw ()
  "Redraw the preview and footer, keeping the list."
  (when-let* ((buf (get-buffer artifacthub--buffer-name)))
    (with-current-buffer buf
      (artifacthub--draw-preview))
    (force-mode-line-update t)))

(defun artifacthub--confine-point ()
  (when (and artifacthub--preview-start
             (marker-position artifacthub--preview-start)
             (>= (point) artifacthub--preview-start)
             (> artifacthub--preview-start (point-min)))
    (goto-char (1- artifacthub--preview-start))
    (beginning-of-line)))

(defun artifacthub--on-command ()
  (when (derived-mode-p 'artifacthub-mode)
    (artifacthub--confine-point)
    (when (tabulated-list-get-id)
      ;; Point moved by other means, a click say: select what it is on.
      (setq artifacthub--index (+ artifacthub--top (1- (line-number-at-pos)))))
    (unless (or artifacthub--help-visible (artifacthub--own-fill-p))
      (let ((id (tabulated-list-get-id)))
        (unless (equal id artifacthub--preview-id)
          (setq artifacthub--preview-id id)
          (artifacthub--draw-preview))))))

(defun artifacthub--kinds-text ()
  "The query's kinds with their counts, the current one in brackets."
  (mapconcat (lambda (kind)
               (let ((text (format "%s %s" (artifacthub--kind-label kind)
                                   (or (alist-get kind artifacthub--facets) 0))))
                 (if (eql kind artifacthub--kind)
                     (propertize (format "[%s]" text) 'face 'bold)
                   text)))
             (artifacthub--kinds-offered) " · "))

(defun artifacthub--footer ()
  "The overlay's mode line; a % in it shows as itself."
  (string-replace
   "%" "%%"
   (concat
    " artifacthub  "
    (if artifacthub--input-on
        (propertize (concat "/" artifacthub--query "▏") 'face 'highlight)
      (concat "/" artifacthub--query))
    (cond (artifacthub--busy (concat "  " artifacthub--busy))
          (artifacthub--wanted "  searching…")
          (artifacthub--error (concat "  " (propertize artifacthub--error 'face 'error)))
          (artifacthub--total (format "  %d/%d" (length artifacthub--packages)
                                      artifacthub--total)))
    (when artifacthub--facets (concat "  " (artifacthub--kinds-text)))
    (cond
     ((artifacthub--own-fill-p) (ghostel-cmd--fill-footer ghostel-cmd--fill))
     (artifacthub--input-on "   RET list  TAB kind  C-n/C-p move  Esc close")
     (artifacthub--help-visible "   ? close help")
     ((not (eql artifacthub--kind 0))
      "   o open  y copy  n more  TAB kind  / search  ? keys")
     (t (format "   RET %s  V version  v values  o open  y copy  n more  / search  ? keys"
                (artifacthub--target-verb)))))))


;;; Typing the query

(defun artifacthub--set-query (query)
  (setq artifacthub--query query)
  (artifacthub--query-changed))

(defun artifacthub-input-self-insert ()
  "Type the key into the query."
  (interactive)
  (let ((ev last-command-event))
    (cond
     ((ghostel-cmd--return-event-p ev)
      (artifacthub-input-done))
     ((and (characterp ev) (>= ev 32) (not (eq ev 127)))
      (artifacthub--set-query (concat artifacthub--query (char-to-string ev)))))))

(defun artifacthub-input-backspace ()
  (interactive)
  (when (> (length artifacthub--query) 0)
    (artifacthub--set-query (substring artifacthub--query 0 -1))))

(defun artifacthub-input-clear ()
  (interactive)
  (artifacthub--set-query ""))

(defun artifacthub-input-backward-kill-word ()
  "Delete the query's last word."
  (interactive)
  (artifacthub--set-query
   (replace-regexp-in-string "[^ \t]*[ \t]*\\'" "" artifacthub--query)))

(defun artifacthub-input-yank ()
  "Append the latest kill to the query, as one line."
  (interactive)
  (artifacthub--set-query (concat artifacthub--query (ghostel-cmd--yank-text))))

(defun artifacthub-input-read ()
  "Edit the query in the minibuffer, where an input method works.
The list follows as you type; C-g puts the old query back."
  (interactive)
  (let ((old artifacthub--query))
    (condition-case nil
        (artifacthub--set-query
         (ghostel-cmd--read-line "Artifact Hub: " old #'artifacthub--set-query))
      (quit (artifacthub--set-query old)))))

(defun artifacthub--set-input (on)
  (setq artifacthub--input-on (and on t))
  (when-let* ((buf (get-buffer artifacthub--buffer-name)))
    (with-current-buffer buf
      (artifacthub-input-mode (if on 1 -1))
      (when (fboundp 'evil-normalize-keymaps)
        (evil-normalize-keymaps))))
  (force-mode-line-update t))

(defun artifacthub-input-done ()
  "Leave the query for the list."
  (interactive)
  (artifacthub--set-input nil))

(defun artifacthub-search-input ()
  "Type the query again."
  (interactive)
  (when (artifacthub--own-fill-p)
    (user-error "Finish or cancel the fill first"))
  (setq artifacthub--help-visible nil)
  (artifacthub--set-input t)
  (artifacthub--refresh))

(defvar-keymap artifacthub-input-map
  :doc "Keys while typing the query."
  "RET"         #'artifacthub-input-done
  "C-m"         #'artifacthub-input-done
  "<return>"    #'artifacthub-input-done
  "TAB"         #'artifacthub-next-kind
  "<tab>"       #'artifacthub-next-kind
  "<backtab>"   #'artifacthub-previous-kind
  "C-n"         #'artifacthub-next
  "C-p"         #'artifacthub-previous
  "<down>"      #'artifacthub-next
  "<up>"        #'artifacthub-previous
  "DEL"         #'artifacthub-input-backspace
  "<backspace>" #'artifacthub-input-backspace
  "<delete>"    #'artifacthub-input-backspace
  "C-h"         #'artifacthub-input-backspace
  "C-u"         #'artifacthub-input-clear
  "C-w"         #'artifacthub-input-backward-kill-word
  "M-DEL"       #'artifacthub-input-backward-kill-word
  "C-y"         #'artifacthub-input-yank
  "C-\\"        #'artifacthub-input-read
  "C-g"         #'artifacthub-quit
  "<escape>"    #'artifacthub-quit)

(define-key artifacthub-input-map [t] #'artifacthub-input-self-insert)
(define-key artifacthub-input-map [return] #'artifacthub-input-done)
(define-key artifacthub-input-map [kp-enter] #'artifacthub-input-done)
(ghostel-cmd--bind-printable-keys artifacthub-input-map #'artifacthub-input-self-insert)

(define-minor-mode artifacthub-input-mode
  "Type the Artifact Hub query on the overlay footer."
  :lighter nil
  :keymap artifacthub-input-map)


;;; Kinds, more, again

(defun artifacthub--kinds-offered ()
  "Kinds TAB cycles: Helm, the query's other kinds by count, the current one."
  (delete-dups (append (list 0) (mapcar #'car artifacthub--facets)
                       (list artifacthub--kind))))

(defun artifacthub--cycle-kind (step)
  (let* ((kinds (artifacthub--kinds-offered))
         (pos (or (cl-position artifacthub--kind kinds) 0))
         (next (nth (mod (+ pos step) (length kinds)) kinds)))
    (if (eql next artifacthub--kind)
        (message "No other kind matches %S" (artifacthub--search-query))
      (setq artifacthub--kind next
            artifacthub--packages nil
            artifacthub--total nil)
      (artifacthub--cancel-timer)
      (if (< (length (artifacthub--search-query)) artifacthub-min-input)
          (artifacthub--refresh)
        (artifacthub--request 0)
        (artifacthub--refresh)))))

(defun artifacthub-next-kind ()
  "Show the next kind of package, with its count."
  (interactive)
  (artifacthub--cycle-kind 1))

(defun artifacthub-previous-kind ()
  "Show the previous kind of package."
  (interactive)
  (artifacthub--cycle-kind -1))

(defun artifacthub-more ()
  "Load the next page of packages."
  (interactive)
  (cond
   (artifacthub--wanted (message "Still searching…"))
   ((null artifacthub--total) (message "Nothing to load"))
   ((>= (length artifacthub--packages) artifacthub--total)
    (message "All %d are listed" artifacthub--total))
   (t
    (artifacthub--request (length artifacthub--packages))
    (artifacthub--refresh))))

(defun artifacthub-again ()
  "Search the query again, past the cache."
  (interactive)
  (let ((query (artifacthub--search-query)))
    (when (>= (length query) artifacthub-min-input)
      (cl-loop for offset from 0 below (max 1 (length artifacthub--packages))
               by artifacthub-page-size
               do (remhash (artifacthub--search-url query artifacthub--kind offset)
                           artifacthub--cache))
      (artifacthub--request 0)
      (artifacthub--refresh))))


;;; Actions

(defun artifacthub--with-version (pkg fn)
  "Call FN with the version PKG's actions use.
That is the one picked with V, else the listed one when it is not a
pre-release, else the newest that is not, which needs PKG's versions
first: then FN runs when they land."
  (let ((picked (gethash (alist-get 'package_id pkg) artifacthub--picked))
        (listed (alist-get 'version pkg)))
    (if (or picked (not (artifacthub--prerelease-p listed)))
        (funcall fn (or picked listed))
      (artifacthub--fetching (format "%s's versions" (alist-get 'name pkg)))
      (artifacthub--detail pkg (lambda (detail err)
                                 (artifacthub--fetching nil)
                                 (if err
                                     (message "artifacthub: %s" err)
                                   (funcall fn (or (artifacthub--latest-stable detail)
                                                   listed))))))))

(defun artifacthub--fetching (what)
  "Show on the footer that WHAT is being fetched; nil when done."
  (setq artifacthub--busy (and what (format "fetching %s…" what)))
  (force-mode-line-update t))

(defun artifacthub--reference (pkg version)
  "PKG at VERSION as helm takes it: REPO/CHART --version V."
  (format "%s --version %s" (artifacthub--chart-ref pkg) version))

(defun artifacthub--copy (text)
  (kill-new text)
  (message "Copied %s" text))

(defun artifacthub--helm-command (pkg version)
  "The helm commands that install PKG at VERSION, {{release}} and
{{namespace}} left to fill."
  (let ((chart (alist-get 'name pkg))
        (version (ghostel-cmd--shell-arg version)))
    (if (artifacthub--oci-p pkg)
        (format "helm install {{release:%s}} %s --version %s -n {{namespace:default}}"
                chart (ghostel-cmd--shell-arg (artifacthub--chart-ref pkg)) version)
      (let ((repo (ghostel-cmd--shell-arg (artifacthub--repo pkg 'name))))
        (format (concat "helm repo add %s %s && helm repo update && "
                        "helm install {{release:%s}} %s/%s --version %s -n {{namespace:default}}")
                repo (ghostel-cmd--shell-arg (artifacthub--repo-url pkg))
                chart repo (ghostel-cmd--shell-arg chart) version)))))

(defun artifacthub--own-fill-p ()
  "Non-nil while ghostel-cmd's fill is filling for this overlay."
  (and ghostel-cmd--fill
       (eq (plist-get ghostel-cmd--fill :buffer) (get-buffer artifacthub--buffer-name))))

(defun artifacthub--fill-terminal (pkg version submit)
  "Fill and paste the helm commands for PKG at VERSION into the terminal.
ghostel-cmd's footer fill asks for {{release}} and {{namespace}},
remembering them per project and chart; SUBMIT presses Enter too."
  (let* ((terminal (plist-get artifacthub--caller :buffer))
         (record (list :name (format "helm install %s" (artifacthub--chart-ref pkg))
                       :cmd (artifacthub--helm-command pkg version)))
         (deliver (lambda (cmd submit)
                    (artifacthub--hide)
                    (ghostel-cmd--deliver cmd submit terminal))))
    (artifacthub--set-input nil)
    (if (ghostel-cmd--start-fill record (plist-get artifacthub--caller :root) 'artifacthub
                                 submit
                                 :buffer (get-buffer artifacthub--buffer-name)
                                 :redraw #'artifacthub--redraw
                                 :deliver deliver)
        (artifacthub--redraw)
      (funcall deliver (plist-get record :cmd) submit))))

(defun artifacthub--edit-caller (describe fn &rest args)
  "Hide the overlay and apply FN to ARGS in the buffer it was opened from.
FN returns (BEG END . MORE); point goes to BEG and the region flashes.
DESCRIBE gets that list and returns the message."
  (let ((buffer (plist-get artifacthub--caller :buffer)))
    (unless (buffer-live-p buffer)
      (user-error "The buffer the overlay was opened from is gone"))
    (artifacthub--hide)
    (let ((win (get-buffer-window buffer)))
      (if win (select-window win) (pop-to-buffer buffer))
      (with-current-buffer buffer
        (let ((result (with-undo-amalgamate (apply fn args))))
          (goto-char (car result))
          (set-window-point (selected-window) (car result))
          (pulse-momentary-highlight-region (car result) (cadr result))
          (message "%s" (funcall describe result)))))))

(defun artifacthub--use (pkg version submit)
  "Use PKG at VERSION where the overlay was opened."
  (let ((name (alist-get 'name pkg)))
    (pcase (plist-get artifacthub--caller :target)
      ('chart
       (artifacthub--edit-caller
        (lambda (r)
          (if (nth 2 r)
              (format "Added %s %s to the dependencies; helm dependency update fetches it"
                      name version)
            (format "Already a dependency: %s from %s (line %d)" name
                    (artifacthub--repo-url pkg) (line-number-at-pos (car r)))))
        #'artifacthub-yaml-add-dependency name version (artifacthub--repo-url pkg)))
      ('helmfile
       (artifacthub--edit-caller
        (lambda (r)
          (format "Added release %s/%s %s%s" (nth 2 r) name version
                  (if (nth 3 r) (format ", and repository %s" (nth 2 r)) "")))
        #'artifacthub-yaml-add-release name version (artifacthub--repo pkg 'name)
        (artifacthub--repo-url pkg) (artifacthub--oci-p pkg)))
      ('terminal
       (if (artifacthub--showing-p)
           (artifacthub--fill-terminal pkg version submit)
         (message "artifacthub: the overlay closed before %s's versions came" name)))
      (_
       (artifacthub--hide)
       (artifacthub--copy (artifacthub--reference pkg version))))))

(defun artifacthub-use (&optional submit)
  "Use the chart at point where the overlay was opened.
A Chart.yaml gets a dependency, a helmfile.yaml a release and its
repository, a Ghostel terminal the helm commands to paste (SUBMIT, or
C-RET, presses Enter too); elsewhere `repo/chart --version V' is
copied."
  (interactive)
  (let ((pkg (artifacthub--helm-package)))
    (artifacthub--with-version pkg (lambda (v) (artifacthub--use pkg v submit)))))

(defun artifacthub-use-and-run ()
  "Like `artifacthub-use'; in a terminal, press Enter after the paste."
  (interactive)
  (artifacthub-use t))

(defun artifacthub--with-parent-input (fn)
  "Hide the posframe, run FN where the minibuffer works, show it again."
  (let ((parent artifacthub--posframe-parent)
        (posframe (artifacthub--showing-p)))
    (when posframe
      (artifacthub--hide-posframe t)
      (when (frame-live-p parent)
        (select-frame-set-input-focus parent)))
    (unwind-protect
        (funcall fn)
      (when (and posframe (not (minibufferp)))
        (artifacthub--show-overlay)))))

(defun artifacthub--read-version (pkg detail)
  "Ask for one of DETAIL's versions of PKG, the newest first."
  (let* ((versions (artifacthub--sorted-versions detail))
         (names (mapcar (lambda (v) (alist-get 'version v)) versions))
         (current (or (gethash (alist-get 'package_id pkg) artifacthub--picked)
                      (artifacthub--latest-stable detail)))
         (annotate (lambda (name)
                     (let ((v (cl-find name versions :key (lambda (v) (alist-get 'version v))
                                       :test #'equal)))
                       (concat
                        (propertize " " 'display '(space :align-to 24))
                        (propertize
                         (concat (when-let* ((app (alist-get 'app_version v))) (concat "app " app))
                                 (when-let* ((ts (alist-get 'ts v)))
                                   (format-time-string "  %Y-%m-%d" ts))
                                 (when (artifacthub--prerelease-p name (alist-get 'prerelease v))
                                   "  pre-release")
                                 (when (alist-get 'contains_security_updates v)
                                   "  security fixes"))
                         'face 'completions-annotations)))))
         (table (lambda (string pred action)
                  (if (eq action 'metadata)
                      `(metadata (display-sort-function . identity)
                                 (cycle-sort-function . identity)
                                 (annotation-function . ,annotate))
                    (complete-with-action action names string pred)))))
    (completing-read (format "%s version (default %s): " (alist-get 'name pkg) current)
                     table nil t nil nil current)))

(defun artifacthub-pick-version ()
  "Pick the version of the chart at point that the actions use."
  (interactive)
  (let* ((pkg (artifacthub--helm-package))
         (sync t)
         (pick (lambda (detail)
                 (artifacthub--with-parent-input
                  (lambda ()
                    (let ((version (artifacthub--read-version pkg detail)))
                      (if (equal version (artifacthub--latest-stable detail))
                          (remhash (alist-get 'package_id pkg) artifacthub--picked)
                        (puthash (alist-get 'package_id pkg) version artifacthub--picked))
                      (artifacthub--refresh)))))))
    (artifacthub--fetching (format "%s's versions" (alist-get 'name pkg)))
    (artifacthub--detail
     pkg (lambda (detail err)
           (artifacthub--fetching nil)
           (cond (err (message "artifacthub: %s" err))
                 (sync (funcall pick detail))
                 ((and (artifacthub--showing-p)
                       (equal (alist-get 'package_id (artifacthub--package-at-point-of-overlay))
                              (alist-get 'package_id pkg)))
                  (run-at-time 0 nil pick detail)))))
    (setq sync nil)))

(defun artifacthub--package-at-point-of-overlay ()
  (when-let* ((buf (get-buffer artifacthub--buffer-name)))
    (with-current-buffer buf (artifacthub--package-at-point))))

(defvar-keymap artifacthub-values-mode-map
  :doc "Keys of a values.yaml buffer."
  "q" #'quit-window)

(define-minor-mode artifacthub-values-mode
  "A chart's default values.yaml, read-only; q quits."
  :lighter " values"
  :keymap artifacthub-values-mode-map)

(defun artifacthub--show-values (pkg version text)
  "Show TEXT, PKG's values.yaml at VERSION, in a read-only buffer CHART@VERSION."
  (let ((buffer (get-buffer-create (format "%s@%s" (alist-get 'name pkg) version))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text))
      ;; No mode hooks of the user's yaml-mode: an LSP client would
      ;; start for a buffer that has no file.
      (if (fboundp 'yaml-mode)
          (let ((yaml-mode-hook nil)) (yaml-mode))
        (fundamental-mode))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      (artifacthub-values-mode 1)
      ;; Evil read the keymaps when the major mode started; q is newer.
      (when (fboundp 'evil-normalize-keymaps)
        (evil-normalize-keymaps))
      (goto-char (point-min)))
    (artifacthub--hide)
    (pop-to-buffer buffer '((display-buffer-reuse-window display-buffer-pop-up-window)
                            (inhibit-same-window . t)))))

(defun artifacthub-values ()
  "Show the default values.yaml of the chart at point, at its version."
  (interactive)
  (let ((pkg (artifacthub--helm-package)))
    (artifacthub--with-version
     pkg (lambda (version)
           (artifacthub--fetching (format "%s@%s values" (alist-get 'name pkg) version))
           (artifacthub--values
            pkg version
            (lambda (text err)
              (artifacthub--fetching nil)
              (cond (err (message "artifacthub: no values for %s@%s: %s"
                                  (alist-get 'name pkg) version err))
                    ((string-empty-p (string-trim text))
                     (message "%s@%s has no default values" (alist-get 'name pkg) version))
                    (t (artifacthub--show-values pkg version text)))))))))

(defun artifacthub-open (&optional in-emacs)
  "Open the page of the package at point in the browser.
With a prefix (\\[universal-argument]), in an Emacs xwidget."
  (interactive "P")
  (let ((url (artifacthub--package-url (artifacthub--package))))
    (artifacthub--hide)
    (ghostel-cmd--open-url url in-emacs)))

(defun artifacthub-open-in-emacs ()
  "Open the page of the package at point in an Emacs xwidget."
  (interactive)
  (artifacthub-open t))

(defun artifacthub-copy ()
  "Copy the chart at point as `repo/chart --version V'; another kind's URL."
  (interactive)
  (let ((pkg (artifacthub--package)))
    (if (artifacthub--helm-p pkg)
        (artifacthub--with-version
         pkg (lambda (v) (artifacthub--copy (artifacthub--reference pkg v))))
      (artifacthub--copy (artifacthub--package-url pkg)))))

(defun artifacthub-copy-url ()
  "Copy the page URL of the package at point."
  (interactive)
  (artifacthub--copy (artifacthub--package-url (artifacthub--package))))

(defun artifacthub-help ()
  "Toggle the key legend under the list."
  (interactive)
  (setq artifacthub--help-visible (not artifacthub--help-visible))
  (artifacthub--refresh))


;;; Posframe / side window

(defun artifacthub--use-posframe-p ()
  (and artifacthub-use-posframe (ghostel-cmd--posframe-available-p)))

(defun artifacthub--posframe-frame ()
  (when-let* ((buf (get-buffer artifacthub--buffer-name)))
    (and (boundp 'posframe--frame)
         (buffer-local-value 'posframe--frame buf))))

(defun artifacthub--showing-p ()
  "Non-nil while the overlay is on screen."
  (if-let* ((frame (artifacthub--posframe-frame))
            ((frame-live-p frame))
            ((frame-visible-p frame)))
      t
    (when-let* ((buf (get-buffer artifacthub--buffer-name)))
      (and (get-buffer-window buf) t))))

(defun artifacthub--posframe-hooks (on)
  (dolist (hook '((window-size-change-functions . artifacthub--on-parent-resize)
                  (window-selection-change-functions . artifacthub--hide-on-focus-leave)))
    (if on
        (add-hook (car hook) (cdr hook))
      (remove-hook (car hook) (cdr hook)))))

(defun artifacthub--drop-fill ()
  "End this overlay's fill, if one is under way."
  (when (artifacthub--own-fill-p)
    (setq ghostel-cmd--fill nil)
    (ghostel-cmd--set-filling nil (get-buffer artifacthub--buffer-name))))

(defun artifacthub--hide-posframe (&optional keep-focus)
  "Hide the posframe, giving focus back to its parent unless KEEP-FOCUS."
  (let ((parent artifacthub--posframe-parent)
        (buf (get-buffer artifacthub--buffer-name)))
    (setq artifacthub--posframe-parent nil)
    (artifacthub--posframe-hooks nil)
    (when (and buf (fboundp 'posframe-hide))
      (posframe-hide buf))
    (when (and parent (frame-live-p parent) (not keep-focus))
      (select-frame-set-input-focus parent))))

(defun artifacthub--hide ()
  "Hide the overlay; its search stays for the next time."
  (artifacthub--cancel-timer)
  (artifacthub--drop-fill)
  (setq artifacthub--help-visible nil)
  (artifacthub--set-input nil)
  (if (when-let* ((frame (artifacthub--posframe-frame)))
        (and (frame-live-p frame) (frame-visible-p frame)))
      (artifacthub--hide-posframe)
    (when-let* ((buf (get-buffer artifacthub--buffer-name))
                (win (get-buffer-window buf)))
      (quit-window nil win))))

(defun artifacthub--hide-on-focus-leave (_frame)
  "Hide the posframe once a window outside it is selected; not the minibuffer."
  (when-let* ((frame (artifacthub--posframe-frame))
              ((frame-visible-p frame))
              (win (selected-window))
              ((not (eq (window-frame win) frame)))
              ((not (window-minibuffer-p win))))
    (artifacthub--drop-fill)
    (artifacthub--set-input nil)
    (artifacthub--hide-posframe t)))

(defun artifacthub--on-parent-resize (frame)
  (when (and artifacthub--posframe-parent
             (eq frame artifacthub--posframe-parent)
             (artifacthub--showing-p))
    (let ((width (ghostel-cmd--posframe-char-width frame)))
      (unless (eql width artifacthub--width)
        (setq artifacthub--width width)
        (artifacthub--refresh)
        (artifacthub--show-posframe (get-buffer artifacthub--buffer-name))))))

(defun artifacthub-quit ()
  "Close the help, else the overlay."
  (interactive)
  (if artifacthub--help-visible
      (artifacthub-help)
    (artifacthub--hide)))

(defun artifacthub--show-posframe (buf)
  (let* ((parent (or (and (frame-live-p artifacthub--posframe-parent)
                          artifacthub--posframe-parent)
                     (frame-parent (selected-frame))
                     (selected-frame)))
         (width (or artifacthub--width (ghostel-cmd--posframe-char-width parent)))
         (maxh (ghostel-cmd--posframe-max-height parent))
         (preview (1+ artifacthub--preview-lines))
         (chrome ghostel-cmd--posframe-chrome-lines)
         (table (max 4 (min artifacthub-page-size (- maxh preview chrome))))
         (height (+ chrome table preview)))
    (setq artifacthub--posframe-parent parent
          artifacthub--rows table)
    (artifacthub--posframe-hooks t)
    (with-selected-frame parent
      (posframe-show buf
                     :poshandler #'posframe-poshandler-frame-center
                     :width width
                     :min-width (min 48 width)
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
                     :window-point (with-current-buffer buf (point))))
    (when-let* (((not ghostel-cmd--keep-focus))
                (frame (artifacthub--posframe-frame))
                (win (get-buffer-window buf frame)))
      (select-frame-set-input-focus frame)
      (select-window win))))

(defun artifacthub--show-side-window (buf)
  (pop-to-buffer buf `((display-buffer-in-side-window)
                       (side . bottom)
                       (slot . 0)
                       (window-height . ,(+ 3 artifacthub-page-size artifacthub--preview-lines)))))

(defun artifacthub--show-overlay ()
  "Show the overlay as the search left it."
  (let* ((posframe (artifacthub--use-posframe-p))
         (buf (get-buffer-create artifacthub--buffer-name)))
    (setq artifacthub--width (and posframe (ghostel-cmd--posframe-char-width
                                            (or (frame-parent (selected-frame))
                                                (selected-frame))))
          artifacthub--rows nil)
    (with-current-buffer buf
      (unless (derived-mode-p 'artifacthub-mode)
        (artifacthub-mode)))
    (if posframe
        (artifacthub--show-posframe buf)
      (artifacthub--show-side-window buf))
    (artifacthub--refresh)
    (artifacthub--set-input artifacthub--input-on)
    buf))

;;;###autoload
(defun artifacthub-search (&optional query)
  "Search Artifact Hub in an overlay; type the query on its footer.
QUERY, or the region, starts it.  RET on a chart uses it where the
overlay was opened: a dependency in a Chart.yaml, a release in a
helmfile.yaml, the helm commands in a Ghostel terminal, else a copy."
  (interactive
   (list (when (use-region-p)
           (buffer-substring-no-properties (region-beginning) (region-end)))))
  (when (region-active-p) (deactivate-mark))
  (when (derived-mode-p 'artifacthub-mode)
    (user-error "Already searching"))
  (setq artifacthub--caller
        (list :buffer (current-buffer)
              :root (ghostel-cmd--current-project-root)
              :target (artifacthub--target (current-buffer)))
        artifacthub--kind 0
        artifacthub--help-visible nil
        artifacthub--input-on t
        artifacthub--busy nil
        artifacthub--query (string-trim (or query "")))
  (artifacthub--drop-fill)
  (artifacthub--show-overlay)
  (artifacthub--query-changed))


;;; Mode / keys

(defvar-keymap artifacthub-mode-map
  :doc "Keys of the Artifact Hub overlay."
  "j"          #'artifacthub-next
  "k"          #'artifacthub-previous
  "C-n"        #'artifacthub-next
  "C-p"        #'artifacthub-previous
  "<down>"     #'artifacthub-next
  "<up>"       #'artifacthub-previous
  "<home>"     #'artifacthub-first
  "<end>"      #'artifacthub-last
  "RET"        #'artifacthub-use
  "<return>"   #'artifacthub-use
  "C-RET"      #'artifacthub-use-and-run
  "C-<return>" #'artifacthub-use-and-run
  "V"          #'artifacthub-pick-version
  "v"          #'artifacthub-values
  "o"          #'artifacthub-open
  "O"          #'artifacthub-open-in-emacs
  "y"          #'artifacthub-copy
  "Y"          #'artifacthub-copy-url
  "n"          #'artifacthub-more
  "TAB"        #'artifacthub-next-kind
  "<tab>"      #'artifacthub-next-kind
  "<backtab>"  #'artifacthub-previous-kind
  "/"          #'artifacthub-search-input
  "i"          #'artifacthub-search-input
  "g"          #'artifacthub-again
  "?"          #'artifacthub-help
  "q"          #'artifacthub-quit
  "C-g"        #'artifacthub-quit
  "<escape>"   #'artifacthub-quit)

(defun artifacthub--setup-evil ()
  ;; One call: a keymap holds a single intercept state, so a second
  ;; call for another state would take the first one's place.
  (when (fboundp 'evil-make-intercept-map)
    (evil-make-intercept-map artifacthub-input-map))
  (when (fboundp 'evil-define-key*)
    (evil-define-key* '(normal motion) artifacthub-input-map
      (kbd "RET") #'artifacthub-input-done
      (kbd "<return>") #'artifacthub-input-done
      (kbd "C-m") #'artifacthub-input-done
      (kbd "<escape>") #'artifacthub-quit)
    (evil-define-key* '(normal motion) artifacthub-values-mode-map
      (kbd "q") #'quit-window)
    (evil-define-key* '(normal motion) artifacthub-mode-map
      (kbd "j") #'artifacthub-next
      (kbd "k") #'artifacthub-previous
      (kbd "C-n") #'artifacthub-next
      (kbd "C-p") #'artifacthub-previous
      (kbd "<down>") #'artifacthub-next
      (kbd "<up>") #'artifacthub-previous
      (kbd "gg") #'artifacthub-first
      (kbd "G") #'artifacthub-last
      (kbd "RET") #'artifacthub-use
      (kbd "<return>") #'artifacthub-use
      (kbd "C-RET") #'artifacthub-use-and-run
      (kbd "C-<return>") #'artifacthub-use-and-run
      (kbd "V") #'artifacthub-pick-version
      (kbd "v") #'artifacthub-values
      (kbd "o") #'artifacthub-open
      (kbd "O") #'artifacthub-open-in-emacs
      (kbd "y") #'artifacthub-copy
      (kbd "Y") #'artifacthub-copy-url
      (kbd "n") #'artifacthub-more
      (kbd "TAB") #'artifacthub-next-kind
      (kbd "<tab>") #'artifacthub-next-kind
      (kbd "<backtab>") #'artifacthub-previous-kind
      (kbd "/") #'artifacthub-search-input
      (kbd "i") #'artifacthub-search-input
      (kbd "gr") #'artifacthub-again
      (kbd "?") #'artifacthub-help
      (kbd "q") #'artifacthub-quit
      (kbd "<escape>") #'artifacthub-quit)))

(with-eval-after-load 'evil
  (artifacthub--setup-evil))

(define-derived-mode artifacthub-mode tabulated-list-mode "Artifacthub"
  "The Artifact Hub search overlay."
  (setq tabulated-list-padding 1)
  (setq tabulated-list-format (artifacthub--format (artifacthub--width)))
  (setq truncate-lines t)
  (setq-local mode-line-format '("" (:eval (artifacthub--footer))))
  (hl-line-mode 1)
  (add-hook 'post-command-hook #'artifacthub--on-command nil t)
  (when (fboundp 'evil-normalize-keymaps)
    (evil-normalize-keymaps))
  (tabulated-list-init-header))

(provide 'artifacthub)
;;; artifacthub.el ends here

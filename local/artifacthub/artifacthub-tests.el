;;; artifacthub-tests.el --- Tests for artifacthub  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;   emacs --batch --init-directory=~/.emacs.d/emacs-home/ \
;;         -L ~/.emacs.d/local/ghostel-cmd -L ~/.emacs.d/local/artifacthub \
;;         -l artifacthub.el -l artifacthub-tests.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; No network: the API answers come from fixtures/, replies recorded
;; from artifacthub.io on 2026-10-23 (READMEs and long version lists
;; trimmed).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'artifacthub)

(defconst artifacthub-tests--dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defun artifacthub-tests--reply (fixture)
  "FIXTURE's recorded reply, read the way url.el leaves a reply buffer."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally
     (expand-file-name (concat "fixtures/" fixture) artifacthub-tests--dir))
    (goto-char (point-min))
    (looking-at "HTTP/[0-9.]+ \\([0-9]+\\)")
    (setq-local url-http-response-status (string-to-number (match-string 1)))
    (re-search-forward "^\r*$")
    (setq-local url-http-end-of-headers (point-marker))
    (artifacthub--read-reply)))

(defun artifacthub-tests--packages (fixture)
  (plist-get (artifacthub--parse-search (artifacthub-tests--reply fixture)) :packages))

(defun artifacthub-tests--package (fixture repo name)
  (cl-find-if (lambda (p) (and (equal (artifacthub--repo p 'name) repo)
                               (equal (alist-get 'name p) name)))
              (artifacthub-tests--packages fixture)))

(defvar artifacthub-tests--requests nil
  "Requests the fake HTTP has open, newest first: (URL . CALLBACK).")

(defvar artifacthub-tests--clock 1000.0
  "What `artifacthub--now' says under `artifacthub-tests--with-search'.")

(defmacro artifacthub-tests--with-search (&rest body)
  "Run BODY on a fresh search state, the HTTP faked and the clock still."
  (declare (indent 0))
  `(let ((artifacthub-tests--requests nil)
         (artifacthub--http-function
          (lambda (url callback) (push (cons url callback) artifacthub-tests--requests)))
         (artifacthub--cache (make-hash-table :test #'equal))
         (artifacthub--inflight (make-hash-table :test #'equal))
         (artifacthub--picked (make-hash-table :test #'equal))
         (artifacthub--query "")
         (artifacthub--kind 0)
         (artifacthub--packages nil)
         (artifacthub--total nil)
         (artifacthub--facets nil)
         (artifacthub--wanted nil)
         (artifacthub--error nil)
         (artifacthub--busy nil)
         (artifacthub--index 0)
         (artifacthub--top 0)
         (artifacthub--timer nil)
         (artifacthub-tests--clock 1000.0))
     (cl-letf (((symbol-function 'artifacthub--now) (lambda () artifacthub-tests--clock)))
       (unwind-protect (progn ,@body)
         (artifacthub--cancel-timer)))))

(defun artifacthub-tests--answer (url reply)
  "Answer the open request for URL with REPLY, a plist or a fixture name."
  (let ((request (assoc url artifacthub-tests--requests)))
    (unless request (error "No open request for %s" url))
    (setq artifacthub-tests--requests (delq request artifacthub-tests--requests))
    (funcall (cdr request) (if (stringp reply) (artifacthub-tests--reply reply) reply))))

(defun artifacthub-tests--fire-debounce ()
  "Run the debounce timer now, as its 0.3 s would."
  (let ((timer artifacthub--timer))
    (should (timerp timer))
    (cancel-timer timer)
    (setq artifacthub--timer nil)
    (apply (timer--function timer) (timer--args timer))))

(defun artifacthub-tests--names ()
  (mapcar (lambda (p) (format "%s/%s" (artifacthub--repo p 'name) (alist-get 'name p)))
          artifacthub--packages))


;;; Replies

(ert-deftest artifacthub-test-read-reply ()
  (let* ((reply (artifacthub-tests--reply "search-loki.http"))
         (page (artifacthub--parse-search reply)))
    (should (eql (plist-get reply :status) 200))
    (should (equal (cdr (assoc "cache-control" (plist-get reply :headers))) "max-age=300"))
    (should (= (length (plist-get page :packages)) 20))
    (should (eql (plist-get page :total) 70))
    (should (equal (plist-get page :facets) '((0 . 70) (20 . 1) (24 . 1) (3 . 1))))
    (should (equal (alist-get 'name (car (plist-get page :packages))) "loki"))))

(ert-deftest artifacthub-test-values-reply-starts-at-the-yaml ()
  (let ((body (plist-get (artifacthub-tests--reply "values-grafana-loki.http") :body)))
    (should (string-prefix-p "# -- Overrides the version" body))))

(ert-deftest artifacthub-test-reply-errors ()
  (should (string-match-p "429" (artifacthub--reply-error '(:status 429 :body ""))))
  (should (equal (artifacthub--reply-error '(:status 404)) "HTTP 404"))
  (should (equal (artifacthub--reply-error '(:error "timed out")) "timed out"))
  (should-not (artifacthub--reply-error '(:status 200 :body "{}"))))

(ert-deftest artifacthub-test-search-url ()
  (should (equal (artifacthub--search-url "loki stack" 0 20)
                 (concat "https://artifacthub.io/api/v1/packages/search?ts_query_web=loki%20stack"
                         "&kind=0&facets=false&sort=relevance&limit=20&offset=20")))
  (should (string-match-p "facets=true" (artifacthub--search-url "loki" 3 0))))


;;; Search: debounce, stale answers, cache

(ert-deftest artifacthub-test-short-query-asks-nothing ()
  (artifacthub-tests--with-search
    (artifacthub--set-query "l")
    (should-not artifacthub--timer)
    (should-not artifacthub-tests--requests)
    (should-not artifacthub--wanted)))

(ert-deftest artifacthub-test-debounce-then-one-request ()
  (artifacthub-tests--with-search
    (dolist (q '("lo" "lok" "loki"))
      (artifacthub--set-query q))
    (should-not artifacthub-tests--requests)
    (artifacthub-tests--fire-debounce)
    (should (equal (mapcar #'car artifacthub-tests--requests)
                   (list (artifacthub--search-url "loki" 0 0))))
    (artifacthub-tests--answer (artifacthub--search-url "loki" 0 0) "search-loki.http")
    (should (= (length artifacthub--packages) 20))
    (should (eql artifacthub--total 70))
    (should-not artifacthub--wanted)))

(ert-deftest artifacthub-test-stale-answer-dropped ()
  (artifacthub-tests--with-search
    (artifacthub--set-query "loki")
    (artifacthub-tests--fire-debounce)
    (artifacthub--set-query "redis")
    (artifacthub-tests--fire-debounce)
    ;; redis lands first, then loki's late answer: loki must not show.
    (artifacthub-tests--answer (artifacthub--search-url "redis" 0 0) "search-redis.http")
    (should (equal (car (artifacthub-tests--names)) "bitnami/redis"))
    (artifacthub-tests--answer (artifacthub--search-url "loki" 0 0) "search-loki.http")
    (should (equal (car (artifacthub-tests--names)) "bitnami/redis"))
    (should (eql artifacthub--total 268))
    ;; ...but it was kept: going back to loki shows it with no request.
    (artifacthub--set-query "loki")
    (should-not artifacthub-tests--requests)
    (should (equal (car (artifacthub-tests--names)) "grafana-community/loki"))))

(ert-deftest artifacthub-test-answer-during-the-next-debounce-dropped ()
  (artifacthub-tests--with-search
    (artifacthub--set-query "loki")
    (artifacthub-tests--fire-debounce)
    ;; Typing on: the new query waits out its pause, the old answer lands.
    (artifacthub--set-query "loki-stack")
    (artifacthub-tests--answer (artifacthub--search-url "loki" 0 0) "search-loki.http")
    (should-not artifacthub--packages)
    (should (equal artifacthub--wanted '("loki-stack" 0 0)))))

(ert-deftest artifacthub-test-cache-five-minutes ()
  (artifacthub-tests--with-search
    (artifacthub--set-query "loki")
    (artifacthub-tests--fire-debounce)
    (artifacthub-tests--answer (artifacthub--search-url "loki" 0 0) "search-loki.http")
    (artifacthub--set-query "")
    (should-not artifacthub--packages)
    ;; Within the five minutes: at once, no pause, no request.
    (setq artifacthub-tests--clock (+ artifacthub-tests--clock 299))
    (artifacthub--set-query "loki")
    (should-not artifacthub--timer)
    (should-not artifacthub-tests--requests)
    (should (= (length artifacthub--packages) 20))
    ;; Past them: asked again.
    (setq artifacthub-tests--clock (+ artifacthub-tests--clock 2))
    (artifacthub--set-query "")
    (artifacthub--set-query "loki")
    (artifacthub-tests--fire-debounce)
    (should (equal (mapcar #'car artifacthub-tests--requests)
                   (list (artifacthub--search-url "loki" 0 0))))))

(ert-deftest artifacthub-test-expired-answers-pruned ()
  (artifacthub-tests--with-search
    (artifacthub--cache-put "a" 300 1)
    (setq artifacthub-tests--clock (+ artifacthub-tests--clock 301))
    (artifacthub--cache-put "b" 300 2)
    (should (equal (hash-table-keys artifacthub--cache) '("b")))))

(ert-deftest artifacthub-test-one-request-per-url-in-flight ()
  (artifacthub-tests--with-search
    (let ((got nil) (url (artifacthub--search-url "loki" 0 0)))
      (dotimes (_ 2)
        (artifacthub--get url #'artifacthub--parse-search 300
                          (lambda (page _err) (push (plist-get page :total) got))))
      (should (= (length artifacthub-tests--requests) 1))
      (artifacthub-tests--answer url "search-loki.http")
      (should (equal got '(70 70))))))

(ert-deftest artifacthub-test-a-lost-request-is-not-waited-on-forever ()
  (artifacthub-tests--with-search
    (let ((url (artifacthub--search-url "loki" 0 0)))
      (artifacthub--get url #'artifacthub--parse-search 300 #'ignore)
      (setq artifacthub-tests--clock (+ artifacthub-tests--clock artifacthub-timeout 6))
      (artifacthub--get url #'artifacthub--parse-search 300 #'ignore)
      (should (= (length artifacthub-tests--requests) 2)))))

(ert-deftest artifacthub-test-load-more ()
  (artifacthub-tests--with-search
    (artifacthub--set-query "loki")
    (artifacthub-tests--fire-debounce)
    (artifacthub-tests--answer (artifacthub--search-url "loki" 0 0) "search-loki.http")
    (artifacthub--request 20)
    (should (equal (caar artifacthub-tests--requests) (artifacthub--search-url "loki" 0 20)))
    (artifacthub-tests--answer (artifacthub--search-url "loki" 0 20) "search-loki-20.http")
    (should (= (length artifacthub--packages) 40))
    (should (eql artifacthub--total 70))
    (should (equal (length (delete-dups (mapcar (lambda (p) (alist-get 'package_id p))
                                                artifacthub--packages)))
                   40))
    ;; The facets of the first page stay.
    (should (equal (car artifacthub--facets) '(0 . 70)))))

(ert-deftest artifacthub-test-error-keeps-the-list ()
  (artifacthub-tests--with-search
    (artifacthub--set-query "loki")
    (artifacthub-tests--fire-debounce)
    (artifacthub-tests--answer (artifacthub--search-url "loki" 0 0) "search-loki.http")
    (artifacthub--set-query "lokii")
    (artifacthub-tests--fire-debounce)
    (artifacthub-tests--answer (artifacthub--search-url "lokii" 0 0) '(:status 429 :body ""))
    (should (string-match-p "429" artifacthub--error))
    (should (= (length artifacthub--packages) 20))
    ;; A failure is not cached.
    (should-not (artifacthub--cached (artifacthub--search-url "lokii" 0 0)))))

(ert-deftest artifacthub-test-kinds-cycle-with-counts ()
  (artifacthub-tests--with-search
    (artifacthub--set-query "loki")
    (artifacthub-tests--fire-debounce)
    (artifacthub-tests--answer (artifacthub--search-url "loki" 0 0) "search-loki.http")
    (should (equal (substring-no-properties (artifacthub--kinds-text))
                   "[Helm 70] · KCL 1 · Meshery 1 · OLM 1"))
    (artifacthub-next-kind)
    (should (eql artifacthub--kind 20))
    (should (equal (caar artifacthub-tests--requests) (artifacthub--search-url "loki" 20 0)))
    (artifacthub-previous-kind)
    (should (eql artifacthub--kind 0))
    ;; Helm's page was cached: no request for it.
    (should (= (length artifacthub-tests--requests) 1))
    (should (= (length artifacthub--packages) 20))))


;;; Kinds and URLs

(ert-deftest artifacthub-test-kind-url-map ()
  ;; The site's own map (its frontend, read 2026-10-23): not the names.
  (should (equal (mapcar (lambda (k) (cons (car k) (cadr k))) artifacthub-kinds)
                 '((0 . "helm") (1 . "falco") (2 . "opa") (3 . "olm") (4 . "tbaction")
                   (5 . "krew") (6 . "helm-plugin") (7 . "tekton-task") (8 . "keda-scaler")
                   (9 . "coredns") (10 . "keptn") (11 . "tekton-pipeline") (12 . "container")
                   (13 . "kubewarden") (14 . "gatekeeper") (15 . "kyverno")
                   (16 . "knative-client-plugin") (17 . "backstage") (18 . "argo-template")
                   (19 . "kubearmor") (20 . "kcl") (21 . "headlamp") (22 . "inspektor-gadget")
                   (23 . "tekton-stepaction") (24 . "meshery") (25 . "opencost")
                   (26 . "radius") (27 . "bootc") (28 . "kagent")))))

(ert-deftest artifacthub-test-package-urls ()
  (cl-flet ((pkg (kind repo name normalized)
              `((name . ,name) (normalized_name . ,normalized)
                (repository (name . ,repo) (kind . ,kind)))))
    (should (equal (artifacthub--package-url
                    (artifacthub-tests--package "search-loki.http" "grafana" "loki"))
                   "https://artifacthub.io/packages/helm/grafana/loki"))
    (should (equal (artifacthub--package-url (pkg 6 "git" "helm-git" "helm-git"))
                   "https://artifacthub.io/packages/helm-plugin/git/helm-git"))
    (should (equal (artifacthub--package-url (pkg 12 "werf" "werf" "werf"))
                   "https://artifacthub.io/packages/container/werf/werf"))
    (should (equal (artifacthub--package-url
                    (pkg 24 "meshery-catalog" "all-relationships" "all-relationships"))
                   "https://artifacthub.io/packages/meshery/meshery-catalog/all-relationships"))
    ;; The path takes the normalized name, never the display name.
    (should (equal (artifacthub--package-url
                    (pkg 10 "keptn-integrations" "k6 Keptn Integration" "k6-keptn-integration"))
                   "https://artifacthub.io/packages/keptn/keptn-integrations/k6-keptn-integration"))
    (should (equal (artifacthub--package-url (pkg 1 "security-hub" "Kubernetes" "kubernetes"))
                   "https://artifacthub.io/packages/falco/security-hub/kubernetes"))
    ;; A kind this does not know searches for it instead.
    (should (equal (artifacthub--package-url (pkg 99 "r" "some thing" "some-thing"))
                   "https://artifacthub.io/packages/search?ts_query_web=some%20thing&kind=99"))))

(ert-deftest artifacthub-test-chart-refs ()
  (let ((grafana (artifacthub-tests--package "search-loki.http" "grafana" "loki"))
        (oci (artifacthub-tests--package "search-redis.http" "cloudpirates-redis" "redis"))
        (slash (artifacthub-tests--package "search-loki.http" "grafana-community" "loki")))
    (should (equal (artifacthub--chart-ref grafana) "grafana/loki"))
    (should (equal (artifacthub--repo-url grafana) "https://grafana.github.io/helm-charts"))
    (should (equal (artifacthub--repo-url slash) "https://grafana-community.github.io/helm-charts"))
    (should (artifacthub--oci-p oci))
    (should (equal (artifacthub--chart-ref oci) "oci://registry-1.docker.io/cloudpirates/redis"))
    (should (equal (artifacthub--repo-url oci) "oci://registry-1.docker.io/cloudpirates"))
    (should (equal (artifacthub--reference grafana "7.5.0") "grafana/loki --version 7.5.0"))
    (should (equal (artifacthub--helm-command grafana "7.5.0")
                   (concat "helm repo add grafana https://grafana.github.io/helm-charts"
                           " && helm repo update && helm install {{release:loki}}"
                           " grafana/loki --version 7.5.0 -n {{namespace:default}}")))
    (should (equal (artifacthub--helm-command oci "0.37.8")
                   (concat "helm install {{release:redis}}"
                           " oci://registry-1.docker.io/cloudpirates/redis"
                           " --version 0.37.8 -n {{namespace:default}}")))))


;;; Versions

(ert-deftest artifacthub-test-version-order ()
  (should (artifacthub--version< "1.0.0-alpha.1" "1.0.0"))
  (should (artifacthub--version< "1.9.0" "1.10.0"))
  (should (artifacthub--version< "1.0.0-alpha.2" "1.0.0-alpha.10"))
  (should (artifacthub--version< "1.0.0-alpha" "1.0.0-beta"))
  (should (artifacthub--version< "v2.0.0" "2.0.1"))
  (should-not (artifacthub--version< "1.0.0" "1.0.0"))
  (should (artifacthub--prerelease-p "1.2.0-alpha.0"))
  (should-not (artifacthub--prerelease-p "7.5.0"))
  (should (artifacthub--prerelease-p "7.5.0" t)))

(ert-deftest artifacthub-test-latest-stable-not-the-apis-word ()
  ;; The API leaves 1.2.0-alpha.0 unmarked and lists the versions unsorted.
  (let ((detail (artifacthub--json
                 (plist-get (artifacthub-tests--reply "package-kubeblocks-loki-cluster.http")
                            :body))))
    (should (equal (alist-get 'version detail) "1.2.0-alpha.0"))
    (should (equal (artifacthub--latest-stable detail) "1.0.3"))
    (should (equal (alist-get 'version (car (artifacthub--sorted-versions detail)))
                   "1.2.0-alpha.0"))))

(ert-deftest artifacthub-test-a-pre-release-row-uses-the-latest-stable ()
  (artifacthub-tests--with-search
    (let ((pkg (artifacthub-tests--package "search-loki.http" "kubeblocks" "loki-cluster"))
          (got nil))
      (should (equal (alist-get 'version pkg) "1.2.0-alpha.0"))
      (artifacthub--with-version pkg (lambda (v) (setq got v)))
      (should-not got)
      (artifacthub-tests--answer "https://artifacthub.io/api/v1/packages/helm/kubeblocks/loki-cluster"
                                 "package-kubeblocks-loki-cluster.http")
      (should (equal got "1.0.3"))
      ;; A stable row needs no request; a picked version wins.
      (let ((grafana (artifacthub-tests--package "search-loki.http" "grafana" "loki")))
        (artifacthub--with-version grafana (lambda (v) (setq got v)))
        (should (equal got "7.5.0"))
        (puthash (alist-get 'package_id grafana) "6.0.0" artifacthub--picked)
        (artifacthub--with-version grafana (lambda (v) (setq got v)))
        (should (equal got "6.0.0")))
      (should-not artifacthub-tests--requests))))


;;; Chart.yaml

(defmacro artifacthub-tests--yaml (text &rest body)
  "Run BODY in a buffer holding TEXT; return the buffer's text after."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (goto-char (point-min))
     ,@body
     (buffer-string)))

(defconst artifacthub-tests--chart-indentless "\
apiVersion: v2
name: loki-promtail-helm
version: 0.1.0
appVersion: \"1.16.0\"

dependencies:
- name: loki
  repository: \"https://grafana.github.io/helm-charts\"
  version: \"^2.5.0\"
  condition: loki.enabled

- name: \"promtail\"
  repository: \"https://grafana.github.io/helm-charts\"
  version: \"^3.6.0\"
  condition: promtail.enabled
"
  "loki-promtail's shape: dashes at column 0, repository before version,
quoted values, a blank line between entries.")

(ert-deftest artifacthub-test-chart-dependency-in-the-files-style ()
  (should (equal (artifacthub-tests--yaml artifacthub-tests--chart-indentless
                   (should (nth 2 (artifacthub-yaml-add-dependency
                                   "tempo" "1.10.0" "https://grafana.github.io/helm-charts"))))
                 (concat artifacthub-tests--chart-indentless "
- name: tempo
  repository: \"https://grafana.github.io/helm-charts\"
  version: \"1.10.0\"
"))))

(defconst artifacthub-tests--chart-nested "\
apiVersion: v2
name: vider
version: 0.1.0
dependencies:
  - name: django-template
    version: \"0.5.27\"
    repository: http://staging-chartmuseum.example.com:8080
    tags:
        - django-template
# kept where it is
maintainers:
  - name: ops
")

(ert-deftest artifacthub-test-chart-dependency-after-a-nested-list ()
  (let ((out (artifacthub-tests--yaml artifacthub-tests--chart-nested
               (let ((r (artifacthub-yaml-add-dependency
                         "redis" "28.3.1" "https://charts.bitnami.com/bitnami")))
                 (should (equal (buffer-substring (car r) (cadr r))
                                (concat "  - name: redis\n"
                                        "    version: \"28.3.1\"\n"
                                        "    repository: https://charts.bitnami.com/bitnami")))))))
    (should (string-match-p (regexp-quote "\
        - django-template
  - name: redis
    version: \"28.3.1\"
    repository: https://charts.bitnami.com/bitnami
# kept where it is
maintainers:
") out))))

(ert-deftest artifacthub-test-chart-no-final-newline ()
  (should (equal (artifacthub-tests--yaml "\
dependencies:
  - name: django-server
    version: \"0.1.0\"
    repository: http://charts.example.com:8080"
                   (artifacthub-yaml-add-dependency "redis" "1.0.0" "https://charts.bitnami.com/bitnami"))
                 "\
dependencies:
  - name: django-server
    version: \"0.1.0\"
    repository: http://charts.example.com:8080
  - name: redis
    version: \"1.0.0\"
    repository: https://charts.bitnami.com/bitnami")))

(ert-deftest artifacthub-test-chart-key-absent-takes-the-files-indentation ()
  ;; The file's other list sets the style: dashes at 0 here.
  (should (equal (artifacthub-tests--yaml "\
apiVersion: v2
name: x
maintainers:
- name: ops
  email: ops@example.com
version: 0.1.0"
                   (artifacthub-yaml-add-dependency "loki" "7.5.0" "https://grafana.github.io/helm-charts"))
                 "\
apiVersion: v2
name: x
maintainers:
- name: ops
  email: ops@example.com
version: 0.1.0

dependencies:
- name: loki
  version: 7.5.0
  repository: https://grafana.github.io/helm-charts
"))
  ;; No list anywhere: Helm's documented style.
  (should (equal (artifacthub-tests--yaml "apiVersion: v2\nname: x\nversion: 0.1.0\n"
                   (artifacthub-yaml-add-dependency "loki" "7.5.0" "https://grafana.github.io/helm-charts"))
                 "\
apiVersion: v2
name: x
version: 0.1.0

dependencies:
  - name: loki
    version: 7.5.0
    repository: https://grafana.github.io/helm-charts
")))

(ert-deftest artifacthub-test-chart-empty-key ()
  (should (equal (artifacthub-tests--yaml "name: x\ndependencies: []\nversion: 0.1.0\n"
                   (artifacthub-yaml-add-dependency "loki" "7.5.0" "https://grafana.github.io/helm-charts"))
                 "\
name: x
dependencies:
  - name: loki
    version: 7.5.0
    repository: https://grafana.github.io/helm-charts
version: 0.1.0
"))
  (should (equal (artifacthub-tests--yaml "name: x\ndependencies:\n\nversion: 0.1.0\n"
                   (artifacthub-yaml-add-dependency "loki" "7.5.0" "https://grafana.github.io/helm-charts"))
                 "\
name: x
dependencies:
  - name: loki
    version: 7.5.0
    repository: https://grafana.github.io/helm-charts

version: 0.1.0
")))

(ert-deftest artifacthub-test-chart-dependency-already-there ()
  (let ((r nil))
    (should (equal (artifacthub-tests--yaml artifacthub-tests--chart-indentless
                     (setq r (artifacthub-yaml-add-dependency
                              "promtail" "6.0.0" "https://grafana.github.io/helm-charts/")))
                   artifacthub-tests--chart-indentless))
    (should-not (nth 2 r))))

(ert-deftest artifacthub-test-chart-quotes-what-would-misread ()
  (should (string-match-p "version: \"1.10\""
                          (artifacthub-tests--yaml "name: x\n"
                            (artifacthub-yaml-add-dependency "a" "1.10" "https://x.example.com"))))
  (should (equal (artifacthub-yaml--scalar "on" nil) "\"on\""))
  (should (equal (artifacthub-yaml--scalar "it's" ?\') "'it''s'")))

(ert-deftest artifacthub-test-chart-flow-list-left-alone ()
  (with-temp-buffer
    (insert "dependencies: [{name: a}]\n")
    (should-error (artifacthub-yaml-add-dependency "b" "1.0.0" "https://x") :type 'user-error)
    (should (equal (buffer-string) "dependencies: [{name: a}]\n"))))


;;; helmfile.yaml

(defconst artifacthub-tests--helmfile "\
environments:
  staging:
    kubeContext: staging

---

repositories:
 - name: grafana
   url: https://grafana.github.io/helm-charts
 - name: prometheus-community
   url: https://prometheus-community.github.io/helm-charts


releases:
- name: pyroscope
  namespace: prometheus
  chart: grafana/pyroscope
  version: 1.1.0
  installed: false
  values:
    - ./values/pyroscope/{{ .Environment.Name }}.yaml

- name: kube-prometheus-stack
  namespace: prometheus
  chart: prometheus-community/kube-prometheus-stack
  version: 51.9.1
  hooks:
    - events: [\"preapply\"]
      command: \"kubectl\"
"
  "prometheus-helm's shape: two documents, repositories dashed at column
1, releases at 0, a blank line between releases.")

(ert-deftest artifacthub-test-helmfile-release-with-a-known-repository ()
  (let* ((r nil)
         (out (artifacthub-tests--yaml artifacthub-tests--helmfile
                (setq r (artifacthub-yaml-add-release
                         "loki" "7.5.0" "grafana" "https://grafana.github.io/helm-charts/")))))
    (should (equal (nth 2 r) "grafana"))
    (should-not (nth 3 r))
    (should (equal out (concat artifacthub-tests--helmfile "
- name: loki
  chart: grafana/loki
  version: 7.5.0
")))))

(ert-deftest artifacthub-test-helmfile-adds-the-repository ()
  (let* ((r nil)
         (out (artifacthub-tests--yaml artifacthub-tests--helmfile
                (setq r (artifacthub-yaml-add-release
                         "redis" "28.3.1" "bitnami" "https://charts.bitnami.com/bitnami")))))
    (should (nth 3 r))
    (should (string-match-p (regexp-quote "\
 - name: prometheus-community
   url: https://prometheus-community.github.io/helm-charts
 - name: bitnami
   url: https://charts.bitnami.com/bitnami


releases:") out))
    (should (string-suffix-p "\
      command: \"kubectl\"

- name: redis
  chart: bitnami/redis
  version: 28.3.1
" out))))

(ert-deftest artifacthub-test-helmfile-repository-matched-by-url-not-name ()
  (let* ((r nil)
         (out (artifacthub-tests--yaml "\
repositories:
- name: prom
  url: \"https://prometheus-community.github.io/helm-charts/\"
releases:
- name: x
  chart: prom/x
"
                (setq r (artifacthub-yaml-add-release
                         "kube-prometheus-stack" "51.9.1" "prometheus-community"
                         "https://prometheus-community.github.io/helm-charts")))))
    (should (equal (nth 2 r) "prom"))
    (should (string-suffix-p "\
- name: kube-prometheus-stack
  chart: prom/kube-prometheus-stack
  version: 51.9.1
" out))
    (should-not (string-match-p "name: prometheus-community" out))))

(ert-deftest artifacthub-test-helmfile-keys-absent ()
  (should (equal (artifacthub-tests--yaml "helmDefaults:\n  wait: true\n"
                   (artifacthub-yaml-add-release
                    "loki" "7.5.0" "grafana" "https://grafana.github.io/helm-charts"))
                 "\
helmDefaults:
  wait: true

repositories:
- name: grafana
  url: https://grafana.github.io/helm-charts

releases:
- name: loki
  chart: grafana/loki
  version: 7.5.0
")))

(ert-deftest artifacthub-test-helmfile-oci-and-a-taken-name ()
  (let ((out (artifacthub-tests--yaml "\
repositories:
  - name: cloudpirates-redis
    url: https://example.com/other
releases:
  - name: redis
    chart: cloudpirates-redis/redis
"
               (artifacthub-yaml-add-release
                "redis" "0.37.8" "cloudpirates-redis"
                "oci://registry-1.docker.io/cloudpirates" t))))
    (should (equal out "\
repositories:
  - name: cloudpirates-redis
    url: https://example.com/other
  - name: cloudpirates-redis-2
    url: registry-1.docker.io/cloudpirates
    oci: true
releases:
  - name: redis
    chart: cloudpirates-redis/redis
  - name: redis-2
    chart: cloudpirates-redis-2/redis
    version: 0.37.8
"))))


;;; Where RET goes

(ert-deftest artifacthub-test-targets ()
  (cl-flet ((target (file &optional read-only)
              (with-temp-buffer
                (setq buffer-file-name file buffer-read-only read-only)
                (prog1 (artifacthub--target (current-buffer))
                  (setq buffer-file-name nil)))))
    (should (eq (target "/p/Chart.yaml") 'chart))
    (should (eq (target "/p/requirements.yaml") 'chart))
    (should (eq (target "/p/helmfile.yaml") 'helmfile))
    (should (eq (target "/p/helmfile.yaml.gotmpl") 'helmfile))
    (should (eq (target "/p/helmfile.d/10-monitoring.yaml") 'helmfile))
    (should (eq (target "/p/values.yaml") 'copy))
    (should (eq (target "/p/Chart.yaml" t) 'copy))
    (should (eq (target nil) 'copy))))


;;; The overlay, with ghostel-cmd's fill

(defmacro artifacthub-tests--with-overlay (target &rest body)
  "Run BODY in the overlay listing loki's search, opened from TARGET.
No posframe: the side window, in the selected window's frame."
  (declare (indent 1))
  `(artifacthub-tests--with-search
     (let* ((caller (generate-new-buffer " caller"))
            (artifacthub-use-posframe nil)
            (artifacthub--rows 20)
            (artifacthub--caller (list :buffer caller :root "/tmp/proj/" :target ,target))
            (artifacthub--input-on nil)
            (artifacthub--help-visible nil)
            (ghostel-cmd--fill nil)
            (ghostel-cmd--last-values (make-hash-table :test #'equal))
            (buf (artifacthub--show-overlay)))
       (unwind-protect
           (with-current-buffer buf
             (setq artifacthub--rows 20)
             (artifacthub--set-query "loki")
             (artifacthub-tests--fire-debounce)
             (artifacthub-tests--answer (artifacthub--search-url "loki" 0 0) "search-loki.http")
             ,@body)
         (ghostel-cmd--set-filling nil buf)
         (kill-buffer buf)
         (kill-buffer caller)))))

(defun artifacthub-tests--goto (repo name)
  "Select REPO/NAME in the overlay, as moving to it would."
  (artifacthub--select (cl-position-if (lambda (p) (and (equal (artifacthub--repo p 'name) repo)
                                                        (equal (alist-get 'name p) name)))
                                       artifacthub--packages))
  (let ((p (artifacthub--package-at-point)))
    (should (equal (list (artifacthub--repo p 'name) (alist-get 'name p)) (list repo name)))))

(ert-deftest artifacthub-test-overlay-rows ()
  (artifacthub-tests--with-overlay 'copy
    (should (= (length tabulated-list-entries) 20))
    (artifacthub-tests--goto "grafana-community" "loki")
    (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
      (should (string-match-p "loki" line))
      ;; A long name is cut, never its mark.
      (should (string-match-p "Grafana Comm[^ ]*… ✓" line))
      (should (string-match-p "19\\.1\\.1 (3\\.7\\.9)" line))
      (should (string-match-p "54" line)))
    (should (string-match-p "RET copy · https://grafana-community.github.io/helm-charts"
                            (buffer-substring-no-properties artifacthub--preview-start (point-max))))
    (should (string-match-p "20/70" (artifacthub--footer)))))

(ert-deftest artifacthub-test-more-rows-than-fit-keep-the-preview ()
  (artifacthub-tests--with-overlay 'copy
    (artifacthub--request 20)
    (artifacthub-tests--answer (artifacthub--search-url "loki" 0 20) "search-loki-20.http")
    (setq artifacthub--rows 10)
    (artifacthub--refresh)
    (should (= (length artifacthub--packages) 40))
    (should (= (length tabulated-list-entries) 10))
    (set-window-buffer (selected-window) (current-buffer))
    (execute-kbd-macro (kbd "<end>"))
    ;; The last ten show, the last selected, the preview right under them.
    (should (= artifacthub--top 30))
    (should (equal (alist-get 'name (artifacthub--package-at-point)) "nginx-logging-loki-demo"))
    (should (= (line-number-at-pos) 10))
    (should (string-match-p "nginx-logging-loki-demo"
                            (buffer-substring-no-properties artifacthub--preview-start (point-max))))
    ;; The rule under the rows counts the ones hidden, as ghostel-cmd's does.
    (should (string-search "↑ 30 above" (buffer-substring-no-properties
                                         artifacthub--preview-start (point-max))))
    ;; Up past the top row scrolls a row at a time.
    (artifacthub--select 30)
    (execute-kbd-macro (kbd "<up>"))
    (should (= artifacthub--index 29))
    (should (= artifacthub--top 29))
    (should (= (line-number-at-pos) 1))
    ;; Within the rows shown, point just moves.
    (execute-kbd-macro (kbd "<down> <down>"))
    (should (= artifacthub--top 29))
    (should (= artifacthub--index 31))
    (should (= (line-number-at-pos) 3))))

(ert-deftest artifacthub-test-deprecated-rows-dimmed ()
  (artifacthub-tests--with-overlay 'copy
    (setf (alist-get 'deprecated (car artifacthub--packages)) t)
    (artifacthub--refresh)
    (goto-char (point-min))
    (let ((face (get-text-property (+ (line-beginning-position) 2) 'face)))
      (should (eq face 'shadow)))
    (forward-line 1)
    (should-not (eq (get-text-property (+ (line-beginning-position) 2) 'face) 'shadow))))

(ert-deftest artifacthub-test-typing-reaches-the-query ()
  (artifacthub-tests--with-overlay 'copy
    (artifacthub--set-input t)
    (set-window-buffer (selected-window) (current-buffer))
    (execute-kbd-macro (kbd "- s t a c k"))
    (should (equal artifacthub--query "loki-stack"))
    (execute-kbd-macro (kbd "DEL DEL DEL DEL DEL DEL"))
    (should (equal artifacthub--query "loki"))
    (execute-kbd-macro (kbd "RET"))
    (should-not artifacthub--input-on)))

(ert-deftest artifacthub-test-elsewhere-copies ()
  (artifacthub-tests--with-overlay 'copy
    (artifacthub-tests--goto "grafana" "loki")
    (let ((kill-ring nil))
      (artifacthub-use)
      (should (equal (car kill-ring) "grafana/loki --version 7.5.0")))))

(ert-deftest artifacthub-test-non-helm-gets-only-open-and-copy ()
  (artifacthub-tests--with-overlay 'copy
    (setf (alist-get 'kind (alist-get 'repository (car artifacthub--packages))) 3)
    (artifacthub--refresh)
    (goto-char (point-min))
    (should-error (artifacthub-use) :type 'user-error)
    (should-error (artifacthub-values) :type 'user-error)
    (should-error (artifacthub-pick-version) :type 'user-error)
    (let ((kill-ring nil))
      (artifacthub-copy)
      (should (string-prefix-p "https://artifacthub.io/packages/olm/" (car kill-ring))))))

(ert-deftest artifacthub-test-terminal-fills-on-the-footer-then-pastes ()
  (artifacthub-tests--with-overlay 'terminal
    (let ((delivered nil))
      (cl-letf (((symbol-function 'ghostel-cmd--deliver)
                 (lambda (cmd submit &optional terminal)
                   (push (list cmd submit terminal) delivered))))
        (set-window-buffer (selected-window) (current-buffer))
        (artifacthub-tests--goto "grafana" "loki")
        (artifacthub-use)
        ;; ghostel-cmd's own fill, on this overlay's footer.
        (should (artifacthub--own-fill-p))
        (should ghostel-cmd-fill-mode)
        (should (string-match-p "{{release}} \\[loki\\]" (artifacthub--footer)))
        (should (string-match-p "helm install loki grafana/loki"
                                (string-replace
                                 "\n" " " (buffer-substring-no-properties
                                           artifacthub--preview-start (point-max)))))
        (execute-kbd-macro (kbd "C-u l o g s RET C-u m o n RET"))
        (should (equal delivered
                       `((,(concat "helm repo add grafana https://grafana.github.io/helm-charts"
                                   " && helm repo update && helm install logs grafana/loki"
                                   " --version 7.5.0 -n mon")
                          nil ,(plist-get artifacthub--caller :buffer)))))
        ;; The paste hid the overlay; its window shows something else now.
        (set-buffer buf)
        (should-not ghostel-cmd--fill)
        (should-not ghostel-cmd-fill-mode)
        ;; C-RET runs it, and the values filled come back as defaults.
        (setq delivered nil)
        (set-window-buffer (selected-window) buf)
        (artifacthub-tests--goto "grafana" "loki")
        (artifacthub-use-and-run)
        (should (string-match-p "{{release}} \\[logs\\]" (artifacthub--footer)))
        (execute-kbd-macro (kbd "RET RET"))
        (should (string-suffix-p "helm install logs grafana/loki --version 7.5.0 -n mon"
                                 (car (car delivered))))
        (should (eq (nth 1 (car delivered)) t))))))

(ert-deftest artifacthub-test-fill-cancel-returns-to-the-list ()
  (artifacthub-tests--with-overlay 'terminal
    (set-window-buffer (selected-window) (current-buffer))
    (artifacthub-tests--goto "grafana" "loki")
    (artifacthub-use)
    (execute-kbd-macro (kbd "<escape>"))
    (should-not ghostel-cmd--fill)
    (should-not ghostel-cmd-fill-mode)
    (should (string-match-p "RET helm install" (artifacthub--footer)))))

(ert-deftest artifacthub-test-chart-yaml-from-the-overlay ()
  (artifacthub-tests--with-search
    (let* ((dir (make-temp-file "artifacthub-" t))
           (file (expand-file-name "Chart.yaml" dir)))
      (unwind-protect
          (progn
            (with-temp-file file (insert artifacthub-tests--chart-indentless))
            (let ((chart (find-file-noselect file)))
              (with-current-buffer chart
                (let ((artifacthub-use-posframe nil))
                  (cl-letf (((symbol-function 'artifacthub--query-changed) #'ignore))
                    (artifacthub-search "loki"))))
              (should (eq (plist-get artifacthub--caller :target) 'chart))
              (with-current-buffer artifacthub--buffer-name
                (artifacthub--set-query "loki")
                (artifacthub-tests--fire-debounce)
                (artifacthub-tests--answer (artifacthub--search-url "loki" 0 0)
                                           "search-loki.http")
                (artifacthub-tests--goto "grafana-community" "loki")
                (artifacthub-use))
              (with-current-buffer chart
                (should (string-suffix-p "\
- name: loki
  repository: \"https://grafana-community.github.io/helm-charts\"
  version: \"19.1.1\"
" (buffer-string)))
                (should (looking-at-p "- name: loki\n  repository: \"https://grafana-community"))
                (set-buffer-modified-p nil)
                (kill-buffer))))
        (when (get-buffer artifacthub--buffer-name)
          (kill-buffer artifacthub--buffer-name))
        (delete-directory dir t)))))

(provide 'artifacthub-tests)
;;; artifacthub-tests.el ends here

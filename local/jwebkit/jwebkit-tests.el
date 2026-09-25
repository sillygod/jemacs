;;; jwebkit-tests.el --- Tests for jwebkit  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;   emacs --batch --init-directory=~/.emacs.d/emacs-home/ \
;;         -L ~/.emacs.d/local/jwebkit \
;;         -l jwebkit.el -l jwebkit-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'json)
(require 'jwebkit)

(ert-deftest jwebkit-test-find-js-encodes-query ()
  (let ((js (jwebkit--find-js "foo\"bar" nil t)))
    (should (string-match-p "window\\.find" js))
    (should (string-match-p "foo\\\\\"bar" js))
    (should (string-match-p ",false,true)" js)))
  (should (string-match-p ",true,false)" (jwebkit--find-js "x" t)))
  (should (string-match-p ",false,true)" (jwebkit--find-js "x" nil t))))

(ert-deftest jwebkit-test-parse-labels-json-string ()
  (should (equal (jwebkit--parse-labels "[\"a\",\"s\",\"d\"]")
                 '("a" "s" "d"))))

(ert-deftest jwebkit-test-parse-labels-vector ()
  (should (equal (jwebkit--parse-labels ["a" "s"]) '("a" "s"))))

(ert-deftest jwebkit-test-parse-json-objects ()
  (let ((rows (jwebkit--parse-json
               "[{\"text\":\"Home\",\"href\":\"https://ex.com\"}]")))
    (should (equal (alist-get 'text (car rows)) "Home"))
    (should (equal (alist-get 'href (car rows)) "https://ex.com"))))

(ert-deftest jwebkit-test-ace-js-no-title-bridge ()
  (should-not (string-match-p "document\\.title" jwebkit--ace-js))
  (should-not (string-match-p "document\\.title" jwebkit--collect-links-js))
  (should (string-match-p "data-jw-ace" jwebkit--ace-js))
  (should (string-match-p "querySelectorAll" jwebkit--ace-js)))

(ert-deftest jwebkit-test-ace-inject-calls-with-keys ()
  (let ((js (let ((jwebkit-ace-keys "asdf"))
              (jwebkit--ace-inject-js))))
    (should (string-suffix-p "(\"asdf\");" js))
    (should (string-match-p "i%k" js))))

(ert-deftest jwebkit-test-ace-js-is-valid-enough ()
  (should (string-match-p "window\\.__jwAce" jwebkit--ace-js))
  (should (string-match-p "function lab" jwebkit--ace-js)))


;;; PDF

(defmacro jwebkit-tests--with-pdfjs (&rest body)
  "Run BODY against a fake pdf.js tree and a fresh server."
  (declare (indent 0))
  `(let* ((jwebkit-pdf-directory (make-temp-file "jwebkit-pdfjs-" t))
          (jwebkit-pdf-version "0.0.0")
          (jwebkit-pdf--server nil)
          (jwebkit-pdf--docs (make-hash-table :test #'equal))
          (web (expand-file-name "0.0.0/web" jwebkit-pdf-directory)))
     (make-directory web t)
     (with-temp-file (expand-file-name "viewer.html" web)
       (insert "<html><head><title>v</title></head><body></body></html>"))
     (with-temp-file (expand-file-name "viewer.mjs" web)
       (insert "export {};"))
     (unwind-protect (progn ,@body)
       (jwebkit-pdf-stop)
       (delete-directory jwebkit-pdf-directory t))))

(defun jwebkit-tests--get (path)
  "Body of GET PATH from the jwebkit-pdf server, as unibyte."
  (let ((buf (url-retrieve-synchronously
              (concat (jwebkit-pdf--origin) path) t t 5)))
    (unwind-protect
        (with-current-buffer buf
          (set-buffer-multibyte nil)
          (list url-http-response-status
                (buffer-substring-no-properties
                 (1+ url-http-end-of-headers) (point-max))))
      (kill-buffer buf))))

(ert-deftest jwebkit-test-pdf-request-target ()
  (should (equal (jwebkit-pdf--request-target
                  "GET /pdfjs/web/viewer.html?file=%2Fpdf%2Fx HTTP/1.1\r\nHost: h\r\n\r\n")
                 '("GET" . "/pdfjs/web/viewer.html")))
  (should (equal (jwebkit-pdf--request-target "HEAD /pdf/a%20b/c.pdf HTTP/1.1\r\n\r\n")
                 '("HEAD" . "/pdf/a b/c.pdf")))
  (should-not (jwebkit-pdf--request-target "garbage")))

(ert-deftest jwebkit-test-pdf-route-stays-in-root ()
  "Static paths cannot climb out of the pdf.js tree, and documents are
reachable only by token, never by path."
  (jwebkit-tests--with-pdfjs
    (should (eq 200 (car (jwebkit-pdf--route "/pdfjs/web/viewer.mjs"))))
    (should (eq 404 (car (jwebkit-pdf--route "/pdfjs/../../../../etc/passwd"))))
    (should (eq 404 (car (jwebkit-pdf--route "/pdfjs/web/../../0.0.0x/secret"))))
    (should (eq 404 (car (jwebkit-pdf--route "/pdf/nope/x.pdf"))))
    (should (eq 404 (car (jwebkit-pdf--route "/etc/passwd"))))))

(ert-deftest jwebkit-test-pdf-shim-injected-before-head ()
  (let ((html (jwebkit-pdf--inject-shim "<head><x></head><body>")))
    (should (string-match-p "<script src=\"/jwebkit/shim.js\"></script>\n</head>" html)))
  (should (string-match-p "window\\.jwebkitPdf" jwebkit-pdf--shim-js))
  (should (string-match-p "viewerContainer" jwebkit-pdf--shim-js)))

(ert-deftest jwebkit-test-pdf-serves-viewer-and-document ()
  "End to end over a real socket: the viewer comes back with the shim,
and a registered PDF comes back byte for byte."
  (jwebkit-tests--with-pdfjs
    (let* ((bytes (concat "%PDF-1.4\n" (unibyte-string 0 200 255) "\n%%EOF"))
           (pdf (make-temp-file "jwebkit-test-" nil ".pdf")))
      (unwind-protect
          (progn
            (let ((coding-system-for-write 'binary))
              (write-region bytes nil pdf nil 'silent))
            (let* ((url (jwebkit-pdf--viewer-url
                         (jwebkit-pdf--register pdf pdf) "my doc.pdf"))
                   (file (url-unhex-string
                          (progn (string-match "file=\\(.*\\)" url)
                                 (match-string 1 url)))))
              (should (string-match-p "\\`/pdf/[0-9a-f]\\{64\\}/my%20doc\\.pdf\\'" file))
              (pcase-let ((`(,status ,body) (jwebkit-tests--get "/pdfjs/web/viewer.html")))
                (should (eq status 200))
                (should (string-match-p "/jwebkit/shim.js" body)))
              (pcase-let ((`(,status ,body) (jwebkit-tests--get file)))
                (should (eq status 200))
                (should (equal body bytes)))
              (should (eq 404 (car (jwebkit-tests--get "/pdf/deadbeef/x.pdf"))))))
        (delete-file pdf)))))

(ert-deftest jwebkit-test-pdf-busy-port-falls-back ()
  "A preferred port that is already taken yields another free port
instead of an error."
  (jwebkit-tests--with-pdfjs
    (let* ((squatter (make-network-process
                      :name "jwebkit-squatter" :server t :host 'local
                      :family 'ipv4 :service t :noquery t))
           (taken (process-contact squatter :service)))
      (unwind-protect
          (let* ((jwebkit-pdf-port taken)
                 (inhibit-message t)
                 (port (jwebkit-pdf--port)))
            (should (integerp port))
            (should-not (eq port taken))
            (should (process-live-p jwebkit-pdf--server)))
        (delete-process squatter)))))

(ert-deftest jwebkit-test-pdf-free-preferred-port-is-used ()
  (jwebkit-tests--with-pdfjs
    (let* ((probe (make-network-process
                   :name "jwebkit-probe" :server t :host 'local
                   :family 'ipv4 :service t :noquery t))
           (free (process-contact probe :service)))
      (delete-process probe)
      (let ((jwebkit-pdf-port free))
        (should (eq (jwebkit-pdf--port) free))))))

(ert-deftest jwebkit-test-pdf-name-and-url-detection ()
  (should (equal (jwebkit-pdf--name "https://ex.com/a/b%20c.pdf?x=1") "b c.pdf"))
  (should (equal (jwebkit-pdf--name "https://ex.com/") "document.pdf"))
  (should (jwebkit-pdf--pdf-url-p "https://ex.com/x.PDF?dl=1"))
  (should-not (jwebkit-pdf--pdf-url-p "https://ex.com/x.pdf.html")))

(ert-deftest jwebkit-test-pdf-find-js ()
  (let ((js (jwebkit-pdf--find-js "a\"b" t nil)))
    (should (string-match-p "jwebkitPdf\\.find(\"a\\\\\"b\",true,false)" js))))

(ert-deftest jwebkit-test-pdf-fetch-rejects-non-pdf ()
  "A login page served where the PDF was expected is not saved as one."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert "HTTP/1.1 200 OK\r\n\r\n<html>login</html>")
    (should-not (jwebkit-pdf--pdf-bytes-p 20))
    (erase-buffer)
    (insert "HTTP/1.1 200 OK\r\n\r\n%PDF-1.7")
    (should (jwebkit-pdf--pdf-bytes-p 20))))

(provide 'jwebkit-tests)
;;; jwebkit-tests.el ends here

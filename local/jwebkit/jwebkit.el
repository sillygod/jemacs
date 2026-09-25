;;; jwebkit.el --- Vim-like xwidget-webkit UX -*- lexical-binding: t; -*-

;; Author: Jing
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, hypermedia
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Fills the gaps evil-collection leaves on `xwidget-webkit-mode':
;; in-page search, link hints, typing into the page, follow-link.
;; Does not use document.title (pr-view owns that bridge).
;;
;;   / n N   search via window.find (NS xwidget-webkit-search is a stub)
;;   f / F   ace hints (current / new session)
;;   gf      follow link via completing-read
;;   i       pass keys through to the page
;;   gi      focus first input, then edit
;;   *       search for the current selection
;;   Y       yank page selection
;;   &       open current URL in the system browser
;;   gp gO   PDF page / outline (pdf.js viewer; see jwebkit-pdf.el)

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'xwidget)
(require 'jwebkit-pdf)

(declare-function evil-define-key* "evil")
(declare-function evil-emacs-state "evil")
(declare-function evil-normal-state "evil")


(defgroup jwebkit nil
  "Vim-like xwidget-webkit UX."
  :group 'xwidget
  :prefix "jwebkit-")

(defcustom jwebkit-ace-keys "asdfghjklqwertyuiop"
  "Characters used for on-page hints."
  :type 'string
  :group 'jwebkit)

(defcustom jwebkit-cookie-file
  (expand-file-name "xwidget-cookies" user-emacs-directory)
  "Cookie jar for xwidget-webkit.  Nil to leave `xwidget-webkit-cookie-file' alone."
  :type '(choice (const nil) file)
  :group 'jwebkit)

(defvar-local jwebkit--query ""
  "Last in-page search string.")

(defvar jwebkit--find-history nil)


;;; Session helpers

(defun jwebkit--session ()
  (or (xwidget-webkit-current-session)
      (user-error "No xwidget session")))

(defun jwebkit--parse-json (raw)
  "Parse RAW from `xwidget-webkit-execute-script' into Lisp."
  (cond
   ((null raw) nil)
   ((and (stringp raw) (string-empty-p (string-trim raw))) nil)
   ((stringp raw)
    (ignore-errors
      (json-parse-string raw
                         :object-type 'alist
                         :array-type 'list
                         :null-object nil
                         :false-object nil)))
   ((vectorp raw) (append raw nil))
   (t raw)))

(defun jwebkit--parse-labels (raw)
  (let ((v (jwebkit--parse-json raw)))
    (cond
     ((and (listp v) (stringp (car-safe v))) v)
     ((and (vectorp raw) (stringp (aref raw 0))) (append raw nil))
     (t nil))))


;;; Search
;;
;; `xwidget-webkit-search' is implemented only for GTK (WebKitFindController).
;; On the NS port it is fbound but a no-op, which is why C-s / isearch in
;; xwidget looks like it does nothing.  window.find works on WKWebView.

(defun jwebkit--find-js (query reverse &optional fresh)
  "JS to run window.find for QUERY.
REVERSE searches backward.  FRESH starts from the top of the page
instead of continuing from the current selection."
  (format
   "(function(q,rev,fresh){
  var sel = window.getSelection && window.getSelection();
  if (sel && sel.rangeCount) {
    if (fresh) sel.removeAllRanges();
    else if (!sel.isCollapsed) {
      if (rev) sel.collapseToStart(); else sel.collapseToEnd();
    }
  }
  return window.find(q, false, rev, true, false, true, false);
})(%s,%s,%s);"
   (json-encode query)
   (if reverse "true" "false")
   (if fresh "true" "false")))

(defun jwebkit--search-js (xw query reverse &optional fresh)
  "JS searching XW for QUERY.
In the pdf.js viewer this goes through pdf.js's find controller:
window.find only sees pages whose text layer has been rendered."
  (if (jwebkit-pdf-viewer-p xw)
      (jwebkit-pdf--find-js query (not fresh) reverse)
    (jwebkit--find-js query reverse fresh)))

(defun jwebkit-find-start (query)
  "Search the page for QUERY.  Empty QUERY clears the selection."
  (interactive
   (list (read-string "Search: " nil 'jwebkit--find-history jwebkit--query)))
  (let ((xw (jwebkit--session)))
    (setq jwebkit--query query)
    (if (string-empty-p query)
        (progn
          (xwidget-webkit-execute-script
           xw (if (jwebkit-pdf-viewer-p xw)
                  "window.jwebkitPdf && window.jwebkitPdf.clear();"
                "window.getSelection && window.getSelection().removeAllRanges();"))
          (message "Search cleared"))
      (xwidget-webkit-execute-script xw (jwebkit--search-js xw query nil t))
      (message "Search: %s  (n/N)" query))))

(defun jwebkit-find-next ()
  "Next match of the last `/` query."
  (interactive)
  (if (string-empty-p jwebkit--query)
      (call-interactively #'jwebkit-find-start)
    (let ((xw (jwebkit--session)))
      (xwidget-webkit-execute-script
       xw (jwebkit--search-js xw jwebkit--query nil)))))

(defun jwebkit-find-prev ()
  "Previous match of the last `/` query."
  (interactive)
  (if (string-empty-p jwebkit--query)
      (call-interactively #'jwebkit-find-start)
    (let ((xw (jwebkit--session)))
      (xwidget-webkit-execute-script
       xw (jwebkit--search-js xw jwebkit--query t)))))

(defun jwebkit-find-selection ()
  "Search for the current page selection."
  (interactive)
  (xwidget-webkit-get-selection
   (lambda (s)
     (let ((s (and s (string-trim s))))
       (if (or (null s) (string-empty-p s))
           (user-error "No selection")
         (jwebkit-find-start s))))))


;;; Edit / pass keys through

(defun jwebkit-edit ()
  "Send keys to the page (forms, PR comments)."
  (interactive)
  (xwidget-webkit-edit-mode 1)
  (when (fboundp 'evil-emacs-state)
    (evil-emacs-state)))

(defun jwebkit-stop-edit ()
  "Stop sending keys to the page."
  (interactive)
  (when (bound-and-true-p xwidget-webkit-edit-mode)
    (xwidget-webkit-edit-mode -1))
  (when (fboundp 'evil-normal-state)
    (evil-normal-state)))

(defconst jwebkit--focus-first-js
  "(function(){
  var el = document.querySelector('input:not([type=hidden]),textarea,[contenteditable=\"true\"]');
  if (!el) return false;
  el.focus();
  return true;
})();")

(defun jwebkit-focus-input ()
  "Focus the first text field and enter edit mode."
  (interactive)
  (let ((xw (jwebkit--session)))
    (xwidget-webkit-execute-script
     xw jwebkit--focus-first-js
     (lambda (ok)
       (if (or (eq ok t) (equal ok :true) (equal ok "true"))
           (jwebkit-edit)
         (message "No input field"))))))


;;; Yank / external

(defun jwebkit-open-external ()
  "Open the current page in the system browser.
A pdf.js page opens the PDF it came from, not the local viewer."
  (interactive)
  (let* ((xw (jwebkit--session))
         (url (or (jwebkit-pdf-source xw) (xwidget-webkit-uri xw))))
    (unless (and url (not (string-empty-p url)))
      (user-error "No URL"))
    (if (file-name-absolute-p url)
        (browse-url-of-file url)
      (browse-url url))))


;;; Follow link (completing-read)

(defconst jwebkit--collect-links-js
  "(function(){
  function vis(el){
    var r = el.getBoundingClientRect();
    var s = getComputedStyle(el);
    return r.width>1 && r.height>1 && r.bottom>0 && r.top<innerHeight
      && r.right>0 && r.left<innerWidth
      && s.visibility!=='hidden' && s.display!=='none';
  }
  var els = Array.from(document.querySelectorAll('a[href]')).filter(vis);
  return JSON.stringify(els.map(function(el){
    return {text: (el.innerText || el.getAttribute('aria-label') || el.href || '').trim().slice(0,120),
            href: el.href};
  }));
})();")

(defun jwebkit-follow-link (&optional new-session)
  "Pick a visible link with completion.
With NEW-SESSION (or prefix), open in a new xwidget session."
  (interactive "P")
  (let ((xw (jwebkit--session)))
    (xwidget-webkit-execute-script
     xw jwebkit--collect-links-js
     (lambda (raw)
       (let ((rows (jwebkit--parse-json raw)))
         (unless rows
           (user-error "No visible links"))
         (let* ((cands (mapcar
                        (lambda (r)
                          (let ((text (or (alist-get 'text r) ""))
                                (href (or (alist-get 'href r) "")))
                            (cons (if (string-empty-p text) href
                                    (format "%s  %s" text href))
                                  href)))
                        rows))
                (pick (completing-read "Link: " cands nil t))
                (href (cdr (assoc pick cands))))
           (when (and href (not (string-empty-p href)))
             (xwidget-webkit-browse-url href new-session))))))))


;;; Ace hints

(defconst jwebkit--ace-js
  "(function(keys){
  if (window.__jwAce) window.__jwAce.remove();
  function vis(el){
    var r = el.getBoundingClientRect();
    var s = getComputedStyle(el);
    return r.width>1 && r.height>1 && r.bottom>0 && r.top<innerHeight
      && r.right>0 && r.left<innerWidth
      && s.visibility!=='hidden' && s.display!=='none' && s.opacity!=='0';
  }
  var sel = 'a[href],button,[role=button],input:not([type=hidden]),textarea,select,summary,[onclick],[data-act],[data-id]';
  var els = Array.from(document.querySelectorAll(sel)).filter(vis);
  var k = keys.length;
  var n = Math.min(els.length, k*k);
  var width = n<=k ? 1 : 2;
  var root = document.createElement('div');
  root.id = 'jwebkit-ace';
  root.style.cssText = 'position:fixed;inset:0;z-index:2147483647;pointer-events:none;';
  var map = {};
  function lab(i){
    return width===1 ? keys[i] : keys[Math.floor(i/k)] + keys[i%k];
  }
  for (var i=0;i<n;i++){
    var el = els[i];
    var r = el.getBoundingClientRect();
    var L = lab(i);
    map[L] = true;
    el.setAttribute('data-jw-ace', L);
    var sp = document.createElement('span');
    sp.textContent = L;
    sp.style.cssText = 'position:fixed;left:'+r.left+'px;top:'+r.top+'px;background:#ffd60a;color:#111;font:bold 12px/1.2 ui-monospace,monospace;padding:1px 4px;border-radius:2px;box-shadow:0 1px 2px rgba(0,0,0,.4);';
    root.appendChild(sp);
  }
  document.documentElement.appendChild(root);
  window.__jwAce = {
    remove: function(){
      root.remove();
      document.querySelectorAll('[data-jw-ace]').forEach(function(e){ e.removeAttribute('data-jw-ace'); });
      window.__jwAce = null;
    },
    act: function(L, news){
      var el = document.querySelector('[data-jw-ace=\"'+L+'\"]');
      var href = el && el.href ? el.href : '';
      window.__jwAce.remove();
      if (!el) return '';
      if (news && href) return href;
      el.focus();
      el.click();
      return href;
    }
  };
  return JSON.stringify(Object.keys(map));
})")

(defun jwebkit--ace-inject-js ()
  (concat jwebkit--ace-js "(" (json-encode jwebkit-ace-keys) ");"))

(defun jwebkit--read-label (labels)
  "Read hint keys until they uniquely match LABELS.  C-g / ESC abort."
  (unless labels
    (user-error "No visible targets"))
  (catch 'done
    (let ((sofar ""))
      (while t
        (let* ((ch (read-key (format "Hint%s: "
                                     (if (string-empty-p sofar) ""
                                       (concat " " sofar)))))
               (c (cond
                   ((memq ch '(7 27)) (throw 'done nil))
                   ((eq ch 'escape) (throw 'done nil))
                   ((characterp ch) (char-to-string ch))
                   (t nil))))
          (unless c (throw 'done nil))
          (setq sofar (concat sofar c))
          (cond
           ((member sofar labels)
            (throw 'done sofar))
           ((not (cl-some (lambda (l) (string-prefix-p sofar l)) labels))
            (message "No hint %s" sofar)
            (throw 'done nil))))))))

(defun jwebkit-ace (&optional new-session)
  "Hint visible clickable elements, then click.
With NEW-SESSION (or prefix), open an `<a href>` in a new session."
  (interactive "P")
  (let ((xw (jwebkit--session)))
    (xwidget-webkit-execute-script
     xw
     (jwebkit--ace-inject-js)
     (lambda (raw)
       (let ((labels (jwebkit--parse-labels raw))
             (acted nil))
         (unless labels
           (user-error "No visible targets"))
         (unwind-protect
             (let ((lab (jwebkit--read-label labels)))
               (when lab
                 (setq acted t)
                 (xwidget-webkit-execute-script
                  xw
                  (format "window.__jwAce && window.__jwAce.act(%s,%s);"
                          (json-encode lab)
                          (if new-session "true" "false"))
                  (lambda (href)
                    (when (and new-session href (stringp href)
                               (not (string-empty-p href)))
                      (xwidget-webkit-browse-url href t))))))
           (unless acted
             (xwidget-webkit-execute-script
              xw "window.__jwAce && window.__jwAce.remove();"))))))))

(defun jwebkit-ace-new ()
  "Ace-hint and open links in a new session."
  (interactive)
  (jwebkit-ace t))


;;; Keys

(defun jwebkit--bind (states key fn)
  (dolist (st (ensure-list states))
    (if (fboundp 'evil-define-key*)
        (evil-define-key* st xwidget-webkit-mode-map key fn)
      (define-key xwidget-webkit-mode-map key fn))))

(defvar jwebkit--setup-done nil)

;;;###autoload
(defun jwebkit-setup ()
  "Install jwebkit keys, cookie file and PDF routing.  Safe to call more than once."
  (interactive)
  (jwebkit-pdf-enable)
  (when (and jwebkit-cookie-file
             (boundp 'xwidget-webkit-cookie-file)
             (null xwidget-webkit-cookie-file))
    (setq xwidget-webkit-cookie-file jwebkit-cookie-file))
  (unless jwebkit--setup-done
    (setq jwebkit--setup-done t)
    (jwebkit--bind '(normal motion)
                   (kbd "/") #'jwebkit-find-start)
    (jwebkit--bind '(normal motion)
                   (kbd "n") #'jwebkit-find-next)
    (jwebkit--bind '(normal motion)
                   (kbd "N") #'jwebkit-find-prev)
    (jwebkit--bind '(normal motion)
                   (kbd "*") #'jwebkit-find-selection)
    (jwebkit--bind '(normal motion)
                   (kbd "f") #'jwebkit-ace)
    (jwebkit--bind '(normal motion)
                   (kbd "F") #'jwebkit-ace-new)
    (jwebkit--bind '(normal motion)
                   (kbd "gf") #'jwebkit-follow-link)
    (jwebkit--bind '(normal motion)
                   (kbd "i") #'jwebkit-edit)
    (jwebkit--bind '(normal motion)
                   (kbd "gi") #'jwebkit-focus-input)
    (jwebkit--bind '(normal motion)
                   (kbd "Y") #'xwidget-webkit-copy-selection-as-kill)
    (jwebkit--bind '(normal motion)
                   (kbd "&") #'jwebkit-open-external)
    (jwebkit--bind '(normal motion)
                   (kbd "gp") #'jwebkit-pdf-goto-page)
    (jwebkit--bind '(normal motion)
                   (kbd "gO") #'jwebkit-pdf-outline)
    (jwebkit--bind 'emacs
                   (kbd "<escape>") #'jwebkit-stop-edit)
    (jwebkit--bind 'emacs
                   (kbd "C-g") #'jwebkit-stop-edit)
    (define-key xwidget-webkit-edit-mode-map (kbd "<escape>") #'jwebkit-stop-edit)
    (define-key xwidget-webkit-edit-mode-map (kbd "C-g") #'jwebkit-stop-edit)))

(provide 'jwebkit)
;;; jwebkit.el ends here

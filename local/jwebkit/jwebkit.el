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
;;   f / F   ace hints (current / new session); onto a field, then edit
;;   gf      follow link via completing-read
;;   i       pass keys through to the page; ⌘V (M-v) there pastes
;;   gi      focus first input, then edit
;;   *       search for the current selection
;;   Y       yank page selection
;;   &       open current URL in the system browser
;;   gp gO   PDF page / outline (pdf.js viewer; see jwebkit-pdf.el)
;;   gr      Markdown reload (see jwebkit-md.el)

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'xwidget)
(require 'jwebkit-pdf)
(require 'jwebkit-md)

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

(defconst jwebkit--blur-js
  "(function(){
  var a = document.activeElement;
  if (a && a !== document.body && a.blur) a.blur();
})();"
  "Take the page's focus off the field it is in.")

(defun jwebkit-stop-edit ()
  "Stop sending keys to the page.
The field you were typing in lets go of the focus too: a field focused
is where WebKit keeps the keys -- a click had made its view the window's
first responder -- and Emacs would see none.  The view hands the
keyboard back only as `jwebkit--give-focus-back' does it, and when it
may."
  (interactive)
  (when (bound-and-true-p xwidget-webkit-edit-mode)
    (xwidget-webkit-edit-mode -1))
  (when (fboundp 'evil-normal-state)
    (evil-normal-state))
  (when-let* (((derived-mode-p 'xwidget-webkit-mode))
              (xw (xwidget-webkit-current-session)))
    (ignore-errors (xwidget-webkit-execute-script xw jwebkit--blur-js))
    (jwebkit--give-focus-back)))

;; ⌘V while keys go to the page.
;;
;; WebKit under Emacs never sees ⌘V as a paste: the paste is the menu's
;; to send, and Emacs's menu does not send it to the web view.  While
;; keys go to the page, ⌘V (M-v, ⌘ being Meta) is Emacs's, so Emacs puts
;; the clipboard's text in the focused field, at the caret, as typing
;; would: the page's own input events fire, and its undo keeps it.  The
;; field hears it first, as the xwapp page kit's do: "xw-paste", its
;; detail {text}, cancelable -- "" for a clipboard with no text, which
;; an xwapp page may take for an image to ask for.  The clipboard is read
;; on your key press only: nothing a page does reads it.  A field a click
;; put the keys in is WebKit's, and its ⌘V never reaches Emacs.

(defun jwebkit--paste-js (text)
  "JS putting TEXT in the page's focused field, saying what it did:
\"pasted\", \"taken\" (the page took it), \"empty\" (no text) or
\"nofield\"."
  (format "(function(t){
  var el = document.activeElement;
  while (el && el.tagName === 'IFRAME') {
    try { el = el.contentDocument.activeElement; } catch (e) { return 'nofield'; }
  }
  var ok = el && !el.disabled && !el.readOnly && (el.isContentEditable || el.tagName === 'TEXTAREA' ||
    (el.tagName === 'INPUT' && /^(text|search|url|tel|email|password)$/i.test(el.type || 'text')));
  if (!ok) return 'nofield';
  var ev = new CustomEvent('xw-paste', {bubbles: true, cancelable: true, detail: {text: t}});
  if (!el.dispatchEvent(ev)) return 'taken';
  if (!t) return 'empty';
  if (!el.ownerDocument.execCommand('insertText', false, t) && typeof el.setRangeText === 'function') {
    el.setRangeText(t, el.selectionStart, el.selectionEnd, 'end');
    el.dispatchEvent(new InputEvent('input', {bubbles: true, inputType: 'insertFromPaste', data: t}));
  }
  return 'pasted';
})(%s);"
          (json-encode text)))

(defun jwebkit-paste ()
  "Paste the clipboard's text into the page's focused field, at the caret.
On your own key press only (⌘V while keys go to the page): called from
Lisp, it reads nothing."
  (interactive)
  (when (called-interactively-p 'any)
    (let ((text (or (ignore-errors (gui-get-selection 'CLIPBOARD 'STRING)) "")))
      (xwidget-webkit-execute-script
       (jwebkit--session) (jwebkit--paste-js (if (stringp text) (substring-no-properties text) ""))
       (lambda (said)
         (pcase said
           ("nofield" (message "No text field has focus in the page"))
           ("empty" (message "No text on the clipboard"))))))))

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
  var sel = 'a[href],button,[role=button],input:not([type=hidden]),textarea,select,summary,[onclick],[data-act],[data-id],[contenteditable]:not([contenteditable=false])';
  function typable(el){
    if (el.disabled || el.readOnly) return false;
    if (el.isContentEditable || el.tagName === 'TEXTAREA' || el.tagName === 'SELECT') return true;
    return el.tagName === 'INPUT' && !/^(button|submit|reset|checkbox|radio|file|image|color|range)$/i.test(el.type);
  }
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
      if (!el) return JSON.stringify({href: ''});
      if (news && href) return JSON.stringify({href: href});
      el.focus();
      el.click();
      return JSON.stringify({href: href, edit: typable(el) && document.activeElement === el});
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

(defun jwebkit--acted (buf new-session raw)
  "What `jwebkit-ace' does once the page acted on a hint, RAW saying
what: open its link in a new session (NEW-SESSION), or, the hint having
put the focus in a field, send the keys to the page from BUF -- else
typing there would run Evil's commands."
  (let* ((got (jwebkit--parse-json raw))
         (href (alist-get 'href got)))
    (cond
     ((and new-session (stringp href) (not (string-empty-p href)))
      (xwidget-webkit-browse-url href t))
     ((and (alist-get 'edit got) (buffer-live-p buf))
      (with-current-buffer buf (jwebkit-edit))))))

(defun jwebkit-ace (&optional new-session)
  "Hint visible clickable elements, then click.
With NEW-SESSION (or prefix), open an `<a href>` in a new session.
A field -- a text input, a textarea, a select, an editable element --
is focused, and keys go to the page, as after `gi'."
  (interactive "P")
  (let ((xw (jwebkit--session))
        (buf (current-buffer)))
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
                  (lambda (said) (jwebkit--acted buf new-session said)))))
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

;;; Keyboard focus when a page goes (macOS)
;;
;; A click in a page makes WebKit's view the window's first responder;
;; keys then reach Emacs through it, forwarded unless an input has focus.
;; Killing the buffer runs nsxwidget_kill, which removes that view
;; without handing first responder back.  Keys land on the bare
;; NSWindow: nobody takes them, macOS beeps -- Esc is where one notices
;; -- and Emacs sees none of them until a click on Emacs restores it.
;;
;; No Lisp primitive sets the first responder.  The one path in
;; nsxwidget.m that hands it back is the page's own C-g message
;; (xwKeyDown posts it; the handler calls makeFirstResponder with the
;; Emacs view), and `kill-buffer-hook' runs before the xwidget is
;; killed.  So post that message from the page and let the run loop
;; deliver it.
;;
;; The handler reads the xwidget's view pointer, which is NULL once the
;; view is gone: posting then would crash Emacs.  Hence only for a page
;; shown in the selected window with a live view -- the only case where
;; it can hold focus anyway -- and a wait that does not redisplay, so
;; the view cannot go in between.  Late delivery is safe: nsxwidget_kill
;; removes the handler before anything else.  Switching away from a page
;; is fine: redisplay only moves its view off screen
;; (nsxwidget_hide_view), where it stays first responder and passes keys
;; it does not use on to Emacs.

(defconst jwebkit--give-focus-back-js
  "window.webkit && window.webkit.messageHandlers.keyDown && window.webkit.messageHandlers.keyDown.postMessage('C-g');"
  "What xwidget's own script sends on C-g in a page.")

(defun jwebkit--give-focus-back ()
  "Hand keyboard focus back to Emacs from this buffer's page, when it may:
before the buffer dies, and as you stop typing in it (`jwebkit-stop-edit')."
  (when (and (eq (framep (selected-frame)) 'ns)
             (derived-mode-p 'xwidget-webkit-mode)
             (eq (window-buffer (selected-window)) (current-buffer))
             ;; One view only: deleting one of two views clears the
             ;; pointer the handler reads while the other is still up.
             (= 1 (length (get-buffer-window-list (current-buffer) nil t))))
    (let ((posted nil))
      (dolist (xw (ignore-errors (get-buffer-xwidgets (current-buffer))))
        (when (and (xwidget-live-p xw)
                   (xwidget-view-lookup xw (selected-window)))
          (ignore-errors
            (xwidget-webkit-execute-script xw jwebkit--give-focus-back-js)
            (setq posted t))))
      ;; Not `sit-for': a redisplay here could delete the view.
      (when posted
        (accept-process-output nil 0.1)))))

;; evil-collection binds f and gi to `undefined' in xwidget (its TODO
;; for link hints), and does so whenever `evil-collection-init' gets to
;; xwidget.  With evil on `:defer 0' that is *after* `jwebkit-setup':
;; `:after evil-collection' fires when the feature is provided, before
;; evil-collection's own `:config' calls init.  Binding again from its
;; setup hook makes the order irrelevant.

(defvar jwebkit--setup-done nil)

(defun jwebkit--bind-keys ()
  "Bind jwebkit's keys in `xwidget-webkit-mode-map'."
  (jwebkit--bind '(normal motion) (kbd "/") #'jwebkit-find-start)
  (jwebkit--bind '(normal motion) (kbd "n") #'jwebkit-find-next)
  (jwebkit--bind '(normal motion) (kbd "N") #'jwebkit-find-prev)
  (jwebkit--bind '(normal motion) (kbd "*") #'jwebkit-find-selection)
  (jwebkit--bind '(normal motion) (kbd "f") #'jwebkit-ace)
  (jwebkit--bind '(normal motion) (kbd "F") #'jwebkit-ace-new)
  (jwebkit--bind '(normal motion) (kbd "gf") #'jwebkit-follow-link)
  (jwebkit--bind '(normal motion) (kbd "i") #'jwebkit-edit)
  (jwebkit--bind '(normal motion) (kbd "gi") #'jwebkit-focus-input)
  (jwebkit--bind '(normal motion) (kbd "Y") #'xwidget-webkit-copy-selection-as-kill)
  ;; ⌘C too (⌘ is meta here), the habit from everywhere else -- copies the
  ;; page selection, as Y does.  WebKit forwards the key to Emacs unless an
  ;; input has focus, where it copies within the page itself instead.
  (jwebkit--bind '(normal motion) (kbd "M-c") #'xwidget-webkit-copy-selection-as-kill)
  (jwebkit--bind '(normal motion) (kbd "&") #'jwebkit-open-external)
  (jwebkit--bind '(normal motion) (kbd "gp") #'jwebkit-pdf-goto-page)
  (jwebkit--bind '(normal motion) (kbd "gO") #'jwebkit-pdf-outline)
  (jwebkit--bind '(normal motion) (kbd "gr") #'jwebkit-md-reload)
  (jwebkit--bind 'emacs (kbd "<escape>") #'jwebkit-stop-edit)
  (jwebkit--bind 'emacs (kbd "C-g") #'jwebkit-stop-edit)
  (define-key xwidget-webkit-edit-mode-map (kbd "<escape>") #'jwebkit-stop-edit)
  (define-key xwidget-webkit-edit-mode-map (kbd "C-g") #'jwebkit-stop-edit)
  ;; ⌘V, while keys go to the page: see "⌘V while keys go to the page".
  (define-key xwidget-webkit-edit-mode-map (kbd "M-v") #'jwebkit-paste))

(defun jwebkit--after-evil-collection (mode _keymaps)
  "Bind again once evil-collection has set MODE up, if MODE is xwidget."
  (when (eq mode 'xwidget)
    (jwebkit--bind-keys)))

;;;###autoload
(defun jwebkit-setup ()
  "Install jwebkit keys, cookie file, PDF routing and Markdown preview.  Safe to call more than once."
  (interactive)
  (jwebkit-pdf-enable)
  (jwebkit-md-enable)
  (when (and jwebkit-cookie-file
             (boundp 'xwidget-webkit-cookie-file)
             (null xwidget-webkit-cookie-file))
    (setq xwidget-webkit-cookie-file jwebkit-cookie-file))
  (unless jwebkit--setup-done
    (setq jwebkit--setup-done t)
    (jwebkit--bind-keys)
    (add-hook 'evil-collection-setup-hook #'jwebkit--after-evil-collection)
    (add-hook 'kill-buffer-hook #'jwebkit--give-focus-back)))

(provide 'jwebkit)
;;; jwebkit.el ends here

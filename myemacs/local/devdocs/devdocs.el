;; devdocs.el --- Quick doc search on devdocs.io -*- lexical-binding: t; -*-
;; To use, type M-x devdocs-search

;; you can M-x custom-group and input "emaca" to see the all groups
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Customization-Groups.html#Customization-Groups
(defgroup devdocs nil
  "Search doc by devdocs."
  :group 'external)

;; Emacs' groups has the hierarchy concept so the above define a group named devdocs
;; and is a child of external group

(defcustom devdocs-url "https://devdocs.io"
  "the url of devdocs.io"
  :type 'string
  :group 'devdocs)

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Customization-Types.html#Customization-Types
(defcustom devdocs-alist
  '((c-mode . "c")
    (c++-mode . "c++")
    (css-mode . "css")
    (rust-mode . "rust")
    (go-mode . "go")
    (haskell-mode . "haskell")
    (js2-mode . "javascript")
    (python-mode . "python")
    (php-mode . "php"))
  "major-mode maps to documentation on devdocs.io"
  :type '(repeat (cons (symbol :tag "major mode")
                       (string :tag "devdocs doc")))
  :group 'devdocs)

(defun devdocs--get-search-target ()
  "Get the search content from the region or thing-at-point"
  (let ((document (cdr (assoc major-mode devdocs-alist)))
        (query (if (use-region-p)
                   (buffer-substring (region-beginning) (region-end))
                 (thing-at-point 'symbol))))
    (string-trim (string-join (list document query) " "))))

(defun devdocs-do-search (content)
  "Open the browser and go to the url"
  (browse-url
   (format "%s/#q=%s" devdocs-url (url-hexify-string content))))

;;;###autoload
(defun devdocs-search (&optional confirm)
  "Search the content on the devdocs"
  (interactive "P")
  (let ((content (funcall 'devdocs--get-search-target)))
    (when confirm
      (setq content (read-string "Searching devdocs: " content)))
    (devdocs-do-search content)))


(provide 'devdocs)

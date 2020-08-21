(with-eval-after-load 'exercism
  (evil-define-key 'normal exercism-mode-map (kbd "g s") 'exercism-submit))

;; try to bind the this function with evil key maps
;; TODO: rethink the key binding
(evil-define-key 'visual 'global
  (kbd "g y") 'copy-region-and-base64-decode
  (kbd "g e") 'copy-region-and-urlencode)


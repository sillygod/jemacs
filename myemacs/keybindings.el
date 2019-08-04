(with-eval-after-load 'exercism
  (evil-define-key 'normal exercism-mode-map (kbd "g s") 'exercism-submit))

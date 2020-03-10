(defconst configs-packages
  '(
    evil
    python
    conda
    dap-mode
    diff-hl
    go-mode))

(defun configs/post-init-evil ()
  ;; the spacemacs use the evil-escape pacakge to acheive the 'evil-force-normal-state'
  ;; function. the following is the my configuration.
  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-unordered-key-sequence t)
  (setq-default evil-escape-delay 0.1))

(defun configs/post-init-go-mode ()
  (setq go-format-before-save t))

(defun configs/post-init-go-mode ()
  (evil-define-key 'normal go-mode-map (kbd "g i") 'lsp-find-implementation)
  (evil-define-key 'normal go-mode-map (kbd "g d") 'lsp-find-definition)
  (evil-define-key 'normal go-mode-map (kbd "g h") 'lsp-describe-thing-at-point))

(defun configs/pre-init-dap-mode ()
  (add-to-list 'spacemacs--dap-supported-modes 'go-mode))

(defun configs/pre-init-diff-hl ()
  (let* ((height (frame-char-height))
         (width 2)
         (ones (1- (expt 2 width)))
         (bits (make-vector height ones)))
    (define-fringe-bitmap 'my-diff-hl-bitmap bits height width))
  (setq diff-hl-fringe-bmp-function (lambda (type pos) 'my-diff-hl-bitmap)))

(defun configs/post-init-python ()
  ;;   (add-hook 'before-save-hook
  ;;             (lambda ()
  ;;               (when (eq major-mode 'python-mode)
  ;;                 (spacemacs/python-format-buffer))))
  (evil-define-key 'normal python-mode-map (kbd "g h") 'lsp-describe-thing-at-point))


(defun configs/pre-init-conda ()
  (setq conda-anaconda-home "/Users/jing/miniconda2"))


(defun configs/init-conda ()
  (use-package conda
    :config
    (conda-env-initialize-interactive-shells)))

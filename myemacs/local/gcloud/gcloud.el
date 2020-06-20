;; gcloud.el -- simplify the workflow when using the gcloud sdk to manipulate some daily operation
;;
;; provide a magit like interface to operate the gcloud command

;;;###autoload
(progn
  (defgroup gcloud nil
    ""
    :group 'external)

  ;;
  )

(defvar *gcloud-exec*
  (or gcloud-bin
      (replace-regexp-in-string "\n" ""
                                (shell-command-to-string "which gcloud")))
  "Set the gcloud exectuable path.")

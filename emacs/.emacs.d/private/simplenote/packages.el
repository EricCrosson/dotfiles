;;; packages.el --- simplenote Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar simplenote-packages '(simplenote2))

(defun simplenote/init-simplenote2 ()
  "Initialize `simplenote2'."
  (use-package simplenote2
    :defer t
    :commands
    simplenote2-browse
    simplenote2-push-buffer
    simplenote2-pull-buffer
    :init
    (setq simplenote2-email "simplenote@ericcrosson.com")
    (simplenote2-setup)

    (evil-leader/set-key
      "os" 'simplenote2-browse)))

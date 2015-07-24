;;; packages.el --- twittering Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq twittering-packages '(twittering-mode))

(defun twittering/init-twittering-mode ()
  "Initialize `twittering-mode'."
  (use-package twittering-mode
    :defer t
    :commands twit
    :init
    (spacemacs/declare-prefix "aS" "social")
    (evil-leader/set-key
      "aSt" 'twit)
    :config
    (setq twittering-use-master-password t)))

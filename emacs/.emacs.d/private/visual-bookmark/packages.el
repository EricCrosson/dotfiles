;;; packages.el --- visual-bookmark Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq visual-bookmark-packages '(bm))

(defun visual-bookmark/init-bm ()
  "Initialize my package"
  (use-package bm
    :defer t
    :commands bm-toggle bm-next bm-previous
    :config
    (evil-leader/set-key
      "md" bm-toggle
      "mp" bm-previous
      "mn" bm-next)))

;;; packages.el --- midnight Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar midnight-packages '(midnight))

(defun midnight/init-midnight ()
  "Initialize my package"
  (use-package midnight
    :defer t
    :config (midnight-delay-set 'midnight-delay "5:00am")))

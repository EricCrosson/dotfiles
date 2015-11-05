;;; packages.el --- bury-successful-compilation layer packages file for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq bury-successful-compilation-packages
  '(bury-successful-compilation))

(defun bury-successful-compilation/init-bury-successful-compilation ()
  "Initialize bury successful compilation."
  (use-package bury-successful-compilation
    :config (bury-successful-compilation 1)))

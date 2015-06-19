;;; packages.el --- ibuffer-extras Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq ibuffer-extras-packages '(ibuffer-vc ibuffer-tramp))

(defun ibuffer-extras/init-ibuffer-vc ()
  "Initialize `ibuffer-vc'."
  (use-package ibuffer-vc))

(defun ibuffer-extras/init-ibuffer-tramp ()
  "Initialize `ibuffer-tramp'."
  (use-package ibuffer-tramp))

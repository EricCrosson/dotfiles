;;; packages.el --- unkillable-scratch Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar unkillable-scratch-packages
  '(unkillable-scratch))

(defun unkillable-scratch/init-unkillable-scratch ()
  "Initialize my package"
  (use-package unkillable-scratch
    :defer t
    :config
    (setq unkillable-scratch-behavior 'bury)))

;;; packages.el --- evil-extra-text-objects Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq evil-extra-text-objects-packages '(evil-extra-text-objects))

(defun evil-extra-text-objects/init-evil-extra-text-objects ()
  "Initialize `evil-extra-text-objects'."
  (use-package evil-extra-text-objects))

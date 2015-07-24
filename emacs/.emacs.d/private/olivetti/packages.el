;;; packages.el --- olivetti Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq olivetti-packages '(olivetti))

(defun olivetti/init-olivetti ()
  "Initialize `olivetti'."
  (use-package olivetti
    :defer t
    :config
    (evil-leader/set-key "oo" 'olivetti-mode)))

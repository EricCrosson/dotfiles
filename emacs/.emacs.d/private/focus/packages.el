;;; packages.el --- focus Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar focus-packages '(focus)) 

(defun focus/init-focus ()
  "Initialize `focus'."
  (use-package focus
    :defer t
    :commands focus-mode
    :config
    (evil-leader/set-key
      "of" 'focus-mode)))

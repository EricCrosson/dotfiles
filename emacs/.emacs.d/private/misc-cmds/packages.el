;;; packages.el --- misc-cmds Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar misc-cmds-packages '(misc-cmds)) 

(defun misc-cmds/init-misc-cmds ()
  "Initialize `misc-cmds'."
  (use-package misc-cmds
    :defer t
    :commands revert-buffer-no-confirm
    :config
    (evil-leader/set-key
      "rn" 'revert-buffer-no-confirm)))

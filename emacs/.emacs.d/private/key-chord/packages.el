;;; packages.el --- key-chord Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq key-chord-packages '(key-chord))

(defun key-chord/init-key-chord ()
  "Initialize `key-chord'."
  (use-package key-chord
    :defer t
    :config
    (key-chord-mode 1)
    ;; TODO; remove this mapping inside of helm
    ;; (key-chord-define-global "" 'helm-M-x)
    ))

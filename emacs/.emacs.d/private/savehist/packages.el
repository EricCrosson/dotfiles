;;; packages.el --- savehist Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq savehist-packages '(savehist))

(defun savehist/init-savehist ()
  "Initialize `savehist'."
  (use-package savehist
    :config
    (setq savehist-save-minibuffer-history 1)
    (setq savehist-additional-variables
          '(kill-ring
            search-ring
            regexp-search-ring))
    (savehist-mode 1)))

;;; packages.el --- sublimity Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq sublimity-packages '(sublimity))

(defun sublimity/init-sublimity ()
  "Initialize `sublimity'."
  (use-package sublimity
    :defer t
    :config
    (mapc (lambda (mode) (require mode))
          '(sublimity-scroll
            sublimity-map
            sublimity-attractive))
    (evil-leader/set-key
      "tM" 'sublimity-mode)))

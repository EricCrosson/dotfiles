;; packages.el --- bliss Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq bliss-packages '(bliss-theme))

(defun bliss/init-bliss-theme ()
  "Initialize `bliss-theme'."
  (use-package bliss-theme
    :config
    ;; (eval-after-load 'bliss-theme
    ;;   (mapc (lambda (data) (set-face-attribute (car data) nil
    ;;                                     :underline  nil
    ;;                                     :foreground 'unspecified
    ;;                                     :background (cadr data)))
    ;;         '((highlight "#333")
    ;;           (region    "#444"))))
    ))

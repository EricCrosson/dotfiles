;;; packages.el --- swiper Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq swiper-packages '(swiper))

(defun swiper/init-swiper ()
  "Initialize my package"
  (use-package swiper
    :bind ("C-." . swiper)
    :config (evil-leader/set-key "." 'swiper))
  )

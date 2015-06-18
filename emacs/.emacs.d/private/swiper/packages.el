;;; packages.el --- swiper Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq swiper-packages '(swiper))

(defun swiper/init-swiper ()
  "Initialize `swiper'."
  (use-package swiper
    :config
    (evil-leader/set-key "." 'swiper)))

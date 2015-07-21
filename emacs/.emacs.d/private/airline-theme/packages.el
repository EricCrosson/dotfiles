;;; packages.el --- airline-theme Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq airline-theme-packages '(airline-themes))

(defun airline-theme/init-airline-themes ()
  "Initialize `airline-themes'."
  (use-package airline-themes))

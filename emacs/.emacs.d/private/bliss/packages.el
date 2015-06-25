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
    (after 'bliss-theme
      (set-face-attribute 'highlight nil :underline  nil :foreground 'unspecified :background "#333")
      (set-face-attribute 'region nil :underline  nil :foreground 'unspecified :background "#444"))))

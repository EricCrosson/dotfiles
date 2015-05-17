;;; packages.el --- org-cliplink Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq org-cliplink-packages '(org-cliplink))

(defun org-cliplink/init-org-cliplink ()
  "Initialize my package"
  (use-package org-cliplink)
  )

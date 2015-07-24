;;; packages.el --- offlineimap Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq offlineimap-packages '(offlineimap))

(defun offlineimap/init-offlineimap ()
  "Initialize my package"
  (use-package offlineimap
    :defer t
    :config
    (evil-leader/set-key
      "amo" 'offlineimap)))

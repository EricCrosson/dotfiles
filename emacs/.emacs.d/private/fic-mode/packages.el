;;; packages.el --- fic-mode Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq fic-mode-packages '(fic-mode))

(defun fic-mode/init-fic-mode ()
  "Initialize my package"
  (use-package fic-mode
    :defer t
    :config
    (add-hook 'prog-mode-hook 'turn-on-fic-mode)

    (spacemacs|add-toggle fic-mode
                          :status fic-mode
                          :on (fic-mode)
                          :off (fic-mode -1)
                          :documentation "FIC mode."
                          :evil-leader "tP")
    (spacemacs|diminish fic-mode " Ⓟ" " P")))

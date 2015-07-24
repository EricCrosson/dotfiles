;;; packages.el --- flx-ido Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq flx-ido-packages
  '(flx-ido
    ido-hacks
    ido-vertical-mode))

(defun flx-ido/init-ido-hacks ()
  "Initialize my package"
  (use-package ido-hacks
    :defer t
    :config (ido-hacks-mode 1)))

(defun flx-ido/init-flx-ido ()
  "Initialize my package"
  (use-package flx-ido
    :defer t
    :config
    (add-to-list 'ido-ignore-buffers "\\*Async Shell Command\\*")
    (add-to-list 'ido-ignore-buffers "^\\*.*\\.*preprocessed\\*")
    (ido-mode t)
    (setq ido-use-virtual-buffers t)
    (ido-everywhere 1)
    (flx-ido-mode 1)
    ;; disable ido faces to see flx highlights.
    (setq ido-enable-flex-matching t
          ido-use-faces nil)
    (setq ido-create-new-buffer 'always
          ido-file-extensions-order '(".org" ".el"
                                      ".c" ".c"
                                      ".dox" ".txt"))))

(defun flx-ido/init-ido-vertical-mode ()
  "Initialize my package"
  (use-package ido-vertical-mode
    :defer t
    :config (ido-vertical-mode 1)))

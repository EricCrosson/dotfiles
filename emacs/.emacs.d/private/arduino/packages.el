;;; packages.el --- arduino Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq arduino-packages
      '(arduino-mode
        ;; TODO: implement company mode. See instructions on the
        ;; [[https://github.com/yuutayamada/company-arduino#prerequisite][GitHub]]
        ;; company-arduino
        ))


(defun arduino/init-arduino-mode ()
  "Initialize `arduino-mode'."
  (use-package arduino-mode
    :defer t
    :commands (arduino-mode)
    :mode "\\.ino\\*"))

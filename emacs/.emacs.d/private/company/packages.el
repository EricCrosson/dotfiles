;;; packages.el --- company Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq company-packages '(company))

(defun company/init-company ()
  "Initialize my package"
  (use-package company
    :config
    (setf company-idle-delay 0
          company-minimum-prefix-length 2
          company-show-numbers t
          company-selection-wrap-around t
          company-dabbrev-ignore-case t
          company-dabbrev-ignore-invisible t
          company-dabbrev-downcase nil)
    (global-company-mode t)

    ;; Sometimes `company-mode' isn't the best option.
    (defun turn-off-company-mode ()
      (company-mode -1))
    (mapc (lambda (mode-hook)
            (add-hook mode-hook 'turn-off-company-mode))
          '(shell-mode-hook
            gud-mode-hook)))
  )

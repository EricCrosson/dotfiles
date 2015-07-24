;;; packages.el --- writegood-mode Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq writegood-mode-packages '(writegood-mode))

(defun writegood-mode/init-wrotegood-mode ()
  "Initialize `writegood-mode'."
  (use-package writegood-mode
    :defer t
    :config
    (mapc (lambda (hook) (add-hook hook 'writegood-mode))
          ;; Activate `writegood-mode' with the following modes:
          '(fundamental-mode-hook
            org-mode-hook
            text-mode-hook
            latex-mode-hook))))

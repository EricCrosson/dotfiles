;;; packages.el --- pretty-lambdada Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq pretty-lambdada-packages '(pretty-lambdada))

(defun pretty-lambdada/init-pretty-lambdada ()
  "Initialize my package"
  (use-package pretty-lambdada
    :defer t
    :config
    (mapc (lambda (prettify-this-mode)
            (add-hook prettify-this-mode 'pretty-lambda))
          '(emacs-lisp-mode-hook
            org-mode-hook ;to include source blocks :\
            enh-ruby-mode-hook
            ruby-mode-hook))))

;;; packages.el --- highlight-stages Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq highlight-stages-packages '(highlight-stages))

(defun highlight-stages/init-highlight-stages ()
  "Initialize `highlight-stages'."
  (use-package highlight-stages
    :defer t
    :config
    (highlight-stages-global-mode t)
    ;; TODO: diminish Stg in the mode line properly
    (after 'highlight-parentheses
      (spacemacs|diminish highlight-parentheses-mode " ⓟ" " p"))
    (add-hook 'prog-mode-hook 'highlight-parentheses-mode)))

;;; packages.el --- vagrant Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq vagrant-packages '(vagrant-tramp))

(defun vagrant/init-vagrant-tramp ()
  "Initialize `vagrant-tramp'."
  (use-package vagrant-tramp
    :config
    (eval-after-load 'tramp
      '(vagrant-tramp-enable))))

;;; packages.el --- org-cliplink Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq org-extras-packages '(org-cliplink org-download))

(defun org-extras/init-org-cliplink ()
  "Initialize `org-cliplink'."
  (use-package org-cliplink
    :defer t
    :config
    (add-hook 'org-mode-hook
              (defun org-mode/define-org-cliplink-leader-keys ()
                (interactive)
                (evil-leader/set-key
                  "ml" 'org-cliplink

                  "mti" 'org-clock-in
                  "mto" 'org-clock-out
                  "mtl" 'org-clock-in-last
                  "mtc" 'org-clock-cancel
                  "mtg" 'org-clock-goto
                  "mtd" 'org-clock-display)))))

;; fixme -- dysfunctional
(defun org-extras/init-org-download ()
  "Initialize `org-download'."
  ;; misc org settings
  (setq org-src-fontify-natively t)

  (use-package org-download
    :defer t
    :config
    (setq org-download-method 'directory
          org-download-image-dir "img")))

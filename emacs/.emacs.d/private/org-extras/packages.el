;;; packages.el --- org-cliplink Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq org-extras-packages '(org-cliplink org-download))

(defun org-extras/init-org-cliplink ()
  "Initialize `org-cliplink'."
  (use-package org-cliplink
    :config
    (add-hook 'org-mode-hook
              (defun org-mode/define-org-cliplink-leader-keys ()
                (interactive)
                (evil-leader/set-key "ml" 'org-cliplink)))))

;; fixme -- dysfunctional
(defun org-extras/init-org-download ()
  "Initialize `org-download'."
  ;; misc org settings
  (setq org-src-fontify-natively t)

  (use-package org-download
    :config
    (setq org-download-method 'directory
          org-download-image-dir "img")))

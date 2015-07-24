;;; packages.el --- browse-web Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq browse-web-packages '(eww))

(defun browse-web/init-eww ()
  "Initialize my package"
  (spacemacs/declare-prefix "B" "browse-web")
  (evil-leader/set-key
    "BB" 'browse-web
    "Bu" 'browse-url
    "Bc" 'browse-url-chromium
    "Bf" 'browse-url-firefox

    "Bl" 'eww-back-url
    "Bb" 'ace-link-eww
    "Bv" 'eww-view-source)

  (defadvice eww-render (after set-eww-buffer-name activate)
    (rename-buffer (concat "*eww-" (or eww-current-title
                                       (if (string-match "://" eww-current-url)
                                           (substring eww-current-url (match-beginning 0))
                                         eww-current-url)) "*") t)))

;;; packages.el --- engine-mode Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq engine-mode-packages '(engine-mode))

(defun engine-mode/init-engine-mode ()
  "Initialize my package"
  (use-package engine-mode
    :defer t
    :config
    (defengine duckduckgo "https://duckduckgo.com/?q=%s")
    (defengine github "https://github.com/search?ref=simplesearch&q=%s")
    (defengine youtube "https://www.youtube.com/results?search_query=%s")
    (evil-leader/set-key
      "swd" 'engine/search-duckduckgo
      "swh" 'engine/search-github
      "swy" 'engine/search-youtube)))

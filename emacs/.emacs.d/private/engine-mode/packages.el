;;; packages.el --- engine-mode Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq engine-mode-packages '(engine-mode))

(defun engine-mode/init-engine-mode ()
  "Initialize my package"
  (use-package engine-mode
    :config
    (defengine duckduckgo "https://duckduckgo.com/?q=%s")
    (defengine github "https://github.com/search?ref=simplesearch&q=%s")
    (defengine google "https://google.com/search?q=%s&{google:RLZ}{google:originalQueryForSuggestion}{google:assistedQueryStats}{google:searchFieldtrialParameter}{google:bookmarkBarPinned}{google:searchClient}{google:sourceId}{google:instantExtendedEnabledParameter}{google:omniboxStartMarginParameter}{google:contextualSearchVersion}ie={inputEncoding}")
    (defengine youtube "https://www.youtube.com/results?search_query=%s")
    (evil-leader/set-key
      "oed" 'engine/search-duckduckgo
      "oeh" 'engine/search-github
      "oeg" 'engine/search-google
      "oey" 'engine/search-youtube))
  )

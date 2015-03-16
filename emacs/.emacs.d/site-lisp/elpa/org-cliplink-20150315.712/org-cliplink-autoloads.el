;;; org-cliplink-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "org-cliplink" "org-cliplink.el" (21766 62985
;;;;;;  995901 524000))
;;; Generated autoloads from org-cliplink.el

(autoload 'org-cliplink-retrieve-title "org-cliplink" "\
Tries to retrieve a title from an HTML page by the given URL
and calls TITLE-CALLBACK callback with URL and the retrieved
title as arguments. If it is not possible to retrive the
title (the HTML page doesn't have a title or URL doesn't point to
an HTML page at all) the TITLE-CALLBACK callback will be called
with URL and nil as arguments.

Example:
  (org-cliplink-retrieve-title
   \"https://google.com/\"
   `(lambda (url title)
      (if title
          (message \"%s has title %s\" url title)
        (message \"%s doesn't have title\" url))))

\(fn URL TITLE-CALLBACK)" nil nil)

(autoload 'org-cliplink-retrieve-title-synchronously "org-cliplink" "\


\(fn URL)" nil nil)

(autoload 'org-cliplink "org-cliplink" "\
Takes a URL from the clipboard and inserts an org-mode link
with the title of a page found by the URL into the current
buffer

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; org-cliplink-autoloads.el ends here

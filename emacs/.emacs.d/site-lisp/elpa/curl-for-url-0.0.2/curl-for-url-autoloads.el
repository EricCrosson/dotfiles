;;; curl-for-url-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "curl-for-url" "curl-for-url.el" (21740 61913
;;;;;;  377437 460000))
;;; Generated autoloads from curl-for-url.el

(autoload 'curl-for-url-install "curl-for-url" "\
Replaces the url-retrieve function with a curl one.

\(fn)" nil nil)

(autoload 'curl-for-url-uninstall "curl-for-url" "\
Restores `url-http' after a `curl-for-url-install'.

\(fn)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; curl-for-url-autoloads.el ends here

;;; apt-utils-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (apt-utils-search apt-utils-show-package) "apt-utils"
;;;;;;  "apt-utils.el" (21537 54415 1688 23000))
;;; Generated autoloads from apt-utils.el

(autoload 'apt-utils-show-package "apt-utils" "\
Show information for a Debian package.
A selection of known packages is presented.  See `apt-utils-mode'
for more detailed help.  If NEW-SESSION is non-nil, generate a
new `apt-utils-mode' buffer.

\(fn &optional NEW-SESSION)" t nil)

(autoload 'apt-utils-search "apt-utils" "\
Search Debian packages for regular expression.
To search for multiple patterns use a string like \"foo && bar\".
The regular expression used to split the
terms (`apt-utils-search-split-regexp') is customisable.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("apt-utils-pkg.el") (21537 54415 26970
;;;;;;  796000))

;;;***

(provide 'apt-utils-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; apt-utils-autoloads.el ends here

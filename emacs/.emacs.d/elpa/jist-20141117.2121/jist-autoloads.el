;;; jist-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (jist-list-starred jist-list-public jist-list-user
;;;;;;  jist-list jist-clone-gist jist-star-gist jist-delete-gist
;;;;;;  jist-auth-buffer-public jist-buffer-public jist-auth-buffer
;;;;;;  jist-buffer jist-auth-region-public jist-region-public jist-auth-region
;;;;;;  jist-region) "jist" "jist.el" (21611 63785 662065 642000))
;;; Generated autoloads from jist.el

(autoload 'jist-region "jist" "\
Create a private gist from BEG and END region.

When PUBLIC is not nil creates a public gist.

\(fn &key (beg (and (use-region-p) (region-beginning))) (end (and (use-region-p) (region-end))) (public nil) (authorized nil))" t nil)

(autoload 'jist-auth-region "jist" "\
Create an authorized gist from an active region.

\(fn)" t nil)

(autoload 'jist-region-public "jist" "\
Create a public gist from an active region.

\(fn)" t nil)

(autoload 'jist-auth-region-public "jist" "\
Create a public and authorized gist from an active region.

\(fn)" t nil)

(autoload 'jist-buffer "jist" "\
Create a gist from the contents of the current buffer.

\(fn)" t nil)

(autoload 'jist-auth-buffer "jist" "\
Create an authorized gist from the contents of the current buffer.

\(fn)" t nil)

(autoload 'jist-buffer-public "jist" "\
Create a public gist from the contents of the current buffer.

\(fn)" t nil)

(autoload 'jist-auth-buffer-public "jist" "\
Create an authorized and public gist from the contents of the current buffer.

\(fn)" t nil)

(autoload 'jist-delete-gist "jist" "\
Delete gist with ID.

\(fn ID)" t nil)

(autoload 'jist-star-gist "jist" "\
Star a gist ID.

\(fn ID)" t nil)

(autoload 'jist-clone-gist "jist" "\
Close gist ID.

\(fn ID)" t nil)

(autoload 'jist-list "jist" "\
Show the list of gists.

\(fn &key (user nil) (public nil) (starred nil))" t nil)

(autoload 'jist-list-user "jist" "\
Show a list of gist of a github USER.

\(fn USER)" t nil)

(autoload 'jist-list-public "jist" "\
Show a list of public gists.

\(fn)" t nil)

(autoload 'jist-list-starred "jist" "\
Show a list of starred gists of the configured user.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("jist-pkg.el") (21611 63785 686971 142000))

;;;***

(provide 'jist-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; jist-autoloads.el ends here

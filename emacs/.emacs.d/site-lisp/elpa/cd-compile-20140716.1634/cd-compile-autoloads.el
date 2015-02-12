;;; cd-compile-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (cd-compile) "cd-compile" "cd-compile.el" (21537
;;;;;;  54413 713686 638000))
;;; Generated autoloads from cd-compile.el

(defvar cd-compile-directory nil "\
Directory in which to run compile.")

(put 'cd-compile-directory 'safe-local-variable 'stringp)

(autoload 'cd-compile "cd-compile" "\
Run compile in a specific directory.
If cd-compile-directory is set then compile will be run in that directory,
otherwise the user will be prompted to enter a directory.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("cd-compile-pkg.el") (21537 54413 738556
;;;;;;  616000))

;;;***

(provide 'cd-compile-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cd-compile-autoloads.el ends here

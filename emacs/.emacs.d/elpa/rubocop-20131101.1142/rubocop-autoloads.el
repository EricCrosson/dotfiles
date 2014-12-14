;;; rubocop-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (rubocop-mode rubocop-autocorrect-current-file
;;;;;;  rubocop-check-current-file rubocop-autocorrect-directory
;;;;;;  rubocop-check-directory rubocop-autocorrect-project rubocop-check-project)
;;;;;;  "rubocop" "rubocop.el" (21428 26940 888246 30000))
;;; Generated autoloads from rubocop.el

(autoload 'rubocop-check-project "rubocop" "\
Run on current project.

\(fn)" t nil)

(autoload 'rubocop-autocorrect-project "rubocop" "\
Run on current project.

\(fn)" t nil)

(autoload 'rubocop-check-directory "rubocop" "\
Run on DIRECTORY if present.
Alternatively prompt user for directory.

\(fn &optional DIRECTORY)" t nil)

(autoload 'rubocop-autocorrect-directory "rubocop" "\
Run on DIRECTORY if present.
Alternatively prompt user for directory.

\(fn &optional DIRECTORY)" t nil)

(autoload 'rubocop-check-current-file "rubocop" "\
Run on current file.

\(fn)" t nil)

(autoload 'rubocop-autocorrect-current-file "rubocop" "\
Run on current file.

\(fn)" t nil)

(autoload 'rubocop-mode "rubocop" "\
Minor mode to interface with RuboCop.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("rubocop-pkg.el") (21428 26940 937753
;;;;;;  782000))

;;;***

(provide 'rubocop-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rubocop-autoloads.el ends here

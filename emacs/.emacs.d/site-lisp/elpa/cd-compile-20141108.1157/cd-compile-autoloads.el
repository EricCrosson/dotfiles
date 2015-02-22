;;; cd-compile-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "cd-compile" "cd-compile.el" (21738 3493 189435
;;;;;;  402000))
;;; Generated autoloads from cd-compile.el

(defvar cd-compile-directory nil "\
Directory in which to run compile.")

(put 'cd-compile-directory 'safe-local-variable 'stringp)

(autoload 'cd-compile "cd-compile" "\
Run compile in a specific directory.
Runs \\[compile] in the directory DIR.

Interactively, uses `cd-compile-directory' for the directory if
non-nil; otherwise prompts the user to enter the directory.

\(fn DIR)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; cd-compile-autoloads.el ends here

;;; update-autoloads-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "update-autoloads" "update-autoloads.el" (21759
;;;;;;  56133 136638 207000))
;;; Generated autoloads from update-autoloads.el

(autoload 'list-files-in-subtree-matching-regexp-recursive "update-autoloads" "\
List the `.el' files in DIRECTORY and in its sub-directories.

If REGEXP is non-nil, compile a list of files matching REGEXP
instead of `.el'.

\(fn DIRECTORY &optional REGEXP)" t nil)

(autoload 'get-project-loaddefs-path "update-autoloads" "\
Return the path to the closest loaddefs.el file relative to BASE.

Only query this defun with BASE as a subdir of ~/.emacs.d/esc-lisp.

\(fn BASE)" t nil)

(autoload 'update-esc-lisp-autoloads "update-autoloads" "\
Update autoload definitions for Lisp files in the directories BASE.
In an interactive call, you must give one argument, the name of a
single directory.  In a call from Lisp, you can supply multiple
directories as separate arguments, but this usage is discouraged.

The function DOES recursively descend into subdirectories of the
directory or directories specified.

Note: this docstring was copied from function `update-directory-autoloads'.

\(fn &optional BASE)" t nil)

;;;***

;;;### (autoloads nil nil ("update-autoloads-pkg.el") (21759 56133
;;;;;;  147517 48000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; update-autoloads-autoloads.el ends here

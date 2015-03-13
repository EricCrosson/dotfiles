;;; mojo-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "mojo" "mojo.el" (21762 28289 527809 476000))
;;; Generated autoloads from mojo.el

(autoload 'byte-compile-directory "mojo" "\
Compile an .elc file for every .el file contained under
      DIR (recursive).

\(fn DIR)" nil nil)

(autoload 'combinations "mojo" "\
Return a list of all possible combinations of the elements of
  LISTS. Think applicative functors from Haskell.

\(fn &rest LISTS)" nil nil)

(autoload 'pull-up-line "mojo" "\
Pull up ARG lines.

\(fn &optional ARG)" t nil)

(autoload 'sudo-edit-current-file "mojo" "\
Edit the current file as sudo with tramp, without needing to
  navigate to it or losing your place in the file. Works on local
  or remote files.

\(fn)" t nil)

(autoload 'occur-dwim "mojo" "\
Call function `occur' with a sane default.

Function `occur-dwim' will offer as the default candidate:
- the current region, if it's active
- the current symbol, otherwise

\(fn)" t nil)

(autoload 'copy-line "mojo" "\
Copy current line in the kill ring.

\(fn &optional ARG)" t nil)

(autoload 'remove-dos-eol "mojo" "\
Do not show ^M in files containing mixed UNIX and DOS line
endings.

\(fn)" t nil)

(autoload 'back-to-indentation-or-beginning "mojo" "\
Returns the point to the beginning of the current line, or if
already there, the beginning of text on the current line.

\(fn)" t nil)

(autoload 'toggle-selective-display "mojo" "\
Enable code folding in current buffer.

\(fn COLUMN)" t nil)

(autoload 'open-line-below "mojo" "\
Create a new line above the current line. Can be used with point
       anywhere on the line.

\(fn)" t nil)

(autoload 'open-line-above "mojo" "\
Create a new line below the current line. Can be used with point
       anywhere on the line.

\(fn)" t nil)

(autoload 'goto-line-with-feedback "mojo" "\
Show line numbers temporarily, while prompting for the line number input.

\(fn)" t nil)

(autoload 'delete-whole-word "mojo" "\
This defun will delete the entire word at point. This function
    relies on `kill-whole-word'.

\(fn)" t nil)

(autoload 'kill-whole-word "mojo" "\
This defun will kill the entire word at point (on both sides
    of point). DELETE, if non-nil, will prevent the word from being
    appended to the kill-ring.

\(fn &optional DELETE)" t nil)

(autoload 'delete-word "mojo" "\
Delete characters forward until encountering the end of a word.
With (prefix) ARG, do this that many times.

\(fn ARG)" t nil)

(autoload 'backward-delete-word "mojo" "\
Delete characters backward until encountering the end of a word.
With (prefix) ARG, do this that many times.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("mojo-pkg.el") (21762 28289 538580 485000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; mojo-autoloads.el ends here

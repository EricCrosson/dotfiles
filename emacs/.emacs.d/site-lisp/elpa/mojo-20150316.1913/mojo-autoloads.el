;;; mojo-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "mojo" "mojo.el" (21767 29297 250620 143000))
;;; Generated autoloads from mojo.el

(autoload 'esc/auto-byte-recompile "mojo" "\
If the current buffer is in emacs-lisp-mode and there already exists an .elc
file corresponding to the current buffer file, then recompile the file.

\(fn)" t nil)

(autoload 'byte-compile-directory "mojo" "\
Compile an .elc file for every .el file contained under
  DIR (recursive).

\(fn DIR)" nil nil)

(autoload 'comint-clear-buffer "mojo" "\


\(fn)" t nil)

(autoload 'esc/dictionary-search "mojo" "\


\(fn)" t nil)

(autoload 'esc/insert-numeric-sequence "mojo" "\
Insert a sequence of numbers at point, separated by spaces. Inclusive.

\(fn)" t nil)

(autoload 'esc/insert-time "mojo" "\
Insert string for the current time formatted as '21:25'.

\(fn)" t nil)

(autoload 'esc/insert-date "mojo" "\
Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name.

\(fn PREFIX)" t nil)

(autoload 'esc/get-buffers-matching-mode "mojo" "\
Returns a list of buffers where their major-mode is equal to MODE.

\(fn MODE)" nil nil)

(autoload 'esc/unroll-cc-arguments "mojo" "\
Unroll a function's arguments into a more readable
  one-per-line format. Be sure to invoke this defun from before the
  opening paren of the function's arguments.

  This function has delimeters based on cc-mode dialects, and as a
  result would not be very useful for a language like Lisp.

\(fn)" t nil)

(autoload 'esc/save-buffers-kill-emacs "mojo" "\
Offer to save each buffer(once only), then kill this Emacs process.
   With prefix ARG, silently save all file-visiting buffers, then kill.

\(fn &optional ARG)" t nil)

(autoload 'esc/save-window-configuration "mojo" "\
Save current window configuration to WIN-REGISTER, run BODY,
and restory WIN-REGISTER.

\(fn WIN-REGISTER &rest BODY)" nil t)

(put 'esc/save-window-configuration 'lisp-indent-function 'defun)

(autoload 'combinations "mojo" "\
Return a list of all possible combinations of the elements of
  LISTS. Think applicative functors from Haskell.

\(fn &rest LISTS)" nil nil)

(autoload 'sudo-edit-current-file "mojo" "\
Edit the current file as sudo with tramp, without needing to
  navigate to it or losing your place in the file. Works on local
  or remote files.

\(fn)" t nil)

(autoload 'remove-dos-eol "mojo" "\
Do not show ^M in files containing mixed UNIX and DOS line
endings.

\(fn)" t nil)

(autoload 'toggle-selective-display "mojo" "\
Enable code folding in current buffer.

\(fn COLUMN)" t nil)

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

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; mojo-autoloads.el ends here

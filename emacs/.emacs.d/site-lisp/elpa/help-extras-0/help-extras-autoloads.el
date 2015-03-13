;;; help-extras-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "help-extras" "help-extras.el" (21762 26264
;;;;;;  256236 931000))
;;; Generated autoloads from help-extras.el

(autoload 'insert-variable "help-extras" "\
Insert VARIABLE at point.

The library where VARIABLE is defined is searched for in
`find-function-source-path', if non-nil, otherwise in `load-path'.
See also `find-function-recenter-line' and `find-function-after-hook'.

\(fn VARIABLE)" t nil)

(autoload 'insert-function-on-key "help-extras" "\
Insert at point the name of the function KEY invokes.  KEY is a string.
If INSERT (the prefix arg) is non-nil, insert the message in the
buffer.  If non-nil, UNTRANSLATED is a vector of the untranslated
events.  It can also be a number in which case the untranslated
events from the last key hit are used.

If KEY is a menu item or a tool-bar button that is disabled, this
command temporarily enables it to allow getting help on disabled
items and buttons.

\(fn &optional KEY INSERT UNTRANSLATED STRING)" t nil)

(autoload 'insert-function-name "help-extras" "\
Insert the name of the FUNCTION at point.

The library where FUNCTION is defined is searched for in
`find-function-source-path', if non-nil, otherwise in `load-path'.
See also `find-function-recenter-line' and `find-function-after-hook'.

\(fn FUNCTION)" t nil)

(autoload 'insert-key-combination "help-extras" "\
Insert string describing KEY sequence. KEY is a string.
If ARG is non nil, wrap the inserted string in some useful text
depending on the value of ARG.

Value of ARG      Example inserted string
positive          \"C-h c\"
negative          (kbd \"C-h c\")

\(fn KEY &optional ARG)" t nil)

(autoload 'describe-keymap "help-extras" "\
Describe bindings in KEYMAP, a variable whose value is a keymap.
Completion is available for the keymap name.

\(fn KEYMAP)" t nil)

(defvar help-extras nil "\
Non-nil if Help-Extras mode is enabled.
See the command `help-extras' for a description of this minor mode.")

(custom-autoload 'help-extras "help-extras" nil)

(autoload 'help-extras "help-extras" "\
A minor mode to extend the default GNU Emacs help system to
include source lookup and insertion of symbol-names at point.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("help-extras-pkg.el") (21762 26264 266980
;;;;;;  483000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; help-extras-autoloads.el ends here

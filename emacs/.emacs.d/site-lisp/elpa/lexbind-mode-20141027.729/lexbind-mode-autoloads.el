;;; lexbind-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "lexbind-mode" "lexbind-mode.el" (21738 3276
;;;;;;  492757 578000))
;;; Generated autoloads from lexbind-mode.el

(autoload 'lexbind-toggle-lexical-binding "lexbind-mode" "\
Toggle the variable `lexical-binding' on and off.  Interactive.
When called with a numeric argument, set `lexical-binding' to t
if the argument is positive, nil otherwise.
Optional argument ARG if nil toggles `lexical-binding', positive
enables it, non-positive disables it.

\(fn &optional ARG)" t nil)

(autoload 'lexbind-lexscratch "lexbind-mode" "\
Make a lexical scratch buffer.

\(fn &optional OTHER-WINDOW)" t nil)

(autoload 'lexbind-insert-lexical-binding-t "lexbind-mode" "\
Attempt to set `lexical-binding' to t in the current buffer's
prop-line.

\(fn &optional VERBOSE)" t nil)

(autoload 'lexbind-modeline-content "lexbind-mode" "\
Generate mode line content to indicate the value of `lexical-binding'.
Optional argument ARGS if provided, the first argument is taken as the value
of `lexical-binding'.

\(fn &rest ARGS)" nil nil)

(autoload 'lexbind-mode "lexbind-mode" "\
Toggle Lexbind mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When lexbind mode is enabled, the mode line of a window will
contain the string (LEX) for lexical binding, (DYN) for dynamic
binding, to indicate the state of the lexical-binding variable in
that buffer.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; lexbind-mode-autoloads.el ends here

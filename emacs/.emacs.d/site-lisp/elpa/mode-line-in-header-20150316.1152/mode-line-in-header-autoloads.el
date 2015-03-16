;;; mode-line-in-header-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "mode-line-in-header" "mode-line-in-header.el"
;;;;;;  (21767 27504 395626 310000))
;;; Generated autoloads from mode-line-in-header.el

(autoload 'mode-line-in-header "mode-line-in-header" "\
Minor mode to display the mode line in the header of the current buffer.

\(fn &optional ARG)" t nil)

(defvar global-mode-line-in-header nil "\
Non-nil if Global-Mode-Line-In-Header mode is enabled.
See the command `global-mode-line-in-header' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-mode-line-in-header'.")

(custom-autoload 'global-mode-line-in-header "mode-line-in-header" nil)

(autoload 'global-mode-line-in-header "mode-line-in-header" "\
Toggle Mode-Line-In-Header mode in all buffers.
With prefix ARG, enable Global-Mode-Line-In-Header mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Mode-Line-In-Header mode is enabled in all buffers where
`(lambda nil (mode-line-in-header 1))' would do it.
See `mode-line-in-header' for more information on Mode-Line-In-Header mode.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; mode-line-in-header-autoloads.el ends here

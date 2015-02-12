;;; eldoc-eval-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (eldoc-eval-expression eldoc-in-minibuffer-mode)
;;;;;;  "eldoc-eval" "eldoc-eval.el" (21575 7607 693009 521000))
;;; Generated autoloads from eldoc-eval.el

(defvar eldoc-in-minibuffer-mode nil "\
Non-nil if Eldoc-In-Minibuffer mode is enabled.
See the command `eldoc-in-minibuffer-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `eldoc-in-minibuffer-mode'.")

(custom-autoload 'eldoc-in-minibuffer-mode "eldoc-eval" nil)

(autoload 'eldoc-in-minibuffer-mode "eldoc-eval" "\
Show eldoc for current minibuffer input.

\(fn &optional ARG)" t nil)

(autoload 'eldoc-eval-expression "eldoc-eval" "\
Eval expression with eldoc support in mode-line.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("eldoc-eval-pkg.el") (21575 7607 716841
;;;;;;  25000))

;;;***

(provide 'eldoc-eval-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; eldoc-eval-autoloads.el ends here

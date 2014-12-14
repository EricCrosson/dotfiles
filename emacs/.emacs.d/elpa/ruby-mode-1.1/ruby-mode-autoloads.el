;;; ruby-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (ruby-mode) "../../../.config/.emacs.d/elpa/ruby-mode-1.1/ruby-mode"
;;;;;;  "ruby-mode.el" "533df5088e694c48304847a92be54bf6")
;;; Generated autoloads from ruby-mode.el

(autoload 'ruby-mode "../../../.config/.emacs.d/elpa/ruby-mode-1.1/ruby-mode" "\
Major mode for editing Ruby scripts.
\\[ruby-indent-line] properly indents subexpressions of multi-line
class, module, def, if, while, for, do, and case statements, taking
nesting into account.

The variable ruby-indent-level controls the amount of indentation.
\\{ruby-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))

(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(add-to-list 'interpreter-mode-alist '("rbx" . ruby-mode))

(add-to-list 'interpreter-mode-alist '("jruby" . ruby-mode))

(add-to-list 'interpreter-mode-alist '("ruby1.9" . ruby-mode))

(add-to-list 'interpreter-mode-alist '("ruby1.8" . ruby-mode))

;;;***

;;;### (autoloads (ruby-mode) "ruby-mode" "../../../../.emacs.d/elpa/ruby-mode-1.1/ruby-mode.el"
;;;;;;  "533df5088e694c48304847a92be54bf6")
;;; Generated autoloads from ../../../../.emacs.d/elpa/ruby-mode-1.1/ruby-mode.el

(autoload 'ruby-mode "ruby-mode" "\
Major mode for editing Ruby scripts.
\\[ruby-indent-line] properly indents subexpressions of multi-line
class, module, def, if, while, for, do, and case statements, taking
nesting into account.

The variable ruby-indent-level controls the amount of indentation.
\\{ruby-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))

(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(add-to-list 'interpreter-mode-alist '("rbx" . ruby-mode))

(add-to-list 'interpreter-mode-alist '("jruby" . ruby-mode))

(add-to-list 'interpreter-mode-alist '("ruby1.9" . ruby-mode))

(add-to-list 'interpreter-mode-alist '("ruby1.8" . ruby-mode))

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/ruby-mode-1.1/ruby-mode-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/ruby-mode-1.1/ruby-mode-pkg.el"
;;;;;;  "../../../../.emacs.d/elpa/ruby-mode-1.1/ruby-mode.el" "ruby-mode.el")
;;;;;;  (21522 12749 705429 215000))

;;;***

(provide 'ruby-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ruby-mode-autoloads.el ends here

;;; brainfuck-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (brainfuck-mode) "brainfuck-mode" "brainfuck-mode.el"
;;;;;;  (21436 5873 744552 457000))
;;; Generated autoloads from brainfuck-mode.el

(autoload 'brainfuck-mode "brainfuck-mode" "\
Major mode for brainfuck

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.bf\\'" . brainfuck-mode))

(langdoc-define-help-mode bf-help "Major mode for brainfuck help" "*Brainfuck Help*" 'bf-help-sym-called-at-point '(">" "<" "+" "-" "." "," "[" "]") 'bf-help-lookup-doc "`\\([^']+\\)'" (lambda (a b) b) (lambda (a b) b) "`" "'")

;;;***

;;;### (autoloads nil nil ("brainfuck-mode-pkg.el") (21436 5873 794946
;;;;;;  178000))

;;;***

(provide 'brainfuck-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; brainfuck-mode-autoloads.el ends here

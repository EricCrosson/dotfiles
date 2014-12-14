;;; expand-region-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (er/expand-region) "expand-region" "expand-region.el"
;;;;;;  (21393 45754 131887 462000))
;;; Generated autoloads from expand-region.el

(autoload 'er/expand-region "expand-region" "\
Increase selected region by semantic units.

With prefix argument expands the region that many times.
If prefix argument is negative calls `er/contract-region'.
If prefix argument is 0 it resets point and mark to their state
before calling `er/expand-region' for the first time.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (expand-region-exclude-text-mode-expansions expand-region-reset-fast-key
;;;;;;  expand-region-contract-fast-key expand-region-fast-keys-enabled
;;;;;;  expand-region-skip-whitespace expand-region-autocopy-register
;;;;;;  expand-region-guess-python-mode expand-region-preferred-python-mode
;;;;;;  expand-region) "expand-region-custom" "expand-region-custom.el"
;;;;;;  (21393 45754 58554 130000))
;;; Generated autoloads from expand-region-custom.el

(let ((loads (get 'expand-region 'custom-loads))) (if (member '"expand-region-custom" loads) nil (put 'expand-region 'custom-loads (cons '"expand-region-custom" loads))))

(defvar expand-region-preferred-python-mode 'python "\
The name of your preferred python mode")

(custom-autoload 'expand-region-preferred-python-mode "expand-region-custom" t)

(defvar expand-region-guess-python-mode t "\
If expand-region should attempt to guess your preferred python mode")

(custom-autoload 'expand-region-guess-python-mode "expand-region-custom" t)

(defvar expand-region-autocopy-register "" "\
If set to a string of a single character (try \"e\"), then the
contents of the most recent expand or contract command will
always be copied to the register named after that character.")

(custom-autoload 'expand-region-autocopy-register "expand-region-custom" t)

(defvar expand-region-skip-whitespace t "\
If expand-region should skip past whitespace on initial expansion")

(custom-autoload 'expand-region-skip-whitespace "expand-region-custom" t)

(defvar expand-region-fast-keys-enabled t "\
If expand-region should bind fast keys after initial expand/contract")

(custom-autoload 'expand-region-fast-keys-enabled "expand-region-custom" t)

(defvar expand-region-contract-fast-key "-" "\
Key to use after an initial expand/contract to contract once more.")

(custom-autoload 'expand-region-contract-fast-key "expand-region-custom" t)

(defvar expand-region-reset-fast-key "0" "\
Key to use after an initial expand/contract to undo.")

(custom-autoload 'expand-region-reset-fast-key "expand-region-custom" t)

(defvar expand-region-exclude-text-mode-expansions '(html-mode nxml-mode) "\
List of modes which derive from `text-mode' for which text mode expansions are not appropriate.")

(custom-autoload 'expand-region-exclude-text-mode-expansions "expand-region-custom" t)

;;;***

;;;### (autoloads nil nil ("cc-mode-expansions.el" "clojure-mode-expansions.el"
;;;;;;  "cperl-mode-expansions.el" "css-mode-expansions.el" "enh-ruby-mode-expansions.el"
;;;;;;  "er-basic-expansions.el" "erlang-mode-expansions.el" "expand-region-core.el"
;;;;;;  "expand-region-pkg.el" "feature-mode-expansions.el" ...)
;;;;;;  (21393 45754 634648 164000))

;;;***

(provide 'expand-region-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; expand-region-autoloads.el ends here

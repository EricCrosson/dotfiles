;;; goto-last-change-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (goto-last-change) "goto-last-change" "goto-last-change.el"
;;;;;;  (21552 36841 513721 679000))
;;; Generated autoloads from goto-last-change.el

(autoload 'goto-last-change "goto-last-change" "\
Set point to the position of the last change.
Consecutive calls set point to the position of the previous change.
With a prefix arg (optional arg MARK-POINT non-nil), set mark so \\[exchange-point-and-mark]
will return point to the current position.

\(fn &optional MARK-POINT MINIMAL-LINE-DISTANCE)" t nil)

;;;***

;;;### (autoloads nil nil ("goto-last-change-pkg.el") (21552 36841
;;;;;;  537033 50000))

;;;***

(provide 'goto-last-change-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; goto-last-change-autoloads.el ends here

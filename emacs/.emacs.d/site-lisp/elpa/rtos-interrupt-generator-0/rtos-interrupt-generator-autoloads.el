;;; rtos-interrupt-generator-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "rtos-interrupt-generator" "rtos-interrupt-generator.el"
;;;;;;  (21748 46167 864868 364000))
;;; Generated autoloads from rtos-interrupt-generator.el

(autoload 'rtos/combinations "rtos-interrupt-generator" "\
Return a list of all possible combinations of the elements of LISTS.

\(fn &rest LISTS)" nil nil)

(autoload 'rtos/generate-interrupts "rtos-interrupt-generator" "\
Insert at point interrupt handlers for every permutation of
device and channel number in list `rtos/interrupt-devices' and
list `rtos/interrupt-channels', respectively.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("rtos-interrupt-generator-pkg.el") (21748
;;;;;;  46167 895135 61000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; rtos-interrupt-generator-autoloads.el ends here

;;; rtos-interrupt-generator-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "rtos-interrupt-generator" "rtos-interrupt-generator.el"
;;;;;;  (21748 43114 556682 655000))
;;; Generated autoloads from rtos-interrupt-generator.el

(autoload 'rtos/generate-interrupts "rtos-interrupt-generator" "\
Insert at point interrupt handlers for every permutation of
device and channel number in `rtos/interrupt-devices' and
`rtos/interrupt-channels', respectively.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; rtos-interrupt-generator-autoloads.el ends here

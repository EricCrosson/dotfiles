;;; unkillable-scratch-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "unkillable-scratch" "unkillable-scratch.el"
;;;;;;  (21762 26489 310967 965000))
;;; Generated autoloads from unkillable-scratch.el

(autoload 'unkillable-scratch-buffer "unkillable-scratch" "\
A hook designed to be added to hook
`kill-buffer-query-functions' to prevent the *scratch* buffer
from ever being killed. Instead of a successful kill, the
*scratch* buffer will be regenerated.

\(fn)" nil nil)

(defvar unkillable-scratch nil "\
Non-nil if Unkillable-Scratch mode is enabled.
See the command `unkillable-scratch' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `unkillable-scratch'.")

(custom-autoload 'unkillable-scratch "unkillable-scratch" nil)

(autoload 'unkillable-scratch "unkillable-scratch" "\
A minor mode to disallow the *scratch* buffer from being killed.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; unkillable-scratch-autoloads.el ends here

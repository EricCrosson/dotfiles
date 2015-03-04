;;; ido-extras-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "ido-extras" "ido-extras.el" (21750 18677 440154
;;;;;;  323000))
;;; Generated autoloads from ido-extras.el

(autoload 'ido-recentf-open "ido-extras" "\
Use `ido-completing-read' to \\[find-file] a recent file.

\(fn)" t nil)

(autoload 'ido-goto-symbol "ido-extras" "\
Use `ido-completing-read` to query a function in the current
buffer and jump to it. Functions are defined by the active minor
mode.

\(fn &optional SYMBOL-LIST)" t nil)

(autoload 'ido-switch-buffer-current-major-mode "ido-extras" "\
Invoke function `ido-switch-buffer' listing only buffers of
the same major mode as the current buffer.

from http://emacswiki.org/emacs/InteractivelyDoThings#toc6

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("ido-extras-pkg.el") (21750 18677 452438
;;;;;;  62000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ido-extras-autoloads.el ends here

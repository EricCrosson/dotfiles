;;; os-interaction-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "os-interaction" "os-interaction.el" (21762
;;;;;;  28779 60504 750000))
;;; Generated autoloads from os-interaction.el

(autoload 'middle-click-yank "os-interaction" "\
Yank from the middle click mouse buffer.

\(fn)" t nil)

(autoload 'insert-file-name "os-interaction" "\
Inserts the name of the current file (including extension) at point.

When ARG is non-nil, the filename will be printed in a different format.
If ARG is 0, insert the full path of the filename.
If ARG is - (or -1), insert the filename without extension.

\(fn &optional ARG)" t nil)

(autoload 'copy-file-name-to-clipboard "os-interaction" "\
Copy the current buffer file name to the clipboard.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("os-interaction-pkg.el") (21762 28779
;;;;;;  73788 101000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; os-interaction-autoloads.el ends here

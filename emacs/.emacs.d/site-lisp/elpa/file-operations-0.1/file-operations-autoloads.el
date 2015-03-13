;;; file-operations-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "file-operations" "file-operations.el" (21762
;;;;;;  27655 110968 853000))
;;; Generated autoloads from file-operations.el

(autoload 'minibuffer-display-buffer-file-name "file-operations" "\
Display `buffer-file-name' in the minibuffer.

\(fn)" t nil)

(autoload 'file-delete "file-operations" "\
Removes file connected to current buffer and kills buffer.

\(fn)" t nil)

(autoload 'file-rename "file-operations" "\
Renames current buffer and file it is visiting.

\(fn)" t nil)

(autoload 'touch "file-operations" "\
Updates mtime on the file described by the current buffer.

\(fn)" t nil)

(autoload 'indent-buffer "file-operations" "\
Indent the entire buffer without adjusting point or mark.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; file-operations-autoloads.el ends here

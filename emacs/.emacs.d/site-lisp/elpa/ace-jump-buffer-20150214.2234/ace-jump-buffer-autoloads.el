;;; ace-jump-buffer-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "ace-jump-buffer" "ace-jump-buffer.el" (21738
;;;;;;  3498 509435 676000))
;;; Generated autoloads from ace-jump-buffer.el

(autoload 'ace-jump-buffer "ace-jump-buffer" "\
Quickly hop to buffer with `ace-jump-mode'.

\(fn)" t nil)

(autoload 'ace-jump-buffer-other-window "ace-jump-buffer" "\
Quickly hop to buffer with `ace-jump-mode' in other window.

\(fn)" t nil)

(autoload 'ace-jump-buffer-in-one-window "ace-jump-buffer" "\
Quickly hop to buffer with `ace-jump-mode' in one window.

\(fn)" t nil)

(autoload 'ace-jump-buffer-with-configuration "ace-jump-buffer" "\
Quickly hop to buffer with `ace-jump-mode' with selected configuration.

\(fn)" t nil)

(autoload 'make-ace-jump-buffer-function "ace-jump-buffer" "\
Create a `bs-configuration' and interactive defun using NAME that displays buffers
that don't get rejected by the body of BUFFER-LIST-REJECT-FILTER.

\(fn NAME &rest BUFFER-LIST-REJECT-FILTER)" nil t)

(put 'make-ace-jump-buffer-function 'lisp-indent-function '1)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ace-jump-buffer-autoloads.el ends here

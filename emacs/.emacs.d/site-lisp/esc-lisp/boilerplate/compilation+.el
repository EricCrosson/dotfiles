;;; compilation+.el --- Compilation-related functions

;;; Commentary:
;;

;;; Code:

;;;###autoload
  (defun esc/previous-error (n)
    "Visit previous compilation error message and corresponding source code.
  Complement to `next-error'."
    (interactive "p")
    (next-error (- n)))

;;;###autoload
(defun esc/auto-byte-recompile ()
  "If the current buffer is in emacs-lisp-mode and there already exists an .elc
file corresponding to the current buffer file, then recompile the file."
  (interactive)
  (when (require 'bytecomp nil 'noerror)
    (when (and (eq major-mode 'emacs-lisp-mode)
	       (file-exists-p (byte-compile-dest-file buffer-file-name)))
      (byte-compile-file buffer-file-name))))

;;;###autoload
(defun esc/bury-compilation-buffer-if-successful (buffer string)
  "Bury the compilation BUFFER after a successful compile.
Argument STRING provided by compilation hooks."
  (when (and
	 (string-match "compilation" (buffer-name buffer))
	 (string-match "finished" string)
	 (not (search-forward "warning" nil t)))
    (bury-buffer buffer)
    (switch-to-prev-buffer (get-buffer-window buffer) 'kill)
    ;; TODO: winner-undo if compile created a new buffer
    (message "Compilation successful.")))

(provide 'compilation+)

;;; compilation+.el ends here

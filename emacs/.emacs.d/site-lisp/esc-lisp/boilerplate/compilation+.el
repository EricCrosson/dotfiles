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
  (if (not (and
	    (string-match "compilation" (buffer-name buffer))
	    (string-match "finished" string)
	    (not (search-forward "warning" nil t))))
      (when (boundp esc-precompile-window-norestore)
	(setq esc-precompile-window-norestore t))
    (when (boundp esc-precompile-window-state)
      (jump-to-register esc-precompile-window-state))
    (message "Compilation successful.")))

;;;###autoload
(defun esc/org-next-source-code-block ()
  (interactive)
  (re-search-forward "#\\+begin_src" nil t)
  (next-line)
  (beginning-of-line))

;;;###autoload
(defun esc/org-prev-source-code-block ()
  (interactive)
  (previous-line)
  (beginning-of-line)
  (re-search-backward "#\\+begin_src" nil t))

;;;###autoload
(defun byte-compile-directory(dir)
  "Compile an .elc file for every .el file contained under
  DIR (recursive)."
  (byte-recompile-directory (expand-file-name dir) 0))

(provide 'compilation+)

;;; compilation+.el ends here

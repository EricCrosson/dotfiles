;;; os-interaction.el --- Emacs should play nicely with whatever OS it's hiding

;;; Commentary:
;;

;;; Code:

;;;###autoload
(defun esc/middle-click-yank ()
  "Yank from the middle click mouse buffer."
  (interactive)
  (mouse-yank-primary 1))

;;;###autoload
(defun esc/insert-file-name (&optional ARG)
  "Inserts the name of the current file (including extension) at point.

When ARG is non-nil, the filename will be printed in a different format.
If ARG is 0, insert the full path of the filename.
If ARG is - (or -1), insert the filename without extension."
  (interactive "p")
  (let ((filename (if (equal major-mode 'dired-mode)
		      default-directory
		    (buffer-file-name))))
    (when filename
      (let ((output (cond ((eq ARG 0)   filename)
			  ((eq ARG -1)  (file-name-nondirectory
					 (file-name-sans-extension filename)))
			  (t (file-name-nondirectory filename)))))
	(insert output)))))

;;;###autoload
(defun esc/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
		      default-directory
		    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(provide 'os-interaction)

;;; os-interaction.el ends here

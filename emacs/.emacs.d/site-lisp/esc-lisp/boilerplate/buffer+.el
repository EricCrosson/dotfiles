;;; buffer+.el --- Buffer and window management functions

;;; Commentary:
;;

;;; Code:

;;;###autoload
(defmacro esc/toggle-fullscreen-buffer (win-register toggled-mode-test toggle-command
						     &optional
						     toggle-command-test
						     clear-command)
  "Bring up a temporary buffer in fullscreen mode, or restore the
previous window configuration.

WIN-REGISTER         is the register to store the old window configuration in.

TOGGLED-MODE-TEST    is the major mode of the toggled state, in other words a
                     test to determine which way to toggle the buffers.

TOGGLE-COMMAND       is the command to run when toggling into the temporary
                     state.

CLEAR-COMMAND        is an optional command to run when reverting back to the
                     original state; i.e. toggle a flag"
  (declare (indent defun))
  `(progn
     (if ,toggled-mode-test
	 (progn (jump-to-register ,win-register)
		(when (not (equal nil ,clear-command))
		  ,clear-command))
       (window-configuration-to-register ,win-register)
       ,toggle-command
       (delete-other-windows))))

;;;###autoload
(defmacro esc/save-window-configuration (win-register &rest body)
  "Save current window configuration to WIN-REGISTER, run BODY,
and restory WIN-REGISTER."
  (declare (indent defun))
  `(progn
     (window-configuration-to-register ,win-register)
     (progn ,@body)
     (jump-to-register ,win-register)))

;;;###autoload
(defun esc/should-have-opened-this-in-other-window (&optional COUNT)
  "Returns to the previous buffer in current window, calls
`other-window', and opens the buffer in the new window.

COUNT is the number of windows to advance; the argument is passed
directly to `other-window', so see the documentation for more
details."
  (interactive)
  (when (equal nil COUNT)
    (setq COUNT 1))
  (let ((target (buffer-name)))
    (switch-to-prev-buffer)
    (other-window COUNT)
    (switch-to-buffer target)))

;;;###autoload
(defun esc/toggle-window-selectability ()
  "Ignore/recognize a window from `other-window'."
  (interactive)
  (if (window-parameter (selected-window) 'no-other-window)
      (progn
	(set-window-parameter (selected-window) 'no-other-window nil)
	(message "Window will now be recognized by `other-window'"))
    (set-window-parameter (selected-window) 'no-other-window t)
    (message "Window will now be ignored by `other-window'")))

;;;###autoload
(defun esc/toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not.

When dedicated, `display-buffer' will refrain from displaying
another buffer in a window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
	 (set-window-dedicated-p window
				 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

;;;###autoload
(defun buffers-matching-regexp (regexp &optional names)
  "Return a list of buffers matching REGEXP.

If NAMES is non-nil, return a list of names (strings) instead of
buffers."
  (remq nil
	(mapcar (lambda (buf)
		  (let ((name (buffer-name buf)))
		    (when (string-match regexp name)
		      (if names
			  (buffer-name buf)
			buf))))
		(buffer-list))))

;;;###autoload
(defun esc/rotate-window-split ()
  "Transform a vertically split window to a horizontally split
window."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

;;;###autoload
(defun esc/bury-buffer-delete-window ()
  "Kill current window and bury the current buffer to the bottom
of the buffer list."
  (interactive)
  (bury-buffer)
  (delete-window))

;;;###autoload
(defun esc/bury-other-buffer (&optional switch-to-other-buffer)
  "Bury the buffer that `mode-line-other-buffer' will take you
to.

If SWITCH-TO-OTHER-BUFFER is non-nil (e.g. this command is
prefixed) then after the other-buffer is buried, the command
`mode-line-other-buffer' will be used to switch buffers."
  (interactive "p")
  (mode-line-other-buffer)
  (bury-buffer)
  (when current-prefix-arg (mode-line-other-buffer)))

;;;###autoload
(defun esc/raise-eshell ()
  "Bring up a full-screen eshell or restore previous window
configuration."
  (interactive)
  (esc/toggle-fullscreen-buffer :eshell-fullscreen
    (string= "eshell-mode" major-mode)
    (eshell)))

;;;###autoload
(defun esc/raise-eshell-in-current-dir ()
  "Bring up a full-screen eshell in the current directory or
restore previous window configuration."
  (interactive)
  (esc/toggle-fullscreen-buffer
    :eshell-fullscreen
    (string= "eshell-mode" major-mode)
    (when (buffer-file-name)
      (let ((dir (file-name-directory (buffer-file-name))))
	(eshell)
	(insert "cd " dir)
	(with-no-warnings (eshell-send-input))))))

;;;###autoload
(defun esc/raise-magit-status ()
  "Bring up a full-screen magit-status or restore previous
window configuration."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (when (re-search-forward "^<<<<<<< variant" nil 'noerror)
      (error "Resolve merge conflicts first.")))
  (esc/toggle-fullscreen-buffer
    :magit-fullscreen
    (string= "magit-status-mode" major-mode)
    (progn (if (not (buffer-file-name))
	       (message "Buffer not associated with a file")
	     (save-buffer)
	     (magit-status (file-name-directory (buffer-file-name)))))))

;; Thanks sacha! See
;; [[http://pages.sachachua.com/.emacs.d/Sacha.html#sec-1-5-1][Sacha
;; Chua's Emacs configuration]] for more tips.
;;;###autoload
(defun esc/vsplit-last-buffer (prefix)
  "Split the window vertically and display the previous buffer.
Argument PREFIX when nil switches the new buffer to the last buffer."
  (interactive "p")
  (split-window-vertically)
  (other-window 1 nil)
  (unless prefix (switch-to-next-buffer)))

;;;###autoload
(defun esc/hsplit-last-buffer (prefix)
  "Split the window horizontally and display the previous buffer.
Argument PREFIX when nil switches the new buffer to the last buffer."
  (interactive "p")
  (split-window-horizontally)
  (other-window 1 nil)
  (unless prefix (switch-to-next-buffer)))

(provide 'buffer+)

;;; buffer+.el ends here

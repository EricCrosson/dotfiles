
;;;###autoload
(defun color-theme-push ()
  "Switch to a theme, saving the old one."
  (push (color-theme-make-snapshot) color-theme-stack)
  (message "Color theme pushed"))

;;;###autoload
(defun color-theme-pop ()
  "Restore the previous theme in use."
  (funcall (pop color-theme-stack))
  (message "Color theme popped"))


(require 'cl)
(require 'color-theme)

(defvar color-theme-stack nil
  "Stack of color themes.")

(defun color-theme-push ()
  "Switch to a theme, saving the old one."
  (push (color-theme-make-snapshot) color-theme-stack))

(defun color-theme-pop ()
  "Restore the previous theme in use."
  (funcall (pop color-theme-stack)))

;;; color-theme-stack.el --- Provide a stack for color themes
;; Version: 0.0.2014030101

;; Copyright (C) 2015 Eric Crosson

;; Author: Eric Crosson <esc@ericcrosson.com>
;; Keywords: compilation
;; Package-Version: 0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a minor mode that will do two things
;; after a successful recompile:
;; 1) bury the *compilation* buffer, and
;; 2) restore your window configuration to how it looked when you
;; issued the recompile.

;; Usage:

;; (color-theme-push)
;; (color-theme-pop)
;; (color-theme-load-and-push 'wombat)

;;; Code:

(defcustom color-theme-stack nil
  "Stack of color themes for package 'color-theme-stack."
  :group 'color-theme)

;;;###autoload
(defun color-theme-load-and-push (theme)
  "Switch to THEME after pushing the current theme on variable
`color-theme-stack'."
  (interactive)
  (color-theme-push)
  (load-theme theme))

;;;###autoload
(defun color-theme-push ()
  (interactive)
  "Save the current color theme on the stack."
  (push (color-theme-make-snapshot) color-theme-stack)
  (message "Color theme pushed"))

;;;###autoload
(defun color-theme-pop ()
  (interactive)
  "Restore the previous theme in use."
  (funcall (pop color-theme-stack))
  (message "Color theme popped"))

(provide 'color-theme-stack)

;;; color-theme-stack.el ends here

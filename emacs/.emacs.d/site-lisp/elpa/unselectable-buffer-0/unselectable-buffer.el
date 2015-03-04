;;; unselectable-buffer.el --- Prevent a window from being selected by `other-buffer'
;; Version 0.0.20140228

;; Copyright (C) 2015     Eric Steven Crosson (esc)

;; Author: Eric Crosson <esc@ericcrosson.com>
;; Keywords: buffer,window
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

;;; Usage:

;; (global-set-key (kbd "C-c C-t") 'unselectable-buffer)
;;   - or -
;; M-x unselectable-buffer
;;   to toggle a buffer's selectable status inside a window

;;; Code:

(defun unselectable-buffer-on ()
  "Turn on function `unselectable-buffer'"
  (set-window-parameter (selected-window) 'no-other-window t)
  (message "Window will now be ignored by `other-window'"))

(defun unselectable-buffer-off ()
  "Turn off function `unselectable-buffer'"
  (set-window-parameter (selected-window) 'no-other-window nil)
  (message "Window will now be recognized by `other-window'"))

;;;###autoload
(define-minor-mode unselectable-buffer
  "Toggle minor mode `unselectable-buffer'.

When enabled, `unselectable-buffer' prevents a buffer from being
chosen by `other-window'."
  :init-value nil
  :lighter " unselectable"
  :global nil
  :group 'unselectable-buffer
  (if unselectable-buffer
      (unselectable-buffer-on)
    (unselectable-buffer-on)))

(provide 'unselectable-buffer)

;;; unselectable-buffer.el ends here

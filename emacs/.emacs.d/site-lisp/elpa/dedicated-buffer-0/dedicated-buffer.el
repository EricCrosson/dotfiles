;;; dedicated-buffer.el --- Bind a buffer to a window
;; Version: 0.0.20140303

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

;; (global-set-key (kbd "C-c C-t") 'dedicated-buffer)
;;   - or -
;; M-x dedicated-buffer
;;   to toggle a buffer's dedicated status inside a window

;;; Code:

;;;###autoload
(define-minor-mode dedicated-buffer
  "Toggle `dedicated-buffer'.

Dedicated mode binds a window to a buffer; no other buffer Will
be displayed until the window is undedicated."
  :init-value nil
  :lighter " dedicated"
  :global nil
  :group 'dedicated
  (set-window-dedicated-p (get-buffer-window (current-buffer)) dedicated-buffer))

(provide 'dedicated-buffer)

;;; dedicated-buffer.el ends here

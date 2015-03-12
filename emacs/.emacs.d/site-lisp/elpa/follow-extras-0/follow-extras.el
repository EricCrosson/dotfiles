;;; follow-extras.el --- Extras for function `follow-mode'.
;; Version: 0.0.20140311

;; Copyright (C) 2015 Eric Crosson

;; Author: Eric Crosson <esc@ericcrosson.com>
;; Keywords: follow
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

;; This package provides defuns that interact with function
;; `follow-mode'.

;; Usage:

;; Invoke functions as you need them

;;; Code:

;; TODO: find a way to keep these internal window changes from being
;; registered by winner-mode

;;;###autoload
(defun follow-mode-80-char-compliant ()
  "Open the current buffer in `follow-mode' in as many 80-char
windows as you can fit in the current frame. All other windows
will be closed."
  (interactive)
  (delete-other-windows)
  (follow-mode 1)
  (let ((width (window-total-width nil 'floor)))
    (while (> width 80)
      (split-window-horizontally)
      (balance-windows)
      (setq width (window-total-width nil 'floor))))
  (delete-window)
  (balance-windows)
  (recenter-top-bottom))

;;;###autoload
(defun hsplit-current-buffer-follow-mode ()
  "Split the current buffer horizontally and engage function
`follow-mode'."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (follow-mode 1))

(provide 'follow-extras)

;;; follow-extras.el ends here

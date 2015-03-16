;;; os-interaction.el --- Emacs should play nicely with whatever OS it's hiding
;; Version: 0.0.20140311

;; Copyright (C) 2015 Eric Crosson

;; Author: Eric Crosson <esc@ericcrosson.com>
;; Keywords: os
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
;;

;;; Usage:

;; This file is intended for use by esc. If you wish to see how this
;; package is configured, a process that is likely in flux, look how
;; it is activated in esc's init.org

;;; Code:

;;;###autoload
(defun insert-file-name (&optional ARG)
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
(defun copy-file-name-to-clipboard ()
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

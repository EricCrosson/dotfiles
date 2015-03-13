;;; file-operations.el --- Operate on files and their respective buffers
;; Version: 0.0.20140312

;; Copyright (C) 2015 Eric Crosson

;; Author: Eric Crosson <esc@ericcrosson.com>
;; Keywords: file
;; Package-Version: 0.1

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

;; This package provides functions to operate on files and their
;; respective buffers.

;; Usage:

;; Call individual functions as you need them

;;; Code:

;; TOOD: accept a prefix to yank `buffer-file-name'
;;;###autoload
(defun minibuffer-display-buffer-file-name ()
  "Display `buffer-file-name' in the minibuffer."
  (interactive)
  (ignore-errors
    (message (format "%s" (buffer-file-name)))))

;;;###autoload
(defun file-delete ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
	(buffer (current-buffer))
	(name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
	(ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
	(delete-file filename)
	(kill-buffer buffer)
	(message "File '%s' successfully removed" filename)))))

;;;###autoload
(defun file-rename ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
	(error "Buffer '%s' is not visiting a file" name)
      (let ((new-name (read-file-name "New name: " filename)))
	(if (get-buffer new-name)
	    (error "A buffer named '%s' already exists" new-name)
	  (rename-file filename new-name 1)
	  (rename-buffer new-name)
	  (set-visited-file-name new-name)
	  (set-buffer-modified-p nil)
	  (message "File '%s' successfully renamed to '%s'"
		   name new-name))))))

  ;;;###autoload
(defun touch ()
  "Updates mtime on the file described by the current buffer."
  (interactive)
  (let ((file (buffer-file-name)))
    (shell-command (concat "touch " (shell-quote-argument file)))
    (clear-visited-file-modtime)
    (message (format "File %s touched" file))))

;;;###autoload
(defun indent-buffer ()
  "Indent the entire buffer without adjusting point or mark."
  (interactive)
  (save-excursion (indent-region (point-min) (point-max))))

(provide 'file-operations)

;;; file-operations.el ends here

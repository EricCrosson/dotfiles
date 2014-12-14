;; Single-mode.el --- A mode to browse files with less keystrokes
;; Copyright (C) <year>  <name of author>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.



;;; Commentary:
;; This program is really only intended for personal use- it is a
;; keymap that I find less painful than vi's, while allowing me to
;; navigate files fairly well. This is a continual work in progress.


;;; Code:

;;;###autoload
(defmacro single-mode-enabled ()
  "A macro to determine if \\[single-mode] is currently enabled."
  `(progn (and (boundp 'single-mode) single-mode)))

;;;###autoload
(defmacro single-wrap-around-read-only-mode (&rest block)
  "Wrap a BLOCK around read-only mode."
  `(progn (let ((friendly buffer-read-only))
       (read-only-mode -1)
       ,block
       (when friendly
	 (read-only-mode 1)))))

;;;###autoload
(defun single/quit-single-mode ()
  "Exit \\[single-mode]."
    (interactive)
    (single-mode -1))

;;;###autoload
(defun single/scroll-up (&optional arg)
  "Scroll up the page without moving point.

The number of lines to scroll is determined by the variable
`single-line-shift-amount'.

ARG determines the number of times to scroll.

This command can be prefixed."
  (interactive "p")
  (dotimes (i arg)
    (scroll-up single-line-shift-amount)))

;;;###autoload
(defun single/scroll-down (&optional arg)
  "Scroll down the page without moving point.

The number of lines to scroll is determined by the variable
`single-line-shift-amount'.

ARG determines the number of times to scroll.

This command can be prefixed."
  (interactive "p")
  (dotimes (i arg)
    (scroll-down single-line-shift-amount)))

;; TODO: implement revert-buffer-in-single-mode

;;;###autoload
(defun single/ff-in-single-mode ()
  "Find a file and display the buffer in \\[single-mode]."
  (interactive)
  (ido-find-file)
  (single-mode 1))

;;;###autoload
(defun single/query-replace ()
  "Invoke `query-replace' on the entire buffer.

This means, starting from `beginning-of-buffer'."
  (interactive)
  (beginning-of-buffer)
  (query-replace from-string to-string))

;;;###autoload
(defun single/undo ()
  "Undo, circumnavigating the `read-only-mode'."
  (interactive)
  (single-wrap-around-read-only-mode (undo)))

;;;###autoload
(defun single/read-only-mode ()
  "Toggle read-only mode."
  (interactive)
  (if buffer-read-only
      (read-only-mode -1)
    (read-only-mode 1)))

;;;###autoload
(defun single/iedit-mode ()
  "Invokes `iedit-mode', circumnavigating `read-only-mode'."
  (interactive)
  (single-wrap-around-read-only-mode (call-interactively (iedit-mode))))

;;;###autoload
(defun single/kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

;;;###autoload
(defun single/single-mode-hook ()
  "Hook for function `single-mode'."
  (if (single-mode-enabled)
      (progn
	(when (not buffer-read-only)
	  (setq single-restore-nil-read-only-state t)
	  (read-only-mode 1))
	(message "Single mode enabled"))
    (when single-restore-nil-read-only-state
      (setq single-restore-nil-read-only-state nil)
      (read-only-mode -1))
    (message "Single mode disabled")))

(provide 'single-mode)

;;; single-mode.el ends here

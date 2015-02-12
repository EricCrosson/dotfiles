
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

(defvar single-mode-map (make-keymap)
  "The keymap for \\[single-mode].")

(define-minor-mode single-mode
  "Toggle single-mode.
  A minor mode for quick navigation- reinventing the vim wheel."
  nil " single" 'single-mode-map
  (suppress-keymap single-mode-map))
(add-hook 'single-mode-hook 'single/single-mode-hook)

(defvar single-line-shift-amount 6
  "The number of lines to shift in esc-mode-map.")

(defvar single-restore-nil-read-only-state nil
  "This indicates we need to restore a state of (read-only-mode -1).
  This variable is nil by default.")

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

(defun single/quit-single-mode ()
  "Exit \\[single-mode]."
    (interactive)
    (single-mode -1))

(defun single/scroll-up (&optional arg)
  "Scroll up the page without moving point.

The number of lines to scroll is determined by the variable
`single-line-shift-amount'.

ARG determines the number of times to scroll.

This command can be prefixed."
  (interactive "p")
  (dotimes (i arg)
    (scroll-up single-line-shift-amount)))

(defun single/scroll-down (&optional arg)
  "Scroll down the page without moving point.

The number of lines to scroll is determined by the variable
`single-line-shift-amount'.

ARG determines the number of times to scroll.

This command can be prefixed."
  (interactive "p")
  (dotimes (i arg)
    (scroll-down single-line-shift-amount)))

;;;###autoload
(defun single/ff-in-single-mode ()
  "Find a file and display the buffer in \\[single-mode]."
  (interactive)
  (ido-find-file)
  (single-mode 1))

(defun single/query-replace ()
  "Invoke `query-replace' on the entire buffer.

This means, starting from `beginning-of-buffer'."
  (interactive)
  (beginning-of-buffer)
  (query-replace from-string to-string))

(defun single/undo ()
  "Undo, circumnavigating the `read-only-mode'."
  (interactive)
  (single-wrap-around-read-only-mode (undo)))

(defun single/read-only-mode ()
  "Toggle read-only mode."
  (interactive)
  (if buffer-read-only
      (read-only-mode -1)
    (read-only-mode 1)))

(defun single/iedit-mode ()
  "Invokes `iedit-mode', circumnavigating `read-only-mode'."
  (interactive)
  (single-wrap-around-read-only-mode (call-interactively (iedit-mode))))

(defun single/kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

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

(define-key single-mode-map (kbd "'") 'single/quit-single-mode)
(define-key single-mode-map (kbd "<escape>") 'single/quit-single-mode)
(define-key single-mode-map (kbd "j") 'single/scroll-up)
(define-key single-mode-map (kbd "k") 'single/scroll-down)
(define-key single-mode-map (kbd ",") 'beginning-of-buffer)
(define-key single-mode-map (kbd ".") 'end-of-buffer)
(define-key single-mode-map (kbd "5") 'single/query-replace)
(define-key single-mode-map (kbd "Z") 'single/undo)
(define-key single-mode-map (kbd "q") 'single/read-only-mode)
(define-key single-mode-map (kbd "`") 'single/iedit-mode)
(define-key single-mode-map (kbd "K") 'single/kill-current-buffer)

(define-key single-mode-map (kbd "`") 'kill-current-buffer)
(define-key single-mode-map (kbd "SPC") 'ace-jump-mode)
(define-key single-mode-map (kbd "x") 'execute-extended-command)
(define-key single-mode-map (kbd "p") 'scroll-down)
(define-key single-mode-map (kbd "n") 'scroll-up)
(define-key single-mode-map (kbd "9") 'end-of-buffer)
(define-key single-mode-map (kbd "0") 'beginning-of-buffer)
(define-key single-mode-map (kbd "s") 'isearch-forward)
(define-key single-mode-map (kbd "r") 'isearch-backward)
(define-key single-mode-map (kbd "e") 'eval-region)

(provide 'single-mode)

;;; single-mode.el ends here

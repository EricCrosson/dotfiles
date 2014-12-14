;;  lisp-infection.el --- Commands to *enhance* S-exp editing
;; Copyright (C) 2013 Eric Crosson
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


;; Author: Eric Crosson
;; URL: https://github.com/EricCrosson/lisp-infection
;; Version: 0.0.10

;;; Commentary:
;;
;; General enhancements to the Lisp production process.
;;
;; Note: This package is a much newer and less feature-rich version of
;; ParEdit.  I suggest you don't get your hopes too high for this
;; package, which was really a learning experience for me.  For
;; functionality, see ParEdit or a related package.
;;
;; Update: After writing this janky (buggy) package, I was turned on
;; to paredit mode, smart parens.  Take your pick!

;;; Code:

;;;###autoload
(defun li/mark-current-defun ()
  "Mark the current defun."
  (interactive)
  (beginning-of-defun)
  (push-mark)
  (end-of-defun))

;;;###autoload
(defun li/mark-current-sexp ()
  "Mark the current sexp."
  (interactive)
  (backward-up-list)
  (push-mark)
  (forward-sexp))

;;;###autoload
(defun li/copy-sexp ()
  "Save a sexp to the kill ring without killing it."
  (interactive)
  (let ((begin (li/beginning-of-sexp))
	(end (li/end-of-sexp)))
  (kill-ring-save begin end)
  (message "Sexp copied. %s %s" begin end)))

;;;###autoload
(defun li/end-of-sexp ()
  "Return the end of the current sexp."
  (save-excursion
    (unless (looking-at ")")
      (backward-up-list)
      (forward-sexp))
    (point)))

;;;###autoload
(defun li/beginning-of-sexp ()
  "Return the beginning of the current sexp."
  (save-excursion
    (unless (looking-at "(")
      (backward-up-list))
    (point)))

;;;###autoload
(defun li/delete-sexp ()
  "Delete sexp without saving to the kill ring."
  (interactive)
  (save-excursion
    (let ((begin (progn (when (not (looking-at "("))
			  (backward-up-list))
			(point)))
	  (end (progn (forward-sexp) (point))))
      (delete-region begin end))))

;;;###autoload
(defun li/eval-current-sexp ()
  "Evaluate the current sexp."
  (interactive)
  (save-excursion
    (when (not (char-equal (char-before) ?\)))
      (when (not (looking-at "("))
	(backward-up-list))
      (forward-sexp))
    (call-interactively 'eval-last-sexp)))

;;;###autoload
(defun li/indent-entire-defun ()
  "Re-indent the entire defun for ease of editing."
  (interactive)
  (save-excursion
    (let ((begin (progn (beginning-of-defun) (point)))
	  (end (progn (end-of-defun) (point))))
      (indent-region begin end)
      (indent-according-to-mode)
      (message "Indented defun."))))

;;;###autoload
(defun li/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;;; lisp-infection.el ends here

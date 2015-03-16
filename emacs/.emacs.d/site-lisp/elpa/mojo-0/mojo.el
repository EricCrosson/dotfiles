;;; mojo.el --- Extend the global namespace of GNU Emacs
;; Version: 0.0.20140312

;; Copyright (C) 2015 Eric Crosson

;; Author: Eric Crosson <esc@ericcrosson.com>
;; Keywords: miscellaneous
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

;; This package provides functions to extend the global namespace of
;; GNU Emacs.

;; Usage:

;; Call individual functions as you need them

;;; Code:

;; (defun rename-defun (function)
;;   "Rename loaded FUNCTION to a new name. Makfunbound FUNCTION after
;;   evaling the newly named defun."
;;   (interactive (find-function-read))
;;   (if (null function)
;;       (message "You didn't specify a function")
;;     (let ((function-name (symbol-name function)))
;;       (find-function-do-it function nil 'switch-to-buffer)
;;       (let ((begin (point))
;; 	    (end (save-excursion (end-of-defun) (point)))
;; 	    (new-name (read-string (concat "Rename " function-name " to: "))))
;; 	(save-excursion
;; 	  (replace-string function-name new-name t begin end))
;; 	(eval-defun nil)
;; 	(makfunbound function)))))

;;;###autoload
(defun byte-compile-directory(dir)
  "Compile an .elc file for every .el file contained under
      DIR (recursive)."
  (byte-recompile-directory (expand-file-name dir) 0))

;; [[http://stackoverflow.com/a/18680600][Feelin' a little Haskelly]]

(require 'cl-lib)

;;;###autoload
(defun combinations (&rest lists)
  "Return a list of all possible combinations of the elements of
  LISTS. Think applicative functors from Haskell."
  (if (car lists)
      (cl-mapcan (lambda (inner-val)
		(mapcar (lambda (outer-val)
			  (cons outer-val
				inner-val))
			(car lists)))
	      (apply #'combinations (cdr lists)))
    (list nil)))

;; [[http://www.emacswiki.org/emacs/TrampMode#toc30][EmacsWiki: Tramp Mode]]

;;;###autoload
(defun sudo-edit-current-file ()
  "Edit the current file as sudo with tramp, without needing to
  navigate to it or losing your place in the file. Works on local
  or remote files."
  (interactive)
  (let ((position (point)))
    (find-alternate-file
     (if (file-remote-p (buffer-file-name))
	 (let ((vec (tramp-dissect-file-name (buffer-file-name))))
	   (tramp-make-tramp-file-name
	    "sudo"
	    (tramp-file-name-user vec)
	    (tramp-file-name-host vec)
	    (tramp-file-name-localname vec)))
       (concat "/sudo:root@localhost:" (buffer-file-name))))
    (goto-char position)))

;;;###autoload
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line
endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;;;###autoload
(defun toggle-selective-display (column)
  "Enable code folding in current buffer."
  (interactive "P")
  (set-selective-display (if selective-display nil (or column 1))))

;;;###autoload
(defun delete-whole-word ()
  "This defun will delete the entire word at point. This function
    relies on `kill-whole-word'."
  (interactive)
  (kill-whole-word t))

;; TODO: accept a prefix arg
;;;###autoload
(defun kill-whole-word (&optional delete)
  "This defun will kill the entire word at point (on both sides
    of point). DELETE, if non-nil, will prevent the word from being
    appended to the kill-ring."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (save-excursion
      (goto-char (car bounds))
      (if (not delete)
	  (kill-word 1)
	(delete-region (car bounds) (cdr bounds))))))

;;;###autoload
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With (prefix) ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

;;;###autoload
(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With (prefix) ARG, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(provide 'mojo)

;;; mojo.el ends here

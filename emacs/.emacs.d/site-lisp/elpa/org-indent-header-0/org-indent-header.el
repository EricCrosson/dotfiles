;;; org-indent-header.el --- Provide commands to un/indent org headers in region
;; Version: 0.0.20140312

;; Copyright (C) 2015 Eric Crosson

;; Author: Eric Crosson <esc@ericcrosson.com>
;; Keywords: org
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

;; This package provides commands to un/indent org headers in region

;; Usage:

;; M-x org-indent-headers

;;; Code:

;; todo: use current line in absence of region
(defmacro define-org-header-indentation(funcname callback)
  "Define functions to manipulate the header level of selected
  region in `org-mode'."
  (let ((command-name (intern (format "org-%s-headers-in-region" funcname)))
	(docstring (format "%s `org-mode' headers arg times in currently
  selected region."
			   funcname)))
    `(defun ,command-name (&optional arg)
       ,docstring
       (interactive "p")
       (save-excursion
         (let ((beg (save-excursion
                      (goto-char (region-beginning)) (beginning-of-line) (point)))
               (end (save-excursion
                      (goto-char (region-end)) (end-of-line) (point))))
           (goto-char end)
           (while (and (< beg (point))
                       (re-search-backward "^\*" beg 'noerror))
             (dotimes (i arg) ,callback)
             (previous-line)
             (end-of-line)))))))

;; define commands `org-indent-headers' and `org-unindent-headers'.
(mapc (lambda (data)
	(let ((funcname (car data))
	      (callback (cadr data)))
	  (eval `(define-org-header-indentation ,funcname ,callback))))
      '((indent (insert "*"))
	(unindent (kill-forward-chars 1))))

(provide 'org-indent-header)

;;; org-indent-header.el ends here

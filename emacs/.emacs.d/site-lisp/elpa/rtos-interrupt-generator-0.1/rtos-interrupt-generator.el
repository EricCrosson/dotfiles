;;; rtos-interrupt-generator.el --- Generate isrs for hershic's ee445m
;; Version: 0.0.20140302

;; Copyright (C) 2015 Eric Crosson

;; Author: Eric Crosson <esc@ericcrosson.com>
;; Keywords: rtos
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
;;

;;; Usage:

;; ;; or prefer use-package like a bos
;; (require 'rtos-interrupt-generator)
;; (rtos/generate-interrupts)

;;; Code:

(defcustom rtos/interrupt-channels '(0 1 2)
  "Templates of interruptable device channel numbers on the
TM4C123G Cortex M4."
  :group 'rtos)
; (setq rtos/interrupt-channels )

(defcustom rtos/interrupt-devices '(uart timer)
  "Templates of interruptable device device numbers on the
TM4C123G Cortex M4."
  :group 'rtos)
;; (setq rtos/interrupt-devices '(uart timer))

(defun rtos/combinations (&rest lists)
  "Return a list of all possible combinations of the elements of LISTS."
  (if (car lists)
      (cl-mapcan (lambda (inner-val)
                (cl-mapcar (lambda (outer-val)
                          (cons outer-val
                                inner-val))
                        (car lists)))
              (apply #'rtos/combinations (cdr lists)))
    (list nil)))

(defun rtos/isr-template (file sub)
  "Return a list of lines of a file at FILE (symbol) with format
strings %s substituted with (concat FILE SUB)."
  (let* ((interface (symbol-name file))
	 (template-dir (if (string-equal user-login-name "eric")
			   "~/workspace"
			 "~")))
    (with-temp-buffer
      (insert-file-contents
       (format
	"%s/ee445m-labs/bin/lisp/templates/%s.c" template-dir interface))
      (buffer-string))))

;;;###autoload
(defun rtos/generate-interrupts()
  "Insert at point interrupt handlers for every permutation of
device and channel number in `rtos/interrupt-devices' and
`rtos/interrupt-channels', respectively."
  (interactive)
  (save-excursion
    (mapcar (lambda (template)
	      (let* ((device (cadr template))
		     (id     (car template))
		     (isr-handler (upcase (symbol-name device)))
		     (fn-body (format "%s\n" (rtos/isr-template device id))))
		;; todo wrap in fn prototype and interpolate
		(insert
		 (replace-regexp-in-string
		  "%s"
		  (format "%s%d" isr-handler id) fn-body))))
	    (rtos/combinations rtos/interrupt-channels rtos/interrupt-devices))
    (kill-line)))

(provide 'rtos-interrupt-generator)

;;; rtos-interrupt-generator.el ends here

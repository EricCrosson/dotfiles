;;; clean-mode-line.el --- Transform mode-line symbols into user-defined strings
;; Version: 0.0.20140302

;; Copyright (C) 2015 Eric Crosson

;; Author: Eric Crosson <esc@ericcrosson.com>
;; Keywords: mode-line
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

;; Thanks to
;; [[http://www.masteringemacs.org/article/hiding-replacing-modeline-strings]][[Mastering Emacs]]

;; This package transforms symbols on the mode line to user-defined
;; strings

;;; Usage

;;       (setq clean-mode-line-alist
;;               `(;; (mode . replacement_string)
;;                 (enh-ruby-mode . "enh-Rb")
;;                 (ruby-mode   . "Rb")
;;                 (python-mode . "Py")
;;                 (lisp-interaction-mode . "λ")
;;                 (emacs-lisp-mode . "eλ")
;;                 (lisp-mode . "λ")))

;;; Code:

(defcustom clean-mode-line-alist nil
  "Alist for function `clean-mode-line'.

When adding a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.

Example:

    (setq clean-mode-line-alist
             `( ;; (mode . replacement_string)
              (enh-ruby-mode . \"enh-Rb\")
              (ruby-mode   . \"Rb\")
              (python-mode . \"Py\")
              (lisp-interaction-mode . \"λ\")
              (emacs-lisp-mode . \"eλ\")
              (lisp-mode . \"λ\")))"
  :group 'mode-line)

;;;###autoload
(defun clean-mode-line ()
  "Clean the mode line by transforming symbols into
custom-defined strings.

See variable `clean-mode-line-alist' for more documentation."
  (interactive)
  (loop for cleaner in clean-mode-line-alist
        do (let* ((mode (car cleaner))
		  (mode-str (cdr cleaner))
		  (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
	       (setcar old-mode-str mode-str))
	     ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))

(provide 'clean-mode-line)

;;; clean-mode-line.el ends here

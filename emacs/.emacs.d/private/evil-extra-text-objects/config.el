;;; evil-extra-text-objects.el --- Nonstandard evil text-objects

;; Author: Eric Crosson <esc@ericcrosson.com>
;; Created: 2015-07-17
;; Version: 1.0.0
;; Keywords: convenience evil
;; Package-Requires: ((evil "0"))
;;
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Adds new textobjects:
;;;
;;; ie, ae: entire buffer
;;; il, al, current line

;;; Code:

(defmacro define-and-bind-text-object (key start-regex end-regex)
  (let ((inner-name (make-symbol "inner-name"))
        (outer-name (make-symbol "outer-name")))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

;;;###autoload
(after 'evil
  ;; create "il"/"al" (inside/around) line text objects:
  (define-and-bind-text-object "l" "^\\s-*" "\\s-*$")
  ;; create "ie"/"ae" (inside/around) entire buffer text objects:
  (define-and-bind-text-object "e" "\\`\\s-*" "\\s-*\\'"))

(provide 'evil-extra-text-objects)
;;; evil-extra-text-objects ends here

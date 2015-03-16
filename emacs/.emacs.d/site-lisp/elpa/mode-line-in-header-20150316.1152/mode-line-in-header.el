;;; mode-line-in-header.el --- Minor mode to display the mode line in the header of the current buffer
;; Version: 0.0.20140228

;; Copyright (C) 2015  Eric Crosson

;; Author: Eric Crosson <esc@ericcrosson.com>
;; Keywords: mode-line
;; Package-Version: 20150316.1152
;; Package-X-Original-Version: 0

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

;;; Contributors:

;; This mode was inspired by Bastien Guerry
;; [[http://bzg.fr/emacs-strip-tease.html][#Emacs, naked]].

;; Eric Crosson -- https://github.com/EricCrosson
;;      created initial package

;; Lars Tveito -- https://github.com/larstvei
;;      globalized minor mode

;;; Commentary:

;; This minor mode displays the mode line in the header of the current
;; buffer.

;;; Usage:

;; (mode-line-in-header 1)

;;; Code:

;;;###autoload
(define-minor-mode mode-line-in-header
  "Minor mode to display the mode line in the header of the current buffer."
  :init-value nil
  :lighter " mode-header"
  :global nil
  :group 'editing-basics
  (if mode-line-in-header
      (setq header-line-format mode-line-format
            mode-line-format nil)
    (setq mode-line-format header-line-format
          header-line-format nil))
  (force-mode-line-update))

;;;###autoload
(define-globalized-minor-mode
  global-mode-line-in-header
  mode-line-in-header
  (lambda () (mode-line-in-header 1)))

(provide 'mode-line-in-header)

;;; mode-line-in-header.el ends here

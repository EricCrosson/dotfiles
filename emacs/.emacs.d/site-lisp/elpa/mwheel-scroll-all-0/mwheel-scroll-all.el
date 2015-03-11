;;; mwheel-scroll-all.el --- Use the mouse during `scroll-all-mode'.
;; Version: 0.0.20140311

;; Copyright (C) 2015 Eric Crosson

;; Author: Eric Crosson <esc@ericcrosson.com>
;; Keywords: scroll,mwheel
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

;; This package provides commands to configure the mouse scroll wheel
;; properly during `scroll-all-mode'.

;;; Usage:

;; This library is intended for esc only. It fits in his configs
;; (hopefully), if you want to see how that works see his init.org.

;;; Code
(defgroup mwheel-all nil
  "Use the mouse during `scroll-all-mode'."
  :group 'mwheel-all)

;;;###autoload
(defun mwheel-scroll-all-function-all (func arg)
  (if scroll-all-mode
      (save-selected-window
        (walk-windows
         (lambda (win)
           (select-window win)
           (condition-case nil
               (funcall func arg)
             (error nil)))))
    (funcall func arg)))

;;;###autoload
(defun mwheel-scroll-all-scroll-up-all (&optional arg)
  (mwheel-scroll-all-function-all 'scroll-up arg))

;;;###autoload
(defun mwheel-scroll-all-scroll-down-all (&optional arg)
  (mwheel-scroll-all-function-all 'scroll-down arg))

(provide 'mwheel-scroll-all)

;;; mwheel-scroll-all.el ends here

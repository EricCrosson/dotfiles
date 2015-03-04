;;; browse-cliplink.el --- invoke browse-web at the url in your clipboard
;; Version: 0.0.20140302

;; Copyright (C) 2015  Eric Crosson

;; Author: Eric Crosson <esc@ericcrosson.com>
;; Keywords: browse,web
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

;; This package provides M-x browse-cliplink, which will invoke M-x
;; browse-web on the url in your clipboard.

;;; Usage:

;; M-x browse-cliplink

;;; Code:

;;;###autoload
(defun browse-cliplink ()
  "Invoke function `browse-web' on the url in your clipboard."
  (interactive)
  (let ((url (substring-no-properties (current-kill 0))))
    (browse-web url)))

(provide 'browse-cliplink)

;;; browse-cliplink.el ends here

;;; tagsgen.el --- Generate etags in a dir chosen by ido-completing-read

;; Version: 0.0.20140227

;; Copyright (C) 2015 Eric Crosson

;; Author: Eric Crosson <esc@ericcrosson.com>
;; Keywords: tags
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

;; This package provides `tagsgen', which prompts the user for a dir
;; with `ido-read-directory-name' and generates a TAGS file for that
;; dir via exuberant ctags.

;; Usage:

;; M-x tagsgen

;;; Code

;;;###autoload
(defun tagsgen ()
  "Create a TAGS file using etags in a directory queried with
`ido-read-directory-name'."
  (interactive)
  (let* ((tags-dir (ido-read-directory-name "Generate tags in dir: " )))
    (shell-command
     (format
      "cd %s && find . -regex \".*\\.[cChH]\\(pp\\)?\" -print | etags -"
      tags-dir))))

(provide 'tagsgen)

;;; tagsgen.el ends here

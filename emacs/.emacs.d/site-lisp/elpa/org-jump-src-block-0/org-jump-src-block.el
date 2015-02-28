;;; org-jump-src-block.el --- Move point by org source blocks
;; Version: 0.0.20140227

;; Copyright (C) 2015 Eric Crosson

;; Author: Eric Crosson <esc@ericcrosson.com>
;; Keywords: scroll
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

;; This package provides commands for moving point by org source
;; blocks.

;; Usage:

;; (global-set-key (kbd "C-.") 'org-jump-prev-src-block)
;; (global-set-key (kbd "C-,") 'org-jump-next-src-block)

;;; Code

(defvar org-jump-src-block-regexp
  "^#\\+[Bb][Ee][Gg][Ii][Nn]_[Ss][Rr][Cc]"
  "Regexp of an org src block.")

;;;###autoload
(defun org-jump-next-src-block (&optional n)
  "Jump forward by 1 or N src blocks."
  (interactive)
  (dotimes (i (or n 1))
    (re-search-forward org-jump-src-block-regexp nil t)
    (next-line)
    (beginning-of-line)))

;;;###autoload
(defun org-jump-prev-src-block (&optional n)
  "Jump backward by 1 or N src blocks."
  (interactive)
  (dotimes (i (or n 1))
    (previous-line)
    (beginning-of-line)
    (re-search-backward org-jump-src-block-regexp nil t)))

(provide 'org-jump-src-block)

;;; org-jump-src-block.el ends here

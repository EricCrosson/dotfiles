;;; test-mode.el --- A destructable minor mode of a playground to make restarts less frequent

;; Copyright (C) 2015   Eric Steven Crosson esc

;; Author: Eric Crosson <esc@ericcrosson.com>
;; Keywords: compilation
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
;; I don't do anything specific with this mode but test on it until
;; it's entirely hosed and then I disable it.

;;; Code:

(defvar test-mode-map (make-keymap)
  "The keymap for function `test-mode'.")

;;;###autoload
(define-minor-mode test-mode
  "Toggle test-keys mode.
                   A destructable minor mode to make restarts less frequent."
  t " test" 'test-mode-map)

(provide 'test-mode)

;;; test-mode.el ends here

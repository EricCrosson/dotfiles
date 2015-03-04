;;; big-fringe.el --- Minor mode to use a big fringe in the current buffer
;; Version: 0.0.20140228

;; Copyright (C) 2015 Eric Crosson

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

;; Thanks to [[http://bzg.fr/emacs-strip-tease.html][#Emacs, naked]]
;; who gets all credit for this mode.

;; This mode enables a large fringe for fullscreen-viewing of a
;; centered buffer.

;; Note: this package is intended for use with only one buffer
;; visible.

;;; Code:
;;:###autoload
(define-minor-mode big-fringe
  "Minor mode to use big fringe in the current buffer."
  :init-value nil
  :lighter " fringe"
  :global nil
  :group 'editing-basics
  (if (not big-fringe)
      (set-fringe-style nil)
    (set-fringe-mode
     (/ (- (frame-pixel-width)
	   (* 100 (frame-char-width)))
	2))))

(provide 'big-fringe)

;;; big-fringe.el ends here

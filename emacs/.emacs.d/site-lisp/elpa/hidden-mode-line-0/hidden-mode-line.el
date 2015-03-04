;;; hidden-mode-line.el --- Minor mode to hide the mode line in current buffer
;; Version: 0.0.20140228

;; Copyright (C) 2015  Eric Crosson

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

;; This minor mode hides the mode line in the current buffer when
;; enabled.

;; This mode is simply a cleaned up version of one of the snippets on
;; [[http://bzg.fr/emacs-strip-tease.html][#Emacs, naked]].

;;; Usage:

;; (hidden-mode-line 1)

;;; Code:

;;;###autoload
(define-minor-mode hidden-mode-line
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :lighter " mode-none"
  :global nil
  :group 'editing-basics
  (if hidden-mode-line
      (setq hide-mode-line   mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line   nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line to make the mode-line appear."))))

(provide 'hidden-mode-line)

;;; hidden-mode-line.el ends here

;;; ido-extras.el --- Miscellaneous defuns utilizing ido
;; Version: 0.0.20140302

;; Copyright (C) 2015  Eric Crosson

;; Author: Eric Crosson <esc@ericcrosson.com>
;; Keywords: ido
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

;; Each function pretty much stands on its own.  See each defun's
;; docstring for more details.

;;; Code:

;;;###autoload
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file."
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;;;###autoload
(defun ido-goto-symbol (&optional symbol-list)
  "Use `ido-completing-read` to query a function in the current
buffer and jump to it. Functions are defined by the active minor
mode."
  (interactive)
  (defvar symbol-names)
  (defvar name-and-pos)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          name-and-pos symbol-names position)
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol: " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (push-mark nil t nil)             ;set the mark
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (ido-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position))))))))

;;;###autoload
(defun ido-switch-buffer-current-major-mode ()
  "Invoke function `ido-switch-buffer' listing only buffers of
the same major mode as the current buffer.

from http://emacswiki.org/emacs/InteractivelyDoThings#toc6"
  (interactive)
  (let ((the-mode major-mode))
    (switch-to-buffer
     (ido-completing-read
      (format "Buffers of %s: " the-mode)
      (save-excursion
        (delq nil (mapcar (lambda (buf) (when (buffer-live-p buf)
				     (with-current-buffer buf
				       (and (eq major-mode the-mode)
					    (buffer-name buf)))))
			  (buffer-list))))))))

(provide 'ido-extras)

;;; ido-extras.el ends here

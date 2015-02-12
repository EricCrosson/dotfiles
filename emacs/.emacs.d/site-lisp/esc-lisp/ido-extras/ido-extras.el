
;;; ido-config.el --- Miscellaneous defuns shining with ido
;; Configuration functions for using interactive-do.

;;; Commentary:
;; Each function pretty much stands on its own. See each defun's
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

(provide 'ido-config)

;;; ido-config.el ends here


;; todo: use current line in absence of region
(defmacro esc/define-org-header-indentation(funcname callback)
  "Define functions to manipulate the header level of selected
region in `org-mode'."
  (let ((command-name (intern (format "esc/org-%s-headers-in-region" funcname)))
        (docstring (format "%s `org-mode' headers arg times in currently
selected region."
                           funcname)))
  `(defun ,command-name (&optional arg)
     ,docstring
     (interactive "p")
     (save-excursion
       (let ((beg (save-excursion
                    (goto-char (region-beginning)) (beginning-of-line) (point)))
             (end (save-excursion
                    (goto-char (region-end)) (end-of-line) (point))))
         (goto-char end)
         (while (and (< beg (point))
                     (re-search-backward "^\*" beg 'noerror))
           (dotimes (i arg) ,callback)
           (previous-line)
           (end-of-line)))))))

(mapcar (lambda (data)
          (let ((funcname (car data))
                (callback (cadr data)))
            (eval `(esc/define-org-header-indentation ,funcname ,callback))))
        '((indent (insert "*"))
          (unindent (kill-forward-chars 1))))

(provide 'org+)

;;; org+.el ends here

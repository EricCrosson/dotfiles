;;; browse-cliplink.el --- invoke browse-web at the url in your clipboard

;;; Commentary:
;;

;;; Code:

;;;###autoload
(defun browse-cliplink ()
  "\\[browse-web] at the url in your clipboard."
  (interactive)
  (let ((url (substring-no-properties (current-kill 0))))
    (browse-web url)))

(provide 'browse-cliplink)

;;; browse-cliplink.el ends here

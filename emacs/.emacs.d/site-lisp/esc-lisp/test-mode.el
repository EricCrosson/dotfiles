
;;; test-mode.el --- A destructable minor mode to make restarts less frequent.


;;; Commentary:
;;

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

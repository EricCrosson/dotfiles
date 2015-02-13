
(defvar mode-line-cleaner-alist
  `(;; Major modes
    (enh-ruby-mode . "enh-Rb")
    (ruby-mode   . "Rb")
    (python-mode . "Py")
    (lisp-interaction-mode . "λ")
    (emacs-lisp-mode . "eλ")
    (lisp-mode . "λ"))
  "Alist for `clean-mode-line'.

When adding a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

;;;###autoload
(defun clean-mode-line ()
  "Clean the mode line by transforming symbols into
custom-defined strings.

See `mode-line-cleaner-alist' for more documentation."
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                 (mode-str (cdr cleaner))
                 (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
                 (setcar old-mode-str mode-str))
               ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))

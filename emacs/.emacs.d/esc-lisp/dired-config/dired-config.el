
(defcustom esc/dired-mode-hook nil
  "esc's hook to add to \\[dired-mode-hook]."
  :type    'hook
  :options '((define-key dired-mode-map (kbd "M-RET") 'esc/dired-find-file-single-mode)
             (define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'esc/dired-back-to-top)
             (define-key dired-mode-map (vector 'remap 'end-of-buffer) 'esc/dired-jump-to-bottom)
             (define-key dired-mode-map (kbd "^") 'esc/dired-up-level-reuse-buffer))
  :group   'esc-dired)

;;;###autoload
(defun esc/dired-mode-hook ()
  "esc's hook to \\[dired-mode-hook]."
  (let ((activate (if (eq major-mode 'dired-mode) 1 -1)))
    (hl-line-mode activate)
    (after 'dired-x (dired-omit-mode activate)))
  (setq-default dired-details-hidden-string "--- "
                dired-recursive-copies 'always
                dired-recursive-deletes 'top
                dired-omit-verbose nil)
  (define-key dired-mode-map (kbd "C-a") 'esc/dired-back-to-start-of-files)
  (define-key dired-mode-map (kbd "M-RET") 'esc/dired-find-file-single-mode)
  (define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'esc/dired-back-to-top)
  (define-key dired-mode-map (vector 'remap 'end-of-buffer) 'esc/dired-jump-to-bottom)
  (define-key dired-mode-map (kbd "^") 'esc/dired-up-level-reuse-buffer))

(defcustom esc/dired-load-hook nil
  "esc's hook to add to \\[dired-mode-hook]."
  :type    'hook
  :options '((load "dired-x" nil t)
             (setq dired-listing-switches "-alh") ; todo: make friendly
             (esc/dired-load-hook-omit-files))
  :group   'esc-dired)

;;;###autoload
(defun esc/dired-load-hook ()
  "esc's hook to \\[dired-load-hook]."
  (load "dired-x" nil t) ; todo: make friendly
  (setq-default dired-listing-switches "-alh")
  (setq-default dired-recursive-copies 'always)
  (esc/dired-load-hook-omit-files))

;;;###autoload
(defun esc/dired-load-hook-omit-files ()
  (setq dired-omit-verbose nil)
  (setq dired-omit-files (concat dired-omit-files "\\|^\\.dropbox$"))
  (after 'undo-tree
    (setq dired-omit-files (concat dired-omit-files "\\|^\\.*~undo-tree~$"))))

;;;###autoload
(defun esc/dired-back-to-top ()
  "Goes to the first line in dired, not the top of the buffer."
  (interactive)
  (beginning-of-buffer)
  (when (not (search-forward ".." nil 'noerror))
    (beginning-of-buffer)
    (dired-next-line 1))          ;likely dired-omit-mode is enabled
  (dired-next-line 1))

;;;###autoload
(defun esc/dired-jump-to-bottom ()
  "Goes to the last line in dired, not the bottom of the buffer."
  (interactive)
  (end-of-buffer)
  (dired-previous-line 1))

;;;###autoload
(defun esc/dired-find-file-single-mode ()
  "This defun will invoke `dired-find-file` and open the file(s) at point in `single-mode'."
  (interactive)
  (dired-find-file)
  (single-mode 1))

;;;###autoload
(defun esc/dired-up-level-reuse-buffer ()
  "This defun will go to the parent directory in dired while reusing the current buffer."
  (interactive)
  (find-alternate-file ".."))

;;;###autoload
(defun esc/search-my-lisp-dir ()
  "Open esc-lisp in `dired' for easy editing of configs."
  (interactive)
  (esc/edit-configs)
  (esc/dired-back-to-top)
  (call-interactively 'isearch-forward))

;;;###autoload
(defun esc/dired-back-to-start-of-files ()
  (interactive)
  (backward-char (- (current-column) 2)))

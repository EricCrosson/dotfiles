
;;;###autoload
(defun esc/prog-mode-hook ()
 "esc's hook to \\[prog-mode-hook]."
  (auto-revert-mode t)
  (smerge-mode t))

;;;###autoload
(defun esc/minibuffer-setup-hook ()
  "esc's hook to run whilst entering the minibuffer's domain."
  (autopair-mode -1)
  (define-key minibuffer-local-map (kbd "<escape>") nil))

;;;###autoload
(defun esc/c-mode-common-hook ()
  "esc's code to \\[c-mode-common-hook]."
  ;(idle-highlight t)
  (autoload 'disaster "disaster")
  (local-set-key (kbd "C-c d") 'disaster)
  (local-set-key (kbd "C-c o") 'ff-find-other-file)
  (local-set-key (kbd "C-c d") 'esc/c-insert-ifdef-ndebug-printf)
  (local-set-key (kbd "C-M-;") 'esc/yank-displaced-doxygen-comment)
  (local-set-key (kbd "C-M-S-u") 'esc/unroll-cc-arguments)
  (c-turn-on-eldoc-mode)
  (setq ff-always-in-other-window t
        ff-always-try-to-create nil))

;;;###autoload
(defun esc/c++-mode-hook ()
  "esc's code to \\[c++-mode-hook]."
  (setq comment-start "/*"
        comment-end "*/"))

;;;###autoload
(defun esc/emacs-lisp-mode-hook ()
  "esc's code to \\[emacs-lisp-mode-hook]."
  (turn-on-eldoc-mode))

;;;###autoload
(defun esc/fundamental-mode-hook ()
  "esc's code to \\[fundamental-mode-hook]."
  (autopair-mode -1)
  (flyspell-mode 1))

(defun esc/eshell-mode-hook ()
  (define-key eshell-mode-map (kbd "C-x C-p") 'eshell-previous-matching-input-from-input)
  (define-key eshell-mode-map (kbd "C-x C-n") 'eshell-next-matching-input-from-input)
  (when (require 'em-smart nil 'noerror)
    (setq eshell-where-to-jump 'begin
          eshell-review-quick-commands nil
          eshell-smart-space-goes-to-end t)))

;;;###autoload
(defun esc/erc-mode-hook ()
  "esc's code to \\[erc-mode-hook]."
  (autopair-mode -1))

;;;###autoload
(defun esc/comint-mode-hook ()
  "esc's code to \\[comint-mode-hook]."
  (bind-key "M" 'self-insert-command))

;;;###autoload
(defun esc/org-mode-hook()
  (org-indent-mode)
  (imenu-add-to-menubar "Imenu")
  (local-set-key (kbd "C-M-n") 'outline-next-visible-heading)
  (local-set-key (kbd "C-M-p") 'outline-previous-visible-heading)
  (local-set-key (kbd "C-c C-a") 'org-todo))

;;; Dired configuration

;;;###autoload
(defun esc/dired-load-hook-omit-files ()
  (setq dired-omit-verbose nil)
  (after 'undo-tree
    (setq dired-omit-files (concat dired-omit-files "\\|^\\.*~undo-tree~$"))))

;;;###autoload
(defun esc/dired-back-to-top ()
  "Goes to the first line in dired, not the top of the buffer."
  (interactive)
  (beginning-of-buffer)
  (when (not (search-forward ".." nil 'noerror))
    (beginning-of-buffer))          ;likely dired-omit-mode is enabled
  (dired-next-line 1))

;;;###autoload
(defun esc/dired-jump-to-bottom ()
  "Goes to the last line in dired, not the bottom of the buffer."
  (interactive)
  (end-of-buffer)
  (dired-previous-line 1))

;;;###autoload
(defun esc/dired-up-level-reuse-buffer ()
  "This defun will go to the parent directory in dired while reusing the current buffer."
  (interactive)
  (find-alternate-file ".."))

;;;###autoload
(defun esc/dired-back-to-start-of-files ()
  (interactive)
  (backward-char (- (current-column) 2)))


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
  (define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'esc/dired-back-to-top)
  (define-key dired-mode-map (vector 'remap 'end-of-buffer) 'esc/dired-jump-to-bottom)
  (define-key dired-mode-map (kbd "^") 'esc/dired-up-level-reuse-buffer))

;;;###autoload
(defun esc/dired-load-hook ()
  "esc's hook to \\[dired-load-hook]."
  (load "dired-x" nil t) ; todo: make friendly
  (setq-default dired-listing-switches "-alh")
  (setq-default dired-recursive-copies 'always
		dired-recursive-deletes 'top)
  (esc/dired-load-hook-omit-files))

;;; Multiple cursors config


;;;###autoload
(defun esc/multiple-cursors-mode-enabled-hook ()
  "esc's code to function `multiple-cursors-mode-enabled-hook'."
  (autopair-mode -1)
  ;; mc/keymap isn't created until this point
  (defvar mc/search--last-term nil)
  (define-key mc/keymap (kbd "C-s") 'mc/search-forward)
  (define-key mc/keymap (kbd "C-r") 'mc/search-backward))

;;;###autoload
(defun esc/multiple-cursors-mode-disabled-hook ()
  "esc's code to function `multiple-cursors-mode-disabled-hook`."
  (autopair-mode 1))

;;;###autoload
(defun mc/search (search-command)
  "Iterate through each active cursor and search for the designated string.
  SEARCH-COMMAND is the command to use for movement- either
  function `search-forward` or function `search-backward`.

  Helper defun for function `mc/search-forward` and function `mc/search-backward`."
  ;; Read new search term when not repeated command or applying to fake cursors
  (when (and (not mc--executing-command-for-fake-cursor)
	     (not (eq last-command 'mc/search-forward))
	     (not (eq last-command 'mc/search-backward)))
    (setq mc/search--last-term (read-from-minibuffer "Search: ")))
  (funcall search-command mc/search--last-term))

;;;###autoload
(defun mc/search-forward ()
  "Simplified version of forward search that supports multiple cursors."
  (interactive)
  (mc/search 'search-forward))

;;;###autoload
(defun mc/search-backward ()
  "Simplified version of backward search that supports multiple cursors."
  (interactive)
  (mc/search 'search-backward))


;;; Attach `esc-mode' hooks

(defun esc/install-hooks ()
  "Install hoos to be added or removed with the de/activation of
function `esc-mode'."

  ;; add canonical hooks
  (mapc (lambda (hook)
	  (let ((attach (intern (format "esc/%s" hook))))
	    (if esc-mode
		(add-hook hook attach)
	      (remove-hook hook attach))))
	'(erc-mode-hook
	  prog-mode-hook
	  fundamental-mode-hook
	  emacs-lisp-mode-hook
	  c-mode-common-hook
	  c++-mode-hook
	  eshell-mode-hook
	  iedit-mode-hook
	  comint-mode-hook
	  org-mode-hook
	  dired-mode-hook
	  dired-load-hook
	  multiple-cursors-mode-enabled-hook
	  save-buffers-kill-emacs))

  ;; add miscellaneous hooks
  (mapc (lambda (hook-data)
	  (let ((hook (car hook-data))
		(attach (cdr hook-data)))
	    (if esc-mode
		(add-hook hook attach)
	      (remove-hook hook attach))))
	'((text-mode-hook . turn-on-auto-fill)
	  (find-file-hook . remove-dos-eol)
	  (before-save-hook . delete-trailing-whitespace)
	  (after-save-hook . executable-make-buffer-file-executable-if-script-p))))

(add-hook 'esc-mode-hook 'esc/install-hooks)

(provide 'esc-hooks)

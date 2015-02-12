
(defcustom esc/multiple-cursors-mode-enabled-hook nil
  "esc's code to \\[multiple-cursors-mode-enabled-hook]."
  :type         'hook
  :options      '((autopair-mode -1)
		  (define-key mc/keymap (kbd "C-s") 'mc/search-forward)
		  (define-key mc/keymap (kbd "C-r") 'mc/search-backward))
  :group        'multiple-cursors)

;;;###autoload
(defun esc/multiple-cursors-mode-enabled-hook ()
  "esc's code to \\[multiple-cursors-mode-enabled-hook]."
  (autopair-mode -1)
  ;; mc/keymap isn't created until this point
  (defvar mc/search--last-term nil)
  (define-key mc/keymap (kbd "C-s") 'mc/search-forward)
  (define-key mc/keymap (kbd "C-r") 'mc/search-backward))

(defcustom esc/multiple-cursors-mode-disabled-hook nil
  "esc's code to \\[multiple-cursors-mode-disabled-hook]."
  :type         'hook
  :options      '((autopair-mode 1))
  :group        'multiple-cursors)

;;;###autoload
(defun esc/multiple-cursors-mode-disabled-hook ()
  "esc's code to \\[multiple-cursors-mode-disabled-hook]."
  (autopair-mode 1))

;; Thanks, Jeff!
;; http://www.jeffchu.com/posts/2013/01/2013-01-29-multiple-cursors-mode.html
;;;###autoload
(defun mc/search (search-command)
  "Iterate through each active cursor and search for the designated string.
SEARCH-COMMAND is the command to use for movement- either
\\[search-forward] or \\[search-backward].

Helper defun for \\[mc/search-forward] and \\[mc/search-backward]."
  ;; Read new search term when not repeated command or applying to fake cursors
  (when (and (not mc--executing-command-for-fake-cursor)
             (not (eq last-command 'mc/search-forward))
             (not (eq last-command 'mc/search-backward)))
    (setq mc/search--last-term (read-from-minibuffer "Search: ")))
  (funcall search-command mc/search--last-term))

;; TODO: allow for regexp's
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

;;; esc-mode global namespace macros

(defmacro esc-mode-enabledp ()
  "A macro to determine if \\[esc-mode] is currently enabled."
  `(progn (and (boundp 'esc-mode) esc-mode)))

(defmacro dont (operand)
  "A macro to avoid awkward code in `esc-mode-hook'.
Argument OPERAND is an integer to use as an argument for a mode."
  `(ignore-errors
     (if (numberp ,operand)
         (* (- 1) ,operand)
       (if ,operand nil t))))

(defmacro esc-key (sequence action)
  "Bind SEQUENCE to ACTION in `esc-mode-map'."
  `(define-key esc-mode-map (kbd ,sequence) ,action))

(defmacro esc-keys (&rest binding-list)
  "Bind all keybindings and functions in BINDING-LIST
in `esc-mode-map'."
  (declare (indent defun))
  `(mapc (lambda (binding) (esc-key (car binding) (cdr binding)))
	 '(,@binding-list)))

(defmacro esc-replace-key (old-action new-action)
  "Replace OLD-ACTION with NEW-ACTION in `esc-mode-map'."
  `(define-key esc-mode-map (vector 'remap ,old-action) ,new-action))

;;; esc-mode global namespace functions

(defun search-word-backward ()
  "Find the previous occurrence of the word at point."
  (interactive)
  (let ((cur (point)))
    (skip-syntax-backward "w_")
    (goto-char
     (if (re-search-backward (concat "\\_<" (current-word) "\\_>") nil t)
	 (match-beginning 0)
       cur))))

(defun search-word-forward ()
  "Find the next occurrence of the word at point."
  (interactive)
  (let ((cur (point)))
    (skip-syntax-forward "w_")
    (goto-char
     (if (re-search-forward (concat "\\_<" (current-word) "\\_>") nil t)
	 (match-beginning 0)
       cur))))

;; Thanks sacha! See
;; [[http://pages.sachachua.com/.emacs.d/Sacha.html#sec-1-5-1][Sacha
;; Chua's Emacs configuration]] for more tips.
;;;###autoload
(defun esc/vsplit-last-buffer (prefix)
  "Split the window vertically and display the previous buffer.
Argument PREFIX when nil switches the new buffer to the last buffer."
  (interactive "p")
  (split-window-vertically)
  (other-window 1 nil)
  (unless prefix (switch-to-next-buffer)))

;;;###autoload
(defun esc/hsplit-last-buffer (prefix)
  "Split the window horizontally and display the previous buffer.
Argument PREFIX when nil switches the new buffer to the last buffer."
  (interactive "p")
  (split-window-horizontally)
  (other-window 1 nil)
  (unless prefix (switch-to-next-buffer)))

;;;###autoload
(defun mode-line-other-buffer-other-window ()
  "Switch to `other-window', use `mode-line-other-buffer', and
switch back to the original window."
  (interactive)
  (other-window 1)
  (mode-line-other-buffer)
  (other-window -1))

;;;###autoload
(defun back-to-indentation-or-beginning ()
  "Returns the point to the beginning of the current line, or if
already there, the beginning of text on the current line."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line)
    (when (eq pt (point))
      (beginning-of-line-text))))

;; Courtesy of [[http://oremacs.com/2015/01/26/occur-dwim/][or emacs]].
;;;###autoload
(defun occur-dwim ()
  "Call function `occur' with a sane default.

Function `occur-dwim' will offer as the default candidate:
- the current region, if it's active
- the current symbol, otherwise"
  (interactive)
  (push (if (region-active-p)
	    (buffer-substring-no-properties
	     (region-beginning)
	     (region-end))
	  (thing-at-point 'symbol))
	regexp-history)
  (call-interactively 'occur))

;;;###autoload
(defun middle-click-yank ()
  "Yank from the middle click mouse buffer."
  (interactive)
  (mouse-yank-primary 1))

;;;###autoload
(defun copy-line (&optional arg)
  "Copy current line in the kill ring."
  (interactive "p")
  (let ((lines (if arg (+ 1 arg) 2)))
    (kill-ring-save (line-beginning-position)
		    (line-beginning-position lines))
    (message (if (eq 2 lines)
		 "Line copied."
	       (format "%d lines copied." lines)))))

;;;###autoload
(defun open-line-below ()
  "Create a new line above the current line. Can be used with point
anywhere on the line."
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

;;;###autoload
(defun open-line-above ()
  "Create a new line below the current line. Can be used with point
anywhere on the line."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

;;;###autoload
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect (progn
		    (linum-mode 1)
		    (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))


;; [[http://whattheemacsd.com/key-bindings.el-03.html][Magnar]] said
;; it best, here's one command I could not live without.

;;;###autoload
(defun pull-up-line (&optional arg)
  "Pull up ARG lines."
  (interactive "p")
  (dotimes (i arg)
    (join-line -1)))

;; TODO: find a way to keep these internal window changes from being
;; registered by winner-mode

;;;###autoload
(defun follow-mode-80-char-compliant ()
  "Open the current buffer in `follow-mode' in as many 80-char
windows as you can fit in the current frame. All other windows
will be closed."
  (interactive)
  (delete-other-windows)
  (follow-mode 1)
  (let ((width (window-total-width nil 'floor)))
    (while (> width 80)
      (split-window-horizontally)
      (balance-windows)
      (setq width (window-total-width nil 'floor))))
  (delete-window)
  (balance-windows)
  (recenter-top-bottom))

;;;###autoload
(defun hsplit-current-buffer-follow-mode ()
  "Split the current buffer horizontally and engage function
`follow-mode'."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (follow-mode 1))

(defun esc/visit-favorite-dir (files-too)
  "Offer all directories inside a set of directories.
Compile a list of all directories inside each element of
`esc/favorite-directories', and visit one of them with
`ido-completing-read'.
With prefix argument FILES-TOO also offer to find files."
  (interactive "P")
  (let ((completions
         (mapcar #'abbreviate-file-name
		 (cl-remove-if-not
		  (if files-too #'file-readable-p
		    #'file-directory-p)
		  (apply #'append
			 (mapcar (lambda (x)
				   (directory-files
				    (expand-file-name x)
				    t "^[^\.].*" t))
				 esc/favorite-directories))))))
    (dired
     (ido-completing-read "Open directory: "
                          completions 'ignored nil ""))))


  ;;;###autoload
(defmacro esc/toggle-fullscreen-buffer (win-register toggled-mode-test toggle-command
						     &optional
						     toggle-command-test
						     clear-command)
  "Bring up a temporary buffer in fullscreen mode, or restore the
  previous window configuration.

  WIN-REGISTER         is the register to store the old window configuration in.

  TOGGLED-MODE-TEST    is the major mode of the toggled state, in other words a
                       test to determine which way to toggle the buffers.

  TOGGLE-COMMAND       is the command to run when toggling into the temporary
                       state.

  CLEAR-COMMAND        is an optional command to run when reverting back to the
                       original state; i.e. toggle a flag"
  (declare (indent defun))
  `(progn
     (if ,toggled-mode-test
	 (progn (jump-to-register ,win-register)
		(when (not (equal nil ,clear-command))
		  ,clear-command))
       (window-configuration-to-register ,win-register)
       ,toggle-command
       (delete-other-windows))))

  ;;;###autoload
(defun esc/raise-magit-status ()
  "Bring up a full-screen magit-status or restore previous
  window configuration.

  This defun will not raise magit if you have merge conflicts in
  the current buffer.

  This defun kills all buffers matching regexp '^*magit: ' upon the
  exit toggle of the fullscreen magit buffer"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (when (re-search-forward "^<<<<<<< variant" nil 'noerror)
      (error "Resolve merge conflicts first.")))
  (esc/toggle-fullscreen-buffer
    :magit-fullscreen
    (string= "magit-status-mode" major-mode)
    (progn (if (not (buffer-file-name))
	       (message "Buffer not associated with a file")
	     (save-buffer)
	     (magit-status (file-name-directory (buffer-file-name)))))
    nil (kill-matching-buffers-no-ask "^*magit: ")))

;;;###autoload
(defun esc/should-have-opened-this-in-other-window (&optional COUNT)
  "Returns to the previous buffer in current window, calls
`other-window', and opens the buffer in the new window.

COUNT is the number of windows to advance; the argument is passed
directly to `other-window', so see the documentation for more
details."
  (interactive)
  (when (equal nil COUNT)
    (setq COUNT 1))
  (let ((target (buffer-name)))
    (switch-to-prev-buffer)
    (other-window COUNT)
    (switch-to-buffer target)))

;;;###autoload
(defun esc/rotate-window-split ()
  "Transform a vertically split window to a horizontally split
window."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;;;###autoload
(defun esc/swap-buffer-locations ()
  "Rotate your windows around and around."
  (interactive)
  (if (not (> (count-windows) 1))
      (error "You can't rotate a single window"))
  (let ((i 1))
    (let ((numWindows (count-windows)))
      (while  (< i numWindows)
	(let* ((w1 (elt (window-list) i))
	       (w2 (elt (window-list) (+ (% i numWindows) 1)))
	       (b1 (window-buffer w1))
	       (b2 (window-buffer w2))
	       (s1 (window-start w1))
	       (s2 (window-start w2)))
	  (set-window-buffer w1  b2)
	  (set-window-buffer w2 b1)
	  (set-window-start w1 s2)
	  (set-window-start w2 s1)
	  (setq i (1+ i)))))))


;;;###autoload
(defun esc/eval-and-replace (&optional arg)
  "Replace the preceding sexp with its value."
  (interactive "P")
  (backward-kill-sexp)
  (let ((expression (current-kill 0)))
    (condition-case nil
	(progn
	  (when arg (insert (concat expression " = ")))
	  (prin1 (eval (read expression))
		 (current-buffer)))
      (error (message "Invalid expression")
	     (insert expression)))))

(provide 'esc-defuns)

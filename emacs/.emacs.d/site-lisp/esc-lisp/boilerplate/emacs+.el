
;;;###autoload
(defun esc/pull-up-line (&optional arg)
  "Pull up ARG lines."
  (interactive "p")
  (dotimes (i arg)
    (join-line -1)))

;;;###autoload
(defun esc/toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
  Toggles between: “all lower”, “Init Caps”, “ALL CAPS”."
  (interactive)
  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word) ) )
        (setq p1 (car bds) p2 (cdr bds)) ) )
    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char p1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]")
          (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]")
          (put this-command 'state "all caps"))
         ((looking-at "[[:upper:]][[:lower:]]")
          (put this-command 'state "init caps"))
         ((looking-at "[[:lower:]]")
          (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]]")
          (put this-command 'state "all caps"))
         (t (put this-command 'state "all lower")))))
    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region p1 p2)
      (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region p1 p2)
      (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region p1 p2)
      (put this-command 'state "all lower")))))

(defun comint-clear-buffer ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

;;;###autoload
(defun esc/dictionary-search ()
  (interactive)
  (require 'dictionary)
  (let ((word (current-word))
        (enable-recursive-minibuffers t)
        (val))
    (setq val (read-from-minibuffer
               (concat "Word"
                       (when word
                         (concat " (" word ")"))
                       ": ")))
    (dictionary-new-search
     (cons (cond
            ((and (equal val "") word)
             word)
            ((> (length val) 0)
             val)
            (t
             (error "No word to lookup")))
           dictionary-default-dictionary))))

;;;###autoload
(defun occur-dwim ()
  "Call `occur' with a sane default.

\\[occur-dwim] will offer as the default candidate:

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
(defun esc/insert-numeric-sequence ()
  "Insert a sequence of numbers at point, separated by spaces. Inclusive."
  (interactive)
  (let ((begin (read-number "Begin: "))
        (end (read-number "End: ")))
    (dotimes (i (- (+ 1 end) begin))
      (insert (number-to-string (+ i begin)))
      (insert " "))))

;;;###autoload
(defun esc/copy-line (&optional arg)
  "Copy current line in the kill ring."
  (interactive "p")
  (dotimes (i arg)
    (kill-ring-save (line-beginning-position)
                    (line-beginning-position 2)))
  (message "Line copied."))

;;;###autoload
(defun esc/remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;;;###autoload
(defun esc/word-count (&optional begin end)
  "Count words bounded by mark and cursor; if no region defined, use buffer."
  (interactive "r")
  (let ((b (if mark-active begin (point-min)))
        (e (if mark-active end (point-max))))
    (message "Word count: %s" (how-many "\\w+" b e))))

;;;###autoload
(defun esc/time ()
  "Insert string for the current esc/time formatted as '2:34 PM'."
  (interactive)
  (insert (format-time-string "%I:%M %p")))

;;;###autoload
(defun esc/date ()
  "Insert string for today's esc/date nicely formatted in American style,
   e.g. Sunday September 17, 2000."
  (interactive)
  (let ((str "%A %B %e, %Y"))
    (if (called-interactively-p 'any)
        (insert (format-time-string str))
      (message (format-time-string str)))))

;;;###autoload
(defun esc/insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%d.%m.%Y")
                 ((equal prefix '(4)) "%Y-%m-%d")
                 ((equal prefix '(16)) "%A, %d. %B %Y")))
        (system-time-locale "de_DE"))
    (insert (format-time-string format))))

;;;###autoload
(defun esc/back-to-indentation-or-beginning ()
  "Returns the point to the beginning of the current line, or if
already there, the beginning of text on the current line."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line)
    (when (eq pt (point))
      (beginning-of-line-text))))

;;;###autoload
(defun esc/increment-number-at-point (arg)
  "Increment the number at point. Can be prefaced with a multiplier."
  (interactive "p")
  (dotimes (i arg)
    (skip-chars-backward "0123456789")
    (or (looking-at "[[:digit:]]+")
        (error "No number at point"))
    (replace-match (number-to-string
                    (1+ (string-to-number (match-string 0)))))))

;;;###autoload
(defun esc/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

;;;###autoload
(defun esc/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name new-name))))))

;;;###autoload
(defun esc/get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE."
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (if (eq mode major-mode)
            (add-to-list 'buffer-mode-matches buf))))
    buffer-mode-matches))

;;;###autoload
(defun esc/multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (esc/get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

;;;###autoload
(defun esc/toggle-selective-display (column)
  "Enable code folding in current buffer."
  (interactive "P")
  (set-selective-display (if selective-display nil (or column 1))))

;;;###autoload
(defun esc/toggle-fullscreen ()
  "Toggles whether the currently selected frame consumes the entire display
   or is decorated with a window border"
  (interactive)
  (let ((f (selected-frame)))
    (modify-frame-parameters f
                             `((fullscreen . ,(if (eq nil (frame-parameter f 'fullscreen))
                                                  'fullboth nil))))))

;;;###autoload
(defun esc/unroll-cc-arguments ()
  "Unroll a function's arguments into a more readable
  one-per-line format. Be sure to invoke this defun from before the
  opening paren of the function's arguments.

  This function has delimeters based on cc-mode dialects, and as a
  result would not be very useful for a language like Lisp."
  (interactive)
  (let ((limit (save-excursion
                 (search-forward "(")
                 (backward-char)
                 (forward-sexp)
                 (point))))
    (save-excursion
      (while (and (< (point) limit)
                  (re-search-forward "[,\"]" limit t))
        (cond ((char-equal ?, (char-before))
               (newline-and-indent))
              ((char-equal ?\" (char-before))
               (re-search-forward "\""))))))
  (message "done"))


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

;; generate programatically
;;;###autoload
(defun esc/open-line-below ()
  "Create a new line above the current line. Can be used with point
   anywhere on the line."
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

;;;###autoload
(defun esc/open-line-above ()
  "Create a new line below the current line. Can be used with point
   anywhere on the line."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

;;;###autoload
(defun esc/goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect (progn
                    (linum-mode 1)
                    (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

;;;###autoload
(defun esc/minimap-toggle ()
  "Toggle minimap for current buffer."
  (interactive)
  (when (require 'minimap nil 'noerror)
    (if (not (boundp 'minimap-bufname))
        (setf minimap-bufname nil))
    (if (null minimap-bufname)
        (minimap-create)
      (minimap-kill))))

;;;###autoload
(defun esc/delete-whole-word ()
  "This defun will delete the entire word at point. This function
relies on `esc/kill-whole-word'."
  (interactive)
  (esc/kill-whole-word t))

;; TODO: accept a prefix arg
;;;###autoload
(defun esc/kill-whole-word (&optional delete)
  "This defun will kill the entire word at point (on both sides
of point). DELETE, if non-nil, will prevent the word from being
appended to the kill-ring."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (save-excursion
      (goto-char (car bounds))
      (if (not delete)
          (kill-word 1)
        (delete-region (car bounds) (cdr bounds))))))

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

;;;###autoload
(defun esc/save-buffers-kill-emacs (&optional arg)
  "Offer to save each buffer(once only), then kill this Emacs process.
   With prefix ARG, silently save all file-visiting buffers, then kill."
  (interactive "P")
  (save-some-buffers arg t)
  (and (or (not (fboundp 'process-list))
           (let ((processes (process-list)) ;process-list is not defined on DOS
                 active)
             (while processes
               (and (memq (process-status (car processes))
                          '(run stop open listen))
                    (process-query-on-exit-flag (car processes))
                    (setq active t))
               (setq processes (cdr processes)))
             (or (not active)
                 (progn (list-processes t)
                        (yes-or-no-p
                         "Active processes exist; kill them and exit anyway? ")))))
       ;; Query the user for other things, perhaps.
       (run-hook-with-args-until-failure 'kill-emacs-query-functions)
       (or (null confirm-kill-emacs)
           (funcall confirm-kill-emacs "Really exit Emacs? "))
       (kill-emacs)))

;;;###autoload
(defun esc/unkillable-scratch-buffer ()
  "Prevent the *scratch* buffer from ever being killed."
  (if (equal (buffer-name (current-buffer)) "*scratch*")
      (progn
        (delete-region (point-min) (point-max))
        (insert initial-scratch-message)
        nil)
    t))

;;;###autoload
(defun esc/mode-line-other-buffer-other-window ()
  "Switch to `other-window', use `mode-line-other-buffer', and
    switch back to the original window."
  (interactive)
  (other-window 1)
  (mode-line-other-buffer)
  (other-window -1))

  ;;;###autoload
  (defun delete-word (arg)
    "Delete characters forward until encountering the end of a word.
  With argument, do this that many times."
    (interactive "p")
    (delete-region (point) (progn (forward-word arg) (point))))

  ;;;###autoload
  (defun backward-delete-word (arg)
    "Delete characters backward until encountering the end of a word.
  With argument, do this that many times."
    (interactive "p")
    (delete-word (- arg)))

(provide 'emacs+)

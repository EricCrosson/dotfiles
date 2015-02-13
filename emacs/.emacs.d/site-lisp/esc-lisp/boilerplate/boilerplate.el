
;; boilerplate library - a home for useful, logically disparate functions
;; Copyright (C) <year>  <name of author>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; Boilerplate Library
;; This library is for the code so straightforward that it doesn't
;; belong in my .emacs. Only tried-and-tested programs make it into
;; this vault.

(defcustom esc/emacs-init-org-path nil
  "The path to esc's init.org configuration file."
  :type         'file
  :options      '("~/.emacs.d/init.org"
                  "~/dotfiles/emacs/.emacs.d/init.org")
  :group        'boil)
(setq esc/emacs-init-org-path "~/dotfiles/emacs/.emacs.d/init.org")

(defcustom esc/bash-org-path nil
  "The path to esc's bash.org configuration file."
  :type         'file
  :options      '("~/dotfiles/bash/bash.org")
  :group        'boil)
(setq esc/bash-org-path "~/dotfiles/bash/bash.org")

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

(defcustom esc/prog-mode-hook nil
    "esc's code to \\[prog-mode-hook]."
    :type 'hook
    :options '((auto-revert-mode t)
               (smerge-mode t))
    :group 'boil)

;;;###autoload
(defun esc/prog-mode-hook ()
 "esc's hook to \\[prog-mode-hook]."
  (auto-revert-mode t)
  (smerge-mode t))

(defcustom esc/minibuffer-setup-hook nil
  "esc's hook to run upon entering the minibuffer."
  :type         'hook
  :options      '(autopair-mode -1)
  :group        'boil)

;;;###autoload
(defun esc/minibuffer-setup-hook ()
  "esc's hook to run whilst entering the minibuffer's domain."
  (autopair-mode -1)
  (define-key minibuffer-local-map (kbd "<escape>") nil))

(defcustom esc/minibuffer-exit-hook nil
  "esc's hook to run upon exiting the minibuffer."
  :type         'hook
  :options      '()
  :group        'boil)

;;;###autoload
(defun esc/minibuffer-exit-hook ()
  "esc's hook to run whilst exiting the minibuffer's domain."
  nil)

(defcustom esc/c-insert-ifdef-ndebug-printf-string nil
  "Debugging string to insert in \\[esc/c-insert-ifdef-ndebug-printf]."
  :type '(string)
  :options '("printf(\"%s \\n\", __FUNCTION__);")
  :group 'boil)
(setq esc/c-insert-ifdef-ndebug-printf-string "printf(\"%s \\n\", __FUNCTION__);")

(defcustom esc/c-mode-common-hook nil
  "esc's code to \\[c-mode-common-hook]."
  :type 'hook
  :options '((setq ff-always-in-other-window t)
             (setq ff-always-try-to-create nil)
             ;(idle-highlight t)
             (autoload 'disaster "disaster")
             (local-set-key (kbd "C-c C-d") 'disaster)
             (local-set-key (kbd "C-c o") 'ff-find-other-file)
             (local-set-key (kbd "C-c d") 'esc/c-insert-ifdef-ndebug-printf))
  :group 'boil)

;;;###autoload
(defun esc/c-mode-common-hook ()
  "esc's code to \\[c-mode-common-hook]."
  ;(idle-highlight t)
  (autoload 'disaster "disaster")
  (local-set-key (kbd "C-c C-d") 'disaster)
  (local-set-key (kbd "C-c o") 'ff-find-other-file)
  (local-set-key (kbd "C-c d") 'esc/c-insert-ifdef-ndebug-printf)
  (local-set-key (kbd "C-M-S-u") 'esc/unroll-cc-arguments)
  (setq ff-always-in-other-window t
        ff-always-try-to-create nil))

(defcustom esc/c++-mode-hook nil
  "esc's code to \\[c++-mode-hook]."
  :type 'hook
  :options '((setq comment-start "/*")
             (setq comment-end "*/"))
  :group 'boil)

;;;###autoload
(defun esc/c++-mode-hook ()
  "esc's code to \\[c++-mode-hook]."
  (setq comment-start "/*"
        comment-end "*/"))

(defcustom esc/emacs-lisp-mode-hook nil
  "esc's code to \\[emacs-lisp-mode-hook]."
  :type         'hook
  :options      '((setq mode-name "elisp")
                  ;(idle-highlight t)
                  )
  :group        'boil)

;;;###autoload
(defun esc/emacs-lisp-mode-hook ()
  "esc's code to \\[emacs-lisp-mode-hook]."
  (setq mode-name "elisp")
  ;(idle-highlight t)
  )

(defcustom esc/fundamental-mode-hook nil
  "esc's code to \\[fundamental-mode-hook]."
  :type         'hook
  :options      '((autopair-mode -1)
                  (flyspell-mode 1))
  :group        'boil)

;;;###autoload
(defun esc/fundamental-mode-hook ()
  "esc's code to \\[fundamental-mode-hook]."
  (autopair-mode -1)
  (flyspell-mode 1))

(defcustom esc/enh-ruby-mode-hook nil
  "esc's code to \\[enh-ruby-mode-hook]."
  :type         'hook
  :options      '((idle-highlight t))
  :group        'boil)

;;;###autoload
(defun esc/enh-ruby-mode-hook ()
  "esc's code to \\[enh-ruby-mode-hook]."
  ;(idle-highlight t)
  )

(defcustom esc/erc-mode-hook nil
  "esc's code to \\[erc-mode-hook]."
  :type         'hook
  :options      '((autopair-mode -1))
  :group        'boil)

;;;###autoload
(defun esc/erc-mode-hook ()
  "esc's code to \\[erc-mode-hook]."
  (autopair-mode -1))

(defcustom esc/comint-mode-hook nil
  "esc's code to \\[comint-mode-hook]."
  :type         'hook
  :options      '((define-key comint-mode-map "M-p" 'comint-previous-input)
                  (define-key comint-mode-map "M-S-p" 'ace-window))
  :group        'boil)

;;;###autoload
(defun esc/comint-mode-hook ()
  "esc's code to \\[comint-mode-hook]."
  ;; TODO: find the active keymap in comint-mode
  (define-key comint-mode-map "M-p" 'comint-previous-input)
  (define-key comint-mode-map "M-S-p" 'ace-window))

;;;###autoload
(defun esc/iedit-mode-hook ()
  "esc's hook to \\[iedit-mode-hook]."
  (define-key iedit-mode-keymap (kbd "<RET>") 'iedit-mode) ;exit
  ;; Don't go overriding my M-x chord, you hear
  (define-key iedit-mode-keymap (kbd "C-'") 'execute-extended-command))

;;;###autoload
(defun esc/insert-short-gpl ()
  "Insert the short version of the GNU GPL v3."
  (interactive)
  (insert
   "<one line to give the program's name and a brief idea of what it does.>
Copyright (C) <year>  <name of author>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <http://www.gnu.org/licenses/>.
"))

;;;###autoload
(defun esc/javadoc-method-comment ()
  "Insert a javadoc method comment at point."
  (interactive)
  (insert
   "/**
 *
 *
 *
 *
 * @param
 * @return
 * @exeption
 * @see
 */
")
  (previous-line 8)
  (end-of-line))

;;;###autoload
(defmacro esc/yank-and-move (ins back)
  "This macro is a helper function to the keys in `esc-mode-map' that
insert duplicates of characters."
  `(progn
     (dotimes (i ARG)
       (insert ,ins)
       (backward-char ,back))))

;; TODO: redefine as defun
  ;;;###autoload
  (fset 'bang-word-at-point "$\C-s \C-m\C-b$")


(let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (message (concat "Filename is : " (file-name-sans-extension filename)))))



  ;; TODO: create a macro to reduce boilerplate
  ;;;###autoload
  (defun esc/insert-surrounding-ticks (&optional ARG)
    "Insert a backtick and fronttick at point. This command can be
  prefixed with a numerical argument."
    (interactive "p") (esc/yank-and-move "`'" 1))

  ;;;###autoload
  (defun esc/insert-little-arrow (&optional ARG)
    "Insert an arrow at point. This command can be prefixed with a
    numeric argument."
    (interactive "p") (esc/yank-and-move "->" 0))

  ;;;###autoload
  (defun esc/insert-surrounding-parens (&optional ARG)
    "Insert parenthesis, and center point between them. This command
    can be prefixed with a numeric argument."
    (interactive "p") (esc/yank-and-move "()" 1))

  ;;;###autoload
  (defun esc/insert-surrounding-braces (&optional ARG)
    "Insert a pair of braces, and center point between them. This
    command can be prefixed with a numeric argument."
    (interactive "p") (esc/yank-and-move "{}" 1))

  ;;;###autoload
  (defun esc/insert-surrounding-pipes (&optional ARG)
    "Insert a pair of pipes, and center point between them. This
    command can be prefixed with a numeric argument."
    (interactive "p") (esc/yank-and-move "||" 1))

  ;;;###autoload
  (defun esc/insert-surrounding-brackets (&optional ARG)
    "Insert a pair of square brackets, and center point between
    them. This command can be prefixed with a numeric argument."
    (interactive "p") (esc/yank-and-move "[]" 1))

  ;;;###autoload
  (defun esc/insert-surrounding-brackets-with-colon (&optional ARG)
    "Insert a pair of square brackets around a colon, and center
    point between them (after the colon). This command can be
    prefixed with a numeric argument."
    (interactive "p") (esc/yank-and-move "[:]" 1))

  ;;;###autoload
  (defun esc/insert-surrounding-chevrons (&optional ARG)
    "Insert a pair of chevrons, and center point between them. This
    command can be prefixed with a numeric argument."
    (interactive "p") (esc/yank-and-move "<>" 1))

  ;;;###autoload
  (defun esc/insert-surrounding-quotes (&optional ARG)
    "Insert double quotes, and center point between them. This command
    can be prefixed with a numeric argument."
    (interactive "p") (esc/yank-and-move "\"\"" 1))

  ;;;###autoload
  (defun esc/insert-surrounding-stars (&optional ARG)
    "Insert dual stars, and center point between them. This command
    can be prefixed with a numeric argument."
    (interactive "p") (esc/yank-and-move "**" 1))

  ;;;###autoload
  (defun esc/insert-surrounding-dollars (&optional ARG)
    "Insert dual stars, and center point between them. This command
    can be prefixed with a numeric argument."
    (interactive "p") (esc/yank-and-move "$$" 1))

;;;###autoload
(defun esc/middle-click-yank ()
  "Yank from the middle click mouse buffer."
  (interactive)
  (mouse-yank-primary 1))

;;;###autoload
(defun esc/insert-file-name (&optional ARG)
  "Inserts the name of the current file (including extension) at point.

When ARG is non-nil, the filename will be printed in a different format.
If ARG is 0, insert the full path of the filename.
If ARG is - (or -1), insert the filename without extension."
  (interactive "p")
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (let ((output (cond ((eq ARG 0)   filename)
                          ((eq ARG -1)  (file-name-nondirectory
                                         (file-name-sans-extension filename)))
                          (t (file-name-nondirectory filename)))))
        (insert output)))))

;;;###autoload
(defun esc/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;;;###autoload
(defun esc/magic-autoload-next-defun-or-macro ()
  "Prefix the next defun or macro after point with a line
identifying the sexp as a magically autoloaded cookie."
  (interactive)
  (let ((autoloadable "(def[um][na]c?r?o?"))
    (when (re-search-forward autoloadable)
      (search-backward "(")
      (split-line)
      (insert ";;;###autoload")
      ;; move point past this match
      (re-search-forward autoloadable))))

;;;###autoload
(defun esc/mode-line-other-buffer-other-window ()
  "Switch to `other-window', use `mode-line-other-buffer', and
switch back to the original window."
  (interactive)
  (other-window 1)
  (mode-line-other-buffer)
  (other-window -1))

;; TODO also check that the register to restore the window
;; configuration to exists and has content before attempting to
;; restore. Maybe this todo belongs somewhere else
;;;###autoload
(defun esc/edit-my-emacs()
  (interactive)
  (esc/toggle-fullscreen-buffer
    :edit-my-config
    (and (string= "org-mode" major-mode)
         (string= ".emacs.org" (file-name-nondirectory (buffer-file-name))))
    (progn (find-file-existing esc/emacs-init-org-path)
           (helm-imenu)
           (org-cycle))))

;;;###autoload
(defun esc/edit-my-bash()
  (interactive)
  (esc/toggle-fullscreen-buffer
    :edit-my-config
    (and (string= "org-mode" major-mode)
         (string= "bash.org" (file-name-nondirectory (buffer-file-name))))
    (progn (find-file-existing esc/bash-org-path)
           (helm-imenu)
           (org-cycle))))

;;;###autoload
(defun esc/delete-whole-word ()
  "This defun will delete the entire word at point. This function
relies on `esc/kill-whole-word'."
  (interactive)
  (esc/kill-whole-word t))

;; TODO: accept a prefix arg
;;;###autoload
(defun esc/kill-whole-word (&optional delete)
  "This defun will kill the entire word at point (on both sides of
point).

DELETE, if non-nil, will prevent the word from being appended to the
kill-ring."
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

;; TODO make this insert parens, and the appropriate spaces for
;; arguments. I'm envisioning a clean, cdlatex-mode type thing
;;;###autoload
(defun esc/insert-defun-at-point (&optional key insert untranslated string)
  "Insert at point the name of the function KEY invokes.  KEY is a string.
If INSERT (the prefix arg) is non-nil, insert the message in the
buffer.  If non-nil, UNTRANSLATED is a vector of the untranslated
events.  It can also be a number in which case the untranslated
events from the last key hit are used.

If KEY is a menu item or a tool-bar button that is disabled, this command
temporarily enables it to allow getting help on disabled items and buttons."
  (interactive
   (let ((enable-disabled-menus-and-buttons t)
         (cursor-in-echo-area t)
         saved-yank-menu)
     (unwind-protect
         (let (key)
           ;; If yank-menu is empty, populate it temporarily, so that
           ;; "Select and Paste" menu can generate a complete event.
           (when (null (cdr yank-menu))
             (setq saved-yank-menu (copy-sequence yank-menu))
             (menu-bar-update-yank-menu "(any string)" nil))
           (setq key (read-key-sequence "Describe key (or click or menu item): "))
           ;; Clear the echo area message (Bug#7014).
           (message nil)
           ;; If KEY is a down-event, read and discard the
           ;; corresponding up-event.  Note that there are also
           ;; down-events on scroll bars and mode lines: the actual
           ;; event then is in the second element of the vector.
           (and (vectorp key)
                (let ((last-idx (1- (length key))))
                  (and (eventp (aref key last-idx))
                       (memq 'down (event-modifiers (aref key last-idx)))))
                (read-event))
           (list
            key
            (if current-prefix-arg (prefix-numeric-value current-prefix-arg))
            1))
       ;; Put yank-menu back as it was, if we changed it.
       (when saved-yank-menu
         (setq yank-menu (copy-sequence saved-yank-menu))
         (fset 'yank-menu (cons 'keymap yank-menu))))))

  (if (numberp untranslated)
      (setq untranslated (this-single-command-raw-keys)))
  (let* ((event (if (and (symbolp (aref key 0))
                         (> (length key) 1)
                         (consp (aref key 1)))
                    (aref key 1)
                  (aref key 0)))
         (modifiers (event-modifiers event))
         (standard-output (if insert (current-buffer) standard-output))
         (mouse-msg (if (or (memq 'click modifiers) (memq 'down modifiers)
                            (memq 'drag modifiers)) " at that spot" ""))
         (defn (key-binding key t))
         key-desc)
    ;; Handle the case where we faked an entry in "Select and Paste" menu.
    (if (and (eq defn nil)
             (stringp (aref key (1- (length key))))
             (eq (key-binding (substring key 0 -1)) 'yank-menu))
        (setq defn 'menu-bar-select-yank))
    ;; Don't bother user with strings from (e.g.) the select-paste menu.
    (if (stringp (aref key (1- (length key))))
        (aset key (1- (length key)) "(any string)"))
    (if (and (> (length untranslated) 0)
             (stringp (aref untranslated (1- (length untranslated)))))
        (aset untranslated (1- (length untranslated)) "(any string)"))
    ;; Now describe the key, perhaps as changed.
    (setq key-desc (help-key-description key untranslated))
    (if (or (null defn) (integerp defn) (equal defn 'undefined))
        (princ (format "%s%s is undefined" key-desc mouse-msg))
      (if string
          defn
        (insert (format "%S" defn))))))

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
(defun esc/display-emacs-init-time ()   ;only to be run once- non-interactive
  (message ".emacs loaded in %s" (emacs-init-time)))

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
(defun esc/zoom-in ()
  "Increase font size by 10 points"
  (interactive)
  (set-face-attribute 'default nil
                      :height (+ (face-attribute 'default :height) 10)))

;;;###autoload
(defun esc/zoom-out ()
  "Decrease font size by 10 points"
  (interactive)
  (set-face-attribute 'default nil
                      :height (- (face-attribute 'default :height) 10)))

;;;###autoload
(defun esc/proced-in-this-frame ()
  "Open proced in the current window."
  (interactive)
  (proced)
  (previous-multiframe-window)
  (delete-window))

;;;###autoload
(defun esc/edit-configs ()
  "Open a dired buffer in my root .emacs.d for quick edits."
  (interactive)
  (dired esc-lisp-path))

;; TODO: move to esc-mode
  ;;;###autoload
(defun esc/scroll-up-slight(&optional arg)
  "Scroll up the page without moving point. The number of lines to
scroll is determined by the variable `esc-line-shift-amount'.

This command can be prefixed."
  (interactive "p")
  (dotimes (i arg)
    (scroll-up esc-line-shift-amount)))

;;;###autoload
(defun esc/scroll-down-slight (&optional arg)
  "Scroll down the page without moving point. The number of lines to
scroll is determined by the variable `esc-line-shift-amount'.

This command can be prefixed."
  (interactive "p")
  (dotimes (i arg)
    (scroll-down esc-line-shift-amount)))

;;;###autoload
(defun esc/backward-kill-line (&optional arg)
  "Kill up to `beginning-of-line', and kill ARG lines above point.
Everything will be appended to the kill ring."
  (interactive "p")
  (dotimes (i arg)
    (if (not (eq (point) (line-beginning-position)))
        (kill-line 0)
      (if (eq i 1)
          (append-next-kill))
      (kill-line -1)))
  (indent-according-to-mode))

;;;###autoload
(defun esc/pull-up-line (&optional arg)         ;thanks magnar
  "Pull up ARG lines."
  (interactive "p")
  (dotimes (i arg)
    (join-line -1)))

;;;###autoload
(defun describe-keymap (keymap)
  "Describe bindings in KEYMAP, a variable whose value is a keymap.
Completion is available for the keymap name."
  (interactive
   (list (intern
          (completing-read
           "Keymap: " obarray
           (lambda (m) (and (boundp m) (keymapp (symbol-value m))))
           t nil 'variable-name-history))))
  (unless (and (symbolp keymap) (boundp keymap) (keymapp (symbol-value keymap)))
    (error "`%S' is not a keymapp" keymap))
  (let ((name  (symbol-name keymap))
        (doc   (documentation-property keymap 'variable-documentation)))
    (help-setup-xref (list #'describe-keymap keymap) (interactive-p))
    (with-output-to-temp-buffer "*Help*"
      (princ name) (terpri)
      (princ (make-string (length name) ?-)) (terpri) (terpri)
      (when doc (princ doc) (terpri) (terpri))
      ;; Use `insert' instead of `princ', so control chars (e.g. \377) insert correctly.
      (with-current-buffer "*Help*"
        (insert (substitute-command-keys (concat "\\{" name "}")))))))



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
(defun rename-defun (function)
  "Rename FUNCTION to a new name. Makfunbound FUNCTION after
evaling the newly named defun."
  (interactive (find-function-read))
  (if (null function)
      (message "You didn't specify a function")
    (let ((function-name (symbol-name function)))
      (find-function-do-it function nil 'switch-to-buffer)
      (let ((begin (point))
            (end (save-excursion (end-of-defun) (point)))
            (new-name (read-string (concat "Rename " function-name " to: "))))
        (save-excursion
          (replace-string function-name new-name t begin end))
        (eval-defun nil)
        (makfunbound function)))))

;;;###autoload
(defun touch ()
  "Updates mtime on the file described by the current buffer."
  (interactive)
  (shell-command (concat "touch " (shell-quote-argument (buffer-file-name))))
  (clear-visited-file-modtime)
  (message "File touched"))

;;;###autoload
(defun esc/insert-key-combination (key &optional arg)
  "Insert string describing KEY sequence. KEY is a string.
If ARG is non nil, wrap the inserted string in some useful text
depending on the value of ARG.

    Value of ARG      Example inserted string
    positive          \"C-h c\"
    negative          (kbd \"C-h c\")"
  (interactive "kType a key combination: \np")
  (let ((str (key-description key)))
    (insert (cond ((< arg 0)        (concat "(kbd \"" str "\")"))
                  ((not (eq arg 1)) (concat "\"" str "\""))
                  (t                 str)))))

;;;###autoload
(defun esc/org-next-source-code-block ()
  (interactive)
  (re-search-forward "#\\+begin_src" nil t)
  (next-line)
  (beginning-of-line))

;;;###autoload
(defun esc/org-prev-source-code-block ()
  (interactive)
  (previous-line)
  (beginning-of-line)
  (re-search-backward "#\\+begin_src" nil t))

;;;###autoload
(defun esc/bury-buffer-delete-window ()
  "Kill current window and bury the current buffer to the bottom of the buffer list."
  (interactive)
  (bury-buffer)
  (delete-window))

;;;###autoload
(defun esc/bury-other-buffer (&optional switch-to-other-buffer)
  "Bury the buffer that \\[mode-line-other-buffer] will take you
to.

If SWITCH-TO-OTHER-BUFFER is non-nil (e.g. this command is
prefixed) then after the other-buffer is buried, the command
\\[mode-line-other-buffer] will be used to switch buffers."
  (interactive "p")
  (mode-line-other-buffer)
  (bury-buffer)
  (when current-prefix-arg (mode-line-other-buffer)))

;; I don't really use ansi-term at this time.
;;;###autoload
(defun esc/raise-ansi-term ()
  "If the current buffer is:

   1) a running ansi-term named *ansi-term*, rename it.
   2) a stopped ansi-term, kill it and create a new one.
   3) a non ansi-term, go to an already running ansi-term
      or start a new one while killing a defunct one."
  (interactive)
  (when (require 'term nil 'noerror)
    (define-key esc-mode-map (kbd "s-s") 'esc/raise-ansi-term )
    (let ((is-term (string= "term-mode" major-mode))
          (is-running (term-check-proc (buffer-name)))
          (term-cmd "/bin/bash")
          (anon-term (get-buffer "*ansi-term*")))
      (if is-term
          (if is-running
              (if (string= "*ansi-term*" (buffer-name))
                  (call-interactively 'rename-buffer)
                (if anon-term
                    (switch-to-buffer "*ansi-term*")
                  (ansi-term term-cmd)))
            (kill-buffer (buffer-name))
            (ansi-term term-cmd))
        (if anon-term
            (if (term-check-proc "*ansi-term*")
                (switch-to-buffer "*ansi-term*")
              (kill-buffer "*ansi-term*")
              (ansi-term term-cmd))
          (ansi-term term-cmd))))))

;;;###autoload
(defun esc/raise-eshell ()
  "Bring up a full-screen eshell or restore previous window
configuration."
  (interactive)
  (esc/toggle-fullscreen-buffer :eshell-fullscreen
    (string= "eshell-mode" major-mode)
    (eshell)))

;;;###autoload
(defun esc/raise-eshell-in-current-dir ()
  "Bring up a full-screen eshell in the current directory or
restore previous window configuration."
  (interactive)
  (esc/toggle-fullscreen-buffer
    :eshell-fullscreen
    (string= "eshell-mode" major-mode)
    (when (buffer-file-name)
      (let ((dir (file-name-directory (buffer-file-name))))
        (eshell)
        (insert "cd " dir)
        (with-no-warnings (eshell-send-input))))))

;; I don't use magit-status at this time since it seems to be in a
;; state of flux.
;;;###autoload
(defun esc/raise-magit-status ()
  "Bring up a full-screen magit-status or restore previous
window configuration."
  (interactive)
  (esc/toggle-fullscreen-buffer
    :magit-fullscreen
    (string= "magit-status-mode" major-mode)
    (progn (if (not (buffer-file-name))
               (message "Buffer not associated with a file")
             (save-buffer)
             (magit-status (file-name-directory (buffer-file-name)))))))

;;;###autoload
(defun esc/vsplit-last-buffer (prefix)
  "Split the window vertically and display the previous buffer."
  (interactive "p")
  (split-window-vertically)
  (other-window 1 nil)
  (unless prefix (switch-to-next-buffer)))

;;;###autoload
(defun esc/hsplit-last-buffer (prefix)
  "Split the window horizontally and display the previous buffer."
  (interactive "p")
  (split-window-horizontally)
  (other-window 1 nil)
  (unless prefix (switch-to-next-buffer)))

(defun esc/search-word-backward ()
  "Find the previous occurrence of the current word."
  (interactive)
  (let ((cur (point)))
    (skip-syntax-backward "w_")
    (goto-char
     (if (re-search-backward (concat "\\_<" (current-word) "\\_>") nil t)
         (match-beginning 0)
       cur))))

(defun esc/search-word-forward ()
  "Find the next occurrence of the current word."
  (interactive)
  (let ((cur (point)))
    (skip-syntax-forward "w_")
    (goto-char
     (if (re-search-forward (concat "\\_<" (current-word) "\\_>") nil t)
         (match-beginning 0)
       cur))))

 ;; This was included too- haven't determined the utility of it yet
 ;; (defadvice search-for-keyword (around esc/search activate)
 ;;   "Match in a case-insensitive way."
 ;;   (let ((case-fold-search t))
 ;;     ad-do-it))

;;;###autoload
(defun esc/should-have-opened-this-in-other-window (&optional COUNT)
  "Returns to the previous buffer in current window, calls
  \\[other-window], and opens the buffer in the new window.

  COUNT is the number of windows to advance; the argument is
  passed directly to \\[other-window], so see the documentation
  for more details."
  (interactive)
  (when (equal nil COUNT)
    (setq COUNT 1))
  (let ((target (buffer-name)))
    (switch-to-prev-buffer)
    (other-window COUNT)
    (switch-to-buffer target)))

;;;###autoload
(defun esc/toggle-window-selectability ()
  "Ignore/recognize a window from `other-window'."
  (interactive)
  (if (window-parameter (selected-window) 'no-other-window)
      (progn
        (set-window-parameter (selected-window) 'no-other-window nil)
        (message "Window will now be recognized by `other-window'"))
    (set-window-parameter (selected-window) 'no-other-window t)
    (message "Window will now be ignored by `other-window'")))

;;;###autoload
(defun esc/toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not.

When dedicated, `display-buffer' will refrain from displaying
another buffer in a window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

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
(defun esc/previous-error (n)
  "Visit previous compilation error message and corresponding source code.
Complement to \\[next-error]."
  (interactive "p")
  (next-error (- n)))

;;;###autoload
(defun esc/auto-byte-recompile ()
  "If the current buffer is in emacs-lisp-mode and there already exists an .elc
file corresponding to the current buffer file, then recompile the file."
  (interactive)
  (when (require 'bytecomp nil 'noerror)
    (when (and (eq major-mode 'emacs-lisp-mode)
               (file-exists-p (byte-compile-dest-file buffer-file-name)))
      (byte-compile-file buffer-file-name))))

;;;###autoload
(defun esc/bury-compilation-buffer-if-successful (buffer string)
  "Bury the compilation buffer after a successful compile."
  (when (and
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         (not (search-forward "warning" nil t)))
    (bury-buffer buffer)
    (switch-to-prev-buffer (get-buffer-window buffer) 'kill)
    ;; TODO: winner-undo if compile created a new buffer
    (message "Compilation successful.")))

(defun esc/c-insert-ifdef-ndebug-printf ()
  "Insert a printf template wrapped in #ifdefs for easy debugging."
  (interactive)
  (beginning-of-line)
  (insert (concat "#ifndef NDEBUG\n  "
                  esc/c-insert-ifdef-ndebug-printf-string
                  "\n#endif\n"))
  (search-backward " \\")
  (forward-char 1))

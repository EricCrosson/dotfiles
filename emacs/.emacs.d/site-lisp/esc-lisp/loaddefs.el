;;; loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "boilerplate/boilerplate" "../../../../../.emacs.d/site-lisp/esc-lisp/boilerplate/boilerplate.el"
;;;;;;  (21741 17535 334700 67000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/boilerplate/boilerplate.el

(autoload 'esc/prog-mode-hook "boilerplate/boilerplate" "\
esc's hook to \\[prog-mode-hook].

\(fn)" nil nil)

(autoload 'esc/minibuffer-setup-hook "boilerplate/boilerplate" "\
esc's hook to run whilst entering the minibuffer's domain.

\(fn)" nil nil)

(autoload 'esc/minibuffer-exit-hook "boilerplate/boilerplate" "\
esc's hook to run whilst exiting the minibuffer's domain.

\(fn)" nil nil)

(autoload 'esc/c-mode-common-hook "boilerplate/boilerplate" "\
esc's code to \\[c-mode-common-hook].

\(fn)" nil nil)

(autoload 'esc/c++-mode-hook "boilerplate/boilerplate" "\
esc's code to \\[c++-mode-hook].

\(fn)" nil nil)

(autoload 'esc/emacs-lisp-mode-hook "boilerplate/boilerplate" "\
esc's code to \\[emacs-lisp-mode-hook].

\(fn)" nil nil)

(autoload 'esc/fundamental-mode-hook "boilerplate/boilerplate" "\
esc's code to \\[fundamental-mode-hook].

\(fn)" nil nil)

(autoload 'esc/enh-ruby-mode-hook "boilerplate/boilerplate" "\
esc's code to \\[enh-ruby-mode-hook].

\(fn)" nil nil)

(autoload 'esc/erc-mode-hook "boilerplate/boilerplate" "\
esc's code to \\[erc-mode-hook].

\(fn)" nil nil)

(autoload 'esc/comint-mode-hook "boilerplate/boilerplate" "\
esc's code to \\[comint-mode-hook].

\(fn)" nil nil)

(autoload 'esc/iedit-mode-hook "boilerplate/boilerplate" "\
esc's hook to \\[iedit-mode-hook].

\(fn)" nil nil)

(autoload 'esc/org-mode-hook "boilerplate/boilerplate" "\


\(fn)" nil nil)

(autoload 'esc/big-fringe-mode-hook "boilerplate/boilerplate" "\


\(fn)" nil nil)

(autoload 'esc/insert-short-gpl "boilerplate/boilerplate" "\
Insert the short version of the GNU GPL v3.

\(fn)" t nil)

(autoload 'rename-defun "boilerplate/boilerplate" "\
Rename loaded FUNCTION to a new name. Makfunbound FUNCTION after
evaling the newly named defun.

\(fn FUNCTION)" t nil)

(autoload 'touch "boilerplate/boilerplate" "\
Updates mtime on the file described by the current buffer.

\(fn)" t nil)

(autoload 'esc/insert-key-combination "boilerplate/boilerplate" "\
Insert string describing KEY sequence. KEY is a string.
If ARG is non nil, wrap the inserted string in some useful text
depending on the value of ARG.

    Value of ARG      Example inserted string
    positive          \"C-h c\"
    negative          (kbd \"C-h c\")

\(fn KEY &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "boilerplate/buffer+" "../../../../../.emacs.d/site-lisp/esc-lisp/boilerplate/buffer+.el"
;;;;;;  (21741 17534 721366 769000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/boilerplate/buffer+.el

(autoload 'esc/indent-buffer "boilerplate/buffer+" "\
Indent the entire buffer without adjusting point or mark.

\(fn)" t nil)

(autoload 'kill-matching-buffers-no-ask "boilerplate/buffer+" "\
Kill buffers whose name matches the specified REGEXP.
The optional second argument indicates whether to kill internal buffers too.

\(fn REGEXP &optional INTERNAL-TOO)" t nil)

(autoload 'esc/toggle-fullscreen-buffer "boilerplate/buffer+" "\
Bring up a temporary buffer in fullscreen mode, or restore the
previous window configuration.

WIN-REGISTER         is the register to store the old window configuration in.

TOGGLED-MODE-TEST    is the major mode of the toggled state, in other words a
                     test to determine which way to toggle the buffers.

TOGGLE-COMMAND       is the command to run when toggling into the temporary
                     state.

CLEAR-COMMAND        is an optional command to run when reverting back to the
                     original state; i.e. toggle a flag

\(fn WIN-REGISTER TOGGLED-MODE-TEST TOGGLE-COMMAND &optional TOGGLE-COMMAND-TEST CLEAR-COMMAND)" nil t)

(put 'esc/toggle-fullscreen-buffer 'lisp-indent-function 'defun)

(autoload 'esc/save-window-configuration "boilerplate/buffer+" "\
Save current window configuration to WIN-REGISTER, run BODY,
and restory WIN-REGISTER.

\(fn WIN-REGISTER &rest BODY)" nil t)

(put 'esc/save-window-configuration 'lisp-indent-function 'defun)

(autoload 'esc/should-have-opened-this-in-other-window "boilerplate/buffer+" "\
Returns to the previous buffer in current window, calls
`other-window', and opens the buffer in the new window.

COUNT is the number of windows to advance; the argument is passed
directly to `other-window', so see the documentation for more
details.

\(fn &optional COUNT)" t nil)

(autoload 'esc/toggle-window-selectability "boilerplate/buffer+" "\
Ignore/recognize a window from `other-window'.

\(fn)" t nil)

(autoload 'esc/toggle-window-dedicated "boilerplate/buffer+" "\
Toggle whether the current active window is dedicated or not.

When dedicated, `display-buffer' will refrain from displaying
another buffer in a window.

\(fn)" t nil)

(autoload 'buffers-matching-regexp "boilerplate/buffer+" "\
Return a list of buffers matching REGEXP.

If NAMES is non-nil, return a list of names (strings) instead of
buffers.

\(fn REGEXP &optional NAMES)" nil nil)

(autoload 'esc/rotate-window-split "boilerplate/buffer+" "\
Transform a vertically split window to a horizontally split
window.

\(fn)" t nil)

(autoload 'esc/bury-buffer-delete-window "boilerplate/buffer+" "\
Kill current window and bury the current buffer to the bottom
of the buffer list.

\(fn)" t nil)

(autoload 'esc/bury-other-buffer "boilerplate/buffer+" "\
Bury the buffer that `mode-line-other-buffer' will take you
to.

If SWITCH-TO-OTHER-BUFFER is non-nil (e.g. this command is
prefixed) then after the other-buffer is buried, the command
`mode-line-other-buffer' will be used to switch buffers.

\(fn &optional SWITCH-TO-OTHER-BUFFER)" t nil)

(autoload 'esc/raise-eshell "boilerplate/buffer+" "\
Bring up a full-screen eshell or restore previous window
configuration.

\(fn)" t nil)

(autoload 'esc/raise-eshell-in-current-dir "boilerplate/buffer+" "\
Bring up a full-screen eshell in the current directory or
restore previous window configuration.

\(fn)" t nil)

(autoload 'esc/raise-magit-status "boilerplate/buffer+" "\
Bring up a full-screen magit-status or restore previous
window configuration.

This defun will not raise magit if you have merge conflicts in
the current buffer.

This defun kills all buffers matching regexp '^*magit: ' upon the
exit toggle of the fullscreen magit buffer

\(fn)" t nil)

(autoload 'esc/vsplit-last-buffer "boilerplate/buffer+" "\
Split the window vertically and display the previous buffer.
Argument PREFIX when nil switches the new buffer to the last buffer.

\(fn PREFIX)" t nil)

(autoload 'esc/hsplit-last-buffer "boilerplate/buffer+" "\
Split the window horizontally and display the previous buffer.
Argument PREFIX when nil switches the new buffer to the last buffer.

\(fn PREFIX)" t nil)

;;;***

;;;### (autoloads nil "boilerplate/compilation+" "../../../../../.emacs.d/site-lisp/esc-lisp/boilerplate/compilation+.el"
;;;;;;  (21741 17534 761366 767000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/boilerplate/compilation+.el

(autoload 'esc/previous-error "boilerplate/compilation+" "\
Visit previous compilation error message and corresponding source code.
  Complement to `next-error'.

\(fn N)" t nil)

(autoload 'esc/auto-byte-recompile "boilerplate/compilation+" "\
If the current buffer is in emacs-lisp-mode and there already exists an .elc
file corresponding to the current buffer file, then recompile the file.

\(fn)" t nil)

(autoload 'esc/bury-compilation-buffer-if-successful "boilerplate/compilation+" "\
Bury the compilation BUFFER after a successful compile.
Argument STRING provided by compilation hooks.

\(fn BUFFER STRING)" nil nil)

(autoload 'esc/org-next-source-code-block "boilerplate/compilation+" "\


\(fn)" t nil)

(autoload 'esc/org-prev-source-code-block "boilerplate/compilation+" "\


\(fn)" t nil)

(autoload 'byte-compile-directory "boilerplate/compilation+" "\
Compile an .elc file for every .el file contained under
  DIR (recursive).

\(fn DIR)" nil nil)

;;;***

;;;### (autoloads nil "boilerplate/displaced-yank" "../../../../../.emacs.d/site-lisp/esc-lisp/boilerplate/displaced-yank.el"
;;;;;;  (21741 17534 794700 98000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/boilerplate/displaced-yank.el

(autoload 'esc/define-displaced-yank "boilerplate/displaced-yank" "\
Create a defun of name FUNCNAME that yanks and moves according
to DATA. DATA is of the form (STR, MOVE). STR is the string to
yank and MOVE is the number of chars to move backward.

Note that negative values of MOVE are valid.

\(fn FUNCNAME DATA)" nil t)

;;;***

;;;### (autoloads nil "boilerplate/emacs+" "../../../../../.emacs.d/site-lisp/esc-lisp/boilerplate/emacs+.el"
;;;;;;  (21741 17535 64700 82000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/boilerplate/emacs+.el

(autoload 'esc/minibuffer-display-buffer-file-name "boilerplate/emacs+" "\
Display `buffer-file-name' in the minibuffer.

\(fn)" t nil)

(autoload 'combinations "boilerplate/emacs+" "\
Return a list of all possible combinations of the elements of
LISTS. Think applicative functors from Haskell.

\(fn &rest LISTS)" nil nil)

(autoload 'esc/pull-up-line "boilerplate/emacs+" "\
Pull up ARG lines.

\(fn &optional ARG)" t nil)

(autoload 'esc/toggle-letter-case "boilerplate/emacs+" "\
Toggle the letter case of current word or text selection.
  Toggles between: “all lower”, “Init Caps”, “ALL CAPS”.

\(fn)" t nil)

(autoload 'sudo-edit-current-file "boilerplate/emacs+" "\
Edit the current file as sudo with tramp, without needing to
navigate to it or losing your place in the file. Works on local
or remote files.

\(fn)" t nil)

(autoload 'esc/dictionary-search "boilerplate/emacs+" "\


\(fn)" t nil)

(autoload 'occur-dwim "boilerplate/emacs+" "\
Call `occur' with a sane default.

\\[occur-dwim] will offer as the default candidate:

- the current region, if it's active
- the current symbol, otherwise

\(fn)" t nil)

(autoload 'esc/insert-numeric-sequence "boilerplate/emacs+" "\
Insert a sequence of numbers at point, separated by spaces. Inclusive.

\(fn)" t nil)

(autoload 'esc/copy-line "boilerplate/emacs+" "\
Copy current line in the kill ring.

\(fn &optional ARG)" t nil)

(autoload 'esc/remove-dos-eol "boilerplate/emacs+" "\
Do not show ^M in files containing mixed UNIX and DOS line endings.

\(fn)" t nil)

(autoload 'esc/word-count "boilerplate/emacs+" "\
Count words bounded by mark and cursor; if no region defined, use buffer.

\(fn &optional BEGIN END)" t nil)

(autoload 'esc/time "boilerplate/emacs+" "\
Insert string for the current esc/time formatted as '2:34 PM'.

\(fn)" t nil)

(autoload 'esc/date "boilerplate/emacs+" "\
Insert string for today's esc/date nicely formatted in American style,
   e.g. Sunday September 17, 2000.

\(fn)" t nil)

(autoload 'esc/insert-date "boilerplate/emacs+" "\
Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name.

\(fn PREFIX)" t nil)

(autoload 'esc/back-to-indentation-or-beginning "boilerplate/emacs+" "\
Returns the point to the beginning of the current line, or if
already there, the beginning of text on the current line.

\(fn)" t nil)

(autoload 'esc/increment-number-at-point "boilerplate/emacs+" "\
Increment the number at point. Can be prefaced with a multiplier.

\(fn ARG)" t nil)

(autoload 'esc/delete-current-buffer-file "boilerplate/emacs+" "\
Removes file connected to current buffer and kills buffer.

\(fn)" t nil)

(autoload 'esc/rename-current-buffer-file "boilerplate/emacs+" "\
Renames current buffer and file it is visiting.

\(fn)" t nil)

(autoload 'esc/get-buffers-matching-mode "boilerplate/emacs+" "\
Returns a list of buffers where their major-mode is equal to MODE.

\(fn MODE)" nil nil)

(autoload 'esc/multi-occur-in-this-mode "boilerplate/emacs+" "\
Show all lines matching REGEXP in buffers with this major mode.

\(fn)" t nil)

(autoload 'esc/toggle-selective-display "boilerplate/emacs+" "\
Enable code folding in current buffer.

\(fn COLUMN)" t nil)

(autoload 'esc/toggle-fullscreen "boilerplate/emacs+" "\
Toggles whether the currently selected frame consumes the entire display
   or is decorated with a window border

\(fn)" t nil)

(autoload 'esc/unroll-cc-arguments "boilerplate/emacs+" "\
Unroll a function's arguments into a more readable
  one-per-line format. Be sure to invoke this defun from before the
  opening paren of the function's arguments.

  This function has delimeters based on cc-mode dialects, and as a
  result would not be very useful for a language like Lisp.

\(fn)" t nil)

(autoload 'esc/swap-buffer-locations "boilerplate/emacs+" "\
Rotate your windows around and around.

\(fn)" t nil)

(autoload 'esc/open-line-below "boilerplate/emacs+" "\
Create a new line above the current line. Can be used with point
   anywhere on the line.

\(fn)" t nil)

(autoload 'esc/open-line-above "boilerplate/emacs+" "\
Create a new line below the current line. Can be used with point
   anywhere on the line.

\(fn)" t nil)

(autoload 'esc/goto-line-with-feedback "boilerplate/emacs+" "\
Show line numbers temporarily, while prompting for the line number input.

\(fn)" t nil)

(autoload 'esc/delete-whole-word "boilerplate/emacs+" "\
This defun will delete the entire word at point. This function
relies on `esc/kill-whole-word'.

\(fn)" t nil)

(autoload 'esc/kill-whole-word "boilerplate/emacs+" "\
This defun will kill the entire word at point (on both sides
of point). DELETE, if non-nil, will prevent the word from being
appended to the kill-ring.

\(fn &optional DELETE)" t nil)

(autoload 'esc/eval-and-replace "boilerplate/emacs+" "\
Replace the preceding sexp with its value.

\(fn &optional ARG)" t nil)

(autoload 'esc/save-buffers-kill-emacs "boilerplate/emacs+" "\
Offer to save each buffer(once only), then kill this Emacs process.
   With prefix ARG, silently save all file-visiting buffers, then kill.

\(fn &optional ARG)" t nil)

(autoload 'esc/unkillable-scratch-buffer "boilerplate/emacs+" "\
Prevent the *scratch* buffer from ever being killed.

\(fn)" nil nil)

(autoload 'esc/mode-line-other-buffer-other-window "boilerplate/emacs+" "\
Switch to `other-window', use `mode-line-other-buffer', and
    switch back to the original window.

\(fn)" t nil)

(autoload 'delete-word "boilerplate/emacs+" "\
Delete characters forward until encountering the end of a word.
  With argument, do this that many times.

\(fn ARG)" t nil)

(autoload 'backward-delete-word "boilerplate/emacs+" "\
Delete characters backward until encountering the end of a word.
  With argument, do this that many times.

\(fn ARG)" t nil)

(autoload 'esc/follow-mode-80-char-compliant "boilerplate/emacs+" "\
Open the current buffer in `follow-mode' in as many 80-char
windows as you can fit on this screen.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "boilerplate/help+" "../../../../../.emacs.d/site-lisp/esc-lisp/boilerplate/help+.el"
;;;;;;  (21741 17535 181366 743000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/boilerplate/help+.el

(autoload 'esc/insert-defun-at-point "boilerplate/help+" "\
Insert at point the name of the function KEY invokes.  KEY is a string.
  If INSERT (the prefix arg) is non-nil, insert the message in the
  buffer.  If non-nil, UNTRANSLATED is a vector of the untranslated
  events.  It can also be a number in which case the untranslated
  events from the last key hit are used.

  If KEY is a menu item or a tool-bar button that is disabled, this command
  temporarily enables it to allow getting help on disabled items and buttons.

\(fn &optional KEY INSERT UNTRANSLATED STRING)" t nil)

(autoload 'describe-keymap "boilerplate/help+" "\
Describe bindings in KEYMAP, a variable whose value is a keymap.
Completion is available for the keymap name.

\(fn KEYMAP)" t nil)

;;;***

;;;### (autoloads nil "boilerplate/os-interaction" "../../../../../.emacs.d/site-lisp/esc-lisp/boilerplate/os-interaction.el"
;;;;;;  (21741 17535 134700 78000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/boilerplate/os-interaction.el

(autoload 'esc/middle-click-yank "boilerplate/os-interaction" "\
Yank from the middle click mouse buffer.

\(fn)" t nil)

(autoload 'esc/insert-file-name "boilerplate/os-interaction" "\
Inserts the name of the current file (including extension) at point.

When ARG is non-nil, the filename will be printed in a different format.
If ARG is 0, insert the full path of the filename.
If ARG is - (or -1), insert the filename without extension.

\(fn &optional ARG)" t nil)

(autoload 'esc/copy-file-name-to-clipboard "boilerplate/os-interaction" "\
Copy the current buffer file name to the clipboard.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "boilerplate/programming+" "../../../../../.emacs.d/site-lisp/esc-lisp/boilerplate/programming+.el"
;;;;;;  (21741 17535 201366 741000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/boilerplate/programming+.el

(autoload 'esc/tagsgen "boilerplate/programming+" "\
Create a TAGS file using TAGSGEN in a directory that is
prompted with `ido-read-directory-name'.

\(fn)" t nil)

(autoload 'esc/recompile "boilerplate/programming+" "\
Save current window configuration to
`esc-precompile-window-state' and execute
`recompile'. `esc/bury-compilation-buffer-if-successful' will
bury the compilation buffer if compilation succeeds.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "browse-cliplink" "../../../../../.emacs.d/site-lisp/esc-lisp/browse-cliplink.el"
;;;;;;  (21741 9450 211110 247000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/browse-cliplink.el

(autoload 'browse-cliplink "browse-cliplink" "\
\\[browse-web] at the url in your clipboard.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "clean-mode-line" "../../../../../.emacs.d/site-lisp/esc-lisp/clean-mode-line.el"
;;;;;;  (21741 9450 237776 914000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/clean-mode-line.el

(autoload 'clean-mode-line "clean-mode-line" "\
Clean the mode line by transforming symbols into
custom-defined strings.

See `mode-line-cleaner-alist' for more documentation.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "color-theme-stack" "../../../../../.emacs.d/site-lisp/esc-lisp/color-theme-stack.el"
;;;;;;  (21741 9450 277776 913000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/color-theme-stack.el

(autoload 'color-theme-push "color-theme-stack" "\
Switch to a theme, saving the old one.

\(fn)" nil nil)

(autoload 'color-theme-pop "color-theme-stack" "\
Restore the previous theme in use.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "dired-config" "../../../../../.emacs.d/site-lisp/esc-lisp/dired-config.el"
;;;;;;  (21741 9450 487776 910000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/dired-config.el

(autoload 'esc/dired-mode-hook "dired-config" "\
esc's hook to \\[dired-mode-hook].

\(fn)" nil nil)

(autoload 'esc/dired-load-hook "dired-config" "\
esc's hook to \\[dired-load-hook].

\(fn)" nil nil)

(autoload 'esc/dired-load-hook-omit-files "dired-config" "\


\(fn)" nil nil)

(autoload 'esc/dired-back-to-top "dired-config" "\
Goes to the first line in dired, not the top of the buffer.

\(fn)" t nil)

(autoload 'esc/dired-jump-to-bottom "dired-config" "\
Goes to the last line in dired, not the bottom of the buffer.

\(fn)" t nil)

(autoload 'esc/dired-find-file-single-mode "dired-config" "\
This defun will invoke `dired-find-file` and open the file(s) at point in `single-mode'.

\(fn)" t nil)

(autoload 'esc/dired-up-level-reuse-buffer "dired-config" "\
This defun will go to the parent directory in dired while reusing the current buffer.

\(fn)" t nil)

(autoload 'esc/search-my-lisp-dir "dired-config" "\
Open esc-lisp in `dired' for easy editing of configs.

\(fn)" t nil)

(autoload 'esc/dired-back-to-start-of-files "dired-config" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "esc-mode" "../../../../../.emacs.d/site-lisp/esc-lisp/esc-mode.el"
;;;;;;  (21741 13347 624942 72000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/esc-mode.el

(autoload 'esc-mode "esc-mode" "\
Toggle esc-keys mode.
                 A minor mode so that my key settings override annoying major modes.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "gntp" "../../../../../.emacs.d/site-lisp/esc-lisp/gntp.el"
;;;;;;  (21741 9450 907776 904000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/gntp.el

(autoload 'gntp-notify "gntp" "\
Send notification NAME with TITLE, TEXT, PRIORITY and ICON to SERVER:PORT.
PORT defaults to `gntp-server-port'

\(fn NAME TITLE TEXT SERVER &optional PORT PRIORITY ICON)" nil nil)

;;;***

;;;### (autoloads nil "hidden-mode-line-mode" "../../../../../.emacs.d/site-lisp/esc-lisp/hidden-mode-line-mode.el"
;;;;;;  (21741 9450 994443 568000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/hidden-mode-line-mode.el

(autoload 'hidden-mode-line-mode "hidden-mode-line-mode" "\
Minor mode to hide the mode-line in the current buffer.

\(fn &optional ARG)" t nil)

(autoload 'mode-line-in-header "hidden-mode-line-mode" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "ido-extras" "../../../../../.emacs.d/site-lisp/esc-lisp/ido-extras.el"
;;;;;;  (21741 9451 57776 900000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/ido-extras.el

(autoload 'ido-recentf-open "ido-extras" "\
Use `ido-completing-read' to \\[find-file] a recent file.

\(fn)" t nil)

(autoload 'ido-goto-symbol "ido-extras" "\
Use `ido-completing-read` to query a function in the current
buffer and jump to it. Functions are defined by the active minor
mode.

\(fn &optional SYMBOL-LIST)" t nil)

(autoload 'esc/visit-favorite-dir "ido-extras" "\
Offer all directories inside a set of directories.
Compile a list of all directories inside each element of
`esc/favorite-directories', and visit one of them with
`ido-completing-read'.
With prefix argument FILES-TOO also offer to find files.

\(fn FILES-TOO)" t nil)

;;;***

;;;### (autoloads nil "multiple-cursors-config" "../../../../../.emacs.d/site-lisp/esc-lisp/multiple-cursors-config.el"
;;;;;;  (21741 9451 107776 900000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/multiple-cursors-config.el

(autoload 'esc/multiple-cursors-mode-enabled-hook "multiple-cursors-config" "\
esc's code to \\[multiple-cursors-mode-enabled-hook].

\(fn)" nil nil)

(autoload 'esc/multiple-cursors-mode-disabled-hook "multiple-cursors-config" "\
esc's code to \\[multiple-cursors-mode-disabled-hook].

\(fn)" nil nil)

(autoload 'mc/search "multiple-cursors-config" "\
Iterate through each active cursor and search for the designated string.
SEARCH-COMMAND is the command to use for movement- either
\\[search-forward] or \\[search-backward].

Helper defun for \\[mc/search-forward] and \\[mc/search-backward].

\(fn SEARCH-COMMAND)" nil nil)

(autoload 'mc/search-forward "multiple-cursors-config" "\
Simplified version of forward search that supports multiple cursors.

\(fn)" t nil)

(autoload 'mc/search-backward "multiple-cursors-config" "\
Simplified version of backward search that supports multiple cursors.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "rtos/disaster-arm" "../../../../../.emacs.d/site-lisp/esc-lisp/rtos/disaster-arm.el"
;;;;;;  (21735 15125 456763 333000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/rtos/disaster-arm.el

(autoload 'disaster-arm "rtos/disaster-arm" "\
Shows assembly code for current line of C/C++ file.

Here's the logic path it follows:

- Is there a Makefile in this directory? Run `make bufname.o`.
- Or is there a Makefile in a parent directory? Run `make -C .. bufname.o`.
- Or is this a C file? Run `cc -g -O3 -c -o bufname.o bufname.c`
- Or is this a C++ file? Run `c++ -g -O3 -c -o bufname.o bufname.c`
- If build failed, display errors in compile-mode.
- Run objdump inside a new window while maintaining focus.
- Jump to line matching current line.

If FILE and LINE are not specified, the current editing location
is used.

\(fn &optional FILE LINE)" t nil)

;;;***

;;;### (autoloads nil "rtos/rtos-dev-mode" "../../../../../.emacs.d/site-lisp/esc-lisp/rtos/rtos-dev-mode.el"
;;;;;;  (21738 4228 156675 581000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/rtos/rtos-dev-mode.el

(autoload 'rtos-dev-mode "rtos/rtos-dev-mode" "\
A minor mode to augment the development of
https://github.com/hershic/ee445m-labs.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "scroll-all-config" "../../../../../.emacs.d/site-lisp/esc-lisp/scroll-all-config.el"
;;;;;;  (21741 9451 164443 566000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/scroll-all-config.el

(autoload 'mwheel-scroll-all-function-all "scroll-all-config" "\


\(fn FUNC ARG)" nil nil)

(autoload 'mwheel-scroll-all-scroll-up-all "scroll-all-config" "\


\(fn &optional ARG)" nil nil)

(autoload 'mwheel-scroll-all-scroll-down-all "scroll-all-config" "\


\(fn &optional ARG)" nil nil)

;;;***

;;;### (autoloads nil "test-mode" "../../../../../.emacs.d/site-lisp/esc-lisp/test-mode.el"
;;;;;;  (21741 9450 511110 243000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/test-mode.el

(autoload 'test-mode "test-mode" "\
Toggle test-keys mode.
                   A destructable minor mode to make restarts less frequent.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "update-autoloads" "../../../../../.emacs.d/site-lisp/esc-lisp/update-autoloads.el"
;;;;;;  (21741 9451 254443 565000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/update-autoloads.el

(let ((loads (get 'autoloads 'custom-loads))) (if (member '"update-autoloads" loads) nil (put 'autoloads 'custom-loads (cons '"update-autoloads" loads))))

(autoload 'list-files-in-subtree-matching-regexp-recursive "update-autoloads" "\
List the `.el' files in DIRECTORY and in its sub-directories.

If REGEXP is non-nil, compile a list of files matching REGEXP
instead of `.el'.

\(fn DIRECTORY &optional REGEXP)" t nil)

(autoload 'get-project-loaddefs-path "update-autoloads" "\
Return the path to the closest loaddefs.el file relative to BASE.

Only query this defun with BASE as a subdir of ~/.emacs.d/esc-lisp.

\(fn BASE)" t nil)

(let ((loads (get 'autoloads 'custom-loads))) (if (member '"update-autoloads" loads) nil (put 'autoloads 'custom-loads (cons '"update-autoloads" loads))))

(autoload 'list-files-in-subtree-matching-regexp-recursive "update-autoloads" "\
List the `.el' files in DIRECTORY and in its sub-directories.

If REGEXP is non-nil, compile a list of files matching REGEXP
instead of `.el'.

\(fn DIRECTORY &optional REGEXP)" t nil)

(autoload 'get-project-loaddefs-path "update-autoloads" "\
Return the path to the closest loaddefs.el file relative to BASE.

Only query this defun with BASE as a subdir of ~/.emacs.d/site-lisp/esc-lisp.

\(fn BASE)" t nil)

(autoload 'update-esc-lisp-autoloads "update-autoloads" "\
Update autoload definitions for Lisp files in the directories DIRS.
In an interactive call, you must give one argument, the name of a
single directory.  In a call from Lisp, you can supply multiple
directories as separate arguments, but this usage is discouraged.

The function DOES recursively descend into subdirectories of the
directory or directories specified.

Note: this docstring was copied from \\[update-directory-autoloads].

\(fn &optional BASE)" t nil)

;;;***

(provide 'loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; loaddefs.el ends here

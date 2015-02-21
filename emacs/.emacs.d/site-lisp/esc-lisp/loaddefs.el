;;

;;;### (autoloads nil "boilerplate/boilerplate" "../../../../../.emacs.d/site-lisp/esc-lisp/boilerplate/boilerplate.el"
;;;;;;  (21736 2513 429488 242000))
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
;;;;;;  (21736 3046 9515 664000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/boilerplate/buffer+.el

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
;;;;;;  (21735 36608 397869 431000))
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
;;;;;;  (21735 37563 587918 611000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/boilerplate/displaced-yank.el

(autoload 'esc/define-displaced-yank "boilerplate/displaced-yank" "\


\(fn FUNCNAME DATA)" nil t)

;;;***

;;;### (autoloads nil "boilerplate/emacs+" "../../../../../.emacs.d/site-lisp/esc-lisp/boilerplate/emacs+.el"
;;;;;;  (21736 2513 302821 569000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/boilerplate/emacs+.el

(autoload 'esc/pull-up-line "boilerplate/emacs+" "\
Pull up ARG lines.

\(fn &optional ARG)" t nil)

(autoload 'esc/toggle-letter-case "boilerplate/emacs+" "\
Toggle the letter case of current word or text selection.
  Toggles between: “all lower”, “Init Caps”, “ALL CAPS”.

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

(autoload 'esc/minimap-toggle "boilerplate/emacs+" "\
Toggle minimap for current buffer.

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

;;;***

;;;### (autoloads nil "boilerplate/help+" "../../../../../.emacs.d/site-lisp/esc-lisp/boilerplate/help+.el"
;;;;;;  (21736 2513 346154 905000))
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
;;;;;;  (21734 43959 918207 6000))
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

;;;### (autoloads nil "browse-cliplink" "../../../../../.emacs.d/site-lisp/esc-lisp/browse-cliplink.el"
;;;;;;  (21727 59419 637096 841000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/browse-cliplink.el

(autoload 'browse-cliplink "browse-cliplink" "\
\\[browse-web] at the url in your clipboard.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "clear-mode-line/clean-mode-line" "../../../../../.emacs.d/site-lisp/esc-lisp/clear-mode-line/clean-mode-line.el"
;;;;;;  (21725 33444 983599 545000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/clear-mode-line/clean-mode-line.el

(autoload 'clean-mode-line "clear-mode-line/clean-mode-line" "\
Clean the mode line by transforming symbols into
custom-defined strings.

See `mode-line-cleaner-alist' for more documentation.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "color-theme-stack" "../../../../../.emacs.d/site-lisp/esc-lisp/color-theme-stack.el"
;;;;;;  (21736 7981 93103 91000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/color-theme-stack.el

(autoload 'color-theme-push "color-theme-stack" "\
Switch to a theme, saving the old one.

\(fn)" nil nil)

(autoload 'color-theme-pop "color-theme-stack" "\
Restore the previous theme in use.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "colors/color-theme-stack" "../../../../.emacs.d/esc-lisp/colors/color-theme-stack.el"
;;;;;;  (21724 64931 226454 333000))
;;; Generated autoloads from ../../../../.emacs.d/esc-lisp/colors/color-theme-stack.el

(autoload 'color-theme-push "colors/color-theme-stack" "\
Switch to a theme, saving the old one.

\(fn)" nil nil)

(autoload 'color-theme-pop "colors/color-theme-stack" "\
Restore the previous theme in use.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "dired-config/dired-config" "../../../../../.emacs.d/site-lisp/esc-lisp/dired-config/dired-config.el"
;;;;;;  (21726 19616 941308 484000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/dired-config/dired-config.el

(autoload 'esc/dired-mode-hook "dired-config/dired-config" "\
esc's hook to \\[dired-mode-hook].

\(fn)" nil nil)

(autoload 'esc/dired-load-hook "dired-config/dired-config" "\
esc's hook to \\[dired-load-hook].

\(fn)" nil nil)

(autoload 'esc/dired-load-hook-omit-files "dired-config/dired-config" "\


\(fn)" nil nil)

(autoload 'esc/dired-back-to-top "dired-config/dired-config" "\
Goes to the first line in dired, not the top of the buffer.

\(fn)" t nil)

(autoload 'esc/dired-jump-to-bottom "dired-config/dired-config" "\
Goes to the last line in dired, not the bottom of the buffer.

\(fn)" t nil)

(autoload 'esc/dired-find-file-single-mode "dired-config/dired-config" "\
This defun will invoke `dired-find-file` and open the file(s) at point in `single-mode'.

\(fn)" t nil)

(autoload 'esc/dired-up-level-reuse-buffer "dired-config/dired-config" "\
This defun will go to the parent directory in dired while reusing the current buffer.

\(fn)" t nil)

(autoload 'esc/search-my-lisp-dir "dired-config/dired-config" "\
Open esc-lisp in `dired' for easy editing of configs.

\(fn)" t nil)

(autoload 'esc/dired-back-to-start-of-files "dired-config/dired-config" "\


\(fn)" t nil)

;;;***

;;;### (autoloads (file-notify-handle-event) "emacs-wiki/filenotify"
;;;;;;  "emacs-wiki/filenotify.el" (21421 36514 493444 365000))
;;; Generated autoloads from emacs-wiki/filenotify.el

(autoload 'file-notify-handle-event "emacs-wiki/filenotify" "\
Handle file system monitoring event.
If EVENT is a filewatch event, call its callback.
Otherwise, signal a `file-notify-error'.

\(fn EVENT)" t nil)

;;;***

;;;### (autoloads nil "esc-mode" "../../../../../.emacs.d/site-lisp/esc-lisp/esc-mode.el"
;;;;;;  (21736 7981 623103 118000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/esc-mode.el

(autoload 'esc-mode "esc-mode" "\
Toggle esc-keys mode.
                 A minor mode so that my key settings override annoying major modes.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "gntp" "../../../../../.emacs.d/site-lisp/esc-lisp/gntp.el"
;;;;;;  (21729 24160 64656 197000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/gntp.el

(autoload 'gntp-notify "gntp" "\
Send notification NAME with TITLE, TEXT, PRIORITY and ICON to SERVER:PORT.
PORT defaults to `gntp-server-port'

\(fn NAME TITLE TEXT SERVER &optional PORT PRIORITY ICON)" nil nil)

;;;***

;;;### (autoloads nil "hidden-mode-line-mode/hidden-mode-line-mode"
;;;;;;  "../../../../../.emacs.d/site-lisp/esc-lisp/hidden-mode-line-mode/hidden-mode-line-mode.el"
;;;;;;  (21725 34156 213613 881000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/hidden-mode-line-mode/hidden-mode-line-mode.el

(autoload 'hidden-mode-line-mode "hidden-mode-line-mode/hidden-mode-line-mode" "\
Minor mode to hide the mode-line in the current buffer.

\(fn &optional ARG)" t nil)

(autoload 'mode-line-in-header "hidden-mode-line-mode/hidden-mode-line-mode" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "ido-config/ido-config" "../../../../.emacs.d/esc-lisp/ido-config/ido-config.el"
;;;;;;  (21705 16240 864682 627000))
;;; Generated autoloads from ../../../../.emacs.d/esc-lisp/ido-config/ido-config.el

(autoload 'ido-recentf-open "ido-config/ido-config" "\
Use `ido-completing-read' to \\[find-file] a recent file

\(fn)" t nil)

(autoload 'ido-goto-symbol "ido-config/ido-config" "\
Refresh imenu and jump to a function in the buffer using
   ido. Functions are defined by the active minor mode.

\(fn &optional SYMBOL-LIST)" t nil)

;;;***

;;;### (autoloads nil "ido-extras/ido-extras" "../../../../../.emacs.d/site-lisp/esc-lisp/ido-extras/ido-extras.el"
;;;;;;  (21724 64935 793120 736000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/ido-extras/ido-extras.el

(autoload 'ido-recentf-open "ido-extras/ido-extras" "\
Use `ido-completing-read' to \\[find-file] a recent file.

\(fn)" t nil)

(autoload 'ido-goto-symbol "ido-extras/ido-extras" "\
Use `ido-completing-read` to query a function in the current
buffer and jump to it. Functions are defined by the active minor
mode.

\(fn &optional SYMBOL-LIST)" t nil)

;;;***

;;;### (autoloads nil "lisp-infection/lisp-infection" "../../../../.emacs.d/esc-lisp/lisp-infection/lisp-infection.el"
;;;;;;  (21705 16240 864682 627000))
;;; Generated autoloads from ../../../../.emacs.d/esc-lisp/lisp-infection/lisp-infection.el

(autoload 'li/mark-current-defun "lisp-infection/lisp-infection" "\
Mark the current defun.

\(fn)" t nil)

(autoload 'li/mark-current-sexp "lisp-infection/lisp-infection" "\
Mark the current sexp.

\(fn)" t nil)

(autoload 'li/copy-sexp "lisp-infection/lisp-infection" "\
Save a sexp to the kill ring without killing it.

\(fn)" t nil)

(autoload 'li/end-of-sexp "lisp-infection/lisp-infection" "\
Return the end of the current sexp.

\(fn)" nil nil)

(autoload 'li/beginning-of-sexp "lisp-infection/lisp-infection" "\
Return the beginning of the current sexp.

\(fn)" nil nil)

(autoload 'li/delete-sexp "lisp-infection/lisp-infection" "\
Delete sexp without saving to the kill ring.

\(fn)" t nil)

(autoload 'li/eval-current-sexp "lisp-infection/lisp-infection" "\
Evaluate the current sexp.

\(fn)" t nil)

(autoload 'li/indent-entire-defun "lisp-infection/lisp-infection" "\
Re-indent the entire defun for ease of editing.

\(fn)" t nil)

(autoload 'li/eval-and-replace "lisp-infection/lisp-infection" "\
Replace the preceding sexp with its value.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "multiple-cursors-config/mc-config" "../../../../../.emacs.d/site-lisp/esc-lisp/multiple-cursors-config/mc-config.el"
;;;;;;  (21726 19616 941308 484000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/multiple-cursors-config/mc-config.el

(autoload 'esc/multiple-cursors-mode-enabled-hook "multiple-cursors-config/mc-config" "\
esc's code to \\[multiple-cursors-mode-enabled-hook].

\(fn)" nil nil)

(autoload 'esc/multiple-cursors-mode-disabled-hook "multiple-cursors-config/mc-config" "\
esc's code to \\[multiple-cursors-mode-disabled-hook].

\(fn)" nil nil)

(autoload 'mc/search "multiple-cursors-config/mc-config" "\
Iterate through each active cursor and search for the designated string.
SEARCH-COMMAND is the command to use for movement- either
\\[search-forward] or \\[search-backward].

Helper defun for \\[mc/search-forward] and \\[mc/search-backward].

\(fn SEARCH-COMMAND)" nil nil)

(autoload 'mc/search-forward "multiple-cursors-config/mc-config" "\
Simplified version of forward search that supports multiple cursors.

\(fn)" t nil)

(autoload 'mc/search-backward "multiple-cursors-config/mc-config" "\
Simplified version of backward search that supports multiple cursors.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "rtos/disaster-arm" "../../../../../.emacs.d/site-lisp/esc-lisp/rtos/disaster-arm.el"
;;;;;;  (21731 51453 320025 84000))
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

;;;### (autoloads nil "scroll-all-mode-config/scroll-all" "../../../../.emacs.d/esc-lisp/scroll-all-mode-config/scroll-all.el"
;;;;;;  (21724 64935 923120 729000))
;;; Generated autoloads from ../../../../.emacs.d/esc-lisp/scroll-all-mode-config/scroll-all.el

(autoload 'mwheel-scroll-all-function-all "scroll-all-mode-config/scroll-all" "\


\(fn FUNC ARG)" nil nil)

(autoload 'mwheel-scroll-all-scroll-up-all "scroll-all-mode-config/scroll-all" "\


\(fn &optional ARG)" nil nil)

(autoload 'mwheel-scroll-all-scroll-down-all "scroll-all-mode-config/scroll-all" "\


\(fn &optional ARG)" nil nil)

;;;***

;;;### (autoloads nil "scroll-all-mode-config/scroll-all-config"
;;;;;;  "../../../../../.emacs.d/site-lisp/esc-lisp/scroll-all-mode-config/scroll-all-config.el"
;;;;;;  (21724 64935 923120 729000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/scroll-all-mode-config/scroll-all-config.el

(autoload 'mwheel-scroll-all-function-all "scroll-all-mode-config/scroll-all-config" "\


\(fn FUNC ARG)" nil nil)

(autoload 'mwheel-scroll-all-scroll-up-all "scroll-all-mode-config/scroll-all-config" "\


\(fn &optional ARG)" nil nil)

(autoload 'mwheel-scroll-all-scroll-down-all "scroll-all-mode-config/scroll-all-config" "\


\(fn &optional ARG)" nil nil)

;;;***

;;;### (autoloads (single/single-mode-hook single/kill-current-buffer
;;;;;;  single/iedit-mode single/read-only-mode single/undo single/query-replace
;;;;;;  single/scroll-down single/scroll-up single/quit-single-mode
;;;;;;  single-wrap-around-read-only-mode single-mode-enabled) "single-mode/single-mode"
;;;;;;  "single-mode/single-mode.el" (21420 35608 792359 142000))
;;; Generated autoloads from single-mode/single-mode.el

(autoload 'single-mode-enabled "single-mode/single-mode" "\
A macro to determine if \\[single-mode] is currently enabled.

\(fn)" nil t)

(autoload 'single-wrap-around-read-only-mode "single-mode/single-mode" "\
Wrap a BLOCK around read-only mode.

\(fn &rest BLOCK)" nil t)

(autoload 'single/quit-single-mode "single-mode/single-mode" "\
Exit \\[single-mode].

\(fn)" t nil)

(autoload 'single/scroll-up "single-mode/single-mode" "\
Scroll up the page without moving point.

The number of lines to scroll is determined by the variable
`single-line-shift-amount'.

ARG determines the number of times to scroll.

This command can be prefixed.

\(fn &optional ARG)" t nil)

(autoload 'single/scroll-down "single-mode/single-mode" "\
Scroll down the page without moving point.

The number of lines to scroll is determined by the variable
`single-line-shift-amount'.

ARG determines the number of times to scroll.

This command can be prefixed.

\(fn &optional ARG)" t nil)

(autoload 'single/query-replace "single-mode/single-mode" "\
Invoke `query-replace' on the entire buffer.

This means, starting from `beginning-of-buffer'.

\(fn)" t nil)

(autoload 'single/undo "single-mode/single-mode" "\
Undo, circumnavigating the `read-only-mode'.

\(fn)" t nil)

(autoload 'single/read-only-mode "single-mode/single-mode" "\
Toggle read-only mode.

\(fn)" t nil)

(autoload 'single/iedit-mode "single-mode/single-mode" "\
Invokes `iedit-mode', circumnavigating `read-only-mode'.

\(fn)" t nil)

(autoload 'single/kill-current-buffer "single-mode/single-mode" "\
Kill the current buffer.

\(fn)" t nil)

(autoload 'single/single-mode-hook "single-mode/single-mode" "\
Hook for function `single-mode'.

\(fn)" nil nil)

;;;***


;;;### (autoloads (tf/capture-command-loop tf/capture-command tf/backspace
;;;;;;  tf/return tf/twinkle-fingers-quit tf/newline) "twinkle-fingers/twinkle-fingers"
;;;;;;  "twinkle-fingers/twinkle-fingers.el" (21420 36357 369000
;;;;;;  481000))
;;; Generated autoloads from twinkle-fingers/twinkle-fingers.el

(autoload 'tf/newline "twinkle-fingers/twinkle-fingers" "\
RET for `twinkle-fingers'.

\(fn)" t nil)

(autoload 'tf/twinkle-fingers-quit "twinkle-fingers/twinkle-fingers" "\
Quit `twinkle-fingers'.

\(fn)" t nil)

(autoload 'tf/return "twinkle-fingers/twinkle-fingers" "\


\(fn)" t nil)

(autoload 'tf/backspace "twinkle-fingers/twinkle-fingers" "\


\(fn)" t nil)

(autoload 'tf/capture-command "twinkle-fingers/twinkle-fingers" "\
Return a nicely formatted string containing the last executed
command.

\(fn)" nil nil)

(autoload 'tf/capture-command-loop "twinkle-fingers/twinkle-fingers" "\
Loop to capture and insert commands for `twinkle-fingers'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "update-autoloads/update-autoloads" "../../../../../.emacs.d/site-lisp/esc-lisp/update-autoloads/update-autoloads.el"
;;;;;;  (21735 37385 504576 108000))
;;; Generated autoloads from ../../../../../.emacs.d/site-lisp/esc-lisp/update-autoloads/update-autoloads.el

(let ((loads (get 'autoloads 'custom-loads))) (if (member '"update-autoloads/update-autoloads" loads) nil (put 'autoloads 'custom-loads (cons '"update-autoloads/update-autoloads" loads))))

(autoload 'list-files-in-subtree-matching-regexp-recursive "update-autoloads/update-autoloads" "\
List the `.el' files in DIRECTORY and in its sub-directories.

If REGEXP is non-nil, compile a list of files matching REGEXP
instead of `.el'.

\(fn DIRECTORY &optional REGEXP)" t nil)

(autoload 'get-project-loaddefs-path "update-autoloads/update-autoloads" "\
Return the path to the closest loaddefs.el file relative to BASE.

Only query this defun with BASE as a subdir of ~/.emacs.d/esc-lisp.

\(fn BASE)" t nil)

(autoload 'update-esc-lisp-autoloads "update-autoloads/update-autoloads" "\
Update autoload definitions for Lisp files in the directories DIRS.
In an interactive call, you must give one argument, the name of a
single directory.  In a call from Lisp, you can supply multiple
directories as separate arguments, but this usage is discouraged.

The function DOES recursively descend into subdirectories of the
directory or directories specified.

Note: this docstring was copied from \\[update-directory-autoloads].

\(fn &optional BASE)" t nil)

;;;***

;;

;;;### (autoloads (esc/bury-compilation-buffer-if-successful esc/auto-byte-recompile
;;;;;;  esc/previous-error esc/rotate-window-split esc/toggle-window-dedicated
;;;;;;  esc/toggle-window-selectability esc/should-have-opened-this-in-other-window
;;;;;;  esc/hsplit-last-buffer esc/vsplit-last-buffer esc/raise-magit-status
;;;;;;  esc/raise-eshell-in-current-dir esc/raise-eshell esc/raise-ansi-term
;;;;;;  esc/toggle-fullscreen-buffer esc/bury-other-buffer esc/bury-buffer-delete-window
;;;;;;  esc/org-prev-source-code-block esc/org-next-source-code-block
;;;;;;  esc/insert-key-combination touch rename-defun describe-keymap
;;;;;;  esc/pull-up-line esc/backward-kill-line esc/scroll-down-slight
;;;;;;  esc/scroll-up-slight esc/edit-configs esc/proced-in-this-frame
;;;;;;  esc/zoom-out esc/zoom-in esc/minimap-toggle esc/display-emacs-init-time
;;;;;;  esc/goto-line-with-feedback esc/open-line-above esc/open-line-below
;;;;;;  esc/swap-buffer-locations esc/unroll-cc-arguments esc/toggle-fullscreen
;;;;;;  esc/toggle-selective-display esc/multi-occur-in-this-mode
;;;;;;  esc/get-buffers-matching-mode esc/rename-current-buffer-file
;;;;;;  esc/delete-current-buffer-file esc/increment-number-at-point
;;;;;;  esc/back-to-indentation-or-beginning esc/insert-date esc/date
;;;;;;  esc/time esc/word-count esc/remove-dos-eol esc/copy-line
;;;;;;  esc/insert-numeric-sequence esc/toggle-letter-case esc/save-buffers-kill-emacs
;;;;;;  esc/insert-defun-at-point esc/eval-and-replace esc/kill-whole-word
;;;;;;  esc/delete-whole-word esc/edit-my-bash esc/edit-my-emacs
;;;;;;  esc/mode-line-other-buffer-other-window esc/magic-autoload-next-defun-or-macro
;;;;;;  esc/copy-file-name-to-clipboard esc/insert-file-name esc/middle-click-yank
;;;;;;  esc/insert-surrounding-dollars esc/insert-surrounding-stars
;;;;;;  esc/insert-surrounding-quotes esc/insert-surrounding-chevrons
;;;;;;  esc/insert-surrounding-brackets-with-colon esc/insert-surrounding-brackets
;;;;;;  esc/insert-surrounding-pipes esc/insert-surrounding-braces
;;;;;;  esc/insert-surrounding-parens esc/insert-little-arrow esc/insert-surrounding-ticks
;;;;;;  esc/yank-and-move esc/javadoc-method-comment esc/insert-short-gpl
;;;;;;  esc/iedit-mode-hook esc/comint-mode-hook esc/erc-mode-hook
;;;;;;  esc/enh-ruby-mode-hook esc/fundamental-mode-hook esc/emacs-lisp-mode-hook
;;;;;;  esc/c++-mode-hook esc/c-mode-common-hook esc/minibuffer-exit-hook
;;;;;;  esc/minibuffer-setup-hook esc/prog-mode-hook) "boilerplate/boilerplate"
;;;;;;  "../../../../.emacs.d/esc-lisp/boilerplate/boilerplate.el"
;;;;;;  (21646 3087 366057 136000))
;;; Generated autoloads from ../../../../.emacs.d/esc-lisp/boilerplate/boilerplate.el

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

(autoload 'esc/insert-short-gpl "boilerplate/boilerplate" "\
Insert the short version of the GNU GPL v3.

\(fn)" t nil)

(autoload 'esc/javadoc-method-comment "boilerplate/boilerplate" "\
Insert a javadoc method comment at point.

\(fn)" t nil)

(autoload 'esc/yank-and-move "boilerplate/boilerplate" "\
This macro is a helper function to the keys in `esc-mode-map' that
insert duplicates of characters.

\(fn INS BACK)" nil t)

(fset 'bang-word-at-point "$ $")

(autoload 'esc/insert-surrounding-ticks "boilerplate/boilerplate" "\
Insert a backtick and fronttick at point. This command can be
  prefixed with a numerical argument.

\(fn &optional ARG)" t nil)

(autoload 'esc/insert-little-arrow "boilerplate/boilerplate" "\
Insert an arrow at point. This command can be prefixed with a
    numeric argument.

\(fn &optional ARG)" t nil)

(autoload 'esc/insert-surrounding-parens "boilerplate/boilerplate" "\
Insert parenthesis, and center point between them. This command
    can be prefixed with a numeric argument.

\(fn &optional ARG)" t nil)

(autoload 'esc/insert-surrounding-braces "boilerplate/boilerplate" "\
Insert a pair of braces, and center point between them. This
    command can be prefixed with a numeric argument.

\(fn &optional ARG)" t nil)

(autoload 'esc/insert-surrounding-pipes "boilerplate/boilerplate" "\
Insert a pair of pipes, and center point between them. This
    command can be prefixed with a numeric argument.

\(fn &optional ARG)" t nil)

(autoload 'esc/insert-surrounding-brackets "boilerplate/boilerplate" "\
Insert a pair of square brackets, and center point between
    them. This command can be prefixed with a numeric argument.

\(fn &optional ARG)" t nil)

(autoload 'esc/insert-surrounding-brackets-with-colon "boilerplate/boilerplate" "\
Insert a pair of square brackets around a colon, and center
    point between them (after the colon). This command can be
    prefixed with a numeric argument.

\(fn &optional ARG)" t nil)

(autoload 'esc/insert-surrounding-chevrons "boilerplate/boilerplate" "\
Insert a pair of chevrons, and center point between them. This
    command can be prefixed with a numeric argument.

\(fn &optional ARG)" t nil)

(autoload 'esc/insert-surrounding-quotes "boilerplate/boilerplate" "\
Insert double quotes, and center point between them. This command
    can be prefixed with a numeric argument.

\(fn &optional ARG)" t nil)

(autoload 'esc/insert-surrounding-stars "boilerplate/boilerplate" "\
Insert dual stars, and center point between them. This command
    can be prefixed with a numeric argument.

\(fn &optional ARG)" t nil)

(autoload 'esc/insert-surrounding-dollars "boilerplate/boilerplate" "\
Insert dual stars, and center point between them. This command
    can be prefixed with a numeric argument.

\(fn &optional ARG)" t nil)

(autoload 'esc/middle-click-yank "boilerplate/boilerplate" "\
Yank from the middle click mouse buffer.

\(fn)" t nil)

(autoload 'esc/insert-file-name "boilerplate/boilerplate" "\
Inserts the name of the current file (including extension) at point.

When ARG is non-nil, the filename will be printed in a different format.
If ARG is 0, insert the full path of the filename.
If ARG is - (or -1), insert the filename without extension.

\(fn &optional ARG)" t nil)

(autoload 'esc/copy-file-name-to-clipboard "boilerplate/boilerplate" "\
Copy the current buffer file name to the clipboard.

\(fn)" t nil)

(autoload 'esc/magic-autoload-next-defun-or-macro "boilerplate/boilerplate" "\
Prefix the next defun or macro after point with a line
identifying the sexp as a magically autoloaded cookie.

\(fn)" t nil)

(autoload 'esc/mode-line-other-buffer-other-window "boilerplate/boilerplate" "\
Switch to `other-window', use `mode-line-other-buffer', and
switch back to the original window.

\(fn)" t nil)

(autoload 'esc/edit-my-emacs "boilerplate/boilerplate" "\


\(fn)" t nil)

(autoload 'esc/edit-my-bash "boilerplate/boilerplate" "\


\(fn)" t nil)

(autoload 'esc/delete-whole-word "boilerplate/boilerplate" "\
This defun will delete the entire word at point. This function
relies on `esc/kill-whole-word'.

\(fn)" t nil)

(autoload 'esc/kill-whole-word "boilerplate/boilerplate" "\
This defun will kill the entire word at point (on both sides of
point).

DELETE, if non-nil, will prevent the word from being appended to the
kill-ring.

\(fn &optional DELETE)" t nil)

(autoload 'esc/eval-and-replace "boilerplate/boilerplate" "\
Replace the preceding sexp with its value.

\(fn &optional ARG)" t nil)

(autoload 'esc/insert-defun-at-point "boilerplate/boilerplate" "\
Insert at point the name of the function KEY invokes.  KEY is a string.
If INSERT (the prefix arg) is non-nil, insert the message in the
buffer.  If non-nil, UNTRANSLATED is a vector of the untranslated
events.  It can also be a number in which case the untranslated
events from the last key hit are used.

If KEY is a menu item or a tool-bar button that is disabled, this command
temporarily enables it to allow getting help on disabled items and buttons.

\(fn &optional KEY INSERT UNTRANSLATED STRING)" t nil)

(autoload 'esc/save-buffers-kill-emacs "boilerplate/boilerplate" "\
Offer to save each buffer(once only), then kill this Emacs process.
   With prefix ARG, silently save all file-visiting buffers, then kill.

\(fn &optional ARG)" t nil)

(autoload 'esc/toggle-letter-case "boilerplate/boilerplate" "\
Toggle the letter case of current word or text selection.
Toggles between: all lower, Init Caps, ALL CAPS.

\(fn)" t nil)

(autoload 'esc/insert-numeric-sequence "boilerplate/boilerplate" "\
Insert a sequence of numbers at point, separated by spaces. Inclusive.

\(fn)" t nil)

(autoload 'esc/copy-line "boilerplate/boilerplate" "\
Copy current line in the kill ring.

\(fn &optional ARG)" t nil)

(autoload 'esc/remove-dos-eol "boilerplate/boilerplate" "\
Do not show ^M in files containing mixed UNIX and DOS line endings.

\(fn)" t nil)

(autoload 'esc/word-count "boilerplate/boilerplate" "\
Count words bounded by mark and cursor; if no region defined, use buffer.

\(fn &optional BEGIN END)" t nil)

(autoload 'esc/time "boilerplate/boilerplate" "\
Insert string for the current esc/time formatted as '2:34 PM'.

\(fn)" t nil)

(autoload 'esc/date "boilerplate/boilerplate" "\
Insert string for today's esc/date nicely formatted in American style,
   e.g. Sunday September 17, 2000.

\(fn)" t nil)

(autoload 'esc/insert-date "boilerplate/boilerplate" "\
Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name.

\(fn PREFIX)" t nil)

(autoload 'esc/back-to-indentation-or-beginning "boilerplate/boilerplate" "\
Returns the point to the beginning of the current line, or if
already there, the beginning of text on the current line.

\(fn)" t nil)

(autoload 'esc/increment-number-at-point "boilerplate/boilerplate" "\
Increment the number at point. Can be prefaced with a multiplier.

\(fn ARG)" t nil)

(autoload 'esc/delete-current-buffer-file "boilerplate/boilerplate" "\
Removes file connected to current buffer and kills buffer.

\(fn)" t nil)

(autoload 'esc/rename-current-buffer-file "boilerplate/boilerplate" "\
Renames current buffer and file it is visiting.

\(fn)" t nil)

(autoload 'esc/get-buffers-matching-mode "boilerplate/boilerplate" "\
Returns a list of buffers where their major-mode is equal to MODE.

\(fn MODE)" nil nil)

(autoload 'esc/multi-occur-in-this-mode "boilerplate/boilerplate" "\
Show all lines matching REGEXP in buffers with this major mode.

\(fn)" t nil)

(autoload 'esc/toggle-selective-display "boilerplate/boilerplate" "\
Enable code folding in current buffer.

\(fn COLUMN)" t nil)

(autoload 'esc/toggle-fullscreen "boilerplate/boilerplate" "\
Toggles whether the currently selected frame consumes the entire display
   or is decorated with a window border

\(fn)" t nil)

(autoload 'esc/unroll-cc-arguments "boilerplate/boilerplate" "\
Unroll a function's arguments into a more readable
one-per-line format. Be sure to invoke this defun from before the
opening paren of the function's arguments.

This function has delimeters based on cc-mode dialects, and as a
result would not be very useful for a language like Lisp.

\(fn)" t nil)

(autoload 'esc/swap-buffer-locations "boilerplate/boilerplate" "\
Rotate your windows around and around.

\(fn)" t nil)

(autoload 'esc/open-line-below "boilerplate/boilerplate" "\
Create a new line above the current line. Can be used with point
   anywhere on the line.

\(fn)" t nil)

(autoload 'esc/open-line-above "boilerplate/boilerplate" "\
Create a new line below the current line. Can be used with point
   anywhere on the line.

\(fn)" t nil)

(autoload 'esc/goto-line-with-feedback "boilerplate/boilerplate" "\
Show line numbers temporarily, while prompting for the line number input.

\(fn)" t nil)

(autoload 'esc/display-emacs-init-time "boilerplate/boilerplate" "\


\(fn)" nil nil)

(autoload 'esc/minimap-toggle "boilerplate/boilerplate" "\
Toggle minimap for current buffer.

\(fn)" t nil)

(autoload 'esc/zoom-in "boilerplate/boilerplate" "\
Increase font size by 10 points

\(fn)" t nil)

(autoload 'esc/zoom-out "boilerplate/boilerplate" "\
Decrease font size by 10 points

\(fn)" t nil)

(autoload 'esc/proced-in-this-frame "boilerplate/boilerplate" "\
Open proced in the current window.

\(fn)" t nil)

(autoload 'esc/edit-configs "boilerplate/boilerplate" "\
Open a dired buffer in my root .emacs.d for quick edits.

\(fn)" t nil)

(autoload 'esc/scroll-up-slight "boilerplate/boilerplate" "\
Scroll up the page without moving point. The number of lines to
scroll is determined by the variable `esc-line-shift-amount'.

This command can be prefixed.

\(fn &optional ARG)" t nil)

(autoload 'esc/scroll-down-slight "boilerplate/boilerplate" "\
Scroll down the page without moving point. The number of lines to
scroll is determined by the variable `esc-line-shift-amount'.

This command can be prefixed.

\(fn &optional ARG)" t nil)

(autoload 'esc/backward-kill-line "boilerplate/boilerplate" "\
Kill up to `beginning-of-line', and kill ARG lines above point.
Everything will be appended to the kill ring.

\(fn &optional ARG)" t nil)

(autoload 'esc/pull-up-line "boilerplate/boilerplate" "\
Pull up ARG lines.

\(fn &optional ARG)" t nil)

(autoload 'describe-keymap "boilerplate/boilerplate" "\
Describe bindings in KEYMAP, a variable whose value is a keymap.
Completion is available for the keymap name.

\(fn KEYMAP)" t nil)

(autoload 'rename-defun "boilerplate/boilerplate" "\
Rename FUNCTION to a new name. Makfunbound FUNCTION after
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

(autoload 'esc/org-next-source-code-block "boilerplate/boilerplate" "\


\(fn)" t nil)

(autoload 'esc/org-prev-source-code-block "boilerplate/boilerplate" "\


\(fn)" t nil)

(autoload 'esc/bury-buffer-delete-window "boilerplate/boilerplate" "\
Kill current window and bury the current buffer to the bottom of the buffer list.

\(fn)" t nil)

(autoload 'esc/bury-other-buffer "boilerplate/boilerplate" "\
Bury the buffer that \\[mode-line-other-buffer] will take you
to.

If SWITCH-TO-OTHER-BUFFER is non-nil (e.g. this command is
prefixed) then after the other-buffer is buried, the command
\\[mode-line-other-buffer] will be used to switch buffers.

\(fn &optional SWITCH-TO-OTHER-BUFFER)" t nil)

(autoload 'esc/toggle-fullscreen-buffer "boilerplate/boilerplate" "\
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

(autoload 'esc/raise-ansi-term "boilerplate/boilerplate" "\
If the current buffer is:

   1) a running ansi-term named *ansi-term*, rename it.
   2) a stopped ansi-term, kill it and create a new one.
   3) a non ansi-term, go to an already running ansi-term
      or start a new one while killing a defunct one.

\(fn)" t nil)

(autoload 'esc/raise-eshell "boilerplate/boilerplate" "\
Bring up a full-screen eshell or restore previous window
configuration.

\(fn)" t nil)

(autoload 'esc/raise-eshell-in-current-dir "boilerplate/boilerplate" "\
Bring up a full-screen eshell in the current directory or
restore previous window configuration.

\(fn)" t nil)

(autoload 'esc/raise-magit-status "boilerplate/boilerplate" "\
Bring up a full-screen magit-status or restore previous
window configuration.

\(fn)" t nil)

(autoload 'esc/vsplit-last-buffer "boilerplate/boilerplate" "\
Split the window vertically and display the previous buffer.

\(fn PREFIX)" t nil)

(autoload 'esc/hsplit-last-buffer "boilerplate/boilerplate" "\
Split the window horizontally and display the previous buffer.

\(fn PREFIX)" t nil)

(autoload 'esc/should-have-opened-this-in-other-window "boilerplate/boilerplate" "\
Returns to the previous buffer in current window, calls
  \\[other-window], and opens the buffer in the new window.

  COUNT is the number of windows to advance; the argument is
  passed directly to \\[other-window], so see the documentation
  for more details.

\(fn &optional COUNT)" t nil)

(autoload 'esc/toggle-window-selectability "boilerplate/boilerplate" "\
Ignore/recognize a window from `other-window'.

\(fn)" t nil)

(autoload 'esc/toggle-window-dedicated "boilerplate/boilerplate" "\
Toggle whether the current active window is dedicated or not.

When dedicated, `display-buffer' will refrain from displaying
another buffer in a window.

\(fn)" t nil)

(autoload 'esc/rotate-window-split "boilerplate/boilerplate" "\
Transform a vertically split window to a horizontally split
   window.

\(fn)" t nil)

(autoload 'esc/previous-error "boilerplate/boilerplate" "\
Visit previous compilation error message and corresponding source code.
Complement to \\[next-error].

\(fn N)" t nil)

(autoload 'esc/auto-byte-recompile "boilerplate/boilerplate" "\
If the current buffer is in emacs-lisp-mode and there already exists an .elc
file corresponding to the current buffer file, then recompile the file.

\(fn)" t nil)

(autoload 'esc/bury-compilation-buffer-if-successful "boilerplate/boilerplate" "\
Bury the compilation buffer after a successful compile.

\(fn BUFFER STRING)" nil nil)

;;;***

;;;### (autoloads (esc/dired-back-to-start-of-files esc/search-my-lisp-dir
;;;;;;  esc/dired-up-level-reuse-buffer esc/dired-find-file-single-mode
;;;;;;  esc/dired-jump-to-bottom esc/dired-back-to-top esc/dired-load-hook-install-details
;;;;;;  esc/dired-load-hook-omit-files esc/dired-load-hook-hide-data
;;;;;;  esc/dired-load-hook esc/dired-mode-hook) "dired-config/dired-config"
;;;;;;  "../../../../.emacs.d/esc-lisp/dired-config/dired-config.el"
;;;;;;  (21645 60759 403562 413000))
;;; Generated autoloads from ../../../../.emacs.d/esc-lisp/dired-config/dired-config.el

(autoload 'esc/dired-mode-hook "dired-config/dired-config" "\
esc's hook to \\[dired-mode-hook].

\(fn)" nil nil)

(autoload 'esc/dired-load-hook "dired-config/dired-config" "\
esc's hook to \\[dired-load-hook].

\(fn)" nil nil)

(autoload 'esc/dired-load-hook-hide-data "dired-config/dired-config" "\


\(fn)" nil nil)

(autoload 'esc/dired-load-hook-omit-files "dired-config/dired-config" "\


\(fn)" nil nil)

(autoload 'esc/dired-load-hook-install-details "dired-config/dired-config" "\


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

;;;### (autoloads (ido-goto-symbol ido-recentf-open) "ido-config/ido-config"
;;;;;;  "../../../../.emacs.d/esc-lisp/ido-config/ido-config.el"
;;;;;;  (21645 60759 403562 413000))
;;; Generated autoloads from ../../../../.emacs.d/esc-lisp/ido-config/ido-config.el

(autoload 'ido-recentf-open "ido-config/ido-config" "\
Use `ido-completing-read' to \\[find-file] a recent file

\(fn)" t nil)

(autoload 'ido-goto-symbol "ido-config/ido-config" "\
Refresh imenu and jump to a function in the buffer using
   ido. Functions are defined by the active minor mode.

\(fn &optional SYMBOL-LIST)" t nil)

;;;***

;;;### (autoloads (li/eval-and-replace li/indent-entire-defun li/eval-current-sexp
;;;;;;  li/delete-sexp li/beginning-of-sexp li/end-of-sexp li/copy-sexp
;;;;;;  li/mark-current-sexp li/mark-current-defun) "lisp-infection/lisp-infection"
;;;;;;  "../../../../.emacs.d/esc-lisp/lisp-infection/lisp-infection.el"
;;;;;;  (21645 60759 399562 365000))
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

;;;### (autoloads (mc/search-backward mc/search-forward mc/search
;;;;;;  esc/multiple-cursors-mode-disabled-hook esc/multiple-cursors-mode-enabled-hook)
;;;;;;  "multiple-cursors-config/mc-config" "../../../../.emacs.d/esc-lisp/multiple-cursors-config/mc-config.el"
;;;;;;  (21645 60759 403562 413000))
;;; Generated autoloads from ../../../../.emacs.d/esc-lisp/multiple-cursors-config/mc-config.el

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

;;;### (autoloads (mwheel-scroll-all-scroll-down-all mwheel-scroll-all-scroll-up-all
;;;;;;  mwheel-scroll-all-function-all) "scroll-all-mode-config/scroll-all-config"
;;;;;;  "../../../../.emacs.d/esc-lisp/scroll-all-mode-config/scroll-all-config.el"
;;;;;;  (21645 60759 403562 413000))
;;; Generated autoloads from ../../../../.emacs.d/esc-lisp/scroll-all-mode-config/scroll-all-config.el

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

;;;### (autoloads (update-esc-lisp-autoloads get-project-loaddefs-path
;;;;;;  list-files-in-subtree-matching-regexp-recursive autoloads)
;;;;;;  "update-autoloads/update-autoloads" "../../../../.emacs.d/esc-lisp/update-autoloads/update-autoloads.el"
;;;;;;  (21645 60759 403562 413000))
;;; Generated autoloads from ../../../../.emacs.d/esc-lisp/update-autoloads/update-autoloads.el

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

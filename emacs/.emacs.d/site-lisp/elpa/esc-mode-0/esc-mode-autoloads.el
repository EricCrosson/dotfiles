;;; esc-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "../../../../dotfiles/emacs/.emacs.d/site-lisp/elpa/esc-mode-0/esc-mode"
;;;;;;  "esc-mode.el" "87dc2d7b3c279014b62f6581da1dba64")
;;; Generated autoloads from esc-mode.el

(defvar esc-mode t "\
Non-nil if Esc mode is enabled.
See the command `esc-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `esc-mode'.")

(custom-autoload 'esc-mode "../../../../dotfiles/emacs/.emacs.d/site-lisp/elpa/esc-mode-0/esc-mode" nil)

(autoload 'esc-mode "../../../../dotfiles/emacs/.emacs.d/site-lisp/elpa/esc-mode-0/esc-mode" "\
Toggle esc-keys mode, a minor mode so that my key settings
override annoying major modes.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "esc-defuns" "../../../../../../.emacs.d/site-lisp/elpa/esc-mode-0/esc-defuns.el"
;;;;;;  "526607980b5f78816a738c890d29fba7")
;;; Generated autoloads from ../../../../../../.emacs.d/site-lisp/elpa/esc-mode-0/esc-defuns.el

(autoload 'esc/vsplit-last-buffer "esc-defuns" "\
Split the window vertically and display the previous buffer.
Argument PREFIX when nil switches the new buffer to the last buffer.

\(fn PREFIX)" t nil)

(autoload 'esc/hsplit-last-buffer "esc-defuns" "\
Split the window horizontally and display the previous buffer.
Argument PREFIX when nil switches the new buffer to the last buffer.

\(fn PREFIX)" t nil)

(autoload 'mode-line-other-buffer-other-window "esc-defuns" "\
Switch to `other-window', use `mode-line-other-buffer', and
switch back to the original window.

\(fn)" t nil)

(autoload 'back-to-indentation-or-beginning "esc-defuns" "\
Returns the point to the beginning of the current line, or if
already there, the beginning of text on the current line.

\(fn)" t nil)

(autoload 'occur-dwim "esc-defuns" "\
Call function `occur' with a sane default.

Function `occur-dwim' will offer as the default candidate:
- the current region, if it's active
- the current symbol, otherwise

\(fn)" t nil)

(autoload 'middle-click-yank "esc-defuns" "\
Yank from the middle click mouse buffer.

\(fn)" t nil)

(autoload 'copy-line "esc-defuns" "\
Copy current line in the kill ring.

\(fn &optional ARG)" t nil)

(autoload 'open-line-below "esc-defuns" "\
Create a new line above the current line. Can be used with point
anywhere on the line.

\(fn)" t nil)

(autoload 'open-line-above "esc-defuns" "\
Create a new line below the current line. Can be used with point
anywhere on the line.

\(fn)" t nil)

(autoload 'goto-line-with-feedback "esc-defuns" "\
Show line numbers temporarily, while prompting for the line number input.

\(fn)" t nil)

(autoload 'pull-up-line "esc-defuns" "\
Pull up ARG lines.

\(fn &optional ARG)" t nil)

(autoload 'follow-mode-80-char-compliant "esc-defuns" "\
Open the current buffer in `follow-mode' in as many 80-char
windows as you can fit in the current frame. All other windows
will be closed.

\(fn)" t nil)

(autoload 'hsplit-current-buffer-follow-mode "esc-defuns" "\
Split the current buffer horizontally and engage function
`follow-mode'.

\(fn)" t nil)

(autoload 'esc/toggle-fullscreen-buffer "esc-defuns" "\
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

(autoload 'esc/raise-magit-status "esc-defuns" "\
Bring up a full-screen magit-status or restore previous
  window configuration.

  This defun will not raise magit if you have merge conflicts in
  the current buffer.

  This defun kills all buffers matching regexp '^*magit: ' upon the
  exit toggle of the fullscreen magit buffer

\(fn)" t nil)

(autoload 'esc/should-have-opened-this-in-other-window "esc-defuns" "\
Returns to the previous buffer in current window, calls
`other-window', and opens the buffer in the new window.

COUNT is the number of windows to advance; the argument is passed
directly to `other-window', so see the documentation for more
details.

\(fn &optional COUNT)" t nil)

(autoload 'esc/rotate-window-split "esc-defuns" "\
Transform a vertically split window to a horizontally split
window.

\(fn)" t nil)

(autoload 'esc/swap-buffer-locations "esc-defuns" "\
Rotate your windows around and around.

\(fn)" t nil)

(autoload 'esc/eval-and-replace "esc-defuns" "\
Replace the preceding sexp with its value.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "esc-hooks" "../../../../../../.emacs.d/site-lisp/elpa/esc-mode-0/esc-hooks.el"
;;;;;;  "12addc72430402ea2770e308850fe6c6")
;;; Generated autoloads from ../../../../../../.emacs.d/site-lisp/elpa/esc-mode-0/esc-hooks.el

(autoload 'esc/prog-mode-hook "esc-hooks" "\
esc's hook to \\[prog-mode-hook].

\(fn)" nil nil)

(autoload 'esc/minibuffer-setup-hook "esc-hooks" "\
esc's hook to run whilst entering the minibuffer's domain.

\(fn)" nil nil)

(autoload 'esc/c-mode-common-hook "esc-hooks" "\
esc's code to \\[c-mode-common-hook].

\(fn)" nil nil)

(autoload 'esc/c++-mode-hook "esc-hooks" "\
esc's code to \\[c++-mode-hook].

\(fn)" nil nil)

(autoload 'esc/emacs-lisp-mode-hook "esc-hooks" "\
esc's code to \\[emacs-lisp-mode-hook].

\(fn)" nil nil)

(autoload 'esc/fundamental-mode-hook "esc-hooks" "\
esc's code to \\[fundamental-mode-hook].

\(fn)" nil nil)

(autoload 'esc/erc-mode-hook "esc-hooks" "\
esc's code to \\[erc-mode-hook].

\(fn)" nil nil)

(autoload 'esc/comint-mode-hook "esc-hooks" "\
esc's code to \\[comint-mode-hook].

\(fn)" nil nil)

(autoload 'esc/org-mode-hook "esc-hooks" "\


\(fn)" nil nil)

(autoload 'esc/dired-load-hook-omit-files "esc-hooks" "\


\(fn)" nil nil)

(autoload 'esc/dired-back-to-top "esc-hooks" "\
Goes to the first line in dired, not the top of the buffer.

\(fn)" t nil)

(autoload 'esc/dired-jump-to-bottom "esc-hooks" "\
Goes to the last line in dired, not the bottom of the buffer.

\(fn)" t nil)

(autoload 'esc/dired-up-level-reuse-buffer "esc-hooks" "\
This defun will go to the parent directory in dired while reusing the current buffer.

\(fn)" t nil)

(autoload 'esc/dired-back-to-start-of-files "esc-hooks" "\


\(fn)" t nil)

(autoload 'esc/dired-mode-hook "esc-hooks" "\
esc's hook to \\[dired-mode-hook].

\(fn)" nil nil)

(autoload 'esc/dired-load-hook "esc-hooks" "\
esc's hook to \\[dired-load-hook].

\(fn)" nil nil)

(autoload 'esc/multiple-cursors-mode-enabled-hook "esc-hooks" "\
esc's code to function `multiple-cursors-mode-enabled-hook'.

\(fn)" nil nil)

(autoload 'esc/multiple-cursors-mode-disabled-hook "esc-hooks" "\
esc's code to function `multiple-cursors-mode-disabled-hook`.

\(fn)" nil nil)

(autoload 'mc/search "esc-hooks" "\
Iterate through each active cursor and search for the designated string.
  SEARCH-COMMAND is the command to use for movement- either
  function `search-forward` or function `search-backward`.

  Helper defun for function `mc/search-forward` and function `mc/search-backward`.

\(fn SEARCH-COMMAND)" nil nil)

(autoload 'mc/search-forward "esc-hooks" "\
Simplified version of forward search that supports multiple cursors.

\(fn)" t nil)

(autoload 'mc/search-backward "esc-hooks" "\
Simplified version of backward search that supports multiple cursors.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "esc-mode" "../../../../../../.emacs.d/site-lisp/elpa/esc-mode-0/esc-mode.el"
;;;;;;  "87dc2d7b3c279014b62f6581da1dba64")
;;; Generated autoloads from ../../../../../../.emacs.d/site-lisp/elpa/esc-mode-0/esc-mode.el

(defvar esc-mode t "\
Non-nil if Esc mode is enabled.
See the command `esc-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `esc-mode'.")

(custom-autoload 'esc-mode "esc-mode" nil)

(autoload 'esc-mode "esc-mode" "\
Toggle esc-keys mode, a minor mode so that my key settings
override annoying major modes.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../../../.emacs.d/site-lisp/elpa/esc-mode-0/esc-defuns.el"
;;;;;;  "../../../../../../.emacs.d/site-lisp/elpa/esc-mode-0/esc-hooks.el"
;;;;;;  "../../../../../../.emacs.d/site-lisp/elpa/esc-mode-0/esc-mode-autoloads.el"
;;;;;;  "../../../../../../.emacs.d/site-lisp/elpa/esc-mode-0/esc-mode-pkg.el"
;;;;;;  "../../../../../../.emacs.d/site-lisp/elpa/esc-mode-0/esc-mode.el"
;;;;;;  "esc-mode.el") (21767 29922 108448 391000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; esc-mode-autoloads.el ends here

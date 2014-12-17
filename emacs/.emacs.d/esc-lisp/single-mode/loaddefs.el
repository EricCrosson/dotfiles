;;

;;;### (autoloads (single/single-mode-hook single/kill-current-buffer
;;;;;;  single/iedit-mode single/read-only-mode single/undo single/query-replace
;;;;;;  single/ff-in-single-mode single/scroll-down single/scroll-up
;;;;;;  single/quit-single-mode single-wrap-around-read-only-mode
;;;;;;  single-mode-enabled) "single-mode" "../../../../../.emacs.d/esc-lisp/single-mode/single-mode.el"
;;;;;;  (21645 60759 403562 413000))
;;; Generated autoloads from ../../../../../.emacs.d/esc-lisp/single-mode/single-mode.el

(autoload 'single-mode-enabled "single-mode" "\
A macro to determine if \\[single-mode] is currently enabled.

\(fn)" nil t)

(autoload 'single-wrap-around-read-only-mode "single-mode" "\
Wrap a BLOCK around read-only mode.

\(fn &rest BLOCK)" nil t)

(autoload 'single/quit-single-mode "single-mode" "\
Exit \\[single-mode].

\(fn)" t nil)

(autoload 'single/scroll-up "single-mode" "\
Scroll up the page without moving point.

The number of lines to scroll is determined by the variable
`single-line-shift-amount'.

ARG determines the number of times to scroll.

This command can be prefixed.

\(fn &optional ARG)" t nil)

(autoload 'single/scroll-down "single-mode" "\
Scroll down the page without moving point.

The number of lines to scroll is determined by the variable
`single-line-shift-amount'.

ARG determines the number of times to scroll.

This command can be prefixed.

\(fn &optional ARG)" t nil)

(autoload 'single/ff-in-single-mode "single-mode" "\
Find a file and display the buffer in \\[single-mode].

\(fn)" t nil)

(autoload 'single/query-replace "single-mode" "\
Invoke `query-replace' on the entire buffer.

This means, starting from `beginning-of-buffer'.

\(fn)" t nil)

(autoload 'single/undo "single-mode" "\
Undo, circumnavigating the `read-only-mode'.

\(fn)" t nil)

(autoload 'single/read-only-mode "single-mode" "\
Toggle read-only mode.

\(fn)" t nil)

(autoload 'single/iedit-mode "single-mode" "\
Invokes `iedit-mode', circumnavigating `read-only-mode'.

\(fn)" t nil)

(autoload 'single/kill-current-buffer "single-mode" "\
Kill the current buffer.

\(fn)" t nil)

(autoload 'single/single-mode-hook "single-mode" "\
Hook for function `single-mode'.

\(fn)" nil nil)

;;;***

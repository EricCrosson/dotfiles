;;; help+-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "help+" "help+.el" (21762 27046 130887 598000))
;;; Generated autoloads from help+.el

(autoload 'describe-key "help+" "\
Describe the command that a keyboard/menu/mouse sequence invokes.
KEY can be any kind of a key sequence; it can include keyboard events,
mouse events, and/or menu events.  When calling from a program,
pass KEY as a string or a vector.

If non-nil, UNTRANSLATED is a vector of the corresponding untranslated events.
It can also be a number, in which case the untranslated events from
the last key sequence entered are used.
UP-EVENT is the up-event that was discarded by reading KEY, or nil.

If KEY is a menu item or a tool-bar button that is disabled, this command
temporarily enables it to allow getting help on disabled items and buttons.
Return nil if KEY is undefined; else return t.

\(fn &optional KEY UNTRANSLATED UP-EVENT)" t nil)

(autoload 'where-is "help+" "\
Show keyboard/menu/mouse sequences that invoke specified command.
Argument is a command definition, usually a symbol with a function
definition.  Default candidate is: preferably the
`symbol-nearest-point', or else the innermost function call
surrounding point (`function-called-at-point').

With no prefix argument, only commands actually bound to keys are
completion candidates.  With a prefix argument, all commands are
candidates.

With a plain (non-numeric) prefix argument, `C-u', insert the message
in the current buffer.

\(fn DEFINITION &optional INSERT)" t nil)

(autoload 'help-on-click/key "help+" "\
Give help on a key/menu sequence or object clicked with the mouse.
The object can be any part of an Emacs window or a name appearing in a
buffer.  You can do any of the following:

    type a key sequence (e.g. `C-M-s')
    choose a menu item (e.g. [menu-bar files open-file])
    click on a scroll bar
    click on the mode line
    click in the minibuffer
    click on an Emacs-related name in a buffer: apropos is called
    click anywhere else in a buffer: its modes are described

Help is generally provided using `describe-key' and the Emacs online
manual (via `Info-goto-emacs-key-command-node').  If no entry is found
in the index of the Emacs manual, then the manual is searched from the
beginning for literal occurrences of KEY.

For example, the KEY `C-g' is not in the index (for some reason), so
the manual is searched.  (Once an occurrence is found, you can
repeatedly type `s' in *Info* to search for additional occurrences.)

If you click on a name in a buffer, then `apropos-documentation' and
`apropos' are used to find information on the name.  These functions
are not used when you do something besides click on a name.

If you click elsewhere in a buffer other than the minibuffer, then
`describe-mode' is used to describe the buffer's current mode(s).

\(fn KEY)" t nil)

(autoload 'mouse-help-on-click "help+" "\
Give help on an object clicked with the mouse.

\(fn EVENT)" t nil)

(autoload 'mouse-help-on-mode-line-click "help+" "\
Give help on the mode line.

\(fn EVENT)" t nil)

(autoload 'pop-to-help-toggle "help+" "\
Pop to buffer *Help* or back to the buffer that sent you to *Help*.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; help+-autoloads.el ends here

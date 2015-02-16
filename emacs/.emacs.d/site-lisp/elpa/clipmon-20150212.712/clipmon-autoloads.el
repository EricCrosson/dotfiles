;;; clipmon-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "clipmon" "clipmon.el" (21729 18738 407710
;;;;;;  385000))
;;; Generated autoloads from clipmon.el

(define-key-after global-map [menu-bar options clipmon-separator] '(menu-item "---") 'highlight-paren-mode)

(define-key-after global-map [menu-bar options clipmon-killring] '(menu-item "Clipboard Monitor (Add to Kill Ring)" clipmon-mode :help "Add changes to the system clipboard to Emacs's kill ring." :button (:toggle . clipmon-mode)) 'clipmon-separator)

(define-key-after global-map [menu-bar options clipmon-autoinsert] '(menu-item "Clipboard Monitor Autoinsert" clipmon-autoinsert-toggle :help "Automatically insert changes to the system clipboard at the current location." :button (:toggle . clipmon--autoinsert)) 'clipmon-killring)

(defvar clipmon-mode nil "\
Non-nil if Clipmon mode is enabled.
See the command `clipmon-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `clipmon-mode'.")

(custom-autoload 'clipmon-mode "clipmon" nil)

(autoload 'clipmon-mode "clipmon" "\
Start/stop clipboard monitor - watch system clipboard, add changes to kill ring.
To also insert the changes to the system clipboard at the current
location, call `clipmon-autoinsert-toggle' to turn autoinsert on
and off. See commentary in source file for more information - M-x
find-library RET clipmon RET.

\(fn &optional ARG)" t nil)

(autoload 'clipmon-mode-start "clipmon" "\
Start clipboard monitor - watch system clipboard, add changes to kill ring.

\(fn)" t nil)

(autoload 'clipmon-autoinsert-toggle "clipmon" "\
Turn autoinsert on/off - watch system clipboard and insert changes.
Will change cursor color and play a sound.  Text will be
transformed before insertion according to various settings - see
`clipmon--transform-text'.

\(fn)" t nil)

(autoload 'clipmon-persist "clipmon" "\
Persist the kill ring to disk using Emacs's savehist library.
Will save the kill ring at the end of the session and at various
intervals as specified by variable `savehist-autosave-interval'.
Note that savehist also includes various other Emacs settings by
default, including the minibuffer history - see function
`savehist-mode' for more details.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("clipmon-pkg.el") (21729 18738 486204
;;;;;;  693000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; clipmon-autoloads.el ends here

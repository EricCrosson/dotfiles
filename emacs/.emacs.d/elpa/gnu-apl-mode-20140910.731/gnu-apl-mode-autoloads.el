;;; gnu-apl-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (gnu-apl) "gnu-apl-interactive" "gnu-apl-interactive.el"
;;;;;;  (21582 47366 298182 943000))
;;; Generated autoloads from gnu-apl-interactive.el

(autoload 'gnu-apl "gnu-apl-interactive" "\
Start the GNU APL interpreter in a buffer.
APL-EXECUTABLE is the path to the apl program (defaults
to `gnu-apl-executable').

\(fn APL-EXECUTABLE)" t nil)

;;;***

;;;### (autoloads (gnu-apl-indent-amounts gnu-apl-program-extra-args
;;;;;;  gnu-apl-gnuplot-program gnu-apl-native-listener-port gnu-apl-show-tips-on-start
;;;;;;  gnu-apl-show-apl-welcome gnu-apl-show-keymap-on-startup gnu-apl-redefine-function-when-in-use-action
;;;;;;  gnu-apl-auto-function-editor-popup gnu-apl-executable gnu-apl)
;;;;;;  "gnu-apl-mode" "gnu-apl-mode.el" (21582 47366 266182 900000))
;;; Generated autoloads from gnu-apl-mode.el

(let ((loads (get 'gnu-apl 'custom-loads))) (if (member '"gnu-apl-mode" loads) nil (put 'gnu-apl 'custom-loads (cons '"gnu-apl-mode" loads))))

(defvar gnu-apl-executable "apl" "\
Where the GNU APL implementaion is located.")

(custom-autoload 'gnu-apl-executable "gnu-apl-mode" t)

(defvar gnu-apl-auto-function-editor-popup t "\
Edit function definitions in an Emacs buffer.
If non-nil, the function editor will start automatically when
the function definition command is entered. If nil, the
function editor must be opened manually using the function
`gnu-apl-edit-function'.")

(custom-autoload 'gnu-apl-auto-function-editor-popup "gnu-apl-mode" t)

(defvar gnu-apl-redefine-function-when-in-use-action 'ask "\
What action to take when trying to save a function that is on the )SI stack.
This parameter controls the behaviour when an attempt is made
to redefine a function which is already on the )SI stack.
Permitted values are:

    error - Signal an error message
    clear - Clear the )SI stack before editing
    ask - Ask the user what action to take")

(custom-autoload 'gnu-apl-redefine-function-when-in-use-action "gnu-apl-mode" t)

(defvar gnu-apl-show-keymap-on-startup t "\
Choose if the keymap should be automatically displayed.
When non-nil, automatically display the keymap when activating
the GNU APL buffer using the command `gnu-apl'. The keyboard help
buffer can also be toggled using the command
`gnu-apl-show-keyboard'.")

(custom-autoload 'gnu-apl-show-keymap-on-startup "gnu-apl-mode" t)

(defvar gnu-apl-show-apl-welcome t "\
Choose if the GNU APL welcome screen should be displayed.
When non-nil, display the GNU APL welcome screen. When this value
is nil, the apl binary is called with the --silent flag.")

(custom-autoload 'gnu-apl-show-apl-welcome "gnu-apl-mode" t)

(defvar gnu-apl-show-tips-on-start t "\
When non-nil, show some help when starting a new APL session.")

(custom-autoload 'gnu-apl-show-tips-on-start "gnu-apl-mode" t)

(defvar gnu-apl-native-listener-port 0 "\
The port number that the native listener should listen to.
If zero, randomly choose an available port.
If -1, request the use of Unix domain sockets.")

(custom-autoload 'gnu-apl-native-listener-port "gnu-apl-mode" t)

(defvar gnu-apl-gnuplot-program "gnuplot" "\
The name of the gnuplot executable.")

(custom-autoload 'gnu-apl-gnuplot-program "gnu-apl-mode" t)

(defvar gnu-apl-program-extra-args nil "\
List of strings containing extra commandline arguments to pass
to the apl binary.")

(custom-autoload 'gnu-apl-program-extra-args "gnu-apl-mode" t)

(defvar gnu-apl-indent-amounts '(0 2 0 2) "\
The amounts by which to indent lines within APL functions.
The ∇s are always flush-left, as are all lines outside of functions.")

(custom-autoload 'gnu-apl-indent-amounts "gnu-apl-mode" t)

(defface gnu-apl-default nil "\
Face used for APL buffers" :group (quote gnu-apl))

(defface gnu-apl-error '((((class color)) :foreground "red" :inherit gnu-apl-default) (t :inherit gnu-apl-default)) "\
Face used for error messages in the interactive APL buffer" :group (quote gnu-apl))

(defface gnu-apl-user-status-text '((((class color)) :foreground "#ff0080" :inherit gnu-apl-default) (t :inherit gnu-apl-default)) "\
Face used for user diagnostic messages in the interactive APL buffer" :group (quote gnu-apl))

(defface gnu-apl-help '((t :inherit gnu-apl-default)) "\
Face used for displaying text in help buffers" :group (quote gnu-apl))

(defface gnu-apl-kbd-help-screen '((t :inherit gnu-apl-default)) "\
Face used to display the keyboard help popup" :group (quote gnu-apl))

(add-to-list 'auto-mode-alist '("\\.apl\\'" . gnu-apl-mode))

(add-to-list 'interpreter-mode-alist '("apl" . gnu-apl-mode))

;;;***

;;;### (autoloads nil nil ("gnu-apl-documentation.el" "gnu-apl-editor.el"
;;;;;;  "gnu-apl-follow.el" "gnu-apl-input.el" "gnu-apl-mode-pkg.el"
;;;;;;  "gnu-apl-network.el" "gnu-apl-osx-workaround.el" "gnu-apl-plot.el"
;;;;;;  "gnu-apl-refdocs-bsd-license.el" "gnu-apl-spreadsheet.el"
;;;;;;  "gnu-apl-symbols.el" "gnu-apl-util.el") (21582 47366 334798
;;;;;;  816000))

;;;***

(provide 'gnu-apl-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; gnu-apl-mode-autoloads.el ends here

;;; hydra-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "hydra" "hydra.el" (21734 11493 34019 255000))
;;; Generated autoloads from hydra.el

(autoload 'defhydra "hydra" "\
Create a Hydra - a family of functions with prefix NAME.

NAME should be a symbol, it will be the prefix of all functions
defined here.

BODY has the format:

    (BODY-MAP BODY-KEY &rest PLIST)

DOCSTRING will be displayed in the echo area to identify the
Hydra.

Functions are created on basis of HEADS, each of which has the
format:

    (KEY CMD &optional HINT &rest PLIST)

BODY-MAP is a keymap; `global-map' is used quite often.  Each
function generated from HEADS will be bound in BODY-MAP to
BODY-KEY + KEY, and will set the transient map so that all
following heads can be called though KEY only.

The heads inherit their PLIST from the body and are allowed to
override each key.  The keys recognized are :color and :bind.
:color can be:

- red (default): this head will continue the Hydra state.
- blue: this head will stop the Hydra state.
- amaranth (applies to body only): similar to red, but no binding
except a blue head can stop the Hydra state.

:bind can be:
- nil: this head will not be bound in BODY-MAP.
- a lambda taking KEY and CMD used to bind a head

\(fn NAME BODY &optional DOCSTRING &rest HEADS)" nil t)

(put 'defhydra 'lisp-indent-function 'defun)

;;;***

;;;### (autoloads nil nil ("hydra-examples.el" "hydra-pkg.el" "lv.el")
;;;;;;  (21734 11493 76342 163000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; hydra-autoloads.el ends here

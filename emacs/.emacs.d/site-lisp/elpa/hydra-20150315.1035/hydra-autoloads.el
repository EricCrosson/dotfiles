;;; hydra-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "hydra" "hydra.el" (21766 62983 412558 99000))
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
BODY-KEY + KEY (both are strings passed to `kbd'), and will set
the transient map so that all following heads can be called
though KEY only.

CMD is a callable expression: either an interactive function
name, or an interactive lambda, or a single sexp (it will be
wrapped in an interactive lambda).

HINT is a short string that identifies its head.  It will be
printed beside KEY in the echo erea if `hydra-is-helpful' is not
nil.  If you don't even want the KEY to be printed, set HINT
explicitly to nil.

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

It is possible to omit both BODY-MAP and BODY-KEY if you don't
want to bind anything.  In that case, typically you will bind the
generated NAME/body command.  This command is also the return
result of `defhydra'.

\(fn NAME BODY &optional DOCSTRING &rest HEADS)" nil t)

(put 'defhydra 'lisp-indent-function 'defun)

;;;***

;;;### (autoloads nil nil ("hydra-examples.el" "hydra-init.el" "hydra-ox.el"
;;;;;;  "hydra-pkg.el" "lv.el") (21766 62983 465926 419000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; hydra-autoloads.el ends here

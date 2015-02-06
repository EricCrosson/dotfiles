;;; hydra-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "hydra" "hydra.el" (21705 26436 54574 849000))
;;; Generated autoloads from hydra.el

(autoload 'hydra-create "hydra" "\
Create a hydra with a BODY prefix and HEADS with METHOD.
This will result in `global-set-key' statements with the keys
being the concatenation of BODY and each head in HEADS.  HEADS is
an list of (KEY FUNCTION &optional HINT).

After one of the HEADS is called via BODY+KEY, it and the other
HEADS can be called with only KEY (no need for BODY).  This state
is broken once any key binding that is not in HEADS is called.

METHOD is a lambda takes two arguments: a KEY and a COMMAND.
It defaults to `global-set-key'.
When `(keymapp METHOD)`, it becomes:

    (lambda (key command) (define-key METHOD key command))

\(fn BODY HEADS &optional METHOD)" nil t)

(put 'hydra-create 'lisp-indent-function '1)

;;;***

;;;### (autoloads nil nil ("hydra-examples.el" "hydra-pkg.el") (21705
;;;;;;  26436 78241 274000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; hydra-autoloads.el ends here

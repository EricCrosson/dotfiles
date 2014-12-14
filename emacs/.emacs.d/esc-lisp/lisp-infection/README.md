# Lisp Infection

This package is also available on
[Marmalade](http://marmalade-repo.org/). Feel free to install it with
`M-x install-package lisp-infection`, which will help you stay on top
of updates.

General enhancements to the lisp production process. When I started
writing copious amounts of emacs-lisp, I devised several logical
S-expression manipulations to greatly ease the entry of S-expressions
into Emacs.

### Disclaimer

This package is not as cool as paredit or smartparens. If manipulating
S-expressions is your goal, you should probably start there instead.

## Interactive functions

This excerpt from the autoloads offering should give you a good idea
of the functions contained in this package:

```lisp
;; Beginning of lisp-infection-autoloads.el automatically generated autoloads
(autoload 'li/mark-current-defun "lisp-infection" nil t)
(autoload 'li/mark-current-sexp "lisp-infection" nil t)
(autoload 'li/copy-sexp "lisp-infection" nil t)
(autoload 'li/end-of-sexp "lisp-infection" nil t)
(autoload 'li/beginning-of-sexp "lisp-infection" nil t)
(autoload 'li/delete-sexp "lisp-infection" nil t)
(autoload 'li/eval-current-sexp "lisp-infection" nil t)
(autoload 'li/indent-entire-defun "lisp-infection" nil t)
(autoload 'li/eval-and-replace "lisp-infection" nil t)
;; End of lisp-infection.el autoloads
```

## Potential keybindings

Here is one way to bind some of the most useful functions in emacs
lisp, although you should substitute better key combinations if you
can think of them:

```lisp
(global-set-key (kbd "C-c l \\") 'li/indent-entire-defun)
(global-set-key (kbd "C-c l e") 'li/eval-current-sexp)
```

## Installing lisp-infection.el

To install `lisp-infection`, simply download these two .el files and
place in your load-path. The following lines will ensure
`lisp-infection` loads properly:

```lisp
(require 'lisp-infection-autoloads nil 'noerror)
```

Evaluating this same code (`M-x eval-region`) should hotplug
`lisp-infection` into your current session as well.

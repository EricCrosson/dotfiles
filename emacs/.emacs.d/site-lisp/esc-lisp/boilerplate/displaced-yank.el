
;;;###autoload
(defmacro esc/define-displaced-yank (funcname data)
  (let ((funsymbol (intern (format "esc/yank-displaced-%s" funcname)))
        (char (car data))
        (back (or (cadr data) 1)))
    `(defun ,funsymbol (&optional ARG)
       "This defun was generated pragmatically. Docstrings have
not been ironed out yet. See the defun-generating code for more
information."
       (interactive "p")
       (dotimes (i ARG)
         (insert ,char)
         (backward-char ,back)))))

(mapcar* (lambda (function)
           (let ((funcname (car function))
                 (data     (cdr function)))
             (eval `(esc/define-displaced-yank ,funcname ,data))))
         '((parens "()")
           (braces "{}")
           (brackets "[]")
           (brackets-with-colon "[:]")
           (chevrons "<>")
           (quotes "\"\"")
           (stars "**")
           (dollars "$$")
           ;; a good example of code reuse
           (ticks "`'")
           (little-arrow "->" 0)
           (doxygen-comment "/*!  */" 3))) ;; todo: implement with yasnippet

(provide 'displaced-yank)

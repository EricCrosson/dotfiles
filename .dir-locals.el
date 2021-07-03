;; .dir-locals.el in the project root
((sh-mode
  (eval remove-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)))

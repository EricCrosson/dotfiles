This package provides a minor mode that will do two things
after a successful recompile:
1) bury the *compilation* buffer, and
2) restore your window configuration to how it looked when you
issued the recompile.

Usage:

(bury-successful-compilation-mode 1)

Code

(defcustom bury-successful-compilation-precompile-window-state nil
  "Storage for `bury-successful-compilation' to restore
window configuration after a successful compilation."
  :type 'boolean
  :group 'bscb)

(defcustom bury-successful-compilation-save-windows t
  "If nil, the user is attempting to recompile after a failed
attempt. What this means to advice
`bury-successful-compilation-save-window' is now is not
the time to save current-window configuration to variable
`bury-successful-compilation-precompile-window-state'."
  :type 'boolean
  :group 'bscb)

(defadvice compilation-start (before
			      bury-successful-compilation-save-windows
			      activate)
  "Save window configuration to
`bury-successful-compilation-precompile-window-state' unless `bury-successful-compilation-save-windows' is nil."
  (when bury-successful-compilation-save-windows
    (window-configuration-to-register
     bury-successful-compilation-precompile-window-state)))

(defun bury-successful-compilation (buffer string)
  "Bury the compilation BUFFER after a successful compile.
Argument STRING provided by compilation hooks."
  (setq bury-successful-compilation-save-windows
	(and
	 (string-match "compilation" (buffer-name buffer))
	 (string-match "finished" string)
	 (not (search-forward "warning" nil t))))
  (when bury-successful-compilation-save-windows
    (ignore-errors
      (jump-to-register
       bury-successful-compilation-precompile-window-state))
    (message "Compilation successful.")))

(defun bury-successful-compilation-turn-on ()
  "Turn on function `bury-successful-compilation'."
  (ad-enable-advice 'compilation-start 'before
'bury-successful-compilation-save-windows)
  (add-hook 'compilation-finish-functions 'bury-successful-compilation))

(defun bury-successful-compilation-turn-off ()
  "Turn off function `bury-successful-compilation'."
  (setq bury-successful-compilation-precompile-window-state nil)
  (ad-disable-advice 'compilation-start 'before
		     'bury-successful-compilation-save-windows)
  (remove-hook 'compilation-finish-functions
	       'bury-successful-compilation))

###autoload
(define-minor-mode bury-successful-compilation-mode
  "A minor mode to bury the *compilation* buffer upon successful
compilations."
  :init-value nil
  :global t
  :group 'bscb
  (if bury-successful-compilation-mode
      (bury-successful-compilation-turn-on)
    (bury-successful-compilation-turn-off)))

(provide 'bury-successful-compilation)

bury-successful-compilation.el ends here

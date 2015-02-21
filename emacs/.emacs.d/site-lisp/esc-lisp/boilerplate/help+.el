
;; TODO make this insert parens, and the appropriate spaces for
;; arguments. I'm envisioning a clean, cdlatex-mode type thing
;;;###autoload
(defun esc/insert-defun-at-point (&optional key insert untranslated string)
  "Insert at point the name of the function KEY invokes.  KEY is a string.
  If INSERT (the prefix arg) is non-nil, insert the message in the
  buffer.  If non-nil, UNTRANSLATED is a vector of the untranslated
  events.  It can also be a number in which case the untranslated
  events from the last key hit are used.

  If KEY is a menu item or a tool-bar button that is disabled, this command
  temporarily enables it to allow getting help on disabled items and buttons."
  (interactive
   (let ((enable-disabled-menus-and-buttons t)
         (cursor-in-echo-area t)
         saved-yank-menu)
     (unwind-protect
         (let (key)
           ;; If yank-menu is empty, populate it temporarily, so that
           ;; "Select and Paste" menu can generate a complete event.
           (when (null (cdr yank-menu))
             (setq saved-yank-menu (copy-sequence yank-menu))
             (menu-bar-update-yank-menu "(any string)" nil))
           (setq key (read-key-sequence "Describe key (or click or menu item): "))
           ;; Clear the echo area message (Bug#7014).
           (message nil)
           ;; If KEY is a down-event, read and discard the
           ;; corresponding up-event.  Note that there are also
           ;; down-events on scroll bars and mode lines: the actual
           ;; event then is in the second element of the vector.
           (and (vectorp key)
                (let ((last-idx (1- (length key))))
                  (and (eventp (aref key last-idx))
                       (memq 'down (event-modifiers (aref key last-idx)))))
                (read-event))
           (list
            key
            (if current-prefix-arg (prefix-numeric-value current-prefix-arg))
            1))
       ;; Put yank-menu back as it was, if we changed it.
       (when saved-yank-menu
         (setq yank-menu (copy-sequence saved-yank-menu))
         (fset 'yank-menu (cons 'keymap yank-menu))))))

  (if (numberp untranslated)
      (setq untranslated (this-single-command-raw-keys)))
  (let* ((event (if (and (symbolp (aref key 0))
                         (> (length key) 1)
                         (consp (aref key 1)))
                    (aref key 1)
                  (aref key 0)))
         (modifiers (event-modifiers event))
         (standard-output (if insert (current-buffer) standard-output))
         (mouse-msg (if (or (memq 'click modifiers) (memq 'down modifiers)
                            (memq 'drag modifiers)) " at that spot" ""))
         (defn (key-binding key t))
         key-desc)
    ;; Handle the case where we faked an entry in "Select and Paste" menu.
    (if (and (eq defn nil)
             (stringp (aref key (1- (length key))))
             (eq (key-binding (substring key 0 -1)) 'yank-menu))
        (setq defn 'menu-bar-select-yank))
    ;; Don't bother user with strings from (e.g.) the select-paste menu.
    (if (stringp (aref key (1- (length key))))
        (aset key (1- (length key)) "(any string)"))
    (if (and (> (length untranslated) 0)
             (stringp (aref untranslated (1- (length untranslated)))))
        (aset untranslated (1- (length untranslated)) "(any string)"))
    ;; Now describe the key, perhaps as changed.
    (setq key-desc (help-key-description key untranslated))
    (if (or (null defn) (integerp defn) (equal defn 'undefined))
        (princ (format "%s%s is undefined" key-desc mouse-msg))
      (if string
          defn
        (insert (format "%S" defn))))))

(provide 'help+)

;;;###autoload
(defun describe-keymap (keymap)
  "Describe bindings in KEYMAP, a variable whose value is a keymap.
Completion is available for the keymap name."
  (interactive
   (list (intern
          (completing-read
           "Keymap: " obarray
           (lambda (m) (and (boundp m) (keymapp (symbol-value m))))
           t nil 'variable-name-history))))
  (unless (and (symbolp keymap) (boundp keymap) (keymapp (symbol-value keymap)))
    (error "`%S' is not a keymapp" keymap))
  (let ((name  (symbol-name keymap))
        (doc   (documentation-property keymap 'variable-documentation)))
    (help-setup-xref (list #'describe-keymap keymap) (interactive-p))
    (with-output-to-temp-buffer "*Help*"
      (princ name) (terpri)
      (princ (make-string (length name) ?-)) (terpri) (terpri)
      (when doc (princ doc) (terpri) (terpri))
      ;; Use `insert' instead of `princ', so control chars (e.g. \377) insert correctly.
      (with-current-buffer "*Help*"
        (insert (substitute-command-keys (concat "\\{" name "}")))))))

;;; help-extras.el --- Extend the default help system to include source lookup and insertion of symbol-names at point
;; Version: 0.0.20140312

;; Copyright (C) 2015 Eric Crosson

;; Author: Eric Crosson <esc@ericcrosson.com>
;; Keywords: help
;; Package-Version: 0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package completes the extra help functions offered by default
;; GNU Emacs. See Usage for more details on the functions offered.

;; To enable the suggested keybindings (the natural extension of the
;; default help system) enable the minor mode `help-extras',
;; demonstrated below:

;; (help-extras 1)
;;   - or -
;; M-x help-extras

;;; Usage:


;; The default help keys are

;; (bind-key "C-h c" 'describe-key-briefly)
;; (bind-key "C-h k" 'describe-key)
;; (bind-key "C-h f" 'describe-function)
;; (bind-key "C-h v" 'describe-variable)



;; The following bindings extend the default help system to include
;; analagous commands that find the source code defining each lisp
;; construct.

;; (Currently there is no analagous function to find a binding in code
;; -- not sure this is possible yet. It would certainly be useful.)

;; (bind-key "C-h C-k" 'find-function-on-key)
;; (bind-key "C-h C-f" 'find-function)
;; (bind-key "C-h C-v" 'find-variable)

;; This is to say, to find the source defining a lisp construct of
;; interest, invoke the normal help keybinding but continue holding
;; control during the second keypress.

;; I also recommend the miscellaneous bindings

;; (bind-key "C-h C-l" 'find-library)

;; (bind-key "C-h C-M-b" 'describe-function-binding-briefly)


;; The following bindings extend the default help system to include
;; analagous commands that find insert the selected list construct.

;; (bind-key "C-h C-M-c" 'insert-function-on-key)
;; (bind-key "C-h C-M-k" 'insert-key-combination)
;; (bind-key "C-h C-M-f" 'insert-function-name)
;; (bind-key "C-h C-M-v" 'insert-variable)

;; This is to say, to insert the symbol name of the selected construct
;; at point, invoke the normal help keybinding but hold control and
;; meta during the second keypress.

;;; Code:

;;;###autoload
(defun insert-variable (variable)
  "Insert VARIABLE at point.

The library where VARIABLE is defined is searched for in
`find-function-source-path', if non-nil, otherwise in `load-path'.
See also `find-function-recenter-line' and `find-function-after-hook'."
  (interactive (find-function-read 'defvar))
  (insert (symbol-name variable)))

(defun describe-function-binding-briefly (function)
  "Describe the key binding associated with FUNCTION."
  (interactive (find-function-read))
  (describe-function function)
  (save-excursion
  (switch-to-buffer "*Help*")
  (when (re-search-forward "It is bound to \\(.*?\\)\\(\\.\\|\\,\\)")
    (let ((binding (buffer-substring-no-properties (match-beginning 1)
						   (match-end 1))))
      (kill-buffer "*Help*")
      (message "%s is bound by %s" (symbol-name function) binding)))))

;; TODO make this insert parens, and the appropriate spaces for
;; arguments. I'm envisioning a clean, cdlatex-mode type thing

;; TODO: clean up the code, you don't need everything that was copied
;; here
;;;###autoload
(defun insert-function-on-key (&optional key insert untranslated string)
  "Insert at point the name of the function KEY invokes.  KEY is a string.
If INSERT (the prefix arg) is non-nil, insert the message in the
buffer.  If non-nil, UNTRANSLATED is a vector of the untranslated
events.  It can also be a number in which case the untranslated
events from the last key hit are used.

If KEY is a menu item or a tool-bar button that is disabled, this
command temporarily enables it to allow getting help on disabled
items and buttons."
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

;;;###autoload
(defun insert-function-name (function)
  "Insert the name of the FUNCTION at point.

The library where FUNCTION is defined is searched for in
`find-function-source-path', if non-nil, otherwise in `load-path'.
See also `find-function-recenter-line' and `find-function-after-hook'."
  (interactive (find-function-read))
  (insert (symbol-name function)))

;;;###autoload
(defun insert-key-combination (key &optional arg)
  "Insert string describing KEY sequence. KEY is a string.
If ARG is non nil, wrap the inserted string in some useful text
depending on the value of ARG.

Value of ARG      Example inserted string
positive          \"C-h c\"
negative          (kbd \"C-h c\")"
  (interactive "kType a key combination: \np")
  (let ((str (key-description key)))
    (insert (cond ((< arg 0)        (concat "(kbd \"" str "\")"))
		  ((not (eq arg 1)) (concat "\"" str "\""))
		  (t                 str)))))

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
    (help-setup-xref (list #'describe-keymap keymap) (called-interactively-p 'interactive))
    (with-output-to-temp-buffer "*Help*"
      (princ name) (terpri)
      (princ (make-string (length name) ?-)) (terpri) (terpri)
      (when doc (princ doc) (terpri) (terpri))
      ;; Use `insert' instead of `princ', so control chars (e.g. \377) insert correctly.
      (with-current-buffer "*Help*"
        (insert (substitute-command-keys (concat "\\{" name "}")))))))

(defvar help-extras-map (make-keymap)
  "The keymap for function `help-extras'.")

(bind-key "C-h c" 'describe-key-briefly help-extras-map)
(bind-key "C-h k" 'describe-key help-extras-map)
(bind-key "C-h f" 'describe-function help-extras-map)
(bind-key "C-h v" 'describe-variable help-extras-map)

(bind-key "C-h C-k" 'find-function-on-key help-extras-map)
(bind-key "C-h C-f" 'find-function help-extras-map)
(bind-key "C-h C-v" 'find-variable help-extras-map)

(bind-key "C-h C-l" 'find-library help-extras-map)
(bind-key "C-h C-M-b" 'describe-function-binding-briefly help-extras-map)

(bind-key "C-h C-M-c" 'insert-function-on-key help-extras-map)
(bind-key "C-h C-M-k" 'insert-key-combination help-extras-map)
(bind-key "C-h C-M-f" 'insert-function-name help-extras-map)
(bind-key "C-h C-M-v" 'insert-variable help-extras-map)

;;;###autoload
(define-minor-mode help-extras
  "A minor mode to extend the default GNU Emacs help system to
include source lookup and insertion of symbol-names at point."
  :init-value nil
  :global t
  :group 'help
  :keymap help-extras-map)

(provide 'help-extras)

;;; help-extras.el ends here

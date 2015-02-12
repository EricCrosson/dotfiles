
(defcustom esc-line-shift-amount 6
    "The number of lines to shift in `esc-mode-map'."
    :type    'integer
    :options '(5 6)
    :group   'esc-mode)

(defvar esc-mode-map (make-keymap)
    "The keymap for \\[esc-mode].")

(define-minor-mode esc-mode
    "Toggle esc-keys mode.
                 A minor mode so that my key settings override annoying major modes."
    t " esc" 'esc-mode-map)

;; TODO: devise method of keeping these options in sync with the entire current implementation of esc/accompanying-mode-hook
(defcustom esc/accompanying-mode-hook nil
  "Hook that is appended to esc-mode-hook."
  :type         'hook
  :options      '(;; do's
                  (when (fboundp 'ido-mode) (ido-mode (esc-mode-enabledp)))
                  (when (fboundp 'eldoc-mode) (eldoc-mode (esc-mode-enabledp)))
                  (when (fboundp 'winner-mode) (winner-mode (esc-mode-enabledp)))
                  (when (fboundp 'keyfreq-mode) (keyfreq-mode (esc-mode-enabledp)))
                  (when (fboundp 'recentf-mode) (recentf-mode (esc-mode-enabledp)))
                  (when (fboundp 'icomplete-mode) (icomplete-mode (esc-mode-enabledp)))
                  (when (fboundp 'guide-key-mode) (guide-key-mode (esc-mode-enabledp)))
                  (when (fboundp 'auto-fill-mode) (auto-fill-mode (esc-mode-enabledp)))
                  (when (fboundp 'show-paren-mode) (show-paren-mode (esc-mode-enabledp)))
                  (when (fboundp 'line-number-mode) (line-number-mode (esc-mode-enabledp)))
                  (when (fboundp 'display-time-mode) (display-time-mode (esc-mode-enabledp)))
                  (when (fboundp 'column-number-mode) (column-number-mode (esc-mode-enabledp)))
                  (when (fboundp 'which-function-mode) (which-function-mode (esc-mode-enabledp)))
                  (when (fboundp 'global-hl-line-mode) (global-hl-line-mode (esc-mode-enabledp)))
                  (when (fboundp 'display-battery-mode) (display-battery-mode (esc-mode-enabledp)))
                  (when (fboundp 'autopair-global-mode) (autopair-global-mode (esc-mode-enabledp)))
                  (when (fboundp 'auto-compression-mode) (auto-compression-mode (esc-mode-enabledp)))
                  (when (fboundp 'global-on-screen-mode) (global-on-screen-mode (esc-mode-enabledp)))
                  (when (fboundp 'global-font-lock-mode) (global-font-lock-mode (esc-mode-enabledp)))
                  (when (fboundp 'global-auto-revert-mode) (global-auto-revert-mode (esc-mode-enabledp)))
                  (when (fboundp 'global-rainbow-delimiters-mode) (global-rainbow-delimiters-mode (esc-mode-enabledp)))
                  ;; dont's
                  (when (fboundp 'tool-bar-mode) (tool-bar-mode (dont (esc-mode-enabledp))))
                  (when (fboundp 'menu-bar-mode) (menu-bar-mode (dont (esc-mode-enabledp))))
                  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode (dont (esc-mode-enabledp))))
                  (when (fboundp 'blink-cursor-mode) (blink-cursor-mode (dont (esc-mode-enabledp))))
                  (when (fboundp 'transient-mark-mode) (transient-mark-mode (dont (esc-mode-enabledp)))))
  :group        'esc-mode)

(defmacro esc-mode-enabledp ()
    "A macro to determine if \\[esc-mode] is currently enabled."
    `(progn (and (boundp 'esc-mode) esc-mode)))

(defmacro add-or-remove-hook (hook function)
  "A macro to add a HOOK to FUNCTION, if `esc-mode' is being enabled;
          or remove a HOOK from FUNCTION, if `esc-mode' is being disabled."
  `(progn (if (esc-mode-enabledp)
              (add-hook ,hook ,function)
            (remove-hook ,hook ,function))))

(after 'diminish-autoloads
  (defmacro diminish-or-restore (mode)
    "A macro to diminish a MODE, if `esc-mode' is being enabled;
          or disable a MODE, if `esc-mode' is being disabled."
    `(progn
       (after 'diminish-autoloads
         (if (esc-mode-enabledp)
             (diminish ,mode)
           (diminish-undo ,mode))))))

(defmacro dont (operand)
  "A macro to avoid awkward, unintuitive code in \\[esc-accompanying-mode-hook].
          OPERAND is an integer to de/activate a given mode."
  `(progn
     (if (numberp ,operand)
         (* (- 1) ,operand)
       (if ,operand nil t))))

(defmacro esc-key (sequence action)
  "A macro to bind SEQUENCE to ACTION in `esc-mode-map'."
  `(define-key esc-mode-map (kbd ,sequence) ,action))

(defmacro esc-keys (&rest binding-list)
  "A macro to bind all keybindings and functions in BINDING-LIST
in `esc-mode-map'.

This macro runs conses through \\[esc-key] for convenience."
  (declare (indent defun))
  `(mapc (lambda (binding) (esc-key (car binding) (cdr binding)))
         '(,@binding-list)))

;; Enhanced buffer movement (not point movement)
(esc-keys
  ("C-," . esc/scroll-up-slight)
  ("C-." . esc/scroll-down-slight))

;; Enhanced keybindings
(esc-keys
  ("C-'"     . execute-extended-command)
  ("M-g"     . esc/goto-line-with-feedback)
  ("C-j"     . newline-and-indent) ;for consistency in *scratch*
  ("C-a"     . esc/back-to-indentation-or-beginning)
  ("s-z"     . other-window)
  ("C-x 4 k" . esc/mode-line-other-buffer-other-window)
  ("C-x 4 9" . esc/bury-buffer-delete-window)
  ("M-x"     . execute-extended-command)
  ("C-'"     . query-replace)
  ("C-x M-r" . revert-buffer-no-confirm)
  ("C-x 2"   . esc/vsplit-last-buffer)
  ("C-x 3"   . esc/hsplit-last-buffer)
  ("M-s o"   . occur-dwim))

;; Search for current word up or down from point
(esc-keys
  ("<M-down>" . esc/search-word-forward)
  ("<M-up>" . esc/search-word-backward))

;; Equivalent to middle-click yank on mouse
(esc-key "C-c y" 'esc/middle-click-yank)

;; Windmove from shift keys
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Window adjustment
(esc-keys
  ("C-c ["   . esc/rotate-window-split)
  ("C-c ]"   . esc/swap-buffer-locations)
  ;; TODO: rename defun
  ("C-M-c [" . esc/should-have-opened-this-in-other-window)
  ("C-M-c ]" . esc/toggle-window-selectability))

;; Org mode keybindings
(after 'org
  (define-key org-mode-map (kbd "C-c n") 'esc/org-next-source-code-block)
  (define-key org-mode-map (kbd "C-c p") 'esc/org-prev-source-code-block))

;; Miscellaneous keybindings
(esc-keys
  ("C-c a"   . org-agenda)
  ("M-K"     . kill-sentence)
  ("M-Z"     . zap-up-to-char) ; up-to, life saver
  ("M-j"     . just-one-space) ; conflicts with spotlight
  ("C-c k"   . esc/copy-line)
  ("<f6>"    . spray-mode)
  ("C-S-l"   . esc/toggle-letter-case)
  ("M-P"     . align-regexp)
  ("C-c e"   . esc/eval-and-replace)
  ("C-c C-b" . mode-line-other-buffer)
  ;; TODO: change sexp wrapping (i.e. {}[]()) from Xah Lee
  )

;; f-related-keybindings
(esc-keys
  ("C-x F"   . recentf-open-files)
  ("C-x f"   . esc/toggle-selective-display)
  ("C-x M-f" . single/ff-in-single-mode))

;; Fold-this keybindings. Thanks again [[https://github.com/magnars/fold-this.el][Magnar]]!
(esc-keys
  ("C-c f"   . fold-this)
  ("C-c F"   . fold-this-unfold-all))

;; ido-extras keybindings
(esc-keys
  ("M-i"       . ido-goto-symbol)
  ("C-x C-r"   . ido-recentf-open)
  ("C-x C-S-r" . find-file-read-only))

;; Programming bindings
(esc-keys
  ("C-c m" . compile)
  ("C-c C-m" . recompile))

;; C-q cluster
(esc-keys
  ("C-q"     . delete-region)  ;like C-w, but gone
  ("C-c q"   . auto-fill-mode) ;more frequented than
  ("C-c C-q" . quoted-insert))   ;this command


;; w3m bindings
(after 'w3m-autoloads
  (esc-keys
    ("C-x j"   . w3m-google-new-tab)
    ("C-x J"   . w3m-wiki-new-tab)
    ("C-x C-m" . w3m-browse-url)))

;; Text movement
(after 'move-text-autoloads
  (esc-keys
    ("<C-S-up>"     . move-text-up)
    ("<C-S-down>"   . move-text-down)
    ("<C-return>"   . esc/open-line-below)
    ("<C-S-return>" . esc/open-line-above)))

;; LaTeX bindings
(esc-key "C-c l b" 'esc/insert-latex-block)

;; Minimap bindings
(esc-key "C-c M-m" 'esc/minimap-toggle)

;; Ace jump mode. Like an ace
(after 'ace-jump-mode-autoloads
  (esc-keys
    ("C-c SPC" . ace-jump-mode)
    ("C-x SPC" . ace-jump-mode-pop-mark)))

;; Ace jump buffer, nice in a pinch
(after 'ace-jump-buffer
  (esc-key "C-M-S-l" 'ace-jump-buffer))

;; Ace window. Genius!
(after 'ace-window
   (esc-key "C-M-o" 'ace-window)
   ;; how unfriendly! This replaces \\[split-line].
   (esc-key "C-M-O" 'split-line))

;; Mark commands
(esc-keys
  ("C-x m" . pop-to-mark-command))

;; Hide-lines bindings
(after 'hide-lines-autoloads
  (esc-key "C-c h" 'hide-lines)
  (esc-key "C-c M-h" 'esc/reveal-all-hidden-lines))

(esc-key "C-;" 'comment-dwim)

(after 'multiple-cursors-autoloads
  (esc-keys
    ("C-S-c C-S-c" . mc/edit-lines)
    ("C->" . mc/mark-next-like-this)
    ("C-<" . mc/mark-previous-like-this)
    ("C-c !" . mc/sort-regions)    ;1
    ("C-c @" . mc/reverse-regions) ;2
    ("C-c #" . mc/insert-numbers)  ;3
    ("C-c *" . mc/mark-all-like-this)
    ("C-c C-*" . mc/mark-all-like-this-dwim)))

;; Lisp/sexp movement
(esc-keys
  ("C-S-k"    . kill-sexp)
  ("C-c l \\" . li/indent-entire-defun)
  ("C-s-e"    . li/eval-current-defun)
  ("C-c l e"  . li/eval-current-sexp))
;; Preserving stock org functionality
(esc-key "C-c l l" 'org-store-link)

;; single-mode != vi
(esc-keys
  ("C-c l s"  . single-mode)
  ("<escape>" . single-mode))

;; Buffer control
(esc-keys
  ;("C-x C-b" . ibuffer)              ; use font-lock with buffer lists
  ;TODO: replace this with something ok (was just using this at the time of )
  ; editing and didn't know what to use this key for
  ("C-x C-b" . global-git-gutter+-mode) ; every invocation was accidental
  ("C-x M-b" . bury-buffer)
  ("C-x M-B" . esc/bury-other-buffer)
  ("C-c o"   . clone-indirect-buffer-other-window)
  ("C-c C-o" . ff-find-other-file)
  ("C-o"     . ace-window))

(after 'expand-region-autoloads      ;Three guesses [[https://github.com/magnars/expand-region.el][who]]
  (esc-key "C-=" 'er/expand-region)) ;wrote this package

;; Help+
(esc-keys
  ("C-h C-f"   . find-function)
  ("C-h C-k"   . find-function-on-key)
  ("C-h C-v"   . find-variable)
  ("C-h C-l"   . find-library)
  ("C-h C-n"   . esc/insert-defun-at-point)
  ("C-h M-k"   . describe-keymap)
  ("C-h C-M-c" . esc/insert-key-combination))

;; Un- and re- doing
(esc-keys
  ("C-c /"   . goto-last-change))

;; Font maniplation
(esc-keys
  ("C-M-<" . esc/zoom-out)
  ("C-M->" . esc/zoom-in))

;;; Function keys
(esc-key "<f7>" 'scroll-all-mode)
(esc-key "<f8>" 'follow-delete-other-windows-and-split)
(esc-key "<f9>" 'flyspell-buffer)

;; Helm
(after 'helm-autoloads
  (esc-keys
    ("M-s-x"   . helm-M-x)
    ("C-c i"   . helm-imenu)
    ("C-x C-j" . helm-for-files)))

;; Sysadmin bindings
(esc-keys
  ("C-M-+"     . esc/search-my-lisp-dir)
  ("C-c C-i"   . esc/edit-my-emacs)
  ("C-c C-M-i" . esc/edit-my-bash))

;; esc delimeter and line hacks
(esc-keys
  ("C-<backspace>" . esc/backward-kill-line)
  ("M-k"   . esc/pull-up-line)
  ("C-M--" . esc/insert-little-arrow)
  ("C-M-j" . esc/insert-surrounding-parens)
  ("C-M-k" . esc/insert-surrounding-braces)
  ("C-M-|" . esc/insert-surrounding-pipes)
  ("C-M-l" . esc/insert-surrounding-brackets)
  ("C-M-;" . esc/insert-surrounding-brackets-with-colon)
  ("C-M-," . esc/insert-surrounding-chevrons)
  ("C-M-'" . esc/insert-surrounding-quotes)
  ("C-M-*" . esc/insert-surrounding-stars)
  ("C-M-g" . esc/insert-surrounding-dollars)
  ("C-M-`" . esc/insert-surrounding-ticks))

;; programming delimeter and line hacks
(esc-keys
  ("M-'"   . toggle-quotes))

;; Buffer-overlay hacks: super useful!
(esc-keys
 ("s-e" . esc/raise-eshell)
 ("s-s" . esc/raise-ansi-term)
 ("s-q" . esc/raise-magit-status))

;; The proper definition of `esc-mode-hook'
(defun esc/accompanying-mode-hook ()
  "esc's customizations added to \\[esc-mode-hook]."
  (let ((activate-bool (esc-mode-enabledp)))
    (setq activate (if activate-bool 1 -1))
    ;; esc-mode wouldn't be complete without these helper modes

    ;; activate these modes
    (mapc (lambda (mode) (when (fboundp mode)
                             (ignore-errors (funcall mode activate))))
          '(ido-mode
            eldoc-mode
            winner-mode
            keyfreq-mode
            recentf-mode
            icomplete-mode
            guide-key-mode
            auto-fill-mode
            show-paren-mode
            line-number-mode
            display-time-mode
            column-number-mode
            which-function-mode
            global-hl-line-mode
            display-battery-mode
            autopair-global-mode
            auto-compression-mode
            global-on-screen-mode
            global-font-lock-mode
            global-auto-revert-mode
            global-rainbow-delimiters-mode))

    ;; deactivate these modes
    (mapc (lambda (mode) (when (fboundp mode)
                             (funcall mode (dont activate))))
          '(tool-bar-mode
            menu-bar-mode
            scroll-bar-mode
            blink-cursor-mode
            transient-mark-mode))

    ;; esc-mode enjoys these settings also
    (when (fboundp 'global-git-gutter-mode)
      (global-git-gutter-mode activate-bool))
    (setq-default save-place       activate-bool
                  abbrev-mode      activate-bool
                  auto-revert-mode activate-bool)
    (diminish 'auto-revert-mode)
    (setq
     display-time-24hr-format             activate-bool
     global-auto-revert-non-file-buffers  activate-bool
     next-line-add-newlines               activate-bool
     kill-whole-line                      activate-bool
     vc-follow-symlinks                   activate-bool
     search-highlight                     activate-bool
     query-replace-highlight              activate-bool
     auto-revert-verbose                  (not activate-bool)
     confirm-nonexistent-file-or-buffer   (not activate-bool))

    ;; TODO: this was determined to have a bug in it. Handle the bug
    ;;(setq revert-buffer-function (if (esc-mode-enabledp)
    ;;                                 'revert-buffer-keep-undo
    ;;                               'revert-buffer))

    ;; TODO: clean up this garbage (but keep functionality)
    ;;   - possibility: wrap with 'ignore-errors
    (if (<= emacs-major-version 23)
        (message "Old Emacs prevents visual-line-mode, auto-complete-mode")
      (global-visual-line-mode activate) ;word wrap by default
      (diminish 'visual-line-mode)
      (after 'auto-complete
        (global-auto-complete-mode activate))) ;in all buffers

    (after 'undo-tree-autoloads
      (global-undo-tree-mode activate)
      (setq undo-tree-visualizer-timestamps t
            undo-tree-visualizer-relative-timestamps t))

    ;; less clutter on the mode line
    ;; TODO - why the fuck did this stop working???? time to bisect
    ;; (mapc (lambda (dim) (after (car dim) (diminish-or-restore (cdr dim))))
    ;;    '((abbrev           . abbrev-mode)
    ;;      (autopair         . autopair-mode)
    ;;      (auto-complete    . auto-complete-mode)
    ;;      (fic-mode         . fic-mode)
    ;;      (enh-ruby-mode    . enh-ruby-mode)
    ;;      (smerge-mode      . smerge-mode)
    ;;      (git-gutter+-mode . git-gutter+-mode)
    ;;      (org-indent       . org-indent-mode)
    ;;      (undo-tree        . undo-tree-mode)))
    ))

(add-hook 'esc-mode-hook 'esc/accompanying-mode-hook)

(esc-mode 1)
;(diminish-or-restore 'esc-mode) ;in the background

(provide 'esc-mode)


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

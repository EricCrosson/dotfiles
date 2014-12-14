(defvar single-mode-map (make-keymap)
  "The keymap for \\[single-mode].")

(define-minor-mode single-mode
  "Toggle single-mode.
A minor mode for quick navigation- reinventing the vim wheel."
  nil " single" 'single-mode-map
  (suppress-keymap single-mode-map))
(add-hook 'single-mode-hook 'single/single-mode-hook)

(defvar single-line-shift-amount 6
  "The number of lines to shift in esc-mode-map.")

(defvar single-restore-nil-read-only-state nil
  "This indicates we need to restore a state of (read-only-mode -1).
This variable is nil by default.")

(define-key single-mode-map (kbd "'") 'single/quit-single-mode)
(define-key single-mode-map (kbd "<escape>") 'single/quit-single-mode)
(define-key single-mode-map (kbd "j") 'single/scroll-up)
(define-key single-mode-map (kbd "k") 'single/scroll-down)
(define-key single-mode-map (kbd ",") 'beginning-of-buffer)
(define-key single-mode-map (kbd ".") 'end-of-buffer)
(define-key single-mode-map (kbd "5") 'single/query-replace)
(define-key single-mode-map (kbd "Z") 'single/undo)
(define-key single-mode-map (kbd "q") 'single/read-only-mode)
(define-key single-mode-map (kbd "`") 'single/iedit-mode)
(define-key single-mode-map (kbd "K") 'single/kill-current-buffer)

(define-key single-mode-map (kbd "`") 'kill-current-buffer)
(define-key single-mode-map (kbd "SPC") 'ace-jump-mode)
(define-key single-mode-map (kbd "x") 'execute-extended-command)
(define-key single-mode-map (kbd "p") 'scroll-down)
(define-key single-mode-map (kbd "n") 'scroll-up)
(define-key single-mode-map (kbd "9") 'end-of-buffer)
(define-key single-mode-map (kbd "0") 'beginning-of-buffer)
(define-key single-mode-map (kbd "s") 'isearch-forward)
(define-key single-mode-map (kbd "r") 'isearch-backward)
(define-key single-mode-map (kbd "e") 'eval-region)

(provide 'single-mode-autoloads)

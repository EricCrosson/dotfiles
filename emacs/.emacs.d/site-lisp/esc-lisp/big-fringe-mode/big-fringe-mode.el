
(defvar big-fringe-mode nil "State of big-fringe-mode")

;;:###autoload
(define-minor-mode big-fringe-mode
  "Minor mode to use big fringe in the current buffer."
  :init-value nil
  :global t
  :variable big-fringe-mode
  :group 'editing-basics
  (if (not big-fringe-mode)
      (set-fringe-style nil)
    (set-fringe-mode
     (/ (- (frame-pixel-width)
           (* 100 (frame-char-width)))
        2))))

Global minor mode to highlight the current thing under point. Uses built-in
thingatpt and hi-lock functionality to identify the thing under point and
highlight it. Does not require font-lock to be enabled as hi-lock falls back
to overlays.

More information: https://github.com/fgeller/highlight-thing.el

(require 'thingatpt)

(defvar highlight-thing-what-thing 'symbol
  "What kind of thing to highlight. (cf. `thing-at-point')")

(defvar highlight-thing-last-thing nil
  "Last highlighted thing.")

(defvar highlight-thing-last-buffer nil
  "Buffer where last thing was highlighted.")

(defvar highlight-thing-delay-seconds 0.5
  "Time to wait before highlighting thing at point.")

(defvar highlight-thing-timer nil
  "Timer that triggers highlighting.")

(defvar highlight-thing-excluded-major-modes nil
  "List of major modes to exclude from highlighting.")

(defun highlight-thing-loop ()
  (cond (highlight-thing-mode (highlight-thing-do))
	(t (highlight-thing-deactivate))))

(defun highlight-thing-deactivate ()
  (highlight-thing-remove-last)
  (when highlight-thing-timer (cancel-timer highlight-thing-timer)))

(defun highlight-thing-regexp (thing)
  (cond ((eq highlight-thing-what-thing 'symbol) (concat "\\_<" (regexp-quote thing) "\\_>"))
	((eq highlight-thing-what-thing 'word) (concat "\\<" (regexp-quote thing) "\\>"))
	(t (regexp-quote thing))))

(defun highlight-thing-remove-last ()
  (when (and highlight-thing-last-thing
	     highlight-thing-last-buffer
	     (buffer-live-p highlight-thing-last-buffer))
    (with-current-buffer highlight-thing-last-buffer
      (hi-lock-unface-buffer (highlight-thing-regexp highlight-thing-last-thing)))))

(defun highlight-thing-should-highlight ()
  (and (not (minibufferp))
       (not (member major-mode highlight-thing-excluded-major-modes))))

(defun highlight-thing-do ()
  (interactive)
  (let* ((thing (thing-at-point highlight-thing-what-thing)))
    (highlight-thing-remove-last)
    (when (and (highlight-thing-should-highlight) thing)
      (highlight-regexp (highlight-thing-regexp thing))
      (setq highlight-thing-last-buffer (current-buffer))
      (setq highlight-thing-last-thing thing))))

###autoload
(define-minor-mode highlight-thing-mode
  "Minor mode that highlights things at point"
  nil " hlt" nil
  :global t :group 'highlight-thing
  (setq highlight-thing-timer (run-with-idle-timer highlight-thing-delay-seconds t 'highlight-thing-loop)))

(provide 'highlight-thing)

highlight-thing.el ends here

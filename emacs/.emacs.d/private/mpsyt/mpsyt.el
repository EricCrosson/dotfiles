;;; myspt.el -- interface with youtube
;; Authors: pc, esc
;; Date: 2015-08-13
;; Version: 0.2

(defun pc/short-url-at-point ()
  "Gets the short url at point.

This function is required only because
`thing-at-point-url-at-point' ignores urls (without a scheme)
that don't start with www."
  (or
   (ignore-errors
     (let ((bounds (thing-at-point-bounds-of-url-at-point t)))
       (when (and bounds (apply #'< bounds))
         (buffer-substring-no-properties (car bounds) (cdr bounds)))))
   "no url found"))

(defun pc/mpsyt-url (url)
  (let ((buffer (current-buffer))
        (mpsyt-proc-name "*mpsyt*"))

    ;; Start a new term with *mpsyt* if there isn't one
    (unless (get-process mpsyt-proc-name)
      (when (get-buffer mpsyt-proc-name)
        (kill-buffer (get-buffer mpsyt-proc-name)))
      (ansi-term "mpsyt" "mpsyt"))

    ;; Play given url in mpsyt
    (let ((mpsyt-proc (get-process mpsyt-proc-name)))
      ;; If something is already playing, stop it and play this...
      (term-send-string mpsyt-proc "\n\n\n")
      ;; since looking for the prompt seems to fail, sometimes?
      (sleep-for 1)
      (term-send-string mpsyt-proc "\n")

      ;; Actually send the command to playurl
      (term-simple-send (get-process mpsyt-proc-name)
                        (format "playurl %s" url)))

    (switch-to-buffer buffer)))

(defun pc/is-youtube-url (string)
  "True if SRING is a YouTube url."
  (string-match "\\.\\(com\\|be\\)" string))

(defun pc/mpsyt-url-dwim ()
  "Play a URL in mpsyt. This defun looks for a url at point, then
in the kill-ring."
  (interactive)
  (let* ((url-at-point (or (url-get-url-at-point)
                           (pc/short-url-at-point)))
         (url (if (pc/is-youtube-url url-at-point)
                  url-at-point
                (current-kill 0 t))))
    (if (pc/is-youtube-url url)
        (pc/mpsyt-url url)
      (message "Unable to detect a url to stream."))))

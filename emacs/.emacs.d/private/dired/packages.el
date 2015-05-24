;;; packages.el --- dired Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar dired-packages '(dired-details+))

(defun dired/init-dired-details+ ()
  "Initialize `dired-details+'."
  (setq dired-details-propagate-flag nil)

  (after "dired-aux"
    (setq dired-free-space-args "-Ph")
    (setq dired-guess-shell-alist-user '(("\\.mp4$" "vlc" "mplayer")
                                         ("\\.pdf$" "evince")))
    (add-to-list 'dired-compress-file-suffixes '("\\.zip$" "unzip")))
  (setq dired-listing-switches "-alhv")
  (setq dired-recursive-copies 'always)
  (after "ibuf-ext"
    (add-to-list 'ibuffer-saved-filter-groups '("default" ("dired" (mode . dired-mode)))))

  ;; Allow running multiple async commands simultaneously
  (defadvice shell-command (after shell-in-new-buffer
                             (command &optional output-buffer error-buffer))
    (when (get-buffer "*Async Shell Command*")
      (with-current-buffer "*Async Shell Command*"
        (rename-uniquely))))
  (ad-activate 'shell-command)

  (setq dired-dwim-target t))

;;; packages.el --- dired Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar dired-packages '(dired+))

(defun dired/init-dired+ ()
  "Initialize `dired+'."

  (setq diredp-hide-details-initially-flag t)
  (use-package dired+)
  (use-package dired-x
    :config
    (setq-default dired-omit-files-p t)
    (setq dired-omit-files
          (concat dired-omit-files "\\|\\.pyc$\\|\\.elc$")))

  (after "dired-aux"
    (setq dired-free-space-args "-Ph")
    (setq dired-guess-shell-alist-user '(("\\.mp4$" "cvlc" "mplayer")
                                         ("\\.avi$" "cvlc" "mplayer")
                                         ("\\.mkv$" "cvlc" "mplayer")
                                         ("\\.pdf$" "evince" "zathura")
                                         ("\\.tar.bz2" "dtrx -n --one=here" "tar jxvf")
                                         ("\\.tar.gz" "dtrx -n --one=here" "tar xzvf")
                                         ("\\.rar" "dtrx -n --one=here" "unrar e")
                                         ("\\.zip" "dtrx -n --one=here")
                                         ("\\.*$" "xdg-open")))
    (add-to-list 'dired-compress-file-suffixes '("\\.zip$" "unzip")))
  (setq dired-listing-switches "-lhv")
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (after "ibuf-ext"
    (add-to-list 'ibuffer-saved-filter-groups
                 '("default" ("dired" (mode . dired-mode)))))

  ;; Allow running multiple async commands simultaneously
  (defadvice shell-command (after shell-in-new-buffer
                             (command &optional output-buffer error-buffer))
    (when (get-buffer "*Async Shell Command*")
      (with-current-buffer "*Async Shell Command*" (rename-uniquely))))
  (ad-activate 'shell-command)

  (evil-leader/set-key
    "d" (defun dired-here ()
          (interactive)
          (let ((cwd (file-name-directory (or (buffer-file-name) ""))))
            (cond
             ((and cwd (file-exists-p cwd))
              (dired cwd))
             (t
              (message "I'm not sure which dir to view."))))))

  (setq dired-dwim-target t)
  (after "dired"
    (defun dired-find-parent-directory ()
      (interactive)
      (find-alternate-file ".."))
    (define-key dired-mode-map (kbd "<right>") 'dired-find-alternate-file)
    (define-key dired-mode-map (vector 'remap 'evil-forward-char) 'dired-find-alternate-file)
    (define-key dired-mode-map (kbd "<left>") 'dired-find-parent-directory)
    (define-key dired-mode-map (vector 'remap 'evil-backward-char) 'dired-find-parent-directory)

    (define-key dired-mode-map (vector 'remap 'beginning-of-buffer)
      (defun dired-back-to-top ()
        (interactive)
        (beginning-of-buffer)
        (unless (search-forward ".." nil 'noerror)
          (beginning-of-buffer))
        (dired-next-line 1)))

    (define-key dired-mode-map (vector 'remap 'end-of-buffer)
      (defun dired-jump-to-bottom ()
        (interactive)
        (end-of-buffer)
        (dired-next-line -1)))))

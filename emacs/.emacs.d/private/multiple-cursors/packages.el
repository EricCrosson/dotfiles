;;; packages.el --- multiple-cursors Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq multiple-cursors-packages '(multiple-cursors))

(defun multiple-cursors/init-multiple-cursors ()
  "Initialize `multiple-cursors'."
  (use-package multiple-cursors
    :defer t
    :config
    (defvar jc/mc-search--last-term nil)
    (defun jc/mc-search (search-command)
      ;; Read new search term when not repeated command or applying to fake cursors
      (when (and (not mc--executing-command-for-fake-cursor)
                 (not (eq last-command 'jc/mc-search-forward))
                 (not (eq last-command 'jc/mc-search-backward)))
        (setq jc/mc-search--last-term (read-from-minibuffer "Search: ")))
      (funcall search-command jc/mc-search--last-term))
    (defun jc/mc-search-forward ()
      "Simplified version of forward search that supports multiple cursors."
      (interactive)
      (jc/mc-search 'search-forward))
    (defun jc/mc-search-backward ()
      "Simplified version of backward search that supports multiple cursors."
      (interactive)
      (jc/mc-search 'search-backward))
    (after 'multiple-cursors
      (bind-key "C-s" 'jc/mc-search-forward mc/keymap)
      (bind-key "C-r" 'jc/mc-search-backward mc/keymap))

    ;; TODO: fd escapes from multiple-cursors, the next escapes emacs mode

    (evil-leader/set-key
      ".i" 'mc/edit-lines
      ".h" 'set-rectangular-region-anchor
      ;; TODO: enter a micro-state for selection
      ".j" 'mc/mark-next-like-this
      ".k" 'mc/mark-previous-like-this
      ".a" 'mc/mark-all-like-this
      ".d" 'mc/mark-all-like-this-dwim
      ".n" 'mc/insert-numbers
      ".s" 'mc/sort-regions
      ".r" 'mc/reverse-regions
      ".e" 'mc/edit-ends-of-lines
      ".b" 'mc/edit-beginnings-of-lines)))

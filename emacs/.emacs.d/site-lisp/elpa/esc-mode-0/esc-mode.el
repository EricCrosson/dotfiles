;;; esc-mode.el --- esc's settings
;; Version: 0.0.20140312

;; Copyright (C) 2015 Eric Crosson

;; Author: Eric Crosson <esc@ericcrosson.com>
;; Keywords: esc
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

;; This package provides a mode for esc's settings, which may be
;; toggled entirely at any point.

;;; Usage:

;; (esc-mode 1)
;;    - or -
;; M-x esc-mode

;;; Code:

(defgroup esc nil
  "esc's namespace."
  :group 'esc)

(defcustom esc/favorite-directories
  '("~/workspace/ee445m-labs" "~/workspace/classes")
  "List of favorite directories.
Used in `esc/visit-favorite-dir'. The order here
affects the order that completions will be offered."
  :type '(repeat directory)
  :group 'esc)

(defcustom esc/associated-modes nil
  "List of modes associated with `esc-mode'. In other words,
`esc-mode' enables these modes on entry and restores their
prior respective states on exit."
  :type '(repeat function)
  :group 'esc)

(defcustom esc/disassociated-modes nil
  "List of modes NOT associated with `esc-mode'. In other words,
`esc-mode' disables these modes on entry and restores their
prior respective states on exit."
  :type '(repeat function)
  :group 'esc)

(defcustom esc/associated-modes-state '()
  "List of states of `esc/associated-modes' upon entry of
`esc-mode'. These states will be restored upon the exit of
`esc-mode'."
  :group 'esc)

(defvar esc-mode-map (make-keymap)
    "The keymap for `esc-mode'.")

(require 'esc-defuns)
(require 'esc-hooks)

;;; esc-mode key bindings

;; Remaps -- wherever they may be bound
(esc-replace-key 'goto-line 'goto-line-with-feedback)
(esc-replace-key 'split-window-right 'esc/hsplit-last-buffer)
(esc-replace-key 'split-window-below 'esc/vsplit-last-buffer)

;; todo: pull buffer management into another hydra
;; Buffer management
(esc-key "C-x 4 k"    'mode-line-other-buffer-other-window)
(esc-key "C-c C-M-\\" 'indent-buffer)

;; Enhanced keybindings
(esc-key "C-a"     'back-to-indentation-or-beginning)
(esc-key "M-s o"   'occur-dwim)

(esc-key "C-c y" 'middle-click-yank)

;; Miscellaneous keybindings
(esc-key "C-c C-b" 'mode-line-other-buffer)
(esc-key "M-K"     'kill-sentence)
(esc-key "M-j"     'just-one-space) ; conflicts with spotlight
(esc-key "C-c k"   'copy-line)
(esc-key "M-P"     'align-regexp)
(esc-key "C-'"     'query-replace)
;; TODO: change sexp wrapping (esc-key i 'e. {}[]())

;; Programming bindings
(esc-key "C-c m" 'compile)
(esc-key "C-c C-m" 'recompile)

;; C-q cluster
(esc-key "C-q" 'delete-region)     ;like C-w, but gone forever.
(esc-key "C-c q" 'auto-fill-mode)  ;more frequented than
(esc-key "C-c C-q" 'quoted-insert) ;  this command

;; newline creation
(esc-key "<C-return>"   'open-line-below)
(esc-key "<C-S-return>" 'open-line-above)
(esc-key "C-M-O"        'split-line)
(esc-key "M-k"          'pull-up-line)

 ;; Mark commands
(esc-key "C-x m" 'pop-to-mark-command)

;; Lisp/sexp movement
(esc-key "C-S-k" 'kill-sexp)

;; Buffer control
(esc-key "C-x o" 'ace-window)
(esc-key "C-x M-b" 'bury-buffer)
(esc-key "C-c o" 'clone-indirect-buffer-other-window)
(esc-key "C-c C-o" 'ff-find-other-file)

;;; Function keys
(esc-key "<f5>"  'minibuffer-display-buffer-file-name)
(esc-key "<f6>"  'spray-mode)
(esc-key "<f8>"  'follow-mode-80-char-compliant)
(esc-key "<f9>"  'flyspell-buffer)
(esc-key "<f10>" 'golden-ratio-mode)

(esc-key "C-c f" 'esc/visit-favorite-dir)

(esc-key "s-q" 'esc/raise-magit-status)

(setq hydra-lv nil)

(defhydra hydra-text-scale (esc-mode-map  "<f2>")
  "text-scale"
  ("j" text-scale-decrease "dec")
  ("k" text-scale-increase "inc")
  ("." default-text-scale-increase "default-dec")
  ("," default-text-scale-decrease "default-inc"))

(defhydra hydra-eval (esc-mode-map "C-c C-e")
  "eval"
  ("r" eval-region "region")
  ("b" eval-buffer "buffer")
  ("s" eval-last-sexp "sexp")
  ("d" eval-defun "defun")
  ("b" edebug-defun "edebug")
  ("p" esc/eval-and-replace "& replace"))

(defhydra hydra-error (esc-mode-map "M-g")
  "goto-error"
  ("h" first-error "first")
  ("j" next-error "next")
  ("k" previous-error "prev")
  ("l" goto-line-with-feedback "goto-line" :color blue))

(defhydra hydra-multiple-cursors (esc-mode-map "M-a")
  "multiple-cursors"
  ("l" mc/edit-lines "edit")
  ("n" mc/mark-next-like-this "mark next")
  ("p" mc/mark-previous-like-this "mark prev")
  ("s" mc/sort-regions "sort")
  ("r" mc/reverse-regions "reverse")
  ("e" mc/insert-numbers "enumerate")
  ("*" mc/mark-all-like-this "mark all")
  ("o" mc/mark-all-like-this-dwim "mark all dwim"))

;; todo: move to beginning of word in pre hook
(defhydra hydra-case-word-at-point (esc-mode-map "C-S-l")
  "case word at point"
  ("c" capitalize-word "capitalize")
  ("u" upcase-word "upcase")
  ("l" downcase-word "lowercase"))

(esc-key "M-m M-m" 'back-to-indentation)
;; todo: create a mirror that manipulates `other-buffer'
(defhydra hydra-scroll-buffer (esc-mode-map "M-m")
  "cursor-movement"
  ("j" scroll-slight-up "up")
  ("k" scroll-slight-down "dw")
  ("s" search-word-forward "i-f")
  ("r" search-word-backward "i-r")
  ("h" backward-char "back")
  ("l" forward-char "frwd")
  ("n" next-line "next")
  ("p" previous-line "prev")
  ("e" end-of-line "end-ln")
  ("a" beginning-of-line "beg-ln")
  ("d" scroll-up "pg up")
  ("u" scroll-down "pg dn")
  ("q" read-only-mode)
  ("/" goto-last-change "last-chng")
  ("." find-tag "tag")
  ("*" pop-tag-mark "pop-tag"))

(defhydra hydra-window-adjustment (esc-mode-map "C-z")
  "window adjustment"
  ("r" esc/rotate-window-split "rotate")
  ("p" esc/swap-buffer-locations "swap")
  ("o" esc/should-have-opened-this-in-other-window "other window" :color blue)
  ("s" unselectable-buffer "selectable")
  ("d" dedicated-buffer "dedicated")
  ("b" mode-line-other-buffer "mode-line-other-buffer")
  ("u" bury-buffer "bury"))

;; (after 'org
;;   (defhydra hydra-org-nav-source-block (esc-mode-map "C-c n")
;;     "org src block nav"
;;     ("n" org-jump-next-src-block "next")
;;     ("p" org-jump-prev-src-block "prev")))

(defhydra hydra-move-text (esc-mode-map "<C-M-return>")
  "move text"
  ("j" move-text-down "down")
  ("k" move-text-up "up")
  ("h" fold-this "fold-this")
  ("r" fold-this-unfold-all "unfold all"))

;;;###autoload
(define-minor-mode esc-mode
  "Toggle esc-keys mode, a minor mode so that my key settings
override annoying major modes."
  :init-value t
  :global t
  :lighter " esc"
  :keymap 'esc-mode-map
  :group 'esc
  ;;; esc-mode wouldn't be complete without these helper modes
  (if (not esc-mode)
      ;; when esc-mode is disabled
      ;; restore associated modes
      (mapc (lambda (mode-state)
	      (let ((mode      (car mode-state))
		    (old-state (cdr mode-state)))
		(when (and old-state
			   (boundp mode)) (funcall mode old-state))))
	    esc/associated-modes-state)

    ;; when esc-mode is enabled
    ;; save the state of associated modes
    (mapc (lambda (mode)
	    (add-to-list 'esc/associated-modes-state
			 `(,mode ,(if (boundp mode)
				      (if (symbol-value mode)
					  t
					-1) nil))))
	  (append esc/associated-modes esc/disassociated-modes))

    ;; activate associated modes
    (mapc (lambda (mode) (when (fboundp mode) (ignore-errors (funcall mode 1))))
	  esc/associated-modes)

    ;; deactivate disassociated modes
    (mapc (lambda (mode) (when (fboundp mode) (funcall mode -1)))
	  esc/disassociated-modes))


  ;; todo: save and restore
  ;;; esc-mode enjoys these settings also
  (let ((activate-bool (esc-mode-enabledp)))
    (setq-default save-place       activate-bool
		  abbrev-mode      activate-bool
		  auto-revert-mode activate-bool
		  truncate-lines   activate-bool)

    (setq
     display-time-24hr-format             activate-bool
     global-auto-revert-non-file-buffers  activate-bool
     next-line-add-newlines               activate-bool
     kill-whole-line                      activate-bool
     vc-follow-symlinks                   activate-bool
     search-highlight                     activate-bool
     query-replace-highlight              activate-bool
     auto-revert-verbose                  (not activate-bool)
     confirm-nonexistent-file-or-buffer   (not activate-bool))))

(provide 'esc-mode)

;;; esc-mode.el ends here

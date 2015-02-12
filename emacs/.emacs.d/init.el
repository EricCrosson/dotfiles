
;;; .emacs.el

;;; Commentary:
;;; This is the .emacs file written and used by esc. The .el file is
;;; not the original form of this document; it was written in org
;;; babel. If you are not viewing the org document, you should try to
;;; locate it. It's much nicer to humans.

;;; License:
;;; esc's .emacs configuration file, for a smoother Emacs experience.
;;; Copyright (C) 2013 Eric Crosson
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(mapc (lambda (mode) (when (fboundp mode) (funcall mode -1)))
      '(menu-bar-mode
        tool-bar-mode
        scroll-bar-mode))

(load-theme 'wombat)

(eval-when-compile
  (defvar package-user-dir)
  (defvar package-archives)
  (defvar uniquify-separator)
  (defvar uniquify-buffer-name-style)
  (defvar save-place-file)
  (defvar display-time-load-average-threshold)
  (defvar dabbrev-case-replace)
  (defvar compilation-save-buffers-predicate)
  (defvar starttls-use-gnutls)
  (defvar recentf-auto-cleanup)
  (defvar c-default-style)
  (defvar c-basic-offset)
  (defvar ido-create-new-buffer)
  (defvar desktop-save)
  (defvar desktop-path)
  (defvar desktop-base-filename)
  (defvar desktop-load-locked-desktop)
  (defvar ff-always-in-other-window)
  (defvar ff-always-try-to-create)
  (defvar ff-search-directories)
  (defvar eshell-mode-map)
  (defvar eshell-where-to-jump)
  (defvar eshell-review-quick-commands)
  (defvar eshell-smart-space-goes-to-end)
  (defvar w3m-mode-map)
  (defvar iedit-mode-keymap)
  (defvar w32-pass-lwindow-to-system)
  (defvar w32-pass-rwindow-to-system)
  (defvar w32-pass-apps-to-system)
  (defvar w32-lwindow-modifier)
  (defvar w32-rwindow-modifier)
  (defvar w32-apps-modifier)
  (defvar mac-command-modifier)
  (defvar mac-option-modifier)
  (defvar ns-function-modifier)
  (defvar dired-mode-map)
  (defvar org-replace-disputed-keys)
  (defvar org-clock-persist)
  (defvar org-hide-leading-stars)
  (defvar org-hide-emphasis-markers)
  (defvar org-src-fontify-natively)
  (defvar org-agenda-files)
  (defvar org-confirm-babel-evaluate)
  (defvar display-time-24hr-format)
  (defvar global-auto-revert-non-file-buffers)
  (defvar auto-revert-verbose))

(defcustom esc-lisp-path nil
  "Path to esc's lisp library."
  :type 'path
  :options '("~/.emacs.d/esc-lisp/")
  :group 'esc-mode)
(setq esc-lisp-path "~/.emacs.d/esc-lisp/")

(defcustom esc-loaddefs-path nil
  "Path to `loaddefs.el' file used by esc's Emacs sessions."
  :type 'path
  :options '("~/.emacs.d/esc-lisp/loaddefs.el")
  :group 'esc-mode)
(setq esc-loaddefs-path "~/.emacs.d/esc-lisp/loaddefs.el")

(let ((default-directory "~/.emacs.d/"))       ;for easy
  (normal-top-level-add-to-load-path '("."))   ;recursive
  (normal-top-level-add-subdirs-to-load-path)) ;loading

(autoload 'list-files-in-subtree-matching-regexp-recursive
  (concat esc-lisp-path "update-autoloads/update-autoloads.el"))
;; Load all project's loaddefs.el (automatically managed)
(mapc (lambda (loaddef) (load-file loaddef))
      (list-files-in-subtree-matching-regexp-recursive
       esc-lisp-path "loaddefs.el"))

(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

(defmacro message-progress (message &rest body)
  "Message MESSAGE and run BODY. Then message MESSSAGE...done."
  (declare (indent defun))
  `(progn
     (message ,message)
     (progn ,@body)
     (message (concat ,message "...done"))))

(defmacro autoload-from-package (package functions)
  "From PACKAGE (string), autoload FUNCTIONS (list)."
  (declare (indent defun))
  `(mapc (lambda (fn) (autoload fn ,package nil t))
         ,functions))

(defmacro require-package (packages)
  "Require PACKAGES (list) quietly."
  (declare (indent defun))
  `(mapc (lambda (package) (require package nil 'noerror))
         ,packages))

(setq load-prefer-newer t)
(require-package '(auto-compile))
(auto-compile-on-load-mode 1)

(when (require 'package nil 'noerror)
  (setq package-user-dir "~/.emacs.d/elpa/")
  (mapc (lambda (source)
            (add-to-list 'package-archives source) t)
        '(("gnu" . "http://elpa.gnu.org/packages/")
          ("marmalade" . "http://marmalade-repo.org/packages/")
          ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
          ("melpa" . "http://melpa.milkbox.net/packages/")))
  (package-initialize))

(setq user-full-name "Eric Crosson"
      user-mail-address "esc@ericcrosson.com")

(put 'overwrite-mode 'disabled t)       ;There shall be no 'insert'
(fset 'yes-or-no-p 'y-or-n-p)           ;change yes-no to y-n
(setq-default size-indication-mode t)
(setq debug-on-error t
      initial-scratch-message nil
      ring-bell-function 'ignore        ;turn off alarms completely
      uniquify-separator ":"            ;needs to be set before uniquify
      uniquify-buffer-name-style 'post-forward ;is loaded
      disabled-command-function 'beep   ;alert me when accessing disabled funcs
      x-select-enable-clipboard t       ;global clipboard
      kill-ring-max 80                  ;kill ring entries
      redisplay-dont-pause t            ;don't pause refreshes
      frame-title-format '("emacs@" system-name ": %f") ;include path of frame
      save-place-file (expand-file-name "meta/places" user-emacs-directory)
      display-time-load-average-threshold 0.6
      dabbrev-case-replace nil
      ; begin deprecation: TODO fix
      display-buffer-reuse-frames t     ;raise buffers, not spawn
      ; end deprecation
      remote-file-name-inhibit-cache t  ;don't resolve remote file attrubutes
      auto-save-default nil
      inhibit-startup-screen t
      large-file-warning-threshold nil
      compile-command "make"
      compilation-ask-about-save nil
      compilation-save-buffers-predicate '(lambda () nil) ;never ask to save
      ff-search-directories '("." "../inc" "../src")
      set-mark-command-repeat-pop t
      starttls-use-gnutls t
      recentf-auto-cleanup 'never   ;must be set before recentf loaded
      mouse-yank-at-point t
      browse-url-browser-function 'w3m-browse-url
      kill-buffer-query-functions (remq 'process-kill-buffer-query-function kill-buffer-query-functions)
      minibuffer-prompt-properties '(read-only t point-entered
                                               minibuffer-avoid-prompt face
                                               minibuffer-prompt))

;; Diminish compiler warnings
(setq byte-compile-warnings '(not interactive-only free-vars))

;; Char and font encoding
(set-buffer-file-coding-system 'unix)   ;Unix mode. Always
(setq c-default-style "linux"
      c-basic-offset 2
      ido-create-new-buffer 'always
      require-final-newline 'visit-save ;compliance
      indent-tabs-mode nil
      comment-style 'indent)

;; Backup settings
(push '("." . "~/.config/.emacs.d/") backup-directory-alist)
(desktop-save-mode 1)                   ;use desktop file
(setq desktop-save 'if-exists                 ;save open buffers
      desktop-path '("~/emacs.d")       ;local desktop files
      desktop-base-filename "desktop"
      desktop-load-locked-desktop t     ;never freeze after crash
      backup-by-copying-when-linked t
      backup-by-copying-when-mismatch t)

(set-face-attribute 'highlight nil :foreground 'unspecified :underline nil)
(set-face-attribute 'region nil :foreground 'unspecified :underline nil :background "#666")

(defalias 'undefun 'fmakunbound)

(defadvice org-agenda (around shrink-agenda-buffer activate)
  "Shrink the agenda after initial display."
  ad-do-it
  (shrink-window-if-larger-than-buffer))

;; Also, keep it shrunken upon refresh
(defadvice org-agenda-redo (around shrink-agenda-buffer-after-refresh activate)
  "Shrink the agenda after refreshing the display."
  ad-do-it
  (shrink-window-if-larger-than-buffer))

(defadvice find-file (before make-directory-maybe
                             (filename &optional wildcards) activate)
  "Create nonexistent parent directories while visiting files."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))

(defadvice comment-dwim (around comment-line-maybe activate)
  "If invoked from the beginning of a line or the beginning of
text on a line, comment the current line instead of appending a
comment to the line."
  (if (and (not (use-region-p))
           (not (eq (line-end-position)
                    (save-excursion (back-to-indentation) (point))))
           (or (eq (point) (line-beginning-position))
               (eq (point) (save-excursion (back-to-indentation) (point)))))
      (comment-or-uncomment-region (line-beginning-position)
                                   (line-end-position))
    ad-do-it
    (setq deactivate-mark nil)))

(defadvice comment-dwim (around delete-comment-if-prefixed activate)
  "If the universal prefix to \\[comment-dwim] is 0, delete the
  comment from the current line or marked region."
  (if (not (eq current-prefix-arg 0))   ; normal behavior
      ad-do-it
    (let ((comments (if (region-active-p)
                        (count-lines (region-beginning) (region-end))
                      1)))
      (save-excursion
        (when (region-active-p)
          (goto-char (region-beginning)))
        (comment-kill comments)))))

(require-package
  '(cl-lib                               ;The Golden Package
    org                                  ;The Platinum Package
    saveplace                            ;included in gnuemacs
    uniquify                             ;included in gnuemacs
    midnight                             ;included in gnuemacs
    which-func                           ;included in gnuemacs
    eldoc                                ;included in gnuemacs
    auto-complete
    notifications
    dired-x

    ;; custom packages
    ; is there a way to get these autoloads loaded implicitly, like
    ; elpa does?
    esc-mode                             ;The Power Glove
    cnsim-mode-autoloads
    big-fringe-mode ;; why is this here again? how to auto-load by nature of being in esc-lisp?
    ))

(add-hook 'after-save-hook 'esc/auto-byte-recompile)

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

(autoload-from-package "lua-mode" '(lua-mode))
(after 'lua-mode-autoloads
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))

(autoload-from-package "undo-tree"
  '(undo-tree-undo
    undo-tree-redo))

(defadvice undo-tree-undo (around keep-region activate)
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
            (p (set-marker (make-marker) (point))))
        ad-do-it
        (goto-char p)
        (set-mark m)
        (set-marker p nil)
        (set-marker m nil))
    ad-do-it))

(autoload-from-package "spray" '(spray-mode))

(autoload-from-package "move-text"
  '(move-text-up
    move-text-down))

(autoload-from-package "multi-term"
  '(multi-term
    multi-term-mext))

(autoload-from-package "ace-jump-mode"
  '(ace-jump-mode
    ace-jump-mode-pop-mark))

(autoload-from-package "ace-jump-buffer"
  '(ace-jump-buffer))

(autoload-from-package "ace-window"
  '(ace-window))

(autoload-from-package "htmlize"
  '(htmlize-region
    htmlize-buffer
    htmlize-ile))

(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)

(autoload-from-package "iedit"
  '(iedit-mode-toggle-on-function
    iedit-prev-occurrance
    iedit-next-occurrance
    iedit-mode))

(autoload-from-package "minimap"
  '(minimap-kill
    minimap-create
    minimap-mode))

(autoload-from-package "tea-time"
  '(tea-time
    tea-timer-cancel))

(autoload-from-package "iedit"          '(iedit)) ;multi-replace
(autoload-from-package "hide-lines"     '(hide-lines))
(autoload-from-package "magit"          '(magit-status))
(autoload-from-package "markdown-mode"  '(markdown-mode))
(autoload-from-package "w3m"            '(w3m-browse-url)) ;web browsing
(autoload-from-package "dic-lookup-w3m" '(dic-lookup-w3m)) ;web browsing
(autoload-from-package "misc"           '(zap-up-to-char))
(autoload-from-package "misc-cmds"      '(revert-buffer-no-confirm))
(autoload-from-package "expand-region"  '(er/expand-region))
(autoload-from-package "autopair"       '(autopair-global-mode)) ;autopair characters
(autoload-from-package "auto-complete"  '(global-auto-complete-mode)) ;autocomplete syntax

(setq mwheel-scroll-up-function 'mwheel-scroll-all-scroll-up-all
      mwheel-scroll-down-function 'mwheel-scroll-all-scroll-down-all)

(add-hook 'erc-mode-hook 'esc/erc-mode-hook)

(add-hook 'minibuffer-setup-hook 'esc/minibuffer-setup-hook)

;; Use human readable Size column instead of original one
(after 'ibuffer
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 100000)  (format "%7.0fk" (/ (buffer-size) 1000.0)))
     ((> (buffer-size) 1000)    (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))

  (setq ibuffer-formats
        '((mark modified read-only         " "
                (name 18 18  :left :elide) " "
                (size-h 9 -1 :right)       " "
                (mode 16 16  :left :elide) " "
                filename-and-process))))

(after 'ibuffer-vc-autoloads
  (defun esc/ibuffer-vc-refresh ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  (add-hook 'ibuffer-hook 'esc/ibuffer-vc-refresh))

(add-hook 'prog-mode-hook 'esc/prog-mode-hook)

(add-hook 'prog-mode-hook 'turn-on-fic-mode)

(after 'notifications
  (defun esc/notify-tea-steeped ()
    (notifications-notify :title "Tea time"
                          :body "Rip out that sac, because your tea bag is done"
                          :app-name "Tea Time"
                          :sound-name "alarm-clock-elapsed"))
  (add-hook 'tea-time-notification-hook 'esc/notify-tea-steeped))

(after 'fundamental
  (add-hook 'fundamental-mode-hook 'esc/fundamental-mode-hook))

(after 'vlf-integrate
  (vlf-set-batch-size (* 10 1024))    ;1.mb
  (custom-set-variables
   '(vlf-application 'dont-ask)))

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(add-to-list
 'auto-mode-alist
 '("\\.\\(screenrc\\)\\'" . conf-mode))

(add-to-list
 'auto-mode-alist
 '("\\.\\(taml\\)\\'" . yaml-mode))

(add-to-list 'auto-mode-alist '("\\.jsp$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
(add-to-list
 'auto-mode-alist
 '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'"
   . enh-ruby-mode))
(add-hook 'enh-ruby-mode-hook 'esc/enh-ruby-mode-hook)

(after 'saveplace
    (setq save-place-file (concat user-emacs-directory "meta/saveplace.el"))
    (setq-default save-place t))

(after 'recentf
  (setq recentf-max-menu-items 25
        recentf-max-saved-items 25
        recentf-keep '(file-remote-p file-readable-p)))

(add-hook 'find-file-hook (lambda() (esc/remove-dos-eol)))

(after 'flyspell-mode
  (setq flyspell-issue-message-flag nil))

(after 'doc-view-mode
  (setq doc-view-continuous t))

(after 'abbrev-mode
  (setq abbrev-file-name "~/emacs.d/abbrev.lst"
        save-abbrevs t)
  (if (file-exists-p abbrev-file-name) ;load custom abbrevs
      (quietly-read-abbrev-file)))

(after 'auto-complete-mode
  (ac-config-default)
  (add-to-list 'ac-dictionary-directories
               "~/.emacs.d/auto-complete/ac-dict"))

(add-hook 'emacs-lisp-mode-hook 'esc/emacs-lisp-mode-hook)

(after 'lexbind-mode
  (add-hook 'emacs-lisp-mode-hook 'lexbind-mode))

(after 'latex-mode
  ;; (add-to-list 'org-export-latex-classes
  ;;           '("article"
  ;;             "\\documentclass{article}"
  ;;             ("\\section{%s}" . "\\section*{%s}")
  ;;             ("\\subsection{%s}" . "\\subsection*{%s}")
  ;;             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ;;             ("\\paragraph{%s}" . "\\paragraph*{%s}")
  ;;             ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (load "auctex.el" nil t t)
  (load "preview-latex.el" nil t t)
  (setq TeX-command-default "latex"
        TeX-auto-save t
        TeX-parse-self t
        TeX-PDF-mode t
        latex-run-command "pdflatex")
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (local-set-key (kbd "C-c C-s") 'latex-math-preview-expression))
;; (add-hook 'org-mode-hook 'turn-on-org-cdlatex)

(add-hook 'c-mode-common-hook 'esc/c-mode-common-hook)

(add-hook 'c++-mode-hook 'esc/c++-mode-hook)
(add-to-list 'auto-mode-alist '("\\.tcc$" . c++-mode))

(after 'idle-highlight
  (setq-default idle-highlight-idle-time 10.0))

(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  "Kill `term-mode' buffers when an exit signal is received."
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))

(defadvice ansi-term (before force-bash)
  "Always use bash for `ansi-term'."
  (interactive (list my-term-shell)))

(defvar my-term-shell "/bin/bash")

(after 'term
;(autoload-from-package "boilerplate" 'esc/term-paste) ;;-already loaded! on TODO
  (add-hook 'term-mode-hook
            (lambda()
              (autopair-mode -1)
              (setq term-buffer-maximum-size 0 ;keep entire session
                    autopair-dont-activate t
                    multi-term-program "/bin/bash")
              (define-key term-raw-map (kbd "C-y") 'esc-term-paste)
              (define-key esc-mode-map (kbd "C-c T") 'ansi-term)
              (define-key esc-mode-map (kbd "C-c t") 'ansi-term-next))))

(add-hook 'eshell-named-command-hook 'esc/eshell-exec-perl)
(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map (kbd "C-x C-p")
              'eshell-previous-matching-input-from-input)
            (define-key eshell-mode-map (kbd "C-x C-n")
              'eshell-next-matching-input-from-input)
            (when (require 'em-smart nil 'noerror)
              (setq eshell-where-to-jump 'begin
                    eshell-review-quick-commands nil
                    eshell-smart-space-goes-to-end t))))

(autoload-from-package "w3m"
  '(w3m
    w3m-copy-buffer))

(autoload-from-package "w3m-config"
  '(w3m-go-to-linknum
    w3m-first-or-subsequent-google-result
    w3m-prev-google-result
    w3m-find-a-google-result
    w3m-filter-all
    w3m-new-tab
    w3m-browse-url-new-tab
    w3m-wiki-new-tab
    w3m-google-new-tab
    w3m-browse-current-buffer))

(add-hook 'w3m-mode-hook
          (lambda ()
            (define-key w3m-mode-map "n" 'w3m-first-or-subsequent-google-result)
            (define-key w3m-mode-map "p" 'w3m-prev-google-result)

            (define-key w3m-mode-map "."
              (lambda() (interactive) (scroll-down 6)))

            (define-key w3m-mode-map ","
              (lambda() (interactive) (scroll-up 6)))

            (define-key w3m-mode-map "C-."
              (lambda() (interactive) (scroll-right 3)))

            (define-key w3m-mode-map "C-,"
              (lambda() (interactive) (scroll-left 3)))

            ;; Eliminate tailing whitespace for a friendlier C-e
            (add-hook 'w3m-display-hook
                      (lambda (url)
                        (let ((buffer-read-only nil))
                          (delete-trailing-whitespace))))))

(after 'ido
  (setq ido-everywhere t                             ;always Ido
        ido-enable-flex-matching t                   ;smarter Ido
        ido-create-new-buffer 'always                ;quieter Ido
        ido-file-extensions-order '(".org" ".txt"))) ;precedence

(autoload-from-package "ido-config"
  '(ido-recentf-open
    ido-goto-symbol))

(add-hook 'iedit-mode-hook 'esc/iedit-mode-hook)

;(global-git-gutter+-mode t)
(after 'git-gutter+
  ;;; Jump between hunks
  (define-key git-gutter+-mode-map (kbd "C-x n") 'git-gutter+-next-hunk)
  (define-key git-gutter+-mode-map (kbd "C-x p") 'git-gutter+-previous-hunk)
   ;;; Act on hunks
  (define-key git-gutter+-mode-map (kbd "C-x v =") 'git-gutter+-show-hunk)
  ;; (define-key git-gutter+-mode-map (kbd "C-x r") nil) ;; stupid
  ;; Stage hunk at point.
  ;; If region is active, stage all hunk lines within the region.
  (define-key git-gutter+-mode-map (kbd "C-x s") 'git-gutter+-stage-hunks)
  (define-key git-gutter+-mode-map (kbd "C-x c") 'git-gutter+-commit)
  (define-key git-gutter+-mode-map (kbd "C-x C") 'git-gutter+-stage-and-commit))

(after 'multiple-cursors-autoloads
  (setq mc/list-file "~/.emacs.d/meta/.mc-lists.el")
  (add-hook 'multiple-cursors-mode-enabled-hook  'esc/multiple-cursors-mode-enabled-hook)
  (add-hook 'multiple-cursors-mode-disabled-hook 'esc/multiple-cursors-mode-disabled-hook))

(after 'ace-jump-mode
    (ace-jump-mode-enable-mark-sync))

(after 'ace-window
     (setq aw-keys '(?a ?b ?c ?d ?e ?f ?g ?h ?i)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(after 'which-func
  (mapc (lambda (mode) (add-to-list 'which-func-modes mode))
        '(org-mode
          emacs-lisp-mode
          c-mode
          c++-mode
          java-mode
          ruby-mode
          enh-ruby-mode)))

(add-hook 'comint-mode-hook 'esc/comint-mode-hook)

(after 'midnight                        ;clean stale buffers
  (midnight-delay-set 'midnight-delay "5:00am"))

(after 'keyfreq                               ;let's take some stats
  (keyfreq-autosave-mode 1)
  (setq keyfreq-file "~/.emacs.d/meta/keyfreq"))

(after 'browse-kill-ring
  (browse-kill-ring-default-keybindings))

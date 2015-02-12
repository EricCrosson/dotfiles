
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

(let ((default-directory "~/.emacs.d/"))       ;for easy
  (normal-top-level-add-to-load-path '("."))   ;recursive
  (normal-top-level-add-subdirs-to-load-path)) ;loading

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

(autoload-from-package "iedit"         '(iedit)) ;multi-replace
(autoload-from-package "hide-lines"    '(hide-lines))
(autoload-from-package "magit"         '(magit-status))
(autoload-from-package "markdown-mode" '(markdown-mode))
(autoload-from-package "misc"          '(zap-up-to-char))
(autoload-from-package "misc-cmds"     '(revert-buffer-no-confirm))
(autoload-from-package "expand-region" '(er/expand-region))
(autoload-from-package "autopair"      '(autopair-global-mode)) ;autopair characters
(autoload-from-package "auto-complete" '(global-auto-complete-mode)) ;autocomplete syntax
(autoload-from-package "single-mode"   '(single-mode)) ;an esc-package

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

(mouse-avoidance-mode 'exile)

(after 'eldoc
  (after 'diminish (diminish 'eldoc-mode))
  (add-hook 'emacs-lisp-mode-hook (lambda() (turn-on-eldoc-mode))))

(add-hook 'compilation-finish-functions
          'esc/bury-compilation-buffer-if-successful)
(add-to-list 'same-window-buffer-names "*compilation*")

(defadvice he-substitute-string (after he-paredit-fix)
  "Remove extra paren when expanding line in paredit"
  (when (and (fboundp 'paredit-mode)
             paredit-mode (equal (substring str -1) ")"))
    (backward-delete-char 1)
    (forward-char)))

(after 'bitly
  (setq bitly-access-token "b4a5cd4e51df442ab97012cfc2764c599d6eabf8"))

(setq paradox-github-token "37204ef66b6566274616d130ec61a0cd4f98e066")

(add-hook 'big-fringe-mode-hook 'esc/big-fringe-mode-hook)

(add-hook 'haskell-mode-hook 'haskell-indent-mode)

(add-hook 'kill-emacs-hook 'update-esc-lisp-autoloads)

(fset 'save-buffers-kill-emacs 'esc/save-buffers-kill-emacs)

(cond ((or (eq system-type 'ms-dos)
           (eq system-type 'windows-nt)
           (eq system-type 'cygwin))

       ;; Windows config
       (message-progress "Loading Windows specific configuration..."
         (setq w32-pass-lwindow-to-system nil
               w32-pass-rwindow-to-system nil
               w32-pass-apps-to-system nil
               w32-lwindow-modifier 'super ; Left Windows key
               w32-rwindow-modifier 'super ; Right Windows key
               w32-apps-modifier 'hyper)   ; Menu key
         (require-package '(w32-symlinks))

(customize-option 'w32-symlinks-handle-shortcuts)

))

((or (eq system-type 'darwin))
 (message-progress "Loading Darwin specific configuration..."
   (setq mac-command-modifier 'meta)
   (setq mac-option-modifier 'super)
   (setq ns-function-modifier 'hyper))))

(require 'dired-details)
(dired-details-install)

;; auto-dired-reload
  ;; Reload dired after making changes
  (after 'dash
    (put '--each 'lisp-indent-function 1)
    (--each '(dired-do-rename
                dired-create-directory
                wdired-abort-changes)
        (eval `(defadvice ,it (after revert-buffer activate)
                 (revert-buffer)))))
  ;; end auto-dired-reload- thanks Magnar

(add-hook 'dired-mode-hook 'esc/dired-mode-hook)
(add-hook 'dired-load-hook 'esc/dired-load-hook)

(eval-after-load "wdired"
  '(progn
     (define-key wdired-mode-map (kbd "C-a") 'esc/dired-back-to-start-of-files)
     (define-key wdired-mode-map
       (vector 'remap 'beginning-of-buffer) 'esc/dired-back-to-top)
     (define-key wdired-mode-map
       (vector 'remap 'end-of-buffer) 'esc/dired-jump-to-bottom)))

(defvar color-theme-stack nil "Stack of color themes.")

(set-face-attribute 'highlight nil :foreground 'unspecified :underline nil)
(set-face-attribute 'region nil :foreground 'unspecified :underline nil :background "#666")

(setq-default major-mode 'org-mode)  ;default mode for new buffers
(setq org-replace-disputed-keys t    ;must be set before org is loaded
      org-clock-persist 'history
      org-hide-leading-stars t
      org-hide-emphasis-markers t
      org-hierarchical-todo-statistics     nil
      org-checkbox-hierarchical-statistics nil
      org-src-fontify-natively t
      org-directory "~/org"
      org-plantuml-jar-path "~/classes/ee460n/res/plantuml.jar"
      org-agenda-files (append '("~/org/todo.org")))

(after 'org
  ;; TODO: maybe put these defuns somewhere
  (defun esc/add-imenu-to-menubar ()
    (imenu-add-to-menubar "Imenu"))
  (add-hook 'org-mode-hook 'esc/add-imenu-to-menubar)

  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 55)
  (defun esc/after-org-mode-hook ()
    (org-indent-mode)
    (local-set-key (kbd "C-M-n") 'outline-next-visible-heading)
    (local-set-key (kbd "C-M-p") 'outline-previous-visible-heading)
    (local-set-key (kbd "C-c C-a") 'org-todo))
  (add-hook 'org-mode-hook 'esc/after-org-mode-hook))

(after 'org
       (add-to-list 'org-structure-template-alist
                    '("E"
                      "#+BEGIN_SRC emacs-lisp ?\n\n#+END_SRC"
                      "<emacs-lisp>\n?\n</emacs-lisp>")))

(add-hook 'org-mode-hook
          (lambda ()
            (setq org-confirm-babel-evaluate nil)

            (org-babel-do-load-languages
             'org-babel-load-languages
             '(;; Always execute these languages
               (R               .       t)
               (ditaa           .       t)
               (dot             .       t)
               (plantuml        .       t)
               (emacs-lisp      .       t)
               (lisp            .       t)
               (clojure         .       t)
               (scala           .       t)
               (gnuplot         .       t)
               (haskell         .       t)
               (ocaml           .       t)
               (python          .       t)
               (ruby            .       t)
               (sh              .       t)
               (sqlite          .       t)
               (octave          .       t)
               (plantuml        .       t)
               ;; Never execute these languages
               (screen          .       nil)
               (sql             .       nil)))))

(add-to-list 'org-structure-template-alist
        '("E"
          "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"
          "<src lang=\"emacs-lisp\">\n\n</src>"))

;;; org-export-blocks-format-plantuml.el Export UML using plantuml
;;
;; OBSOLETED, use ob-plantuml.el bundled in org instead.
;;
;; Copy from org-export-blocks-format-ditaa
;;
;; E.g.
;; #+BEGIN_UML
;;   Alice -> Bob: Authentication Request
;;   Bob --> Alice: Authentication Response
;; #+END_UML

(eval-after-load "org-exp-blocks"
  '(progn
     (add-to-list 'org-export-blocks '(uml iy/org-export-blocks-format-plantuml nil))
     (add-to-list 'org-protecting-blocks "uml")))

(defvar iy/org-plantuml-jar-path (expand-file-name "~/Dropbox/java-libs/plantuml.jar")
  "Path to the plantuml jar executable.")
(defun iy/org-export-blocks-format-plantuml (body &rest headers)
  "Pass block BODY to the plantuml utility creating an image.
  Specify the path at which the image should be saved as the first
  element of headers, any additional elements of headers will be
  passed to the plantuml utility as command line arguments."
  (message "plantuml-formatting...")
  (let* ((args (if (cdr headers) (mapconcat 'identity (cdr headers) " ")))
         (data-file (make-temp-file "org-plantuml"))
         (hash (progn
                 (set-text-properties 0 (length body) nil body)
                 (sha1 (prin1-to-string (list body args)))))
         (raw-out-file (if headers (car headers)))
         (out-file-parts (if (string-match "\\(.+\\)\\.\\([^\\.]+\\)$" raw-out-file)
                             (cons (match-string 1 raw-out-file)
                                   (match-string 2 raw-out-file))
                           (cons raw-out-file "png")))
         (out-file (concat (car out-file-parts) "_" hash "." (cdr out-file-parts))))
    (unless (file-exists-p iy/org-plantuml-jar-path)
      (error (format "Could not find plantuml.jar at %s" iy/org-plantuml-jar-path)))
    (setq body (if (string-match "^\\([^:\\|:[^ ]\\)" body)
                   body
                 (mapconcat (lambda (x) (substring x (if (> (length x) 1) 2 1)))
                            (org-split-string body "\n")
                            "\n")))
    (cond
     ((or htmlp latexp docbookp)
      (unless (file-exists-p out-file)
        (mapc ;; remove old hashed versions of this file
         (lambda (file)
           (when (and (string-match (concat (regexp-quote (car out-file-parts))
                                            "_\\([[:alnum:]]+\\)\\."
                                            (regexp-quote (cdr out-file-parts)))
                                    file)
                      (= (length (match-string 1 out-file)) 40))
             (delete-file (expand-file-name file
                                            (file-name-directory out-file)))))
         (directory-files (or (file-name-directory out-file)
                              default-directory)))
        (with-temp-file data-file (insert (concat "@startuml\n" body "\n@enduml")))
        (message (concat "java -jar " iy/org-plantuml-jar-path " -pipe " args))
        (with-temp-buffer
          (call-process-shell-command
           (concat "java -jar " iy/org-plantuml-jar-path " -pipe " args)
           data-file
           '(t nil))
          (write-region nil nil out-file)))
      (format "\n[[file:%s]]\n" out-file))
     (t (concat
         "\n#+BEGIN_EXAMPLE\n"
         body (if (string-match "\n$" body) "" "\n")
         "#+END_EXAMPLE\n")))))

;; Include the latex-exporter
(require 'ox-latex nil 'noerror)
;; Add minted to the defaults packages to include when exporting.
(add-to-list 'org-latex-packages-alist '("" "minted"))
;; Tell the latex export to use the minted package for source
;; code coloration.
(setq org-latex-listings 'minted)
;; Let the exporter use the -shell-escape option to let latex
;; execute external programs.
;; This obviously and can be dangerous to activate!
(setq org-latex-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; allow for export=>beamer by placing

;; #+LaTeX_CLASS: beamer in org files
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
(add-to-list 'org-export-latex-classes
  ;; beamer class, for presentations
  '("beamer"
     "\\documentclass[11pt]{beamer}\n
      \\mode<{{{beamermode}}}>\n
      \\usetheme{{{{beamertheme}}}}\n
      \\usecolortheme{{{{beamercolortheme}}}}\n
      \\beamertemplateballitem\n
      \\setbeameroption{show notes}
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{hyperref}\n
      \\usepackage{color}
      \\usepackage{listings}
      \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
  frame=single,
  basicstyle=\\small,
  showspaces=false,showstringspaces=false,
  showtabs=false,
  keywordstyle=\\color{blue}\\bfseries,
  commentstyle=\\color{red},
  }\n
      \\usepackage{verbatim}\n
      \\institute{{{{beamerinstitute}}}}\n
       \\subject{{{{beamersubject}}}}\n"

     ("\\section{%s}" . "\\section*{%s}")

     ("\\begin{frame}[fragile]\\frametitle{%s}"
       "\\end{frame}"
       "\\begin{frame}[fragile]\\frametitle{%s}"
       "\\end{frame}")))

  ;; letter class, for formal letters

  (add-to-list 'org-export-latex-classes

  '("letter"
     "\\documentclass[11pt]{letter}\n
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{color}"

     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq org-default-notes-file (concat org-directory "/capture.org"))
(after 'org
  (after 'esc-mode
    (esc-key "C-c C-p" 'org-capture)))

(setq org-capture-templates
      '(;; General tasks go here
        ("t" "Todo" entry
         (file+headline (concat org-directory "/todo.org") "Tasks")
         "* TODO %?\n  %a")
        ;; Used to record my state
        ("j" "Journal" entry
         (file+datetree (concat org-directory "/journal.org"))
         "* %?\nEntered on %U\n  %i\n  %a")

        ;;; Work-related captures
        ("c" "Centaur" entry
         (file+datetree (concat org-directory "/centtech/centtech.org"))
         "* TODO %?\n  %i\n  %a")

        ;;; Personal captures
        ;; Notes about Super Smash Bros. 64
        ("s" "Smash Bros." entry
         (file+headline (concat org-directory "/smash/smash.org") "Notes")
         "* %?\n")))

(setq ;; Work refile locations
 esc-refile-targets-centtech
 `(,(concat org-directory "/centtech/lru.org")
   ,(concat org-directory "/centtech/pse.org")
   ,(concat org-directory "/centtech/newreg.org"))

 ;; Personal refile locations
 esc-refile-targets-smash
 `(,(concat org-directory "/smash/64.org")
   ,(concat org-directory "/smash/melee.org")
   ,(concat org-directory "/smash/pm.org"))

 org-refile-targets '((nil                         :maxlevel . 5)
                      (esc-refile-targets-centtech :maxlevel . 5)
                      (esc-refile-targets-smash    :maxlevel . 5)
                      (org-agenda-files            :maxlevel . 4)))

(hydra-create "<f2>"
  '(("k" text-scale-increase)
    ("j" text-scale-decrease)))

(hydra-create "C-M-<"
  '(("," esc/zoom-out)
    ("." esc/zoom-in)))

(hydra-create "C-`"
  '(("h" first-error "first")
    ("j" next-error "next")
    ("k" previous-error "prev")))

(message "All done, %s%s" (user-login-name) ".")
;;; .emacs.el ends here

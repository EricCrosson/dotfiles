;; -*- mode: dotspacemacs -*-

(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/dotfiles/emacs/.emacs.d/private/")
   dotspacemacs-configuration-layers
   `(;; auto-completion
     better-defaults
     (git :variables
          git-gutter-use-fringe t
          git-magit-status-fullscreen t
          git-use-magit-next t)
     markdown
     org
     shell
     emoji
     games
     eyebrowse
     puppet
     pandoc
     semantic
     shell-scripts
     python
     ruby
     emacs-lisp
     markdown
     lua
     c-c++
     ;; irc
     (colors :variables
             colors-enable-nyan-cat-progress-bar ,(display-graphic-p))
     ;; auctex
     floobits
     restclient
     syntax-checking
     xkcd
     ;; begin custom layers
     bury-successful-compilation
     sublimity
     unkillable-scratch
     swiper
     focus
     misc-cmds
     company
     chess
     writegood-mode
     bliss
     savehist
     twittering
     engine-mode
     midnight
     tea-time
     dictionary
     fic-mode
     ;; spray-mode
     weather-metno
     offlineimap
     dired
     org-extras
     ibuffer-extras
     simplenote
     flx-ido
     visual-bookmark
     pretty-lambdada
     browse-web)

   dotspacemacs-excluded-packages '()
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs
initialization before layers configuration. This defun contains
an exhaustive list of all spacemacs configuration options."
  (setq-default
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'doge
   dotspacemacs-always-show-changelog t
   dotspacemacs-startup-lists '(recents projects bookmarks)
   dotspacemacs-themes '(solarized-light
                         bliss
                         solarized-dark
                         leuven)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("DejaVu Sans Mono"
                               :size 11
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-command-key ":"
   ;; If non nil the paste micro-state is enabled. While enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state t
   dotspacemacs-guide-key-delay 0.4
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil)

  ;; User initialization goes here
  (setq user-full-name "Eric Crosson"
        user-mail-address "esc@ericcrosson.com")

  (put 'overwrite-mode 'disabled t)       ;There shall be no 'insert'
  (fset 'yes-or-no-p 'y-or-n-p)           ;change yes-no to y-n
  (setq-default size-indication-mode t
                indent-tabs-mode nil)
  (setq
   kill-whole-line t
   sentence-end-double-space t
   fill-french-nobreak-p t
   fill-single-char-nobreak-p t
   fill-single-word-nobreak-p t
   ring-bell-function 'ignore
   disabled-command-function 'beep
   vc-follow-symlinks t
   redisplay-dont-pause t
   frame-title-format '("emacs@" system-name ":%f") ;include path of frame
   display-time-load-average-threshold 0.6
   dabbrev-case-replace nil
   display-buffer-reuse-frames t
   remote-file-name-inhibit-cache t
   auto-save-default nil
   large-file-warning-threshold nil
   save-interprogram-paste-before-kill t
   set-mark-command-repeat-pop t
   starttls-use-gnutls t
   browse-url-browser-function 'browse-web
   kill-buffer-query-functions (remq 'process-kill-buffer-query-function
                                     kill-buffer-query-functions)
   search-whitespace-regexp "[ \t\r\n]+"
   minibuffer-prompt-properties '(read-only t
                                  point-entered minibuffer-avoid-prompt
                                  face minibuffer-prompt)
   c-default-style "linux"
   c-basic-offset 4
   tab-width 4
   require-final-newline 'visit-save
   comment-style 'indent
   x-select-enable-clipboard t       ;global clipboard
   mouse-yank-at-point t             ;I will not touch vermin
   doc-view-continuous t
   ff-search-directories '("." "../inc" "../src"))

  ;; Char and font encoding
  (set-buffer-file-coding-system 'unix)

  ;; Backup settings
  (push '("." . "~/.config/.emacs.d/") backup-directory-alist)

  ;; Make windmove work in org-mode:
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)
  (windmove-default-keybindings)

  ;; Enable save desktop
  (setq desktop-base-filename "default"
        desktop-load-locked-desktop t     ;never freeze after crash
        backup-by-copying-when-linked t
        backup-by-copying-when-mismatch t)
  ;; ensure desktop-save dir exists
  (when (boundp 'desktop-path) (mkdir (car desktop-path) t))
  (desktop-save-mode 1)

  (mouse-avoidance-mode 'exile)

  ;; compilation preferences
  (add-to-list 'same-window-buffer-names "*compilation*")
  (setq compile-command "make -k -j32"
        compilation-ask-about-save nil
        compilation-save-buffers-predicate '(lambda () nil)
        byte-compile-warnings '(not interactive-only free-vars))

  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p))

(defun dotspacemacs/config ()
  "Configuration function.
This function is called at the very end of Spacemacs initialization after
layers configuration."

  (global-hl-line-mode -1)
  (rainbow-mode 1)

  (setq helm-echo-input-in-header-line t)
  (defun helm-hide-minibuffer-maybe ()
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))
  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

  (let ((mypst (format "/home/%s/workspace/mpsyt.el" user-login-name)))
    (when (file-exists-p mypst)
      (load-file mypst)
      (evil-leader/set-key
        "oy" 'pc/mpsyt-url-dwim)))

  (global-set-key (kbd "M-x") 'helm-M-x)
  (evil-leader/set-key
    "to"    'org-toggle-inline-images
    "y"     'helm-M-x
    "bi"    'ibuffer
    "js"    'just-one-space

    "bf" 'follow-mode
    "bF" 'follow-delete-other-windows-and-split

    "med" 'edebug-defun

    "od"  (defun xset-dim () (interactive) (shell-command "xset dpms force off"))

    ;; todo: finish incorporating help-extras and properly get the group name to
    ;; appear in guide key
    "hff" 'find-function
    "hfv" 'find-variable
    ;; "hfk"
    "hfl" 'find-library

    "rn" 'revert-buffer-no-confirm
    "rb" 'revert-buffer
    "xs" 'save-buffer

    "bB"    'bury-buffer
    "cm"    'recompile)
  (spacemacs/declare-prefix "hf" "help-find")

  (add-to-list 'evil-emacs-state-modes 'git-commit-mode)
  (add-to-list 'evil-emacs-state-modes 'shell-mode)

  (setq Don t    ;allows `eval-buffer' on *scratch*
        Panic t  ;with `initial-scratch-message'
        initial-scratch-message
        (concat (propertize "Don't\nPanic\n"
                            'font-lock-face '(:height 10.0 :inherit variable-pitch))
                "\n")) ;newline makes user-inserted text normal-sized
  (server-start)
  (message "All done, %s%s" (user-login-name) "."))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-safe-themes
   (quote
    ("ea489f6710a3da0738e7dbdfc124df06a4e3ae82f191ce66c2af3e0a15e99b90" default)))
 '(magit-use-overlays nil)
 '(org-agenda-files (quote ("~/org/ibm.org")))
 '(paradox-github-token t)
 '(ring-bell-function (quote ignore) t)
 '(safe-local-variable-values
   (quote
    ((eval when
           (and
            (buffer-file-name)
            (file-regular-p
             (buffer-file-name))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
           (emacs-lisp-mode)
           (when
               (fboundp
                (quote flycheck-mode))
             (flycheck-mode -1))
           (unless
               (featurep
                (quote package-build))
             (let
                 ((load-path
                   (cons ".." load-path)))
               (require
                (quote package-build))))
           (package-build-minor-mode)
           (set
            (make-local-variable
             (quote package-build-working-dir))
            (expand-file-name "../working/"))
           (set
            (make-local-variable
             (quote package-build-archive-dir))
            (expand-file-name "../packages/"))
           (set
            (make-local-variable
             (quote package-build-recipes-dir))
            default-directory))
     (firestarter compile "make -k -j32 -C ~/workspace/ee445m-labs/build/")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; -*- mode: dotspacemacs -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; --------------------------------------------------------
     ;; Example of useful layers you may want to use right away
     ;; Uncomment a layer name and press C-c C-c to install it
     ;; --------------------------------------------------------
     ;; auto-completion
     better-defaults
     (git :variables
          git-gutter-use-fringe t
          git-magit-status-fullscreen t)
     markdown
     org
     shell-scripts
     python
     ruby
     emacs-lisp
     markdown
     lua
     c-c++
     ;; dash
     irc
     colors
     auctex
     floobits
     restclient
     ;; Causes theme problems
     ;; themes-megapack
     syntax-checking
     xkcd
     ;; begin custom layers
     bury-successful-compilation
     unkillable-scratch
     swiper
     focus
     misc-cmds
     company
     chess
     writegood-mode
     ;; bliss
     savehist
     twittering
     engine-mode
     spotify
     midnight
     tea-time
     ;; work in progress: quelpa
     ;; esc-custom-packages
     dictionary
     fic-mode
     spray-mode
     weather-metno
     offlineimap
     dired
     org-cliplink
     flx-ido
     visual-bookmark
     pretty-lambdada
     browse-web)

   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Either `vim' or `emacs'. Evil is always enabled but if the variable
   ;; is `emacs' then the `holy-mode' is enabled at startup.
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progess in `*Messages*' buffer.
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to a .PNG file.
   ;; If the value is nil then no banner is displayed.
   ;; dotspacemacs-startup-banner 'official
   dotspacemacs-startup-banner 'official
   ;; t if you always want to see the changelog at startup
   dotspacemacs-always-show-changelog t
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents projects bookmarks)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(solarized-light
                         solarized-dark
                         leuven)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil the paste micro-state is enabled. While enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state t
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   )
  ;; User initialization goes here
  (put 'overwrite-mode 'disabled t)       ;There shall be no 'insert'
  (fset 'yes-or-no-p 'y-or-n-p)           ;change yes-no to y-n
  (setq-default size-indication-mode t
                indent-tabs-mode nil)
  (setq
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
   require-final-newline 'visit-save ;compliance
   comment-style 'indent
   x-select-enable-clipboard t       ;global clipboard
   mouse-yank-at-point t
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
  ;ensure desktop-save dir exists
  (when (boundp 'desktop-path) (mkdir (car desktop-path) t))
  (desktop-save-mode 1)

  (add-to-list 'same-window-buffer-names "*compilation*")

  (mouse-avoidance-mode 'exile)

  ;; compilation preferences
  (setq compile-command "make -k -j32"
        compilation-ask-about-save nil
        compilation-save-buffers-predicate '(lambda () nil)
        byte-compile-warnings '(not interactive-only free-vars))
  )

(defun dotspacemacs/config ()
  "Configuration function.
This function is called at the very end of Spacemacs initialization after
layers configuration."
  (setq user-full-name "Eric Crosson"
        user-mail-address "esc@ericcrosson.com")

  (evil-leader/set-key
    "d"     (defun dired-here ()
              (interactive)
              (let ((cwd (file-name-directory (or (buffer-file-name) ""))))
              (cond
               ((and cwd (file-exists-p cwd))
                (dired cwd))
               (t
                (message "I'm not sure which dir to view.")))))
    "y"     'helm-M-x
    "bi"    'ibuffer
    "jd"    (defun join-line-below (&optional arg)
              (interactive "p")
              (dotimes (i arg) (join-line -1)))
    "js"    'just-one-space

    "bf" 'follow-mode
    "bF" 'follow-delete-other-windows-and-split

    ;; "med" 'edebug-defun

    "od"   (defun xset-dim ()
             (interactive)
             (shell-command "xset dpms force off"))

    ;; todo: finish incorporating help-extras and properly get the group name to
    ;; appear in guide key
    "hff" 'find-function
    "hfv" 'find-variable
    ;; "hfk"
    "hfl" 'find-library

    "rb" 'revert-buffer
    "xs" 'save-buffer

    "bB"    'bury-buffer
    "cm"    'recompile
    )
  (spacemacs/declare-prefix "hf" "help-find")

  (add-to-list 'evil-emacs-state-modes 'git-commit-mode)
  
  ;; advise configuration-layer/create-layer to open existing layers.
  ;; (spacemacs|advise-commands 'configuration-layer/create-or-open
  ;;                            '(configuration-layer/create-layer)
  ;;                            around
  ;;                            (let ((layer-dir (configuration-layer//get-private-layer-dir))))
  ;;                            )

   (setq Don t    ;allows `eval-buffer' on *scratch*
        Panic t   ;with `initial-scratch-message'
        initial-scratch-message
        (concat (propertize "Don't\nPanic\n"
                            'font-lock-face '(:height 10.0 :inherit variable-pitch))
                "\n")) ;newline makes user-inserted text normal-sized
  (server-start)
  (message "All done, %s%s" (user-login-name) ".")
  )

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
 '(paradox-github-token t)
 '(ring-bell-function (quote ignore) t)
 '(safe-local-variable-values
   (quote
    ((firestarter compile "make -k -j32 -C ~/workspace/ee445m-labs/build/")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

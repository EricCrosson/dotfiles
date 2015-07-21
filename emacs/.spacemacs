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
   ;; dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   `(better-defaults
     (auto-completion :variables
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-complete-with-key-sequence "jk"
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t)
     (git :variables
          git-gutter-use-fringe t
          git-magit-status-fullscreen t
          git-use-magit-next t)
     github
     diff-hl
     version-control
     markdown
     org
     shell
     evil-extra-text-objects
     emoji
     games
     eyebrowse
     pcre2el
     puppet
     gtags
     pandoc
     ansible
     dockerfile
     erc
     semantic
     shell-scripts
     python
     ruby
     emacs-lisp
     markdown
     lua
     c-c++
     (colors :variables
             colors-enable-nyan-cat-progress-bar ,(display-graphic-p))
     latex
     floobits
     arduino
     restclient
     syntax-checking
     xkcd
     bury-successful-compilation
     sublimity
     ;; key-chord
     olivetti
     multiple-cursors
     unkillable-scratch
     highlight-stages
     focus
     misc-cmds
     chess
     writegood-mode
     bliss
     savehist
     twittering
     engine-mode
     midnight
     tea-time
     dictionary
     vagrant
     fic-mode
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

   ;; dotspacemacs-excluded-packages '()
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs
initialization before layers configuration. This defun contains
an exhaustive list of all spacemacs configuration options."
  (setq-default
   dotspacemacs-editing-style 'vim
   ;; dotspacemacs-verbose-loading t
   dotspacemacs-startup-banner 'doge
   dotspacemacs-always-show-changelog t
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-startup-lists '(recents projects bookmarks)
   dotspacemacs-themes '(bliss
                         solarized-light
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
   ;; dotspacemacs-fullscreen-at-startup t
   ;; dotspacemacs-fullscreen-use-non-native t
   ;; dotspacemacs-maximized-at-startup t
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; dotspacemacs-smartparens-strict-mode nil
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; dotspacemacs-persistent-server t
   ;; dotspacemacs-default-package-repository t
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")))

(defun dotspacemacs/config ()
  "Configuration function.
This function is called at the very end of Spacemacs initialization after
layers configuration."

  (global-hl-line-mode nil)
  (rainbow-mode t)
  (nyan-mode t)

  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

  (setq user-full-name "Eric Crosson"
        user-mail-address "esc@ericcrosson.com")

  (put 'overwrite-mode 'disabled t)       ;There shall be no 'insert'
  (fset 'yes-or-no-p 'y-or-n-p)           ;change yes-no to y-n
  (setq-default size-indication-mode t
                auto-save-default nil
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
   doc-view-continuous t
   ff-search-directories '("." "../inc" "../src"))

  ;; Char and font encoding
  (set-buffer-file-coding-system 'unix)

  ;; Backup settings
  (push '("~/.config/.emacs.d/") backup-directory-alist)

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

  ;; I will not touch vermin
  (setq mouse-yank-at-point t)
  (mouse-avoidance-mode 'exile)

  ;; compilation preferences
  (add-to-list 'same-window-buffer-names "*compilation*")
  (setq compile-command "make -k -j32"
        compilation-ask-about-save nil
        compilation-save-buffers-predicate '(lambda () nil)
        byte-compile-warnings '(not interactive-only free-vars))

  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

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

  ;; stupid that I have to fix this defun
  (defun kill-this-buffer ()	; for the menu bar
    "Kill the current buffer.
When called in the minibuffer, get out of the minibuffer
using `abort-recursive-edit'."
    (interactive)
    (cond
     ((menu-bar-non-minibuffer-window-p)
      (kill-buffer (current-buffer)))
     (t
      (abort-recursive-edit))))

  (add-hook 'focus-out-hook
            (defun save-current-buffer-if-needed ()
              (interactive)
              (when (and (buffer-file-name) (buffer-modified-p))
                (save-buffer))))

  ;; TODO: configuration layer 'ibuffer-by-project'
  (after 'projectile
    (after 'ibuffer
      (setq ibuffer-saved-filter-groups
            (list (cons "Default"
                        (append
                         (mapcar (lambda (it)
                                   (let ((name (file-name-nondirectory
                                                (directory-file-name it))))
                                     `(,name (filename . ,(expand-file-name it)))))
                                 projectile-known-projects)
                         `(("Org" (mode . org-mode))
                           ("Dired" (mode . dired-mode))
                           ("IRC" (mode . erc-mode))
                           ("Emacs"
                            (or (name . "\\*Messages\\*")
                                (name . "\\*Compile-Log\\*")
                                (name . "\\*scratch\\*")
                                (name . "\\*spacemacs\\*")
                                (name . "\\*emacs\\*")))
                           ("Terminal" (or (name . "\\*ansi-term\\*")
                                           (name . "\\*eshell\\*")))
                           ("Magit" (name . "\\*magit"))
                           ("Help" (name . "\\*Help\\*"))
                           ("Helm" (name . "\\*helm")))))))
      (add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode)
      (add-hook 'ibuffer-mode-hook
                (defun ibuffer-switch-to-default-filter-group ()
                  (ibuffer-switch-to-saved-filter-groups "Default")))))

  (define-key (current-global-map) [remap save-buffers-kill-terminal]
    (defun kill-emacs-psych-out ()
      (interactive)
      (message "I'm sorry %s, I'm afraid I can't do that."
               (or (user-login-name) "Dave"))))

  (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
    "Avoid informing me of 'active processes exist'ing when killing emacs."
    (flet ((process-list ())) ad-do-it))

  (after 'company
    (global-company-mode)
    (setq company-show-numbers t)
    (defun turn-off-company-mode ()
      (interactive)
      (company-mode -1))
    (mapc (lambda (mode-hook)
            (add-hook mode-hook 'turn-off-company-mode))
          '(shell-mode-hook
            org-mode-hook
            sh-mode-hook
            gud-mode-hook))
    (defun company-quickhelp--show ()
      (company-quickhelp--ensure-compatibility)
      (company-quickhelp--cancel-timer)
      (let* ((selected (nth company-selection company-candidates))
             (doc (company-quickhelp--doc selected))
             (ovl company-pseudo-tooltip-overlay)
             (overlay-width (* (frame-char-width) (if ovl (overlay-get ovl 'company-width) 0)))
             (overlay-position (* (frame-char-width) (- (if ovl (overlay-get ovl 'company-column) 1) 1)))
             (x-gtk-use-system-tooltips nil))
        (when (and ovl doc)
          (with-no-warnings
            (pos-tip-show doc nil (overlay-start ovl) nil 300 80 nil (+ overlay-width overlay-position) 1))))))

  (add-to-list 'auto-mode-alist '("\\.offlineimap" . conf-mode))
  (after 'helm-gtags (diminish 'helm-gtags-mode))

  (defvar xorg/sleep-delay 0.8
    "Seconds to sleep before forcing xorg off with dpms.")

  (global-set-key (kbd "M-x") 'helm-M-x)
  (evil-leader/set-key
    "y" 'helm-M-x
    "si" 'helm-semantic-or-imenu
    "to" 'org-toggle-inline-images
    "bi" 'ibuffer
    "js" 'just-one-space
    "med" 'edebug-defun

    "bf" 'follow-mode
    "bF" 'follow-delete-other-windows-and-split

    "od"  (defun xset-dim () (interactive) (shell-command (format "sleep %s && xset dpms force off" xorg/sleep-delay)))

    "hff" 'find-function
    "hfv" 'find-variable
    "hfk" 'find-function-on-key
    "hfl" 'find-library

    "rn" 'revert-buffer-no-confirm
    "rb" 'revert-buffer
    "xs" 'save-buffer

    "bB" 'bury-buffer
    "cm" 'recompile

    "Fc" 'make-frame-command
    "Fd" 'delete-frame

    "<SPC>" 'avy-goto-char
    "," 'avy-goto-char-2

    "1" 'eyebrowse-switch-to-window-config-1
    "2" 'eyebrowse-switch-to-window-config-2
    "3" 'eyebrowse-switch-to-window-config-3
    "4" 'eyebrowse-switch-to-window-config-4
    "5" 'eyebrowse-switch-to-window-config-5
    "6" 'eyebrowse-switch-to-window-config-6
    "7" 'eyebrowse-switch-to-window-config-7
    "8" 'eyebrowse-switch-to-window-config-8
    "9" 'eyebrowse-switch-to-window-config-9
    "0" 'eyebrowse-switch-to-window-config-0)

  (mapc (lambda (mode) (add-to-list 'evil-emacs-state-modes mode))
        '(shell-mode
          text-mode))

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

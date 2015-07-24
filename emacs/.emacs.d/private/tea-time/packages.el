;;; packages.el --- tea-time Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar tea-time-packages '(tea-time))

(defun tea-time/init-tea-time ()
  "Initialize `tea-time'."
  (use-package tea-time
    :defer t
    :commands tea-time
    :config
    (use-package notifications
      :commands notifications-notify)
    (defun esc/notify-tea-steeped ()
      (notifications-notify :title "Tea time"
                            :body "Rip out that sac, because your tea bag is done"
                            :app-name "Tea Time"
                            :sound-name "alarm-clock-elapsed"))
    (add-hook 'tea-time-notification-hook 'esc/notify-tea-steeped)))

;;; packages.el --- weather-mento Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq weather-metno-packages '(weather-metno))

(defun weather-metno/init-weather-metno ()
  "Initialize my package"
  (setq weather-metno-location-name "Austin, Texas"
        weather-metno-location-latitude 30
        weather-metno-location-longitude 97)
  )

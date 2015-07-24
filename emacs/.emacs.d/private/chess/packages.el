;;; packages.el --- chess Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq chess-packages '(chess))

(defun chess/init-chess ()
  "Initialize package chess."
  (use-package chess
    :defer t
    :config
    (evil-leader/set-key
      "agcc" 'chess
      "agci" 'chess-ics
      "agcp" 'chess-puzzle
      "agcl" 'chess-pgn-read
      "agct" 'chess-display-chat
      "agcs" 'chess-display-pass
      "agcb" 'chess-display-abort
      "agcm" 'chess-display-match
      "agcf" 'chess-display-force
      "agca" 'chess-display-accept
      "agcr" 'chess-display-resign
      "agce" 'chess-display-create)))

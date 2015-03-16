;;; spinner.el --- Add spinners and progress-bars to the mode-line for ongoing operations -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; Version: 1.0
;; Package-Requires: ((cl-lib "0.5"))
;; URL: https://github.com/Bruce-Connor/spinner.el
;; Keywords: processes mode-line

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; 1 Usage
;; ═══════
;;
;; 1. Add `(spinner "1.0")' to your package’s dependencies.
;;
;; 2. Call `(spinner-start)' and a spinner will be added to the
;; mode-line.
;;
;; 3. Call `(spinner-stop)' on the same buffer when you want to remove
;; it.
;;
;;
;; 2 Behavior
;; ══════════
;;
;; The default spinner is a line drawing that rotates. You can pass an
;; argument to `spinner-start' to specify which spinner you want. All
;; possibilities are listed in the `spinner-types' variable, but here are
;; a few examples for you to try:
;;
;; • `(spinner-start 'vertical-breathing 10)'
;; • `(spinner-start 'minibox)'
;; • `(spinner-start 'moon)'
;; • `(spinner-start 'triangle)'


;;; Code:
(require 'cl-lib)

(defconst spinner-types
  '((3-line-clock . ["┤" "┘" "┴" "└" "├" "┌" "┬" "┐"])
    (2-line-clock . ["┘" "└" "┌" "┐"])
    (flipping-line . ["_" "\\" "|" "/"])
    (rotating-line . ["-" "\\" "|" "/"])
    (progress-bar . ["[    ]" "[=   ]" "[==  ]" "[=== ]" "[====]" "[ ===]" "[  ==]" "[   =]"])
    (progress-bar-filled . ["|    |" "|█   |" "|██  |" "|███ |" "|████|" "| ███|" "|  ██|" "|   █|"])
    (vertical-breathing . ["▁" "▂" "▃" "▄" "▅" "▆" "▇" "█" "▇" "▆" "▅" "▄" "▃" "▂" "▁" " "])
    (vertical-rising . ["▁" "▄" "█" "▀" "▔"])
    (horizontal-breathing . [" " "▏" "▎" "▍" "▌" "▋" "▊" "▉" "▉" "▊" "▋" "▌" "▍" "▎" "▏"])
    (horizontal-breathing-long
     . ["  " "▎ " "▌ " "▊ " "█ " "█▎" "█▌" "█▊" "██" "█▊" "█▌" "█▎" "█ " "▊ " "▋ " "▌ " "▍ " "▎ " "▏ "])
    (horizontal-moving . ["  " "▌ " "█ " "▐▌" " █" " ▐"])
    (minibox . ["▖" "▘" "▝" "▗"])
    (triangle . ["◢" "◣" "◤" "◥"])
    (box-in-box . ["◰" "◳" "◲" "◱"])
    (box-in-circle . ["◴" "◷" "◶" "◵"])
    (half-circle . ["◐" "◓" "◑" "◒"])
    (moon . ["🌑" "🌘" "🌖" "🌕" "🌔" "🌒"]))
  "Predefined alist of spinners.
Each car is a symbol identifying the spinner, and each cdr is a
vector, the spinner itself.")

(defvar spinner-current nil
  "Spinner curently being displayed on the mode-line.")
(make-variable-buffer-local 'spinner-current)

(defvar spinner--counter 0
  "Current frame of the spinner.")
(make-variable-buffer-local 'spinner--counter)

(defconst spinner--mode-line-construct
  '((spinner-current
     (" "
      (:eval (elt spinner-current
                  (% spinner--counter
                     (length spinner-current)))))
     (spinner--timer
      (:eval (spinner-stop)))))
  "Construct used to display the spinner.")
(put 'spinner--mode-line-construct 'risky-local-variable t)

(defvar spinner--timer nil
  "Holds the timer being used on the current buffer.")
(make-variable-buffer-local 'spinner--timer)

(defvar spinner-frames-per-second 5
  "Default speed at which spinners spin, in frames per second.
Applications can override this value.")


;;; The main function
;;;###autoload
(defun spinner-start (&optional type fps)
  "Start a mode-line spinner of given TYPE.
Spinners are buffer local. It is added to the mode-line in the
buffer where `spinner-start' is called.

Return value is a function which can be called anywhere to stop
this spinner.  You can also call `spinner-stop' in the same
buffer where the spinner was created.

FPS, if given, is the number of desired frames per second.
Default is `spinner-frames-per-second'.

If TYPE is nil, use the first element of `spinner-types'.
If TYPE is `random', use a random element of `spinner-types'.
If it is a symbol, it specifies an element of `spinner-types'.
If it is a vector, it used as the spinner.
If it is a list, it should be a list of symbols, and a random one
is chosen as the spinner type."
  ;; Choose type.
  (setq spinner-current
        (cond
         ((vectorp type) type)
         ((not type) (cdr (car spinner-types)))
         ((eq type 'random)
          (cdr (elt spinner-types
                    (random (length spinner-types)))))
         ((listp type)
          (cdr (assq (elt type (random (length type)))
                     spinner-types)))
         ((symbolp type) (cdr (assq type spinner-types)))
         (t (error "Unknown spinner type: %s" type))))
  (setq spinner--counter 0)

  ;; Maybe add to mode-line.
  (unless (memq 'spinner--mode-line-construct mode-line-format)
    (setq mode-line-format (cl-copy-list mode-line-format))
    (let ((cell (memq 'mode-line-buffer-identification mode-line-format)))
      (if cell
          (setcdr cell (cons 'spinner--mode-line-construct (cdr cell)))
        (setcdr (last mode-line-format) '(spinner--mode-line-construct)))))

  ;; Create timer.
  (when (timerp spinner--timer)
    (cancel-timer spinner--timer))
  (let ((buffer (current-buffer))
        ;; Create the timer as a lex variable so it can cancel itself.
        (timer (run-at-time t
                            (/ 1.0 (or fps spinner-frames-per-second))
                            #'ignore)))
    (timer-set-function
     timer (lambda ()
             (if (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (setq spinner--counter (1+ spinner--counter))
                   (force-mode-line-update))
               (ignore-errors (cancel-timer timer)))))
    (setq spinner--timer timer)
    ;; Return a stopping function.
    (lambda () (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (spinner-stop))))))

(defun spinner-stop ()
  "Stop the current buffer's spinner."
  (when (timerp spinner--timer)
    (cancel-timer spinner--timer))
  (setq spinner--timer nil
        spinner-current nil)
  (setq mode-line-format
        (remove 'spinner--mode-line-construct mode-line-format)))

;;;; ChangeLog:

;; 2015-03-07  Artur Malabarba  <bruce.connor.am@gmail.com>
;; 
;; 	Merge commit '7eca7d023c95bc21c7838467b3a58d549afaf68d'
;; 
;; 2015-03-07  Artur Malabarba  <bruce.connor.am@gmail.com>
;; 
;; 	Merge commit 'a7b4e52766977b58c6b9899305e962a2b5235bda'
;; 
;; 2015-03-07  Artur Malabarba  <bruce.connor.am@gmail.com>
;; 
;; 	Add 'packages/spinner/' from commit
;; 	'9477ee899d62259d4b946f243cdcdd9cdeb1e910'
;; 
;; 	git-subtree-dir: packages/spinner git-subtree-mainline:
;; 	5736e852fd48a0f1ba1c328dd4d03e3fa008a406 git-subtree-split:
;; 	9477ee899d62259d4b946f243cdcdd9cdeb1e910
;; 


(provide 'spinner)

;;; spinner.el ends here

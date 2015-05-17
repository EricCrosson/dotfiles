;;; packages.el --- dictionary Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq dictionary-packages '(dictionary))

(defun dictionary/init-dictionary ()
  "Initialize my package"
  (use-package dictionary
    :config
    (defun esc/dictionary-search ()
      (interactive)
      (autoload 'dictionary-new-search "dictionary")
      (let* ((enable-recursive-minibuffers t)
             (word (current-word))
             (word-wrapped (if word (concat " (" word ")") ""))
             (word-query (concat "Word" word-wrapped ": "))
             (val (read-from-minibuffer word-query)))
        (dictionary-new-search
         (cons (cond
                ((and (equal val "") word) word)
                ((> (length val) 0) val)
                (t (error "No word to lookup")))
               dictionary-default-dictionary)))))
  )

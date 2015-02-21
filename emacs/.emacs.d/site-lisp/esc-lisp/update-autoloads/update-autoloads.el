
;;;###autoload
(defgroup autoloads nil
  "Simplified maintenance of magic-cookie autoloads."
  :version "0.1.0")

;;;###autoload
(defun list-files-in-subtree-matching-regexp-recursive (directory &optional regexp)
  "List the `.el' files in DIRECTORY and in its sub-directories.

If REGEXP is non-nil, compile a list of files matching REGEXP
instead of `.el'."
  (interactive "Directory: ")
  (let (el-files-list
        (current-directory-list
         (directory-files-and-attributes directory t)))
    (let ((file-regexp (or regexp ".el")))
      ;; while we are in the current directory
      (while current-directory-list
        (cond
         ;; check to see whether filename ends in `.el'
         ;; and if so, append its name to a list.
         ((string-match file-regexp (car (car current-directory-list)))
          (setq el-files-list
                (cons (car (car current-directory-list)) el-files-list)))
         ;; check whether filename is that of a directory
         ((eq t (car (cdr (car current-directory-list))))
          ;; decide whether to skip or recurse
          (if
              (equal "."
                     (substring (car (car current-directory-list)) -1))
              ;; then do nothing since filename is that of
              ;;   current directory or parent, "." or ".."
              ()
            ;; else descend into the directory and repeat the process
            (setq el-files-list
                  (append
                   (list-files-in-subtree-matching-regexp-recursive
                    (car (car current-directory-list)) file-regexp)
                   el-files-list)))))
        ;; move to the next filename in the list; this also
        ;; shortens the list so the while loop eventually comes to an end
        (setq current-directory-list (cdr current-directory-list)))
      el-files-list)))

;;;###autoload
(defun get-project-loaddefs-path (base)
  "Return the path to the closest loaddefs.el file relative to BASE.

Only query this defun with BASE as a subdir of ~/.emacs.d/esc-lisp."
  (interactive)
  (catch 'found
    (let ((loaddefs "loaddefs.el")
          (base     (replace-regexp-in-string "/$" "" (expand-file-name base)))
          (top-dir  (expand-file-name "~/.emacs.d/site-lisp")))
      (unless (file-exists-p    base) (throw 'found nil))
      (unless (file-directory-p base)
        (setq base (replace-regexp-in-string
                    "/$" "" (file-name-directory base))))
      (while (string-match top-dir base) ;base is a subdir of home
        (when (member loaddefs (directory-files base))
          (throw 'found (concat base "/" loaddefs)))
        (setq base (file-name-directory ;remove trailing slash
                    (replace-regexp-in-string "/$" "" base)))))
    (throw 'found nil)))

;;;###autoload
(defun update-esc-lisp-autoloads (&optional base)
  "Update autoload definitions for Lisp files in the directories DIRS.
In an interactive call, you must give one argument, the name of a
single directory.  In a call from Lisp, you can supply multiple
directories as separate arguments, but this usage is discouraged.

The function DOES recursively descend into subdirectories of the
directory or directories specified.

Note: this docstring was copied from \\[update-directory-autoloads]."
  (interactive
   (let ((file buffer-file-name))
     (list
      (read-file-name "Update autoloads (recursively) in which dir: "
                      esc-lisp-path esc-lisp-path))))
  (unless base (setq base esc-lisp-path))
  (setq base (replace-regexp-in-string "/$" "" (expand-file-name base)))
  (let ((el-files (list-files-in-subtree-matching-regexp-recursive
                   (expand-file-name base))))
    (dolist (el el-files)
      (ignore-errors
        (unless (string-match "loaddefs.el" el)
          (update-file-autoloads el t (get-project-loaddefs-path el)))))
    (message (concat "Generating autoloads for " base "...done"))))

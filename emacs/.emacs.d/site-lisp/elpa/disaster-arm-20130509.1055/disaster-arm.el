;;; disaster-arm.el --- Disassemble C/C++ code for an ARM Cortex under cursor in Emacs

;; Copyright (C) 2013 Justine Tunney.
;; Edited 2015 Eric Crosson: changed compiler/objdumper/disassembler to gnu-arm-eabi-*

;; Author: Justine Tunney <jtunney@gmail.com>
;; Created: 2015-03-02
;; Version: 20130509.1055
;; X-Original-Version: 0.1
;; Keywords: tools

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; ![Screenshot](http://i.imgur.com/kMoN1m6.png)
;;
;; Disaster-Arm lets you press `C-c d` to see the compiled assembly code for the
;; C/C++ file you're currently editing. It even jumps to and highlights the
;; line of assembly corresponding to the line beneath your cursor.
;;
;; It works by running your cmade binaries through objdump to generate
;; the human-readable assembly.

;;; Installation:

;; Make sure to place `disaster-arm.el` somewhere in the load-path and add the
;; following lines to your `.emacs` file to enable the `C-c d` shortcut to
;; invoke `disaster-arm':
;;
;;     (add-to-list 'load-path "/PATH/TO/DISASTER-ARM")
;;     (require 'disaster-arm)
;;     (define-key c-mode-base-map (kbd "C-c d") 'disaster-arm)
;;

;;; Code:

(defgroup disaster-arm nil
  "Disassemble C/C++ under cursor (Works best with Clang)."
  :prefix "disaster-arm-"
  :group 'tools)

(defcustom disaster-arm-make-flags "-k"
  "Command line options to pass to make if a Makefile is found."
  :group 'disaster-arm
  :type 'string)

(defcustom disaster-arm-cc "arm-none-eabi-gcc"
  "The command for your C compiler."
  :group 'disaster-arm
  :type 'string)

(defcustom disaster-arm-cxx (or (getenv "CXX") "c++")
  "The command for your C++ compiler."
  :group 'disaster-arm
  :type 'string)

(defcustom disaster-arm-cflags (or (getenv "CFLAGS")
                               "")
  "Command line options to use when compiling C."
  :group 'disaster-arm
  :type 'string)

(defcustom disaster-arm-cxxflags (or (getenv "CXXFLAGS")
                                 "")
  "Command line options to use when compiling C++.!"
  :group 'disaster-arm
  :type 'string)

(defcustom disaster-arm-objdump "arm-none-eabi-objdump -D -Sl"
  "The command name and flags for running objdump."
  :group 'disaster-arm
  :type 'string)

(defcustom disaster-arm-buffer-compiler "*compilation*"
  "Buffer name to use for assembler output."
  :group 'disaster-arm
  :type 'string)

(defcustom disaster-arm-buffer-assembly "*assembly*"
  "Buffer name to use for objdump assembly output."
  :group 'disaster-arm
  :type 'string)

(defcustom disaster-arm-project-root-files
  (list (list ".git/")         ;; Git directory is almost always project root.
        (list "setup.py"       ;; Python apps.
              "package.json")  ;; node.js apps.
        (list "Makefile"))     ;; Makefiles are sometimes in subdirectories.
  "List of lists of files that may indicate software project root
   directory. Sublist are ordered from highest to lowest
   precedence."
  :group 'disaster-arm
  :type '(repeat (repeat string)))

(defvar save-place)

(defun disaster-arm-list-files-in-subtree-matching-regexp-recursive (directory &optional regexp)
    "List the `.el' files in DIRECTORY and in its sub-directories.

  If REGEXP is non-nil, compile a list of files matching REGEXP
  instead of `.el'."
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

(defun disaster-arm-project-root-build-directory ()
  "Return either esc's or hershal's root dir to the ee445m-labs repo."
  (if (string-equal (user-login-name) "eric")
      "~/workspace/ee445m-labs/build"
    "/ee445m-labs/build"))

;;;###autoload
(defun disaster-arm (&optional file line)
  "Shows assembly code for current line of C/C++ file.

Here's the logic path it follows:

- Is there a Makefile in this directory? Run `make bufname.o`.
- Or is there a Makefile in a parent directory? Run `make -C .. bufname.o`.
- Or is this a C file? Run `cc -g -O3 -c -o bufname.o bufname.c`
- Or is this a C++ file? Run `c++ -g -O3 -c -o bufname.o bufname.c`
- If build failed, display errors in compile-mode.
- Run objdump inside a new window while maintaining focus.
- Jump to line matching current line.

If FILE and LINE are not specified, the current editing location
is used."
  (interactive)
  (save-buffer)
  (let* ((file (or file (file-name-nondirectory (buffer-file-name))))
         (line (or line (line-number-at-pos)))
         (file-line (format "%s:%d" file line))
         (makebuf (get-buffer-create disaster-arm-buffer-compiler))
         (asmbuf (get-buffer-create disaster-arm-buffer-assembly)))
    (if (not (string-match "\\.c[cp]?p?$" file))
        (message "Not C/C++ non-header file")
      ;; optional todo: build before dumping first
      (let* ((cwd (file-name-directory (expand-file-name (buffer-file-name))))
             (obj-file (car (disaster-arm-list-files-in-subtree-matching-regexp-recursive
			     (disaster-arm-project-root-build-directory)
			     (concat "/" (file-name-sans-extension file) ".c.obj"))))
             (make-root (disaster-arm-find-project-root "Makefile" file))
             (cc (if make-root
                     (if (equal cwd make-root)
                         (format "make %s %s" disaster-arm-make-flags obj-file)
                       (format "make %s -C %s %s"
                               disaster-arm-make-flags make-root
                               (file-relative-name obj-file make-root)))))
             (dump (format "%s %s" disaster-arm-objdump obj-file))
             (line-text (save-excursion
                          (buffer-substring-no-properties
                           (progn (beginning-of-line) (point))
                           (progn (end-of-line) (point))))))
        (if (file-exists-p obj-file)
            (when (eq 0 (progn
                          (message (format "Running: %s" dump))
                          (shell-command dump asmbuf)))
              (kill-buffer makebuf)
              (with-current-buffer asmbuf
                ;; saveplace.el will prevent us from hopping to a line.
                (set (make-local-variable 'save-place) nil)
                (asm-mode)
		(read-only-mode)
                (disaster-arm--shadow-non-assembly-code))
              (let ((oldbuf (current-buffer)))
                (switch-to-buffer-other-window asmbuf)
                (goto-char 0)
                (if (or (search-forward line-text nil t)
                        (search-forward file-line nil t))
                    (progn
                      (recenter)
                      (overlay-put (make-overlay (save-excursion
                                                   (beginning-of-line)
                                                   (point))
                                                 (save-excursion
                                                   (forward-line)
                                                   (beginning-of-line)
                                                   (point)))
                                   'face 'region))
		  (message "Couldn't find corresponding assembly line."))
                (switch-to-buffer-other-window oldbuf)))
          (with-current-buffer makebuf
            (save-excursion
              (goto-char 0)
              (insert (concat cc "\n")))
            (compilation-mode)
            (display-buffer makebuf)))))))

(defun disaster-arm--shadow-non-assembly-code ()
  "Scans current buffer, which should be in asm-mode, and uses
the standard `shadow' face for lines that don't appear to contain
assembly code."
  (remove-overlays)
  (save-excursion
    (goto-char 0)
    (while (not (eobp))
      (beginning-of-line)
      (if (not (looking-at "[ \t]+[a-f0-9]+:[ \t]+"))
          (let ((eol (save-excursion (end-of-line) (point))))
            (overlay-put (make-overlay (point) eol)
                         'face 'shadow)))
      (forward-line))))

(defun disaster-arm--find-parent-dirs (&optional file)
  "Returns a list of parent directories with trailing slashes.

For example:

    (disaster--find-parent-dirs \"/home/jart/disaster-disaster.el\")
    => (\"/home/jart/disaster-\" \"/home/jart/\" \"/home/\" \"/\")

FILE defaults to `buffer-file-name'."
  (let ((res nil)
        (dir (file-name-directory
              (expand-file-name (or file (buffer-file-name))))))
    (while dir
      (setq res (cons dir res)
            dir (if (string-match "/[^/]+/$" dir)
                    (substring dir 0 (+ 1 (match-beginning 0))))))
    (reverse res)))

(defun disaster-arm--dir-has-file (dir file)
  "Returns t if DIR contains FILE (or any file if FILE is a list).

For example:

    (disaster-arm--dir-has-file \"/home/jart/\" \".bashrc\")
    (disaster-arm--dir-has-file \"/home/jart/\" (list \".bashrc\" \".screenrc\"))"
  (let ((res nil)
        (dir (file-name-as-directory dir))
        (files (if (listp file)
                   file
                 (list file))))
    (while (and files (not res))
      (setq res (file-exists-p (concat dir (car files)))
            files (cdr files)))
    res))

(defun disaster-arm-find-project-root (&optional looks file)
  "General-purpose Heuristic to detect bottom directory of project.

This works by scanning parent directories of FILE (using
`disaster-arm--find-parent-dirs') for certain types of files like a
`.git/` directory or a `Makefile` (which is less preferred).

The canonical structure of LOOKS is a list of lists of files
to look for in each parent directory where sublists are ordered
from highest precedence to lowest.  However you may specify
LOOKS as a single string or a list of strings for your
convenience. If LOOKS is not specified, it'll default to
`disaster-arm-project-root-files'."

  (let ((res nil)
        (looks (if looks
                   (if (listp looks)
                       (if (listp (car looks))
                           looks
                         (list looks))
                     (list (list looks)))
                 disaster-arm-project-root-files))
        (parents (disaster-arm--find-parent-dirs file)))
    (while (and looks (null res))
      (while (and parents (null res))
        (setq res (if (disaster-arm--dir-has-file
                       (car parents) (car looks))
                      (car parents))
              parents (cdr parents)))
      (setq looks (cdr looks)))
    res))

(provide 'disaster-arm)

;;; disaster-arm.el ends here

;;; fpaste.el --- Send text to http://fpaste.org

;; Copyright (C) 2014 Sergio Durigan Junior

;; Author: Sergio Durigan Junior <sergiodj (at) sergiodj (dot) net>
;; Maintainer: Sergio Durigan Junior <sergiodj (at) sergiodj (dot) net>
;; URL: http://git.sergiodj.net/?p=fpaste-el.git;a=summary
;; Version: 0.1.3
;; Created: 25th October 2014
;; Keywords: fpaste, paste

;; This file is not (yet) part of GNU Emacs.
;; However, it is distributed under the same license.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This mode has been made because I could not find any other mode for
;; Emacs that could send text to fpaste (Fedora pastebin).  I like
;; fpaste because of its simple interface and easy API.  It is
;; actually running StickyNotes.

;; This file only needs the url and json packages, which should be
;; available in your Emacs if you are using a recent enough version
;; (>= 23).

;;; Usage:

;; This mode basically exports one function:
;;
;;   M-x fpaste
;;
;;     This function will paste the selected region to fpaste.  If you
;;     do not have a region active, it will send the whole buffer
;;     instead.

;;; Code:

(require 'url)
(require 'url-http)
(require 'json)

;;;###autoload
(defgroup fpaste nil
  "Fpaste -- fpaste.org client"
  :tag "fpaste"
  :group 'comm)

(defcustom fpaste-user nil
  "The username of the paste author.  Default is nil."
  :group 'fpaste
  :type '(string))

(defcustom fpaste-password nil
  "The password for each paste.  Default is nil."
  :group 'fpaste
  :type '(string))

(defcustom fpaste-private t
  "Whether the pastes are private or not.  Default is t."
  :group 'fpaste
  :type '(boolean))

(defcustom fpaste-expire-time (* 24 60 60)
  "Time (in seconds) after which the paste will be deleted from
the server.  Set this to zero to disable this feature.  Default
is `(* 24 60 60)' (1 day)."
  :group 'fpaste)

;; This alist was copied from pastebin.el.  Its purpose is to map the
;; current major mode with a language string supported by fpaste, so
;; that the paste will be properly highlighted on the web.
(defvar fpaste-lang-type-assoc
  '((actionscript-mode . " actionscript")
    (ada-mode . "ada")
    (asm-mode . "asm")
    (autoconf-mode . "bash")
    (bibtex-mode . "bibtex")
    (cmake-mode . "cmake")
    (c-mode . "c")
    (c++-mode . "cpp")
    (cobol-mode . "cobol")
    (conf-colon-mode . "properties")
    (conf-javaprop-mode . "properties")
    (conf-mode . "ini")
    (conf-space-mode . "properties")
    (conf-unix-mode . "ini")
    (conf-windows-mode . "ini")
    (cperl-mode . "perl")
    (csharp-mode . "csharp")
    (css-mode . "css")
    (delphi-mode . "delphi")
    (diff-mode . "dff")
    (ebuild-mode . "bash")
    (eiffel-mode . "eiffel")
    (emacs-lisp-mode . "lisp")
    (erlang-mode . "erlang")
    (erlang-shell-mode . "erlang")
    (espresso-mode . "javascript")
    (fortran-mode . "fortran")
    (glsl-mode . "glsl")
    (gnuplot-mode . "gnuplot")
    (graphviz-dot-mode . "dot")
    (haskell-mode . "haskell")
    (html-mode . "html4strict")
    (idl-mode . "idl")
    (inferior-haskell-mode . "haskell")
    (inferior-octave-mode . "octave")
    (inferior-python-mode . "python")
    (inferior-ruby-mode . "ruby")
    (java-mode . "java")
    (js2-mode . "javascript")
    (jython-mode . "python")
    (latex-mode . "latex")
    (lisp-mode . "lisp")
    (lua-mode . "lua")
    (makefile-mode . "make")
    (makefile-automake-mode . "make")
    (makefile-gmake-mode . "make")
    (makefile-makepp-mode . "make")
    (makefile-bsdmake-mode . "make")
    (makefile-imake-mode . "make")
    (matlab-mode . "matlab")
    (nxml-mode . "xml")
    (oberon-mode . "oberon2")
    (objc-mode . "objc")
    (ocaml-mode . "ocaml")
    (octave-mode . "matlab")
    (pascal-mode . "pascal")
    (perl-mode . "perl")
    (php-mode . "php")
    (plsql-mode . "plsql")
    (po-mode . "gettext")
    (prolog-mode . "prolog")
    (python-2-mode . "python")
    (python-3-mode . "python")
    (python-basic-mode . "python")
    (python-mode . "python")
    (ruby-mode . "ruby")
    (scheme-mode . "lisp")
    (shell-mode . "bash")
    (sh-mode . "bash")
    (smalltalk-mode . "smalltalk")
    (sql-mode . "sql")
    (tcl-mode . "tcl")
    (visual-basic-mode . "vb")
    (xml-mode . "xml")
    (yaml-mode . "properties"))
  "Alist composed of major-mode names and corresponding fpaste
highlight formats.")

(defun fpaste--get-language ()
  "Return the language currently used in the buffer, according to
  the major mode being used"
  (or (assoc-default major-mode fpaste-lang-type-assoc) "text"))

(defun fpaste--url-callback (args)
  "Callbak to url-retrieve."
  (cond
   ((equal :error (car args))
    (signal 'fpaste-error (cdr args)))
   (t
    (re-search-forward "\n\n")
    (let* ((b (buffer-substring (point) (point-max)))
	   (json-obj (json-read-from-string b))
	   (res (assoc-default 'result json-obj))
	   (id (assoc-default 'id res))
	   (hash (if fpaste-private
		     (concat "/" (assoc-default 'hash res))
		   ""))
	   (final-url (format "http://fpaste.org/%s%s" id hash)))
      (with-temp-buffer
	(insert final-url)
        ;; save to x selection and clipboard
        (let ((x-select-enable-primary t)
              (x-select-enable-clipboard t))
          (kill-ring-save (point-min) (point-max))))
      (message "Your fpaste URL is <%s>.  It is also in the killring."
	       final-url)))))

(defun fpaste--send-paste (data)
  "Send DATA to fpaste.org."
  (let* ((url-request-method "POST")
	 (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
	 (fpaste-url "http://fpaste.org/")
	 (url-request-data (concat (format "paste_data=%s"
					   (url-hexify-string data))

				   ; The language of the paste.
				   (format "&paste_lang=%s"
					   (url-hexify-string
					    (fpaste--get-language)))

				   ; api_submit needs to be always true for us.
				   "&api_submit=true"

				   ; We only support JSON replies for now.
				   "&mode=json"

				   ; Is the paste going to be private?
				   (if fpaste-private
				       "&paste_private=yes"
				     "&paste_private=no")

				   ; Does the paste belong to a user?
				   (and fpaste-user
					(format "&paste_user=%s"
						(url-hexify-string fpaste-user)))

				   ; Will the paste be password
				   ; protected?  Note that it is
				   ; possible to password-protect a
				   ; post without specifying a user.
				   (and fpaste-password
					(format "&paste_password=%s"
						(url-hexify-string
						 fpaste-password)))

				   ; What is the expire time of the paste?
				   (format "&paste_expire=%d"
					   (if (> fpaste-expire-time 0)
					       fpaste-expire-time
					     0))))
	 (result (url-retrieve fpaste-url 'fpaste--url-callback)))))

(defun fpaste--paste-1 (pmin pmax)
  "Helper function to paste a chunk of data between PMIN and
PMAX."
  (let ((data (buffer-substring-no-properties pmin pmax)))
    (when (null data)
      (error "There is no data selected to be pasted"))
    (fpaste--send-paste data)))

;;;###autoload
(defun fpaste ()
  "Send the text to fpaste.  If there is a region selected, send it.
Otherwise, send the whole buffer."
  (interactive)
  (if (use-region-p)
      (fpaste--paste-1 (region-beginning) (region-end))
    (fpaste--paste-1 (point-min) (point-max))))

(provide 'fpaste)

;;; fpaste.el ends here

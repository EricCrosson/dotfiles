;;; org-eldoc.el --- display org header and src block info using eldoc

;; Copyright (c) 2014 Łukasz Gruner

;; Author: Łukasz Gruner <lukasz@gruner.lu>
;; Maintainer: Łukasz Gruner <lukasz@gruner.lu>
;; Version: 20141018.1633
;; X-Original-Version: 4
;; Package-Requires: ((org "8"))
;; URL: https://bitbucket.org/ukaszg/org-eldoc
;; Created: 25/05/2014
;; Keywords: eldoc, outline, breadcrumb, org, babel, minibuffer

;; This file is not part of Emacs.


;; This work ‘as-is’ we provide.
;; No warranty express or implied.
;; We’ve done our best,
;; to debug and test.
;; Liability for damages denied.
;;
;; Permission is granted hereby,
;; to copy, share, and modify.
;; Use as is fit,
;; free or for profit.
;; These rights, on this notice, rely.


;;; Commentary:

;; To enable, put the following in your .emacs file:
;;
;; (org-eldoc-hook-setup) ;; have org-eldoc add itself to `org-mode-hook'
;; OR run:
;; (org-eldoc-load)       ;; to setup org-eldoc and enable `eldoc-mode'
;;
;;
;; Report bugs and feature requests at:
;;
;; https://bitbucket.org/ukaszg/org-eldoc/issues


;;; Code:

(require 'org)
(require 'ob-core)
(require 'eldoc)

(defgroup org-eldoc nil "" :group 'org)

(defcustom org-eldoc-breadcrumb-separator "/"
  "Breadcrumb separator."
  :group 'org-eldoc
  :type 'string)

(defcustom org-eldoc-test-buffer-name " *Org-eldoc test buffer*"
  "Name of the buffer used while testing for mode-local variable values."
  :group 'org-eldoc
  :type 'string)

(defun org-eldoc-get-breadcrumb ()
  "Return breadcrumb if on a headline or nil."
  (let ((case-fold-search t) cur)
    (save-excursion
      (beginning-of-line)
      (save-match-data
        (when (looking-at org-complex-heading-regexp)
          (setq cur (match-string 4))
          (org-format-outline-path
           (append (org-get-outline-path) (list cur))
           (frame-width) "" org-eldoc-breadcrumb-separator))))))

(defun org-eldoc-get-src-header ()
  "Returns lang and list of header properties if on src definition line and nil otherwise."
  (let ((case-fold-search t) info lang hdr-args)
    (save-excursion
      (beginning-of-line)
      (save-match-data
        (when (looking-at "^[ \t]*#\\+\\(begin\\|end\\)_src")
          (setq info (org-babel-get-src-block-info 'light)
                lang (propertize (nth 0 info) 'face 'font-lock-string-face)
                hdr-args (nth 2 info))
          (concat
           lang
           ": "
           (mapconcat
            (lambda (elem)
              (when (and (cdr elem) (not (string= "" (cdr elem))))
                (concat
                 (propertize (symbol-name (car elem)) 'face 'org-list-dt)
                 " "
                 (propertize (cdr elem) 'face 'org-verbatim)
                 " ")))
            hdr-args " ")))))))

(defun org-eldoc-get-src-lang ()
  "Return value of lang for the current block if in block body and nil otherwise."
  (let ((case-fold-search t))
    (save-match-data
      (when (org-between-regexps-p ".*#\\+begin_src"
                                   ".*#\\+end_src")
        (save-excursion
          (goto-char (org-babel-where-is-src-block-head))
          (car (org-babel-parse-src-block-match)))))))

(defvar org-eldoc-local-functions-cache (make-hash-table :size 40 :test 'equal)
  "Cache of major-mode's eldoc-documentation-functions,
 used by \\[org-eldoc-get-mode-local-documentation-function].")

(defun org-eldoc-get-mode-local-documentation-function (lang)
  "Check if LANG-mode sets eldoc-documentation-function and return its value."
  (let ((cached-func (gethash lang org-eldoc-local-functions-cache 'empty))
        (mode-func (intern-soft (format "%s-mode" lang)))
        doc-func)
    (if (eq 'empty cached-func)
        (progn
          (when (fboundp mode-func)
            (with-current-buffer (generate-new-buffer org-eldoc-test-buffer-name)
              (funcall mode-func)
              (setq doc-func (and eldoc-documentation-function
                                  (symbol-value 'eldoc-documentation-function)))
              (puthash lang doc-func org-eldoc-local-functions-cache)
              (kill-buffer)))
          doc-func)
    cached-func)))

(declare-function c-eldoc-print-current-symbol-info "c-eldoc" ())
(declare-function css-eldoc-function "css-eldoc" ())
(declare-function php-eldoc-function "php-eldoc" ())
(declare-function go-eldoc--documentation-function "go-eldoc" ())

(defun org-eldoc-documentation-function ()
  "Return breadcrumbs when on a headline, args for src block header-line,
  calls other documentation functions depending on lang when inside src body."
  (or
   (org-eldoc-get-breadcrumb)
   (org-eldoc-get-src-header)
   (let ((lang (org-eldoc-get-src-lang)))
     (cond ((or
             (string= lang "c") ;; http://github.com/nflath/c-eldoc
             (string= lang "C")) (when (require 'c-eldoc nil t)
                                   (c-eldoc-print-current-symbol-info)))
           ;; https://github.com/zenozeng/css-eldoc
           ((string= lang "css") (when (require 'css-eldoc nil t)
                                   (css-eldoc-function)))
           ;; https://github.com/zenozeng/php-eldoc
           ((string= lang "php") (when (require 'php-eldoc nil t)
                                   (php-eldoc-function)))
           ((or
             (string= lang "go")
             (string= lang "golang")) (when (require 'go-eldoc nil t)
                                        (go-eldoc--documentation-function)))
           (t (let ((doc-fun (org-eldoc-get-mode-local-documentation-function lang)))
                (when (fboundp doc-fun) (funcall doc-fun))))))))

;;;###autoload
(defun org-eldoc-load ()
  "Set up org-eldoc and enable `eldoc-mode'."
  (interactive)
  (setq-local eldoc-documentation-function #'org-eldoc-documentation-function)
  (eldoc-mode 1))

;;;###autoload
(defun org-eldoc-hook-setup ()
  "Add org-eldoc initialization code to `org-mode-hook'."
  (add-hook 'org-mode-hook #'org-eldoc-load))

(provide 'org-eldoc)

;; -*- coding: utf-8-emacs; -*-

;;; org-eldoc.el ends here

;;; package-safe-delete.el --- Safely delete package.el packages -*- lexical-binding: t -*-

;; Author: Fanael Linithien <fanael4@gmail.com>
;; URL: https://github.com/Fanael/package-safe-delete
;; Version: 20141021.1011
;; X-Original-Version: 0.1.4
;; Package-Requires: ((emacs "24.3") (epl "0.7-cvs"))

;; This file is NOT part of GNU Emacs.

;; Copyright (c) 2014, Fanael Linithien
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;; * Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;; * Redistributions in binary form must reproduce the above copyright
;; notice, this list of conditions and the following disclaimer in the
;; documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; Delete package.el packages safely, without leaving unresolved dependencies.

;;; Code:
(eval-when-compile (require 'cl-lib))
(require 'epl)

(defgroup package-safe-delete nil
  "Safely remove package.el packages."
  :group 'package)

(defcustom package-safe-delete-required-packages '()
  "List of explicitly required packages.
Each element is a package name, as a symbol.

The packages in this list are treated as required by a dummy package, and thus
are never deleted."
  :type '(repeat symbol)
  :group 'package-safe-delete)

(cl-defstruct (package-safe-delete--packages
               (:constructor nil)
               (:constructor package-safe-delete--packages-make-internal
                             (packages names))
               (:copier nil)
               (:predicate nil)
               (:type vector) :named)
  packages
  names)

(defun package-safe-delete--installed-packages ()
  "Create a `package-safe-delete--packages' containing all installed packages."
  (let* ((installed (epl-installed-packages))
         (names (mapcar #'epl-package-name installed)))
    (package-safe-delete--packages-make-internal installed names)))

(defun package-safe-delete--list-to-hashtable (list)
  "Convert a LIST based set to a hashtable based set."
  (let ((result (make-hash-table :test #'eq)))
    (dolist (elt list)
      (puthash elt t result))
    result))

(defun package-safe-delete--installed-package-dependencies (installed excluded)
  "Get a dependency tree of the installed packages.
INSTALLED is a `package-safe-delete--packages' containing all installed
packages.
Dependencies of EXCLUDED packages are ignored.

The returned value is a hash table of the form package => list of packages
requiring it."
  (let ((installednames (package-safe-delete--packages-names installed))
        (dependencies (make-hash-table :test #'eq)))
    (dolist (package (package-safe-delete--packages-packages installed))
      (let ((packagename (epl-package-name package)))
        (unless (memq packagename excluded)
          (dolist (requirement (epl-package-requirements package))
            (let ((requirementname (epl-requirement-name requirement)))
              (when (memq requirementname installednames)
                (push packagename (gethash requirementname dependencies))))))))
    (let ((requiredpackagesymbol (make-symbol "<required-package>")))
      (dolist (requiredpackage package-safe-delete-required-packages)
        (push requiredpackagesymbol (gethash requiredpackage dependencies))))
    dependencies))

(defun package-safe-delete--delete (packages force)
  "Delete PACKAGES.

PACKAGES is a list of package name symbols.
With FORCE non-nil, the user is not prompted for confirmation before the
packages are deleted."
  (when (or force
            (yes-or-no-p
             (pcase packages
               (`(,package)
                (format "Delete package `%s'? " package))
               (_
                (format "Delete these packages: %s? "
                        (mapconcat #'symbol-name packages ", "))))))
    (dolist (package packages)
      (mapc #'epl-package-delete (epl-find-installed-packages package)))))

(defun package-safe-delete--prompt-package-name (prompt)
  "Read a package name in the minibuffer.
PROMPT is a string to prompt with."
  (list
   (intern
    (completing-read prompt
                     (mapcar #'epl-package-name (epl-installed-packages))
                     nil
                     t))))

(defun package-safe-delete--ensure-installed (packages)
  "Ensure all PACKAGES are installed.
If at least one is not installed, an error is signaled."
  (dolist (package packages)
    (unless (epl-package-installed-p package)
      (error "Package `%S' is not installed" package))))

(defun package-safe-delete--ensure-no-dependencies (packages dependencies)
  "Ensure no installed package relies on the PACKAGES being deleted.
If any other installed package requires a package in PACKAGES, an error is
signaled.

DEPENDENCIES is a dependency tree as generated by
`package-safe-delete--installed-package-dependencies'."
  (dolist (package packages)
    (pcase (gethash package dependencies)
      (`nil)
      (`(,dependentpackage)
       (error "Cannot delete `%S' because it's required by `%S'"
              package
              dependentpackage))
      (dependentpackages
       (error "Cannot delete `%S' because it's required by: %s"
              package
              (mapconcat #'symbol-name dependentpackages ", "))))))

;;;###autoload
(defun package-safe-delete-packages (packages &optional force)
  "Delete PACKAGES.

PACKAGES is a list of package name symbols.
None of the PACKAGES are deleted when there's a package depending on one of
them, or if one of the PACKAGES is not installed.
With FORCE non-nil, the user is not prompted for confirmation before the
packages are deleted."
  (package-safe-delete--ensure-installed packages)
  (let* ((installed (package-safe-delete--installed-packages))
         (dependencies (package-safe-delete--installed-package-dependencies
                        installed
                        packages)))
    (package-safe-delete--ensure-no-dependencies packages dependencies))
  (package-safe-delete--delete packages force))

;;;###autoload
(defun package-safe-delete (package)
  "Delete a PACKAGE.

PACKAGE is a package name symbol.
PACKAGE is not deleted when there are other packages requiring it.
Interactively, prompt for its name."
  (interactive (package-safe-delete--prompt-package-name "Delete package: "))
  (package-safe-delete-packages (list package)))

;;;###autoload
(defun package-safe-delete-packages-recursively (packages &optional force)
  "Delete PACKAGES.
Each of the PACKAGES and every packages required only by the PACKAGES are
deleted.

PACKAGES is a list of package name symbols.
None of the PACKAGES are deleted when there's a package depending on one of
them, or if one of the PACKAGES is not installed.
With FORCE non-nil, the user is not prompted for confirmation before the
packages are deleted."
  (package-safe-delete--ensure-installed packages)
  (let* ((installed (package-safe-delete--installed-packages))
         (installednames (package-safe-delete--packages-names installed))
         (dependencies (package-safe-delete--installed-package-dependencies
                        installed
                        packages)))
    (package-safe-delete--ensure-no-dependencies packages dependencies)
    ;; Convert those lists into hash tables because they're less of a hassle to
    ;; modify.
    (maphash (lambda (k v)
               (puthash k (package-safe-delete--list-to-hashtable v) dependencies))
             dependencies)
    (let ((totalpackages '()))
      (while packages
        (let ((pendingdependencies '()))
          (dolist (packagename packages)
            (dolist (package (epl-find-installed-packages packagename))
              (dolist (requirement (epl-package-requirements package))
                (let* ((requirementname (epl-requirement-name requirement))
                       (requirementbucket (gethash requirementname dependencies)))
                  (when (or (null requirementbucket)
                            (progn
                              (remhash packagename requirementbucket)
                              ;; Was `package' the last package requiring
                              ;; `requirement'?
                              (= 0 (hash-table-count requirementbucket))))
                    (when (memq requirementname installednames)
                      (push requirementname pendingdependencies)))))))
          ;; We're done with `packages', handle their direct dependencies now.
          (setq totalpackages (append packages totalpackages))
          (setq packages pendingdependencies)))
      (setq packages totalpackages)))
  (package-safe-delete--delete packages force))

;;;###autoload
(defun package-safe-delete-recursively (package)
  "Recursively delete a PACKAGE.
PACKAGE and all packages required only by it are deleted.

PACKAGE is a package name symbol.
PACKAGE is not deleted when there are other packages requiring it.
Interactively, prompt for its name."
  (interactive (package-safe-delete--prompt-package-name
                "Recursively delete package: "))
  (package-safe-delete-packages-recursively (list package)))

;;;###autoload
(defun package-safe-delete-all (&optional force)
  "Delete all packages not explicitly required.

With FORCE non-nil, the user is not prompted for confirmation before the
packages are deleted."
  (interactive)
  (let* ((installed (package-safe-delete--installed-packages))
         (installednames (package-safe-delete--packages-names installed))
         (dependencies (package-safe-delete--installed-package-dependencies
                        installed
                        '()))
         (packagestodelete '()))
    ;; Collect only those packages not required by the user and not required by
    ;; other packages, `package-safe-delete-packages-recursively' will take care
    ;; of the rest.
    (dolist (packagename installednames)
      (when (and (null (gethash packagename dependencies))
                 (null (memq packagename
                             package-safe-delete-required-packages)))
        (push packagename packagestodelete)))
    (package-safe-delete-packages-recursively packagestodelete force)))

(provide 'package-safe-delete)
;;; package-safe-delete.el ends here

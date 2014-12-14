;;; vlf-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (vlf) "vlf" "vlf.el" (21398 9795 632016 815000))
;;; Generated autoloads from vlf.el

(autoload 'vlf "vlf" "\
View Large FILE in batches.
You can customize number of bytes displayed by customizing
`vlf-batch-size'.
Return newly created buffer.

\(fn FILE)" t nil)

;;;***

;;;### (autoloads (vlf-ediff-files vlf-ediff-buffers) "vlf-ediff"
;;;;;;  "vlf-ediff.el" (21398 9795 608683 484000))
;;; Generated autoloads from vlf-ediff.el

(autoload 'vlf-ediff-buffers "vlf-ediff" "\
Run batch by batch ediff over VLF buffers BUFFER-A and BUFFER-B.
Batch size is determined by the size in BUFFER-A.
Requesting next or previous difference at the end or beginning
respectively of difference list, runs ediff over the adjacent chunks.

\(fn BUFFER-A BUFFER-B)" t nil)

(autoload 'vlf-ediff-files "vlf-ediff" "\
Run batch by batch ediff over FILE-A and FILE-B.
Files are processed with VLF with BATCH-SIZE chunks.
Requesting next or previous difference at the end or beginning
respectively of difference list, runs ediff over the adjacent chunks.

\(fn FILE-A FILE-B BATCH-SIZE)" t nil)

;;;***

;;;### (autoloads nil nil ("vlf-base.el" "vlf-follow.el" "vlf-integrate.el"
;;;;;;  "vlf-occur.el" "vlf-pkg.el" "vlf-search.el" "vlf-write.el")
;;;;;;  (21398 9795 708350 135000))

;;;***

(provide 'vlf-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; vlf-autoloads.el ends here

;;; auto-overlays-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "auto-overlay-common" "auto-overlay-common.el"
;;;;;;  (21723 58811 93835 791000))
;;; Generated autoloads from auto-overlay-common.el

(autoload 'auto-overlays-at-point "auto-overlay-common" "\
Return overlays overlapping POINT
\(or the point, if POINT is null). If PROP-TEST is supplied, it
should be a list which specifies a property test with one of the
following forms (or a list of such lists if more than one
property test is required):

  (FUNCTION PROPERTY)

  (FUNCTION PROPERTY VALUE)

  (FUNCTION (PROPERTY1 PROPERTY2 ...) (VALUE1 VALUE2 ...))

where PROPERTY indicates an overlay property name (a symbol), and
VALUE indicates an arbitrary value or lisp expression.

For each overlay overlapping POINT, first the values
corresponding to the property names are retrieved from the
overlay, then FUNCTION is called with the properties values
followed by the other values as its arguments. The test is
satisfied if the result is non-nil, otherwise it fails. Tests are
evaluated in order, but only up to the first failure. Only
overlays that satisfy all property tests are returned.

If INACTIVE is non-nil, both active and inactive overlays are
returned (usually inactive ones are ignored).

Note that this function returns any overlay. If you want to
restrict it to auto overlays, include '(identity auto-overlay) in
PROP-TEST.

\(fn &optional POINT PROP-TEST INACTIVE)" nil nil)

(autoload 'auto-overlays-in "auto-overlay-common" "\
Return auto overlays overlapping region between START and END.

If PROP-TEST is supplied, it should be a list which specifies a
property test with one of the following forms (or a list of such
lists if more than one property test is required):

  (FUNCTION PROPERTY)

  (FUNCTION PROPERTY VALUE)

  (FUNCTION (PROPERTY1 PROPERTY2 ...) (VALUE1 VALUE2 ...))

where PROPERTY indicates an overlay property name (a symbol), and
VALUE indicates an arbitrary value or lisp expression.

For each overlay between START and END, first the values
corresponding to the property names are retrieved from the
overlay, then FUNCTION is called with the properties values
followed by the other values as its arguments. The test is
satisfied if the result is non-nil, otherwise it fails. Tests are
evaluated in order, but only up to the first failure. Only
overlays that satisfy all property tests are returned.

If WITHIN is non-nil, only overlays entirely within START and END
are returned. If INACTIVE is non-nil, both active and inactive
overlays are returned (usually inactive ones are ignored).

Note that this function returns any overlay. If you want to
restrict it to auto overlays, include '(identity auto-overlay) in
PROP-TEST.

\(fn START END &optional PROP-TEST WITHIN INACTIVE)" nil nil)

(autoload 'auto-overlay-highest-priority-at-point "auto-overlay-common" "\
Return highest priority overlay at POINT (defaults to the point).

If two overlays have the same priority, the innermost one takes
precedence (i.e. the one that begins later, or if they begin at
the same point the one that ends earlier).

See `auto-overlays-at' for ane explanation of the PROPTEST argument.

\(fn &optional POINT PROPTEST)" nil nil)

(autoload 'auto-overlay-local-binding "auto-overlay-common" "\
Return \"overlay local \" binding of SYMBOL at POINT,
or the current local binding if there is no overlay binding. If
there is no overlay binding and SYMBOL is not bound, return
nil. POINT defaults to the point.

If ONLY-OVERLAY is non-nil, only overlay bindings are
returned. If none exists at POINT, nil is returned

An \"overlay local\" binding is created by giving an overlay a
non-nil value for a property named SYMBOL. If more than one
overlay at POINT has a non-nil SYMBOL property, the value from
the highest priority overlay is returned.

See `auto-overlay-highest-priority-at-point' for a definition of
\"highest priority\".

\(fn SYMBOL &optional POINT ONLY-OVERLAY)" nil nil)

;;;***

;;;### (autoloads nil "auto-overlays" "auto-overlays.el" (21723 58811
;;;;;;  3835 796000))
;;; Generated autoloads from auto-overlays.el

(autoload 'auto-overlay-load-definition "auto-overlays" "\
Load DEFINITION into the set of auto-overlay definitions SET-ID
in the current buffer. If SET-ID does not exist, it is created.

If POS is nil, DEFINITION is added at the end of the list of
auto-overlay definitions. If it is t, it is added at the
beginning. If it is an integer, it is added at that position in
the list. The position in the list makes no difference to the
behaviour of the auto-overlays. But it can make a difference to
the speed and efficiency. In general, higher-priority and
exclusive DEFINITIONS should appear earlier in the list.

If DEFINITION-ID is supplied, it should be a symbol that can be
used to uniquely identify DEFINITION (see
`auto-overlay-unload-definition').


DEFINITION should be a list of the form:

  (CLASS @optional :id DEFINITION-ID @rest REGEXP1 REGEXP2 ... )

CLASS is a symbol specifying the auto-overlay class. The standard
classes are 'word, 'line, 'self, 'flat and 'nested. The :id
property is optional. It should be a symbol that can be used to
uniquely identify DEFINITION (see
`auto-overlay-unload-definition').

The REGEXP's should be lists of the form:

  (RGXP &optional :edge EDGE :id REGEXP-ID
        &rest PROPERTY1 PROPERTY2 ... )

RGXP is either a single regular expression (a string), or a cons
cell of the form (RGXP . GROUP) where RGXP is a regular
expression and GROUP is an integer specifying which group in the
regular expression forms the delimiter for the auto-overlay. The
rest of the PROPERTY entries should be cons cells of the
form (NAME . VALUE) where NAME is an overlay property name (a
symbol) and VALUE is its value.

The properties :edge and :id are optional. The :edge property
EDGE should be one of the symbols 'start or 'end. If it is not
specified, :edge is assumed to be 'start. The :id property is a
symbol that can be used to uniquely identify REGEXP (see
`auto-overlay-unload-regexp').

\(fn SET-ID DEFINITION &optional POS)" nil nil)

(autoload 'auto-overlay-load-regexp "auto-overlays" "\
Load REGEXP into the auto-overlay definition identified by
DEFINITION-ID in the regexp list named SET-ID in the current
buffer.

If POS is nil, REGEXP is added at the end of the definition. If
it is t, it is added at the beginning. If it is an integer, it is
added at that position.


REGEXP should be a list of the form:

  (RGXP &optional :edge EDGE :id REGEXP-ID
        &rest PROPERTY1 PROPERTY2 ... )

RGXP is either a single regular expression (a string), or a cons
cell of the form (RGXP . GROUP) where RGXP is a regular
expression and GROUP is an integer specifying which group in the
regular expression forms the delimiter for the auto-overlay. The
rest of the PROPERTY entries should be cons cells of the
form (NAME . VALUE) where NAME is an overlay property name (a
symbol) and VALUE is its value.

The properties :edge and :id are optional. The :edge property
EDGE should be one of the symbols 'start or 'end. If it is not
specified, :edge is assumed to be 'start. The :id property is a
symbol that can be used to uniquely identify REGEXP (see
`auto-overlay-unload-regexp').

\(fn SET-ID DEFINITION-ID REGEXP &optional POS)" nil nil)

(autoload 'auto-overlay-share-regexp-set "auto-overlays" "\
Make TO-BUFFER share the regexp set identified by SET-ID with FROM-BUFFER.
Any changes to that regexp set in either buffer will be reflected in the
other. TO-BUFFER defaults to the current buffer.

\(fn SET-ID FROM-BUFFER &optional TO-BUFFER)" nil nil)

(autoload 'auto-overlay-load-overlays "auto-overlays" "\
Load overlays for BUFFER from FILE.
Returns t if successful, nil otherwise.
Defaults to the current buffer.

If FILE is null, or is a string that only specifies a directory,
the filename is constructed from the buffer's file name and
SET-ID. If the buffer is not associated with a file and FILE
doesn't specify a full filename, an error occurs.

The FILE should be generated by `auto-overlay-save-overlays'. By
default, the buffer contents and regexp definitions for SET-ID
will be checked to make sure neither have changed since the
overlays were saved. If they don't match, the saved overlay data
will not be loaded, and the function will return nil.

If NO-REGEXP-CHECK is non-nil, the check for matching regexp
definitions will be skipped; the saved overlays will be loaded
even if different regexp definitions were active when the
overlays were saved.

\(fn SET-ID &optional BUFFER FILE NO-REGEXP-CHECK)" nil nil)

;;;***

;;;### (autoloads nil nil ("auto-overlay-flat.el" "auto-overlay-line.el"
;;;;;;  "auto-overlay-nested.el" "auto-overlay-self.el" "auto-overlay-word.el"
;;;;;;  "auto-overlays-compat.el" "auto-overlays-pkg.el") (21723
;;;;;;  58811 116592 442000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; auto-overlays-autoloads.el ends here

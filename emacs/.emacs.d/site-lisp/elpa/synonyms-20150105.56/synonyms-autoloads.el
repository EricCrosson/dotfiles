;;; synonyms-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "synonyms" "synonyms.el" (21738 3241 192755
;;;;;;  761000))
;;; Generated autoloads from synonyms.el

(let ((loads (get 'Synonyms 'custom-loads))) (if (member '"synonyms" loads) nil (put 'Synonyms 'custom-loads (cons '"synonyms" loads))))

(defface synonyms-heading '((((background dark)) (:foreground "Yellow")) (t (:foreground "Blue"))) "\
*Face for different synonym types." :group (quote Synonyms) :group (quote faces))

(defface synonyms-search-text '((t (:foreground "Red"))) "\
*Face for the term whose synonyms were sought." :group (quote Synonyms) :group (quote faces))

(defface synonyms-link '((((background dark)) (:foreground "Yellow" :underline t)) (t (:foreground "Blue" :underline t))) "\
*Face for history links." :group (quote Synonyms) :group (quote faces))

(defface synonyms-mouse-face '((((background dark)) (:background "DarkCyan")) (t (:background "Cyan"))) "\
*Mouse face for the term whose synonyms were sought." :group (quote Synonyms) :group (quote faces))

(defvar synonyms-append-result-flag nil "\
*t means that `synonyms' appends search result to previous results.
No other value, besides t, has this effect.

This can be overridden by using a negative prefix argument,
for example, `M--'.  If you use `C-u C-u', then both this and
`synonyms-match-more-flag' are overridden.")

(custom-autoload 'synonyms-append-result-flag "synonyms" t)

(defvar synonyms-cache-file "" "\
*Location to write cache file containing synonyms.
Written to save the list of synonyms used for completion.
This is an absolute (complete-path) location, including the file name.")

(custom-autoload 'synonyms-cache-file "synonyms" t)

(defvar synonyms-file "" "\
*Location of thesaurus file `mthesaur.txt'.
This is an absolute (complete-path) location, including the file name.")

(custom-autoload 'synonyms-file "synonyms" t)

(defvar synonyms-fill-column 80 "\
*Synonyms* buffer text is wrapped (filled) to this many columns.")

(custom-autoload 'synonyms-fill-column "synonyms" t)

(defvar synonyms-match-more-flag nil "\
*t means additional thesaurus entries can be matched by `synonyms'.
No other value, besides t, has this effect.

A value of t means two things:
 1) Input can match parts of synonyms, in addition to whole synonyms.
 2) All synonyms are shown, even if input matches a thesaurus entry.

This can be overridden by using a positive prefix argument,
  for example, `C-u'.  If you use `C-u C-u', then both this and
`synonyms-append-result-flag' are overridden.")

(custom-autoload 'synonyms-match-more-flag "synonyms" t)

(defvar synonyms-mode-hook nil "\
*Normal hook run when entering Thesaurus mode.")

(custom-autoload 'synonyms-mode-hook "synonyms" t)

(defvar synonyms-use-cygwin-flag nil "\
*Non-nil means to double backslashes in arguments to `call-process'.
There is apparently a bug in the Emacs (at least versions 20-22) C
code that implements function `call-process' on MS Windows.  When
using native Windows Emacs with Cygwin commands, such as `grep', the C
code removes a level of backslashes, so string arguments supplied to
`call-process' need to have twice as many backslashes as they should
need.  If you are using Emacs on Windows and Cygwin `grep', then you
probably will want to use a non-nil value for
`synonyms-use-cygwin-flag'.")

(custom-autoload 'synonyms-use-cygwin-flag "synonyms" t)

(defvar synonyms-dictionary-url "http://dictionary.reference.com/search?q=" "\
*URL of a Web dictionary lookup.  Text to look up is appended to this.
See also `synonyms-dictionaries-url'.")

(custom-autoload 'synonyms-dictionary-url "synonyms" t)

(defvar synonyms-dictionary-alternate-url "http://www.onelook.com/?ls=b&w=" "\
*URL of a Web dictionary lookup.  Text to look up is appended to this.
The default value, \"http://www.onelook.com/?ls=b&w=\" lets you use `?'
and `*' as wildcards in the terms you look up.  These are not used as
regexp wildcards, however.  `?' stands for any single character, and
`*' stands for any sequence of characters.  In terms of regexp syntax,
`?' here is equivalent to the regexp `.', and `*' is equivalent to the
regexp `.*'.  See http://www.onelook.com/?c=faq#patterns for more
information on the allowed wildcard patterns.
See also `synonyms-dictionary-url'.")

(custom-autoload 'synonyms-dictionary-alternate-url "synonyms" t)

(autoload 'synonyms-mode "synonyms" "\
Major mode for browsing thesaurus entries (synonyms).
Like Text mode but with these additional key bindings:

 \\<synonyms-mode-map>\\[synonyms-mouse],     \\[synonyms-no-read],     \\[synonyms] - Look up synonyms for a word or phrase
 \\[synonyms-mouse-match-more],   \\[synonyms-match-more]   - Like \\[synonyms-no-read], but try to match more terms
 \\[synonyms-mouse-append-result],   \\[synonyms-append-result]   - Like \\[synonyms-no-read], but add result to previous result
 \\[synonyms-mouse-match-more+append-result], \\[synonyms-match-more+append-result] - Like \\[synonyms-match-more] and \\[synonyms-append-result] combined

 \\[scroll-up] - Scroll down through the buffer of synonyms
 \\[scroll-down] - Scroll up through the buffer of synonyms
 \\[describe-mode]   - Display this help
 \\[quit-window]   - Quit Synonyms mode

Of the various key bindings that look up synonyms, the most flexible
is \\[synonyms] - it prompts you for the search string to match.  This
can be a regular expression (regexp).  The other lookup bindings are
for convenience - just click.

In Synonyms mode, Transient Mark mode is enabled.

Options `synonyms-match-more-flag' and `synonyms-append-result-flag'
affect synonym matching and the results.  For convenience, \\[synonyms-mouse-match-more],
\\[synonyms-mouse-append-result], and \\[synonyms-mouse-match-more+append-result] toggle the effect of those options for the
duration of the command.

Note that even though Synonyms mode is similar to Text mode, buffer
`*Synonyms*' is read-only, by default - use `C-x C-q' to toggle.

Turning on Synonyms mode runs the normal hooks `text-mode-hook' and
`synonyms-mode-hook' (in that order).

\(fn)" t nil)

(autoload 'synonyms-ensure-synonyms-read-from-cache "synonyms" "\
Ensure synonyms are in `synonyms-obarray', from `synonyms-cache-file'.
If this file does not yet exist, then it and the obarray are created.
Creating the obarray for the first time takes 2-3 minutes.
This does nothing if the obarray is already complete.

\(fn)" t nil)

(autoload 'synonyms-make-obarray "synonyms" "\
Fill `synonyms-obarray' with the available synonyms.

\(fn)" t nil)

(autoload 'synonyms-write-synonyms-to-cache "synonyms" "\
Write synonyms in `synonyms-obarray' to file `synonyms-cache-file'.

\(fn)" t nil)

(autoload 'synonyms-no-read "synonyms" "\
Same as command `synonyms', but uses the default input text (regexp).

\(fn ARG)" t nil)

(autoload 'synonyms-match-more "synonyms" "\
Same as using `synonyms' with `synonyms-match-more-flag' = t.

\(fn)" t nil)

(autoload 'synonyms-match-more-no-read "synonyms" "\
Same as using `synonyms' with `synonyms-match-more-flag' = t.

\(fn ARG)" t nil)

(autoload 'synonyms-append-result "synonyms" "\
Same as using `synonyms' with `synonyms-append-result-flag' = t.

\(fn)" t nil)

(autoload 'synonyms-append-result-no-read "synonyms" "\
Same as using `synonyms' with `synonyms-append-result-flag' = t.

\(fn ARG)" t nil)

(autoload 'synonyms-match-more+append-result "synonyms" "\
Like `synonyms-match-more-flag' = `synonyms-append-result-flag' = t.

\(fn)" t nil)

(autoload 'synonyms-match-more+append-result-no-read "synonyms" "\
Like `synonyms-match-more-flag' = `synonyms-append-result-flag' = t.

\(fn ARG)" t nil)

(autoload 'synonyms-mouse "synonyms" "\
Show synonyms that match a regular expression (e.g. a word or phrase).
The regexp to match is the synonym or region clicked with mouse-2.  If
the region is active, but a synonym elsewhere is clicked, that synonym
is used, not the selected text.

You can either click a listed synonym, to see its synonyms, or select
one or more words and click the selection, to see matching synonyms.
To quickly select a series of words: double-click mouse-1 to select
the first word, then click mouse-3 to extend the selection to the last
word.

Selection is useful when you want to see synonyms of a similar term.
For example, instead of clicking the listed synonym `bleeding heart', you
might select `heart' and click that.

The prefix argument acts the same as for command `synonyms'.

If you click a history link with mouse-2, previously retrieved search
results are revisited.

\(fn EVENT ARG)" t nil)

(autoload 'synonyms-mouse-match-more "synonyms" "\
Same as `synonyms-mouse' with `synonyms-match-more-flag' = t.

\(fn EVENT ARG)" t nil)

(autoload 'synonyms-mouse-append-result "synonyms" "\
Same as `synonyms-mouse' with `synonyms-append-result-flag' = t.

\(fn EVENT ARG)" t nil)

(autoload 'synonyms-mouse-match-more+append-result "synonyms" "\
Like `synonyms-match-more-flag' = `synonyms-append-result-flag' = t.

\(fn EVENT ARG)" t nil)

(autoload 'synonyms-history-backward "synonyms" "\
Run `synonyms' on a previous argument, moving backward in the history.
A prefix argument has the same meaning as for command `synonyms'.

\(fn ARG)" t nil)

(autoload 'synonyms-history-forward "synonyms" "\
Run `synonyms' on a previous argument, moving forward in the history.
A prefix argument has the same meaning as for command `synonyms'.

\(fn ARG)" t nil)

(defalias 'dictionary-definition 'synonyms-definition)

(autoload 'synonyms-definition "synonyms" "\
Look up the definition of a word or phrase using online dictionaries.
The dictionary used is `synonyms-dictionary-url'.
With prefix arg, look up the definition in the alternate dictionary,
`synonyms-dictionary-alternate-url'.

\(fn SEARCH-TEXT ALTERNATE-P)" t nil)

(autoload 'synonyms-definition-no-read "synonyms" "\
Look up the definition of a word or phrase using online dictionaries.
The dictionary used is `synonyms-dictionary-url'.
With prefix arg, look up the definition in the alternate dictionary,
`synonyms-dictionary-alternate-url'.

\(fn ALTERNATE-P)" t nil)

(autoload 'synonyms-definition-mouse "synonyms" "\
Look up the definition of a word or phrase using online dictionaries.
The dictionary used is `synonyms-dictionary-url'.
With prefix arg, look up the definition in the alternate dictionary,
`synonyms-dictionary-alternate-url'.

\(fn EVENT ALTERNATE-P)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; synonyms-autoloads.el ends here

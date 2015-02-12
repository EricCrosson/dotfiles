;;; misc-cmds-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (view-X11-colors revert-buffer-no-confirm switch-to-alternate-buffer-other-window
;;;;;;  switch-to-alternate-buffer clear-search-histories clear-regexp-search-history
;;;;;;  clear-search-history indirect-buffer kill-buffer-and-its-windows
;;;;;;  chown chgrp chmod region-to-file region-to-buffer comment-region-lines
;;;;;;  delete-lines goto-long-line goto-longest-line region-length
;;;;;;  mark-buffer-before-point mark-buffer-after-point recenter-top-bottom-2
;;;;;;  recenter-top-bottom-1 recenter-top-bottom delete-window-maybe-kill-buffer
;;;;;;  delete-extra-windows-for-buffer indent-rigidly-tab-stops
;;;;;;  beginning-or-indentation beginning-of-line+ end-of-line+
;;;;;;  forward-char-same-line forward-overlay) "misc-cmds" "misc-cmds.el"
;;;;;;  (21526 154 918981 558000))
;;; Generated autoloads from misc-cmds.el

(autoload 'forward-overlay "misc-cmds" "\
Move forward ARG overlays.
Move cursor to next position where an overlay starts or ends.
If there are no more overlay boundaries, move to (point-max).

\(fn &optional ARG)" t nil)

(autoload 'forward-char-same-line "misc-cmds" "\
Move forward a max of ARG chars on the same line, or backward if ARG < 0.
Returns the signed number of chars moved if /= ARG, else returns nil.

\(fn &optional ARG)" t nil)

(autoload 'end-of-line+ "misc-cmds" "\
Move cursor to end of current line or end of next line if repeated.
This is similar to `end-of-line', but:
  If called interactively with no prefix arg:
     If the previous command was also `end-of-line+', then move to the
     end of the next line.  Else, move to the end of the current line.
  Otherwise, move to the end of the Nth next line (Nth previous line
     if N<0).  Command `end-of-line', by contrast, moves to the end of
     the (N-1)th next line.

\(fn &optional N)" t nil)

(autoload 'beginning-of-line+ "misc-cmds" "\
Move cursor to beginning of current line or next line if repeated.
This is the similar to `beginning-of-line', but:
1. With arg N, the direction is the opposite: this command moves
   backward, not forward, N lines.
2. If called interactively with no prefix arg:
      If the previous command was also `beginning-of-line+', then move
      to the beginning of the previous line.  Else, move to the
      beginning of the current line.
   Otherwise, move to the beginning of the Nth previous line (Nth next
      line if N<0).  Command `beginning-of-line', by contrast, moves to
      the beginning of the (N-1)th next line.

\(fn &optional N)" t nil)
 (autoload 'end-of-visual-line+ "misc-cmds")
 (autoload 'beginning-of-visual-line+ "misc-cmds")

(autoload 'beginning-or-indentation "misc-cmds" "\
Move cursor to beginning of this line or to its indentation.
If at indentation position of this line, move to beginning of line.
If at beginning of line, move to beginning of previous line.
Else, move to indentation position of this line.

With arg N, move backward to the beginning of the Nth previous line.
Interactively, N is the prefix arg.

\(fn &optional N)" t nil)

(autoload 'indent-rigidly-tab-stops "misc-cmds" "\
Indent the region rigidly according to the NTH tab stop.
`tab-stop-list' defines the available tab stops.  NTH is the numeric
prefix arg.  One means indent rigidly the amount given by the first
tab stop.  If NTH is negative then indent negatively (outdent).

\(fn START END NTH)" t nil)

(autoload 'delete-extra-windows-for-buffer "misc-cmds" "\
Delete all other windows showing the selected window's buffer.

\(fn)" t nil)

(autoload 'delete-window-maybe-kill-buffer "misc-cmds" "\
Delete selected window.
    If no other window shows its buffer, kill the buffer too.

\(fn)" t nil)

(autoload 'recenter-top-bottom "misc-cmds" "\
Move current line to window center, top, and bottom, successively.
With a prefix argument, this is the same as `recenter':
 With numeric prefix ARG, move current line to window-line ARG.
 With plain `C-u', move current line to window center.

Otherwise move current line to window center on first call, and to
top, middle, or bottom on successive calls.

The starting position of the window determines the cycling order:
 If initially in the top or middle third: top -> middle -> bottom.
 If initially in the bottom third: bottom -> middle -> top.

Top and bottom destinations are actually `scroll-conservatively' lines
from true window top and bottom.

\(fn &optional ARG)" t nil)

(autoload 'recenter-top-bottom-1 "misc-cmds" "\
Move current line to window center, top, and bottom, successively.
With prefix ARG, move current line to window-line ARG.
Top and bottom destinations are actually `scroll-conservatively' lines
from true top and bottom.

\(fn &optional ARG)" t nil)

(autoload 'recenter-top-bottom-2 "misc-cmds" "\
Move current line to line ARG, window center, top, or bottom.
With a prefix argument, this is the same as `recenter':
 With numeric prefix ARG, move current line to window-line ARG.
 With plain `C-u', move current line to window center.

Otherwise, the window starting position determines the next position:
 If in the top third, move to bottom.
 If in middle third,  move to top.
 If in bottom third,  move tocenter.

Top and bottom destinations are actually `scroll-conservatively' lines
from true top and bottom.

\(fn &optional ARG)" t nil)

(autoload 'mark-buffer-after-point "misc-cmds" "\
Select the part of the buffer after point.
With a prefix argument, select the part before point.

\(fn REVERSEP)" t nil)

(autoload 'mark-buffer-before-point "misc-cmds" "\
Select the part of the buffer before point.
With a prefix argument, select the part after point.

\(fn REVERSEP)" t nil)

(defalias 'selection-length 'region-length)

(defalias 'count-chars-in-region 'region-length)

(autoload 'region-length "misc-cmds" "\
Display the number of characters in the region in a message.

\(fn)" t nil)

(autoload 'goto-longest-line "misc-cmds" "\
Go to the first of the longest lines in the region or buffer.
If the region is active, it is checked.
If not, the buffer (or its restriction) is checked.

Returns a list of three elements:

 (LINE LINE-LENGTH OTHER-LINES LINES-CHECKED)

LINE is the first of the longest lines measured.
LINE-LENGTH is the length of LINE.
OTHER-LINES is a list of other lines checked that are as long as LINE.
LINES-CHECKED is the number of lines measured.

Interactively, a message displays this information.

If there is only one line in the active region, then the region is
deactivated after this command, and the message mentions only LINE and
LINE-LENGTH.

If this command is repeated, it checks for the longest line after the
cursor.  That is *not* necessarily the longest line other than the
current line.  That longest line could be before or after the current
line.

To search only from the current line forward, not throughout the
buffer, you can use `C-SPC' to set the mark, then use this
\(repeatedly).

\(fn BEG END)" t nil)

(autoload 'goto-long-line "misc-cmds" "\
Go to the first line that is at least LEN characters long.
Use a prefix arg to provide LEN.
Plain `C-u' (no number) uses `fill-column' as LEN.

\(fn LEN)" t nil)

(autoload 'delete-lines "misc-cmds" "\
Delete NUM-LINES lines, starting at point.
Lines are deleted, not killed.
With positive prefix arg, deletion is forward.
With negative prefix arg, deletion is backward.

\(fn NUM-LINES)" t nil)

(autoload 'comment-region-lines "misc-cmds" "\
Like `comment-region' (which see), but comment/uncomment whole lines.

\(fn BEG END &optional ARG)" t nil)

(autoload 'region-to-buffer "misc-cmds" "\
Copy region to BUFFER: At beginning (prefix >= 0), end (< 0), or replace.
START and END are the region boundaries.
BUFFER is a buffer or its name (a string).
With prefix ARG >= 0: `append-to-buffer':
  Append contents of region to end of BUFFER.
  (Point is moved to end of BUFFER first.)
With prefix ARG < 0:  `prepend-to-buffer':
  Prepend contents of region to beginning of BUFFER.
  (Point is moved to beginning of BUFFER first.)
With no prefix ARG (nil): `copy-to-buffer'.
  Write region to BUFFER, replacing any previous contents.

\(fn START END BUFFER ARG)" t nil)

(autoload 'region-to-file "misc-cmds" "\
With prefix arg, this is `append-to-file'.  Without, it is `write-region'.
START and END are the region boundaries.
Prefix ARG non-nil means append region to end of file FILENAME.
Prefix ARG nil means write region to FILENAME, replacing contents.

\(fn START END FILENAME ARG)" t nil)

(autoload 'chmod "misc-cmds" "\
Execute Unix command `chmod'.  Current buffer's file is default arg.
CMD is the command to execute (interactively, `chmod').

\(fn CMD)" t nil)

(autoload 'chgrp "misc-cmds" "\
Execute Unix command `chgrp'.  Current buffer's file is default arg.
CMD is the command to execute (interactively, `chgrp').

\(fn CMD)" t nil)

(autoload 'chown "misc-cmds" "\
Execute Unix command `chown'.  Current buffer's file is default arg.
CMD is the command to execute (interactively, `chown').

\(fn CMD)" t nil)

(autoload 'kill-buffer-and-its-windows "misc-cmds" "\
Kill BUFFER and delete its windows.  Default is `current-buffer'.
BUFFER may be either a buffer or its name (a string).

\(fn BUFFER)" t nil)

(autoload 'indirect-buffer "misc-cmds" "\
Edit stuff in this buffer in an indirect-buffer window.
The indirect buffer can have a different major mode from current.

\(fn)" t nil)

(defalias 'clear-search-ring 'clear-search-history)

(autoload 'clear-search-history "misc-cmds" "\
Clear the search history (empty it).
With prefix arg, clear also the regular-expression search history.

\(fn &optional REGEXP-TOO-P)" t nil)

(defalias 'clear-regexp-search-ring 'clear-regexp-search-history)

(autoload 'clear-regexp-search-history "misc-cmds" "\
Clear the regular-expression search history (empty it).
With prefix arg, clear also the simple search history.

\(fn &optional SIMPLE-TOO-P)" t nil)

(autoload 'clear-search-histories "misc-cmds" "\
Clear both search histories: simple search and regexp search.

\(fn)" t nil)

(autoload 'switch-to-alternate-buffer "misc-cmds" "\
Like `switch-to-buffer', but also kill the current buffer.

\(fn BUFFER &optional NORECORD FORCE-SAME-WINDOW)" t nil)

(autoload 'switch-to-alternate-buffer-other-window "misc-cmds" "\
Like `switch-to-buffer-other-window', but also kill the current buffer.

\(fn BUFFER &optional NORECORD)" t nil)

(autoload 'revert-buffer-no-confirm "misc-cmds" "\
Revert buffer without confirmation.

\(fn)" t nil)

(autoload 'view-X11-colors "misc-cmds" "\
View file `/usr/lib/X11/rgb.txt', which lists available X11 colors.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("misc-cmds-pkg.el") (21526 154 942504
;;;;;;  397000))

;;;***

(provide 'misc-cmds-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; misc-cmds-autoloads.el ends here

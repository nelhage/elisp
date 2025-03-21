;;; elgrep-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "../../../.emacs.d/elpa/elgrep-20230814.1215/elgrep"
;;;;;;  "../../../.emacs.d/elpa/elgrep-20230814.1215/elgrep.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/elgrep-20230814.1215/elgrep.el

(autoload 'elgrep-mode "../../../.emacs.d/elpa/elgrep-20230814.1215/elgrep" "\
Major mode for elgrep buffers.
See `elgrep' and `elgrep-menu' for details.

\(fn)" t nil)

(autoload 'elgrep-menu-arg-list "../../../.emacs.d/elpa/elgrep-20230814.1215/elgrep" "\
Collect `elgrep' arguments from `elgrep-menu' buffer." t nil)

(autoload 'elgrep-menu "../../../.emacs.d/elpa/elgrep-20230814.1215/elgrep" "\
Present a menu with most of the parameters for `elgrep'.
Reset the menu entries if RESET is non-nil.
You can adjust the parameters there and start `elgrep'.

\(fn &optional RESET)" t nil)

(autoload 'elgrep "../../../.emacs.d/elpa/elgrep-20230814.1215/elgrep" "\
In path DIR grep files with name matching FILE-NAME-RE for text matching RE.
This is done via Emacs Lisp (no dependence on external grep).
Return list of filematches.

FILE-NAME-RE can be a regexp or a filter function taking a list of file names
and returning a filtered list of file names.

Each filematch is a cons (file . matchdata).
file is the file name.
matchdata is a list of matches.
Each match is a list of sub-matches.
Each submatch is a plist of :match, :context, :line,
:linestart, :beg and :end.

OPTIONS is a plist
Flags:

:abs absolute file names
t: full absolute file names;
nil: (default) file names relative to `default-directory'
of the last visited buffer

:interactive
t: call as interactive

:r-beg record begin
Beginning of next record.
Can be a regular expression, a function without args
or a list of record delimiters.
If the function finds a record beginning, it should return its position
like `search-forward'.
Search starts at buffer beginning or at end of last record.
Defaults to `point-min'.
A list of record delimiters allows to define nested records.
One example where this becomes handy is, when one wants to grep
for identifiers in org source blocks within certain sections of Org-files.
In that example the first record could start at a match of \"^\\* SECTION\"
and end at a match of \"^\\* \\|\\'\"
and the second record could be delimited by matches of
\" *#+begin_src\" and \" *#+end_src\".
A list of record delimiters is marked with the value t in its first element.
Starting with its cdr, it contains record delimiters.
Each record delimiter is a list.
The first element of that list is the regular expression or
the function matching the beginning of the record
and the second element of that list is the regular expression
or function matching the end of the record.
For the above example the `elgrep' command would look like:
\(elgrep ...
    :r-beg (t
            (\"^\\\\* W:22205\" \"^\\\\* \\\\|\\\\'\")
            (\" *#\\\\+begin_src\" \" *#\\\\+end_src \"))
    ...)

:r-end record end
End of record.
Can be a regular expression or a function without args.
If the function finds a record end it should return its position
like `search-forward'.
Search starts at search result for :r-beg.
Defaults to `point-max'.

:c-beg context begin (line beginning)
Lines before match defaults to 0. Can also be a regular expression.
Then this re is searched for in backward-direction
starting at the beginning of the current elgrep-match.
It can also be a function moving point to the context beginning
starting at the match of RE.

:c-end context end (line end)
Lines behind match defaults to 0. Can also be a regular expression.
Then this re is searched for in forward-direction
starting at the end of the current elgrep-match.
It can also be a function moving point to the context end
starting at the match of :c-beg.

:c-beg-only
Use the context beginning literally.
That means do not extend the context to the beginning of line.

:c-end-only
Use the context end literally.
That means do not extend the context to the end of line.

:c-op
Context operation gets beginning and end position of context as arguments.
Defaults to `buffer-substring-no-properties'.

:recursive
t: also grep recursively subdirectories in dir
\(also if called interactively with prefix arg)
Defaults to nil.

:symlink
t: also follow symbolic links when recursing

:formatter
Formatting function to call for each match
if called interactively with non-nil RE.
Inputs: format string \"%s:%d:%s
\", file-name, line number,

:exclude-file-re
Regular expression matching the files that should not be grepped.
Do not exclude files if this option is nil, unset, or the empty string.
Can also be a filter function that gets the full list of file names
as argument and should return the filtered list to be used for `elgrep'.
Defaults to nil.

:dir-re
Regular expression matching the directories
that should be entered in recursive grep.
Defaults to \"\".

:exclude-dir-re
Regular expression matching the directories
that should not be entered in recursive grep.
If this is the empty string no directories are excluded.
Defaults to \"^\\.\".

:case-fold-search
Ignore case if non-nil.
Defaults to the value of `case-fold-search'.

:buffer-init may be one of the following symbols:
nil (default): Do not initialize buffer.
syntax-table: Just set the syntax table corresponding
              to the auto-mode of the file.
major-mode: Full major-mode initialization of the auto-mode corresponding
            to the file.

:file-fun
Predicate function called with the file path as argument.
The function should return non-nil if that file should be searched.
If the return value is a string it is used as new file name for `elgrep-save'.
Option :abs decides whether the path is relative or absolute.

:search-fun
Function to search forward for occurences of RE
with the same arguments as `re-search-forward'.
It gets RE as first argument.
Thereby it is not required that RE is a regular expression.
Defaults to `re-search-forward'.

:keep-elgrep-buffer
Keep buffer <*elgrep*> even when there are no matches.

:no-header
Avoid descriptive header into <*elgrep*> buffer.

:async
Asynchronous search (experimental).
Search synchronous if this option is nil,
search in a separate thread if this option is equal to 'thread,
and search with the help of the library async otherwise.

:mindepth Minimal depth. Defaults to 0.

:maxdepth Maximal depth. Defaults to the value of `most-positive-fixnum'.

:depth Internal. Should not be used.

\(fn DIR FILE-NAME-RE RE &rest OPTIONS)" t nil)

(autoload 'elgrep-occur "../../../.emacs.d/elpa/elgrep-20230814.1215/elgrep" "\
Run elgrep `occur'-like for REGEXP on the current buffer.
OPTIONS are the same as for the command `elgrep'.

\(fn REGEXP &rest OPTIONS)" t nil)

(autoload 'elgrep-search "../../../.emacs.d/elpa/elgrep-20230814.1215/elgrep" "\
In path DIR grep files with name matching FILE-NAME-RE for text matching RE.
This is done via Emacs Lisp (no dependence on external grep).
Return list of filematches.

RE may be a list of regular expressions.
In that case each file is searched for all occurences
of the first regular expression if each of the other
regular expressions occur at least once in the file.

Each filematch is a cons (file . matchdata).
file is the file name.
matchdata is a list of matches.
Each match is a list of sub-matches.
Each submatch is a plist of :match, :context, :line,
:linestart, :beg, :end, :context-beg and :context-end.

FILE-NAME-RE can also be a list of FILE-NAME-MATCHERs.

Each FILE-NAME-MATCHER can be a regular expression
or a list of a regular expression and substitution strings
\(RE FILTER1 FILTER2 ...)
The FILTERs are treated as regular expressions but the references
like \\1 are inserted in a quoted form (via `regexp-quote').

If SUBST1 starts with ?\\! then file names matching SUBST1
without the leading ?\\! will be filtered out.

Example: (\"\\\\(.*\\\\)\\.k\\\\'\" \"!\\\\1\\.\\\\(h\\\\|cpp\\\\)\\\\'\")

See `elgrep' for the valid options in plist OPTIONS.

\(fn DIR FILE-NAME-RE RE &rest OPTIONS)" nil nil)

(require 'easymenu)

(easy-menu-add-item global-map '("menu-bar" "tools") ["Search Files (Elgrep)..." elgrep-menu t] "grep")

(autoload 'elgrep-load-elgrep-data-file "../../../.emacs.d/elpa/elgrep-20230814.1215/elgrep" "\
Load the `elgrep-data-file'." t nil)

(autoload 'elgrep-save-elgrep-data-file "../../../.emacs.d/elpa/elgrep-20230814.1215/elgrep" "\
Save the elgrep data file if `elgrep-data-file' is a string.
This can be used as `kill-emacs-hook'.
Unconditionally return the value of `elgrep-data-file'." t nil)

(register-definition-prefixes "../../../.emacs.d/elpa/elgrep-20230814.1215/elgrep" '("elgrep"))

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/elgrep-20230814.1215/elgrep-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/elgrep-20230814.1215/elgrep-pkg.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; elgrep-autoloads.el ends here

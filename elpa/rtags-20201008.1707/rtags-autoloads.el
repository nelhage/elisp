;;; rtags-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "../../../.emacs.d/elpa/rtags-20201008.1707/rtags"
;;;;;;  "../../../.emacs.d/elpa/rtags-20201008.1707/rtags.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/rtags-20201008.1707/rtags.el

(autoload 'rtags-set-periodic-reparse-timeout "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\
Set `rtags-periodic-reparse-timeout' to TIME.

\(fn TIME)" t nil)

(autoload 'rtags-call-bury-or-delete "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\
Call `rtags-bury-buffer-function' function." t nil)

(autoload 'rtags-next-match "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-previous-match "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-next-diag "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-previous-diag "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-preprocess-file "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\
Preprocess selected region or buffer.
If optional BUFFER is given, use BUFFER instead of `current-buffer'.
It uses the stored compile command from the RTags database for preprocessing.

\(fn &optional BUFFER)" t nil)

(autoload 'rtags-asm-file "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\
Assemble buffer.
If optional BUFFER is given, use BUFFER instead of `current-buffer'.
It uses the stored compile command from the RTags database for assembling.

\(fn &optional BUFFER)" t nil)

(autoload 'rtags-set-current-project "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\
Set active project.
Uses `completing-read' to ask for the project." t nil)

(autoload 'rtags-print-symbol-info "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\
Print information about the symbol under cursor.

\(fn &optional VERBOSE)" t nil)

(autoload 'rtags-symbol-type "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\
Print symbol type under cursor." t nil)

(autoload 'rtags-print-dependencies "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\
Print dependency information of the file in buffer.

If optional PREFIX is given, a selection of what type of dependency
information should be shown will be offered. Currently only one can
be chosen.
\"includes\"        - Print includes the file in buffer includes.
\"included-by\"     - Print files which include the file in buffer.
\"depends-on\"      - Print files the file in buffer depends on.
\"depended-on\"     - ...
\"tree-depends-on\" - ...

If optional BUFFER is given print dependencies for file in BUFFER
instead of file from `current-buffer'.

\(fn &optional PREFIX BUFFER)" t nil)

(defvar rtags-dependency-tree-data nil)

(autoload 'rtags-references-tree "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-find-functions-called-by-this-function "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-find-all-functions-called-this-function "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil nil nil)

(autoload 'rtags-list-results "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\
Show the RTags results buffer." t nil)

(autoload 'rtags-print-source-arguments "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\


\(fn &optional BUFFER)" t nil)

(autoload 'rtags-print-class-hierarchy "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-print-enum-value-at-point "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\


\(fn &optional LOCATION)" t nil)

(autoload 'rtags-goto-offset "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\


\(fn POS)" t nil)

(autoload 'rtags-location-stack-filter "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\


\(fn PATH/LAMBDA/RX)" t nil)

(autoload 'rtags-location-stack-jump "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\


\(fn BY)" t nil)

(autoload 'rtags-location-stack-visualize-update "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil nil nil)

(autoload 'rtags-enable-standard-keybindings "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\
Setup standard keybindings for the RTags commands.

If optional MAP is non-nil, add the keys to MAP instead of `c-mode-base-map'.
If optional PREFIX is non-nil, use PREFIX as prefix key for the commands,
default is \"C-c r \". It doesn't matter whether you add a space at the end
of PREFIX or not, if doesn't contain one, one will be added.

\(fn &optional MAP PREFIX)" t nil)

(autoload 'rtags-print-current-location "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-location-stack-forward "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-location-stack-back "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-location-stack-reset "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-find-symbol-at-point "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\
Find the natural target for the symbol under the cursor and moves to that location.
For references this means to jump to the definition/declaration of the referenced symbol (it jumps to the definition if it is indexed).
For definitions it jumps to the declaration (if there is only one) For declarations it jumps to the definition.
If called with prefix, open first match in other window

\(fn &optional PREFIX)" t nil)

(autoload 'rtags-find-references-at-point "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\
Find all references to the symbol under the cursor.

If there's exactly one result jump directly to it, and if optional
PREFIX is given jump to it in other window. If there's more show a
buffer with the different alternatives and jump to the first one, if
`rtags-jump-to-first-match' is true. References to references will be
treated as references to the referenced symbol.

\(fn &optional PREFIX)" t nil)

(autoload 'rtags-find-virtuals-at-point "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\
List all reimplementations of function under cursor.
This includes both declarations and definitions.

\(fn &optional PREFIX)" t nil)

(autoload 'rtags-find-all-references-at-point "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\


\(fn &optional PREFIX)" t nil)

(autoload 'rtags-guess-function-at-point "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-rename-symbol "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\
Rename symbol (identifier) at point.

Normally this function will ask the user (via minibuffer) for the
replacement and then ask for confirmation.  However, when the scope
of the symbol at point is just one file (the file that's being
visited by current buffer), the variable `rtags-use-multiple-cursors'
is non-nil and the `multiple-cursors' package is available, then this
function will create fake cursors at all occurrences of the symbol.

The optional argument NO-CONFIRM means agree to all replacements and
can be specified with a prefix argument.

\(fn &optional NO-CONFIRM)" t nil)

(autoload 'rtags-find-symbol "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\


\(fn &optional PREFIX)" t nil)

(autoload 'rtags-find-references "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\


\(fn &optional PREFIX)" t nil)

(autoload 'rtags-find-symbol-current-file "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-find-references-current-file "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-find-symbol-current-dir "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-find-references-current-dir "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-clear-diagnostics-overlays "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\


\(fn &optional BUF)" t nil)

(autoload 'rtags-is-running "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-cycle-through-diagnostics "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-fix-fixit-at-point "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\
Apply compiler fix-it at point." t nil)

(autoload 'rtags-restart-tracking-timer "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-post-command-hook "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-toggle-diagnostics-suspended "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\


\(fn &optional QUIET)" t nil)

(autoload 'rtags-set-diagnostics-suspended "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\


\(fn &optional QUIET)" t nil)

(autoload 'rtags-stop-diagnostics "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-clear-diagnostics "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-diagnostics "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\


\(fn &optional RESTART)" t nil)

(autoload 'rtags-compilation-flags "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-close-taglist "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-taglist "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\


\(fn &optional DEST-WINDOW)" t nil)

(autoload 'rtags-select "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\


\(fn &optional OTHER-WINDOW REMOVE SHOW)" t nil)

(autoload 'rtags-select-other-window "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\


\(fn &optional NOT-OTHER-WINDOW)" t nil)

(autoload 'rtags-select-caller "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\


\(fn &optional NOT-OTHER-WINDOW)" t nil)

(autoload 'rtags-select-caller-other-window "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-show-in-other-window "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-select-and-remove-rtags-buffer "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-imenu "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-flatten-max-depth-one "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\


\(fn UNFLATTENED)" nil nil)

(autoload 'rtags-create-index-function "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-activate-imenu "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\
Overrides imenu index generation function for the current function." t nil)

(autoload 'rtags-copy-and-print-current-location "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-find-file "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\
Find files matching a file pattern in the RTags database.

With PREFIX and when `rtags-use-filename-completion' is nil,
negate `rtags-find-file-prefer-exact-match' when matching files.

Initial file pattern to match is obtained from `rtags-current-symbol'
which can be overridden by specifying DEFAULT-FILE

\(fn &optional PREFIX DEFAULT-FILE)" t nil)

(autoload 'rtags-show-rtags-buffer "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-fixit "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\


\(fn &optional EDIFF BUFFER)" t nil)

(autoload 'rtags-remove-other-window "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-update-current-project "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-show-target-in-other-window "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\
DEST-WINDOW : destination window. Can be nil; in this case the current window is split
according to `rtags-other-window-window-size-percentage'.
CENTER-WINDOW : if true the target window is centered.
TRY-DECLARATION-FIRST : first try to find the declaration of the item, then the
definition.

\(fn &optional DEST-WINDOW CENTER-WINDOW TRY-DECLARATION-FIRST)" t nil)

(autoload 'rtags-suspend-file "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\


\(fn &optional ARG)" t nil)

(autoload 'rtags-unsuspend-file "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-toggle-file-suspended "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-clear-suspended-files "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\


\(fn &optional A B)" t nil)

(autoload 'rtags-suspend-all-files "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\


\(fn &optional A)" t nil)

(autoload 'rtags-list-suspended-files "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-compile-file "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\


\(fn &optional BUFFER)" t nil)

(autoload 'rtags-recompile-file "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-quit-rdm "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\
Quit the RTags process (rdm)." t nil)

(autoload 'rtags-restart-process "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\
Restart the RTags process (rdm)." t nil)

(autoload 'rtags-start-process-unless-running "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\
Launch the RTags process (rdm) if it's not already started." t nil)

(autoload 'rtags-reparse-file "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\
WAIT-REPARSING : t to wait for reparsing to finish, nil for async (no waiting).

\(fn &optional BUFFER PERIODIC)" t nil)

(autoload 'rtags-maybe-reparse-file "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\


\(fn &optional BUFFER)" t nil)

(autoload 'rtags-display-summary "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\
Display a short text describing the item at point.
See `rtags-get-summary-text' for details.
If `rtags-display-summary-as-tooltip' is t, a tooltip is displayed.

\(fn &optional HIDE-EMPTY POS)" t nil)

(autoload 'rtags-display-summary-as-message "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\
Display a short text in message area describing the item at point.
See `rtags-get-summary-text' for details." t nil)

(autoload 'rtags-get-include-file-for-symbol "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\
Insert #include declaration to buffer corresponding to the input symbol.
With optional PREFIX insert include at point.

\(fn &optional PREFIX)" t nil)

(autoload 'rtags-make-member "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\
Create a stub member functions. Type a declaration and then
`rtags-make-member' can be used to create the stub definition in
the class.
" t nil)

(autoload 'rtags-check-includes "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" nil t nil)

(autoload 'rtags-tokens "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\


\(fn &optional FROM TO CALLBACK)" t nil)

(autoload 'rtags-create-doxygen-comment "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" "\
Creates doxygen comment for function at point.

Comment will be inserted before current line. It uses yasnippet to let
the user enter missing field manually." t nil)

(register-definition-prefixes "../../../.emacs.d/elpa/rtags-20201008.1707/rtags" '("rtags-"))

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/rtags-20201008.1707/rtags-autoloads.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rtags-autoloads.el ends here

;;; counsel-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "../../../.emacs.d/elpa/counsel-20200922.1006/counsel"
;;;;;;  "../../../.emacs.d/elpa/counsel-20200922.1006/counsel.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/counsel-20200922.1006/counsel.el

(autoload 'counsel-company "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Complete using `company-candidates'." t nil)

(autoload 'counsel-irony "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Inline C/C++ completion using Irony." t nil)

(autoload 'counsel-describe-variable "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Forward to `describe-variable'.

Variables declared using `defcustom' are highlighted according to
`ivy-highlight-face'." t nil)

(autoload 'counsel-describe-function "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Forward to `describe-function'.

Interactive functions (i.e., commands) are highlighted according
to `ivy-highlight-face'." t nil)

(autoload 'counsel-describe-symbol "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Forward to `describe-symbol'." t nil)

(autoload 'counsel-set-variable "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Set a variable SYM, with completion.

When the selected variable is a `defcustom' with the type boolean
or radio, offer completion of all possible values.

Otherwise, offer a variant of `eval-expression', with the initial
input corresponding to the chosen variable.

With a prefix arg, restrict list to variables defined using
`defcustom'.

\(fn SYM)" t nil)

(autoload 'counsel-apropos "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Show all matching symbols.
See `apropos' for further information on what is considered
a symbol and how to search for them." t nil)

(autoload 'counsel-info-lookup-symbol "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Forward SYMBOL to `info-lookup-symbol' with ivy completion.
With prefix arg MODE a query for the symbol help mode is offered.

\(fn SYMBOL &optional MODE)" t nil)

(autoload 'counsel-M-x "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Ivy version of `execute-extended-command'.
Optional INITIAL-INPUT is the initial input in the minibuffer.
This function integrates with either the `amx' or `smex' package
when available, in that order of precedence.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-command-history "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Show the history of commands." t nil)

(autoload 'counsel-load-library "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Load a selected the Emacs Lisp library.
The libraries are offered from `load-path'." t nil)

(autoload 'counsel-find-library "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Visit a selected the Emacs Lisp library.
The libraries are offered from `load-path'." t nil)

(autoload 'counsel-load-theme "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Forward to `load-theme'.
Usable with `ivy-resume', `ivy-next-line-and-call' and
`ivy-previous-line-and-call'." t nil)

(autoload 'counsel-descbinds "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Show a list of all defined keys and their definitions.
If non-nil, show only bindings that start with PREFIX.
BUFFER defaults to the current one.

\(fn &optional PREFIX BUFFER)" t nil)

(autoload 'counsel-describe-face "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Completion for `describe-face'." t nil)

(autoload 'counsel-faces "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Complete faces with preview.
Actions are provided by default for describing or customizing the
selected face." t nil)

(autoload 'counsel-git "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Find file in the current Git repository.
INITIAL-INPUT can be given as the initial minibuffer input.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-git-grep "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Grep for a string in the current Git repository.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
When CMD is a string, use it as a \"git grep\" command.
When CMD is non-nil, prompt for a specific \"git grep\" command.

\(fn &optional INITIAL-INPUT INITIAL-DIRECTORY CMD)" t nil)

(autoload 'counsel-git-stash "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Search through all available git stashes." t nil)

(autoload 'counsel-git-change-worktree "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Find the file corresponding to the current buffer on a different worktree." t nil)

(autoload 'counsel-git-checkout "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Call the \"git checkout\" command." t nil)

(autoload 'counsel-git-log "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Call the \"git log --grep\" shell command." t nil)

(autoload 'counsel-find-file "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Forward to `find-file'.
When INITIAL-INPUT is non-nil, use it in the minibuffer during completion.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-dired "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Forward to `dired'.
When INITIAL-INPUT is non-nil, use it in the minibuffer during completion.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-recentf "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Find a file on `recentf-list'." t nil)

(autoload 'counsel-buffer-or-recentf "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Find a buffer visiting a file or file on `recentf-list'." t nil)

(autoload 'counsel-bookmark "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Forward to `bookmark-jump' or `bookmark-set' if bookmark doesn't exist." t nil)

(autoload 'counsel-bookmarked-directory "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Ivy interface for bookmarked directories.

With a prefix argument, this command creates a new bookmark which points to the
current value of `default-directory'." t nil)

(autoload 'counsel-file-register "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Search file in register.

You cannot use Emacs' normal register commands to create file
registers.  Instead you must use the `set-register' function like
so: `(set-register ?i \"/home/eric/.emacs.d/init.el\")'.  Now you
can use `C-x r j i' to open that file." t nil)

(autoload 'counsel-locate-action-extern "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Pass X to `xdg-open' or equivalent command via the shell.

\(fn X)" t nil)

(autoload 'counsel-locate "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Call the \"locate\" shell command.
INITIAL-INPUT can be given as the initial minibuffer input.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-tracker "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" nil t nil)

(autoload 'counsel-fzf "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Open a file using the fzf shell command.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
FZF-PROMPT, if non-nil, is passed as `ivy-read' prompt argument.

\(fn &optional INITIAL-INPUT INITIAL-DIRECTORY FZF-PROMPT)" t nil)

(autoload 'counsel-dpkg "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Call the \"dpkg\" shell command." t nil)

(autoload 'counsel-rpm "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Call the \"rpm\" shell command." t nil)

(autoload 'counsel-file-jump "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Jump to a file below the current directory.
List all files within the current directory or any of its sub-directories.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.

\(fn &optional INITIAL-INPUT INITIAL-DIRECTORY)" t nil)

(autoload 'counsel-dired-jump "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Jump to a directory (see `dired-jump') below the current directory.
List all sub-directories within the current directory.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.

\(fn &optional INITIAL-INPUT INITIAL-DIRECTORY)" t nil)

(autoload 'counsel-ag "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Grep for a string in a root directory using ag.

By default, the root directory is the first directory containing a .git subdirectory.

INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
EXTRA-AG-ARGS, if non-nil, is appended to `counsel-ag-base-command'.
AG-PROMPT, if non-nil, is passed as `ivy-read' prompt argument.
CALLER is passed to `ivy-read'.

With a `\\[universal-argument]' prefix argument, prompt for INITIAL-DIRECTORY.
With a `\\[universal-argument] \\[universal-argument]' prefix argument, prompt additionally for EXTRA-AG-ARGS.

\(fn &optional INITIAL-INPUT INITIAL-DIRECTORY EXTRA-AG-ARGS AG-PROMPT &key CALLER)" t nil)

(autoload 'counsel-pt "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Grep for a string in the current directory using pt.
INITIAL-INPUT can be given as the initial minibuffer input.
This uses `counsel-ag' with `counsel-pt-base-command' instead of
`counsel-ag-base-command'.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-ack "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Grep for a string in the current directory using ack.
INITIAL-INPUT can be given as the initial minibuffer input.
This uses `counsel-ag' with `counsel-ack-base-command' replacing
`counsel-ag-base-command'.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-rg "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Grep for a string in the current directory using rg.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
EXTRA-RG-ARGS string, if non-nil, is appended to `counsel-rg-base-command'.
RG-PROMPT, if non-nil, is passed as `ivy-read' prompt argument.

Example input with inclusion and exclusion file patterns:
    require i -- -g*.el

\(fn &optional INITIAL-INPUT INITIAL-DIRECTORY EXTRA-RG-ARGS RG-PROMPT)" t nil)

(autoload 'counsel-grep "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Grep for a string in the file visited by the current buffer.
When non-nil, INITIAL-INPUT is the initial search pattern.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-grep-backward "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Grep for a string in the file visited by the current buffer going
backward similar to `swiper-backward'. When non-nil, INITIAL-INPUT is
the initial search pattern.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-grep-or-swiper "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Call `swiper' for small buffers and `counsel-grep' for large ones.
When non-nil, INITIAL-INPUT is the initial search pattern.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-grep-or-swiper-backward "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Call `swiper-backward' for small buffers and `counsel-grep-backward' for
large ones.  When non-nil, INITIAL-INPUT is the initial search pattern.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel-recoll "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Search for a string in the recoll database.
You'll be given a list of files that match.
Selecting a file will launch `swiper' for that file.
INITIAL-INPUT can be given as the initial minibuffer input.

\(fn &optional INITIAL-INPUT)" t nil)

(autoload 'counsel--org-get-tags "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" nil nil nil)

(autoload 'counsel-org-tag "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Add or remove tags in `org-mode'." t nil)

(autoload 'counsel-org-tag-agenda "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Set tags for the current agenda item." t nil)

(defalias 'counsel-org-goto #'counsel-outline)

(autoload 'counsel-org-goto-all "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Go to a different location in any org file." t nil)

(autoload 'counsel-org-file "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Browse all attachments for current Org file." t nil)

(autoload 'counsel-org-entity "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Complete Org entities using Ivy." t nil)

(autoload 'counsel-org-capture "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Capture something." t nil)

(autoload 'counsel-org-agenda-headlines "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Choose from headers of `org-mode' files in the agenda." t nil)

(autoload 'counsel-org-link "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Insert a link to an headline with completion." t nil)

(autoload 'counsel-mark-ring "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Browse `mark-ring' interactively.
Obeys `widen-automatically', which see." t nil)

(autoload 'counsel-evil-marks "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Ivy replacement for `evil-show-marks'.
By default, this function respects `counsel-evil-marks-exclude-registers'.
When ARG is non-nil, display all active evil registers.

\(fn &optional ARG)" t nil)

(autoload 'counsel-package "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Install or delete packages.

Packages not currently installed are prefixed with \"+\", and
selecting one of these will try to install it.
Packages currently installed are prefixed with \"-\", and
selecting one of these will try to delete it.

Additional actions:\\<ivy-minibuffer-map>

  \\[ivy-dispatching-done] d: Describe package
  \\[ivy-dispatching-done] h: Visit package's homepage" t nil)

(autoload 'counsel-tmm "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Text-mode emulation of looking and choosing from a menu bar." t nil)

(autoload 'counsel-yank-pop "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Ivy replacement for `yank-pop'.
With a plain prefix argument (\\[universal-argument]),
temporarily toggle the value of `counsel-yank-pop-after-point'.
Any other value of ARG has the same meaning as in `yank-pop', but
`counsel-yank-pop-preselect-last' determines its default value.
See also `counsel-yank-pop-filter' for how to filter candidates.

Note: Duplicate elements of `kill-ring' are always deleted.

\(fn &optional ARG)" t nil)

(autoload 'counsel-register "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Interactively choose a register." t nil)

(autoload 'counsel-evil-registers "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Ivy replacement for `evil-show-registers'." t nil)

(autoload 'counsel-imenu "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Jump to a buffer position indexed by imenu." t nil)

(autoload 'counsel-list-processes "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Offer completion for `process-list'.
The default action deletes the selected process.
An extra action allows to switch to the process buffer." t nil)

(autoload 'counsel-minibuffer-history "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Browse minibuffer history." t nil)

(autoload 'counsel-esh-history "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Browse Eshell history." t nil)

(autoload 'counsel-shell-history "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Browse shell history." t nil)

(autoload 'counsel-slime-repl-history "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Browse Slime REPL history." t nil)

(autoload 'counsel-hydra-heads "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Call a head of the current/last hydra." t nil)

(autoload 'counsel-semantic "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Jump to a semantic tag in the current buffer." t nil)

(autoload 'counsel-semantic-or-imenu "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" nil t nil)

(autoload 'counsel-outline "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Jump to an outline heading with completion." t nil)

(autoload 'counsel-ibuffer "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Use ibuffer to switch to another buffer.
NAME specifies the name of the buffer (defaults to \"*Ibuffer*\").

\(fn &optional NAME)" t nil)

(autoload 'counsel-switch-to-shell-buffer "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Switch to a shell buffer, or create one." t nil)

(autoload 'counsel-unicode-char "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Insert COUNT copies of a Unicode character at point.
COUNT defaults to 1.

\(fn &optional COUNT)" t nil)

(autoload 'counsel-colors-emacs "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Show a list of all supported colors for a particular frame.

You can insert or kill the name or hexadecimal RGB value of the
selected color." t nil)

(autoload 'counsel-colors-web "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Show a list of all W3C web colors for use in CSS.

You can insert or kill the name or hexadecimal RGB value of the
selected color." t nil)

(autoload 'counsel-fonts "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Show a list of all supported font families for a particular frame.

You can insert or kill the name of the selected font." t nil)

(autoload 'counsel-kmacro "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Interactively choose and run a keyboard macro.

With prefix argument, run macro that many times.

Macros are run using the current value of `kmacro-counter-value'
and their respective counter format. Displayed next to each macro is
the counter's format and initial value.

One can use actions to copy the counter format or initial counter
value of a macro, using them for a new macro." t nil)

(autoload 'counsel-geiser-doc-look-up-manual "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Search Scheme documentation." t nil)

(autoload 'counsel-rhythmbox "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Choose a song from the Rhythmbox library to play or enqueue.

\(fn &optional ARG)" t nil)

(autoload 'counsel-linux-app "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Launch a Linux desktop application, similar to Alt-<F2>.
When ARG is non-nil, ignore NoDisplay property in *.desktop files.

\(fn &optional ARG)" t nil)

(autoload 'counsel-wmctrl "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Select a desktop window using wmctrl." t nil)

(autoload 'counsel-switch-buffer "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Switch to another buffer.
Display a preview of the selected ivy completion candidate buffer
in the current window." t nil)

(autoload 'counsel-switch-buffer-other-window "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Switch to another buffer in another window.
Display a preview of the selected ivy completion candidate buffer
in the current window." t nil)

(autoload 'counsel-compile "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Call `compile' completing with smart suggestions, optionally for DIR.

Additional actions:

\\{counsel-compile-map}

\(fn &optional DIR)" t nil)

(autoload 'counsel-compile-env "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Update `counsel-compile-env' interactively." t nil)

(autoload 'counsel-minor "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Enable or disable minor mode.

Disabled minor modes are prefixed with \"+\", and
selecting one of these will enable it.
Enabled minor modes are prefixed with \"-\", and
selecting one of these will enable it.

Additional actions:\\<ivy-minibuffer-map>

  \\[ivy-dispatching-done] d: Go to minor mode definition
  \\[ivy-dispatching-done] h: Describe minor mode" t nil)

(autoload 'counsel-major "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" nil t nil)

(autoload 'counsel-compilation-errors "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Compilation errors." t nil)

(autoload 'counsel-flycheck "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Flycheck errors." t nil)

(defvar counsel-mode nil "\
Non-nil if Counsel mode is enabled.
See the `counsel-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `counsel-mode'.")

(custom-autoload 'counsel-mode "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" nil)

(autoload 'counsel-mode "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" "\
Toggle Counsel mode on or off.
Turn Counsel mode on if ARG is positive, off otherwise. Counsel
mode remaps built-in emacs functions that have counsel
replacements.

Local bindings (`counsel-mode-map'):
\\{counsel-mode-map}

\(fn &optional ARG)" t nil)

(register-definition-prefixes "../../../.emacs.d/elpa/counsel-20200922.1006/counsel" '("counsel-" "ivy-function-called-at-point" "tmm-km-list"))

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/counsel-20200922.1006/counsel-autoloads.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; counsel-autoloads.el ends here

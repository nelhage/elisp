;;; haskell-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ghc-core" "../../../.emacs.d/elpa/haskell-mode-20221113.1425/ghc-core.el"
;;;;;;  "90768c8c7110a9bcc4f947ca5981461b")
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/ghc-core.el

(autoload 'ghc-core-create-core "ghc-core" "\
Compile and load the current buffer as tidy core." t nil)

(add-to-list 'auto-mode-alist '("\\.hcr\\'" . ghc-core-mode))

(add-to-list 'auto-mode-alist '("\\.dump-simpl\\'" . ghc-core-mode))

(autoload 'ghc-core-mode "ghc-core" "\
Major mode for GHC Core files.

\(fn)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "ghc-core" "../../../.emacs.d/elpa/haskell-mode-20221113.1425/ghc-core.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/ghc-core.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ghc-core" '("ghc-core-")))

;;;***

;;;***

;;;### (autoloads nil "ghci-script-mode" "../../../.emacs.d/elpa/haskell-mode-20221113.1425/ghci-script-mode.el"
;;;;;;  "88ca63573fc26aea310d7d28a34395d0")
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/ghci-script-mode.el

(autoload 'ghci-script-mode "ghci-script-mode" "\
Major mode for working with .ghci files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.ghci\\'" . ghci-script-mode))

;;;### (autoloads "actual autoloads are elsewhere" "ghci-script-mode"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/ghci-script-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/ghci-script-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ghci-script-mode" '("ghci-script-mode-")))

;;;***

;;;***

;;;### (autoloads nil "haskell" "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell.el"
;;;;;;  "60a31c533c0791a82d83baf529e567e4")
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell.el

(autoload 'interactive-haskell-mode "haskell" "\
Minor mode for enabling haskell-process interaction.

If called interactively, enable Interactive-Haskell mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'haskell-interactive-mode-return "haskell" "\
Handle the return key." t nil)

(autoload 'haskell-session-kill "haskell" "\
Kill the session process and buffer, delete the session.
1. Kill the process.
2. Kill the interactive buffer unless LEAVE-INTERACTIVE-BUFFER is not given.
3. Walk through all the related buffers and set their haskell-session to nil.
4. Remove the session from the sessions list.

\(fn &optional LEAVE-INTERACTIVE-BUFFER)" t nil)

(autoload 'haskell-interactive-kill "haskell" "\
Kill the buffer and (maybe) the session." t nil)

(autoload 'haskell-session "haskell" "\
Get the Haskell session, prompt if there isn't one or fail." nil nil)

(autoload 'haskell-interactive-switch "haskell" "\
Switch to the interactive mode for this session." t nil)

(autoload 'haskell-session-change "haskell" "\
Change the session for the current buffer." t nil)

(autoload 'haskell-kill-session-process "haskell" "\
Kill the process.

\(fn &optional SESSION)" t nil)

(autoload 'haskell-interactive-mode-visit-error "haskell" "\
Visit the buffer of the current (or last) error message." t nil)

(autoload 'haskell-mode-jump-to-tag "haskell" "\
Jump to the tag of the given identifier.

Give optional NEXT-P parameter to override value of
`xref-prompt-for-identifier' during definition search.

\(fn &optional NEXT-P)" t nil)

(autoload 'haskell-mode-after-save-handler "haskell" "\
Function that will be called after buffer's saving." nil nil)

(autoload 'haskell-mode-tag-find "haskell" "\
The tag find function, specific for the particular session.

\(fn &optional NEXT-P)" t nil)

(autoload 'haskell-interactive-bring "haskell" "\
Bring up the interactive mode for this session." t nil)

(autoload 'haskell-process-load-file "haskell" "\
Load the current buffer file." t nil)

(autoload 'haskell-process-reload "haskell" "\
Re-load the current buffer file." t nil)

(autoload 'haskell-process-reload-file "haskell" nil nil nil)

(autoload 'haskell-process-load-or-reload "haskell" "\
Load or reload. Universal argument toggles which.

\(fn &optional TOGGLE)" t nil)

(autoload 'haskell-process-cabal-build "haskell" "\
Build the Cabal project." t nil)

(autoload 'haskell-process-cabal "haskell" "\
Prompts for a Cabal command to run.

\(fn P)" t nil)

(autoload 'haskell-process-minimal-imports "haskell" "\
Dump minimal imports." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell" "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell" '("haskell-" "interactive-haskell-mode-map" "xref-prompt-for-identifier")))

;;;***

;;;***

;;;### (autoloads nil "haskell-align-imports" "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-align-imports.el"
;;;;;;  "41ea3dd92f1894369eca73112986e85b")
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-align-imports.el

(autoload 'haskell-align-imports "haskell-align-imports" "\
Align all the imports in the buffer." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-align-imports"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-align-imports.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-align-imports.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-align-imports" '("haskell-align-imports-")))

;;;***

;;;***

;;;### (autoloads nil "haskell-c2hs" "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-c2hs.el"
;;;;;;  "0ec3d86b917497fd5f5b00f82e928d0b")
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-c2hs.el

(add-to-list 'auto-mode-alist '("\\.chs\\'" . haskell-c2hs-mode))

(autoload 'haskell-c2hs-mode "haskell-c2hs" "\
Mode for editing *.chs files of the c2hs haskell tool.

\(fn)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-c2hs"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-c2hs.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-c2hs.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-c2hs" '("haskell-c2hs-font-lock-keywords")))

;;;***

;;;***

;;;### (autoloads nil "haskell-cabal" "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-cabal.el"
;;;;;;  "78ff5dcbba21b039789024a0edb1f7c0")
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-cabal.el

(add-to-list 'auto-mode-alist '("\\.cabal\\'\\|/cabal\\.project\\|/\\.cabal/config\\'" . haskell-cabal-mode))

(autoload 'haskell-cabal-mode "haskell-cabal" "\
Major mode for Cabal package description files.

\(fn)" t nil)

(autoload 'haskell-cabal-get-field "haskell-cabal" "\
Read the value of field with NAME from project's cabal file.
If there is no valid .cabal file to get the setting from (or
there is no corresponding setting with that name in the .cabal
file), then this function returns nil.

\(fn NAME)" t nil)

(autoload 'haskell-cabal-get-dir "haskell-cabal" "\
Get the Cabal dir for a new project.
Various ways of figuring this out, and indeed just prompting the user.  Do them
all.

\(fn &optional USE-DEFAULTS)" nil nil)

(autoload 'haskell-cabal-visit-file "haskell-cabal" "\
Locate and visit package description file for file visited by current buffer.
This uses `haskell-cabal-find-file' to locate the closest
\".cabal\" file and open it.  This command assumes a common Cabal
project structure where the \".cabal\" file is in the top-folder
of the project, and all files related to the project are in or
below the top-folder.  If called with non-nil prefix argument
OTHER-WINDOW use `find-file-other-window'.

\(fn OTHER-WINDOW)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-cabal"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-cabal.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-cabal.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-cabal" '("haskell-")))

;;;***

;;;***

;;;### (autoloads nil "haskell-collapse" "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-collapse.el"
;;;;;;  "279c0f14f186b17d9e1177ef9c464878")
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-collapse.el

(autoload 'haskell-collapse-mode "haskell-collapse" "\
Minor mode to collapse and expand haskell expressions

If called interactively, enable Haskell-Collapse mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-collapse"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-collapse.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-collapse.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-collapse" '("haskell-")))

;;;***

;;;***

;;;### (autoloads nil "haskell-commands" "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-commands.el"
;;;;;;  "3ad5c212d17600b5e1ae7a7e1f53d8f4")
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-commands.el

(autoload 'haskell-process-restart "haskell-commands" "\
Restart the inferior Haskell process." t nil)

(autoload 'haskell-process-clear "haskell-commands" "\
Clear the current process." t nil)

(autoload 'haskell-process-interrupt "haskell-commands" "\
Interrupt the process (SIGINT)." t nil)

(autoload 'haskell-describe "haskell-commands" "\
Describe the given identifier IDENT.

\(fn IDENT)" t nil)

(autoload 'haskell-rgrep "haskell-commands" "\
Grep the effective project for the symbol at point.
Very useful for codebase navigation.

Prompts for an arbitrary regexp given a prefix arg PROMPT.

\(fn &optional PROMPT)" t nil)

(autoload 'haskell-process-do-info "haskell-commands" "\
Print info on the identifier at point.
If PROMPT-VALUE is non-nil, request identifier via mini-buffer.

\(fn &optional PROMPT-VALUE)" t nil)

(autoload 'haskell-process-do-type "haskell-commands" "\
Print the type of the given expression.

Given INSERT-VALUE prefix indicates that result type signature
should be inserted.

\(fn &optional INSERT-VALUE)" t nil)

(autoload 'haskell-mode-jump-to-def-or-tag "haskell-commands" "\
Jump to the definition.
Jump to definition of identifier at point by consulting GHCi, or
tag table as fallback.

Remember: If GHCi is busy doing something, this will delay, but
it will always be accurate, in contrast to tags, which always
work but are not always accurate.
If the definition or tag is found, the location from which you jumped
will be pushed onto `xref--marker-ring', so you can return to that
position with `xref-pop-marker-stack'.

\(fn &optional NEXT-P)" t nil)

(autoload 'haskell-mode-goto-loc "haskell-commands" "\
Go to the location of the thing at point.
Requires the :loc-at command from GHCi." t nil)

(autoload 'haskell-mode-jump-to-def "haskell-commands" "\
Jump to definition of identifier IDENT at point.

\(fn IDENT)" t nil)

(autoload 'haskell-process-cd "haskell-commands" "\
Change directory.

\(fn &optional NOT-INTERACTIVE)" t nil)

(autoload 'haskell-process-cabal-macros "haskell-commands" "\
Send the cabal macros string." t nil)

(autoload 'haskell-mode-show-type-at "haskell-commands" "\
Show type of the thing at point or within active region asynchronously.
This function requires GHCi 8+ or GHCi-ng.

\\<haskell-interactive-mode-map>
To make this function works sometimes you need to load the file in REPL
first using command `haskell-process-load-file' bound to
\\[haskell-process-load-file].

Optional argument INSERT-VALUE indicates that
recieved type signature should be inserted (but only if nothing
happened since function invocation).

\(fn &optional INSERT-VALUE)" t nil)

(autoload 'haskell-process-unignore "haskell-commands" "\
Unignore any ignored files.
Do not ignore files that were specified as being ignored by the
inferior GHCi process." t nil)

(autoload 'haskell-session-change-target "haskell-commands" "\
Set the build TARGET for cabal REPL.

\(fn TARGET)" t nil)

(autoload 'haskell-mode-stylish-buffer "haskell-commands" "\
Apply stylish-haskell to the current buffer.

Use `haskell-mode-stylish-haskell-path' to know where to find
stylish-haskell executable.  This function tries to preserve
cursor position and markers by using
`haskell-mode-buffer-apply-command'." t nil)

(autoload 'haskell-mode-find-uses "haskell-commands" "\
Find use cases of the identifier at point and highlight them all." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-commands"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-commands.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-commands.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-commands" '("haskell-")))

;;;***

;;;***

;;;### (autoloads nil "haskell-compile" "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-compile.el"
;;;;;;  "5ad4f61fb105ffa945dc995b76b15749")
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-compile.el

(autoload 'haskell-compile "haskell-compile" "\
Run a compile command for the current Haskell buffer.
Obeys haskell-compiler-type to choose the appropriate build command.

If prefix argument EDIT-COMMAND is non-nil (and not a negative
prefix `-'), prompt for a custom compile command.

If EDIT-COMMAND contains the negative prefix argument `-', call
the alternative command defined in
`haskell-compile-stack-build-alt-command' /
`haskell-compile-cabal-build-alt-command'.

If there is no prefix argument, the most recent custom compile
command is used, falling back to
`haskell-compile-stack-build-command' for stack builds
`haskell-compile-cabal-build-command' for cabal builds, and
`haskell-compile-command' otherwise.

'% characters in the `-command' templates are replaced by the
base directory for build tools, or the current buffer for
`haskell-compile-command'.

\(fn &optional EDIT-COMMAND)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-compile"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-compile.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-compile.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-compile" '("haskell-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "haskell-complete-module"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-complete-module.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-complete-module.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-complete-module" '("haskell-complete-module")))

;;;***

;;;### (autoloads nil "haskell-completions" "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-completions.el"
;;;;;;  "8ad62dc7088d3bdca19874fa881f6400")
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-completions.el

(autoload 'haskell-completions-completion-at-point "haskell-completions" "\
Provide completion list for thing at point.
This function is used in non-interactive `haskell-mode'.  It
provides completions for haskell keywords, language pragmas,
GHC's options, and language extensions, but not identifiers." nil nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-completions"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-completions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-completions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-completions" '("haskell-completions-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "haskell-customize"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-customize.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-customize.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-customize" '("haskell-" "inferior-haskell-root-dir")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "haskell-debug"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-debug.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-debug.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-debug" '("haskell-debug")))

;;;***

;;;### (autoloads nil "haskell-decl-scan" "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-decl-scan.el"
;;;;;;  "6ddcecaddc61fc29c3fb6721237abc99")
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-decl-scan.el

(autoload 'haskell-ds-create-imenu-index "haskell-decl-scan" "\
Function for finding `imenu' declarations in Haskell mode.
Finds all declarations (classes, variables, imports, instances and
datatypes) in a Haskell file for the `imenu' package." nil nil)

(autoload 'turn-on-haskell-decl-scan "haskell-decl-scan" "\
Unconditionally activate `haskell-decl-scan-mode'." t nil)

(autoload 'haskell-decl-scan-mode "haskell-decl-scan" "\
Toggle Haskell declaration scanning minor mode on or off.
With a prefix argument ARG, enable minor mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.

See also info node `(haskell-mode)haskell-decl-scan-mode' for
more details about this minor mode.

Top-level declarations are scanned and listed in the menu item
\"Declarations\" (if enabled via option
`haskell-decl-scan-add-to-menubar').  Selecting an item from this
menu will take point to the start of the declaration.

\\[beginning-of-defun] and \\[end-of-defun] move forward and backward to the start of a declaration.

This may link with `haskell-doc-mode'.

For non-literate and LaTeX-style literate scripts, we assume the
common convention that top-level declarations start at the first
column.  For Bird-style literate scripts, we assume the common
convention that top-level declarations start at the third column,
ie. after \"> \".

Anything in `font-lock-comment-face' is not considered for a
declaration.  Therefore, using Haskell font locking with comments
coloured in `font-lock-comment-face' improves declaration scanning.

Literate Haskell scripts are supported: If the value of
`haskell-literate' (set automatically by `haskell-literate-mode')
is `bird', a Bird-style literate script is assumed.  If it is nil
or `tex', a non-literate or LaTeX-style literate script is
assumed, respectively.

Invokes `haskell-decl-scan-mode-hook' on activation.

\(fn &optional ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-decl-scan"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-decl-scan.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-decl-scan.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-decl-scan" '("haskell-d" "literate-haskell-ds-")))

;;;***

;;;***

;;;### (autoloads nil "haskell-doc" "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-doc.el"
;;;;;;  "a9889eb51b4a4525f568c22aeb996aaa")
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-doc.el

(autoload 'haskell-doc-mode "haskell-doc" "\
Enter `haskell-doc-mode' for showing fct types in the echo area.
See variable docstring.

\(fn &optional ARG)" t nil)

(defalias 'turn-on-haskell-doc-mode 'haskell-doc-mode)

(defalias 'turn-on-haskell-doc 'haskell-doc-mode)

(autoload 'haskell-doc-current-info "haskell-doc" "\
Return the info about symbol at point.
Meant for `eldoc-documentation-function'." nil nil)

(autoload 'haskell-doc-show-type "haskell-doc" "\
Show the type of the function near point or given symbol SYM.
For the function under point, show the type in the echo area.
This information is extracted from the `haskell-doc-prelude-types' alist
of prelude functions and their types, or from the local functions in the
current buffer.

\(fn &optional SYM)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-doc"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-doc.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-doc.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-doc" '("haskell-" "inferior-haskell-" "turn-off-haskell-doc")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "haskell-font-lock"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-font-lock.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-font-lock.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-font-lock" '("haskell-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "haskell-ghc-support"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-ghc-support.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-ghc-support.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-ghc-support" '("haskell-")))

;;;***

;;;### (autoloads nil "haskell-hoogle" "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-hoogle.el"
;;;;;;  "b9ee764119671b632c007c5f4a4d94d2")
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-hoogle.el

(autoload 'haskell-hoogle "haskell-hoogle" "\
Do a Hoogle search for QUERY.

If prefix argument INFO is given, then `haskell-hoogle-command'
is asked to show extra info for the items matching QUERY..

\(fn QUERY &optional INFO)" t nil)

(defalias 'hoogle 'haskell-hoogle)

(autoload 'haskell-hoogle-lookup-from-website "haskell-hoogle" "\
Lookup QUERY at `haskell-hoogle-url'.

\(fn QUERY)" t nil)

(autoload 'haskell-hoogle-lookup-from-local "haskell-hoogle" "\
Lookup QUERY on local hoogle server." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-hoogle"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-hoogle.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-hoogle.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-hoogle" '("haskell-hoogle-" "hoogle-prompt")))

;;;***

;;;***

;;;### (autoloads nil "haskell-indent" "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-indent.el"
;;;;;;  "0952e63fce39ad6c56a4277c7bc8bd0c")
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-indent.el

(autoload 'turn-on-haskell-indent "haskell-indent" "\
Turn on ``intelligent'' Haskell indentation mode." nil nil)

(autoload 'haskell-indent-mode "haskell-indent" "\
``Intelligent'' Haskell indentation mode.
This deals with the layout rule of Haskell.
\\[haskell-indent-cycle] starts the cycle which proposes new
possibilities as long as the TAB key is pressed.  Any other key
or mouse click terminates the cycle and is interpreted except for
RET which merely exits the cycle.
Other special keys are:
    \\[haskell-indent-insert-equal]
      inserts an =
    \\[haskell-indent-insert-guard]
      inserts an |
    \\[haskell-indent-insert-otherwise]
      inserts an | otherwise =
these functions also align the guards and rhs of the current definition
    \\[haskell-indent-insert-where]
      inserts a where keyword
    \\[haskell-indent-align-guards-and-rhs]
      aligns the guards and rhs of the region
    \\[haskell-indent-put-region-in-literate]
      makes the region a piece of literate code in a literate script

If `ARG' is falsey, toggle `haskell-indent-mode'.  Else sets
`haskell-indent-mode' to whether `ARG' is greater than 0.

Invokes `haskell-indent-hook' if not nil.

\(fn &optional ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-indent"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-indent.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-indent.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-indent" '("haskell-indent-" "turn-off-haskell-indent")))

;;;***

;;;***

;;;### (autoloads nil "haskell-indentation" "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-indentation.el"
;;;;;;  "348bf44c3d8e9a48e3a10932d2654527")
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-indentation.el

(autoload 'haskell-indentation-mode "haskell-indentation" "\
Haskell indentation mode that deals with the layout rule.
It rebinds RET, DEL and BACKSPACE, so that indentations can be
set and deleted as if they were real tabs.

If called interactively, enable Haskell-Indentation mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-haskell-indentation "haskell-indentation" "\
Turn on the haskell-indentation minor mode." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-indentation"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-indentation.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-indentation.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-indentation" '("haskell-indentation-")))

;;;***

;;;***

;;;### (autoloads nil "haskell-interactive-mode" "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-interactive-mode.el"
;;;;;;  "d820c12009f3495d33fb63bfedd80b53")
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-interactive-mode.el

(autoload 'haskell-interactive-mode-reset-error "haskell-interactive-mode" "\
Reset the error cursor position.

\(fn SESSION)" t nil)

(autoload 'haskell-interactive-mode-echo "haskell-interactive-mode" "\
Echo a read only piece of text before the prompt.

\(fn SESSION MESSAGE &optional MODE)" nil nil)

(autoload 'haskell-process-show-repl-response "haskell-interactive-mode" "\
Send LINE to the GHCi process and echo the result in some fashion.
Result will be printed in the minibuffer or presented using
function `haskell-presentation-present', depending on variable
`haskell-process-use-presentation-mode'.

\(fn LINE)" nil nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-interactive-mode"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-interactive-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-interactive-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-interactive-mode" '("haskell-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "haskell-lexeme"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-lexeme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-lexeme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-lexeme" '("haskell-lexeme-")))

;;;***

;;;### (autoloads nil "haskell-load" "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-load.el"
;;;;;;  "31f34dffe2c289a8ebf7671f20846c2c")
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-load.el

(autoload 'haskell-process-reload-devel-main "haskell-load" "\
Reload the module `DevelMain' and then run `DevelMain.update'.

This is for doing live update of the code of servers or GUI
applications.  Put your development version of the program in
`DevelMain', and define `update' to auto-start the program on a
new thread, and use the `foreign-store' package to access the
running context across :load/:reloads in GHCi." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-load"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-load.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-load.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-load" '("haskell-")))

;;;***

;;;***

;;;### (autoloads nil "haskell-menu" "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-menu.el"
;;;;;;  "526348cfc437316493103137e70f7763")
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-menu.el

(autoload 'haskell-menu "haskell-menu" "\
Launch the Haskell sessions menu." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-menu"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-menu.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-menu.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-menu" '("haskell-menu-")))

;;;***

;;;***

;;;### (autoloads nil "haskell-mode" "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-mode.el"
;;;;;;  "9bd782e14ef1eb537fcd3e217f67d48e")
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-mode.el

(autoload 'haskell-version "haskell-mode" "\
Show the `haskell-mode` version in the echo area.
With prefix argument HERE, insert it at point.

\(fn &optional HERE)" t nil)

(autoload 'haskell-mode-view-news "haskell-mode" "\
Display information on recent changes to haskell-mode." t nil)

(autoload 'haskell-mode "haskell-mode" "\
Major mode for editing Haskell programs.

\\<haskell-mode-map>

Literate Haskell scripts are supported via `haskell-literate-mode'.
The variable `haskell-literate' indicates the style of the script in the
current buffer.  See the documentation on this variable for more details.

Use `haskell-version' to find out what version of Haskell mode you are
currently using.

Additional Haskell mode modules can be hooked in via `haskell-mode-hook'.

Indentation modes:

    `haskell-indentation-mode', Kristof Bastiaensen, Gergely Risko
      Intelligent semi-automatic indentation Mk2

    `haskell-indent-mode', Guy Lapalme
      Intelligent semi-automatic indentation.

Interaction modes:

    `interactive-haskell-mode'
      Interact with per-project GHCi processes through a REPL and
      directory-aware sessions.

Other modes:

    `haskell-decl-scan-mode', Graeme E Moss
      Scans top-level declarations, and places them in a menu.

    `haskell-doc-mode', Hans-Wolfgang Loidl
      Echoes types of functions or syntax of keywords when the cursor is idle.

To activate a minor-mode, simply run the interactive command. For
example, `M-x haskell-doc-mode'. Run it again to disable it.

To enable a mode for every haskell-mode buffer, add a hook in
your Emacs configuration. To do that you can customize
`haskell-mode-hook' or add lines to your .emacs file. For
example, to enable `interactive-haskell-mode', use the following:

    (add-hook \\='haskell-mode-hook \\='interactive-haskell-mode)

Minor modes that work well with `haskell-mode':

- `smerge-mode': show and work with diff3 conflict markers used
  by git, svn and other version control systems.

\(fn)" t nil)

(autoload 'haskell-forward-sexp "haskell-mode" "\
Haskell specific version of `forward-sexp'.

Move forward across one balanced expression (sexp).  With ARG, do
it that many times.  Negative arg -N means move backward across N
balanced expressions.  This command assumes point is not in a
string or comment.

If unable to move over a sexp, signal `scan-error' with three
arguments: a message, the start of the obstacle (a parenthesis or
list marker of some kind), and end of the obstacle.

\(fn &optional ARG)" t nil)

(autoload 'haskell-literate-mode "haskell-mode" "\
As `haskell-mode' but for literate scripts.

\(fn)" t nil)

(define-obsolete-function-alias 'literate-haskell-mode 'haskell-literate-mode "2020-04")

(add-to-list 'auto-mode-alist '("\\.[gh]s\\'" . haskell-mode))

(add-to-list 'auto-mode-alist '("\\.hsig\\'" . haskell-mode))

(add-to-list 'auto-mode-alist '("\\.l[gh]s\\'" . haskell-literate-mode))

(add-to-list 'auto-mode-alist '("\\.hsc\\'" . haskell-mode))

(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))

(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode))

(add-to-list 'completion-ignored-extensions ".hi")

(autoload 'haskell-mode-generate-tags "haskell-mode" "\
Generate tags using Hasktags.  This is synchronous function.

If optional AND-THEN-FIND-THIS-TAG argument is present it is used
with function `xref-find-definitions' after new table was
generated.

\(fn &optional AND-THEN-FIND-THIS-TAG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-mode"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-mode" '("haskell-")))

;;;***

;;;***

;;;### (autoloads nil "haskell-modules" "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-modules.el"
;;;;;;  "63ddc21c2c3189347004c6b96b3dd370")
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-modules.el

(autoload 'haskell-session-installed-modules "haskell-modules" "\
Get the modules installed in the current package set.

\(fn SESSION &optional DONTCREATE)" nil nil)

(autoload 'haskell-session-all-modules "haskell-modules" "\
Get all modules -- installed or in the current project.
If DONTCREATE is non-nil don't create a new session.

\(fn SESSION &optional DONTCREATE)" nil nil)

(autoload 'haskell-session-project-modules "haskell-modules" "\
Get the modules of the current project.
If DONTCREATE is non-nil don't create a new session.

\(fn SESSION &optional DONTCREATE)" nil nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-modules"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-modules.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-modules.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-modules" '("haskell-")))

;;;***

;;;***

;;;### (autoloads nil "haskell-move-nested" "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-move-nested.el"
;;;;;;  "86fcd274db0e470784ad6376c008a13c")
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-move-nested.el

(autoload 'haskell-move-nested "haskell-move-nested" "\
Shift the nested off-side-rule block adjacent to point.
It shift the nested off-side-rule block adjacent to point by COLS
columns to the right.

In Transient Mark mode, if the mark is active, operate on the contents
of the region instead.

\(fn COLS)" nil nil)

(autoload 'haskell-move-nested-right "haskell-move-nested" "\
Increase indentation of the following off-side-rule block adjacent to point.

Use a numeric prefix argument to indicate amount of indentation to apply.

In Transient Mark mode, if the mark is active, operate on the contents
of the region instead.

\(fn COLS)" t nil)

(autoload 'haskell-move-nested-left "haskell-move-nested" "\
Decrease indentation of the following off-side-rule block adjacent to point.

Use a numeric prefix argument to indicate amount of indentation to apply.

In Transient Mark mode, if the mark is active, operate on the contents
of the region instead.

\(fn COLS)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-move-nested"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-move-nested.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-move-nested.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-move-nested" '("haskell-")))

;;;***

;;;***

;;;### (autoloads nil "haskell-navigate-imports" "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-navigate-imports.el"
;;;;;;  "a0eee690f9c980af33f7072c145b63b0")
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-navigate-imports.el

(autoload 'haskell-navigate-imports "haskell-navigate-imports" "\
Cycle the Haskell import lines or return to point (with prefix arg).

\(fn &optional RETURN)" t nil)

(autoload 'haskell-navigate-imports-go "haskell-navigate-imports" "\
Go to the first line of a list of consecutive import lines. Cycles." t nil)

(autoload 'haskell-navigate-imports-return "haskell-navigate-imports" "\
Return to the non-import point we were at before going to the module list.
   If we were originally at an import list, we can just cycle through easily." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-navigate-imports"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-navigate-imports.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-navigate-imports.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-navigate-imports" '("haskell-navigate-imports-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "haskell-presentation-mode"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-presentation-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-presentation-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-presentation-mode" '("haskell-presentation-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "haskell-process"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-process.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-process.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-process" '("haskell-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "haskell-repl"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-repl.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-repl.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-repl" '("haskell-interactive-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "haskell-sandbox"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-sandbox.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-sandbox.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-sandbox" '("haskell-sandbox-")))

;;;***

;;;### (autoloads nil "haskell-session" "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-session.el"
;;;;;;  "e72d7b162c279e57b53b3bbbb237501b")
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-session.el

(autoload 'haskell-session-maybe "haskell-session" "\
Maybe get the Haskell session, return nil if there isn't one." nil nil)

(autoload 'haskell-session-process "haskell-session" "\
Get the session process.

\(fn S)" nil nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-session"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-session.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-session.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-session" '("haskell-session")))

;;;***

;;;***

;;;### (autoloads nil "haskell-sort-imports" "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-sort-imports.el"
;;;;;;  "b33b23845070710f1afceb0c495f59ed")
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-sort-imports.el

(autoload 'haskell-sort-imports "haskell-sort-imports" "\
Sort the import list at point. It sorts the current group
i.e. an import list separated by blank lines on either side.

If the region is active, it will restrict the imports to sort
within that region." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "haskell-sort-imports"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-sort-imports.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-sort-imports.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-sort-imports" '("haskell-sort-imports-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "haskell-string"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-string.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-string.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-string" '("haskell-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "haskell-svg"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-svg.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-svg.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-svg" '("haskell-svg-")))

;;;***

;;;### (autoloads nil "haskell-unicode-input-method" "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-unicode-input-method.el"
;;;;;;  "f1d7c7c83482df43f6e1a63cbecce71a")
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-unicode-input-method.el

(autoload 'haskell-unicode-input-method-enable "haskell-unicode-input-method" "\
Set input method `haskell-unicode'." t nil)

(define-obsolete-function-alias 'turn-on-haskell-unicode-input-method 'haskell-unicode-input-method-enable "2020-04")

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "haskell-utils"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-utils.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-utils.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "haskell-utils" '("haskell-")))

;;;***

;;;### (autoloads nil "highlight-uses-mode" "../../../.emacs.d/elpa/haskell-mode-20221113.1425/highlight-uses-mode.el"
;;;;;;  "55334f1b8202e898a89d0be733b16421")
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/highlight-uses-mode.el

(autoload 'highlight-uses-mode "highlight-uses-mode" "\
Minor mode for highlighting and jumping between uses.

If called interactively, enable Highlight-Uses mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "highlight-uses-mode"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/highlight-uses-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/highlight-uses-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "highlight-uses-mode" '("highlight-uses-mode-")))

;;;***

;;;***

;;;### (autoloads nil "inf-haskell" "../../../.emacs.d/elpa/haskell-mode-20221113.1425/inf-haskell.el"
;;;;;;  "610822d6d8188399cf0c0a34f70a048d")
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/inf-haskell.el

(autoload 'run-haskell "inf-haskell" "\
Show the inferior-haskell buffer.  Start the process if needed." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "inf-haskell"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/inf-haskell.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/inf-haskell.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "inf-haskell" '("haskell-" "inf")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "w3m-haddock"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/w3m-haddock.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/haskell-mode-20221113.1425/w3m-haddock.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "w3m-haddock" '("haskell-w3m-" "w3m-haddock-")))

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/haskell-mode-20221113.1425/ghc-core.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/ghci-script-mode.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-align-imports.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-c2hs.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-cabal.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-collapse.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-commands.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-compile.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-complete-module.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-completions.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-customize.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-debug.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-decl-scan.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-doc.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-font-lock.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-ghc-support.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-hoogle.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-indent.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-indentation.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-interactive-mode.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-lexeme.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-load.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-menu.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-mode-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-mode-pkg.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-mode.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-modules.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-move-nested.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-navigate-imports.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-presentation-mode.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-process.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-repl.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-sandbox.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-session.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-sort-imports.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-string.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-svg.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-unicode-input-method.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell-utils.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/haskell.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/highlight-uses-mode.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/inf-haskell.el"
;;;;;;  "../../../.emacs.d/elpa/haskell-mode-20221113.1425/w3m-haddock.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; haskell-mode-autoloads.el ends here

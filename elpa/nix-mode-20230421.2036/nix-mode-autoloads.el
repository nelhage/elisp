;;; nix-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix"
;;;;;;  "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/nix-mode-20230421.2036/nix.el

(autoload 'pcomplete/nix "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix" "\
Completion for the nix command." nil nil)

(register-definition-prefixes "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix" '("nix-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-build"
;;;;;;  "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-build.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-build.el

(autoload 'nix-build "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-build" "\
Run nix-build in a compilation buffer.
FILE the file to parse.
ATTR the attribute to build.

\(fn &optional FILE ATTR)" t nil)

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-drv-mode"
;;;;;;  "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-drv-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-drv-mode.el

(autoload 'nix-drv-mode "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-drv-mode" "\
Pretty print Nix’s .drv files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("^/nix/store/.+\\.drv\\'" . nix-drv-mode))

(register-definition-prefixes "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-drv-mode" '("nix-drv-mode-dejsonify-buffer"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-edit"
;;;;;;  "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-edit.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-edit.el

(autoload 'nix-edit "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-edit" "\
Open the nix log.
FILE the nix file to load from.
ATTR the attribute to find in nix expressions.

\(fn &optional FILE ATTR)" t nil)

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-flake"
;;;;;;  "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-flake.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-flake.el
 (autoload 'nix-flake-dispatch "nix-flake" nil t)

(autoload 'nix-flake "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-flake" "\
Dispatch a transient interface for Nix commands.

DIR is a directory on the file system in which flake.nix resides.

Alternatively, you can specify FLAKE-REF which follows the syntax
of flake-url. It can refer to a remote url, a local file path, or
whatever supported by Nix.

\(fn DIR &key FLAKE-REF)" t nil)
 (autoload 'nix-flake-init "nix-flake" nil t)

(autoload 'nix-flake-init "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-flake" "\
Run \"nix flake init\" command via a transient interface." t nil)

(add-to-list 'auto-mode-alist '("\\flake.lock\\'" . js-mode))

(register-definition-prefixes "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-flake" '("nix-flake-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-format"
;;;;;;  "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-format.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-format.el

(autoload 'nix-format-before-save "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-format" "\
Add this to `before-save-hook' to run nixfmt when saving." nil nil)

(register-definition-prefixes "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-format" '("nix-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-instantiate"
;;;;;;  "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-instantiate.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-instantiate.el

(register-definition-prefixes "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-instantiate" '("nix-instantiate"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-log"
;;;;;;  "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-log.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-log.el

(autoload 'nix-log "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-log" "\
Open the nix log.
FILE nix file to parse.
ATTR attribute to load the log of.

\(fn FILE ATTR)" t nil)

(register-definition-prefixes "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-log" '("nix-log-path"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-mode"
;;;;;;  "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-mode.el

(autoload 'nix-mode-format "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-mode" "\
Format the entire `nix-mode' buffer." t nil)

(autoload 'nix-indent-line "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-mode" "\
Indent current line in a Nix expression." t nil)

(autoload 'nix-indent-region "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-mode" "\
Indent on a whole region. Enabled by default.
START where to start in region.
END where to end the region.

\(fn START END)" t nil)

(autoload 'nix-mode-ffap-nixpkgs-path "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-mode" "\
Support `ffap' for <nixpkgs> declarations.
If STR contains brackets, call `nix-instantiate' to find the
location of STR. If `nix-instantiate' has a nonzero exit code,
don’t do anything

\(fn STR)" nil nil)

(autoload 'nix-mode "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-mode" "\
Major mode for editing Nix expressions.

The following commands may be useful:

  '\\[newline-and-indent]'
    Insert a newline and move the cursor to align with the previous
    non-empty line.

  '\\[fill-paragraph]'
    Refill a paragraph so that all lines are at most `fill-column'
    lines long.  This should do the right thing for comments beginning
    with `#'.  However, this command doesn't work properly yet if the
    comment is adjacent to code (i.e., no intervening empty lines).
    In that case, select the text to be refilled and use
    `\\[fill-region]' instead.

The hook `nix-mode-hook' is run when Nix mode is started.

\\{nix-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

(register-definition-prefixes "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-mode" '("nix-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-prettify-mode"
;;;;;;  "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-prettify-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-prettify-mode.el

(autoload 'nix-prettify-mode "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-prettify-mode" "\
Toggle Nix Prettify mode.

With a prefix argument ARG, enable Nix Prettify mode if ARG is
positive, and disable it otherwise. If called from Lisp, enable
the mode if ARG is omitted or nil.

When Nix Prettify mode is enabled, hash-parts of the Nix store
file names (see `nix-prettify-regexp') are prettified,
i.e. displayed as `nix-prettify-char' character.  This mode can
be enabled programmatically using hooks:

  (add-hook 'shell-mode-hook 'nix-prettify-mode)

It is possible to enable the mode in any buffer, however not any
buffer's highlighting may survive after adding new elements to
`font-lock-keywords' (see `nix-prettify-special-modes' for
details).

Also you can use `global-nix-prettify-mode' to enable Nix
Prettify mode for all modes that support font-locking.

\(fn &optional ARG)" t nil)

(put 'nix-prettify-global-mode 'globalized-minor-mode t)

(defvar nix-prettify-global-mode nil "\
Non-nil if Nix-Prettify-Global mode is enabled.
See the `nix-prettify-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `nix-prettify-global-mode'.")

(custom-autoload 'nix-prettify-global-mode "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-prettify-mode" nil)

(autoload 'nix-prettify-global-mode "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-prettify-mode" "\
Toggle Nix-Prettify mode in all buffers.
With prefix ARG, enable Nix-Prettify-Global mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Nix-Prettify mode is enabled in all buffers where
`nix-prettify-turn-on' would do it.

See `nix-prettify-mode' for more information on Nix-Prettify mode.

\(fn &optional ARG)" t nil)

(define-obsolete-function-alias 'global-nix-prettify-mode 'nix-prettify-global-mode "v1.2.2")

(register-definition-prefixes "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-prettify-mode" '("nix-prettify-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-repl"
;;;;;;  "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-repl.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-repl.el

(autoload 'nix-repl "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-repl" "\
Load the Nix-REPL." t nil)

(autoload 'nix-repl-completion-at-point "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-repl" "\
Completion at point function for Nix using \"nix-repl\".
See `completion-at-point-functions'." nil nil)

(register-definition-prefixes "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-repl" '("nix-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-search"
;;;;;;  "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-search.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-search.el

(autoload 'nix-search--search "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-search" "\


\(fn SEARCH FILE &optional NO-CACHE USE-FLAKES)" nil nil)

(autoload 'nix-search--display "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-search" "\


\(fn RESULTS &optional DISPLAY-BUFFER USE-FLAKES SEARCH FILE)" nil nil)

(autoload 'nix-search "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-search" "\
Run nix search.
SEARCH a search term to use.
FILE a Nix expression to search in.

\(fn SEARCH &optional FILE DISPLAY-BUFFER)" t nil)

(register-definition-prefixes "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-search" '("nix-search-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-shebang"
;;;;;;  "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-shebang.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-shebang.el

(register-definition-prefixes "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-shebang" '("nix-shebang-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-shell"
;;;;;;  "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-shell.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-shell.el

(autoload 'nix-shell-unpack "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-shell" "\
Run Nix’s unpackPhase.
FILE is the file to unpack from.
ATTR is the attribute to unpack.

\(fn FILE ATTR)" t nil)

(autoload 'nix-shell-configure "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-shell" "\
Run Nix’s configurePhase.
FILE is the file to configure from.
ATTR is the attribute to configure.

\(fn FILE ATTR)" t nil)

(autoload 'nix-shell-build "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-shell" "\
Run Nix’s buildPhase.
FILE is the file to build from.
ATTR is the attribute to build.

\(fn FILE ATTR)" t nil)

(autoload 'nix-eshell-with-packages "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-shell" "\
Create an Eshell buffer that has the shell environment in it.
PACKAGES a list of packages to pull in.
PKGS-FILE a file to use to get the packages.

\(fn PACKAGES &optional PKGS-FILE)" nil nil)

(autoload 'nix-eshell "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-shell" "\
Create an Eshell buffer that has the shell environment in it.
FILE the .nix expression to create a shell for.
ATTR attribute to instantiate in NIX-FILE.

\(fn FILE &optional ATTR)" t nil)

(autoload 'nix-shell-with-string "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-shell" "\
A nix-shell emulator in Emacs from a string.
STRING the nix expression to use.

\(fn STRING)" nil nil)

(autoload 'nix-shell "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-shell" "\
A nix-shell emulator in Emacs.
FILE the file to instantiate.
ATTR an attribute of the Nix file to use.

\(fn FILE &optional ATTR)" t nil)

(register-definition-prefixes "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-shell" '("nix-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-store"
;;;;;;  "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-store.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-store.el

(register-definition-prefixes "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-store" '("nix-"))

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-mode-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/nix-mode-20230421.2036/nix-mode-pkg.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; nix-mode-autoloads.el ends here
